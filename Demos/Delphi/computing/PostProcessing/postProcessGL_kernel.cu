// includes, system
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

// clamp x to range [a, b]
__device__ int clamp(int x, int a, int b)
{
    return max(a, min(b, x));
}

// convert floating point rgb color to 8-bit integer
__device__ int rgbToInt(float r, float g, float b)
{
    r = clamp(r, 0.0f, 255.0f);
    g = clamp(g, 0.0f, 255.0f);
    b = clamp(b, 0.0f, 255.0f);
    return (int(b)<<16) | (int(g)<<8) | int(r);
}

// get pixel from 2D image, with clamping to border
__device__ int getPixel(int *data, int x, int y, int width, int height)
{
    x = clamp(x, 0, width-1);
    y = clamp(y, 0, height-1);
    return data[y*width+x];
}

// macros to make indexing shared memory easier
#define SMEM(X, Y) sdata[(Y)*tilew+(X)]

/*
    2D convolution using shared memory
    - operates on 8-bit RGB data stored in 32-bit int
    - assumes kernel radius is less than or equal to block size
    - not optimized for performance
     _____________
    |   :     :   |
    |_ _:_____:_ _|
    |   |     |   |
    |   |     |   |
    |_ _|_____|_ _|
  r |   :     :   |
    |___:_____:___|
      r    bw   r
    <----tilew---->
*/

extern __shared__ int sdata[];

extern "C"
__global__ void
cudaProcess_k(
int* g_data,
int* g_odata,
int imgw,
int imgh,
int tilew,
int r,
float threshold,
float highlight)
{
    

    int tx = threadIdx.x;
    int ty = threadIdx.y;
    int bw = blockDim.x;
    int bh = blockDim.y;
    int x = blockIdx.x*bw + tx;
    int y = blockIdx.y*bh + ty;

    // copy tile to shared memory
    // center region
    SMEM(r + tx, r + ty) = getPixel(g_data, x, y, imgw, imgh);

    // borders
    if (threadIdx.x < r) {
        // left
        SMEM(tx, r + ty) = getPixel(g_data, x - r, y, imgw, imgh);
        // right
        SMEM(r + bw + tx, r + ty) = getPixel(g_data, x + bw, y, imgw, imgh);
    }
    if (threadIdx.y < r) {
        // top
        SMEM(r + tx, ty) = getPixel(g_data, x, y - r, imgw, imgh);
        // bottom
        SMEM(r + tx, r + bh + ty) = getPixel(g_data, x, y + bh, imgw, imgh);
    }

    // load corners
    if ((threadIdx.x < r) && (threadIdx.y < r)) {
        // tl
        SMEM(tx, ty) = getPixel(g_data, x - r, y - r, imgw, imgh);
        // bl
        SMEM(tx, r + bh + ty) = getPixel(g_data, x - r, y + bh, imgw, imgh);
        // tr
        SMEM(r + bw + tx, ty) = getPixel(g_data, x + bh, y - r, imgw, imgh);
        // br
        SMEM(r + bw + tx, r + bh + ty) = getPixel(g_data, x + bw, y + bh, imgw, imgh);
    }

    // wait for loads to complete
    __syncthreads();

    // perform convolution
    float rsum = 0.0;
    float gsum = 0.0;
    float bsum = 0.0;
    float samples = 0.0;

    for(int dy=-r; dy<=r; dy++) {
        for(int dx=-r; dx<=r; dx++) {
			int pixel = SMEM(r+tx+dx, r+ty+dy);

            // only sum pixels within disc-shaped kernel
            float l = dx*dx + dy*dy;
			if (l <= r*r)
			{
                float r = float(pixel&0xff);
                float g = float((pixel>>8)&0xff);
                float b = float((pixel>>16)&0xff);

//                // brighten highlights
//                float lum = (r + g + b) / (255*3);
//				if (lum > threshold)
//				{
//                    r *= highlight;
//					g *= highlight;
//                    b *= highlight;
//				}

                rsum += r;
                gsum += g;
                bsum += b;
                samples += 1.0;
            }
        }
    }

    rsum /= samples;
    gsum /= samples;
    bsum /= samples;
    g_odata[y*imgw+x] = rgbToInt(rsum, gsum, bsum);
}
