// declare texture reference for 2D float texture
texture<float, 2, cudaReadModeElementType> tex;

////////////////////////////////////////////////////////////////////////////////
//! Transform an image using texture lookups
//! @param g_odata  output data in global memory
////////////////////////////////////////////////////////////////////////////////
extern "C"
__global__ void
transformKernel( float* g_odata, int width, int height, float theta) 
{
    // calculate normalized texture coordinates
    unsigned int x = blockIdx.x*blockDim.x + threadIdx.x;
    unsigned int y = blockIdx.y*blockDim.y + threadIdx.y;

    float u = x / (float) width;
    float v = y / (float) height;

    // transform coordinates
    u -= 0.5f;
    v -= 0.5f;
    float tu = u*cosf(theta) - v*sinf(theta) + 0.5f;
    float tv = v*cosf(theta) + u*sinf(theta) + 0.5f;

    // read from texture and write to global memory
    g_odata[y*width + x] = tex2D(tex, tu, tv);
}
