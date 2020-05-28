////////////////////////////////////////////////////////////////////////////////
//! Simple kernel to modify vertex positions in sine wave pattern
//! @param data  data in global memory'
////////////////////////////////////////////////////////////////////////////////
__global__ void kernel(
float4* pos,
unsigned int width,
unsigned int height,
float time)
{
	unsigned int x = blockIdx.x*blockDim.x + threadIdx.x;
	unsigned int y = blockIdx.y*blockDim.y + threadIdx.y;

	// calculate uv coordinates
        float u = x / (float) width;
        float v = y / (float) height;
        u = u*2.0f - 1.0f;
        v = v*2.0f - 1.0f;

	// calculate simple sine wave pattern
	float freq = 4.0f;
	float w = sinf(u*freq + time) * cosf(v*freq + time) * 0.5f;

	// write output vertex
	pos[y*width+x] = make_float4(u, w, v, 1.0f);
}