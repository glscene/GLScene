#version 330 compatibility

uniform sampler2DArray ColorLayers;
uniform sampler2DArray DepthLayers;

void main() 
{ 
	const int layerCount = 6;
    	
	vec4  vColor[layerCount];
	float vDepth[layerCount];
	int   vSurfOrder[layerCount];
	int   i = 0;
	
	// Calculate un-normalized texture coordinates
	vec2 tmp = floor(textureSize(DepthLayers, 0).xy * gl_TexCoord[0].st); 
		
	// First, get sample data and init the surface order
	for (i = 0; i < layerCount; i++)
	{
		vSurfOrder[i] = i;
        	vColor[i] = texelFetch(ColorLayers, ivec3(tmp, i), 0);
		vDepth[i] = texelFetch(DepthLayers, ivec3(tmp, i), 0).r;
	}
	
	// Sort depth values, largest to front and smallest to back
	// Must run through array (size^2-size) times, or early-exit
	// if any pass shows all samples to be in order
	for (int j = 0; j < layerCount; j++)
	{
		bool bFinished = true;
        	for (i = 0; i < (layerCount-1); i++)
	    	{
	        	float temp1 = vDepth[vSurfOrder[i]];
	        	float temp2 = vDepth[vSurfOrder[i+1]];
    	    
	        	if (temp2 < temp1)
	        	{
	            	// swap values
	            	int tempIndex   = vSurfOrder[i];
	            	vSurfOrder[i]   = vSurfOrder[i+1];
	            	vSurfOrder[i+1] = tempIndex;
	            	bFinished = false;
	        	}
	    	}
	    
	    if (bFinished) j = layerCount;
	}
	
	// Now, sum all colors in order from front to back. Apply alpha.
	bool bFoundFirstColor = false;
	vec4 summedColor = vec4(0.0, 0.0, 0.0, 0.0);
	for (i = (layerCount-1); i >= 0; i--)
	{
		int surfIndex = vSurfOrder[i];
		if(vColor[surfIndex].a > 0.001)
		{
			if (bFoundFirstColor == false)
			{
				// apply 100% of the first color
				summedColor = vColor[surfIndex];
				bFoundFirstColor = true;
			}
			else
			{
				// apply color with alpha
				summedColor.rgb = (summedColor.rgb * (1.0 - vColor[surfIndex].a))     +
				                  (vColor[surfIndex].rgb * vColor[surfIndex].a);
			}
		}
	}
   
	gl_FragColor = vec4(summedColor.rgb, 1.0);
}
