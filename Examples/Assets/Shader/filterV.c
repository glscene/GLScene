void main(
                    float4  iPos  	: POSITION,
                    float2  iTex0	: TEXCOORD0,
                out float4  oPos  	: POSITION,
                out float2  oTex0 	: TEXCOORD0,

            uniform float4x4         MVP
         )
{
			/* Convert vertex coordinates and pass texcoords to fragment shader */
			oPos.xyzw	= mul( MVP, iPos ).xyzw;
			oTex0.xy	= iTex0.xy;
}
