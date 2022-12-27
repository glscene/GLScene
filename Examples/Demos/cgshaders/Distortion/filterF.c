void main(
                    float2 iTex0 	: TEXCOORD0,
                out float4 oCol  	: COLOR,

            uniform samplerRECT refractTex,
            uniform float		screenW,
            uniform float		screenH,
            uniform float		cursorX,
            uniform float		cursorY

         )
{

    		/* Deform by screwing up the texcoords. There are
               millions of ways to do that. Here's one, the closer to the cursor,
               the more deformation	 */
	float	distX		= cursorX - iTex0.x;
    float	distY		= cursorY - iTex0.y;
	float	dist		= sqrt( distX*distX + distY*distY );

            iTex0.x    *= screenW + dist * 200;
            iTex0.y    *= screenH + dist * 200;

			/* And a little bit grayscale stuff. We want the surroundings
               of the cursor to be grayscaled, the bigger the distance,
               the more brownish grayscale	 */
    float3	pixel		= texRECT( refractTex, iTex0.xy ).rgb;
    float	grayScale	= 0.3*pixel.r + 0.59*pixel.g + 0.11*pixel.b;

    		oCol.rgb	= lerp( pow(pixel,2)*3, grayScale * float3(0.31,0.28,0.21), sqrt( sqrt(dist)) ).rgb;
			oCol.a		= 1;
}
