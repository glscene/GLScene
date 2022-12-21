uniform vec4 furColorScale;
uniform sampler2D Fur;
uniform sampler2D FurColor;

varying vec2 vTexCoord;

void main(void)
{
    // Get the alpha component for this shaft
    vec4 fAlpha = texture2D( Fur, vTexCoord );    
    
    // Get the hair shaft color, and scale by the color scale
    vec4 fColor = furColorScale * texture2D( FurColor, vTexCoord ) * fAlpha;
        
    // Return the calculated color
    gl_FragColor = fColor;
}