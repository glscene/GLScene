uniform float shell_distance;
uniform float pass_index;
varying vec2  vTexCoord;

void main(void)
{
   vTexCoord    = gl_MultiTexCoord0.xy;
   
   vec4 Position = gl_Vertex;
   Position.xyz += shell_distance * pass_index * gl_Normal; 
   gl_Position = gl_ModelViewProjectionMatrix * Position;
}
