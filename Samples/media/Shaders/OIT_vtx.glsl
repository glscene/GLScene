#version 120

void main()
{
   gl_Position = vec4(sign(gl_Vertex.xy), -1., 1.);
   gl_TexCoord[0] = gl_MultiTexCoord0;
}