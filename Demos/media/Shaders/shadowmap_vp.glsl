#version 120
uniform mat4 EyeToLightMatrix;

void main()
{
  gl_Position = ftransform();
  vec4 Pe = gl_ModelViewMatrix * gl_Vertex;

  gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;
  gl_TexCoord[1] =  EyeToLightMatrix * Pe;
}
