varying vec3 Normal;
varying vec3 LightVector;
varying vec3 CameraVector;
varying vec2 Texcoord;


void main(void)
{
  gl_Position = ftransform();
  Texcoord = gl_MultiTexCoord0.xy;
  Normal = normalize(gl_NormalMatrix * gl_Normal);
  vec3 p = (gl_ModelViewMatrix * gl_Vertex).xyz;
  LightVector = normalize(gl_LightSource[0].position.xyz - p);
  CameraVector = normalize(p);
}

