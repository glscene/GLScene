uniform vec4 DiffuseColor;
uniform vec4 AmbientColor;

uniform float LightIntensity;
uniform sampler2D MainTexture;

varying vec3 Normal;
varying vec3 LightVector;
varying vec3 CameraVector;
varying vec2 Texcoord;

void main(void)
{
  vec4 TextureContrib = texture2D(MainTexture, Texcoord);
  vec4 DiffuseContrib = clamp(DiffuseColor * dot(LightVector, Normal), 0.0, 1.0);

  float SpecPower = 8.0;
  float SpecMult = 2.0;

  vec3 reflect_vec = reflect(CameraVector, -Normal);
  float Temp = max(dot(reflect_vec, LightVector), 0.0);
  float SpecContrib = pow(Temp, SpecPower);
  SpecContrib = SpecMult * clamp(SpecContrib, 0.0, 0.95);


  gl_FragColor = TextureContrib * LightIntensity * (AmbientColor + DiffuseContrib + SpecContrib);
}

