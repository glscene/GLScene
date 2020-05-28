#version 120
uniform sampler2D TextureMap;
uniform sampler2DShadow ShadowMap;
uniform sampler2D LightspotMap;

uniform bool Softly;
uniform float Scale;

void main()
{
  float shadow = shadow2DProj(ShadowMap, gl_TexCoord[1]).r;
  float proj  = texture2DProj(LightspotMap, gl_TexCoord[1]).r;

  if (Softly)
  {
    float shadow1 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(-0.00048,0.0,0.0,0.0)*Scale).r;
    float shadow2 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.00048,0.0,0.0,0.0)*Scale).r;
    float shadow3 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.0,0.00048,0.0,0.0)*Scale).r;
    float shadow4 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.0,-0.00048,0.0,0.0)*Scale).r;

    float shadow5 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(-0.00096,0.0,0.0,0.0)*Scale).r;
    float shadow6 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.00096,0.0,0.0,0.0)*Scale).r;
    float shadow7 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.0,0.00096,0.0,0.0)*Scale).r;
    float shadow8 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.0,-0.00096,0.0,0.0)*Scale).r;

    float shadow9 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(-0.00144,0.0,0.0,0.0)*Scale).r;
    float shadow10 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.00144,0.0,0.0,0.0)*Scale).r;
    float shadow11 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.0,0.00144,0.0,0.0)*Scale).r;
    float shadow12 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.0,-0.00144,0.0,0.0)*Scale).r;

    shadow = shadow + shadow1 + shadow2 + shadow3 + shadow4;
    shadow = shadow + shadow5 + shadow6 + shadow7 + shadow8;
    shadow = shadow + shadow9 + shadow10 + shadow11 + shadow12;
    shadow = shadow * 0.08;
  }
  shadow = shadow * proj;

  vec4 color = texture2D(TextureMap, gl_TexCoord[0].st);

  gl_FragColor = color * vec4(mix(shadow, 1.0, 0.2));
  gl_FragColor.a = color.a;
}
