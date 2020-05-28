#version 120
uniform sampler2DShadow ShadowMap;

void main()
{
  float depth = shadow2D(ShadowMap, gl_TexCoord[0].stp).r;

  gl_FragColor = vec4(vec3(depth), 0.0);
}
