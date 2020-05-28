varying vec4 Ca;
varying vec4 Cd;
varying vec4 Cs;

varying vec4 V_eye;
varying vec4 L_eye;
varying vec4 N_eye;

const float edgeWidth = 0.3;

float bias(float value, float b)
{
  return (b > 0.0) ? pow(value, log2(b) / log2(0.5)) : 0.0;
}

void main(void)
{
  vec3 V = normalize(vec3(V_eye));
  vec3 L = normalize(vec3(L_eye));
  vec3 N = normalize(vec3(N_eye));

  float diffuse = clamp(dot(L, N), 0.0, 1.0);

  float edgeScale = bias(1.0 - dot(V, N), edgeWidth);
  edgeScale = max(0.7, 4.0*edgeScale);
  diffuse = diffuse * edgeScale;

  gl_FragColor = Ca + (Cd*diffuse);
}

