varying vec4 Ca;
varying vec4 Cd;
varying vec4 Cs;

varying vec4 V_eye;
varying vec4 L_eye;
varying vec4 N_eye;

vec3 reflect(vec3 N, vec3 L)
{
  return 2.0*N*dot(N, L) - L;
}

void main(void)
{
  vec3 V = normalize(vec3(V_eye));
  vec3 L = normalize(vec3(L_eye));
  vec3 N = normalize(vec3(N_eye));

  float diffuse = clamp(dot(L, N), 0.0, 1.0);

  vec3 R = reflect(N, L);
  float specular = clamp(pow(dot(R, V), 16.0), 0.0, 1.0);

  gl_FragColor = Ca + (Cd*diffuse) + (Cs*specular);
}

