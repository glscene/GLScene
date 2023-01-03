varying vec4 Ca;
varying vec4 Cd;
varying vec4 Cs;

varying vec4 V_eye;
varying vec4 L_eye;
varying vec4 N_eye;

void main(void)
{
  vec3 V = normalize(vec3(V_eye));
  vec3 L = normalize(vec3(L_eye));
  vec3 N = normalize(vec3(N_eye));

  float diffuse_f = clamp(dot(L, N), 0.0, 1.0);
  float diffuse_b = clamp(dot(L, -N), 0.0, 1.0);

  vec3 H = normalize(L + V);
  float specular = clamp(pow(dot(N, H), 32.0), 0.0, 1.0);

  gl_FragColor = Ca + 0.8*(Cd*diffuse_f) + 0.2*(Cd*diffuse_b) + (Cs*specular);
}

