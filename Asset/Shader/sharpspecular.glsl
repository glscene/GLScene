varying vec4 Ca;
varying vec4 Cd;
varying vec4 Cs;

varying vec4 V_eye;
varying vec4 L_eye;
varying vec4 N_eye;

const float sharpness = 0.2;

void main(void)
{
  vec3 V = normalize(vec3(V_eye));
  vec3 L = normalize(vec3(L_eye));
  vec3 N = normalize(vec3(N_eye));

  float diffuse = clamp(dot(L, N), 0.0, 1.0);

  vec3 H = normalize(L + V);
  float specular = clamp(pow(dot(N, H), 32.0), 0.0, 1.0);

  float w = 0.18 * (1.0 - sharpness);
  specular = smoothstep(0.72 - w, 0.72 + w, specular);

  gl_FragColor = Ca + (Cd*diffuse) + (Cs*specular);
}

