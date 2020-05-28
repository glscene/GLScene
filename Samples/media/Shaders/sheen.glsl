varying vec4 Ca;
varying vec4 Cd;
varying vec4 Cs;

varying vec4 V_eye;
varying vec4 L_eye;
varying vec4 N_eye;

const vec3 esheen = vec3( 0.1, 0.2, 0.5 );    // Environment sheen
const vec3 lsheen = vec3( 0.3, 0.4, 0.5 );    // Light sheen
const vec3 gsheen = vec3( 0.4, 0.35, 0.3 );   // Glow sheen
const float breathe = 0.8;                    // Sheen attenuation

void main(void)
{
  vec3 V = normalize(vec3(V_eye));
  vec3 L = normalize(vec3(L_eye));
  vec3 N = normalize(vec3(N_eye));

  float diffuse = clamp(dot(L, N), 0.0, 1.0);

  vec3 H = normalize(L + V);

  float cos = dot(N, V);
  float sin = sqrt(1.0-pow(cos, 2.0));
  vec3 specular = vec3(0.0, 0.0, 0.0);

  specular = specular + pow(sin, (1.0/breathe*5.0)) * dot(L, V) * vec3(Cs) * esheen;
  specular = specular + pow(sin, (1.0/breathe*5.0)) * dot(L, N) * vec3(Cs) * lsheen;
  specular = specular + pow(cos, (breathe*5.0)) * dot(L, N) * vec3(Cs) * gsheen;

  gl_FragColor = Ca + (Cd*diffuse) + vec4(specular, 1.0);
}

