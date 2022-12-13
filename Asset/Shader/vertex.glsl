const vec4 AMBIENT = vec4( 0.1, 0.1, 0.1, 1.0 );
const vec4 SPECULAR = vec4( 1.0, 1.0, 1.0, 1.0 );
uniform vec4 light;

varying vec4 Ca;
varying vec4 Cd;
varying vec4 Cs;

varying vec4 V_eye;
varying vec4 L_eye;
varying vec4 N_eye;

void main(void)
{
  V_eye = gl_ModelViewMatrix * gl_Vertex;
  L_eye = (gl_ModelViewMatrix * light) - V_eye;
  N_eye = vec4(gl_NormalMatrix * gl_Normal, 1.0);

  gl_Position = gl_ProjectionMatrix * V_eye;
  V_eye = -V_eye;

  Ca = AMBIENT;
  Cd = gl_Color;
  Cs = SPECULAR;
}

