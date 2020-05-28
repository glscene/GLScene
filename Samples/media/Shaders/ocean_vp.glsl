#version 120
uniform float Time;
uniform vec4  EyePos;

varying vec3   EyeVec;
varying mat3 ObjToTangentSpace;

const float cTexScale = 0.2;
const float cBumpScale = 0.15;
const float cBumpSpeed = 0.4;

// Waves parameters

#define NWAVES 3
struct Wave {
   float freq;  // 2*PI / wavelength
   float amp;   // amplitude
   float phase; // speed * 2*PI / wavelength
   vec2 dir;
};
Wave wave[NWAVES];

const float k = 2.0;

Wave initWave(float freq, float amp, float phase, vec2 dir)
{
   Wave result;
   result.freq = freq;
   result.amp = amp;
   result.phase = phase;
   result.dir = dir;
   return result;
}

vec2 evaluateWave(Wave w, vec2 pos, float t)
{
   vec2 result;

   float wavePos = dot(w.dir, pos)*w.freq + t*w.phase;
   float waveAmp = sin(wavePos)*0.5 + 0.5;

   result.x = w.amp * pow(waveAmp, k);
   result.y = k*w.freq*w.amp * pow(waveAmp, k-1.0) * cos(wavePos);

   return result;
}

void main()
{
   vec4 P = gl_Vertex;

   wave[0] = initWave( 0.2, 0.9, 12.0, vec2(1.0, 0.0) );
   wave[1] = initWave( 0.3, 0.7, 9.0,  vec2(0.98, 0.2) );
   wave[2] = initWave( 0.4, 0.5, 8.0,  vec2(0.99, -0.15) );

   // sum waves
   vec2 dd = vec2(0.0, 0.0);
   for(int i=0; i<NWAVES; i++) {
      vec2 waveEval = evaluateWave(wave[i], P.xy, Time);
      P.z += waveEval.x;
      dd += waveEval.y * wave[i].dir;
   }

   gl_Position = gl_ModelViewProjectionMatrix * P;

   // compute tangent basis
   vec3 B = vec3(1.0, 0.0, dd.x);
   vec3 T = vec3(0.0, 1.0, dd.y);
   vec3 N = vec3(-dd.x, -dd.y, 1);

   // compute the 3x3 tranform from tangent space to object space
   // first rows are the tangent and binormal scaled by the bump scale
   ObjToTangentSpace[0] = cBumpScale * normalize(T);
   ObjToTangentSpace[1] = cBumpScale * normalize(B);
   ObjToTangentSpace[2] = normalize(N);

   float texTime = Time*cBumpSpeed;
   gl_TexCoord[0].xy = gl_Vertex.xy*cTexScale + texTime;
   gl_TexCoord[1].xy = gl_Vertex.xy*(2.0*cTexScale) - texTime;

   EyeVec = normalize(gl_Vertex.xyz - EyePos.xyz);
}
