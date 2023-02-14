uniform mat4 view_matrix;
uniform float speed;
uniform float lerpMin;
uniform float lerpMax;
uniform float time_0_X;

varying vec3 vViewVec;
varying vec3 vNormal;
varying vec4 vDiffuse;


void main(void)
{
   vec4 Pos = gl_Vertex;
   // Define the two key frames
   vec3 spherePos = normalize(gl_Vertex.xyz);
   vec3 cubePos = 0.9 * gl_Vertex.xyz;

   vec3 sphereNormal = spherePos;
   
   vec3 cubeNormal = gl_Normal;

   // Make a smooth 0->1->0 curve
   float t = fract(speed * time_0_X);
   t = smoothstep(0.0, 0.5, t) - smoothstep(0.5, 1.0, t);

   // Find the interpolation factor
   float lrp = lerpMin + (lerpMax - lerpMin) * t;

   // Linearly interpolate the position and normal
   Pos = vec4(mix(spherePos, cubePos, lrp), 1.0);

   vNormal   = mix(sphereNormal, cubeNormal, lrp);


   // Use position as base color
   vDiffuse = 0.5 + 0.5 * Pos;
   
   // Give the thing some size
   Pos.xyz *= 30.0;
   

   // Eye-space lighting
   vNormal    =   gl_NormalMatrix * vNormal;
   vViewVec   = -(gl_NormalMatrix * Pos.xyz);
   vViewVec.z = - vViewVec.z;
   

   gl_Position = gl_ModelViewProjectionMatrix * Pos;

}