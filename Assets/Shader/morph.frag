uniform vec4 lightDir;
varying vec3 vViewVec;
varying vec3 vNormal;
varying vec4 vDiffuse;


void main(void)
{
   vec3 lDir = vec3 (lightDir.x, lightDir.y, -lightDir.z);
   vec3 nNormal = normalize(vNormal);

   // Soft diffuse
   float diffuse = 0.5 + 0.5 * dot(lDir, nNormal);
   // Standard specular
   float specular = pow(clamp(dot(reflect(-normalize(vViewVec), nNormal), lDir),0.0, 1.0), 24.0);

   gl_FragColor = diffuse * vDiffuse + specular;
}