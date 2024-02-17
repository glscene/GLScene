uniform vec4 camera;
void main(){
  vec3 cY = vec3(0.0, 1.0, 0.0);
  vec3 cZ = vec3(0.0, 0.0, 1.0);
  vec3 cX = vec3(1.0, 0.0, 0.0);
  vec3 vZ = vec3(camera - gl_Vertex);
  vec3 vZp = normalize(vec3(vZ.x, 0.0, vZ.z));
  vZ = normalize(vZ);
  vec3 vX = normalize(cross(cY, vZ));
  vec3 vY = cross(vZ, vX);
  float a1 = sign(dot(cX, vX))*acos(dot(cZ, vX));
  vec4 p = vec4(vY*gl_Normal.y + vX*gl_Normal.x, 0.0);
  gl_Position = gl_ModelViewProjectionMatrix*(gl_Vertex + p);
  gl_TexCoord[0].x = round((a1+gl_Normal.z)*5.09+gl_MultiTexCoord0.x)*0.03125;
  gl_TexCoord[0].y = round(dot(vZ, vZp)*7+gl_MultiTexCoord0.y)*0.125;
}
