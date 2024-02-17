uniform sampler2D BaseTex;
void main(){
  gl_FragColor = texture2D(BaseTex, gl_TexCoord[0].xy);
}
