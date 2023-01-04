uniform	samplerCube	sky;
varying	vec3		vTexCoord;

void main (void) {

   gl_FragColor = textureCube( sky, vTexCoord );
	
}
