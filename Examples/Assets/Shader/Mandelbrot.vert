varying vec2 pixel;

void main() 
{
	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
	pixel = gl_Position.xy;
}
