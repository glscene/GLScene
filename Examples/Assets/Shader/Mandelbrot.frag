#define MAX_ITER 64.0

varying vec2 pixel;

uniform float positionX;
uniform float positionY;
uniform float scale;

uniform sampler2D colorMap;

void main() {
	float x, y, x0, y0;
	x = x0 = (pixel.x * scale + positionX);
	y = y0 = (pixel.y * scale + positionY);

	float x2 = x*x;
	float y2 = y*y;

	float iteration = 0.0;

	while(x2 + y2 < 4.0 && iteration < MAX_ITER) {
 		y = 2.0 * x * y + y0;
	        x = x2 - y2 + x0;

		x2 = x*x;     
		y2 = y*y;

		iteration++;
	};
					 
	gl_FragColor = texture2D(colorMap, vec2(iteration/MAX_ITER, iteration/MAX_ITER));
}
