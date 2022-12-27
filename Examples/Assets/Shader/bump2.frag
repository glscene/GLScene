/*
	Normal 			- normal texture
	Base_Height		- Base texture with height map in alpha channel
*/

uniform sampler2D Normal;
uniform sampler2D base_tex;
uniform sampler2D Base_Height; // height in alpha ch.

varying	vec3 g_lightVec;
varying	vec3 g_viewVec;

uniform vec3 cBumpSize;// = 0.02 * vec2 (2.0, -1.0);

void main()
{   
	float LightAttenuation = clamp(1.0 - dot(g_lightVec, g_lightVec), 0.0, 1.0);
	vec3 lightVec = normalize(g_lightVec);
	vec3 viewVec = normalize(g_viewVec);
	
	float height = texture2D(Base_Height, gl_TexCoord[0].xy).r;
	height = height * cBumpSize.x + cBumpSize.y;

	vec2 newUV = gl_TexCoord[0].xy + viewVec.xy * height;
	vec4 color_base = texture2D(base_tex,newUV);
	vec3 bump = texture2D(Normal, newUV.xy).rgb * 2.0 - 1.0;
	bump = normalize(bump);

	//vec4 base = texture2D(Base_Height, newUV.xy);
	float base = texture2D(Base_Height, newUV.xy).r;
	
	float diffuse = clamp(dot(lightVec, bump), 0.0, 1.0);
	float specular = pow(clamp(dot(reflect(-viewVec, bump), lightVec), 0.0, 1.0), 16.0);
	gl_FragColor = color_base * gl_LightSource[0].diffuse 
					* (diffuse * base + 0.7 * specular)
					* LightAttenuation;
	
}

