#version 120
uniform sampler2D NormalMap;
uniform samplerCube EnvironmentMap;

varying vec3 EyeVec;
varying mat3 ObjToTangentSpace;

const float cFresnelBias = 0.1;
const vec3 cDeepColor = vec3(0.0, 0.1, 0.2);
const vec3 cShallowColor = vec3(0.0, 0.2, 0.4);

const vec3 cNormalCorrection = vec3(-1.0, -1.0, 0.0);

void main()
{
	// sum normal maps
    vec3 t0 = texture2D(NormalMap, gl_TexCoord[0].xy).rgb;
    vec3 t1 = texture2D(NormalMap, gl_TexCoord[1].xy).rgb;
    vec3 normal = t0 + t1 + cNormalCorrection;

    vec3 nW = normalize(ObjToTangentSpace * normal);

    vec3 r = reflect(EyeVec, nW);
    vec4 rColor = textureCube(EnvironmentMap, r);
    float hdr = 1.0+5.0*rColor.a;

    float  facing = 1.0-max(dot(normalize(-EyeVec), nW), 0.0);
    vec3 waterColor = mix(cDeepColor, cShallowColor, facing);

    float fresnel = cFresnelBias + (1.0-cFresnelBias)*pow(facing, 4.0);

    gl_FragColor = vec4(waterColor + (fresnel*hdr)*rColor.rgb, 1.0);
}
