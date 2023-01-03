#ifndef __GLSL_CG_DATA_TYPES
# define half float
# define half3x3 mat3
# define half2 vec2
# define half3 vec3
# define half4 vec4
#endif

uniform vec4  EyePos;

uniform sampler2D WaveMap;
#ifndef __GLSL_CG_DATA_TYPES
uniform sampler2D ReflectionMap;
#else
uniform sampler2DRect ReflectionMap;
#endif

varying vec3  EyeVec;
varying half4 FogColor;

const float cFresnelBias = 0.1;
const half3 cDeepColor = half3(0, 0.1, 0.2);
const half3 cShallowColor = half3(0, 0.3, 0.6);

void main()
{
    half3 wave0 = texture2D(WaveMap, gl_TexCoord[1].xz).xyz;
    half3 wave1 = texture2D(WaveMap, gl_TexCoord[2].xz).xyz;
    half3 wave = normalize(wave0 + wave1 - half3(1.0, 1.0, 0.0));

    float fDist = 10.0/(length(EyeVec)+1.0);

    float facing = 1.0+EyeVec.y*fDist*(0.1-wave.y*0.01);
    float fresnel = cFresnelBias + (1.0-cFresnelBias)*pow(facing, 4.0);

    half3 waterColor = mix(cDeepColor, cShallowColor, facing);

    half2 waveJitter = wave.xy*fDist;
    waveJitter.y = abs(waveJitter.y);

#ifndef __GLSL_CG_DATA_TYPES
    vec3 reflecUV = gl_TexCoord[0].xyw;
    reflecUV.xy -= waveJitter*reflecUV.z;
    half4 rColor = texture2DProj(ReflectionMap, reflecUV);
#else
    vec3 reflecUV = gl_TexCoord[0].xyw;
    reflecUV.xy -= waveJitter*250*reflecUV.z;
    half4 rColor = texture2DRectProj(ReflectionMap, reflecUV);
#endif

    half  hdr = 6.0-5.0*rColor.a;

    half3 finalColor = mix(waterColor, hdr*rColor.rgb , fresnel);

    gl_FragColor = half4(mix(finalColor, FogColor.rgb, FogColor.a), 1.0);
}
