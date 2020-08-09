#ifndef __GLSL_CG_DATA_TYPES
# define half float
# define half3x3 mat3
# define half2 vec2
# define half3 vec3
# define half4 vec4
#endif

uniform vec4  EyePos;
uniform float Time;

varying vec3  EyeVec;
varying half4 FogColor;

void main()
{
    EyeVec = gl_Vertex.xyz-EyePos.xyz;

    FogColor.rgb = gl_Fog.color.rgb;
    FogColor.a = clamp((distance(EyePos.xyz, gl_Vertex.xyz)-gl_Fog.start)*gl_Fog.scale, 0.0, 1.0);

    gl_TexCoord[0] = gl_TextureMatrix[0]*gl_Vertex;
    gl_TexCoord[1] = gl_Vertex*0.01+Time*0.02;
    gl_TexCoord[2] = gl_Vertex*0.02+Time*0.02;

    gl_Position = gl_ModelViewProjectionMatrix*gl_Vertex;
}
