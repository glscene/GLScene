uniform sampler2D basetex;
uniform sampler2D bumptex;
varying vec3 eyeVec;

void main()
{
    vec2 texUV, srcUV = gl_TexCoord[0].xy;
    float height = texture2D(bumptex, srcUV).r;
    float v = height * 0.04 - 0.02;
    vec3 eye = normalize(eyeVec);
    texUV = srcUV + (eye.xy * v);        
        
    vec3 rgb = texture2D(basetex, texUV).rgb;

    // output final color
   gl_FragColor = vec4(vec3(rgb), 1.0);
   // gl_FragColor = vec4(vec3(rgb)*height, 1.0);
  
}

