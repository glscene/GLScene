//
//
// The graphics platform GLXcene https://github.com/glscene
//
//

unit GLX.GLSLVertexDisplacementShader;

(*
   VertexDisplacement shader : Basic Vertex Displacement with Perlin Noise
   You can Improved it :
   The vertex displacement can be done by reading a 2D or 3D texture.
   It can be done along the normal or the tangent.
   It can be scaled, twisted, modulated, inverted... 
    Converted from : https://www.clicktorelease.com/blog/vertex-displacement-noise-3d-webgl-glsl-three-js 

   At this time only one light source is supported

 *)

interface

{$I Scenario.inc}

uses
  Winapi.OpenGL, 
  Winapi.OpenGLext,  
  System.Classes,
  
  GXL.OpenGLx, 
  GLX.BaseClasses, 
  GLX.VectorGeometry, 
  GLX.Scene, 
  GLX.CrossPlatform, 
  GLX.State,
  GLX.Context, 
  GLX.RenderContextInfo, 
  GLX.Coordinates,
  Scenario.TextureFormat, 
  GLX.Color, 
  GLX.Texture, 
  GLX.Material,
  GLX.GLSLShader, 
  GLX.CustomShader;

(* Custom class for GLSLVertexDisplacementShader. 
 VertexDisplacement Shader : Spherical Environment Mapping *)
Type
  TgxCustomGLSLVertexDisplacementShader = class(TgxCustomGLSLShader)
  private
    FAmbientColor: TgxColor;
//    FDiffuseColor: TgxColor;
    FSpecularColor: TgxColor;
    FAmbientFactor : Single;
    FDiffuseFactor : Single;
    FSpecularFactor : Single;
    FMaterialLibrary: TgxAbstractMaterialLibrary;
    FMainTexture: TgxTexture;
    FMainTexName   : TgxLibMaterialName;
    FElapsedTime : Single;
    FNoise : Single;
    FDisplacementScale : Single;
    FNoiseScale : Single;
    FTurbulenceFactor : Single;
    FNoisePeriod : Single;
    FTimeFactor : Single;
    function GetMaterialLibrary: TgxAbstractMaterialLibrary;
    procedure SetMainTexTexture(const Value: TgxTexture);
    function GetMainTexName: TgxLibMaterialName;
    procedure SetMainTexName(const Value: TgxLibMaterialName);
    //procedure SetDiffuseColor(AValue: TgxColor);
    procedure SetAmbientColor(AValue: TgxColor);
    procedure SetSpecularColor(AValue: TgxColor);
  protected
    procedure DoApply(var rci : TgxRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
    procedure SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
//    property DiffuseColor : TgxColor read FDiffuseColor Write setDiffuseColor;
    property SpecularColor : TgxColor Read FSpecularColor Write setSpecularColor;
    property AmbientColor : TgxColor Read FAmbientColor Write setAmbientColor;
    property AmbientFactor : Single Read FAmbientFactor Write FAmbientFactor;
    property DiffuseFactor : Single Read FDiffuseFactor Write FDiffuseFactor;
    property SpecularFactor : Single Read FSpecularFactor Write FSpecularFactor;
    property MaterialLibrary: TgxAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property MainTexture: TgxTexture read FMainTexture write SetMainTexTexture;
    property MainTextureName: TgxLibMaterialName read GetMainTexName write SetMainTexName;
    property ElapsedTime: Single read FElapsedTime write FElapsedTime;
    property NoiseFactor : Single read FNoise write FNoise;
    property NoiseScale : Single read FNoiseScale write FNoiseScale;
    property TurbulenceFactor : Single read FTurbulenceFactor write FTurbulenceFactor;
    property NoisePeriod : Single read FNoisePeriod write FNoisePeriod;
    property DisplacementScale : Single read FDisplacementScale write FDisplacementScale;
    property TimeFactor : Single read FTimeFactor write FTimeFactor;
  end;

  TgxSLVertexDisplacementShader = class(TgxCustomGLSLVertexDisplacementShader)
  published
    property AmbientColor;
//    property DiffuseColor;
    property SpecularColor;
    property AmbientFactor;
    property DiffuseFactor;
    property SpecularFactor;
    property MaterialLibrary;
    property MainTexture;
    property MainTextureName;
    property ElapsedTime;
    property NoiseFactor;
    property NoiseScale;
    property TurbulenceFactor;
    property NoisePeriod;
    property DisplacementScale;
    property TimeFactor;
  end;
  
//-------------------------------------------  
implementation
//-------------------------------------------  

constructor TgxCustomGLSLVertexDisplacementShader.Create(AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    clear;
    Add('uniform float time; ');
    Add('uniform float NoiseFactor; ');
    Add('uniform float TurbulenceFactor; ');
    Add('uniform float NoiseScale; ');
    Add('uniform float NoisePeriod; ');
    Add('uniform float DisplacementScale; ');
    Add('uniform float TimeFactor; ');

    Add('varying vec3 viewVec; ');
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');
    Add('varying vec2 vTexCoord; ');
    Add('varying float noise; ');

    //
    //GLSL textureless classic 3D noise "cnoise",
    // with an RSL-style periodic variant "pnoise".
    // Author:  Stefan Gustavson (stefan.gustavson@liu.se)
    // Version: 2011-10-11
    //
    // Many thanks to Ian McEwan of Ashima Arts for the
    // ideas for permutation and gradient selection.
    //
    // Copyright (c) 2011 Stefan Gustavson. All rights reserved.
    // Distributed under the MIT license. See LICENSE file.
    // https://github.com/ashima/webgl-noise
    //

    Add('vec3 mod289(vec3 x) ');
    Add('{ ');
    Add('  return x - floor(x * (1.0 / 289.0)) * 289.0; ');
    Add('} ');

    Add('vec4 mod289(vec4 x) ');
    Add('{ ');
    Add('  return x - floor(x * (1.0 / 289.0)) * 289.0; ');
    Add('} ');

    Add('vec4 permute(vec4 x) ');
    Add('{ ');
    Add('  return mod289(((x*34.0)+1.0)*x); ');
    Add('} ');

    Add('vec4 taylorInvSqrt(vec4 r) ');
    Add('{ ');
    Add('  return 1.79284291400159 - 0.85373472095314 * r; ');
    Add('} ');

    Add('vec3 fade(vec3 t) { ');
    Add('  return t*t*t*(t*(t*6.0-15.0)+10.0); ');
    Add('} ');

    // Classic Perlin noise, periodic variant
    Add('float pnoise(vec3 P, vec3 rep) ');
    Add('{ ');
    Add('  vec3 Pi0 = mod(floor(P), rep); // Integer part, modulo period ');
    Add('  vec3 Pi1 = mod(Pi0 + vec3(1.0), rep); // Integer part + 1, mod period ');
    Add('  Pi0 = mod289(Pi0); ');
    Add('  Pi1 = mod289(Pi1); ');
    Add('  vec3 Pf0 = fract(P); // Fractional part for interpolation ');
    Add('  vec3 Pf1 = Pf0 - vec3(1.0); // Fractional part - 1.0 ');
    Add('  vec4 ix = vec4(Pi0.x, Pi1.x, Pi0.x, Pi1.x); ');
    Add('  vec4 iy = vec4(Pi0.yy, Pi1.yy); ');
    Add('  vec4 iz0 = Pi0.zzzz; ');
    Add('  vec4 iz1 = Pi1.zzzz; ');

    Add('  vec4 ixy = permute(permute(ix) + iy); ');
    Add('  vec4 ixy0 = permute(ixy + iz0); ');
    Add('  vec4 ixy1 = permute(ixy + iz1); ');

    Add('  vec4 gx0 = ixy0 * (1.0 / 7.0); ');
    Add('  vec4 gy0 = fract(floor(gx0) * (1.0 / 7.0)) - 0.5; ');
    Add('  gx0 = fract(gx0); ');
    Add('  vec4 gz0 = vec4(0.5) - abs(gx0) - abs(gy0); ');
    Add('  vec4 sz0 = step(gz0, vec4(0.0)); ');
    Add('  gx0 -= sz0 * (step(0.0, gx0) - 0.5); ');
    Add('  gy0 -= sz0 * (step(0.0, gy0) - 0.5); ');

    Add('  vec4 gx1 = ixy1 * (1.0 / 7.0); ');
    Add('  vec4 gy1 = fract(floor(gx1) * (1.0 / 7.0)) - 0.5; ');
    Add('  gx1 = fract(gx1); ');
    Add('  vec4 gz1 = vec4(0.5) - abs(gx1) - abs(gy1); ');
    Add('  vec4 sz1 = step(gz1, vec4(0.0)); ');
    Add('  gx1 -= sz1 * (step(0.0, gx1) - 0.5); ');
    Add('  gy1 -= sz1 * (step(0.0, gy1) - 0.5); ');

    Add('  vec3 g000 = vec3(gx0.x,gy0.x,gz0.x); ');
    Add('  vec3 g100 = vec3(gx0.y,gy0.y,gz0.y); ');
    Add('  vec3 g010 = vec3(gx0.z,gy0.z,gz0.z); ');
    Add('  vec3 g110 = vec3(gx0.w,gy0.w,gz0.w); ');
    Add('  vec3 g001 = vec3(gx1.x,gy1.x,gz1.x); ');
    Add('  vec3 g101 = vec3(gx1.y,gy1.y,gz1.y); ');
    Add('  vec3 g011 = vec3(gx1.z,gy1.z,gz1.z); ');
    Add('  vec3 g111 = vec3(gx1.w,gy1.w,gz1.w); ');

    Add('  vec4 norm0 = taylorInvSqrt(vec4(dot(g000, g000), dot(g010, g010), dot(g100, g100), dot(g110, g110))); ');
    Add('  g000 *= norm0.x; ');
    Add('  g010 *= norm0.y; ');
    Add('  g100 *= norm0.z; ');
    Add('  g110 *= norm0.w; ');
    Add('  vec4 norm1 = taylorInvSqrt(vec4(dot(g001, g001), dot(g011, g011), dot(g101, g101), dot(g111, g111))); ');
    Add('  g001 *= norm1.x; ');
    Add('  g011 *= norm1.y; ');
    Add('  g101 *= norm1.z; ');
    Add('  g111 *= norm1.w; ');

    Add('  float n000 = dot(g000, Pf0); ');
    Add('  float n100 = dot(g100, vec3(Pf1.x, Pf0.yz)); ');
    Add('  float n010 = dot(g010, vec3(Pf0.x, Pf1.y, Pf0.z)); ');
    Add('  float n110 = dot(g110, vec3(Pf1.xy, Pf0.z)); ');
    Add('  float n001 = dot(g001, vec3(Pf0.xy, Pf1.z)); ');
    Add('  float n101 = dot(g101, vec3(Pf1.x, Pf0.y, Pf1.z)); ');
    Add('  float n011 = dot(g011, vec3(Pf0.x, Pf1.yz)); ');
    Add('  float n111 = dot(g111, Pf1); ');

    Add('  vec3 fade_xyz = fade(Pf0); ');
    Add('  vec4 n_z = mix(vec4(n000, n100, n010, n110), vec4(n001, n101, n011, n111), fade_xyz.z); ');
    Add('  vec2 n_yz = mix(n_z.xy, n_z.zw, fade_xyz.y); ');
    Add('  float n_xyz = mix(n_yz.x, n_yz.y, fade_xyz.x); ');
    Add('  return 2.2 * n_xyz; ');
    Add('} ');

    Add('float turbulence( vec3 p ) { ');
    Add('  float w = 100.0; ');
    Add('  float t = -.5; ');
    Add('  for (float f = 1.0 ; f <= 10.0 ; f++ ){ ');
    Add('      float power = pow( 2.0, f ); ');
    Add('      t += abs( pnoise( vec3( power * p ), vec3( 10.0, 10.0, 10.0 ) ) / power ); ');
    Add('  } ');
    Add('  return t; ');
    Add('} ');

    Add('void main() { ');      //96
    Add('  mat4 mWorld = gl_ModelViewMatrix; ');
    Add('  vec3 Normal = gl_NormalMatrix * gl_Normal; //gl_Normal; ');
    Add('  vec4 Position = gl_Vertex; ');
    Add('  vec4 vert =  gl_ModelViewMatrix * gl_Vertex; ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');

    Add('  vTexCoord = gl_MultiTexCoord0; ');

    Add('  vec3 vNormal = normalize( Normal * mat3(mWorld )); ');
    Add('  time = TimeFactor*time; ');
    // add time to the noise parameters so it's animated
    Add('  noise = NoiseFactor* -0.10* turbulence( TurbulenceFactor * vNormal+time ); ');
    // get a 3d noise using the position, low frequency
    Add('  float b = (NoisePeriod*time)*pnoise( vec3((NoiseScale *time)* (Position.xyz + vec3(time ))), vec3(100) ); ');
    // compose both noises
    Add('  float displacement =( noise + b); ');

    Add('  vec4 newPosition =vec4((Position.xyz + DisplacementScale*(vec3(vNormal * displacement))),1.0); ');

    Add('  normal = vNormal; ');
    Add('  lightVec = vec3(lightPos - vert); ');
    Add('  viewVec = -vec3(vert); ');

    Add('  gl_Position = gl_ModelViewProjectionMatrix  * newPosition; ');

    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    clear;
    Add('uniform vec4 AmbientColor; ');
    Add('uniform vec4 SpecularColor; ');
    Add('uniform float DiffuseIntensity; ');
    Add('uniform float AmbientIntensity; ');
    Add('uniform float SpecularIntensity; ');

    Add('uniform sampler2D MainTexture; ');

    Add('varying vec3 viewVec; ');
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');
    Add('varying float noise; ');

    Add('float random( vec3 scale, float seed ){ ');
    Add('  return fract( sin( dot( gl_FragCoord.xyz + seed, scale ) ) * 43758.5453 + seed ) ; ');
    Add('} ');

    Add('void main() { ');
    // get a random offset
    Add('  float r = 0.01 * random( vec3( 12.9898, 78.233, 151.7182 ), 0.0 ); ');
    // lookup vertically in the texture, using noise and offset
    // to get the right RGB colour
    Add('  vec2 tPos = vec2( 0, 1.0 - 1.3 * noise + r ); ');
    Add('  vec4 DiffuseColor;    ');
    Add('  DiffuseColor = texture2D( MainTexture, tPos ); ');

    // Simple Lighting
    Add('  vec3 L = normalize(lightVec); ');
    Add('  vec3 V = normalize(viewVec); ');
    Add('  vec3 halfAngle = normalize(L + V); ');
    Add('  float NdotL = dot(L, normal); ');
    Add('  float NdotH = clamp(dot(halfAngle, normal), 0.0, 1.0); ');
    Add('  // "Half-Lambert" technique for more pleasing diffuse term ');
    Add('  float diffuse = DiffuseColor*(0.5 * NdotL + 0.5); ');
    Add('  float specular = pow(NdotH, 64.0); ');

    Add('  vec4 FinalColour = AmbientColor*AmbientIntensity + ');
    Add('                     DiffuseColor*diffuse*DiffuseIntensity + ');
    Add('                     SpecularColor*specular*SpecularIntensity; ');
    Add('  gl_FragColor = FinalColour; ; ');
//    Add('  gl_FragColor = vec4(DiffuseColor,1.0); ');
    Add('} ');
  end;

  FAmbientColor := TgxColor.Create(Self);
  //FDiffuseColor := TgxColor.Create(Self);
  FSpecularColor := TgxColor.Create(Self);

  //setup initial parameters
  FAmbientColor.SetColor(0.15, 0.15, 0.15, 1.0);
  //FDiffuseColor.SetColor(1, 1, 1, 1);
  FSpecularColor.SetColor(1.0, 1.0, 1.0, 1.0);
  FAmbientFactor  := 0.8;
  FDiffuseFactor  :=0.9;
  FSpecularFactor :=0.8;
  FElapsedTime := 1.0;
  FNoise := 10.0;
  FDisplacementScale:=1.0;
  FNoiseScale:=0.05;
  FTurbulenceFactor:=0.5;
  FNoisePeriod := 5.0;
  FTimeFactor := 0.05;
end;

destructor TgxCustomGLSLVertexDisplacementShader.Destroy;
begin
  FAmbientColor.Destroy;
 // FDiffuseColor.Destroy;
  FSpecularColor.Destroy;

  inherited;
end;

procedure TgxCustomGLSLVertexDisplacementShader.DoApply(var rci: TgxRenderContextInfo; Sender: TObject);
begin

  GetGLSLProg.UseProgramObject;
//  Param['DiffuseColor'].AsVector4f := FDiffuseColor.Color;
  param['AmbientColor'].AsVector4f := FAmbientColor.Color;
  param['SpecularColor'].AsVector4f := FSpecularColor.Color;
  param['AmbientIntensity'].AsVector1f := FAmbientFactor;
  param['DiffuseIntensity'].AsVector1f := FDiffuseFactor;
  param['SpecularIntensity'].AsVector1f := FSpecularFactor;

  Param['time'].AsVector1f := FElapsedTime;
  Param['NoiseFactor'].AsVector1f := FNoise;
  Param['NoiseScale'].AsVector1f := FNoiseScale;
  Param['TurbulenceFactor'].AsVector1f := FTurbulenceFactor;
  Param['NoisePeriod'].AsVector1f := FNoisePeriod;
  Param['DisplacementScale'].AsVector1f := FDisplacementScale;
  Param['TimeFactor'].AsVector1f := FTimeFactor;

  Param['MainTexture'].AsTexture2D[0] := FMainTexture;

end;

function TgxCustomGLSLVertexDisplacementShader.DoUnApply(var rci: TgxRenderContextInfo): Boolean;
begin
  gl.ActiveTexture(GL_TEXTURE0_ARB);
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;


function TgxCustomGLSLVertexDisplacementShader.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TgxCustomGLSLVertexDisplacementShader.SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TgxAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TgxCustomGLSLVertexDisplacementShader.SetMainTexTexture(const Value: TgxTexture);
begin
  if FMainTexture = Value then Exit;
  FMainTexture := Value;
  NotifyChange(Self)
end;

function TgxCustomGLSLVertexDisplacementShader.GetMainTexName: TgxLibMaterialName;
begin
  Result := TgxMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTexture);
  if Result = '' then Result := FMainTexName;
end;

procedure TgxCustomGLSLVertexDisplacementShader.SetMainTexName(const Value: TgxLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTexture := TgxMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;


//procedure TgxCustomGLSLVertexDisplacementShader.SetDiffuseColor(AValue: TgxColor);
//begin
//  FDiffuseColor.DirectColor := AValue.Color;
//end;

procedure TgxCustomGLSLVertexDisplacementShader.SetAmbientColor(AValue: TgxColor);
begin
  FAmbientColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLVertexDisplacementShader.SetSpecularColor(AValue: TgxColor);
begin
  FSpecularColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLVertexDisplacementShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin
        if FMainTexture <> nil then
        begin
          Index := TgxMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            SetMainTexTexture(nil);
        end;
        FMaterialLibrary := nil;
      end;
end;

end.
