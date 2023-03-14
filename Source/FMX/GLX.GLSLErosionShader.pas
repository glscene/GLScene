//
// Graphic Scene Engine, http://glscene.org
//
{
   Erosion shader Erode surface object and render with Anisotropic Specular Reflection 
   At this time one light source is supported
}
unit GLX.GLSLErosionShader;

interface

{$I Scena.inc}

uses
  System.Classes,
  
  GLX.Scene, 
  GLX.CrossPlatform, 
  GLX.BaseClasses, 
  GLX.State, 
  Winapi.OpenGL, 
  Winapi.OpenGLext,  
  GLX.OpenGL1x, 
  GLX.Context, 
  GLX.RenderContextInfo, 
  GLX.Coordinates, 
  Scena.VectorGeometry, 
  Scena.VectorTypes,
  Scena.TextureFormat, 
  GLX.Color, 
  GLX.Texture, 
  GLX.Material,
  GLX.GLSLShader, 
  GLX.CustomShader;

{ Custom class for GLSLSimpleErosionShader. 
 A shader that Erode surface object }
Type
  TgxCustomGLSLSimpleErosionShader = class(TgxCustomGLSLShader)
  private
    FMaterialLibrary: TgxAbstractMaterialLibrary;
    FMainTex  : TgxTexture;
    FNoiseTex : TgxTexture;
    FErosionTex : TgxTexture;
    FMainTexName   : TgxLibMaterialName;
    FNoiseTexName  : TgxLibMaterialName;
    FErosionTexName  : TgxLibMaterialName;

    FErosionScale: Single;
    FErosionFactor: Single;
    FIntensityFactor1: Single;
    FIntensityFactor2: Single;
    FSpecularColor : TgxColor;
    FAmbientColor : TgxColor;
    FAmbientFactor : Single;
    FDiffuseFactor : Single;
    FSpecularFactor : Single;
    FSpecularRoughness : Single;
    FAnisotropicRoughness : Single;
    function GetMaterialLibrary: TgxAbstractMaterialLibrary;
    procedure SetMainTexTexture(const Value: TgxTexture);
    procedure SetNoiseTexTexture(const Value: TgxTexture);
    procedure SetErosionTexTexture(const Value: TgxTexture);
    function GetMainTexName: TgxLibMaterialName;
    procedure SetMainTexName(const Value: TgxLibMaterialName);
    function GetNoiseTexName: TgxLibMaterialName;
    procedure SetNoiseTexName(const Value: TgxLibMaterialName);
    function GetErosionTexName: TgxLibMaterialName;
    procedure SetErosionTexName(const Value: TgxLibMaterialName);
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
//    property Color1: TgxColor read FColor1;
//    property Color2: TgxColor read FColor2;
    property MaterialLibrary: TgxAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property MainTexture: TgxTexture read FMainTex write SetMainTexTexture;
    property MainTextureName: TgxLibMaterialName read GetMainTexName write SetMainTexName;
    property NoiseTexture: TgxTexture read FNoiseTex write SetNoiseTexTexture;
    property NoiseTextureName: TgxLibMaterialName read GetNoiseTexName write SetNoiseTexName;
    property ErosionTexture: TgxTexture read FErosionTex write SetErosionTexTexture;
    property ErosionTextureName: TgxLibMaterialName read GetErosionTexName write SetErosionTexName;
    property ErosionFactor: Single read FErosionFactor write FErosionFactor;
    property ErosionScale: Single read FErosionFactor write FErosionFactor;
    property IntensityFactor1: Single read FIntensityFactor1 write FIntensityFactor1;
    property IntensityFactor2: Single read FIntensityFactor2 write FIntensityFactor2;
    property SpecularColor : TgxColor Read FSpecularColor Write setSpecularColor;
    property AmbientColor : TgxColor Read FAmbientColor Write setAmbientColor;
    property AmbientFactor : Single Read FAmbientFactor Write FAmbientFactor;
    property DiffuseFactor : Single Read FDiffuseFactor Write FDiffuseFactor;
    property SpecularFactor : Single Read FSpecularFactor Write FSpecularFactor;
    property SpecularRoughness : Single Read FSpecularRoughness  Write FSpecularRoughness;
    property AnisotropicRoughness : Single Read FAnisotropicRoughness Write FAnisotropicRoughness;
  end;

  TgxSLSimpleErosionShader = class(TgxCustomGLSLSimpleErosionShader)
  published
//    property Color1;
//    property Color2;
    property MaterialLibrary;
    property MainTexture;
    property MainTextureName;
    property NoiseTexture;
    property NoiseTextureName;
    property ErosionTexture;
    property ErosionTextureName;
    property ErosionScale;
    property ErosionFactor;
    property IntensityFactor1;
    property IntensityFactor2;
    property SpecularColor;
    property AmbientColor;
    property AmbientFactor;
    property DiffuseFactor;
    property SpecularFactor;
    property SpecularRoughness;
    property AnisotropicRoughness;

  end;

implementation

{ TgxCustomGLSLSimpleErosionShader }

constructor TgxCustomGLSLSimpleErosionShader.Create(AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('uniform float Scale; ');
    Add('varying vec3 normal; ');
    Add('varying vec2 vTexCoord; ');
    Add('varying vec3 lightVec; ');
    Add('varying vec3 viewVec; ');
    Add('varying vec3 Position; ');
    Add(' ');
    Add('void main(void) { ');
   // Add('  mat4 mWorld = gl_ModelViewMatrix; ');
    Add('  vec3 Normal = gl_Normal; ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');
    Add('  vec4 vert =  gl_ModelViewMatrix * gl_Vertex; ');
    Add('  normal = gl_NormalMatrix * gl_Normal; ');
    Add('  Position       = vec3(gl_Vertex)*Scale; ');
    Add('  vTexCoord = gl_MultiTexCoord0; ');
    Add('  lightVec = vec3(lightPos - vert); ');
    Add('  viewVec = -vec3(vert); ');
    Add('  gl_Position    = ftransform(); ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    Add('uniform float ErosionFactor; ');
    Add('uniform float IntensityFactor1; ');
    Add('uniform float IntensityFactor2; ');
    Add('uniform sampler2D MainTexture; ');
    Add('uniform sampler2D Noise2d; ');
    Add('uniform sampler2D ErosionTexture; ');
    Add('uniform vec4  SpecularColor; ');
    Add('uniform vec4  AmbientColor; ');
    Add('uniform float DiffuseIntensity; ');
    Add('uniform float AmbientIntensity; ');
    Add('uniform float SpecularIntensity; ');
    Add('uniform float SpecularRoughness; ');
    Add('uniform float AnisoRoughness; ');
    Add('varying vec3 normal; ');
    Add('varying vec2 vTexCoord; ');
    Add('varying vec3 lightVec; ');
    Add('varying vec3 viewVec; ');
    Add('varying vec3 Position; ');
    Add(' ');
    Add('void main (void) ');
    Add('{ ');
    Add('  vec3 offset     = vec3(- ErosionFactor, - ErosionFactor + 0.06, - ErosionFactor * 0.92); ');
    Add('  vec4 DiffuseColor;    ');
    Add('  vec4 Color1 = texture2D(MainTexture,vTexCoord); ');
    Add('  vec4 Color2 = texture2D(ErosionTexture,vTexCoord); ');
    Add('  // Compute noise ');
    Add('  vec3 noiseCoord = Position.xyz + offset; ');
    Add('  vec4 noiseVec   = texture2D(Noise2d, noiseCoord.xy); ');
    Add('  float intensity = (abs(noiseVec[0] - 0.25) + ');
    Add('                     abs(noiseVec[1] - 0.125) + ');
    Add('                     abs(noiseVec[2] - 0.0625) + ');
    Add('                     abs(noiseVec[3] - 0.03125)); ');
    Add('  // continue noise evaluation');
    Add('  intensity = IntensityFactor1 * (noiseVec.x + noiseVec.y+ noiseVec.z + noiseVec.w); ');
    Add('  intensity = IntensityFactor2 * abs(2.0 * intensity -1.0); ');
    Add('  // discard pixels in a psuedo-random fashion (noise) ');
    Add('  if (intensity < fract(0.5 - offset.x - offset.y - offset.z)) discard; ');

    Add('  // color fragments different colors using noise ');
    Add('  clamp(intensity, 0.0, 1.0); ');
    Add('  Color2.a =1.0-intensity; ');
    Add('  Color1.a =1.0; ');
    Add('  DiffuseColor = mix(Color2, Color1, intensity); ');
    Add('  DiffuseColor.a = intensity; ');

    Add('  // Anisotropic Specular Lighting Reflection ');
    // Anisotropic Specular Reflection
    // This  is useful for depicting surfaces
    // such as velvet or brushed metal,
    // it allows you to stretch the highlight along a
    // SpecDirection vector (in object space)
    // add  new var and replace the follow line
    // vec3 T = cross(norm,V) by vec3 T = cross(norm,normalize(SpecDirection));
    Add('  vec3 norm = normalize(normal); ');
    Add('  vec3 L = normalize(lightVec); ');
    Add('  vec3 V = normalize(viewVec); ');
    Add('  vec3 halfAngle = normalize(L + V); ');
    Add('  vec3 T = cross(norm,V);  ');
    Add('  float NdotL = dot(L, norm); ');
    Add('  float NdotH = clamp(dot(halfAngle, norm), 0.0, 1.0); ');
    Add('  // "Half-Lambert" technique for more pleasing diffuse term ');
    Add('  float diffuse = 0.5 * NdotL + 0.5; ');
    Add('  float specular = pow(NdotH,1.0/SpecularRoughness); '); //54
    Add('  // Heidrich-Seidel anisotropic distribution ');
    Add('  float ldott = dot(L,T); ');
    Add('  float vdott = dot(V,T); ');
    Add('  float aniso = pow(sin(ldott)*sin(vdott) + cos(ldott)*cos(vdott),1.0/AnisoRoughness); ');
    Add(' vec3 FinalColour = AmbientColor*AmbientIntensity + ');
    Add('                         DiffuseColor*diffuse*DiffuseIntensity + ');
    Add('                         SpecularColor*aniso*specular*SpecularIntensity; ');
    Add('  gl_FragColor = vec4(FinalColour,intensity); ');
    Add('} '); 
  end;

  //setup initial parameters

  FAmbientColor := TgxColor.Create(self);
  FAmbientColor.SetColor(0.2,0.2,0.2,1.0);
  FSpecularColor := TgxColor.Create(self);
  FSpecularColor.SetColor(0.75,0.75,0.75,1.0);
  FAmbientFactor  := 0.8;
  FDiffuseFactor  :=0.9;
  FSpecularFactor :=0.8;
  FSpecularRoughness :=0.45;
  FAnisotropicRoughness :=0.35;

  FErosionScale     := 0.03;
  FErosionFactor    := 0.35;
  FIntensityFactor1 := 0.75;
  FIntensityFactor2 := 1.95;
end;

destructor TgxCustomGLSLSimpleErosionShader.Destroy;
begin
  FAmbientColor.Free;
  FSpecularColor.Free;
  inherited;
end;

procedure TgxCustomGLSLSimpleErosionShader.DoApply(var rci : TgxRenderContextInfo; Sender : TObject);
begin
  GetGLSLProg.UseProgramObject;

  param['AmbientColor'].AsVector4f := FAmbientColor.Color;
  param['SpecularColor'].AsVector4f := FSpecularColor.Color;
  param['AmbientIntensity'].AsVector1f := FAmbientFactor;
  param['DiffuseIntensity'].AsVector1f := FDiffuseFactor;
  param['SpecularIntensity'].AsVector1f := FSpecularFactor;
  param['SpecularRoughness'].AsVector1f := FSpecularRoughness;
  param['AnisoRoughness'].AsVector1f := FAnisotropicRoughness;
  param['ErosionFactor'].AsVector1f := FErosionFactor;
  param['IntensityFactor1'].AsVector1f := FIntensityFactor1;
  param['IntensityFactor2'].AsVector1f := FIntensityFactor2;
  param['Scale'].AsVector1f := FErosionScale;

  param['MainTexture'].AsTexture2D[0] := FMainTex;
  param['Noise2d'].AsTexture2D[1] := FNoiseTex;
  param['ErosionTexture'].AsTexture2D[2] := FErosionTex;
 // GetGLSLProg.UniformTextureHandle['Noise2d', 1, GL_TEXTURE_2D] := FNoiseTexture.Handle;
end;

function TgxCustomGLSLSimpleErosionShader.DoUnApply(var rci: TgxRenderContextInfo): Boolean;
begin
  GetGLSLProg.EndUseProgramObject;
  //glPopAttrib;
  Result := False;
end;

function TgxCustomGLSLSimpleErosionShader.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TgxCustomGLSLSimpleErosionShader.SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TgxAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TgxCustomGLSLSimpleErosionShader.SetMainTexTexture(const Value: TgxTexture);
begin
  if FMainTex = Value then Exit;
  FMainTex := Value;
  NotifyChange(Self)
end;

procedure TgxCustomGLSLSimpleErosionShader.SetNoiseTexTexture(const Value: TgxTexture);
begin
  if FNoiseTex = Value then Exit;
  FNoiseTex := Value;
  NotifyChange(Self);
end;

procedure TgxCustomGLSLSimpleErosionShader.SetErosionTexTexture(const Value: TgxTexture);
begin
  if FErosionTex = Value then Exit;
  FErosionTex := Value;
  NotifyChange(Self);
end;

function TgxCustomGLSLSimpleErosionShader.GetNoiseTexName: TgxLibMaterialName;
begin
  Result := TgxMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FNoiseTex);
  if Result = '' then Result := FNoiseTexName;
end;

procedure TgxCustomGLSLSimpleErosionShader.SetNoiseTexName(const Value: TgxLibMaterialName);
begin
  //Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FNoiseTexName = Value then Exit;
  FNoiseTexName  := Value;
  FNoiseTex := TgxMaterialLibrary(FMaterialLibrary).TextureByName(FNoiseTexName);
  NotifyChange(Self);
end;

function TgxCustomGLSLSimpleErosionShader.GetMainTexName: TgxLibMaterialName;
begin
  Result := TgxMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTex);
  if Result = '' then Result := FMainTexName;
end;

procedure TgxCustomGLSLSimpleErosionShader.SetMainTexName(const Value: TgxLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTex := TgxMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;

function TgxCustomGLSLSimpleErosionShader.GetErosionTexName: TgxLibMaterialName;
begin
  Result := TgxMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FErosionTex);
  if Result = '' then Result := FErosionTexName;
end;

procedure TgxCustomGLSLSimpleErosionShader.SetErosionTexName(const Value: TgxLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FErosionTexName = Value then Exit;
  FErosionTexName  := Value;

  FErosionTex := TgxMaterialLibrary(FMaterialLibrary).TextureByName(FErosionTexName);
  NotifyChange(Self);
end;

procedure TgxCustomGLSLSimpleErosionShader.SetAmbientColor(AValue: TgxColor);
begin
  FAmbientColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLSimpleErosionShader.SetSpecularColor(AValue: TgxColor);
begin
  FSpecularColor.DirectColor := AValue.Color;
end;


procedure TgxCustomGLSLSimpleErosionShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin
        // Need to nil the textures that were owned by it
        if FNoiseTex <> nil then
        begin
          Index := TgxMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FNoiseTex);
          if Index <> -1 then
            SetNoiseTexTexture(nil);
        end;

        if FMainTex <> nil then
        begin
          Index := TgxMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTex);
          if Index <> -1 then
            SetMainTexTexture(nil);
        end;

        if FErosionTex <> nil then
        begin
          Index := TgxMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FErosionTex);
          if Index <> -1 then
            SetErosionTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;

end.

