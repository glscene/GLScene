// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLSLSemShader;

(*
   SEM shader : Spherical Environment Mapping
   The main idea of SEM is to get the UV coordinates (which are used to lookup the matCap texture)
   from the normal vector on the fragment instead of the original texture coordinates from the object. 
    
   A material using SEM is very useful to highlight variations in the mesh: creases, bumps, even slow ondulations.
   It doesn't work that well on a cube, for instance. And does absolutely nothing on a sphere:
   SEM on a sphere is exactly the same as a planar projection of the matCap texture. 
   At this time only one light source is supported
*)

interface

{$I GLScene.inc}

uses
  System.Classes,
  
  OpenGLTokens,
  GLScene,
  GLBaseClasses,
  GLState,
  GLContext,
  GLRenderContextInfo,
  GLVectorGeometry,
  GLCoordinates,
  GLTextureFormat,
  GLColor,
  GLTexture,
  GLMaterial,
  GLSLShader,
  GLCustomShader;

Type

  {Custom class for GLSLSEMShader. 
   SEM Shader : Spherical Environment Mapping }
  TGLCustomGLSLSemShader = class(TGLCustomGLSLShader)
  private
    FAmbientColor: TGLColor;
//    FDiffuseColor: TGLColor;
    FSpecularColor: TGLColor;
    FAmbientFactor : Single;
    FDiffuseFactor : Single;
    FSpecularFactor : Single;
    FMaterialLibrary: TGLAbstractMaterialLibrary;
    FMainTexture: TGLTexture;
    FMainTexName   : TGLLibMaterialName;
//    FSpecularPower: Single;
//    FLightPower: Single;
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
    procedure SetMainTexTexture(const Value: TGLTexture);
    function GetMainTexName: TGLLibMaterialName;
    procedure SetMainTexName(const Value: TGLLibMaterialName);
    //procedure SetDiffuseColor(AValue: TGLColor);
    procedure SetAmbientColor(AValue: TGLColor);
    procedure SetSpecularColor(AValue: TGLColor);
  protected
    procedure DoApply(var rci : TGLRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
    procedure SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
//    property DiffuseColor : TGLColor read FDiffuseColor Write setDiffuseColor;
    property SpecularColor : TGLColor Read FSpecularColor Write setSpecularColor;
    property AmbientColor : TGLColor Read FAmbientColor Write setAmbientColor;
    property AmbientFactor : Single Read FAmbientFactor Write FAmbientFactor;
    property DiffuseFactor : Single Read FDiffuseFactor Write FDiffuseFactor;
    property SpecularFactor : Single Read FSpecularFactor Write FSpecularFactor;
    property MaterialLibrary: TGLAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property MainTexture: TGLTexture read FMainTexture write SetMainTexTexture;
    property MainTextureName: TGLLibMaterialName read GetMainTexName write SetMainTexName;
//    property SpecularPower: Single read FSpecularPower write FSpecularPower;
//    property LightPower: Single read FLightPower write FLightPower;
  end;

  TGLSLSemShader = class(TGLCustomGLSLSemShader)
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
  end;
  
//====================================================  
implementation
//====================================================  

constructor TGLCustomGLSLSemShader.Create(AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    clear;
    Add('varying vec3 viewVec; ');
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');

    Add('void main() { ');
    Add('  vec4 p = gl_ModelViewMatrix * gl_Vertex; ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');
    Add('  lightVec = vec3(lightPos - p); ');
    Add('  viewVec = -vec3(p); ');
    Add('  normal = normalize(gl_NormalMatrix * gl_Normal ); ');

    Add('  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; ');
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

    Add('void main() { ');
    Add('  vec3 V = normalize(viewVec); ');
    Add('  vec3 r = reflect( V, normal ); ');
    Add('  float m = 2.0 * sqrt( pow( r.x, 2.0 ) + pow( r.y, 2.0 ) + pow( r.z + 1.0, 2.0 ) ); ');
    Add('  vec2 vN = r.xy / m + 0.5; ');
    Add('  vec4 DiffuseColor;    ');
    Add('  DiffuseColor = texture2D( MainTexture, vN ); //.rgb; ');

    // Simple Lighting
    Add('  vec3 L = normalize(lightVec); ');

    Add('  vec3 halfAngle = normalize(L + V); ');
    Add('  float NdotL = dot(L, normal); ');
    Add('  float NdotH = clamp(dot(halfAngle, normal), 0.0, 1.0); ');
    Add('  // "Half-Lambert" technique for more pleasing diffuse term ');
    Add('  float diffuse = DiffuseColor*(0.5 * NdotL + 0.5); ');
    Add('  float specular = pow(NdotH, 64.0); ');

    Add('  vec4 FinalColour = AmbientColor*AmbientIntensity + ');
    Add('                     DiffuseColor*diffuse*DiffuseIntensity + ');
    Add('                     SpecularColor*specular*SpecularIntensity; ');

    Add('  gl_FragColor = FinalColour; //vec4( FinalColour, 1.0 ); ');
    Add('} ');
  end;

  FAmbientColor := TGLColor.Create(Self);
  //FDiffuseColor := TGLColor.Create(Self);
  FSpecularColor := TGLColor.Create(Self);

  //setup initial parameters
  FAmbientColor.SetColor(0.15, 0.15, 0.15, 1.0);
  //FDiffuseColor.SetColor(1, 1, 1, 1);
  FSpecularColor.SetColor(1.0, 1.0, 1.0, 1.0);
  FAmbientFactor  := 0.8;
  FDiffuseFactor  :=0.9;
  FSpecularFactor :=0.8;
end;

destructor TGLCustomGLSLSemShader.Destroy;
begin
  FAmbientColor.Destroy;
 // FDiffuseColor.Destroy;
  FSpecularColor.Destroy;

  inherited;
end;

procedure TGLCustomGLSLSemShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
begin

  GetGLSLProg.UseProgramObject;
  //Param['DiffuseColor'].AsVector4f := FDiffuseColor.Color;
  param['AmbientColor'].AsVector4f := FAmbientColor.Color;
  param['SpecularColor'].AsVector4f := FSpecularColor.Color;
  param['AmbientIntensity'].AsVector1f := FAmbientFactor;
  param['DiffuseIntensity'].AsVector1f := FDiffuseFactor;
  param['SpecularIntensity'].AsVector1f := FSpecularFactor;

//  Param['SpecPower'].AsVector1f := FSpecularPower;
//  Param['LightIntensity'].AsVector1f := FLightPower;

  Param['MainTexture'].AsTexture2D[0] := FMainTexture;

end;

function TGLCustomGLSLSemShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  gl.ActiveTexture(GL_TEXTURE0_ARB);
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;


function TGLCustomGLSLSemShader.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLCustomGLSLSemShader.SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TGLAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TGLCustomGLSLSemShader.SetMainTexTexture(const Value: TGLTexture);
begin
  if FMainTexture = Value then Exit;
  FMainTexture := Value;
  NotifyChange(Self)
end;

function TGLCustomGLSLSemShader.GetMainTexName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTexture);
  if Result = '' then Result := FMainTexName;
end;

procedure TGLCustomGLSLSemShader.SetMainTexName(const Value: TGLLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTexture := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;


//procedure TGLCustomGLSLSemShader.SetDiffuseColor(AValue: TGLColor);
//begin
//  FDiffuseColor.DirectColor := AValue.Color;
//end;

procedure TGLCustomGLSLSemShader.SetAmbientColor(AValue: TGLColor);
begin
  FAmbientColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLSemShader.SetSpecularColor(AValue: TGLColor);
begin
  FSpecularColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLSemShader.Notification(AComponent: TComponent; Operation: TOperation);
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
          Index := TGLMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            SetMainTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;


end.
