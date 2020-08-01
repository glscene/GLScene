//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLSL.ShaderLattice;

(*
   Lattice shader that simulate Lattice. 
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
  Scene.VectorGeometry,
  GLCoordinates,
  GLTextureFormat,
  GLColor,
  GLTexture,
  GLMaterial,
  GLSL.Shader,
  GLS.ShaderCustom;

(* Custom class for GLSLSimpleLatticeShader.
 A shader that simulate Lattice *)
type
  TGLCustomGLSLSimpleLatticeShader = class(TGLCustomGLSLShader)
  private
    FLatticeScale: TGLCoordinates2;
    FLatticeThreshold: TGLCoordinates2;
    procedure SetLatticeScale(const Value: TGLCoordinates2);
    procedure SetLatticeThreshold(const Value: TGLCoordinates2);
  protected
    procedure DoApply(var rci : TGLRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property LatticeScale: TGLCoordinates2 read FLatticeScale write SetLatticeScale;
    property LatticeThreshold: TGLCoordinates2 read FLatticeThreshold write SetLatticeThreshold;
  end;

(* Custom class for GLSLLatticeShader.
 A shader that simulate Lattice with Diffuse/Specular and support Texture *)
  TGLCustomGLSLLatticeShader = class(TGLCustomGLSLSimpleLatticeShader)
  private
    FAmbientColor: TGLColor;
    FDiffuseColor: TGLColor;
    FSpecularColor: TGLColor;
    FMaterialLibrary: TGLAbstractMaterialLibrary;
    FMainTexture: TGLTexture;
    FMainTexName   : TGLLibMaterialName;
    FSpecularPower: Single;
    FLightPower: Single;
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
    procedure SetMainTexTexture(const Value: TGLTexture);
    function GetMainTexName: TGLLibMaterialName;
    procedure SetMainTexName(const Value: TGLLibMaterialName);
    procedure SetDiffuseColor(AValue: TGLColor);
    procedure SetAmbientColor(AValue: TGLColor);
    procedure SetSpecularColor(AValue: TGLColor);
  protected
    procedure DoInitialize(var rci : TGLRenderContextInfo; Sender : TObject); override;
    procedure DoApply(var rci : TGLRenderContextInfo; Sender : TObject); override;
    procedure SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property DiffuseColor : TGLColor read FDiffuseColor Write setDiffuseColor;
    property SpecularColor : TGLColor Read FSpecularColor Write setSpecularColor;
    property AmbientColor : TGLColor Read FAmbientColor Write setAmbientColor;
    property MaterialLibrary: TGLAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property MainTexture: TGLTexture read FMainTexture write SetMainTexTexture;
    property MainTextureName: TGLLibMaterialName read GetMainTexName write SetMainTexName;
    property SpecularPower: Single read FSpecularPower write FSpecularPower;
    property LightPower: Single read FLightPower write FLightPower;
  end;

  TGLSLSimpleLatticeShader = class(TGLCustomGLSLSimpleLatticeShader)
  published
    property LatticeScale;
    property LatticeThreshold;
  end;

  TGLSLLatticeShader = class(TGLCustomGLSLLatticeShader)
  published
    property LatticeScale;
    property LatticeThreshold;
    property AmbientColor;
    property DiffuseColor;
    property SpecularColor;
    property MainTexture;
    property SpecularPower;
    property LightPower;
  end;

//------------------------------------------------------
implementation
//------------------------------------------------------

//------------------------------------------------------
// TGLCustomGLSLSimpleLatticeShader
//------------------------------------------------------

constructor TGLCustomGLSLSimpleLatticeShader.Create(AOwner: TComponent);
begin
  inherited;
  with FragmentProgram.Code do
  begin
    Clear;
    Add('  uniform vec2  Scale; ');
    Add('  uniform vec2  Threshold; ');
    Add(' ');
    Add('  void main (void) ');
    Add('{ ');
    Add('    float ss = fract(gl_TexCoord[0].s * Scale.s); ');
    Add('    float tt = fract(gl_TexCoord[0].t * Scale.t); ');
    Add(' ');
    Add('    if ((ss > Threshold.s) && (tt > Threshold.t)) discard; ');
    Add('    gl_FragColor = gl_Color;');
    Add('} ');
  end;

  // Initial stuff.
  FLatticeScale := TGLCoordinates2.Create(Self);
  FLatticeThreshold := TGLCoordinates2.Create(Self);

  FLatticeScale.SetPoint2D(10, 40);
  FLatticeThreshold.SetPoint2D(0.15, 0.3);
end;

destructor TGLCustomGLSLSimpleLatticeShader.Destroy;
begin
  FLatticeScale.Destroy;
  FLatticeThreshold.Destroy;
  inherited;
end;

procedure TGLCustomGLSLSimpleLatticeShader.DoApply(var rci: TGLRenderContextInfo;Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  Param['Scale'].AsVector2f := FLatticeScale.AsPoint2D;
  Param['Threshold'].AsVector2f := FLatticeThreshold.AsPoint2D;
end;

function TGLCustomGLSLSimpleLatticeShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  Result := False;
  //gl.ActiveTexture(GL_TEXTURE0_ARB);
  GetGLSLProg.EndUseProgramObject;
end;

procedure TGLCustomGLSLSimpleLatticeShader.SetLatticeScale(
  const Value: TGLCoordinates2);
begin
  FLatticeScale.Assign(Value);
end;

procedure TGLCustomGLSLSimpleLatticeShader.SetLatticeThreshold(
  const Value: TGLCoordinates2);
begin
  FLatticeThreshold.Assign(Value);
end;

{ TGLCustomGLSLLatticeShader }

constructor TGLCustomGLSLLatticeShader.Create(
  AOwner: TComponent);
begin
  inherited;
  FAmbientColor := TGLColor.Create(Self);
  FDiffuseColor := TGLColor.Create(Self);
  FSpecularColor := TGLColor.Create(Self);

  //setup initial parameters
  FAmbientColor.SetColor(0.15, 0.15, 0.15, 1);
  FDiffuseColor.SetColor(1, 1, 1, 1);
  FSpecularColor.SetColor(1, 1, 1, 1);

  FSpecularPower  := 8;  //6
  FLightPower     := 1;
end;

destructor TGLCustomGLSLLatticeShader.Destroy;
begin
  FAmbientColor.Destroy;
  FDiffuseColor.Destroy;
  FSpecularColor.Destroy;

  inherited;
end;

procedure TGLCustomGLSLLatticeShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
begin

  inherited;

  Param['AmbientColor'].AsVector4f := FAmbientColor.Color;
  Param['DiffuseColor'].AsVector4f := FDiffuseColor.Color;
  Param['SpecularColor'].AsVector4f := FSpecularColor.Color;

  Param['SpecPower'].AsVector1f := FSpecularPower;
  Param['LightIntensity'].AsVector1f := FLightPower;

  Param['MainTexture'].AsTexture2D[0] := FMainTexture;

end;

procedure TGLCustomGLSLLatticeShader.DoInitialize(var rci : TGLRenderContextInfo; Sender : TObject);
begin
  with VertexProgram.Code do
  begin
    Clear;
    Add('varying vec3 Normal; ');
    Add('varying vec3 LightVector; ');
    Add('varying vec3 CameraVector; ');
    Add('varying vec2 Texcoord; ');
    Add(' ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add('  gl_Position = ftransform(); ');
    Add('  Texcoord = gl_MultiTexCoord0.xy; ');
    Add('  Normal = normalize(gl_NormalMatrix * gl_Normal); ');
    Add('  vec3 p = (gl_ModelViewMatrix * gl_Vertex).xyz; ');
    Add('  LightVector = normalize(gl_LightSource[0].position.xyz - p); ');
    Add('  CameraVector = normalize(p); ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    Clear;
    Add('  uniform vec2  Scale; ');
    Add('  uniform vec2  Threshold; ');
    Add(' ');
    Add('uniform vec4 AmbientColor; ');
    Add('uniform vec4 DiffuseColor; ');
    Add('uniform vec4 SpecularColor; ');
    Add(' ');
    Add('uniform float LightIntensity; ');
    Add('uniform float SpecPower; ');
    Add('uniform sampler2D MainTexture; ');
    Add(' ');
    Add('varying vec3 Normal; ');
    Add('varying vec3 LightVector; ');
    Add('varying vec3 CameraVector; ');
    Add('varying vec2 Texcoord; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add('    float ss = fract(Texcoord[0] * Scale.s); ');
    Add('    float tt = fract(Texcoord[1] * Scale.t); ');
    Add(' ');
    Add('    if ((ss > Threshold.s) && (tt > Threshold.t)) discard; ');
    Add(' ');
    Add('  vec4 TextureContrib = texture2D(MainTexture, Texcoord); ');
    Add('  vec4 DiffuseContrib = clamp(DiffuseColor * dot(LightVector, Normal), 0.0, 1.0); ');
    Add(' ');
    Add('  vec3 reflect_vec = reflect(CameraVector, -Normal); ');
    Add('  float Temp = dot(reflect_vec, LightVector); ');
    Add('  vec4 SpecContrib = SpecularColor * clamp(pow(Temp, SpecPower), 0.0, 0.95); ');
    Add(' ');
    Add('  gl_FragColor = TextureContrib * LightIntensity * (AmbientColor + DiffuseContrib) + LightIntensity * SpecContrib; ');
    Add('} ');
  end;
  inherited;
end;


function TGLCustomGLSLLatticeShader.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLCustomGLSLLatticeShader.SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TGLAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TGLCustomGLSLLatticeShader.SetMainTexTexture(const Value: TGLTexture);
begin
  if FMainTexture = Value then Exit;
  FMainTexture := Value;
  NotifyChange(Self)
end;

function TGLCustomGLSLLatticeShader.GetMainTexName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTexture);
  if Result = '' then Result := FMainTexName;
end;

procedure TGLCustomGLSLLatticeShader.SetMainTexName(const Value: TGLLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTexture := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;


procedure TGLCustomGLSLLatticeShader.SetDiffuseColor(AValue: TGLColor);
begin
  FDiffuseColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLLatticeShader.SetAmbientColor(AValue: TGLColor);
begin
  FAmbientColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLLatticeShader.SetSpecularColor(AValue: TGLColor);
begin
  FSpecularColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLLatticeShader.Notification(AComponent: TComponent; Operation: TOperation);
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

