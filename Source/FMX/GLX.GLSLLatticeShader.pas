//
// Graphic Scene Engine, http://glscene.org
//
{
   Lattice shader that simulate Lattice. 
   At this time only one light source is supported
           
}


unit GLX.GLSLLatticeShader;

interface

{$I GLX.Scene.inc}

uses
  Winapi.OpenGL, 
  Winapi.OpenGLext,  
  System.Classes,
  
  GLX.Scene, 
  GLX.CrossPlatform, 
  GLX.BaseClasses, 
  GLX.State, 
  GLX.OpenGL1x, 
  GLX.Context, 
  GLX.RenderContextInfo, 
  GLX.VectorGeometry, 
  GLX.Coordinates,
  GLX.TextureFormat, 
  GLX.Color, 
  GLX.Texture, 
  GLX.Material,
  GLX.GLSLShader, 
  GLX.CustomShader;

{ Custom class for GLSLSimpleLatticeShader. 
 A shader that simulate Lattice }
type
  TgxCustomGLSLSimpleLatticeShader = class(TgxCustomGLSLShader)
  private
    FLatticeScale: TgxCoordinates2;
    FLatticeThreshold: TgxCoordinates2;
    procedure SetLatticeScale(const Value: TgxCoordinates2);
    procedure SetLatticeThreshold(const Value: TgxCoordinates2);
  protected
    procedure DoApply(var rci : TgxRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property LatticeScale: TgxCoordinates2 read FLatticeScale write SetLatticeScale;
    property LatticeThreshold: TgxCoordinates2 read FLatticeThreshold write SetLatticeThreshold;
  end;


//TgxCustomGLSLLatticeShader
//
{ Custom class for GLSLLatticeShader. 
 A shader that simulate Lattice with Diffuse/Specular and support Texture }
  TgxCustomGLSLLatticeShader = class(TgxCustomGLSLSimpleLatticeShader)
  private
    FAmbientColor: TgxColor;
    FDiffuseColor: TgxColor;
    FSpecularColor: TgxColor;

    FMaterialLibrary: TgxAbstractMaterialLibrary;
    FMainTexture: TgxTexture;
    FMainTexName   : TgxLibMaterialName;

    FSpecularPower: Single;
    FLightPower: Single;

    function GetMaterialLibrary: TgxAbstractMaterialLibrary;

    procedure SetMainTexTexture(const Value: TgxTexture);
    function GetMainTexName: TgxLibMaterialName;
    procedure SetMainTexName(const Value: TgxLibMaterialName);

    procedure SetDiffuseColor(AValue: TgxColor);
    procedure SetAmbientColor(AValue: TgxColor);
    procedure SetSpecularColor(AValue: TgxColor);

  protected
    procedure DoInitialize(var rci : TgxRenderContextInfo; Sender : TObject); override;
    procedure DoApply(var rci : TgxRenderContextInfo; Sender : TObject); override;

    procedure SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property DiffuseColor : TgxColor read FDiffuseColor Write setDiffuseColor;
    property SpecularColor : TgxColor Read FSpecularColor Write setSpecularColor;
    property AmbientColor : TgxColor Read FAmbientColor Write setAmbientColor;

    property MaterialLibrary: TgxAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property MainTexture: TgxTexture read FMainTexture write SetMainTexTexture;
    property MainTextureName: TgxLibMaterialName read GetMainTexName write SetMainTexName;

    property SpecularPower: Single read FSpecularPower write FSpecularPower;
    property LightPower: Single read FLightPower write FLightPower;

  end;

  TgxSLSimpleLatticeShader = class(TgxCustomGLSLSimpleLatticeShader)
  published
    property LatticeScale;
    property LatticeThreshold;
  end;

  TgxSLLatticeShader = class(TgxCustomGLSLLatticeShader)
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

implementation

{ TgxCustomGLSLSimpleLatticeShader }

constructor TgxCustomGLSLSimpleLatticeShader.Create(AOwner: TComponent);
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
  FLatticeScale := TgxCoordinates2.Create(Self);
  FLatticeThreshold := TgxCoordinates2.Create(Self);

  FLatticeScale.SetPoint2D(10, 40);
  FLatticeThreshold.SetPoint2D(0.15, 0.3);
end;

destructor TgxCustomGLSLSimpleLatticeShader.Destroy;
begin
  FLatticeScale.Destroy;
  FLatticeThreshold.Destroy;
  inherited;
end;

procedure TgxCustomGLSLSimpleLatticeShader.DoApply(var rci: TgxRenderContextInfo;Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  Param['Scale'].AsVector2f := FLatticeScale.AsPoint2D;
  Param['Threshold'].AsVector2f := FLatticeThreshold.AsPoint2D;
end;

function TgxCustomGLSLSimpleLatticeShader.DoUnApply(var rci: TgxRenderContextInfo): Boolean;
begin
  Result := False;
  //gl.ActiveTexture(GL_TEXTURE0_ARB);
  GetGLSLProg.EndUseProgramObject;
end;

procedure TgxCustomGLSLSimpleLatticeShader.SetLatticeScale(
  const Value: TgxCoordinates2);
begin
  FLatticeScale.Assign(Value);
end;

procedure TgxCustomGLSLSimpleLatticeShader.SetLatticeThreshold(
  const Value: TgxCoordinates2);
begin
  FLatticeThreshold.Assign(Value);
end;

{ TgxCustomGLSLLatticeShader }

constructor TgxCustomGLSLLatticeShader.Create(
  AOwner: TComponent);
begin
  inherited;
  FAmbientColor := TgxColor.Create(Self);
  FDiffuseColor := TgxColor.Create(Self);
  FSpecularColor := TgxColor.Create(Self);

  //setup initial parameters
  FAmbientColor.SetColor(0.15, 0.15, 0.15, 1);
  FDiffuseColor.SetColor(1, 1, 1, 1);
  FSpecularColor.SetColor(1, 1, 1, 1);

  FSpecularPower  := 8;  //6
  FLightPower     := 1;
end;

destructor TgxCustomGLSLLatticeShader.Destroy;
begin
  FAmbientColor.Destroy;
  FDiffuseColor.Destroy;
  FSpecularColor.Destroy;

  inherited;
end;

procedure TgxCustomGLSLLatticeShader.DoApply(var rci: TgxRenderContextInfo; Sender: TObject);
begin

  inherited;

  Param['AmbientColor'].AsVector4f := FAmbientColor.Color;
  Param['DiffuseColor'].AsVector4f := FDiffuseColor.Color;
  Param['SpecularColor'].AsVector4f := FSpecularColor.Color;

  Param['SpecPower'].AsVector1f := FSpecularPower;
  Param['LightIntensity'].AsVector1f := FLightPower;

  Param['MainTexture'].AsTexture2D[0] := FMainTexture;

end;

procedure TgxCustomGLSLLatticeShader.DoInitialize(var rci : TgxRenderContextInfo; Sender : TObject);
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


function TgxCustomGLSLLatticeShader.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TgxCustomGLSLLatticeShader.SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TgxAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TgxCustomGLSLLatticeShader.SetMainTexTexture(const Value: TgxTexture);
begin
  if FMainTexture = Value then Exit;
  FMainTexture := Value;
  NotifyChange(Self)
end;

function TgxCustomGLSLLatticeShader.GetMainTexName: TgxLibMaterialName;
begin
  Result := TgxMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTexture);
  if Result = '' then Result := FMainTexName;
end;

procedure TgxCustomGLSLLatticeShader.SetMainTexName(const Value: TgxLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTexture := TgxMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;


procedure TgxCustomGLSLLatticeShader.SetDiffuseColor(AValue: TgxColor);
begin
  FDiffuseColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLLatticeShader.SetAmbientColor(AValue: TgxColor);
begin
  FAmbientColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLLatticeShader.SetSpecularColor(AValue: TgxColor);
begin
  FSpecularColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLLatticeShader.Notification(AComponent: TComponent; Operation: TOperation);
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

