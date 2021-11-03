//
// The graphics rendering engine GLScene http://glscene.org
//
unit Cg.BombShader;

(*  Just a good looking shader *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  GLS.Texture,
  GLS.Cadencer,
  GLS.Context,
  GLS.Strings,
  GLS.Material,
  GLS.RenderContextInfo,
  GLS.TextureFormat,
  GLS.State,

  Cg.GL,
  Cg.Shader;

type
  ECgBombShaderException = class(ECgShaderException);
  TGLCgBombShaderTextureSource = (stsPrimaryTexture, stsSecondadyTexture,
                                  stsThirdTexture, stsUserSelectedTexture);

  // Just a good-looking shader.
  TCgCustomBombShader = class(TCadencableCustomCgShader, IGLMaterialLibrarySupported)
  private
    FMaterialLibrary: TGLAbstractMaterialLibrary;
    FGradientTexture: TGLTexture;
    FMainTexture:     TGLTexture;
    FMainTextureName:     TGLLibMaterialName;
    FGradientTextureName: TGLLibMaterialName;
    FSharpness:  Single;
    FColorRange: Single;
    FSpeed:      Single;
    FDisplacement: Single;
    FAlpha:      Single;
    FTurbDensity: Single;
    FColorSharpness: Single;
    FGradientTextureShare: Single;
    FMainTextureShare: Single;
{$IFNDEF USE_OPTIMIZATIONS}
    FMainTextureSource: TGLCgBombShaderTextureSource;
{$ENDIF}
    procedure SetGradientTexture(const Value: TGLTexture);
    procedure SetMainTexture(const Value: TGLTexture);
    function GetMainTextureName: TGLLibMaterialName;
    procedure SetMainTextureName(const Value: TGLLibMaterialName);
    function GetGradientTextureName: TGLLibMaterialName;
    procedure SetGradientTextureName(const Value: TGLLibMaterialName);
    function StoreColorRange: Boolean;
    function StoreColorSharpness: Boolean;
    function StoreDisplacement: Boolean;
    function StoreGradientTextureShare: Boolean;
    function StoreSharpness: Boolean;
    function StoreSpeed: Boolean;
    function StoreTurbDensity: Boolean;
    function StoreMainTextureShare: Boolean;
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
  protected
    procedure DoInitialize(var rci : TGLRenderContextInfo; Sender : TObject); override;
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    procedure OnApplyVP(CgProgram: TCgProgram; Sender: TObject); virtual;
    procedure OnApplyFP(CgProgram: TCgProgram; Sender: TObject); virtual;
    procedure OnUnApplyFP(CgProgram: TCgProgram); virtual;
    procedure SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    property MainTexture: TGLTexture read FMainTexture write SetMainTexture;
    property MainTextureName: TGLLibMaterialName read GetMainTextureName write SetMainTextureName;
    property GradientTexture: TGLTexture read FGradientTexture write SetGradientTexture;
    property GradientTextureName: TGLLibMaterialName read GetGradientTextureName write SetGradientTextureName;
    property GradientTextureShare: Single read FGradientTextureShare write FGradientTextureShare stored StoreGradientTextureShare;
    property MainTextureShare: Single read FMainTextureShare write FMainTextureShare stored StoreMainTextureShare;
    property Alpha: Single read FAlpha write FAlpha;
    property Displacement: Single read FDisplacement write FDisplacement stored StoreDisplacement;
    property Sharpness: Single read FSharpness write FSharpness stored StoreSharpness;
    property ColorSharpness: Single read FColorSharpness write FColorSharpness stored StoreColorSharpness;
    property Speed: Single read FSpeed write FSpeed stored StoreSpeed;
    property TurbDensity: Single read FTurbDensity write FTurbDensity stored StoreTurbDensity;
    property ColorRange: Single read FColorRange write FColorRange stored StoreColorRange;
{$IFNDEF USE_OPTIMIZATIONS}
    property MainTextureSource: TGLCgBombShaderTextureSource read FMainTextureSource write FMainTextureSource;
{$ENDIF}
    property MaterialLibrary: TGLAbstractMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
  end;

  TCgBombShader = class(TCgCustomBombShader)
  protected
    procedure DoInitialize(var rci : TGLRenderContextInfo; Sender : TObject); override;
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    procedure OnApplyVP(CgProgram: TCgProgram; Sender: TObject); override;
    procedure OnApplyFP(CgProgram: TCgProgram; Sender: TObject); override;
    procedure OnUnApplyFP(CgProgram: TCgProgram); override;
  published
    property MainTextureShare;
    property MainTextureName;
    property GradientTextureShare;
    property GradientTextureName;
    property Alpha;
    property Cadencer;
    property Displacement;
    property Sharpness;
    property ColorSharpness;
    property Speed;
    property TurbDensity;
    property ColorRange;
    property MaterialLibrary;
    property DesignEnable;
  end;

//=============================================================
implementation
//=============================================================

const
  EPS = 0.001;

//--------------------------------
// TGLCustomCGBombShader
//--------------------------------

constructor TCgCustomBombShader.Create(AOwner: TComponent);
begin
  inherited;
  VertexProgram.OnApply := OnApplyVP;
  VertexProgram.ManualNotification := True;
  FragmentProgram.OnApply := OnApplyFP;
  FragmentProgram.OnUnApply := OnUnApplyFP;
  FragmentProgram.ManualNotification := True;

  FAlpha := 0.7;
  FDisplacement := 0.3;
  FSharpness := 3;
  FColorSharpness := 1;
  FSpeed := 0.3;
  FTurbDensity := 1;
  FColorRange := 0.24;
  FGradientTextureShare := 0.7;
  FMainTextureShare := 0.7;
{$IFNDEF USE_OPTIMIZATIONS}
  FMainTextureSource := stsUserSelectedTexture;
{$ENDIF}
end;


procedure TCgCustomBombShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
begin
  VertexProgram.Apply(rci, Sender);
  FragmentProgram.Apply(rci, Sender);
{$IFDEF USE_OPTIMIZATIONS}
  if FMainTexture <> nil then
    FragmentProgram.ParamByName('MainTextureSampler').SetAsTexture2D(FMainTexture.Handle);
{$ELSE}
  case FMainTextureSource of
    stsPrimaryTexture: FragmentProgram.ParamByName('MainTextureSampler').SetAsTexture2D(rci.GLStates.TextureBinding[0, ttTexture2D]);
    stsSecondadyTexture: FragmentProgram.ParamByName('MainTextureSampler').SetAsTexture2D(rci.GLStates.TextureBinding[1, ttTexture2D]);
    stsThirdTexture: FragmentProgram.ParamByName('MainTextureSampler').SetAsTexture2D(rci.GLStates.TextureBinding[2, ttTexture2D]);
    stsUserSelectedTexture:
      begin
        if FMainTexture <> nil then
          FragmentProgram.ParamByName('MainTextureSampler').SetAsTexture2D(FMainTexture.Handle);
      end;
  end;
{$ENDIF}
end;


procedure TCgCustomBombShader.DoInitialize(var rci : TGLRenderContextInfo; Sender : TObject);
begin
  if FGradientTexture = nil then
  try
    FGradientTexture := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FGradientTextureName);
  except
    Enabled := False;
    raise;
  end;
  if FMainTexture = nil then
  try
    FMainTexture := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FMainTextureName);
  except end;

  with VertexProgram.Code do
  begin
    Clear;
    Add(' ');
    Add('//in ');
    Add('struct appData ');
    Add('{ ');
    Add('    float4 Position     : POSITION; ');
    Add('    float4 Normal       : NORMAL; ');
    Add('    float4 TexCoord0    : TEXCOORD0; ');
    Add('}; ');
    Add(' ');
    Add('// out ');
    Add('struct vertexOutData ');
    Add('{ ');
    Add('    float4 HPosition	: POSITION; ');
    Add('    float4 Color0	: COLOR0; ');
    Add('    float4 TexCoord0    : TEXCOORD0; ');
    Add('}; ');
    Add(' ');
    Add(' ');
    Add(' ');
    Add('vertexOutData main( ');
    Add('					appData IN, ');
    Add('                    uniform float4x4 WorldViewProj, ');
    Add('                    const float4x4 NoiseMatrix, ');
    Add('                    uniform float Timer, ');
    Add('                    uniform float Displacement, ');
    Add('                    uniform float Sharpness, ');
    Add('                    uniform float ColorSharpness , ');
    Add('                    uniform float Speed, ');
    Add('                    uniform float TurbDensity, ');
    Add('                    uniform float ColorRange ');
    Add('                   ) ');
    Add('{ ');
    Add('    vertexOutData OUT; ');
    Add('    OUT.TexCoord0 = IN.TexCoord0; ');
    Add('    float4 noisePos = TurbDensity * mul(IN.Position + (Speed * Timer), NoiseMatrix); ');
    Add('    float i = sin(noisePos.x + noisePos.y + noisePos.z + tan(noisePos.x + noisePos.y + noisePos.z)/100000 ); ');
    Add('    float cr = 0.5 + ColorRange * i; ');
    Add('    cr = pow(cr,ColorSharpness); ');
    Add('    OUT.Color0 = float4((cr).xxx, 1.0f); ');
    Add('    // Displacement along normal ');
    Add('    float ni = pow(abs(i), Sharpness); ');
    Add('    float4 Nn = float4(normalize(IN.Position).xyz,0); ');
    Add('    float4 NewPos = IN.Position - (Nn * (ni - 0.5) * Displacement) * 10; ');
    Add('     OUT.HPosition = mul(WorldViewProj, NewPos); ');
    Add('    return OUT; ');
    Add('} ');
  end;


  with FragmentProgram.Code do
  begin
    Clear;
    Add('struct vertexOutData ');
    Add('{ ');
    Add('    float4 Color0	: COLOR0; ');
    Add('    float4 TexCoord0    : TEXCOORD0; ');
    Add('}; ');
    Add(' ');
    Add('float4 main( ');
    Add('            vertexOutData IN, ');
    Add('            uniform sampler2D GradeSampler, ');
    Add('            uniform float GradientTextureShare, ');
    if FMainTexture <> nil then
    begin
      Add('            uniform sampler2D MainTextureSampler, ');
      Add('            uniform float MainTextureShare, ');
    end;
    Add('            uniform float Alpha ');
    Add('            ): COLOR ');
    Add('{ ');
    Add('	   float4 GradeColor = tex2D(GradeSampler, float2(IN.Color0.x, IN.Color0.y)); ');
    if FMainTexture <> nil then
      Add('    float4 TextureColor = tex2D(MainTextureSampler, IN.TexCoord0.xy); ');
    Add('    ');
    if FMainTexture <> nil then
      Add('    TextureColor = TextureColor * MainTextureShare + GradeColor * GradientTextureShare; ')
    else
      Add('    float4 TextureColor = GradeColor * GradientTextureShare; ');
    Add('    TextureColor.w = Alpha; ');
    Add('	   return TextureColor;');
    Add('} ');
  end;

  inherited DoInitialize(rci, Sender);

  // May be there was an error and shader disabled itself.
  if Enabled then
  begin
    Assert(FGradientTexture <> nil);
    VertexProgram.ParamByName('NoiseMatrix').SetAsStateMatrix(CG_GL_TEXTURE_MATRIX, CG_GL_MATRIX_IDENTITY);
    FragmentProgram.ParamByName('GradeSampler').SetAsTexture2D(FGradientTexture.Handle);
  end;
end;


function TCgCustomBombShader.GetGradientTextureName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FGradientTexture);
  if Result = '' then Result := FGradientTextureName;
end;

function TCgCustomBombShader.GetMainTextureName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTexture);
  if Result = '' then Result := FMainTextureName;
end;

function TCgCustomBombShader.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TCgCustomBombShader.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin
        // Need to nil the textures that were owned by it
        if FMainTexture <> nil then
        begin
          Index := TGLMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            SetMainTexture(nil);
        end;

        if FGradientTexture <> nil then
        begin
          Index := TGLMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FGradientTexture);
          if Index <> -1 then
            SetGradientTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;

procedure TCgCustomBombShader.OnApplyFP(CgProgram: TCgProgram; Sender: TObject);
begin
  CgProgram.ParamByName('Alpha').SetAsScalar(FAlpha);
  CgProgram.ParamByName('GradientTextureShare').SetAsScalar(FGradientTextureShare);
  CgProgram.ParamByName('GradeSampler').EnableTexture;
  if FMainTexture <> nil then
  begin
    CgProgram.ParamByName('MainTextureShare').SetAsScalar(FMainTextureShare);
    CgProgram.ParamByName('MainTextureSampler').EnableTexture;
  end;
end;


procedure TCgCustomBombShader.OnApplyVP(CgProgram: TCgProgram; Sender: TObject);
begin
  CgProgram.ParamByName('WorldViewProj').SetAsStateMatrix(CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);
  CgProgram.ParamByName('Timer').SetAsScalar(Cadencer.CurrentTime);
  CgProgram.ParamByName('Displacement').SetAsScalar(FDisplacement);
  CgProgram.ParamByName('Sharpness').SetAsScalar(FSharpness);
  CgProgram.ParamByName('ColorSharpness').SetAsScalar(FColorSharpness);
  CgProgram.ParamByName('Speed').SetAsScalar(FSpeed);
  CgProgram.ParamByName('TurbDensity').SetAsScalar(FTurbDensity);
  CgProgram.ParamByName('ColorRange').SetAsScalar(FColorRange);
end;


procedure TCgCustomBombShader.OnUnApplyFP(CgProgram: TCgProgram);
begin
  CgProgram.ParamByName('GradeSampler').DisableTexture;
  if FMainTexture <> nil then
    CgProgram.ParamByName('MainTextureSampler').DisableTexture;
end;

procedure TCgCustomBombShader.SetGradientTexture(const Value: TGLTexture);
begin
  if FGradientTexture = Value then Exit;
  FGradientTexture := Value;
  NotifyChange(Self);
end;

procedure TCgCustomBombShader.SetGradientTextureName(
  const Value: TGLLibMaterialName);
begin
  FGradientTextureName := Value;
  if ShaderInitialized then
    NotifyChange(Self);
end;

procedure TCgCustomBombShader.SetMainTexture(
  const Value: TGLTexture);
begin
  if FMainTexture = Value then Exit;
  FMainTexture := Value;
  NotifyChange(Self);
end;

procedure TCgCustomBombShader.SetMainTextureName(
  const Value: TGLLibMaterialName);
begin
  FMainTextureName := Value;
  if ShaderInitialized then
    NotifyChange(Self);
end;

procedure TCgCustomBombShader.SetMaterialLibrary(
  const Value: TGLAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TGLAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

function TCgCustomBombShader.StoreColorRange: Boolean;
begin
  Result := Abs(FColorRange - 0.24) > EPS;
end;

function TCgCustomBombShader.StoreColorSharpness: Boolean;
begin
  Result := Abs(FColorSharpness - 1) > EPS;
end;

function TCgCustomBombShader.StoreDisplacement: Boolean;
begin
  Result := Abs(FDisplacement - 0.3) > EPS;
end;

function TCgCustomBombShader.StoreGradientTextureShare: Boolean;
begin
  Result := Abs(FGradientTextureShare - 0.7) > EPS;
end;

function TCgCustomBombShader.StoreMainTextureShare: Boolean;
begin
  Result := Abs(FMainTextureShare - 0.7) > EPS;
end;

function TCgCustomBombShader.StoreSharpness: Boolean;
begin
  Result := Abs(FSharpness - 3) > EPS;
end;

function TCgCustomBombShader.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 0.3) > EPS;
end;

function TCgCustomBombShader.StoreTurbDensity: Boolean;
begin
  Result := Abs(FTurbDensity - 1) > EPS;
end;

//--------------------------------
// TGLCgBombShader
//--------------------------------

procedure TCgBombShader.DoApply(var rci: TGLRenderContextInfo;
  Sender: TObject);
begin
{$IFNDEF USE_OPTIMIZATIONS}
  if (not (csDesigning in ComponentState)) or DesignEnable then
    inherited;
{$ENDIF}
end;

procedure TCgBombShader.DoInitialize(var rci : TGLRenderContextInfo; Sender : TObject);
begin
{$IFNDEF USE_OPTIMIZATIONS}
  if (not (csDesigning in ComponentState)) or DesignEnable then
    inherited;
{$ENDIF}
end;

procedure TCgBombShader.OnApplyFP(CgProgram: TCgProgram;
  Sender: TObject);
begin
{$IFNDEF USE_OPTIMIZATIONS}
  if (not (csDesigning in ComponentState)) or DesignEnable then
    inherited;
{$ENDIF}
end;

procedure TCgBombShader.OnApplyVP(CgProgram: TCgProgram;
  Sender: TObject);
begin
{$IFNDEF USE_OPTIMIZATIONS}
  if (not (csDesigning in ComponentState)) or DesignEnable then
    inherited;
{$ENDIF}
end;

procedure TCgBombShader.OnUnApplyFP(CgProgram: TCgProgram);
begin
{$IFNDEF USE_OPTIMIZATIONS}
  if (not (csDesigning in ComponentState)) or DesignEnable then
    inherited;
{$ENDIF}
end;

initialization
  RegisterClasses([TCgCustomBombShader, TCgBombShader]);

end.

