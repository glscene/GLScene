//
// The graphics rendering engine GLScene http://glscene.org
//
unit CGx.BombShader;

(*  Just a good looking shader *)

interface

{$I Scena.inc}

uses
  System.Classes,
  System.SysUtils,

  GLX.Texture,
  GLX.Cadencer,
  GLX.Context,
  Scena.Strings,
  GLX.Material,
  GLX.RenderContextInfo,
  Scena.TextureFormat,
  GLX.State,

  CG.GL,
  CGx.Shader;

type
  ECGxBombShaderException = class(ECGxShaderException);
  TCGxBombShaderTextureSource = (stsPrimaryTexture, stsSecondadyTexture,
                                  stsThirdTexture, stsUserSelectedTexture);

  // Just a good-looking shader.
  TCGxCustomBombShader = class(TCGxCadencableCustomShader, IgxMaterialLibrarySupported)
  private
    FMaterialLibrary: TgxAbstractMaterialLibrary;
    FGradientTexture: TgxTexture;
    FMainTexture:     TgxTexture;
    FMainTextureName:     TgxLibMaterialName;
    FGradientTextureName: TgxLibMaterialName;
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
    FMainTextureSource: TCGxBombShaderTextureSource;
{$ENDIF}
    procedure SetGradientTexture(const Value: TgxTexture);
    procedure SetMainTexture(const Value: TgxTexture);
    function GetMainTextureName: TgxLibMaterialName;
    procedure SetMainTextureName(const Value: TgxLibMaterialName);
    function GetGradientTextureName: TgxLibMaterialName;
    procedure SetGradientTextureName(const Value: TgxLibMaterialName);
    function StoreColorRange: Boolean;
    function StoreColorSharpness: Boolean;
    function StoreDisplacement: Boolean;
    function StoreGradientTextureShare: Boolean;
    function StoreSharpness: Boolean;
    function StoreSpeed: Boolean;
    function StoreTurbDensity: Boolean;
    function StoreMainTextureShare: Boolean;
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TgxAbstractMaterialLibrary;
  protected
    procedure DoInitialize(var rci : TgxRenderContextInfo; Sender : TObject); override;
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    procedure OnApplyVP(CgProgram: TCGxProgram; Sender: TObject); virtual;
    procedure OnApplyFP(CgProgram: TCGxProgram; Sender: TObject); virtual;
    procedure OnUnApplyFP(CgProgram: TCGxProgram); virtual;
    procedure SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    property MainTexture: TgxTexture read FMainTexture write SetMainTexture;
    property MainTextureName: TgxLibMaterialName read GetMainTextureName write SetMainTextureName;
    property GradientTexture: TgxTexture read FGradientTexture write SetGradientTexture;
    property GradientTextureName: TgxLibMaterialName read GetGradientTextureName write SetGradientTextureName;
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
    property MainTextureSource: TCGxBombShaderTextureSource read FMainTextureSource write FMainTextureSource;
{$ENDIF}
    property MaterialLibrary: TgxAbstractMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
  end;

  TCGxBombShader = class(TCGxCustomBombShader)
  protected
    procedure DoInitialize(var rci : TgxRenderContextInfo; Sender : TObject); override;
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    procedure OnApplyVP(CgProgram: TCGxProgram; Sender: TObject); override;
    procedure OnApplyFP(CgProgram: TCGxProgram; Sender: TObject); override;
    procedure OnUnApplyFP(CgProgram: TCGxProgram); override;
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
// TCGxCustomBombShader
//--------------------------------

constructor TCGxCustomBombShader.Create(AOwner: TComponent);
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


procedure TCGxCustomBombShader.DoApply(var rci: TgxRenderContextInfo; Sender: TObject);
begin
  VertexProgram.Apply(rci, Sender);
  FragmentProgram.Apply(rci, Sender);
{$IFDEF USE_OPTIMIZATIONS}
  if FMainTexture <> nil then
    FragmentProgram.ParamByName('MainTextureSampler').SetAsTexture2D(FMainTexture.Handle);
{$ELSE}
  case FMainTextureSource of
    stsPrimaryTexture: FragmentProgram.ParamByName('MainTextureSampler').SetAsTexture2D(rci.gxStates.TextureBinding[0, ttTexture2D]);
    stsSecondadyTexture: FragmentProgram.ParamByName('MainTextureSampler').SetAsTexture2D(rci.gxStates.TextureBinding[1, ttTexture2D]);
    stsThirdTexture: FragmentProgram.ParamByName('MainTextureSampler').SetAsTexture2D(rci.gxStates.TextureBinding[2, ttTexture2D]);
    stsUserSelectedTexture:
      begin
        if FMainTexture <> nil then
          FragmentProgram.ParamByName('MainTextureSampler').SetAsTexture2D(FMainTexture.Handle);
      end;
  end;
{$ENDIF}
end;


procedure TCGxCustomBombShader.DoInitialize(var rci : TgxRenderContextInfo; Sender : TObject);
begin
  if FGradientTexture = nil then
  try
    FGradientTexture := TgxMaterialLibrary(FMaterialLibrary).TextureByName(FGradientTextureName);
  except
    Enabled := False;
    raise;
  end;
  if FMainTexture = nil then
  try
    FMainTexture := TgxMaterialLibrary(FMaterialLibrary).TextureByName(FMainTextureName);
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


function TCGxCustomBombShader.GetGradientTextureName: TgxLibMaterialName;
begin
  Result := TgxMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FGradientTexture);
  if Result = '' then Result := FGradientTextureName;
end;

function TCGxCustomBombShader.GetMainTextureName: TgxLibMaterialName;
begin
  Result := TgxMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTexture);
  if Result = '' then Result := FMainTextureName;
end;

function TCGxCustomBombShader.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TCGxCustomBombShader.Notification(AComponent: TComponent;
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
          Index := TgxMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            SetMainTexture(nil);
        end;

        if FGradientTexture <> nil then
        begin
          Index := TgxMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FGradientTexture);
          if Index <> -1 then
            SetGradientTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;

procedure TCGxCustomBombShader.OnApplyFP(CgProgram: TCGxProgram; Sender: TObject);
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


procedure TCGxCustomBombShader.OnApplyVP(CgProgram: TCGxProgram; Sender: TObject);
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


procedure TCGxCustomBombShader.OnUnApplyFP(CgProgram: TCGxProgram);
begin
  CgProgram.ParamByName('GradeSampler').DisableTexture;
  if FMainTexture <> nil then
    CgProgram.ParamByName('MainTextureSampler').DisableTexture;
end;

procedure TCGxCustomBombShader.SetGradientTexture(const Value: TgxTexture);
begin
  if FGradientTexture = Value then Exit;
  FGradientTexture := Value;
  NotifyChange(Self);
end;

procedure TCGxCustomBombShader.SetGradientTextureName(
  const Value: TgxLibMaterialName);
begin
  FGradientTextureName := Value;
  if ShaderInitialized then
    NotifyChange(Self);
end;

procedure TCGxCustomBombShader.SetMainTexture(
  const Value: TgxTexture);
begin
  if FMainTexture = Value then Exit;
  FMainTexture := Value;
  NotifyChange(Self);
end;

procedure TCGxCustomBombShader.SetMainTextureName(
  const Value: TgxLibMaterialName);
begin
  FMainTextureName := Value;
  if ShaderInitialized then
    NotifyChange(Self);
end;

procedure TCGxCustomBombShader.SetMaterialLibrary(
  const Value: TgxAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TgxAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

function TCGxCustomBombShader.StoreColorRange: Boolean;
begin
  Result := Abs(FColorRange - 0.24) > EPS;
end;

function TCGxCustomBombShader.StoreColorSharpness: Boolean;
begin
  Result := Abs(FColorSharpness - 1) > EPS;
end;

function TCGxCustomBombShader.StoreDisplacement: Boolean;
begin
  Result := Abs(FDisplacement - 0.3) > EPS;
end;

function TCGxCustomBombShader.StoreGradientTextureShare: Boolean;
begin
  Result := Abs(FGradientTextureShare - 0.7) > EPS;
end;

function TCGxCustomBombShader.StoreMainTextureShare: Boolean;
begin
  Result := Abs(FMainTextureShare - 0.7) > EPS;
end;

function TCGxCustomBombShader.StoreSharpness: Boolean;
begin
  Result := Abs(FSharpness - 3) > EPS;
end;

function TCGxCustomBombShader.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 0.3) > EPS;
end;

function TCGxCustomBombShader.StoreTurbDensity: Boolean;
begin
  Result := Abs(FTurbDensity - 1) > EPS;
end;

//--------------------------------
// TCGxBombShader
//--------------------------------

procedure TCGxBombShader.DoApply(var rci: TgxRenderContextInfo;
  Sender: TObject);
begin
{$IFNDEF USE_OPTIMIZATIONS}
  if (not (csDesigning in ComponentState)) or DesignEnable then
    inherited;
{$ENDIF}
end;

procedure TCGxBombShader.DoInitialize(var rci : TgxRenderContextInfo; Sender : TObject);
begin
{$IFNDEF USE_OPTIMIZATIONS}
  if (not (csDesigning in ComponentState)) or DesignEnable then
    inherited;
{$ENDIF}
end;

procedure TCGxBombShader.OnApplyFP(CgProgram: TCGxProgram;
  Sender: TObject);
begin
{$IFNDEF USE_OPTIMIZATIONS}
  if (not (csDesigning in ComponentState)) or DesignEnable then
    inherited;
{$ENDIF}
end;

procedure TCGxBombShader.OnApplyVP(CgProgram: TCGxProgram;
  Sender: TObject);
begin
{$IFNDEF USE_OPTIMIZATIONS}
  if (not (csDesigning in ComponentState)) or DesignEnable then
    inherited;
{$ENDIF}
end;

procedure TCGxBombShader.OnUnApplyFP(CgProgram: TCGxProgram);
begin
{$IFNDEF USE_OPTIMIZATIONS}
  if (not (csDesigning in ComponentState)) or DesignEnable then
    inherited;
{$ENDIF}
end;

initialization
  RegisterClasses([TCGxCustomBombShader, TCGxBombShader]);

end.

