//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLS.ShaderCel;

(*
   A shader that applies cel shading through a vertex program
   and shade definition texture. 
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
   
  OpenGLTokens,
  GLTexture, 
  GLContext, 
  GLGraphics, 
  GLS.Utils,
  GLVectorGeometry, 
  GLColor, 
  GLRenderContextInfo,
  GLMaterial, 
  GLState, 
  GLTextureFormat;

type
  (*Cel shading options. 
     csoOutlines: Render a second outline pass.
     csoTextured: Allows for a primary texture that the cel shading
                  is modulated with and forces the shade definition
                  to render as a second texture. *)
  TGLCelShaderOption = (csoOutlines, csoTextured, csoNoBuildShadeTexture);
  TGLCelShaderOptions = set of TGLCelShaderOption;

  // An event for user defined cel intensity.
  TGLCelShaderGetIntensity = procedure(Sender: TObject; var intensity: Byte) of
    object;

  // A generic cel shader.
  TGLCelShader = class(TGLShader)
  private
    FOutlineWidth: Single;
    FCelShaderOptions: TGLCelShaderOptions;
    FVPHandle: TGLARBVertexProgramHandle;
    FShadeTexture: TGLTexture;
    FOnGetIntensity: TGLCelShaderGetIntensity;
    FOutlinePass,
      FUnApplyShadeTexture: Boolean;
    FOutlineColor: TGLColor;
  protected
    procedure SetCelShaderOptions(const val: TGLCelShaderOptions);
    procedure SetOutlineWidth(const val: Single);
    procedure SetOutlineColor(const val: TGLColor);
    procedure BuildShadeTexture;
    procedure Loaded; override;
    function GenerateVertexProgram: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
    property ShadeTexture: TGLTexture read FShadeTexture;
  published
    property CelShaderOptions: TGLCelShaderOptions read FCelShaderOptions write
      SetCelShaderOptions;
    property OutlineColor: TGLColor read FOutlineColor write SetOutlineColor;
    property OutlineWidth: Single read FOutlineWidth write SetOutlineWidth;
    property OnGetIntensity: TGLCelShaderGetIntensity read FOnGetIntensity write
      FOnGetIntensity;
  end;

// ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TGLCelShader ------------------
// ------------------

constructor TGLCelShader.Create(AOwner: TComponent);
begin
  inherited;

  FOutlineWidth := 3;
  FCelShaderOptions := [csoOutlines];
  FShadeTexture := TGLTexture.Create(Self);
  with FShadeTexture do
  begin
    Enabled := True;
    MinFilter := miNearest;
    MagFilter := maNearest;
    TextureWrap := twNone;
    TextureMode := tmModulate;
  end;

  FOutlineColor := TGLColor.Create(Self);
  FOutlineColor.OnNotifyChange := NotifyChange;
  FOutlineColor.Initialize(clrBlack);

  ShaderStyle := ssLowLevel;

  FVPHandle := TGLARBVertexProgramHandle.Create;
end;

 
destructor TGLCelShader.Destroy;
begin
  FVPHandle.Free;
  FShadeTexture.Free;
  FOutlineColor.Free;
  inherited;
end;


procedure TGLCelShader.Loaded;
begin
  inherited;
  BuildShadeTexture;
end;


procedure TGLCelShader.BuildShadeTexture;
var
  bmp32: TGLBitmap32;
  i: Integer;
  intensity: Byte;
begin
  if csoNoBuildShadeTexture in FCelShaderOptions then
    exit;

  with FShadeTexture do
  begin
    ImageClassName := 'TGLBlankImage';
    TGLBlankImage(Image).Width := 128;
    TGLBlankImage(Image).Height := 2;
  end;

  bmp32 := FShadeTexture.Image.GetBitmap32;
  bmp32.Blank := false;
  for i := 0 to bmp32.Width - 1 do
  begin
    intensity := i * (256 div bmp32.Width);

    if Assigned(FOnGetIntensity) then
      FOnGetIntensity(Self, intensity)
    else
    begin
      if intensity > 230 then
        intensity := 255
      else if intensity > 150 then
        intensity := 230
      else if intensity > 100 then
        intensity := intensity + 50
      else
        intensity := 150;
    end;

    bmp32.Data^[i].r := intensity;
    bmp32.Data^[i].g := intensity;
    bmp32.Data^[i].b := intensity;
    bmp32.Data^[i].a := 1;
    bmp32.Data^[i + bmp32.Width] := bmp32.Data^[i];
  end;
end;


function TGLCelShader.GenerateVertexProgram: string;
var
  VP: TStringList;
begin
  VP := TStringList.Create;

  VP.Add('!!ARBvp1.0');
  VP.Add('OPTION ARB_position_invariant;');

  VP.Add('PARAM mvinv[4] = { state.matrix.modelview.inverse };');
  VP.Add('PARAM lightPos = program.local[0];');
  VP.Add('TEMP temp, light, normal;');

  VP.Add('   DP4 light.x, mvinv[0], lightPos;');
  VP.Add('   DP4 light.y, mvinv[1], lightPos;');
  VP.Add('   DP4 light.z, mvinv[2], lightPos;');
  VP.Add('   ADD light, light, -vertex.position;');
  VP.Add('   DP3 temp.x, light, light;');
  VP.Add('   RSQ temp.x, temp.x;');
  VP.Add('   MUL light, temp.x, light;');

  VP.Add('   DP3 temp, vertex.normal, vertex.normal;');
  VP.Add('   RSQ temp.x, temp.x;');
  VP.Add('   MUL normal, temp.x, vertex.normal;');

  VP.Add('   MOV result.color, state.material.diffuse;');

  if csoTextured in FCelShaderOptions then
  begin
    VP.Add('   MOV result.texcoord[0], vertex.texcoord[0];');
    VP.Add('   DP3 result.texcoord[1].x, normal, light;');
  end
  else
  begin
    VP.Add('   DP3 result.texcoord[0].x, normal, light;');
  end;

  VP.Add('END');

  Result := VP.Text;
  VP.Free;
end;


procedure TGLCelShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
var
  light: TVector;
begin
  if (csDesigning in ComponentState) then
    exit;

  FVPHandle.AllocateHandle;
  if FVPHandle.IsDataNeedUpdate then
  begin
    FVPHandle.LoadARBProgram(GenerateVertexProgram);
    Enabled := FVPHandle.Ready;
    FVPHandle.NotifyDataUpdated;
    if not Enabled then
      Abort;
  end;

  rci.GLStates.Disable(stLighting);
  gl.GetLightfv(GL_LIGHT0, GL_POSITION, @light.X);
  FVPHandle.Enable;
  FVPHandle.Bind;
  gl.ProgramLocalParameter4fv(GL_VERTEX_PROGRAM_NV, 0, @light.X);

  if (csoTextured in FCelShaderOptions) then
    FShadeTexture.ApplyAsTexture2(rci, nil)
  else
    FShadeTexture.Apply(rci);

  FOutlinePass := csoOutlines in FCelShaderOptions;
  FUnApplyShadeTexture := True;
end;


function TGLCelShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  Result := False;
  if (csDesigning in ComponentState) then
    exit;

  FVPHandle.Disable;

  if FUnApplyShadeTexture then
  begin
    if (csoTextured in FCelShaderOptions) then
      FShadeTexture.UnApplyAsTexture2(rci, false)
    else
      FShadeTexture.UnApply(rci);
    FUnApplyShadeTexture := False;
  end;

  if FOutlinePass then
    with rci.GLStates do
    begin
      ActiveTexture := 0;
      ActiveTextureEnabled[ttTexture2D] := False;
      Enable(stBlend);
      Enable(stLineSmooth);
      Disable(stLineStipple);
      Enable(stCullFace);

      PolygonMode := pmLines;
      LineWidth := FOutlineWidth;
      CullFaceMode := cmFront;
      LineSmoothHint := hintNicest;
      SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      DepthFunc := cfLEqual;
      gl.Color4fv(FOutlineColor.AsAddress);

      Result := True;
      FOutlinePass := False;
      Exit;
    end
  else
    with rci.GLStates do
    begin
      rci.GLStates.PolygonMode := pmFill;
      rci.GLStates.CullFaceMode := cmBack;
      rci.GLStates.DepthFunc := cfLEqual;
    end;

end;


procedure TGLCelShader.SetCelShaderOptions(const val: TGLCelShaderOptions);
begin
  if val <> FCelShaderOptions then
  begin
    FCelShaderOptions := val;
    BuildShadeTexture;
    FVPHandle.NotifyChangesOfData;
    NotifyChange(Self);
  end;
end;


procedure TGLCelShader.SetOutlineWidth(const val: Single);
begin
  if val <> FOutlineWidth then
  begin
    FOutlineWidth := val;
    NotifyChange(Self);
  end;
end;


procedure TGLCelShader.SetOutlineColor(const val: TGLColor);
begin
  if val <> FOutlineColor then
  begin
    FOutlineColor.Assign(val);
    NotifyChange(Self);
  end;
end;

end.

