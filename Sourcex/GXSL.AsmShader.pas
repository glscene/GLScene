//
// The graphics engine GXScene https://github.com/glscene
//
unit GXSL.AsmShader;

(*
    TgxAsmShader is a wrapper for all ARB shaders
    This component is only a template and has to be replaced with a
    proper version by someone who uses ARB shaders.
*)

interface

uses
  Winapi.OpenGL,

  System.Classes,
  System.SysUtils,

  GXS.Context,
  GXS.VectorGeometry,
  GXS.VectorTypes,
  GXS.Texture,
  GXSL.CustomShader,
  GXS.RenderContextInfo;

type
  TgxCustomAsmShader = class;
  TgxAsmShaderEvent = procedure(Shader: TgxCustomAsmShader) of object;
  TgxAsmShaderUnUplyEvent = procedure(Shader: TgxCustomAsmShader; var ThereAreMorePasses: Boolean) of object;

  TgxAsmShaderParameter = class(TgxCustomShaderParameter)
  protected
{
    function GetAsVector1f: Single; override;
    function GetAsVector1i: Integer; override;
    function GetAsVector2f: TVector2f; override;
    function GetAsVector2i: TVector2i; override;
    function GetAsVector3f: TVector3f; override;
    function GetAsVector3i: TVector3i; override;
    function GetAsVector4f: TVector4f; override;
    function GetAsVector4i: TVector4i; override;

    procedure SetAsVector1f(const Value: Single); override;
    procedure SetAsVector1i(const Value: Integer); override;
    procedure SetAsVector2i(const Value: TVector2i); override;
    procedure SetAsVector3i(const Value: TVector3i); override;
    procedure SetAsVector4i(const Value: TVector4i); override;
    procedure SetAsVector2f(const Value: TVector2f); override;
    procedure SetAsVector3f(const Value: TVector3f); override;
    procedure SetAsVector4f(const Value: TVector4f); override;

    function GetAsMatrix2f: TMatrix2f; override;
    function GetAsMatrix3f: TMatrix3f; override;
    function GetAsMatrix4f: TMatrix4f; override;
    procedure SetAsMatrix2f(const Value: TMatrix2f); override;
    procedure SetAsMatrix3f(const Value: TMatrix3f); override;
    procedure SetAsMatrix4f(const Value: TMatrix4f); override;

    procedure SetAsTexture1D(const TextureIndex: Integer;
      const Value: TgxTexture);
    procedure SetAsTexture2D(const TextureIndex: Integer;
      const Value: TgxTexture);
    procedure SetAsTexture3D(const TextureIndex: Integer;
      const Value: TgxTexture);
    procedure SetAsTextureCube(const TextureIndex: Integer;
      const Value: TgxTexture);
    procedure SetAsTextureRect(const TextureIndex: Integer;
      const Value: TgxTexture);

    function GetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word): Cardinal; override;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word; const Value: Cardinal); override;
}
  end;

  TgxCustomAsmShader = class(TgxCustomShader)
  private
    FVPHandle: TgxVertexProgramHandle;
    FFPHandle: TgxFragmentProgramHandle;
    FGPHandle: TgxGeometryProgramHandle;
    FOnInitialize: TgxAsmShaderEvent;
    FOnApply: TgxAsmShaderEvent;
    FOnUnApply: TgxAsmShaderUnUplyEvent;
  protected
    procedure ApplyShaderPrograms;
    procedure UnApplyShaderPrograms;
    procedure DestroyARBPrograms; virtual;
    property OnApply: TgxAsmShaderEvent read FOnApply write FOnApply;
    property OnUnApply: TgxAsmShaderUnUplyEvent read FOnUnApply write FOnUnApply;
    property OnInitialize: TgxAsmShaderEvent read FOnInitialize write FOnInitialize;
    procedure DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
    procedure DoFinalize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ShaderSupported: Boolean; override;
  end;

  TgxAsmShader = class(TgxCustomAsmShader)
  published
    property FragmentProgram;
    property VertexProgram;
    property GeometryProgram;
    property OnApply;
    property OnUnApply;
    property OnInitialize;
    property ShaderStyle;
    property FailedInitAction;
  end;

//==============================
implementation
//==============================

//------------------------------
// TgxCustomAsmShader
//------------------------------

procedure TgxCustomAsmShader.DoFinalize;
begin
  inherited;
  DestroyARBPrograms;
end;

procedure TgxCustomAsmShader.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TgxCustomAsmShader then
  begin
    // Nothing here ...yet
  end;
end;

constructor TgxCustomAsmShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TgxCustomAsmShader.Destroy;
begin
  DestroyARBPrograms;
  inherited Destroy;
end;

procedure TgxCustomAsmShader.DestroyARBPrograms;
begin
  FVPHandle.Free;
  FVPHandle := nil;
  FFPHandle.Free;
  FFPHandle := nil;
  FGPHandle.Free;
  FGPHandle := nil;
end;

procedure TgxCustomAsmShader.DoApply(var rci: TgxRenderContextInfo; Sender: TObject);
begin
  ApplyShaderPrograms();

  if Assigned(FOnApply) then
    FOnApply(Self);
end;

procedure TgxCustomAsmShader.DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject);
begin
  if not ShaderSupported then
  begin
    Enabled := False;
    HandleFailedInitialization;
  end
  else
  begin
    if VertexProgram.Enabled then
    begin
      if not Assigned(FVPHandle) then
        FVPHandle := TgxVertexProgramHandle.CreateAndAllocate;
      FVPHandle.LoadARBProgram(VertexProgram.Code.Text);
      VertexProgram.Enabled := FVPHandle.Ready;
    end;

    if FragmentProgram.Enabled then
    begin
      if not Assigned(FFPHandle) then
        FFPHandle := TgxFragmentProgramHandle.CreateAndAllocate;
      FFPHandle.LoadARBProgram(FragmentProgram.Code.Text);
      FragmentProgram.Enabled := FFPHandle.Ready;
    end;

    if GeometryProgram.Enabled then
    begin
      if not Assigned(FGPHandle) then
        FGPHandle := TgxGeometryProgramHandle.CreateAndAllocate;
      FGPHandle.LoadARBProgram(GeometryProgram.Code.Text);
      GeometryProgram.Enabled := FGPHandle.Ready;
    end;

    Enabled := (FragmentProgram.Enabled or VertexProgram.Enabled or GeometryProgram.Enabled);

    if Enabled then
    begin
      if Assigned(FOnInitialize) then
        FOnInitialize(Self)
    end;
  end;
end;

function TgxCustomAsmShader.DoUnApply(var rci: TgxRenderContextInfo): Boolean;
begin
  if Assigned(FOnUnApply) then
    FOnUnApply(Self, Result)
  else
    Result := False;

  UnApplyShaderPrograms();
end;

function TgxCustomAsmShader.ShaderSupported: Boolean;
begin
  Result :=
    (VertexProgram.Enabled) or
    (FragmentProgram.Enabled) or
    (GeometryProgram.Enabled);
end;

procedure TgxCustomAsmShader.ApplyShaderPrograms;
begin
  if VertexProgram.Enabled then
  begin
    FVPHandle.Enable;
    FVPHandle.Bind;
  end;
  if FragmentProgram.Enabled then
  begin
    FFPHandle.Enable;
    FFPHandle.Bind;
  end;
  if GeometryProgram.Enabled then
  begin
    FGPHandle.Enable;
    FGPHandle.Bind;
  end;
end;

procedure TgxCustomAsmShader.UnApplyShaderPrograms;
begin
  if VertexProgram.Enabled then
    FVPHandle.Disable;
  if FragmentProgram.Enabled then
    FFPHandle.Disable;
  if GeometryProgram.Enabled then
    FGPHandle.Disable;
end;

//-------------------------------------
initialization
//-------------------------------------

  RegisterClasses([TgxCustomAsmShader, TgxAsmShader]);

end.

