//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLSL.AsmShader;

(*
    TGLAsmShader is a wrapper for all ARB shaders 
    This component is only a template and has to be replaced with a
    proper version by someone who uses ARB shaders.
*)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  
  GLS.OpenGLTokens,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.Texture,
  GLS.Context,
  GLSL.CustomShader,
  GLS.RenderContextInfo;

type
  TGLCustomAsmShader = class;
  TGLAsmShaderEvent = procedure(Shader: TGLCustomAsmShader) of object;
  TGLAsmShaderUnUplyEvent = procedure(Shader: TGLCustomAsmShader; var ThereAreMorePasses: Boolean) of object;

  TGLAsmShaderParameter = class(TGLCustomShaderParameter)
  private
  protected
{
    function GetAsVector1f: Single; override;
    function GetAsVector1i: Integer; override;
    function GetAsVector2f: TVector2f; override;
    function GetAsVector2i: TVector2i; override;
    function GetAsVector3f: TVector3f; override;
    function GetAsVector3i: TVector3i; override;
    function GetAsVector4f: TGLVector; override;
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
      const Value: TGLTexture);
    procedure SetAsTexture2D(const TextureIndex: Integer;
      const Value: TGLTexture);
    procedure SetAsTexture3D(const TextureIndex: Integer;
      const Value: TGLTexture);
    procedure SetAsTextureCube(const TextureIndex: Integer;
      const Value: TGLTexture);
    procedure SetAsTextureRect(const TextureIndex: Integer;
      const Value: TGLTexture);

    function GetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word): Cardinal; override;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word; const Value: Cardinal); override;
}
  end;

  TGLCustomAsmShader = class(TGLCustomShader)
  private
    FVPHandle: TGLARBVertexProgramHandle;
    FFPHandle: TGLARBFragmentProgramHandle;
    FGPHandle: TGLARBGeometryProgramHandle;
    FOnInitialize: TGLAsmShaderEvent;
    FOnApply: TGLAsmShaderEvent;
    FOnUnApply: TGLAsmShaderUnUplyEvent;
  protected
    procedure ApplyShaderPrograms;
    procedure UnApplyShaderPrograms;
    procedure DestroyARBPrograms; virtual;
    property OnApply: TGLAsmShaderEvent read FOnApply write FOnApply;
    property OnUnApply: TGLAsmShaderUnUplyEvent read FOnUnApply write FOnUnApply;
    property OnInitialize: TGLAsmShaderEvent read FOnInitialize write FOnInitialize;
    procedure DoInitialize(var rci: TGLRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
    procedure DoFinalize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ShaderSupported: Boolean; override;
  end;

  TGLAsmShader = class(TGLCustomAsmShader)
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

//--------------------------------------------------------------
implementation
//--------------------------------------------------------------

{ TGLCustomAsmShader }

procedure TGLCustomAsmShader.DoFinalize;
begin
  inherited;
  DestroyARBPrograms;
end;

procedure TGLCustomAsmShader.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TGLCustomAsmShader then
  begin
    // Nothing here ...yet
  end;
end;

constructor TGLCustomAsmShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TGLCustomAsmShader.Destroy;
begin
  DestroyARBPrograms;

  inherited Destroy;
end;

procedure TGLCustomAsmShader.DestroyARBPrograms;
begin
  FVPHandle.Free;
  FVPHandle := nil;
  FFPHandle.Free;
  FFPHandle := nil;
  FGPHandle.Free;
  FGPHandle := nil;
end;

procedure TGLCustomAsmShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
begin
  ApplyShaderPrograms();

  if Assigned(FOnApply) then
    FOnApply(Self);
end;

procedure TGLCustomAsmShader.DoInitialize(var rci: TGLRenderContextInfo; Sender: TObject);
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
        FVPHandle := TGLARBVertexProgramHandle.CreateAndAllocate;
      FVPHandle.LoadARBProgram(VertexProgram.Code.Text);
      VertexProgram.Enabled := FVPHandle.Ready;
    end;

    if FragmentProgram.Enabled then
    begin
      if not Assigned(FFPHandle) then
        FFPHandle := TGLARBFragmentProgramHandle.CreateAndAllocate;
      FFPHandle.LoadARBProgram(FragmentProgram.Code.Text);
      FragmentProgram.Enabled := FFPHandle.Ready;
    end;

    if GeometryProgram.Enabled then
    begin
      if not Assigned(FGPHandle) then
        FGPHandle := TGLARBGeometryProgramHandle.CreateAndAllocate;
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

function TGLCustomAsmShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  if Assigned(FOnUnApply) then
    FOnUnApply(Self, Result)
  else
    Result := False;

  UnApplyShaderPrograms();
end;

function TGLCustomAsmShader.ShaderSupported: Boolean;
begin
  Result :=
    (VertexProgram.Enabled and TGLARBVertexProgramHandle.IsSupported) or
    (FragmentProgram.Enabled and TGLARBFragmentProgramHandle.IsSupported) or
    (GeometryProgram.Enabled and TGLARBGeometryProgramHandle.IsSupported);
end;

procedure TGLCustomAsmShader.ApplyShaderPrograms;
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

procedure TGLCustomAsmShader.UnApplyShaderPrograms;
begin
  if VertexProgram.Enabled then
    FVPHandle.Disable;
  if FragmentProgram.Enabled then
    FFPHandle.Disable;
  if GeometryProgram.Enabled then
    FGPHandle.Disable;
end;

//-----------------------------------------
initialization
//-----------------------------------------

  RegisterClasses([TGLCustomAsmShader, TGLAsmShader]);

end.

