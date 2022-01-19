//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLSL.Shader;

(* TGLSLShader is a wrapper for GLS shaders. *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  
  GLS.OpenGLTokens,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.Texture,
  GLS.Context,
  GLSL.CustomShader,
  GLS.RenderContextInfo,
  GLS.TextureFormat,
  GLSL.ShaderParameter,
  GLS.Material,
  GLS.State;


type
  TGLSLShaderParameter = class;
  TGLCustomGLSLShader = class;
  EGLSLShaderException = class(EGLCustomShaderException);

  TGLSLShaderEvent = procedure(Shader: TGLCustomGLSLShader) of object;
  TGLSLShaderUnApplyEvent = procedure(Shader: TGLCustomGLSLShader;
                                     var ThereAreMorePasses: Boolean) of object;
  TGLSLShaderEventEx = procedure(Shader: TGLCustomGLSLShader;
    Sender: TObject) of object;

  TGLActiveAttrib = record
    Name: string;
    Size: Integer;
    AType: TGLSLDataType;
    Location: Integer;
  end;

  TGLActiveAttribArray = array of TGLActiveAttrib;

  TGLCustomGLSLShader = class(TGLCustomShader)
  private
    FGLSLProg: TGLProgramHandle;
    FParam: TGLSLShaderParameter;
    FActiveVarying: TStrings;
    FTransformFeedBackMode: TGLTransformFeedBackMode;
    FOnInitialize: TGLSLShaderEvent;
    FOnApply: TGLSLShaderEvent;
    FOnUnApply: TGLSLShaderUnApplyEvent;
    FOnInitializeEx: TGLSLShaderEventEx;
    FOnApplyEx: TGLSLShaderEventEx;
    FNextTexIndex : integer; // for auto texture unit indicing
    function GetParam(const Index: string): TGLSLShaderParameter;
    function GetDirectParam(const Index: Cardinal): TGLSLShaderParameter;
    procedure OnChangeActiveVarying(Sender: TObject);
  protected
    property OnApply: TGLSLShaderEvent read FOnApply write FOnApply;
    property OnUnApply: TGLSLShaderUnApplyEvent read FOnUnApply write FOnUnApply;
    property OnInitialize: TGLSLShaderEvent read FOnInitialize write FOnInitialize;
    property OnInitializeEx: TGLSLShaderEventEx read FOnInitializeEx write FOnInitializeEx;
    property OnApplyEx: TGLSLShaderEventEx read FOnApplyEx write FOnApplyEx;
    function GetGLSLProg: TGLProgramHandle; virtual;
    function GetCurrentParam: TGLSLShaderParameter; virtual;
    procedure SetActiveVarying(const Value: TStrings);
    procedure SetTransformFeedBackMode(const Value: TGLTransformFeedBackMode);
    procedure DoInitialize(var rci: TGLRenderContextInfo; Sender: TObject); override;
    procedure DoFinalize; override;
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ShaderSupported: Boolean; override;
    function GetActiveAttribs: TGLActiveAttribArray;
    // SetTex() sets texture with automatic book-keeping of texture unit indices.
    // Users can just call SetTex() in the OnApply event without keeping track of texture unit indices.
    // Call from OnApply() only.
    procedure SetTex(const TexParamName : String; Tex : TGLTexture); overload;
    procedure SetTex(const TexParamName : String; Mat : TGLLibMaterial); overload;
    procedure SetTex(TexParam : TGLSLShaderParameter; Tex : TGLTexture); overload;
    procedure SetTex(TexParam : TGLSLShaderParameter; Mat : TGLLibMaterial); overload;
    property Param[const Index: string]: TGLSLShaderParameter read GetParam;
    property DirectParam[const Index: Cardinal]: TGLSLShaderParameter read GetDirectParam;
    property ActiveVarying: TStrings read FActiveVarying write SetActiveVarying;
    property TransformFeedBackMode: TGLTransformFeedBackMode read FTransformFeedBackMode write SetTransformFeedBackMode default tfbmInterleaved;
  end;

  // Wrapper around a parameter of a GLSL program.
  TGLSLShaderParameter = class(TGLCustomShaderParameter)
  private
    FGLSLProg: TGLProgramHandle;
    FParameterID: Integer;
  protected
    function GetAsVector1f: Single; override;
    function GetAsVector2f: TVector2f; override;
    function GetAsVector3f: TVector3f; override;
    function GetAsVector4f: TGLVector; override;
    function GetAsVector1i: Integer; override;
    function GetAsVector2i: TVector2i; override;
    function GetAsVector3i: TVector3i; override;
    function GetAsVector4i: TVector4i; override;
    function GetAsVector1ui: Cardinal; override;
    function GetAsVector2ui: TVector2ui; override;
    function GetAsVector3ui: TVector3ui; override;
    function GetAsVector4ui: TVector4ui; override;
    procedure SetAsVector1f(const Value: Single); override;
    procedure SetAsVector2f(const Value: TVector2f); override;
    procedure SetAsVector3f(const Value: TVector3f); override;
    procedure SetAsVector4f(const Value: TVector4f); override;
    procedure SetAsVector1i(const Value: Integer); override;
    procedure SetAsVector2i(const Value: TVector2i); override;
    procedure SetAsVector3i(const Value: TVector3i); override;
    procedure SetAsVector4i(const Value: TVector4i); override;
    procedure SetAsVector1ui(const Value: Cardinal); override;
    procedure SetAsVector2ui(const Value: TVector2ui); override;
    procedure SetAsVector3ui(const Value: TVector3ui); override;
    procedure SetAsVector4ui(const Value: TVector4ui); override;
    function GetAsMatrix2f: TMatrix2f; override;
    function GetAsMatrix3f: TMatrix3f; override;
    function GetAsMatrix4f: TMatrix4f; override;
    procedure SetAsMatrix2f(const Value: TMatrix2f); override;
    procedure SetAsMatrix3f(const Value: TMatrix3f); override;
    procedure SetAsMatrix4f(const Value: TMatrix4f); override;
    function GetAsCustomTexture(const TextureIndex: Integer;
      TextureTarget: TGLTextureTarget): Cardinal; override;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      TextureTarget: TGLTextureTarget; const Value: Cardinal); override;
    function GetAsUniformBuffer: Cardinal; override;
    procedure SetAsUniformBuffer( UBO: Cardinal); override;
   public
     // Nothing here ...yet.
   end;

  TGLSLShader = class(TGLCustomGLSLShader)
  published
    property FragmentProgram;
    property VertexProgram;
    property GeometryProgram;    
    property OnApply;
    property OnApplyEx;
    property OnUnApply;
    property OnInitialize;
    property OnInitializeEx;
    property ShaderStyle;
    property FailedInitAction;
    property ActiveVarying;
    property TransformFeedBackMode;
  end;

//------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------

//----------------------------------
// TGLCustomGLSLShader
//----------------------------------

procedure TGLCustomGLSLShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
begin
  FGLSLProg.UseProgramObject;
  FNextTexIndex := 0;
  if Assigned(FOnApply) then
    FOnApply(Self);
  if Assigned(FOnApplyEx) then
    FOnApplyEx(Self, Sender);
end;


procedure TGLCustomGLSLShader.DoInitialize(var rci: TGLRenderContextInfo; Sender: TObject);
const
  cBufferMode: array[tfbmInterleaved..tfbmSeparate] of Cardinal = (
    GL_INTERLEAVED_ATTRIBS_EXT, GL_SEPARATE_ATTRIBS_EXT);
var
  i, NumVarying: Integer;
  sVaryings: array of AnsiString;
  pVaryings: array of PAnsiChar;
begin
  try
    if not ShaderSupported then
      HandleFailedInitialization
    else
    try
      FGLSLProg.AllocateHandle;
      if FGLSLProg.IsDataNeedUpdate then
      begin
        if Name <> '' then
          FGLSLProg.Name := Name
        else
          FGLSLProg.Name := ClassName;

        FGLSLProg.DetachAllObject;
        if VertexProgram.Enabled then
          FGLSLProg.AddShader(TGLVertexShaderHandle, VertexProgram.Code.Text, FDebugMode);
        if FragmentProgram.Enabled then
          FGLSLProg.AddShader(TGLFragmentShaderHandle, FragmentProgram.Code.Text, FDebugMode);
        if GeometryProgram.Enabled then
          FGLSLProg.AddShader(TGLGeometryShaderHandle, GeometryProgram.Code.Text, FDebugMode);

        if VertexProgram.Enabled or FragmentProgram.Enabled or GeometryProgram.Enabled then
        begin
          if GeometryProgram.Enabled then
          begin
            gl.ProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_INPUT_TYPE_EXT,
              cGLgsInTypes[GeometryProgram.InputPrimitiveType]);
            gl.ProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_OUTPUT_TYPE_EXT,
              cGLgsOutTypes[GeometryProgram.OutputPrimitiveType]);
            gl.ProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_VERTICES_OUT_EXT,
              GeometryProgram.VerticesOut);
          end;

          NumVarying := FActiveVarying.Count;
          if NumVarying > 0 then
          begin
            // Activate varying
            SetLength(sVaryings, NumVarying);
            SetLength(pVaryings, NumVarying);
            for i := 0 to NumVarying - 1 do
            begin
              sVaryings[i] := AnsiString(FActiveVarying.Strings[i]) + #0;
              pVaryings[i] := PAnsiChar( sVaryings[i] );
            end;
            gl.TransformFeedbackVaryings(
              FGLSLProg.Handle, NumVarying, @pVaryings[0],
              cBufferMode[FTransformFeedBackMode] );
          end;

          if (not FGLSLProg.LinkProgram) then
            raise EGLSLShaderException.Create(FGLSLProg.InfoLog);

          FGLSLProg.DetachAllObject;  // Detach shaders after linking. 
        end;
        FGLSLProg.NotifyDataUpdated;
      end;
    except
      on E: Exception do
      begin
        Enabled := False;
        HandleFailedInitialization(E.Message);
      end;
    end;

  finally
    if Enabled then
    try
      if Assigned(FOnInitialize) then
      begin
        FGLSLProg.UseProgramObject;
        FOnInitialize(Self);
        FGLSLProg.EndUseProgramObject;
      end;
      if Assigned(FOnInitializeEx) then
      begin
        FGLSLProg.UseProgramObject;
        FOnInitializeEx(Self, Sender);
        FGLSLProg.EndUseProgramObject;
      end;
      if (not FGLSLProg.ValidateProgram) then
        raise EGLSLShaderException.Create(FGLSLProg.InfoLog);
    except
      on E: Exception do
      begin
        Enabled := False;
        HandleFailedInitialization(E.Message);
      end;
    end;
  end;
end;

function TGLCustomGLSLShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnUnApply) then
    FOnUnApply(Self, Result);
  if not Result then
    FGLSLProg.EndUseProgramObject;
end;

function TGLCustomGLSLShader.ShaderSupported: Boolean;
begin
  Result := (GL.ARB_shader_objects and GL.ARB_vertex_program and
             GL.ARB_vertex_shader and GL.ARB_fragment_shader);
end;

function TGLCustomGLSLShader.GetActiveAttribs: TGLActiveAttribArray;
var
  LRci: TGLRenderContextInfo;
  i, j: Integer;
  buff: array[0..127] of AnsiChar;
  len: Integer;
  max: Integer;
  glType: Cardinal;
begin
  DoInitialize(LRci, Self);

  SetLength(Result, 16);
  j := 0;
  if FGLSLProg.Handle<>0 then
  begin
    gl.GetProgramiv(FGLSLProg.Handle, GL_ACTIVE_ATTRIBUTES, @max);
    for i := 0 to 16 - 1 do
    if i<max then
    begin
      gl.GetActiveAttrib(FGLSLProg.Handle, i, Length(buff), @len, @Result[j].Size,
        @glType, @buff[0]);
      if glType > 0 then
        with Result[j] do
        begin
          case glType of
            GL_FLOAT: AType := GLSLType1F;
            GL_FLOAT_VEC2: AType := GLSLType2F;
            GL_FLOAT_VEC3: AType := GLSLType3F;
            GL_FLOAT_VEC4: AType := GLSLType4F;
            GL_INT: AType := GLSLType1I;
            GL_INT_VEC2: AType := GLSLType2I;
            GL_INT_VEC3: AType := GLSLType3I;
            GL_INT_VEC4: AType := GLSLType4I;
            GL_UNSIGNED_INT: AType := GLSLType1UI;
            GL_UNSIGNED_INT_VEC2: AType := GLSLType2UI;
            GL_UNSIGNED_INT_VEC3: AType := GLSLType3UI;
            GL_UNSIGNED_INT_VEC4: AType := GLSLType4UI;
            GL_BOOL: AType := GLSLType1I;
            GL_BOOL_VEC2: AType := GLSLType2I;
            GL_BOOL_VEC3: AType := GLSLType3I;
            GL_BOOL_VEC4: AType := GLSLType4I;
            GL_FLOAT_MAT2: AType := GLSLTypeMat2F;
            GL_FLOAT_MAT3: AType := GLSLTypeMat3F;
            GL_FLOAT_MAT4: AType := GLSLTypeMat4F;
          end;
          Name := Copy(string(buff), 0, len);
          Location := i;
          Inc(j);
        end;
    end;
  end;
  SetLength(Result, j);
end;

procedure TGLCustomGLSLShader.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TGLCustomGLSLShader then
  begin
    FreeAndNil(FGLSLProg); //just free the handle for it to be recreated on next initialization
  end;
end;

procedure TGLCustomGLSLShader.DoFinalize;
begin
  inherited;
  if Assigned(FGLSLProg) then
    FGLSLProg.NotifyChangesOfData;
end;

function TGLCustomGLSLShader.GetGLSLProg: TGLProgramHandle;
begin
  Result := FGLSLProg;
end;

function TGLCustomGLSLShader.GetParam(
  const Index: string): TGLSLShaderParameter;
begin
  FParam.FParameterID := FGLSLProg.GetUniformLocation(Index);
  Result := FParam;
end;

function TGLCustomGLSLShader.GetDirectParam(
  const Index: Cardinal): TGLSLShaderParameter;
begin
  FParam.FParameterID := Index;
  Result := FParam;
end;

function TGLCustomGLSLShader.GetCurrentParam: TGLSLShaderParameter;
begin
  Result := FParam;
end;

constructor TGLCustomGLSLShader.Create(AOwner: TComponent);
begin
  inherited;
  FGLSLProg := TGLProgramHandle.Create;
  FParam := TGLSLShaderParameter.Create;
  FParam.FGLSLProg := FGLSLProg;
  FActiveVarying := TStringList.Create;
  TStringList(FActiveVarying).OnChange := OnChangeActiveVarying;
  FTransformFeedBackMode := tfbmInterleaved;
end;

destructor TGLCustomGLSLShader.Destroy;
begin
  FreeAndNil(FGLSLProg);
  FreeAndNil(FParam);
  FreeAndNil(FActiveVarying);
  inherited;
end;

procedure TGLCustomGLSLShader.SetActiveVarying(const Value: TStrings);
begin
  FActiveVarying.Assign(Value);
  NotifyChange(Self);
end;

procedure TGLCustomGLSLShader.SetTransformFeedBackMode(const Value: TGLTransformFeedBackMode);
begin
  if Value <> FTransformFeedBackMode then
  begin
    FTransformFeedBackMode := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLCustomGLSLShader.OnChangeActiveVarying(Sender: TObject);
begin
  NotifyChange(Self);
end;

procedure TGLCustomGLSLShader.SetTex(TexParam : TGLSLShaderParameter; Tex : TGLTexture);
begin
  TexParam.AsTexture[FNextTexIndex] := Tex;
  inc(FNextTexIndex);
end;

procedure TGLCustomGLSLShader.SetTex(TexParam : TGLSLShaderParameter; Mat : TGLLibMaterial);
begin
  SetTex(TexParam, Mat.Material.Texture);
end;

procedure TGLCustomGLSLShader.SetTex(const TexParamName: String; Tex: TGLTexture);
begin
  SetTex(Param[TexParamName], Tex);
end;

procedure TGLCustomGLSLShader.SetTex(const TexParamName: String; Mat: TGLLibMaterial);
begin
  SetTex(TexParamName, Mat.Material.Texture);
end;

{ TGLSLShaderParameter }

function TGLSLShaderParameter.GetAsCustomTexture(
  const TextureIndex: Integer; TextureTarget: TGLTextureTarget): Cardinal;
begin
  gl.GetUniformiv(FGLSLProg.Handle, TextureIndex, @Result);
end;

function TGLSLShaderParameter.GetAsMatrix2f: TMatrix2f;
begin
  gl.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsMatrix3f: TMatrix3f;
begin
  gl.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsMatrix4f: TMatrix4f;
begin
  gl.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector1f: Single;
begin
  gl.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector1i: Integer;
begin
  gl.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector2f: TVector2f;
begin
  gl.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector2i: TVector2i;
begin
  gl.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector3f: TVector3f;
begin
  gl.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector3i: TVector3i;
begin
  gl.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector4f: TGLVector;
begin
  gl.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector4i: TVector4i;
begin
  gl.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TGLSLShaderParameter.SetAsCustomTexture(
  const TextureIndex: Integer; TextureTarget: TGLTextureTarget;
  const Value: Cardinal);
begin
  CurrentGLContext.GLStates.TextureBinding[TextureIndex, TextureTarget] := Value;
  gl.Uniform1i(FParameterID, TextureIndex);
end;

procedure TGLSLShaderParameter.SetAsMatrix2f(const Value: TMatrix2f);
begin
  gl.UniformMatrix2fv(FParameterID, 1, False, @Value);
end;

procedure TGLSLShaderParameter.SetAsMatrix3f(const Value: TMatrix3f);
begin
  gl.UniformMatrix3fv(FParameterID, 1, False, @Value);
end;

procedure TGLSLShaderParameter.SetAsMatrix4f(const Value: TMatrix4f);
begin
  gl.UniformMatrix4fv(FParameterID, 1, False, @Value);
end;

procedure TGLSLShaderParameter.SetAsVector1f(const Value: Single);
begin
  gl.Uniform1f(FParameterID, Value);
end;

procedure TGLSLShaderParameter.SetAsVector1i(const Value: Integer);
begin
  gl.Uniform1i(FParameterID, Value);
end;

procedure TGLSLShaderParameter.SetAsVector2f(const Value: TVector2f);
begin
  gl.Uniform2f(FParameterID, Value.X, Value.Y);
end;

procedure TGLSLShaderParameter.SetAsVector2i(const Value: TVector2i);
begin
  gl.Uniform2i(FParameterID, Value.X, Value.Y);
end;

procedure TGLSLShaderParameter.SetAsVector3f(const Value: TVector3f);
begin
  gl.Uniform3f(FParameterID, Value.X, Value.Y, Value.Z);
end;

procedure TGLSLShaderParameter.SetAsVector3i(const Value: TVector3i);
begin
  gl.Uniform3i(FParameterID, Value.X, Value.Y, Value.Z);
end;

procedure TGLSLShaderParameter.SetAsVector4f(const Value: TVector4f);
begin
  gl.Uniform4f(FParameterID, Value.X, Value.Y, Value.Z, Value.W);
end;

procedure TGLSLShaderParameter.SetAsVector4i(const Value: TVector4i);
begin
  gl.Uniform4i(FParameterID, Value.X, Value.Y, Value.Z, Value.W);
end;

function TGLSLShaderParameter.GetAsUniformBuffer: Cardinal;
begin
  gl.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector1ui: Cardinal;
begin
  gl.GetUniformuiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TGLSLShaderParameter.SetAsVector1ui(const Value: Cardinal);
begin
  gl.Uniform1ui(FParameterID, Value);
end;

function TGLSLShaderParameter.GetAsVector2ui: TVector2ui;
begin
  gl.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TGLSLShaderParameter.SetAsVector2ui(const Value: TVector2ui);
begin
  gl.Uniform2ui(FParameterID, Value.X, Value.Y);
end;

function TGLSLShaderParameter.GetAsVector3ui: TVector3ui;
begin
  gl.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TGLSLShaderParameter.SetAsVector3ui(const Value: TVector3ui);
begin
  gl.Uniform3ui(FParameterID, Value.X, Value.Y, Value.Z);
end;

function TGLSLShaderParameter.GetAsVector4ui: TVector4ui;
begin
  gl.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TGLSLShaderParameter.SetAsVector4ui(const Value: TVector4ui);
begin
  gl.Uniform4ui(FParameterID, Value.X, Value.Y, Value.Z, Value.W);
end;

procedure TGLSLShaderParameter.SetAsUniformBuffer(UBO: Cardinal);
begin
  CurrentGLContext.GLStates.UniformBufferBinding := UBO;
  gl.UniformBuffer(FGLSLProg.Handle, FParameterID, UBO);
end;

//--------------------------------------------------
initialization
//--------------------------------------------------

  RegisterClasses([TGLCustomGLSLShader, TGLSLShader]);

end.

