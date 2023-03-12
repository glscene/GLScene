//
// The graphics platform GLXcene https://github.com/glscene
//
unit GLSLx.Shader;

(* GLXLShader is a wrapper for all GLX shaders *)

interface

{$I Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,

  GLX.VectorGeometry,
  GLX.VectorTypes,
  GLX.Texture,
  GLX.Context,
  GLX.RenderContextInfo,
  GLX.TextureFormat,

  GLSLx.CustomShader,
  GLSLx.Parameter;

type
  TgxGLSLShaderParameter = class;
  TgxCustomGLSLShader = class;
  EGLSLShaderException = class(ECustomShaderException);

  TgxGLSLShaderEvent = procedure(Shader: TgxCustomGLSLShader) of object;
  TgxGLSLShaderUnApplyEvent = procedure(Shader: TgxCustomGLSLShader;
                                     var ThereAreMorePasses: Boolean) of object;
  TgxGLSLShaderEventEx = procedure(Shader: TgxCustomGLSLShader;
    Sender: TObject) of object;

  TgxActiveAttrib = record
    Name: string;
    Size: Integer;
    AType: TgxSLDataType;
    Location: Integer;
  end;

  TgxActiveAttribArray = array of TgxActiveAttrib;

  TgxCustomGLSLShader = class(TgxCustomShader)
  private
    FGLSLProg: TgxProgramHandle;
    FParam: TgxGLSLShaderParameter;
    FActiveVarying: TStrings;
    FTransformFeedBackMode: TgxTransformFeedBackMode;
    FOnInitialize: TgxGLSLShaderEvent;
    FOnApply: TgxGLSLShaderEvent;
    FOnUnApply: TgxGLSLShaderUnApplyEvent;
    FOnInitializeEx: TgxGLSLShaderEventEx;
    FOnApplyEx: TgxGLSLShaderEventEx;
    function GetParam(const Index: string): TgxGLSLShaderParameter;
    function GetDirectParam(const Index: Cardinal): TgxGLSLShaderParameter;
    procedure OnChangeActiveVarying(Sender: TObject);
  protected
    property OnApply: TgxGLSLShaderEvent read FOnApply write FOnApply;
    property OnUnApply: TgxGLSLShaderUnApplyEvent read FOnUnApply write FOnUnApply;
    property OnInitialize: TgxGLSLShaderEvent read FOnInitialize write FOnInitialize;
    property OnInitializeEx: TgxGLSLShaderEventEx read FOnInitializeEx write FOnInitializeEx;
    property OnApplyEx: TgxGLSLShaderEventEx read FOnApplyEx write FOnApplyEx;
    function GetGLSLProg: TgxProgramHandle; virtual;
    function GetCurrentParam: TgxGLSLShaderParameter; virtual;
    procedure SetActiveVarying(const Value: TStrings);
    procedure SetTransformFeedBackMode(const Value: TgxTransformFeedBackMode);
    procedure DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject); override;
    procedure DoFinalize; override;
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ShaderSupported: Boolean; override;
    function GetActiveAttribs: TgxActiveAttribArray;
    property Param[const Index: string]: TgxGLSLShaderParameter read GetParam;
    property DirectParam[const Index: Cardinal]: TgxGLSLShaderParameter read GetDirectParam;
    property ActiveVarying: TStrings read FActiveVarying write SetActiveVarying;
    property TransformFeedBackMode: TgxTransformFeedBackMode read FTransformFeedBackMode write SetTransformFeedBackMode default tfbmInterleaved;
  end;

  // Wrapper around a parameter of a GLSL program.
  TgxGLSLShaderParameter = class(TgxCustomShaderParameter)
  private
    FGLSLProg: TgxProgramHandle;
    FParameterID: Integer;
  protected
    function GetAsVector1f: Single; override;
    function GetAsVector2f: TVector2f; override;
    function GetAsVector3f: TVector3f; override;
    function GetAsVector4f: TgxVector; override;
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
      TextureTarget: TgxTextureTarget): Cardinal; override;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      TextureTarget: TgxTextureTarget; const Value: Cardinal); override;
    function GetAsUniformBuffer: GLenum; override;
    procedure SetAsUniformBuffer( UBO: GLenum); override;
   public
     // Nothing here ...yet.
   end;

  TgxGLSLShader = class(TgxCustomGLSLShader)
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


//=============================================================
implementation
//=============================================================

uses
  GLX.State;

//---------------------------------
// TgxCustomGLSLShader
//---------------------------------

procedure TgxCustomGLSLShader.DoApply(var rci: TgxRenderContextInfo; Sender: TObject);
begin
  FGLSLProg.UseProgramObject;
  if Assigned(FOnApply) then
    FOnApply(Self);
  if Assigned(FOnApplyEx) then
    FOnApplyEx(Self, Sender);
end;

procedure TgxCustomGLSLShader.DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject);
const
  cBufferMode: array[tfbmInterleaved..tfbmSeparate] of GLenum = (
    GL_INTERLEAVED_ATTRIBS_EXT, GL_SEPARATE_ATTRIBS_EXT);
var
  i, NumVarying: Integer;
  sVaryings: array of AnsiString;
  pVaryings: array of PGLChar;
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
          FGLSLProg.AddShader(TgxVertexShaderHandle, VertexProgram.Code.Text, FDebugMode);
        if FragmentProgram.Enabled then
          FGLSLProg.AddShader(TgxFragmentShaderHandle, FragmentProgram.Code.Text, FDebugMode);
        if GeometryProgram.Enabled then
          FGLSLProg.AddShader(TgxGeometryShaderHandle, GeometryProgram.Code.Text, FDebugMode);

        if VertexProgram.Enabled or FragmentProgram.Enabled or GeometryProgram.Enabled then
        begin
          if GeometryProgram.Enabled then
          begin
            glProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_INPUT_TYPE_EXT,
              cGLgsInTypes[GeometryProgram.InputPrimitiveType]);
            glProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_OUTPUT_TYPE_EXT,
              cGLgsOutTypes[GeometryProgram.OutputPrimitiveType]);
            glProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_VERTICES_OUT_EXT,
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
            glTransformFeedbackVaryings(
              FGLSLProg.Handle, NumVarying, @pVaryings[0],
              cBufferMode[FTransformFeedBackMode] );
          end;

          if (not FGLSLProg.LinkProgram) then
            raise EGLSLShaderException.Create(FGLSLProg.InfoLog);
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


function TgxCustomGLSLShader.DoUnApply(var rci: TgxRenderContextInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnUnApply) then
    FOnUnApply(Self, Result);
  if not Result then
    FGLSLProg.EndUseProgramObject;
end;

function TgxCustomGLSLShader.ShaderSupported: Boolean;
begin
  Result := True; (*  (GL_ARB_shader_objects and GL_ARB_vertex_program and
             GL_ARB_vertex_shader and GL_ARB_fragment_shader); *)
end;

function TgxCustomGLSLShader.GetActiveAttribs: TgxActiveAttribArray;
var
  LRci: TgxRenderContextInfo;
  i, j: Integer;
  buff: array[0..127] of AnsiChar;
  len: GLsizei;
  max: GLInt;
  glType: GLEnum;
begin
  DoInitialize(LRci, Self);

  SetLength(Result, 16);
  j := 0;
  if FGLSLProg.Handle<>0 then
  begin
    glGetProgramiv(FGLSLProg.Handle, GL_ACTIVE_ATTRIBUTES, @max);
    for i := 0 to 16 - 1 do
    if i<max then
    begin
      glGetActiveAttrib(FGLSLProg.Handle, i, Length(buff), @len, @Result[j].Size,
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

procedure TgxCustomGLSLShader.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TgxCustomGLSLShader then
  begin
    FreeAndNil(FGLSLProg); //just free the handle for it to be recreated on next initialization
  end;
end;

procedure TgxCustomGLSLShader.DoFinalize;
begin
  inherited;
  if Assigned(FGLSLProg) then
    FGLSLProg.NotifyChangesOfData;
end;

function TgxCustomGLSLShader.GetGLSLProg: TgxProgramHandle;
begin
  Result := FGLSLProg;
end;

function TgxCustomGLSLShader.GetParam(
  const Index: string): TgxGLSLShaderParameter;
begin
  FParam.FParameterID := FGLSLProg.GetUniformLocation(Index);
  Result := FParam;
end;

function TgxCustomGLSLShader.GetDirectParam(
  const Index: Cardinal): TgxGLSLShaderParameter;
begin
  FParam.FParameterID := Index;
  Result := FParam;
end;

function TgxCustomGLSLShader.GetCurrentParam: TgxGLSLShaderParameter;
begin
  Result := FParam;
end;

constructor TgxCustomGLSLShader.Create(AOwner: TComponent);
begin
  inherited;
  FGLSLProg := TgxProgramHandle.Create;
  FParam := TgxGLSLShaderParameter.Create;
  FParam.FGLSLProg := FGLSLProg;
  FActiveVarying := TStringList.Create;
  TStringList(FActiveVarying).OnChange := OnChangeActiveVarying;
  FTransformFeedBackMode := tfbmInterleaved;
end;

destructor TgxCustomGLSLShader.Destroy;
begin
  FreeAndNil(FGLSLProg);
  FreeAndNil(FParam);
  FreeAndNil(FActiveVarying);
  inherited;
end;

procedure TgxCustomGLSLShader.SetActiveVarying(const Value: TStrings);
begin
  FActiveVarying.Assign(Value);
  NotifyChange(Self);
end;

procedure TgxCustomGLSLShader.SetTransformFeedBackMode(const Value: TgxTransformFeedBackMode);
begin
  if Value <> FTransformFeedBackMode then
  begin
    FTransformFeedBackMode := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxCustomGLSLShader.OnChangeActiveVarying(Sender: TObject);
begin
  NotifyChange(Self);
end;

//------------------------------------------------------------
// TgxGLSLShaderParameter
//------------------------------------------------------------

function TgxGLSLShaderParameter.GetAsCustomTexture(
  const TextureIndex: Integer; TextureTarget: TgxTextureTarget): Cardinal;
begin
  glGetUniformiv(FGLSLProg.Handle, TextureIndex, @Result);
end;

function TgxGLSLShaderParameter.GetAsMatrix2f: TMatrix2f;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TgxGLSLShaderParameter.GetAsMatrix3f: TMatrix3f;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TgxGLSLShaderParameter.GetAsMatrix4f: TMatrix4f;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TgxGLSLShaderParameter.GetAsVector1f: Single;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TgxGLSLShaderParameter.GetAsVector1i: Integer;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TgxGLSLShaderParameter.GetAsVector2f: TVector2f;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TgxGLSLShaderParameter.GetAsVector2i: TVector2i;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TgxGLSLShaderParameter.GetAsVector3f: TVector3f;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TgxGLSLShaderParameter.GetAsVector3i: TVector3i;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TgxGLSLShaderParameter.GetAsVector4f: TgxVector;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TgxGLSLShaderParameter.GetAsVector4i: TVector4i;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TgxGLSLShaderParameter.SetAsCustomTexture(
  const TextureIndex: Integer; TextureTarget: TgxTextureTarget;
  const Value: Cardinal);
begin
  CurrentContext.gxStates.TextureBinding[TextureIndex, TextureTarget] := Value;
  glUniform1i(FParameterID, TextureIndex);
end;

procedure TgxGLSLShaderParameter.SetAsMatrix2f(const Value: TMatrix2f);
begin
  glUniformMatrix2fv(FParameterID, 1, 1, @Value);
end;

procedure TgxGLSLShaderParameter.SetAsMatrix3f(const Value: TMatrix3f);
begin
  glUniformMatrix3fv(FParameterID, 1, 1, @Value);
end;

procedure TgxGLSLShaderParameter.SetAsMatrix4f(const Value: TMatrix4f);
begin
  glUniformMatrix4fv(FParameterID, 1, 1, @Value);
end;

procedure TgxGLSLShaderParameter.SetAsVector1f(const Value: Single);
begin
  glUniform1f(FParameterID, Value);
end;

procedure TgxGLSLShaderParameter.SetAsVector1i(const Value: Integer);
begin
  glUniform1i(FParameterID, Value);
end;

procedure TgxGLSLShaderParameter.SetAsVector2f(const Value: TVector2f);
begin
  glUniform2f(FParameterID, Value.X, Value.Y);
end;

procedure TgxGLSLShaderParameter.SetAsVector2i(const Value: TVector2i);
begin
  glUniform2i(FParameterID, Value.X, Value.Y);
end;

procedure TgxGLSLShaderParameter.SetAsVector3f(const Value: TVector3f);
begin
  glUniform3f(FParameterID, Value.X, Value.Y, Value.Z);
end;

procedure TgxGLSLShaderParameter.SetAsVector3i(const Value: TVector3i);
begin
  glUniform3i(FParameterID, Value.X, Value.Y, Value.Z);
end;

procedure TgxGLSLShaderParameter.SetAsVector4f(const Value: TVector4f);
begin
  glUniform4f(FParameterID, Value.X, Value.Y, Value.Z, Value.W);
end;

procedure TgxGLSLShaderParameter.SetAsVector4i(const Value: TVector4i);
begin
  glUniform4i(FParameterID, Value.X, Value.Y, Value.Z, Value.W);
end;

function TgxGLSLShaderParameter.GetAsUniformBuffer: GLenum;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TgxGLSLShaderParameter.GetAsVector1ui: GLuint;
begin
  glGetUniformuiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TgxGLSLShaderParameter.SetAsVector1ui(const Value: GLuint);
begin
  glUniform1ui(FParameterID, Value);
end;

function TgxGLSLShaderParameter.GetAsVector2ui: TVector2ui;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TgxGLSLShaderParameter.SetAsVector2ui(const Value: TVector2ui);
begin
  glUniform2ui(FParameterID, Value.X, Value.Y);
end;

function TgxGLSLShaderParameter.GetAsVector3ui: TVector3ui;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TgxGLSLShaderParameter.SetAsVector3ui(const Value: TVector3ui);
begin
  glUniform3ui(FParameterID, Value.X, Value.Y, Value.Z);
end;

function TgxGLSLShaderParameter.GetAsVector4ui: TVector4ui;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TgxGLSLShaderParameter.SetAsVector4ui(const Value: TVector4ui);
begin
  glUniform4ui(FParameterID, Value.X, Value.Y, Value.Z, Value.W);
end;

procedure TgxGLSLShaderParameter.SetAsUniformBuffer(UBO: Cardinal);
begin
  CurrentContext.gxStates.UniformBufferBinding := UBO;
  glUniformBufferEXT(FGLSLProg.Handle, FParameterID, UBO);
end;

//=======================================================
initialization
//=======================================================

  RegisterClasses([TgxCustomGLSLShader, TgxGLSLShader]);

end.
