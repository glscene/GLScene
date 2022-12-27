//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit Cg.Shader;

(* Base Cg shader classes *)

interface

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,

  GLS.OpenGLTokens,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.VectorTypes,
  GLS.Texture,
  GLS.Strings,
  GLS.Cadencer,
  GLS.Context,
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.Material,
  GLS.TextureFormat,

  Cg.Import,
  Cg.GL;

{.$I GLScene.inc}
{ .$DEFINE OutputCompilerWarnings }

(* Define OutputCompilerWarnings to output Cg compiler warnings to a file. Useful
  for detecting bugs caused by using uninitialized value, implicit type cast, etc. *)
type
  ECgShaderException = class(EGLShaderException);

  TCustomCgShader = class;
  TCgProgram = class;
  TCgParameter = class;
  TCgApplyEvent = procedure(CgProgram: TCgProgram; Sender: TObject) of object;
  TCgUnApplyEvent = procedure(CgProgram: TCgProgram) of object;
  TCgShaderEvent = procedure(CgShader: TCustomCgShader) of object;
  TcgProgramType = (ptVertex, ptFragment);
  // Available vertex program profile
  TCgVPProfile = (vpDetectLatest, vp20, vp30, vp40, arbvp1);
  // Available fragment program profile
  TCgFPProfile = (fpDetectLatest, fp20, fp30, fp40, arbfp1);
  TPrecisionSetting = (psFull, psFast);

  // Wrapper around a Cg program.
  TCgProgram = class(TGLUpdateAbleObject)
  private
    FCgContext: PcgContext;
    FCode: TStrings; // the Cg program itself
    FProgramName: String;
    FHandle: PCGprogram;
    FParams: TList;
    FOnApply: TCgApplyEvent;
    FOnUnApply: TCgUnApplyEvent;
    FOnProgramChanged: TNotifyEvent;
    FEnabled: boolean;
    FDetectProfile: boolean;
    FPrecision: TPrecisionSetting;
    procedure SetPrecision(const Value: TPrecisionSetting);
    function GetManualNotification: boolean;
    procedure SetManualNotification(const Value: boolean);
  protected
    FProgramType: TcgProgramType;
    FProfile: TcgProfile;
    procedure SetCode(const val: TStrings);
    procedure SetProgramName(const val: String);
    function GetParam(index: String): TCgParameter;
    procedure AddParamsItem(const Param: PCGParameter);
    (* Build a list of parameters used in the shader code.
      Iteratively queries all parameters so that we can manage and access them
      easily. Currently only collects leaf parameters i.e. data structure is
      not retrieved. *)
    procedure BuildParamsList;
    procedure ClearParamsList;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function GetLatestProfile: TcgProfile; virtual; abstract;
    procedure Initialize; virtual;
    procedure Finalize;
    procedure Apply(var rci: TGLRenderContextInfo; Sender: TObject);
    procedure UnApply(var rci: TGLRenderContextInfo);
    // ParamByName returns CgParameter; returns nil if not found.
    function ParamByName(const name: String): TCgParameter;
    (* Use Param instead of ParamByName if you want implicit check for the
      existence of your requested parameter. *)
    property Param[index: String]: TCgParameter read GetParam;
    property Params: TList read FParams;
    // Returns a handle to a Cg parameter
    function DirectParamByName(const name: String): PCGParameter;
    function ParamCount: Integer;
    function GetProfileStringA: string;
    procedure LoadFromFile(const fileName: String);
    procedure ListCompilation(Output: TStrings);
    procedure ListParameters(Output: TStrings);
    // shorthands for accessing parameters
    procedure SetParam(ParamName: string; SingleVal: Single); overload;
    procedure SetParam(ParamName: string;
      const Vector2fVal: TVector2f); overload;
    procedure SetParam(ParamName: string;
      const Vector3fVal: TVector3f); overload;
    procedure SetParam(ParamName: string;
      const Vector4fVal: TVector4f); overload;
    procedure SetStateMatrix(ParamName: string; matrix, Transform: Cardinal);
    procedure SetTexture(ParamName: string; TextureID: Cardinal);
    // retruns ShaderName.[program type].ProgramName
    function LongName: string;
    (* Direct access to the profile.
      Set Profile of the sub-classes to any but DetectLatest if you want to
      specify the profile directly. *)
    property DirectProfile: TcgProfile read FProfile write FProfile;
    // Seams, that this event is never called. Probably should be deleted...
    property OnProgramChanged: TNotifyEvent read FOnProgramChanged
      write FOnProgramChanged;
    // If True, that shader is not reset when TCgProgram' parameters change.
    property ManualNotification: boolean read GetManualNotification
      write SetManualNotification default False;
  published
    property Code: TStrings read FCode write SetCode;
    property ProgramName: String read FProgramName write SetProgramName;
    property Enabled: boolean read FEnabled write FEnabled default True;
    (* Precision controls data precision of GPU operation.
      Possible options are 16-bit (psFast) or 32-bit (psFull). 16-bit operation
      is generally faster. *)
    property Precision: TPrecisionSetting read FPrecision write SetPrecision
      default psFull;
    property OnApply: TCgApplyEvent read FOnApply write FOnApply;
    property OnUnApply: TCgUnApplyEvent read FOnUnApply write FOnUnApply;
  end;

  // Wrapper around a Cg parameter of the main program.
  TCgParameter = class(TObject)
  private
    FOwner: TCgProgram;
    FName: String;
    FHandle: PCGParameter;
    FValueType: TCGtype; // e.g. CG_FLOAT
    FDirection: TCGenum; // e.g. CG_INOUT
    FVariability: TCGenum; // e.g. CG_UNIFORM
  protected
    function TypeMismatchMessage: string;
    procedure CheckValueType(aType: TCGtype); overload;
    procedure CheckValueType(const types: array of TCGtype); overload;
    procedure CheckAllTextureTypes;
    procedure CheckAllScalarTypes;
    procedure CheckAllVector2fTypes;
    procedure CheckAllVector3fTypes;
    procedure CheckAllVector4fTypes;
    procedure SetAsVector2f(const val: TVector2f);
    procedure SetAsVector3f(const val: TVector3f);
    procedure SetAsVector4f(const val: TVector4f);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    (* Procedures for setting uniform pamareters.
      Implicitly check for data type. *)
    procedure SetAsScalar(const val: Single); overload;
    procedure SetAsScalar(const val: boolean); overload;
    procedure SetAsVector(const val: TVector2f); overload;
    procedure SetAsVector(const val: TVector3f); overload;
    procedure SetAsVector(const val: TVector4f); overload;
    (* This overloaded SetAsVector accepts open array as input. e.g.
      SetAsVector([0.1, 0.2]). Array length must between 1-4. *)
    procedure SetAsVector(const val: array of Single); overload;
    procedure SetAsStateMatrix(matrix, Transform: Cardinal);
    procedure SetAsMatrix(const val: TMatrix4f);

    (* Procedures for dealing with texture pamareters. *)
    // SetAsTexture checks for all texture types
    procedure SetAsTexture(TextureID: Cardinal);
    // SetAsTexture* check for specific type
    procedure SetAsTexture1D(TextureID: Cardinal);
    procedure SetAsTexture2D(TextureID: Cardinal);
    procedure SetAsTexture3D(TextureID: Cardinal);
    procedure SetAsTextureCUBE(TextureID: Cardinal);
    procedure SetAsTextureRECT(TextureID: Cardinal);
    // SetToTextureOf determines texture type on-the-fly.
    procedure SetToTextureOf(LibMaterial: TGLLibMaterial);
    procedure EnableTexture;
    procedure DisableTexture;
    // Procedures for setting varying parameters with an array of values.
    procedure SetParameterPointer(Values: TGLVectorList); overload;
    procedure SetParameterPointer(Values: TGLAffineVectorList); overload;
    procedure EnableClientState;
    procedure DisableClientState;
    // LongName retruns ShaderName.[program type].ProgramName.ParamName.
    function LongName: string;
    property Owner: TCgProgram read FOwner;
    property Name: String read FName;
    property ValueType: TCGtype read FValueType;
    property Handle: PCGParameter read FHandle write FHandle;
    property Direction: TCGenum read FDirection write FDirection;
    property Variability: TCGenum read FVariability write FVariability;
    // GLScene-friendly properties
    property AsVector: TGLVector write SetAsVector4f; // position f.i.
    property AsAffineVector: TAffineVector write SetAsVector3f; // normal f.i.
    property AsVector2f: TVector2f write SetAsVector2f; // texCoord f.i.
  end;

  TCgVertexProgram = class(TCgProgram)
  private
    FVPProfile: TCgVPProfile;
    procedure SetVPProfile(v: TCgVPProfile);
  public
    constructor Create(AOwner: TPersistent); override;
    function GetLatestProfile: TcgProfile; override;
  published
    property Profile: TCgVPProfile read FVPProfile write SetVPProfile
      default vpDetectLatest;
  end;

  TCgFragmentProgram = class(TCgProgram)
  private
    FFPProfile: TCgFPProfile;
    FManageTexture: boolean;
    procedure SetFPProfile(v: TCgFPProfile);
    procedure SetManageTexture(const Value: boolean);
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Initialize; override;
    function GetLatestProfile: TcgProfile; override;
  published
    property Profile: TCgFPProfile read FFPProfile write SetFPProfile
      default fpDetectLatest;
    // Switch for auto enabling of texture parameters (Cg 1.2 feature)
    // With Cg 1.2.1, default is OFF
    property ManageTexture: boolean read FManageTexture write SetManageTexture
      default False;
  end;

  TCustomCgShader = class(TGLShader)
  private
    FVertexProgram: TCgVertexProgram;
    FFragmentProgram: TCgFragmentProgram;
    FOnInitialize: TCgShaderEvent;
    FDesignEnable: boolean;
  protected
    // Vertex Program
    procedure SetVertexProgram(const val: TCgVertexProgram);
    procedure SetOnApplyVertexProgram(const val: TCgApplyEvent);
    function GetOnApplyVertexProgram: TCgApplyEvent;
    procedure SetOnUnApplyVertexProgram(const val: TCgUnApplyEvent);
    function GetOnUnApplyVertexProgram: TCgUnApplyEvent;
    // Fragment Program
    procedure SetFragmentProgram(const val: TCgFragmentProgram);
    procedure SetOnApplyFragmentProgram(const val: TCgApplyEvent);
    function GetOnApplyFragmentProgram: TCgApplyEvent;
    procedure SetOnUnApplyFragmentProgram(const val: TCgUnApplyEvent);
    function GetOnUnApplyFragmentProgram: TCgUnApplyEvent;
    // OnInitialize
    function GetOnInitialize: TCgShaderEvent;
    procedure SetOnInitialize(const val: TCgShaderEvent);
    procedure DoInitialize(var rci: TGLRenderContextInfo;
      Sender: TObject); override;
    procedure DoFinalize; override;
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): boolean; override;
    // IsProfileSupported to be obsoleted by global function IsCgProfileSupported
    function IsProfileSupported(Profile: TcgProfile): boolean;

    (* Everything is moved here from the public and protected sections
      because I would like to shield end-users of descendant shader
      classes from all this stuff. Those who want direct access
      to shader events and parameters should use the TCgShader class,
      where everything is published. *)
    property OnApplyVP: TCgApplyEvent read GetOnApplyVertexProgram
      write SetOnApplyVertexProgram;
    property OnApplyFP: TCgApplyEvent read GetOnApplyFragmentProgram
      write SetOnApplyFragmentProgram;
    property OnUnApplyVP: TCgUnApplyEvent read GetOnUnApplyVertexProgram
      write SetOnUnApplyVertexProgram;
    property OnUnApplyFP: TCgUnApplyEvent read GetOnUnApplyFragmentProgram
      write SetOnUnApplyFragmentProgram;
    (* OnInitialize can be use to set parameters that need to be set once only.
      See demo "Cg Texture" for example. *)
    property OnInitialize: TCgShaderEvent read GetOnInitialize
      write SetOnInitialize;
    property DesignEnable: boolean read FDesignEnable write FDesignEnable
      default False;
    property VertexProgram: TCgVertexProgram read FVertexProgram
      write SetVertexProgram;
    property FragmentProgram: TCgFragmentProgram read FFragmentProgram
      write SetFragmentProgram;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadShaderPrograms(const VPFilename, FPFilename: string);
    function ShaderSupported: boolean; override;
  end;

  // Allows to use a Cadencer, which is used for noise generation in many shaders.
  TCadencableCustomCgShader = class(TCustomCgShader)
  private
    FCadencer: TGLCadencer;
    procedure SetCadencer(const Value: TGLCadencer);
  protected
    procedure DoInitialize(var rci: TGLRenderContextInfo;
      Sender: TObject); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    property Cadencer: TGLCadencer read FCadencer write SetCadencer;
  end;

  TCgShader = class(TCustomCgShader)
  published
    property DesignEnable;
    property ShaderStyle;
    property FailedInitAction;
    property VertexProgram;
    property FragmentProgram;
    property OnApplyVP;
    property OnApplyFP;
    property OnUnApplyVP;
    property OnUnApplyFP;
    property OnInitialize;
  end;

  // global variables/functions
var
  (* Set IncludeFilePath to indicate where to find your include file for your
    Cg source files. This avoids error from the Cg Compiler when the current
    directory is not the right path as the shader is being compiled. *)
  IncludeFilePath: string;
{$IFDEF OutputCompilerWarnings}
  (* Edit the string WarningFilePath for the output filename. Default
    WarningFilePath is set to application path. *)
  WarningFilePath: string;
{$ENDIF}
  // Misc. global functions
function IsCgProfileSupported(Profile: TcgProfile): boolean;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

const
  CgBoolean: array [False .. True] of TCGbool = (CG_FALSE, CG_TRUE);

var
  vCgContextCount: Integer;
  CurCgProgram: TCgProgram; // for reporting error
{$IFDEF OutputCompilerWarnings}
  CompilerMsg: TStringList; // useful for seeing compiler warnings
{$ENDIF}

function IsCgProfileSupported(Profile: TcgProfile): boolean;
begin
  result := cgGLIsProfileSupported(Profile) = CG_TRUE;
end;

{$IFDEF OutputCompilerWarnings}

procedure RecordWarnings;
begin
  with CurCgProgram do
    CompilerMsg.Add('[' + LongName + '] ' + cgGetErrorString(cgGetError) + #10 +
      cgGetLastListing(FCgContext));
end;
{$ENDIF}

procedure ErrorCallBack; cdecl;
var
  Msg: string;
begin
  with CurCgProgram do
    Msg := '[' + LongName + '] ' + String(cgGetErrorString(cgGetError)) + #10 +
      String(cgGetLastListing(FCgContext));
  raise ECgShaderException.Create(Msg);
end;

// ------------------
// ------------------ TCgProgram ------------------
// ------------------

constructor TCgProgram.Create(AOwner: TPersistent);
begin
  inherited;
  FCode := TStringList.Create;
  TStringList(FCode).OnChange := NotifyChange;
  FParams := TList.Create;
  FEnabled := True;
  FDetectProfile := True;
end;

destructor TCgProgram.Destroy;
begin
  inherited Destroy;
  Assert((FParams.Count = 0), '[' + LongName + ']: bug! params unbound!');
  ClearParamsList;
  FParams.Free;
  FCode.Free;
end;

procedure TCgProgram.SetCode(const val: TStrings);
begin
  FCode.Assign(val);
end;

procedure TCgProgram.LoadFromFile(const fileName: String);
begin
  Code.LoadFromFile(fileName);
end;

procedure TCgProgram.SetProgramName(const val: String);
begin
  if val <> FProgramName then
  begin
    FProgramName := val;
    if not GetManualNotification then
      NotifyChange(Self);
  end;
end;

procedure TCgProgram.AddParamsItem(const Param: PCGParameter);
var
  newParamObj: TCgParameter;
begin
  newParamObj := TCgParameter.Create;
  with newParamObj do
  begin
    FOwner := Self;
    FName := { StrPas } String(cgGetParameterName(Param));
    FHandle := Param;
    FValueType := cgGetParameterType(Param);
    FDirection := cgGetParameterDirection(Param);
    FVariability := cgGetParameterVariability(Param);
  end;
  FParams.Add(newParamObj);
end;

procedure TCgProgram.BuildParamsList;
var
  CurParam: PCGParameter;
begin
  ClearParamsList;
  CurParam := cgGetFirstLeafParameter(FHandle, CG_PROGRAM);

  // build params list
  while Assigned(CurParam) do
  begin
    AddParamsItem(CurParam);
    CurParam := cgGetNextLeafParameter(CurParam);
  end;
end;

procedure TCgProgram.ClearParamsList;
var
  i: Integer;
begin
  for i := FParams.Count - 1 downto 0 do
    TCgParameter(FParams[i]).Free;
  FParams.Clear;
end;

function TCgProgram.GetParam(index: String): TCgParameter;
begin
  result := ParamByName(index);
  Assert(result <> nil, '[' + LongName + ']: Parameter "' +
    index + '" not found.');
end;

function TCgProgram.ParamByName(const name: String): TCgParameter;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to FParams.Count - 1 do
  begin
    if TCgParameter(FParams.Items[i]).name = name then
    begin
      result := TCgParameter(FParams.Items[i]);
      Exit;
    end;
  end;
end;

function TCgProgram.DirectParamByName(const name: String): PCGParameter;
begin
  result := cgGetNamedParameter(FHandle, PCharCG(StringCG(name)));
end;

function TCgProgram.ParamCount: Integer;
begin
  result := FParams.Count;
end;

procedure TCgProgram.Initialize;
var
  buf: StringCG;
  Arg: array of PCharCG;
  PArg: PPCharCG;
begin
  Assert(FCgContext = nil);

  buf := StringCG(Trim(Code.Text));
  if buf = '' then
    Exit;

  if Precision = psFast then
  begin
    setlength(Arg, 2);
    Arg[0] := PCharCG('-fastprecision');
    Arg[1] := nil;
    PArg := @Arg[0];
  end
  else
    PArg := nil;

  // To force 'if' statement, use sth. like:
  // setlength(Arg, 3);
  // Arg[0]:=PChar('-ifcvt');
  // Arg[1]:=PChar('none');
  // Arg[2]:=nil;
  // PArg:=@Arg[0];

  // get a new context
  FCgContext := cgCreateContext;
  Inc(vCgContextCount);
  CurCgProgram := Self;
  try
    if IncludeFilePath <> '' then
      SetCurrentDir(IncludeFilePath);
    if FDetectProfile then
      FProfile := GetLatestProfile;
    cgGLSetOptimalOptions(FProfile);
    if FProgramName = '' then
      FProgramName := 'main'; // default program name
    FHandle := cgCreateProgram(FCgContext, CG_SOURCE, PCharCG(buf), FProfile,
      PCharCG(StringCG(FProgramName)), PArg);
    cgGLLoadProgram(FHandle);
    // build parameter list for the selected program
    BuildParamsList;
{$IFDEF OutputCompilerWarnings}
    RecordWarnings;
{$ENDIF}
  except
    cgDestroyContext(FCgContext);
    FCgContext := nil;
    Dec(vCgContextCount);
    raise;
  end;
end;

procedure TCgProgram.Finalize;
begin
  if not Assigned(FCgContext) then
    Exit;

  FProgramName := '';
  ClearParamsList;
  cgDestroyContext(FCgContext);
  FCgContext := nil;
  FHandle := nil; // $added - 29/04/2006 - PhP
  Dec(vCgContextCount);
end;

procedure TCgProgram.Apply(var rci: TGLRenderContextInfo; Sender: TObject);
begin
  if not Assigned(FHandle) then
    Exit;
  if not FEnabled then
    Exit;

  CurCgProgram := Self;

  cgGLBindProgram(FHandle);
  cgGLEnableProfile(FProfile);

  if Assigned(FOnApply) then
    FOnApply(Self, Sender);
end;

procedure TCgProgram.UnApply(var rci: TGLRenderContextInfo);
begin
  if not Assigned(FHandle) then
    Exit;
  if not FEnabled then
    Exit;
  if Assigned(FOnUnApply) then
    FOnUnApply(Self);
  cgGLDisableProfile(FProfile);
end;

function TCgProgram.GetProfileStringA: string;
begin
  result := String(cgGetProfileString(FProfile));
end;

procedure TCgProgram.ListParameters(Output: TStrings);
var
  i: Integer;
begin
  Output.Clear;
  for i := 0 to ParamCount - 1 do
    Output.Add(TCgParameter(FParams[i]).name);
end;

procedure TCgProgram.ListCompilation(Output: TStrings);

  procedure OutputAsTStrings(s: String);
  var
    i: Integer;
  begin
    while Length(s) > 0 do
    begin
      i := Pos(#10, s);
      if i = 0 then
        i := 255;
      Output.Add(Copy(s, 1, i - 1));
      Delete(s, 1, i);
    end;
  end;

begin
  Output.BeginUpdate;
  Output.Clear;
  if FCgContext <> nil then
    OutputAsTStrings(String(cgGetProgramString(FHandle, CG_COMPILED_PROGRAM)))
  else
    Output.Add('Cg program not yet initialized');
  Output.EndUpdate;
end;

procedure TCgProgram.SetParam(ParamName: string; const Vector3fVal: TVector3f);
begin
  ParamByName(ParamName).SetAsVector3f(Vector3fVal);
end;

procedure TCgProgram.SetParam(ParamName: string; const Vector2fVal: TVector2f);
begin
  ParamByName(ParamName).SetAsVector2f(Vector2fVal);
end;

procedure TCgProgram.SetParam(ParamName: string; SingleVal: Single);
begin
  Param[ParamName].SetAsScalar(SingleVal);
end;

procedure TCgProgram.SetParam(ParamName: string; const Vector4fVal: TVector4f);
begin
  ParamByName(ParamName).SetAsVector4f(Vector4fVal);
end;

procedure TCgProgram.SetStateMatrix(ParamName: string;
  matrix, Transform: Cardinal);
begin
  ParamByName(ParamName).SetAsStateMatrix(matrix, Transform);
end;

procedure TCgProgram.SetTexture(ParamName: string; TextureID: Cardinal);
begin
  ParamByName(ParamName).SetAsTexture(TextureID);
end;

function TCgProgram.LongName: string;
const
  ProTypeStr: array [ptVertex .. ptFragment] of string = ('VP', 'FP');
begin
  result := (Owner as TCgShader).name + '.' + ProTypeStr[FProgramType] + '.' +
    ProgramName;
end;

procedure TCgProgram.SetPrecision(const Value: TPrecisionSetting);
begin
  if FPrecision <> Value then
  begin
    FPrecision := Value;
    if not GetManualNotification then
      NotifyChange(Self);
  end;
end;

function TCgProgram.GetManualNotification: boolean;
begin
  result := not Assigned(TStringList(FCode).OnChange);
end;

procedure TCgProgram.SetManualNotification(const Value: boolean);
begin
  if Value = GetManualNotification then
    Exit;
  if Value then
    TStringList(FCode).OnChange := nil
  else
    TStringList(FCode).OnChange := NotifyChange;
end;

// ------------------
// ------------------ TCgParameter ------------------
// ------------------

constructor TCgParameter.Create;
begin
  inherited;
end;

destructor TCgParameter.Destroy;
begin
  inherited;
end;

function TCgParameter.LongName: string;
begin
  result := Owner.LongName + '.' + FName;
end;

function TCgParameter.TypeMismatchMessage: string;
begin
  result := '[' + LongName + ']: Parameter type mismatch.';
end;

procedure TCgParameter.CheckValueType(aType: TCGtype);
begin
  Assert(aType = FValueType, TypeMismatchMessage);
end;

procedure TCgParameter.CheckValueType(const types: array of TCGtype);
  function DoCheck: boolean;
  var
    i: Integer;
  begin
    result := False;
    for i := Low(types) to High(types) do
      if FValueType = types[i] then
      begin
        result := True;
        Break;
      end;
  end;

begin
  Assert(DoCheck, TypeMismatchMessage);
end;

procedure TCgParameter.CheckAllScalarTypes;
begin
  CheckValueType([CG_FLOAT, CG_HALF, CG_FIXED, CG_BOOL]);
end;

procedure TCgParameter.CheckAllTextureTypes;
begin
  CheckValueType([CG_SAMPLER2D, CG_SAMPLER1D, CG_SAMPLERRECT, CG_SAMPLERCUBE,
    CG_SAMPLER3D]);
end;

procedure TCgParameter.CheckAllVector2fTypes;
begin
  CheckValueType([CG_FLOAT2, CG_HALF2, CG_FIXED2]);
end;

procedure TCgParameter.CheckAllVector3fTypes;
begin
  CheckValueType([CG_FLOAT3, CG_HALF3, CG_FIXED3]);
end;

procedure TCgParameter.CheckAllVector4fTypes;
begin
  CheckValueType([CG_FLOAT4, CG_HALF4, CG_FIXED4]);
end;

procedure TCgParameter.SetAsScalar(const val: Single);
begin
  CheckAllScalarTypes;
  cgGLSetParameter1f(FHandle, val);
end;

procedure TCgParameter.SetAsScalar(const val: boolean);
const
  BoolToFloat: array [False .. True] of Single = (CG_FALSE, CG_TRUE);
begin
  SetAsScalar(BoolToFloat[val]);
end;

procedure TCgParameter.SetAsVector2f(const val: TVector2f);
begin
  CheckAllVector2fTypes;
  cgGLSetParameter2fv(FHandle, @val);
end;

procedure TCgParameter.SetAsVector3f(const val: TVector3f);
begin
  CheckAllVector3fTypes;
  cgGLSetParameter3fv(FHandle, @val);
end;

procedure TCgParameter.SetAsVector4f(const val: TVector4f);
begin
  CheckAllVector4fTypes;
  cgGLSetParameter4fv(FHandle, @val);
end;

procedure TCgParameter.SetAsVector(const val: TVector2f);
begin
  SetAsVector2f(val);
end;

procedure TCgParameter.SetAsVector(const val: TVector3f);
begin
  SetAsVector3f(val);
end;

procedure TCgParameter.SetAsVector(const val: TVector4f);
begin
  SetAsVector4f(val);
end;

procedure TCgParameter.SetAsVector(const val: array of Single);
begin
  case high(val) of
    0:
      SetAsScalar(val[0]);
    1:
      begin
        CheckAllVector2fTypes;
        cgGLSetParameter2fv(FHandle, @val);
      end;
    2:
      begin
        CheckAllVector3fTypes;
        cgGLSetParameter3fv(FHandle, @val);
      end;
    3:
      begin
        CheckAllVector4fTypes;
        cgGLSetParameter4fv(FHandle, @val);
      end;
  else
    Assert(False, 'Vector length must be between 1 to 4');
  end;
end;

procedure TCgParameter.SetAsTexture(TextureID: Cardinal);
begin
  CheckAllTextureTypes;
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTexture1D(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLER1D);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTexture2D(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLER2D);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTexture3D(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLER3D);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTextureRECT(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLERRECT);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTextureCUBE(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLERCUBE);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetToTextureOf(LibMaterial: TGLLibMaterial);
var
  TexType: TCGtype;
begin
  case LibMaterial.Material.Texture.Image.NativeTextureTarget of
    ttTexture2D:
      TexType := CG_SAMPLER2D;
    ttTextureCUBE:
      TexType := CG_SAMPLER2D;
    ttTextureRECT:
      TexType := CG_SAMPLERRECT;
    ttTexture1D:
      TexType := CG_SAMPLER1D;
    ttTexture3D:
      TexType := CG_SAMPLER3D;
  else
    begin
      Assert(False, 'Unknown texture target');
      TexType := CG_SAMPLER2D; // to subpress compilation warning
    end;
  end;
  CheckValueType(TexType);
  cgGLSetTextureParameter(FHandle, LibMaterial.Material.Texture.Handle);
end;

procedure TCgParameter.DisableTexture;
begin
  CheckAllTextureTypes;
  cgGLDisableTextureParameter(FHandle);
end;

procedure TCgParameter.EnableTexture;
begin
  CheckAllTextureTypes;
  cgGLEnableTextureParameter(FHandle);
end;

procedure TCgParameter.SetAsStateMatrix(matrix, Transform: Cardinal);
// Assuming values of matrix types are contiguous to simplify the type checking
const
  MinFloatA = CG_FLOAT1x1;
  MaxFloatA = CG_FLOAT4x4;
  MinHalfA = CG_HALF1x1;
  MaxHalfA = CG_HALF4x4;
  MinFixedA = CG_FIXED1x1;
  MaxFixedA = CG_FIXED4x4;
begin
  Assert(((FValueType >= MinFloatA) and (FValueType <= MaxFloatA) or
    (FValueType >= MinHalfA) and (FValueType <= MaxHalfA) or
    (FValueType >= MinFixedA) and (FValueType <= MaxFixedA)),
    TypeMismatchMessage);
  cgGLSetStateMatrixParameter(FHandle, matrix, Transform);
end;

procedure TCgParameter.SetAsMatrix(const val: TMatrix4f);
begin
  cgGLSetMatrixParameterfr(FHandle, @val);
end;

procedure TCgParameter.DisableClientState;
begin
  Assert(FVariability = CG_VARYING);
  cgGLDisableClientState(FHandle);
end;

procedure TCgParameter.EnableClientState;
begin
  Assert(FVariability = CG_VARYING);
  cgGLEnableClientState(FHandle);
end;

procedure TCgParameter.SetParameterPointer(Values: TGLAffineVectorList);
begin
  Assert(FVariability = CG_VARYING);
  cgGLSetParameterPointer(FHandle, 3, GL_FLOAT, 0, Values.List);
end;

procedure TCgParameter.SetParameterPointer(Values: TGLVectorList);
begin
  Assert(FVariability = CG_VARYING);
  cgGLSetParameterPointer(FHandle, 4, GL_FLOAT, 0, Values.List);
end;

// ------------------
// ------------------ TCgVertexProgram ------------------
// ------------------

constructor TCgVertexProgram.Create;
begin
  inherited;
  FProgramType := ptVertex;
  FVPProfile := vpDetectLatest;
end;

function TCgVertexProgram.GetLatestProfile: TcgProfile;
begin
  result := cgGLGetLatestProfile(CG_GL_VERTEX);
end;

procedure TCgVertexProgram.SetVPProfile(v: TCgVPProfile);
begin
  if FVPProfile = v then
    Exit;
  FVPProfile := v;
  case v of
    vp20:
      FProfile := CG_PROFILE_VP20;
    vp30:
      FProfile := CG_PROFILE_VP30;
    vp40:
      FProfile := CG_PROFILE_VP40;
    arbvp1:
      FProfile := CG_PROFILE_ARBVP1;
  end;

  FDetectProfile := v = vpDetectLatest;
end;

// ------------------
// ------------------ TCgFragmentProgram ------------------
// ------------------

constructor TCgFragmentProgram.Create;
begin
  inherited;
  FProgramType := ptFragment;
  FFPProfile := fpDetectLatest;
  FManageTexture := False;
end;

procedure TCgFragmentProgram.SetManageTexture(const Value: boolean);
begin
  FManageTexture := Value;
  if FCgContext <> nil then
    cgGLSetManageTextureParameters(@FCgContext, CgBoolean[FManageTexture]);
  // If FCgContext = nil (i.e. program not yet initialized), set it in
  // TCgFragmentProgram.Initialize
end;

procedure TCgFragmentProgram.Initialize;
begin
  inherited;
  if FManageTexture then // ManageTexture is off by default
    cgGLSetManageTextureParameters(@FCgContext, CgBoolean[FManageTexture]);
end;

function TCgFragmentProgram.GetLatestProfile: TcgProfile;
begin
  result := cgGLGetLatestProfile(CG_GL_FRAGMENT);
end;

procedure TCgFragmentProgram.SetFPProfile(v: TCgFPProfile);
begin
  if FFPProfile = v then
    Exit;
  FFPProfile := v;
  case v of
    fp20:
      FProfile := CG_PROFILE_FP20;
    fp30:
      FProfile := CG_PROFILE_FP30;
    fp40:
      FProfile := CG_PROFILE_FP40;
    arbfp1:
      FProfile := CG_PROFILE_ARBFP1;
  end;
  FDetectProfile := v = fpDetectLatest;
end;

// ------------------
// ------------------ TCustomCgShader ------------------
// ------------------

constructor TCustomCgShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVertexProgram := TCgVertexProgram.Create(Self);
  FFragmentProgram := TCgFragmentProgram.Create(Self);
end;

destructor TCustomCgShader.Destroy;
begin
  inherited Destroy;
  FVertexProgram.Free;
  FFragmentProgram.Free;
end;

procedure TCustomCgShader.SetVertexProgram(const val: TCgVertexProgram);
begin
  FVertexProgram.Code := val.Code;
end;

procedure TCustomCgShader.SetFragmentProgram(const val: TCgFragmentProgram);
begin
  FFragmentProgram.Code := val.Code;
end;

procedure TCustomCgShader.SetOnApplyVertexProgram(const val: TCgApplyEvent);
begin
  FVertexProgram.OnApply := val;
end;

function TCustomCgShader.GetOnApplyVertexProgram: TCgApplyEvent;
begin
  result := FVertexProgram.OnApply;
end;

procedure TCustomCgShader.SetOnApplyFragmentProgram(const val: TCgApplyEvent);
begin
  FFragmentProgram.OnApply := val;
end;

function TCustomCgShader.GetOnApplyFragmentProgram: TCgApplyEvent;
begin
  result := FFragmentProgram.OnApply;
end;

procedure TCustomCgShader.SetOnUnApplyVertexProgram(const val: TCgUnApplyEvent);
begin
  FVertexProgram.OnUnApply := val;
end;

function TCustomCgShader.GetOnUnApplyVertexProgram: TCgUnApplyEvent;
begin
  result := FVertexProgram.OnUnApply;
end;

procedure TCustomCgShader.SetOnUnApplyFragmentProgram
  (const val: TCgUnApplyEvent);
begin
  FFragmentProgram.OnUnApply := val;
end;

function TCustomCgShader.GetOnUnApplyFragmentProgram: TCgUnApplyEvent;
begin
  result := FFragmentProgram.OnUnApply;
end;

function TCustomCgShader.GetOnInitialize: TCgShaderEvent;
begin
  result := FOnInitialize;
end;

procedure TCustomCgShader.SetOnInitialize(const val: TCgShaderEvent);
begin
  FOnInitialize := val;
end;

procedure TCustomCgShader.DoInitialize(var rci: TGLRenderContextInfo;
  Sender: TObject);
begin
  if (csDesigning in ComponentState) and (not FDesignEnable) then
    Exit;

  if not ShaderSupported then
  begin
    Enabled := False;
    HandleFailedInitialization;
  end
  else
    try
      FVertexProgram.Initialize;
      FFragmentProgram.Initialize;
      if Assigned(FOnInitialize) then
        FOnInitialize(Self);
    except
      on E: Exception do
      begin
        Enabled := False;
        HandleFailedInitialization(E.Message);
      end;
    end;
end;

procedure TCustomCgShader.DoApply(var rci: TGLRenderContextInfo;
  Sender: TObject);
begin
  if (csDesigning in ComponentState) and (not FDesignEnable) then
    Exit;
  FVertexProgram.Apply(rci, Sender);
  FFragmentProgram.Apply(rci, Sender);
end;

function TCustomCgShader.DoUnApply(var rci: TGLRenderContextInfo): boolean;
begin
  if (not(csDesigning in ComponentState)) or FDesignEnable then
  begin
    FVertexProgram.UnApply(rci);
    FFragmentProgram.UnApply(rci);
  end;
  result := False;
end;

procedure TCustomCgShader.DoFinalize;
begin
  FVertexProgram.Finalize;
  FFragmentProgram.Finalize;
end;

procedure TCustomCgShader.LoadShaderPrograms(const VPFilename,
  FPFilename: string);
begin
  VertexProgram.LoadFromFile(VPFilename);
  FragmentProgram.LoadFromFile(FPFilename);
end;

function TCustomCgShader.IsProfileSupported(Profile: TcgProfile): boolean;
begin
  result := cgGLIsProfileSupported(Profile) = CG_TRUE;
end;

function TCustomCgShader.ShaderSupported: boolean;
begin
  result := (GL.ARB_shader_objects and GL.ARB_vertex_program and
    GL.ARB_vertex_shader and GL.ARB_fragment_shader);
end;


// ------------------
// ------------------ TCadencableCustomCgShader ------------------
// ------------------

procedure TCadencableCustomCgShader.DoInitialize(var rci: TGLRenderContextInfo;
  Sender: TObject);
begin
  if FCadencer = nil then
  begin
    Enabled := False;
    raise ECgShaderException.CreateFmt(strErrorEx + strCadencerNotDefinedEx,
      [ClassName]);
  end
  else
    inherited;
end;

procedure TCadencableCustomCgShader.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent is TGLCadencer) and (Operation = opRemove) then
  begin
    FCadencer := nil;
    Enabled := False;
  end;
end;

procedure TCadencableCustomCgShader.SetCadencer(const Value: TGLCadencer);
begin
  if Value = FCadencer then
    Exit;

  if Value = nil then
    if Enabled then
      Enabled := False;

  if FCadencer <> nil then
    FCadencer.RemoveFreeNotification(Self);
  FCadencer := Value;
  if FCadencer <> nil then
    FCadencer.FreeNotification(Self);
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

// class registrations
RegisterClasses([TCgShader, TCustomCgShader, TCadencableCustomCgShader,
  TCgFragmentProgram, TCgVertexProgram, TCgProgram]);

cgSetErrorCallBack(ErrorCallBack);

{$IFDEF OutputCompilerWarnings}
CompilerMsg := TStringList.Create;
// default WarningFilePath is set to app. path
WarningFilePath := extractfilepath(ParamStr(0));
{$ENDIF}

finalization

{$IFDEF OutputCompilerWarnings}
  CompilerMsg.SaveToFile(WarningFilePath + 'CG_Warnings.txt');
CompilerMsg.Free;
{$ENDIF}

end.
