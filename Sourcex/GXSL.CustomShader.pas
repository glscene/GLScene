//
// The graphics engine GXScene https://github.com/glscene
//
unit GXSL.CustomShader;

(*
    A collection of pure abstract classes - descendants of TgxShader, which are
    used for purpose of not having to write the same stuff all over and over
    again in your own shader classes.
    It also contains a procedures and function that can be used in all shaders.
 *)

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,

  GXS.VectorGeometry,
  GXS.VectorTypes,
  GXS.Texture, 
  GXS.Cadencer, 
  GXS.Scene,
  GXS.Strings, 
  GXS.Context,
  GXS.RenderContextInfo,
  GXS.Material,
  GXS.VectorLists, 
  GXS.TextureFormat,
  GXSL.Parameter;

const
  gxsShaderMaxLightSources = 8;

type
  TgxShaderFogSupport = (sfsEnabled, sfsDisabled, sfsAuto);
  TgxTransformFeedBackMode = (tfbmInterleaved, tfbmSeparate);

  ECustomShaderException = class(EShaderException);

  TgxCustomShader = class;
  TgxVertexProgram = class;
  TgxFragmentProgram = class;
  TgxGeometryProgram = class;

  TgxShaderEvent = procedure(Shader: TgxCustomShader) of object;
  TgxShaderUnAplyEvent = procedure(Shader: TgxCustomShader; var ThereAreMorePasses: Boolean) of object;

  TgxLightSourceEnum = 1..gxsShaderMaxLightSources;
  TgxLightSourceSet = set of TgxLightSourceEnum;

  (* This interface describes user shaders, in order to be able to access them
    via a unified interface. If user shader does not support some option, don't
    raise an axception, just ignore it. *)
  IgxShaderDescription = interface
  ['{04089C64-60C2-43F5-AC9C-38ED46264812}']
    procedure SetShaderTextures(const Textures: array of TgxTexture);
    procedure GetShaderTextures(var Textures: array of TgxTexture);
    procedure SetShaderColorParams(const AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);
    procedure GetShaderColorParams(var AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);
    procedure SetShaderMiscParameters(const ACadencer: TgxCadencer; const AMatLib: TgxMaterialLibrary; const ALightSources: TgxLightSourceSet);
    procedure GetShaderMiscParameters(var ACadencer: TgxCadencer; var AMatLib: TgxMaterialLibrary; var ALightSources: TgxLightSourceSet);
    function GetShaderAlpha: Single;
    procedure SetShaderAlpha(const Value: Single);
    function GetShaderDescription: string;
  end;

  // Used in the TgxPostShaderHolder component.
  IgxPostShader = interface
  ['{68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}']
    // Called on every pass.
    procedure DoUseTempTexture(const TempTexture: TgxTextureHandle;
      TextureTarget: TgxTextureTarget);
    // Called to determine if it is compatible.
    function GetTextureTarget: TgxTextureTarget;
  end;

  // A pure abstract class, must be overriden.
  TgxCustomShader = class(TgxShader)
  private
    FFragmentProgram: TgxFragmentProgram;
    FVertexProgram: TgxVertexProgram;
    FGeometryProgram: TgxGeometryProgram;

    FTagObject: TObject;
    procedure SetFragmentProgram(const Value: TgxFragmentProgram);
    procedure SetGeometryProgram(const Value: TgxGeometryProgram);
    procedure SetVertexProgram(const Value: TgxVertexProgram);
    function StoreFragmentProgram: Boolean;
    function StoreGeometryProgram: Boolean;
    function StoreVertexProgram: Boolean;
  protected
    FDebugMode: Boolean;
    procedure SetDebugMode(const Value: Boolean); virtual;

    property FragmentProgram: TgxFragmentProgram read FFragmentProgram write SetFragmentProgram stored StoreFragmentProgram;
    property VertexProgram: TgxVertexProgram read FVertexProgram write SetVertexProgram stored StoreVertexProgram;
    property GeometryProgram: TgxGeometryProgram read FGeometryProgram write SetGeometryProgram stored StoreGeometryProgram;

    { Treats warnings as errors and displays this error,
       instead of a general shader-not-supported message. }
    property DebugMode: Boolean read FDebugMode write SetDebugMode default False;
    property TagObject: TObject read FTagObject write FTagObject default nil;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadShaderPrograms(const VPFilename, FPFilename: string; GPFilename: string = '');
  end;

  // A custom shader program.
  TgxShaderProgram = class(TPersistent)
  private
    FParent: TgxCustomShader;
    FEnabled: Boolean;
    FCode: TStrings;
    procedure SetCode(const Value: TStrings);
    procedure SetEnabled(const Value: Boolean);
    procedure OnChangeCode(Sender: TObject);
  protected
    function GetOwner: TPersistent; override;
  public
    procedure LoadFromFile(const AFileName: string);
    procedure Apply; virtual;
    constructor Create(const AParent: TgxCustomShader); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Code: TStrings read FCode write SetCode;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
  end;

  TgxVertexProgram = class(TgxShaderProgram)
  published
    property Code;
    property Enabled;
  end;

  TgxFragmentProgram = class(TgxShaderProgram)
  published
    property Code;
    property Enabled;
  end;

  TgxGeometryProgram = class(TgxShaderProgram)
  private
    FInputPrimitiveType: TgxgsInTypes;
    FOutputPrimitiveType: TgxgsOutTypes;
    FVerticesOut: GLint;
    procedure SetInputPrimitiveType(const Value: TgxgsInTypes);
    procedure SetOutputPrimitiveType(const Value: TgxgsOutTypes);
    procedure SetVerticesOut(const Value: GLint);
  public
    constructor Create(const AParent: TgxCustomShader); override;
  published
    property Code;
    property Enabled;

    property InputPrimitiveType: TgxgsInTypes read FInputPrimitiveType write SetInputPrimitiveType default gsInPoints;
    property OutputPrimitiveType: TgxgsOutTypes read FOutputPrimitiveType write SetOutputPrimitiveType default gsOutPoints;
    property VerticesOut: GLint read FVerticesOut write SetVerticesOut default 0;
  end;

  // Wrapper around a parameter of the main program.
  TgxCustomShaderParameter = class(TObject)
  private
    
  protected
    
    function GetAsVector1f: Single; virtual; abstract;
    function GetAsVector2f: TVector2f; virtual; abstract;
    function GetAsVector3f: TVector3f; virtual; abstract;
    function GetAsVector4f: TVector4f; virtual; abstract;

    function GetAsVector1i: Integer; virtual; abstract;
    function GetAsVector2i: TVector2i; virtual; abstract;
    function GetAsVector3i: TVector3i; virtual; abstract;
    function GetAsVector4i: TVector4i; virtual; abstract;

    function GetAsVector1ui: GLuint; virtual; abstract;
    function GetAsVector2ui: TVector2ui; virtual; abstract;
    function GetAsVector3ui: TVector3ui; virtual; abstract;
    function GetAsVector4ui: TVector4ui; virtual; abstract;

    procedure SetAsVector1f(const Value: Single); virtual; abstract;
    procedure SetAsVector2f(const Value: TVector2f); virtual; abstract;
    procedure SetAsVector3f(const Value: TVector3f); virtual; abstract;
    procedure SetAsVector4f(const Value: TVector4f); virtual; abstract;

    procedure SetAsVector1i(const Value: Integer); virtual; abstract;
    procedure SetAsVector2i(const Value: TVector2i); virtual; abstract;
    procedure SetAsVector3i(const Value: TVector3i); virtual; abstract;
    procedure SetAsVector4i(const Value: TVector4i); virtual; abstract;

    procedure SetAsVector1ui(const Value: GLuint); virtual; abstract;
    procedure SetAsVector2ui(const Value: TVector2ui); virtual; abstract;
    procedure SetAsVector3ui(const Value: TVector3ui); virtual; abstract;
    procedure SetAsVector4ui(const Value: TVector4ui); virtual; abstract;

    function GetAsMatrix2f: TMatrix2f; virtual; abstract;
    function GetAsMatrix3f: TMatrix3f; virtual; abstract;
    function GetAsMatrix4f: TMatrix4f; virtual; abstract;
    procedure SetAsMatrix2f(const Value: TMatrix2f); virtual; abstract;
    procedure SetAsMatrix3f(const Value: TMatrix3f); virtual; abstract;
    procedure SetAsMatrix4f(const Value: TMatrix4f); virtual; abstract;

    procedure SetAsTexture(const TextureIndex: Integer;
      const Value: TgxTexture);
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
      TextureTarget: TgxTextureTarget): Cardinal; virtual; abstract;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      TextureTarget: TgxTextureTarget; const Value: Cardinal); virtual; abstract;
    function GetAsUniformBuffer: GLenum; virtual; abstract;
    procedure SetAsUniformBuffer(UBO: GLenum); virtual; abstract;
  public
    

    { This overloaded SetAsVector accepts open array as input. e.g.
       SetAsVectorF([0.1, 0.2]). Array length must between 1-4. }
    procedure SetAsVectorF(const Values: array of Single); overload;
    procedure SetAsVectorI(const Values: array of Integer); overload;

    { SetToTextureOf determines texture type on-the-fly.}
    procedure SetToTextureOf(const LibMaterial: TgxLibMaterial; const TextureIndex: Integer); overload;
    procedure SetToTextureOf(const Texture: TgxTexture; const TextureIndex: Integer); overload;

    // friendly properties.
    property AsVector: TVector4f read GetAsVector4f write SetAsVector4f;
    property AsAffineVector: TAffineVector read GetAsVector3f write SetAsVector3f;

    // Standard types.
    property AsFloat: Single read GetAsVector1f write SetAsVector1f;
    property AsInteger: Integer read GetAsVector1i write SetAsVector1i;

    // Float vector types.
    property AsVector1f: Single    read GetAsVector1f write SetAsVector1f;
    property AsVector2f: TVector2f read GetAsVector2f write SetAsVector2f;
    property AsVector3f: TVector3f read GetAsVector3f write SetAsVector3f;
    property AsVector4f: TVector4f read GetAsVector4f write SetAsVector4f;

    // Integer vector  types.
    property AsVector1i: Integer   read GetAsVector1i write SetAsVector1i;
    property AsVector2i: TVector2i read GetAsVector2i write SetAsVector2i;
    property AsVector3i: TVector3i read GetAsVector3i write SetAsVector3i;
    property AsVector4i: TVector4i read GetAsVector4i write SetAsVector4i;

    // Unsigned integer vector  types.
    property AsVector1ui: GLuint   read GetAsVector1ui write SetAsVector1ui;
    property AsVector2ui: TVector2ui read GetAsVector2ui write SetAsVector2ui;
    property AsVector3ui: TVector3ui read GetAsVector3ui write SetAsVector3ui;
    property AsVector4ui: TVector4ui read GetAsVector4ui write SetAsVector4ui;

    // Matrix Types.
    property AsMatrix2f: TMatrix2f read GetAsMatrix2f write SetAsMatrix2f;
    property AsMatrix3f: TMatrix3f read GetAsMatrix3f write SetAsMatrix3f;
    property AsMatrix4f: TMatrix4f read GetAsMatrix4f write SetAsMatrix4f;

    // Texture Types.
    property AsTexture    [const TextureIndex: Integer]: TgxTexture write SetAsTexture;
    property AsTexture1D  [const TextureIndex: Integer]: TgxTexture write SetAsTexture1D;
    property AsTexture2D  [const TextureIndex: Integer]: TgxTexture write SetAsTexture2D;
    property AsTexture3D  [const TextureIndex: Integer]: TgxTexture write SetAsTexture3D;
    property AsTextureRect[const TextureIndex: Integer]: TgxTexture write SetAsTextureRect;
    property AsTextureCube[const TextureIndex: Integer]: TgxTexture write SetAsTextureCube;

    property AsCustomTexture[const TextureIndex: Integer; TextureTarget: TgxTextureTarget]: Cardinal read GetAsCustomTexture write SetAsCustomTexture;

    property AsUniformBuffer: GLenum read GetAsUniformBuffer write SetAsUniformBuffer;
  end;

  { Adds two more blending modes to standard ones.
    Not sure how to name them or if they should be included in TBlending mode,
    so I created a new type here. }
  TgxBlendingModeEx = (bmxOpaque, bmxTransparency, bmxAdditive,
    bmxAlphaTest50, bmxAlphaTest100, bmxModulate,
    bmxDestColorOne, bmxDestAlphaOne);

// Exported procedures.
procedure ApplyBlendingModeEx(const BlendingMode: TgxBlendingModeEx);
procedure UnApplyBlendingModeEx;
procedure InitTexture(
  const TextureHandle: Cardinal;
  const TextureSize: TgxSize;
  const TextureTarget: TgxTextureTarget = ttTexture2D);
// Probably need to give them proper names, instead of numbers... 
procedure DrawTexturedScreenQuad;
procedure DrawTexturedScreenQuad2(const ViewPortSize: TgxSize);
procedure DrawTexturedScreenQuad3;
procedure DrawTexturedScreenQuad4(const ViewPortSize: TgxSize);
procedure DrawTexturedScreenQuad5(const ViewPortSize: TgxSize);
procedure DrawTexturedScreenQuad6(const ViewPortSize: TgxSize);

procedure CopyScreentoTexture(const ViewPortSize: TgxSize; const TextureTarget: Word = GL_TEXTURE_2D);
procedure CopyScreentoTexture2(const ViewPortSize: TgxSize; const TextureTarget: Word = GL_TEXTURE_2D);

function IsFogEnabled(const AFogSupportMode: TgxShaderFogSupport; var rci: TgxRenderContextInfo): Boolean;
procedure GetActiveLightsList(const ALightIDs: TgxIntegerList);

//===========================================================
implementation
//===========================================================

uses
  GXS.State;

procedure GetActiveLightsList(const ALightIDs: TgxIntegerList);
var
  I: Integer;
begin
  ALightIDs.Clear;
  with CurrentContext.gxStates do
  begin
    for I := 0 to MaxLights - 1 do
    begin
      if LightEnabling[I] then
        ALightIDs.Add(I);
    end;
  end;
end;

function IsFogEnabled(const AFogSupportMode: TgxShaderFogSupport; var rci: TgxRenderContextInfo): Boolean;
begin
  case AFogSupportMode of
    sfsEnabled:  Result := True;
    sfsDisabled: Result := False;
    sfsAuto:     Result := TgxSceneBuffer(rci.buffer).FogEnable;
  else
    Result := False;
    Assert(False, strUnknownType);
  end;
end;

procedure CopyScreentoTexture(const ViewPortSize: TgxSize; const TextureTarget: Word = GL_TEXTURE_2D);
begin
  glCopyTexSubImage2D(TextureTarget, 0, 0, 0, 0, 0, ViewPortSize.cx, ViewPortSize.cy);
end;

procedure CopyScreentoTexture2(const ViewPortSize: TgxSize; const TextureTarget: Word = GL_TEXTURE_2D);
begin
  glCopyTexImage2D(TextureTarget, 0, GL_RGB, 0, 0, ViewPortSize.cx, ViewPortSize.cy, 0);
end;

procedure ApplyBlendingModeEx(const BlendingMode: TgxBlendingModeEx);
begin
  with CurrentContext.gxStates do
  begin
    Enable(stBlend);

    case BlendingMode of
      bmxOpaque: SetBlendFunc(bfSRCALPHA, bfONE);
      bmxTransparency: SetBlendFunc(bfSRCALPHA, bfONEMINUSSRCALPHA);
      bmxAdditive: SetBlendFunc(bfSRCALPHA, bfONE);
      bmxAlphaTest50: SetAlphaFunction(cfGEQUAL, 0.5);
      bmxAlphaTest100: SetAlphaFunction(cfGEQUAL, 1.0);
      bmxModulate: SetBlendFunc(bfDSTCOLOR, bfZERO);
      bmxDestColorOne: SetBlendFunc(bfDSTCOLOR, bfONE);
      bmxDestAlphaOne: SetBlendFunc(bfDSTALPHA, bfONE);
      else
        Assert(False, strErrorEx + strUnknownType);
    end;
  end;
end;

procedure UnApplyBlendingModeEx;
begin
end;

procedure DrawTexturedScreenQuad;
begin
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadIdentity;
  glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;

    // drawing rectangle over screen
    glDisable(GL_DEPTH_TEST);
    DrawTexturedScreenQuad3;
    glEnable(GL_DEPTH_TEST);

  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
end;

procedure DrawTexturedScreenQuad2(const ViewPortSize: TgxSize);
begin
  glPushMatrix;
  glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    glOrtho(0, ViewPortSize.cx, ViewPortSize.cy, 0, 0, 1);
    glDisable(GL_DEPTH_TEST);
    glDepthMask(GLboolean(False));
    glBegin(GL_QUADS);
      glTexCoord2f(0.0, ViewPortSize.cy);             glVertex2f(0, 0);
      glTexCoord2f(0.0, 0.0);                         glVertex2f(0, ViewPortSize.cy);
      glTexCoord2f(ViewPortSize.cx, 0.0);             glVertex2f(ViewPortSize.cx, ViewPortSize.cy);
      glTexCoord2f(ViewPortSize.cx, ViewPortSize.cy); glVertex2f(ViewPortSize.cx, 0);
    glEnd;
    glDepthMask(GLboolean(True));
    glEnable(GL_DEPTH_TEST);
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
end;

procedure DrawTexturedScreenQuad4(const ViewPortSize: TgxSize);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);                             glVertex2f(-1, -1);
    glTexCoord2f(ViewPortSize.cx, 0);               glVertex2f( 1, -1);
    glTexCoord2f(ViewPortSize.cx, ViewPortSize.cy); glVertex2f( 1,  1);
    glTexCoord2f(0, ViewPortSize.cy);               glVertex2f(-1,  1);
  glEnd;
end;

procedure DrawTexturedScreenQuad5(const ViewPortSize: TgxSize);
begin
  glMatrixMode( GL_PROJECTION );
  glPushMatrix;
    glLoadIdentity;
    glOrtho( 0, ViewPortSize.cx, ViewPortSize.cy, 0, 0, 1 );
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
      glLoadIdentity;
      glDisable(GL_DEPTH_TEST);
      glDepthMask(GLboolean(False));
      DrawTexturedScreenQuad3;
      glDepthMask(GLboolean(True));
      glEnable(GL_DEPTH_TEST);
    glPopMatrix;
    glMatrixMode( GL_PROJECTION );
  glPopMatrix;
  glMatrixMode( GL_MODELVIEW );
end;

procedure DrawTexturedScreenQuad6(const ViewPortSize: TgxSize);
begin
  glMatrixMode( GL_PROJECTION );
  glPushMatrix;
    glLoadIdentity;
    glOrtho( 0, ViewPortSize.cx, ViewPortSize.cy, 0, 0, 1 );
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
      glLoadIdentity;
      glDisable(GL_DEPTH_TEST);
      glDepthMask(GLboolean(FALSE));
      DrawTexturedScreenQuad4(ViewPortSize);;
      glDepthMask(GLboolean(True));
      glEnable(GL_DEPTH_TEST);
    glPopMatrix;
    glMatrixMode(GL_PROJECTION );
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW );
end;

procedure DrawTexturedScreenQuad3;
begin
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(-1, -1);
    glTexCoord2f(1, 0); glVertex2f(1, -1);
    glTexCoord2f(1, 1); glVertex2f(1, 1);
    glTexCoord2f(0, 1); glVertex2f(-1, 1);
  glEnd;
end;

procedure InitTexture(
  const TextureHandle: Cardinal;
  const TextureSize: TgxSize;
  const TextureTarget: TgxTextureTarget = ttTexture2D);
var
  glTarget: GLEnum;
begin
  with CurrentContext.gxStates do
  begin
    TextureBinding[ActiveTexture, TextureTarget] := TextureHandle;
  end;
  glTarget := DecodeTextureTarget(TextureTarget);
  glTexParameteri(glTarget, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(glTarget, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(glTarget, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(glTarget, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glCopyTexImage2D(glTarget, 0, GL_RGBA8, 0, 0, TextureSize.cx, TextureSize.cy, 0);
end;

{ TgxShaderProgram }

procedure TgxShaderProgram.Apply;
begin
  FParent.FinalizeShader;
end;


procedure TgxShaderProgram.Assign(Source: TPersistent);
begin
  if Source = nil then
    Exit;

  if (Source is TgxShaderProgram) then
  begin
    FEnabled := TgxShaderProgram(Source).FEnabled;
    FCode.Assign(TgxShaderProgram(Source).FCode);
  end
  else
    inherited; //die, die, die!!!
end;


constructor TgxShaderProgram.Create(const AParent: TgxCustomShader);
begin
  FParent := AParent;
  FCode := TStringList.Create;
  TStringList(FCode).OnChange := OnChangeCode;
  FEnabled := False;
end;


destructor TgxShaderProgram.Destroy;
begin
  FCode.Destroy;
end;


function TgxShaderProgram.GetOwner: TPersistent;
begin
  Result := FParent;
end;

procedure TgxShaderProgram.LoadFromFile(const AFileName: string);
begin
  FCode.LoadFromFile(AFileName);
  FEnabled := True;
end;


procedure TgxShaderProgram.OnChangeCode(Sender: TObject);
begin
  FEnabled := True;
  FParent.NotifyChange(self);
end;


procedure TgxShaderProgram.SetCode(const Value: TStrings);
begin
  FCode.Assign(Value);
  FParent.NotifyChange(self);
end;


procedure TgxShaderProgram.SetEnabled(const Value: Boolean);
begin
  if Value = FEnabled then
    Exit;
  FEnabled := Value;
  if FEnabled then
    FParent.FinalizeShader;
end;


{ TgxCustomShader }

procedure TgxCustomShader.Assign(Source: TPersistent);
begin
  if Source is TgxCustomShader then
  begin
    FFragmentProgram.Assign(TgxCustomShader(Source).FFragmentProgram);
    FVertexProgram.Assign(TgxCustomShader(Source).FVertexProgram);
    FGeometryProgram.Assign(TgxCustomShader(Source).FGeometryProgram);
    FTagObject := TgxCustomShader(Source).FTagObject;
  end;
  inherited;
end;


constructor TgxCustomShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDebugMode := False;
  FFragmentProgram := TgxFragmentProgram.Create(Self);
  FVertexProgram := TgxVertexProgram.Create(Self);
  FGeometryProgram := TgxGeometryProgram.Create(Self);
end;


destructor TgxCustomShader.Destroy;
begin
  FFragmentProgram.Destroy;
  FVertexProgram.Destroy;
  FGeometryProgram.Destroy;

  inherited;
end;

procedure TgxCustomShader.LoadShaderPrograms(const VPFilename, FPFilename: string; GPFilename: string = '');
begin
  If VPFilename <> '' then VertexProgram.LoadFromFile(VPFilename);
  If FPFilename <> '' then FragmentProgram.LoadFromFile(FPFilename);
  If GPFilename <> '' then GeometryProgram.LoadFromFile(GPFilename);
end;

procedure TgxCustomShader.SetDebugMode(const Value: Boolean);
begin
  if FDebugMode <> Value then
  begin
    FDebugMode := Value;

    if FDebugMode then
      FailedInitAction := fiaReRaiseException
    else
      FailedInitAction := fiaRaiseStandardException;
  end;
end;

procedure TgxCustomShader.SetFragmentProgram(const Value: TgxFragmentProgram);
begin
  FFragmentProgram.Assign(Value);
end;

procedure TgxCustomShader.SetGeometryProgram(const Value: TgxGeometryProgram);
begin
  FGeometryProgram.Assign(Value);
end;

procedure TgxCustomShader.SetVertexProgram(const Value: TgxVertexProgram);
begin
  FVertexProgram.Assign(Value);
end;

function TgxCustomShader.StoreFragmentProgram: Boolean;
begin
  Result := FFragmentProgram.Enabled or (FFragmentProgram.Code.Text <> '')
end;

function TgxCustomShader.StoreGeometryProgram: Boolean;
begin
  Result := FGeometryProgram.Enabled or (FGeometryProgram.Code.Text <> '')
end;

function TgxCustomShader.StoreVertexProgram: Boolean;
begin
  Result := FVertexProgram.Enabled or (FVertexProgram.Code.Text <> '')
end;

{ TgxCustomShaderParameter }

procedure TgxCustomShaderParameter.SetAsTexture(
  const TextureIndex: Integer; const Value: TgxTexture);
begin
  SetAsCustomTexture(TextureIndex, Value.TextureHandle.Target, Value.Handle);
end;

procedure TgxCustomShaderParameter.SetAsTexture1D(
  const TextureIndex: Integer; const Value: TgxTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTexture1D, Value.Handle);
end;

procedure TgxCustomShaderParameter.SetAsTexture2D(
  const TextureIndex: Integer; const Value: TgxTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTexture2D, Value.Handle);
end;

procedure TgxCustomShaderParameter.SetAsTexture3D(
  const TextureIndex: Integer; const Value: TgxTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTexture3D, Value.Handle);
end;

procedure TgxCustomShaderParameter.SetAsTextureCube(
  const TextureIndex: Integer; const Value: TgxTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTextureCube, Value.Handle);
end;

procedure TgxCustomShaderParameter.SetAsTextureRect(
  const TextureIndex: Integer; const Value: TgxTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTextureRect, Value.Handle);
end;

procedure TgxCustomShaderParameter.SetAsVectorF(const Values: array of Single);
begin
  case Length(Values) of
    1: SetAsVector1f(Values[0]);
    2: SetAsVector2f(Vector2fMake(Values[0], Values[1]));
    3: SetAsVector3f(Vector3fMake(Values[0], Values[1], Values[2]));
    4: SetAsVector4f(Vector4fMake(Values[0], Values[1], Values[2], Values[3]));
  else
    Assert(False, 'Vector length must be between 1 to 4');
  end;
end;

procedure TgxCustomShaderParameter.SetAsVectorI(const Values: array of Integer);
begin
  case Length(Values) of
    1: SetAsVector1i(Values[0]);
    2: SetAsVector2i(Vector2iMake(Values[0], Values[1]));
    3: SetAsVector3i(Vector3iMake(Values[0], Values[1], Values[2]));
    4: SetAsVector4i(Vector4iMake(Values[0], Values[1], Values[2], Values[3]));
  else
    Assert(False, 'Vector length must be between 1 to 4');
  end;
end;

procedure TgxCustomShaderParameter.SetToTextureOf(
  const LibMaterial: TgxLibMaterial; const TextureIndex: Integer);
begin
  SetToTextureOf(LibMaterial.Material.Texture, TextureIndex);
end;

procedure TgxCustomShaderParameter.SetToTextureOf(
  const Texture: TgxTexture; const TextureIndex: Integer);
begin
  SetAsCustomTexture(TextureIndex, Texture.Image.NativeTextureTarget, Texture.Handle);
end;

constructor TgxGeometryProgram.Create(const AParent: TgxCustomShader);
begin
  inherited Create(AParent);
  FInputPrimitiveType := gsInPoints;
  FOutputPrimitiveType := gsOutPoints;
  FVerticesOut := 0;
end;

procedure TgxGeometryProgram.SetInputPrimitiveType(const Value: TgxgsInTypes);
begin
  if Value <> FInputPrimitiveType then
  begin
    FInputPrimitiveType := Value;
    FParent.NotifyChange(Self);
  end;
end;

procedure TgxGeometryProgram.SetOutputPrimitiveType(const Value: TgxgsOutTypes);
begin
  if Value<>FOutputPrimitiveType then
  begin
    FOutputPrimitiveType := Value;
    FParent.NotifyChange(Self);
  end;
end;

procedure TgxGeometryProgram.SetVerticesOut(const Value: GLint);
begin
  if Value<>FVerticesOut then
  begin
    FVerticesOut := Value;
    FParent.NotifyChange(Self);
  end;
end;

initialization
  RegisterClasses([TgxCustomShader, TgxShaderProgram,
                   TgxVertexProgram, TgxFragmentProgram, TgxGeometryProgram]);

end.