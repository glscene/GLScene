//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.State;

(*  Tools for managing an application-side cache of OpenGL state. *)

(*
 TODO: Proper client-side pushing + popping of state, in OpenGL 3+ contexts,
 rather than using glPushAttrib + glPopAttrib.
 TODO: Proper support for textures, taking into account that they probably
 won't be linked to texture units in some future version of OpenGL.
 TODO: Once more of GLScene is cache-aware, enable some of the checks before
 changing OpenGL state (where we will gain a speed increase).
 DONE: Cache some relevant legacy state
 TODO: improve binding objects to binding points
 TODO: decide how to implement the new Enable* options (without going above
 32 elements in sets if possible, which would be slower in 32bit Delphi)
 DONE: remove stTexture1D, 2D, etc from TGLState if possible, since they are
 per texture-unit + also deprecated in OpenGL 3+
 *)

interface

{$I GLScene.inc}
{ .$DEFINE USE_CACHE_MISS_CHECK }

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,

  GLS.OpenGLTokens,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.TextureFormat,
  GLS.Utils;

const
  GLS_VERTEX_ATTR_NUM = 16;

type

  TGLStateType = (sttCurrent, sttPoint, sttLine, sttPolygon, sttPolygonStipple,
    sttPixelMode, sttLighting, sttFog, sttDepthBuffer, sttAccumBuffer,
    sttStencilBuffer, sttViewport, sttTransform, sttEnable, sttColorBuffer,
    sttHint, sttEval, sttList, sttTexture, sttScissor,
    sttMultisample);
  TGLStateTypes = set of TGLStateType;

const
  cAllAttribBits = [low(TGLStateType) .. High(TGLStateType)];

type

  TGLMeshPrimitive = (
    mpNOPRIMITIVE,
    mpTRIANGLES,
    mpTRIANGLE_STRIP,
    mpTRIANGLE_FAN,
    mpPOINTS,
    mpLINES,
    mpLINE_LOOP,
    mpLINE_STRIP,
    mpLINES_ADJACENCY,
    mpLINE_STRIP_ADJACENCY,
    mpTRIANGLES_ADJACENCY,
    mpTRIANGLE_STRIP_ADJACENCY,
    mpPATCHES
    );

  TGLMeshPrimitives = set of TGLMeshPrimitive;

const
  cAllMeshPrimitive = [
    mpTRIANGLES,
    mpTRIANGLE_STRIP,
    mpTRIANGLE_FAN,
    mpPOINTS,
    mpLINES,
    mpLINE_LOOP,
    mpLINE_STRIP,
    mpLINES_ADJACENCY,
    mpLINE_STRIP_ADJACENCY,
    mpTRIANGLES_ADJACENCY,
    mpTRIANGLE_STRIP_ADJACENCY,
    mpPATCHES];

type

  // Reflects all relevant (binary) states of OpenGL subsystem
  TGLState = (stAlphaTest, stAutoNormal,
    stBlend, stColorMaterial, stCullFace, stDepthTest, stDither,
    stFog, stLighting, stLineSmooth, stLineStipple,
    stIndexLogicOp, stColorLogicOp, stNormalize, stPointSmooth, stPointSprite,
    stPolygonSmooth, stPolygonStipple, stScissorTest, stStencilTest,
    stPolygonOffsetPoint, stPolygonOffsetLine, stPolygonOffsetFill,
    stDepthClamp);

  TGLStates = set of TGLState;

  TGLComparisonFunction = (cfNever, cfAlways, cfLess, cfLEqual, cfEqual,
    cfGreater, cfNotEqual, cfGEqual);
  TGLStencilFunction = TGLComparisonFunction;
  TGLDepthFunction = TGLComparisonFunction;

  TGLBlendFunction = (bfZero, bfOne,
    bfSrcColor, bfOneMinusSrcColor, bfDstColor, bfOneMinusDstColor,
    bfSrcAlpha, bfOneMinusSrcAlpha, bfDstAlpha, bfOneMinusDstAlpha,
    bfConstantColor, bfOneMinusConstantColor,
    bfConstantAlpha, bfOneMinusConstantAlpha,
    bfSrcAlphaSat);

  TGLDstBlendFunction = bfZero..bfOneMinusConstantAlpha;
  TGLBlendEquation = (beAdd, beSubtract, beReverseSubtract, beMin, beMax);
  TGLStencilOp = (soKeep, soZero, soReplace, soIncr, soDecr, soInvert, soIncrWrap,
    soDecrWrap);

  TGLLogicOp = (loClear, loAnd, loAndReverse, loCopy, loAndInverted, loNoOp,
    loXOr, loOr, loNor, loEquiv, loInvert, loOrReverse, loCopyInverted,
    loOrInverted, loNAnd, loSet);

  TGLQueryType = (
    qrySamplesPassed,
    qryPrimitivesGenerated,
    qryTransformFeedbackPrimitivesWritten,
    qryTimeElapsed,
    qryAnySamplesPassed);

  // Describe what kind of winding has a front face
  TGLFaceWinding = (fwCounterClockWise, fwClockWise);

  TGLPolygonMode = (pmFill, pmLines, pmPoints);

  TGLCullFaceMode = (cmFront, cmBack, cmFrontAndBack);
  //  TSingleCullFaceMode = cmFront..cmBack;

  TGLColorComponent = (ccRed, ccGreen, ccBlue, ccAlpha);
  TGLColorMask = set of TGLColorComponent;

const
  cAllColorComponents = [ccRed, ccGreen, ccBlue, ccAlpha];
  MAX_HARDWARE_LIGHT = 16;
  MAX_SHADER_LIGHT = 8;
  MAX_HARDWARE_TEXTURE_UNIT = 48;
  MAX_HARDWARE_UNIFORM_BUFFER_BINDING = 75;

type

  TGLHintType = (hintDontCare, hintFastest, hintNicest);

  TGLLightSourceState = packed record
    Position: array [0 .. MAX_HARDWARE_LIGHT - 1] of TGLVector;
    Ambient: array [0 .. MAX_HARDWARE_LIGHT - 1] of TGLVector;
    Diffuse: array [0 .. MAX_HARDWARE_LIGHT - 1] of TGLVector;
    Specular: array [0 .. MAX_HARDWARE_LIGHT - 1] of TGLVector;
    SpotDirection: array [0 .. MAX_HARDWARE_LIGHT - 1] of TGLVector;
    SpotCosCutoffExponent: array [0 .. MAX_HARDWARE_LIGHT - 1] of TGLVector;
    Attenuation: array [0 .. MAX_HARDWARE_LIGHT - 1] of TGLVector;
  end;

  TGLShaderLightSourceState = packed record
    Position: array [0 .. MAX_SHADER_LIGHT - 1] of TGLVector;
    Ambient: array [0 .. MAX_SHADER_LIGHT - 1] of TGLVector;
    Diffuse: array [0 .. MAX_SHADER_LIGHT - 1] of TGLVector;
    Specular: array [0 .. MAX_SHADER_LIGHT - 1] of TGLVector;
    SpotDirection: array [0 .. MAX_SHADER_LIGHT - 1] of TGLVector;
    SpotCosCutoffExponent: array [0 .. MAX_SHADER_LIGHT - 1] of TGLVector;
    Attenuation: array [0 .. MAX_SHADER_LIGHT - 1] of TGLVector;
  end;

  TGLOnLightsChanged = procedure(Sender: TObject);

  TGLBufferBindingTarget = (bbtUniform, bbtTransformFeedBack);

  TUBOStates = record
    FUniformBufferBinding: Cardinal;
    FOffset: TGLintptr;
    FSize: TGLsizeiptr;
  end;

  TGLMaterialLevel = (mlAuto, mlFixedFunction, mlMultitexturing, mlSM3, mlSM4, mlSM5);

  (* Manages an application-side cache of OpenGL states and parameters.
    Purpose of this class is to eliminate redundant state and parameter
    changes, and there will typically be no more than one state cache per
    OpenGL context *)
  TGLStateCache = class
  strict private
    // Legacy state
    FFrontBackColors: array [0 .. 1, 0 .. 3] of TGLVector;
    FFrontBackShininess: array [0 .. 1] of Integer;
    FAlphaFunc: TGLComparisonFunction;
    FAlphaRef: Single;
    FPolygonBackMode: TGLPolygonMode; // Front + back have same polygon mode
    // Lighting state
    FMaxLights: Cardinal;
    FLightEnabling: array [0 .. MAX_HARDWARE_LIGHT - 1] of Boolean;
    FLightIndices: array [0 .. MAX_HARDWARE_LIGHT - 1] of TGLint;
    FLightNumber: Integer;
    FLightStates: TGLLightSourceState;
    FSpotCutoff: array [0 .. MAX_HARDWARE_LIGHT - 1] of Single;
    FShaderLightStates: TGLShaderLightSourceState;
    FShaderLightStatesChanged: Boolean;
    FColorWriting: Boolean; // TODO: change to per draw buffer (FColorWriteMask)
    FStates: TGLStates;
    FListStates: array of TGLStateTypes;
    FCurrentList: Cardinal;
    FTextureMatrixIsIdentity: array [0 .. 3] of Boolean;
    // FForwardContext: Boolean;
    FFFPLight: Boolean;
    // Vertex Array Data state
    FVertexArrayBinding: Cardinal;
    FArrayBufferBinding: Cardinal;
    FElementBufferBinding: Cardinal;
    FTextureBufferBinding: Cardinal;
    FEnablePrimitiveRestart: TGLboolean;
    FPrimitiveRestartIndex: Cardinal;
    // Transformation state
    FViewPort: TVector4i;
    FDepthRange: array [0 .. 1] of TGLclampd;
    FEnableClipDistance: array [0 .. 7] of TGLboolean;
    FEnableDepthClamp: TGLboolean;
    // Coloring state
    FClampReadColor: Cardinal; // GL_FIXED_ONLY
    FProvokingVertex: Cardinal; // GL_LAST_VERTEX_CONVENTION
    // Rasterization state
    FPointSize: TGLfloat;
    FPointFadeThresholdSize: TGLfloat;
    FPointSpriteCoordOrigin: Cardinal; // GL_UPPER_LEFT
    FLineWidth: Single;
    FLineStippleFactor: TGLint;
    FLineStipplePattern: TGLushort;
    FEnableLineSmooth: TGLboolean;
    FEnableCullFace: TGLboolean;
    FCullFaceMode: TGLCullFaceMode;
    FFrontFace: TGLFaceWinding;
    FEnablePolygonSmooth: TGLboolean;
    FPolygonMode: TGLPolygonMode;
    FPolygonOffsetFactor: TGLfloat;
    FPolygonOffsetUnits: TGLfloat;
    FEnablePolygonOffsetPoint: TGLboolean;
    FEnablePolygonOffsetLine: TGLboolean;
    FEnablePolygonOffsetFill: TGLboolean;
    // Multisample state
    FEnableMultisample: TGLboolean;
    FEnableSampleAlphaToCoverage: TGLboolean;
    FEnableSampleAlphaToOne: TGLboolean;
    FEnableSampleCoverage: TGLboolean;
    FSampleCoverageValue: TGLfloat;
    FSampleCoverageInvert: TGLboolean;
    FEnableSampleMask: TGLboolean;
    FSampleMaskValue: array [0 .. 15] of TGLbitfield;
    // Texture state
    FMaxTextureSize: Cardinal;
    FMax3DTextureSize: Cardinal;
    FMaxCubeTextureSize: Cardinal;
    FMaxArrayTextureSize: Cardinal;
    FMaxTextureImageUnits: Cardinal;
    FMaxTextureAnisotropy: Cardinal;
    FMaxSamples: Cardinal;
    FTextureBinding: array [0 .. MAX_HARDWARE_TEXTURE_UNIT - 1, TGLTextureTarget] of Cardinal;
    FTextureBindingTime: array [0 .. MAX_HARDWARE_TEXTURE_UNIT - 1, TGLTextureTarget] of Double;
    FSamplerBinding: array [0 .. MAX_HARDWARE_TEXTURE_UNIT - 1] of Cardinal;
    // Active texture state
    FActiveTexture: TGLint; // 0 .. Max_texture_units
    FActiveTextureEnabling: array [0 .. MAX_HARDWARE_TEXTURE_UNIT - 1, TGLTextureTarget] of Boolean;
    // Pixel operation state
    FEnableScissorTest: TGLboolean;
    FScissorBox: TVector4i;
    FEnableStencilTest: TGLboolean;
    FStencilFunc: TGLStencilFunction;
    FStencilValueMask: Cardinal;
    FStencilRef: TGLint;
    FStencilFail: TGLStencilOp;
    FStencilPassDepthFail: TGLStencilOp;
    FStencilPassDepthPass: TGLStencilOp;
    FStencilBackFunc: TGLStencilFunction;
    FStencilBackValueMask: Cardinal;
    FStencilBackRef: Cardinal;
    FStencilBackFail: TGLStencilOp;
    FStencilBackPassDepthPass: TGLStencilOp;
    FStencilBackPassDepthFail: TGLStencilOp;
    FEnableDepthTest: TGLboolean;
    FDepthFunc: TGLDepthFunction;
    FEnableBlend: array [0 .. 15] of TGLboolean;
    FBlendSrcRGB: TGLBlendFunction;
    FBlendSrcAlpha: TGLBlendFunction;
    FBlendDstRGB: TGLDstBlendFunction;
    FBlendDstAlpha: TGLDstBlendFunction;
    FBlendEquationRGB: TGLBlendEquation;
    FBlendEquationAlpha: TGLBlendEquation;
    FBlendColor: TGLVector;
    FEnableFramebufferSRGB: TGLboolean;
    FEnableDither: TGLboolean;
    FEnableColorLogicOp: TGLboolean;
    FLogicOpMode: TGLLogicOp;
    // Framebuffer control state
    FColorWriteMask: array [0 .. 15] of TGLColorMask;
    FDepthWriteMask: TGLboolean;
    FStencilWriteMask: Cardinal;
    FStencilBackWriteMask: Cardinal;
    FColorClearValue: TGLVector;
    FDepthClearValue: TGLfloat;
    FStencilClearValue: Cardinal;
    // Framebuffer state
    FDrawFrameBuffer: Cardinal;
    FReadFrameBuffer: Cardinal;
    // Renderbuffer state
    FRenderBuffer: Cardinal;
    // Pixels state
    FUnpackSwapBytes: TGLboolean;
    FUnpackLSBFirst: TGLboolean;
    FUnpackImageHeight: Cardinal;
    FUnpackSkipImages: Cardinal;
    FUnpackRowLength: Cardinal;
    FUnpackSkipRows: Cardinal;
    FUnpackSkipPixels: Cardinal;
    FUnpackAlignment: Cardinal;
    FPackSwapBytes: TGLboolean;
    FPackLSBFirst: TGLboolean;
    FPackImageHeight: Cardinal;
    FPackSkipImages: Cardinal;
    FPackRowLength: Cardinal;
    FPackSkipRows: Cardinal;
    FPackSkipPixels: Cardinal;
    FPackAlignment: Cardinal;
    FPixelPackBufferBinding: Cardinal;
    FPixelUnpackBufferBinding: Cardinal;
    // Program state
    FCurrentProgram: Cardinal;
    FMaxTextureUnits: Cardinal;
    FUniformBufferBinding: Cardinal;
    FUBOStates: array [TGLBufferBindingTarget, 0 .. MAX_HARDWARE_UNIFORM_BUFFER_BINDING - 1] of TUBOStates;
    // Vector + Geometry Shader state
    FCurrentVertexAttrib: array [0 .. 15] of TGLVector;
    FEnableProgramPointSize: TGLboolean;
    // Transform Feedback state
    FTransformFeedbackBufferBinding: Cardinal;
    // Hints state
    FTextureCompressionHint: TGLHintType;
    FPolygonSmoothHint: TGLHintType;
    FFragmentShaderDerivitiveHint: TGLHintType;
    FLineSmoothHint: TGLHintType;
    FMultisampleFilterHint: TGLHintType;
    // Misc state
    FCurrentQuery: array [TGLQueryType] of Cardinal;
    FCopyReadBufferBinding: Cardinal;
    FCopyWriteBufferBinding: Cardinal;
    FEnableTextureCubeMapSeamless: TGLboolean;
    FInsideList: Boolean;
    FOnLightsChanged: TGLOnLightsChanged;
  protected
    // Vertex Array Data state
    procedure SetVertexArrayBinding(const Value: Cardinal); inline;
    function GetArrayBufferBinding: Cardinal; inline;
    procedure SetArrayBufferBinding(const Value: Cardinal); inline;
    function GetElementBufferBinding: Cardinal; inline;
    procedure SetElementBufferBinding(const Value: Cardinal); inline;
    function GetEnablePrimitiveRestart: TGLboolean; inline;
    function GetPrimitiveRestartIndex: Cardinal; inline;
    procedure SetEnablePrimitiveRestart(const enabled: TGLboolean); inline;
    procedure SetPrimitiveRestartIndex(const index: Cardinal); inline;
    procedure SetTextureBufferBinding(const Value: Cardinal); inline;
    // Transformation state
    procedure SetViewPort(const Value: TVector4i); inline;
    function GetEnableClipDistance(ClipDistance: Cardinal): TGLboolean; inline;
    procedure SetEnableClipDistance(index: Cardinal; const Value: TGLboolean); inline;
    function GetDepthRangeFar: TGLclampd; inline;
    procedure SetDepthRangeFar(const Value: TGLclampd); inline;
    function GetDepthRangeNear: TGLclampd; inline;
    procedure SetDepthRangeNear(const Value: TGLclampd); inline;
    procedure SetEnableDepthClamp(const enabled: TGLboolean); inline;
    // Coloring state
    procedure SetClampReadColor(const Value: Cardinal); inline;
    procedure SetProvokingVertex(const Value: Cardinal); inline;
    // Rasterization state
    procedure SetPointSize(const Value: TGLfloat); inline;
    procedure SetPointFadeThresholdSize(const Value: TGLfloat); inline;
    procedure SetPointSpriteCoordOrigin(const Value: Cardinal); inline;
    procedure SetLineWidth(const Value: TGLfloat); inline;
    procedure SetLineStippleFactor(const Value: TGLint); inline;
    procedure SetLineStipplePattern(const Value: TGLushort); inline;

    procedure SetEnableLineSmooth(const Value: TGLboolean); inline;
    procedure SetEnableCullFace(const Value: TGLboolean); inline;
    procedure SetCullFaceMode(const Value: TGLCullFaceMode); inline;
    procedure SetFrontFace(const Value: TGLFaceWinding); inline;
    procedure SetEnablePolygonSmooth(const Value: TGLboolean); inline;
    procedure SetPolygonMode(const Value: TGLPolygonMode); inline;
    procedure SetPolygonOffsetFactor(const Value: TGLfloat); inline;
    procedure SetPolygonOffsetUnits(const Value: TGLfloat); inline;
    procedure SetEnablePolygonOffsetPoint(const Value: TGLboolean); inline;
    procedure SetEnablePolygonOffsetLine(const Value: TGLboolean); inline;
    procedure SetEnablePolygonOffsetFill(const Value: TGLboolean); inline;
    // Multisample state
    procedure SetEnableMultisample(const Value: TGLboolean); inline;
    procedure SetEnableSampleAlphaToCoverage(const Value: TGLboolean); inline;
    procedure SetEnableSampleAlphaToOne(const Value: TGLboolean); inline;
    procedure SetEnableSampleCoverage(const Value: TGLboolean); inline;
    procedure SetSampleCoverageValue(const Value: TGLfloat); inline;
    procedure SetSampleCoverageInvert(const Value: TGLboolean); inline;
    procedure SetEnableSampleMask(const Value: TGLboolean); inline;
    function GetSampleMaskValue(index: Integer): TGLbitfield; inline;
    procedure SetSampleMaskValue(index: Integer; const Value: TGLbitfield); inline;
    // Texture state
    function GetMaxTextureSize: Cardinal; inline;
    function GetMax3DTextureSize: Cardinal; inline;
    function GetMaxCubeTextureSize: Cardinal; inline;
    function GetMaxArrayTextureSize: Cardinal; inline;
    function GetMaxTextureImageUnits: Cardinal; inline;
    function GetMaxTextureAnisotropy: Cardinal; inline;
    function GetMaxSamples: Cardinal; inline;
    function GetTextureBinding(index: Integer; target: TGLTextureTarget): Cardinal; inline;
    function GetTextureBindingTime(index: Integer; target: TGLTextureTarget): Double; inline;
    procedure SetTextureBinding(index: Integer; target: TGLTextureTarget; const Value: Cardinal);
    function GetActiveTextureEnabled(target: TGLTextureTarget): Boolean; inline;
    procedure SetActiveTextureEnabled(target: TGLTextureTarget; const Value: Boolean); inline;
    function GetSamplerBinding(index: Cardinal): Cardinal; inline;
    procedure SetSamplerBinding(index: Cardinal; const Value: Cardinal); inline;
    // Active texture
    procedure SetActiveTexture(const Value: TGLint); inline;
    // Pixel operations
    procedure SetEnableScissorTest(const Value: TGLboolean); inline;
    procedure SetScissorBox(const Value: TVector4i); inline;
    procedure SetEnableStencilTest(const Value: TGLboolean); inline;
    procedure SetEnableDepthTest(const Value: TGLboolean); inline;
    procedure SetDepthFunc(const Value: TGLDepthFunction); inline;
    function GetEnableBlend(index: Integer): TGLboolean; inline;
    procedure SetEnableBlend(index: Integer; const Value: TGLboolean); inline;
    procedure SetBlendColor(const Value: TGLVector); inline;
    procedure SetEnableFramebufferSRGB(const Value: TGLboolean); inline;
    procedure SetEnableDither(const Value: TGLboolean); inline;
    procedure SetEnableColorLogicOp(const Value: TGLboolean); inline;
    procedure SetLogicOpMode(const Value: TGLLogicOp); inline;
    // Framebuffer control
    function GetColorWriteMask(index: Integer): TGLColorMask; inline;
    procedure SetColorWriteMask(index: Integer; const Value: TGLColorMask); inline;
    procedure SetDepthWriteMask(const Value: TGLboolean); inline;
    procedure SetStencilWriteMask(const Value: Cardinal); inline;
    procedure SetStencilBackWriteMask(const Value: Cardinal); inline;
    procedure SetColorClearValue(const Value: TGLVector); inline;
    procedure SetDepthClearValue(const Value: TGLfloat); inline;
    procedure SetStencilClearValue(const Value: Cardinal); inline;
    // Framebuffer
    procedure SetDrawFrameBuffer(const Value: Cardinal); inline;
    procedure SetReadFrameBuffer(const Value: Cardinal); inline;
    // Renderbuffer
    procedure SetRenderBuffer(const Value: Cardinal); inline;
    // Pixels
    procedure SetUnpackSwapBytes(const Value: TGLboolean); inline;
    procedure SetUnpackLSBFirst(const Value: TGLboolean); inline;
    procedure SetUnpackImageHeight(const Value: Cardinal); inline;
    procedure SetUnpackSkipImages(const Value: Cardinal); inline;
    procedure SetUnpackRowLength(const Value: Cardinal); inline;
    procedure SetUnpackSkipRows(const Value: Cardinal); inline;
    procedure SetUnpackSkipPixels(const Value: Cardinal); inline;
    procedure SetUnpackAlignment(const Value: Cardinal); inline;
    procedure SetPackSwapBytes(const Value: TGLboolean); inline;
    procedure SetPackLSBFirst(const Value: TGLboolean); inline;
    procedure SetPackImageHeight(const Value: Cardinal); inline;
    procedure SetPackSkipImages(const Value: Cardinal); inline;
    procedure SetPackRowLength(const Value: Cardinal); inline;
    procedure SetPackSkipRows(const Value: Cardinal); inline;
    procedure SetPackSkipPixels(const Value: Cardinal); inline;
    procedure SetPackAlignment(const Value: Cardinal); inline;
    procedure SetPixelPackBufferBinding(const Value: Cardinal);
    procedure SetPixelUnpackBufferBinding(const Value: Cardinal);
    // Program
    procedure SetCurrentProgram(const Value: Cardinal); inline;
    procedure SetUniformBufferBinding(const Value: Cardinal); inline;
    function GetMaxTextureUnits: Cardinal; inline;
    // Vector + Geometry Shader state
    function GetCurrentVertexAttrib(index: Integer): TGLVector; inline;
    procedure SetCurrentVertexAttrib(index: Integer; const Value: TGLVector); inline;
    procedure SetEnableProgramPointSize(const Value: TGLboolean); inline;
    // Transform Feedback state
    procedure SetTransformFeedbackBufferBinding(const Value: Cardinal); inline;
    // Hints
    procedure SetLineSmoothHint(const Value: TGLHintType); inline;
    procedure SetPolygonSmoothHint(const Value: TGLHintType); inline;
    procedure SetTextureCompressionHint(const Value: TGLHintType); inline;
    procedure SetFragmentShaderDerivitiveHint(const Value: TGLHintType); inline;
    procedure SetMultisampleFilterHint(const Value: TGLHintType); inline;
    // Misc
    function GetCurrentQuery(index: TGLQueryType): Cardinal; inline;
    // procedure SetCurrentQuery(Index: TQueryType; const Value: Cardinal);
    procedure SetCopyReadBufferBinding(const Value: Cardinal); inline;
    procedure SetCopyWriteBufferBinding(const Value: Cardinal); inline;
    procedure SetEnableTextureCubeMapSeamless(const Value: TGLboolean); inline;
    // Ligting
    procedure SetFFPLight(Value: Boolean); inline;
    function GetMaxLights: Integer; inline;
    function GetLightEnabling(I: Integer): Boolean; inline;
    procedure SetLightEnabling(I: Integer; Value: Boolean); inline;
    function GetLightPosition(I: Integer): TGLVector; inline;
    procedure SetLightPosition(I: Integer; const Value: TGLVector); inline;
    function GetLightSpotDirection(I: Integer): TAffineVector; inline;
    procedure SetLightSpotDirection(I: Integer; const Value: TAffineVector); inline;
    function GetLightAmbient(I: Integer): TGLVector; inline;
    procedure SetLightAmbient(I: Integer; const Value: TGLVector); inline;
    function GetLightDiffuse(I: Integer): TGLVector; inline;
    procedure SetLightDiffuse(I: Integer; const Value: TGLVector); inline;
    function GetLightSpecular(I: Integer): TGLVector; inline;
    procedure SetLightSpecular(I: Integer; const Value: TGLVector); inline;
    function GetSpotCutoff(I: Integer): Single; inline;
    procedure SetSpotCutoff(I: Integer; const Value: Single); inline;
    function GetSpotExponent(I: Integer): Single; inline;
    procedure SetSpotExponent(I: Integer; const Value: Single); inline;
    function GetConstantAtten(I: Integer): Single; inline;
    procedure SetConstantAtten(I: Integer; const Value: Single); inline;
    function GetLinearAtten(I: Integer): Single; inline;
    procedure SetLinearAtten(I: Integer; const Value: Single); inline;
    function GetQuadAtten(I: Integer): Single; inline;
    procedure SetQuadAtten(I: Integer; const Value: Single); inline;
    procedure SetForwardContext(Value: Boolean); inline;
    function GetMaterialAmbient(const aFace: TGLCullFaceMode): TGLVector; inline;
    function GetMaterialDiffuse(const aFace: TGLCullFaceMode): TGLVector; inline;
    function GetMaterialSpecular(const aFace: TGLCullFaceMode): TGLVector; inline;
    function GetMaterialEmission(const aFace: TGLCullFaceMode): TGLVector; inline;
    function GetMaterialShininess(const aFace: TGLCullFaceMode): Integer; inline;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure PushAttrib(const stateTypes: TGLStateTypes); inline;
    procedure PopAttrib(); inline;
    procedure Enable(const aState: TGLState);
    procedure Disable(const aState: TGLState);
    procedure PerformEnable(const aState: TGLState); inline;
    procedure PerformDisable(const aState: TGLState); inline;
    procedure SetGLState(const aState: TGLState); deprecated; inline;
    procedure UnSetGLState(const aState: TGLState); deprecated; inline;
    procedure ResetGLPolygonMode; deprecated; inline;
    procedure ResetGLMaterialColors; deprecated; inline;
    procedure ResetGLTexture(const TextureUnit: Integer); deprecated; inline;
    procedure ResetGLCurrentTexture; deprecated;
    procedure ResetGLFrontFace; deprecated;
    procedure SetGLFrontFaceCW; deprecated; inline;
    procedure ResetAll; deprecated; inline;
    // Adjusts material colors for a face
    procedure SetGLMaterialColors(const aFace: TGLCullFaceMode; const emission, Ambient, Diffuse, Specular: TGLVector;
      const shininess: Integer);
    property MaterialAmbient[const aFace: TGLCullFaceMode]: TGLVector read GetMaterialAmbient;
    property MaterialDiffuse[const aFace: TGLCullFaceMode]: TGLVector read GetMaterialDiffuse;
    property MaterialSpecular[const aFace: TGLCullFaceMode]: TGLVector read GetMaterialSpecular;
    property MaterialEmission[const aFace: TGLCullFaceMode]: TGLVector read GetMaterialEmission;
    property MaterialShininess[const aFace: TGLCullFaceMode]: Integer read GetMaterialShininess;
    // Adjusts material alpha channel for a face
    procedure SetGLMaterialAlphaChannel(const aFace: Cardinal; const alpha: TGLfloat);
    // Adjusts material diffuse color for a face
    procedure SetGLMaterialDiffuseColor(const aFace: Cardinal; const Diffuse: TGLVector);
    // Lighting states
    property FixedFunctionPipeLight: Boolean read FFFPLight write SetFFPLight;
    property MaxLights: Integer read GetMaxLights;
    property LightEnabling[Index: Integer]: Boolean read GetLightEnabling write SetLightEnabling;
    property LightPosition[Index: Integer]: TGLVector read GetLightPosition write SetLightPosition;
    property LightSpotDirection[Index: Integer]: TAffineVector read GetLightSpotDirection write SetLightSpotDirection;
    property LightAmbient[Index: Integer]: TGLVector read GetLightAmbient write SetLightAmbient;
    property LightDiffuse[Index: Integer]: TGLVector read GetLightDiffuse write SetLightDiffuse;
    property LightSpecular[Index: Integer]: TGLVector read GetLightSpecular write SetLightSpecular;
    property LightSpotCutoff[Index: Integer]: Single read GetSpotCutoff write SetSpotCutoff;
    property LightSpotExponent[Index: Integer]: Single read GetSpotExponent write SetSpotExponent;
    property LightConstantAtten[Index: Integer]: Single read GetConstantAtten write SetConstantAtten;
    property LightLinearAtten[Index: Integer]: Single read GetLinearAtten write SetLinearAtten;
    property LightQuadraticAtten[Index: Integer]: Single read GetQuadAtten write SetQuadAtten;
    function GetLightIndicesAsAddress: PGLInt;
    function GetLightStateAsAddress: Pointer;
    property LightNumber: Integer read FLightNumber;
    property OnLightsChanged: TGLOnLightsChanged read FOnLightsChanged write FOnLightsChanged;
    // Blending states
    procedure SetGLAlphaFunction(func: TGLComparisonFunction; ref: Single); inline;
    // Vertex Array Data state
    (* The currently bound array buffer (calling glVertexAttribPointer
      locks this buffer to the currently bound VBO). *)
    property VertexArrayBinding: Cardinal read FVertexArrayBinding write SetVertexArrayBinding;
    // The currently bound vertex buffer object (VAO)
    property ArrayBufferBinding: Cardinal read GetArrayBufferBinding write SetArrayBufferBinding;
    // The currently bound element buffer object (EBO)
    property ElementBufferBinding: Cardinal read GetElementBufferBinding write SetElementBufferBinding;
    // Determines whether primitive restart is turned on or off
    property EnablePrimitiveRestart: TGLboolean read GetEnablePrimitiveRestart write SetEnablePrimitiveRestart;
    // The index Value that causes a primitive restart
    property PrimitiveRestartIndex: Cardinal read GetPrimitiveRestartIndex write SetPrimitiveRestartIndex;
    // The currently bound texture buffer object (TBO)
    property TextureBufferBinding: Cardinal read FTextureBufferBinding write SetTextureBufferBinding;
    // Transformation state
    property ViewPort: TVector4i read FViewPort write SetViewPort;
    // Modifies the near + far clipping planes
    procedure SetDepthRange(const ZNear, ZFar: TGLclampd); inline;
    // The near clipping plane distance
    property DepthRangeNear: TGLclampd read GetDepthRangeNear write SetDepthRangeNear;
    // The far clipping plane distance
    property DepthRangeFar: TGLclampd read GetDepthRangeFar write SetDepthRangeFar;
    // Enables/Disables each of the clip distances, used in shaders
    property EnableClipDistance[Index: Cardinal]: TGLboolean read GetEnableClipDistance write SetEnableClipDistance;
    // Enables/Disables depth clamping
    property EnableDepthClamp: TGLboolean read FEnableDepthClamp write SetEnableDepthClamp;
    // Coloring state Controls read color clamping
    property ClampReadColor: Cardinal read FClampReadColor write SetClampReadColor;
    (* The provoking vertex used in flat shading.  All the vertices of each
      primitive will the same value determined by this property. *)
    property ProvokingVertex: Cardinal read FProvokingVertex write SetProvokingVertex;
    // Rasterization state
    (* The default point size, used when EnableProgramPointSize = false *)
    property PointSize: TGLfloat read FPointSize write SetPointSize;
    // If multisampling is enabled, this can control when points are faded out
    property PointFadeThresholdSize: TGLfloat read FPointFadeThresholdSize write SetPointFadeThresholdSize;
    // The texture coordinate origin of point sprites
    property PointSpriteCoordOrigin: Cardinal read FPointSpriteCoordOrigin write SetPointSpriteCoordOrigin;
    // The line width
    property LineWidth: TGLfloat read FLineWidth write SetLineWidth;
    // The line stipple
    property LineStippleFactor: TGLint read FLineStippleFactor write SetLineStippleFactor;
    // The line stipple
    property LineStipplePattern: TGLushort read FLineStipplePattern write SetLineStipplePattern;
    // Enable/Disable line smoothing
    property EnableLineSmooth: TGLboolean read FEnableLineSmooth write SetEnableLineSmooth;
    // Enable/Disable face culling
    property EnableCullFace: TGLboolean read FEnableCullFace write SetEnableCullFace;
    // Selects which faces to cull: front, back or front+back
    property CullFaceMode: TGLCullFaceMode read FCullFaceMode write SetCullFaceMode;
    // The winding direction that indicates a front facing primitive
    property FrontFace: { Cardinal } TGLFaceWinding read FFrontFace write SetFrontFace;
    // Enables/Disables polygon smoothing.
    property EnablePolygonSmooth: TGLboolean read FEnablePolygonSmooth write SetEnablePolygonSmooth;
    // Whether polygons appear filled, lines or points
    property PolygonMode: TGLPolygonMode read FPolygonMode write SetPolygonMode;
    // Scales the maximum depth of the polygon
    property PolygonOffsetFactor: TGLfloat read FPolygonOffsetFactor write SetPolygonOffsetFactor;
    // Scales an implementation-dependent constant that relates to the usable resolution of the depth buffer
    property PolygonOffsetUnits: TGLfloat read FPolygonOffsetUnits write SetPolygonOffsetUnits;
    // Set polygon offset
    procedure SetPolygonOffset(const factor, units: TGLfloat);
    // Enable/Disable polygon offset for polygons in point mode
    property EnablePolygonOffsetPoint: TGLboolean read FEnablePolygonOffsetPoint write SetEnablePolygonOffsetPoint;
    // Enable/Disable polygon offset for polygons in line mode
    property EnablePolygonOffsetLine: TGLboolean read FEnablePolygonOffsetLine write SetEnablePolygonOffsetLine;
    // Enable/Disable polygon offset for polygons in fill mode
    property EnablePolygonOffsetFill: TGLboolean read FEnablePolygonOffsetFill write SetEnablePolygonOffsetFill;
    // Multisample state
    // Enable/Disable multisampling
    property EnableMultisample: TGLboolean read FEnableMultisample write SetEnableMultisample;
    // Enable/Disable sample alpha to coverage
    property EnableSampleAlphaToCoverage: TGLboolean read FEnableSampleAlphaToCoverage write SetEnableSampleAlphaToCoverage;
    // Enable/Disable sample alpha to one
    property EnableSampleAlphaToOne: TGLboolean read FEnableSampleAlphaToOne write SetEnableSampleAlphaToOne;
    // Enable/Disable sample coverage
    property EnableSampleCoverage: TGLboolean read FEnableSampleCoverage write SetEnableSampleCoverage;
    // Sample coverage Value
    property SampleCoverageValue: TGLfloat read FSampleCoverageValue write SetSampleCoverageValue;
    // Inverts sample coverage Value
    property SampleCoverageInvert: TGLboolean read FSampleCoverageInvert write SetSampleCoverageInvert;
    // Set sample coverage
    procedure SetSampleCoverage(const Value: TGLfloat; invert: TGLboolean);
    // Enable/Disable sample mask
    property EnableSampleMask: TGLboolean read FEnableSampleMask write SetEnableSampleMask;
    // Sample mask values
    property SampleMaskValue[Index: Integer]: TGLbitfield read GetSampleMaskValue write SetSampleMaskValue;
    // Textures
    // Textures bound to each texture unit + binding point. 
    property TextureBinding[Index: Integer; target: TGLTextureTarget]: Cardinal read GetTextureBinding write SetTextureBinding;
    property TextureBindingTime[Index: Integer; target: TGLTextureTarget]: Double read GetTextureBindingTime;
    property ActiveTextureEnabled[target: TGLTextureTarget]: Boolean read GetActiveTextureEnabled write SetActiveTextureEnabled;
    property SamplerBinding[Index: Cardinal]: Cardinal read GetSamplerBinding write SetSamplerBinding;
    property MaxTextureSize: Cardinal read GetMaxTextureSize;
    property Max3DTextureSize: Cardinal read GetMax3DTextureSize;
    property MaxCubeTextureSize: Cardinal read GetMaxCubeTextureSize;
    property MaxArrayTextureSize: Cardinal read GetMaxArrayTextureSize;
    property MaxTextureImageUnits: Cardinal read GetMaxTextureImageUnits;
    property MaxTextureAnisotropy: Cardinal read GetMaxTextureAnisotropy;
    property MaxSamples: Cardinal read GetMaxSamples;
    // TODO: GL_TEXTURE_BUFFER_DATA_STORE_BINDING ?
    // Active texture
    (* The active texture unit.  Valid values are 0 .. Max texture units. *)
    property ActiveTexture: TGLint read FActiveTexture write SetActiveTexture;
    // Pixel operations
    (* Enables/Disables scissor test. *)
    property EnableScissorTest: TGLboolean read FEnableScissorTest write SetEnableScissorTest;
    // The bounding box used in scissor test.
    property ScissorBox: TVector4i read FScissorBox write SetScissorBox;
    // Enables/Disables stencil test.
    property EnableStencilTest: TGLboolean read FEnableStencilTest write SetEnableStencilTest;
    (* The stencil function.  Determines the comparison function to be used
      when comparing the reference + stored stencil values. *)
    property StencilFunc: TGLStencilFunction read FStencilFunc;
    // write SetStencilFunc;
    (* The stencil value mask.  Masks both the reference + stored stencil values *)
    property StencilValueMask: Cardinal read FStencilValueMask;
    // write SetStencilValueMask;
    (* The stencil reference value.  Clamped to 0..255 with an 8 bit stencil. *)
    property StencilRef: TGLint read FStencilRef; // write SetStencilRef;
    // The operation to perform when stencil test fails.
    property StencilFail: TGLStencilOp read FStencilFail; // write SetStencilFail;
    // The operation to perform when stencil test passes + depth test fails.
    property StencilPassDepthFail: TGLStencilOp read FStencilPassDepthFail;
    // write SetStencilPassDepthFail;
    (* The operation to perform when stencil test passes + depth test passes. *)
    property StencilPassDepthPass: TGLStencilOp read FStencilPassDepthPass;
    // write SetStencilPassDepthPass;
    (* The stencil back function.  Determines the comparison function to be
      used when comparing the reference + stored stencil values on back facing primitives. *)
    property StencilBackFunc: TGLStencilFunction read FStencilBackFunc;
    // write SetStencilBackFunc;
    (* The stencil back value mask.  Masks both the reference + stored stencil values. *)
    property StencilBackValueMask: Cardinal read FStencilBackValueMask;
    // write SetStencilBackValueMask;
    (* The stencil back reference value.  Clamped to 0..255 with an 8 bit stencil. *)
    property StencilBackRef: Cardinal read FStencilBackRef;
    // write SetStencilBackRef;
    (* The operation to perform when stencil test fails on back facing primitives. *)
    property StencilBackFail: TGLStencilOp read FStencilBackFail;
    // write SetStencilBackFail;
    (* The operation to perform when stencil test passes + depth test fails on
      back facing primitives. *)
    property StencilBackPassDepthFail: TGLStencilOp read FStencilBackPassDepthFail;
    // write SetStencilBackPassDepthFail;
    (* The operation to perform when stencil test passes + depth test passes on
      back facing primitives. *)
    property StencilBackPassDepthPass: TGLStencilOp read FStencilBackPassDepthPass;
    // write SetStencilBackPassDepthPass;
    (* Used to set stencil Function, Reference + Mask values, for both front +
      back facing primitives. *)
    procedure SetStencilFunc(const func: TGLStencilFunction; const ref: TGLint; const mask: Cardinal); inline;
    (* Used to set stencil Function, Reference + Mask values for either the
      front or back facing primitives (or both, which is the same as calling
      SetStencilFunc). *)
    procedure SetStencilFuncSeparate(const face: TGLCullFaceMode; const func: TGLStencilFunction; const ref: TGLint;
      const mask: Cardinal); inline;
    // Used to set the StencilFail, StencilPassDepthFail + StencilPassDepthPass in one go.
    procedure SetStencilOp(const fail, zfail, zpass: TGLStencilOp); inline;
    (* Used to set the StencilFail, StencilPassDepthFail + StencilPassDepthPass
      in one go, for either front or back facing primitives. *)
    procedure SetStencilOpSeparate(const face: TGLCullFaceMode; const sfail, dpfail, dppass: TGLStencilOp); inline;
    // Enables/disables depth testing.
    property EnableDepthTest: TGLboolean read FEnableDepthTest write SetEnableDepthTest;
    (* The depth function.  Used to determine whether to keep a fragment or
      discard it, depending on the current value stored in the depth buffer. *)
    property DepthFunc: TGLDepthFunction read FDepthFunc write SetDepthFunc;
    // Enables/disables blending for each draw buffer.
    property EnableBlend[Index: Integer]: TGLboolean read GetEnableBlend write SetEnableBlend;
    // The weighting factor used in blending equation, for source RGB.
    property BlendSrcRGB: TGLBlendFunction read FBlendSrcRGB;
    // write SetBlendSrcRGB;
    (* The weighting factor used in blending equation, for source alpha. *)
    property BlendSrcAlpha: TGLBlendFunction read FBlendSrcAlpha;
    // write SetBlendSrcAlpha;
    (* The weighting factor used in blending equation, for destination RGB. *)
    property BlendDstRGB: TGLDstBlendFunction read FBlendDstRGB;
    // write SetBlendDstRGB;
    (* The weighting factor used in blending equation, for destination alpha. *)
    property BlendDstAlpha: TGLDstBlendFunction read FBlendDstAlpha;
    // write SetBlendDstAlpha;
    (* Sets the weighting factors to be used by the blending equation, for both color + alpha *)
    procedure SetBlendFunc(const Src: TGLBlendFunction; const Dst: TGLDstBlendFunction); inline;
    (* Sets the weighting factors to be used by the blending equation, with
      separate values used for color + alpha components. *)
    procedure SetBlendFuncSeparate(const SrcRGB: TGLBlendFunction; const DstRGB: TGLDstBlendFunction;
      const SrcAlpha: TGLBlendFunction; const DstAlpha: TGLDstBlendFunction); inline;
    (* The blending equation.  Determines how the incoming source fragment's
      RGB are combined with the destination RGB. *)
    property BlendEquationRGB: TGLBlendEquation read FBlendEquationRGB;
    // write SetBlendEquationRGB;
    (* The blending equation.  Determines how the incoming source fragment's
      alpha values are combined with the destination alpha values. *)
    property BlendEquationAlpha: TGLBlendEquation read FBlendEquationAlpha;
    // write SetBlendEquationAlpha;
    // Sets the blend equation for RGB + alpha to the same value.
    procedure SetBlendEquation(const mode: TGLBlendEquation); inline;
    // Sets the blend equations for RGB + alpha separately.
    procedure SetBlendEquationSeparate(const modeRGB, modeAlpha: TGLBlendEquation); inline;
    // A constant blend color, that can be used in the blend equation.
    property BlendColor: TGLVector read FBlendColor write SetBlendColor;
    // Enables/disables framebuffer SRGB.
    property EnableFramebufferSRGB: TGLboolean read FEnableFramebufferSRGB write SetEnableFramebufferSRGB;
    // Enables/disables dithering.
    property EnableDither: TGLboolean read FEnableDither write SetEnableDither;
    // Enables/disables color logic op.
    property EnableColorLogicOp: TGLboolean read FEnableColorLogicOp write SetEnableColorLogicOp;
    // Logic op mode.
    property LogicOpMode: TGLLogicOp read FLogicOpMode write SetLogicOpMode;
    // The color write mask, for each draw buffer. 
    property ColorWriteMask[Index: Integer]: TGLColorMask read GetColorWriteMask write SetColorWriteMask;
    // Set the color write mask for all draw buffers.
    procedure SetColorMask(mask: TGLColorMask); inline;
    // The depth write mask.
    property DepthWriteMask: TGLboolean read FDepthWriteMask write SetDepthWriteMask;
    // The stencil write mask.
    property StencilWriteMask: Cardinal read FStencilWriteMask write SetStencilWriteMask;
    // The stencil back write mask.
    property StencilBackWriteMask: Cardinal read FStencilBackWriteMask write SetStencilBackWriteMask;
    // The color clear value.
    property ColorClearValue: TGLVector read FColorClearValue write SetColorClearValue;
    // The depth clear value.
    property DepthClearValue: TGLfloat read FDepthClearValue write SetDepthClearValue;
    // The stencil clear value.
    property StencilClearValue: Cardinal read FStencilClearValue write SetStencilClearValue;
    // Framebuffer to be used for draw operations, 0 = default framebuffer.
    property DrawFrameBuffer: Cardinal read FDrawFrameBuffer write SetDrawFrameBuffer;
    // Framebuffer to be used for read operations, 0 = default framebuffer.
    property ReadFrameBuffer: Cardinal read FReadFrameBuffer write SetReadFrameBuffer;
    // set both draw + read framebuffer.
    procedure SetFrameBuffer(const Value: Cardinal); inline;
    // property FrameBuffer: Cardinal read FDrawFrameBuffer write SetFrameBuffer;
    // Renderbuffer currently bound render buffer. 
    property RenderBuffer: Cardinal read FRenderBuffer write SetRenderBuffer;

    // Pixels
    (* Controls whether byte swapping occurs during pixel unpacking. *)
    property UnpackSwapBytes: TGLboolean read FUnpackSwapBytes write SetUnpackSwapBytes;
    // Whether unpacked data is required with LSB (least significant bit) first.
    property UnpackLSBFirst: TGLboolean read FUnpackLSBFirst write SetUnpackLSBFirst;
    // Unpack image height
    property UnpackImageHeight: Cardinal read FUnpackImageHeight write SetUnpackImageHeight;
    // Unpack skip images
    property UnpackSkipImages: Cardinal read FUnpackSkipImages write SetUnpackSkipImages;
    // Unpack row length
    property UnpackRowLength: Cardinal read FUnpackRowLength write SetUnpackRowLength;
    // Unpack skip rows
    property UnpackSkipRows: Cardinal read FUnpackSkipRows write SetUnpackSkipRows;
    // Unpack skip pixels
    property UnpackSkipPixels: Cardinal read FUnpackSkipPixels write SetUnpackSkipPixels;
    // Unpack alignment
    property UnpackAlignment: Cardinal read FUnpackAlignment write SetUnpackAlignment;
    // Controls whether byte swapping occurs during pixel packing
    property PackSwapBytes: TGLboolean read FPackSwapBytes write SetPackSwapBytes;
    // Whether packed data is required with LSB (least significant bit) first
    property PackLSBFirst: TGLboolean read FPackLSBFirst write SetPackLSBFirst;
    // Pack image height
    property PackImageHeight: Cardinal read FPackImageHeight write SetPackImageHeight;
    // Pack skip images
    property PackSkipImages: Cardinal read FPackSkipImages write SetPackSkipImages;
    // Pack row length
    property PackRowLength: Cardinal read FPackRowLength write SetPackRowLength;
    // Pack skip rows
    property PackSkipRows: Cardinal read FPackSkipRows write SetPackSkipRows;
    // Pack skip pixels
    property PackSkipPixels: Cardinal read FPackSkipPixels write SetPackSkipPixels;
    // Pack alignment
    property PackAlignment: Cardinal read FPackAlignment write SetPackAlignment;
    // Buffer bound for pixel packing (eg. ReadPixels)
    property PixelPackBufferBinding: Cardinal read FPixelPackBufferBinding write SetPixelPackBufferBinding;
    // Buffer bound for pixel unpacking (eg. Tex*Image)
    property PixelUnpackBufferBinding: Cardinal read FPixelUnpackBufferBinding write SetPixelUnpackBufferBinding;
    // Currently bound program
    property CurrentProgram: Cardinal read FCurrentProgram write SetCurrentProgram;
    property MaxTextureUnits: Cardinal read GetMaxTextureUnits;
    // Currently bound uniform buffer
    property UniformBufferBinding: Cardinal read FUniformBufferBinding write SetUniformBufferBinding;
    procedure SetBufferIndexedBinding(const Value: Cardinal; ATarget: TGLBufferBindingTarget; AIndex: Cardinal;
      ABufferSize: TGLsizeiptr); overload; inline;
    procedure SetBufferIndexedBinding(const Value: Cardinal; ATarget: TGLBufferBindingTarget; AIndex: Cardinal;
      AOffset: TGLintptr; ARangeSize: TGLsizeiptr); overload; inline;
    // Default values to be used when a vertex array is not used for that attribute
    property CurrentVertexAttrib[Index: Integer]: TGLVector read GetCurrentVertexAttrib write SetCurrentVertexAttrib;
    // Enables/disables program point size
    property EnableProgramPointSize: TGLboolean read FEnableProgramPointSize write SetEnableProgramPointSize;
    // Currently bound transform feedbac buffer
    property TransformFeedbackBufferBinding: Cardinal read FTransformFeedbackBufferBinding
      write SetTransformFeedbackBufferBinding;
    // Line smooth hint
    property LineSmoothHint: TGLHintType read FLineSmoothHint write SetLineSmoothHint;
    // Polygon smooth hint
    property PolygonSmoothHint: TGLHintType read FPolygonSmoothHint write SetPolygonSmoothHint;
    // Texture compression hint
    property TextureCompressionHint: TGLHintType read FTextureCompressionHint write SetTextureCompressionHint;
    // Fragment shader derivitive hint
    property FragmentShaderDerivitiveHint: TGLHintType read FFragmentShaderDerivitiveHint write SetFragmentShaderDerivitiveHint;
    property MultisampleFilterHint: TGLHintType read FMultisampleFilterHint write SetMultisampleFilterHint;
    // Current queries
    property CurrentQuery[Index: TGLQueryType]: Cardinal read GetCurrentQuery;
    // Begins a query of "Target" type.  "Value" must be a valid query object
    procedure BeginQuery(const target: TGLQueryType; const Value: Cardinal); inline;
    // Ends current query of type "Target"
    procedure EndQuery(const target: TGLQueryType); inline;
    (* The buffer currently bound to the copy read buffer binding point, this
      is an extra binding point provided so that you don't need to overwrite
      other binding points to copy between buffers. *)
    property CopyReadBufferBinding: Cardinal read FCopyReadBufferBinding write SetCopyReadBufferBinding;
    (* The buffer currently bound to the copy write buffer binding point, this
      is an extra binding point provided so that you don't need to overwrite
      other binding points to copy between buffers. *)
    property CopyWriteBufferBinding: Cardinal read FCopyWriteBufferBinding write SetCopyWriteBufferBinding;
    // Enables/Disables seamless texture cube maps
    property EnableTextureCubeMapSeamless: TGLboolean read FEnableTextureCubeMapSeamless write SetEnableTextureCubeMapSeamless;
    // Indicates the current presence within the list
    property InsideList: Boolean read FInsideList;
    // Begin new display list
    procedure NewList(list: Cardinal; mode: Cardinal); inline;
    // End display list
    procedure EndList; inline;
    // Call display list
    procedure CallList(list: Cardinal); inline;
    // Defines the OpenGL texture matrix. Assumed texture mode is GL_MODELVIEW.
    procedure SetGLTextureMatrix(const matrix: TGLMatrix); inline;
    procedure ResetGLTextureMatrix; inline;
    procedure ResetAllGLTextureMatrix; inline;
    // note: needs to change to per draw-buffer
    procedure SetGLColorWriting(flag: Boolean); inline;
    // Inverts front face winding (CCW/CW)
    procedure InvertGLFrontFace; inline;
    // read only properties
    property States: TGLStates read FStates;
    // True for ignore deprecated and removed features in OpenGL 3x
    (* property ForwardContext: Boolean read FForwardContext write SetForwardContext; *)
  end;

type
  TStateRecord = record
    GLConst: Cardinal;
    GLDeprecated: Boolean;
  end;

const
{$WARN SYMBOL_DEPRECATED OFF}
  cGLStateTypeToGLEnum: array [TGLStateType] of Cardinal = (GL_CURRENT_BIT, GL_POINT_BIT, GL_LINE_BIT, GL_POLYGON_BIT,
    GL_POLYGON_STIPPLE_BIT, GL_PIXEL_MODE_BIT, GL_LIGHTING_BIT, GL_FOG_BIT, GL_DEPTH_BUFFER_BIT, GL_ACCUM_BUFFER_BIT,
    GL_STENCIL_BUFFER_BIT, GL_VIEWPORT_BIT, GL_TRANSFORM_BIT, GL_ENABLE_BIT, GL_COLOR_BUFFER_BIT, GL_HINT_BIT, GL_EVAL_BIT,
    GL_LIST_BIT, GL_TEXTURE_BIT, GL_SCISSOR_BIT, GL_MULTISAMPLE_BIT);

{$WARN SYMBOL_DEPRECATED ON}
  cGLStateToGLEnum: array[TGLState] of TStateRecord =
    ((GLConst: GL_ALPHA_TEST; GLDeprecated: True),
    (GLConst: GL_AUTO_NORMAL; GLDeprecated: True),
    (GLConst: GL_BLEND; GLDeprecated: False),
    (GLConst: GL_COLOR_MATERIAL; GLDeprecated: True),
    (GLConst: GL_CULL_FACE; GLDeprecated: False),
    (GLConst: GL_DEPTH_TEST; GLDeprecated: False),
    (GLConst: GL_DITHER; GLDeprecated: False),
    (GLConst: GL_FOG; GLDeprecated: True),
    (GLConst: GL_LIGHTING; GLDeprecated: True),
    (GLConst: GL_LINE_SMOOTH; GLDeprecated: True),
    (GLConst: GL_LINE_STIPPLE; GLDeprecated: True),
    (GLConst: GL_INDEX_LOGIC_OP; GLDeprecated: True),
    (GLConst: GL_COLOR_LOGIC_OP; GLDeprecated: False),
    (GLConst: GL_NORMALIZE; GLDeprecated: True),
    (GLConst: GL_POINT_SMOOTH; GLDeprecated: True),
    (GLConst: GL_POINT_SPRITE; GLDeprecated: True),
    (GLConst: GL_POLYGON_SMOOTH; GLDeprecated: True),
    (GLConst: GL_POLYGON_STIPPLE; GLDeprecated: True),
    (GLConst: GL_SCISSOR_TEST; GLDeprecated: False),
    (GLConst: GL_STENCIL_TEST; GLDeprecated: False),
    (GLConst: GL_POLYGON_OFFSET_POINT; GLDeprecated: False),
    (GLConst: GL_POLYGON_OFFSET_LINE; GLDeprecated: False),
    (GLConst: GL_POLYGON_OFFSET_FILL; GLDeprecated: False),
    (GLConst: GL_DEPTH_CLAMP; GLDeprecated: False)
    );

  cGLTexTypeToGLEnum: array[TGLTextureTarget] of Cardinal =
    (0, GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY,
    GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_BUFFER,
    GL_TEXTURE_CUBE_MAP, GL_TEXTURE_2D_MULTISAMPLE,
    GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_CUBE_MAP_ARRAY);

  cGLQueryTypeToGLEnum: array[TGLQueryType] of Cardinal =
    (GL_SAMPLES_PASSED, GL_PRIMITIVES_GENERATED,
    GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN,
    GL_TIME_ELAPSED, GL_ANY_SAMPLES_PASSED);

  cGLStencilOpToGLEnum: array[TGLStencilOp] of Cardinal =
    (GL_KEEP, GL_ZERO, GL_REPLACE, GL_INCR, GL_DECR, GL_INVERT, GL_INCR_WRAP,
    GL_DECR_WRAP);

  cGLLogicOpToGLEnum: array[TGLLogicOp] of Cardinal =
    (GL_CLEAR, GL_AND, GL_AND_REVERSE, GL_COPY, GL_AND_INVERTED, GL_NOOP,
    GL_XOR, GL_OR, GL_NOR, GL_EQUIV, GL_INVERT, GL_OR_REVERSE,
    GL_COPY_INVERTED, GL_OR_INVERTED, GL_NAND, GL_SET);

  cGLComparisonFunctionToGLEnum: array[TGLComparisonFunction] of Cardinal =
    (GL_NEVER, GL_ALWAYS, GL_LESS, GL_LEQUAL, GL_EQUAL, GL_GREATER,
    GL_NOTEQUAL, GL_GEQUAL);

  cGLBlendFunctionToGLEnum: array[TGLBlendFunction] of Cardinal =
    (GL_ZERO, GL_ONE, GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR, GL_DST_COLOR,
    GL_ONE_MINUS_DST_COLOR, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA,
    GL_DST_ALPHA, GL_ONE_MINUS_DST_ALPHA, GL_CONSTANT_COLOR,
    GL_ONE_MINUS_CONSTANT_COLOR, GL_CONSTANT_ALPHA,
    GL_ONE_MINUS_CONSTANT_ALPHA, GL_SRC_ALPHA_SATURATE {valid for src only});

  cGLBlendEquationToGLEnum: array[TGLBlendEquation] of Cardinal =
    (GL_FUNC_ADD, GL_FUNC_SUBTRACT, GL_FUNC_REVERSE_SUBTRACT, GL_MIN,
    GL_MAX);

  cGLFaceWindingToGLEnum: array[TGLFaceWinding] of Cardinal =
    (GL_CCW, GL_CW);

  cGLPolygonModeToGLEnum: array[TGLPolygonMode] of Cardinal =
    (GL_FILL, GL_LINE, GL_POINT);

  cGLCullFaceModeToGLEnum: array[TGLCullFaceMode] of Cardinal =
    (GL_FRONT, GL_BACK, GL_FRONT_AND_BACK);

  cGLHintToGLEnum: array[TGLHintType] of Cardinal =
    (GL_DONT_CARE, GL_FASTEST, GL_NICEST);

  cGLBufferBindingTarget: array[TGLBufferBindingTarget] of Cardinal =
    (GL_UNIFORM_BUFFER, GL_TRANSFORM_FEEDBACK_BUFFER);

//------------------------------------------------------
implementation
// ------------------------------------------------------

uses
  GLS.Context,
  GLS.Color;


// ------------------
// ------------------ TGLStateCache ------------------
// ------------------

procedure TGLStateCache.BeginQuery(const target: TGLQueryType; const Value: Cardinal);
begin
  Assert(FCurrentQuery[target] = 0, 'Can only have one query (of each type)' + ' running at a time');
  // Assert(glIsQuery(Value), 'Not a valid query');
  // if Value<>FCurrentQuery[Target] then
  begin
    FCurrentQuery[target] := Value;
    gl.BeginQuery(cGLQueryTypeToGLEnum[target], Value);
  end;
end;

constructor TGLStateCache.Create;
var
  I: Integer;
begin
  inherited;
  SetLength(FListStates, $FFFFF);
  FCurrentList := 0;

  // Material colors
  FFrontBackColors[0][0] := clrBlack;
  FFrontBackColors[0][1] := clrGray20;
  FFrontBackColors[0][2] := clrGray80;
  FFrontBackColors[0][3] := clrBlack;
  FFrontBackShininess[0] := 0;

  FFrontBackColors[1][0] := clrBlack;
  FFrontBackColors[1][1] := clrGray20;
  FFrontBackColors[1][2] := clrGray80;
  FFrontBackColors[1][3] := clrBlack;
  FFrontBackShininess[1] := 0;

  FAlphaFunc := cfAlways;

  // Lighting
  FFFPLight := True;
  FMaxLights := 0;
  FLightNumber := 0;

  for I := High(FLightEnabling) downto 0 do
  begin
    FLightEnabling[I] := False;
    FLightIndices[I] := 0;
    FLightStates.Position[I] := NullHmgVector;
    FLightStates.Ambient[I] := clrBlack;
    FLightStates.Diffuse[I] := clrBlack;
    FLightStates.Specular[I] := clrBlack;
    FLightStates.SpotDirection[I] := VectorMake(0.0, 0.0, -1.0, 0.0);
    FSpotCutoff[I] := 180.0;
    FLightStates.SpotCosCutoffExponent[I].X := -1;
    FLightStates.SpotCosCutoffExponent[I].Y := 0;
    FLightStates.Attenuation[I] := NullHmgVector;
  end;
  FLightStates.Diffuse[0] := clrWhite;
  FLightStates.Specular[0] := clrWhite;

  for I := High(FTextureMatrixIsIdentity) downto 0 do
    FTextureMatrixIsIdentity[I] := False;
  // FForwardContext := False;

  // Vertex Array Data state
  FVertexArrayBinding := 0;
  FTextureBufferBinding := 0;

  // Transformation state
  // FViewPort := Rect(0,0,0,0);  // (0, 0, Width, Height)
  FDepthRange[0] := 0.0;
  FDepthRange[1] := 1.0;

  FillChar(FEnableClipDistance, sizeof(FEnableClipDistance), $00);
  FEnableDepthClamp := False;

  // Coloring state
  FClampReadColor := GL_FIXED_ONLY;
  FProvokingVertex := GL_LAST_VERTEX_CONVENTION;

  // Rasterization state
  FPointSize := 1.0;
  FPointFadeThresholdSize := 1.0;
  FPointSpriteCoordOrigin := GL_UPPER_LEFT;
  FLineWidth := 1.0;
  FLineStippleFactor := 1;
  FLineStipplePattern := $FFFF;
  FEnableLineSmooth := False;
  FEnableCullFace := False;
  FCullFaceMode := cmBack;
  FFrontFace := fwCounterClockWise;
  FEnablePolygonSmooth := False;
  FPolygonMode := pmFill;
  FPolygonOffsetFactor := 0.0;
  FPolygonOffsetUnits := 0.0;
  FEnablePolygonOffsetPoint := False;
  FEnablePolygonOffsetLine := False;
  FEnablePolygonOffsetFill := False;

  // Multisample state
  FEnableMultisample := True;
  FEnableSampleAlphaToCoverage := False;
  FEnableSampleAlphaToOne := False;
  FEnableSampleCoverage := False;
  FSampleCoverageValue := 1.0;
  FSampleCoverageInvert := False;
  FEnableSampleMask := False;
  FillChar(FSampleMaskValue, sizeof(FSampleMaskValue), $FF);

  // Texture state
  FillChar(FTextureBinding, sizeof(FTextureBinding), $00);
  FillChar(FActiveTextureEnabling, sizeof(FActiveTextureEnabling), $00);

  // Active texture state
  FActiveTexture := 0;

  // Pixel operation state
  FEnableScissorTest := False;
  // FScissorBox := Rect(0, 0, Width, Height);
  FEnableStencilTest := False;
  FStencilFunc := cfAlways;
  FStencilValueMask := $FFFFFFFF;
  FStencilRef := 0;
  FStencilFail := soKeep;
  FStencilPassDepthFail := soKeep;
  FStencilPassDepthPass := soKeep;

  FStencilBackFunc := cfAlways;
  FStencilBackValueMask := $FFFFFFFF;
  FStencilBackRef := 0;
  FStencilBackFail := soKeep;
  FStencilBackPassDepthPass := soKeep;
  FStencilBackPassDepthFail := soKeep;

  FEnableDepthTest := False;
  FDepthFunc := cfLess;

  FillChar(FEnableBlend, sizeof(FEnableBlend), $0);

  FBlendSrcRGB := bfOne;
  FBlendSrcAlpha := bfOne;
  FBlendDstRGB := bfZero;
  FBlendDstAlpha := bfZero;

  FBlendEquationRGB := beAdd;
  FBlendEquationAlpha := beAdd;
  FBlendColor := NullHmgVector;

  FEnableFramebufferSRGB := False;
  FEnableDither := True;
  FEnableColorLogicOp := False;

  FLogicOpMode := loCopy;

  // Framebuffer control state
  // for I := 0 to Length(FColorWriteMask) - 1 do
  // FColorWriteMask[i] := [ccRed, ccGreen, ccBlue, ccAlpha];
  FillChar(FColorWriteMask, sizeof(FColorWriteMask), $F);
  FDepthWriteMask := True;
  FStencilWriteMask := $FFFFFFFF;
  FStencilBackWriteMask := $FFFFFFFF;
  FColorClearValue := NullHmgVector;
  FDepthClearValue := 1.0;
  FStencilClearValue := 0;

  // Framebuffer state
  FDrawFrameBuffer := 0;
  FReadFrameBuffer := 0;

  // Renderbuffer state
  FRenderBuffer := 0;

  // Pixels state
  FUnpackSwapBytes := False;
  FUnpackLSBFirst := False;
  FUnpackImageHeight := 0;
  FUnpackSkipImages := 0;
  FUnpackRowLength := 0;
  FUnpackSkipRows := 0;
  FUnpackSkipPixels := 0;
  FUnpackAlignment := 4;
  FPackSwapBytes := False;
  FPackLSBFirst := False;
  FPackImageHeight := 0;
  FPackSkipImages := 0;
  FPackRowLength := 0;
  FPackSkipRows := 0;
  FPackSkipPixels := 0;
  FPackAlignment := 4;

  FPixelPackBufferBinding := 0;
  FPixelUnpackBufferBinding := 0;

  // Program state
  FCurrentProgram := 0;
  FUniformBufferBinding := 0;
  FillChar(FUBOStates[bbtUniform][0], sizeof(FUBOStates), $00);

  // Vector + Geometry Shader state
  for I := 0 to Length(FCurrentVertexAttrib) - 1 do
    FCurrentVertexAttrib[I] := NullHmgPoint;
  FEnableProgramPointSize := False;

  // Transform Feedback state
  FTransformFeedbackBufferBinding := 0;

  // Hints state
  FTextureCompressionHint := hintDontCare;
  FPolygonSmoothHint := hintDontCare;
  FFragmentShaderDerivitiveHint := hintDontCare;
  FLineSmoothHint := hintDontCare;

  // Misc state
  FillChar(FCurrentQuery, sizeof(FCurrentQuery), $00);
  FCopyReadBufferBinding := 0;
  FCopyWriteBufferBinding := 0;
  FEnableTextureCubeMapSeamless := False;
  FInsideList := False;
end;

destructor TGLStateCache.Destroy;
begin
  inherited;
end;

procedure TGLStateCache.EndQuery(const target: TGLQueryType);
begin
  Assert(FCurrentQuery[target] <> 0, 'No query running');
  FCurrentQuery[target] := 0;
  gl.EndQuery(cGLQueryTypeToGLEnum[target]);
end;

procedure TGLStateCache.Enable(const aState: TGLState);
begin
  { if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit; }
  if not(aState in FStates) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      Include(FStates, aState);
{$IFDEF USE_CACHE_MISS_CHECK}
    if gl.IsEnabled(cGLStateToGLEnum[aState].GLConst) then
      GLSLogger.LogError(strStateCashMissing + 'Enable');
{$ENDIF}
    gl.Enable(cGLStateToGLEnum[aState].GLConst);
  end;
end;

procedure TGLStateCache.Disable(const aState: TGLState);
begin
  { if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit; }
  if (aState in FStates) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      Exclude(FStates, aState);
{$IFDEF USE_CACHE_MISS_CHECK}
    if not gl.IsEnabled(cGLStateToGLEnum[aState].GLConst) then
      GLSLogger.LogError(strStateCashMissing + 'Disable');
{$ENDIF}
    gl.Disable(cGLStateToGLEnum[aState].GLConst);
    if aState = stColorMaterial then
      if FInsideList then
        Include(FListStates[FCurrentList], sttLighting)
      else
        begin
          gl.Materialfv(GL_FRONT, GL_EMISSION, @FFrontBackColors[0][0]);
          gl.Materialfv(GL_FRONT, GL_AMBIENT, @FFrontBackColors[0][1]);
          gl.Materialfv(GL_FRONT, GL_DIFFUSE, @FFrontBackColors[0][2]);
          gl.Materialfv(GL_FRONT, GL_SPECULAR, @FFrontBackColors[0][3]);
          gl.Materiali(GL_FRONT, GL_SHININESS, FFrontBackShininess[0]);

          gl.Materialfv(GL_BACK, GL_EMISSION, @FFrontBackColors[1][0]);
          gl.Materialfv(GL_BACK, GL_AMBIENT, @FFrontBackColors[1][1]);
          gl.Materialfv(GL_BACK, GL_DIFFUSE, @FFrontBackColors[1][2]);
          gl.Materialfv(GL_BACK, GL_SPECULAR, @FFrontBackColors[1][3]);
          gl.Materiali(GL_BACK, GL_SHININESS, FFrontBackShininess[1]);
        end;
  end;
end;

procedure TGLStateCache.PerformEnable(const aState: TGLState);
begin
  { if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit; }
  Include(FStates, aState);
  gl.Enable(cGLStateToGLEnum[aState].GLConst);
end;

procedure TGLStateCache.PerformDisable(const aState: TGLState);
begin
  { if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit; }
  Exclude(FStates, aState);
  gl.Disable(cGLStateToGLEnum[aState].GLConst);
end;

procedure TGLStateCache.PopAttrib;
begin
  // TODO: replace with proper client side push/pop
  gl.PopAttrib();
end;

procedure TGLStateCache.PushAttrib(const stateTypes: TGLStateTypes);
var
  tempFlag: Cardinal;
  I: Integer;
begin
  // TODO: replace with proper client side push/pop
  tempFlag := 0;
  for I := Integer(Low(TGLStateType)) to Integer(high(TGLStateType)) do
  begin
    if TGLStateType(I) in stateTypes then
    begin
      tempFlag := tempFlag or cGLStateTypeToGLEnum[TGLStateType(I)];
    end;
  end;
  gl.PushAttrib(tempFlag);
end;

procedure TGLStateCache.SetGLMaterialColors(const aFace: TGLCullFaceMode;
  const emission, ambient, diffuse, specular: TGLVector;
  const shininess: Integer);
var
  I: Integer;
  currentFace: Cardinal;
begin
  { if FForwardContext then
    exit; }
  Assert((aFace = cmFront) or (aFace = cmBack), 'Only cmFront or cmBack supported');
  I := Integer(aFace);
  currentFace := cGLCullFaceModeToGLEnum[aFace];

  if FInsideList then
  begin

    gl.Materiali(currentFace, GL_SHININESS, shininess);
    gl.Materialfv(currentFace, GL_EMISSION, @emission);
    gl.Materialfv(currentFace, GL_AMBIENT, @Ambient);
    gl.Materialfv(currentFace, GL_DIFFUSE, @Diffuse);
    gl.Materialfv(currentFace, GL_SPECULAR, @Specular);
    Include(FListStates[FCurrentList], sttLighting);
  end
  else
  begin
    if (FFrontBackShininess[I] <> shininess) then
    begin
      gl.Materiali(currentFace, GL_SHININESS, shininess);
      FFrontBackShininess[I] := shininess;
    end;
    if not AffineVectorEquals(FFrontBackColors[I][0], emission) then
    begin
      gl.Materialfv(currentFace, GL_EMISSION, @emission);
      SetVector(FFrontBackColors[I][0], emission);
    end;
    if not AffineVectorEquals(FFrontBackColors[I][1], Ambient) then
    begin
      gl.Materialfv(currentFace, GL_AMBIENT, @Ambient);
      SetVector(FFrontBackColors[I][1], Ambient);
    end;
    if not VectorEquals(FFrontBackColors[I][2], Diffuse) then
    begin
      gl.Materialfv(currentFace, GL_DIFFUSE, @Diffuse);
      SetVector(FFrontBackColors[I][2], Diffuse);
    end;
    if not AffineVectorEquals(FFrontBackColors[I][3], Specular) then
    begin
      gl.Materialfv(currentFace, GL_SPECULAR, @Specular);
      SetVector(FFrontBackColors[I][3], Specular);
    end;
  end;
end;

procedure TGLStateCache.SetGLMaterialAlphaChannel(const aFace: Cardinal; const alpha: TGLfloat);
var
  I: Integer;
  color: TVector4f;
begin
  { if FForwardContext then Exit; }

  if not(stLighting in FStates) then
  begin
    // We need a temp variable, because FColor is cauched.
    gl.GetFloatv(GL_CURRENT_COLOR, @color);
    color.W := alpha;
    gl.Color4fv(@color);
  end
  else
  begin
    I := aFace - GL_FRONT;
    if (FFrontBackColors[I][2].W <> alpha) or FInsideList then
    begin
      if FInsideList then
      begin
        Include(FListStates[FCurrentList], sttLighting);
        gl.Materialfv(aFace, GL_DIFFUSE, @FFrontBackColors[I][2]);

      end
      else
      begin
        FFrontBackColors[I][2].W := alpha;
        gl.Materialfv(aFace, GL_DIFFUSE, @FFrontBackColors[I][2]);
      end;
    end;
  end;
end;

procedure TGLStateCache.SetGLMaterialDiffuseColor(const aFace: Cardinal; const Diffuse: TGLVector);
var
  I: Integer;
begin
  { if FForwardContext then Exit; }

  if not(stLighting in FStates) then
  begin
    gl.Color4fv(@Diffuse);
  end
  else
  begin
    //
    I := aFace - GL_FRONT;
    if (not VectorEquals(FFrontBackColors[I][2], Diffuse)) or FInsideList then
    begin
      if FInsideList then
      begin
        Include(FListStates[FCurrentList], sttLighting);
        gl.Materialfv(aFace, GL_DIFFUSE, @FFrontBackColors[I][2]);
      end
      else
      begin
        FFrontBackColors[I][2] := Diffuse;
        gl.Materialfv(aFace, GL_DIFFUSE, @Diffuse);
      end;
    end;
  end;
end;

procedure TGLStateCache.SetActiveTexture(const Value: TGLint);
begin
  if gl.ARB_multitexture then
    if (Value <> FActiveTexture) or FInsideList then
    begin
      if FInsideList then
        Include(FListStates[FCurrentList], sttTexture)
      else
        FActiveTexture := Value;
      gl.ActiveTexture(GL_TEXTURE0 + Value);
    end;
end;

procedure TGLStateCache.SetVertexArrayBinding(const Value: Cardinal);
begin
  if Value <> FVertexArrayBinding then
  begin
    FVertexArrayBinding := Value;
    gl.BindVertexArray(Value);
  end;
end;

function TGLStateCache.GetArrayBufferBinding: Cardinal;
begin
  Result := FArrayBufferBinding;
end;

procedure TGLStateCache.SetArrayBufferBinding(const Value: Cardinal);
begin
  if (Value <> FArrayBufferBinding) or (FVertexArrayBinding <> 0) then
  begin
    FArrayBufferBinding := Value;
    gl.BindBuffer(GL_ARRAY_BUFFER, Value);
  end;
end;

function TGLStateCache.GetElementBufferBinding: Cardinal;
begin
  Result := FElementBufferBinding
end;

procedure TGLStateCache.SetElementBufferBinding(const Value: Cardinal);
begin
  if (Value <> FElementBufferBinding) or (FVertexArrayBinding <> 0) then
  begin
    FElementBufferBinding := Value;
    gl.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, Value);
  end;
end;

function TGLStateCache.GetEnablePrimitiveRestart: TGLboolean;
begin
  Result := FEnablePrimitiveRestart;
end;

procedure TGLStateCache.SetEnablePrimitiveRestart(const enabled: TGLboolean);
begin
  if enabled <> FEnablePrimitiveRestart then
  begin
    FEnablePrimitiveRestart := enabled;
    if GL.NV_primitive_restart then
    begin
      if enabled then
        gl.EnableClientState(GL_PRIMITIVE_RESTART_NV)
      else
        gl.DisableClientState(GL_PRIMITIVE_RESTART_NV);
    end
    else
    begin
      if enabled then
        gl.Enable(GL_PRIMITIVE_RESTART)
      else
        gl.Disable(GL_PRIMITIVE_RESTART);

    end;
  end;
end;

function TGLStateCache.GetPrimitiveRestartIndex: Cardinal;
begin
  Result := FPrimitiveRestartIndex;
end;

procedure TGLStateCache.SetPrimitiveRestartIndex(const index: Cardinal);
begin
  if index <> FPrimitiveRestartIndex then
  begin
    if gl.NV_primitive_restart then
    begin
      FPrimitiveRestartIndex := index;
      gl.PrimitiveRestartIndex(index)
    end;
  end;
end;

procedure TGLStateCache.SetEnableProgramPointSize(const Value: TGLboolean);
begin
  if Value <> FEnableProgramPointSize then
  begin
    FEnableProgramPointSize := Value;
    if Value then
      gl.Enable(GL_PROGRAM_POINT_SIZE)
    else
      gl.Disable(GL_PROGRAM_POINT_SIZE);
  end;
end;

procedure TGLStateCache.SetBlendColor(const Value: TGLVector);
begin
  if not VectorEquals(Value, FBlendColor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FBlendColor := Value;
    gl.BlendColor(Value.X, Value.Y, Value.Z, Value.W);
  end;
end;

procedure TGLStateCache.SetBlendEquationSeparate(const modeRGB, modeAlpha: TGLBlendEquation);
begin
  if (modeRGB <> FBlendEquationRGB) or (modeAlpha <> FBlendEquationAlpha) or FInsideList then
  begin
    FBlendEquationRGB := modeRGB;
    FBlendEquationAlpha := modeAlpha;
    gl.BlendEquationSeparate(cGLBlendEquationToGLEnum[modeRGB], cGLBlendEquationToGLEnum[modeAlpha]);
  end;
  if FInsideList then
    Include(FListStates[FCurrentList], sttColorBuffer);
end;

procedure TGLStateCache.SetBlendEquation(const mode: TGLBlendEquation);
begin
  if (mode <> FBlendEquationRGB) or (mode <> FBlendEquationAlpha) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FBlendEquationRGB := mode;
      FBlendEquationAlpha := mode;
    end;
    gl.BlendEquation(cGLBlendEquationToGLEnum[mode]);
  end;
end;

procedure TGLStateCache.SetBlendFunc(const Src: TGLBlendFunction; const Dst: TGLDstBlendFunction);
begin
  if (Src <> FBlendSrcRGB) or (Dst <> FBlendDstRGB) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FBlendSrcRGB := Src;
      FBlendDstRGB := Dst;
      FBlendSrcAlpha := Src;
      FBlendSrcAlpha := Dst;
    end;
    gl.BlendFunc(cGLBlendFunctionToGLEnum[Src], cGLBlendFunctionToGLEnum[Dst]);
  end;
end;

procedure TGLStateCache.SetBlendFuncSeparate(const SrcRGB: TGLBlendFunction;
  const DstRGB: TGLDstBlendFunction; const SrcAlpha: TGLBlendFunction; const DstAlpha: TGLDstBlendFunction);
begin
  if (SrcRGB <> FBlendSrcRGB) or (DstRGB <> FBlendDstRGB) or
     (SrcAlpha <> FBlendSrcAlpha) or (DstAlpha <> FBlendDstAlpha)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FBlendSrcRGB := SrcRGB;
      FBlendDstRGB := DstRGB;
      FBlendSrcAlpha := SrcAlpha;
      FBlendDstAlpha := DstAlpha;
    end;
    gl.BlendFuncSeparate(
      cGLBlendFunctionToGLEnum[SrcRGB],
      cGLBlendFunctionToGLEnum[DstRGB],
      cGLBlendFunctionToGLEnum[SrcAlpha],
      cGLBlendFunctionToGLEnum[DstAlpha]);
  end;
end;

procedure TGLStateCache.SetClampReadColor(const Value: Cardinal);
begin
  if (Value <> FClampReadColor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FClampReadColor := Value;
    gl.ClampColor(GL_CLAMP_READ_COLOR, Value);
  end;
end;

procedure TGLStateCache.SetColorWriteMask(index: Integer; const Value: TGLColorMask);
begin
  if FColorWriteMask[Index] <> Value then
  begin
    FColorWriteMask[Index] := Value;
    gl.ColorMaski(Index, ccRed in Value, ccGreen in Value, ccBlue in Value, ccAlpha in Value);
  end;
end;

procedure TGLStateCache.SetCopyReadBufferBinding(const Value: Cardinal);
begin
  if Value <> FCopyReadBufferBinding then
  begin
    FCopyReadBufferBinding := Value;
    gl.BindBuffer(GL_COPY_READ_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetCopyWriteBufferBinding(const Value: Cardinal);
begin
  if Value <> FCopyWriteBufferBinding then
  begin
    FCopyWriteBufferBinding := Value;
    gl.BindBuffer(GL_COPY_WRITE_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetCullFaceMode(const Value: TGLCullFaceMode);
begin
  if (Value <> FCullFaceMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FCullFaceMode := Value;
    gl.CullFace(cGLCullFaceModeToGLEnum[Value]);
  end;

end;

procedure TGLStateCache.SetCurrentProgram(const Value: Cardinal);
begin
  if Value <> FCurrentProgram then
  begin
    FCurrentProgram := Value;
    gl.UseProgram(Value);
  end;
end;

procedure TGLStateCache.SetTextureBufferBinding(const Value: Cardinal);
begin
  if Value <> FTextureBufferBinding then
  begin
    FTextureBufferBinding := Value;
    gl.BindBuffer(GL_TEXTURE_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetCurrentVertexAttrib(index: Integer; const Value: TGLVector);
begin
  if not VectorEquals(Value, FCurrentVertexAttrib[Index]) then
  begin
    FCurrentVertexAttrib[Index] := Value;
    gl.VertexAttrib4fv(Index, @Value.X);
  end;
end;

procedure TGLStateCache.SetDepthClearValue(const Value: TGLfloat);
begin
  if (Value <> FDepthClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthClearValue := Value;
    gl.ClearDepth(Value);
  end;
end;

procedure TGLStateCache.SetDepthFunc(const Value: TGLDepthFunction);
begin
  if (Value <> FDepthFunc) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthFunc := Value;
    gl.DepthFunc(cGLComparisonFunctionToGLEnum[Value]);
  end;

end;

procedure TGLStateCache.SetDepthRange(const ZNear, ZFar: TGLclampd);
begin
  if (ZNear <> FDepthRange[0]) or (ZFar <> FDepthRange[1]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
    begin
      FDepthRange[0] := ZNear;
      FDepthRange[1] := ZFar;
    end;
    gl.DepthRange(ZNear, ZFar);
  end;
end;

procedure TGLStateCache.SetDepthRangeFar(const Value: TGLclampd);
begin
  if (Value <> FDepthRange[1]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FDepthRange[1] := Value;
    gl.DepthRange(FDepthRange[0], Value);
  end;
end;

procedure TGLStateCache.SetDepthRangeNear(const Value: TGLclampd);
begin
  if (Value <> FDepthRange[0]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FDepthRange[0] := Value;
    gl.DepthRange(Value, FDepthRange[1]);
  end;
end;

procedure TGLStateCache.SetDepthWriteMask(const Value: TGLboolean);
begin
  if (Value <> FDepthWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthWriteMask := Value;
    gl.DepthMask(Value);
  end;
end;

procedure TGLStateCache.SetDrawFrameBuffer(const Value: Cardinal);
begin
  if Value <> FDrawFrameBuffer then
  begin
    FDrawFrameBuffer := Value;
    gl.BindFramebuffer(GL_DRAW_FRAMEBUFFER, Value);
  end;
end;

procedure TGLStateCache.SetEnableBlend(index: Integer; const Value: TGLboolean);
begin
  if FEnableBlend[Index] <> Value then
  begin
    FEnableBlend[Index] := Value;
    if Value then
      gl.Enablei(GL_BLEND, Index)
    else
      gl.Disablei(GL_BLEND, Index);
  end;
end;

procedure TGLStateCache.SetEnableClipDistance(index: Cardinal; const Value: TGLboolean);
begin
  if FEnableClipDistance[Index] <> Value then
  begin
    FEnableClipDistance[Index] := Value;
    if Value then
      gl.Enable(GL_CLIP_DISTANCE0 + Index)
    else
      gl.Disable(GL_CLIP_DISTANCE0 + Index);
  end;
end;

procedure TGLStateCache.SetEnableColorLogicOp(const Value: TGLboolean);
begin
  if Value <> FEnableColorLogicOp then
  begin
    FEnableColorLogicOp := Value;
    if Value then
      gl.Enable(GL_COLOR_LOGIC_OP)
    else
      gl.Disable(GL_COLOR_LOGIC_OP);
  end;
end;

procedure TGLStateCache.SetEnableCullFace(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableDepthClamp(const enabled: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableDepthTest(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableDither(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableFramebufferSRGB(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableLineSmooth(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableMultisample(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnablePolygonOffsetFill(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnablePolygonOffsetLine(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnablePolygonOffsetPoint(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnablePolygonSmooth(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableSampleAlphaToCoverage(const Value: TGLboolean);
begin
  if Value <> FEnableSampleAlphaToCoverage then
  begin
    FEnableSampleAlphaToCoverage := Value;
    if Value then
      gl.Enable(GL_SAMPLE_ALPHA_TO_COVERAGE)
    else
      gl.Disable(GL_SAMPLE_ALPHA_TO_COVERAGE);
  end;
end;

procedure TGLStateCache.SetEnableSampleCoverage(const Value: TGLboolean);
begin
  if Value <> FEnableSampleCoverage then
  begin
    FEnableSampleCoverage := Value;
    if Value then
      gl.Enable(GL_SAMPLE_COVERAGE)
    else
      gl.Disable(GL_SAMPLE_COVERAGE);
  end;
end;

procedure TGLStateCache.SetEnableSampleMask(const Value: TGLboolean);
begin
  if Value <> FEnableSampleMask then
  begin
    FEnableSampleMask := Value;
    if Value then
      gl.Enable(GL_SAMPLE_MASK)
    else
      gl.Disable(GL_SAMPLE_MASK);
  end;
end;

procedure TGLStateCache.SetEnableSampleAlphaToOne(const Value: TGLboolean);
begin
  if Value <> FEnableSampleAlphaToOne then
  begin
    FEnableSampleAlphaToOne := Value;
    if Value then
      gl.Enable(GL_SAMPLE_ALPHA_TO_ONE)
    else
      gl.Disable(GL_SAMPLE_ALPHA_TO_ONE);
  end;
end;

procedure TGLStateCache.SetEnableScissorTest(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetEnableStencilTest(const Value: TGLboolean);
begin

end;

procedure TGLStateCache.SetFragmentShaderDerivitiveHint(const Value: TGLHintType);
begin
  if Value <> FFragmentShaderDerivitiveHint then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FFragmentShaderDerivitiveHint := Value;
    gl.Hint(GL_FRAGMENT_SHADER_DERIVATIVE_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetMultisampleFilterHint(const Value: TGLHintType);
begin
  if GL.NV_multisample_filter_hint then
    if Value <> FMultisampleFilterHint then
    begin
      if FInsideList then
        Include(FListStates[FCurrentList], sttHint)
      else
        FMultisampleFilterHint := Value;
      gl.Hint(GL_MULTISAMPLE_FILTER_HINT_NV, cGLHintToGLEnum[Value]);
    end;
end;

procedure TGLStateCache.SetFrameBuffer(const Value: Cardinal);
begin
  if (Value <> FDrawFrameBuffer) or (Value <> FReadFrameBuffer) or FInsideList then
  begin
    FDrawFrameBuffer := Value;
    FReadFrameBuffer := Value;
    gl.BindFramebuffer(GL_FRAMEBUFFER, Value);
  end;
end;

procedure TGLStateCache.SetFrontFace(const Value: TGLFaceWinding);
begin
  if (Value <> FFrontFace) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FFrontFace := Value;
    gl.FrontFace(cGLFaceWindingToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetGLAlphaFunction(func: TGLComparisonFunction; ref: Single);
{$IFDEF USE_CACHE_MISS_CHECK}
var
  I: Cardinal;
  E: Single;
{$ENDIF}
begin
  { if FForwardContext then
    exit; }
{$IFDEF USE_CACHE_MISS_CHECK}
  gl.GetIntegerv(GL_ALPHA_TEST_FUNC, @I);
  if cGLComparisonFunctionToGLEnum[FAlphaFunc] <> I then
    GLSLogger.LogError(strStateCashMissing + 'AlphaTest function');
  gl.GetFloatv(GL_ALPHA_TEST_REF, @E);
  if FAlphaRef <> E then
    GLSLogger.LogError(strStateCashMissing + 'AlphaTest reference');
{$ENDIF}
  if (FAlphaFunc <> func) or (FAlphaRef <> ref) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FAlphaFunc := func;
      FAlphaRef := ref;
    end;
    gl.AlphaFunc(cGLComparisonFunctionToGLEnum[func], ref);
  end;
end;

function TGLStateCache.GetColorWriteMask(index: Integer): TGLColorMask;
begin
  Result := FColorWriteMask[Index];
end;

function TGLStateCache.GetCurrentQuery(index: TGLQueryType): Cardinal;
begin
  Result := FCurrentQuery[Index];
end;

function TGLStateCache.GetCurrentVertexAttrib(index: Integer): TGLVector;
begin
  Result := FCurrentVertexAttrib[Index];
end;

function TGLStateCache.GetDepthRangeFar: TGLclampd;
begin
  Result := FDepthRange[1];
end;

function TGLStateCache.GetDepthRangeNear: TGLclampd;
begin
  Result := FDepthRange[0];
end;

function TGLStateCache.GetEnableBlend(index: Integer): TGLboolean;
begin
  Result := FEnableBlend[Index];
end;

function TGLStateCache.GetEnableClipDistance(ClipDistance: Cardinal): TGLboolean;
begin
  Result := FEnableClipDistance[ClipDistance];
end;

function TGLStateCache.GetSampleMaskValue(index: Integer): TGLbitfield;
begin
  Result := FSampleMaskValue[Index];
end;

function TGLStateCache.GetMaxTextureSize: Cardinal;
begin
  if FMaxTextureSize = 0 then
    gl.GetIntegerv(GL_MAX_TEXTURE_SIZE, @FMaxTextureSize);
  Result := FMaxTextureSize;
end;

function TGLStateCache.GetMaterialAmbient(const aFace: TGLCullFaceMode): TGLVector;
begin
  Result := FFrontBackColors[ord(aFace)][1];
end;

function TGLStateCache.GetMaterialDiffuse(const aFace: TGLCullFaceMode): TGLVector;
begin
  Result := FFrontBackColors[ord(aFace)][2];
end;

function TGLStateCache.GetMaterialEmission(const aFace: TGLCullFaceMode): TGLVector;
begin
  Result := FFrontBackColors[ord(aFace)][0];
end;

function TGLStateCache.GetMaterialShininess(const aFace: TGLCullFaceMode): Integer;
begin
  Result := FFrontBackShininess[ord(aFace)];
end;

function TGLStateCache.GetMaterialSpecular(const aFace: TGLCullFaceMode): TGLVector;
begin
  Result := FFrontBackColors[ord(aFace)][3];
end;

function TGLStateCache.GetMax3DTextureSize: Cardinal;
begin
  if FMax3DTextureSize = 0 then
    gl.GetIntegerv(GL_MAX_3D_TEXTURE_SIZE, @FMax3DTextureSize);
  Result := FMax3DTextureSize;
end;

function TGLStateCache.GetMaxCubeTextureSize: Cardinal;
begin
  if FMaxCubeTextureSize = 0 then
    gl.GetIntegerv(GL_MAX_CUBE_MAP_TEXTURE_SIZE, @FMaxCubeTextureSize);
  Result := FMaxCubeTextureSize;
end;

function TGLStateCache.GetMaxArrayTextureSize: Cardinal;
begin
  if FMaxArrayTextureSize = 0 then
    gl.GetIntegerv(GL_MAX_ARRAY_TEXTURE_LAYERS, @FMaxArrayTextureSize);
  Result := FMaxArrayTextureSize;
end;

function TGLStateCache.GetMaxTextureImageUnits: Cardinal;
begin
  if FMaxTextureImageUnits = 0 then
    gl.GetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, @FMaxTextureImageUnits);
  Result := FMaxTextureImageUnits;
end;

function TGLStateCache.GetMaxTextureAnisotropy: Cardinal;
begin
  if (FMaxTextureAnisotropy = 0) and GL.EXT_texture_filter_anisotropic then
    gl.GetIntegerv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @FMaxTextureAnisotropy);
  Result := FMaxTextureAnisotropy;
end;

function TGLStateCache.GetMaxSamples: Cardinal;
begin
  if (FMaxSamples = 0) and GL.EXT_multisample then
    gl.GetIntegerv(GL_MAX_SAMPLES, @FMaxSamples);
  Result := FMaxSamples;
end;

function TGLStateCache.GetTextureBinding(index: Integer; target: TGLTextureTarget): Cardinal;
begin
  Result := FTextureBinding[Index, target];
end;

function TGLStateCache.GetTextureBindingTime(index: Integer; target: TGLTextureTarget): Double;
begin
  Result := FTextureBindingTime[Index, target];
end;

function TGLStateCache.GetSamplerBinding(index: Cardinal): Cardinal;
begin
  Result := FSamplerBinding[Index];
end;

procedure TGLStateCache.SetSamplerBinding(index: Cardinal; const Value: Cardinal);
begin
  if Index > High(FSamplerBinding) then
    exit;
  if (Value <> FSamplerBinding[Index]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttTexture)
    else
      FSamplerBinding[Index] := Value;
    gl.BindSampler(Index, Value);
  end;
end;

procedure TGLStateCache.SetGLTextureMatrix(const matrix: TGLMatrix);
begin
  { if FForwardContext then
    exit; }
  if FInsideList then
    Include(FListStates[FCurrentList], sttTransform)
  else
    FTextureMatrixIsIdentity[ActiveTexture] := False;
  gl.MatrixMode(GL_TEXTURE);
  gl.LoadMatrixf(PGLFloat(@matrix.V[0].X));
  gl.MatrixMode(GL_MODELVIEW);
end;

procedure TGLStateCache.ResetGLTextureMatrix;
begin
  { if FForwardContext then
    exit; }
  gl.MatrixMode(GL_TEXTURE);
  gl.LoadIdentity;
  FTextureMatrixIsIdentity[ActiveTexture] := True;
  gl.MatrixMode(GL_MODELVIEW);
end;

procedure TGLStateCache.ResetAllGLTextureMatrix;
var
  I: Integer;
  lastActiveTexture: Cardinal;
begin
  { if FForwardContext then
    exit; }
  lastActiveTexture := ActiveTexture;
  gl.MatrixMode(GL_TEXTURE);
  for I := High(FTextureMatrixIsIdentity) downto 0 do
    if not FTextureMatrixIsIdentity[I] then
    begin
      ActiveTexture := I;
      gl.LoadIdentity;
      FTextureMatrixIsIdentity[I] := True;
    end;
  gl.MatrixMode(GL_MODELVIEW);
  ActiveTexture := lastActiveTexture;
end;

procedure TGLStateCache.SetLineSmoothHint(const Value: TGLHintType);
begin
  if (Value <> FLineSmoothHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FLineSmoothHint := Value;
    gl.Hint(GL_LINE_SMOOTH_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetLineWidth(const Value: TGLfloat);
begin
  // note: wide lines no longer deprecated (see OpenGL spec)
  if (Value <> FLineWidth) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLine)
    else
      FLineWidth := Value;
    gl.LineWidth(Value);
  end;
end;

procedure TGLStateCache.SetLineStippleFactor(const Value: TGLint);
begin
  if (Value <> FLineStippleFactor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLine)
    else
      FLineStippleFactor := Value;
    gl.LineStipple(Value, FLineStipplePattern);
  end;
end;

procedure TGLStateCache.SetLineStipplePattern(const Value: TGLushort);
begin
  if (Value <> FLineStipplePattern) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLine)
    else
      FLineStipplePattern := Value;
    gl.LineStipple(FLineStippleFactor, Value);
  end;
end;

procedure TGLStateCache.SetLogicOpMode(const Value: TGLLogicOp);
begin
  if (Value <> FLogicOpMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FLogicOpMode := Value;
    gl.LogicOp(cGLLogicOpToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetPackAlignment(const Value: Cardinal);
begin
  if Value <> FPackAlignment then
  begin
    FPackAlignment := Value;
    gl.PixelStoref(GL_PACK_ALIGNMENT, Value);
  end;
end;

procedure TGLStateCache.SetPackImageHeight(const Value: Cardinal);
begin
  if Value <> FPackImageHeight then
  begin
    FPackImageHeight := Value;
    gl.PixelStoref(GL_PACK_IMAGE_HEIGHT, Value);
  end;
end;

procedure TGLStateCache.SetPackLSBFirst(const Value: TGLboolean);
begin
  if Value <> FPackLSBFirst then
  begin
    FPackLSBFirst := Value;
    gl.PixelStorei(GL_PACK_LSB_FIRST, byte(Value));
  end;
end;

procedure TGLStateCache.SetPackRowLength(const Value: Cardinal);
begin
  if Value <> FPackRowLength then
  begin
    FPackRowLength := Value;
    gl.PixelStoref(GL_PACK_ROW_LENGTH, Value);
  end;
end;

procedure TGLStateCache.SetPackSkipImages(const Value: Cardinal);
begin
  if Value <> FPackSkipImages then
  begin
    FPackSkipImages := Value;
    gl.PixelStoref(GL_PACK_SKIP_IMAGES, Value);
  end;
end;

procedure TGLStateCache.SetPackSkipPixels(const Value: Cardinal);
begin
  if Value <> FPackSkipPixels then
  begin
    FPackSkipPixels := Value;
    gl.PixelStoref(GL_PACK_SKIP_PIXELS, Value);
  end;
end;

procedure TGLStateCache.SetPackSkipRows(const Value: Cardinal);
begin
  if Value <> FPackSkipRows then
  begin
    FPackSkipRows := Value;
    gl.PixelStoref(GL_PACK_SKIP_ROWS, Value);
  end;
end;

procedure TGLStateCache.SetPackSwapBytes(const Value: TGLboolean);
begin
  if Value <> FPackSwapBytes then
  begin
    FPackSwapBytes := Value;
    gl.PixelStorei(GL_PACK_SWAP_BYTES, byte(Value));
  end;
end;

procedure TGLStateCache.SetPixelPackBufferBinding(const Value: Cardinal);
begin
  if Value <> FPixelPackBufferBinding then
  begin
    FPixelPackBufferBinding := Value;
    gl.BindBuffer(GL_PIXEL_PACK_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetPixelUnpackBufferBinding(const Value: Cardinal);
begin
  if Value <> FPixelUnpackBufferBinding then
  begin
    FPixelUnpackBufferBinding := Value;
    gl.BindBuffer(GL_PIXEL_UNPACK_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetPointFadeThresholdSize(const Value: TGLfloat);
begin
  if (Value <> FPointFadeThresholdSize) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointFadeThresholdSize := Value;
    gl.PointParameterf(GL_POINT_FADE_THRESHOLD_SIZE, Value);
  end;
end;

procedure TGLStateCache.SetPointSize(const Value: TGLfloat);
begin
  if (Value <> FPointSize) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointSize := Value;
    gl.PointSize(Value);
  end;
end;

procedure TGLStateCache.SetPointSpriteCoordOrigin(const Value: Cardinal);
begin
  if (Value <> FPointSpriteCoordOrigin) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointSpriteCoordOrigin := Value;
    gl.PointParameterf(GL_POINT_SPRITE_COORD_ORIGIN, Value);
  end;
end;

procedure TGLStateCache.SetPolygonMode(const Value: TGLPolygonMode);
begin
  if (Value <> FPolygonMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
    begin
      FPolygonMode := Value;
      FPolygonBackMode := Value;
    end;
    gl.PolygonMode(GL_FRONT_AND_BACK, cGLPolygonModeToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetPolygonOffset(const factor, units: TGLfloat);
begin
  if (factor <> FPolygonOffsetFactor) or (units <> FPolygonOffsetUnits) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
    begin
      FPolygonOffsetFactor := factor;
      FPolygonOffsetUnits := units;
    end;
    gl.PolygonOffset(factor, units);
  end;
end;

procedure TGLStateCache.SetPolygonOffsetFactor(const Value: TGLfloat);
begin
  if (Value <> FPolygonOffsetFactor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FPolygonOffsetFactor := Value;
    gl.PolygonOffset(Value, FPolygonOffsetUnits);
  end;
end;

procedure TGLStateCache.SetPolygonOffsetUnits(const Value: TGLfloat);
begin
  if (Value <> FPolygonOffsetUnits) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FPolygonOffsetUnits := Value;
    gl.PolygonOffset(FPolygonOffsetFactor, Value);
  end;
end;

procedure TGLStateCache.SetPolygonSmoothHint(const Value: TGLHintType);
begin
  if (Value <> FPolygonSmoothHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FPolygonSmoothHint := Value;
    gl.Hint(GL_POLYGON_SMOOTH_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetProvokingVertex(const Value: Cardinal);
begin
  if Value <> FProvokingVertex then
  begin
    FProvokingVertex := Value;
    gl.ProvokingVertex(Value);
  end;
end;

procedure TGLStateCache.SetReadFrameBuffer(const Value: Cardinal);
begin
  if Value <> FReadFrameBuffer then
  begin
    FReadFrameBuffer := Value;
    gl.BindFramebuffer(GL_READ_FRAMEBUFFER, Value);
  end;
end;

procedure TGLStateCache.SetRenderBuffer(const Value: Cardinal);
begin
  if Value <> FRenderBuffer then
  begin
    FRenderBuffer := Value;
    gl.BindRenderbuffer(GL_RENDERBUFFER, Value);
  end;
end;

procedure TGLStateCache.SetSampleCoverage(const Value: TGLfloat; invert: TGLboolean);
begin
  if (Value <> FSampleCoverageValue) or (invert <> FSampleCoverageInvert) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
    begin
      FSampleCoverageValue := Value;
      FSampleCoverageInvert := invert;
    end;
    gl.SampleCoverage(Value, invert);
  end;
end;

procedure TGLStateCache.SetSampleCoverageInvert(const Value: TGLboolean);
begin
  if (Value <> FSampleCoverageInvert) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleCoverageInvert := Value;
    gl.SampleCoverage(FSampleCoverageValue, Value);
  end;
end;

procedure TGLStateCache.SetSampleCoverageValue(const Value: TGLfloat);
begin
  if (Value <> FSampleCoverageValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleCoverageValue := Value;
    gl.SampleCoverage(Value, FSampleCoverageInvert);
  end;
end;

procedure TGLStateCache.SetSampleMaskValue(index: Integer; const Value: TGLbitfield);
begin
  if (FSampleMaskValue[Index] <> Value) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleMaskValue[Index] := Value;
    gl.SampleMaski(Index, Value);
  end;
end;

procedure TGLStateCache.SetScissorBox(const Value: TVector4i);
begin
  if not VectorEquals(FScissorBox, Value) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttScissor)
    else
      FScissorBox := Value;
    gl.Scissor(Value.X, Value.Y, Value.Z, Value.W);
  end;
end;

procedure TGLStateCache.SetStencilBackWriteMask(const Value: Cardinal);
begin
  if (Value <> FStencilBackWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilBackWriteMask := Value;
    // DONE: ignore if unsupported
    if gl.VERSION_2_0 then
      gl.StencilMaskSeparate(GL_BACK, Value);
  end;
end;

procedure TGLStateCache.SetStencilClearValue(const Value: Cardinal);
{$IFDEF USE_CACHE_MISS_CHECK}
var
  I: Cardinal;
{$ENDIF}
begin
{$IFDEF USE_CACHE_MISS_CHECK}
  gl.GetIntegerv(GL_STENCIL_CLEAR_VALUE, @I);
  if FStencilClearValue <> I then
    GLSLogger.LogError(strStateCashMissing + 'Stencil clear value');
{$ENDIF}
  if (Value <> FStencilClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilClearValue := Value;
    gl.ClearStencil(Value);
  end;
end;

procedure TGLStateCache.SetColorClearValue(const Value: TGLVector);
begin
  if not VectorEquals(Value, FColorClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FColorClearValue := Value;
    gl.ClearColor(Value.X, Value.Y, Value.Z, Value.W);
  end;
end;

procedure TGLStateCache.SetColorMask(mask: TGLColorMask);
var
  I: Integer;
begin
  // it might be faster to keep track of whether all draw buffers are same
  // value or not, since using this is probably more common than setting
  // the color write mask for individual draw buffers
  if FInsideList then
    Include(FListStates[FCurrentList], sttColorBuffer)
  else
    for I := low(FColorWriteMask) to high(FColorWriteMask) do
    begin
      FColorWriteMask[I] := mask;
    end;
  gl.ColorMask(ccRed in mask, ccGreen in mask, ccBlue in mask, ccAlpha in mask);
end;

procedure TGLStateCache.SetStencilFuncSeparate(const face: TGLCullFaceMode; const func: TGLStencilFunction; const ref: TGLint;
  const mask: Cardinal);
{$IFDEF USE_CACHE_MISS_CHECK}
var
  UI: Cardinal;
  I: TGLint;
{$ENDIF}
begin
  // if (func<>FStencilFunc) or (ref<>FStencilRef) or (mask<>FStencilValueMask)
  // or FInsideList then
{$IFDEF USE_CACHE_MISS_CHECK}
  gl.GetIntegerv(GL_STENCIL_FUNC, @UI);
  if cGLComparisonFunctionToGLEnum[FStencilFunc] <> UI then
    GLSLogger.LogError(strStateCashMissing + 'Stencil function');
  gl.GetIntegerv(GL_STENCIL_REF, @I);
  if FStencilRef <> I then
    GLSLogger.LogError(strStateCashMissing + 'Stencil reference');
  GLSLogger.LogError(strStateCashMissing + 'Stencil function');
  gl.GetIntegerv(GL_STENCIL_VALUE_MASK, @UI);
  if FStencilValueMask <> UI then
    GLSLogger.LogError(strStateCashMissing + 'Stencil value mask');
{$ENDIF}
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      case face of
        cmFront:
          begin
            FStencilFunc := func;
            FStencilRef := ref;
            FStencilValueMask := mask;
          end;
        cmBack:
          begin
            FStencilBackFunc := func;
            FStencilBackRef := ref;
            FStencilBackValueMask := mask;
          end;
        cmFrontAndBack:
          begin
            FStencilFunc := func;
            FStencilRef := ref;
            FStencilValueMask := mask;
            FStencilBackFunc := func;
            FStencilBackRef := ref;
            FStencilBackValueMask := mask;
          end;
      end;

    gl.StencilFuncSeparate(cGLCullFaceModeToGLEnum[face], cGLComparisonFunctionToGLEnum[func], ref, mask);
  end;
end;

procedure TGLStateCache.SetStencilFunc(const func: TGLStencilFunction; const ref: TGLint; const mask: Cardinal);
begin
  if (func <> FStencilFunc) or (ref <> FStencilRef) or (mask <> FStencilValueMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
    begin
      FStencilFunc := func;
      FStencilRef := ref;
      FStencilValueMask := mask;
    end;
    gl.StencilFunc(cGLComparisonFunctionToGLEnum[func], ref, mask);
  end;
end;

procedure TGLStateCache.SetStencilOp(const fail, zfail, zpass: TGLStencilOp);
{$IFDEF USE_CACHE_MISS_CHECK}
var
  I: Cardinal;
{$ENDIF}
begin
{$IFDEF USE_CACHE_MISS_CHECK}
  gl.GetIntegerv(GL_STENCIL_FAIL, @I);
  if cGLStencilOpToGLEnum[FStencilFail] <> I then
    GLSLogger.LogError(strStateCashMissing + 'Stencil fail');
  gl.GetIntegerv(GL_STENCIL_PASS_DEPTH_FAIL, @I);
  if cGLStencilOpToGLEnum[FStencilPassDepthFail] <> I then
    GLSLogger.LogError(strStateCashMissing + 'Stencil zfail');
  gl.GetIntegerv(GL_STENCIL_PASS_DEPTH_PASS, @I);
  if cGLStencilOpToGLEnum[FStencilPassDepthPass] <> I then
    GLSLogger.LogError(strStateCashMissing + 'Stencil zpass');
{$ENDIF}
  if (fail <> FStencilFail) or (zfail <> FStencilPassDepthFail) or (zpass <> FStencilPassDepthPass) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
    begin
      FStencilFail := fail;
      FStencilPassDepthFail := zfail;
      FStencilPassDepthPass := zpass;
    end;
    gl.StencilOp(cGLStencilOpToGLEnum[fail],
      cGLStencilOpToGLEnum[zfail],
      cGLStencilOpToGLEnum[zpass]);
  end;
end;

procedure TGLStateCache.SetStencilOpSeparate(const face: TGLCullFaceMode; const sfail, dpfail, dppass: TGLStencilOp);
begin
  if FInsideList then
    Include(FListStates[FCurrentList], sttStencilBuffer)
  else
    case face of
      cmFront:
        begin
          FStencilFail := sfail;
          FStencilPassDepthFail := dpfail;
          FStencilPassDepthPass := dppass;
        end;
      cmBack:
        begin
          FStencilBackFail := sfail;
          FStencilBackPassDepthFail := dpfail;
          FStencilBackPassDepthPass := dppass;
        end;
      cmFrontAndBack:
        begin
          FStencilFail := sfail;
          FStencilPassDepthFail := dpfail;
          FStencilPassDepthPass := dppass;
          FStencilBackFail := sfail;
          FStencilBackPassDepthFail := dpfail;
          FStencilBackPassDepthPass := dppass;
        end;
    end;

  gl.StencilOpSeparate(cGLCullFaceModeToGLEnum[face], cGLStencilOpToGLEnum[sfail], cGLStencilOpToGLEnum[dpfail],
    cGLStencilOpToGLEnum[dppass]);
end;

procedure TGLStateCache.SetStencilWriteMask(const Value: Cardinal);
{$IFDEF USE_CACHE_MISS_CHECK}
var
  I: Cardinal;
{$ENDIF}
begin
{$IFDEF USE_CACHE_MISS_CHECK}
  gl.GetIntegerv(GL_STENCIL_WRITEMASK, @I);
  if FStencilWriteMask <> I then
    GLSLogger.LogError(strStateCashMissing + 'Stencil write mask');
{$ENDIF}
  if (Value <> FStencilWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilWriteMask := Value;
    gl.StencilMaskSeparate(GL_FRONT, Value);
  end;
end;

procedure TGLStateCache.SetTextureBinding(index: Integer; target: TGLTextureTarget; const Value: Cardinal);
var
  lastActiveTexture: Cardinal;
begin
  if target = ttNoShape then
    exit;
  if (Value <> FTextureBinding[Index, target]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttTexture)
    else
      FTextureBinding[Index, target] := Value;
    lastActiveTexture := ActiveTexture;
    ActiveTexture := Index;
    gl.BindTexture(cGLTexTypeToGLEnum[target], Value);
    ActiveTexture := lastActiveTexture;
  end;
  FTextureBindingTime[Index, target] := AppTime;
end;

function TGLStateCache.GetActiveTextureEnabled(target: TGLTextureTarget): Boolean;
begin
  Result := FActiveTextureEnabling[FActiveTexture][target];
end;

procedure TGLStateCache.SetActiveTextureEnabled(target: TGLTextureTarget; const Value: Boolean);
var
  glTarget: Cardinal;
begin
  glTarget := DecodeTextureTarget(target);
  if { FForwardContext or } not IsTargetSupported(glTarget) then
    exit;
  if (Value <> FActiveTextureEnabling[FActiveTexture][target]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      FActiveTextureEnabling[FActiveTexture][target] := Value;
    if Value then
      gl.Enable(glTarget)
    else
      gl.Disable(glTarget);
  end;
end;

procedure TGLStateCache.SetTextureCompressionHint(const Value: TGLHintType);
begin
  if (Value <> FTextureCompressionHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FTextureCompressionHint := Value;
    gl.Hint(GL_TEXTURE_COMPRESSION_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TGLStateCache.SetTransformFeedbackBufferBinding(const Value: Cardinal);
begin
  if (Value <> FTransformFeedbackBufferBinding) or FInsideList then
  begin
    FTransformFeedbackBufferBinding := Value;
    gl.BindBuffer(GL_TRANSFORM_FEEDBACK_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetEnableTextureCubeMapSeamless(const Value: TGLboolean);
begin
  if Value <> FEnableTextureCubeMapSeamless then
  begin
    FEnableTextureCubeMapSeamless := Value;
    if Value = True then
      gl.Enable(GL_TEXTURE_CUBE_MAP_SEAMLESS)
    else
      gl.Disable(GL_TEXTURE_CUBE_MAP_SEAMLESS);
  end;
end;

procedure TGLStateCache.NewList(list: Cardinal; mode: Cardinal);
var
  I: Cardinal;
begin
  Assert(mode = GL_COMPILE, 'Compile & executing not supported by TGLStateCache');
  FCurrentList := list - 1;
  // while High(FListStates) < Integer(FCurrentList) do
  // SetLength(FListStates, 2 * Length(FListStates));

  FListStates[FCurrentList] := [];
  FInsideList := True;
  // Reset VBO binding and client attribute
  begin
    if GL.ARB_vertex_buffer_object then
    begin
      ArrayBufferBinding := 0;
      ElementBufferBinding := 0;
      for I := 0 to 15 do
        gl.DisableVertexAttribArray(I);
    end;
    gl.NewList(list, mode);
  end;
end;

procedure TGLStateCache.EndList;
begin
  gl.EndList;
  FInsideList := False;
end;

procedure TGLStateCache.CallList(list: Cardinal);
begin
  // while High(FListStates) < Integer(list) do
  // SetLength(FListStates, 2 * Length(FListStates));

  if FListStates[list - 1] <> [] then
  begin
    PushAttrib(FListStates[list - 1]);
    gl.CallList(list);
    PopAttrib;
  end
  else
    gl.CallList(list);
end;

procedure TGLStateCache.SetUniformBufferBinding(const Value: Cardinal);
begin
  Assert(not FInsideList);
  if Value <> FUniformBufferBinding then
  begin
    FUniformBufferBinding := Value;
    gl.BindBuffer(GL_UNIFORM_BUFFER, Value);
  end;
end;

procedure TGLStateCache.SetBufferIndexedBinding(const Value: Cardinal;
  ATarget: TGLBufferBindingTarget; AIndex: Cardinal; ABufferSize: TGLsizeiptr);
begin
  Assert(not FInsideList);
  if (FUBOStates[ATarget, AIndex].FUniformBufferBinding <> Value)
    or (FUBOStates[ATarget, AIndex].FOffset > 0)
    or (FUBOStates[ATarget, AIndex].FSize <> ABufferSize) then
  begin
    case ATarget of
      bbtUniform:
        FUniformBufferBinding := Value;
      bbtTransformFeedBack:
        FTransformFeedbackBufferBinding := Value;
    end;
    FUBOStates[ATarget, AIndex].FUniformBufferBinding := Value;
    FUBOStates[ATarget, AIndex].FOffset := 0;
    FUBOStates[ATarget, AIndex].FSize := ABufferSize;
    gl.BindBufferBase(cGLBufferBindingTarget[ATarget], AIndex, Value);
  end
  else
    case ATarget of
      bbtUniform: SetUniformBufferBinding(Value);
      bbtTransformFeedBack: SetTransformFeedbackBufferBinding(Value);
    end;
end;

procedure TGLStateCache.SetBufferIndexedBinding(const Value: Cardinal; ATarget: TGLBufferBindingTarget; AIndex: Cardinal;
  AOffset: TGLintptr; ARangeSize: TGLsizeiptr);
begin
  Assert(not FInsideList);
  if (FUBOStates[ATarget, AIndex].FUniformBufferBinding <> Value)
    or (FUBOStates[ATarget, AIndex].FOffset <> AOffset)
    or (FUBOStates[ATarget, AIndex].FSize <> ARangeSize) then
  begin
    case ATarget of
      bbtUniform: FUniformBufferBinding := Value;
      bbtTransformFeedBack: FTransformFeedbackBufferBinding := Value;
    end;
    FUBOStates[ATarget, AIndex].FUniformBufferBinding := Value;
    FUBOStates[ATarget, AIndex].FOffset := AOffset;
    FUBOStates[ATarget, AIndex].FSize := ARangeSize;
    gl.BindBufferRange(cGLBufferBindingTarget[ATarget], AIndex, Value, AOffset, ARangeSize);
  end;
end;

function TGLStateCache.GetMaxTextureUnits: Cardinal;
begin
  if FMaxTextureUnits = 0 then
    gl.GetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS_ARB, @FMaxTextureUnits);
  Result := FMaxTextureUnits;
end;

procedure TGLStateCache.SetUnpackAlignment(const Value: Cardinal);
begin
  if Value <> FUnpackAlignment then
  begin
    FUnpackAlignment := Value;
    gl.PixelStoref(GL_UNPACK_ALIGNMENT, Value);
  end;
end;

procedure TGLStateCache.SetUnpackImageHeight(const Value: Cardinal);
begin
  if Value <> FUnpackImageHeight then
  begin
    FUnpackImageHeight := Value;
    gl.PixelStoref(GL_UNPACK_IMAGE_HEIGHT, Value);
  end;
end;

procedure TGLStateCache.SetUnpackLSBFirst(const Value: TGLboolean);
begin
  if Value <> FUnpackLSBFirst then
  begin
    FUnpackLSBFirst := Value;
    gl.PixelStorei(GL_UNPACK_LSB_FIRST, byte(Value));
  end;
end;

procedure TGLStateCache.SetUnpackRowLength(const Value: Cardinal);
begin
  if Value <> FUnpackRowLength then
  begin
    FUnpackRowLength := Value;
    gl.PixelStoref(GL_UNPACK_ROW_LENGTH, Value);
  end;
end;

procedure TGLStateCache.SetUnpackSkipImages(const Value: Cardinal);
begin
  if Value <> FUnpackSkipImages then
  begin
    FUnpackSkipImages := Value;
    gl.PixelStoref(GL_UNPACK_SKIP_IMAGES, Value);
  end;
end;

procedure TGLStateCache.SetUnpackSkipPixels(const Value: Cardinal);
begin
  if Value <> FUnpackSkipPixels then
  begin
    FUnpackSkipPixels := Value;
    gl.PixelStoref(GL_UNPACK_SKIP_PIXELS, Value);
  end;
end;

procedure TGLStateCache.SetUnpackSkipRows(const Value: Cardinal);
begin
  if Value <> FUnpackSkipRows then
  begin
    FUnpackSkipRows := Value;
    gl.PixelStoref(GL_UNPACK_SKIP_ROWS, Value);
  end;
end;

procedure TGLStateCache.SetUnpackSwapBytes(const Value: TGLboolean);
begin
  if Value <> FUnpackSwapBytes then
  begin
    FUnpackSwapBytes := Value;
    gl.PixelStorei(GL_UNPACK_SWAP_BYTES, byte(Value));
  end;
end;

procedure TGLStateCache.SetViewPort(const Value: TVector4i);
begin
  if not VectorEquals(Value, FViewPort) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FViewPort := Value;
    gl.ViewPort(Value.X, Value.Y, Value.Z, Value.W);
  end;
end;

procedure TGLStateCache.SetFFPLight(Value: Boolean);
begin
  FFFPLight := Value { and not FForwardContext };
end;

function TGLStateCache.GetMaxLights: Integer;
begin
  if FMaxLights = 0 then
    { if FForwardContext then
      FMaxLights := MAX_HARDWARE_LIGHT
      else }
    gl.GetIntegerv(GL_MAX_LIGHTS, @FMaxLights);
  Result := FMaxLights;
end;

function TGLStateCache.GetLightEnabling(I: Integer): Boolean;
begin
  Result := FLightEnabling[I];
end;

procedure TGLStateCache.SetLightEnabling(I: Integer; Value: Boolean);
var
  J, K: Integer;
begin
  if (FLightEnabling[I] <> Value) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightEnabling[I] := Value;

    if FFFPLight then
    begin
      if Value then
        gl.Enable(GL_LIGHT0 + I)
      else
        gl.Disable(GL_LIGHT0 + I);
    end;

    K := 0;
    for J := 0 to MAX_HARDWARE_LIGHT - 1 do
      if FLightEnabling[J] then
      begin
        FLightIndices[K] := J;
        Inc(K);
      end;
    FLightNumber := K;

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TGLStateCache.GetLightIndicesAsAddress: PGLInt;
begin
  Result := @FLightIndices[0];
end;

function TGLStateCache.GetLightStateAsAddress: Pointer;
var
  I, J, C: Integer;
begin
  C := MinInteger(FLightNumber, MAX_SHADER_LIGHT);
  if FShaderLightStatesChanged then
  begin
    if C > 0 then
    begin
      if GL.VERSION_3_0 then
      begin
        Move(FLightStates.Position,
          FShaderLightStates.Position,
          SizeOf(FShaderLightStates.Position));
        Move(FLightStates.Ambient,
         FShaderLightStates.Ambient,
         SizeOf(FShaderLightStates.Ambient));
        Move(FLightStates.Diffuse,
          FShaderLightStates.Diffuse,
          SizeOf(FShaderLightStates.Diffuse));
        Move(FLightStates.Specular,
          FShaderLightStates.Specular,
          SizeOf(FShaderLightStates.Specular));
        Move(FLightStates.SpotDirection,
          FShaderLightStates.SpotDirection,
          SizeOf(FShaderLightStates.SpotDirection));
        Move(FLightStates.SpotCosCutoffExponent,
          FShaderLightStates.SpotCosCutoffExponent,
          SizeOf(FShaderLightStates.SpotCosCutoffExponent));
        Move(FLightStates.Attenuation,
          FShaderLightStates.Attenuation,
          SizeOf(FShaderLightStates.Attenuation));
      end
      else
      begin
        for I := C - 1 downto 0 do
        begin
          J := FLightIndices[I];
          FShaderLightStates.Position[I] := FLightStates.Position[J];
          FShaderLightStates.Ambient[I] := FLightStates.Ambient[J];
          FShaderLightStates.Diffuse[I] := FLightStates.Diffuse[J];
          FShaderLightStates.Specular[I] := FLightStates.Specular[J];
          FShaderLightStates.SpotDirection[I] := FLightStates.SpotDirection[J];
          FShaderLightStates.SpotCosCutoffExponent[I] := FLightStates.SpotCosCutoffExponent[J];
          FShaderLightStates.Attenuation[I] := FLightStates.Attenuation[J];
        end;
      end;
    end
    else
      FillChar(FShaderLightStatesChanged, SizeOf(FShaderLightStatesChanged), $00);
    FShaderLightStatesChanged := False;
  end;

  Result := @FShaderLightStates;
end;

function TGLStateCache.GetLightPosition(I: Integer): TGLVector;
begin
  Result := FLightStates.Position[I];
end;

procedure TGLStateCache.SetLightPosition(I: Integer; const Value: TGLVector);
begin
  if not VectorEquals(Value, FLightStates.Position[I]) then
  begin
    FLightStates.Position[I] := Value;
    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TGLStateCache.GetLightSpotDirection(I: Integer): TAffineVector;
begin
  Result := AffineVectorMake(FLightStates.SpotDirection[I]);
end;

procedure TGLStateCache.SetLightSpotDirection(I: Integer; const Value: TAffineVector);
begin
  if not VectorEquals(Value, AffineVectorMake(FLightStates.SpotDirection[I])) then
  begin
    FLightStates.SpotDirection[I] := VectorMake(Value);
    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TGLStateCache.GetLightAmbient(I: Integer): TGLVector;
begin
  Result := FLightStates.Ambient[I];
end;

procedure TGLStateCache.SetLightAmbient(I: Integer; const Value: TGLVector);
begin
  if not VectorEquals(Value, FLightStates.Ambient[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Ambient[I] := Value;

    if FFFPLight then
      gl.Lightfv(GL_LIGHT0 + I, GL_AMBIENT, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TGLStateCache.GetLightDiffuse(I: Integer): TGLVector;
begin
  Result := FLightStates.Diffuse[I];
end;

procedure TGLStateCache.SetLightDiffuse(I: Integer; const Value: TGLVector);
begin
  if not VectorEquals(Value, FLightStates.Diffuse[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Diffuse[I] := Value;

    if FFFPLight then
      gl.Lightfv(GL_LIGHT0 + I, GL_DIFFUSE, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TGLStateCache.GetLightSpecular(I: Integer): TGLVector;
begin
  Result := FLightStates.Specular[I];
end;

procedure TGLStateCache.SetLightSpecular(I: Integer; const Value: TGLVector);
begin
  if not VectorEquals(Value, FLightStates.Specular[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Specular[I] := Value;

    if FFFPLight then
      gl.Lightfv(GL_LIGHT0 + I, GL_SPECULAR, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TGLStateCache.GetSpotCutoff(I: Integer): Single;
begin
  Result := FSpotCutoff[I];
end;

procedure TGLStateCache.SetSpotCutoff(I: Integer; const Value: Single);
begin
  if (Value <> FSpotCutoff[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
    begin
      FSpotCutoff[I] := Value;
      FLightStates.SpotCosCutoffExponent[I].X := cos(DegToRadian(Value));
    end;

    if FFFPLight then
      gl.Lightfv(GL_LIGHT0 + I, GL_SPOT_CUTOFF, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TGLStateCache.GetSpotExponent(I: Integer): Single;
begin
  Result := FLightStates.SpotCosCutoffExponent[I].Y;
end;

procedure TGLStateCache.SetSpotExponent(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.SpotCosCutoffExponent[I].Y) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.SpotCosCutoffExponent[I].Y := Value;

    if FFFPLight then
      gl.Lightfv(GL_LIGHT0 + I, GL_SPOT_EXPONENT, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TGLStateCache.GetConstantAtten(I: Integer): Single;
begin
  Result := FLightStates.Attenuation[I].X;
end;

procedure TGLStateCache.SetConstantAtten(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.Attenuation[I].X) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Attenuation[I].X := Value;

    if FFFPLight then
      gl.Lightfv(GL_LIGHT0 + I, GL_CONSTANT_ATTENUATION, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TGLStateCache.GetLinearAtten(I: Integer): Single;
begin
  Result := FLightStates.Attenuation[I].Y;
end;

procedure TGLStateCache.SetLinearAtten(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.Attenuation[I].Y) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Attenuation[I].Y := Value;

    if FFFPLight then
      gl.Lightfv(GL_LIGHT0 + I, GL_LINEAR_ATTENUATION, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TGLStateCache.GetQuadAtten(I: Integer): Single;
begin
  Result := FLightStates.Attenuation[I].Z;
end;

procedure TGLStateCache.SetQuadAtten(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.Attenuation[I].Z) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Attenuation[I].Z := Value;

    if FFFPLight then
      gl.Lightfv(GL_LIGHT0 + I, GL_QUADRATIC_ATTENUATION, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

procedure TGLStateCache.SetForwardContext(Value: Boolean);
begin
  { if Value <> FForwardContext then
    begin
    FForwardContext := Value;
    if Value then
    begin
    SetFFPlight(False);
    end;
    end;
  }
end;

procedure TGLStateCache.SetGLColorWriting(flag: Boolean);
begin
  if (FColorWriting <> flag) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FColorWriting := flag;
    gl.ColorMask(flag, flag, flag, flag);
  end;
end;

procedure TGLStateCache.InvertGLFrontFace;
begin
  if FFrontFace = fwCounterClockWise then
    FrontFace := fwClockWise
  else
    FrontFace := fwCounterClockWise;
end;

procedure TGLStateCache.SetGLState(const aState: TGLState);
begin
  Enable(aState);
end;

procedure TGLStateCache.UnSetGLState(const aState: TGLState);
begin
  Disable(aState);
end;

procedure TGLStateCache.ResetGLPolygonMode;
begin
  gl.PolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  FPolygonMode := pmFill;
  FPolygonBackMode := pmFill;
end;

procedure TGLStateCache.ResetGLMaterialColors;
begin
  gl.Materialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @clrGray20);
  gl.Materialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @clrGray80);
  gl.Materialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @clrBlack);
  gl.Materialfv(GL_FRONT_AND_BACK, GL_EMISSION, @clrBlack);
  gl.Materiali(GL_FRONT_AND_BACK, GL_SHININESS, 0);
  FillChar(FFrontBackColors, SizeOf(FFrontBackColors), 127);
  FFrontBackShininess[0] := 0;
  FFrontBackShininess[1] := 0;
end;

procedure TGLStateCache.ResetGLTexture(const TextureUnit: Integer);
var
  t: TGLTextureTarget;
  glTarget: Cardinal;
begin
  gl.ActiveTexture(GL_TEXTURE0 + TextureUnit);
  for t := Low(TGLTextureTarget) to High(TGLTextureTarget) do
  begin
    glTarget := DecodeTextureTarget(t);
    if IsTargetSupported(glTarget) then
    begin
      gl.BindTexture(glTarget, 0);
      FTextureBinding[TextureUnit, t] := 0;
    end;
  end;
  gl.ActiveTexture(GL_TEXTURE0);
  FActiveTexture := 0;
end;

procedure TGLStateCache.ResetGLCurrentTexture;
var
  a: TGLint;
  t: TGLTextureTarget;
  glTarget: Cardinal;
begin
  if GL.ARB_multitexture then
  begin
    for a := MaxTextureImageUnits - 1 to 0 do
    begin
      gl.ActiveTexture(GL_TEXTURE0 + a);
      for t := Low(TGLTextureTarget) to High(TGLTextureTarget) do
      begin
        glTarget := DecodeTextureTarget(t);
        if IsTargetSupported(glTarget) then
        begin
          gl.BindTexture(glTarget, 0);
          FTextureBinding[a, t] := 0;
        end;
      end;
    end;
  end
  else
    for t := Low(TGLTextureTarget) to High(TGLTextureTarget) do
    begin
      glTarget := DecodeTextureTarget(t);
      if IsTargetSupported(glTarget) then
      begin
        gl.BindTexture(glTarget, 0);
        FTextureBinding[0, t] := 0;
      end;
    end;
end;

procedure TGLStateCache.ResetGLFrontFace;
begin
  gl.FrontFace(GL_CCW);
  FFrontFace := fwCounterClockWise;
end;


procedure TGLStateCache.SetGLFrontFaceCW;
begin
  if FFrontFace = fwCounterClockWise then
  begin
    gl.FrontFace(GL_CW);
    FFrontFace := fwClockWise;
  end;
end;

procedure TGLStateCache.ResetAll;
begin
 {$WARN SYMBOL_DEPRECATED OFF}
  ResetGLPolygonMode;
  ResetGLMaterialColors;
  ResetGLCurrentTexture;
  ResetGLFrontFace;
 {$WARN SYMBOL_DEPRECATED ON}
end;

end.
