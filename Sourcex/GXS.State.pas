//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.State;

(* Tools for managing an application-side cache of OpenGL state *)

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

{$I GXS.Scene.inc}
{ .$DEFINE USE_CACHE_MISS_CHECK }

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  System.Classes,
  System.SysUtils,

  GXS.Strings,
  GXS.TextureFormat,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.Utils;

const
  VERTEX_ATTR_NUM = 16;

type

  TgxStateType = (sttCurrent, sttPoint, sttLine, sttPolygon, sttPolygonStipple,
    sttPixelMode, sttLighting, sttFog, sttDepthBuffer, sttAccumBuffer,
    sttStencilBuffer, sttViewport, sttTransform, sttEnable, sttColorBuffer,
    sttHint, sttEval, sttList, sttTexture, sttScissor,
    sttMultisample);
  TgxStateTypes = set of TgxStateType;

const
  cAllAttribBits = [Low(TgxStateType)..High(TgxStateType)];

type

  TgxMeshPrimitive = (
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

  TgxMeshPrimitives = set of TgxMeshPrimitive;

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
  TgxState = (stAlphaTest, stAutoNormal,
    stBlend, stColorMaterial, stCullFace, stDepthTest, stDither,
    stFog, stLighting, stLineSmooth, stLineStipple,
    stIndexLogicOp, stColorLogicOp, stNormalize, stPointSmooth, stPointSprite,
    stPolygonSmooth, stPolygonStipple, stScissorTest, stStencilTest,
    stPolygonOffsetPoint, stPolygonOffsetLine, stPolygonOffsetFill,
    stDepthClamp);

  TgxStates = set of TgxState;

  TgxComparisonFunction = (cfNever, cfAlways, cfLess, cfLEqual, cfEqual,
    cfGreater, cfNotEqual, cfGEqual);
  TgxStencilFunction = TgxComparisonFunction;
  TgxDepthFunction = TgxComparisonFunction;

  TgxBlendFunction = (bfZero, bfOne,
    bfSrcColor, bfOneMinusSrcColor, bfDstColor, bfOneMinusDstColor,
    bfSrcAlpha, bfOneMinusSrcAlpha, bfDstAlpha, bfOneMinusDstAlpha,
    bfConstantColor, bfOneMinusConstantColor,
    bfConstantAlpha, bfOneMinusConstantAlpha,
    bfSrcAlphaSat);

  TgxDstBlendFunction = bfZero..bfOneMinusConstantAlpha;

  TgxBlendEquation = (beAdd, beSubtract, beReverseSubtract, beMin, beMax);

  TgxStencilOp = (soKeep, soZero, soReplace, soIncr, soDecr, soInvert, soIncrWrap,
    soDecrWrap);

  TgxLogicOp = (loClear, loAnd, loAndReverse, loCopy, loAndInverted, loNoOp,
    loXOr, loOr, loNor, loEquiv, loInvert, loOrReverse, loCopyInverted,
    loOrInverted, loNAnd, loSet);

  TgxQueryType = (
    qrySamplesPassed,
    qryPrimitivesGenerated,
    qryTransformFeedbackPrimitivesWritten,
    qryTimeElapsed,
    qryAnySamplesPassed);

  // Describe what kind of winding has a front face
  TgxFaceWinding = (fwCounterClockWise, fwClockWise);

  TgxPolygonMode = (pmFill, pmLines, pmPoints);

  TgxCullFaceMode = (cmFront, cmBack, cmFrontAndBack);
  //  TSingleCullFaceMode = cmFront..cmBack;

  TgxColorComponent = (ccRed, ccGreen, ccBlue, ccAlpha);
  TgxColorMask = set of TgxColorComponent;

const
  cAllColorComponents = [ccRed, ccGreen, ccBlue, ccAlpha];
  MAX_HARDWARE_LIGHT = 16;
  MAX_SHADER_LIGHT = 8;
  MAX_HARDWARE_TEXTURE_UNIT = 48;
  MAX_HARDWARE_UNIFORM_BUFFER_BINDING = 75;

type

  TgxHintType = (hintDontCare, hintFastest, hintNicest);

  TgxLightSourceState = packed record
    Position: array[0..MAX_HARDWARE_LIGHT-1] of TVector4f;
    Ambient: array[0..MAX_HARDWARE_LIGHT-1] of TVector4f;
    Diffuse: array[0..MAX_HARDWARE_LIGHT-1] of TVector4f;
    Specular: array[0..MAX_HARDWARE_LIGHT-1] of TVector4f;
    SpotDirection: array[0..MAX_HARDWARE_LIGHT-1] of TVector4f;
    SpotCosCutoffExponent: array[0..MAX_HARDWARE_LIGHT-1] of TVector4f;
    Attenuation: array[0..MAX_HARDWARE_LIGHT-1] of TVector4f;
  end;

  TgxShaderLightSourceState = packed record
    Position: array[0..MAX_SHADER_LIGHT-1] of TVector4f;
    Ambient: array[0..MAX_SHADER_LIGHT-1] of TVector4f;
    Diffuse: array[0..MAX_SHADER_LIGHT-1] of TVector4f;
    Specular: array[0..MAX_SHADER_LIGHT-1] of TVector4f;
    SpotDirection: array[0..MAX_SHADER_LIGHT-1] of TVector4f;
    SpotCosCutoffExponent: array[0..MAX_SHADER_LIGHT-1] of TVector4f;
    Attenuation: array[0..MAX_SHADER_LIGHT-1] of TVector4f;
  end;

  TgxOnLightsChanged = procedure(Sender: TObject);

  TgxBufferBindingTarget = (bbtUniform, bbtTransformFeedBack);

  TUBOStates = record
    FUniformBufferBinding: Cardinal;
    FOffset: ^Integer;
    FSize: PGLsizei;
  end;

  TgxMaterialLevel = (mlAuto, mlFixedFunction, mlMultitexturing, mlSM3, mlSM4, mlSM5);

  { Manages an application-side cache of OpenGL states and parameters.
     Purpose of this class is to eliminate redundant state and parameter
     changes, and there will typically be no more than one state cache per
     OpenGL context. }
  TgxStateCache = class
  private
    // Legacy state
    FFrontBackColors: array[0..1, 0..3] of TVector4f;
    FFrontBackShininess: array[0..1] of Integer;
    FAlphaFunc: TgxComparisonFunction;
    FAlphaRef: Single;
    FPolygonBackMode: TgxPolygonMode; // Front + back have same polygon mode
    // Lighting state
    FMaxLights: Cardinal;
    FLightEnabling: array[0..MAX_HARDWARE_LIGHT - 1] of Boolean;
    FLightIndices: array[0..MAX_HARDWARE_LIGHT - 1] of GLint;
    FLightNumber: Integer;
    FLightStates: TgxLightSourceState;
    FSpotCutoff: array[0..MAX_HARDWARE_LIGHT-1] of Single;
    FShaderLightStates: TgxShaderLightSourceState;
    FShaderLightStatesChanged: Boolean;
    FColorWriting: Boolean; // TODO: change to per draw buffer (FColorWriteMask)
    FStates: TgxStates;
    FListStates: array of TgxStateTypes;
    FCurrentList: Cardinal;
    FTextureMatrixIsIdentity: array[0..3] of Boolean;
    FForwardContext: Boolean;
    FFFPLight: Boolean;
    // Vertex Array Data state
    FVertexArrayBinding: Cardinal;
    FArrayBufferBinding: Cardinal;
    FElementBufferBinding: Cardinal;
    FTextureBufferBinding: Cardinal;
    FEnablePrimitiveRestart: GLboolean;
    FPrimitiveRestartIndex: Cardinal;
    // Transformation state
    FViewPort: TVector4i;
    FDepthRange: array[0..1] of GLclampd;
    FEnableClipDistance: array[0..7] of GLboolean;
    FEnableDepthClamp: GLboolean;
    // Coloring state
    FClampReadColor: Cardinal; // GL_FIXED_ONLY
    FProvokingVertex: Cardinal; // GL_LAST_VERTEX_CONVENTION
    // Rasterization state
    FPointSize: Single;
    FPointFadeThresholdSize: Single;
    FPointSpriteCoordOrigin: Cardinal; // GL_UPPER_LEFT
    FLineWidth: Single;
    FLineStippleFactor: GLint;
    FLineStipplePattern: GLushort;
    FEnableLineSmooth: GLboolean;
    FEnableCullFace: GLboolean;
    FCullFaceMode: TgxCullFaceMode;
    FFrontFace: TgxFaceWinding;
    FEnablePolygonSmooth: GLboolean;
    FPolygonMode: TgxPolygonMode;
    FPolygonOffsetFactor: Single;
    FPolygonOffsetUnits: Single;
    FEnablePolygonOffsetPoint: GLboolean;
    FEnablePolygonOffsetLine: GLboolean;
    FEnablePolygonOffsetFill: GLboolean;
    // Multisample state
    FEnableMultisample: GLboolean;
    FEnableSampleAlphaToCoverage: GLboolean;
    FEnableSampleAlphaToOne: GLboolean;
    FEnableSampleCoverage: GLboolean;
    FSampleCoverageValue: Single;
    FSampleCoverageInvert: GLboolean;
    FEnableSampleMask: GLboolean;
    FSampleMaskValue: array[0..15] of GLbitfield;
    // Texture state
    FMaxTextureSize: Cardinal;
    FMax3DTextureSize: Cardinal;
    FMaxCubeTextureSize: Cardinal;
    FMaxArrayTextureSize: Cardinal;
    FMaxTextureImageUnits: Cardinal;
    FMaxTextureAnisotropy: Cardinal;
    FMaxSamples: Cardinal;
    FTextureBinding: array[0..MAX_HARDWARE_TEXTURE_UNIT - 1, TgxTextureTarget] of Cardinal;
    FTextureBindingTime: array[0..MAX_HARDWARE_TEXTURE_UNIT - 1, TgxTextureTarget] of Double;
    FSamplerBinding: array[0..MAX_HARDWARE_TEXTURE_UNIT - 1] of Cardinal;
    // Active texture state
    FActiveTexture: GLint; // 0 .. Max_texture_units
    FActiveTextureEnabling: array[0..MAX_HARDWARE_TEXTURE_UNIT - 1, TgxTextureTarget] of Boolean;
    // Pixel operation state
    FEnableScissorTest: GLboolean;
    FScissorBox: TVector4i;
    FEnableStencilTest: GLboolean;
    FStencilFunc: TgxStencilFunction;
    FStencilValueMask: Cardinal;
    FStencilRef: GLint;
    FStencilFail: TgxStencilOp;
    FStencilPassDepthFail: TgxStencilOp;
    FStencilPassDepthPass: TgxStencilOp;
    FStencilBackFunc: TgxStencilFunction;
    FStencilBackValueMask: GLuint;
    FStencilBackRef: GLuint;
    FStencilBackFail: TgxStencilOp;
    FStencilBackPassDepthPass: TgxStencilOp;
    FStencilBackPassDepthFail: TgxStencilOp;
    FEnableDepthTest: GLboolean;
    FDepthFunc: TgxDepthFunction;
    FEnableBlend: array[0..15] of GLboolean;
    FBlendSrcRGB: TgxBlendFunction;
    FBlendSrcAlpha: TgxBlendFunction;
    FBlendDstRGB: TgxDstBlendFunction;
    FBlendDstAlpha: TgxDstBlendFunction;
    FBlendEquationRGB: TgxBlendEquation;
    FBlendEquationAlpha: TgxBlendEquation;
    FBlendColor: TVector4f;
    FEnableFramebufferSRGB: GLboolean;
    FEnableDither: GLboolean;
    FEnableColorLogicOp: GLboolean;
    FLogicOpMode: TgxLogicOp;
    // Framebuffer control state
    FColorWriteMask: array[0..15] of TgxColorMask;
    FDepthWriteMask: Boolean;
    FStencilWriteMask: GLuint;
    FStencilBackWriteMask: GLuint;
    FColorClearValue: TVector4f;
    FDepthClearValue: Single;
    FStencilClearValue: GLuint;
    // Framebuffer state
    FDrawFrameBuffer: Cardinal;
    FReadFrameBuffer: Cardinal;
    // Renderbuffer state
    FRenderBuffer: Cardinal;
    // Pixels state
    FUnpackSwapBytes: GLboolean;
    FUnpackLSBFirst: GLboolean;
    FUnpackImageHeight: GLuint;
    FUnpackSkipImages: GLuint;
    FUnpackRowLength: GLuint;
    FUnpackSkipRows: GLuint;
    FUnpackSkipPixels: GLuint;
    FUnpackAlignment: Cardinal;
    FPackSwapBytes: GLboolean;
    FPackLSBFirst: GLboolean;
    FPackImageHeight: GLuint;
    FPackSkipImages: GLuint;
    FPackRowLength: GLuint;
    FPackSkipRows: GLuint;
    FPackSkipPixels: GLuint;
    FPackAlignment: GLuint;
    FPixelPackBufferBinding: GLuint;
    FPixelUnpackBufferBinding: GLuint;
    // Program state
    FCurrentProgram: GLuint;
    FMaxTextureUnits: GLuint;
    FUniformBufferBinding: GLuint;
    FUBOStates: array[TgxBufferBindingTarget, 0..MAX_HARDWARE_UNIFORM_BUFFER_BINDING-1] of TUBOStates;
    // Vector + Geometry Shader state
    FCurrentVertexAttrib: array[0..15] of TVector4f;
    FEnableProgramPointSize: GLboolean;
    // Transform Feedback state
    FTransformFeedbackBufferBinding: Cardinal;
    // Hints state
    FTextureCompressionHint: TgxHintType;
    FPolygonSmoothHint: TgxHintType;
    FFragmentShaderDerivitiveHint: TgxHintType;
    FLineSmoothHint: TgxHintType;
    FMultisampleFilterHint: TgxHintType;
    // Misc state
    FCurrentQuery: array[TgxQueryType] of GLuint;
    FCopyReadBufferBinding: GLuint;
    FCopyWriteBufferBinding: GLuint;
    FEnableTextureCubeMapSeamless: GLboolean;
    FInsideList: Boolean;
    FOnLightsChanged: TgxOnLightsChanged;
  protected
    // Vertex Array Data state
    procedure SetVertexArrayBinding(const Value: GLuint);
    function GetArrayBufferBinding: GLuint;
    procedure SetArrayBufferBinding(const Value: GLuint);
    function GetElementBufferBinding: GLuint;
    procedure SetElementBufferBinding(const Value: GLuint);
    function GetEnablePrimitiveRestart: GLboolean;
    function GetPrimitiveRestartIndex: GLuint;
    procedure SetEnablePrimitiveRestart(const enabled: GLboolean);
    procedure SetPrimitiveRestartIndex(const index: GLuint);
    procedure SetTextureBufferBinding(const Value: GLuint);
    // Transformation state
    procedure SetViewPort(const Value: TVector4i);
    function GetEnableClipDistance(ClipDistance: Cardinal): GLboolean;
    procedure SetEnableClipDistance(Index: Cardinal; const Value: GLboolean);
    function GetDepthRangeFar:GLclampd;
    procedure SetDepthRangeFar(const Value: GLclampd);
    function GetDepthRangeNear:GLclampd;
    procedure SetDepthRangeNear(const Value: GLclampd);
    procedure SetEnableDepthClamp(const enabled: GLboolean);
    // Coloring state
    procedure SetClampReadColor(const Value: GLEnum);
    procedure SetProvokingVertex(const Value: GLEnum);
    // Rasterization state
    procedure SetPointSize(const Value: Single);
    procedure SetPointFadeThresholdSize(const Value: Single);
    procedure SetPointSpriteCoordOrigin(const Value: GLEnum);
    procedure SetLineWidth(const Value: Single);
    procedure SetLineStippleFactor(const Value: GLint);
    procedure SetLineStipplePattern(const Value: GLushort);
    procedure SetEnableLineSmooth(const Value: GLboolean);
    procedure SetEnableCullFace(const Value: GLboolean);
    procedure SetCullFaceMode(const Value: TgxCullFaceMode);
    procedure SetFrontFace(const Value: TgxFaceWinding);
    procedure SetEnablePolygonSmooth(const Value: GLboolean);
    procedure SetPolygonMode(const Value: TgxPolygonMode);
    procedure SetPolygonOffsetFactor(const Value: Single);
    procedure SetPolygonOffsetUnits(const Value: Single);
    procedure SetEnablePolygonOffsetPoint(const Value: GLboolean);
    procedure SetEnablePolygonOffsetLine(const Value: GLboolean);
    procedure SetEnablePolygonOffsetFill(const Value: GLboolean);
    // Multisample state
    procedure SetEnableMultisample(const Value: GLboolean);
    procedure SetEnableSampleAlphaToCoverage(const Value: GLboolean);
    procedure SetEnableSampleAlphaToOne(const Value: GLboolean);
    procedure SetEnableSampleCoverage(const Value: GLboolean);
    procedure SetSampleCoverageValue(const Value: Single);
    procedure SetSampleCoverageInvert(const Value: GLboolean);
    procedure SetEnableSampleMask(const Value: GLboolean);
    function GetSampleMaskValue(Index: Integer): GLbitfield;
    procedure SetSampleMaskValue(Index: Integer; const Value: GLbitfield);
    // Texture state
    function GetMaxTextureSize: GLuint;
    function GetMax3DTextureSize: GLuint;
    function GetMaxCubeTextureSize: GLuint;
    function GetMaxArrayTextureSize: GLuint;
    function GetMaxTextureImageUnits: GLuint;
    function GetMaxTextureAnisotropy: GLuint;
    function GetMaxSamples: GLuint;
    function GetTextureBinding(Index: Integer; target: TgxTextureTarget):
      GLuint;
    function GetTextureBindingTime(Index: Integer; target: TgxTextureTarget):
      Double;
    procedure SetTextureBinding(Index: Integer; target: TgxTextureTarget;
      const Value: GLuint);
    function GetActiveTextureEnabled(Target: TgxTextureTarget): Boolean;
    procedure SetActiveTextureEnabled(Target: TgxTextureTarget; const Value:
      Boolean);
    function GetSamplerBinding(Index: GLuint): GLuint;
    procedure SetSamplerBinding(Index: GLuint; const Value: GLuint);
    // Active texture
    procedure SetActiveTexture(const Value: GLint);
    // Pixel operations
    procedure SetEnableScissorTest(const Value: GLboolean);
    procedure SetScissorBox(const Value: TVector4i);
    procedure SetEnableStencilTest(const Value: GLboolean);
    procedure SetEnableDepthTest(const Value: GLboolean);
    procedure SetDepthFunc(const Value: TgxDepthFunction);
    function GetEnableBlend(Index: Integer): GLboolean;
    procedure SetEnableBlend(Index: Integer; const Value: GLboolean);
    procedure SetBlendColor(const Value: TVector4f);
    procedure SetEnableFramebufferSRGB(const Value: GLboolean);
    procedure SetEnableDither(const Value: GLboolean);
    procedure SetEnableColorLogicOp(const Value: GLboolean);
    procedure SetLogicOpMode(const Value: TgxLogicOp);
    // Framebuffer control
    function GetColorWriteMask(Index: Integer): TgxColorMask;
    procedure SetColorWriteMask(Index: Integer; const Value: TgxColorMask);
    procedure SetDepthWriteMask(const Value: Boolean);
    procedure SetStencilWriteMask(const Value: GLuint);
    procedure SetStencilBackWriteMask(const Value: GLuint);
    procedure SetColorClearValue(const Value: TVector4f);
    procedure SetDepthClearValue(const Value: Single);
    procedure SetStencilClearValue(const Value: GLuint);
    // Framebuffer
    procedure SetDrawFrameBuffer(const Value: GLuint);
    procedure SetReadFrameBuffer(const Value: GLuint);
    // Renderbuffer
    procedure SetRenderBuffer(const Value: GLuint);
    // Pixels
    procedure SetUnpackSwapBytes(const Value: GLboolean);
    procedure SetUnpackLSBFirst(const Value: GLboolean);
    procedure SetUnpackImageHeight(const Value: GLuint);
    procedure SetUnpackSkipImages(const Value: GLuint);
    procedure SetUnpackRowLength(const Value: GLuint);
    procedure SetUnpackSkipRows(const Value: GLuint);
    procedure SetUnpackSkipPixels(const Value: GLuint);
    procedure SetUnpackAlignment(const Value: GLuint);
    procedure SetPackSwapBytes(const Value: GLboolean);
    procedure SetPackLSBFirst(const Value: GLboolean);
    procedure SetPackImageHeight(const Value: GLuint);
    procedure SetPackSkipImages(const Value: GLuint);
    procedure SetPackRowLength(const Value: GLuint);
    procedure SetPackSkipRows(const Value: GLuint);
    procedure SetPackSkipPixels(const Value: GLuint);
    procedure SetPackAlignment(const Value: GLuint);
    procedure SetPixelPackBufferBinding(const Value: GLuint);
    procedure SetPixelUnpackBufferBinding(const Value: GLuint);
    // Program
    procedure SetCurrentProgram(const Value: GLuint);
    procedure SetUniformBufferBinding(const Value: GLuint);
    function GetMaxTextureUnits: GLuint;
    // Vector + Geometry Shader state
    function GetCurrentVertexAttrib(Index: Integer): TVector4f;
    procedure SetCurrentVertexAttrib(Index: Integer; const Value: TVector4f);
    procedure SetEnableProgramPointSize(const Value: GLboolean);
    // Transform Feedback state
    procedure SetTransformFeedbackBufferBinding(const Value: GLuint);
    // Hints
    procedure SetLineSmoothHint(const Value: TgxHintType);
    procedure SetPolygonSmoothHint(const Value: TgxHintType);
    procedure SetTextureCompressionHint(const Value: TgxHintType);
    procedure SetFragmentShaderDerivitiveHint(const Value: TgxHintType);
    procedure SetMultisampleFilterHint(const Value: TgxHintType);
    // Misc
    function GetCurrentQuery(Index: TgxQueryType): GLuint;
    //    procedure SetCurrentQuery(Index: TgxQueryType; const Value: GLuint);
    procedure SetCopyReadBufferBinding(const Value: GLuint);
    procedure SetCopyWriteBufferBinding(const Value: GLuint);
    procedure SetEnableTextureCubeMapSeamless(const Value: GLboolean);
    // Ligting
    procedure SetFFPLight(Value: Boolean);
    function GetMaxLights: Integer;
    function GetLightEnabling(I: Integer): Boolean;
    procedure SetLightEnabling(I: Integer; Value: Boolean);
    function GetLightPosition(I: Integer): TVector4f;
    procedure SetLightPosition(I: Integer; const Value: TVector4f);
    function GetLightSpotDirection(I: Integer): TAffineVector;
    procedure SetLightSpotDirection(I: Integer; const Value: TAffineVector);
    function GetLightAmbient(I: Integer): TVector4f;
    procedure SetLightAmbient(I: Integer; const Value: TVector4f);
    function GetLightDiffuse(I: Integer): TVector4f;
    procedure SetLightDiffuse(I: Integer; const Value: TVector4f);
    function GetLightSpecular(I: Integer): TVector4f;
    procedure SetLightSpecular(I: Integer; const Value: TVector4f);
    function GetSpotCutoff(I: Integer): Single;
    procedure SetSpotCutoff(I: Integer; const Value: Single);
    function GetSpotExponent(I: Integer): Single;
    procedure SetSpotExponent(I: Integer; const Value: Single);
    function GetConstantAtten(I: Integer): Single;
    procedure SetConstantAtten(I: Integer; const Value: Single);
    function GetLinearAtten(I: Integer): Single;
    procedure SetLinearAtten(I: Integer; const Value: Single);
    function GetQuadAtten(I: Integer): Single;
    procedure SetQuadAtten(I: Integer; const Value: Single);
    procedure SetForwardContext(Value: Boolean);
    function GetMaterialAmbient(const aFace: TgxCullFaceMode): TVector4f;
    function GetMaterialDiffuse(const aFace: TgxCullFaceMode): TVector4f;
    function GetMaterialSpecular(const aFace: TgxCullFaceMode): TVector4f;
    function GetMaterialEmission(const aFace: TgxCullFaceMode): TVector4f;
    function GetMaterialShininess(const aFace: TgxCullFaceMode): Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure PushAttrib(stateTypes: TgxStateTypes);
    procedure PopAttrib();
    procedure Enable(const aState: TgxState);
    procedure Disable(const aState: TgxState);
    procedure PerformEnable(const aState: TgxState);
    procedure PerformDisable(const aState: TgxState);
    procedure SetVxState(const aState : TgxState); deprecated;
    procedure UnSetVxState(const aState : TgxState); deprecated;
    procedure ResetPolygonMode; deprecated;
    procedure ResetMaterialColors; deprecated;
    procedure ResetTexture(const TextureUnit: Integer); deprecated;
    procedure ResetCurrentTexture; deprecated;
    procedure ResetFrontFace; deprecated;
    procedure SetGLFrontFaceCW; deprecated;
    procedure ResetAll; deprecated;
    // Adjusts material colors for a face.
    procedure SetMaterialColors(const aFace: TgxCullFaceMode;
      const emission, ambient, diffuse, specular: TVector4f;
      const shininess: Integer);
    property MaterialAmbient[const aFace: TgxCullFaceMode]: TVector4f
      read GetMaterialAmbient;
    property MaterialDiffuse[const aFace: TgxCullFaceMode]: TVector4f
      read GetMaterialDiffuse;
    property MaterialSpecular[const aFace: TgxCullFaceMode]: TVector4f
      read GetMaterialSpecular;
    property MaterialEmission[const aFace: TgxCullFaceMode]: TVector4f
      read GetMaterialEmission;
    property MaterialShininess[const aFace: TgxCullFaceMode]: Integer
      read GetMaterialShininess;
    // Adjusts material alpha channel for a face.
    procedure SetMaterialAlphaChannel(const aFace: GLEnum; const alpha: Single);
    // Adjusts material diffuse color for a face.
    procedure SetMaterialDiffuseColor(const aFace: GLEnum; const diffuse: TVector4f);
    // Lighting states
    property FixedFunctionPipeLight: Boolean read FFFPLight write SetFFPLight;
    property MaxLights: Integer read GetMaxLights;
    property LightEnabling[Index: Integer]: Boolean read GetLightEnabling write SetLightEnabling;
    property LightPosition[Index: Integer]: TVector4f read GetLightPosition write SetLightPosition;
    property LightSpotDirection[Index: Integer]: TAffineVector read GetLightSpotDirection write SetLightSpotDirection;
    property LightAmbient[Index: Integer]: TVector4f read GetLightAmbient write SetLightAmbient;
    property LightDiffuse[Index: Integer]: TVector4f read GetLightDiffuse write SetLightDiffuse;
    property LightSpecular[Index: Integer]: TVector4f read GetLightSpecular write SetLightSpecular;
    property LightSpotCutoff[Index: Integer]: Single read GetSpotCutoff write SetSpotCutoff;
    property LightSpotExponent[Index: Integer]: Single read GetSpotExponent write SetSpotExponent;
    property LightConstantAtten[Index: Integer]: Single read GetConstantAtten write SetConstantAtten;
    property LightLinearAtten[Index: Integer]: Single read GetLinearAtten write SetLinearAtten;
    property LightQuadraticAtten[Index: Integer]: Single read GetQuadAtten write SetQuadAtten;
    function GetLightIndicesAsAddress: PGLInt;
    function GetLightStateAsAddress: Pointer;
    property LightNumber: Integer read FLightNumber;
    property OnLightsChanged: TgxOnLightsChanged read FOnLightsChanged write FOnLightsChanged;
    // Blending states
    procedure SetAlphaFunction(func: TgxComparisonFunction; ref: Single);
    // Vertex Array Data state
    (* The currently bound array buffer (calling glVertexAttribPointer
       locks this buffer to the currently bound VBO). *)
    property VertexArrayBinding: GLuint read FVertexArrayBinding write SetVertexArrayBinding;
    // The currently bound vertex buffer object (VAO).
    property ArrayBufferBinding: GLuint read GetArrayBufferBinding write SetArrayBufferBinding;
    // The currently bound element buffer object (EBO).
    property ElementBufferBinding: GLuint read GetElementBufferBinding write SetElementBufferBinding;
    // Determines whether primitive restart is turned on or off.
    property EnablePrimitiveRestart: GLboolean read GetEnablePrimitiveRestart write SetEnablePrimitiveRestart;
    // The index Value that causes a primitive restart.
    property PrimitiveRestartIndex: GLuint read GetPrimitiveRestartIndex write SetPrimitiveRestartIndex;
    // The currently bound texture buffer object (TBO).
    property TextureBufferBinding: GLuint read FTextureBufferBinding write SetTextureBufferBinding;
    // Transformation state
    property ViewPort: TVector4i read FViewPort write SetViewPort;
    // Modifies the near + far clipping planes.
    procedure SetDepthRange(const ZNear, ZFar: GLclampd);
    // The near clipping plane distance.
    property DepthRangeNear: GLclampd read GetDepthRangeNear write SetDepthRangeNear;
    // The far clipping plane distance.
    property DepthRangeFar: GLclampd read GetDepthRangeFar write SetDepthRangeFar;
    // Enables/Disables each of the clip distances, used in shaders.
    property EnableClipDistance[Index: Cardinal]: GLboolean read GetEnableClipDistance write SetEnableClipDistance;
    // Enables/Disables depth clamping.
    property EnableDepthClamp: GLboolean read FEnableDepthClamp write SetEnableDepthClamp;
    // Coloring state. Controls read color clamping.
    property ClampReadColor: GLEnum read FClampReadColor write SetClampReadColor;
    (* The provoking vertex used in flat shading.  All the vertices of each
       primitive will the same value determined by this property. *)
    property ProvokingVertex: GLEnum read FProvokingVertex write SetProvokingVertex;
    // Rasterization state
    (* The default point size, used when EnableProgramPointSize = false. *)
    property PointSize: Single read FPointSize write SetPointSize;
    // If multisampling is enabled, this can control when points are faded out.
    property PointFadeThresholdSize: Single read FPointFadeThresholdSize write SetPointFadeThresholdSize;
    // The texture coordinate origin of point sprites.
    property PointSpriteCoordOrigin: GLEnum read FPointSpriteCoordOrigin write SetPointSpriteCoordOrigin;
    // The line width.
    property LineWidth: Single read FLineWidth write SetLineWidth;
    // The line stipple.
    property LineStippleFactor: GLint read FLineStippleFactor write SetLineStippleFactor;
    // The line stipple.
    property LineStipplePattern: GLushort read FLineStipplePattern write SetLineStipplePattern;
    // Enable/Disable line smoothing.
    property EnableLineSmooth: GLboolean read FEnableLineSmooth write SetEnableLineSmooth;
    // Enable/Disable face culling.
    property EnableCullFace: GLboolean read FEnableCullFace write SetEnableCullFace;
    // Selects which faces to cull: front, back or front+back.
    property CullFaceMode: TgxCullFaceMode read FCullFaceMode write SetCullFaceMode;
    // The winding direction that indicates a front facing primitive.
    property FrontFace: {GLEnum} TgxFaceWinding read FFrontFace write SetFrontFace;
    // Enables/Disables polygon smoothing.
    property EnablePolygonSmooth: GLboolean read FEnablePolygonSmooth write SetEnablePolygonSmooth;
    // Whether polygons appear filled, lines or points.
    property PolygonMode: TgxPolygonMode read FPolygonMode write SetPolygonMode;
    // Scales the maximum depth of the polygon.
    property PolygonOffsetFactor: Single read FPolygonOffsetFactor write
      SetPolygonOffsetFactor;
    (* Scales an implementation-dependent constant that relates to the usable
       resolution of the depth buffer. *)
    property PolygonOffsetUnits: Single read FPolygonOffsetUnits write SetPolygonOffsetUnits;
    // Set polygon offset.
    procedure SetPolygonOffset(const factor, units: Single);
    // Enable/Disable polygon offset for polygons in point mode.
    property EnablePolygonOffsetPoint: GLboolean read FEnablePolygonOffsetPoint write SetEnablePolygonOffsetPoint;
    // Enable/Disable polygon offset for polygons in line mode.
    property EnablePolygonOffsetLine: GLboolean read FEnablePolygonOffsetLine write SetEnablePolygonOffsetLine;
    // Enable/Disable polygon offset for polygons in fill mode.
    property EnablePolygonOffsetFill: GLboolean read FEnablePolygonOffsetFill write SetEnablePolygonOffsetFill;
    // Multisample state
    // Enable/Disable multisampling
    property EnableMultisample: GLboolean read FEnableMultisample write SetEnableMultisample;
    // Enable/Disable sample alpha to coverage
    property EnableSampleAlphaToCoverage: GLboolean read FEnableSampleAlphaToCoverage write SetEnableSampleAlphaToCoverage;
    // Enable/Disable sample alpha to one
    property EnableSampleAlphaToOne: GLboolean read FEnableSampleAlphaToOne write SetEnableSampleAlphaToOne;
    // Enable/Disable sample coverage
    property EnableSampleCoverage: GLboolean read FEnableSampleCoverage write SetEnableSampleCoverage;
    // Sample coverage Value
    property SampleCoverageValue: Single read FSampleCoverageValue write SetSampleCoverageValue;
    // Inverts sample coverage Value
    property SampleCoverageInvert: GLboolean read FSampleCoverageInvert write SetSampleCoverageInvert;
    // Set sample coverage
    procedure SetSampleCoverage(const Value: Single; invert: GLboolean);
    // Enable/Disable sample mask
    property EnableSampleMask: GLboolean read FEnableSampleMask write SetEnableSampleMask;
    // Sample mask values
    property SampleMaskValue[Index: Integer]: GLbitfield read GetSampleMaskValue write SetSampleMaskValue;
    // Textures
    // Textures bound to each texture unit + binding point. 
    property TextureBinding[Index: Integer; target: TgxTextureTarget]: GLuint read GetTextureBinding write SetTextureBinding;
    property TextureBindingTime[Index: Integer; target: TgxTextureTarget]: Double read GetTextureBindingTime;
    property ActiveTextureEnabled[Target: TgxTextureTarget]: Boolean read GetActiveTextureEnabled write SetActiveTextureEnabled;
    property SamplerBinding[Index: GLuint]: GLuint read GetSamplerBinding write SetSamplerBinding;
    property MaxTextureSize: GLuint read GetMaxTextureSize;
    property Max3DTextureSize: GLuint read GetMax3DTextureSize;
    property MaxCubeTextureSize: GLuint read GetMaxCubeTextureSize;
    property MaxArrayTextureSize: GLuint read GetMaxArrayTextureSize;
    property MaxTextureImageUnits: GLuint read GetMaxTextureImageUnits;
    property MaxTextureAnisotropy: GLuint read GetMaxTextureAnisotropy;
    property MaxSamples: GLuint read GetMaxSamples;
    // TODO: GL_TEXTURE_BUFFER_DATA_STORE_BINDING ?
    // Active texture
    (* The active texture unit.  Valid values are 0 .. Max texture units. *)
    property ActiveTexture: GLint read FActiveTexture write SetActiveTexture;
    // Pixel operations
    (* Enables/Disables scissor test. *)
    property EnableScissorTest: GLboolean read FEnableScissorTest write SetEnableScissorTest;
    // The bounding box used in scissor test.
    property ScissorBox: TVector4i read FScissorBox write SetScissorBox;
    // Enables/Disables stencil test.
    property EnableStencilTest: GLboolean read FEnableStencilTest write SetEnableStencilTest;
    (* The stencil function.  Determines the comparison function to be used
      when comparing the reference + stored stencil values. *)
    property StencilFunc: TgxStencilFunction read FStencilFunc;
    // write SetStencilFunc;
    (* The stencil value mask.  Masks both the reference + stored stencil values *)
    property StencilValueMask: GLuint read FStencilValueMask;
    // write SetStencilValueMask;
    (* The stencil reference value.  Clamped to 0..255 with an 8 bit stencil. *)
    property StencilRef: GLint read FStencilRef; // write SetStencilRef;
    // The operation to perform when stencil test fails.
    property StencilFail: TgxStencilOp read FStencilFail; // write SetStencilFail;
    // The operation to perform when stencil test passes + depth test fails.
    property StencilPassDepthFail: TgxStencilOp read FStencilPassDepthFail;
    // write SetStencilPassDepthFail;
    (* The operation to perform when stencil test passes + depth test passes. *)
    property StencilPassDepthPass: TgxStencilOp read FStencilPassDepthPass;
    // write SetStencilPassDepthPass;
    (* The stencil back function.  Determines the comparison function to be
      used when comparing the reference + stored stencil values on back facing primitives. *)
    property StencilBackFunc: TgxStencilFunction read FStencilBackFunc;
    // write SetStencilBackFunc;
    (* The stencil back value mask.  Masks both the reference + stored stencil values. *)
    property StencilBackValueMask: GLuint read FStencilBackValueMask;
    // write SetStencilBackValueMask;
    (* The stencil back reference value.  Clamped to 0..255 with an 8 bit stencil. *)
    property StencilBackRef: GLuint read FStencilBackRef;
    // write SetStencilBackRef;
    (* The operation to perform when stencil test fails on back facing primitives. *)
    property StencilBackFail: TgxStencilOp read FStencilBackFail;
    // write SetStencilBackFail;
    (* The operation to perform when stencil test passes + depth test fails on
      back facing primitives. *)
    property StencilBackPassDepthFail: TgxStencilOp read FStencilBackPassDepthFail;
    // write SetStencilBackPassDepthFail;
  { The operation to perform when stencil test passes + depth test passes on
     back facing primitives. }
    property StencilBackPassDepthPass: TgxStencilOp read FStencilBackPassDepthPass;
    // write SetStencilBackPassDepthPass;
  { Used to set stencil Function, Reference + Mask values, for both front +
     back facing primitives. }
    procedure SetStencilFunc(const func: TgxStencilFunction; const ref: GLint; const mask: GLuint);
    (* Used to set stencil Function, Reference + Mask values for either the
      front or back facing primitives (or both, which is the same as calling
      SetStencilFunc). *)
    procedure SetStencilFuncSeparate(const face: TgxCullFaceMode; const func: TgxStencilFunction; const ref: GLint; 
	   const mask: GLuint);  inline;
    { Used to set the StencilFail, StencilPassDepthFail + StencilPassDepthPass
       in one go. }
    procedure SetStencilOp(const fail, zfail, zpass: TgxStencilOp); inline;
    { Used to set the StencilFail, StencilPassDepthFail + StencilPassDepthPass
       in one go, for either front or back facing primitives. }
    procedure SetStencilOpSeparate(const face: TgxCullFaceMode; const sfail, dpfail, dppass: TgxStencilOp);

    { Enables/disables depth testing. }
    property EnableDepthTest: GLboolean read FEnableDepthTest write SetEnableDepthTest;
    { The depth function.  Used to determine whether to keep a fragment or
       discard it, depending on the current value stored in the depth buffer. }
    property DepthFunc: TgxDepthFunction read FDepthFunc write SetDepthFunc;
    { Enables/disables blending for each draw buffer. }
    property EnableBlend[Index: Integer]: GLboolean read GetEnableBlend write SetEnableBlend;
    { The weighting factor used in blending equation, for source RGB. }
    property BlendSrcRGB: TgxBlendFunction read FBlendSrcRGB;
    // write SetBlendSrcRGB;
    (* The weighting factor used in blending equation, for source alpha. *)
    property BlendSrcAlpha: TgxBlendFunction read FBlendSrcAlpha;
    // write SetBlendSrcAlpha;
    (* The weighting factor used in blending equation, for destination RGB. *)
    property BlendDstRGB: TgxDstBlendFunction read FBlendDstRGB;
    // write SetBlendDstRGB;
  { The weighting factor used in blending equation, for destination alpha. }
    property BlendDstAlpha: TgxDstBlendFunction read FBlendDstAlpha;
    // write SetBlendDstAlpha;
  { Sets the weighting factors to be used by the blending equation, for both color + alpha. }
    procedure SetBlendFunc(const Src: TgxBlendFunction; const Dst: TgxDstBlendFunction);
    { Sets the weighting factors to be used by the blending equation, with
       separate values used for color + alpha components. }
    procedure SetBlendFuncSeparate(const SrcRGB: TgxBlendFunction; const DstRGB: TgxDstBlendFunction; 
	  const SrcAlpha: TgxBlendFunction; const DstAlpha: TgxDstBlendFunction); inline;
    { The blending equation.  Determines how the incoming source fragment's
       RGB are combined with the destination RGB. }
    property BlendEquationRGB: TgxBlendEquation read FBlendEquationRGB;
    // write SetBlendEquationRGB;
  { The blending equation.  Determines how the incoming source fragment's
     alpha values are combined with the destination alpha values. }
    property BlendEquationAlpha: TgxBlendEquation read FBlendEquationAlpha;
    // write SetBlendEquationAlpha;
    // Sets the blend equation for RGB + alpha to the same value.
    procedure SetBlendEquation(const mode: TgxBlendEquation);
    // Sets the blend equations for RGB + alpha separately.
    procedure SetBlendEquationSeparate(const modeRGB, modeAlpha: TgxBlendEquation);
    { A constant blend color, that can be used in the blend equation. }
    property BlendColor: TVector4f read FBlendColor write SetBlendColor;
    { Enables/disables framebuffer SRGB. }
    property EnableFramebufferSRGB: GLboolean read FEnableFramebufferSRGB write SetEnableFramebufferSRGB;
    // Enables/disables dithering.
    property EnableDither: GLboolean read FEnableDither write SetEnableDither;
    // Enables/disables color logic op.
    property EnableColorLogicOp: GLboolean read FEnableColorLogicOp write SetEnableColorLogicOp;
    { Logic op mode. }
    property LogicOpMode: TgxLogicOp read FLogicOpMode write SetLogicOpMode;
    // Framebuffer control
    { The color write mask, for each draw buffer. }
    property ColorWriteMask[Index: Integer]: TgxColorMask read GetColorWriteMask
    write SetColorWriteMask;
    { Set the color write mask for all draw buffers. }
    procedure SetColorMask(mask: TgxColorMask);
    { The depth write mask. }
    property DepthWriteMask: Boolean read FDepthWriteMask write SetDepthWriteMask;
    { The stencil write mask. }
    property StencilWriteMask: GLuint read FStencilWriteMask write SetStencilWriteMask;
    { The stencil back write mask. }
    property StencilBackWriteMask: GLuint read FStencilBackWriteMask write SetStencilBackWriteMask;
    { The color clear value. }
    property ColorClearValue: TVector4f read FColorClearValue write SetColorClearValue;
    { The depth clear value. }
    property DepthClearValue: Single read FDepthClearValue write SetDepthClearValue;
    // The stencil clear value.
    property StencilClearValue: GLuint read FStencilClearValue write SetStencilClearValue;
    // Framebuffer to be used for draw operations, 0 = default framebuffer.
    property DrawFrameBuffer: GLuint read FDrawFrameBuffer write SetDrawFrameBuffer;
    // Framebuffer to be used for read operations, 0 = default framebuffer.
    property ReadFrameBuffer: GLuint read FReadFrameBuffer write SetReadFrameBuffer;
    // set both draw + read framebuffer.
    procedure SetFrameBuffer(const Value: GLuint);
    //property FrameBuffer: GLuint read FDrawFrameBuffer write SetFrameBuffer;
    { Currently bound render buffer. }
    property RenderBuffer: GLuint read FRenderBuffer write SetRenderBuffer;
    // Pixels
    (* Controls whether byte swapping occurs during pixel unpacking. *)
    property UnpackSwapBytes: GLboolean read FUnpackSwapBytes write SetUnpackSwapBytes;
    // Whether unpacked data is required with LSB (least significant bit) first.
    property UnpackLSBFirst: GLboolean read FUnpackLSBFirst write SetUnpackLSBFirst;
    { Unpack image height. }
    property UnpackImageHeight: GLuint read FUnpackImageHeight write SetUnpackImageHeight;
    { Unpack skip images. }
    property UnpackSkipImages: GLuint read FUnpackSkipImages write SetUnpackSkipImages;
    { Unpack row length. }
    property UnpackRowLength: GLuint read FUnpackRowLength write SetUnpackRowLength;
    { Unpack skip rows. }
    property UnpackSkipRows: GLuint read FUnpackSkipRows write SetUnpackSkipRows;
    { Unpack skip pixels. }
    property UnpackSkipPixels: GLuint read FUnpackSkipPixels write SetUnpackSkipPixels;
    { Unpack alignment. }
    property UnpackAlignment: GLuint read FUnpackAlignment write SetUnpackAlignment;
    { Controls whether byte swapping occurs during pixel packing. }
    property PackSwapBytes: GLboolean read FPackSwapBytes write SetPackSwapBytes;
    { Whether packed data is required with LSB (least significant bit) first. }
    property PackLSBFirst: GLboolean read FPackLSBFirst write SetPackLSBFirst;
    // Pack image height
    property PackImageHeight: GLuint read FPackImageHeight write SetPackImageHeight;
    // Pack skip images
    property PackSkipImages: GLuint read FPackSkipImages write SetPackSkipImages;
    // Pack row length
    property PackRowLength: GLuint read FPackRowLength write SetPackRowLength;
    // Pack skip rows
    property PackSkipRows: GLuint read FPackSkipRows write SetPackSkipRows;
    // Pack skip pixels
    property PackSkipPixels: GLuint read FPackSkipPixels write SetPackSkipPixels;
    // Pack alignment
    property PackAlignment: GLuint read FPackAlignment write SetPackAlignment;
    // Buffer bound for pixel packing (eg. ReadPixels)
    property PixelPackBufferBinding: GLuint read FPixelPackBufferBinding write SetPixelPackBufferBinding;
    // Buffer bound for pixel unpacking (eg. Tex*Image)
    property PixelUnpackBufferBinding: GLuint read FPixelUnpackBufferBinding write SetPixelUnpackBufferBinding;
    // Currently bound program
    property CurrentProgram: GLuint read FCurrentProgram write SetCurrentProgram;
    property MaxTextureUnits: GLuint read GetMaxTextureUnits;
    // Currently bound uniform buffer
    property UniformBufferBinding: GLuint read FUniformBufferBinding write SetUniformBufferBinding;
    procedure SetBufferIndexedBinding(const Value: GLuint; ATarget: TgxBufferBindingTarget; AIndex: GLuint; 
	  ABufferSize: PGLsizei); overload;
    procedure SetBufferIndexedBinding(const Value: GLuint; ATarget: TgxBufferBindingTarget; AIndex: GLuint; 
	   AOffset: GLint; ARangeSize: PGLsizei); overload;
    // Default values to be used when a vertex array is not used for that attribute
    property CurrentVertexAttrib[Index: Integer]: TVector4f read GetCurrentVertexAttrib write SetCurrentVertexAttrib;
    // Enables/disables program point size
    property EnableProgramPointSize: GLboolean read FEnableProgramPointSize write SetEnableProgramPointSize;
    // Currently bound transform feedbac buffer
    property TransformFeedbackBufferBinding: GLuint read FTransformFeedbackBufferBinding 
	  write SetTransformFeedbackBufferBinding;
    // Line smooth hint
    property LineSmoothHint: TgxHintType read FLineSmoothHint write SetLineSmoothHint;
    // Polygon smooth hint
    property PolygonSmoothHint: TgxHintType read FPolygonSmoothHint write SetPolygonSmoothHint;
    // Texture compression hint
    property TextureCompressionHint: TgxHintType read FTextureCompressionHint write SetTextureCompressionHint;
    // Fragment shader derivitive hint
    property FragmentShaderDerivitiveHint: TgxHintType read FFragmentShaderDerivitiveHint write SetFragmentShaderDerivitiveHint;
    property MultisampleFilterHint: TgxHintType read FMultisampleFilterHint write SetMultisampleFilterHint;
    // Current queries. Misc
    property CurrentQuery[Index: TgxQueryType]: GLuint read GetCurrentQuery;
    // Begins a query of "Target" type.  "Value" must be a valid query object
    procedure BeginQuery(const Target: TgxQueryType; const Value: GLuint);
    { Ends current query of type "Target". }
    procedure EndQuery(const Target: TgxQueryType); inline;
    { The buffer currently bound to the copy read buffer binding point, this
       is an extra binding point provided so that you don't need to overwrite
       other binding points to copy between buffers. }
    property CopyReadBufferBinding: GLuint read FCopyReadBufferBinding write SetCopyReadBufferBinding;
    { The buffer currently bound to the copy write buffer binding point, this
       is an extra binding point provided so that you don't need to overwrite
       other binding points to copy between buffers. }
    property CopyWriteBufferBinding: GLuint read FCopyWriteBufferBinding write SetCopyWriteBufferBinding;
    { Enables/Disables seamless texture cube maps. }
    property EnableTextureCubeMapSeamless: GLboolean read FEnableTextureCubeMapSeamless write SetEnableTextureCubeMapSeamless;
    // Indicates the current presence within the list
    property InsideList: Boolean read FInsideList;
    // Begin new display list
    procedure NewList(list: GLuint; mode: GLEnum); inline;
    // End display list
    procedure EndList; inline;
    // Call display list
    procedure CallList(list: Cardinal); inline;
    // Defines the OpenGL texture matrix. Assumed texture mode is GL_MODELVIEW.
    procedure SetTextureMatrix(const matrix: TMatrix4f); inline;
    procedure ResetTextureMatrix; inline;
    procedure ResetAllTextureMatrix; inline;
    // note: needs to change to per draw-buffer
    procedure SetColorWriting(flag: Boolean); inline;
    // Inverts front face winding (CCW/CW)
    procedure InvertFrontFace; inline;
    // read only properties
    property States: TgxStates read FStates;
    // True for ignore deprecated and removed features in OpenGL 3x
    property ForwardContext: Boolean read FForwardContext write SetForwardContext;
  end;

type
  TgxStateRecord = record
    GLConst: Cardinal;   //OpenGL
    VKConst: Cardinal;  //Vulkan
    IsDeprecated: Boolean;
  end;

const
{$WARN SYMBOL_DEPRECATED OFF}
  cGLStateTypeToGLEnum: array[TgxStateType] of Cardinal = (
    GL_CURRENT_BIT, GL_POINT_BIT, GL_LINE_BIT, GL_POLYGON_BIT,
    GL_POLYGON_STIPPLE_BIT, GL_PIXEL_MODE_BIT, GL_LIGHTING_BIT, GL_FOG_BIT,
    GL_DEPTH_BUFFER_BIT, GL_ACCUM_BUFFER_BIT, GL_STENCIL_BUFFER_BIT,
    GL_VIEWPORT_BIT, GL_TRANSFORM_BIT, GL_ENABLE_BIT, GL_COLOR_BUFFER_BIT,
    GL_HINT_BIT, GL_EVAL_BIT, GL_LIST_BIT, GL_TEXTURE_BIT, GL_SCISSOR_BIT,
    GL_MULTISAMPLE_BIT);

{$WARN SYMBOL_DEPRECATED ON}
  cGLStateToGLEnum: array[TgxState] of TgxStateRecord =
    ((GLConst: GL_ALPHA_TEST; IsDeprecated: True),
    (GLConst: GL_AUTO_NORMAL; IsDeprecated: True),
    (GLConst: GL_BLEND; IsDeprecated: False),
    (GLConst: GL_COLOR_MATERIAL; IsDeprecated: True),
    (GLConst: GL_CULL_FACE; IsDeprecated: False),
    (GLConst: GL_DEPTH_TEST; IsDeprecated: False),
    (GLConst: GL_DITHER; IsDeprecated: False),
    (GLConst: GL_FOG; IsDeprecated: True),
    (GLConst: GL_LIGHTING; IsDeprecated: True),
    (GLConst: GL_LINE_SMOOTH; IsDeprecated: True),
    (GLConst: GL_LINE_STIPPLE; IsDeprecated: True),
    (GLConst: GL_INDEX_LOGIC_OP; IsDeprecated: True),
    (GLConst: GL_COLOR_LOGIC_OP; IsDeprecated: False),
    (GLConst: GL_NORMALIZE; IsDeprecated: True),
    (GLConst: GL_POINT_SMOOTH; IsDeprecated: True),
    (GLConst: GL_POINT_SPRITE; IsDeprecated: True),
    (GLConst: GL_POLYGON_SMOOTH; IsDeprecated: True),
    (GLConst: GL_POLYGON_STIPPLE; IsDeprecated: True),
    (GLConst: GL_SCISSOR_TEST; IsDeprecated: False),
    (GLConst: GL_STENCIL_TEST; IsDeprecated: False),
    (GLConst: GL_POLYGON_OFFSET_POINT; IsDeprecated: False),
    (GLConst: GL_POLYGON_OFFSET_LINE; IsDeprecated: False),
    (GLConst: GL_POLYGON_OFFSET_FILL; IsDeprecated: False),
    (GLConst: GL_DEPTH_CLAMP; IsDeprecated: False)
    );

  cGLTexTypeToGLEnum: array[TgxTextureTarget] of GLEnum =
    (0, GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY,
    GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_BUFFER,
    GL_TEXTURE_CUBE_MAP, GL_TEXTURE_2D_MULTISAMPLE,
    GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_CUBE_MAP_ARRAY);

  cGLQueryTypeToGLEnum: array[TgxQueryType] of GLEnum =
    (GL_SAMPLES_PASSED, GL_PRIMITIVES_GENERATED,
    GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN,
    GL_TIME_ELAPSED, GL_ANY_SAMPLES_PASSED);

  cGLStencilOpToGLEnum: array[TgxStencilOp] of GLEnum =
    (GL_KEEP, GL_ZERO, GL_REPLACE, GL_INCR, GL_DECR, GL_INVERT, GL_INCR_WRAP,
    GL_DECR_WRAP);

  cGLLogicOpToGLEnum: array[TgxLogicOp] of GLEnum =
    (GL_CLEAR, GL_AND, GL_AND_REVERSE, GL_COPY, GL_AND_INVERTED, GL_NOOP,
    GL_XOR, GL_OR, GL_NOR, GL_EQUIV, GL_INVERT, GL_OR_REVERSE,
    GL_COPY_INVERTED, GL_OR_INVERTED, GL_NAND, GL_SET);

  cGLComparisonFunctionToGLEnum: array[TgxComparisonFunction] of GLEnum =
    (GL_NEVER, GL_ALWAYS, GL_LESS, GL_LEQUAL, GL_EQUAL, GL_GREATER,
    GL_NOTEQUAL, GL_GEQUAL);

  cGLBlendFunctionToGLEnum: array[TgxBlendFunction] of GLEnum =
    (GL_ZERO, GL_ONE, GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR, GL_DST_COLOR,
    GL_ONE_MINUS_DST_COLOR, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA,
    GL_DST_ALPHA, GL_ONE_MINUS_DST_ALPHA, GL_CONSTANT_COLOR,
    GL_ONE_MINUS_CONSTANT_COLOR, GL_CONSTANT_ALPHA,
    GL_ONE_MINUS_CONSTANT_ALPHA, GL_SRC_ALPHA_SATURATE {valid for src only});

  cGLBlendEquationToGLEnum: array[TgxBlendEquation] of GLEnum =
    (GL_FUNC_ADD, GL_FUNC_SUBTRACT, GL_FUNC_REVERSE_SUBTRACT, GL_MIN,
    GL_MAX);

  cGLFaceWindingToGLEnum: array[TgxFaceWinding] of GLEnum =
    (GL_CCW, GL_CW);

  cGLPolygonModeToGLEnum: array[TgxPolygonMode] of GLEnum =
    (GL_FILL, GL_LINE, GL_POINT);

  cGLCullFaceModeToGLEnum: array[TgxCullFaceMode] of GLEnum =
    (GL_FRONT, GL_BACK, GL_FRONT_AND_BACK);

  cGLHintToGLEnum: array[TgxHintType] of GLEnum =
    (GL_DONT_CARE, GL_FASTEST, GL_NICEST);

  cGLBufferBindingTarget: array[TgxBufferBindingTarget] of GLEnum =
    (GL_UNIFORM_BUFFER, GL_TRANSFORM_FEEDBACK_BUFFER);

//------------------------------------------------------
implementation
//------------------------------------------------------

uses
  GXS.Context,
  GXS.Color;

  // ------------------
  // ------------------ TgxStateCache ------------------
  // ------------------

procedure TgxStateCache.BeginQuery(const Target: TgxQueryType; const Value:
  GLuint);
begin
  Assert(FCurrentQuery[Target] = 0, 'Can only have one query (of each type)' +
    ' running at a time');
  // Assert(glIsQuery(Value), 'Not a valid query');
 //  if Value<>FCurrentQuery[Target] then
  begin
    FCurrentQuery[Target] := Value;
    glBeginQuery(cGLQueryTypeToGLEnum[Target], Value);
  end;
end;

constructor TgxStateCache.Create;
var
  I: Integer;
begin
  inherited;
  SetLength(FListStates, 128);
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
  FEnableDepthClamp := 0;

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
  FEnableLineSmooth := 0;
  FEnableCullFace := 0;
  FCullFaceMode := cmBack;
  FFrontFace := fwCounterClockWise;
  FEnablePolygonSmooth := 0;
  FPolygonMode := pmFill;
  FPolygonOffsetFactor := 0.0;
  FPolygonOffsetUnits := 0.0;
  FEnablePolygonOffsetPoint := 0;
  FEnablePolygonOffsetLine := 0;
  FEnablePolygonOffsetFill := 0;

  // Multisample state
  FEnableMultisample := 1;
  FEnableSampleAlphaToCoverage := 0;
  FEnableSampleAlphaToOne := 0;
  FEnableSampleCoverage := 0;
  FSampleCoverageValue := 1.0;
  FSampleCoverageInvert := 0;
  FEnableSampleMask := 0;
  FillChar(FSampleMaskValue, sizeof(FSampleMaskValue), $FF);

  // Texture state
  FillChar(FTextureBinding, sizeof(FTextureBinding), $00);
  FillChar(FActiveTextureEnabling, sizeof(FActiveTextureEnabling), $00);

  // Active texture state
  FActiveTexture := 0;

  // Pixel operation state
  FEnableScissorTest := 0;
  //    FScissorBox := Rect(0, 0, Width, Height);
  FEnableStencilTest := 0;
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

  FEnableDepthTest := 0;
  FDepthFunc := cfLess;

  FillChar(FEnableBlend, sizeof(FEnableBlend), $0);

  FBlendSrcRGB := bfOne;
  FBlendSrcAlpha := bfOne;
  FBlendDstRGB := bfZero;
  FBlendDstAlpha := bfZero;

  FBlendEquationRGB := beAdd;
  FBlendEquationAlpha := beAdd;
  FBlendColor := NullHmgVector;

  FEnableFramebufferSRGB := 0;
  FEnableDither := 1;
  FEnableColorLogicOp := 0;

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
  FUnpackSwapBytes := 0;
  FUnpackLSBFirst := 0;
  FUnpackImageHeight := 0;
  FUnpackSkipImages := 0;
  FUnpackRowLength := 0;
  FUnpackSkipRows := 0;
  FUnpackSkipPixels := 0;
  FUnpackAlignment := 4;
  FPackSwapBytes := 0;
  FPackLSBFirst := 0;
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
  FillChar(FUBOStates[bbtUniform][0], SizeOf(FUBOStates), $00);

  // Vector + Geometry Shader state
  for I := 0 to Length(FCurrentVertexAttrib) - 1 do
    FCurrentVertexAttrib[I] := NullHmgPoint;
  FEnableProgramPointSize := 0;

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
  FEnableTextureCubeMapSeamless := 0;
  FInsideList := False;
end;

destructor TgxStateCache.Destroy;
begin
  inherited;
end;

procedure TgxStateCache.EndQuery(const Target: TgxQueryType);
begin
  Assert(FCurrentQuery[Target] <> 0, 'No query running');
  FCurrentQuery[Target] := 0;
  glEndQuery(cGLQueryTypeToGLEnum[Target]);
end;

procedure TgxStateCache.Enable(const aState: TgxState);
begin
  { if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit; }
  if not(aState in FStates) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      Include(FStates, aState);
    glEnable(cGLStateToGLEnum[aState].GLConst);
  end;
end;

procedure TgxStateCache.Disable(const aState: TgxState);
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
    if not glIsEnabled(cGLStateToGLEnum[aState].GLConst) then
      ShowMessages(strStateCashMissing + 'Disable');
{$ENDIF}
    glDisable(cGLStateToGLEnum[aState].GLConst);
    if aState = stColorMaterial then
      if FInsideList then
        Include(FListStates[FCurrentList], sttLighting)
      else
        begin
          glMaterialfv(GL_FRONT, GL_EMISSION, @FFrontBackColors[0][0]);
          glMaterialfv(GL_FRONT, GL_AMBIENT, @FFrontBackColors[0][1]);
          glMaterialfv(GL_FRONT, GL_DIFFUSE, @FFrontBackColors[0][2]);
          glMaterialfv(GL_FRONT, GL_SPECULAR, @FFrontBackColors[0][3]);
          glMateriali(GL_FRONT, GL_SHININESS, FFrontBackShininess[0]);

          glMaterialfv(GL_BACK, GL_EMISSION, @FFrontBackColors[1][0]);
          glMaterialfv(GL_BACK, GL_AMBIENT, @FFrontBackColors[1][1]);
          glMaterialfv(GL_BACK, GL_DIFFUSE, @FFrontBackColors[1][2]);
          glMaterialfv(GL_BACK, GL_SPECULAR, @FFrontBackColors[1][3]);
          glMateriali(GL_BACK, GL_SHININESS, FFrontBackShininess[1]);
        end;
  end;
end;

procedure TgxStateCache.PerformEnable(const aState: TgxState);
begin
  { if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit; }
  Include(FStates, aState);
  glEnable(cGLStateToGLEnum[aState].GLConst);
end;

procedure TgxStateCache.PerformDisable(const aState: TgxState);
begin
  { if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit; }
  Exclude(FStates, aState);
  glDisable(cGLStateToGLEnum[aState].GLConst);
end;

procedure TgxStateCache.PopAttrib;
begin
  // TODO: replace with proper client side push/pop
  glPopAttrib();
end;

procedure TgxStateCache.PushAttrib(stateTypes: TgxStateTypes);
var
  tempFlag: GLuint;
  I: Integer;
begin
  // TODO: replace with proper client side push/pop
  tempFlag := 0;
  for I := Integer(Low(TgxStateType)) to Integer(high(TgxStateType)) do
  begin
    if TgxStateType(I) in stateTypes then
    begin
      tempFlag := tempFlag or cGLStateTypeToGLEnum[TgxStateType(I)];
    end;
  end;
  glPushAttrib(tempFlag);
end;

procedure TgxStateCache.SetMaterialColors(const aFace: TgxCullFaceMode;
  const emission, ambient, diffuse, specular: TVector4f;
  const shininess: Integer);
var
  i: Integer;
  currentFace: GLEnum;
begin
  if FForwardContext then
    exit;
  Assert((aFace = cmFront) or (aFace = cmBack),
    'Only cmFront or cmBack supported');
  i := Integer(aFace);
  currentFace := cGLCullFaceModeToGLEnum[aFace];

  if (FFrontBackShininess[i] <> shininess)
    or FInsideList then
  begin
    glMateriali(currentFace, GL_SHININESS, shininess);
    if not FInsideList then
      FFrontBackShininess[i] := shininess;
  end;
  if not AffineVectorEquals(FFrontBackColors[i][0], emission)
    or FInsideList then
  begin
    glMaterialfv(currentFace, GL_EMISSION, @emission);
    if not FInsideList then
      SetVector(FFrontBackColors[i][0], emission);
  end;
  if not AffineVectorEquals(FFrontBackColors[i][1], ambient)
    or FInsideList then
  begin
    glMaterialfv(currentFace, GL_AMBIENT, @ambient);
    if not FInsideList then
      SetVector(FFrontBackColors[i][1], ambient);
  end;
  if not VectorEquals(FFrontBackColors[i][2], diffuse)
    or FInsideList then
  begin
    glMaterialfv(currentFace, GL_DIFFUSE, @diffuse);
    if not FInsideList then
      SetVector(FFrontBackColors[i][2], diffuse);
  end;
  if not AffineVectorEquals(FFrontBackColors[i][3], specular)
    or FInsideList then
  begin
    glMaterialfv(currentFace, GL_SPECULAR, @specular);
    if not FInsideList then
      SetVector(FFrontBackColors[i][3], specular);
  end;
  if FInsideList then
    Include(FListStates[FCurrentList], sttLighting);
end;

procedure TgxStateCache.SetMaterialAlphaChannel(const aFace: GLEnum; const alpha: Single);
var
  i: Integer;
  color: TVector4f;
begin
  if FForwardContext then Exit;

  if not(stLighting in FStates) then
  begin
    // We need a temp variable, because FColor is cauched.
    glGetFloatv(GL_CURRENT_COLOR, @color);
    color.W := alpha;
    glColor4fv(@color);
  end
  else
  begin
    i := aFace - GL_FRONT;
    if (FFrontBackColors[i][2].W <> alpha) or FInsideList then
    begin
      if FInsideList then
      begin
        Include(FListStates[FCurrentList], sttLighting);
        glMaterialfv(aFace, GL_DIFFUSE, @FFrontBackColors[i][2]);

      end
      else
      begin
        FFrontBackColors[i][2].W := alpha;
        glMaterialfv(aFace, GL_DIFFUSE, @FFrontBackColors[i][2]);
      end;
    end;
  end;
end;

procedure TgxStateCache.SetMaterialDiffuseColor(const aFace: GLEnum; const diffuse: TVector4f);
var
  i: Integer;
begin
  { if FForwardContext then Exit; }

  if not(stLighting in FStates) then
  begin
    glColor4fv(@diffuse);
  end
  else
  begin
    //
    i := aFace - GL_FRONT;
    if (not VectorEquals(FFrontBackColors[i][2], diffuse)) or FInsideList then
    begin
      if FInsideList then
      begin
        Include(FListStates[FCurrentList], sttLighting);
        glMaterialfv(aFace, GL_DIFFUSE, @FFrontBackColors[i][2]);
      end
      else
      begin
        FFrontBackColors[i][2] := diffuse;
        glMaterialfv(aFace, GL_DIFFUSE, @diffuse);
      end;
    end;
  end;
end;

procedure TgxStateCache.SetActiveTexture(const Value: GLint);
begin
    if (Value <> FActiveTexture) or FInsideList then
    begin
      if FInsideList then
        Include(FListStates[FCurrentList], sttTexture)
      else
        FActiveTexture := Value;
      glActiveTexture(GL_TEXTURE0 + Value);
    end;
end;

procedure TgxStateCache.SetVertexArrayBinding(const Value: GLuint);
begin
  if Value <> FVertexArrayBinding then
  begin
    FVertexArrayBinding := Value;
    glBindVertexArray(Value);
  end;
end;

function TgxStateCache.GetArrayBufferBinding: GLuint;
begin
  Result := FArrayBufferBinding;
end;

procedure TgxStateCache.SetArrayBufferBinding(const Value: GLuint);
begin
  if (Value <> FArrayBufferBinding) or (FVertexArrayBinding <> 0) then
  begin
    FArrayBufferBinding := Value;
    glBindBuffer(GL_ARRAY_BUFFER, Value);
  end;
end;

function TgxStateCache.GetElementBufferBinding: GLuint;
begin
  Result := FElementBufferBinding
end;

procedure TgxStateCache.SetElementBufferBinding(const Value: GLuint);
begin
  if (Value <> FElementBufferBinding) or (FVertexArrayBinding <> 0) then
  begin
    FElementBufferBinding := Value;
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Value);
  end;
end;

function TgxStateCache.GetEnablePrimitiveRestart: GLboolean;
begin
  Result := FEnablePrimitiveRestart;
end;

procedure TgxStateCache.SetEnablePrimitiveRestart(const enabled: GLboolean);
begin
  if enabled <> FEnablePrimitiveRestart then
  begin
    FEnablePrimitiveRestart := enabled;
    if FForwardContext then
    begin
      if enabled > 0 then
        glEnable(GL_PRIMITIVE_RESTART)
      else
        glDisable(GL_PRIMITIVE_RESTART);
    end
    else
    begin
      if enabled > 0 then
        glEnableClientState(GL_PRIMITIVE_RESTART_NV)
      else
        glDisableClientState(GL_PRIMITIVE_RESTART_NV);
    end;
  end;
end;

function TgxStateCache.GetPrimitiveRestartIndex: GLuint;
begin
  Result := FPrimitiveRestartIndex;
end;

procedure TgxStateCache.SetPrimitiveRestartIndex(const index: GLuint);
begin
  if index <> FPrimitiveRestartIndex then
  begin
    if FForwardContext then
    begin
      FPrimitiveRestartIndex := index;
      glPrimitiveRestartIndex(index)
    end;
  end;
end;

procedure TgxStateCache.SetEnableProgramPointSize(const Value: GLboolean);
begin
  if Value <> FEnableProgramPointSize then
  begin
    FEnableProgramPointSize := Value;
    if Value > 0 then
      glEnable(GL_PROGRAM_POINT_SIZE)
    else
      glDisable(GL_PROGRAM_POINT_SIZE);
  end;
end;

procedure TgxStateCache.SetBlendColor(const Value: TVector4f);
begin
  if not VectorEquals(Value, FBlendColor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FBlendColor := Value;
    glBlendColor(Value.X, Value.Y, Value.Z, Value.W);
  end;
end;

procedure TgxStateCache.SetBlendEquationSeparate(const modeRGB, modeAlpha: TgxBlendEquation);
begin
  if (modeRGB <> FBlendEquationRGB) or (modeAlpha <> FBlendEquationAlpha)
    or FInsideList then
  begin
    FBlendEquationRGB := modeRGB;
    FBlendEquationAlpha := modeAlpha;
    glBlendEquationSeparate(cGLBlendEquationToGLEnum[modeRGB],
      cGLBlendEquationToGLEnum[modeAlpha]);
  end;
  if FInsideList then
    Include(FListStates[FCurrentList], sttColorBuffer);
end;

procedure TgxStateCache.SetBlendEquation(const mode: TgxBlendEquation);
begin
  if (mode <> FBlendEquationRGB) or (mode <> FBlendEquationAlpha)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FBlendEquationRGB := mode;
      FBlendEquationAlpha := mode;
    end;
    glBlendEquation(cGLBlendEquationToGLEnum[mode]);
  end;
end;

procedure TgxStateCache.SetBlendFunc(const Src: TgxBlendFunction;
  const Dst: TgxDstBlendFunction);
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
    glBlendFunc(cGLBlendFunctionToGLEnum[Src], cGLBlendFunctionToGLEnum[Dst]);
  end;
end;

procedure TgxStateCache.SetBlendFuncSeparate(const SrcRGB: TgxBlendFunction;
  const DstRGB: TgxDstBlendFunction; const SrcAlpha: TgxBlendFunction;
  const DstAlpha: TgxDstBlendFunction);
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
    glBlendFuncSeparate(
      cGLBlendFunctionToGLEnum[SrcRGB],
      cGLBlendFunctionToGLEnum[DstRGB],
      cGLBlendFunctionToGLEnum[SrcAlpha],
      cGLBlendFunctionToGLEnum[DstAlpha]);
  end;
end;

procedure TgxStateCache.SetClampReadColor(const Value: GLEnum);
begin
  if (Value <> FClampReadColor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FClampReadColor := Value;
    glClampColor(GL_CLAMP_READ_COLOR, Value);
  end;
end;

procedure TgxStateCache.SetColorWriteMask(Index: Integer;
  const Value: TgxColorMask);
begin
  if FColorWriteMask[Index] <> Value then
  begin
    FColorWriteMask[Index] := Value;
    glColorMaski(Index,  Byte(ccRed in Value),
                         Byte(ccGreen in Value),
                         Byte(ccBlue in Value),
                         Byte(ccAlpha in Value));
  end;
end;

procedure TgxStateCache.SetCopyReadBufferBinding(const Value: GLuint);
begin
  if Value <> FCopyReadBufferBinding then
  begin
    FCopyReadBufferBinding := Value;
    glBindBuffer(GL_COPY_READ_BUFFER, Value);
  end;
end;

procedure TgxStateCache.SetCopyWriteBufferBinding(const Value: GLuint);
begin
  if Value <> FCopyWriteBufferBinding then
  begin
    FCopyWriteBufferBinding := Value;
    glBindBuffer(GL_COPY_WRITE_BUFFER, Value);
  end;
end;

procedure TgxStateCache.SetCullFaceMode(const Value: TgxCullFaceMode);
begin
  if (Value <> FCullFaceMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FCullFaceMode := Value;
    glCullFace(cGLCullFaceModeToGLEnum[Value]);
  end;

end;

procedure TgxStateCache.SetCurrentProgram(const Value: GLuint);
begin
  if Value <> FCurrentProgram then
  begin
    FCurrentProgram := Value;
    glUseProgram(Value);
  end;
end;

procedure TgxStateCache.SetTextureBufferBinding(const Value: GLuint);
begin
  if Value <> FTextureBufferBinding then
  begin
    FTextureBufferBinding := Value;
    glBindBuffer(GL_TEXTURE_BUFFER, Value);
  end;
end;

procedure TgxStateCache.SetCurrentVertexAttrib(Index: Integer; const Value: TVector4f);
begin
  if not VectorEquals(Value, FCurrentVertexAttrib[Index]) then
  begin
    FCurrentVertexAttrib[Index] := Value;
    glVertexAttrib4fv(Index, @Value.X);
  end;
end;

procedure TgxStateCache.SetDepthClearValue(const Value: Single);
begin
  if (Value <> FDepthClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthClearValue := Value;
    glClearDepth(Value);
  end;

end;

procedure TgxStateCache.SetDepthFunc(const Value: TgxDepthFunction);
begin
  if (Value <> FDepthFunc) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthFunc := Value;
    glDepthFunc(cGLComparisonFunctionToGLEnum[Value]);
  end;

end;

procedure TgxStateCache.SetDepthRange(const ZNear, ZFar: GLclampd);
begin
  if (ZNear <> FDepthRange[0]) or (ZFar <> FDepthRange[1])
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
    begin
      FDepthRange[0] := ZNear;
      FDepthRange[1] := ZFar;
    end;
    glDepthRange(ZNear, ZFar);
  end;
end;

procedure TgxStateCache.SetDepthRangeFar(const Value: GLclampd);
begin
  if (Value <> FDepthRange[1]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FDepthRange[1] := Value;
    glDepthRange(FDepthRange[0], Value);
  end;
end;

procedure TgxStateCache.SetDepthRangeNear(const Value: GLclampd);
begin
  if (Value <> FDepthRange[0]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FDepthRange[0] := Value;
    glDepthRange(Value, FDepthRange[1]);
  end;
end;

procedure TgxStateCache.SetDepthWriteMask(const Value: Boolean);
begin
  if (Value <> FDepthWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthWriteMask := Value;
    glDepthMask(Byte(Value));
  end;
end;

procedure TgxStateCache.SetDrawFrameBuffer(const Value: GLuint);
begin
  if Value <> FDrawFrameBuffer then
  begin
    FDrawFrameBuffer := Value;
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, Value);
  end;
end;

procedure TgxStateCache.SetEnableBlend(Index: Integer;
  const Value: GLboolean);
begin
  if FEnableBlend[Index] <> Value then
  begin
    FEnableBlend[Index] := Value;
    if Value > 0 then
      glEnablei(GL_BLEND, Index)
    else
      glDisablei(GL_BLEND, Index);
  end;
end;

procedure TgxStateCache.SetEnableClipDistance(Index: Cardinal; const Value: GLboolean);
begin
  if FEnableClipDistance[Index] <> Value then
  begin
    FEnableClipDistance[Index] := Value;
    if Value > 0 then
      glEnable(GL_CLIP_DISTANCE0 + Index)
    else
      glDisable(GL_CLIP_DISTANCE0 + Index);
  end;
end;

procedure TgxStateCache.SetEnableColorLogicOp(const Value: GLboolean);
begin
  if Value <> FEnableColorLogicOp then
  begin
    FEnableColorLogicOp := Value;
    if Value > 0 then
      glEnable(GL_COLOR_LOGIC_OP)
    else
      glDisable(GL_COLOR_LOGIC_OP);
  end;
end;

procedure TgxStateCache.SetEnableCullFace(const Value: GLboolean);
begin

end;

procedure TgxStateCache.SetEnableDepthClamp(const enabled: GLboolean);
begin

end;

procedure TgxStateCache.SetEnableDepthTest(const Value: GLboolean);
begin

end;

procedure TgxStateCache.SetEnableDither(const Value: GLboolean);
begin

end;

procedure TgxStateCache.SetEnableFramebufferSRGB(const Value: GLboolean);
begin

end;

procedure TgxStateCache.SetEnableLineSmooth(const Value: GLboolean);
begin

end;

procedure TgxStateCache.SetEnableMultisample(const Value: GLboolean);
begin

end;

procedure TgxStateCache.SetEnablePolygonOffsetFill(const Value: GLboolean);
begin

end;

procedure TgxStateCache.SetEnablePolygonOffsetLine(const Value: GLboolean);
begin

end;

procedure TgxStateCache.SetEnablePolygonOffsetPoint(const Value: GLboolean);
begin

end;

procedure TgxStateCache.SetEnablePolygonSmooth(const Value: GLboolean);
begin

end;

procedure TgxStateCache.SetEnableSampleAlphaToCoverage(const Value: GLboolean);
begin
  if Value <> FEnableSampleAlphaToCoverage then
  begin
    FEnableSampleAlphaToCoverage := Value;
    if Value > 0 then // True
      glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE)
    else
      glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
  end;
end;

procedure TgxStateCache.SetEnableSampleCoverage(const Value: GLboolean);
begin
  if Value <> FEnableSampleCoverage then
  begin
    FEnableSampleCoverage := Value;
    if Value > 0 then
      glEnable(GL_SAMPLE_COVERAGE)
    else
      glDisable(GL_SAMPLE_COVERAGE);
  end;
end;

procedure TgxStateCache.SetEnableSampleMask(const Value: GLboolean);
begin
  if Value <> FEnableSampleMask then
  begin
    FEnableSampleMask := Value;
    if Value > 0 then
      glEnable(GL_SAMPLE_MASK)
    else
      glDisable(GL_SAMPLE_MASK);
  end;
end;

procedure TgxStateCache.SetEnableSampleAlphaToOne(const Value: GLboolean);
begin
  if Value <> FEnableSampleAlphaToOne then
  begin
    FEnableSampleAlphaToOne := Value;
    if Value > 0 then
      glEnable(GL_SAMPLE_ALPHA_TO_ONE)
    else
      glDisable(GL_SAMPLE_ALPHA_TO_ONE);
  end;
end;

procedure TgxStateCache.SetEnableScissorTest(const Value: GLboolean);
begin

end;

procedure TgxStateCache.SetEnableStencilTest(const Value: GLboolean);
begin

end;

procedure TgxStateCache.SetFragmentShaderDerivitiveHint(const Value: TgxHintType);
begin
  if Value <> FFragmentShaderDerivitiveHint then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FFragmentShaderDerivitiveHint := Value;
    glHint(GL_FRAGMENT_SHADER_DERIVATIVE_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TgxStateCache.SetMultisampleFilterHint(const Value: TgxHintType);
begin
  if Value <> FMultisampleFilterHint then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FMultisampleFilterHint := Value;
    glHint(GL_MULTISAMPLE_FILTER_HINT_NV, cGLHintToGLEnum[Value]);
  end;
end;

procedure TgxStateCache.SetFrameBuffer(const Value: GLuint);
begin
  if (Value <> FDrawFrameBuffer) or (Value <> FReadFrameBuffer) or FInsideList then
  begin
    FDrawFrameBuffer := Value;
    FReadFrameBuffer := Value;
    glBindFramebuffer(GL_FRAMEBUFFER, Value);
  end;
end;

procedure TgxStateCache.SetFrontFace(const Value: TgxFaceWinding);
begin
  if (Value <> FFrontFace) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FFrontFace := Value;
    glFrontFace(cGLFaceWindingToGLEnum[Value]);
  end;
end;

procedure TgxStateCache.SetAlphaFunction(func: TgxComparisonFunction;
  ref: Single);
begin
  if FForwardContext then
    exit;
  if (FAlphaFunc <> func) or (FAlphaRef <> ref) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FAlphaFunc := func;
      FAlphaRef := ref;
    end;
    glAlphaFunc(cGLComparisonFunctionToGLEnum[func], ref);
  end;
end;

function TgxStateCache.GetColorWriteMask(Index: Integer): TgxColorMask;
begin
  Result := FColorWriteMask[Index];
end;

function TgxStateCache.GetCurrentQuery(Index: TgxQueryType): GLuint;
begin
  Result := FCurrentQuery[Index];
end;

function TgxStateCache.GetCurrentVertexAttrib(Index: Integer): TVector4f;
begin
  Result := FCurrentVertexAttrib[Index];
end;

function TgxStateCache.GetDepthRangeFar: GLclampd;
begin
  Result := FDepthRange[1];
end;

function TgxStateCache.GetDepthRangeNear: GLclampd;
begin
  Result := FDepthRange[0];
end;

function TgxStateCache.GetEnableBlend(Index: Integer): GLboolean;
begin
  Result := FEnableBlend[Index];
end;

function TgxStateCache.GetEnableClipDistance(ClipDistance: Cardinal): GLboolean;
begin
  Result := FEnableClipDistance[ClipDistance];
end;

function TgxStateCache.GetSampleMaskValue(Index: Integer): GLbitfield;
begin
  Result := FSampleMaskValue[Index];
end;

function TgxStateCache.GetMaxTextureSize: GLuint;
begin
  if FMaxTextureSize = 0 then
    glGetIntegerv(GL_MAX_TEXTURE_SIZE, @FMaxTextureSize);
  Result := FMaxTextureSize;
end;

function TgxStateCache.GetMaterialAmbient(const aFace: TgxCullFaceMode): TVector4f;
begin
  Result := FFrontBackColors[ord(aFace)][1];
end;

function TgxStateCache.GetMaterialDiffuse(const aFace: TgxCullFaceMode): TVector4f;
begin
  Result := FFrontBackColors[ord(aFace)][2];
end;

function TgxStateCache.GetMaterialEmission(const aFace: TgxCullFaceMode): TVector4f;
begin
  Result := FFrontBackColors[ord(aFace)][0];
end;

function TgxStateCache.GetMaterialShininess(const aFace: TgxCullFaceMode): Integer;
begin
  Result := FFrontBackShininess[ord(aFace)];
end;

function TgxStateCache.GetMaterialSpecular(const aFace: TgxCullFaceMode): TVector4f;
begin
  Result := FFrontBackColors[ord(aFace)][3];
end;

function TgxStateCache.GetMax3DTextureSize: GLuint;
begin
  if FMax3DTextureSize = 0 then
    glGetIntegerv(GL_MAX_3D_TEXTURE_SIZE, @FMax3DTextureSize);
  Result := FMax3DTextureSize;
end;

function TgxStateCache.GetMaxCubeTextureSize: GLuint;
begin
  if FMaxCubeTextureSize = 0 then
    glGetIntegerv(GL_MAX_CUBE_MAP_TEXTURE_SIZE, @FMaxCubeTextureSize);
  Result := FMaxCubeTextureSize;
end;

function TgxStateCache.GetMaxArrayTextureSize: GLuint;
begin
  if FMaxArrayTextureSize = 0 then
    glGetIntegerv(GL_MAX_ARRAY_TEXTURE_LAYERS, @FMaxArrayTextureSize);
  Result := FMaxArrayTextureSize;
end;


function TgxStateCache.GetMaxTextureImageUnits: GLuint;
begin
  if FMaxTextureImageUnits = 0 then
    glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, @FMaxTextureImageUnits);
  Result := FMaxTextureImageUnits;
end;

function TgxStateCache.GetMaxTextureAnisotropy: GLuint;
begin
  if (FMaxTextureAnisotropy = 0) then
    glGetIntegerv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @FMaxTextureAnisotropy);
  Result := FMaxTextureAnisotropy;
end;

function TgxStateCache.GetMaxSamples: GLuint;
begin
  if (FMaxSamples = 0) then
    glGetIntegerv(GL_MAX_SAMPLES, @FMaxSamples);
  Result := FMaxSamples;
end;

function TgxStateCache.GetTextureBinding(Index: Integer; target: TgxTextureTarget): GLuint;
begin
  Result := FTextureBinding[Index, target];
end;

function TgxStateCache.GetTextureBindingTime(Index: Integer; target: TgxTextureTarget):
  Double;
begin
  Result := FTextureBindingTime[Index, target];
end;

function TgxStateCache.GetSamplerBinding(Index: GLuint): GLuint;
begin
  Result := FSamplerBinding[Index];
end;

procedure TgxStateCache.SetSamplerBinding(Index: GLuint; const Value: GLuint);
begin
  if Index > High(FSamplerBinding) then
    exit;
  if (Value <> FSamplerBinding[Index]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttTexture)
    else
      FSamplerBinding[Index] := Value;
    glBindSampler(Index, Value);
  end;
end;

procedure TgxStateCache.SetTextureMatrix(const matrix: TMatrix4f);
begin
  if FForwardContext then
    exit;
  if FInsideList then
    Include(FListStates[FCurrentList], sttTransform)
  else
    FTextureMatrixIsIdentity[ActiveTexture] := False;
  glMatrixMode(GL_TEXTURE);
  glLoadMatrixf(PGLFloat(@matrix.X.X));
  glMatrixMode(GL_MODELVIEW);
end;

procedure TgxStateCache.ResetTextureMatrix;
begin
  if FForwardContext then
    Exit;
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity;
  FTextureMatrixIsIdentity[ActiveTexture] := True;
  glMatrixMode(GL_MODELVIEW);
end;

procedure TgxStateCache.ResetAllTextureMatrix;
var
  I: Integer;
  lastActiveTexture: GLuint;
begin
  if FForwardContext then
    exit;
  lastActiveTexture := ActiveTexture;
  glMatrixMode(GL_TEXTURE);
  for I := High(FTextureMatrixIsIdentity) downto 0 do
    if not FTextureMatrixIsIdentity[I] then
    begin
      ActiveTexture := I;
      glLoadIdentity;
      FTextureMatrixIsIdentity[I] := True;
    end;
  glMatrixMode(GL_MODELVIEW);
  ActiveTexture := lastActiveTexture;
end;

procedure TgxStateCache.SetLineSmoothHint(const Value: TgxHintType);
begin
  if (Value <> FLineSmoothHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FLineSmoothHint := Value;
    glHint(GL_LINE_SMOOTH_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TgxStateCache.SetLineWidth(const Value: Single);
begin
  // note: wide lines no longer deprecated (see OpenGL spec)
  if (Value <> FLineWidth) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLine)
    else
      FLineWidth := Value;
    glLineWidth(Value);
  end;
end;

procedure TgxStateCache.SetLineStippleFactor(const Value: GLint);
begin
  if (Value <> FLineStippleFactor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLine)
    else
      FLineStippleFactor := Value;
    glLineStipple(Value, FLineStipplePattern);
  end;
end;

procedure TgxStateCache.SetLineStipplePattern(const Value: GLushort);
begin
  if (Value <> FLineStipplePattern) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLine)
    else
      FLineStipplePattern := Value;
    glLineStipple(FLineStippleFactor, Value);
  end;
end;

procedure TgxStateCache.SetLogicOpMode(const Value: TgxLogicOp);
begin
  if (Value <> FLogicOpMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FLogicOpMode := Value;
    glLogicOp(cGLLogicOpToGLEnum[Value]);
  end;
end;

procedure TgxStateCache.SetPackAlignment(const Value: GLuint);
begin
  if Value <> FPackAlignment then
  begin
    FPackAlignment := Value;
    glPixelStoref(GL_PACK_ALIGNMENT, Value);
  end;
end;

procedure TgxStateCache.SetPackImageHeight(const Value: GLuint);
begin
  if Value <> FPackImageHeight then
  begin
    FPackImageHeight := Value;
    glPixelStoref(GL_PACK_IMAGE_HEIGHT, Value);
  end;
end;

procedure TgxStateCache.SetPackLSBFirst(const Value: GLboolean);
begin
  if Value <> FPackLSBFirst then
  begin
    FPackLSBFirst := Value;
    glPixelStorei(GL_PACK_LSB_FIRST, byte(Value));
  end;
end;

procedure TgxStateCache.SetPackRowLength(const Value: GLuint);
begin
  if Value <> FPackRowLength then
  begin
    FPackRowLength := Value;
    glPixelStoref(GL_PACK_ROW_LENGTH, Value);
  end;
end;

procedure TgxStateCache.SetPackSkipImages(const Value: GLuint);
begin
  if Value <> FPackSkipImages then
  begin
    FPackSkipImages := Value;
    glPixelStoref(GL_PACK_SKIP_IMAGES, Value);
  end;
end;

procedure TgxStateCache.SetPackSkipPixels(const Value: GLuint);
begin
  if Value <> FPackSkipPixels then
  begin
    FPackSkipPixels := Value;
    glPixelStoref(GL_PACK_SKIP_PIXELS, Value);
  end;
end;

procedure TgxStateCache.SetPackSkipRows(const Value: GLuint);
begin
  if Value <> FPackSkipRows then
  begin
    FPackSkipRows := Value;
    glPixelStoref(GL_PACK_SKIP_ROWS, Value);
  end;
end;

procedure TgxStateCache.SetPackSwapBytes(const Value: GLboolean);
begin
  if Value <> FPackSwapBytes then
  begin
    FPackSwapBytes := Value;
    glPixelStorei(GL_PACK_SWAP_BYTES, byte(Value));
  end;
end;

procedure TgxStateCache.SetPixelPackBufferBinding(const Value: GLuint);
begin
  if Value <> FPixelPackBufferBinding then
  begin
    FPixelPackBufferBinding := Value;
    glBindBuffer(GL_PIXEL_PACK_BUFFER, Value);
  end;
end;

procedure TgxStateCache.SetPixelUnpackBufferBinding(const Value: GLuint);
begin
  if Value <> FPixelUnpackBufferBinding then
  begin
    FPixelUnpackBufferBinding := Value;
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, Value);
  end;
end;

procedure TgxStateCache.SetPointFadeThresholdSize(const Value: Single);
begin
  if (Value <> FPointFadeThresholdSize) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointFadeThresholdSize := Value;
    glPointParameterf(GL_POINT_FADE_THRESHOLD_SIZE, Value);
  end;
end;

procedure TgxStateCache.SetPointSize(const Value: Single);
begin
  if (Value <> FPointSize) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointSize := Value;
    glPointSize(Value);
  end;
end;

procedure TgxStateCache.SetPointSpriteCoordOrigin(const Value: GLEnum);
begin
  if (Value <> FPointSpriteCoordOrigin) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointSpriteCoordOrigin := Value;
    glPointParameterf(GL_POINT_SPRITE_COORD_ORIGIN, Value);
  end;
end;

procedure TgxStateCache.SetPolygonMode(const Value: TgxPolygonMode);
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
    glPolygonMode(GL_FRONT_AND_BACK, cGLPolygonModeToGLEnum[Value]);
  end;
end;

procedure TgxStateCache.SetPolygonOffset(const factor, units: Single);
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
    glPolygonOffset(factor, units);
  end;
end;

procedure TgxStateCache.SetPolygonOffsetFactor(const Value: Single);
begin
  if (Value <> FPolygonOffsetFactor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FPolygonOffsetFactor := Value;
    glPolygonOffset(Value, FPolygonOffsetUnits);
  end;
end;

procedure TgxStateCache.SetPolygonOffsetUnits(const Value: Single);
begin
  if (Value <> FPolygonOffsetUnits) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FPolygonOffsetUnits := Value;
    glPolygonOffset(FPolygonOffsetFactor, Value);
  end;
end;

procedure TgxStateCache.SetPolygonSmoothHint(const Value: TgxHintType);
begin
  if (Value <> FPolygonSmoothHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FPolygonSmoothHint := Value;
    glHint(GL_POLYGON_SMOOTH_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TgxStateCache.SetProvokingVertex(const Value: GLEnum);
begin
  if Value <> FProvokingVertex then
  begin
    FProvokingVertex := Value;
    glProvokingVertex(Value);
  end;
end;

procedure TgxStateCache.SetReadFrameBuffer(const Value: GLuint);
begin
  if Value <> FReadFrameBuffer then
  begin
    FReadFrameBuffer := Value;
    glBindFramebuffer(GL_READ_FRAMEBUFFER, Value);
  end;
end;

procedure TgxStateCache.SetRenderBuffer(const Value: GLuint);
begin
  if Value <> FRenderBuffer then
  begin
    FRenderBuffer := Value;
    glBindRenderbuffer(GL_RENDERBUFFER, Value);
  end;
end;

procedure TgxStateCache.SetSampleCoverage(const Value: Single; invert: GLboolean);
begin
  if (Value <> FSampleCoverageValue) or (invert <> FSampleCoverageInvert)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
    begin
      FSampleCoverageValue := Value;
      FSampleCoverageInvert := invert;
    end;
    glSampleCoverage(Value, invert);
  end;
end;

procedure TgxStateCache.SetSampleCoverageInvert(const Value: GLboolean);
begin
  if (Value <> FSampleCoverageInvert) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleCoverageInvert := Value;
    glSampleCoverage(FSampleCoverageValue, Value);
  end;
end;

procedure TgxStateCache.SetSampleCoverageValue(const Value: Single);
begin
  if (Value <> FSampleCoverageValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleCoverageValue := Value;
    glSampleCoverage(Value, FSampleCoverageInvert);
  end;
end;

procedure TgxStateCache.SetSampleMaskValue(Index: Integer;
  const Value: GLbitfield);
begin
  if (FSampleMaskValue[Index] <> Value) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleMaskValue[Index] := Value;
    glSampleMaski(Index, Value);
  end;
end;

procedure TgxStateCache.SetScissorBox(const Value: TVector4i);
begin
  if not VectorEquals(FScissorBox, Value) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttScissor)
    else
      FScissorBox := Value;
    glScissor(Value.X, Value.Y, Value.Z, Value.W);
  end;
end;

procedure TgxStateCache.SetStencilBackWriteMask(const Value: GLuint);
begin
  if (Value <> FStencilBackWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilBackWriteMask := Value;
  end;
end;

procedure TgxStateCache.SetStencilClearValue(const Value: GLuint);
begin
  if (Value <> FStencilClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilClearValue := Value;
    glClearStencil(Value);
  end;
end;

procedure TgxStateCache.SetColorClearValue(const Value: TVector4f);
begin
  if not VectorEquals(Value, FColorClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FColorClearValue := Value;
    glClearColor(Value.X, Value.Y, Value.Z, Value.W);
  end;
end;

procedure TgxStateCache.SetColorMask(mask: TgxColorMask);
var
  i: integer;
  Color : GLBoolean;
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
  glColorMask(Byte(ccRed in mask),
              Byte(ccGreen in mask),
              Byte(ccBlue in mask),
              Byte(ccAlpha in mask));
end;

procedure TgxStateCache.SetStencilFuncSeparate(const face: TgxCullFaceMode;
  const func: TgxStencilFunction; const ref: GLint; const mask: GLuint);

begin
//  if (func<>FStencilFunc) or (ref<>FStencilRef) or (mask<>FStencilValueMask)
//    or FInsideList then
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

    glStencilFuncSeparate(cGLCullFaceModeToGLEnum[face], cGLComparisonFunctionToGLEnum[func], ref, mask);
  end;
end;

procedure TgxStateCache.SetStencilFunc(const func: TgxStencilFunction; const ref: GLint; const mask: GLuint);
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
    glStencilFunc(cGLComparisonFunctionToGLEnum[func], ref, mask);
  end;
end;

procedure TgxStateCache.SetStencilOp(const fail, zfail, zpass: TgxStencilOp);
{$IFDEF USE_CACHE_MISS_CHECK}
var I: GLuint;
{$ENDIF}
begin
{$IFDEF USE_CACHE_MISS_CHECK}
  glGetIntegerv(GL_STENCIL_FAIL, @I);
  if cGLStencilOpToGLEnum[FStencilFail] <> I then
    ShowMessages(strStateCashMissing + 'Stencil fail');
  glGetIntegerv(GL_STENCIL_PASS_DEPTH_FAIL, @I);
  if cGLStencilOpToGLEnum[FStencilPassDepthFail] <> I then
    ShowMessages(strStateCashMissing + 'Stencil zfail');
  glGetIntegerv(GL_STENCIL_PASS_DEPTH_PASS, @I);
  if cGLStencilOpToGLEnum[FStencilPassDepthPass] <> I then
    ShowMessages(strStateCashMissing + 'Stencil zpass');
{$ENDIF}
  if (fail <> FStencilFail) or (zfail <> FStencilPassDepthFail)
    or (zpass <> FStencilPassDepthPass) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
    begin
      FStencilFail := fail;
      FStencilPassDepthFail := zfail;
      FStencilPassDepthPass := zpass;
    end;
    glStencilOp(cGLStencilOpToGLEnum[fail],
      cGLStencilOpToGLEnum[zfail],
      cGLStencilOpToGLEnum[zpass]);
  end;
end;

procedure TgxStateCache.SetStencilOpSeparate(const face: TgxCullFaceMode; const sfail, dpfail, dppass: TgxStencilOp);
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

  glStencilOpSeparate(cGLCullFaceModeToGLEnum[face],
    cGLStencilOpToGLEnum[sfail],
    cGLStencilOpToGLEnum[dpfail],
    cGLStencilOpToGLEnum[dppass]);
end;

procedure TgxStateCache.SetStencilWriteMask(const Value: GLuint);
{$IFDEF USE_CACHE_MISS_CHECK}
var I: GLuint;
{$ENDIF}
begin
{$IFDEF USE_CACHE_MISS_CHECK}
  glGetIntegerv(GL_STENCIL_WRITEMASK, @I);
  if FStencilWriteMask <> I then
    ShowMessages(strStateCashMissing + 'Stencil write mask');
{$ENDIF}
  if (Value <> FStencilWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilWriteMask := Value;
    glStencilMaskSeparate(GL_FRONT, Value);
  end;
end;

procedure TgxStateCache.SetTextureBinding(Index: Integer; target: TgxTextureTarget;
  const Value: GLuint);
var
  lastActiveTexture: GLuint;
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
    glBindTexture(cGLTexTypeToGLEnum[target], Value);
    ActiveTexture := lastActiveTexture;
  end;
  FTextureBindingTime[Index, target] := AppTime;
end;

function TgxStateCache.GetActiveTextureEnabled(Target: TgxTextureTarget):
  Boolean;
begin
  Result := FActiveTextureEnabling[FActiveTexture][Target];
end;

procedure TgxStateCache.SetActiveTextureEnabled(Target: TgxTextureTarget;
  const Value: Boolean);
var
  glTarget: GLEnum;
begin
  glTarget := DecodeTextureTarget(Target);
  if FForwardContext or not IsTargetSupported(glTarget) then
    exit;
  if (Value <> FActiveTextureEnabling[FActiveTexture][Target]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      FActiveTextureEnabling[FActiveTexture][Target] := Value;
    if Value then
      glEnable(glTarget)
    else
      glDisable(glTarget);
  end;
end;

procedure TgxStateCache.SetTextureCompressionHint(const Value: TgxHintType);
begin
  if (Value <> FTextureCompressionHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FTextureCompressionHint := Value;
    glHint(GL_TEXTURE_COMPRESSION_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TgxStateCache.SetTransformFeedbackBufferBinding(const Value: GLuint);
begin
  if (Value <> FTransformFeedbackBufferBinding) or FInsideList then
  begin
    FTransformFeedbackBufferBinding := Value;
    glBindBuffer(GL_TRANSFORM_FEEDBACK_BUFFER, Value);
  end;
end;

procedure TgxStateCache.SetEnableTextureCubeMapSeamless(const Value: GLboolean);
begin
  if Value <> FEnableTextureCubeMapSeamless then
  begin
    FEnableTextureCubeMapSeamless := Value;
    if Value = 0 then
      glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS)
    else
      glDisable(GL_TEXTURE_CUBE_MAP_SEAMLESS);
  end;
end;

procedure TgxStateCache.NewList(list: GLuint; mode: GLEnum);
var
  I: GLuint;
begin
  Assert(mode = GL_COMPILE,
    'Compile & executing not supported by TgxStateCache');
  FCurrentList := list - 1;
  while High(FListStates) < Integer(FCurrentList) do
    SetLength(FListStates, 2 * Length(FListStates));

  FListStates[FCurrentList] := [];
  FInsideList := True;
  // Reset VBO binding and client attribute
  begin
    begin
      ArrayBufferBinding := 0;
      ElementBufferBinding := 0;
      for I := 0 to 15 do
        glDisableVertexAttribArray(I);
    end;
    NewList(list, mode);
  end;
end;

procedure TgxStateCache.EndList;
begin
  glEndList;
  FInsideList := False;
end;

procedure TgxStateCache.CallList(list: GLuint);
begin
  // while High(FListStates) < Integer(list) do
  // SetLength(FListStates, 2 * Length(FListStates));

  if FListStates[list - 1] <> [] then
  begin
    PushAttrib(FListStates[list - 1]);
    glCallList(list);
    PopAttrib;
  end
  else
    glCallList(list);
end;

procedure TgxStateCache.SetUniformBufferBinding(const Value: GLuint);
begin
  Assert(not FInsideList);
  if Value <> FUniformBufferBinding then
  begin
    FUniformBufferBinding := Value;
    glBindBuffer(GL_UNIFORM_BUFFER, Value);
  end;
end;

procedure TgxStateCache.SetBufferIndexedBinding(const Value: GLuint;
  ATarget: TgxBufferBindingTarget; AIndex: GLuint; ABufferSize: PGLsizei);
begin
  Assert(not FInsideList);
  if (FUBOStates[ATarget, AIndex].FUniformBufferBinding <> Value)
    or (FUBOStates[ATarget, AIndex].FOffset^ > 0)
    or (FUBOStates[ATarget, AIndex].FSize <> ABufferSize) then
  begin
    case ATarget of
      bbtUniform: FUniformBufferBinding := Value;
      bbtTransformFeedBack: FTransformFeedbackBufferBinding := Value;
    end;
    FUBOStates[ATarget, AIndex].FUniformBufferBinding := Value;
    FUBOStates[ATarget, AIndex].FOffset := 0;
    FUBOStates[ATarget, AIndex].FSize := ABufferSize;
    glBindBufferBase(cGLBufferBindingTarget[ATarget], AIndex, Value);
  end
  else
    case ATarget of
      bbtUniform: SetUniformBufferBinding(Value);
      bbtTransformFeedBack: SetTransformFeedbackBufferBinding(Value);
    end;
end;

procedure TgxStateCache.SetBufferIndexedBinding(const Value: GLuint; ATarget: TgxBufferBindingTarget; AIndex: GLuint;
    AOffset: GLint; ARangeSize: PGLsizei);
begin
  Assert(not FInsideList);
  if (FUBOStates[ATarget, AIndex].FUniformBufferBinding <> Value)
    or (FUBOStates[ATarget, AIndex].FOffset <> @AOffset)
    or (FUBOStates[ATarget, AIndex].FSize <> ARangeSize) then
  begin
    case ATarget of
      bbtUniform: FUniformBufferBinding := Value;
      bbtTransformFeedBack: FTransformFeedbackBufferBinding := Value;
    end;
    FUBOStates[ATarget, AIndex].FUniformBufferBinding := Value;
    FUBOStates[ATarget, AIndex].FOffset := @AOffset;
    FUBOStates[ATarget, AIndex].FSize := ARangeSize;
    glBindBufferRange(cGLBufferBindingTarget[ATarget], AIndex, Value, AOffset, ARangeSize^);
  end;
end;

function TgxStateCache.GetMaxTextureUnits: GLuint;
begin
  if FMaxTextureUnits = 0 then
    glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS_ARB, @FMaxTextureUnits);
  Result := FMaxTextureUnits;
end;

procedure TgxStateCache.SetUnpackAlignment(const Value: GLuint);
begin
  if Value <> FUnpackAlignment then
  begin
    FUnpackAlignment := Value;
    glPixelStoref(GL_UNPACK_ALIGNMENT, Value);
  end;
end;

procedure TgxStateCache.SetUnpackImageHeight(const Value: GLuint);
begin
  if Value <> FUnpackImageHeight then
  begin
    FUnpackImageHeight := Value;
    glPixelStoref(GL_UNPACK_IMAGE_HEIGHT, Value);
  end;
end;

procedure TgxStateCache.SetUnpackLSBFirst(const Value: GLboolean);
begin
  if Value <> FUnpackLSBFirst then
  begin
    FUnpackLSBFirst := Value;
    glPixelStorei(GL_UNPACK_LSB_FIRST, byte(Value));
  end;
end;

procedure TgxStateCache.SetUnpackRowLength(const Value: GLuint);
begin
  if Value <> FUnpackRowLength then
  begin
    FUnpackRowLength := Value;
    glPixelStoref(GL_UNPACK_ROW_LENGTH, Value);
  end;
end;

procedure TgxStateCache.SetUnpackSkipImages(const Value: GLuint);
begin
  if Value <> FUnpackSkipImages then
  begin
    FUnpackSkipImages := Value;
    glPixelStoref(GL_UNPACK_SKIP_IMAGES, Value);
  end;
end;

procedure TgxStateCache.SetUnpackSkipPixels(const Value: GLuint);
begin
  if Value <> FUnpackSkipPixels then
  begin
    FUnpackSkipPixels := Value;
    glPixelStoref(GL_UNPACK_SKIP_PIXELS, Value);
  end;
end;

procedure TgxStateCache.SetUnpackSkipRows(const Value: GLuint);
begin
  if Value <> FUnpackSkipRows then
  begin
    FUnpackSkipRows := Value;
    glPixelStoref(GL_UNPACK_SKIP_ROWS, Value);
  end;
end;

procedure TgxStateCache.SetUnpackSwapBytes(const Value: GLboolean);
begin
  if Value <> FUnpackSwapBytes then
  begin
    FUnpackSwapBytes := Value;
    glPixelStorei(GL_UNPACK_SWAP_BYTES, byte(Value));
  end;
end;

procedure TgxStateCache.SetViewPort(const Value: TVector4i);
begin
  if not VectorEquals(Value, FViewPort) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FViewPort := Value;
    glViewport(Value.X, Value.Y, Value.Z, Value.W);
  end;
end;

procedure TgxStateCache.SetFFPLight(Value: Boolean);
begin
  FFFPLight := Value { and not FForwardContext};
end;

function TgxStateCache.GetMaxLights: Integer;
begin
  if FMaxLights = 0 then
 (* if FForwardContext then
    FMaxLights := MAX_HARDWARE_LIGHT
  else *)
    glGetIntegerv(GL_MAX_LIGHTS, @FMaxLights);
  Result := FMaxLights;
end;

function TgxStateCache.GetLightEnabling(I: Integer): Boolean;
begin
  Result := FLightEnabling[I];
end;

procedure TgxStateCache.SetLightEnabling(I: Integer; Value: Boolean);
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
        glEnable(GL_LIGHT0 + I)
      else
        glDisable(GL_LIGHT0 + I);
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

function TgxStateCache.GetLightIndicesAsAddress: PGLInt;
begin
  Result := @FLightIndices[0];
end;

function TgxStateCache.GetLightStateAsAddress: Pointer;
var
  I, J, C: Integer;
begin
  C := MinInteger(FLightNumber, MAX_SHADER_LIGHT);
  if FShaderLightStatesChanged then
  begin
    if C > 0 then
    begin
      if (GL_VERSION >= 3.0) then
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

function TgxStateCache.GetLightPosition(I: Integer): TVector4f;
begin
  Result := FLightStates.Position[I];
end;

procedure TgxStateCache.SetLightPosition(I: Integer; const Value: TVector4f);
begin
  if not VectorEquals(Value, FLightStates.Position[I]) then
  begin
    FLightStates.Position[I] := Value;
    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TgxStateCache.GetLightSpotDirection(I: Integer): TAffineVector;
begin
  Result := AffineVectorMake(FLightStates.SpotDirection[I]);
end;

procedure TgxStateCache.SetLightSpotDirection(I: Integer; const Value: TAffineVector);
begin
  if not VectorEquals(Value, AffineVectorMake(FLightStates.SpotDirection[I])) then
  begin
    FLightStates.SpotDirection[I] := VectorMake(Value);
    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TgxStateCache.GetLightAmbient(I: Integer): TVector4f;
begin
  Result := FLightStates.Ambient[I];
end;

procedure TgxStateCache.SetLightAmbient(I: Integer; const Value: TVector4f);
begin
  if not VectorEquals(Value, FLightStates.Ambient[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Ambient[I] := Value;

    if FFFPLight then
      glLightfv(GL_LIGHT0 + I, GL_AMBIENT, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TgxStateCache.GetLightDiffuse(I: Integer): TVector4f;
begin
  Result := FLightStates.Diffuse[I];
end;

procedure TgxStateCache.SetLightDiffuse(I: Integer; const Value: TVector4f);
begin
  if not VectorEquals(Value, FLightStates.Diffuse[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Diffuse[I] := Value;

    if FFFPLight then
      glLightfv(GL_LIGHT0 + I, GL_DIFFUSE, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TgxStateCache.GetLightSpecular(I: Integer): TVector4f;
begin
  Result := FLightStates.Specular[I];
end;

procedure TgxStateCache.SetLightSpecular(I: Integer; const Value: TVector4f);
begin
  if not VectorEquals(Value, FLightStates.Specular[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Specular[I] := Value;

    if FFFPLight then
      glLightfv(GL_LIGHT0 + I, GL_SPECULAR, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TgxStateCache.GetSpotCutoff(I: Integer): Single;
begin
  Result := FSpotCutoff[I];
end;

procedure TgxStateCache.SetSpotCutoff(I: Integer; const Value: Single);
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
      glLightfv(GL_LIGHT0 + I, GL_SPOT_CUTOFF, @Value);
                  
    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TgxStateCache.GetSpotExponent(I: Integer): Single;
begin
  Result := FLightStates.SpotCosCutoffExponent[I].Y;
end;

procedure TgxStateCache.SetSpotExponent(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.SpotCosCutoffExponent[I].Y ) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.SpotCosCutoffExponent[I].Y  := Value;

    if FFFPLight then
      glLightfv(GL_LIGHT0 + I, GL_SPOT_EXPONENT, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TgxStateCache.GetConstantAtten(I: Integer): Single;
begin
  Result := FLightStates.Attenuation[I].X ;
end;

procedure TgxStateCache.SetConstantAtten(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.Attenuation[I].X ) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Attenuation[I].X  := Value;

    if FFFPLight then
      glLightfv(GL_LIGHT0 + I, GL_CONSTANT_ATTENUATION, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TgxStateCache.GetLinearAtten(I: Integer): Single;
begin
  Result := FLightStates.Attenuation[I].Y ;
end;

procedure TgxStateCache.SetLinearAtten(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.Attenuation[I].Y ) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Attenuation[I].Y  := Value;

    if FFFPLight then
      glLightfv(GL_LIGHT0 + I, GL_LINEAR_ATTENUATION, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TgxStateCache.GetQuadAtten(I: Integer): Single;
begin
  Result := FLightStates.Attenuation[I].Z ;
end;

procedure TgxStateCache.SetQuadAtten(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.Attenuation[I].Z ) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Attenuation[I].Z  := Value;

    if FFFPLight then
      glLightfv(GL_LIGHT0 + I, GL_QUADRATIC_ATTENUATION, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

procedure TgxStateCache.SetForwardContext(Value: Boolean);
begin
  if Value <> FForwardContext then
  begin
    FForwardContext := Value;
    if Value then
    begin
      SetFFPlight(False);
    end;
  end;
end;


procedure TgxStateCache.SetColorWriting(flag: Boolean);
begin
  if (FColorWriting <> flag) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FColorWriting := flag;
    glColorMask(GLboolean(flag), GLboolean(flag), GLboolean(flag), GLboolean(flag));
  end;
end;

procedure TgxStateCache.InvertFrontFace;
begin
  if FFrontFace = fwCounterClockWise then
    FrontFace := fwClockWise
  else
    FrontFace := fwCounterClockWise;
end;

procedure TgxStateCache.SetVxState(const aState : TgxState);
begin
	Enable(aState);
end;

procedure TgxStateCache.UnSetVxState(const aState : TgxState);
begin
	Disable(aState);
end;

procedure TgxStateCache.ResetPolygonMode;
begin
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  FPolygonMode := pmFill;
  FPolygonBackMode := pmFill;
end;

procedure TgxStateCache.ResetMaterialColors;
begin
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @clrGray20);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @clrGray80);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @clrBlack);
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, @clrBlack);
  glMateriali(GL_FRONT_AND_BACK, GL_SHININESS, 0);
  FillChar(FFrontBackColors, SizeOf(FFrontBackColors), 127);
  FFrontBackShininess[0] := 0;
  FFrontBackShininess[1] := 0;
end;

procedure TgxStateCache.ResetTexture(const TextureUnit: Integer);
var
  t: TgxTextureTarget;
  glTarget: GLEnum;
begin
  glActiveTexture(GL_TEXTURE0 + TextureUnit);
  for t := Low(TgxTextureTarget) to High(TgxTextureTarget) do
  begin
    glTarget := DecodeTextureTarget(t);
    if IsTargetSupported(glTarget) then
    begin
      glBindTexture(glTarget, 0);
      FTextureBinding[TextureUnit, t] := 0;
    end;
  end;
  glActiveTexture(GL_TEXTURE0);
  FActiveTexture := 0;
end;

procedure TgxStateCache.ResetCurrentTexture;
var
  a: GLint;
  t: TgxTextureTarget;
  glTarget: GLEnum;
begin
  for a := MaxTextureImageUnits - 1 to 0 do
  begin
    glActiveTexture(GL_TEXTURE0 + a);
    for t := Low(TgxTextureTarget) to High(TgxTextureTarget) do
    begin
      glTarget := DecodeTextureTarget(t);
      if IsTargetSupported(glTarget) then
      begin
        glBindTexture(glTarget, 0);
        FTextureBinding[a, t] := 0;
      end;
    end;
  end;
end;

procedure TgxStateCache.ResetFrontFace;
begin
  glFrontFace(GL_CCW);
  FFrontFace := fwCounterClockWise;
end;


procedure TgxStateCache.SetGLFrontFaceCW;
begin
  if FFrontFace = fwCounterClockWise then
  begin
    glFrontFace(GL_CW);
    FFrontFace := fwClockWise;
  end;
end;

procedure TgxStateCache.ResetAll;
begin
  ResetPolygonMode;
  ResetMaterialColors;
  ResetCurrentTexture;
  ResetFrontFace;
end;

end.

