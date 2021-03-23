//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.MaterialEx;

(*
  Handles extended material and it components:
  textures, samplers, combiners, shaders and etc.

  Features:
   - material can contain different level of applying accordingly to hardware i.e.
     Feateres scaling.
   - if automatically or by user selected level failed, material down to lower level.
   - direct state access can be used for uniforms setting.
   - economy mode for texture binding to active units,
     i.e. if textures less than maximum units may be not one binding occur per frame.
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Math,
  Vcl.Graphics,

  GLS.OpenGLTokens,
  GLS.RenderContextInfo,
  GLS.PipelineTransformation,
  GLS.BaseClasses,
  GLS.Context,
  GLS.VectorTypes,
  GLS.Material,
  GLS.Texture,
  GLS.Color,
  GLS.Coordinates,
  GLS.VectorGeometry,
  GLS.Graphics,
  GLS.PersistentClasses,
  GLS.State,
  GLS.TextureFormat,
  GLS.XCollection,
  GLS.TextureCombiners,
  GLSL.ShaderParameter,
  GLS.ApplicationFileIO,
  GLS.Strings,
  GLS.ImageUtils,
  GLS.Utils,
  GLS.XOpenGL,
  GLS.Logger;

type

  TGLMaterialComponentName = string;
  TGLMaterialLibraryEx = class;
  TGLMatLibComponents = class;
  TGLLibMaterialEx = class;
  TGLBaseShaderModel = class;
  TGLASMVertexProgram = class;

  TOnAsmProgSetting = procedure(Sender: TGLASMVertexProgram;
    var ARci: TGLRenderContextInfo) of object;
  TOnUniformInitialize = procedure(Sender: TGLBaseShaderModel) of object;
  TOnUniformSetting = procedure(Sender: TGLBaseShaderModel;
    var ARci: TGLRenderContextInfo) of object;

  TGLBaseMaterialCollectionItem = class(
      TXCollectionItem,
      IGLMaterialLibrarySupported)
  private
    FNameHashKey: Integer;
    FUserList: TPersistentObjectList;
    FDefferedInit: Boolean;
    FNotifying: Boolean;
    FIsValid: Boolean;
    function GetUserList: TPersistentObjectList;
    function GetMaterialLibraryEx: TGLMaterialLibraryEx;
  protected
    procedure SetName(const AValue: TGLMaterialComponentName); override;
    procedure NotifyChange(Sender: TObject); virtual;
    property UserList: TPersistentObjectList read GetUserList;
    procedure DoOnPrepare(Sender: TGLContext); virtual; abstract;
  public
    destructor Destroy; override;
    procedure RegisterUser(AUser: TGLUpdateAbleObject);
    procedure UnregisterUser(AUser: TGLUpdateAbleObject);
    function GetUserCount: Integer;
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
    property MaterialLibrary: TGLMaterialLibraryEx read GetMaterialLibraryEx;
    property IsValid: Boolean read FIsValid;
  published
    property Name: TGLMaterialComponentName read GetName write SetName;
    (* Run-time flag, indicate that resource
       should initialize in case of failure material's level. *)
    property DefferedInit: Boolean read FDefferedInit write FDefferedInit
      default False;
  end;

  CGLBaseMaterialCollectionItem = class of TGLBaseMaterialCollectionItem;

  TGLLibMaterialProperty = class(TGLUpdateAbleObject, IGLMaterialLibrarySupported)
  protected
    FEnabled: Boolean;
    FNextPassName: TGLLibMaterialName;
    function GetMaterial: TGLLibMaterialEx;
    function GetMaterialLibraryEx: TGLMaterialLibraryEx;
    procedure SetEnabled(AValue: Boolean); virtual;
    procedure SetNextPass(const AValue: TGLLibMaterialName);
    procedure Loaded; virtual;
    property NextPass: TGLLibMaterialName read FNextPassName write SetNextPass;
  public
    procedure NotifyChange(Sender: TObject); override;
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
    property MaterialLibrary: TGLMaterialLibraryEx read GetMaterialLibraryEx;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  TGLTextureSampler = class(TGLBaseMaterialCollectionItem)
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    FHandle: TGLSamplerHandle;
    FMinFilter: TGLMinFilter;
    FMagFilter: TGLMagFilter;
    FFilteringQuality: TGLTextureFilteringQuality;
    FLODBias: Integer;
    FLODBiasFract: Single;
    FWrap: array[0..2] of TGLSeparateTextureWrap;
    FBorderColor: TGLColor;
    FCompareMode: TGLTextureCompareMode;
    FCompareFunc: TGLDepthFunction;
    FDecodeSRGB: Boolean;
    procedure SetMagFilter(AValue: TGLMagFilter);
    procedure SetMinFilter(AValue: TGLMinFilter);
    procedure SetLODBias(AValue: Integer);
    procedure SetFilteringQuality(AValue: TGLTextureFilteringQuality);
    function GetWrap(Index: Integer): TGLSeparateTextureWrap;
    procedure SetWrap(Index: Integer; AValue: TGLSeparateTextureWrap);
    procedure SetBorderColor(const AValue: TGLColor);
    procedure SetCompareMode(AValue: TGLTextureCompareMode);
    procedure SetCompareFunc(AValue: TGLDepthFunction);
    procedure SetDecodeSRGB(AValue: Boolean);
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure DoOnPrepare(Sender: TGLContext); override;
    procedure Apply(var ARci: TGLRenderContextInfo);
    procedure UnApply(var ARci: TGLRenderContextInfo);
    class function FriendlyName: string; override;
    property Handle: TGLSamplerHandle read FHandle;
  published
    // Texture magnification filter.
    property MagFilter: TGLMagFilter read FMagFilter write SetMagFilter
      default maLinear;
    // Texture minification filter.
    property MinFilter: TGLMinFilter read FMinFilter write SetMinFilter
      default miLinearMipMapLinear;
    property FilteringQuality: TGLTextureFilteringQuality read FFilteringQuality
      write SetFilteringQuality default tfAnisotropic;
    // Texture LOD bias.
    property LodBias: Integer read FLODBias write SetLODBias default 0;
    // Address mode for the texture.
    property WrapX: TGLSeparateTextureWrap index 0 read GetWrap write SetWrap
      default twRepeat;
    property WrapY: TGLSeparateTextureWrap index 1 read GetWrap write SetWrap
      default twRepeat;
    property WrapZ: TGLSeparateTextureWrap index 2 read GetWrap write SetWrap
      default twRepeat;
    // Texture border color.
    property BorderColor: TGLColor read FBorderColor
      write SetBorderColor;
    // Compare mode and function for depth texture
    property CompareMode: TGLTextureCompareMode read FCompareMode
      write SetCompareMode default tcmNone;
    property CompareFunc: TGLDepthFunction read FCompareFunc
      write SetCompareFunc default cfLEqual;
    (* Force retrieving the undecoded sRGB data from the
       texture and manipulate that directly. *)
    property sRGB_Encode: Boolean read FDecodeSRGB write SetDecodeSRGB
      default True;
  end;

  TGLAbstractTexture = class(TGLBaseMaterialCollectionItem)
  protected
    FHandle: TGLTextureHandle;
    FInternalFormat: TGLInternalFormat;
    FWidth: Integer;
    FHeight: Integer;
    FDepth: Integer;
    FSwizzles: TSwizzleVector;
    FApplicableSampler: TGLTextureSampler;
    FLastSampler: TGLTextureSampler;
    function GetTextureTarget: TGLTextureTarget;
    procedure Apply(var ARci: TGLRenderContextInfo); virtual; abstract;
    procedure UnApply(var ARci: TGLRenderContextInfo); virtual; abstract;
  public
    property Handle: TGLTextureHandle read FHandle;
  published
    property Shape: TGLTextureTarget read GetTextureTarget;
  end;

  TMipmapGenerationMode =
    (
    mgmNoMip,
    mgmLeaveExisting,
    mgmOnFly,
    mgmBoxFilter,
    mgmTriangleFilter,
    mgmHermiteFilter,
    mgmBellFilter,
    mgmSplineFilter,
    mgmLanczos3Filter,
    mgmMitchellFilter
    );

  TGLTextureImageEx = class(TGLAbstractTexture)
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    FCompression: TGLTextureCompression;
    FImage: TGLBaseImage;
    FImageAlpha: TGLTextureImageAlpha;
    FImageBrightness: Single;
    FImageGamma: Single;
    FHeightToNormalScale: Single;
    FSourceFile: string;
    FApplyCounter: Integer;
    FInternallyStored: Boolean;
    FMipGenMode: TMipmapGenerationMode;
    FUseStreaming: Boolean;
    FBaseLevel: Integer;
    FMaxLevel: Integer;
    FLastTime: Double;
    procedure SetInternalFormat(const AValue: TGLInternalFormat);
    procedure SetImageAlpha(const AValue: TGLTextureImageAlpha);
    procedure SetImageBrightness(const AValue: Single);
    function StoreBrightness: Boolean;
    procedure SetImageGamma(const AValue: Single);
    function StoreGamma: Boolean;
    procedure SetNormalMapScale(const AValue: Single);
    function StoreNormalMapScale: Boolean;
    procedure SetCompression(const AValue: TGLTextureCompression);
    procedure SetSourceFile(AValue: string);
    procedure SetInternallyStored(const AValue: Boolean);
    procedure SetMipGenMode(const AValue: TMipmapGenerationMode);
    procedure SetUseStreaming(const AValue: Boolean);
    procedure PrepareImage;
    procedure FullTransfer;
    procedure StreamTransfer;
    procedure CalcLODRange(out AFirstLOD, ALastLOD: Integer);
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure DoOnPrepare(Sender: TGLContext); override;
    procedure Apply(var ARci: TGLRenderContextInfo); override;
    procedure UnApply(var ARci: TGLRenderContextInfo); override;
    class function FriendlyName: string; override;
  published
    // Factual texture properties
    property InternalWidth: Integer read FWidth;
    property InternalHeight: Integer read FHeight;
    property InternalDepth: Integer read FDepth;
    property InternalFormat: TGLInternalFormat read FInternalFormat
      write SetInternalFormat default tfRGBA8;
    (* Automatic Image Alpha setting.
      Allows to control how and if the image's Alpha channel (transparency)
      is computed. *)
    property ImageAlpha: TGLTextureImageAlpha read FImageAlpha write
      SetImageAlpha default tiaDefault;
    (* Texture brightness correction.
      This correction is applied upon loading a TGLTextureImage, it's a
      simple saturating scaling applied to the RGB components of
      the 32 bits image, before it is passed to OpenGL, and before
      gamma correction (if any). *)
    property ImageBrightness: Single read FImageBrightness write
      SetImageBrightness stored StoreBrightness;
      (*Texture gamma correction.
      The gamma correction is applied upon loading a TGLTextureImage,
      applied to the RGB components of the 32 bits image, before it is
      passed to OpenGL, after brightness correction (if any). *)
    property ImageGamma: Single read FImageGamma write SetImageGamma stored
      StoreGamma;
    (* Texture compression control.
      If True the compressed TextureFormat variant (the OpenGL ICD must
      support GL_ARB_texture_compression, or this option is ignored). *)
    property Compression: TGLTextureCompression read FCompression write
      SetCompression default tcDefault;
    (* Normal Map scaling.
      Force normal map generation from height map and controls
      the intensity of the bumps. *)
    property HeightToNormalScale: Single read FHeightToNormalScale
      write SetNormalMapScale stored StoreNormalMapScale;
    // Source file path and name.
    property SourceFile: string read FSourceFile write SetSourceFile;
    // Force to store image levels in separate files in ready to transfer format
    property InternallyStored: Boolean read FInternallyStored
      write SetInternallyStored default False;
    // Mipmap generation mode.
    property MipGenMode: TMipmapGenerationMode read FMipGenMode
      write SetMipGenMode default mgmOnFly;
    // Enable streaming loading.
    property UseStreaming: Boolean read FUseStreaming
      write SetUseStreaming default False;
  end;

  TGLFrameBufferAttachment = class(TGLAbstractTexture)
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    FRenderBufferHandle: TGLRenderbufferHandle;
    FLayered: Boolean;
    FCubeMap: Boolean;
    FSamples: Integer;
    FOnlyWrite: Boolean;
    FFixedSamplesLocation: Boolean;
    procedure SetWidth(AValue: Integer);
    procedure SetHeight(AValue: Integer);
    procedure SetDepth(AValue: Integer);
    procedure SetInternalFormat(const AValue: TGLInternalFormat);
    procedure SetOnlyWrite(AValue: Boolean);
    procedure SetLayered(AValue: Boolean);
    procedure SetCubeMap(AValue: Boolean);
    procedure SetSamples(AValue: Integer);
    procedure SetFixedSamplesLocation(AValue: Boolean);
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure DoOnPrepare(Sender: TGLContext); override;
    procedure Apply(var ARci: TGLRenderContextInfo); override;
    procedure UnApply(var ARci: TGLRenderContextInfo); override;
    class function FriendlyName: string; override;
  published
    property InternalWidth: Integer read FWidth
      write SetWidth default 256;
    property InternalHeight: Integer read FHeight
      write SetHeight default 256;
    property InternalDepth: Integer read FDepth
      write SetDepth default 0;
    property InternalFormat: TGLInternalFormat read FInternalFormat
      write SetInternalFormat default tfRGBA8;
    (* This flag makes use render buffer as target which makes
       it impossible to read it as texture, but improves efficiency. *)
    property OnlyWrite: Boolean read FOnlyWrite
      write SetOnlyWrite default False;
    // Force targe be texture array.
    property Layered: Boolean read FLayered
      write SetLayered default False;
    // Force target be cube map.
    property CubeMap: Boolean read FCubeMap
      write SetCubeMap default False;
    // Number of samples. Positive value makes texture be multisample.
    property Samples: Integer read FSamples
      write SetSamples default -1;
    (* FixedSamplesLocation flag makes image will use identical
      sample locations and the same number of samples for all texels in
      the image, and the sample locations will not depend on the
      internalformat or size of the image. *)
    property FixedSamplesLocation: Boolean read FFixedSamplesLocation
      write SetFixedSamplesLocation default False;
  end;

  (* Swizzle the components of a texture fetches in
        shader or fixed-function pipeline. *)
  TGLTextureSwizzling = class(TGLUpdateAbleObject)
  private
    FSwizzles: TSwizzleVector;
    function GetSwizzle(AIndex: Integer): TGLTextureSwizzle;
    procedure SetSwizzle(AIndex: Integer; AValue: TGLTextureSwizzle);
    function StoreSwizzle(AIndex: Integer): Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(AWriter: TWriter);
    procedure ReadFromFiler(AReader: TReader);
  published
    property RedFrom: TGLTextureSwizzle index 0 read GetSwizzle
      write SetSwizzle stored StoreSwizzle;
    property GreenFrom: TGLTextureSwizzle index 1 read GetSwizzle
      write SetSwizzle stored StoreSwizzle;
    property BlueFrom: TGLTextureSwizzle index 2 read GetSwizzle
      write SetSwizzle stored StoreSwizzle;
    property AlphaFrom: TGLTextureSwizzle index 3 read GetSwizzle
      write SetSwizzle stored StoreSwizzle;
  end;

  TGLTextureProperties = class(TGLLibMaterialProperty)
  private
    FLibTextureName: TGLMaterialComponentName;
    FLibSamplerName: TGLMaterialComponentName;
    FLibTexture: TGLAbstractTexture;
    FLibSampler: TGLTextureSampler;
    FTextureOffset, FTextureScale: TGLCoordinates;
    FTextureRotate: Single;
    FTextureMatrixIsIdentity: Boolean;
    FTextureOverride: Boolean;
    FTextureMatrix: TGLMatrix;
    FMappingMode: TGLTextureMappingMode;
    FEnvColor: TGLColor;
    FMapSCoordinates: TGLCoordinates4;
    FMapTCoordinates: TGLCoordinates4;
    FMapRCoordinates: TGLCoordinates4;
    FMapQCoordinates: TGLCoordinates4;
    FSwizzling: TGLTextureSwizzling;
    function GetLibTextureName: TGLMaterialComponentName;
    function GetLibSamplerName: TGLMaterialComponentName;
    procedure SetLibTextureName(const AValue: TGLMaterialComponentName);
    procedure SetLibSamplerName(const AValue: TGLMaterialComponentName);
    function GetTextureOffset: TGLCoordinates;
    procedure SetTextureOffset(const AValue: TGLCoordinates);
    function StoreTextureOffset: Boolean;
    function GetTextureScale: TGLCoordinates;
    procedure SetTextureScale(const AValue: TGLCoordinates);
    function StoreTextureScale: Boolean;
    procedure SetTextureMatrix(const AValue: TGLMatrix);
    procedure SetTextureRotate(AValue: Single);
    function StoreTextureRotate: Boolean;
    procedure SetMappingMode(const AValue: TGLTextureMappingMode);
    function GetMappingSCoordinates: TGLCoordinates4;
    procedure SetMappingSCoordinates(const AValue: TGLCoordinates4);
    function StoreMappingSCoordinates: Boolean;
    function GetMappingTCoordinates: TGLCoordinates4;
    procedure SetMappingTCoordinates(const AValue: TGLCoordinates4);
    function StoreMappingTCoordinates: Boolean;
    function GetMappingRCoordinates: TGLCoordinates4;
    procedure SetMappingRCoordinates(const AValue: TGLCoordinates4);
    function StoreMappingRCoordinates: Boolean;
    function GetMappingQCoordinates: TGLCoordinates4;
    procedure SetMappingQCoordinates(const AValue: TGLCoordinates4);
    function StoreMappingQCoordinates: Boolean;
    procedure SetSwizzling(const AValue: TGLTextureSwizzling);
    function StoreSwizzling: Boolean;
    procedure SetEnvColor(const AValue: TGLColor);
    procedure CalculateTextureMatrix;
    procedure ApplyMappingMode;
    procedure UnApplyMappingMode;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure Notification(Sender: TObject; Operation: TOperation); override;
    function IsValid: Boolean;
    procedure Apply(var ARci: TGLRenderContextInfo);
    procedure UnApply(var ARci: TGLRenderContextInfo);
    property TextureMatrix: TGLMatrix read FTextureMatrix write SetTextureMatrix;
  published
    property LibTextureName: TGLMaterialComponentName read GetLibTextureName
      write SetLibTextureName;
    property LibSamplerName: TGLMaterialComponentName read GetLibSamplerName
      write SetLibSamplerName;
    property TextureOffset: TGLCoordinates read GetTextureOffset write
      SetTextureOffset stored StoreTextureOffset;
    (* Texture coordinates scaling.
       Scaling is applied before applying the offset, and is applied
       to the texture coordinates, meaning that a scale factor of (2, 2, 2)
       will make your texture look twice smaller. *)
    property TextureScale: TGLCoordinates read GetTextureScale write
      SetTextureScale stored StoreTextureScale;
    (* Texture coordinates rotating.
       Rotating is applied after applying offset and scale,
       and rotate ST direction around R axis. *)
    property TextureRotate: Single read FTextureRotate write
      SetTextureRotate stored StoreTextureRotate;
    // Texture Environment color.
    property EnvColor: TGLColor read FEnvColor write SetEnvColor;
    (* Texture coordinates mapping mode.
    This property controls automatic texture coordinates generation. *)
    property MappingMode: TGLTextureMappingMode read FMappingMode write
      SetMappingMode default tmmUser;
    (* Texture mapping coordinates mode for S, T, R and Q axis.
    This property stores the coordinates for automatic texture
    coordinates generation. *)
    property MappingSCoordinates: TGLCoordinates4 read GetMappingSCoordinates
      write SetMappingSCoordinates stored StoreMappingSCoordinates;
    property MappingTCoordinates: TGLCoordinates4 read GetMappingTCoordinates
      write SetMappingTCoordinates stored StoreMappingTCoordinates;
    property MappingRCoordinates: TGLCoordinates4 read GetMappingRCoordinates
      write SetMappingRCoordinates stored StoreMappingRCoordinates;
    property MappingQCoordinates: TGLCoordinates4 read GetMappingQCoordinates
      write SetMappingQCoordinates stored StoreMappingQCoordinates;
    // Texture color fetching parameters.
    property Swizzling: TGLTextureSwizzling read FSwizzling write
      SetSwizzling stored StoreSwizzling;
  end;

  TGLFixedFunctionProperties = class(TGLLibMaterialProperty)
  private
    FFrontProperties: TGLFaceProperties;
    FBackProperties: TGLFaceProperties;
    FDepthProperties: TGLDepthProperties;
    FBlendingMode: TGLBlendingMode;
    FBlendingParams: TGLBlendingParameters;
    FTexProp: TGLTextureProperties;
    FMaterialOptions: TGLMaterialOptions;
    FFaceCulling: TGLFaceCulling;
    FPolygonMode: TGLPolygonMode;
    FTextureMode: TGLTextureMode;
    function GetBackProperties: TGLFaceProperties;
    procedure SetBackProperties(AValues: TGLFaceProperties);
    procedure SetFrontProperties(AValues: TGLFaceProperties);
    procedure SetDepthProperties(AValues: TGLDepthProperties);
    procedure SetBlendingMode(const AValue: TGLBlendingMode);
    procedure SetMaterialOptions(const AValue: TGLMaterialOptions);
    procedure SetFaceCulling(const AValue: TGLFaceCulling);
    procedure SetPolygonMode(AValue: TGLPolygonMode);
    procedure SetBlendingParams(const AValue: TGLBlendingParameters);
    procedure SetTexProp(AValue: TGLTextureProperties);
    procedure SetTextureMode(AValue: TGLTextureMode);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Apply(var ARci: TGLRenderContextInfo);
    procedure UnApply(var ARci: TGLRenderContextInfo);
    // Returns True if the material is blended.
    function Blended: Boolean;
  published
    property MaterialOptions: TGLMaterialOptions read FMaterialOptions write
      SetMaterialOptions default [];
    property BackProperties: TGLFaceProperties read GetBackProperties write
      SetBackProperties;
    property FrontProperties: TGLFaceProperties read FFrontProperties write
      SetFrontProperties;
    property DepthProperties: TGLDepthProperties read FDepthProperties write
      SetDepthProperties;
    property BlendingMode: TGLBlendingMode read FBlendingMode write SetBlendingMode
      default bmOpaque;
    property BlendingParams: TGLBlendingParameters read FBlendingParams write
      SetBlendingParams;
    property FaceCulling: TGLFaceCulling read FFaceCulling write SetFaceCulling
      default fcBufferDefault;
    property PolygonMode: TGLPolygonMode read FPolygonMode write SetPolygonMode
      default pmFill;
    property Texture: TGLTextureProperties read FTexProp write SetTexProp;
    // Texture application mode.
    property TextureMode: TGLTextureMode read FTextureMode write SetTextureMode
      default tmDecal;
    // Next pass of FFP.
    property NextPass;
  end;

  TGLTextureCombiner = class(TGLBaseMaterialCollectionItem)
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    FHandle: TGLVirtualHandle;
    FScript: TStringList;
    FCommandCache: TCombinerCache;
    procedure SetScript(AValue: TStringList);
    procedure DoAllocate(Sender: TGLVirtualHandle; var handle: Cardinal);
    procedure DoDeallocate(Sender: TGLVirtualHandle; var handle: Cardinal);
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure DoOnPrepare(Sender: TGLContext); override;
    class function FriendlyName: string; override;
  published
    property Script: TStringList read FScript write SetScript;
  end;

  TGLASMVertexProgram = class(TGLBaseMaterialCollectionItem)
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    FHandle: TGLARBVertexProgramHandle;
    FSource: TStringList;
    FSourceFile: string;
    FInfoLog: string;
    procedure SetSource(AValue: TStringList);
    procedure SetSourceFile(AValue: string);
    function GetHandle: TGLARBVertexProgramHandle;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoOnPrepare(Sender: TGLContext); override;
    class function FriendlyName: string; override;
    procedure NotifyChange(Sender: TObject); override;
    property Handle: TGLARBVertexProgramHandle read GetHandle;
  published
    property Source: TStringList read FSource write SetSource;
    property SourceFile: string read FSourceFile write SetSourceFile;
    property InfoLog: string read FInfoLog;
  end;

  TLightDir2TexEnvColor = (
    l2eNone,
    l2eEnvColor0,
    l2eEnvColor1,
    l2eEnvColor2,
    l2eEnvColor3
    );

  TGLMultitexturingProperties = class(TGLLibMaterialProperty)
  private
    FLibCombiner: TGLTextureCombiner;
    FLibAsmProg: TGLASMVertexProgram;
    FLibCombinerName: TGLMaterialComponentName;
    FLibAsmProgName: TGLMaterialComponentName;
    FTexProps: array[0..3] of TGLTextureProperties;
    FTextureMode: TGLTextureMode;
    FLightDir: TLightDir2TexEnvColor;
    FLightSourceIndex: Integer;
    function GetLibCombinerName: string;
    function GetLibAsmProgName: string;
    procedure SetLibCombinerName(const AValue: string);
    procedure SetLibAsmProgName(const AValue: string);
    function GetTexProps(AIndex: Integer): TGLTextureProperties;
    procedure SetTexProps(AIndex: Integer; AValue: TGLTextureProperties);
    procedure SetTextureMode(AValue: TGLTextureMode);
    procedure SetLightSourceIndex(AValue: Integer);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Notification(Sender: TObject; Operation: TOperation); override;
    function IsValid: Boolean;
    procedure Apply(var ARci: TGLRenderContextInfo);
    procedure UnApply(var ARci: TGLRenderContextInfo);
  published
    property LibCombinerName: string read GetLibCombinerName
      write SetLibCombinerName;
    property LibAsmProgName: string read GetLibAsmProgName
      write SetLibAsmProgName;
    property Texture0: TGLTextureProperties index 0 read GetTexProps write
      SetTexProps;
    property Texture1: TGLTextureProperties index 1 read GetTexProps write
      SetTexProps;
    property Texture2: TGLTextureProperties index 2 read GetTexProps write
      SetTexProps;
    property Texture3: TGLTextureProperties index 3 read GetTexProps write
      SetTexProps;
    // Texture application mode.
    property TextureMode: TGLTextureMode read FTextureMode write SetTextureMode
      default tmDecal;
    (* Pass light source direction to enviroment color of choosen texture.
       Vector in model space. *)
    property LightDirTo: TLightDir2TexEnvColor read FLightDir
      write FLightDir default l2eNone;
    // Specify index of light source for LightDirTo.
    property LightSourceIndex: Integer read FLightSourceIndex
      write SetLightSourceIndex default 0;
    // Next pass of combiner.
    property NextPass;
  end;

  TGLShaderType =
    (
    shtVertex,
    shtControl,
    shtEvaluation,
    shtGeometry,
    shtFragment
    );

  TGLShaderEx = class(TGLBaseMaterialCollectionItem)
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    FHandle: array[TGLShaderType] of TGLShaderHandle;
    FSource: TStringList;
    FSourceFile: string;
    FShaderType: TGLShaderType;
    FInfoLog: string;
    FGeometryInput: TGLgsInTypes;
    FGeometryOutput: TGLgsOutTypes;
    FGeometryVerticesOut: Integer;
    procedure SetSource(AValue: TStringList);
    procedure SetSourceFile(AValue: string);
    procedure SetShaderType(AValue: TGLShaderType);
    procedure SetGeometryInput(AValue: TGLgsInTypes);
    procedure SetGeometryOutput(AValue: TGLgsOutTypes);
    procedure SetGeometryVerticesOut(AValue: Integer);
    function GetHandle: TGLShaderHandle;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoOnPrepare(Sender: TGLContext); override;
    class function FriendlyName: string; override;
    procedure NotifyChange(Sender: TObject); override;
    property Handle: TGLShaderHandle read GetHandle;
  published
    property Source: TStringList read FSource write SetSource;
    property SourceFile: string read FSourceFile write SetSourceFile;
    property ShaderType: TGLShaderType read FShaderType
      write SetShaderType default shtVertex;
    property InfoLog: string read FInfoLog;
    property GeometryInput: TGLgsInTypes read FGeometryInput
      write SetGeometryInput default gsInPoints;
    property GeometryOutput: TGLgsOutTypes read FGeometryOutput
      write SetGeometryOutput default gsOutPoints;
    property GeometryVerticesOut: Integer read FGeometryVerticesOut
      write SetGeometryVerticesOut default 1;
  end;


  TGLAbstractShaderUniform = class(TGLUpdateAbleObject, IShaderParameter)
  protected
    FName: string;
    FNameHashCode: Integer;
    FType: TGLSLDataType;
    FSamplerType: TGLSLSamplerType;
    function GetName: string;
    function GetGLSLType: TGLSLDataType;
    function GetGLSLSamplerType: TGLSLSamplerType;
    function GetAutoSetMethod: string; virtual;
    function GetTextureName: string; virtual;
    function GetSamplerName: string; virtual;
    function GetTextureSwizzle: TSwizzleVector; virtual;
    procedure SetTextureName(const AValue: string); virtual;
    procedure SetSamplerName(const AValue: string); virtual;
    procedure SetAutoSetMethod(const AValue: string); virtual;
    procedure SetTextureSwizzle(const AValue: TSwizzleVector); virtual;
    function GetFloat: Single; virtual;
    function GetVec2: TVector2f; virtual;
    function GetVec3: TVector3f; virtual;
    function GetVec4: TGLVector; virtual;
    function GetInt: TGLint; virtual;
    function GetIVec2: TVector2i; virtual;
    function GetIVec3: TVector3i; virtual;
    function GetIVec4: TVector4i; virtual;
    function GetUInt: Cardinal; virtual;
    function GetUVec2: TVector2ui; virtual;
    function GetUVec3: TVector3ui; virtual;
    function GetUVec4: TVector4ui; virtual;
    procedure SetFloat(const Value: TGLFloat); virtual;
    procedure SetVec2(const Value: TVector2f); virtual;
    procedure SetVec3(const Value: TVector3f); virtual;
    procedure SetVec4(const Value: TVector4f); virtual;
    procedure SetInt(const Value: Integer); virtual;
    procedure SetIVec2(const Value: TVector2i); virtual;
    procedure SetIVec3(const Value: TVector3i); virtual;
    procedure SetIVec4(const Value: TVector4i); virtual;
    procedure SetUInt(const Value: Cardinal); virtual;
    procedure SetUVec2(const Value: TVector2ui); virtual;
    procedure SetUVec3(const Value: TVector3ui); virtual;
    procedure SetUVec4(const Value: TVector4ui); virtual;
    function GetMat2: TMatrix2f; virtual;
    function GetMat3: TMatrix3f; virtual;
    function GetMat4: TMatrix4f; virtual;
    procedure SetMat2(const Value: TMatrix2f); virtual;
    procedure SetMat3(const Value: TMatrix3f); virtual;
    procedure SetMat4(const Value: TMatrix4f); virtual;
    procedure SetFloatArray(const Values: PGLFloat; Count: Integer); virtual;
    procedure SetIntArray(const Values: PGLInt; Count: Integer); virtual;
    procedure SetUIntArray(const Values: PGLUInt; Count: Integer); virtual;
    procedure WriteToFiler(AWriter: TWriter); virtual;
    procedure ReadFromFiler(AReader: TReader); virtual;
    procedure Apply(var ARci: TGLRenderContextInfo); virtual;
  end;

  CGLAbstractShaderUniform = class of TGLAbstractShaderUniform;

  TGLShaderUniform = class(TGLAbstractShaderUniform, IShaderParameter)
  protected
    
    FLocation: Integer;
    FStoreProgram: Cardinal;
    FAutoSet: TUniformAutoSetMethod;
    function GetProgram: Cardinal; inline;
    procedure PushProgram; inline;
    procedure PopProgram; inline;
    function GetFloat: Single; override;
    function GetVec2: TVector2f; override;
    function GetVec3: TVector3f; override;
    function GetVec4: TGLVector; override;
    function GetInt: TGLInt; override;
    function GetIVec2: TVector2i; override;
    function GetIVec3: TVector3i; override;
    function GetIVec4: TVector4i; override;
    function GetUInt: Cardinal; override;
    function GetUVec2: TVector2ui; override;
    function GetUVec3: TVector3ui; override;
    function GetUVec4: TVector4ui; override;
    procedure SetFloat(const Value: TGLFloat); override;
    procedure SetVec2(const Value: TVector2f); override;
    procedure SetVec3(const Value: TVector3f); override;
    procedure SetVec4(const Value: TVector4f); override;
    procedure SetInt(const Value: Integer); override;
    procedure SetIVec2(const Value: TVector2i); override;
    procedure SetIVec3(const Value: TVector3i); override;
    procedure SetIVec4(const Value: TVector4i); override;
    procedure SetUInt(const Value: Cardinal); override;
    procedure SetUVec2(const Value: TVector2ui); override;
    procedure SetUVec3(const Value: TVector3ui); override;
    procedure SetUVec4(const Value: TVector4ui); override;
    function GetMat2: TMatrix2f; override;
    function GetMat3: TMatrix3f; override;
    function GetMat4: TMatrix4f; override;
    procedure SetMat2(const Value: TMatrix2f); override;
    procedure SetMat3(const Value: TMatrix3f); override;
    procedure SetMat4(const Value: TMatrix4f); override;
    function GetAutoSetMethod: string; override;
    procedure SetAutoSetMethod(const AValue: string); override;
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  public
    procedure SetFloatArray(const Values: PGLFloat; Count: Integer); override;
    procedure SetIntArray(const Values: PGLInt; Count: Integer); override;
    procedure SetUIntArray(const Values: PGLUInt; Count: Integer); override;
    procedure Assign(Source: TPersistent); override;
    procedure Apply(var ARci: TGLRenderContextInfo); override;
    property Name: string read GetName;
    property Location: Integer read FLocation;
    property GLSLType: TGLSLDataType read GetGLSLType;
  end;

  TGLShaderUniformDSA = class(TGLShaderUniform)
  protected
    procedure SetFloat(const Value: TGLFloat); override;
    procedure SetVec2(const Value: TVector2f); override;
    procedure SetVec3(const Value: TVector3f); override;
    procedure SetVec4(const Value: TVector4f); override;
    procedure SetInt(const Value: Integer); override;
    procedure SetIVec2(const Value: TVector2i); override;
    procedure SetIVec3(const Value: TVector3i); override;
    procedure SetIVec4(const Value: TVector4i); override;
    procedure SetUInt(const Value: Cardinal); override;
    procedure SetUVec2(const Value: TVector2ui); override;
    procedure SetUVec3(const Value: TVector3ui); override;
    procedure SetUVec4(const Value: TVector4ui); override;
    procedure SetMat2(const Value: TMatrix2f); override;
    procedure SetMat3(const Value: TMatrix3f); override;
    procedure SetMat4(const Value: TMatrix4f); override;
  public
    procedure SetFloatArray(const Values: PGLFloat; Count: Integer); override;
    procedure SetIntArray(const Values: PGLInt; Count: Integer); override;
    procedure SetUIntArray(const Values: PGLUInt; Count: Integer); override;
  end;

  TGLShaderUniformTexture = class(TGLShaderUniform)
  private
    FLibTexture: TGLAbstractTexture;
    FLibSampler: TGLTextureSampler;
    FTarget: TGLTextureTarget;
    FSwizzling: TSwizzleVector;
  protected
    FLibTexureName: TGLMaterialComponentName;
    FLibSamplerName: TGLMaterialComponentName;
    function GetTextureName: string; override;
    function GetSamplerName: string; override;
    function GetTextureSwizzle: TSwizzleVector; override;
    procedure SetTextureName(const AValue: string); override;
    procedure SetSamplerName(const AValue: string); override;
    procedure SetTextureSwizzle(const AValue: TSwizzleVector); override;
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
    procedure Loaded;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Notification(Sender: TObject; Operation: TOperation); override;
    procedure Apply(var ARci: TGLRenderContextInfo); override;
    property LibTextureName: TGLMaterialComponentName read GetTextureName
      write SetTextureName;
    property LibSamplerName: TGLMaterialComponentName read GetSamplerName
      write SetSamplerName;
    property GLSLSampler: TGLSLSamplerType read GetGLSLSamplerType;
    property Swizzling: TSwizzleVector read GetTextureSwizzle write
      SetTextureSwizzle;
  end;


  TGLBaseShaderModel = class(TGLLibMaterialProperty)
  protected
    FHandle: TGLProgramHandle;
    FLibShaderName: array[TGLShaderType] of string;
    FShaders: array[TGLShaderType] of TGLShaderEx;
    FIsValid: Boolean;
    FInfoLog: string;
    FUniforms: TPersistentObjectList;
    FAutoFill: Boolean;
    function GetLibShaderName(AType: TGLShaderType): string;
    procedure SetLibShaderName(AType: TGLShaderType; const AValue: string);
    function GetUniform(const AName: string): IShaderParameter;
    class procedure ReleaseUniforms(AList: TPersistentObjectList);
    property LibVertexShaderName: TGLMaterialComponentName index shtVertex
      read GetLibShaderName write SetLibShaderName;
    property LibFragmentShaderName: TGLMaterialComponentName index shtFragment
      read GetLibShaderName write SetLibShaderName;
    property LibGeometryShaderName: TGLMaterialComponentName index shtGeometry
      read GetLibShaderName write SetLibShaderName;
    property LibTessEvalShaderName: TGLMaterialComponentName index shtEvaluation
      read GetLibShaderName write SetLibShaderName;
    property LibTessControlShaderName: TGLMaterialComponentName index shtControl
      read GetLibShaderName write SetLibShaderName;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadUniforms(AStream: TStream);
    procedure WriteUniforms(AStream: TStream);
    procedure Loaded; override;
    class function IsSupported: Boolean; virtual; abstract;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure Notification(Sender: TObject; Operation: TOperation); override;
    procedure DoOnPrepare(Sender: TGLContext);
    procedure Apply(var ARci: TGLRenderContextInfo); virtual;
    procedure UnApply(var ARci: TGLRenderContextInfo); virtual;
    procedure GetUniformNames(Proc: TGetStrProc);
    property Handle: TGLProgramHandle read FHandle;
    property IsValid: Boolean read FIsValid;
    property Uniforms[const AName: string]: IShaderParameter read GetUniform;
  published
    // Compilation info log for design time
    property InfoLog: string read FInfoLog;
    // Turn on autofill of uniforms
    property AutoFillOfUniforms: Boolean read FAutoFill
      write FAutoFill stored False;
    property NextPass;
  end;

  TGLShaderModel3 = class(TGLBaseShaderModel)
  public
    class function IsSupported: Boolean; override;
  published
    property LibVertexShaderName;
    property LibFragmentShaderName;
  end;

  TGLShaderModel4 = class(TGLBaseShaderModel)
  public
    class function IsSupported: Boolean; override;
  published
    property LibVertexShaderName;
    property LibGeometryShaderName;
    property LibFragmentShaderName;
  end;

  TGLShaderModel5 = class(TGLBaseShaderModel)
  public
    procedure Apply(var ARci: TGLRenderContextInfo); override;
    procedure UnApply(var ARci: TGLRenderContextInfo); override;
    class function IsSupported: Boolean; override;
  published
    property LibTessControlShaderName;
    property LibTessEvalShaderName;
    property LibVertexShaderName;
    property LibGeometryShaderName;
    property LibFragmentShaderName;
  end;

  TGLLibMaterialEx = class(TGLAbstractLibMaterial)
  private
    FHandle: TGLVirtualHandle;
    FApplicableLevel: TGLMaterialLevel;
    FSelectedLevel: TGLMaterialLevel;
    FFixedFunc: TGLFixedFunctionProperties;
    FMultitexturing: TGLMultitexturingProperties;
    FSM3: TGLShaderModel3;
    FSM4: TGLShaderModel4;
    FSM5: TGLShaderModel5;
    FOnAsmProgSetting: TOnAsmProgSetting;
    FOnSM3UniformInit: TOnUniformInitialize;
    FOnSM3UniformSetting: TOnUniformSetting;
    FOnSM4UniformInit: TOnUniformInitialize;
    FOnSM4UniformSetting: TOnUniformSetting;
    FOnSM5UniformInit: TOnUniformInitialize;
    FOnSM5UniformSetting: TOnUniformSetting;
    FNextPass: TGLLibMaterialEx;
    FStoreAmalgamating: Boolean;
    procedure SetLevel(AValue: TGLMaterialLevel);
    procedure SetFixedFunc(AValue: TGLFixedFunctionProperties);
    procedure SetMultitexturing(AValue: TGLMultitexturingProperties);
    procedure SetSM3(AValue: TGLShaderModel3);
    procedure SetSM4(AValue: TGLShaderModel4);
    procedure SetSM5(AValue: TGLShaderModel5);
    procedure DoAllocate(Sender: TGLVirtualHandle; var handle: Cardinal);
    procedure DoDeallocate(Sender: TGLVirtualHandle; var handle: Cardinal);
  protected
    procedure Loaded; override;
    procedure RemoveDefferedInit;
    procedure DoOnPrepare(Sender: TGLContext);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure Apply(var ARci: TGLRenderContextInfo); override;
    function UnApply(var ARci: TGLRenderContextInfo): Boolean; override;
    function Blended: Boolean; override;
  published
    property ApplicableLevel: TGLMaterialLevel read FApplicableLevel write SetLevel default mlAuto;
    property SelectedLevel: TGLMaterialLevel read FSelectedLevel;
    property FixedFunction: TGLFixedFunctionProperties read FFixedFunc write SetFixedFunc;
    property Multitexturing: TGLMultitexturingProperties read FMultitexturing write SetMultitexturing;
    property ShaderModel3: TGLShaderModel3 read FSM3 write SetSM3;
    property ShaderModel4: TGLShaderModel4 read FSM4 write SetSM4;
    property ShaderModel5: TGLShaderModel5 read FSM5 write SetSM5;
    // Asm vertex program event
    property OnAsmProgSetting: TOnAsmProgSetting read FOnAsmProgSetting
      write FOnAsmProgSetting;
    // Shader model 3 event
    property OnSM3UniformInitialize: TOnUniformInitialize read FOnSM3UniformInit
      write FOnSM3UniformInit;
    property OnSM3UniformSetting: TOnUniformSetting read FOnSM3UniformSetting
      write FOnSM3UniformSetting;
    // Shader model 4 event
    property OnSM4UniformInitialize: TOnUniformInitialize read FOnSM4UniformInit
      write FOnSM4UniformInit;
    property OnSM4UniformSetting: TOnUniformSetting read FOnSM4UniformSetting
      write FOnSM4UniformSetting;
    // Shader model 5 event
    property OnSM5UniformInitialize: TOnUniformInitialize read FOnSM5UniformInit
      write FOnSM5UniformInit;
    property OnSM5UniformSetting: TOnUniformSetting read FOnSM5UniformSetting
      write FOnSM5UniformSetting;
  end;


  TGLLibMaterialsEx = class(TGLAbstractLibMaterials)
  protected
    procedure SetItems(AIndex: Integer; const AValue: TGLLibMaterialEx);
    function GetItems(AIndex: Integer): TGLLibMaterialEx;
  public
    constructor Create(AOwner: TComponent);
    function MaterialLibrary: TGLMaterialLibraryEx;
    function IndexOf(const Item: TGLLibMaterialEx): Integer;
    function Add: TGLLibMaterialEx;
    function FindItemID(ID: Integer): TGLLibMaterialEx;
    property Items[index: Integer]: TGLLibMaterialEx read GetItems
    write SetItems; default;
    function GetLibMaterialByName(const AName: TGLLibMaterialName):
      TGLLibMaterialEx;
  end;

  TGLMatLibComponents = class(TXCollection)
  protected
    function GetItems(index: Integer): TGLBaseMaterialCollectionItem;
  public
    function GetNamePath: string; override;
    class function ItemsClass: TXCollectionItemClass; override;
    property Items[index: Integer]: TGLBaseMaterialCollectionItem
    read GetItems; default;
    function GetItemByName(const AName: TGLMaterialComponentName):
      TGLBaseMaterialCollectionItem;
    function GetTextureByName(const AName: TGLMaterialComponentName):
      TGLAbstractTexture;
    function GetAttachmentByName(const AName: TGLMaterialComponentName):
      TGLFrameBufferAttachment;
    function GetSamplerByName(const AName: TGLMaterialComponentName):
      TGLTextureSampler;
    function GetCombinerByName(const AName: TGLMaterialComponentName):
      TGLTextureCombiner;
    function GetShaderByName(const AName: TGLMaterialComponentName):
      TGLShaderEx;
    function GetAsmProgByName(const AName: TGLMaterialComponentName):
      TGLASMVertexProgram;
    function MakeUniqueName(const AName: TGLMaterialComponentName):
      TGLMaterialComponentName;
  end;

  TGLMaterialLibraryEx = class(TGLAbstractMaterialLibrary)
  private
    FComponents: TGLMatLibComponents;
  protected
    procedure Loaded; override;
    function GetMaterials: TGLLibMaterialsEx;
    procedure SetMaterials(AValue: TGLLibMaterialsEx);
    function StoreMaterials: Boolean;
    procedure SetComponents(AValue: TGLMatLibComponents);

    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteComponents(AStream: TStream);
    procedure ReadComponents(AStream: TStream);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetNames(Proc: TGetStrProc;
      AClass: CGLBaseMaterialCollectionItem); overload;
    function AddTexture(const AName: TGLMaterialComponentName):
      TGLTextureImageEx;
    function AddAttachment(const AName: TGLMaterialComponentName):
      TGLFrameBufferAttachment;
    function AddSampler(const AName: TGLMaterialComponentName):
      TGLTextureSampler;
    function AddCombiner(const AName: TGLMaterialComponentName):
      TGLTextureCombiner;
    function AddShader(const AName: TGLMaterialComponentName): TGLShaderEx;
    function AddAsmProg(const AName: TGLMaterialComponentName):
      TGLASMVertexProgram;
    procedure SetLevelForAll(const ALevel: TGLMaterialLevel);
  published
    // The materials collection.
    property Materials: TGLLibMaterialsEx read GetMaterials write SetMaterials
      stored StoreMaterials;
    property Components: TGLMatLibComponents read FComponents
      write SetComponents;
    property TexturePaths;
  end;

procedure RegisterGLMaterialExNameChangeEvent(AEvent: TNotifyEvent);
procedure DeRegisterGLMaterialExNameChangeEvent(AEvent: TNotifyEvent);

//=================================================================
implementation
//=================================================================

const
  cTextureMagFilter: array[maNearest..maLinear] of Cardinal =
    (GL_NEAREST, GL_LINEAR);
  cTextureMinFilter: array[miNearest..miLinearMipmapLinear] of Cardinal =
    (GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_LINEAR);
  cTextureWrapMode: array[twRepeat..twMirrorClampToBorder] of Cardinal =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER,
    GL_MIRRORED_REPEAT, GL_MIRROR_CLAMP_TO_EDGE_ATI,
      GL_MIRROR_CLAMP_TO_BORDER_EXT);
  cTextureCompareMode: array[tcmNone..tcmCompareRtoTexture] of Cardinal =
    (GL_NONE, GL_COMPARE_R_TO_TEXTURE);
  cSamplerToTexture: array[TGLSLSamplerType] of TGLTextureTarget =
    (
    ttNoShape,
    ttTexture1D,
    ttTexture2D,
    ttTexture3D,
    ttTextureCube,
    ttTexture1D,
    ttTexture2D,
    ttTexture1DArray,
    ttTexture2DArray,
    ttTexture1DArray,
    ttTexture1DArray,
    ttTextureCube,
    ttTexture1D,
    ttTexture2D,
    ttTexture3D,
    ttTextureCube,
    ttTexture1DArray,
    ttTexture2DArray,
    ttTexture1D,
    ttTexture2D,
    ttTexture3D,
    ttTextureCube,
    ttTexture1DArray,
    ttTexture2DArray,
    ttTextureRect,
    ttTextureRect,
    ttTextureBuffer,
    ttTextureRect,
    ttTextureBuffer,
    ttTextureRect,
    ttTextureBuffer,
    ttTexture2DMultisample,
    ttTexture2DMultisample,
    ttTexture2DMultisample,
    ttTexture2DMultisampleArray,
    ttTexture2DMultisampleArray,
    ttTexture2DMultisample
    );

  cTextureSwizzle: array[TGLTextureSwizzle] of Cardinal =
    (
    GL_RED,
    GL_GREEN,
    GL_BLUE,
    GL_ALPHA,
    GL_ZERO,
    GL_ONE
    );

const
  cTextureMode: array[TGLTextureMode] of Cardinal =
    (GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE, GL_ADD);

const
  cShaderTypeName: array[TGLShaderType] of string =
    ('vertex', 'control', 'evaluation', 'geomtery', 'fragment');

type
  TFriendlyImage = class(TGLBaseImage);

  TStandartUniformAutoSetExecutor = class
  public
    constructor Create;
    procedure SetModelMatrix(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetViewMatrix(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetProjectionMatrix(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetInvModelMatrix(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetModelViewMatrix(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetNormalModelMatrix(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetInvModelViewMatrix(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetViewProjectionMatrix(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetWorldViewProjectionMatrix(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetCameraPosition(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    // Lighting
    procedure SetLightSource0Position(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    // Material
    procedure SetMaterialFrontAmbient(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetMaterialFrontDiffuse(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetMaterialFrontSpecular(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetMaterialFrontEmission(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetMaterialFrontShininess(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetMaterialBackAmbient(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetMaterialBackDiffuse(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetMaterialBackSpecular(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetMaterialBackShininess(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
    procedure SetMaterialBackEmission(Sender: IShaderParameter; var ARci: TGLRenderContextInfo);
  end;

var
  vGLMaterialExNameChangeEvent: TNotifyEvent;
  vStandartUniformAutoSetExecutor: TStandartUniformAutoSetExecutor;
  vStoreBegin: procedure(mode: Cardinal);{$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

procedure RegisterGLMaterialExNameChangeEvent(AEvent: TNotifyEvent);
begin
  vGLMaterialExNameChangeEvent := AEvent;
end;

procedure DeRegisterGLMaterialExNameChangeEvent(AEvent: TNotifyEvent);
begin
  vGLMaterialExNameChangeEvent := nil;
end;

function ComputeNameHashKey(
  const AName: string): Integer;
var
  i, n: Integer;
begin
  n := Length(AName);
  Result := n;
  for i := 1 to n do
    Result := (Result shl 1) + Byte(AName[i]);
end;

procedure Div2(var Value: Integer); inline;
begin
  Value := Value div 2;
  if Value = 0 then
    Value := 1;
end;

function CalcTextureLevelNumber(ATarget: TGLTextureTarget; w, h, d: Integer):
  Integer;
begin
  Result := 0;
  case ATarget of
    ttNoShape: ;
    ttTexture1D, ttTexture1DArray, ttTextureCube, ttTextureCubeArray:
      repeat
        Inc(Result);
        Div2(w);
      until w <= 1;

    ttTexture2D, ttTexture2DArray:
      repeat
        Inc(Result);
        Div2(w);
        Div2(h);
      until (w <= 1) and (h <= 1);

    ttTexture3D:
      repeat
        Inc(Result);
        Div2(w);
        Div2(h);
        Div2(d);
      until (w <= 1) and (h <= 1) and (d <= 1);

    ttTextureRect, ttTextureBuffer,
      ttTexture2DMultisample, ttTexture2DMultisampleArray:
      Result := 1;
  end;
end;

destructor TGLBaseMaterialCollectionItem.Destroy;
var
  I: Integer;
begin
  if Assigned(FUserList) then
  begin
    FNotifying := True;
    for I := FUserList.Count - 1 downto 0 do
      TGLLibMaterialProperty(FUserList[I]).Notification(Self, opRemove);
    FreeAndNil(FUserList);
  end;
  inherited;
end;

function TGLBaseMaterialCollectionItem.GetMaterialLibrary:
  TGLAbstractMaterialLibrary;
begin
  Result := TGLAbstractMaterialLibrary(TGLMatLibComponents(Owner).Owner);
end;

function TGLBaseMaterialCollectionItem.GetMaterialLibraryEx:
  TGLMaterialLibraryEx;
begin
  Result := TGLMaterialLibraryEx(TGLMatLibComponents(Owner).Owner);
end;

function TGLBaseMaterialCollectionItem.GetUserCount: Integer;
begin
  if Assigned(FUserList) then
    Result := FUserList.Count
  else
    Result := 0;
end;

function TGLBaseMaterialCollectionItem.GetUserList: TPersistentObjectList;
begin
  if FUserList = nil then
  begin
    FUserList := TPersistentObjectList.Create;
    FNotifying := False;
  end;
  Result := FUserList;
end;

procedure TGLBaseMaterialCollectionItem.NotifyChange(Sender: TObject);
var
  I: Integer;
begin
  if FNotifying then
    exit;
  FNotifying := True;
  if GetUserCount > 0 then
    for I := 0 to FUserList.Count - 1 do
      TGLUpdateAbleObject(FUserList[I]).NotifyChange(Self);
  FNotifying := False;
end;

procedure TGLBaseMaterialCollectionItem.RegisterUser(
  AUser: TGLUpdateAbleObject);
begin
  if not FNotifying and (UserList.IndexOf(AUser) < 0) then
    UserList.Add(AUser);
end;

procedure TGLBaseMaterialCollectionItem.UnregisterUser(
  AUser: TGLUpdateAbleObject);
begin
  if not FNotifying then
    UserList.Remove(AUser);
end;

procedure TGLBaseMaterialCollectionItem.SetName(const AValue: string);
begin
  if AValue <> Name then
  begin
    if not IsValidIdent(AValue) then
    begin
      if IsDesignTime then
        InformationDlg(AValue + ' - is not valid component name');
      exit;
    end;
    if not (csLoading in MaterialLibrary.ComponentState) then
    begin
      if TGLMatLibComponents(Owner).GetItemByName(AValue) <> Self then
        inherited SetName(TGLMatLibComponents(Owner).MakeUniqueName(AValue))
      else
        inherited SetName(AValue);
    end
    else
      inherited SetName(AValue);
    FNameHashKey := ComputeNameHashKey(Name);
    // Notify users
    NotifyChange(Self);
    // Notify designer
    if Assigned(vGLMaterialExNameChangeEvent) then
      vGLMaterialExNameChangeEvent(Self);
  end;
end;

procedure TGLFixedFunctionProperties.Apply(var ARci: TGLRenderContextInfo);
begin
  with ARci.GLStates do
  begin
    Disable(stColorMaterial);
    PolygonMode := FPolygonMode;

    // Fixed functionality state
    if True{ not ARci.GLStates.ForwardContext} then
    begin
      // Lighting switch
      if (moNoLighting in MaterialOptions) or not ARci.bufferLighting then
      begin
        Disable(stLighting);
        FFrontProperties.ApplyNoLighting(ARci, cmFront);
      end
      else
      begin
        Enable(stLighting);
        FFrontProperties.Apply(ARci, cmFront);
      end;
      if FPolygonMode = pmLines then
        Disable(stLineStipple);
      // Fog switch
      if (moIgnoreFog in MaterialOptions) or not ARci.bufferFog then
        Disable(stFog)
      else
        Enable(stFog);
    end;

    // Apply FaceCulling and BackProperties (if needs be)
    case FFaceCulling of
      fcBufferDefault:
        begin
          if ARci.bufferFaceCull then
            Enable(stCullFace)
          else
            Disable(stCullFace);
          BackProperties.Apply(ARci, cmBack);
        end;
      fcCull: Enable(stCullFace);
      fcNoCull:
        begin
          Disable(stCullFace);
          BackProperties.Apply(ARci, cmBack);
        end;
    end;
    // note: Front + Back with different PolygonMode are no longer supported.
    // Currently state cache just ignores back facing mode changes, changes to
    // front affect both front + back PolygonMode

    // Apply Blending mode
    if not ARci.ignoreBlendingRequests then
      case FBlendingMode of
        bmOpaque:
          begin
            Disable(stBlend);
            Disable(stAlphaTest);
          end;
        bmTransparency:
          begin
            Enable(stBlend);
            Enable(stAlphaTest);
            SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
            SetGLAlphaFunction(cfGreater, 0);
          end;
        bmAdditive:
          begin
            Enable(stBlend);
            Enable(stAlphaTest);
            SetBlendFunc(bfSrcAlpha, bfOne);
            SetGLAlphaFunction(cfGreater, 0);
          end;
        bmAlphaTest50:
          begin
            Disable(stBlend);
            Enable(stAlphaTest);
            SetGLAlphaFunction(cfGEqual, 0.5);
          end;
        bmAlphaTest100:
          begin
            Disable(stBlend);
            Enable(stAlphaTest);
            SetGLAlphaFunction(cfGEqual, 1.0);
          end;
        bmModulate:
          begin
            Enable(stBlend);
            Enable(stAlphaTest);
            SetBlendFunc(bfDstColor, bfZero);
            SetGLAlphaFunction(cfGreater, 0);
          end;
        bmCustom:
          begin
            FBlendingParams.Apply(ARci);
          end;
      end;

    // Apply depth properties
    if not ARci.ignoreDepthRequests then
      FDepthProperties.Apply(ARci);

    // Apply texturing
    if ARci.currentMaterialLevel = mlFixedFunction then
    begin
      if FTexProp.Enabled and FTexProp.IsValid then
      begin
        ARci.GLStates.ActiveTexture := 0;
        FTexProp.Apply(ARci);
        gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE,
          cTextureMode[FTextureMode]);
      end;
    end;

  end;
end;

procedure TGLFixedFunctionProperties.Assign(Source: TPersistent);
var
  LFFP: TGLFixedFunctionProperties;
begin
  if Source is TGLFixedFunctionProperties then
  begin
    LFFP := TGLFixedFunctionProperties(Source);
    if Assigned(LFFP.FBackProperties) then
      BackProperties.Assign(LFFP.BackProperties)
    else
      FreeAndNil(FBackProperties);
    FFrontProperties.Assign(LFFP.FFrontProperties);
    FPolygonMode := LFFP.FPolygonMode;
    FBlendingMode := LFFP.FBlendingMode;
    FMaterialOptions := LFFP.FMaterialOptions;
    FFaceCulling := LFFP.FFaceCulling;
    FDepthProperties.Assign(LFFP.FDepthProperties);
    FTexProp.Assign(LFFP.FTexProp);
    FTextureMode := LFFP.TextureMode;
    NotifyChange(Self);
  end;
  inherited;
end;

function TGLFixedFunctionProperties.Blended: Boolean;
begin
  Result := not (FBlendingMode in [bmOpaque, bmAlphaTest50, bmAlphaTest100, bmCustom]);
end;

constructor TGLFixedFunctionProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FFrontProperties := TGLFaceProperties.Create(Self);
  FFaceCulling := fcBufferDefault;
  FPolygonMode := pmFill;
  FBlendingParams := TGLBlendingParameters.Create(Self);
  FDepthProperties := TGLDepthProperties.Create(Self);
  FTexProp := TGLTextureProperties.Create(Self);
  FTextureMode := tmDecal;
  FEnabled := True;
end;

destructor TGLFixedFunctionProperties.Destroy;
begin
  FFrontProperties.Destroy;
  FBackProperties.Free;
  FDepthProperties.Destroy;
  FBlendingParams.Destroy;
  FTexProp.Destroy;
  inherited;
end;

function TGLFixedFunctionProperties.GetBackProperties: TGLFaceProperties;
begin
  if not Assigned(FBackProperties) then
    FBackProperties := TGLFaceProperties.Create(Self);
  Result := FBackProperties;
end;

procedure TGLFixedFunctionProperties.SetBackProperties(AValues:
  TGLFaceProperties);
begin
  BackProperties.Assign(AValues);
  NotifyChange(Self);
end;

procedure TGLFixedFunctionProperties.SetBlendingMode(const AValue:
  TGLBlendingMode);
begin
  if AValue <> FBlendingMode then
  begin
    FBlendingMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFixedFunctionProperties.SetBlendingParams(const AValue:
  TGLBlendingParameters);
begin
  FBlendingParams.Assign(AValue);
  NotifyChange(Self);
end;

procedure TGLFixedFunctionProperties.SetDepthProperties(AValues:
  TGLDepthProperties);
begin
  FDepthProperties.Assign(AValues);
  NotifyChange(Self);
end;

procedure TGLFixedFunctionProperties.SetTexProp(AValue: TGLTextureProperties);
begin
  FTexProp.Assign(AValue);
end;

procedure TGLFixedFunctionProperties.SetTextureMode(AValue: TGLTextureMode);
begin
  if AValue <> FTextureMode then
  begin
    FTextureMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFixedFunctionProperties.SetFaceCulling(const AValue: TGLFaceCulling);
begin
  if AValue <> FFaceCulling then
  begin
    FFaceCulling := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFixedFunctionProperties.SetFrontProperties(AValues:
  TGLFaceProperties);
begin
  FFrontProperties.Assign(AValues);
  NotifyChange(Self);
end;

procedure TGLFixedFunctionProperties.SetMaterialOptions(const AValue:
  TGLMaterialOptions);
begin
  if AValue <> FMaterialOptions then
  begin
    FMaterialOptions := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFixedFunctionProperties.SetPolygonMode(AValue: TGLPolygonMode);
begin
  if AValue <> FPolygonMode then
  begin
    FPolygonMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFixedFunctionProperties.UnApply(var ARci: TGLRenderContextInfo);
begin
  if FTexProp.Enabled and FTexProp.IsValid then
    FTexProp.UnApply(ARci);
end;

function TGLAbstractTexture.GetTextureTarget: TGLTextureTarget;
begin
  Result := FHandle.Target;
end;

procedure TGLTextureImageEx.Apply(var ARci: TGLRenderContextInfo);
begin
  if FIsValid then
  begin
    // Just bind
    with ARci.GLStates do
    begin
      TextureBinding[ActiveTexture, FHandle.Target] := FHandle.Handle;
      ActiveTextureEnabled[FHandle.Target] := True;
    end;

    if not IsDesignTime then
    begin
      if not FUseStreaming and Assigned(FImage) then
      begin
        Inc(FApplyCounter);
        if FApplyCounter > 16 then
          FreeAndNil(FImage);
      end;

      if FUseStreaming then
      begin
        StreamTransfer;
      end;
    end;
  end
  else with ARci.GLStates do
    TextureBinding[ActiveTexture, FHandle.Target] := 0;
end;

procedure TGLTextureImageEx.Assign(Source: TPersistent);
var
  LTexture: TGLTextureImageEx;
begin
  if Source is TGLTextureImageEx then
  begin
    LTexture := TGLTextureImageEx(Source);
    FCompression := LTexture.FCompression;
    if Assigned(LTexture.FImage) then
    begin
      if not Assigned(FImage) then
        FImage := TGLImage.Create;
      FImage.Assign(LTexture.FImage);
    end
    else
      FreeAndNil(FImage);
    FImageAlpha := LTexture.FImageAlpha;
    FImageBrightness := LTexture.FImageBrightness;
    FImageGamma := LTexture.FImageGamma;
    FHeightToNormalScale := LTexture.FHeightToNormalScale;
    FSourceFile := LTexture.FSourceFile;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TGLTextureImageEx.Create(AOwner: TXCollection);
begin
  inherited;
  FDefferedInit := False;
  FHandle := TGLTextureHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FCompression := tcDefault;
  FImageAlpha := tiaDefault;
  FImageBrightness := 1.0;
  FImageGamma := 1.0;
  FHeightToNormalScale := 1.0;
  FInternalFormat := tfRGBA8;
  FInternallyStored := False;
  FMipGenMode := mgmOnFly;
  FUseStreaming := False;
  Name := TGLMatLibComponents(AOwner).MakeUniqueName('Texture');
end;

destructor TGLTextureImageEx.Destroy;
begin
  FHandle.Destroy;
  FImage.Free;
  inherited;
end;

procedure TGLTextureImageEx.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TGLTextureImageEx.DoOnPrepare(Sender: TGLContext);
var
  LTarget: TGLTextureTarget;
  rowSize: Integer;
begin
  if IsDesignTime and FDefferedInit then
    exit;

  FHandle.AllocateHandle;
  if not FHandle.IsDataNeedUpdate then
    exit;

  try
    PrepareImage;

    // Target
    LTarget := FImage.GetTextureTarget;

    // Check supporting
    if not IsTargetSupported(LTarget)
      or not IsFormatSupported(FInternalFormat) then
      Abort;

    if (FHandle.Target <> LTarget)
      and (FHandle.Target <> ttNoShape) then
    begin
      FHandle.DestroyHandle;
      FHandle.AllocateHandle;
    end;
    FHandle.Target := LTarget;

    // Check streaming support
    if not IsDesignTime then
    begin
      FUseStreaming := FUseStreaming and TGLUnpackPBOHandle.IsSupported;
      FUseStreaming := FUseStreaming and IsServiceContextAvaible;
      FUseStreaming := FUseStreaming and (LTarget = ttTexture2D);
    end;

    with Sender.GLStates do
    begin
      ActiveTextureEnabled[FHandle.Target] := True;
      TextureBinding[ActiveTexture, FHandle.Target] := FHandle.Handle;
      UnpackRowLength := 0;
      UnpackSkipRows := 0;
      UnpackSkipPixels := 0;
      rowSize := FImage.LevelWidth[0] * FImage.ElementSize;
      if (rowSize mod 8 = 0) and (FImage.ElementSize > 4) then
        UnpackAlignment := 8
      else
      if rowSize mod 4 = 0 then
        UnpackAlignment := 4
      else if rowSize mod 2 = 0 then
        UnpackAlignment := 2
      else
        UnpackAlignment := 1;
    end;

    if not IsDesignTime and FUseStreaming then
    begin
      TFriendlyImage(FImage).StartStreaming;
      FLastTime := AppTime;
      StreamTransfer;
      FHandle.NotifyDataUpdated;
    end
    else
      FullTransfer;

    Sender.GLStates.ActiveTextureEnabled[FHandle.Target] := False;

    FApplyCounter := 0;
    FIsValid := True;
  except
    FIsValid := False;
  end;
end;

procedure TGLTextureImageEx.FullTransfer;
var
  LCompression: TGLTextureCompression;
  glFormat: Cardinal;
begin
  begin
    if GL.ARB_texture_compression then
    begin
      if Compression = tcDefault then
        if vDefaultTextureCompression = tcDefault then
          LCompression := tcNone
        else
          LCompression := vDefaultTextureCompression
      else
        LCompression := Compression;
    end
    else
      LCompression := tcNone;

    if LCompression <> tcNone then
      with CurrentGLContext.GLStates do
      begin
        case LCompression of
          tcStandard: TextureCompressionHint := hintDontCare;
          tcHighQuality: TextureCompressionHint := hintNicest;
          tcHighSpeed: TextureCompressionHint := hintFastest;
        else
          Assert(False, strErrorEx + strUnknownType);
        end;
        if not GetGenericCompressedFormat(
          FInternalFormat,
          FImage.ColorFormat, glFormat) then
          glFormat := InternalFormatToOpenGLFormat(FInternalFormat);
      end
    else
      glFormat := InternalFormatToOpenGLFormat(FInternalFormat);

    FImage.RegisterAsOpenGLTexture(
      FHandle,
      FMipGenMode = mgmOnFly,
      glFormat,
      FWidth,
      FHeight,
      FDepth);

    if gl.GetError <> GL_NO_ERROR then
    begin
      gl.ClearError;
      CurrentGLContext.GLStates.ActiveTextureEnabled[FHandle.Target] := False;
      GLSLogger.LogErrorFmt('Unable to create texture "%s"', [Self.Name]);
      Abort;
    end
    else
      FHandle.NotifyDataUpdated;
  end;
end;

procedure TGLTextureImageEx.CalcLODRange(out AFirstLOD, ALastLOD: Integer);
var
  I, MaxLODSize, MinLODSize, MaxLODZSize: Integer;
begin
  case FHandle.Target of
    ttTexture3D:
      begin
        MaxLODSize := CurrentGLContext.GLStates.Max3DTextureSize;
        MaxLODZSize := MaxLODSize;
      end;

    ttTextureCube:
      begin
        MaxLODSize := CurrentGLContext.GLStates.MaxCubeTextureSize;
        MaxLODZSize := 0;
      end;

    ttTexture1DArray,
      ttTexture2DArray,
      ttTextureCubeArray,
      ttTexture2DMultisampleArray:
      begin
        MaxLODSize := CurrentGLContext.GLStates.MaxTextureSize;
        MaxLODZSize := CurrentGLContext.GLStates.MaxArrayTextureSize;
      end;

  else
    begin
      MaxLODSize := CurrentGLContext.GLStates.MaxTextureSize;
      MaxLODZSize := 0;
    end;
  end;

  MinLODSize := 1;

  AFirstLOD := 0;

  for I := 0 to High(TGLImagePiramid) do
  begin
    if (FImage.LevelWidth[I] <= MaxLODSize)
      and (FImage.LevelHeight[I] <= MaxLODSize)
      and (FImage.LevelDepth[I] <= MaxLODZSize) then
      break;
    Inc(AFirstLOD);
  end;

  AFirstLOD := MinInteger(AFirstLOD, FImage.LevelCount - 1);
  ALastLOD := AFirstLOD;

  for I := AFirstLOD to High(TGLImagePiramid) do
  begin
    if (FImage.LevelWidth[I] < MinLODSize)
      or (FImage.LevelHeight[I] < MinLODSize) then
      break;
    Inc(ALastLOD);
  end;
  ALastLOD := MinInteger(ALastLOD, FImage.LevelCount - 1);
end;

procedure TGLTextureImageEx.StreamTransfer;
var
  LImage: TFriendlyImage;
  bContinueStreaming: Boolean;
  OldBaseLevel, level: Integer;
  newTime: Double;
  glInternalFormat: Cardinal;
  transferMethod: 0..3;
begin
  LImage := TFriendlyImage(FImage);
  OldBaseLevel := FBaseLevel;
  CalcLODRange(FBaseLevel, FMaxLevel);

  // Select transfer method
  if FImage.IsCompressed then
    transferMethod := 1
  else
    transferMethod := 0;
  if gl.EXT_direct_state_access then
    transferMethod := transferMethod + 2;

  bContinueStreaming := False;
  for level := FMaxLevel downto FBaseLevel do
  begin

    case LImage.LevelStreamingState[level] of

      ssKeeping:
        begin
          if FBaseLevel < Level then
            FBaseLevel := FMaxLevel;
          LImage.LevelStreamingState[Level] := ssLoading;
          LImage.DoStreaming;
          bContinueStreaming := True;
        end;

      ssLoading:
        begin
          LImage.DoStreaming;
          bContinueStreaming := True;
          if FBaseLevel < Level then
            FBaseLevel := FMaxLevel;
        end;

      ssLoaded:
        begin
          LImage.LevelPixelBuffer[Level].AllocateHandle;
          LImage.LevelPixelBuffer[Level].Bind;
          glInternalFormat := InternalFormatToOpenGLFormat(FInternalFormat);
          case transferMethod of
            0: gl.TexImage2D(GL_TEXTURE_2D, Level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.ColorFormat, FImage.DataType, nil);
            1: gl.CompressedTexImage2D(GL_TEXTURE_2D, Level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.LevelSizeInByte[Level], nil);
            2: gl.TextureImage2D(FHandle.Handle, GL_TEXTURE_2D, Level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.ColorFormat, FImage.DataType, nil);
            3: gl.CompressedTextureImage2D(FHandle.Handle, GL_TEXTURE_2D, Level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.LevelSizeInByte[Level], nil);
          end;
          LImage.LevelPixelBuffer[Level].UnBind;
          LImage.LevelStreamingState[Level] := ssTransfered;
          GLSLogger.LogDebug(Format('Texture "%s" level %d loaded', [Name, Level]));
        end;

      ssTransfered:
        begin
          if LImage.LevelPixelBuffer[Level].IsAllocatedForContext then
            LImage.LevelPixelBuffer[Level].DestroyHandle;
          FBaseLevel := Level;
        end;
    end; // of case

    if bContinueStreaming then
      break;
  end; // for level

  if bContinueStreaming then
  begin
    gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, FMaxLevel);
    gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, FBaseLevel);
  end;


  // Smooth transition between levels
  if Assigned(FApplicableSampler) then
  with FApplicableSampler do
  begin
    newTime := AppTime;
    if FLODBiasFract > 0 then
      FLODBiasFract := FLODBiasFract - 0.05 * (newTime - FLastTime)
    else if FLODBiasFract < 0 then
      FLODBiasFract := 0;
    FLastTime := newTime;
    if OldBaseLevel > FBaseLevel then
      FLODBiasFract := FLODBiasFract + (OldBaseLevel - FBaseLevel);

    if FApplicableSampler.IsValid then
      gl.SamplerParameterf(FApplicableSampler.Handle.Handle,
        GL_TEXTURE_LOD_BIAS, FLODBias + FLODBiasFract)
    else
      // To refrash texture parameters when sampler object not supported
      FLastSampler := nil;
  end;
end;

class function TGLTextureImageEx.FriendlyName: string;
begin
  Result := 'Texture Image';
end;

procedure TGLTextureImageEx.PrepareImage;
const
  cAlphaProc: array[TGLTextureImageAlpha] of TImageAlphaProc =
    (
    nil,
    ImageAlphaFromIntensity,
    ImageAlphaSuperBlackTransparent,
    ImageAlphaLuminance,
    ImageAlphaLuminanceSqrt,
    ImageAlphaOpaque,
    ImageAlphaTopLeftPointColorTransparent,
    ImageAlphaInverseLuminance,
    ImageAlphaInverseLuminanceSqrt,
    ImageAlphaBottomRightPointColorTransparent
    );

var
  ext, filename: string;
  BaseImageClass: TGLBaseImageClass;
  LPicture: TPicture;
  LGraphic: TGraphic;
  LImage: TGLImage;
  level: Integer;
  glColorFormat, glDataType: Cardinal;
  bReadFromSource: Boolean;
  LStream: TStream;
  ptr: PByte;

  procedure ReplaceImageClass;
  begin
    if not (FImage is TGLImage) then
    begin
      LImage := TGLImage.Create;
      LImage.Assign(FImage);
      FImage.Destroy;
      FImage := LImage;
    end
    else
      LImage := TGLImage(FImage);
  end;

begin
  if not Assigned(FImage) then
  begin
    try
      SetExeDirectory;
      bReadFromSource := True;

      if FInternallyStored and not IsDesignTime then
      begin
        filename := Name+'.image';
        if FileStreamExists(filename) then
        begin
          FImage := TGLImage.Create;
          FImage.ResourceName := filename;
          TFriendlyImage(FImage).LoadHeader;
          if not FUseStreaming then
          begin
            ReallocMem(TFriendlyImage(FImage).fData, FImage.DataSize);
            for level := FImage.LevelCount - 1 downto 0 do
            begin
              LStream := TFileStream.Create(filename + IntToHex(level, 2), fmOpenRead);
              ptr := PByte(TFriendlyImage(FImage).GetLevelAddress(level));
              LStream.Read(ptr^, FImage.LevelSizeInByte[level]);
              LStream.Destroy;
            end;
          end;
          bReadFromSource := False;
        end
        else
        begin
          FInternallyStored := False;
          FUseStreaming := False;
        end;
      end;

      if bReadFromSource then
      begin
        if (Length(FSourceFile) > 0) and FileStreamExists(FSourceFile) then
        begin
          // At first check moder image file loaders
          ext := ExtractFileExt(FSourceFile);
          System.Delete(ext, 1, 1);
          BaseImageClass := GetRasterFileFormats.FindExt(ext);

          if Assigned(BaseImageClass) then
          begin
            FImage := BaseImageClass.Create;
            FImage.LoadFromFile(FSourceFile);
          end
          else
          begin
            // Check old loaders
            FImage := TGLImage.Create;
            if ApplicationFileIODefined then
            begin
              LGraphic := CreateGraphicFromFile(FSourceFile);
              FImage.Assign(LGraphic);
              LGraphic.Free;
            end
            else
            begin
              LPicture := TPicture.Create;
              LPicture.LoadFromFile(FSourceFile);
              FImage.Assign(LPicture.Graphic);
              LPicture.Destroy;
            end;
          end;

          if FInternalFormat <> FImage.InternalFormat then
          begin
            ReplaceImageClass;
            FindCompatibleDataFormat(FInternalFormat, glColorFormat, glDataType);
            TGLImage(FImage).SetColorFormatDataType(glColorFormat, glDataType);
            TFriendlyImage(FImage).fInternalFormat := FInternalFormat;
          end;

          if (ImageAlpha <> tiaDefault)
            or (FImageBrightness <> 1.0)
            or (FImageGamma <> 1.0) then
          begin
            ReplaceImageClass;
            for level := 0 to FImage.LevelCount - 1 do
            begin
              AlphaGammaBrightCorrection(
                TFriendlyImage(FImage).GetLevelAddress(level),
                FImage.ColorFormat,
                FImage.DataType,
                FImage.LevelWidth[level],
                FImage.LevelHeight[level],
                cAlphaProc[ImageAlpha],
                FImageBrightness,
                FImageGamma);
            end;
          end
          else if FHeightToNormalScale <> 1.0 then
          begin
            ReplaceImageClass;
  //          HeightToNormalMap();
  {$Message Hint 'TGLTextureImageEx.HeightToNormalScale not yet implemented' }
          end;

          case FMipGenMode of
            mgmNoMip:
              FImage.UnMipmap;

            mgmLeaveExisting, mgmOnFly: ;

            mgmBoxFilter:
              FImage.GenerateMipmap(ImageBoxFilter);

            mgmTriangleFilter:
              FImage.GenerateMipmap(ImageTriangleFilter);

            mgmHermiteFilter:
              FImage.GenerateMipmap(ImageHermiteFilter);

            mgmBellFilter:
              FImage.GenerateMipmap(ImageBellFilter);

            mgmSplineFilter:
              FImage.GenerateMipmap(ImageSplineFilter);

            mgmLanczos3Filter:
              FImage.GenerateMipmap(ImageLanczos3Filter);

            mgmMitchellFilter:
              FImage.GenerateMipmap(ImageMitchellFilter);
          end;

          // Store cooked image
          if FInternallyStored and IsDesignTime then
          begin
            filename := Name+'.image';
            FImage.ResourceName := filename;
            TFriendlyImage(FImage).SaveHeader;
            for level := FImage.LevelCount - 1 downto 0 do
            begin
              LStream := TFileStream.Create(filename + IntToHex(level, 2),
                fmOpenWrite or fmCreate);
              ptr := PByte(TFriendlyImage(FImage).GetLevelAddress(level));
              LStream.Write(ptr^, FImage.LevelSizeInByte[level]);
              LStream.Destroy;
            end;
          end;

        end
        else
        begin // no SourceFile
          FImage := TGLImage.Create;
          FImage.SetErrorImage;
          GLSLogger.LogErrorFmt('Source file of texture "%s" image not found',
            [Self.Name]);
        end;
      end; // if bReadFromSource

    except
      on E: Exception do
      begin
        FImage.Free;
        FImage := TGLImage.Create;
        FImage.SetErrorImage;
        if IsDesignTime then
          InformationDlg(Self.Name + ' - ' + E.ClassName + ': ' + E.Message)
        else
          GLSLogger.LogError(Self.Name + ' - ' + E.ClassName + ': ' +
            E.Message);
      end;
    end;
  end; // of not Assigned
end;

procedure TGLTextureImageEx.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name := ReadString;
      FDefferedInit := ReadBoolean;
      FInternalFormat := TGLInternalFormat(ReadInteger);
      FCompression := TGLTextureCompression(ReadInteger);
      FImageAlpha := TGLTextureImageAlpha(ReadInteger);
      FImageBrightness := ReadFloat;
      FImageBrightness := ReadFloat;
      FImageGamma := ReadFloat;
      FHeightToNormalScale := ReadFloat;
      FSourceFile := ReadString;
      FInternallyStored := ReadBoolean;
      FMipGenMode := TMipmapGenerationMode(ReadInteger);
      FUseStreaming := ReadBoolean;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TGLTextureImageEx.SetCompression(const AValue: TGLTextureCompression);
begin
  if AValue <> FCompression then
  begin
    FCompression := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLTextureImageEx.SetImageAlpha(const AValue: TGLTextureImageAlpha);
begin
  if FImageAlpha <> AValue then
  begin
    FImageAlpha := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TGLTextureImageEx.SetImageBrightness(const AValue: Single);
begin
  if FImageBrightness <> AValue then
  begin
    FImageBrightness := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TGLTextureImageEx.SetImageGamma(const AValue: Single);
begin
  if FImageGamma <> AValue then
  begin
    FImageGamma := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TGLTextureImageEx.SetInternalFormat(const AValue: TGLInternalFormat);
begin
  if AValue <> FInternalFormat then
  begin
    FInternalFormat := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TGLTextureImageEx.SetInternallyStored(const AValue: Boolean);
begin
  if FInternallyStored <> AValue then
  begin
    FInternallyStored := AValue;
    if not AValue then
      FUseStreaming := AValue
    else
      FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TGLTextureImageEx.SetMipGenMode(const AValue: TMipmapGenerationMode);
begin
  if FMipGenMode <> AValue then
  begin
    FMipGenMode := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TGLTextureImageEx.SetNormalMapScale(const AValue: Single);
begin
  if AValue <> FHeightToNormalScale then
  begin
    FHeightToNormalScale := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLTextureImageEx.SetSourceFile(AValue: string);
begin
  FixPathDelimiter(AValue);
  if FSourceFile <> AValue then
  begin
    FSourceFile := AValue;
    FUseStreaming := False;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TGLTextureImageEx.SetUseStreaming(const AValue: Boolean);
begin
  if AValue <> FUseStreaming then
  begin
    if AValue then
    begin
      if not Assigned(FImage) then
        exit;
      if FImage.LevelCount = 1 then
      begin
        if IsDesignTime then
          InformationDlg('Image must be more than one level');
        exit;
      end;
      FInternallyStored := True;
    end;
    FUseStreaming := AValue;
    NotifyChange(Self);
  end;
end;

function TGLTextureImageEx.StoreBrightness: Boolean;
begin
  Result := (FImageBrightness <> 1.0);
end;

function TGLTextureImageEx.StoreGamma: Boolean;
begin
  Result := (FImageGamma <> 1.0);
end;

function TGLTextureImageEx.StoreNormalMapScale: Boolean;
begin
  Result := (FHeightToNormalScale <> cDefaultNormalMapScale);
end;

procedure TGLTextureImageEx.UnApply(var ARci: TGLRenderContextInfo);
begin
  ARci.GLStates.ActiveTextureEnabled[FHandle.Target] := False;
end;

procedure TGLTextureImageEx.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    WriteInteger(Integer(FInternalFormat));
    WriteInteger(Integer(FCompression));
    WriteInteger(Integer(FImageAlpha));
    WriteFloat(FImageBrightness);
    WriteFloat(FImageBrightness);
    WriteFloat(FImageGamma);
    WriteFloat(FHeightToNormalScale);
    WriteString(FSourceFile);
    WriteBoolean(FInternallyStored);
    WriteInteger(Integer(FMipGenMode));
    WriteBoolean(FUseStreaming);
  end;
end;

procedure TGLTextureSampler.Apply(var ARci: TGLRenderContextInfo);
begin
  if FIsValid then
    ARci.GLStates.SamplerBinding[ARci.GLStates.ActiveTexture] := FHandle.Handle;
end;

procedure TGLTextureSampler.Assign(Source: TPersistent);
var
  LSampler: TGLTextureSampler;
begin
  if Source is TGLTextureSampler then
  begin
    LSampler := TGLTextureSampler(Source);
    FMinFilter := LSampler.FMinFilter;
    FMagFilter := LSampler.FMagFilter;
    FFilteringQuality := LSampler.FFilteringQuality;
    FLODBias := LSampler.FLODBias;
    FLODBiasFract := 0;
    FBorderColor.Assign(LSampler.FBorderColor);
    FWrap := LSampler.FWrap;
    FCompareMode := LSampler.FCompareMode;
    FCompareFunc := LSampler.FCompareFunc;
    FDecodeSRGB := LSampler.FDecodeSRGB;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TGLTextureSampler.Create(AOwner: TXCollection);
begin
  inherited;
  FDefferedInit := False;
  FHandle := TGLSamplerHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FMagFilter := maLinear;
  FMinFilter := miLinearMipMapLinear;
  FFilteringQuality := tfAnisotropic;
  FLODBias := 0;
  FLODBiasFract := 0;
  FWrap[0] := twRepeat;
  FWrap[1] := twRepeat;
  FWrap[2] := twRepeat;
  FBorderColor := TGLColor.CreateInitialized(Self, clrTransparent);
  FCompareMode := tcmNone;
  FCompareFunc := cfLequal;
  FDecodeSRGB := True;
  Name := TGLMatLibComponents(AOwner).MakeUniqueName('Sampler');
end;

destructor TGLTextureSampler.Destroy;
begin
  FHandle.Destroy;
  FBorderColor.Destroy;
  inherited;
end;

function TGLTextureSampler.GetWrap(Index: Integer): TGLSeparateTextureWrap;
begin
  Result := FWrap[Index];
end;

procedure TGLTextureSampler.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TGLTextureSampler.DoOnPrepare(Sender: TGLContext);
var
  ID: Cardinal;
begin
  if IsDesignTime and FDefferedInit then
    exit;
  try
    if FHandle.IsSupported then
    begin
      FHandle.AllocateHandle;
      ID := FHandle.Handle;
      if FHandle.IsDataNeedUpdate then
        with Sender.GL do
        begin
          SamplerParameterfv(ID, GL_TEXTURE_BORDER_COLOR, FBorderColor.AsAddress);
          SamplerParameteri(ID, GL_TEXTURE_WRAP_S, cTextureWrapMode[FWrap[0]]);
          SamplerParameteri(ID, GL_TEXTURE_WRAP_T, cTextureWrapMode[FWrap[1]]);
          SamplerParameteri(ID, GL_TEXTURE_WRAP_R, cTextureWrapMode[FWrap[2]]);
          SamplerParameterf(ID, GL_TEXTURE_LOD_BIAS, FLODBias + FLODBiasFract);
          SamplerParameteri(ID, GL_TEXTURE_MIN_FILTER,
            cTextureMinFilter[FMinFilter]);
          SamplerParameteri(ID, GL_TEXTURE_MAG_FILTER,
            cTextureMagFilter[FMagFilter]);

          if EXT_texture_filter_anisotropic then
          begin
            if FFilteringQuality = tfAnisotropic then
              SamplerParameteri(ID, GL_TEXTURE_MAX_ANISOTROPY_EXT,
                CurrentGLContext.GLStates.MaxTextureAnisotropy)
            else
              SamplerParameteri(ID, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
          end;

          SamplerParameteri(ID, GL_TEXTURE_COMPARE_MODE,
            cTextureCompareMode[FCompareMode]);
          SamplerParameteri(ID, GL_TEXTURE_COMPARE_FUNC,
            cGLComparisonFunctionToGLEnum[FCompareFunc]);

          if EXT_texture_sRGB_decode then
          begin
            if FDecodeSRGB then
              SamplerParameteri(ID, GL_TEXTURE_SRGB_DECODE_EXT, GL_DECODE_EXT)
            else
              SamplerParameteri(ID, GL_TEXTURE_SRGB_DECODE_EXT,
                GL_SKIP_DECODE_EXT);
          end;
{$IFDEF USE_OPENGL_DEBUG}
          CheckError;
{$ENDIF}

          FHandle.NotifyDataUpdated;
        end;
      FIsValid := True;
    end
    else
      FIsValid := False;

  except
    FIsValid := False;
  end;
end;

class function TGLTextureSampler.FriendlyName: string;
begin
  Result := 'Texture Sampler';
end;

procedure TGLTextureSampler.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name := ReadString;
      FDefferedInit := ReadBoolean;
      FMinFilter := TGLMinFilter(ReadInteger);
      FMagFilter := TGLMagFilter(ReadInteger);
      FFilteringQuality := TGLTextureFilteringQuality(ReadInteger);
      FLODBias := ReadInteger;
      FWrap[0] := TGLSeparateTextureWrap(ReadInteger);
      FWrap[1] := TGLSeparateTextureWrap(ReadInteger);
      FWrap[2] := TGLSeparateTextureWrap(ReadInteger);
      Read(FBorderColor.AsAddress^, SizeOf(TColorVector));
      FCompareMode := TGLTextureCompareMode(ReadInteger);
      FCompareFunc := TGLDepthFunction(ReadInteger);
      FDecodeSRGB := ReadBoolean;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TGLTextureSampler.SetBorderColor(const AValue: TGLColor);
begin
  FBorderColor.Assign(AValue);
  NotifyChange(Self);
end;

procedure TGLTextureSampler.SetCompareFunc(AValue: TGLDepthFunction);
begin
  if FCompareFunc <> AValue then
  begin
    FCompareFunc := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLTextureSampler.SetCompareMode(AValue: TGLTextureCompareMode);
begin
  if FCompareMode <> AValue then
  begin
    FCompareMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLTextureSampler.SetDecodeSRGB(AValue: Boolean);
begin
  if FDecodeSRGB <> AValue then
  begin
    FDecodeSRGB := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLTextureSampler.SetFilteringQuality(
  AValue: TGLTextureFilteringQuality);
begin
  if FFilteringQuality <> AValue then
  begin
    FFilteringQuality := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLTextureSampler.SetLODBias(AValue: Integer);
begin
  if FLODBias <> AValue then
  begin
    FLODBias := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLTextureSampler.SetMagFilter(AValue: TGLMagFilter);
begin
  if FMagFilter <> AValue then
  begin
    FMagFilter := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLTextureSampler.SetMinFilter(AValue: TGLMinFilter);
begin
  if FMinFilter <> AValue then
  begin
    FMinFilter := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLTextureSampler.SetWrap(Index: Integer;
  AValue: TGLSeparateTextureWrap);
begin
  if FWrap[Index] <> AValue then
  begin
    FWrap[Index] := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLTextureSampler.UnApply(var ARci: TGLRenderContextInfo);
begin
  if FHandle.IsSupported then
    with ARci.GLStates do
      SamplerBinding[ActiveTexture] := 0;
end;

procedure TGLTextureSampler.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    WriteInteger(Integer(FMinFilter));
    WriteInteger(Integer(FMagFilter));
    WriteInteger(Integer(FFilteringQuality));
    WriteInteger(FLODBias);
    WriteInteger(Integer(FWrap[0]));
    WriteInteger(Integer(FWrap[1]));
    WriteInteger(Integer(FWrap[2]));
    Write(FBorderColor.AsAddress^, SizeOf(TColorVector));
    WriteInteger(Integer(FCompareMode));
    WriteInteger(Integer(FCompareFunc));
    WriteBoolean(FDecodeSRGB);
  end;
end;

{ TVXTextureCombiner }
procedure TGLTextureCombiner.Assign(Source: TPersistent);
var
  LCombiner: TGLTextureCombiner;
begin
  if Source is TGLTextureCombiner then
  begin
    LCombiner := TGLTextureCombiner(Source);
    FScript.Assign(LCombiner.FScript);
  end;
  inherited;
end;

constructor TGLTextureCombiner.Create(AOwner: TXCollection);
begin
  inherited;
  FDefferedInit := False;
  FHandle := TGLVirtualHandle.Create;
  FHandle.OnAllocate := DoAllocate;
  FHandle.OnDestroy := DoDeallocate;
  FHandle.OnPrapare := DoOnPrepare;
  FScript := TStringList.Create;
  FScript.OnChange := NotifyChange;
  FIsValid := True;
  Name := TGLMatLibComponents(AOwner).MakeUniqueName('Combiner');
end;

destructor TGLTextureCombiner.Destroy;
begin
  FHandle.Destroy;
  FScript.Destroy;
  inherited;
end;

procedure TGLTextureCombiner.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TGLTextureCombiner.DoAllocate(Sender: TGLVirtualHandle;
  var handle: Cardinal);
begin
  handle := 1;
end;

procedure TGLTextureCombiner.DoDeallocate(Sender: TGLVirtualHandle;
  var handle: Cardinal);
begin
  handle := 0;
end;

procedure TGLTextureCombiner.DoOnPrepare(Sender: TGLContext);
begin
  if IsDesignTime and FDefferedInit then
    exit;
  if Sender.gl.ARB_multitexture then
  begin
    FHandle.AllocateHandle;
    if FHandle.IsDataNeedUpdate then
    begin
      try
        FCommandCache := GetTextureCombiners(FScript);
        FIsValid := True;
      except
        on E: Exception do
        begin
          FIsValid := False;
          if IsDesignTime then
            InformationDlg(E.ClassName + ': ' + E.Message)
          else
            GLSLogger.LogError(E.ClassName + ': ' + E.Message);
        end;
      end;
      FHandle.NotifyDataUpdated;
    end;
  end
  else
    FIsValid := False;
end;

class function TGLTextureCombiner.FriendlyName: string;
begin
  Result := 'Texture Combiner';
end;

procedure TGLTextureCombiner.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name := ReadString;
      FDefferedInit := ReadBoolean;
      FScript.Text := ReadString;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TGLTextureCombiner.SetScript(AValue: TStringList);
begin
  FScript.Assign(AValue);
  NotifyChange(Self);
end;

procedure TGLTextureCombiner.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    WriteString(FScript.Text);
  end;
end;


{ TVXLibMaterialEx }
procedure TGLLibMaterialEx.Apply(var ARci: TGLRenderContextInfo);
var
  LevelReady: array[TGLMaterialLevel] of Boolean;
  L, MaxLevel: TGLMaterialLevel;
begin
  if Assigned(FNextPass) then
  begin
    FNextPass := nil;
    exit;
  end;

  FHandle.AllocateHandle;
  if FHandle.IsDataNeedUpdate then
  begin
    // Other value than mlAuto indicates a level failure
    // Need remove deffered initialization and reinitialize used resources
    if not IsDesignTime and (FSelectedLevel <> mlAuto) then
      RemoveDefferedInit;
    // Level selection
    LevelReady[mlFixedFunction] := FFixedFunc.Enabled;
    LevelReady[mlMultitexturing] := FMultitexturing.Enabled and
      FMultitexturing.IsValid;
    LevelReady[mlSM3] := FSM3.Enabled and FSM3.IsValid;
    LevelReady[mlSM4] := FSM4.Enabled and FSM4.IsValid;
    LevelReady[mlSM5] := FSM5.Enabled and FSM5.IsValid;

    if FApplicableLevel = mlAuto then
      MaxLevel := mlSM5
    else
      MaxLevel := FApplicableLevel;

    FSelectedLevel := mlAuto;
    for L := MaxLevel downto mlFixedFunction do
      if LevelReady[L] then
      begin
        FSelectedLevel := L;
        break;
      end;

    FStoreAmalgamating := ARci.amalgamating;
    ARci.amalgamating := True;
    FHandle.NotifyDataUpdated;
  end;

  ARci.currentMaterialLevel := FSelectedLevel;

  case FSelectedLevel of
    mlAuto: ; // No one level can be used. Worst case.

    mlFixedFunction:
      begin
        FFixedFunc.Apply(ARci);
      end;

    mlMultitexturing:
      begin
        if LevelReady[mlFixedFunction] then
          FFixedFunc.Apply(ARci);
        FMultitexturing.Apply(ARci);
      end;

    mlSM3:
      begin
        if LevelReady[mlFixedFunction] then
          FFixedFunc.Apply(ARci);
        FSM3.Apply(ARci);
      end;

    mlSM4:
      begin
        if LevelReady[mlFixedFunction] then
          FFixedFunc.Apply(ARci);
        FSM4.Apply(ARci);
      end;

    mlSM5:
      begin
        if LevelReady[mlFixedFunction] then
          FFixedFunc.Apply(ARci);
        FSM5.Apply(ARci);
      end;
  end;
end;

procedure TGLLibMaterialEx.Assign(Source: TPersistent);
var
  LMaterial: TGLLibMaterialEx;
begin
  if Source is TGLLibMaterialEx then
  begin
    LMaterial := TGLLibMaterialEx(Source);
    FFixedFunc.Assign(LMaterial.FFixedFunc);
    FMultitexturing.Assign(LMaterial.FMultitexturing);
    FSM3.Assign(LMaterial.FSM3);
    FSM4.Assign(LMaterial.FSM4);
    FSM5.Assign(LMaterial.FSM5);
    FApplicableLevel := LMaterial.FApplicableLevel;
    NotifyChange(Self);
  end;
  inherited;
end;

function TGLLibMaterialEx.Blended: Boolean;
begin
  Result := FFixedFunc.Blended;
end;

constructor TGLLibMaterialEx.Create(ACollection: TCollection);
begin
  inherited;
  FHandle := TGLVirtualHandle.Create;
  FHandle.OnAllocate := DoAllocate;
  FHandle.OnDestroy := DoDeallocate;
  FHandle.OnPrapare := DoOnPrepare;
  FApplicableLevel := mlAuto;
  FSelectedLevel := mlAuto;
  FFixedFunc := TGLFixedFunctionProperties.Create(Self);
  FMultitexturing := TGLMultitexturingProperties.Create(Self);
  FSM3 := TGLShaderModel3.Create(Self);
  FSM4 := TGLShaderModel4.Create(Self);
  FSM5 := TGLShaderModel5.Create(Self);
end;

type
  TGLFreindlyMaterial = class(TGLMaterial);

destructor TGLLibMaterialEx.Destroy;
var
  I: Integer;
  LUser: TObject;
begin
  FHandle.Destroy;
  FFixedFunc.Destroy;
  FMultitexturing.Destroy;
  FSM3.Destroy;
  FSM4.Destroy;
  FSM5.Destroy;
  for I := 0 to FUserList.Count - 1 do
  begin
    LUser := TObject(FUserList[i]);
    if LUser is TGLMaterial then
      TGLFreindlyMaterial(LUser).NotifyLibMaterialDestruction;
  end;
  inherited;
end;

procedure TGLLibMaterialEx.DoAllocate(Sender: TGLVirtualHandle;
  var handle: Cardinal);
begin
  handle := 1;
end;

procedure TGLLibMaterialEx.DoDeallocate(Sender: TGLVirtualHandle;
  var handle: Cardinal);
begin
  handle := 0;
end;

procedure TGLLibMaterialEx.DoOnPrepare(Sender: TGLContext);
begin
end;

procedure TGLLibMaterialEx.Loaded;
begin
  FFixedFunc.FTexProp.Loaded;
  FMultitexturing.Loaded;
  FSM3.Loaded;
  FSM4.Loaded;
  FSM5.Loaded;
end;

procedure TGLLibMaterialEx.NotifyChange(Sender: TObject);
begin
  inherited;
  FHandle.NotifyChangesOfData;
end;

procedure TGLLibMaterialEx.RemoveDefferedInit;
var
  I: Integer;
  ST: TGLShaderType;
begin
  if FFixedFunc.FTexProp.Enabled then
  begin
    if Assigned(FFixedFunc.FTexProp.FLibTexture) then
      FFixedFunc.FTexProp.FLibTexture.FDefferedInit := False;
    if Assigned(FFixedFunc.FTexProp.FLibSampler) then
      FFixedFunc.FTexProp.FLibSampler.FDefferedInit := False;
  end;

  if FMultitexturing.Enabled then
  begin
    if Assigned(FMultitexturing.FLibCombiner) then
    begin
      FMultitexturing.FLibCombiner.FDefferedInit := False;
      for I := 0 to 3 do
        if Assigned(FMultitexturing.FTexProps[I]) then
          with FMultitexturing.FTexProps[I] do
          begin
            if Assigned(FLibTexture) then
              FLibTexture.FDefferedInit := False;
            if Assigned(FLibSampler) then
              FLibSampler.FDefferedInit := False;
          end;
    end;
  end;

  if FSM3.Enabled then
  begin
    for ST := Low(TGLShaderType) to High(TGLShaderType) do
      if Assigned(FSM3.FShaders[ST]) then
        FSM3.FShaders[ST].FDefferedInit := False;
  end;

  if FSM4.Enabled then
  begin
    for ST := Low(TGLShaderType) to High(TGLShaderType) do
      if Assigned(FSM4.FShaders[ST]) then
        FSM4.FShaders[ST].FDefferedInit := False;
  end;

  if FSM5.Enabled then
  begin
    for ST := Low(TGLShaderType) to High(TGLShaderType) do
      if Assigned(FSM5.FShaders[ST]) then
        FSM5.FShaders[ST].FDefferedInit := False;
  end;

  CurrentGLContext.PrepareHandlesData;
end;

procedure TGLLibMaterialEx.SetMultitexturing(AValue:
  TGLMultitexturingProperties);
begin
  FMultitexturing.Assign(AValue);
end;

procedure TGLLibMaterialEx.SetFixedFunc(AValue: TGLFixedFunctionProperties);
begin
  FFixedFunc.Assign(AValue);
end;

procedure TGLLibMaterialEx.SetLevel(AValue: TGLMaterialLevel);
begin
  if FApplicableLevel <> AValue then
  begin
    FApplicableLevel := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLLibMaterialEx.SetSM3(AValue: TGLShaderModel3);
begin
  FSM3.Assign(AValue);
end;

procedure TGLLibMaterialEx.SetSM4(AValue: TGLShaderModel4);
begin
  FSM4.Assign(AValue);
end;

procedure TGLLibMaterialEx.SetSM5(AValue: TGLShaderModel5);
begin
  FSM5.Assign(AValue);
end;

function TGLLibMaterialEx.UnApply(var ARci: TGLRenderContextInfo): Boolean;

  procedure GetNextPass(AProp: TGLLibMaterialProperty);
  begin
    if Length(AProp.NextPass) > 0 then
      FNextPass :=
        TGLMaterialLibraryEx(GetMaterialLibrary).Materials.GetLibMaterialByName(AProp.NextPass)
    else
      FNextPass := nil;

    if FNextPass = Self then
    begin
      AProp.NextPass := '';
      FNextPass := nil;
    end;
  end;

begin
  if FStoreAmalgamating <> ARci.amalgamating then
    ARci.amalgamating := FStoreAmalgamating;

  if Assigned(FNextPass) then
  begin
    Result := FNextPass.UnApply(ARci);
    if Result then
      FNextPass.Apply(ARci)
    else
      FNextPass := nil;
    exit;
  end;

  case FSelectedLevel of
    mlFixedFunction:
      begin
        FFixedFunc.UnApply(ARci);
        GetNextPass(FFixedFunc);
      end;

    mlMultitexturing:
      begin
        if FFixedFunc.Enabled then
          FFixedFunc.UnApply(ARci);
        FMultitexturing.UnApply(ARci);
        GetNextPass(FMultitexturing);
      end;

    mlSM3:
      begin
        if FFixedFunc.Enabled then
          FFixedFunc.UnApply(ARci);
        FSM3.UnApply(ARci);
        GetNextPass(FSM3);
      end;

    mlSM4:
      begin
        if FFixedFunc.Enabled then
          FFixedFunc.UnApply(ARci);
        FSM4.UnApply(ARci);
        GetNextPass(FSM4);
      end;

    mlSM5:
      begin
        if FFixedFunc.Enabled then
          FFixedFunc.UnApply(ARci);
        FSM5.UnApply(ARci);
        GetNextPass(FSM5);
      end;
  else
    FNextPass := nil;
  end;
  ARci.GLStates.ActiveTexture := 0;

  Result := Assigned(FNextPass);
  if Result then
    FNextPass.Apply(ARCi);
end;

{ TVXMultitexturingProperties }
procedure TGLMultitexturingProperties.Apply(var ARci: TGLRenderContextInfo);
var
  N, U: Integer;
  LDir: TGLVector;
begin
  if FEnabled then
  begin
    if Assigned(FLibCombiner) and not FLibCombiner.FIsValid then
      exit;
    if Assigned(FLibAsmProg) and not FLibAsmProg.FIsValid then
      exit;

    U := 0;
    for N := 0 to High(FTexProps) do
    begin
      if Assigned(FTexProps[N]) and FTexProps[N].Enabled then
      begin
        ARci.GLStates.ActiveTexture := N;
        FTexProps[N].Apply(ARci);
        if Ord(FLightDir) = N+1 then
        begin
          LDir := ARci.GLStates.LightPosition[FLightSourceIndex];
          LDir := VectorTransform(LDir, ARci.PipelineTransformation.InvModelMatrix^);
          NormalizeVector(LDir);
          gl.TexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, @LDir);
        end;
        U := U or (1 shl N);
      end;
    end;

    if Assigned(FLibAsmProg) then
    begin
      FLibAsmProg.Handle.Bind;
      gl.Enable(GL_VERTEX_PROGRAM_ARB);
      if Assigned(GetMaterial.FOnAsmProgSetting) then
        GetMaterial.FOnAsmProgSetting(Self.FLibAsmProg, ARci);
    end;

    with GL, ARci.GLStates do
    begin
      if Assigned(FLibCombiner) and (Length(FLibCombiner.FCommandCache) > 0)
        then
      begin
        for N := 0 to High(FLibCombiner.FCommandCache) do
        begin
          ActiveTexture := FLibCombiner.FCommandCache[N].ActiveUnit;
          TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);
          TexEnvi(GL_TEXTURE_ENV,
            FLibCombiner.FCommandCache[N].Arg1,
            FLibCombiner.FCommandCache[N].Arg2);
        end;
      end;
      TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
      ActiveTexture := 0;

    end;

    XGL.BeginUpdate;
    if U > 3 then
      XGL.MapTexCoordToArbitrary(U)
    else if (FTexProps[0].Enabled)
      and (FTexProps[0].MappingMode = tmmUser) then
      if FTexProps[1].MappingMode = tmmUser then
        XGL.MapTexCoordToDual
      else
        XGL.MapTexCoordToMain
    else if FTexProps[1].MappingMode = tmmUser then
      XGL.MapTexCoordToSecond
    else
      XGL.MapTexCoordToMain;
    XGL.EndUpdate;

  end;
end;

constructor TGLMultitexturingProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FEnabled := False;
  FTextureMode := tmDecal;
  FLightDir := l2eNone;
  FLightSourceIndex := 0;
end;

destructor TGLMultitexturingProperties.Destroy;
begin
  if Assigned(FLibCombiner) then
    FLibCombiner.UnregisterUser(Self);
  if Assigned(FLibAsmProg) then
    FLibAsmProg.UnregisterUser(Self);
  FTexProps[0].Free;
  FTexProps[1].Free;
  FTexProps[2].Free;
  FTexProps[3].Free;
  inherited;
end;

function TGLMultitexturingProperties.GetLibCombinerName: string;
begin
  if Assigned(FLibCombiner) then
    Result := FLibCombiner.Name
  else
    Result := '';
end;

function TGLMultitexturingProperties.GetLibAsmProgName: string;
begin
  if Assigned(FLibAsmProg) then
    Result := FLibAsmProg.Name
  else
    Result := '';
end;

function TGLMultitexturingProperties.IsValid: Boolean;
var
  I: Integer;
begin
  Result := True;
  if Assigned(FLibCombiner) then
    Result := Result and FLibCombiner.IsValid;
  if Assigned(FLibAsmProg) then
    Result := Result and FLibAsmProg.IsValid;
  for I := 0 to High(FTexProps) do
    if Assigned(FTexProps[I]) and FTexProps[I].FEnabled then
      Result := Result and FTexProps[I].IsValid;
end;

procedure TGLMultitexturingProperties.Loaded;
var
  I: Integer;
begin
  SetLibCombinerName(FLibCombinerName);
  SetLibAsmProgName(FLibAsmProgName);
  for I := 0 to High(FTexProps) do
    if Assigned(FTexProps[I]) then
      FTexProps[I].Loaded;
end;

procedure TGLMultitexturingProperties.Notification(Sender: TObject; Operation:
  TOperation);
begin
  if Operation = opRemove then
  begin
    if Sender = FLibCombiner then
      FLibCombiner := nil;
    if Sender = FLibAsmProg then
      FLibAsmProg := nil;
  end;
  inherited;
end;

procedure TGLMultitexturingProperties.SetLibCombinerName(const AValue: string);
var
  LCombiner: TGLTextureCombiner;
begin
  if csLoading in GetMaterialLibraryEx.ComponentState then
  begin
    FLibCombinerName := AValue;
    exit;
  end;

  if Assigned(FLibCombiner) then
  begin
    if FLibCombiner.Name = AValue then
      exit;
    FLibCombiner.UnregisterUser(Self);
    FLibCombiner := nil;
  end;
  LCombiner := GetMaterialLibraryEx.Components.GetCombinerByName(AValue);
  if Assigned(LCombiner) then
  begin
    LCombiner.RegisterUser(Self);
    FLibCombiner := LCombiner;
  end;
  NotifyChange(Self);
end;

procedure TGLMultitexturingProperties.SetLightSourceIndex(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0
  else if AValue > 7 then
    AValue := 7;
  FLightSourceIndex := AValue;
end;

procedure TGLMultitexturingProperties.SetLibAsmProgName(const AValue: string);
var
  LProg: TGLASMVertexProgram;
begin
  if csLoading in GetMaterialLibraryEx.ComponentState then
  begin
    FLibAsmProgName := AValue;
    exit;
  end;

  if Assigned(FLibAsmProg) then
  begin
    if FLibAsmProg.Name = AValue then
      exit;
    FLibAsmProg.UnregisterUser(Self);
    FLibAsmProg := nil;
  end;
  LProg := GetMaterialLibraryEx.Components.GetAsmProgByName(AValue);
  if Assigned(LProg) then
  begin
    LProg.RegisterUser(Self);
    FLibAsmProg := LProg;
  end;
  NotifyChange(Self);
end;

function TGLMultitexturingProperties.GetTexProps(AIndex: Integer):
  TGLTextureProperties;
begin
  if not Assigned(FTexProps[AIndex]) then
    FTexProps[AIndex] := TGLTextureProperties.Create(Self);
  Result := FTexProps[AIndex];
end;

procedure TGLMultitexturingProperties.SetTexProps(AIndex: Integer;
  AValue: TGLTextureProperties);
begin
  FTexProps[AIndex].Assign(AValue);
end;

procedure TGLMultitexturingProperties.SetTextureMode(AValue: TGLTextureMode);
begin
  if AValue <> FTextureMode then
  begin
    FTextureMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLMultitexturingProperties.UnApply(var ARci: TGLRenderContextInfo);
var
  N: Integer;
begin
  for N := 0 to High(FTexProps) do
  begin
    if FTexProps[N].Enabled then
    begin
      ARci.GLStates.ActiveTexture := N;
      FTexProps[N].UnApply(ARci);
    end;
  end;
  ARci.GLStates.ActiveTexture := 0;

  if Assigned(FLibAsmProg) then
    gl.Disable(GL_VERTEX_PROGRAM_ARB);
end;

{ TVXTextureProperties }
procedure TGLTextureProperties.Apply(var ARci: TGLRenderContextInfo);
var
  glTarget: Cardinal;
begin
  if Assigned(FLibTexture) then
    begin
      FLibTexture.FApplicableSampler := FLibSampler;
      FLibTexture.Apply(ARci);

      // Apply swizzling if possible
      glTarget := DecodeTextureTarget(FLibTexture.Shape);
      if GL.ARB_texture_swizzle or GL.EXT_texture_swizzle then
      begin
        if FSwizzling.FSwizzles[0] <> FLibTexture.FSwizzles[0] then
        begin
          FLibTexture.FSwizzles[0] := FSwizzling.FSwizzles[0];
          gl.TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_R,
            cTextureSwizzle[FSwizzling.FSwizzles[0]]);
        end;
        if FSwizzling.FSwizzles[1] <> FLibTexture.FSwizzles[1] then
        begin
          FLibTexture.FSwizzles[1] := FSwizzling.FSwizzles[1];
          gl.TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_G,
            cTextureSwizzle[FSwizzling.FSwizzles[1]]);
        end;
        if FSwizzling.FSwizzles[2] <> FLibTexture.FSwizzles[2] then
        begin
          FLibTexture.FSwizzles[2] := FSwizzling.FSwizzles[2];
          gl.TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_B,
            cTextureSwizzle[FSwizzling.FSwizzles[2]]);
        end;
        if FSwizzling.FSwizzles[3] <> FLibTexture.FSwizzles[3] then
        begin
          FLibTexture.FSwizzles[3] := FSwizzling.FSwizzles[3];
          gl.TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_A,
            cTextureSwizzle[FSwizzling.FSwizzles[3]]);
        end;
      end;

      if Assigned(FLibSampler) then
      begin
        if FLibSampler.IsValid then
          FLibSampler.Apply(ARci)
        else if FLibTexture.FLastSampler <> FLibSampler then
        begin
          // Sampler object not supported, lets use texture states
          gl.TexParameterfv(glTarget, GL_TEXTURE_BORDER_COLOR,
            FLibSampler.BorderColor.AsAddress);
          gl.TexParameteri(glTarget, GL_TEXTURE_WRAP_S,
            cTextureWrapMode[FLibSampler.WrapX]);
          gl.TexParameteri(glTarget, GL_TEXTURE_WRAP_T,
            cTextureWrapMode[FLibSampler.WrapY]);
          gl.TexParameteri(glTarget, GL_TEXTURE_WRAP_R,
            cTextureWrapMode[FLibSampler.WrapZ]);
          gl.TexParameterf(glTarget, GL_TEXTURE_LOD_BIAS, FLibSampler.LODBias +
            FLibSampler.FLODBiasFract);
          gl.TexParameteri(glTarget, GL_TEXTURE_MIN_FILTER,
            cTextureMinFilter[FLibSampler.MinFilter]);
          gl.TexParameteri(glTarget, GL_TEXTURE_MAG_FILTER,
            cTextureMagFilter[FLibSampler.MagFilter]);

          if GL.EXT_texture_filter_anisotropic then
          begin
            if FLibSampler.FilteringQuality = tfAnisotropic then
              gl.TexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT,
                CurrentGLContext.GLStates.MaxTextureAnisotropy)
            else
              gl.TexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
          end;

          gl.TexParameteri(glTarget, GL_TEXTURE_COMPARE_MODE,
            cTextureCompareMode[FLibSampler.CompareMode]);
          gl.TexParameteri(glTarget, GL_TEXTURE_COMPARE_FUNC,
            cGLComparisonFunctionToGLEnum[FLibSampler.CompareFunc]);

          if GL.EXT_texture_sRGB_decode then
          begin
            if FLibSampler.sRGB_Encode then
              gl.TexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT, GL_DECODE_EXT)
            else
              gl.TexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT,
                GL_SKIP_DECODE_EXT);
          end;

          FLibTexture.FLastSampler := FLibSampler;
        end;
      end;

      if not FTextureMatrixIsIdentity and (MappingMode = tmmUser) then
        ARci.GLStates.SetGLTextureMatrix(FTextureMatrix);

      if ARci.currentMaterialLevel < mlSM3 then
      begin
        gl.TexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);
        ApplyMappingMode;
        if ARci.currentMaterialLevel = mlFixedFunction then
          XGL.MapTexCoordToMain;
      end;
    end;
end;

procedure TGLTextureProperties.ApplyMappingMode;
var
  R_Dim: Boolean;
begin
  begin
    R_Dim := GL.ARB_texture_cube_map or GL.EXT_texture3D;

    case MappingMode of

      tmmUser: ; // nothing to do, but checked first (common case)

      tmmObjectLinear:
        begin
          gl.TexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          gl.TexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          gl.TexGenfv(GL_S, GL_OBJECT_PLANE, @MappingSCoordinates.DirectVector);
          gl.TexGenfv(GL_T, GL_OBJECT_PLANE, @MappingTCoordinates.DirectVector);
          gl.Enable(GL_TEXTURE_GEN_S);
          gl.Enable(GL_TEXTURE_GEN_T);

          if R_Dim then
          begin
            gl.TexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            gl.TexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            gl.TexGenfv(GL_R, GL_OBJECT_PLANE, @MappingRCoordinates.DirectVector);
            gl.TexGenfv(GL_Q, GL_OBJECT_PLANE, @MappingQCoordinates.DirectVector);
            gl.Enable(GL_TEXTURE_GEN_R);
            gl.Enable(GL_TEXTURE_GEN_Q);
          end;
        end;

      tmmEyeLinear:
        begin
          gl.TexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
          gl.TexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
          // specify planes in eye space, not world space
          gl.MatrixMode(GL_MODELVIEW);
          gl.PushMatrix;
          gl.LoadIdentity;
          gl.TexGenfv(GL_S, GL_EYE_PLANE, @MappingSCoordinates.DirectVector);
          gl.TexGenfv(GL_T, GL_EYE_PLANE, @MappingTCoordinates.DirectVector);
          gl.Enable(GL_TEXTURE_GEN_S);
          gl.Enable(GL_TEXTURE_GEN_T);
          if R_Dim then
          begin
            gl.TexGenfv(GL_R, GL_EYE_PLANE, @MappingRCoordinates.DirectVector);
            gl.TexGenfv(GL_Q, GL_EYE_PLANE, @MappingQCoordinates.DirectVector);
            gl.Enable(GL_TEXTURE_GEN_R);
            gl.Enable(GL_TEXTURE_GEN_Q);
          end;
          gl.PopMatrix;
        end;

      tmmSphere:
        begin
          gl.TexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
          gl.TexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
          gl.Enable(GL_TEXTURE_GEN_S);
          gl.Enable(GL_TEXTURE_GEN_T);
        end;

      tmmCubeMapReflection, tmmCubeMapCamera:
        if R_Dim then
        begin
          gl.TexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
          gl.TexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
          gl.TexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
          gl.Enable(GL_TEXTURE_GEN_S);
          gl.Enable(GL_TEXTURE_GEN_T);
          gl.Enable(GL_TEXTURE_GEN_R);
        end;

      tmmCubeMapNormal, tmmCubeMapLight0:
        if R_Dim then
        begin
          gl.TexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
          gl.TexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
          gl.TexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
          gl.Enable(GL_TEXTURE_GEN_S);
          gl.Enable(GL_TEXTURE_GEN_T);
          gl.Enable(GL_TEXTURE_GEN_R);
        end;
    end;
  end;
end;

procedure TGLTextureProperties.Assign(Source: TPersistent);
var
  LTexProp: TGLTextureProperties;
begin
  if Source is TGLTextureProperties then
  begin
    LTexProp := TGLTextureProperties(Source);
    LibTextureName := LTexProp.LibTextureName;
    LibSamplerName := LTexProp.LibSamplerName;
    TextureOffset.Assign(LTexProp.TextureOffset);
    TextureScale.Assign(LTexProp.TextureScale);
    FTextureRotate := LTexProp.TextureRotate;
    FEnvColor.Assign(LTexProp.EnvColor);
    FMappingMode := LTexProp.MappingMode;
    MappingSCoordinates.Assign(LTexProp.MappingSCoordinates);
    MappingTCoordinates.Assign(LTexProp.MappingTCoordinates);
    MappingRCoordinates.Assign(LTexProp.MappingRCoordinates);
    MappingQCoordinates.Assign(LTexProp.MappingQCoordinates);
  end;
  inherited;
end;

procedure TGLTextureProperties.CalculateTextureMatrix;
begin
  if not (Assigned(FTextureOffset) or Assigned(FTextureScale)
    or StoreTextureRotate) then
  begin
    FTextureMatrixIsIdentity := True;
    exit;
  end;

  if TextureOffset.Equals(NullHmgVector)
    and TextureScale.Equals(XYZHmgVector)
    and not StoreTextureRotate then
    FTextureMatrixIsIdentity := True
  else
  begin
    FTextureMatrixIsIdentity := False;
    FTextureMatrix := CreateScaleAndTranslationMatrix(
      TextureScale.AsVector,
      TextureOffset.AsVector);
    if StoreTextureRotate then
      FTextureMatrix := MatrixMultiply(FTextureMatrix,
        CreateRotationMatrixZ(DegToRad(FTextureRotate)));
  end;
  FTextureOverride := False;
  NotifyChange(Self);
end;

constructor TGLTextureProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FTextureRotate := 0;
  FMappingMode := tmmUser;
  FTextureMatrix := IdentityHmgMatrix;
  FEnabled := False;
  FSwizzling := TGLTextureSwizzling.Create(Self);
  FEnvColor := TGLColor.CreateInitialized(Self, clrTransparent);
end;

destructor TGLTextureProperties.Destroy;
begin
  if Assigned(FLibSampler) then
    FLibSampler.UnregisterUser(Self);
  if Assigned(FLibTexture) then
    FLibTexture.UnregisterUser(Self);
  FTextureOffset.Free;
  FTextureScale.Free;
  FMapSCoordinates.Free;
  FMapTCoordinates.Free;
  FMapRCoordinates.Free;
  FMapQCoordinates.Free;
  FSwizzling.Destroy;
  FEnvColor.Destroy;
  inherited;
end;

function TGLTextureProperties.GetLibSamplerName: TGLMaterialComponentName;
begin
  if Assigned(FLibSampler) then
    Result := FLibSampler.Name
  else
    Result := '';
end;

function TGLTextureProperties.GetLibTextureName: TGLMaterialComponentName;
begin
  if Assigned(FLibTexture) then
    Result := FLibTexture.Name
  else
    Result := '';
end;

function TGLTextureProperties.GetMappingQCoordinates: TGLCoordinates4;
begin
  if not Assigned(FMapQCoordinates) then
    FMapQCoordinates := TGLCoordinates4.CreateInitialized(Self, WHmgVector,
      csVector);
  Result := FMapQCoordinates;
end;

function TGLTextureProperties.GetMappingRCoordinates: TGLCoordinates4;
begin
  if not Assigned(FMapRCoordinates) then
    FMapRCoordinates := TGLCoordinates4.CreateInitialized(Self, ZHmgVector,
      csVector);
  Result := FMapRCoordinates;
end;

function TGLTextureProperties.GetMappingSCoordinates: TGLCoordinates4;
begin
  if not Assigned(FMapSCoordinates) then
    FMapSCoordinates := TGLCoordinates4.CreateInitialized(Self, XHmgVector,
      csVector);
  Result := FMapSCoordinates;
end;

function TGLTextureProperties.GetMappingTCoordinates: TGLCoordinates4;
begin
  if not Assigned(FMapTCoordinates) then
    FMapTCoordinates := TGLCoordinates4.CreateInitialized(Self, YHmgVector,
      csVector);
  Result := FMapTCoordinates;
end;

function TGLTextureProperties.GetTextureOffset: TGLCoordinates;
begin
  if not Assigned(FTextureOffset) then
    FTextureOffset :=
      TGLCoordinates3.CreateInitialized(Self, NullHmgVector, csPoint);
  Result := FTextureOffset;
end;

function TGLTextureProperties.GetTextureScale: TGLCoordinates;
begin
  if not Assigned(FTextureScale) then
    FTextureScale :=
      TGLCoordinates3.CreateInitialized(Self, VectorMake(1, 1, 1, 1), csVector);
  Result := FTextureScale;
end;

function TGLTextureProperties.IsValid: Boolean;
begin
  if Assigned(FLibTexture) then
    Result := FLibTexture.IsValid
  else
    Result := False;
end;

procedure TGLTextureProperties.Loaded;
begin
  SetLibTextureName(FLibTextureName);
  SetLibSamplerName(FLibSamplerName);
  CalculateTextureMatrix;
end;

procedure TGLTextureProperties.Notification(Sender: TObject;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if Sender = FLibTexture then
      FLibTexture := nil
    else if Sender = FLibSampler then
      FLibSampler := nil;
  end;
end;

procedure TGLTextureProperties.NotifyChange(Sender: TObject);
begin
  inherited;
  if (Sender = FTextureOffset) or (Sender = FTextureScale) then
    CalculateTextureMatrix;
  if (Sender = FLibSampler) and Assigned(FLibTexture) then
    FLibTexture.FLastSampler := nil;
end;

procedure TGLTextureProperties.SetLibSamplerName(const AValue:
  TGLMaterialComponentName);
var
  LSampler: TGLTextureSampler;
begin
  if csLoading in GetMaterialLibraryEx.ComponentState then
  begin
    FLibSamplerName := AValue;
    exit;
  end;

  if Assigned(FLibSampler) then
  begin
    if FLibSampler.Name = AValue then
      exit;
    FLibSampler.UnregisterUser(Self);
    FLibSampler := nil;
  end;
  LSampler := GetMaterialLibraryEx.Components.GetSamplerByName(AValue);
  if Assigned(LSampler) then
  begin
    LSampler.RegisterUser(Self);
    FLibSampler := LSampler;
  end;
  NotifyChange(Self);
end;

procedure TGLTextureProperties.SetLibTextureName(const AValue:
  TGLMaterialComponentName);
var
  LTexture: TGLAbstractTexture;
begin
  if csLoading in GetMaterialLibraryEx.ComponentState then
  begin
    FLibTextureName := AValue;
    exit;
  end;

  if Assigned(FLibTexture) then
  begin
    if FLibTexture.Name = AValue then
      exit;
    FLibTexture.UnregisterUser(Self);
    FLibTexture := nil;
  end;

  LTexture := GetMaterialLibraryEx.Components.GetTextureByName(AValue);

  if Assigned(LTexture) then
  begin
    if LTexture is TGLFrameBufferAttachment then
    begin
      if TGLFrameBufferAttachment(LTexture).OnlyWrite then
      begin
        if IsDesignTime then
          InformationDlg('Can not use write only attachment as texture')
        else
          GLSLogger.LogErrorFmt('Attempt to use write only attachment "%s" as texture',
            [LTexture.Name]);
        NotifyChange(Self);
        exit;
      end;
    end;
    LTexture.RegisterUser(Self);
    FLibTexture := LTexture;
  end;
  NotifyChange(Self);
end;

procedure TGLTextureProperties.SetMappingMode(
  const AValue: TGLTextureMappingMode);
begin
  if AValue <> FMappingMode then
  begin
    FMappingMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLTextureProperties.SetMappingQCoordinates(
  const AValue: TGLCoordinates4);
begin
  MappingQCoordinates.Assign(AValue);
end;

procedure TGLTextureProperties.SetMappingRCoordinates(
  const AValue: TGLCoordinates4);
begin
  MappingRCoordinates.Assign(AValue);
end;

procedure TGLTextureProperties.SetMappingSCoordinates(
  const AValue: TGLCoordinates4);
begin
  MappingSCoordinates.Assign(AValue);
end;

procedure TGLTextureProperties.SetMappingTCoordinates(
  const AValue: TGLCoordinates4);
begin
  MappingTCoordinates.Assign(AValue);
end;

procedure TGLTextureProperties.SetSwizzling(const AValue: TGLTextureSwizzling);
begin
  FSwizzling.Assign(AValue);
end;

procedure TGLTextureProperties.SetTextureMatrix(const AValue: TGLMatrix);
begin
  FTextureMatrixIsIdentity := CompareMem(@AValue.V[0], @IdentityHmgMatrix.V[0],
    SizeOf(TGLMatrix));
  FTextureMatrix := AValue;
  FTextureOverride := True;
  NotifyChange(Self);
end;

procedure TGLTextureProperties.SetTextureOffset(const AValue: TGLCoordinates);
begin
  TextureOffset.Assign(AValue);
  CalculateTextureMatrix;
end;

procedure TGLTextureProperties.SetTextureRotate(AValue: Single);
begin
  if AValue <> FTextureRotate then
  begin
    FTextureRotate := AValue;
    CalculateTextureMatrix;
    NotifyChange(Self);
  end;
end;

procedure TGLTextureProperties.SetTextureScale(const AValue: TGLCoordinates);
begin
  TextureScale.Assign(AValue);
  CalculateTextureMatrix;
end;

function TGLTextureProperties.StoreMappingQCoordinates: Boolean;
begin
  if Assigned(FMapQCoordinates) then
    Result := not VectorEquals(FMapQCoordinates.AsVector, WHmgVector)
  else
    Result := false;
end;

function TGLTextureProperties.StoreMappingRCoordinates: Boolean;
begin
  if Assigned(FMapRCoordinates) then
    Result := not VectorEquals(FMapRCoordinates.AsVector, ZHmgVector)
  else
    Result := false;
end;

function TGLTextureProperties.StoreMappingSCoordinates: Boolean;
begin
  if Assigned(FMapSCoordinates) then
    Result := not VectorEquals(FMapSCoordinates.AsVector, XHmgVector)
  else
    Result := false;
end;

function TGLTextureProperties.StoreMappingTCoordinates: Boolean;
begin
  if Assigned(FMapTCoordinates) then
    Result := not VectorEquals(FMapTCoordinates.AsVector, YHmgVector)
  else
    Result := false;
end;

function TGLTextureProperties.StoreSwizzling: Boolean;
begin
  Result := FSwizzling.StoreSwizzle(0);
end;

function TGLTextureProperties.StoreTextureOffset: Boolean;
begin
  Result := Assigned(FTextureOffset);
end;

function TGLTextureProperties.StoreTextureRotate: Boolean;
begin
  Result := Abs(FTextureRotate) > EPSILON;
end;

function TGLTextureProperties.StoreTextureScale: Boolean;
begin
  Result := Assigned(FTextureScale);
end;

procedure TGLTextureProperties.SetEnvColor(const AValue:
  TGLColor);
begin
  FEnvColor.Assign(AValue);
  NotifyChange(Self);
end;

procedure TGLTextureProperties.UnApply(var ARci: TGLRenderContextInfo);
begin
  if Assigned(FLibTexture) then
  begin
    FLibTexture.UnApply(ARci);
    if Assigned(FLibSampler) then
      FLibSampler.UnApply(ARci);

    if ARci.currentMaterialLevel < mlSM3 then
    begin
      if not FTextureMatrixIsIdentity and (MappingMode = tmmUser) then
        ARci.GLStates.SetGLTextureMatrix(IdentityHmgMatrix);
      UnApplyMappingMode;
    end;
  end;
end;

procedure TGLTextureProperties.UnApplyMappingMode;
begin
  if MappingMode <> tmmUser then
    begin
      gl.Disable(GL_TEXTURE_GEN_S);
      gl.Disable(GL_TEXTURE_GEN_T);
      if gl.EXT_texture3D or gl.ARB_texture_cube_map then
      begin
        gl.Disable(GL_TEXTURE_GEN_R);
        gl.Disable(GL_TEXTURE_GEN_Q);
      end;
    end;
end;

{ TVXShaderEx }
procedure TGLShaderEx.Assign(Source: TPersistent);
var
  LShader: TGLShaderEx;
begin
  if Source is TGLShaderEx then
  begin
    LShader := TGLShaderEx(Source);
    FSource.Assign(LShader.Source);
    FShaderType := LShader.FShaderType;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TGLShaderEx.Create(AOwner: TXCollection);
const
  cShaderClasses: array[TGLShaderType] of TGLShaderHandleClass =
    (
    TGLVertexShaderHandle,
    TGLTessControlShaderHandle,
    TGLTessEvaluationShaderHandle,
    TGLGeometryShaderHandle,
    TGLFragmentShaderHandle
    );
var
  S: TGLShaderType;
begin
  inherited;
  FDefferedInit := False;
  for S := Low(TGLShaderType) to High(TGLShaderType) do
  begin
    FHandle[S] := cShaderClasses[S].Create;
    FHandle[S].OnPrapare := DoOnPrepare;
  end;
  FSource := TStringList.Create;
  FSource.OnChange := NotifyChange;
  FShaderType := shtVertex;
  FGeometryInput := gsInPoints;
  FGeometryOutput := gsOutPoints;
  FGeometryVerticesOut := 1;
  Name := TGLMatLibComponents(AOwner).MakeUniqueName('Shader');
end;

destructor TGLShaderEx.Destroy;
var
  S: TGLShaderType;
begin
  for S := Low(TGLShaderType) to High(TGLShaderType) do
    FHandle[S].Destroy;
  FSource.Destroy;
  inherited;
end;

procedure TGLShaderEx.NotifyChange(Sender: TObject);
var
  S: TGLShaderType;
begin
  for S := Low(TGLShaderType) to High(TGLShaderType) do
    FHandle[S].NotifyChangesOfData;

  if (Sender = FSource) and IsDesignTime and (Length(FSourceFile) > 0) then
    FSource.SaveToFile(FSourceFile);

  inherited;
end;

procedure TGLShaderEx.DoOnPrepare(Sender: TGLContext);
begin
  if not IsDesignTime and FDefferedInit then
    exit;
  try
    if FHandle[FShaderType].IsSupported then
    begin
      FHandle[FShaderType].AllocateHandle;
      if FHandle[FShaderType].IsDataNeedUpdate then
      begin
        SetExeDirectory;
        if (Length(FSourceFile) > 0) and FileStreamExists(FSourceFile) then
          FSource.LoadFromFile(FSourceFile);
        FHandle[FShaderType].ShaderSource(AnsiString(FSource.Text));
        FIsValid := FHandle[FShaderType].CompileShader;
        if IsDesignTime then
        begin
          FInfoLog := FHandle[FShaderType].InfoLog;
          if (Length(FInfoLog) = 0) and FIsValid then
            FInfoLog := 'Compilation successful';
        end
        else if FIsValid then
          GLSLogger.LogInfoFmt('Shader "%s" compilation successful - %s',
            [Name, FHandle[FShaderType].InfoLog])
        else
          GLSLogger.LogErrorFmt('Shader "%s" compilation failed - %s',
            [Name, FHandle[FShaderType].InfoLog]);
        FHandle[FShaderType].NotifyDataUpdated;
      end;
    end
    else
    begin
      FIsValid := False;
      if IsDesignTime then
        FInfoLog := 'Not supported by hardware';
    end;
  except
    on E: Exception do
    begin
      FIsValid := False;
      if IsDesignTime then
        InformationDlg(E.ClassName + ': ' + E.Message)
      else
        GLSLogger.LogError(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

class function TGLShaderEx.FriendlyName: string;
begin
  Result := 'GLSL Shader';
end;

function TGLShaderEx.GetHandle: TGLShaderHandle;
begin
  Result := FHandle[FShaderType];
end;

procedure TGLShaderEx.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name := ReadString;
      FDefferedInit := ReadBoolean;
      FSource.Text := ReadString;
      FSourceFile := ReadString;
      FShaderType := TGLShaderType(ReadInteger);
      FGeometryInput := TGLgsInTypes(ReadInteger);
      FGeometryOutput := TGLgsOutTypes(ReadInteger);
      FGeometryVerticesOut := ReadInteger;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TGLShaderEx.SetGeometryInput(AValue: TGLgsInTypes);
begin
  if AValue <> FGeometryInput then
  begin
    FGeometryInput := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLShaderEx.SetGeometryOutput(AValue: TGLgsOutTypes);
begin
  if AValue <> FGeometryOutput then
  begin
    FGeometryOutput := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLShaderEx.SetGeometryVerticesOut(AValue: TGLint);
begin
  if AValue < 1 then
    AValue := 1
  else if AValue > 1024 then
    AValue := 1024;

  if AValue <> FGeometryVerticesOut then
  begin
    FGeometryVerticesOut := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLShaderEx.SetShaderType(AValue: TGLShaderType);
begin
  if FShaderType <> AValue then
  begin
    FShaderType := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLShaderEx.SetSource(AValue: TStringList);
begin
  FSource.Assign(AValue);
end;

procedure TGLShaderEx.SetSourceFile(AValue: string);
begin
  FixPathDelimiter(AValue);
  if FSourceFile <> AValue then
  begin
    FSourceFile := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLShaderEx.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    if Length(FSourceFile) = 0 then
      WriteString(FSource.Text)
    else
      WriteString('');
    WriteString(FSourceFile);
    WriteInteger(Integer(FShaderType));
    WriteInteger(Integer(FGeometryInput));
    WriteInteger(Integer(FGeometryOutput));
    WriteInteger(FGeometryVerticesOut);
  end;
end;


{ TVXLibMaterialProperty }
function TGLLibMaterialProperty.GetMaterial: TGLLibMaterialEx;
begin
  if Owner is TGLLibMaterialEx then
    Result := TGLLibMaterialEx(Owner)
  else if Owner is TGLLibMaterialProperty then
    Result := TGLLibMaterialProperty(Owner).GetMaterial
  else
    Result := nil;
end;

function TGLLibMaterialProperty.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  if Owner is TGLBaseMaterialCollectionItem then
    Result := TGLBaseMaterialCollectionItem(Owner).GetMaterialLibrary
  else
    Result := GetMaterial.GetMaterialLibrary;
end;

function TGLLibMaterialProperty.GetMaterialLibraryEx: TGLMaterialLibraryEx;
begin
  if Owner is TGLBaseMaterialCollectionItem then
    Result := TGLBaseMaterialCollectionItem(Owner).GetMaterialLibraryEx
  else
    Result := TGLMaterialLibraryEx(GetMaterial.GetMaterialLibrary);
end;

procedure TGLLibMaterialProperty.SetNextPass(const AValue: TGLLibMaterialName);
begin
  if AValue <> FNextPassName then
  begin
    FNextPassName := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLLibMaterialProperty.Loaded;
begin
end;

procedure TGLLibMaterialProperty.NotifyChange(Sender: TObject);
var
  NA: IGLNotifyAble;
begin
  if Assigned(Owner) then
  begin
    if Supports(Owner, IGLNotifyAble, NA) then
      NA.NotifyChange(Self)
  end;
  if Assigned(OnNotifyChange) then
    OnNotifyChange(Self);
end;

procedure TGLLibMaterialProperty.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if Owner is TGLLibMaterialEx then
      GetMaterial.NotifyChange(Self);
  end;
end;


{ TVXLibMaterialsEx }
function TGLLibMaterialsEx.Add: TGLLibMaterialEx;
begin
  Result := (inherited Add) as TGLLibMaterialEx;
end;

constructor TGLLibMaterialsEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TGLLibMaterialEx);
end;

function TGLLibMaterialsEx.FindItemID(ID: Integer): TGLLibMaterialEx;
begin
  Result := (inherited FindItemID(ID)) as TGLLibMaterialEx;
end;

function TGLLibMaterialsEx.GetItems(AIndex: Integer): TGLLibMaterialEx;
begin
  Result := TGLLibMaterialEx(inherited Items[AIndex]);
end;

function TGLLibMaterialsEx.GetLibMaterialByName(
  const AName: string): TGLLibMaterialEx;
var
  LMaterial: TGLAbstractLibMaterial;
begin
  LMaterial := GetMaterial(AName);
  if Assigned(LMaterial) and (LMaterial is TGLLibMaterialEx) then
    Result := TGLLibMaterialEx(LMaterial)
  else
    Result := nil;
end;

function TGLLibMaterialsEx.IndexOf(const Item: TGLLibMaterialEx): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I) = Item then
      begin
        Result := I;
        Exit;
      end;
end;

function TGLLibMaterialsEx.MaterialLibrary: TGLMaterialLibraryEx;
begin
  Result := TGLMaterialLibraryEx(GetOwner);
end;

procedure TGLLibMaterialsEx.SetItems(AIndex: Integer;
  const AValue: TGLLibMaterialEx);
begin
  inherited Items[AIndex] := AValue;
end;

{ TVXBaseShaderModel }
procedure TGLBaseShaderModel.Apply(var ARci: TGLRenderContextInfo);
var
  I: Integer;
  LEvent: TOnUniformSetting;
begin
  if FIsValid then
  begin
    FHandle.UseProgramObject;
    if FAutoFill then
      for I := FUniforms.Count - 1 downto 0 do
        TGLAbstractShaderUniform(FUniforms[I]).Apply(ARci);

    if Self is TGLShaderModel3 then
      LEvent := GetMaterial.FOnSM3UniformSetting
    else if Self is TGLShaderModel4 then
      LEvent := GetMaterial.FOnSM4UniformSetting
    else if Self is TGLShaderModel5 then
      LEvent := GetMaterial.FOnSM5UniformSetting
    else
      LEvent := nil;

    if Assigned(LEvent) then
      LEvent(Self, ARci);
  end;
end;

procedure TGLBaseShaderModel.Assign(Source: TPersistent);
var
  SM: TGLBaseShaderModel;
begin
  if Source is TGLBaseShaderModel then
  begin
    SM := TGLBaseShaderModel(Source);
    LibVertexShaderName := SM.LibVertexShaderName;
    LibFragmentShaderName := SM.LibFragmentShaderName;
    LibGeometryShaderName := SM.LibGeometryShaderName;
    LibTessControlShaderName := SM.LibTessControlShaderName;
    LibTessEvalShaderName := SM.LibTessEvalShaderName;
  end;
  inherited;
end;

constructor TGLBaseShaderModel.Create(AOwner: TPersistent);
begin
  inherited;
  FHandle := TGLProgramHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FEnabled := False;
  FUniforms := TPersistentObjectList.Create;
  FAutoFill := True;
end;

procedure TGLBaseShaderModel.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty(
    'Uniforms',
    ReadUniforms,
    WriteUniforms,
    FUniforms.Count > 0);
end;

destructor TGLBaseShaderModel.Destroy;
begin
  FHandle.Destroy;
  LibVertexShaderName := '';
  LibFragmentShaderName := '';
  LibGeometryShaderName := '';
  LibTessControlShaderName := '';
  LibTessEvalShaderName := '';
  FUniforms.CleanFree;
  inherited;
end;

procedure TGLBaseShaderModel.DoOnPrepare(Sender: TGLContext);
var
  T: TGLShaderType;
  LUniforms: TPersistentObjectList;
  LUniform, LUniform2: TGLShaderUniform;
  ID: Cardinal;
  I, J, C: Integer;
  buff: array[0..255] of AnsiChar;
  Size: TGLInt;
  Len: TGLsizei;
  Loc: TGLint;
  AType: Cardinal;
  UName: string;
  GLSLData: TGLSLDataType;
  GLSLSampler: TGLSLSamplerType;
  bSampler: Boolean;
  bNew: Boolean;
  LEvent: TOnUniformInitialize;
begin
  if FEnabled then
    try
      if IsSupported and FHandle.IsSupported then
      begin
        FHandle.AllocateHandle;
        if FHandle.IsDataNeedUpdate then
        begin
          // Validate shaders
          for T := Low(TGLShaderType) to High(TGLShaderType) do
            if Assigned(FShaders[T]) then
            begin
              FShaders[T].DoOnPrepare(Sender);
              if not FShaders[T].IsValid then
              begin
                if IsDesignTime then
                  FInfoLog := Format('%s shader "%s" is invalid',
                    [cShaderTypeName[FShaders[T].ShaderType],
                      FShaders[T].Name]);
                FIsValid := False;
                exit;
              end;
            end;
          // Gather shader
          FHandle.DetachAllObject;
          for T := Low(TGLShaderType) to High(TGLShaderType) do
            if Assigned(FShaders[T]) then
              FHandle.AttachObject(FShaders[T].Handle);
          ID := FHandle.Handle;

          begin
            // Can be override by layouts in shader
            if Assigned(FShaders[shtGeometry]) then
            begin
              gl.ProgramParameteri(ID, GL_GEOMETRY_INPUT_TYPE_EXT,
                cGLgsInTypes[FShaders[shtGeometry].GeometryInput]);
              gl.ProgramParameteri(ID, GL_GEOMETRY_OUTPUT_TYPE_EXT,
                cGLgsOutTypes[FShaders[shtGeometry].GeometryOutput]);
              gl.ProgramParameteri(ID, GL_GEOMETRY_VERTICES_OUT_EXT,
                FShaders[shtGeometry].GeometryVerticesOut);
            end;

            if FHandle.LinkProgram then
            begin

              // Get final values
              if Assigned(FShaders[shtGeometry]) then
              begin
                gl.GetProgramiv(ID, GL_GEOMETRY_INPUT_TYPE_EXT, @AType);
                case AType of
                  GL_POINTS: FShaders[shtGeometry].FGeometryInput := gsInPoints;
                  GL_LINES: FShaders[shtGeometry].FGeometryInput := gsInLines;
                  GL_LINES_ADJACENCY_EXT: FShaders[shtGeometry].FGeometryInput
                    := gsInAdjLines;
                  GL_TRIANGLES: FShaders[shtGeometry].FGeometryInput :=
                    gsInTriangles;
                  GL_TRIANGLES_ADJACENCY_EXT:
                    FShaders[shtGeometry].FGeometryInput := gsInAdjTriangles;
                end;
                gl.GetProgramiv(ID, GL_GEOMETRY_OUTPUT_TYPE_EXT, @AType);
                case AType of
                  GL_POINTS: FShaders[shtGeometry].FGeometryOutput :=
                    gsOutPoints;
                  GL_LINE_STRIP: FShaders[shtGeometry].FGeometryOutput :=
                    gsOutLineStrip;
                  GL_TRIANGLE_STRIP: FShaders[shtGeometry].FGeometryOutput :=
                    sOutTriangleStrip;
                end;
                gl.GetProgramiv(ID, GL_GEOMETRY_VERTICES_OUT_EXT, @I);
                if I > 0 then
                  FShaders[shtGeometry].FGeometryVerticesOut := I;
                gl.ClearError;
              end;

              // Get uniforms
              LUniforms := TPersistentObjectList.Create;

              gl.GetProgramiv(ID, GL_ACTIVE_UNIFORMS, @C);
              for I := 0 to C - 1 do
              begin
                gl.GetActiveUniform(
                  ID,
                  Cardinal(I),
                  Length(buff),
                  @Len,
                  @Size,
                  @AType,
                  @buff[0]);
                Loc := gl.GetUniformLocation(ID, @buff[0]);
                if Loc < 0 then
                  continue;
                UName := Copy(string(buff), 0, Len);
                GLSLData := GLSLTypeUndefined;
                GLSLSampler := GLSLSamplerUndefined;
                case AType of
                  GL_FLOAT: GLSLData := GLSLType1F;
                  GL_FLOAT_VEC2: GLSLData := GLSLType2F;
                  GL_FLOAT_VEC3: GLSLData := GLSLType3F;
                  GL_FLOAT_VEC4: GLSLData := GLSLType4F;
                  GL_INT: GLSLData := GLSLType1I;
                  GL_INT_VEC2: GLSLData := GLSLType2I;
                  GL_INT_VEC3: GLSLData := GLSLType3I;
                  GL_INT_VEC4: GLSLData := GLSLType4I;
                  GL_UNSIGNED_INT: GLSLData := GLSLType1UI;
                  GL_UNSIGNED_INT_VEC2: GLSLData := GLSLType2UI;
                  GL_UNSIGNED_INT_VEC3: GLSLData := GLSLType3UI;
                  GL_UNSIGNED_INT_VEC4: GLSLData := GLSLType4UI;
                  GL_BOOL: GLSLData := GLSLType1I;
                  GL_BOOL_VEC2: GLSLData := GLSLType2I;
                  GL_BOOL_VEC3: GLSLData := GLSLType3I;
                  GL_BOOL_VEC4: GLSLData := GLSLType4I;
                  GL_FLOAT_MAT2: GLSLData := GLSLTypeMat2F;
                  GL_FLOAT_MAT3: GLSLData := GLSLTypeMat3F;
                  GL_FLOAT_MAT4: GLSLData := GLSLTypeMat4F;
                  //------------------------------------------------------------------------------
                  GL_SAMPLER_1D: GLSLSampler := GLSLSampler1D;
                  GL_SAMPLER_2D: GLSLSampler := GLSLSampler2D;
                  GL_SAMPLER_3D: GLSLSampler := GLSLSampler3D;
                  GL_SAMPLER_CUBE: GLSLSampler := GLSLSamplerCube;
                  GL_SAMPLER_1D_SHADOW: GLSLSampler := GLSLSampler1DShadow;
                  GL_SAMPLER_2D_SHADOW: GLSLSampler := GLSLSampler2DShadow;
                  GL_SAMPLER_2D_RECT: GLSLSampler := GLSLSamplerRect;
                  GL_SAMPLER_2D_RECT_SHADOW: GLSLSampler :=
                    GLSLSamplerRectShadow;
                  GL_SAMPLER_BUFFER: GLSLSampler := GLSLSamplerBuffer;
                  GL_INT_SAMPLER_2D_RECT: GLSLSampler :=
                    GLSLIntSamplerRect;
                  GL_INT_SAMPLER_BUFFER: GLSLSampler :=
                    GLSLIntSamplerBuffer;
                  GL_UNSIGNED_INT_SAMPLER_1D: GLSLSampler :=
                    GLSLUIntSampler1D;
                  GL_UNSIGNED_INT_SAMPLER_2D: GLSLSampler :=
                    GLSLUIntSampler2D;
                  GL_UNSIGNED_INT_SAMPLER_3D: GLSLSampler :=
                    GLSLUIntSampler3D;
                  GL_UNSIGNED_INT_SAMPLER_CUBE: GLSLSampler :=
                    GLSLUIntSamplerCube;
                  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY: GLSLSampler :=
                    GLSLUIntSampler1DArray;
                  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY: GLSLSampler :=
                    GLSLUIntSampler2DArray;
                  GL_UNSIGNED_INT_SAMPLER_2D_RECT: GLSLSampler :=
                    GLSLUIntSamplerRect;
                  GL_UNSIGNED_INT_SAMPLER_BUFFER: GLSLSampler :=
                    GLSLUIntSamplerBuffer;
                  GL_SAMPLER_2D_MULTISAMPLE: GLSLSampler :=
                    GLSLSamplerMS;
                  GL_INT_SAMPLER_2D_MULTISAMPLE: GLSLSampler :=
                    GLSLIntSamplerMS;
                  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE: GLSLSampler :=
                    GLSLUIntSamplerMS;
                  GL_SAMPLER_2D_MULTISAMPLE_ARRAY: GLSLSampler :=
                    GLSLSamplerMSArray;
                  GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY: GLSLSampler :=
                    GLSLIntSamplerMSArray;
                  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY: GLSLSampler :=
                    GLSLUIntSamplerMSArray;
                end;

                bSampler := False;
                if (GLSLData = GLSLTypeUndefined) and (GLSLSampler =
                  GLSLSamplerUndefined) then
                begin
                  GLSLogger.LogWarningFmt(
                    'Detected active uniform "%s" with unknown type', [UName]);
                  continue;
                end
                else if GLSLData <> GLSLTypeUndefined then
                begin
                  GLSLogger.LogInfoFmt('Detected active uniform: %s %s',
                    [cGLSLTypeString[GLSLData], UName]);
                end
                else
                begin
                  bSampler := True;
                  GLSLogger.LogInfoFmt('Detected active uniform: %s %s',
                    [cGLSLSamplerString[GLSLSampler], UName]);
                end;

                // Find already existing uniform
                bNew := True;
                for J := 0 to FUniforms.Count - 1 do
                begin
                  if not (FUniforms[J] is TGLShaderUniform) then
                    continue;
                  LUniform := TGLShaderUniform(FUniforms[J]);
                  if not Assigned(LUniform) then
                    continue;
                  if LUniform.Name = UName then
                  begin
                    if bSampler and (LUniform is TGLShaderUniformTexture) then
                    begin
                      if TGLShaderUniformTexture(LUniform).FSamplerType =
                        GLSLSampler then
                      begin
                        LUniform.FLocation := Loc;
                        LUniform.FType := GLSLType1I;
                        TGLShaderUniformTexture(LUniform).FTarget :=
                          cSamplerToTexture[GLSLSampler];
                        LUniforms.Add(LUniform);
                        FUniforms[J] := nil;
                        bNew := False;
                        break;
                      end
                    end
                    else
                    begin
                      if LUniform.FType = GLSLData then
                      begin
                        if (LUniform is TGLShaderUniformDSA)
                          and not GL.EXT_direct_state_access then
                        begin
                          LUniform2 := LUniform;
                          LUniform := TGLShaderUniform.Create(Self);
                          LUniform._AddRef;
                          LUniform.Assign(LUniform2);
                          LUniform2._Release;
                        end;
                        LUniform.FLocation := Loc;
                        LUniforms.Add(LUniform);
                        FUniforms[J] := nil;
                        bNew := False;
                        break;
                      end;
                    end;
                  end;
                end; // for J

                if bNew then
                begin
                  // Creates new uniform
                  if bSampler then
                  begin
                    LUniform := TGLShaderUniformTexture.Create(Self);
                    LUniform.FType := GLSLType1I;
                    TGLShaderUniformTexture(LUniform).FSamplerType :=
                      GLSLSampler;
                    TGLShaderUniformTexture(LUniform).FTarget :=
                      cSamplerToTexture[GLSLSampler];
                  end
                  else
                  begin
                    if GL.EXT_direct_state_access then
                      LUniform := TGLShaderUniformDSA.Create(Self)
                    else
                      LUniform := TGLShaderUniform.Create(Self);
                    LUniform.FType := GLSLData;
                  end;
                  LUniform._AddRef;
                  LUniform.FName := UName;
                  LUniform.FNameHashCode := ComputeNameHashKey(UName);
                  LUniform.FLocation := Loc;
                  LUniforms.Add(LUniform);
                end;
              end; // for I

              // Clean old unused uniforms
              ReleaseUniforms(FUniforms);
              // Assign new one
              FUniforms := LUniforms;

              FHandle.NotifyDataUpdated;
              FIsValid := True;

              if Self is TGLShaderModel3 then
                LEvent := GetMaterial.FOnSM3UniformInit
              else if Self is TGLShaderModel4 then
                LEvent := GetMaterial.FOnSM4UniformInit
              else if Self is TGLShaderModel5 then
                LEvent := GetMaterial.FOnSM5UniformInit
              else
                LEvent := nil;

              if Assigned(LEvent) then
                LEvent(Self);

            end // if LinkProgram
            else
              FIsValid := False;
          end; // with GL

          if IsDesignTime then
          begin
            FInfoLog := FHandle.InfoLog;
            if (Length(FInfoLog) = 0) and FIsValid then
              FInfoLog := 'Link successful';
          end
          else if FIsValid then
            GLSLogger.LogInfoFmt('Program "%s" link successful - %s',
              [GetMaterial.Name, FHandle.InfoLog])
          else
            GLSLogger.LogErrorFmt('Program "%s" link failed! - %s',
              [GetMaterial.Name, FHandle.InfoLog]);
        end;
      end
      else
      begin
        if IsDesignTime then
          FInfoLog := 'Not supported by hardware';
        FIsValid := False;
      end;

    except
      on E: Exception do
      begin
        FIsValid := False;
        if IsDesignTime then
          InformationDlg(E.ClassName + ': ' + E.Message)
        else
          GLSLogger.LogError(E.ClassName + ': ' + E.Message);
      end;
    end;
end;

procedure TGLBaseShaderModel.Notification(Sender: TObject; Operation:
  TOperation);
var
  st: TGLShaderType;
begin
  if Operation = opRemove then
  begin
    for st := Low(TGLShaderType) to High(TGLShaderType) do
      if FShaders[st] = Sender then
      begin
        FShaders[st] := nil;
        FLibShaderName[st] := '';
        NotifyChange(Self);
        exit;
      end;
  end;
end;

procedure TGLBaseShaderModel.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TGLBaseShaderModel.ReadUniforms(AStream: TStream);
var
  LReader: TReader;
  N, I: Integer;
  str: string;
  LUniform: TGLAbstractShaderUniform;
  LClass: CGLAbstractShaderUniform;
begin
  LReader := TReader.Create(AStream, 16384);
  try
    N := LReader.ReadInteger;
    for I := 0 to N - 1 do
    begin
      str := LReader.ReadString;
      LClass := CGLAbstractShaderUniform(FindClass(str));
      LUniform := LClass.Create(Self);
      LUniform._AddRef;
      LUniform.ReadFromFiler(LReader);
      FUniforms.Add(LUniform);
    end;
  finally
    LReader.Free;
  end;
end;

class procedure TGLBaseShaderModel.ReleaseUniforms(
  AList: TPersistentObjectList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    if Assigned(AList[I]) then
      TGLAbstractShaderUniform(AList[I])._Release;
  AList.Destroy;
end;

function TGLBaseShaderModel.GetLibShaderName(AType: TGLShaderType): string;
begin
  if Assigned(FShaders[AType]) then
    Result := FShaders[AType].Name
  else
    Result := '';
end;

function TGLBaseShaderModel.GetUniform(const AName: string): IShaderParameter;
var
  H, I: Integer;
  U: TGLAbstractShaderUniform;
begin
  Result := nil;
  H := ComputeNameHashKey(AName);
  for I := 0 to FUniforms.Count - 1 do
  begin
    U := TGLAbstractShaderUniform(FUniforms[I]);
    if (U.FNameHashCode = H) and (U.FName = AName) then
    begin
      Result := U;
      exit;
    end;
  end;

  if not IsDesignTime then
  begin
    GLSLogger.LogErrorFmt('Attempt to use unknow uniform "%s" for material "%s"',
      [AName, GetMaterial.Name]);
    U := TGLAbstractShaderUniform.Create(Self);
    U._AddRef;
    U.FName := AName;
    U.FNameHashCode := H;
    FUniforms.Add(U);
    Result := U;
  end;
end;

procedure TGLBaseShaderModel.Loaded;
var
  T: TGLShaderType;
  I: Integer;
begin
  for T := Low(TGLShaderType) to High(TGLShaderType) do
    SetLibShaderName(T, FLibShaderName[T]);
  for I := 0 to FUniforms.Count - 1 do
    if FUniforms[I] is TGLShaderUniformTexture then
      TGLShaderUniformTexture(FUniforms[I]).Loaded;
end;

procedure TGLBaseShaderModel.GetUniformNames(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to FUniforms.Count - 1 do
    Proc(TGLAbstractShaderUniform(FUniforms[I]).FName);
end;

procedure TGLBaseShaderModel.SetLibShaderName(AType: TGLShaderType;
  const AValue: string);
var
  LShader: TGLShaderEx;
begin
  if csLoading in GetMaterialLibraryEx.ComponentState then
  begin
    FLibShaderName[AType] := AValue;
    exit;
  end;

  if Assigned(FShaders[AType]) then
  begin
    FShaders[AType].UnregisterUser(Self);
    FShaders[AType] := nil;
    FLibShaderName[AType] := '';
  end;

  LShader := GetMaterialLibraryEx.Components.GetShaderByName(AValue);
  if Assigned(LShader) then
  begin
    if LShader.ShaderType <> AType then
    begin
      if IsDesignTime then
        InformationDlg(Format('Incompatible shader type, need %s shader',
          [cShaderTypeName[AType]]));
      exit;
    end;
    LShader.RegisterUser(Self);
    FShaders[AType] := LShader;
    FLibShaderName[AType] := AValue;
  end;
  NotifyChange(Self);
end;

procedure TGLBaseShaderModel.UnApply(var ARci: TGLRenderContextInfo);
begin
  if FIsValid {and not ARci.GLStates.ForwardContext} then
    FHandle.EndUseProgramObject;
end;

procedure TGLBaseShaderModel.WriteUniforms(AStream: TStream);
var
  LWriter: TWriter;
  I: Integer;
begin
  LWriter := TWriter.Create(AStream, 16384);
  try
    LWriter.WriteInteger(FUniforms.Count);
    for I := 0 to FUniforms.Count - 1 do
    begin
      LWriter.WriteString(FUniforms[I].ClassName);
      TGLAbstractShaderUniform(FUniforms[I]).WriteToFiler(LWriter);
    end;
  finally
    LWriter.Free;
  end;
end;

class function TGLShaderModel3.IsSupported: Boolean;
begin
  Result := gl.ARB_shader_objects;
end;

class function TGLShaderModel4.IsSupported: Boolean;
begin
  Result := gl.EXT_gpu_shader4;
end;

class function TGLShaderModel5.IsSupported: Boolean;
begin
  Result := gl.ARB_gpu_shader5;
end;

procedure BeginPatch(mode: Cardinal);{$IFDEF MSWINDOWS} stdcall{$ELSE}cdecl{$ENDIF};
begin
  if mode = GL_PATCHES then
    vStoreBegin(GL_PATCHES)
  else if (mode = GL_TRIANGLES)
    or (mode = GL_TRIANGLE_STRIP)
    or (mode = GL_TRIANGLE_FAN)
    or (mode = GL_QUADS) then
  begin
    if mode = GL_QUADS then
      gl.PatchParameteri(GL_PATCH_VERTICES, 4)
    else
      gl.PatchParameteri(GL_PATCH_VERTICES, 3);
    vStoreBegin(GL_PATCHES);
  end
  else
  begin
    gl.Begin_ := vStoreBegin;
    GLSLogger.LogError('glBegin called with unsupported primitive for tessellation');
    Abort;
  end;
end;

procedure TGLShaderModel5.Apply(var ARci: TGLRenderContextInfo);
begin
  if Assigned(FShaders[shtControl]) or Assigned(FShaders[shtEvaluation]) then
  begin
    vStoreBegin := gl.Begin_;
    gl.Begin_ := BeginPatch;
    ARci.amalgamating := True;
  end;
  inherited;
end;

procedure TGLShaderModel5.UnApply(var ARci: TGLRenderContextInfo);
begin
  inherited;
  if Assigned(FShaders[shtControl]) or Assigned(FShaders[shtEvaluation]) then
    gl.Begin_ := vStoreBegin;
  ARci.amalgamating := False;
end;

{ TVXMatLibComponents }
function TGLMatLibComponents.GetAttachmentByName(
  const AName: TGLMaterialComponentName): TGLFrameBufferAttachment;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TGLFrameBufferAttachment) and (Items[I].FNameHashKey = N)
      then
    begin
      if Items[I].Name = AName then
      begin
        Result := TGLFrameBufferAttachment(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TGLMatLibComponents.GetCombinerByName(
  const AName: TGLMaterialComponentName): TGLTextureCombiner;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TGLTextureCombiner) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TGLTextureCombiner(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TGLMatLibComponents.GetItemByName(
  const AName: TGLMaterialComponentName): TGLBaseMaterialCollectionItem;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I].FNameHashKey = N) and (Items[I].Name = AName) then
    begin
      Result := Items[I];
      exit;
    end;
  end;
  Result := nil;
end;

function TGLMatLibComponents.GetItems(
  index: Integer): TGLBaseMaterialCollectionItem;
begin
  Result := TGLBaseMaterialCollectionItem(inherited GetItems(index));
end;

function TGLMatLibComponents.GetNamePath: string;
var
  s: string;
begin
  Result := ClassName;
  if GetOwner = nil then
    Exit;
  s := GetOwner.GetNamePath;
  if s = '' then
    Exit;
  Result := s + '.Components';
end;

function TGLMatLibComponents.GetSamplerByName(
  const AName: TGLMaterialComponentName): TGLTextureSampler;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TGLTextureSampler) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TGLTextureSampler(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TGLMatLibComponents.GetShaderByName(
  const AName: TGLMaterialComponentName): TGLShaderEx;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TGLShaderEx) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TGLShaderEx(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TGLMatLibComponents.GetAsmProgByName(
  const AName: TGLMaterialComponentName): TGLASMVertexProgram;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TGLASMVertexProgram) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TGLASMVertexProgram(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TGLMatLibComponents.GetTextureByName(
  const AName: TGLMaterialComponentName): TGLAbstractTexture;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TGLAbstractTexture) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TGLTextureImageEx(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

class function TGLMatLibComponents.ItemsClass: TXCollectionItemClass;
begin
  Result := TGLBaseMaterialCollectionItem;
end;

function TGLMatLibComponents.MakeUniqueName(const AName:
  TGLMaterialComponentName): TGLMaterialComponentName;
var
  I: Integer;
begin
  Result := AName;
  I := 1;
  while GetItemByName(Result) <> nil do
  begin
    Result := AName + IntToStr(i);
    Inc(i);
  end;
end;


{ TVXMaterialLibraryEx }
function TGLMaterialLibraryEx.AddAttachment(
  const AName: TGLMaterialComponentName): TGLFrameBufferAttachment;
begin
  Result := TGLFrameBufferAttachment.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TGLMaterialLibraryEx.AddCombiner(
  const AName: TGLMaterialComponentName): TGLTextureCombiner;
begin
  Result := TGLTextureCombiner.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TGLMaterialLibraryEx.AddSampler(
  const AName: TGLMaterialComponentName): TGLTextureSampler;
begin
  Result := TGLTextureSampler.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TGLMaterialLibraryEx.AddShader(
  const AName: TGLMaterialComponentName): TGLShaderEx;
begin
  Result := TGLShaderEx.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TGLMaterialLibraryEx.AddAsmProg(
  const AName: TGLMaterialComponentName): TGLASMVertexProgram;
begin
  Result := TGLASMVertexProgram.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TGLMaterialLibraryEx.AddTexture(
  const AName: TGLMaterialComponentName): TGLTextureImageEx;
begin
  Result := TGLTextureImageEx.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

constructor TGLMaterialLibraryEx.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TGLLibMaterialsEx.Create(Self);
  FComponents := TGLMatLibComponents.Create(Self);
end;

procedure TGLMaterialLibraryEx.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty(
    'ComponentsData',
    ReadComponents,
    WriteComponents,
    Components.Count > 0);
  inherited;
end;

destructor TGLMaterialLibraryEx.Destroy;
begin
  FMaterials.Destroy;
  FComponents.Destroy;
  inherited;
end;

function TGLMaterialLibraryEx.GetMaterials: TGLLibMaterialsEx;
begin
  Result := TGLLibMaterialsEx(FMaterials);
end;

procedure TGLMaterialLibraryEx.GetNames(Proc: TGetStrProc;
  AClass: CGLBaseMaterialCollectionItem);
var
  I: Integer;
begin
  for I := 0 to Components.Count - 1 do
    if Components[I].ClassType = AClass then
      Proc(Components[I].Name)
end;

procedure TGLMaterialLibraryEx.Loaded;
begin
  inherited;
end;

procedure TGLMaterialLibraryEx.ReadComponents(AStream: TStream);
var
  LReader: TReader;
begin
  LReader := TReader.Create(AStream, 16384);
  try
    Components.ReadFromFiler(LReader);
  finally
    LReader.Free;
  end;
end;

procedure TGLMaterialLibraryEx.SetComponents(AValue: TGLMatLibComponents);
begin
  FComponents.Assign(AValue);
end;

procedure TGLMaterialLibraryEx.SetLevelForAll(const ALevel: TGLMaterialLevel);
var
  I: Integer;
begin
  for I := Materials.Count - 1 downto 0 do
    Materials[I].ApplicableLevel := ALevel;
end;

procedure TGLMaterialLibraryEx.SetMaterials(AValue: TGLLibMaterialsEx);
begin
  FMaterials.Assign(AValue);
end;

function TGLMaterialLibraryEx.StoreMaterials: Boolean;
begin
  Result := (FMaterials.Count > 0);
end;

procedure TGLMaterialLibraryEx.WriteComponents(AStream: TStream);
var
  LWriter: TWriter;
begin
  LWriter := TWriter.Create(AStream, 16384);
  try
    Components.WriteToFiler(LWriter);
  finally
    LWriter.Free;
  end;
end;


{ TVXShaderUniformTexture }
procedure TGLShaderUniformTexture.Apply(var ARci: TGLRenderContextInfo);

  function FindHotActiveUnit: Boolean;
  var
    ID: Cardinal;
    I, J: Integer;
    bindTime, minTime: Double;
    LTex: TGLTextureImageEx;
  begin
    with ARci.GLStates do
    begin
      if Assigned(FLibTexture) and FLibTexture.IsValid then
      begin
        ID := FLibTexture.FHandle.Handle;
        // Yar: may be need exract this to new method of TGLTextureImageEx ???
        if FLibTexture is TGLTextureImageEx then
        begin
          LTex := TGLTextureImageEx(FLibTexture);
          Inc(LTex.FApplyCounter);
          if LTex.FApplyCounter > 16 then
            FreeAndNil(LTex.FImage);
        end;
      end
      else
        ID := 0;

      // Find alredy binded texture unit
      for I := 0 to MaxTextureImageUnits - 1 do
      begin
        if TextureBinding[I, FTarget] = ID then
        begin
          gl.Uniform1i(FLocation, I);
          ActiveTexture := I;
          Result := True;
          exit;
        end;
      end;
      // Find unused texture unit
      for I := 0 to MaxTextureImageUnits - 1 do
      begin
        if TextureBinding[I, FTarget] = 0 then
        begin
          TextureBinding[I, FTarget] := ID;
          gl.Uniform1i(FLocation, I);
          ActiveTexture := I;
          Result := True;
          exit;
        end;
      end;
      // Find most useless texture unit
      minTime := AppTime;
      J := 0;
      for I := 0 to MaxTextureImageUnits - 1 do
      begin
        bindTime := TextureBindingTime[I, FTarget];
        if bindTime < minTime then
        begin
          minTime := bindTime;
          J := I;
        end;
      end;

      TextureBinding[J, FTarget] := ID;
      ActiveTexture := J;
      gl.Uniform1i(FLocation, J);
      Result := True;
      exit;
    end;
    Result := False;
  end;

var
  glTarget: Cardinal;
begin
  if FLocation > -1 then
  begin
    if FindHotActiveUnit and Assigned(FLibTexture) and Assigned(FLibSampler)
      then
      begin
        // Apply swizzling if possible
        glTarget := DecodeTextureTarget(FLibTexture.Shape);
        if GL.ARB_texture_swizzle or GL.EXT_texture_swizzle then
        begin

          if FSwizzling[0] <> FLibTexture.FSwizzles[0] then
          begin
            FLibTexture.FSwizzles[0] := FSwizzling[0];
            gl.TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_R,
              cTextureSwizzle[FSwizzling[0]]);
          end;
          if FSwizzling[1] <> FLibTexture.FSwizzles[1] then
          begin
            FLibTexture.FSwizzles[1] := FSwizzling[1];
            gl.TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_G,
              cTextureSwizzle[FSwizzling[1]]);
          end;
          if FSwizzling[2] <> FLibTexture.FSwizzles[2] then
          begin
            FLibTexture.FSwizzles[2] := FSwizzling[2];
            gl.TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_B,
              cTextureSwizzle[FSwizzling[2]]);
          end;
          if FSwizzling[3] <> FLibTexture.FSwizzles[3] then
          begin
            FLibTexture.FSwizzles[3] := FSwizzling[3];
            gl.TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_A,
              cTextureSwizzle[FSwizzling[3]]);
          end;
        end;

        if FLibSampler.IsValid then
          FLibSampler.Apply(ARci)
        else if FLibTexture.FLastSampler <> FLibSampler then
        begin
          // Sampler object not supported, lets use texture states
          gl.TexParameterfv(glTarget, GL_TEXTURE_BORDER_COLOR,
            FLibSampler.BorderColor.AsAddress);
          gl.TexParameteri(glTarget, GL_TEXTURE_WRAP_S,
            cTextureWrapMode[FLibSampler.WrapX]);
          gl.TexParameteri(glTarget, GL_TEXTURE_WRAP_T,
            cTextureWrapMode[FLibSampler.WrapY]);
          gl.TexParameteri(glTarget, GL_TEXTURE_WRAP_R,
            cTextureWrapMode[FLibSampler.WrapZ]);
          gl.TexParameterf(glTarget, GL_TEXTURE_LOD_BIAS, FLibSampler.LODBias +
            FLibSampler.FLODBiasFract);
          gl.TexParameteri(glTarget, GL_TEXTURE_MIN_FILTER,
            cTextureMinFilter[FLibSampler.MinFilter]);
          gl.TexParameteri(glTarget, GL_TEXTURE_MAG_FILTER,
            cTextureMagFilter[FLibSampler.MagFilter]);

          if GL.EXT_texture_filter_anisotropic then
          begin
            if FLibSampler.FilteringQuality = tfAnisotropic then
              gl.TexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT,
                CurrentGLContext.GLStates.MaxTextureAnisotropy)
            else
              gl.TexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
          end;

          gl.TexParameteri(glTarget, GL_TEXTURE_COMPARE_MODE,
            cTextureCompareMode[FLibSampler.CompareMode]);
          gl.TexParameteri(glTarget, GL_TEXTURE_COMPARE_FUNC,
            cGLComparisonFunctionToGLEnum[FLibSampler.CompareFunc]);

          if GL.EXT_texture_sRGB_decode then
          begin
            if FLibSampler.sRGB_Encode then
              gl.TexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT, GL_DECODE_EXT)
            else
              gl.TexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT,
                GL_SKIP_DECODE_EXT);
          end;

          FLibTexture.FLastSampler := FLibSampler;
        end;

      end;
  end;
end;

procedure TGLShaderUniformTexture.Assign(Source: TPersistent);
var
  LUniform: TGLShaderUniformTexture;
begin
  if Source is TGLShaderUniformTexture then
  begin
    LUniform := TGLShaderUniformTexture(Source);
    LibTextureName := LUniform.LibTextureName;
    LibSamplerName := LUniform.LibSamplerName;
  end;
  inherited;
end;

constructor TGLShaderUniformTexture.Create(AOwner: TPersistent);
begin
  inherited;
  FSwizzling := cDefaultSwizzleVector;
end;

destructor TGLShaderUniformTexture.Destroy;
begin
  LibTextureName := '';
  LibSamplerName := '';
  inherited;
end;

function TGLShaderUniformTexture.GetSamplerName: string;
begin
  if Assigned(FLibSampler) then
    Result := FLibSampler.Name
  else
    Result := strNothing;
end;

function TGLShaderUniformTexture.GetTextureName: string;
begin
  if Assigned(FLibTexture) then
    Result := FLibTexture.Name
  else
    Result := strNothing;
end;

function TGLShaderUniformTexture.GetTextureSwizzle: TSwizzleVector;
begin
  Result := FSwizzling;
end;

procedure TGLShaderUniformTexture.Loaded;
begin
  SetTextureName(FLibTexureName);
  SetSamplerName(FLibSamplerName);
end;

procedure TGLShaderUniformTexture.Notification(Sender: TObject;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if Sender = FLibTexture then
      FLibTexture := nil
    else if Sender = FLibSampler then
      FLibSampler := nil;
  end;
end;

procedure TGLShaderUniformTexture.ReadFromFiler(AReader: TReader);
begin
  with AReader do
  begin
    inherited;
    LibTextureName := ReadString;
    LibSamplerName := ReadString;
    FSwizzling[0] := TGLTextureSwizzle(ReadInteger);
    FSwizzling[1] := TGLTextureSwizzle(ReadInteger);
    FSwizzling[2] := TGLTextureSwizzle(ReadInteger);
    FSwizzling[3] := TGLTextureSwizzle(ReadInteger);
  end;
end;

procedure TGLShaderUniformTexture.SetTextureName(
  const AValue: string);
var
  LTexture: TGLAbstractTexture;
begin
  if csLoading in TGLBaseShaderModel(Owner).GetMaterialLibraryEx.ComponentState
    then
  begin
    FLibTexureName := AValue;
    exit;
  end;

  if Assigned(FLibTexture) then
  begin
    if FLibTexture.Name = AValue then
      exit;
    FLibTexture.UnregisterUser(Self);
    FLibTexture := nil;
  end;

  LTexture :=
    TGLBaseShaderModel(Owner).GetMaterialLibraryEx.Components.GetTextureByName(AValue);

  if Assigned(LTexture) then
  begin
    if LTexture is TGLFrameBufferAttachment then
    begin
      if TGLFrameBufferAttachment(LTexture).OnlyWrite then
      begin
        if IsDesignTime then
          InformationDlg('Can not use write only attachment as texture')
        else
          GLSLogger.LogErrorFmt('Attempt to write only attachment "%s" for uniform "%s"',
            [LTexture.Name, Name]);
        NotifyChange(Self);
        exit;
      end;
    end;
    LTexture.RegisterUser(Self);
    FLibTexture := LTexture;
  end;
  NotifyChange(Self);
end;

procedure TGLShaderUniformTexture.SetSamplerName(const AValue: string);
var
  LSampler: TGLTextureSampler;
begin
  if csLoading in TGLBaseShaderModel(Owner).GetMaterialLibraryEx.ComponentState
    then
  begin
    FLibSamplerName := AValue;
    exit;
  end;

  if Assigned(FLibSampler) then
  begin
    if FLibSampler.Name = AValue then
      exit;
    FLibSampler.UnregisterUser(Self);
    FLibSampler := nil;
  end;

  LSampler :=
    TGLBaseShaderModel(Owner).GetMaterialLibraryEx.Components.GetSamplerByName(AValue);

  if Assigned(LSampler) then
  begin
    LSampler.RegisterUser(Self);
    FLibSampler := LSampler;
  end;

  NotifyChange(Self);
end;

procedure TGLShaderUniformTexture.SetTextureSwizzle(const AValue:
  TSwizzleVector);
begin
  FSwizzling := AValue;
end;

procedure TGLShaderUniformTexture.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    inherited;
    WriteString(LibTextureName);
    WriteString(LibSamplerName);
    WriteInteger(Integer(FSwizzling[0]));
    WriteInteger(Integer(FSwizzling[1]));
    WriteInteger(Integer(FSwizzling[2]));
    WriteInteger(Integer(FSwizzling[3]));
  end;
end;

{ TVXAbstractShaderUniform }
function TGLAbstractShaderUniform.GetFloat: Single;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetGLSLSamplerType: TGLSLSamplerType;
begin
  Result := FSamplerType;
end;

function TGLAbstractShaderUniform.GetGLSLType: TGLSLDataType;
begin
  Result := FType;
end;

function TGLAbstractShaderUniform.GetInt: TGLint;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetIVec2: TVector2i;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetIVec3: TVector3i;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetIVec4: TVector4i;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetMat2: TMatrix2f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetMat3: TMatrix3f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetMat4: TMatrix4f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetName: string;
begin
  Result := FName;
end;

function TGLAbstractShaderUniform.GetSamplerName: string;
begin
  Result := strNothing;
end;

procedure TGLAbstractShaderUniform.Apply(var ARci: TGLRenderContextInfo);
begin
end;

function TGLAbstractShaderUniform.GetAutoSetMethod: string;
begin
  Result := strNothing;
end;

function TGLAbstractShaderUniform.GetTextureName: string;
begin
  Result := strNothing;
end;

function TGLAbstractShaderUniform.GetTextureSwizzle: TSwizzleVector;
begin
  Result := cDefaultSwizzleVector;
end;

function TGLAbstractShaderUniform.GetUInt: Cardinal;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetUVec2: TVector2ui;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetUVec3: TVector3ui;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetUVec4: TVector4ui;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetVec2: TVector2f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetVec3: TVector3f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TGLAbstractShaderUniform.GetVec4: TGLVector;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

procedure TGLAbstractShaderUniform.ReadFromFiler(AReader: TReader);
begin
end;

procedure TGLAbstractShaderUniform.SetFloat(const Value: TGLFloat);
begin
end;

procedure TGLAbstractShaderUniform.SetFloatArray(const Values: PGLFloat;
  Count: Integer);
begin
end;

procedure TGLAbstractShaderUniform.SetInt(const Value: Integer);
begin
end;

procedure TGLAbstractShaderUniform.SetIntArray(const Values: PGLInt;
  Count: Integer);
begin
end;

procedure TGLAbstractShaderUniform.SetIVec2(const Value: TVector2i);
begin
end;

procedure TGLAbstractShaderUniform.SetIVec3(const Value: TVector3i);
begin
end;

procedure TGLAbstractShaderUniform.SetIVec4(const Value: TVector4i);
begin
end;

procedure TGLAbstractShaderUniform.SetMat2(const Value: TMatrix2f);
begin
end;

procedure TGLAbstractShaderUniform.SetMat3(const Value: TMatrix3f);
begin
end;

procedure TGLAbstractShaderUniform.SetMat4(const Value: TMatrix4f);
begin
end;

procedure TGLAbstractShaderUniform.SetSamplerName(const AValue: string);
begin
end;

procedure TGLAbstractShaderUniform.SetAutoSetMethod(const AValue: string);
begin
end;

procedure TGLAbstractShaderUniform.SetTextureName(const AValue: string);
begin
end;

procedure TGLAbstractShaderUniform.SetTextureSwizzle(const AValue:
  TSwizzleVector);
begin
end;

procedure TGLAbstractShaderUniform.SetUInt(const Value: Cardinal);
begin
end;

procedure TGLAbstractShaderUniform.SetUIntArray(const Values: PGLUInt;
  Count: Integer);
begin
end;

procedure TGLAbstractShaderUniform.SetUVec2(const Value: TVector2ui);
begin
end;

procedure TGLAbstractShaderUniform.SetUVec3(const Value: TVector3ui);
begin
end;

procedure TGLAbstractShaderUniform.SetUVec4(const Value: TVector4ui);
begin
end;

procedure TGLAbstractShaderUniform.SetVec2(const Value: TVector2f);
begin
end;

procedure TGLAbstractShaderUniform.SetVec3(const Value: TVector3f);
begin
end;

procedure TGLAbstractShaderUniform.SetVec4(const Value: TVector4f);
begin
end;

procedure TGLAbstractShaderUniform.WriteToFiler(AWriter: TWriter);
begin
end;


{ TVXShaderUniform }
function TGLShaderUniform.GetFloat: Single;
begin
  // TODO: Type checking
  gl.GetUniformfv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetInt: TGLint;
begin
  gl.GetUniformiv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetIVec2: TVector2i;
begin
  gl.GetUniformiv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetIVec3: TVector3i;
begin
  gl.GetUniformiv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetIVec4: TVector4i;
begin
  gl.GetUniformiv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetMat2: TMatrix2f;
begin
  gl.GetUniformfv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetMat3: TMatrix3f;
begin
  gl.GetUniformfv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetMat4: TMatrix4f;
begin
  gl.GetUniformfv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetProgram: Cardinal;
begin
  Result := TGLBaseShaderModel(Owner).FHandle.Handle;
end;

procedure TGLShaderUniform.Apply(var ARci: TGLRenderContextInfo);
begin
  if Assigned(FAutoSet) then
    FAutoSet(Self, ARci);
end;

procedure TGLShaderUniform.Assign(Source: TPersistent);
var
  LUniform: TGLShaderUniform;
begin
  if Source is TGLShaderUniform then
  begin
    LUniform := TGLShaderUniform(Source);
    FName := LUniform.Name;
    FNameHashCode := LUniform.FNameHashCode;
    FType := LUniform.FType;
    FSamplerType := LUniform.FSamplerType;
    FAutoSet := LUniform.FAutoSet;
  end;
  inherited;
end;

function TGLShaderUniform.GetAutoSetMethod: string;
begin
  Result := GetUniformAutoSetMethodName(FAutoSet);
end;

function TGLShaderUniform.GetUInt: Cardinal;
begin
  gl.GetUniformuiv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetUVec2: TVector2ui;
begin
  gl.GetUniformuiv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetUVec3: TVector3ui;
begin
  gl.GetUniformuiv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetUVec4: TVector4ui;
begin
  gl.GetUniformuiv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetVec2: TVector2f;
begin
  gl.GetUniformfv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetVec3: TVector3f;
begin
  gl.GetUniformfv(GetProgram, FLocation, @Result);
end;

function TGLShaderUniform.GetVec4: TGLVector;
begin
  gl.GetUniformfv(GetProgram, FLocation, @Result);
end;

procedure TGLShaderUniform.PopProgram;
begin
  CurrentGLContext.GLStates.CurrentProgram := FStoreProgram;
end;

procedure TGLShaderUniform.PushProgram;
begin
  with CurrentGLContext.GLStates do
  begin
    FStoreProgram := CurrentProgram;
    CurrentProgram := GetProgram;
  end;
end;

procedure TGLShaderUniform.ReadFromFiler(AReader: TReader);
begin
  with AReader do
  begin
    FName := ReadString;
    FNameHashCode := ComputeNameHashKey(FName);
    FType := TGLSLDataType(ReadInteger);
    FSamplerType := TGLSLSamplerType(ReadInteger);
    SetAutoSetMethod(ReadString);
  end;
end;

procedure TGLShaderUniform.SetFloat(const Value: TGLFloat);
begin
  PushProgram;
  gl.Uniform1f(FLocation, Value);
  PopProgram;
end;

procedure TGLShaderUniform.SetFloatArray(const Values: PGLFloat;
  Count: Integer);
begin
  PushProgram;
  gl.Uniform1fv(FLocation, Count, Values);
  PopProgram;
end;

procedure TGLShaderUniform.SetInt(const Value: Integer);
begin
  PushProgram;
  gl.Uniform1i(FLocation, Value);
  PopProgram;
end;

procedure TGLShaderUniform.SetIntArray(const Values: PGLInt; Count: Integer);
begin
  PushProgram;
  gl.Uniform1iv(FLocation, Count, Values);
  PopProgram;
end;

procedure TGLShaderUniform.SetIVec2(const Value: TVector2i);
begin
  PushProgram;
  gl.Uniform2i(FLocation, Value.X, Value.Y);
  PopProgram;
end;

procedure TGLShaderUniform.SetIVec3(const Value: TVector3i);
begin
  PushProgram;
  gl.Uniform3i(FLocation, Value.X, Value.Y, Value.Z);
  PopProgram;
end;

procedure TGLShaderUniform.SetIVec4(const Value: TVector4i);
begin
  PushProgram;
  gl.Uniform4i(FLocation, Value.X, Value.Y, Value.Z, Value.W);
  PopProgram;
end;

procedure TGLShaderUniform.SetMat2(const Value: TMatrix2f);
begin
  PushProgram;
  gl.UniformMatrix2fv(FLocation, 1, False, @Value);
  PopProgram;
end;

procedure TGLShaderUniform.SetMat3(const Value: TMatrix3f);
begin
  PushProgram;
  gl.UniformMatrix2fv(FLocation, 1, False, @Value);
  PopProgram;
end;

procedure TGLShaderUniform.SetMat4(const Value: TMatrix4f);
begin
  PushProgram;
  gl.UniformMatrix4fv(FLocation, 1, False, @Value);
  PopProgram;
end;

procedure TGLShaderUniform.SetAutoSetMethod(const AValue: string);
begin
  FAutoSet := GetUniformAutoSetMethod(AValue);
end;

procedure TGLShaderUniform.SetUInt(const Value: Cardinal);
begin
  PushProgram;
  gl.Uniform1ui(FLocation, Value);
  PopProgram;
end;

procedure TGLShaderUniform.SetUIntArray(const Values: PGLUInt; Count: Integer);
begin
  PushProgram;
  gl.Uniform1uiv(FLocation, Count, Values);
  PopProgram;
end;

procedure TGLShaderUniform.SetUVec2(const Value: TVector2ui);
begin
  PushProgram;
  gl.Uniform2ui(FLocation, Value.X, Value.Y);
  PopProgram;
end;

procedure TGLShaderUniform.SetUVec3(const Value: TVector3ui);
begin
  PushProgram;
  gl.Uniform3ui(FLocation, Value.X, Value.Y, Value.Z);
  PopProgram;
end;

procedure TGLShaderUniform.SetUVec4(const Value: TVector4ui);
begin
  PushProgram;
  gl.Uniform4ui(FLocation, Value.X, Value.Y, Value.Z, Value.W);
  PopProgram;
end;

procedure TGLShaderUniform.SetVec2(const Value: TVector2f);
begin
  PushProgram;
  gl.Uniform2f(FLocation, Value.X, Value.Y);
  PopProgram;
end;

procedure TGLShaderUniform.SetVec3(const Value: TVector3f);
begin
  PushProgram;
  gl.Uniform3f(FLocation, Value.X, Value.Y, Value.Z);
  PopProgram;
end;

procedure TGLShaderUniform.SetVec4(const Value: TVector4f);
begin
  PushProgram;
  gl.Uniform4f(FLocation, Value.X, Value.Y, Value.Z, Value.W);
  PopProgram;
end;

procedure TGLShaderUniform.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteString(FName);
    WriteInteger(Integer(FType));
    WriteInteger(Integer(FSamplerType));
    WriteString(GetAutoSetMethod);
  end;
end;

{ TVXShaderUniformDSA }
procedure TGLShaderUniformDSA.SetFloat(const Value: TGLFloat);
begin
  gl.ProgramUniform1f(GetProgram, FLocation, Value);
end;

procedure TGLShaderUniformDSA.SetFloatArray(const Values: PGLFloat;
  Count: Integer);
begin
  gl.ProgramUniform1fv(GetProgram, FLocation, Count, Values);
end;

procedure TGLShaderUniformDSA.SetInt(const Value: Integer);
begin
  gl.ProgramUniform1i(GetProgram, FLocation, Value);
end;

procedure TGLShaderUniformDSA.SetIntArray(const Values: PGLInt; Count: Integer);
begin
  gl.ProgramUniform1iv(GetProgram, FLocation, Count, Values);
end;

procedure TGLShaderUniformDSA.SetIVec2(const Value: TVector2i);
begin
  gl.ProgramUniform2i(GetProgram, FLocation, Value.X, Value.Y);
end;

procedure TGLShaderUniformDSA.SetIVec3(const Value: TVector3i);
begin
  gl.ProgramUniform3i(GetProgram, FLocation, Value.X, Value.Y, Value.Z);
end;

procedure TGLShaderUniformDSA.SetIVec4(const Value: TVector4i);
begin
  gl.ProgramUniform4i(GetProgram, FLocation, Value.X, Value.Y, Value.Z,
    Value.W);
end;

procedure TGLShaderUniformDSA.SetMat2(const Value: TMatrix2f);
begin
  gl.ProgramUniformMatrix2fv(GetProgram, FLocation, 1, False, @Value);
end;

procedure TGLShaderUniformDSA.SetMat3(const Value: TMatrix3f);
begin
  gl.ProgramUniformMatrix3fv(GetProgram, FLocation, 1, False, @Value);
end;

procedure TGLShaderUniformDSA.SetMat4(const Value: TMatrix4f);
begin
  gl.ProgramUniformMatrix4fv(GetProgram, FLocation, 1, False, @Value);
end;

procedure TGLShaderUniformDSA.SetUInt(const Value: Cardinal);
begin
  gl.ProgramUniform1ui(GetProgram, FLocation, Value);
end;

procedure TGLShaderUniformDSA.SetUIntArray(const Values: PGLUInt;
  Count: Integer);
begin
  gl.ProgramUniform1uiv(GetProgram, FLocation, Count, Values);
end;

procedure TGLShaderUniformDSA.SetUVec2(const Value: TVector2ui);
begin
  gl.ProgramUniform2ui(GetProgram, FLocation, Value.X, Value.Y);
end;

procedure TGLShaderUniformDSA.SetUVec3(const Value: TVector3ui);
begin
  gl.ProgramUniform3ui(GetProgram, FLocation, Value.X, Value.Y, Value.Z);
end;

procedure TGLShaderUniformDSA.SetUVec4(const Value: TVector4ui);
begin
  gl.ProgramUniform4ui(GetProgram, FLocation, Value.X, Value.Y, Value.Z,
    Value.W);
end;

procedure TGLShaderUniformDSA.SetVec2(const Value: TVector2f);
begin
  gl.ProgramUniform2f(GetProgram, FLocation, Value.X, Value.Y);
end;

procedure TGLShaderUniformDSA.SetVec3(const Value: TVector3f);
begin
  gl.ProgramUniform3f(GetProgram, FLocation, Value.X, Value.Y, Value.Z);
end;

procedure TGLShaderUniformDSA.SetVec4(const Value: TVector4f);
begin
  gl.ProgramUniform4f(GetProgram, FLocation, Value.X, Value.Y, Value.Z,
    Value.W);
end;

{ TVXTextureSwizzling }
procedure TGLTextureSwizzling.Assign(Source: TPersistent);
var
  LSwizzling: TGLTextureSwizzling;
begin
  if Source is TGLTextureSwizzling then
  begin
    LSwizzling := TGLTextureSwizzling(Source);
    FSwizzles[0] := LSwizzling.FSwizzles[0];
    FSwizzles[1] := LSwizzling.FSwizzles[1];
    FSwizzles[2] := LSwizzling.FSwizzles[2];
    FSwizzles[3] := LSwizzling.FSwizzles[3];
  end;
  inherited;
end;

constructor TGLTextureSwizzling.Create(AOwner: TPersistent);
begin
  inherited;
  FSwizzles := cDefaultSwizzleVector;
end;

function TGLTextureSwizzling.GetSwizzle(AIndex: Integer): TGLTextureSwizzle;
begin
  Result := FSwizzles[AIndex];
end;

procedure TGLTextureSwizzling.ReadFromFiler(AReader: TReader);
begin
  with AReader do
  begin
    ReadInteger;
    FSwizzles[0] := TGLTextureSwizzle(ReadInteger);
    FSwizzles[1] := TGLTextureSwizzle(ReadInteger);
    FSwizzles[2] := TGLTextureSwizzle(ReadInteger);
    FSwizzles[3] := TGLTextureSwizzle(ReadInteger);
  end;
end;

procedure TGLTextureSwizzling.SetSwizzle(AIndex: Integer;
  AValue: TGLTextureSwizzle);
begin
  if AValue <> FSwizzles[AIndex] then
  begin
    FSwizzles[AIndex] := AValue;
    NotifyChange(Self);
  end;
end;

function TGLTextureSwizzling.StoreSwizzle(AIndex: Integer): Boolean;
begin
  Result := (FSwizzles[AIndex] <> cDefaultSwizzleVector[AIndex]);
end;

procedure TGLTextureSwizzling.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0);
    WriteInteger(Integer(FSwizzles[0]));
    WriteInteger(Integer(FSwizzles[1]));
    WriteInteger(Integer(FSwizzles[2]));
    WriteInteger(Integer(FSwizzles[3]));
  end;
end;

{ TVXFrameBufferAttachment }
procedure TGLFrameBufferAttachment.Apply(var ARci: TGLRenderContextInfo);
begin
  if FIsValid and not FOnlyWrite then
  begin
    // Just bind
    with ARci.GLStates do
    begin
      ActiveTextureEnabled[FHandle.Target] := True;
      TextureBinding[ActiveTexture, FHandle.Target] := FHandle.Handle;
    end;
  end
  else
    ARci.GLStates.TextureBinding[ARci.GLStates.ActiveTexture, FHandle.Target] :=
      0;
end;

procedure TGLFrameBufferAttachment.Assign(Source: TPersistent);
var
  LAttachment: TGLFrameBufferAttachment;
begin
  if Source is TGLFrameBufferAttachment then
  begin
    LAttachment := TGLFrameBufferAttachment(Source);
    FLayered := LAttachment.Layered;
    FCubeMap := LAttachment.CubeMap;
    FSamples := LAttachment.Samples;
    FOnlyWrite := LAttachment.OnlyWrite;
    FFixedSamplesLocation := LAttachment.FixedSamplesLocation;
    FWidth := LAttachment.InternalWidth;
    FHeight := LAttachment.InternalHeight;
    FDepth := LAttachment.InternalDepth;
    FInternalFormat := LAttachment.InternalFormat;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TGLFrameBufferAttachment.Create(AOwner: TXCollection);
begin
  inherited;
  FDefferedInit := False;
  FHandle := TGLTextureHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FRenderBufferHandle := TGLRenderbufferHandle.Create;
  FRenderBufferHandle.OnPrapare := DoOnPrepare;
  FInternalFormat := tfRGBA8;
  FWidth := 256;
  FHeight := 256;
  FDepth := 0;
  FSamples := -1;
  FLayered := False;
  FCubeMap := False;
  FOnlyWrite := False;
  FFixedSamplesLocation := False;
  Name := TGLMatLibComponents(AOwner).MakeUniqueName('Attachment');
end;

destructor TGLFrameBufferAttachment.Destroy;
begin
  FHandle.Destroy;
  FRenderBufferHandle.Destroy;
  inherited;
end;

procedure TGLFrameBufferAttachment.DoOnPrepare(Sender: TGLContext);
var
  LTarget: TGLTextureTarget;
  w, h, d, s, Level, MaxLevel: Integer;
  glTarget, glFormat, glFace: Cardinal;
begin
  if IsDesignTime and FDefferedInit then
    exit;

  FHandle.AllocateHandle;
  FRenderBufferHandle.AllocateHandle;
  if not (FHandle.IsDataNeedUpdate or FRenderBufferHandle.IsDataNeedUpdate) then
    exit;

  // Target

  if FSamples < 0 then
  begin
    LTarget := ttTexture2D;
    if FHeight = 1 then
      LTarget := ttTexture1D;
    if FCubeMap then
      LTarget := ttTextureCube;
    if FDepth > 0 then
      LTarget := ttTexture3D;
    if FLayered then
    begin
      if FDepth < 2 then
        LTarget := ttTexture1DArray
      else
        LTarget := ttTexture2DArray;
      if FCubeMap then
        LTarget := ttTextureCubeArray;
    end;
  end
  else
  begin
    if FDepth > 0 then
      LTarget := ttTexture2DMultisampleArray
    else
      LTarget := ttTexture2DMultisample;
  end;

  // Check target support
  if FOnlyWrite and (LTarget = ttTexture2DMultisample)
    and not Sender.gl.EXT_framebuffer_multisample then
  begin
    FIsValid := False;
    exit;
  end;
  if not IsTargetSupported(LTarget) then
  begin
    FIsValid := False;
    exit;
  end;

  // Adjust dimension
  w := FWidth;
  h := FHeight;
  d := FDepth;
  s := FSamples;
  if FCubeMap then
  begin
    if w > Integer(Sender.GLStates.MaxCubeTextureSize) then
      w := Sender.GLStates.MaxCubeTextureSize;
    h := w;
    if FLayered then
    begin
      if d < 6 then
        d := 6
      else if (d mod 6) > 0 then
        d := 6 * (d div 6 + 1);
    end;
  end
  else if w > Integer(Sender.GLStates.MaxTextureSize) then
    w := Sender.GLStates.MaxTextureSize;
  if h > Integer(Sender.GLStates.MaxTextureSize) then
    h := Sender.GLStates.MaxTextureSize;
  if FLayered then
  begin
    if d > Integer(Sender.GLStates.MaxArrayTextureSize) then
      d := Sender.GLStates.MaxArrayTextureSize;
  end
  else if d > Integer(Sender.GLStates.Max3DTextureSize) then
    d := Sender.GLStates.Max3DTextureSize;
  if (s > -1) and (s > Integer(Sender.GLStates.MaxSamples)) then
    s := Sender.GLStates.MaxSamples;

  glTarget := DecodeTextureTarget(LTarget);

  if (FHandle.Target <> LTarget)
    and (FHandle.Target <> ttNoShape) then
  begin
    FHandle.DestroyHandle;
    FHandle.AllocateHandle;
  end;
  FHandle.Target := LTarget;

  glFormat := InternalFormatToOpenGLFormat(FInternalFormat);

  if FOnlyWrite and ((LTarget = ttTexture2D) or (LTarget =
    ttTexture2DMultisample))
    and FRenderBufferHandle.IsSupported then
  begin
    if LTarget = ttTexture2D then
      FRenderBufferHandle.SetStorage(glFormat, w, h)
    else
      FRenderBufferHandle.SetStorageMultisample(glFormat, s, w, h);
  end
  else
    with Sender do
    begin
      GLStates.ActiveTextureEnabled[FHandle.Target] := True;
      GLStates.TextureBinding[GLStates.ActiveTexture, FHandle.Target] :=
        FHandle.Handle;
      MaxLevel := CalcTextureLevelNumber(LTarget, w, h, d);

      case glTarget of

        GL_TEXTURE_1D:
          for Level := 0 to MaxLevel - 1 do
          begin
            gl.TexImage1D(glTarget, Level, glFormat, w, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;

        GL_TEXTURE_2D:
          for Level := 0 to MaxLevel - 1 do
          begin
            gl.TexImage2D(glTarget, Level, glFormat, w, h, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
            Div2(h);
          end;

        GL_TEXTURE_RECTANGLE:
          begin
            gl.TexImage2D(glTarget, 0, glFormat, w, h, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
          end;

        GL_TEXTURE_3D:
          for Level := 0 to MaxLevel - 1 do
          begin
            gl.TexImage3D(glTarget, Level, glFormat, w, h, d, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
            Div2(h);
            Div2(d);
          end;

        GL_TEXTURE_CUBE_MAP:
          for Level := 0 to MaxLevel - 1 do
          begin
            for glFace := GL_TEXTURE_CUBE_MAP_POSITIVE_X to
              GL_TEXTURE_CUBE_MAP_NEGATIVE_Z do
              gl.TexImage2D(glFace, Level, glFormat, w, w, 0, GL_RGBA,
                GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;

        GL_TEXTURE_1D_ARRAY:
          for Level := 0 to MaxLevel - 1 do
          begin
            gl.TexImage2D(glTarget, Level, glFormat, w, h, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;

        GL_TEXTURE_2D_ARRAY:
          for Level := 0 to MaxLevel - 1 do
          begin
            gl.TexImage3D(glTarget, Level, glFormat, w, h, d, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
            Div2(h);
          end;

        GL_TEXTURE_CUBE_MAP_ARRAY:
          for Level := 0 to MaxLevel - 1 do
          begin
            gl.TexImage3D(glTarget, Level, glFormat, w, w, d, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;
      end; // of case

      GLStates.ActiveTextureEnabled[FHandle.Target] := False;
      FOnlyWrite := False;
    end; // of texture

  if gl.GetError <> GL_NO_ERROR then
  begin
    gl.ClearError;
    GLSLogger.LogErrorFmt('Unable to create attachment "%s"', [Self.Name]);
    exit;
  end
  else
    FIsValid := True;

  FHandle.NotifyDataUpdated;
  FRenderBufferHandle.NotifyDataUpdated;
end;

class function TGLFrameBufferAttachment.FriendlyName: string;
begin
  Result := 'Framebuffer Attachment';
end;

procedure TGLFrameBufferAttachment.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  FRenderBufferHandle.NotifyChangesOfData;
  inherited;
end;

procedure TGLFrameBufferAttachment.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name := ReadString;
      FDefferedInit := ReadBoolean;
      FLayered := ReadBoolean;
      FCubeMap := ReadBoolean;
      FSamples := ReadInteger;
      FOnlyWrite := ReadBoolean;
      FFixedSamplesLocation := ReadBoolean;
      FWidth := ReadInteger;
      FHeight := ReadInteger;
      FDepth := ReadInteger;
      FInternalFormat := TGLInternalFormat(ReadInteger);
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TGLFrameBufferAttachment.SetCubeMap(AValue: Boolean);
begin
  if FCubeMap <> AValue then
  begin
    FCubeMap := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFrameBufferAttachment.SetDepth(AValue: Integer);
begin
  if FDepth < 0 then
    FDepth := 0
  else if FDepth > 256 then
    FDepth := 256;
  if FDepth <> AValue then
  begin
    FDepth := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFrameBufferAttachment.SetFixedSamplesLocation(AValue: Boolean);
begin
  if FFixedSamplesLocation <> AValue then
  begin
    FFixedSamplesLocation := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFrameBufferAttachment.SetHeight(AValue: Integer);
begin
  if FHeight < 1 then
    FHeight := 1
  else if FHeight > 8192 then
    FHeight := 8192;
  if FHeight <> AValue then
  begin
    FHeight := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFrameBufferAttachment.SetInternalFormat(
  const AValue: TGLInternalFormat);
begin
  if FInternalFormat <> AValue then
  begin
    FInternalFormat := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFrameBufferAttachment.SetLayered(AValue: Boolean);
begin
  if FLayered <> AValue then
  begin
    FLayered := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFrameBufferAttachment.SetOnlyWrite(AValue: Boolean);
begin
  if FOnlyWrite <> AValue then
  begin
    if AValue
      and ((FDepth > 0) or FLayered or FFixedSamplesLocation or FCubeMap) then
      exit;
    FOnlyWrite := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFrameBufferAttachment.SetSamples(AValue: Integer);
begin
  if AValue < -1 then
    AValue := -1;
  if FSamples <> AValue then
  begin
    FSamples := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFrameBufferAttachment.SetWidth(AValue: Integer);
begin
  if FWidth < 1 then
    FWidth := 1
  else if FWidth > 8192 then
    FWidth := 8192;
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLFrameBufferAttachment.UnApply(var ARci: TGLRenderContextInfo);
begin
  ARci.GLStates.ActiveTextureEnabled[FHandle.Target] := False;
end;

procedure TGLFrameBufferAttachment.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    WriteBoolean(FLayered);
    WriteBoolean(FCubeMap);
    WriteInteger(FSamples);
    WriteBoolean(FOnlyWrite);
    WriteBoolean(FFixedSamplesLocation);
    WriteInteger(FWidth);
    WriteInteger(FHeight);
    WriteInteger(FDepth);
    WriteInteger(Integer(FInternalFormat));
  end;
end;

constructor TStandartUniformAutoSetExecutor.Create;
begin
  RegisterUniformAutoSetMethod('Camera world position', GLSLType4F,
    SetCameraPosition);
  RegisterUniformAutoSetMethod('LightSource[0] world position', GLSLType4F,
    SetLightSource0Position);
  RegisterUniformAutoSetMethod('World (model) matrix', GLSLTypeMat4F,
    SetModelMatrix);
  RegisterUniformAutoSetMethod('WorldView matrix', GLSLTypeMat4F,
    SetModelViewMatrix);
  RegisterUniformAutoSetMethod('WorldNormal matrix', GLSLTypeMat3F,
    SetNormalModelMatrix);
  RegisterUniformAutoSetMethod('Inverse World matrix', GLSLTypeMat4F,
    SetInvModelMatrix);
  RegisterUniformAutoSetMethod('View matrix', GLSLTypeMat4F, SetViewMatrix);
  RegisterUniformAutoSetMethod('Inverse WorldView matrix', GLSLTypeMat4F,
    SetInvModelViewMatrix);
  RegisterUniformAutoSetMethod('Projection matrix', GLSLTypeMat4F,
    SetProjectionMatrix);
  RegisterUniformAutoSetMethod('ViewProjection matrix', GLSLTypeMat4F,
    SetViewProjectionMatrix);
  RegisterUniformAutoSetMethod('WorldViewProjection matrix', GLSLTypeMat4F,
    SetWorldViewProjectionMatrix);
  RegisterUniformAutoSetMethod('Material front face emission', GLSLType4F,
    SetMaterialFrontEmission);
  RegisterUniformAutoSetMethod('Material front face ambient', GLSLType4F,
    SetMaterialFrontAmbient);
  RegisterUniformAutoSetMethod('Material front face diffuse', GLSLType4F,
    SetMaterialFrontDiffuse);
  RegisterUniformAutoSetMethod('Material front face specular', GLSLType4F,
    SetMaterialFrontSpecular);
  RegisterUniformAutoSetMethod('Material front face shininess', GLSLType1F,
    SetMaterialFrontShininess);
  RegisterUniformAutoSetMethod('Material back face emission', GLSLType4F,
    SetMaterialBackEmission);
  RegisterUniformAutoSetMethod('Material back face ambient', GLSLType4F,
    SetMaterialBackAmbient);
  RegisterUniformAutoSetMethod('Material back face diffuse', GLSLType4F,
    SetMaterialBackDiffuse);
  RegisterUniformAutoSetMethod('Material back face specular', GLSLType4F,
    SetMaterialBackSpecular);
  RegisterUniformAutoSetMethod('Material back face shininess', GLSLType1F,
    SetMaterialBackShininess)
end;

procedure TStandartUniformAutoSetExecutor.SetCameraPosition(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.vec4 := ARci.cameraPosition;
end;

procedure TStandartUniformAutoSetExecutor.SetInvModelMatrix(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.InvModelMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetInvModelViewMatrix(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.InvModelViewMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetLightSource0Position(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.LightPosition[0];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackAmbient(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialAmbient[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackDiffuse(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialDiffuse[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackEmission(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialEmission[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackShininess(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.float := ARci.GLStates.MaterialShininess[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackSpecular(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialSpecular[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontAmbient(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialAmbient[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontDiffuse(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialDiffuse[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontEmission(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialEmission[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontShininess(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.float := ARci.GLStates.MaterialShininess[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontSpecular(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialSpecular[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetModelMatrix(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ModelMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetModelViewMatrix(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ModelViewMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetNormalModelMatrix(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.mat3 := ARci.PipelineTransformation.NormalModelMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetProjectionMatrix(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ProjectionMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetViewMatrix(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ViewMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetViewProjectionMatrix(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ViewProjectionMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetWorldViewProjectionMatrix(Sender:
  IShaderParameter; var ARci: TGLRenderContextInfo);
begin
  Sender.mat4 := MatrixMultiply(
    ARci.PipelineTransformation.ModelViewMatrix^,
    ARci.PipelineTransformation.ProjectionMatrix^);
end;


{ TVXASMVertexProgram }
procedure TGLASMVertexProgram.Assign(Source: TPersistent);
var
  LProg: TGLASMVertexProgram;
begin
  if Source is TGLASMVertexProgram then
  begin
    LProg := TGLASMVertexProgram(Source);
    FSource.Assign(LProg.FSource);
  end;
  inherited;
end;

constructor TGLASMVertexProgram.Create(AOwner: TXCollection);
begin
  inherited;
  FHandle := TGLARBVertexProgramHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FSource := TStringList.Create;
  FSource.OnChange := NotifyChange;
  Name := TGLMatLibComponents(AOwner).MakeUniqueName('VertexProg');
end;

destructor TGLASMVertexProgram.Destroy;
begin
  FHandle.Destroy;
  FSource.Destroy;
  inherited;
end;

procedure TGLASMVertexProgram.DoOnPrepare(Sender: TGLContext);
begin
  if FDefferedInit and not IsDesignTime then
    exit;
  try
    if FHandle.IsSupported then
    begin
      FHandle.AllocateHandle;
      if FHandle.IsDataNeedUpdate then
      begin
        SetExeDirectory;
        if (Length(FSourceFile) > 0) and FileStreamExists(FSourceFile) then
          FSource.LoadFromFile(FSourceFile);
        if FSource.Count > 0 then
        begin
          FHandle.LoadARBProgram(FSource.Text);
          FIsValid := FHandle.Ready;
          if IsDesignTime then
          begin
            FInfoLog := FHandle.InfoLog;
            if (Length(FInfoLog) = 0) and FIsValid then
              FInfoLog := 'Compilation successful';
          end
          else if FIsValid then
            GLSLogger.LogInfoFmt('Program "%s" compilation successful - %s',
              [Name, FHandle.InfoLog])
          else
            GLSLogger.LogErrorFmt('Program "%s" compilation failed - %s',
              [Name, FHandle.InfoLog]);
          FHandle.NotifyDataUpdated;
        end
        else
        begin
          if IsDesignTime then
            FInfoLog := 'No source'
          else
            GLSLogger.LogInfoFmt('Program "%s" has no source code', [Name]);
          FIsValid := False;
        end;
      end;
    end
    else
    begin
      FIsValid := False;
      if IsDesignTime then
        FInfoLog := 'Not supported by hardware';
    end;
  except
    on E: Exception do
    begin
      FIsValid := False;
      if IsDesignTime then
        InformationDlg(E.ClassName + ': ' + E.Message)
      else
        GLSLogger.LogError(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

class function TGLASMVertexProgram.FriendlyName: string;
begin
  Result := 'ASM Vertex Program';
end;

function TGLASMVertexProgram.GetHandle: TGLARBVertexProgramHandle;
begin
  Result := FHandle;
end;

procedure TGLASMVertexProgram.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TGLASMVertexProgram.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name := ReadString;
      FDefferedInit := ReadBoolean;
      FSource.Text := ReadString;
      FSourceFile := ReadString;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TGLASMVertexProgram.SetSource(AValue: TStringList);
begin
  FSource.Assign(AValue);
end;

procedure TGLASMVertexProgram.SetSourceFile(AValue: string);
begin
  FixPathDelimiter(AValue);
  if FSourceFile <> AValue then
  begin
    FSourceFile := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLASMVertexProgram.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    if Length(FSourceFile) = 0 then
      WriteString(FSource.Text)
    else
      WriteString('');
    WriteString(FSourceFile);
  end;
end;

initialization

  RegisterClasses(
    [
    TGLTextureImageEx,
      TGLFrameBufferAttachment,
      TGLTextureSampler,
      TGLTextureCombiner,
      TGLShaderEx,
      TGLASMVertexProgram,
      TGLMaterialLibraryEx,
      TGLShaderUniform,
      TGLShaderUniformDSA,
      TGLShaderUniformTexture
      ]);

  RegisterXCollectionItemClass(TGLTextureImageEx);
  RegisterXCollectionItemClass(TGLTextureSampler);
  RegisterXCollectionItemClass(TGLFrameBufferAttachment);
  RegisterXCollectionItemClass(TGLTextureCombiner);
  RegisterXCollectionItemClass(TGLShaderEx);
  RegisterXCollectionItemClass(TGLASMVertexProgram);

  vStandartUniformAutoSetExecutor := TStandartUniformAutoSetExecutor.Create;

finalization

  vStandartUniformAutoSetExecutor.Destroy;

end.

