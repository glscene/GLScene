//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Texture;

(* Handles all the color and texture stuff *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Types,
  FMX.Graphics,
  FMX.Objects,

  GXS.TextureFormat,
  GXS.Strings,

  GXS.XOpenGL,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.BaseClasses,
  GXS.ApplicationFileIO,

  GXS.Graphics,
  GXS.Context,
  GXS.State,
  GXS.PipelineTransformation,
  GXS.Color,
  GXS.Coordinates,
  GXS.RenderContextInfo,
  GXS.Utils;

const
  cDefaultNormalMapScale = 0.125;

  CmtPX = 0;
  CmtNX = 1;
  CmtPY = 2;
  CmtNY = 3;
  CmtPZ = 4;
  CmtNZ = 5;

type
  TgxTextureMode = (tmDecal, tmModulate, tmBlend, tmReplace, tmAdd);
  TgxTextureWrap = (twBoth, twNone, twVertical, twHorizontal, twSeparate);

  TgxMinFilter =
  (
    miNearest,
    miLinear,
    miNearestMipmapNearest,
    miLinearMipmapNearest,
    miNearestMipmapLinear,
    miLinearMipmapLinear
  );

  TgxMagFilter = (maNearest, maLinear);

  (* Specifies how depth values should be treated
     during filtering and texture application *)
  TgxDepthTextureMode = (dtmLuminance, dtmIntensity, dtmAlpha);

  // Specifies the depth comparison function.
  TgxDepthCompareFunc = TgxDepthFunction;

  (* Texture format for OpenGL (rendering) use.
  Internally, GLXScene handles all "base" images as 32 Bits RGBA, but you can
  specify a generic format to reduce OpenGL texture memory use: *)
  TgxTextureFormat = (
    tfDefault,
    tfRGB, // = tfRGB8
    tfRGBA, // = tfRGBA8
    tfRGB16, // = tfRGB5
    tfRGBA16, // = tfRGBA4
    tfAlpha, // = tfALPHA8
    tfLuminance, // = tfLUMINANCE8
    tfLuminanceAlpha, // = tfLUMINANCE8_ALPHA8
    tfIntensity, // = tfINTENSITY8
    tfNormalMap, // = tfRGB8
    tfRGBAFloat16, // = tfRGBA_FLOAT16_ATI
    tfRGBAFloat32, // = tfRGBA_FLOAT32_ATI
    tfExtended);

  TgxTextureCompression = TgxInternalCompression;

  TgxTexture = class;

  IgxTextureNotifyAble = interface(IgxNotifyAble)
    ['{0D9DC0B0-ECE4-4513-A8A1-5AE7022C9426}']
    procedure NotifyTexMapChange(Sender: TObject);
  end;

  TgxTextureNeededEvent = procedure(Sender: TObject; var TextureFileName: string)
    of object;

  TgxTextureChange = (tcImage, tcParams);
  TgxTextureChanges = set of TgxTextureChange;

  (*Defines how and if Alpha channel is defined for a texture image.
    tiaDefault : uses the alpha channel in the image if any
    tiaAlphaFromIntensity : the alpha channel value is deduced from other
    RGB components intensity (the brighter, the more opaque)
    tiaSuperBlackTransparent : pixels with a RGB color of (0, 0, 0) are
    completely transparent, others are completely opaque
    tiaLuminance : the luminance value is calculated for each pixel
    and used for RGB and Alpha values
    tiaLuminanceSqrt : same as tiaLuminance but with an Sqrt(Luminance)
    tiaOpaque : alpha channel is uniformously set to 1.0
    tiaTopLeftPointColorTransparent : points of the same color as the
    top left point of the bitmap are transparent, others are opaque. *)
  TgxTextureImageAlpha =
  (
    tiaDefault,
    tiaAlphaFromIntensity,
    tiaSuperBlackTransparent,
    tiaLuminance,
    tiaLuminanceSqrt,
    tiaOpaque,
    tiaTopLeftPointColorTransparent,
    tiaInverseLuminance,
    tiaInverseLuminanceSqrt,
    tiaBottomRightPointColorTransparent
  );

  (*Base class for texture image data.
   Basicly, subclasses are to be considered as different ways of getting
   a HBitmap (interfacing the actual source).
   SubClasses should be registered using RegisterTextureImageClass to allow
   proper persistence and editability in the IDE experts. *)
  TgxTextureImage = class(TgxUpdateAbleObject)
  private
    function GetResourceName: string;
  protected
    FOwnerTexture: TgxTexture;
    FOnTextureNeeded: TgxTextureNeededEvent;
    FResourceFile: string;
    class function IsSelfLoading: Boolean; virtual;
    procedure LoadTexture(AInternalFormat: TgxInternalFormat); virtual;
    function GetTextureTarget: TgxTextureTarget; virtual;
    function GetHeight: Integer; virtual;
    function GetWidth: Integer; virtual;
    function GetDepth: Integer; virtual;
    property OnTextureNeeded: TgxTextureNeededEvent read FOnTextureNeeded write FOnTextureNeeded;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    property OwnerTexture: TgxTexture read FOwnerTexture write FOwnerTexture;
    procedure NotifyChange(Sender: TObject); override;
    (*Save textureImage to file.
     This may not save a picture, but for instance, parameters, if the
     textureImage is a procedural texture. *)
    procedure SaveToFile(const fileName: string); virtual;
    (*Load textureImage from a file.
     This may not load a picture, but for instance, parameters, if the
     textureImage is a procedural texture.
     Subclasses should invoke inherited which will take care of the
     "OnTextureNeeded" stuff. *)
    procedure LoadFromFile(const fileName: string); virtual;
    (*Returns a user-friendly denomination for the class.
     This denomination is used for picking a texture image class
     in the IDE expert. *)
    class function FriendlyName: string; virtual;
    (*Returns a user-friendly description for the class.
     This denomination is used for helping the user when picking a
     texture image class in the IDE expert. If it's not overriden,
     takes its value from FriendlyName. *)
    class function FriendlyDescription: string; virtual;
    // Request reload/refresh of data upon next use.
    procedure Invalidate; virtual;
    (*Returns image's bitmap handle.
     If the actual image is not a windows bitmap (BMP), descendants should
     take care of properly converting to bitmap. *)
    function GetBitmap32: TgxImage; virtual;
    (*Request for unloading bitmapData, to free some memory.
     This one is invoked when one no longer needs the Bitmap data
     it got through a call to GetHBitmap.
     Subclasses may ignore this call if the HBitmap was obtained at
     no particular memory cost. *)
    procedure ReleaseBitmap32; virtual;
    // AsBitmap : Returns the TextureImage as a TBitmap
    function AsBitmap: TBitmap;
    procedure AssignToBitmap(aBitmap: TBitmap);
     property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Depth: Integer read GetDepth;
    //Native OpenGL texture target.
    property NativeTextureTarget: TgxTextureTarget read GetTextureTarget;
    property ResourceName: string read GetResourceName;
  end;

  TgxTextureImageClass = class of TgxTextureImage;

  (* A texture image with no specified content, only a size.
     This texture image type is of use if the context of your texture is
     calculated at run-time (with a TgxMemoryViewer for instance). *)
  TgxBlankImage = class(TgxTextureImage)
  private
    procedure SetWidth(val: Integer);
    procedure SetHeight(val: Integer);
    procedure SetDepth(val: Integer);
    procedure SetCubeMap(const val: Boolean);
    procedure SetArray(const val: Boolean);
  protected
    fBitmap: TgxImage;
    fWidth, fHeight, fDepth: Integer;
    // Store a icolor format, because fBitmap is not always defined
    fColorFormat: Cardinal;
    // Blank Cube Map
    fCubeMap: Boolean;
    // Flag to interparate depth as layer
    fArray: Boolean;
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: TgxTextureTarget; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBitmap32: TgxImage; override;
    procedure ReleaseBitmap32; override;
    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
  published
    // Width, heigth and depth of the blank image (for memory allocation).
    property Width: Integer read GetWidth write SetWidth default 256;
    property Height: Integer read GetHeight write SetHeight default 256;
    property Depth: Integer read GetDepth write SetDepth default 0;
    property CubeMap: Boolean read fCubeMap write SetCubeMap default false;
    property TextureArray: Boolean read fArray write SetArray default false;
    property ColorFormat: Cardinal read fColorFormat write fColorFormat;
  end;

  // Base class for image data classes internally based on a TgxPicture.
  TgxPictureImage = class(TgxTextureImage)
  private
    FBitmap: TgxImage;
    FVKPicture: TImage;
    FUpdateCounter: Integer;
  protected
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: TgxTextureTarget; override;
    function GetPicture: TImage;
    procedure SetPicture(const aPicture: TImage);
    procedure PictureChanged(Sender: TObject);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    (* Use this function if you are going to modify the Picture directly.
     Each invokation MUST be balanced by a call to EndUpdate. *)
    procedure BeginUpdate;
    // Ends a direct picture modification session. Follows a BeginUpdate.
    procedure EndUpdate;
    function GetBitmap32: TgxImage; override;
    procedure ReleaseBitmap32; override;
    // Holds the image content.
    property Picture: TImage read GetPicture write SetPicture;
  end;

  (* Stores any image compatible with Delphi's TgxPicture mechanism.
   The picture's data is actually stored into the DFM, the original
   picture name or path is not remembered. It is similar in behaviour
   to Delphi's TImage.
   Note that if original image is for instance JPEG format, only the JPEG
   data will be stored in the DFM (compact) *)
  TgxPersistentImage = class(TgxPictureImage)
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property NativeTextureTarget;
  published
    property Picture;
  end;

  (* Uses a picture whose data is found in a file (only filename is stored).
    The image is unloaded after upload to OpenGL. *)
  TgxPicFileImage = class(TgxPictureImage)
  private
    FPictureFileName: string;
    FAlreadyWarnedAboutMissingFile: Boolean;
    FWidth: Integer;
    FHeight: Integer;
  protected
    procedure SetPictureFileName(const val: string);
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // Only picture file name is saved
    procedure SaveToFile(const fileName: string); override;
    (* Load picture file name or use fileName as picture filename.
       The autodetection is based on the filelength and presence of zeros. *)
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property NativeTextureTarget;
    function GetBitmap32: TgxImage; override;
    procedure Invalidate; override;
  published
    // Filename of the picture to use.
    property PictureFileName: string read FPictureFileName write SetPictureFileName;
  end;

 TgxCubeMapTarget = Integer;

  (* A texture image used for specifying and stroing a cube map.
     Not unlike TgxPictureImage, but storing 6 of them instead of just one.
     Saving & loading as a whole currently not supported. *)
  TgxCubeMapImage = class(TgxTextureImage)
  private
    FImage: TgxImage;
    FUpdateCounter: Integer;
    FPicture: array[cmtPX..cmtNZ] of TImage;
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    procedure SetPicture(index: TgxCubeMapTarget; const val: TImage);
    function GetPicture(index: TgxCubeMapTarget): TImage;
    function GetTextureTarget: TgxTextureTarget; override;
    procedure PictureChanged(Sender: TObject);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBitmap32: TgxImage; override;
    procedure ReleaseBitmap32; override;
    (* Use this function if you are going to modify the Picture directly.
     Each invokation MUST be balanced by a call to EndUpdate. *)
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property NativeTextureTarget;
    // Indexed access to the cube map's sub pictures.
    property Picture[index: TgxCubeMapTarget]: TImage read GetPicture write SetPicture;
  published
    property PicturePX: TImage index cmtPX read GetPicture write SetPicture;
    property PictureNX: TImage index cmtNX read GetPicture write SetPicture;
    property PicturePY: TImage index cmtPY read GetPicture write SetPicture;
    property PictureNY: TImage index cmtNY read GetPicture write SetPicture;
    property PicturePZ: TImage index cmtPZ read GetPicture write SetPicture;
    property PictureNZ: TImage index cmtNZ read GetPicture write SetPicture;
  end;

  TgxTextureMappingMode = (tmmUser, tmmObjectLinear, tmmEyeLinear, tmmSphere,
    tmmCubeMapReflection, tmmCubeMapNormal, tmmCubeMapLight0, tmmCubeMapCamera);

  (* Defines basic texturing properties.
     You can control texture wrapping, smoothing/filtering and of course define
     the texture map (note that texturing is disabled by default).
     A built-in mechanism (through ImageAlpha) allows auto-generation of an
     Alpha channel for all bitmaps (see TgxTextureImageAlpha). *)
  TgxTexture = class(TgxUpdateAbleObject)
  private
    FTextureHandle: TgxTextureHandle;
    FSamplerHandle: TgxVirtualHandle;
    FTextureFormat: TgxInternalFormat;
    FTextureMode: TgxTextureMode;
    FTextureWrap: TgxTextureWrap;
    FMinFilter: TgxMinFilter;
    FMagFilter: TgxMagFilter;
    FDisabled: Boolean;
    FImage: TgxTextureImage;
    FImageAlpha: TgxTextureImageAlpha;
    FImageBrightness: Single;
    FImageGamma: Single;
    FMappingMode: TgxTextureMappingMode;
    FMapSCoordinates: TgxCoordinates4;
    FMapTCoordinates: TgxCoordinates4;
    FMapRCoordinates: TgxCoordinates4;
    FMapQCoordinates: TgxCoordinates4;
    FOnTextureNeeded: TgxTextureNeededEvent;
    FCompression: TgxTextureCompression;
    FRequiredMemorySize: Integer;
    FFilteringQuality: TgxTextureFilteringQuality;
    FTexWidth: Integer;
    FTexHeight: Integer;
    FTexDepth: Integer;
    FEnvColor: TgxColor;
    FBorderColor: TgxColor;
    FNormalMapScale: Single;
    FTextureWrapS: TgxSeparateTextureWrap;
    FTextureWrapT: TgxSeparateTextureWrap;
    FTextureWrapR: TgxSeparateTextureWrap;
    fTextureCompareMode: TgxTextureCompareMode;
    fTextureCompareFunc: TgxDepthCompareFunc;
    fDepthTextureMode: TgxDepthTextureMode;
    FKeepImageAfterTransfer: Boolean;
  protected
    procedure SetImage(AValue: TgxTextureImage);
    procedure SetImageAlpha(const val: TgxTextureImageAlpha);
    procedure SetImageBrightness(const val: Single);
    function StoreBrightness: Boolean;
    procedure SetImageGamma(const val: Single);
    function StoreGamma: Boolean;
    procedure SetMagFilter(AValue: TgxMagFilter);
    procedure SetMinFilter(AValue: TgxMinFilter);
    procedure SetTextureMode(AValue: TgxTextureMode);
    procedure SetTextureWrap(AValue: TgxTextureWrap);
    procedure SetTextureWrapS(AValue: TgxSeparateTextureWrap);
    procedure SetTextureWrapT(AValue: TgxSeparateTextureWrap);
    procedure SetTextureWrapR(AValue: TgxSeparateTextureWrap);
    function GetTextureFormat: TgxTextureFormat;
    procedure SetTextureFormat(const val: TgxTextureFormat);
    procedure SetTextureFormatEx(const val: TgxInternalFormat);
    function StoreTextureFormatEx: Boolean;
    procedure SetCompression(const val: TgxTextureCompression);
    procedure SetFilteringQuality(const val: TgxTextureFilteringQuality);
    procedure SetMappingMode(const val: TgxTextureMappingMode);
    function GetMappingSCoordinates: TgxCoordinates4;
    procedure SetMappingSCoordinates(const val: TgxCoordinates4);
    function StoreMappingSCoordinates: Boolean;
    function GetMappingTCoordinates: TgxCoordinates4;
    procedure SetMappingTCoordinates(const val: TgxCoordinates4);
    function StoreMappingTCoordinates: Boolean;
    function GetMappingRCoordinates: TgxCoordinates4;
    procedure SetMappingRCoordinates(const val: TgxCoordinates4);
    function StoreMappingRCoordinates: Boolean;
    function GetMappingQCoordinates: TgxCoordinates4;
    procedure SetMappingQCoordinates(const val: TgxCoordinates4);
    function StoreMappingQCoordinates: Boolean;
    procedure SetDisabled(AValue: Boolean);
    procedure SetEnabled(const val: Boolean);
    function GetEnabled: Boolean;
    procedure SetEnvColor(const val: TgxColor);
    procedure SetBorderColor(const val: TgxColor);
    procedure SetNormalMapScale(const val: Single);
    procedure SetTextureCompareMode(const val: TgxTextureCompareMode);
    procedure SetTextureCompareFunc(const val: TgxDepthCompareFunc);
    procedure SetDepthTextureMode(const val: TgxDepthTextureMode);
    function StoreNormalMapScale: Boolean;
    function StoreImageClassName: Boolean;
    function GetHandle: Cardinal; virtual;
    // Load texture to OpenGL subsystem
    procedure PrepareImage(target: Cardinal); virtual;
    // Setup OpenGL texture parameters
    procedure PrepareParams(target: Cardinal); virtual;
    procedure DoOnTextureNeeded(Sender: TObject; var textureFileName: string);
    procedure OnSamplerAllocate(Sender: TgxVirtualHandle; var Handle: Cardinal);
    procedure OnSamplerDestroy(Sender: TgxVirtualHandle; var Handle: Cardinal);
    // Shows a special image that indicates an error
    procedure SetTextureErrorImage;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    property OnTextureNeeded: TgxTextureNeededEvent read FOnTextureNeeded write
      FOnTextureNeeded;
    procedure PrepareBuildList;
    procedure ApplyMappingMode;
    procedure UnApplyMappingMode;
    procedure Apply(var rci: TgxRenderContextInfo);
    procedure UnApply(var rci: TgxRenderContextInfo);
    // Applies to TEXTURE1
    procedure ApplyAsTexture2(var rci: TgxRenderContextInfo; textureMatrix: PMatrix4f = nil);
    procedure UnApplyAsTexture2(var rci: TgxRenderContextInfo;
      reloadIdentityTextureMatrix: boolean);
    {N=1 for TEXTURE0, N=2 for TEXTURE1, etc. }
    procedure ApplyAsTextureN(n: Integer; var rci: TgxRenderContextInfo;
      textureMatrix: PMatrix4f = nil);
    procedure UnApplyAsTextureN(n: Integer; var rci: TgxRenderContextInfo;
      reloadIdentityTextureMatrix: boolean);
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure NotifyImageChange;
    procedure NotifyParamsChange;
    procedure DestroyHandles;
    procedure SetImageClassName(const val: string);
    function GetImageClassName: string;
    (* Returns the OpenGL memory used by the texture.
      The compressed size is returned if, and only if texture compression
      if active and possible, and the texture has been allocated (Handle
      is defined), otherwise the estimated size (from TextureFormat
      specification) is returned. *)
    function TextureImageRequiredMemory: Integer;
    (* Allocates the texture handle if not already allocated.
      The texture is binded and parameters are setup, but no image data
      is initialized by this call - for expert use only. *)
    function AllocateHandle: Cardinal;
    function IsHandleAllocated: Boolean;
    // Returns OpenGL texture format corresponding to current options.
    function OpenGLTextureFormat: Integer;
    //Returns if of float data type
    function IsFloatType: Boolean;
    // Is the texture enabled? Always equals to 'not Disabled'.
    property Enabled: Boolean read GetEnabled write SetEnabled;
    (* Handle to the OpenGL texture object.
      If the handle hasn't already been allocated, it will be allocated
      by this call (ie. do not use if no OpenGL context is active!) *)
    property Handle: Cardinal read GetHandle;
    property TextureHandle: TgxTextureHandle read FTextureHandle;
    // Actual width, height and depth used for last texture specification binding.
    property TexWidth: Integer read FTexWidth;
    property TexHeight: Integer read FTexHeight;
    property TexDepth: Integer read FTexDepth;
    //Give texture rendering context
  published
    (* Image ClassName for enabling True polymorphism.
    This is ugly, but since the default streaming mechanism does a
    really bad job at storing	polymorphic owned-object properties,
    and neither TFiler nor TgxPicture allow proper use of the built-in
    streaming, that's the only way I found to allow a user-extensible
    mechanism. *)
    property ImageClassName: string read GetImageClassName write
      SetImageClassName stored StoreImageClassName;
    // Image data for the texture.
    property Image: TgxTextureImage read FImage write SetImage;
    (* Automatic Image Alpha setting.
    Allows to control how and if the image's Alpha channel (transparency)
    is computed. *)
    property ImageAlpha: TgxTextureImageAlpha read FImageAlpha write
      SetImageAlpha default tiaDefault;
    (* Texture brightness correction.
    This correction is applied upon loading a TgxTextureImage, it's a
    simple saturating scaling applied to the RGB components of
    the 32 bits image, before it is passed to OpenGL, and before
    gamma correction (if any). *)
    property ImageBrightness: Single read FImageBrightness write
      SetImageBrightness stored StoreBrightness;
    (* Texture gamma correction.
    The gamma correction is applied upon loading a TgxTextureImage,
    applied to the RGB components of the 32 bits image, before it is
    passed to OpenGL, after brightness correction (if any). *)
    property ImageGamma: Single read FImageGamma write SetImageGamma stored StoreGamma;
    // Texture magnification filter.
    property MagFilter: TgxMagFilter read FMagFilter write SetMagFilter default maLinear;
    // Texture minification filter.
    property MinFilter: TgxMinFilter read FMinFilter write SetMinFilter default miLinearMipMapLinear;
    // Texture application mode.
    property TextureMode: TgxTextureMode read FTextureMode write SetTextureMode default tmDecal;
    // Wrapping mode for the texture.
    property TextureWrap: TgxTextureWrap read FTextureWrap write SetTextureWrap default twBoth;
    // Wrapping mode for the texture when TextureWrap=twSeparate.
    property TextureWrapS: TgxSeparateTextureWrap read FTextureWrapS write
      SetTextureWrapS default twRepeat;
    property TextureWrapT: TgxSeparateTextureWrap read FTextureWrapT write
      SetTextureWrapT default twRepeat;
    property TextureWrapR: TgxSeparateTextureWrap read FTextureWrapR write
      SetTextureWrapR default twRepeat;
    // Texture format for use by the renderer. See TgxTextureFormat for details.
    property TextureFormat: TgxTextureFormat read GetTextureFormat write
      SetTextureFormat default tfDefault;
    property TextureFormatEx: TgxInternalFormat read FTextureFormat write
      SetTextureFormatEx stored StoreTextureFormatEx;
    (* Texture compression control.
    If True the compressed TextureFormat variant (the OpenGL ICD must
    support GL_ARB_texture_compression, or this option is ignored). *)
    property Compression: TgxTextureCompression read FCompression write
      SetCompression default tcDefault;
    (*Specifies texture filtering quality.
    You can choose between bilinear and trilinear filetring (anisotropic).
    The OpenGL ICD must support GL_EXT_texture_filter_anisotropic or
    this property is ignored. *)
    property FilteringQuality: TgxTextureFilteringQuality read FFilteringQuality
      write SetFilteringQuality default tfIsotropic;
    (* Texture coordinates mapping mode.
    This property controls automatic texture coordinates generation. *)
    property MappingMode: TgxTextureMappingMode read FMappingMode write
      SetMappingMode default tmmUser;
    (* Texture mapping coordinates mode for S, T, R and Q axis.
    This property stores the coordinates for automatic texture
    coordinates generation. *)
    property MappingSCoordinates: TgxCoordinates4 read GetMappingSCoordinates
      write SetMappingSCoordinates stored StoreMappingSCoordinates;
    property MappingTCoordinates: TgxCoordinates4 read GetMappingTCoordinates
      write SetMappingTCoordinates stored StoreMappingTCoordinates;
    property MappingRCoordinates: TgxCoordinates4 read GetMappingRCoordinates
      write SetMappingRCoordinates stored StoreMappingRCoordinates;
    property MappingQCoordinates: TgxCoordinates4 read GetMappingQCoordinates
      write SetMappingQCoordinates stored StoreMappingQCoordinates;
    // Texture Environment color.
    property EnvColor: TgxColor read FEnvColor write SetEnvColor;
    // Texture Border color.
    property BorderColor: TgxColor read FBorderColor write SetBorderColor;
    // If true, the texture is disabled (not used).
    property Disabled: Boolean read FDisabled write SetDisabled default True;
    (* Normal Map scaling.
    Only applies when TextureFormat is tfNormalMap, this property defines
    the scaling that is applied during normal map generation (ie. controls
    the intensity of the bumps). *)
    property NormalMapScale: Single read FNormalMapScale write SetNormalMapScale
      stored StoreNormalMapScale;
     property TextureCompareMode: TgxTextureCompareMode read fTextureCompareMode
      write SetTextureCompareMode default tcmNone;
    property TextureCompareFunc: TgxDepthCompareFunc read fTextureCompareFunc
      write SetTextureCompareFunc default cfLequal;
    property DepthTextureMode: TgxDepthTextureMode read fDepthTextureMode write
      SetDepthTextureMode default dtmLuminance;
    // Disable image release after transfering it to VGA.
    property KeepImageAfterTransfer: Boolean read FKeepImageAfterTransfer
      write FKeepImageAfterTransfer default False;
  end;

  TgxTextureExItem = class(TCollectionItem, IgxTextureNotifyAble)
  private
    FTexture: TgxTexture;
    FTextureIndex: Integer;
    FTextureOffset, FTextureScale: TgxCoordinates;
    FTextureMatrixIsIdentity: Boolean;
    FTextureMatrix: TMatrix4f;
    FApplied: Boolean;
     //implementing IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    function GetDisplayName: string; override;
    function GetOwner: TPersistent; override;
    procedure SetTexture(const Value: TgxTexture);
    procedure SetTextureIndex(const Value: Integer);
    procedure SetTextureOffset(const Value: TgxCoordinates);
    procedure SetTextureScale(const Value: TgxCoordinates);
    procedure NotifyTexMapChange(Sender: TObject);
    procedure CalculateTextureMatrix;
    procedure OnNotifyChange(Sender: TObject);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject);
    procedure Apply(var rci: TgxRenderContextInfo);
    procedure UnApply(var rci: TgxRenderContextInfo);
  published
    property Texture: TgxTexture read FTexture write SetTexture;
    property TextureIndex: Integer read FTextureIndex write SetTextureIndex;
    property TextureOffset: TgxCoordinates read FTextureOffset write SetTextureOffset;
    property TextureScale: TgxCoordinates read FTextureScale write SetTextureScale;
  end;

  TgxTextureEx = class(TCollection)
  private
    FOwner: TgxUpdateAbleObject;
  protected
    procedure SetItems(index: Integer; const Value: TgxTextureExItem);
    function GetItems(index: Integer): TgxTextureExItem;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TgxUpdateAbleObject);
    procedure NotifyChange(Sender: TObject);
    procedure Apply(var rci: TgxRenderContextInfo);
    procedure UnApply(var rci: TgxRenderContextInfo);
    function IsTextureEnabled(Index: Integer): Boolean;
    function Add: TgxTextureExItem;
    property Items[index: Integer]: TgxTextureExItem read GetItems write
    SetItems; default;
    procedure Loaded;
  end;

  ETexture = class(Exception);
  EShaderException = class(Exception);

// Register a TgxTextureImageClass (used for persistence and IDE purposes)
procedure RegisterTextureImageClass(textureImageClass: TgxTextureImageClass);
// Finds a registerer TgxTextureImageClass using its classname
function FindTextureImageClass(const className: string): TgxTextureImageClass;
// Finds a registerer TgxTextureImageClass using its FriendlyName
function FindTextureImageClassByFriendlyName(const friendlyName: string):
  TgxTextureImageClass;
// Defines a TStrings with the list of registered TgxTextureImageClass.
procedure SetTextureImageClassesToStrings(aStrings: TStrings);
(* Creates a TStrings with the list of registered TgxTextureImageClass.
 To be freed by caller. *)
function GetTextureImageClassesAsStrings: TStrings;
procedure RegisterTGraphicClassFileExtension(const extension: string;
  const aClass: TGraphicClass);
function CreateGraphicFromFile(const fileName: string): TBitmap;

//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------

uses
  GXS.Scene, // TODO: remove dependancy on Scene.pas unit (related to tmmCubeMapLight0)
  GXS.PictureRegisteredFormats;

const
  cTextureMode: array[tmDecal..tmAdd] of Cardinal =
    (GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE, GL_ADD);

  cOldTextureFormatToInternalFormat: array[tfRGB..tfRGBAFloat32] of
    TgxInternalFormat = (
    tfRGB8,
    tfRGBA8,
    tfRGB5,
    tfRGBA4,
    tfALPHA8,
    tfLUMINANCE8,
    tfLUMINANCE8_ALPHA8,
    tfINTENSITY8,
    tfRGB8,
    tfRGBA_FLOAT16,
    tfRGBA_FLOAT32);

var
  vGxTextureImageClasses: TList;
  vTGraphicFileExtension: array of string;
  vTGraphicClass: array of TGraphicClass;

type
  TFriendlyImage = class(TgxBaseImage);

// Dummy methods for CPP
//
function TgxTextureImage.GetTextureTarget: TgxTextureTarget;
begin
end;

function TgxTextureImage.GetHeight: Integer;
begin
  Result := 0;
end;

function TgxTextureImage.GetWidth: Integer;
begin
  Result := 0;
end;

function TgxTextureImage.GetDepth: Integer;
begin
  Result := 0;
end;

procedure TgxTextureImage.SaveToFile(const FileName: String);
begin
end;

class function TgxTextureImage.FriendlyName: String;
begin
  Result := '';
end;

function TgxTextureImage.GetBitmap32: TgxImage;
begin
  Result := nil;
end;

procedure RegisterTGraphicClassFileExtension(const extension: string;
  const aClass: TGraphicClass);
var
  n: Integer;
begin
  n := Length(vTGraphicFileExtension);
  SetLength(vTGraphicFileExtension, n + 1);
  SetLength(vTGraphicClass, n + 1);
  vTGraphicFileExtension[n] := LowerCase(extension);
  vTGraphicClass[n] := aClass;
end;

function CreateGraphicFromFile(const fileName: string): TBitmap;
var
  i: Integer;
  ext: string;
  fs: TStream;
  graphicClass: TGraphicClass;
begin
  Result := nil;
  if FileStreamExists(fileName) then
  begin
    graphicClass := nil;
    ext := LowerCase(ExtractFileExt(fileName));
    for i := 0 to High(vTGraphicFileExtension) do
    begin
      if vTGraphicFileExtension[i] = ext then
      begin
        graphicClass := TGraphicClass(vTGraphicClass[i]);
        Break;
      end;
    end;
    if graphicClass = nil then
      graphicClass := GraphicClassForExtension(ext);
    if graphicClass <> nil then
    begin
      Result := graphicClass.Create;
      try
        fs := TFileStream.Create(fileName, fmOpenRead);
        try
          Result.LoadFromStream(fs);
        finally
          fs.Free;
        end;
      except
        FreeAndNil(Result);
        raise;
      end;
    end;
  end;
end;

procedure RegisterTextureImageClass(textureImageClass: TgxTextureImageClass);
begin
  if not Assigned(vGxTextureImageClasses) then
    vGxTextureImageClasses := TList.Create;
  vGxTextureImageClasses.Add(textureImageClass);
end;

function FindTextureImageClass(const className: string): TgxTextureImageClass;
var
  i: Integer;
  tic: TgxTextureImageClass;
begin
  Result := nil;
  if Assigned(vGxTextureImageClasses) then
    for i := 0 to vGxTextureImageClasses.Count - 1 do
    begin
      tic := TgxTextureImageClass(vGxTextureImageClasses[i]);
      if tic.ClassName = className then
      begin
        Result := tic;
        Break;
      end;
    end;

end;

function FindTextureImageClassByFriendlyName(const friendlyName: string):
  TgxTextureImageClass;
var
  i: Integer;
  tic: TgxTextureImageClass;
begin
  Result := nil;
  if Assigned(vGxTextureImageClasses) then
    for i := 0 to vGxTextureImageClasses.Count - 1 do
    begin
      tic := TgxTextureImageClass(vGxTextureImageClasses[i]);
      if tic.FriendlyName = friendlyName then
      begin
        Result := tic;
        Break;
      end;
    end;
end;

procedure SetTextureImageClassesToStrings(aStrings: TStrings);
var
  i: Integer;
  tic: TgxTextureImageClass;
begin
  with aStrings do
  begin
    BeginUpdate;
    Clear;
    if Assigned(vGxTextureImageClasses) then
      for i := 0 to vGxTextureImageClasses.Count - 1 do
      begin
        tic := TgxTextureImageClass(vGxTextureImageClasses[i]);
        AddObject(tic.FriendlyName, TObject(Pointer(tic)));
      end;
    EndUpdate;
  end;
end;

function GetTextureImageClassesAsStrings: TStrings;
begin
  Result := TStringList.Create;
  SetTextureImageClassesToStrings(Result);
end;



// ------------------
// ------------------ TgxTextureImage ------------------
// ------------------

constructor TgxTextureImage.Create(AOwner: TPersistent);
begin
  inherited;
  FOwnerTexture := (AOwner as TgxTexture);
end;

destructor TgxTextureImage.Destroy;
begin
  inherited Destroy;
end;

class function TgxTextureImage.FriendlyDescription: string;
begin
  Result := FriendlyName;
end;

procedure TgxTextureImage.Invalidate;
begin
  ReleaseBitmap32;
  NotifyChange(Self);
end;

procedure TgxTextureImage.ReleaseBitmap32;
begin
  // nothing here.
end;

// AsBitmap : Returns the TextureImage as a TBitmap
// WARNING: This Creates a new bitmap. Remember to free it, to prevent leaks.
// If possible, rather use AssignToBitmap.
//
function TgxTextureImage.AsBitmap: TBitmap;
begin
  result := self.GetBitmap32.Create32BitsBitmap;
end;

procedure TgxTextureImage.AssignToBitmap(aBitmap: TBitmap);
begin
  Self.GetBitmap32.AssignToBitmap(aBitmap);
end;

procedure TgxTextureImage.NotifyChange(Sender: TObject);
begin
  if Assigned(FOwnerTexture) then
  begin
    FOwnerTexture.FTextureHandle.NotifyChangesOfData;
    FOwnerTexture.FSamplerHandle.NotifyChangesOfData;
    // Check for texture target change
    GetTextureTarget;
    FOwnerTexture.NotifyChange(Self);
  end;
end;

procedure TgxTextureImage.LoadFromFile(const fileName: string);
var
  buf: string;
begin
  if Assigned(FOnTextureNeeded) then
  begin
    buf := fileName;
    FOnTextureNeeded(Self, buf);
  end;
end;

function TgxTextureImage.GetResourceName: string;
begin
  Result := FResourceFile;
end;

class function TgxTextureImage.IsSelfLoading: Boolean;
begin
  Result := False;
end;

procedure TgxTextureImage.LoadTexture(AInternalFormat: TgxInternalFormat);
begin
end;


// ------------------
// ------------------ TgxBlankImage ------------------
// ------------------

constructor TgxBlankImage.Create(AOwner: TPersistent);
begin
  inherited;
  fWidth := 256;
  fHeight := 256;
  fDepth := 0;
  fColorFormat := GL_RGBA;
end;

destructor TgxBlankImage.Destroy;
begin
  ReleaseBitmap32;
  inherited Destroy;
end;

procedure TgxBlankImage.Assign(Source: TPersistent);
var
  img: TgxBlankImage;
begin
  if Assigned(Source) then
  begin
    if (Source is TgxBlankImage) then
    begin
      img := Source as TgxBlankImage;
      FWidth := img.Width;
      FHeight := img.Height;
      FDepth := img.Depth;
      FCubeMap := img.fCubeMap;
      FArray := img.fArray;
      fColorFormat := img.ColorFormat;
      FResourceFile := img.ResourceName;
      Invalidate;
    end
    else
      GetBitmap32.Assign(Source);
    NotifyChange(Self);
  end
  else
    inherited;
end;

procedure TgxBlankImage.SetWidth(val: Integer);
begin
  if val <> FWidth then
  begin
    FWidth := val;
    if FWidth < 1 then
      FWidth := 1;
    Invalidate;
  end;
end;

function TgxBlankImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TgxBlankImage.SetHeight(val: Integer);
begin
  if val <> FHeight then
  begin
    FHeight := val;
    if FHeight < 1 then
      FHeight := 1;
    Invalidate;
  end;
end;

function TgxBlankImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

procedure TgxBlankImage.SetDepth(val: Integer);
begin
  if val <> FDepth then
  begin
    FDepth := val;
    if FDepth < 0 then
      FDepth := 0;
    Invalidate;
  end;
end;

function TgxBlankImage.GetDepth: Integer;
begin
  Result := fDepth;
end;

procedure TgxBlankImage.SetCubeMap(const val: Boolean);
begin
  if val <> fCubeMap then
  begin
    fCubeMap := val;
    Invalidate;
  end;
end;

procedure TgxBlankImage.SetArray(const val: Boolean);
begin
  if val <> fArray then
  begin
    fArray := val;
    Invalidate;
  end;
end;

function TgxBlankImage.GetBitmap32: TgxImage;
begin
  if not Assigned(FBitmap) then
  begin
    fBitmap := TgxImage.Create;
    fBitmap.Width := FWidth;
    fBitmap.Height := FHeight;
    fBitmap.Depth := FDepth;
    fBitmap.CubeMap := FCubeMap;
    fBitmap.TextureArray := FArray;
    fBitmap.SetColorFormatDataType(FColorFormat, GL_UNSIGNED_BYTE);
  end;
  Result := FBitmap;
end;

procedure TgxBlankImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TgxBlankImage.SaveToFile(const fileName: string);
begin
  SaveAnsiStringToFile(fileName, AnsiString(
    '[BlankImage]'#13#10'Width=' + IntToStr(Width) +
    #13#10'Height=' + IntToStr(Height) +
    #13#10'Depth=' + IntToStr(Depth)));
end;

procedure TgxBlankImage.LoadFromFile(const fileName: string);
var
  sl: TStringList;
  buf, temp: string;
begin
  buf := fileName;
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Self, buf);
  if FileExists(buf) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(buf, TEncoding.ASCII);
      FWidth := StrToInt(sl.Values['Width']);
      FHeight := StrToInt(sl.Values['Height']);
      temp := sl.Values['Depth'];
      if Length(temp) > 0 then
        FDepth := StrToInt(temp)
      else
        FDepth := 1;
    finally
      sl.Free;
    end;
  end
  else
  begin
    Assert(False, Format(strFailedOpenFile, [fileName]));
  end;
end;

class function TgxBlankImage.FriendlyName: string;
begin
  Result := 'Blank Image';
end;

class function TgxBlankImage.FriendlyDescription: string;
begin
  Result := 'Blank Image (Width x Height x Depth)';
end;

function TgxBlankImage.GetTextureTarget: TgxTextureTarget;
begin
  Result := ttTexture2D;
  // Choose a texture target
  if Assigned(fBitmap) then
  begin
    FWidth := fBitmap.Width;
    FHeight := fBitmap.Height;
    FDepth := fBitmap.Depth;
    FCubeMap := fBitmap.CubeMap;
    FArray := fBitmap.TextureArray;
  end;

  if FHeight = 1 then
    Result := ttTexture1D;
  if FCubeMap then
    Result := ttTextureCube;
  if FDepth > 0 then
    Result := ttTexture3D;
  if FArray then
  begin
    if FDepth < 2 then
      Result := ttTexture1DArray
    else
      Result := ttTexture2DArray;
    if FCubeMap then
      Result := ttTextureCubeArray;
  end;

  if Assigned(FOwnerTexture) then
  begin
    if ((FOwnerTexture.FTextureFormat >= tfFLOAT_R16)
      and (FOwnerTexture.FTextureFormat <= tfFLOAT_RGBA32)) then
      Result := ttTextureRect;
  end;
end;


// ------------------
// ------------------ TgxPictureImage ------------------
// ------------------

constructor TgxPictureImage.Create(AOwner: TPersistent);
begin
  inherited;
end;

destructor TgxPictureImage.Destroy;
begin
  ReleaseBitmap32;
  FVKPicture.Free;
  inherited Destroy;
end;

procedure TgxPictureImage.Assign(Source: TPersistent);
var
  bmp: TBitmap;
begin
  if Assigned(Source) then
  begin
    if (Source is TgxPersistentImage) then
      Picture.Assign(TgxPersistentImage(Source).Picture)
    else if (Source is TBitmap) then
      Picture.Assign(Source)
    else if (Source is TImage) then
      Picture.Assign(Source)
    else if (Source is TgxImage) then
    begin
      bmp := TgxImage(Source).Create32BitsBitmap;
      Picture.Bitmap := bmp;
      bmp.Free;
      FResourceFile := TgxImage(Source).ResourceName;
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TgxPictureImage.BeginUpdate;
begin
  Inc(FUpdateCounter);
  Picture.Bitmap.OnChange := nil;
end;

procedure TgxPictureImage.EndUpdate;
begin
  Assert(FUpdateCounter > 0, ClassName + ': Unbalanced Begin/EndUpdate');
  Dec(FUpdateCounter);
  Picture.Bitmap.OnChange := PictureChanged;
  if FUpdateCounter = 0 then
    PictureChanged(Picture);
end;

function TgxPictureImage.GetHeight: Integer;
begin
  Result := Picture.Bitmap.Height;
end;

function TgxPictureImage.GetWidth: Integer;
begin
  Result := Picture.Bitmap.Width;
end;

function TgxPictureImage.GetDepth: Integer;
begin
  Result := 0;
end;

function TgxPictureImage.GetBitmap32: TgxImage;
begin
  if not Assigned(FBitmap) then
  begin
    FBitmap := TgxImage.Create;
    // we need to deactivate OnChange, due to a "glitch" in some TGraphics,
    // for instance, TJPegImage triggers an OnChange when it is drawn...
    if Assigned(Picture.Bitmap) then
    begin
      if Assigned(Picture.Bitmap.OnChange) then
      begin
        Picture.Bitmap.OnChange := nil;
        try
          FBitmap.Assign(Picture.Bitmap);
        finally
          Picture.Bitmap.OnChange := PictureChanged;
        end;
      end
      else
        FBitmap.Assign(Picture.Bitmap);
    end
    else
      FBitmap.SetErrorImage;
  end;
  Result := FBitmap;
end;

procedure TgxPictureImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TgxPictureImage.PictureChanged(Sender: TObject);
begin
  Invalidate;
end;

function TgxPictureImage.GetPicture: TImage;
begin
  if not Assigned(FVKPicture) then
  begin
    FVKPicture := TImage.Create(nil);
    FVKPicture.Bitmap.OnChange := PictureChanged;
  end;
  Result := FVKPicture;
end;

procedure TgxPictureImage.SetPicture(const aPicture: TImage);
begin
  Picture.Assign(aPicture);
end;

function TgxPictureImage.GetTextureTarget: TgxTextureTarget;
begin
  Result := ttTexture2D;
end;


// ------------------
// ------------------ TgxPersistentImage ------------------
// ------------------


constructor TgxPersistentImage.Create(AOwner: TPersistent);
begin
  inherited;
end;

destructor TgxPersistentImage.Destroy;
begin
  inherited Destroy;
end;

procedure TgxPersistentImage.SaveToFile(const fileName: string);
begin
  Picture.Bitmap.SaveToFile(fileName);
  FResourceFile := fileName;
end;

procedure TgxPersistentImage.LoadFromFile(const fileName: string);
var
  buf: string;
  gr: TBitmap;
begin
  buf := fileName;
  FResourceFile := fileName;
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Self, buf);
  if ApplicationFileIODefined then
  begin
    gr := CreateGraphicFromFile(buf);
    if Assigned(gr) then
    begin
      Picture.Bitmap := gr;
      gr.Free;
      Exit;
    end;
  end
  else if FileExists(buf) then
  begin
    Picture.Bitmap.LoadFromFile(buf);
    Exit;
  end;
  Picture.Bitmap := nil;
  raise ETexture.CreateFmt(strFailedOpenFile, [fileName]);
end;

class function TgxPersistentImage.FriendlyName: string;
begin
  Result := 'Persistent Image';
end;

class function TgxPersistentImage.FriendlyDescription: string;
begin
  Result := 'Image data is stored in its original format with other form resources,'
    + 'ie. in the DFM at design-time, and embedded in the EXE at run-time.';
end;

// ------------------
// ------------------ TgxPicFileImage ------------------
// ------------------

constructor TgxPicFileImage.Create(AOwner: TPersistent);
begin
  inherited;
end;

destructor TgxPicFileImage.Destroy;
begin
  inherited;
end;

procedure TgxPicFileImage.Assign(Source: TPersistent);
begin
  if Source is TgxPicFileImage then
  begin
    FPictureFileName := TgxPicFileImage(Source).FPictureFileName;
    FResourceFile := TgxPicFileImage(Source).ResourceName;
  end
  else
    inherited;
end;

procedure TgxPicFileImage.SetPictureFileName(const val: string);
begin
  if val <> FPictureFileName then
  begin
    FPictureFileName := val;
    FResourceFile := val;
    FAlreadyWarnedAboutMissingFile := False;
    Invalidate;
  end;
end;

procedure TgxPicFileImage.Invalidate;
begin
  Picture.Bitmap.OnChange := nil;
  try
    Picture.Assign(nil);
    FBitmap := nil;
  finally
    Picture.Bitmap.OnChange := PictureChanged;
  end;
  inherited;
end;

function TgxPicFileImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TgxPicFileImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TgxPicFileImage.GetDepth: Integer;
begin
  Result := 0;
end;

function TgxPicFileImage.GetBitmap32: TgxImage;
var
  buf: string;
  gr: TBitmap;
begin
  if (GetWidth <= 0) and (PictureFileName <> '') then
  begin
    Picture.Bitmap.OnChange := nil;
    try
      buf := PictureFileName;
      SetExeDirectory;
      if Assigned(FOnTextureNeeded) then
        FOnTextureNeeded(Self, buf);
      if FileStreamExists(buf) then
      begin
        gr := CreateGraphicFromFile(buf);
        Picture.Bitmap := gr;
        gr.Free;
      end
      else
      begin
        Picture.Bitmap := nil;
        if not FAlreadyWarnedAboutMissingFile then
        begin
          FAlreadyWarnedAboutMissingFile := True;
          MessageBoxOK(Format(strFailedOpenFileFromCurrentDir, [PictureFileName, GetCurrentDir]),strError);
        end;
      end;
      Result := inherited GetBitmap32;
      FWidth := Result.Width;
      FHeight := Result.Height;
      Picture.Bitmap := nil;
    finally
      Picture.Bitmap.OnChange := PictureChanged;
    end;
  end
  else
    Result := inherited GetBitmap32;
end;

procedure TgxPicFileImage.SaveToFile(const fileName: string);
begin
  FResourceFile := fileName;
  SaveAnsiStringToFile(fileName, AnsiString(PictureFileName));
end;

 
//

procedure TgxPicFileImage.LoadFromFile(const fileName: string);
var
  buf: string;
begin
  inherited;
  // attempt to autodetect if we are pointed to a file containing
  // a filename or directly to an image
  if SizeOfFile(fileName) < 512 then
  begin
    buf := string(LoadAnsiStringFromFile(fileName));
    if Pos(#0, buf) > 0 then
      PictureFileName := fileName
    else
      PictureFileName := buf;
  end
  else
    PictureFileName := fileName;
  FResourceFile := FPictureFileName;
end;

 
//

class function TgxPicFileImage.FriendlyName: string;
begin
  Result := 'PicFile Image';
end;

// FriendlyDescription
//

class function TgxPicFileImage.FriendlyDescription: string;
begin
  Result := 'Image data is retrieved from a file.';
end;

// ------------------
// ------------------ TgxCubeMapImage ------------------
// ------------------

 
constructor TgxCubeMapImage.Create(AOwner: TPersistent);
var
  i: TgxCubeMapTarget;
begin
  inherited;
  for i := Low(FPicture) to High(FPicture) do
  begin
    FPicture[i] := TImage.Create(nil);
    FPicture[i].Bitmap.OnChange := PictureChanged;
  end;
end;

destructor TgxCubeMapImage.Destroy;
var
  i: TgxCubeMapTarget;
begin
  ReleaseBitmap32;
  for i := Low(FPicture) to High(FPicture) do
    FPicture[i].Free;
  inherited Destroy;
end;

procedure TgxCubeMapImage.Assign(Source: TPersistent);
var
  i: TgxCubeMapTarget;
begin
  if Assigned(Source) then
  begin
    if (Source is TgxCubeMapImage) then
    begin
      for i := Low(FPicture) to High(FPicture) do
        FPicture[i].Assign(TgxCubeMapImage(Source).FPicture[i]);
      Invalidate;
    end
    else
      inherited;
  end
  else
    inherited;
end;

function TgxCubeMapImage.GetWidth: Integer;
begin
  Result := FPicture[cmtPX].Bitmap.Width;
end;

function TgxCubeMapImage.GetHeight: Integer;
begin
  Result := FPicture[cmtPX].Bitmap.Height;
end;

function TgxCubeMapImage.GetDepth: Integer;
begin
  Result := 0;
end;

function TgxCubeMapImage.GetBitmap32: TgxImage;
var
  I: Integer;
  LImage: TgxImage;
begin
  if Assigned(FImage) then
    FImage.Free;
  LImage := TgxImage.Create;
  LImage.VerticalReverseOnAssignFromBitmap := True;

  try
    for I := 0 to 5 do
    begin
      FPicture[TgxCubeMapTarget(I)].Bitmap.OnChange := nil;
      try
        LImage.Assign(FPicture[TgxCubeMapTarget(I)].Bitmap);
        if not Assigned(FImage) then
        begin
          FImage := TgxImage.Create;
          FImage.Blank := True;
          FImage.Width := LImage.Width;
          FImage.Height := LImage.Height;
          FImage.SetColorFormatDataType(LImage.ColorFormat, LImage.DataType);
          FImage.CubeMap := True;
          FImage.Blank := False;
        end;
        Move(LImage.Data^, TFriendlyImage(FImage).GetLevelAddress(0, I)^, LImage.LevelSizeInByte[0]);
      finally
        FPicture[TgxCubeMapTarget(I)].Bitmap.OnChange := PictureChanged;
      end;
    end;
  finally
    LImage.Destroy;
  end;
  Result := FImage;
end;

// ReleaseBitmap32
//

procedure TgxCubeMapImage.ReleaseBitmap32;
begin
  if Assigned(FImage) then
  begin
    FImage.Free;
    FImage := nil;
  end;
end;

procedure TgxCubeMapImage.BeginUpdate;
var
  i: TgxCubeMapTarget;
begin
  Inc(FUpdateCounter);
  for i := Low(FPicture) to High(FPicture) do
    FPicture[i].Bitmap.OnChange := nil;
end;

procedure TgxCubeMapImage.EndUpdate;
var
  i: TgxCubeMapTarget;
begin
  Assert(FUpdateCounter > 0, ClassName + ': Unbalanced Begin/EndUpdate');
  Dec(FUpdateCounter);
  for i := Low(FPicture) to High(FPicture) do
    FPicture[i].Bitmap.OnChange := PictureChanged;
  if FUpdateCounter = 0 then
    PictureChanged(FPicture[cmtPX]);
end;

procedure TgxCubeMapImage.SaveToFile(const fileName: string);
var
  fs: TFileStream;
  bmp: TBitmap;
  i: TgxCubeMapTarget;
  version: Word;
begin
  fs := TFileStream.Create(fileName, fmCreate);
  bmp := TBitmap.Create;
  try
    version := $0100;
    fs.Write(version, 2);
    for i := Low(FPicture) to High(FPicture) do
    begin
      bmp.Assign(FPicture[i].Bitmap);
      bmp.SaveToStream(fs);
    end;
  finally
    bmp.Free;
    fs.Free;
  end;
end;

 
procedure TgxCubeMapImage.LoadFromFile(const fileName: string);
var
  fs: TFileStream;
  bmp: TBitmap;
  i: TgxCubeMapTarget;
  version: Word;
begin
  fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyWrite);
  bmp := TBitmap.Create;
  try
    fs.Read(version, 2);
    Assert(version = $0100);
    for i := Low(FPicture) to High(FPicture) do
    begin
      bmp.LoadFromStream(fs);
      FPicture[i].Bitmap := bmp;
    end;
  finally
    bmp.Free;
    fs.Free;
  end;
end;

 
class function TgxCubeMapImage.FriendlyName: string;
begin
  Result := 'CubeMap Image';
end;

class function TgxCubeMapImage.FriendlyDescription: string;
begin
  Result := 'Image data is contain 6 pictures of cubemap faces.';
end;

procedure TgxCubeMapImage.PictureChanged(Sender: TObject);
begin
  Invalidate;
end;

function TgxCubeMapImage.GetTextureTarget: TgxTextureTarget;
begin
  Result := ttTextureCube;
end;

procedure TgxCubeMapImage.SetPicture(index: TgxCubeMapTarget; const val: TImage);
begin
  FPicture[index].Assign(val);
end;

function TgxCubeMapImage.GetPicture(index: TgxCubeMapTarget): TImage;
begin
  Result := FPicture[index];
end;


// ------------------
// ------------------ TgxTexture ------------------
// ------------------

constructor TgxTexture.Create(AOwner: TPersistent);
begin
  inherited;
  FDisabled := True;
  FImage := TgxPersistentImage.Create(Self);
  FImage.OnTextureNeeded := DoOnTextureNeeded;
  FImageAlpha := tiaDefault;
  FImageBrightness := 1.0;
  FImageGamma := 1.0;
  FMagFilter := maLinear;
  FMinFilter := miLinearMipMapLinear;
  FFilteringQuality := tfIsotropic;
  FRequiredMemorySize := -1;
  FTextureHandle := TgxTextureHandle.Create;
  FSamplerHandle := TgxVirtualHandle.Create;
  FSamplerHandle.OnAllocate := OnSamplerAllocate;
  FSamplerHandle.OnDestroy := OnSamplerDestroy;
  FMappingMode := tmmUser;
  FEnvColor := TgxColor.CreateInitialized(Self, clrTransparent);
  FBorderColor := TgxColor.CreateInitialized(Self, clrTransparent);
  FNormalMapScale := cDefaultNormalMapScale;
  FTextureCompareMode := tcmNone;
  FTextureCompareFunc := cfLequal;
  FDepthTextureMode := dtmLuminance;
  TextureFormat := tfDefault;
  FCompression := tcDefault;
  FKeepImageAfterTransfer := False;
end;

destructor TgxTexture.Destroy;
begin
  FEnvColor.Free;
  FBorderColor.Free;
  FMapSCoordinates.Free;
  FMapTCoordinates.Free;
  FMapRCoordinates.Free;
  FMapQCoordinates.Free;
  DestroyHandles;
  FTextureHandle.Free;
  FSamplerHandle.Free;
  FImage.Free;
  inherited Destroy;
end;

procedure TgxTexture.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if (Source is TgxTexture) then
    begin
      if Source <> Self then
      begin
        FImageAlpha := TgxTexture(Source).FImageAlpha;
        FTextureMode := TgxTexture(Source).FTextureMode;
        FTextureWrap := TgxTexture(Source).FTextureWrap;
        FTextureFormat := TgxTexture(Source).FTextureFormat;
        FCompression := TgxTexture(Source).FCompression;
        FMinFilter := TgxTexture(Source).FMinFilter;
        FMagFilter := TgxTexture(Source).FMagFilter;
        FMappingMode := TgxTexture(Source).FMappingMode;
        MappingSCoordinates.Assign(TgxTexture(Source).MappingSCoordinates);
        MappingTCoordinates.Assign(TgxTexture(Source).MappingTCoordinates);
        MappingRCoordinates.Assign(TgxTexture(Source).MappingRCoordinates);
        MappingQCoordinates.Assign(TgxTexture(Source).MappingQCoordinates);
        FDisabled := TgxTexture(Source).FDisabled;
        SetImage(TgxTexture(Source).FImage);
        FImageBrightness := TgxTexture(Source).FImageBrightness;
        FImageGamma := TgxTexture(Source).FImageGamma;
        FFilteringQuality := TgxTexture(Source).FFilteringQuality;
        FEnvColor.Assign(TgxTexture(Source).FEnvColor);
        FBorderColor.Assign(TgxTexture(Source).FBorderColor);
        FNormalMapScale := TgxTexture(Source).FNormalMapScale;
        // Probably don't need to assign these....
        // FOnTextureNeeded := TgxTexture(Source).FImageGamma;
        // FRequiredMemorySize  : Integer;
        // FTexWidth, FTexHeight : Integer;
        FTextureHandle.NotifyChangesOfData;
        FSamplerHandle.NotifyChangesOfData;
      end;
    end
    else if (Source is TBitmap) then
      Image.Assign(Source)
    else if (Source is TImage) then
      Image.Assign(TImage(Source).Bitmap)
    else
      inherited Assign(Source);
  end
  else
  begin
    FDisabled := True;
    SetImage(nil);
    FTextureHandle.NotifyChangesOfData;
    FSamplerHandle.NotifyChangesOfData;
  end;
end;

procedure TgxTexture.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
  begin
    if Owner is TgxTextureExItem then
      TgxTextureExItem(Owner).NotifyChange(Self);
  end;
  if Sender is TgxTextureImage then
    FTextureHandle.NotifyChangesOfData;

  inherited;
end;

procedure TgxTexture.NotifyImageChange;
begin
  FTextureHandle.NotifyChangesOfData;
  NotifyChange(Self);
end;

procedure TgxTexture.NotifyParamsChange;
begin
  FSamplerHandle.NotifyChangesOfData;
  NotifyChange(Self);
end;

procedure TgxTexture.SetImage(AValue: TgxTextureImage);
begin
  if Assigned(aValue) then
  begin
    if FImage.ClassType <> AValue.ClassType then
    begin
      FImage.Free;
      FImage := TgxTextureImageClass(AValue.ClassType).Create(Self);
      FImage.OnTextureNeeded := DoOnTextureNeeded;
    end;
    FImage.Assign(AValue);
  end
  else
  begin
    FImage.Free;
    FImage := TgxPersistentImage.Create(Self);
    FImage.OnTextureNeeded := DoOnTextureNeeded;
  end;
end;

procedure TgxTexture.SetImageClassName(const val: string);
var
  newImage: TgxTextureImage;
  newImageClass: TgxTextureImageClass;
begin
  if val <> '' then
    if FImage.ClassName <> val then
    begin
      newImageClass := FindTextureImageClass(val);
      Assert(newImageClass <> nil, 'Make sure you include the unit for ' + val +
        ' in your uses clause');
      if newImageClass = nil then
        exit;
      newImage := newImageClass.Create(Self);
      newImage.OnTextureNeeded := DoOnTextureNeeded;
      FImage.Free;
      FImage := newImage;
    end;
end;

function TgxTexture.GetImageClassName: string;
begin
  Result := FImage.ClassName;
end;

function TgxTexture.TextureImageRequiredMemory: Integer;
var
  w, h, e, levelSize: Integer;
begin
  if FRequiredMemorySize < 0 then
  begin
    if IsCompressedFormat(fTextureFormat) then
    begin
      w := (Image.Width + 3) div 4;
      h := (Image.Height + 3) div 4;
    end
    else
    begin
      w := Image.Width;
      h := Image.Height;
    end;

    e := GetTextureElementSize(fTextureFormat);
    FRequiredMemorySize := w * h * e;
    if Image.Depth > 0 then
      FRequiredMemorySize := FRequiredMemorySize * Image.Depth;

    if not (MinFilter in [miNearest, miLinear]) then
    begin
      levelSize := FRequiredMemorySize;
      while e < levelSize do
      begin
        levelSize := levelSize div 4;
        FRequiredMemorySize := FRequiredMemorySize + levelSize;
      end;
    end;

    if Image.NativeTextureTarget = ttTextureCube then
      FRequiredMemorySize := FRequiredMemorySize * 6;
  end;
  Result := FRequiredMemorySize;
end;

procedure TgxTexture.SetImageAlpha(const val: TgxTextureImageAlpha);
begin
  if FImageAlpha <> val then
  begin
    FImageAlpha := val;
    NotifyImageChange;
  end;
end;

procedure TgxTexture.SetImageBrightness(const val: Single);
begin
  if FImageBrightness <> val then
  begin
    FImageBrightness := val;
    NotifyImageChange;
  end;
end;

function TgxTexture.StoreBrightness: Boolean;
begin
  Result := (FImageBrightness <> 1.0);
end;

procedure TgxTexture.SetImageGamma(const val: Single);
begin
  if FImageGamma <> val then
  begin
    FImageGamma := val;
    NotifyImageChange;
  end;
end;

function TgxTexture.StoreGamma: Boolean;
begin
  Result := (FImageGamma <> 1.0);
end;

procedure TgxTexture.SetMagFilter(AValue: TgxMagFilter);
begin
  if AValue <> FMagFilter then
  begin
    FMagFilter := AValue;
    NotifyParamsChange;
  end;
end;

procedure TgxTexture.SetMinFilter(AValue: TgxMinFilter);
begin
  if AValue <> FMinFilter then
  begin
    FMinFilter := AValue;
    NotifyParamsChange;
  end;
end;

procedure TgxTexture.SetTextureMode(AValue: TgxTextureMode);
begin
  if AValue <> FTextureMode then
  begin
    FTextureMode := AValue;
    NotifyParamsChange;
  end;
end;

procedure TgxTexture.SetDisabled(AValue: Boolean);
var
  intf: IgxTextureNotifyAble;
begin
  if AValue <> FDisabled then
  begin
    FDisabled := AValue;
    if Supports(Owner, IgxTextureNotifyAble, intf) then
      intf.NotifyTexMapChange(Self)
    else
      NotifyChange(Self);
  end;
end;

procedure TgxTexture.SetEnabled(const val: Boolean);
begin
  Disabled := not val;
end;

function TgxTexture.GetEnabled: Boolean;
begin
  Result := not Disabled;
end;

procedure TgxTexture.SetEnvColor(const val: TgxColor);
begin
  FEnvColor.Assign(val);
  NotifyParamsChange;
end;

procedure TgxTexture.SetBorderColor(const val: TgxColor);
begin
  FBorderColor.Assign(val);
  NotifyParamsChange;
end;

procedure TgxTexture.SetNormalMapScale(const val: Single);
begin
  if val <> FNormalMapScale then
  begin
    FNormalMapScale := val;
    if TextureFormat = tfNormalMap then
      NotifyImageChange;
  end;
end;

function TgxTexture.StoreNormalMapScale: Boolean;
begin
  Result := (FNormalMapScale <> cDefaultNormalMapScale);
end;

procedure TgxTexture.SetTextureWrap(AValue: TgxTextureWrap);
begin
  if AValue <> FTextureWrap then
  begin
    FTextureWrap := AValue;
    NotifyParamsChange;
  end;
end;

procedure TgxTexture.SetTextureWrapS(AValue: TgxSeparateTextureWrap);
begin
  if AValue <> FTextureWrapS then
  begin
    FTextureWrapS := AValue;
    NotifyParamsChange;
  end;
end;

procedure TgxTexture.SetTextureWrapT(AValue: TgxSeparateTextureWrap);
begin
  if AValue <> FTextureWrapT then
  begin
    FTextureWrapT := AValue;
    NotifyParamsChange;
  end;
end;

procedure TgxTexture.SetTextureWrapR(AValue: TgxSeparateTextureWrap);
begin
  if AValue <> FTextureWrapR then
  begin
    FTextureWrapR := AValue;
    NotifyParamsChange;
  end;
end;

function TgxTexture.GetTextureFormat: TgxTextureFormat;
var
  i: TgxTextureFormat;
begin
  if vDefaultTextureFormat = FTextureFormat then
  begin
    Result := tfDefault;
    Exit;
  end;
  for i := tfRGB to tfRGBAFloat32 do
  begin
    if cOldTextureFormatToInternalFormat[i] = FTextureFormat then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := tfExtended;
end;

procedure TgxTexture.SetTextureFormat(const val: TgxTextureFormat);
begin
  if val = tfDefault then
  begin
    FTextureFormat := vDefaultTextureFormat;
  end
  else if val < tfExtended then
  begin
    FTextureFormat := cOldTextureFormatToInternalFormat[val];
  end;
end;

procedure TgxTexture.SetTextureFormatEx(const val: TgxInternalFormat);
begin
  if val <> FTextureFormat then
  begin
    FTextureFormat := val;
    NotifyImageChange;
  end;
end;

function TgxTexture.StoreTextureFormatEx: Boolean;
begin
  Result := GetTextureFormat >= tfExtended;
end;

procedure TgxTexture.SetCompression(const val: TgxTextureCompression);
begin
  if val <> FCompression then
  begin
    FCompression := val;
    NotifyParamsChange;
  end;
end;

procedure TgxTexture.SetFilteringQuality(const val: TgxTextureFilteringQuality);
begin
  if val <> FFilteringQuality then
  begin
    FFilteringQuality := val;
    NotifyParamsChange;
  end;
end;

procedure TgxTexture.SetMappingMode(const val: TgxTextureMappingMode);
var
  texMapChange: Boolean;
  intf: IgxTextureNotifyAble;
begin
  if val <> FMappingMode then
  begin
    texMapChange := ((val = tmmUser) and (FMappingMode <> tmmUser))
      or ((val = tmmUser) and (FMappingMode <> tmmUser));
    FMappingMode := val;
    if texMapChange then
    begin
      // when switching between texGen modes and user mode, the geometry
      // must be rebuilt in whole (to specify/remove texCoord data!)
      if Supports(Owner, IgxTextureNotifyAble, intf) then
        intf.NotifyTexMapChange(Self);
    end
    else
      NotifyChange(Self);
  end;
end;

procedure TgxTexture.SetMappingSCoordinates(const val: TgxCoordinates4);
begin
  MappingSCoordinates.Assign(val);
end;

function TgxTexture.GetMappingSCoordinates: TgxCoordinates4;
begin
  if not Assigned(FMapSCoordinates) then
    FMapSCoordinates := TgxCoordinates4.CreateInitialized(Self, XHmgVector, csVector);
  Result := FMapSCoordinates;
end;

function TgxTexture.StoreMappingSCoordinates: Boolean;
begin
  if Assigned(FMapSCoordinates) then
    Result := not VectorEquals(FMapSCoordinates.AsVector, XHmgVector)
  else
    Result := false;
end;

procedure TgxTexture.SetMappingTCoordinates(const val: TgxCoordinates4);
begin
  MappingTCoordinates.Assign(val);
end;

function TgxTexture.GetMappingTCoordinates: TgxCoordinates4;
begin
  if not Assigned(FMapTCoordinates) then
    FMapTCoordinates := TgxCoordinates4.CreateInitialized(Self, YHmgVector,
      csVector);
  Result := FMapTCoordinates;
end;

function TgxTexture.StoreMappingTCoordinates: Boolean;
begin
  if Assigned(FMapTCoordinates) then
    Result := not VectorEquals(FMapTCoordinates.AsVector, YHmgVector)
  else
    Result := false;
end;

procedure TgxTexture.SetMappingRCoordinates(const val: TgxCoordinates4);
begin
  MappingRCoordinates.Assign(val);
end;

function TgxTexture.GetMappingRCoordinates: TgxCoordinates4;
begin
  if not Assigned(FMapRCoordinates) then
    FMapRCoordinates := TgxCoordinates4.CreateInitialized(Self, ZHmgVector,
      csVector);
  Result := FMapRCoordinates;
end;

function TgxTexture.StoreMappingRCoordinates: Boolean;
begin
  if Assigned(FMapRCoordinates) then
    Result := not VectorEquals(FMapRCoordinates.AsVector, ZHmgVector)
  else
    Result := false;
end;

procedure TgxTexture.SetMappingQCoordinates(const val: TgxCoordinates4);
begin
  MappingQCoordinates.Assign(val);
end;

function TgxTexture.GetMappingQCoordinates: TgxCoordinates4;
begin
  if not Assigned(FMapQCoordinates) then
    FMapQCoordinates := TgxCoordinates4.CreateInitialized(Self, WHmgVector,
      csVector);
  Result := FMapQCoordinates;
end;

function TgxTexture.StoreMappingQCoordinates: Boolean;
begin
  if Assigned(FMapQCoordinates) then
    Result := not VectorEquals(FMapQCoordinates.AsVector, WHmgVector)
  else
    Result := false;
end;

function TgxTexture.StoreImageClassName: Boolean;
begin
  Result := (FImage.ClassName <> TgxPersistentImage.ClassName);
end;

procedure TgxTexture.SetTextureCompareMode(const val: TgxTextureCompareMode);
begin
  if val <> fTextureCompareMode then
  begin
    fTextureCompareMode := val;
    NotifyParamsChange;
  end;
end;

procedure TgxTexture.SetTextureCompareFunc(const val: TgxDepthCompareFunc);
begin
  if val <> fTextureCompareFunc then
  begin
    fTextureCompareFunc := val;
    NotifyParamsChange;
  end;
end;

procedure TgxTexture.SetDepthTextureMode(const val: TgxDepthTextureMode);
begin
  if val <> fDepthTextureMode then
  begin
    fDepthTextureMode := val;
    NotifyParamsChange;
  end;
end;

procedure TgxTexture.PrepareBuildList;
begin
  GetHandle;
end;

procedure TgxTexture.ApplyMappingMode;
begin
  case MappingMode of
    tmmUser: ; // nothing to do, but checked first (common case)
    tmmObjectLinear:
      begin
        glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
        glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
        glTexGenfv(GL_S, GL_OBJECT_PLANE, @MappingSCoordinates.DirectVector);
        glTexGenfv(GL_T, GL_OBJECT_PLANE, @MappingTCoordinates.DirectVector);
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);

        /// if GL_TEXTURE_CUBE_MAP or GL_TEXTURE_3D then
        begin
          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenfv(GL_R, GL_OBJECT_PLANE, @MappingRCoordinates.DirectVector);
          glTexGenfv(GL_Q, GL_OBJECT_PLANE, @MappingQCoordinates.DirectVector);
          glEnable(GL_TEXTURE_GEN_R);
          glEnable(GL_TEXTURE_GEN_Q);
        end;
      end;
    tmmEyeLinear:
      begin
        glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
        glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
        // specify planes in eye space, not world space
        glMatrixMode(GL_MODELVIEW);
        glPushMatrix;
        glLoadIdentity;
        glTexGenfv(GL_S, GL_EYE_PLANE, @MappingSCoordinates.DirectVector);
        glTexGenfv(GL_T, GL_EYE_PLANE, @MappingTCoordinates.DirectVector);
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        /// if GL_TEXTURE_CUBE_MAP or GL_TEXTURE_3D then
        begin
          glTexGenfv(GL_R, GL_EYE_PLANE, @MappingRCoordinates.DirectVector);
          glTexGenfv(GL_Q, GL_EYE_PLANE, @MappingQCoordinates.DirectVector);
          glEnable(GL_TEXTURE_GEN_R);
          glEnable(GL_TEXTURE_GEN_Q);
        end;
        glPopMatrix;
      end;
    tmmSphere:
      begin
        glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
        glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
      end;
    tmmCubeMapReflection, tmmCubeMapCamera:
    /// if GL_TEXTURE_CUBE_MAP then
      begin
        glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
        glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
        glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        glEnable(GL_TEXTURE_GEN_R);
      end;
    tmmCubeMapNormal, tmmCubeMapLight0:
    /// if GL_TEXTURE_CUBE_MAP then
      begin
        glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
        glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
        glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        glEnable(GL_TEXTURE_GEN_R);
      end;
  else
    Assert(False);
  end;
end;

procedure TgxTexture.UnApplyMappingMode;
begin
  if MappingMode <> tmmUser then
  begin
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
    /// if GL_TEXTURE_3D or GL_TEXTURE_CUBE_MAP then
    begin
      glDisable(GL_TEXTURE_GEN_R);
      glDisable(GL_TEXTURE_GEN_Q);
    end;
  end;
end;

procedure TgxTexture.Apply(var rci: TgxRenderContextInfo);

  procedure SetCubeMapTextureMatrix;
  var
    m, mm: TMatrix4f;
  begin
    // compute model view matrix for proper viewing
    case MappingMode of
      tmmCubeMapReflection, tmmCubeMapNormal:
        begin
          m := rci.PipelineTransformation.ViewMatrix^;
          NormalizeMatrix(m);
          TransposeMatrix(m);
          rci.gxStates.SetTextureMatrix(m);
        end;
      tmmCubeMapLight0:
        begin
          with TgxScene(rci.scene).Lights do
            if Count > 0 then
            begin
              m := TgxLightSource(Items[0]).AbsoluteMatrix;
              NormalizeMatrix(m);
              mm := rci.PipelineTransformation.ViewMatrix^;
              NormalizeMatrix(mm);
              TransposeMatrix(mm);
              m := MatrixMultiply(m, mm);
              rci.gxStates.SetTextureMatrix(m);
            end;
        end;
      tmmCubeMapCamera:
        begin
          m.X := VectorCrossProduct(rci.cameraUp, rci.cameraDirection);
          m.Y := VectorNegate(rci.cameraDirection);
          m.Z := rci.cameraUp;
          m.W := WHmgPoint;
          mm := rci.PipelineTransformation.ViewMatrix^;
          NormalizeMatrix(mm);
          TransposeMatrix(mm);
          m := MatrixMultiply(m, mm);
          rci.gxStates.SetTextureMatrix(m);
        end;
    end;
  end;
var
  H : Cardinal;
begin
  // Multisample image do not work with FFP
  if (FTextureHandle.Target = ttTexture2DMultisample) or
    (FTextureHandle.Target = ttTexture2DMultisampleArray) then
    exit;

  H := Handle;
  if not Disabled and (H > 0) then
  begin
    with rci.gxStates do
    begin
      ActiveTexture := 0;
      TextureBinding[0, FTextureHandle.Target] := H;
      ActiveTextureEnabled[FTextureHandle.Target] := True;
    end;

    if not rci.gxStates.ForwardContext then
    begin
      if FTextureHandle.Target = ttTextureCube then
        SetCubeMapTextureMatrix;
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE,
        cTextureMode[FTextureMode]);
      glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);
      ApplyMappingMode;
      xglMapTexCoordToMain;
    end;
  end
  else if not rci.gxStates.ForwardContext then
  begin // default
    xglMapTexCoordToMain;
  end;
end;

procedure TgxTexture.UnApply(var rci: TgxRenderContextInfo);
begin
  if not Disabled
    and not rci.gxStates.ForwardContext then
  begin
    // Multisample image do not work with FFP
    if FTextureHandle.Target in [ttNoShape, ttTexture2DMultisample, ttTexture2DMultisampleArray] then
      exit;
    with rci.gxStates do
    begin
      ActiveTexture := 0;
      ActiveTextureEnabled[FTextureHandle.Target] := False;
      if FTextureHandle.Target = ttTextureCube then
        ResetTextureMatrix;
    end;
    UnApplyMappingMode;
  end;
end;

procedure TgxTexture.ApplyAsTexture2(var rci: TgxRenderContextInfo; textureMatrix:
  PMatrix4f = nil);
begin
  ApplyAsTextureN(2, rci, textureMatrix);
end;

procedure TgxTexture.UnApplyAsTexture2(var rci: TgxRenderContextInfo;
  reloadIdentityTextureMatrix: boolean);
begin
  UnApplyAsTextureN(2, rci, reloadIdentityTextureMatrix);
end;

procedure TgxTexture.ApplyAsTextureN(n: Integer; var rci: TgxRenderContextInfo;
  textureMatrix: PMatrix4f = nil);
var
  m: TMatrix4f;
begin
  if not Disabled then
  begin
    // Multisample image do not work with FFP
    if (FTextureHandle.Target = ttTexture2DMultisample) or
      (FTextureHandle.Target = ttTexture2DMultisampleArray) then
      exit;
    with rci.gxStates do
    begin
      ActiveTexture := n - 1;
      TextureBinding[n - 1, FTextureHandle.Target] := Handle;
      ActiveTextureEnabled[FTextureHandle.Target] := True;
      if Assigned(textureMatrix) then
        SetTextureMatrix(textureMatrix^)
      else if FTextureHandle.Target = ttTextureCube then
      begin
        m := rci.PipelineTransformation.ModelViewMatrix^;
        NormalizeMatrix(m);
        TransposeMatrix(m);
        rci.gxStates.SetTextureMatrix(m);
      end;

      if not ForwardContext then
      begin
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
        glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);
        ApplyMappingMode;
        ActiveTexture := 0;
      end;
    end;
  end;
end;

procedure TgxTexture.UnApplyAsTextureN(n: Integer; var rci: TgxRenderContextInfo;
  reloadIdentityTextureMatrix: boolean);
begin
  if not rci.gxStates.ForwardContext then
  begin
    // Multisample image do not work with FFP
    if (FTextureHandle.Target = ttTexture2DMultisample) or
      (FTextureHandle.Target = ttTexture2DMultisampleArray) then
      exit;
    with rci.gxStates do
    begin
      ActiveTexture := n - 1;
      ActiveTextureEnabled[FTextureHandle.Target] := False;
      UnApplyMappingMode;
      if (FTextureHandle.Target = ttTextureCube) or reloadIdentityTextureMatrix then
        ResetTextureMatrix;
      ActiveTexture := 0;
    end;
  end;
end;

function TgxTexture.AllocateHandle: Cardinal;
var
  vTarget: TgxTextureTarget;
begin
  vTarget := Image.NativeTextureTarget;
  if (vTarget <> ttNoShape) and (FTextureHandle.Target <> vTarget) then
    FTextureHandle.DestroyHandle;

  Result := FTextureHandle.Handle;
  if Result = 0 then
  begin
    FTextureHandle.AllocateHandle;
    Result := FTextureHandle.Handle;
  end;
  if FTextureHandle.IsDataNeedUpdate then
  begin
    FTextureHandle.Target := vTarget;
    FSamplerHandle.NotifyChangesOfData;
  end;
  if FSamplerHandle.Handle = 0 then
    FSamplerHandle.AllocateHandle;

  // bind texture
  if (FTextureHandle.Target <> ttNoShape) and
    IsTargetSupported(FTextureHandle.Target) then
  begin
    if FSamplerHandle.IsDataNeedUpdate then
    begin
      with CurrentContext.gxStates do
        TextureBinding[ActiveTexture, FTextureHandle.Target] := Result;
      PrepareParams(DecodeTextureTarget(FTextureHandle.Target));
      FSamplerHandle.NotifyDataUpdated;
    end;
  end
  else
    Result := 0;
end;

function TgxTexture.IsHandleAllocated: Boolean;
begin
  Result := (FTextureHandle.Handle <> 0);
end;

function TgxTexture.GetHandle: Cardinal;
var
  target: Cardinal;
  LBinding: array[TgxTextureTarget] of Cardinal;

  procedure StoreBindings;
  var
    t: TgxTextureTarget;
  begin
    with CurrentContext.gxStates do
    begin
      if TextureBinding[ActiveTexture, FTextureHandle.Target] = FTextureHandle.Handle then
        TextureBinding[ActiveTexture, FTextureHandle.Target] := 0;
      for t := Low(TgxTextureTarget) to High(TgxTextureTarget) do
        LBinding[t] := TextureBinding[ActiveTexture, t];
    end;
  end;
  procedure RestoreBindings;
  var
    t: TgxTextureTarget;
  begin
    with CurrentContext.gxStates do
      for t := Low(TgxTextureTarget) to High(TgxTextureTarget) do
        TextureBinding[ActiveTexture, t] := LBinding[t];
  end;

begin
  with CurrentContext.gxStates do
  begin
    StoreBindings;
    try
      Result := AllocateHandle;
      if FTextureHandle.IsDataNeedUpdate then
      begin
        FTextureHandle.NotifyDataUpdated;
        // Check supporting
        target := DecodeTextureTarget(Image.NativeTextureTarget);
        if not IsTargetSupported(target)
          or not IsFormatSupported(TextureFormatEx) then
        begin
          SetTextureErrorImage;
          target := GL_TEXTURE_2D;
        end;
        // Load images
        // if not GL_EXT_direct_state_access then
        TextureBinding[ActiveTexture, FTextureHandle.Target] := Result;
        PrepareImage(target);
      end;
    finally
      RestoreBindings;
    end;
  end;
end;

procedure TgxTexture.DestroyHandles;
begin
  FTextureHandle.DestroyHandle;
  FSamplerHandle.DestroyHandle;
  FRequiredMemorySize := -1;
end;

function TgxTexture.IsFloatType: Boolean;
begin
  Result := IsFloatFormat(TextureFormatEx);
end;

function TgxTexture.OpenGLTextureFormat: Integer;
var
  texComp: TgxTextureCompression;

begin
  if GL_TEXTURE_COMPRESSION_HINT > 0 then ///
  begin
    if Compression = tcDefault then
      if vDefaultTextureCompression = tcDefault then
        texComp := tcNone
      else
        texComp := vDefaultTextureCompression
    else
      texComp := Compression;
  end
  else
    texComp := tcNone;

  if IsFloatType then
    texComp := tcNone; // no compression support for float_type

  if (texComp <> tcNone) and (TextureFormat <= tfNormalMap) then
    with CurrentContext.gxStates do
    begin
      case texComp of
        tcStandard: TextureCompressionHint := hintDontCare;
        tcHighQuality: TextureCompressionHint := hintNicest;
        tcHighSpeed: TextureCompressionHint := hintFastest;
      else
        Assert(False);
      end;
      Result := CompressedInternalFormatToOpenGL(TextureFormatEx);
    end
  else
    Result := InternalFormatToOpenGLFormat(TextureFormatEx);
end;

procedure TgxTexture.PrepareImage(target: Cardinal);
var
  bitmap32: TgxImage;
  texComp: TgxTextureCompression;
  glFormat: Cardinal;
begin
  if Image.IsSelfLoading then
  begin
    Image.LoadTexture(FTextureFormat);
  end
  else
  begin

    bitmap32 := Image.GetBitmap32;

    if (bitmap32 = nil) or bitmap32.IsEmpty then
      Exit;

    if TextureFormat = tfNormalMap then
      bitmap32.GrayScaleToNormalMap(NormalMapScale,
        TextureWrap in [twBoth, twHorizontal],
        TextureWrap in [twBoth, twVertical]);
    // prepare AlphaChannel
    case ImageAlpha of
      tiaDefault: ; // nothing to do
      tiaAlphaFromIntensity:
        bitmap32.SetAlphaFromIntensity;
      tiaSuperBlackTransparent:
        bitmap32.SetAlphaTransparentForColor($000000);
      tiaLuminance:
        bitmap32.SetAlphaFromIntensity;
      tiaLuminanceSqrt:
        begin
          bitmap32.SetAlphaFromIntensity;
          bitmap32.SqrtAlpha;
        end;
      tiaOpaque:
        bitmap32.SetAlphaToValue(255);
      tiaTopLeftPointColorTransparent:
        begin
          bitmap32.Narrow;
          bitmap32.SetAlphaTransparentForColor(bitmap32.Data^[0]);
        end;
      tiaInverseLuminance:
        begin
          bitmap32.SetAlphaFromIntensity;
          bitmap32.InvertAlpha;
        end;
      tiaInverseLuminanceSqrt:
        begin
          bitmap32.SetAlphaFromIntensity;
          bitmap32.SqrtAlpha;
          bitmap32.InvertAlpha;
        end;
      tiaBottomRightPointColorTransparent:
        begin
          bitmap32.Narrow;
          bitmap32.SetAlphaTransparentForColor(bitmap32.Data^[bitmap32.Width - 1]);
        end;
    else
      Assert(False);
    end;
    // apply brightness correction
    if FImageBrightness <> 1.0 then
      bitmap32.BrightnessCorrection(FImageBrightness);
    // apply gamma correction
    if FImageGamma <> 1.0 then
      bitmap32.GammaCorrection(FImageGamma);

    if /// GL_ARB_texture_compression and
    (TextureFormat <> tfExtended) then
    begin
      if Compression = tcDefault then
        if vDefaultTextureCompression = tcDefault then
          texComp := tcNone
        else
          texComp := vDefaultTextureCompression
      else
        texComp := Compression;
      if IsFloatType then
        texComp := tcNone;

    end
    else
      texComp := tcNone;

    if (texComp <> tcNone) and (TextureFormat <= tfNormalMap) then
      with CurrentContext.gxStates do
      begin
        case texComp of
          tcStandard: TextureCompressionHint := hintDontCare;
          tcHighQuality: TextureCompressionHint := hintNicest;
          tcHighSpeed: TextureCompressionHint := hintFastest;
        else
          Assert(False, strErrorEx + strUnknownType);
        end;
        glFormat := CompressedInternalFormatToOpenGL(FTextureFormat);
      end
    else
      glFormat := InternalFormatToOpenGLFormat(FTextureFormat);

    bitmap32.RegisterAsOpenRXTexture(
      FTextureHandle,
      not (FMinFilter in [miNearest, miLinear]),
      glFormat,
      FTexWidth,
      FTexHeight,
      FTexDepth);
  end;

  if glGetError <> GL_NO_ERROR then
  begin
    SetTextureErrorImage;
  end
  else
  begin
    FRequiredMemorySize := -1;
    TextureImageRequiredMemory;
    if not IsDesignTime and not FKeepImageAfterTransfer then
      Image.ReleaseBitmap32;
  end;
end;

procedure TgxTexture.PrepareParams(target: Cardinal);
const
  cTextureSWrap: array[twBoth..twHorizontal] of Cardinal =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, GL_REPEAT);
  cTextureTWrap: array[twBoth..twHorizontal] of Cardinal =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_REPEAT, GL_CLAMP_TO_EDGE);
  cTextureRWrap: array[twBoth..twHorizontal] of Cardinal =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_REPEAT, GL_CLAMP_TO_EDGE);
  cTextureSWrapOld: array[twBoth..twHorizontal] of Cardinal =
    (GL_REPEAT, GL_CLAMP, GL_CLAMP, GL_REPEAT);
  cTextureTWrapOld: array[twBoth..twHorizontal] of Cardinal =
    (GL_REPEAT, GL_CLAMP, GL_REPEAT, GL_CLAMP);
  cTextureMagFilter: array[maNearest..maLinear] of Cardinal =
    (GL_NEAREST, GL_LINEAR);
  cTextureMinFilter: array[miNearest..miLinearMipmapLinear] of Cardinal =
    (GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_LINEAR);
  cFilteringQuality: array[tfIsotropic..tfAnisotropic] of Integer = (1, 2);
  cSeparateTextureWrap: array[twRepeat..twMirrorClampToBorder] of Cardinal =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER,
    GL_MIRRORED_REPEAT, GL_MIRROR_CLAMP_TO_EDGE_ATI, GL_MIRROR_CLAMP_TO_BORDER_EXT);
  cTextureCompareMode: array[tcmNone..tcmCompareRtoTexture] of Cardinal =
    (GL_NONE, GL_COMPARE_R_TO_TEXTURE);
  cDepthTextureMode: array[dtmLuminance..dtmAlpha] of Cardinal =
    (GL_LUMINANCE, GL_INTENSITY, GL_ALPHA);

var
  lMinFilter: TgxMinFilter;
begin
  if (target = GL_TEXTURE_2D_MULTISAMPLE)
    or (target = GL_TEXTURE_2D_MULTISAMPLE_ARRAY) then
    Exit;

  with CurrentContext.gxStates do
  begin
    UnpackAlignment := 1;
    UnpackRowLength := 0;
    UnpackSkipRows := 0;
    UnpackSkipPixels := 0;
  end;

  glTexParameterfv(target, GL_TEXTURE_BORDER_COLOR, FBorderColor.AsAddress);

  /// if (GL_VERSION_1_2 or GL_texture_edge_clamp) then
  begin
    if FTextureWrap = twSeparate then
    begin
      glTexParameteri(target, GL_TEXTURE_WRAP_S,
        cSeparateTextureWrap[FTextureWrapS]);
      glTexParameteri(target, GL_TEXTURE_WRAP_T,
        cSeparateTextureWrap[FTextureWrapT]);
      if (target = GL_TEXTURE_3D) then ///
        glTexParameteri(target, GL_TEXTURE_WRAP_R,
          cSeparateTextureWrap[FTextureWrapR]);
    end
    else
    begin
      glTexParameteri(target, GL_TEXTURE_WRAP_S, cTextureSWrap[FTextureWrap]);
      glTexParameteri(target, GL_TEXTURE_WRAP_T, cTextureTWrap[FTextureWrap]);
      /// if R_Dim then
        glTexParameteri(target, GL_TEXTURE_WRAP_R, cTextureRWrap[FTextureWrap]);
    end;
  end;
(*
  else
  begin
    glTexParameteri(target, GL_TEXTURE_WRAP_S, cTextureSWrapOld[FTextureWrap]);
    glTexParameteri(target, GL_TEXTURE_WRAP_T, cTextureTWrapOld[FTextureWrap]);
  end;
*)
  lMinFilter := FMinFilter;
  // Down paramenter to rectangular texture supported
  if (target = GL_TEXTURE_RECTANGLE)
  /// or not (GL_EXT_texture_lod_bias or GL_SGIS_texture_lod)
  then
  begin
    if lMinFilter in [miNearestMipmapNearest, miNearestMipmapLinear] then
      lMinFilter := miNearest;
    if FMinFilter in [miLinearMipmapNearest, miLinearMipmapLinear] then
      lMinFilter := miLinear;
  end;

  glTexParameteri(target, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[lMinFilter]);
  glTexParameteri(target, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMagFilter]);

  /// if GL_EXT_texture_filter_anisotropic then
    glTexParameteri(target, GL_TEXTURE_MAX_ANISOTROPY_EXT,
      cFilteringQuality[FFilteringQuality]);

  if IsDepthFormat(fTextureFormat) then
  begin
    glTexParameteri(target, GL_TEXTURE_COMPARE_MODE,
      cTextureCompareMode[fTextureCompareMode]);
    glTexParameteri(target, GL_TEXTURE_COMPARE_FUNC,
      cGLComparisonFunctionToGLEnum[fTextureCompareFunc]);
   /// if not FTextureHandle.RenderingContext.gxStates.ForwardContext then
      glTexParameteri(target, GL_DEPTH_TEXTURE_MODE,
        cDepthTextureMode[fDepthTextureMode]);
  end;
end;

procedure TgxTexture.DoOnTextureNeeded(Sender: TObject; var textureFileName:
  string);
begin
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Sender, textureFileName);
end;

procedure TgxTexture.OnSamplerAllocate(Sender: TgxVirtualHandle; var Handle: Cardinal);
begin
  Handle := 1;
end;

procedure TgxTexture.OnSamplerDestroy(Sender: TgxVirtualHandle; var Handle: Cardinal);
begin
  Handle := 0;
end;

procedure TgxTexture.SetTextureErrorImage;
var
  img: TgxImage;
begin
  img := TgxImage.Create;
  img.SetErrorImage;

  ImageClassName := TgxBlankImage.className;
  TgxBlankImage(Image).Assign(img);
  img.Free;

  MagFilter := maNearest;
  MinFilter := miNearest;
  TextureWrap := twBoth;
  MappingMode := tmmUser;
  Compression := tcNone;
  AllocateHandle;
end;


// ---------------
// --------------- TgxTextureExItem ---------------
// ---------------

constructor TgxTextureExItem.Create(ACollection: TCollection);
begin
  inherited;

  FTexture := TgxTexture.Create(Self);
  FTextureOffset := TgxCoordinates.CreateInitialized(Self, NullHMGVector,
    csPoint);
  FTextureOffset.OnNotifyChange := OnNotifyChange;
  FTextureScale := TgxCoordinates.CreateInitialized(Self, XYZHmgVector,
    csPoint);
  FTextureScale.OnNotifyChange := OnNotifyChange;

  FTextureIndex := ID;
  FTextureMatrix := IdentityHMGMatrix;

  // not very flexible code, assumes it's owned by a material,
  // that has a Texture property, but may need to re-implement it somehow
  (*
    if ACollection is TgxTextureEx then
    if TgxTextureEx(ACollection).FOwner <> nil then
      FTexture.OnTextureNeeded := TgxTextureEx(ACollection).FOwner.Texture.OnTextureNeeded;
   *)
end;

destructor TgxTextureExItem.Destroy;
begin
  FTexture.Free;
  FTextureOffset.Free;
  FTextureScale.Free;

  inherited;
end;


function TgxTextureExItem.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TgxTextureExItem._AddRef: Integer; stdcall;
begin
  Result := -1; //ignore
end;

function TgxTextureExItem._Release: Integer; stdcall;
begin
  Result := -1; //ignore
end;

procedure TgxTextureExItem.Assign(Source: TPersistent);
begin
  if Source is TgxTextureExItem then
  begin
    Texture := TgxTextureExItem(Source).Texture;
    TextureIndex := TgxTextureExItem(Source).TextureIndex;
    TextureOffset := TgxTextureExItem(Source).TextureOffset;
    TextureScale := TgxTextureExItem(Source).TextureScale;
    NotifyChange(Self);
  end
  else
    inherited;
end;

procedure TgxTextureExItem.NotifyChange(Sender: TObject);
begin
  if Assigned(Collection) then
    TgxTextureEx(Collection).NotifyChange(Self);
end;

procedure TgxTextureExItem.Apply(var rci: TgxRenderContextInfo);
begin
  FApplied := False;
  if FTexture.Enabled then
  begin
    rci.gxStates.ActiveTexture := FTextureIndex;
    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    if FTextureMatrixIsIdentity then
      glLoadIdentity
    else
      glLoadMatrixf(@FTextureMatrix.X.X);
    glMatrixMode(GL_MODELVIEW);
    rci.gxStates.ActiveTexture := 0;
    if FTextureIndex = 0 then
      FTexture.Apply(rci)
    else if FTextureIndex = 1 then
      FTexture.ApplyAsTexture2(rci, nil)
    else if FTextureIndex >= 2 then
      FTexture.ApplyAsTextureN(FTextureIndex + 1, rci, nil);
    FApplied := True;
  end;
end;

procedure TgxTextureExItem.UnApply(var rci: TgxRenderContextInfo);
begin
  if FApplied then
  begin
    if FTextureIndex = 0 then
      FTexture.UnApply(rci)
    else if FTextureIndex = 1 then
      FTexture.UnApplyAsTexture2(rci, false)
    else if FTextureIndex >= 2 then
      FTexture.UnApplyAsTextureN(FTextureIndex + 1, rci, false);
    rci.gxStates.ActiveTexture := FTextureIndex;
    glMatrixMode(GL_TEXTURE);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    rci.gxStates.ActiveTexture := 0;
    FApplied := False;
  end;
end;

function TgxTextureExItem.GetDisplayName: string;
begin
  Result := Format('Tex [%d]', [FTextureIndex]);
end;

function TgxTextureExItem.GetOwner: TPersistent;
begin
  Result := Collection;
end;

procedure TgxTextureExItem.NotifyTexMapChange(Sender: TObject);
var
  intf: IgxTextureNotifyAble;
begin
  if Supports(TObject(TgxTextureEx(Collection).FOwner), IgxTextureNotifyAble,
    intf) then
    intf.NotifyTexMapChange(Sender);
end;

procedure TgxTextureExItem.SetTexture(const Value: TgxTexture);
begin
  FTexture.Assign(Value);
  NotifyChange(Self);
end;

procedure TgxTextureExItem.SetTextureIndex(const Value: Integer);
var
  temp: Integer;
begin
  temp := Value;
  if temp < 0 then
    temp := 0;
  if temp <> FTextureIndex then
  begin
    FTextureIndex := temp;
    NotifyChange(Self);
  end;
end;

procedure TgxTextureExItem.SetTextureOffset(const Value: TgxCoordinates);
begin
  FTextureOffset.Assign(Value);
  NotifyChange(Self);
end;

procedure TgxTextureExItem.SetTextureScale(const Value: TgxCoordinates);
begin
  FTextureScale.Assign(Value);
  NotifyChange(Self);
end;

procedure TgxTextureExItem.CalculateTextureMatrix;
begin
  if TextureOffset.Equals(NullHmgVector) and TextureScale.Equals(XYZHmgVector) then
    FTextureMatrixIsIdentity := True
  else
  begin
    FTextureMatrixIsIdentity := False;
    FTextureMatrix := CreateScaleAndTranslationMatrix(TextureScale.AsVector,
      TextureOffset.AsVector);
  end;
  NotifyChange(Self);
end;

procedure TgxTextureExItem.OnNotifyChange(Sender: TObject);
begin
  CalculateTextureMatrix;
end;

// ---------------
// --------------- TgxTextureEx ---------------
// ---------------

constructor TgxTextureEx.Create(AOwner: TgxUpdateAbleObject);
begin
  inherited Create(TgxTextureExItem);
  FOwner := AOwner;
end;

procedure TgxTextureEx.NotifyChange(Sender: TObject);
begin
  if Assigned(FOwner) then
    FOwner.NotifyChange(Self);
end;

procedure TgxTextureEx.Apply(var rci: TgxRenderContextInfo);
var
  i, texUnits: Integer;
  units: Cardinal;
begin
  /// if not (GL_ARB_multitexture) then exit;

  units := 0;
  glGetIntegeri_v(GL_MAX_TEXTURE_UNITS, 0, @texUnits);
  for i := 0 to Count - 1 do
  begin
    if Items[i].TextureIndex < texUnits then
    begin
      Items[i].Apply(rci);
      if Items[i].FApplied then
        if (Items[i].TextureIndex > 0) and (Items[i].Texture.MappingMode =
          tmmUser) then
          units := units or (1 shl Items[i].TextureIndex);
    end;
  end;
  if units > 0 then
    xglMapTexCoordToArbitraryAdd(units);
end;

procedure TgxTextureEx.UnApply(var rci: TgxRenderContextInfo);
var
  i: Integer;
begin
  /// if not GL_ARB_multitexture then exit;
  for i := 0 to Count - 1 do
    Items[i].UnApply(rci);
end;

function TgxTextureEx.Add: TgxTextureExItem;
begin
  Result := TgxTextureExItem(inherited Add);
end;

procedure TgxTextureEx.Loaded;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].CalculateTextureMatrix;
end;

function TgxTextureEx.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TgxTextureEx.SetItems(index: Integer; const Value: TgxTextureExItem);
begin
  inherited SetItem(index, Value);
end;

function TgxTextureEx.GetItems(index: Integer): TgxTextureExItem;
begin
  Result := TgxTextureExItem(inherited GetItem(index));
end;

function TgxTextureEx.IsTextureEnabled(Index: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Self = nil then
    Exit;
  for i := 0 to Count - 1 do
    if Items[i].TextureIndex = Index then
      Result := Result or Items[i].Texture.Enabled;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterTextureImageClass(TgxBlankImage);
  RegisterTextureImageClass(TgxPersistentImage);
  RegisterTextureImageClass(TgxPicFileImage);
  RegisterTextureImageClass(TgxCubeMapImage);
  RegisterTGraphicClassFileExtension('.bmp', TBitmap);

finalization

  vGxTextureImageClasses.Free;
  vGxTextureImageClasses := nil;

end.
