//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Material;

(* Handles all the material + material library stuff *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  System.Types,
  FMX.Dialogs,
  FMX.Graphics,

  GXS.XOpenGL,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.PersistentClasses,
  GXS.Strings,
  GXS.ApplicationFileIO,

  GXS.RenderContextInfo,
  GXS.BaseClasses,
  GXS.Context,
  GXS.Texture,
  GXS.Color,
  GXS.Coordinates,
  GXS.State,
  GXS.TextureFormat,
  GXS.Graphics,
  GXS.Utils;

{$UNDEF USE_MULTITHREAD}

type
  TgxFaceProperties = class;
  TgxMaterial = class;
  TgxAbstractMaterialLibrary = class;
  TgxMaterialLibrary = class;

  // an interface for proper TgxLibMaterialNameProperty support
  IgxMaterialLibrarySupported = interface(IInterface)
    ['{8E442AF9-D212-4A5E-8A88-92F798BABFD1}']
    function GetMaterialLibrary: TgxAbstractMaterialLibrary;
  end;

  TgxAbstractLibMaterial = class;
  TgxLibMaterial = class;

  (* Define VXShader style application relatively to a material.
    ssHighLevel: shader is applied before material application, and unapplied
         after material unapplication
    ssLowLevel: shader is applied after material application, and unapplied
         before material unapplication
    ssReplace: shader is applied in place of the material (and material
         is completely ignored) *)
  TgxShaderStyle = (ssHighLevel, ssLowLevel, ssReplace);

  (* Defines what to do if for some reason shader failed to initialize.
    fiaSilentdisable:          just disable it
    fiaRaiseHandledException:  raise an exception, and handle it right away
    (usefull, when debigging within Delphi)
    fiaRaiseStardardException: raises the exception with a string from this
    function GetStardardNotSupportedMessage
    fiaReRaiseException:       Re-raises the exception
    fiaGenerateEvent:          Handles the exception, but generates an event
    that user can respond to. For example, he can
    try to compile a substitude shader, or replace
    it by a material.
    Note: HandleFailedInitialization does *not*
    create this event, it is left to user shaders
    which may chose to override this procedure.
    Commented out, because not sure if this
    option should exist, let other generations of developers decide ;) *)
  TgxShaderFailedInitAction = (fiaSilentDisable, fiaRaiseStandardException, fiaRaiseHandledException, fiaReRaiseException
    (* ,fiaGenerateEvent *) );

  (* Generic, abstract shader class.
    Shaders are modeled here as an abstract material-altering entity with
    transaction-like behaviour. The base class provides basic context and user
    tracking, as well as setup/application facilities.
    Subclasses are expected to provide implementation for DoInitialize,
    DoApply, DoUnApply and DoFinalize. *)
  TgxShader = class(TgxUpdateAbleComponent)
  private
    FEnabled: Boolean;
    FLibMatUsers: TList;
    FVirtualHandle: TgxVirtualHandle;
    FShaderStyle: TgxShaderStyle;
    FUpdateCount: Integer;
    FShaderActive: Boolean;
    FFailedInitAction: TgxShaderFailedInitAction;
  protected
    (* Invoked once, before the first call to DoApply.
      The call happens with the OpenGL context being active. *)
    procedure DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject); virtual;
    (* Request to apply the shader.
      Always followed by a DoUnApply when the shader is no longer needed. *)
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); virtual; abstract;
    (* Request to un-apply the shader.
      Subclasses can assume the shader has been applied previously.
      Return True to request a multipass. *)
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; virtual; abstract;
    (* Invoked once, before the destruction of context or release of shader.
      The call happens with the OpenGL context being active. *)
    procedure DoFinalize; virtual;
    function GetShaderInitialized: Boolean;
    procedure InitializeShader(var rci: TgxRenderContextInfo; Sender: TObject);
    procedure FinalizeShader;
    procedure OnVirtualHandleAllocate(Sender: TgxVirtualHandle; var handle: Cardinal);
    procedure OnVirtualHandleDestroy(Sender: TgxVirtualHandle; var handle: Cardinal);
    procedure SetEnabled(val: Boolean);
    property ShaderInitialized: Boolean read GetShaderInitialized;
    property ShaderActive: Boolean read FShaderActive;
    procedure RegisterUser(libMat: TgxLibMaterial);
    procedure UnRegisterUser(libMat: TgxLibMaterial);
    // Used by the DoInitialize procedure of descendant classes to raise errors.
    procedure HandleFailedInitialization(const LastErrorMessage: string = ''); virtual;
    // May be this should be a function inside HandleFailedInitialization...
    function GetStardardNotSupportedMessage: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Subclasses should invoke this function when shader properties are altered.
      This procedure can also be used to reset/recompile the shader. *)
    procedure NotifyChange(Sender: TObject); override;
    procedure BeginUpdate;
    procedure EndUpdate;

    // Apply shader to OpenGL state machine.
    procedure Apply(var rci: TgxRenderContextInfo; Sender: TObject);
    (* UnApply shader.
      When returning True, the caller is expected to perform a multipass
      rendering by re-rendering then invoking UnApply again, until a
      "False" is returned. *)
    function UnApply(var rci: TgxRenderContextInfo): Boolean;
    // Shader application style (default is ssLowLevel).
    property ShaderStyle: TgxShaderStyle read FShaderStyle write FShaderStyle default ssLowLevel;
    procedure Assign(Source: TPersistent); override;
    (* Defines if shader is supported by hardware/drivers.
      Default - always supported. Descendants are encouraged to override this function. *)
    function ShaderSupported: Boolean; virtual;
    (* Defines what to do if for some reason shader failed to initialize.
      Note, that in some cases it cannon be determined by just checking the
      required OpenGL extentions. You need to try to compile and link the
      shader - only at that stage you might catch an error *)
    property FailedInitAction: TgxShaderFailedInitAction read FFailedInitAction write FFailedInitAction
      default fiaRaiseStandardException;
  published
    (* Turns on/off shader application.
      Note that this only turns on/off the shader application, if the
      ShaderStyle is ssReplace, the material won't be applied even if
      the shader is disabled. *)
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TgxShaderClass = class of TgxShader;

  TgxShininess = 0 .. 128;

  (* Stores basic face lighting properties.
    The lighting is described with the standard ambient/diffuse/emission/specular
    properties that behave like those of most rendering tools.
    You also have control over shininess (governs specular lighting) and
    polygon mode (lines / fill). *)
  TgxFaceProperties = class(TgxUpdateAbleObject)
  private
    FAmbient, FDiffuse, FSpecular, FEmission: TgxColor;
    FShininess: TgxShininess;
  protected
    procedure SetAmbient(AValue: TgxColor);
    procedure SetDiffuse(AValue: TgxColor);
    procedure SetEmission(AValue: TgxColor);
    procedure SetSpecular(AValue: TgxColor);
    procedure SetShininess(AValue: TgxShininess);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Apply(var rci: TgxRenderContextInfo; AFace: TgxCullFaceMode);
    procedure ApplyNoLighting(var rci: TgxRenderContextInfo; AFace: TgxCullFaceMode);
    procedure Assign(Source: TPersistent); override;
  published
    property Ambient: TgxColor read FAmbient write SetAmbient;
    property Diffuse: TgxColor read FDiffuse write SetDiffuse;
    property Emission: TgxColor read FEmission write SetEmission;
    property Shininess: TgxShininess read FShininess write SetShininess default 0;
    property Specular: TgxColor read FSpecular write SetSpecular;
  end;

  TgxDepthProperties = class(TgxUpdateAbleObject)
  private
    FDepthTest: Boolean;
    FDepthWrite: Boolean;
    FZNear, FZFar: Single;
    FCompareFunc: TgxDepthfunction;
    FDepthClamp: Boolean;
  protected
    procedure SetZNear(Value: Single);
    procedure SetZFar(Value: Single);
    procedure SetCompareFunc(Value: TgxDepthCompareFunc);
    procedure SetDepthTest(Value: Boolean);
    procedure SetDepthWrite(Value: Boolean);
    procedure SetDepthClamp(Value: Boolean);
    function StoreZNear: Boolean;
    function StoreZFar: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Apply(var rci: TgxRenderContextInfo);
    procedure Assign(Source: TPersistent); override;
  published
    (* Specifies the mapping of the near clipping plane to
      window coordinates.  The initial value is 0. *)
    property ZNear: Single read FZNear write SetZNear stored StoreZNear;
    (* Specifies the mapping of the far clipping plane to
      window coordinates.  The initial value is 1. *)
    property ZFar: Single read FZFar write SetZFar stored StoreZFar;
    (* Specifies the function used to compare each
      incoming pixel depth value with the depth value present in
      the depth buffer. *)
    property DepthCompareFunction: TgxDepthfunction read FCompareFunc write SetCompareFunc default cfLequal;
    (* DepthTest enabling.
      When DepthTest is enabled, objects closer to the camera will hide
      farther ones (via use of Z-Buffering).
      When DepthTest is disabled, the latest objects drawn/rendered overlap
      all previous objects, whatever their distance to the camera.
      Even when DepthTest is enabled, objects may chose to ignore depth
      testing through the osIgnoreDepthBuffer of their ObjectStyle property. *)
    property DepthTest: Boolean read FDepthTest write SetDepthTest default True;
    // If True, object will not write to Z-Buffer.
    property DepthWrite: Boolean read FDepthWrite write SetDepthWrite default False;
    // Enable clipping depth to the near and far planes
    property DepthClamp: Boolean read FDepthClamp write SetDepthClamp default False;
  end;

  TgxLibMaterialName = string;

  (* To show up in design-time editor vTGlAlphaFuncValues and
    vTgxBlendFuncFactorValues arrays if you type smth
	like af_GL_NEVER = GL_NEVER in the definition. *)
  TgxAlphaFunc = TgxComparisonFunction;

  TgxBlendingParameters = class(TgxUpdateAbleObject)
  private
    FUseAlphaFunc: Boolean;
    FUseBlendFunc: Boolean;
    FSeparateBlendFunc: Boolean;
    FAlphaFuncType: TgxAlphaFunc;
    FAlphaFuncRef: Single;
    FBlendFuncSFactor: TgxBlendFunction;
    FBlendFuncDFactor: TgxBlendFunction;
    FAlphaBlendFuncSFactor: TgxBlendFunction;
    FAlphaBlendFuncDFactor: TgxBlendFunction;
    procedure SetUseAlphaFunc(const Value: Boolean);
    procedure SetUseBlendFunc(const Value: Boolean);
    procedure SetSeparateBlendFunc(const Value: Boolean);
    procedure SetAlphaFuncRef(const Value: Single);
    procedure SetAlphaFuncType(const Value: TgxAlphaFunc);
    procedure SetBlendFuncDFactor(const Value: TgxBlendFunction);
    procedure SetBlendFuncSFactor(const Value: TgxBlendFunction);
    procedure SetAlphaBlendFuncDFactor(const Value: TgxBlendFunction);
    procedure SetAlphaBlendFuncSFactor(const Value: TgxBlendFunction);
    function StoreAlphaFuncRef: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Apply(var rci: TgxRenderContextInfo);
  published
    property UseAlphaFunc: Boolean read FUseAlphaFunc write SetUseAlphaFunc default False;
    property AlphaFunctType: TgxAlphaFunc read FAlphaFuncType write SetAlphaFuncType default cfGreater;
    property AlphaFuncRef: Single read FAlphaFuncRef write SetAlphaFuncRef stored StoreAlphaFuncRef;

    property UseBlendFunc: Boolean read FUseBlendFunc write SetUseBlendFunc default True;
    property SeparateBlendFunc: Boolean read FSeparateBlendFunc write SetSeparateBlendFunc default False;
    property BlendFuncSFactor: TgxBlendFunction read FBlendFuncSFactor write SetBlendFuncSFactor default bfSrcAlpha;
    property BlendFuncDFactor: TgxBlendFunction read FBlendFuncDFactor write SetBlendFuncDFactor default bfOneMinusSrcAlpha;
    property AlphaBlendFuncSFactor: TgxBlendFunction read FAlphaBlendFuncSFactor write SetAlphaBlendFuncSFactor
      default bfSrcAlpha;
    property AlphaBlendFuncDFactor: TgxBlendFunction read FAlphaBlendFuncDFactor write SetAlphaBlendFuncDFactor
      default bfOneMinusSrcAlpha;
  end;

  (* Simplified blending options.
    bmOpaque : disable blending
    bmTransparency : uses standard alpha blending
    bmAdditive : activates additive blending (with saturation)
    bmAlphaTest50 : uses opaque blending, with alpha-testing at 50% (full
    transparency if alpha is below 0.5, full opacity otherwise)
    bmAlphaTest100 : uses opaque blending, with alpha-testing at 100%
    bmModulate : uses modulation blending
    bmCustom : uses TgxBlendingParameters options *)
  TgxBlendingMode = (bmOpaque, bmTransparency, bmAdditive, bmAlphaTest50, bmAlphaTest100, bmModulate, bmCustom);

  TgxFaceCulling = (fcBufferDefault, fcCull, fcNoCull);

  (* Control special rendering options for a material.
    moIgnoreFog : fog is deactivated when the material is rendered *)
  TgxMaterialOption = (moIgnoreFog, moNoLighting);
  TgxMaterialOptions = set of TgxMaterialOption;

  (* Describes a rendering material.
    A material is basically a set of face properties (front and back) that take
    care of standard material rendering parameters (diffuse, ambient, emission
    and specular) and texture mapping.
    An instance of this class is available for almost all objects in GLScene
    to allow quick definition of material properties. It can link to a
    TgxLibMaterial (taken for a material library).
    The TgxLibMaterial has more advanced properties (like texture transforms)
    and provides a standard way of sharing definitions and texture maps *)
  TgxMaterial = class(TgxUpdateAbleObject, IgxMaterialLibrarySupported, IgxNotifyAble, IgxTextureNotifyAble)
  private
    FFrontProperties, FBackProperties: TgxFaceProperties;
    FDepthProperties: TgxDepthProperties;
    FBlendingMode: TgxBlendingMode;
    FBlendingParams: TgxBlendingParameters;
    FTexture: TgxTexture;
    FTextureEx: TgxTextureEx;
    FMaterialLibrary: TgxAbstractMaterialLibrary;
    FLibMaterialName: TgxLibMaterialName;
    FMaterialOptions: TgxMaterialOptions;
    FFaceCulling: TgxFaceCulling;
    FPolygonMode: TgxPolygonMode;
    currentLibMaterial: TgxAbstractLibMaterial;
    (* Implementing IVXMaterialLibrarySupported. *)
    function GetMaterialLibrary: TgxAbstractMaterialLibrary;
  protected
    function GetBackProperties: TgxFaceProperties;
    procedure SetBackProperties(Values: TgxFaceProperties);
    procedure SetFrontProperties(Values: TgxFaceProperties);
    procedure SetDepthProperties(Values: TgxDepthProperties);
    procedure SetBlendingMode(const val: TgxBlendingMode);
    procedure SetMaterialOptions(const val: TgxMaterialOptions);
    function GetTexture: TgxTexture;
    procedure SetTexture(ATexture: TgxTexture);
    procedure SetMaterialLibrary(const val: TgxAbstractMaterialLibrary);
    procedure SetLibMaterialName(const val: TgxLibMaterialName);
    procedure SetFaceCulling(const val: TgxFaceCulling);
    procedure SetPolygonMode(AValue: TgxPolygonMode);
    function GetTextureEx: TgxTextureEx;
    procedure SetTextureEx(const Value: TgxTextureEx);
    function StoreTextureEx: Boolean;
    procedure SetBlendingParams(const Value: TgxBlendingParameters);
    procedure NotifyLibMaterialDestruction;
    // Back, Front, Texture and blending not stored if linked to a LibMaterial
    function StoreMaterialProps: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure PrepareBuildList;
    procedure Apply(var rci: TgxRenderContextInfo);
    { Restore non-standard material states that were altered;
      A return value of True is a multipass request. }
    function UnApply(var rci: TgxRenderContextInfo): Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure NotifyTexMapChange(Sender: TObject);
    procedure DestroyHandles;
    procedure Loaded;
    (* Returns True if the material is blended.
      Will return the libmaterial's blending if it is linked to a material  library. *)
    function Blended: Boolean;
    // True if the material has a secondary texture
    function HasSecondaryTexture: Boolean;
    // True if the material comes from the library instead of the texture property
    function MaterialIsLinkedToLib: Boolean;
    // Gets the primary texture either from material library or the texture property
    function GetActualPrimaryTexture: TgxTexture;
    // Gets the primary Material either from material library or the texture property
    function GetActualPrimaryMaterial: TgxMaterial;
    // Return the LibMaterial (see LibMaterialName)
    function GetLibMaterial: TgxLibMaterial;
    procedure QuickAssignMaterial(const MaterialLibrary: TgxMaterialLibrary; const Material: TgxLibMaterial);
  published
    property BackProperties: TgxFaceProperties read GetBackProperties write SetBackProperties stored StoreMaterialProps;
    property FrontProperties: TgxFaceProperties read FFrontProperties write SetFrontProperties stored StoreMaterialProps;
    property DepthProperties: TgxDepthProperties read FDepthProperties write SetDepthProperties stored StoreMaterialProps;
    property BlendingMode: TgxBlendingMode read FBlendingMode write SetBlendingMode stored StoreMaterialProps default bmOpaque;
    property BlendingParams: TgxBlendingParameters read FBlendingParams write SetBlendingParams;
    property MaterialOptions: TgxMaterialOptions read FMaterialOptions write SetMaterialOptions default [];
    property Texture: TgxTexture read GetTexture write SetTexture stored StoreMaterialProps;
    property FaceCulling: TgxFaceCulling read FFaceCulling write SetFaceCulling default fcBufferDefault;
    property MaterialLibrary: TgxAbstractMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property LibMaterialName: TgxLibMaterialName read FLibMaterialName write SetLibMaterialName;
    property TextureEx: TgxTextureEx read GetTextureEx write SetTextureEx stored StoreTextureEx;
    property PolygonMode: TgxPolygonMode read FPolygonMode write SetPolygonMode default pmFill;
  end;

  TgxAbstractLibMaterial = class(TCollectionItem, IgxMaterialLibrarySupported, IgxNotifyAble)
  protected
    FUserList: TList;
    FName: TgxLibMaterialName;
    FNameHashKey: Integer;
    FTag: Integer;
    FNotifying: Boolean; // used for recursivity protection
    // implementing IVXMaterialLibrarySupported
    function GetMaterialLibrary: TgxAbstractMaterialLibrary;
    // implementing IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    function GetDisplayName: string; override;
    class function ComputeNameHashKey(const name: string): Integer;
    procedure SetName(const val: TgxLibMaterialName);
    procedure Loaded; virtual; abstract;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Apply(var ARci: TgxRenderContextInfo); virtual; abstract;
    // Restore non-standard material states that were altered
    function UnApply(var ARci: TgxRenderContextInfo): Boolean; virtual; abstract;
    procedure RegisterUser(Obj: TgxUpdateAbleObject); overload;
    procedure UnRegisterUser(Obj: TgxUpdateAbleObject); overload;
    procedure RegisterUser(comp: TgxUpdateAbleComponent); overload;
    procedure UnRegisterUser(comp: TgxUpdateAbleComponent); overload;
    procedure RegisterUser(libMaterial: TgxLibMaterial); overload;
    procedure UnRegisterUser(libMaterial: TgxLibMaterial); overload;
    procedure NotifyUsers;
    function IsUsed: Boolean; // returns true if the texture has registed users
    property NameHashKey: Integer read FNameHashKey;
    procedure NotifyChange(Sender: TObject); virtual;
    function Blended: Boolean; virtual;
    property MaterialLibrary: TgxAbstractMaterialLibrary read GetMaterialLibrary;
  published
    property Name: TgxLibMaterialName read FName write SetName;
    property Tag: Integer read FTag write FTag;
  end;

  (* Material in a material library.
    Introduces Texture transformations (offset and scale). Those transformations
    are available only for lib materials to minimize the memory cost of basic
    materials (which are used in almost all objects). *)
  TgxLibMaterial = class(TgxAbstractLibMaterial, IgxTextureNotifyAble)
  private
    FMaterial: TgxMaterial;
    FTextureOffset, FTextureScale: TgxCoordinates;
    FTextureRotate: Single;
    FTextureMatrixIsIdentity: Boolean;
    FTextureOverride: Boolean;
    FTextureMatrix: TMatrix4f;
    FTexture2Name: TgxLibMaterialName;
    FShader: TgxShader;
    libMatTexture2: TgxLibMaterial; // internal cache
  protected
    procedure Loaded; override;
    procedure SetMaterial(const val: TgxMaterial);
    procedure SetTextureOffset(const val: TgxCoordinates);
    procedure SetTextureScale(const val: TgxCoordinates);
    procedure SetTextureMatrix(const Value: TMatrix4f);
    procedure SetTexture2Name(const val: TgxLibMaterialName);
    procedure SetShader(const val: TgxShader);
    procedure SetTextureRotate(Value: Single);
    function StoreTextureRotate: Boolean;
    procedure CalculateTextureMatrix;
    procedure DestroyHandles;
    procedure DoOnTextureNeeded(Sender: TObject; var textureFileName: string);
    procedure OnNotifyChange(Sender: TObject);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure PrepareBuildList;
    procedure Apply(var ARci: TgxRenderContextInfo); override;
    // Restore non-standard material states that were altered
    function UnApply(var ARci: TgxRenderContextInfo): Boolean; override;
    procedure NotifyUsersOfTexMapChange;
    property TextureMatrix: TMatrix4f read FTextureMatrix write SetTextureMatrix;
    property TextureMatrixIsIdentity: Boolean read FTextureMatrixIsIdentity;
    procedure NotifyTexMapChange(Sender: TObject);
    function Blended: Boolean; override;
  published
    property Material: TgxMaterial read FMaterial write SetMaterial;
    (* Texture offset in texture coordinates.
      The offset is applied after scaling. *)
    property TextureOffset: TgxCoordinates read FTextureOffset write SetTextureOffset;
    (* Texture coordinates scaling.
      Scaling is applied before applying the offset, and is applied
      to the texture coordinates, meaning that a scale factor of (2, 2, 2)
      will make your texture look twice smaller *)
    property TextureScale: TgxCoordinates read FTextureScale write SetTextureScale;
    property TextureRotate: Single read FTextureRotate write SetTextureRotate stored StoreTextureRotate;
    (* Reference to the second texture.
      The referred LibMaterial *must* be in the same material library.
      Second textures are supported only through ARB multitexturing (ignored
      if not supported). *)
    property Texture2Name: TgxLibMaterialName read FTexture2Name write SetTexture2Name;

    // Optionnal shader for the material.
    property Shader: TgxShader read FShader write SetShader;
  end;

  TgxAbstractLibMaterials = class(TOwnedCollection)
  protected
    procedure Loaded;
    function GetMaterial(const AName: TgxLibMaterialName): TgxAbstractLibMaterial; inline;
  public
    function MakeUniqueName(const nameRoot: TgxLibMaterialName): TgxLibMaterialName; virtual;
  end;

  // A collection of materials, mainly used in material libraries.
  TgxLibMaterials = class(TgxAbstractLibMaterials)
  protected
    procedure SetItems(index: Integer; const val: TgxLibMaterial);
    function GetItems(index: Integer): TgxLibMaterial;
    procedure DestroyHandles;
  public
    constructor Create(AOwner: TComponent);
    function Owner: TPersistent;
    function IndexOf(const Item: TgxLibMaterial): Integer;
    function Add: TgxLibMaterial;
    function FindItemID(ID: Integer): TgxLibMaterial;
    property Items[index: Integer]: TgxLibMaterial read GetItems write SetItems; default;
    function GetLibMaterialByName(const AName: TgxLibMaterialName): TgxLibMaterial;
    // Returns index of this Texture if it exists.
    function GetTextureIndex(const Texture: TgxTexture): Integer;
    // Returns index of this Material if it exists.
    function GetMaterialIndex(const Material: TgxMaterial): Integer;
    // Returns name of this Texture if it exists.
    function GetNameOfTexture(const Texture: TgxTexture): TgxLibMaterialName;
    // Returns name of this Material if it exists.
    function GetNameOfLibMaterial(const Material: TgxLibMaterial): TgxLibMaterialName;
    procedure PrepareBuildList;
    (* Deletes all the unused materials in the collection.
      A material is considered unused if no other material or updateable object references it.
      WARNING: For this to work, objects that use the texture, have to REGISTER to the texture. *)
    procedure DeleteUnusedMaterials;
  end;

  TgxAbstractMaterialLibrary = class(TgxCadenceAbleComponent)
  protected
    FMaterials: TgxAbstractLibMaterials;
    FLastAppliedMaterial: TgxAbstractLibMaterial;
    FTexturePaths: string;
    FTexturePathList: TStringList;
    procedure SetTexturePaths(const val: string);
    property TexturePaths: string read FTexturePaths write SetTexturePaths;
    procedure Loaded; override;
  public
    procedure SetNamesToTStrings(AStrings: TStrings);
    (* Applies the material of given name.
      Returns False if the material could not be found. ake sure this
      call is balanced with a corresponding UnApplyMaterial (or an
      assertion will be triggered in the destructor).
      If a material is already applied, and has not yet been unapplied,
      an assertion will be triggered. *)
    function ApplyMaterial(const AName: string; var ARci: TgxRenderContextInfo): Boolean; virtual;
    (* Un-applies the last applied material.
      Use this function in conjunction with ApplyMaterial.
      If no material was applied, an assertion will be triggered. *)
    function UnApplyMaterial(var ARci: TgxRenderContextInfo): Boolean; virtual;
  end;

  (* Stores a set of materials, to be used and shared by scene objects.
    Use a material libraries for storing commonly used materials, it provides
    an efficient way to share texture and material data among many objects,
    thus reducing memory needs and rendering time.
    Materials in a material library also feature advanced control properties
    like texture coordinates transforms. *)
  TgxMaterialLibrary = class(TgxAbstractMaterialLibrary)
  private
    FDoNotClearMaterialsOnLoad: Boolean;
    FOnTextureNeeded: TgxTextureNeededEvent;
  protected
    function GetMaterials: TgxLibMaterials;
    procedure SetMaterials(const val: TgxLibMaterials);
    function StoreMaterials: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyHandles;
    procedure WriteToFiler(writer: TgxVirtualWriter);
    procedure ReadFromFiler(reader: TgxVirtualReader);
    procedure SaveToStream(aStream: TStream); virtual;
    procedure LoadFromStream(aStream: TStream); virtual;
    procedure AddMaterialsFromStream(aStream: TStream);
    (* Save library content to a file.
      Recommended extension : .GLML
      Currently saves only texture, ambient, diffuse, emission
      and specular colors. *)
    procedure SaveToFile(const fileName: string);
    procedure LoadFromFile(const fileName: string);
    procedure AddMaterialsFromFile(const fileName: string);
    (* Add a "standard" texture material.
      "standard" means linear texturing mode with mipmaps and texture
      modulation mode with default-strength color components.
      If persistent is True, the image will be loaded persistently in memory
      (via a TgxPersistentImage), if false, it will be unloaded after upload
      to OpenRX (via TgxPicFileImage). *)
    function AddTextureMaterial(const MaterialName, fileName: string; persistent: Boolean = True): TgxLibMaterial; overload;
    (* Add a "standard" texture material.
      TgxGraphic based variant. *)
    function AddTextureMaterial(const MaterialName: string; Graphic: TBitmap): TgxLibMaterial; overload;
    // Returns libMaterial of given name if any exists.
    function LibMaterialByName(const AName: TgxLibMaterialName): TgxLibMaterial;
    // Returns Texture of given material's name if any exists.
    function TextureByName(const LibMatName: TgxLibMaterialName): TgxTexture;
    // Returns name of texture if any exists.
    function GetNameOfTexture(const Texture: TgxTexture): TgxLibMaterialName;
    // Returns name of Material if any exists.
    function GetNameOfLibMaterial(const libMat: TgxLibMaterial): TgxLibMaterialName;
  published
    // The materials collection.
    property Materials: TgxLibMaterials read GetMaterials write SetMaterials stored StoreMaterials;
    (* This event is fired whenever a texture needs to be loaded from disk.
      The event is triggered before even attempting to load the texture,
      and before TexturePaths is used. *)
    property OnTextureNeeded: TgxTextureNeededEvent read FOnTextureNeeded write FOnTextureNeeded;
    (* Paths to lookup when attempting to load a texture.
      You can specify multiple paths when loading a texture, the separator
      being the semi-colon ';' character. Directories are looked up from
      first to last, the first file name match is used.
      The current directory is always implicit and checked last.
      Note that you can also use the OnTextureNeeded event to provide a
      filename. *)
    property TexturePaths;
  end;

// ------------------------------------------------------------------------------
implementation
// ------------------------------------------------------------------------------

// ------------------
// ------------------ TgxFaceProperties ------------------
// ------------------
constructor TgxFaceProperties.Create(AOwner: TPersistent);
begin
  inherited;
  // default colors
  FAmbient := TgxColor.CreateInitialized(Self, clrGray20);
  FDiffuse := TgxColor.CreateInitialized(Self, clrGray80);
  FEmission := TgxColor.Create(Self);
  FSpecular := TgxColor.Create(Self);
  FShininess := 0;
end;

destructor TgxFaceProperties.Destroy;
begin
  FAmbient.Free;
  FDiffuse.Free;
  FEmission.Free;
  FSpecular.Free;
  inherited Destroy;
end;

procedure TgxFaceProperties.Apply(var rci: TgxRenderContextInfo; aFace: TgxCullFaceMode);
begin
  with rci.gxStates do
  begin
    SetMaterialColors(aFace, Emission.Color, Ambient.Color, Diffuse.Color, Specular.Color, FShininess);
  end;
end;

procedure TgxFaceProperties.ApplyNoLighting(var rci: TgxRenderContextInfo; aFace: TgxCullFaceMode);
begin
  glColor4fv(Diffuse.AsAddress);
end;

procedure TgxFaceProperties.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TgxFaceProperties) then
  begin
    FAmbient.DirectColor := TgxFaceProperties(Source).Ambient.Color;
    FDiffuse.DirectColor := TgxFaceProperties(Source).Diffuse.Color;
    FEmission.DirectColor := TgxFaceProperties(Source).Emission.Color;
    FSpecular.DirectColor := TgxFaceProperties(Source).Specular.Color;
    FShininess := TgxFaceProperties(Source).Shininess;
    NotifyChange(Self);
  end;
end;

procedure TgxFaceProperties.SetAmbient(AValue: TgxColor);
begin
  FAmbient.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

procedure TgxFaceProperties.SetDiffuse(AValue: TgxColor);
begin
  FDiffuse.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

procedure TgxFaceProperties.SetEmission(AValue: TgxColor);
begin
  FEmission.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

procedure TgxFaceProperties.SetSpecular(AValue: TgxColor);
begin
  FSpecular.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

procedure TgxFaceProperties.SetShininess(AValue: TgxShininess);
begin
  if FShininess <> AValue then
  begin
    FShininess := AValue;
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TgxDepthProperties ------------------
// ------------------

constructor TgxDepthProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDepthTest := True;
  FDepthWrite := False;
  FZNear := 0;
  FZFar := 1;
  FCompareFunc := cfLequal;
  FDepthClamp := False;
end;

procedure TgxDepthProperties.Apply(var rci: TgxRenderContextInfo);
begin
  with rci.gxStates do
  begin
    if FDepthTest and rci.bufferDepthTest then
      Enable(stDepthTest)
    else
      Disable(stDepthTest);
    DepthWriteMask := FDepthWrite;
    DepthFunc := FCompareFunc;
    SetDepthRange(FZNear, FZFar);
    if FDepthClamp then
      Enable(stDepthClamp)
    else
      Disable(stDepthClamp);
  end;
end;

procedure TgxDepthProperties.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TgxDepthProperties) then
  begin
    FDepthTest := TgxDepthProperties(Source).FDepthTest;
    FDepthWrite := TgxDepthProperties(Source).FDepthWrite;
    FZNear := TgxDepthProperties(Source).FZNear;
    FZFar := TgxDepthProperties(Source).FZFar;
    FCompareFunc := TgxDepthProperties(Source).FCompareFunc;
    NotifyChange(Self);
  end;
end;

procedure TgxDepthProperties.SetZNear(Value: Single);
begin
  Value := ClampValue(Value, 0, 1);
  if Value <> FZNear then
  begin
    FZNear := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxDepthProperties.SetZFar(Value: Single);
begin
  Value := ClampValue(Value, 0, 1);
  if Value <> FZFar then
  begin
    FZFar := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxDepthProperties.SetCompareFunc(Value: TgxDepthfunction);
begin
  if Value <> FCompareFunc then
  begin
    FCompareFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxDepthProperties.SetDepthTest(Value: Boolean);
begin
  if Value <> FDepthTest then
  begin
    FDepthTest := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxDepthProperties.SetDepthWrite(Value: Boolean);
begin
  if Value <> FDepthWrite then
  begin
    FDepthWrite := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxDepthProperties.SetDepthClamp(Value: Boolean);
begin
  if Value <> FDepthClamp then
  begin
    FDepthClamp := Value;
    NotifyChange(Self);
  end;
end;

function TgxDepthProperties.StoreZNear: Boolean;
begin
  Result := FZNear <> 0.0;
end;

function TgxDepthProperties.StoreZFar: Boolean;
begin
  Result := FZFar <> 1.0;
end;

// ------------------
// ------------------ TgxShader ------------------
// ------------------

constructor TgxShader.Create(AOwner: TComponent);
begin
  FLibMatUsers := TList.Create;
  FVirtualHandle := TgxVirtualHandle.Create;
  FVirtualHandle.OnAllocate := OnVirtualHandleAllocate;
  FVirtualHandle.OnDestroy := OnVirtualHandleDestroy;
  FShaderStyle := ssLowLevel;
  FEnabled := True;
  FFailedInitAction := fiaRaiseStandardException;
  inherited;
end;

destructor TgxShader.Destroy;
var
  i: Integer;
  list: TList;
begin
  FVirtualHandle.DestroyHandle;
  FinalizeShader;
  inherited;
  list := FLibMatUsers;
  FLibMatUsers := nil;
  for i := list.Count - 1 downto 0 do
    TgxLibMaterial(list[i]).Shader := nil;
  list.Free;
  FVirtualHandle.Free;
end;

procedure TgxShader.NotifyChange(Sender: TObject);
var
  i: Integer;
begin
  if FUpdateCount = 0 then
  begin
    for i := FLibMatUsers.Count - 1 downto 0 do
      TgxLibMaterial(FLibMatUsers[i]).NotifyUsers;
    FinalizeShader;
  end;
end;

procedure TgxShader.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TgxShader.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    NotifyChange(Self);
end;

procedure TgxShader.DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject);
begin
  // nothing here
end;

procedure TgxShader.DoFinalize;
begin
  // nothing here
end;

function TgxShader.GetShaderInitialized: Boolean;
begin
  Result := (FVirtualHandle.handle <> 0);
end;

procedure TgxShader.InitializeShader(var rci: TgxRenderContextInfo; Sender: TObject);
begin
  FVirtualHandle.AllocateHandle;
  if FVirtualHandle.IsDataNeedUpdate then
  begin
    DoInitialize(rci, Sender);
    FVirtualHandle.NotifyDataUpdated;
  end;
end;

procedure TgxShader.FinalizeShader;
begin
  FVirtualHandle.NotifyChangesOfData;
  DoFinalize;
end;

procedure TgxShader.Apply(var rci: TgxRenderContextInfo; Sender: TObject);
begin
{$IFNDEF USE_MULTITHREAD}
  Assert(not FShaderActive, 'Unbalanced shader application.');
{$ENDIF}
  // Need to check it twice, because shader may refuse to initialize
  // and choose to disable itself during initialization.
  if FEnabled then
    if FVirtualHandle.IsDataNeedUpdate then
      InitializeShader(rci, Sender);

  if FEnabled then
    DoApply(rci, Sender);

  FShaderActive := True;
end;

function TgxShader.UnApply(var rci: TgxRenderContextInfo): Boolean;
begin
{$IFNDEF USE_MULTITHREAD}
  Assert(FShaderActive, 'Unbalanced shader application.');
{$ENDIF}
  if Enabled then
  begin
    Result := DoUnApply(rci);
    if not Result then
      FShaderActive := False;
  end
  else
  begin
    FShaderActive := False;
    Result := False;
  end;
end;

procedure TgxShader.OnVirtualHandleDestroy(Sender: TgxVirtualHandle; var handle: Cardinal);
begin
  handle := 0;
end;

procedure TgxShader.OnVirtualHandleAllocate(Sender: TgxVirtualHandle; var handle: Cardinal);
begin
  handle := 1;
end;

procedure TgxShader.SetEnabled(val: Boolean);
begin
{$IFNDEF USE_MULTITHREAD}
  Assert(not FShaderActive, 'Shader is active.');
{$ENDIF}
  if val <> FEnabled then
  begin
    FEnabled := val;
    NotifyChange(Self);
  end;
end;

procedure TgxShader.RegisterUser(libMat: TgxLibMaterial);
var
  i: Integer;
begin
  i := FLibMatUsers.IndexOf(libMat);
  if i < 0 then
    FLibMatUsers.Add(libMat);
end;

procedure TgxShader.UnRegisterUser(libMat: TgxLibMaterial);
begin
  if Assigned(FLibMatUsers) then
    FLibMatUsers.Remove(libMat);
end;

procedure TgxShader.Assign(Source: TPersistent);
begin
  if Source is TgxShader then
  begin
    FShaderStyle := TgxShader(Source).FShaderStyle;
    FFailedInitAction := TgxShader(Source).FFailedInitAction;
    Enabled := TgxShader(Source).FEnabled;
  end
  else
    inherited Assign(Source); // to the pit of doom ;)
end;

function TgxShader.ShaderSupported: Boolean;
begin
  Result := True;
end;

procedure TgxShader.HandleFailedInitialization(const LastErrorMessage: string = '');
begin
  case FailedInitAction of
    fiaSilentDisable:
      ; // Do nothing ;)
    fiaRaiseHandledException:
      try
        raise EShaderException.Create(GetStardardNotSupportedMessage);
      except
      end;
    fiaRaiseStandardException:
      raise EShaderException.Create(GetStardardNotSupportedMessage);
    fiaReRaiseException:
      begin
        if LastErrorMessage <> '' then
          raise EShaderException.Create(LastErrorMessage)
        else
          raise EShaderException.Create(GetStardardNotSupportedMessage)
      end;
    // fiaGenerateEvent:; // Do nothing. Event creation is left up to user shaders
    // // which may choose to override this procedure.
  else
    Assert(False, strErrorEx + strUnknownType);
  end;
end;

function TgxShader.GetStardardNotSupportedMessage: string;
begin
  if Name <> '' then
    Result := 'Your hardware/driver doesn''t support shader "' + Name + '"!'
  else
    Result := 'Your hardware/driver doesn''t support shader "' + ClassName + '"!';
end;

// ----------------- TgxMaterial --------------------------------------------------

constructor TgxMaterial.Create(AOwner: TPersistent);
begin
  inherited;
  FFrontProperties := TgxFaceProperties.Create(Self);
  FTexture := nil; // AutoCreate
  FFaceCulling := fcBufferDefault;
  FPolygonMode := pmFill;
  FBlendingParams := TgxBlendingParameters.Create(Self);
  FDepthProperties := TgxDepthProperties.Create(Self)
end;

destructor TgxMaterial.Destroy;
begin
  if Assigned(currentLibMaterial) then
    currentLibMaterial.UnRegisterUser(Self);
  FBackProperties.Free;
  FFrontProperties.Free;
  FDepthProperties.Free;
  FTexture.Free;
  FTextureEx.Free;
  FBlendingParams.Free;
  inherited Destroy;
end;

function TgxMaterial.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TgxMaterial.SetBackProperties(Values: TgxFaceProperties);
begin
  BackProperties.Assign(Values);
  NotifyChange(Self);
end;

function TgxMaterial.GetBackProperties: TgxFaceProperties;
begin
  if not Assigned(FBackProperties) then
    FBackProperties := TgxFaceProperties.Create(Self);
  Result := FBackProperties;
end;

procedure TgxMaterial.SetFrontProperties(Values: TgxFaceProperties);
begin
  FFrontProperties.Assign(Values);
  NotifyChange(Self);
end;

procedure TgxMaterial.SetDepthProperties(Values: TgxDepthProperties);
begin
  FDepthProperties.Assign(Values);
  NotifyChange(Self);
end;

procedure TgxMaterial.SetBlendingMode(const val: TgxBlendingMode);
begin
  if val <> FBlendingMode then
  begin
    FBlendingMode := val;
    NotifyChange(Self);
  end;
end;

procedure TgxMaterial.SetMaterialOptions(const val: TgxMaterialOptions);
begin
  if val <> FMaterialOptions then
  begin
    FMaterialOptions := val;
    NotifyChange(Self);
  end;
end;

function TgxMaterial.GetTexture: TgxTexture;
begin
  if not Assigned(FTexture) then
    FTexture := TgxTexture.Create(Self);
  Result := FTexture;
end;

procedure TgxMaterial.SetTexture(ATexture: TgxTexture);
begin
  if Assigned(ATexture) then
    Texture.Assign(ATexture)
  else
    FreeAndNil(FTexture);
end;

procedure TgxMaterial.SetFaceCulling(const val: TgxFaceCulling);
begin
  if val <> FFaceCulling then
  begin
    FFaceCulling := val;
    NotifyChange(Self);
  end;
end;

procedure TgxMaterial.SetMaterialLibrary(const val: TgxAbstractMaterialLibrary);
begin
  FMaterialLibrary := val;
  SetLibMaterialName(LibMaterialName);
end;

procedure TgxMaterial.SetLibMaterialName(const val: TgxLibMaterialName);
var
  oldLibrary: TgxMaterialLibrary;

  function MaterialLoopFrom(curMat: TgxLibMaterial): Boolean;
  var
    loopCount: Integer;
  begin
    loopCount := 0;
    while Assigned(curMat) and (loopCount < 16) do
    begin
      with curMat.Material do
      begin
        if Assigned(oldLibrary) then
          curMat := oldLibrary.Materials.GetLibMaterialByName(LibMaterialName)
        else
          curMat := nil;
      end;
      Inc(loopCount)
    end;
    Result := (loopCount >= 16);
  end;

var
  newLibMaterial: TgxAbstractLibMaterial;
begin
  // locate new libmaterial
  if Assigned(FMaterialLibrary) then
    newLibMaterial := FMaterialLibrary.FMaterials.GetMaterial(val)
  else
    newLibMaterial := nil;

  // make sure new won't trigger an infinite loop
  if FMaterialLibrary is TgxMaterialLibrary then
  begin
    oldLibrary := TgxMaterialLibrary(FMaterialLibrary);
    if MaterialLoopFrom(TgxLibMaterial(newLibMaterial)) then
    begin
      if IsDesignTime then
        InformationDlg(Format(strCyclicRefMat, [val]))
      else
        ShowMessage(Format(strCyclicRefMat, [val]));
      exit;
    end;
  end;

  FLibMaterialName := val;
  // unregister if required
  if newLibMaterial <> currentLibMaterial then
  begin
    // unregister from old
    if Assigned(currentLibMaterial) then
      currentLibMaterial.UnRegisterUser(Self);
    currentLibMaterial := newLibMaterial;
    // register with new
    if Assigned(currentLibMaterial) then
      currentLibMaterial.RegisterUser(Self);
    NotifyTexMapChange(Self);
  end;
end;

function TgxMaterial.GetTextureEx: TgxTextureEx;
begin
  if not Assigned(FTextureEx) then
    FTextureEx := TgxTextureEx.Create(Self);
  Result := FTextureEx;
end;

procedure TgxMaterial.SetTextureEx(const Value: TgxTextureEx);
begin
  if Assigned(Value) or Assigned(FTextureEx) then
    TextureEx.Assign(Value);
end;

function TgxMaterial.StoreTextureEx: Boolean;
begin
  Result := (Assigned(FTextureEx) and (TextureEx.Count > 0));
end;

procedure TgxMaterial.SetBlendingParams(const Value: TgxBlendingParameters);
begin
  FBlendingParams.Assign(Value);
  NotifyChange(Self);
end;

procedure TgxMaterial.NotifyLibMaterialDestruction;
begin
  FMaterialLibrary := nil;
  FLibMaterialName := '';
  currentLibMaterial := nil;
end;

procedure TgxMaterial.Loaded;
begin
  inherited;
  if Assigned(FTextureEx) then
    TextureEx.Loaded;
end;

function TgxMaterial.StoreMaterialProps: Boolean;
begin
  Result := not Assigned(currentLibMaterial);
end;

procedure TgxMaterial.PrepareBuildList;
begin
  if Assigned(FTexture) and (not FTexture.Disabled) then
    FTexture.PrepareBuildList;
end;

procedure TgxMaterial.Apply(var rci: TgxRenderContextInfo);
begin
  if Assigned(currentLibMaterial) then
    currentLibMaterial.Apply(rci)
  else
    with rci.gxStates do
    begin
      Disable(stColorMaterial);
      PolygonMode := FPolygonMode;
      if FPolygonMode = pmLines then
        Disable(stLineStipple);

      // Lighting switch
      if (moNoLighting in MaterialOptions) or not rci.bufferLighting then
      begin
        Disable(stLighting);
        FFrontProperties.ApplyNoLighting(rci, cmFront);
      end
      else
      begin
        Enable(stLighting);
        FFrontProperties.Apply(rci, cmFront);
      end;

      // Apply FaceCulling and BackProperties (if needs be)
      case FFaceCulling of
        fcBufferDefault:
          begin
            if rci.bufferFaceCull then
              Enable(stCullFace)
            else
              Disable(stCullFace);
            BackProperties.Apply(rci, cmBack);
          end;
        fcCull:
          Enable(stCullFace);
        fcNoCull:
          begin
            Disable(stCullFace);
            BackProperties.Apply(rci, cmBack);
          end;
      end;
      // note: Front + Back with different PolygonMode are no longer supported.
      // Currently state cache just ignores back facing mode changes, changes to
      // front affect both front + back PolygonMode

      // Apply Blending mode
      if not rci.ignoreBlendingRequests then
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
              SetAlphaFunction(cfGreater, 0);
            end;
          bmAdditive:
            begin
              Enable(stBlend);
              Enable(stAlphaTest);
              SetBlendFunc(bfSrcAlpha, bfOne);
              SetAlphaFunction(cfGreater, 0);
            end;
          bmAlphaTest50:
            begin
              Disable(stBlend);
              Enable(stAlphaTest);
              SetAlphaFunction(cfGEqual, 0.5);
            end;
          bmAlphaTest100:
            begin
              Disable(stBlend);
              Enable(stAlphaTest);
              SetAlphaFunction(cfGEqual, 1.0);
            end;
          bmModulate:
            begin
              Enable(stBlend);
              Enable(stAlphaTest);
              SetBlendFunc(bfDstColor, bfZero);
              SetAlphaFunction(cfGreater, 0);
            end;
          bmCustom:
            begin
              FBlendingParams.Apply(rci);
            end;
        end;

      // Fog switch
      if (moIgnoreFog in MaterialOptions) or not rci.bufferFog then
        Disable(stFog)
      else
        Enable(stFog);

      if not Assigned(FTextureEx) then
      begin
        if Assigned(FTexture) then
          FTexture.Apply(rci)
      end
      else
      begin
        if Assigned(FTexture) and not FTextureEx.IsTextureEnabled(0) then
          FTexture.Apply(rci)
        else if FTextureEx.Count > 0 then
          FTextureEx.Apply(rci);
      end;

      // Apply depth properties
      if not rci.ignoreDepthRequests then
        FDepthProperties.Apply(rci);
    end;
end;

function TgxMaterial.UnApply(var rci: TgxRenderContextInfo): Boolean;
begin
  if Assigned(currentLibMaterial) then
    Result := currentLibMaterial.UnApply(rci)
  else
  begin
    if Assigned(FTexture) and (not FTexture.Disabled) and (not FTextureEx.IsTextureEnabled(0)) then
      FTexture.UnApply(rci)
    else if Assigned(FTextureEx) then
      FTextureEx.UnApply(rci);
    Result := False;
  end;
end;

procedure TgxMaterial.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TgxMaterial) then
  begin
    if Assigned(TgxMaterial(Source).FBackProperties) then
      BackProperties.Assign(TgxMaterial(Source).BackProperties)
    else
      FreeAndNil(FBackProperties);
    FFrontProperties.Assign(TgxMaterial(Source).FFrontProperties);
    FPolygonMode := TgxMaterial(Source).FPolygonMode;
    FBlendingMode := TgxMaterial(Source).FBlendingMode;
    FMaterialOptions := TgxMaterial(Source).FMaterialOptions;
    if Assigned(TgxMaterial(Source).FTexture) then
      Texture.Assign(TgxMaterial(Source).FTexture)
    else
      FreeAndNil(FTexture);
    FFaceCulling := TgxMaterial(Source).FFaceCulling;
    FMaterialLibrary := TgxMaterial(Source).MaterialLibrary;
    SetLibMaterialName(TgxMaterial(Source).LibMaterialName);
    TextureEx.Assign(TgxMaterial(Source).TextureEx);
    FDepthProperties.Assign(TgxMaterial(Source).DepthProperties);
    NotifyChange(Self);
  end
  else
    inherited;
end;

procedure TgxMaterial.NotifyChange(Sender: TObject);
var
  intf: IgxNotifyAble;
begin
  if Supports(Owner, IgxNotifyAble, intf) then
    intf.NotifyChange(Self);
end;

procedure TgxMaterial.NotifyTexMapChange(Sender: TObject);
var
  intf: IgxTextureNotifyAble;
begin
  if Supports(Owner, IgxTextureNotifyAble, intf) then
    intf.NotifyTexMapChange(Self)
  else
    NotifyChange(Self);
end;

procedure TgxMaterial.DestroyHandles;
begin
  if Assigned(FTexture) then
    FTexture.DestroyHandles;
end;

function TgxMaterial.Blended: Boolean;
begin
  if Assigned(currentLibMaterial) then
  begin

    Result := currentLibMaterial.Blended
  end
  else
    Result := not(BlendingMode in [bmOpaque, bmAlphaTest50, bmAlphaTest100, bmCustom]);
end;

function TgxMaterial.HasSecondaryTexture: Boolean;
begin
  Result := Assigned(currentLibMaterial) and (currentLibMaterial is TgxLibMaterial) and
    Assigned(TgxLibMaterial(currentLibMaterial).libMatTexture2);
end;

function TgxMaterial.MaterialIsLinkedToLib: Boolean;
begin
  Result := Assigned(currentLibMaterial);
end;

function TgxMaterial.GetActualPrimaryTexture: TgxTexture;
begin
  if Assigned(currentLibMaterial) and (currentLibMaterial is TgxLibMaterial) then
    Result := TgxLibMaterial(currentLibMaterial).Material.Texture
  else
    Result := Texture;
end;

function TgxMaterial.GetActualPrimaryMaterial: TgxMaterial;
begin
  if Assigned(currentLibMaterial) and (currentLibMaterial is TgxLibMaterial) then
    Result := TgxLibMaterial(currentLibMaterial).Material
  else
    Result := Self;
end;

function TgxMaterial.GetLibMaterial: TgxLibMaterial;
begin
  if Assigned(currentLibMaterial) and (currentLibMaterial is TgxLibMaterial) then
    Result := TgxLibMaterial(currentLibMaterial)
  else
    Result := nil;
end;

procedure TgxMaterial.QuickAssignMaterial(const MaterialLibrary: TgxMaterialLibrary; const Material: TgxLibMaterial);
begin
  FMaterialLibrary := MaterialLibrary;
  FLibMaterialName := Material.FName;

  if Material <> currentLibMaterial then
  begin
    // unregister from old
    if Assigned(currentLibMaterial) then
      currentLibMaterial.UnRegisterUser(Self);
    currentLibMaterial := Material;
    // register with new
    if Assigned(currentLibMaterial) then
      currentLibMaterial.RegisterUser(Self);

    NotifyTexMapChange(Self);
  end;
end;

procedure TgxMaterial.SetPolygonMode(AValue: TgxPolygonMode);
begin
  if AValue <> FPolygonMode then
  begin
    FPolygonMode := AValue;
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TgxAbstractLibMaterial ------------------
// ------------------

constructor TgxAbstractLibMaterial.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FUserList := TList.Create;
  if Assigned(ACollection) then
  begin
    FName := TgxAbstractLibMaterials(ACollection).MakeUniqueName('LibMaterial');
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;

destructor TgxAbstractLibMaterial.Destroy;
begin
  FUserList.Free;
  inherited Destroy;
end;

procedure TgxAbstractLibMaterial.Assign(Source: TPersistent);
begin
  if Source is TgxAbstractLibMaterial then
  begin
    FName := TgxLibMaterials(Collection).MakeUniqueName(TgxLibMaterial(Source).name);
    FNameHashKey := ComputeNameHashKey(FName);
  end
  else
    inherited; // Raise AssignError
end;

function TgxAbstractLibMaterial.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TgxAbstractLibMaterial._AddRef: Integer; stdcall;
begin
  Result := -1; // ignore
end;

function TgxAbstractLibMaterial._Release: Integer; stdcall;
begin
  Result := -1; // ignore
end;

procedure TgxAbstractLibMaterial.RegisterUser(Obj: TgxUpdateAbleObject);
begin
  Assert(FUserList.IndexOf(Obj) < 0);
  FUserList.Add(Obj);
end;

procedure TgxAbstractLibMaterial.UnRegisterUser(Obj: TgxUpdateAbleObject);
begin
  FUserList.Remove(Obj);
end;

procedure TgxAbstractLibMaterial.RegisterUser(comp: TgxUpdateAbleComponent);
begin
  Assert(FUserList.IndexOf(comp) < 0);
  FUserList.Add(comp);
end;

procedure TgxAbstractLibMaterial.UnRegisterUser(comp: TgxUpdateAbleComponent);
begin
  FUserList.Remove(comp);
end;

procedure TgxAbstractLibMaterial.RegisterUser(libMaterial: TgxLibMaterial);
begin
  Assert(FUserList.IndexOf(libMaterial) < 0);
  FUserList.Add(libMaterial);
end;

procedure TgxAbstractLibMaterial.UnRegisterUser(libMaterial: TgxLibMaterial);
begin
  FUserList.Remove(libMaterial);
end;

procedure TgxAbstractLibMaterial.NotifyChange(Sender: TObject);
begin
  NotifyUsers();
end;

procedure TgxAbstractLibMaterial.NotifyUsers;
var
  i: Integer;
  Obj: TObject;
begin
  if FNotifying then
    exit;
  FNotifying := True;
  try
    for i := 0 to FUserList.Count - 1 do
    begin
      Obj := TObject(FUserList[i]);
      if Obj is TgxUpdateAbleObject then
        TgxUpdateAbleObject(FUserList[i]).NotifyChange(Self)
      else if Obj is TgxUpdateAbleComponent then
        TgxUpdateAbleComponent(FUserList[i]).NotifyChange(Self)
      else
      begin
        Assert(Obj is TgxAbstractLibMaterial);
        TgxAbstractLibMaterial(FUserList[i]).NotifyUsers;
      end;
    end;
  finally
    FNotifying := False;
  end;
end;

function TgxAbstractLibMaterial.IsUsed: Boolean;
begin
  Result := Assigned(Self) and (FUserList.Count > 0);
end;

function TgxAbstractLibMaterial.GetDisplayName: string;
begin
  Result := Name;
end;

function TgxAbstractLibMaterial.GetMaterialLibrary: TgxAbstractMaterialLibrary;
var
  LOwner: TPersistent;
begin
  Result := nil;
  if Assigned(Collection) then
  begin
    LOwner := TgxAbstractLibMaterials(Collection).Owner;
    if LOwner is TgxAbstractMaterialLibrary then
      Result := TgxAbstractMaterialLibrary(LOwner);
  end;
end;

function TgxAbstractLibMaterial.Blended: Boolean;
begin
  Result := False;
end;

class function TgxAbstractLibMaterial.ComputeNameHashKey(const name: string): Integer;
var
  i, n: Integer;
begin
  n := Length(name);
  Result := n;
  for i := 1 to n do
    Result := (Result shl 1) + Byte(name[i]);
end;

procedure TgxAbstractLibMaterial.SetName(const val: TgxLibMaterialName);
begin
  if val <> FName then
  begin
    if not(csLoading in TComponent(Collection.Owner).ComponentState) then
    begin
      if TgxLibMaterials(Collection).GetLibMaterialByName(val) <> Self then
        FName := TgxLibMaterials(Collection).MakeUniqueName(val)
      else
        FName := val;
    end
    else
      FName := val;
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;

// ------------------
// ------------------ TgxLibMaterial ------------------
// ------------------

constructor TgxLibMaterial.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMaterial := TgxMaterial.Create(Self);
  FMaterial.Texture.OnTextureNeeded := DoOnTextureNeeded;
  FTextureOffset := TgxCoordinates.CreateInitialized(Self, NullHmgVector, csPoint);
  FTextureOffset.OnNotifyChange := OnNotifyChange;
  FTextureScale := TgxCoordinates.CreateInitialized(Self, XYZHmgVector, csPoint);
  FTextureScale.OnNotifyChange := OnNotifyChange;
  FTextureRotate := 0;
  FTextureOverride := False;
  FTextureMatrixIsIdentity := True;
end;

destructor TgxLibMaterial.Destroy;
var
  i: Integer;
  matObj: TObject;
begin
  Shader := nil; // drop dependency
  Texture2Name := ''; // drop dependency
  for i := 0 to FUserList.Count - 1 do
  begin
    matObj := TObject(FUserList[i]);
    if matObj is TgxMaterial then
      TgxMaterial(matObj).NotifyLibMaterialDestruction
    else if matObj is TgxLibMaterial then
    begin
      TgxLibMaterial(matObj).libMatTexture2 := nil;
      TgxLibMaterial(matObj).FTexture2Name := '';
    end;
  end;
  FMaterial.Free;
  FTextureOffset.Free;
  FTextureScale.Free;
  inherited;
end;

procedure TgxLibMaterial.Assign(Source: TPersistent);
begin
  if Source is TgxLibMaterial then
  begin
    FMaterial.Assign(TgxLibMaterial(Source).Material);
    FTextureOffset.Assign(TgxLibMaterial(Source).TextureOffset);
    FTextureScale.Assign(TgxLibMaterial(Source).TextureScale);
    FTextureRotate := TgxLibMaterial(Source).TextureRotate;
    TextureMatrix := TgxLibMaterial(Source).TextureMatrix;
    FTextureOverride := TgxLibMaterial(Source).FTextureOverride;
    FTexture2Name := TgxLibMaterial(Source).Texture2Name;
    FShader := TgxLibMaterial(Source).Shader;
  end;
  inherited;
end;

function TgxLibMaterial.Blended: Boolean;
begin
  Result := Material.Blended;
end;

procedure TgxLibMaterial.PrepareBuildList;
begin
  if Assigned(Self) then
    Material.PrepareBuildList;
end;

procedure TgxLibMaterial.Apply(var ARci: TgxRenderContextInfo);
var
  multitextured: Boolean;
begin
  xglBeginUpdate;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssHighLevel:
        Shader.Apply(ARci, Self);
      ssReplace:
        begin
          Shader.Apply(ARci, Self);
          exit;
        end;
    end;
  end
  else
    ARci.gxStates.CurrentProgram := 0;
  if (Texture2Name <> '') and (not vSecondTextureUnitForbidden) then
  begin
    if not Assigned(libMatTexture2) then
    begin
      libMatTexture2 := TgxLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
      if Assigned(libMatTexture2) then
        libMatTexture2.RegisterUser(Self)
      else
        FTexture2Name := '';
    end;
    multitextured := Assigned(libMatTexture2) and (not libMatTexture2.Material.Texture.Disabled);
  end
  else
    multitextured := False;
  if not multitextured then
  begin
    // no multitexturing ("standard" mode)
    if not FTextureMatrixIsIdentity then
      ARci.gxStates.SetTextureMatrix(FTextureMatrix);
    Material.Apply(ARci);
  end
  else
  begin
    // multitexturing is ON
    if not FTextureMatrixIsIdentity then
      ARci.gxStates.SetTextureMatrix(FTextureMatrix);
    Material.Apply(ARci);

    if not libMatTexture2.FTextureMatrixIsIdentity then
      libMatTexture2.Material.Texture.ApplyAsTexture2(ARci, @libMatTexture2.FTextureMatrix.X.X)
    else
      libMatTexture2.Material.Texture.ApplyAsTexture2(ARci);

    if (not Material.Texture.Disabled) and (Material.Texture.MappingMode = tmmUser) then
      if libMatTexture2.Material.Texture.MappingMode = tmmUser then
        xglMapTexCoordToDual
      else
        xglMapTexCoordToMain
    else if libMatTexture2.Material.Texture.MappingMode = tmmUser then
      xglMapTexCoordToSecond
    else
      xglMapTexCoordToMain;

  end;

  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssLowLevel:
        Shader.Apply(ARci, Self);
    end;
  end;
  xglEndUpdate;
end;

function TgxLibMaterial.UnApply(var ARci: TgxRenderContextInfo): Boolean;
begin
  Result := False;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssLowLevel:
        Result := Shader.UnApply(ARci);
      ssReplace:
        begin
          Result := Shader.UnApply(ARci);
          exit;
        end;
    end;
  end;

  if not Result then
  begin
    if Assigned(libMatTexture2) and (not vSecondTextureUnitForbidden) then
    begin
      libMatTexture2.Material.Texture.UnApplyAsTexture2(ARci, (not libMatTexture2.TextureMatrixIsIdentity));
      xglMapTexCoordToMain;
    end;
    Material.UnApply(ARci);
    if not Material.Texture.Disabled then
      if not FTextureMatrixIsIdentity then
        ARci.gxStates.ResetTextureMatrix;
    if Assigned(FShader) then
    begin
      case Shader.ShaderStyle of
        ssHighLevel:
          Result := Shader.UnApply(ARci);
      end;
    end;
  end;
end;

procedure TgxLibMaterial.NotifyTexMapChange(Sender: TObject);
begin
  NotifyUsersOfTexMapChange();
end;

procedure TgxLibMaterial.NotifyUsersOfTexMapChange;
var
  i: Integer;
  Obj: TObject;
begin
  if FNotifying then
    exit;
  FNotifying := True;
  try
    for i := 0 to FUserList.Count - 1 do
    begin
      Obj := TObject(FUserList[i]);
      if Obj is TgxMaterial then
        TgxMaterial(FUserList[i]).NotifyTexMapChange(Self)
      else if Obj is TgxLibMaterial then
        TgxLibMaterial(FUserList[i]).NotifyUsersOfTexMapChange
      else if Obj is TgxUpdateAbleObject then
        TgxUpdateAbleObject(FUserList[i]).NotifyChange(Self)
      else if Obj is TgxUpdateAbleComponent then
        TgxUpdateAbleComponent(FUserList[i]).NotifyChange(Self);
    end;
  finally
    FNotifying := False;
  end;
end;

procedure TgxLibMaterial.Loaded;
begin
  CalculateTextureMatrix;
  Material.Loaded;
end;

procedure TgxLibMaterial.SetMaterial(const val: TgxMaterial);
begin
  FMaterial.Assign(val);
end;

procedure TgxLibMaterial.SetTextureOffset(const val: TgxCoordinates);
begin
  FTextureOffset.AsVector := val.AsVector;
  CalculateTextureMatrix;
end;

procedure TgxLibMaterial.SetTextureScale(const val: TgxCoordinates);
begin
  FTextureScale.AsVector := val.AsVector;
  CalculateTextureMatrix;
end;

procedure TgxLibMaterial.SetTextureMatrix(const Value: TMatrix4f);
begin
  FTextureMatrixIsIdentity := CompareMem(@Value.X, @IdentityHmgMatrix.X, SizeOf(TMatrix4f));
  FTextureMatrix := Value;
  FTextureOverride := True;
  NotifyUsers;
end;

procedure TgxLibMaterial.SetTextureRotate(Value: Single);
begin
  if Value <> FTextureRotate then
  begin
    FTextureRotate := Value;
    CalculateTextureMatrix;
  end;
end;

function TgxLibMaterial.StoreTextureRotate: Boolean;
begin
  Result := Abs(FTextureRotate) > EPSILON;
end;

procedure TgxLibMaterial.SetTexture2Name(const val: TgxLibMaterialName);
begin
  if val <> Texture2Name then
  begin
    if Assigned(libMatTexture2) then
    begin
      libMatTexture2.UnRegisterUser(Self);
      libMatTexture2 := nil;
    end;
    FTexture2Name := val;
    NotifyUsers;
  end;
end;

procedure TgxLibMaterial.SetShader(const val: TgxShader);
begin
  if val <> FShader then
  begin
    if Assigned(FShader) then
      FShader.UnRegisterUser(Self);
    FShader := val;
    if Assigned(FShader) then
      FShader.RegisterUser(Self);
    NotifyUsers;
  end;
end;

procedure TgxLibMaterial.CalculateTextureMatrix;
begin
  if TextureOffset.Equals(NullHmgVector) and
     TextureScale.Equals(XYZHmgVector) and
     not StoreTextureRotate then
    FTextureMatrixIsIdentity := True
  else
  begin
    FTextureMatrixIsIdentity := False;
    FTextureMatrix := CreateScaleAndTranslationMatrix(TextureScale.AsVector, TextureOffset.AsVector);
    if StoreTextureRotate then
      FTextureMatrix := MatrixMultiply(FTextureMatrix, CreateRotationMatrixZ(DegToRadian(FTextureRotate)));
  end;
  FTextureOverride := False;
  NotifyUsers;
end;

procedure TgxLibMaterial.DestroyHandles;
var
  libMat: TgxLibMaterial;
begin
  FMaterial.DestroyHandles;
  if FTexture2Name <> '' then
  begin
    libMat := TgxLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
    if Assigned(libMat) then
      libMat.DestroyHandles;
  end;
end;

procedure TgxLibMaterial.OnNotifyChange(Sender: TObject);
begin
  CalculateTextureMatrix;
end;

procedure TgxLibMaterial.DoOnTextureNeeded(Sender: TObject; var textureFileName: string);
var
  mLib: TgxMaterialLibrary;
  i: Integer;
  tryName: string;
begin
  if not Assigned(Collection) then
    exit;
  mLib := TgxMaterialLibrary((Collection as TgxLibMaterials).GetOwner);
  with mLib do
    if Assigned(FOnTextureNeeded) then
      FOnTextureNeeded(mLib, textureFileName);
  // if a ':' is present, or if it starts with a '\', consider it as an absolute path
  if (Pos(':', textureFileName) > 0) or (Copy(textureFileName, 1, 1) = PathDelim) then
    exit;
  // ok, not an absolute path, try given paths
  with mLib do
  begin
    if FTexturePathList <> nil then
      for i := 0 to FTexturePathList.Count - 1 do
      begin
        tryName := IncludeTrailingPathDelimiter(FTexturePathList[i]) + textureFileName;
        if (Assigned(vGXAFIOCreateFileStream) and FileStreamExists(tryName)) or FileExists(tryName) then
        begin
          textureFileName := tryName;
          Break;
        end;
      end;
  end;
end;

// ------------------
// ------------------ TgxLibMaterials ------------------
// ------------------

function TgxAbstractLibMaterials.GetMaterial(const AName: TgxLibMaterialName): TgxAbstractLibMaterial;
var
  i, hk: Integer;
  lm: TgxAbstractLibMaterial;
begin
  hk := TgxAbstractLibMaterial.ComputeNameHashKey(AName);
  for i := 0 to Count - 1 do
  begin
    lm := TgxAbstractLibMaterial(inherited Items[i]);
    if (lm.NameHashKey = hk) and (lm.name = AName) then
    begin
      Result := lm;
      exit;
    end;
  end;
  Result := nil;
end;


procedure TgxAbstractLibMaterials.Loaded;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    TgxAbstractLibMaterial(Items[i]).Loaded;
end;

function TgxAbstractLibMaterials.MakeUniqueName(const nameRoot: TgxLibMaterialName): TgxLibMaterialName;
var
  i: Integer;
begin
  Result := nameRoot;
  i := 1;
  while GetMaterial(Result) <> nil do
  begin
    Result := nameRoot + IntToStr(i);
    Inc(i);
  end;
end;


constructor TgxLibMaterials.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TgxLibMaterial);
end;

procedure TgxLibMaterials.SetItems(index: Integer; const val: TgxLibMaterial);
begin
  inherited Items[index] := val;
end;

function TgxLibMaterials.GetItems(index: Integer): TgxLibMaterial;
begin
  Result := TgxLibMaterial(inherited Items[index]);
end;

procedure TgxLibMaterials.DestroyHandles;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].DestroyHandles;
end;

function TgxLibMaterials.Owner: TPersistent;
begin
  Result := GetOwner;
end;

function TgxLibMaterials.Add: TgxLibMaterial;
begin
  Result := (inherited Add) as TgxLibMaterial;
end;

function TgxLibMaterials.FindItemID(ID: Integer): TgxLibMaterial;
begin
  Result := (inherited FindItemID(ID)) as TgxLibMaterial;
end;

function TgxLibMaterials.GetLibMaterialByName(const AName: TgxLibMaterialName): TgxLibMaterial;
var
  LMaterial: TgxAbstractLibMaterial;
begin
  LMaterial := GetMaterial(AName);
  if Assigned(LMaterial) and (LMaterial is TgxLibMaterial) then
    Result := TgxLibMaterial(LMaterial)
  else
    Result := nil;
end;

function TgxLibMaterials.GetTextureIndex(const Texture: TgxTexture): Integer;
var
  i: Integer;
begin
  if Count <> 0 then
    for i := 0 to Count - 1 do
      if GetItems(i).Material.Texture = Texture then
      begin
        Result := i;
        exit;
      end;
  Result := -1;
end;

function TgxLibMaterials.GetMaterialIndex(const Material: TgxMaterial): Integer;
var
  i: Integer;
begin
  if Count <> 0 then
    for i := 0 to Count - 1 do
      if GetItems(i).Material = Material then
      begin
        Result := i;
        exit;
      end;
  Result := -1;
end;

function TgxLibMaterials.GetNameOfTexture(const Texture: TgxTexture): TgxLibMaterialName;
var
  MatIndex: Integer;
begin
  MatIndex := GetTextureIndex(Texture);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).name
  else
    Result := '';
end;

function TgxLibMaterials.GetNameOfLibMaterial(const Material: TgxLibMaterial): TgxLibMaterialName;
var
  MatIndex: Integer;
begin
  MatIndex := IndexOf(Material);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).name
  else
    Result := '';
end;

function TgxLibMaterials.IndexOf(const Item: TgxLibMaterial): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Count <> 0 then
    for i := 0 to Count - 1 do
      if GetItems(i) = Item then
      begin
        Result := i;
        exit;
      end;
end;

procedure TgxLibMaterials.PrepareBuildList;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TgxLibMaterial(inherited Items[i]).PrepareBuildList;
end;

procedure TgxLibMaterials.DeleteUnusedMaterials;
var
  i: Integer;
  gotNone: Boolean;
begin
  BeginUpdate;
  repeat
    gotNone := True;
    for i := Count - 1 downto 0 do
    begin
      if TgxLibMaterial(inherited Items[i]).FUserList.Count = 0 then
      begin
        TgxLibMaterial(inherited Items[i]).Free;
        gotNone := False;
      end;
    end;
  until gotNone;
  EndUpdate;
end;

procedure TgxAbstractMaterialLibrary.SetTexturePaths(const val: string);
var
  i, lp: Integer;

  procedure AddCurrent;
  var
    buf: string;
  begin
    buf := Trim(Copy(val, lp + 1, i - lp - 1));
    if Length(buf) > 0 then
    begin
      // make sure '\' is the terminator
      buf := IncludeTrailingPathDelimiter(buf);
      FTexturePathList.Add(buf);
    end;
  end;

begin
  FTexturePathList.Free;
  FTexturePathList := nil;
  FTexturePaths := val;
  if val <> '' then
  begin
    FTexturePathList := TStringList.Create;
    lp := 0;
    for i := 1 to Length(val) do
    begin
      if val[i] = ';' then
      begin
        AddCurrent;
        lp := i;
      end;
    end;
    i := Length(val) + 1;
    AddCurrent;
  end;
end;

function TgxAbstractMaterialLibrary.ApplyMaterial(const AName: string; var ARci: TgxRenderContextInfo): Boolean;
begin
  FLastAppliedMaterial := FMaterials.GetMaterial(AName);
  Result := Assigned(FLastAppliedMaterial);
  if Result then
    FLastAppliedMaterial.Apply(ARci);
end;

function TgxAbstractMaterialLibrary.UnApplyMaterial(var ARci: TgxRenderContextInfo): Boolean;
begin
  if Assigned(FLastAppliedMaterial) then
  begin
    Result := FLastAppliedMaterial.UnApply(ARci);
    if not Result then
      FLastAppliedMaterial := nil;
  end
  else
    Result := False;
end;

procedure TgxAbstractMaterialLibrary.SetNamesToTStrings(AStrings: TStrings);
var
  i: Integer;
  lm: TgxAbstractLibMaterial;
begin
  with AStrings do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to FMaterials.Count - 1 do
    begin
      lm := TgxAbstractLibMaterial(FMaterials.Items[i]);
      AddObject(lm.name, lm);
    end;
    EndUpdate;
  end;
end;

procedure TgxAbstractMaterialLibrary.Loaded;
begin
  inherited;
  FMaterials.Loaded;
end;

// ------------------
// ------------------ TgxMaterialLibrary ------------------
// ------------------

constructor TgxMaterialLibrary.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TgxLibMaterials.Create(Self);
end;

destructor TgxMaterialLibrary.Destroy;
begin
  Assert(FLastAppliedMaterial = nil, 'Unbalanced material application');
  FTexturePathList.Free;
  FMaterials.Free;
  FMaterials := nil;
  inherited;
end;

procedure TgxMaterialLibrary.DestroyHandles;
begin
  if Assigned(FMaterials) then
    Materials.DestroyHandles;
end;

procedure TgxMaterialLibrary.SetMaterials(const val: TgxLibMaterials);
begin
  FMaterials.Assign(val);
end;

function TgxMaterialLibrary.StoreMaterials: Boolean;
begin
  Result := (FMaterials.Count > 0);
end;

procedure TgxMaterialLibrary.WriteToFiler(writer: TgxVirtualWriter);
var
  i, j: Integer;
  libMat: TgxLibMaterial;
  tex: TgxTexture;
  img: TgxTextureImage;
  pim: TgxPersistentImage;
  ss: TStringStream;
  bmp: TBitmap;
  texExItem: TgxTextureExItem;
begin
  with writer do
  begin
    WriteInteger(4); // archive version 0, texture persistence only
    // archive version 1, libmat properties
    // archive version 2, Material.TextureEx properties
    // archive version 3, Material.Texture properties
    // archive version 4, Material.TextureRotate
    WriteInteger(Materials.Count);
    for i := 0 to Materials.Count - 1 do
    begin
      // version 0
      libMat := Materials[i];
      WriteString(libMat.name);
      tex := libMat.Material.Texture;
      img := tex.Image;
      pim := TgxPersistentImage(img);
      if tex.Enabled and (img is TgxPersistentImage) and (pim.Picture.Bitmap <> nil) then
      begin
        WriteBoolean(True);
        ss := TStringStream.Create('');
        try
          bmp := TBitmap.Create;
          try
            bmp.Assign(pim.Picture.Bitmap);
            bmp.SaveToStream(ss);
          finally
            bmp.Free;
          end;
          WriteString(ss.DataString);
        finally
          ss.Free;
        end;

        // version 3
        with libMat.Material.Texture do
        begin
          Write(BorderColor.AsAddress^, SizeOf(Single) * 4);
          WriteInteger(Integer(Compression));
          WriteInteger(Integer(DepthTextureMode));
          Write(EnvColor.AsAddress^, SizeOf(Single) * 4);
          WriteInteger(Integer(FilteringQuality));
          WriteInteger(Integer(ImageAlpha));
          WriteFloat(ImageBrightness);
          WriteFloat(ImageGamma);
          WriteInteger(Integer(MagFilter));
          WriteInteger(Integer(MappingMode));
          Write(MappingSCoordinates.AsAddress^, SizeOf(Single) * 4);
          Write(MappingTCoordinates.AsAddress^, SizeOf(Single) * 4);
          Write(MappingRCoordinates.AsAddress^, SizeOf(Single) * 4);
          Write(MappingQCoordinates.AsAddress^, SizeOf(Single) * 4);
          WriteInteger(Integer(MinFilter));
          WriteFloat(NormalMapScale);
          WriteInteger(Integer(TextureCompareFunc));
          WriteInteger(Integer(TextureCompareMode));
          WriteInteger(Integer(TextureFormat));
          WriteInteger(Integer(TextureMode));
          WriteInteger(Integer(TextureWrap));
          WriteInteger(Integer(TextureWrapR));
          WriteInteger(Integer(TextureWrapS));
          WriteInteger(Integer(TextureWrapT));
        end;
        // version 3 end

      end
      else
        WriteBoolean(False);
      with libMat.Material.FrontProperties do
      begin
        Write(Ambient.AsAddress^, SizeOf(Single) * 3);
        Write(Diffuse.AsAddress^, SizeOf(Single) * 4);
        Write(Emission.AsAddress^, SizeOf(Single) * 3);
        Write(Specular.AsAddress^, SizeOf(Single) * 3);
      end;

      // version 1
      with libMat.Material.FrontProperties do
      begin
        Write(FShininess, 1);
        WriteInteger(Integer(libMat.Material.PolygonMode));
      end;
      with libMat.Material.BackProperties do
      begin
        Write(Ambient.AsAddress^, SizeOf(Single) * 3);
        Write(Diffuse.AsAddress^, SizeOf(Single) * 4);
        Write(Emission.AsAddress^, SizeOf(Single) * 3);
        Write(Specular.AsAddress^, SizeOf(Single) * 3);
        Write(Byte(FShininess), 1);
        WriteInteger(Integer(libMat.Material.PolygonMode));
      end;
      WriteInteger(Integer(libMat.Material.BlendingMode));

      // version 3
      with libMat.Material do
      begin
        if BlendingMode = bmCustom then
        begin
          WriteBoolean(True);
          with BlendingParams do
          begin
            WriteFloat(AlphaFuncRef);
            WriteInteger(Integer(AlphaFunctType));
            WriteInteger(Integer(BlendFuncDFactor));
            WriteInteger(Integer(BlendFuncSFactor));
            WriteBoolean(UseAlphaFunc);
            WriteBoolean(UseBlendFunc);
          end;
        end
        else
          WriteBoolean(False);

        WriteInteger(Integer(FaceCulling));
      end;
      // version 3 end

      WriteInteger(SizeOf(TgxMaterialOptions));
      Write(libMat.Material.MaterialOptions, SizeOf(TgxMaterialOptions));
      Write(libMat.TextureOffset.AsAddress^, SizeOf(Single) * 3);
      Write(libMat.TextureScale.AsAddress^, SizeOf(Single) * 3);
      WriteString(libMat.Texture2Name);

      // version 4
      WriteFloat(libMat.TextureRotate);

      // version 2
      WriteInteger(libMat.Material.TextureEx.Count);
      for j := 0 to libMat.Material.TextureEx.Count - 1 do
      begin
        texExItem := libMat.Material.TextureEx[j];
        img := texExItem.Texture.Image;
        pim := TgxPersistentImage(img);
        if texExItem.Texture.Enabled and (img is TgxPersistentImage) and (pim.Picture.Bitmap <> nil) then
        begin
          WriteBoolean(True);
          ss := TStringStream.Create('');
          try
            bmp := TBitmap.Create;
            try
              bmp.Assign(pim.Picture.Bitmap);
              bmp.SaveToStream(ss);
            finally
              bmp.Free;
            end;
            WriteString(ss.DataString);
          finally
            ss.Free;
          end;
        end
        else
          WriteBoolean(False);
        WriteInteger(texExItem.TextureIndex);
        Write(texExItem.TextureOffset.AsAddress^, SizeOf(Single) * 3);
        Write(texExItem.TextureScale.AsAddress^, SizeOf(Single) * 3);
      end;
    end;
  end;
end;

procedure TgxMaterialLibrary.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
  libMat: TgxLibMaterial;
  i, n, size, tex, texCount: Integer;
  LName: string;
  ss: TStringStream;
  /// ->  bmp: TBitmap;
  texExItem: TgxTextureExItem;
begin
  archiveVersion := reader.ReadInteger;
  if (archiveVersion >= 0) and (archiveVersion <= 4) then
    with reader do
    begin
      if not FDoNotClearMaterialsOnLoad then
        Materials.Clear;
      n := ReadInteger;
      for i := 0 to n - 1 do
      begin
        // version 0
        LName := ReadString;
        if FDoNotClearMaterialsOnLoad then
          libMat := LibMaterialByName(LName)
        else
          libMat := nil;
        if ReadBoolean then
        begin
          ss := TStringStream.Create(ReadString);
          try
            /// ->            bmp := TBitmap.Create;
            try
              /// ->              bmp.LoadFromStream(ss);
              if libMat = nil then
                { TODO : E2250 There is no overloaded version of 'AddTextureMaterial' that can be called with these arguments }
                (* libMat := AddTextureMaterial(LName, bmp) *)
              else
                /// ->                libMat.Material.Texture.Image.Assign(bmp);
              finally
                /// ->              bmp.Free;
              end;
            finally
              ss.Free;
            end;

            // version 3
            if archiveVersion >= 3 then
              with libMat.Material.Texture do
              begin
                Read(BorderColor.AsAddress^, SizeOf(Single) * 4);
                Compression := TgxTextureCompression(ReadInteger);
                DepthTextureMode := TgxDepthTextureMode(ReadInteger);
                Read(EnvColor.AsAddress^, SizeOf(Single) * 4);
                FilteringQuality := TgxTextureFilteringQuality(ReadInteger);
                ImageAlpha := TgxTextureImageAlpha(ReadInteger);
                ImageBrightness := ReadFloat;
                ImageGamma := ReadFloat;
                MagFilter := TgxMagFilter(ReadInteger);
                MappingMode := TgxTextureMappingMode(ReadInteger);
                Read(MappingSCoordinates.AsAddress^, SizeOf(Single) * 4);
                Read(MappingTCoordinates.AsAddress^, SizeOf(Single) * 4);
                Read(MappingRCoordinates.AsAddress^, SizeOf(Single) * 4);
                Read(MappingQCoordinates.AsAddress^, SizeOf(Single) * 4);
                MinFilter := TgxMinFilter(ReadInteger);
                NormalMapScale := ReadFloat;
                TextureCompareFunc := TgxDepthCompareFunc(ReadInteger);
                TextureCompareMode := TgxTextureCompareMode(ReadInteger);
                TextureFormat := TgxTextureFormat(ReadInteger);
                TextureMode := TgxTextureMode(ReadInteger);
                TextureWrap := TgxTextureWrap(ReadInteger);
                TextureWrapR := TgxSeparateTextureWrap(ReadInteger);
                TextureWrapS := TgxSeparateTextureWrap(ReadInteger);
                TextureWrapT := TgxSeparateTextureWrap(ReadInteger);
              end;
            // version 3 end

          end
        else
        begin
          if libMat = nil then
          begin
            libMat := Materials.Add;
            libMat.name := LName;
          end;
        end;
        with libMat.Material.FrontProperties do
        begin
          Read(Ambient.AsAddress^, SizeOf(Single) * 3);
          Read(Diffuse.AsAddress^, SizeOf(Single) * 4);
          Read(Emission.AsAddress^, SizeOf(Single) * 3);
          Read(Specular.AsAddress^, SizeOf(Single) * 3);
        end;

        // version 1
        if archiveVersion >= 1 then
        begin
          with libMat.Material.FrontProperties do
          begin
            Read(FShininess, 1);
            libMat.Material.PolygonMode := TgxPolygonMode(ReadInteger);
          end;
          with libMat.Material.BackProperties do
          begin
            Read(Ambient.AsAddress^, SizeOf(Single) * 3);
            Read(Diffuse.AsAddress^, SizeOf(Single) * 4);
            Read(Emission.AsAddress^, SizeOf(Single) * 3);
            Read(Specular.AsAddress^, SizeOf(Single) * 3);
            Read(FShininess, 1);
            { PolygonMode := TPolygonMode( } ReadInteger;
          end;
          libMat.Material.BlendingMode := TgxBlendingMode(ReadInteger);

          // version 3
          if archiveVersion >= 3 then
          begin
            if ReadBoolean then
              with libMat.Material.BlendingParams do
              begin
                AlphaFuncRef := ReadFloat;
                AlphaFunctType := TgxAlphaFunc(ReadInteger);
                BlendFuncDFactor := TgxBlendFunction(ReadInteger);
                BlendFuncSFactor := TgxBlendFunction(ReadInteger);
                UseAlphaFunc := ReadBoolean;
                UseBlendFunc := ReadBoolean;
              end;

            libMat.Material.FaceCulling := TgxFaceCulling(ReadInteger);
          end;
          // version 3 end

          size := ReadInteger;
          Read(libMat.Material.FMaterialOptions, size);
          Read(libMat.TextureOffset.AsAddress^, SizeOf(Single) * 3);
          Read(libMat.TextureScale.AsAddress^, SizeOf(Single) * 3);
          libMat.Texture2Name := ReadString;

          // version 4
          if archiveVersion >= 4 then
            libMat.TextureRotate := ReadFloat;
        end;

        // version 2
        if archiveVersion >= 2 then
        begin
          texCount := ReadInteger;
          for tex := 0 to texCount - 1 do
          begin
            texExItem := libMat.Material.TextureEx.Add;
            if ReadBoolean then
            begin
              ss := TStringStream.Create(ReadString);
              /// ->              bmp := TBitmap.Create;
              try
                /// ->                bmp.LoadFromStream(ss);
                /// ->                texExItem.Texture.Image.Assign(bmp);
                texExItem.Texture.Enabled := True;
              finally
                /// ->                bmp.Free;
                ss.Free;
              end;
            end;
            texExItem.TextureIndex := ReadInteger;
            Read(texExItem.TextureOffset.AsAddress^, SizeOf(Single) * 3);
            Read(texExItem.TextureScale.AsAddress^, SizeOf(Single) * 3);
          end;
        end;
      end;
    end
  else
    RaiseFilerException(Self.ClassType, archiveVersion);
end;

procedure TgxMaterialLibrary.SaveToStream(aStream: TStream);
var
  wr: TgxBinaryWriter;
begin
  wr := TgxBinaryWriter.Create(aStream);
  try
    Self.WriteToFiler(wr);
  finally
    wr.Free;
  end;
end;

procedure TgxMaterialLibrary.LoadFromStream(aStream: TStream);
var
  rd: TgxBinaryReader;
begin
  rd := TgxBinaryReader.Create(aStream);
  try
    Self.ReadFromFiler(rd);
  finally
    rd.Free;
  end;
end;

procedure TgxMaterialLibrary.AddMaterialsFromStream(aStream: TStream);
begin
  FDoNotClearMaterialsOnLoad := True;
  try
    LoadFromStream(aStream);
  finally
    FDoNotClearMaterialsOnLoad := False;
  end;
end;

procedure TgxMaterialLibrary.SaveToFile(const fileName: string);
var
  fs: TStream;
begin
  fs := TFileStream.Create(fileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TgxMaterialLibrary.LoadFromFile(const fileName: string);
var
  fs: TStream;
begin
  fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyNone);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TgxMaterialLibrary.AddMaterialsFromFile(const fileName: string);
var
  fs: TStream;
begin
  fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyNone);
  try
    AddMaterialsFromStream(fs);
  finally
    fs.Free;
  end;
end;

function TgxMaterialLibrary.AddTextureMaterial(const MaterialName, fileName: string; persistent: Boolean = True)
  : TgxLibMaterial;
begin
  Result := Materials.Add;
  with Result do
  begin
    Name := MaterialName;
    with Material.Texture do
    begin
      MinFilter := miLinearMipmapLinear;
      MagFilter := maLinear;
      TextureMode := tmModulate;
      Disabled := False;
      if persistent then
      begin
        ImageClassName := TgxPersistentImage.ClassName;
        if fileName <> '' then
          Image.LoadFromFile(fileName);
      end
      else
      begin
        ImageClassName := TgxPicFileImage.ClassName;
        TgxPicFileImage(Image).PictureFileName := fileName;
      end;
    end;
  end;
end;

function TgxMaterialLibrary.AddTextureMaterial(const MaterialName: string; Graphic: TBitmap): TgxLibMaterial;
begin
  Result := Materials.Add;
  with Result do
  begin
    Name := MaterialName;
    with Material.Texture do
    begin
      MinFilter := miLinearMipmapLinear;
      MagFilter := maLinear;
      TextureMode := tmModulate;
      Disabled := False;
      Image.Assign(Graphic);
    end;
  end;
end;

function TgxMaterialLibrary.LibMaterialByName(const AName: TgxLibMaterialName): TgxLibMaterial;
begin
  if Assigned(Self) then
    Result := Materials.GetLibMaterialByName(AName)
  else
    Result := nil;
end;

function TgxMaterialLibrary.TextureByName(const LibMatName: TgxLibMaterialName): TgxTexture;
var
  libMat: TgxLibMaterial;
begin
  if Self = nil then
    raise ETexture.Create(strErrorEx + strMatLibNotDefined)
  else if LibMatName = '' then
    Result := nil
  else
  begin
    libMat := LibMaterialByName(LibMatName);
    if libMat = nil then
      raise ETexture.CreateFmt(strErrorEx + strMaterialNotFoundInMatlibEx, [LibMatName])
    else
      Result := libMat.Material.Texture;
  end;
end;

function TgxMaterialLibrary.GetNameOfTexture(const Texture: TgxTexture): TgxLibMaterialName;
begin
  if (Self = nil) or (Texture = nil) then
    Result := ''
  else
    Result := Materials.GetNameOfTexture(Texture);
end;

function TgxMaterialLibrary.GetMaterials: TgxLibMaterials;
begin
  Result := TgxLibMaterials(FMaterials);
end;

function TgxMaterialLibrary.GetNameOfLibMaterial(const libMat: TgxLibMaterial): TgxLibMaterialName;
begin
  if (Self = nil) or (libMat = nil) then
    Result := ''
  else
    Result := Materials.GetNameOfLibMaterial(libMat);
end;

{ TgxBlendingParameters }

procedure TgxBlendingParameters.Apply(var rci: TgxRenderContextInfo);
begin
  if FUseAlphaFunc then
  begin
    rci.gxStates.Enable(stAlphaTest);
    rci.gxStates.SetAlphaFunction(FAlphaFuncType, FAlphaFuncRef);
  end
  else
    rci.gxStates.Disable(stAlphaTest);
  if FUseBlendFunc then
  begin
    rci.gxStates.Enable(stBlend);
    if FSeparateBlendFunc then
      rci.gxStates.SetBlendFuncSeparate(FBlendFuncSFactor, FBlendFuncDFactor, FAlphaBlendFuncSFactor, FAlphaBlendFuncDFactor)
    else
      rci.gxStates.SetBlendFunc(FBlendFuncSFactor, FBlendFuncDFactor);
  end
  else
    rci.gxStates.Disable(stBlend);
end;

constructor TgxBlendingParameters.Create(AOwner: TPersistent);
begin
  inherited;
  FUseAlphaFunc := False;
  FAlphaFuncType := cfGreater;
  FAlphaFuncRef := 0;

  FUseBlendFunc := True;
  FSeparateBlendFunc := False;
  FBlendFuncSFactor := bfSrcAlpha;
  FBlendFuncDFactor := bfOneMinusSrcAlpha;
  FAlphaBlendFuncSFactor := bfSrcAlpha;
  FAlphaBlendFuncDFactor := bfOneMinusSrcAlpha;
end;

procedure TgxBlendingParameters.SetAlphaFuncRef(const Value: Single);
begin
  if (FAlphaFuncRef <> Value) then
  begin
    FAlphaFuncRef := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxBlendingParameters.SetAlphaFuncType(const Value: TgxAlphaFunc);
begin
  if (FAlphaFuncType <> Value) then
  begin
    FAlphaFuncType := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxBlendingParameters.SetBlendFuncDFactor(const Value: TgxBlendFunction);
begin
  if (FBlendFuncDFactor <> Value) then
  begin
    FBlendFuncDFactor := Value;
    if not FSeparateBlendFunc then
      FAlphaBlendFuncDFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxBlendingParameters.SetBlendFuncSFactor(const Value: TgxBlendFunction);
begin
  if (FBlendFuncSFactor <> Value) then
  begin
    FBlendFuncSFactor := Value;
    if not FSeparateBlendFunc then
      FAlphaBlendFuncSFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxBlendingParameters.SetAlphaBlendFuncDFactor(const Value: TgxBlendFunction);
begin
  if FSeparateBlendFunc and (FAlphaBlendFuncDFactor <> Value) then
  begin
    FAlphaBlendFuncDFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxBlendingParameters.SetAlphaBlendFuncSFactor(const Value: TgxBlendFunction);
begin
  if FSeparateBlendFunc and (FAlphaBlendFuncSFactor <> Value) then
  begin
    FAlphaBlendFuncSFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxBlendingParameters.SetUseAlphaFunc(const Value: Boolean);
begin
  if (FUseAlphaFunc <> Value) then
  begin
    FUseAlphaFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxBlendingParameters.SetUseBlendFunc(const Value: Boolean);
begin
  if (FUseBlendFunc <> Value) then
  begin
    FUseBlendFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxBlendingParameters.SetSeparateBlendFunc(const Value: Boolean);
begin
  if (FSeparateBlendFunc <> Value) then
  begin
    FSeparateBlendFunc := Value;
    if not Value then
    begin
      FAlphaBlendFuncSFactor := FBlendFuncSFactor;
      FAlphaBlendFuncDFactor := FBlendFuncDFactor;
    end;
    NotifyChange(Self);
  end;
end;

function TgxBlendingParameters.StoreAlphaFuncRef: Boolean;
begin
  Result := (Abs(AlphaFuncRef) > 0.001);
end;

//-------------------------------------------------
initialization
//-------------------------------------------------

RegisterClasses([TgxMaterialLibrary, TgxMaterial, TgxShader]);

end.
