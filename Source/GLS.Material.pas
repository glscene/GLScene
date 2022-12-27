//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.Material;

(* Handles all the material + material library stuff *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  Vcl.Graphics,

  GLS.OpenGLTokens,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.RenderContextInfo,
  GLS.BaseClasses,
  GLS.Context,
  GLS.Texture,
  GLS.Color,
  GLS.Coordinates,
  GLS.PersistentClasses,
  GLS.State,
  GLS.TextureFormat, 
  GLS.XOpenGL,
  GLS.ApplicationFileIO,
  GLS.Graphics,
  GLS.Utils,
  GLS.Strings,
  GLS.Logger;

{$UNDEF USE_MULTITHREAD}
type
  TGLFaceProperties = class;
  TGLMaterial = class;
  TGLAbstractMaterialLibrary = class;
  TGLMaterialLibrary = class;

  //an interface for proper TGLLibMaterialNameProperty support
  IGLMaterialLibrarySupported = interface(IInterface)
    ['{8E442AF9-D212-4A5E-8A88-92F798BABFD1}']
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
  end;

  TGLAbstractLibMaterial = class;
  TGLLibMaterial = class;

  (* Define GLShader style application relatively to a material.
      ssHighLevel: shader is applied before material application, and unapplied
           after material unapplication
      ssLowLevel: shader is applied after material application, and unapplied
           before material unapplication
      ssReplace: shader is applied in place of the material (and material
           is completely ignored)*)
  TGLShaderStyle = (ssHighLevel, ssLowLevel, ssReplace);

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
      Note: HandleFailedInitialization does *not* create this event,
      it is left to user shaders which may chose to override this procedure.
      Commented out, because not sure if this option should exist,
      let other generations of developers decide ;) *)
  TGLShaderFailedInitAction = (
    fiaSilentDisable, fiaRaiseStandardException, fiaRaiseHandledException,
    fiaReRaiseException {,fiaGenerateEvent});

  (* Generic, abstract shader class.
     Shaders are modeled here as an abstract material-altering entity with
     transaction-like behaviour. The base class provides basic context and user
     tracking, as well as setup/application facilities.
     Subclasses are expected to provide implementation for DoInitialize,
     DoApply, DoUnApply and DoFinalize. *)
  TGLShader = class(TGLUpdateAbleComponent)
  private
    FEnabled: Boolean;
    FLibMatUsers: TList;
    FVirtualHandle: TGLVirtualHandle;
    FShaderStyle: TGLShaderStyle;
    FUpdateCount: Integer;
    FShaderActive: Boolean;
    FFailedInitAction: TGLShaderFailedInitAction;
  protected
    (*Invoked once, before the first call to DoApply.
      The call happens with the OpenGL context being active. *)
    procedure DoInitialize(var rci: TGLRenderContextInfo; Sender: TObject); virtual;
    (* Request to apply the shader.
       Always followed by a DoUnApply when the shader is no longer needed. *)
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); virtual;
    (* Request to un-apply the shader.
       Subclasses can assume the shader has been applied previously.
       Return True to request a multipass. *)
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; virtual;
    (* Invoked once, before the destruction of context or release of shader.
       The call happens with the OpenGL context being active. *)
    procedure DoFinalize; virtual;
    function GetShaderInitialized: Boolean;
    procedure InitializeShader(var rci: TGLRenderContextInfo; Sender: TObject);
    procedure FinalizeShader;
    procedure OnVirtualHandleAllocate(sender: TGLVirtualHandle; var handle: Cardinal);
    procedure OnVirtualHandleDestroy(sender: TGLVirtualHandle; var handle: Cardinal);
    procedure SetEnabled(val: Boolean);
    property ShaderInitialized: Boolean read GetShaderInitialized;
    property ShaderActive: Boolean read FShaderActive;
    procedure RegisterUser(libMat: TGLLibMaterial);
    procedure UnRegisterUser(libMat: TGLLibMaterial);
    // Used by the DoInitialize procedure of descendant classes to raise errors. 
    procedure HandleFailedInitialization(const LastErrorMessage: string = '');
      virtual;
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
    procedure Apply(var rci: TGLRenderContextInfo; Sender: TObject);
    (* UnApply shader. 
       When returning True, the caller is expected to perform a multipass
       rendering by re-rendering then invoking UnApply again, until a
       "False" is returned. *)
    function UnApply(var rci: TGLRenderContextInfo): Boolean;
    // Shader application style (default is ssLowLevel). 
    property ShaderStyle: TGLShaderStyle read FShaderStyle write FShaderStyle
      default ssLowLevel;
    procedure Assign(Source: TPersistent); override;
    (* Defines if shader is supported by hardware/drivers.
       Default - always supported. Descendants are encouraged to override
       this function. *)
    function ShaderSupported: Boolean; virtual;
    (* Defines what to do if for some reason shader failed to initialize.
       Note, that in some cases it cannon be determined by just checking the
       required OpenGL extentions. You need to try to compile and link the
       shader - only at that stage you might catch an error *)
    property FailedInitAction: TGLShaderFailedInitAction
      read FFailedInitAction write FFailedInitAction default
      fiaRaiseStandardException;
  published
     (* Turns on/off shader application.
        Note that this only turns on/off the shader application, if the
        ShaderStyle is ssReplace, the material won't be applied even if
        the shader is disabled. *)
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TGLShaderClass = class of TGLShader;

  TGLShininess = 0..128;

  (* Stores basic face lighting properties.
     The lighting is described with the standard ambient/diffuse/emission/specular
     properties that behave like those of most rendering tools.
     You also have control over shininess (governs specular lighting) and
     polygon mode (lines / fill). *)
  TGLFaceProperties = class(TGLUpdateAbleObject)
  private
    FAmbient, FDiffuse, FSpecular, FEmission: TGLColor;
    FShininess: TGLShininess;
  protected
    procedure SetAmbient(AValue: TGLColor);
    procedure SetDiffuse(AValue: TGLColor);
    procedure SetEmission(AValue: TGLColor);
    procedure SetSpecular(AValue: TGLColor);
    procedure SetShininess(AValue: TGLShininess);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Apply(var rci: TGLRenderContextInfo; aFace: TGLCullFaceMode); inline;
    procedure ApplyNoLighting(var rci: TGLRenderContextInfo; aFace: TGLCullFaceMode); inline;
    procedure Assign(Source: TPersistent); override;
  published
    property Ambient: TGLColor read FAmbient write SetAmbient;
    property Diffuse: TGLColor read FDiffuse write SetDiffuse;
    property Emission: TGLColor read FEmission write SetEmission;
    property Shininess: TGLShininess read FShininess write SetShininess default 0;
    property Specular: TGLColor read FSpecular write SetSpecular;
  end;

  TGLDepthProperties = class(TGLUpdateAbleObject)
  private
    FDepthTest: boolean;
    FDepthWrite: boolean;
    FZNear, FZFar: Single;
    FCompareFunc: TGLDepthfunction;
    FDepthClamp: Boolean;
  protected
    procedure SetZNear(Value: Single);
    procedure SetZFar(Value: Single);
    procedure SetCompareFunc(Value: TGLDepthCompareFunc);
    procedure SetDepthTest(Value: boolean);
    procedure SetDepthWrite(Value: boolean);
    procedure SetDepthClamp(Value: boolean);
    function StoreZNear: Boolean;
    function StoreZFar: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Apply(var rci: TGLRenderContextInfo);
    procedure Assign(Source: TPersistent); override;
  published
    (* Specifies the mapping of the near clipping plane to
       window coordinates.  The initial value is 0.  *)
    property ZNear: Single read FZNear write SetZNear stored StoreZNear;
    (* Specifies the mapping of the far clipping plane to
       window coordinates.  The initial value is 1. *)
    property ZFar: Single read FZFar write SetZFar stored StoreZFar;
    (* Specifies the function used to compare each
       incoming pixel depth value with the depth value present in
       the depth buffer. *)
    property DepthCompareFunction: TGLDepthFunction
      read FCompareFunc write SetCompareFunc default cfLequal;
    (* DepthTest enabling. 
       When DepthTest is enabled, objects closer to the camera will hide
       farther ones (via use of Z-Buffering).
       When DepthTest is disabled, the latest objects drawn/rendered overlap
       all previous objects, whatever their distance to the camera.
       Even when DepthTest is enabled, objects may chose to ignore depth
       testing through the osIgnoreDepthBuffer of their ObjectStyle property. *)
    property DepthTest: boolean read FDepthTest write SetDepthTest default True;
    // If True, object will not write to Z-Buffer. 
    property DepthWrite: boolean read FDepthWrite write SetDepthWrite default True;
    // Enable clipping depth to the near and far planes 
    property DepthClamp: Boolean read FDepthClamp write SetDepthClamp default False;
  end;

  TGLLibMaterialName = string;

  TGlAlphaFunc = TGLComparisonFunction;

  TGLBlendingParameters = class(TGLUpdateAbleObject)
  private
    FUseAlphaFunc: Boolean;
    FUseBlendFunc: Boolean;
    FSeparateBlendFunc: Boolean;
    FAlphaFuncType: TGlAlphaFunc;
    FAlphaFuncRef: Single;
    FBlendFuncSFactor: TGLBlendFunction;
    FBlendFuncDFactor: TGLBlendFunction;
    FAlphaBlendFuncSFactor: TGLBlendFunction;
    FAlphaBlendFuncDFactor: TGLBlendFunction;
    procedure SetUseAlphaFunc(const Value: Boolean);
    procedure SetUseBlendFunc(const Value: Boolean);
    procedure SetSeparateBlendFunc(const Value: Boolean);
    procedure SetAlphaFuncRef(const Value: Single);
    procedure SetAlphaFuncType(const Value: TGlAlphaFunc);
    procedure SetBlendFuncDFactor(const Value: TGLBlendFunction);
    procedure SetBlendFuncSFactor(const Value: TGLBlendFunction);
    procedure SetAlphaBlendFuncDFactor(const Value: TGLBlendFunction);
    procedure SetAlphaBlendFuncSFactor(const Value: TGLBlendFunction);
    function StoreAlphaFuncRef: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Apply(var rci: TGLRenderContextInfo);  inline;
  published
    property UseAlphaFunc: Boolean read FUseAlphaFunc write SetUseAlphaFunc
      default False;
    property AlphaFunctType: TGlAlphaFunc read FAlphaFuncType write
      SetAlphaFuncType default cfGreater;
    property AlphaFuncRef: Single read FAlphaFuncRef write SetAlphaFuncRef
      stored StoreAlphaFuncRef;
    property UseBlendFunc: Boolean read FUseBlendFunc write SetUseBlendFunc
      default True;
    property SeparateBlendFunc: Boolean read FSeparateBlendFunc write SetSeparateBlendFunc
      default False;
    property BlendFuncSFactor: TGLBlendFunction read FBlendFuncSFactor write
      SetBlendFuncSFactor default bfSrcAlpha;
    property BlendFuncDFactor: TGLBlendFunction read FBlendFuncDFactor write
      SetBlendFuncDFactor default bfOneMinusSrcAlpha;
    property AlphaBlendFuncSFactor: TGLBlendFunction read FAlphaBlendFuncSFactor write
      SetAlphaBlendFuncSFactor default bfSrcAlpha;
    property AlphaBlendFuncDFactor: TGLBlendFunction read FAlphaBlendFuncDFactor write
      SetAlphaBlendFuncDFactor default bfOneMinusSrcAlpha;
  end;

  (* Simplified blending options.
     bmOpaque : disable blending
     bmTransparency : uses standard alpha blending
     bmAdditive : activates additive blending (with saturation)
     bmAlphaTest50 : uses opaque blending, with alpha-testing at 50% (full
        transparency if alpha is below 0.5, full opacity otherwise)
     bmAlphaTest100 : uses opaque blending, with alpha-testing at 100%
     bmModulate : uses modulation blending
     bmCustom : uses TGLBlendingParameters options *)
  TGLBlendingMode = (bmOpaque, bmTransparency, bmAdditive,
    bmAlphaTest50, bmAlphaTest100, bmModulate, bmCustom);

  TGLFaceCulling = (fcBufferDefault, fcCull, fcNoCull);

  (* Control special rendering options for a material.
     moIgnoreFog : fog is deactivated when the material is rendered *)
  TGLMaterialOption = (moIgnoreFog, moNoLighting);
  TGLMaterialOptions = set of TGLMaterialOption;

  (* Describes a rendering material.
     A material is basically a set of face properties (front and back) that take
     care of standard material rendering parameters (diffuse, ambient, emission
     and specular) and texture mapping.
     An instance of this class is available for almost all objects in GLScene
     to allow quick definition of material properties. It can link to a
     TGLLibMaterial (taken for a material library).
     The TGLLibMaterial has more advanced properties (like texture transforms)
     and provides a standard way of sharing definitions and texture maps. *)
  TGLMaterial = class(TGLUpdateAbleObject, IGLMaterialLibrarySupported, IGLTextureNotifyAble)
  private
    FFrontProperties, FBackProperties: TGLFaceProperties;
    FDepthProperties: TGLDepthProperties;
    FBlendingMode: TGLBlendingMode;
    FBlendingParams: TGLBlendingParameters;
    FTexture: TGLTexture;
    FTextureEx: TGLTextureEx;
    FMaterialLibrary: TGLAbstractMaterialLibrary;
    FLibMaterialName: TGLLibMaterialName;
    FMaterialOptions: TGLMaterialOptions;
    FFaceCulling: TGLFaceCulling;
    FPolygonMode: TGLPolygonMode;
    currentLibMaterial: TGLAbstractLibMaterial;
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
  protected
    function GetBackProperties: TGLFaceProperties; inline;
    procedure SetBackProperties(Values: TGLFaceProperties);
    procedure SetFrontProperties(Values: TGLFaceProperties);
    procedure SetDepthProperties(Values: TGLDepthProperties);
    procedure SetBlendingMode(const val: TGLBlendingMode);
    procedure SetMaterialOptions(const val: TGLMaterialOptions);
    function GetTexture: TGLTexture;
    procedure SetTexture(ATexture: TGLTexture);
    procedure SetMaterialLibrary(const val: TGLAbstractMaterialLibrary);
    procedure SetLibMaterialName(const val: TGLLibMaterialName);
    procedure SetFaceCulling(const val: TGLFaceCulling);
    procedure SetPolygonMode(AValue: TGLPolygonMode);
    function GetTextureEx: TGLTextureEx;
    procedure SetTextureEx(const value: TGLTextureEx);
    function StoreTextureEx: Boolean;
    procedure SetBlendingParams(const Value: TGLBlendingParameters);
    procedure NotifyLibMaterialDestruction;
    // Back, Front, Texture and blending not stored if linked to a LibMaterial
    function StoreMaterialProps: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure PrepareBuildList;  inline;
    procedure Apply(var rci: TGLRenderContextInfo);
    (* Restore non-standard material states that were altered;
       A return value of True is a multipass request. *)
    function UnApply(var rci: TGLRenderContextInfo): Boolean; inline;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure NotifyTexMapChange(Sender: TObject);
    procedure DestroyHandles;
    procedure Loaded;
    (* Returns True if the material is blended.
       Will return the libmaterial's blending if it is linked to a material library. *)
    function Blended: Boolean; inline;
    // True if the material has a secondary texture 
    function HasSecondaryTexture: Boolean;
    // True if the material comes from the library instead of the texture property
    function MaterialIsLinkedToLib: Boolean;  inline;
    // Gets the primary texture either from material library or the texture property
    function GetActualPrimaryTexture: TGLTexture;
    // Gets the primary Material either from material library or the texture property
    function GetActualPrimaryMaterial: TGLMaterial;
    // Return the LibMaterial (see LibMaterialName)
    function GetLibMaterial: TGLLibMaterial;
    procedure QuickAssignMaterial(const MaterialLibrary: TGLMaterialLibrary;
      const Material: TGLLibMaterial);
  published
    property BackProperties: TGLFaceProperties read GetBackProperties write
      SetBackProperties stored StoreMaterialProps;
    property FrontProperties: TGLFaceProperties read FFrontProperties write
      SetFrontProperties stored StoreMaterialProps;
    property DepthProperties: TGLDepthProperties read FDepthProperties write
      SetDepthProperties stored StoreMaterialProps;
    property BlendingMode: TGLBlendingMode read FBlendingMode write SetBlendingMode
      stored StoreMaterialProps default bmOpaque;
    property BlendingParams: TGLBlendingParameters read FBlendingParams 
	  write SetBlendingParams;
    property MaterialOptions: TGLMaterialOptions read FMaterialOptions 
	  write SetMaterialOptions default [];
    property Texture: TGLTexture read GetTexture write SetTexture stored StoreMaterialProps;
    property FaceCulling: TGLFaceCulling read FFaceCulling write SetFaceCulling default fcBufferDefault;
    property MaterialLibrary: TGLAbstractMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property LibMaterialName: TGLLibMaterialName read FLibMaterialName write SetLibMaterialName;
    property TextureEx: TGLTextureEx read GetTextureEx write SetTextureEx stored StoreTextureEx;
    property PolygonMode: TGLPolygonMode read FPolygonMode write SetPolygonMode default pmFill;
  end;

  TGLAbstractLibMaterial = class(
    TCollectionItem,
	IGLMaterialLibrarySupported,
	IGLNotifyAble)
  protected
    FUserList: TList;
    FName: TGLLibMaterialName;
    FNameHashKey: Integer;
    FTag: Integer;
	// Used for recursivity protection
    FNotifying: Boolean; 
    {implementing IGLMaterialLibrarySupported}
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
    // Implementing IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    function GetDisplayName: string; override;
    class function ComputeNameHashKey(const name: string): Integer;
    procedure SetName(const val: TGLLibMaterialName);
    procedure Loaded; virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Apply(var ARci: TGLRenderContextInfo); virtual;
    // Restore non-standard material states that were altered
    function UnApply(var ARci: TGLRenderContextInfo): Boolean; virtual;
    procedure RegisterUser(obj: TGLUpdateAbleObject); overload;
    procedure UnregisterUser(obj: TGLUpdateAbleObject); overload;
    procedure RegisterUser(comp: TGLUpdateAbleComponent); overload;
    procedure UnregisterUser(comp: TGLUpdateAbleComponent); overload;
    procedure RegisterUser(libMaterial: TGLLibMaterial); overload;
    procedure UnregisterUser(libMaterial: TGLLibMaterial); overload;
    procedure NotifyUsers;
	// Returns true if the texture has registed users
    function IsUsed: boolean; 
    property NameHashKey: Integer read FNameHashKey;
    procedure NotifyChange(Sender: TObject); virtual;
    function Blended: Boolean; virtual;
    property MaterialLibrary: TGLAbstractMaterialLibrary read GetMaterialLibrary;
  published
    property Name: TGLLibMaterialName read FName write SetName;
    property Tag: Integer read FTag write FTag;
  end;

  (* Material in a material library.
     Introduces Texture transformations (offset and scale). Those transformations
     are available only for lib materials to minimize the memory cost of basic
     materials (which are used in almost all objects). *)
  TGLLibMaterial = class(TGLAbstractLibMaterial, IGLTextureNotifyAble)
  private
    FMaterial: TGLMaterial;
    FTextureOffset, FTextureScale: TGLCoordinates;
    FTextureRotate: Single;
    FTextureMatrixIsIdentity: Boolean;
    FTextureOverride: Boolean;
    FTextureMatrix: TGLMatrix;
    FTexture2Name: TGLLibMaterialName;
    FShader: TGLShader;
    libMatTexture2: TGLLibMaterial; // internal cache
  protected
    procedure Loaded; override;
    procedure SetMaterial(const val: TGLMaterial);
    procedure SetTextureOffset(const val: TGLCoordinates);
    procedure SetTextureScale(const val: TGLCoordinates);
    procedure SetTextureMatrix(const Value: TGLMatrix);
    procedure SetTexture2Name(const val: TGLLibMaterialName);
    procedure SetShader(const val: TGLShader);
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
    procedure Apply(var ARci: TGLRenderContextInfo); override;
    { Restore non-standard material states that were altered}
    function UnApply(var ARci: TGLRenderContextInfo): Boolean; override;
    procedure NotifyUsersOfTexMapChange;
    property TextureMatrix: TGLMatrix read FTextureMatrix write SetTextureMatrix;
    property TextureMatrixIsIdentity: boolean read FTextureMatrixIsIdentity;
    procedure NotifyTexMapChange(Sender: TObject);
    function Blended: Boolean; override;
  published
    property Material: TGLMaterial read FMaterial write SetMaterial;
    // Texture offset in texture coordinates. The offset is applied after scaling
    property TextureOffset: TGLCoordinates read FTextureOffset write
      SetTextureOffset;
    (* Texture coordinates scaling.
       Scaling is applied <i>before</i> applying the offset, and is applied
       to the texture coordinates, meaning that a scale factor of (2, 2, 2)
       will make your texture look twice smaller *)
    property TextureScale: TGLCoordinates read FTextureScale write
      SetTextureScale;
    property TextureRotate: Single read FTextureRotate write
      SetTextureRotate stored StoreTextureRotate;
    (* Reference to the second texture.
       The referred LibMaterial *must* be in the same material library.
       Second textures are supported only through ARB multitexturing (ignored
       if not supported). *)
    property Texture2Name: TGLLibMaterialName read FTexture2Name write
      SetTexture2Name;
    {Optionnal shader for the material. }
    property Shader: TGLShader read FShader write SetShader;
  end;

  TGLAbstractLibMaterials = class(TOwnedCollection)
  protected
    procedure Loaded;
    function GetMaterial(const AName: TGLLibMaterialName): TGLAbstractLibMaterial; inline;
  public
    function MakeUniqueName(const nameRoot: TGLLibMaterialName):
      TGLLibMaterialName;
  end;

  // A collection of materials, mainly used in material libraries. 
  TGLLibMaterials = class(TGLAbstractLibMaterials)
  protected
    procedure SetItems(index: Integer; const val: TGLLibMaterial);
    function GetItems(index: Integer): TGLLibMaterial;
    procedure DestroyHandles;
  public
    constructor Create(AOwner: TComponent);
    function Owner: TPersistent;
    function IndexOf(const Item: TGLLibMaterial): Integer;
    function Add: TGLLibMaterial;
    function FindItemID(ID: Integer): TGLLibMaterial;
    property Items[index: Integer]: TGLLibMaterial read GetItems write SetItems; default;
    function GetLibMaterialByName(const AName: TGLLibMaterialName): TGLLibMaterial;
    // Returns index of this Texture if it exists. 
    function GetTextureIndex(const Texture: TGLTexture): Integer;
    // Returns index of this Material if it exists. 
    function GetMaterialIndex(const Material: TGLMaterial): Integer;
    // Returns name of this Texture if it exists. 
    function GetNameOfTexture(const Texture: TGLTexture): TGLLibMaterialName;
    // Returns name of this Material if it exists. 
    function GetNameOfLibMaterial(const Material: TGLLibMaterial):
      TGLLibMaterialName;
    procedure PrepareBuildList;
    (* Deletes all the unused materials in the collection.
       A material is considered unused if no other material or updateable object references it.
       WARNING: For this to work, objects that use the textuere, have to REGISTER to the texture.*)
    procedure DeleteUnusedMaterials;
  end;

  TGLAbstractMaterialLibrary = class(TGLCadenceAbleComponent)
  protected
    FMaterials: TGLAbstractLibMaterials;
    FLastAppliedMaterial: TGLAbstractLibMaterial;
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
    function ApplyMaterial(const AName: string;
      var ARci: TGLRenderContextInfo): Boolean;
    (* Un-applies the last applied material.
       Use this function in conjunction with ApplyMaterial.
       If no material was applied, an assertion will be triggered. *)
    function UnApplyMaterial(var ARci: TGLRenderContextInfo): Boolean;
  end;

  (* Stores a set of materials, to be used and shared by scene objects.
     Use a material libraries for storing commonly used materials, it provides
     an efficient way to share texture and material data among many objects,
     thus reducing memory needs and rendering time. 
     Materials in a material library also feature advanced control properties
     like texture coordinates transforms. *)
  TGLMaterialLibrary = class(TGLAbstractMaterialLibrary)
  private
    FDoNotClearMaterialsOnLoad: Boolean;
    FOnTextureNeeded: TGLTextureNeededEvent;
  protected
    function GetMaterials: TGLLibMaterials;
    procedure SetMaterials(const val: TGLLibMaterials);
    function StoreMaterials: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyHandles;
    procedure WriteToFiler(writer: TGLVirtualWriter);
    procedure ReadFromFiler(reader: TGLVirtualReader);
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
       (via a TGLPersistentImage), if false, it will be unloaded after upload
       to OpenGL (via TGLPicFileImage). *)
    function AddTextureMaterial(const materialName, fileName: string;
      persistent: Boolean = True): TGLLibMaterial; overload;
    // Add a "standard" texture material. TGLGraphic based variant. 
    function AddTextureMaterial(const materialName: string; graphic:
      TGraphic): TGLLibMaterial; overload;
    // Returns libMaterial of given name if any exists. 
    function LibMaterialByName(const AName: TGLLibMaterialName): TGLLibMaterial;
    // Returns Texture of given material's name if any exists. 
    function TextureByName(const LibMatName: TGLLibMaterialName): TGLTexture;
    // Returns name of texture if any exists. 
    function GetNameOfTexture(const Texture: TGLTexture): TGLLibMaterialName;
    // Returns name of Material if any exists. 
    function GetNameOfLibMaterial(const LibMat: TGLLibMaterial): TGLLibMaterialName;
  published
    // The materials collection. 
    property Materials: TGLLibMaterials read GetMaterials write SetMaterials stored
      StoreMaterials;
    (* This event is fired whenever a texture needs to be loaded from disk. 
       The event is triggered before even attempting to load the texture,
       and before TexturePaths is used. *)
    property OnTextureNeeded: TGLTextureNeededEvent read FOnTextureNeeded write
      FOnTextureNeeded;
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

procedure TGLShader.DoApply(var Rci: TGLRenderContextInfo; Sender: TObject);
begin
end;

function TGLShader.DoUnApply(var Rci: TGLRenderContextInfo): Boolean;
begin
  Result := True;
end;

procedure TGLAbstractLibMaterial.Loaded;
begin
end;

procedure TGLAbstractLibMaterial.Apply(var ARci: TGLRenderContextInfo);
begin
end;

function TGLAbstractLibMaterial.UnApply(var ARci: TGLRenderContextInfo): Boolean;
begin
  Result := True;
end;

// ------------------
// ------------------ TGLFaceProperties ------------------
// ------------------

constructor TGLFaceProperties.Create(aOwner: TPersistent);
begin
  inherited;
  // OpenGL default colors
  FAmbient := TGLColor.CreateInitialized(Self, clrGray20);
  FDiffuse := TGLColor.CreateInitialized(Self, clrGray80);
  FEmission := TGLColor.Create(Self);
  FSpecular := TGLColor.Create(Self);
  FShininess := 0;
end;

destructor TGLFaceProperties.Destroy;
begin
  FAmbient.Free;
  FDiffuse.Free;
  FEmission.Free;
  FSpecular.Free;
  inherited Destroy;
end;

procedure TGLFaceProperties.Apply(var rci: TGLRenderContextInfo;
  aFace: TGLCullFaceMode);
begin
  with rci.GLStates do
  begin
    SetGLMaterialColors(aFace,
    Emission.Color, Ambient.Color, Diffuse.Color, Specular.Color, FShininess);
  end;
end;

procedure TGLFaceProperties.ApplyNoLighting(var rci: TGLRenderContextInfo;
  aFace: TGLCullFaceMode);
begin
  gl.Color4fv(Diffuse.AsAddress);
end;

procedure TGLFaceProperties.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLFaceProperties) then
  begin
    FAmbient.DirectColor := TGLFaceProperties(Source).Ambient.Color;
    FDiffuse.DirectColor := TGLFaceProperties(Source).Diffuse.Color;
    FEmission.DirectColor := TGLFaceProperties(Source).Emission.Color;
    FSpecular.DirectColor := TGLFaceProperties(Source).Specular.Color;
    FShininess := TGLFaceProperties(Source).Shininess;
    NotifyChange(Self);
  end;
end;

procedure TGLFaceProperties.SetAmbient(AValue: TGLColor);
begin
  FAmbient.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

procedure TGLFaceProperties.SetDiffuse(AValue: TGLColor);
begin
  FDiffuse.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

procedure TGLFaceProperties.SetEmission(AValue: TGLColor);
begin
  FEmission.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

procedure TGLFaceProperties.SetSpecular(AValue: TGLColor);
begin
  FSpecular.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

procedure TGLFaceProperties.SetShininess(AValue: TGLShininess);
begin
  if FShininess <> AValue then
  begin
    FShininess := AValue;
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TGLDepthProperties ------------------
// ------------------

constructor TGLDepthProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDepthTest := True;
  FDepthWrite := True;
  FZNear := 0;
  FZFar := 1;
  FCompareFunc := cfLequal;
  FDepthClamp := False;
end;

procedure TGLDepthProperties.Apply(var rci: TGLRenderContextInfo);
begin
  with rci.GLStates do
  begin
    if FDepthTest and rci.bufferDepthTest then
      Enable(stDepthTest)
    else
      Disable(stDepthTest);
    DepthWriteMask := FDepthWrite;
    DepthFunc := FCompareFunc;
    SetDepthRange(FZNear, FZFar);
    if GL.ARB_depth_clamp then
      if FDepthClamp then
        Enable(stDepthClamp)
      else
        Disable(stDepthClamp);
  end;
end;

procedure TGLDepthProperties.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLDepthProperties) then
  begin
    FDepthTest := TGLDepthProperties(Source).FDepthTest;
    FDepthWrite := TGLDepthProperties(Source).FDepthWrite;
    FZNear := TGLDepthProperties(Source).FZNear;
    FZFar := TGLDepthProperties(Source).FZFar;
    FCompareFunc := TGLDepthProperties(Source).FCompareFunc;
    NotifyChange(Self);
  end;
end;

procedure TGLDepthProperties.SetZNear(Value: Single);
begin
  Value := ClampValue(Value, 0, 1);
  if Value <> FZNear then
  begin
    FZNear := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLDepthProperties.SetZFar(Value: Single);
begin
  Value := ClampValue(Value, 0, 1);
  if Value <> FZFar then
  begin
    FZFar := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLDepthProperties.SetCompareFunc(Value: TGLDepthFunction);
begin
  if Value <> FCompareFunc then
  begin
    FCompareFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLDepthProperties.SetDepthTest(Value: boolean);
begin
  if Value <> FDepthTest then
  begin
    FDepthTest := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLDepthProperties.SetDepthWrite(Value: boolean);
begin
  if Value <> FDepthWrite then
  begin
    FDepthWrite := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLDepthProperties.SetDepthClamp(Value: boolean);
begin
  if Value <> FDepthClamp then
  begin
    FDepthClamp := Value;
    NotifyChange(Self);
  end;
end;

function TGLDepthProperties.StoreZNear: Boolean;
begin
  Result := FZNear <> 0.0;
end;

function TGLDepthProperties.StoreZFar: Boolean;
begin
  Result := FZFar <> 1.0;
end;

// ------------------
// ------------------ TGLShader ------------------
// ------------------

constructor TGLShader.Create(AOwner: TComponent);
begin
  FLibMatUsers := TList.Create;
  FVirtualHandle := TGLVirtualHandle.Create;
  FVirtualHandle.OnAllocate := OnVirtualHandleAllocate;
  FVirtualHandle.OnDestroy := OnVirtualHandleDestroy;
  FShaderStyle := ssLowLevel;
  FEnabled := True;
  FFailedInitAction := fiaRaiseStandardException;
  inherited;
end;

destructor TGLShader.Destroy;
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
    TGLLibMaterial(list[i]).Shader := nil;
  list.Free;
  FVirtualHandle.Free;
end;

procedure TGLShader.NotifyChange(Sender: TObject);
var
  i: Integer;
begin
  if FUpdateCount = 0 then
  begin
    for i := FLibMatUsers.Count - 1 downto 0 do
      TGLLibMaterial(FLibMatUsers[i]).NotifyUsers;
    FinalizeShader;
  end;
end;

procedure TGLShader.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TGLShader.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    NotifyChange(Self);
end;

procedure TGLShader.DoInitialize(var rci: TGLRenderContextInfo; Sender: TObject);
begin
  // nothing here
end;

procedure TGLShader.DoFinalize;
begin
  // nothing here
end;

function TGLShader.GetShaderInitialized: Boolean;
begin
  Result := (FVirtualHandle.Handle <> 0);
end;

procedure TGLShader.InitializeShader(var rci: TGLRenderContextInfo; Sender:
  TObject);
begin
  FVirtualHandle.AllocateHandle;
  if FVirtualHandle.IsDataNeedUpdate then
  begin
    DoInitialize(rci, Sender);
    FVirtualHandle.NotifyDataUpdated;
  end;
end;

 procedure TGLShader.FinalizeShader;
begin
  FVirtualHandle.NotifyChangesOfData;
  DoFinalize;
end;

procedure TGLShader.Apply(var rci: TGLRenderContextInfo; Sender: TObject);
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

function TGLShader.UnApply(var rci: TGLRenderContextInfo): Boolean;
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

procedure TGLShader.OnVirtualHandleDestroy(sender: TGLVirtualHandle; var handle:
  Cardinal);
begin
  handle := 0;
end;

procedure TGLShader.OnVirtualHandleAllocate(sender: TGLVirtualHandle; var
  handle: Cardinal);
begin
  handle := 1;
end;

procedure TGLShader.SetEnabled(val: Boolean);
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

procedure TGLShader.RegisterUser(libMat: TGLLibMaterial);
var
  i: Integer;
begin
  i := FLibMatUsers.IndexOf(libMat);
  if i < 0 then
    FLibMatUsers.Add(libMat);
end;

procedure TGLShader.UnRegisterUser(libMat: TGLLibMaterial);
begin
  if Assigned(FLibMatUsers) then
    FLibMatUsers.Remove(libMat);
end;

procedure TGLShader.Assign(Source: TPersistent);
begin
  if Source is TGLShader then
  begin
    FShaderStyle := TGLShader(Source).FShaderStyle;
    FFailedInitAction := TGLShader(Source).FFailedInitAction;
    Enabled := TGLShader(Source).FEnabled;
  end
  else
    inherited Assign(Source); //to the pit of doom ;)
end;

function TGLShader.ShaderSupported: Boolean;
begin
  Result := True;
end;

procedure TGLShader.HandleFailedInitialization(const LastErrorMessage: string =
  '');
begin
  case FailedInitAction of
    fiaSilentdisable: ; // Do nothing ;)
    fiaRaiseHandledException:
      try
        raise EGLShaderException.Create(GetStardardNotSupportedMessage);
      except
      end;
    fiaRaiseStandardException:
      raise EGLShaderException.Create(GetStardardNotSupportedMessage);
    fiaReRaiseException:
      begin
        if LastErrorMessage <> '' then
          raise EGLShaderException.Create(LastErrorMessage)
        else
          raise EGLShaderException.Create(GetStardardNotSupportedMessage)
      end;
    //    fiaGenerateEvent:; // Do nothing. Event creation is left up to user shaders
    //                       // which may choose to override this procedure.
  else
    Assert(False, strErrorEx + strUnknownType);
  end;
end;

function TGLShader.GetStardardNotSupportedMessage: string;
begin
  if Name <> '' then
    Result := 'Your hardware/driver doesn''t support shader "' + Name + '"!'
  else
    Result := 'Your hardware/driver doesn''t support shader "' + ClassName +
      '"!';
end;

// ------------------
//----------------- TGLMaterial -------------------------
// ------------------
constructor TGLMaterial.Create(AOwner: TPersistent);
begin
  inherited;
  FFrontProperties := TGLFaceProperties.Create(Self);
  FBackProperties  := TGLFaceProperties.Create(Self);
  FTexture := nil; // AutoCreate
  FFaceCulling := fcBufferDefault;
  FPolygonMode := pmFill;
  FBlendingParams := TGLBlendingParameters.Create(Self);
  FDepthProperties := TGLDepthProperties.Create(Self)
end;

destructor TGLMaterial.Destroy;
begin
  if Assigned(currentLibMaterial) then
    currentLibMaterial.UnregisterUser(Self);
  FBackProperties.Free;
  FFrontProperties.Free;
  FDepthProperties.Free;
  FTexture.Free;
  FTextureEx.Free;
  FBlendingParams.Free;
  inherited Destroy;
end;

function TGLMaterial.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLMaterial.SetBackProperties(Values: TGLFaceProperties);
begin
  FBackProperties.Assign(Values);
  NotifyChange(Self);
end;

function TGLMaterial.GetBackProperties: TGLFaceProperties;
begin
  Result := FBackProperties;
end;

procedure TGLMaterial.SetFrontProperties(Values: TGLFaceProperties);
begin
  FFrontProperties.Assign(Values);
  NotifyChange(Self);
end;

procedure TGLMaterial.SetDepthProperties(Values: TGLDepthProperties);
begin
  FDepthProperties.Assign(Values);
  NotifyChange(Self);
end;

procedure TGLMaterial.SetBlendingMode(const val: TGLBlendingMode);
begin
  if val <> FBlendingMode then
  begin
    FBlendingMode := val;
    NotifyChange(Self);
  end;
end;

procedure TGLMaterial.SetMaterialOptions(const val: TGLMaterialOptions);
begin
  if val <> FMaterialOptions then
  begin
    FMaterialOptions := val;
    NotifyChange(Self);
  end;
end;

function TGLMaterial.GetTexture: TGLTexture;
begin
  if not Assigned(FTexture) then
    FTexture := TGLTexture.Create(Self);
  Result := FTexture;
end;

procedure TGLMaterial.SetTexture(aTexture: TGLTexture);
begin
  if Assigned(aTexture) then
    Texture.Assign(ATexture)
  else
    FreeAndNil(FTexture);
end;

procedure TGLMaterial.SetFaceCulling(const val: TGLFaceCulling);
begin
  if val <> FFaceCulling then
  begin
    FFaceCulling := val;
    NotifyChange(Self);
  end;
end;

procedure TGLMaterial.SetMaterialLibrary(const val: TGLAbstractMaterialLibrary);
begin
  FMaterialLibrary := val;
  SetLibMaterialName(LibMaterialName);
end;

procedure TGLMaterial.SetLibMaterialName(const val: TGLLibMaterialName);
var
  oldLibrary: TGLMaterialLibrary;
  function MaterialLoopFrom(curMat: TGLLibMaterial): Boolean;
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
  newLibMaterial: TGLAbstractLibMaterial;
begin
  // locate new libmaterial
  if Assigned(FMaterialLibrary) then
    newLibMaterial := FMaterialLibrary.FMaterials.GetMaterial(val)
  else
    newLibMaterial := nil;

   // make sure new won't trigger an infinite loop
  if FMaterialLibrary is TGLMaterialLibrary then
  begin
    oldLibrary := TGLMaterialLibrary(FMaterialLibrary);
    if MaterialLoopFrom(TGLLibMaterial(newLibMaterial)) then
    begin
      if IsDesignTime then
        InformationDlg(Format(strCyclicRefMat, [val]))
      else
        GLSLogger.LogErrorFmt(strCyclicRefMat, [val]);
      exit;
    end;
  end;

  FLibMaterialName := val;
  // unregister if required
  if newLibMaterial <> currentLibMaterial then
  begin
    // unregister from old
    if Assigned(currentLibMaterial) then
      currentLibMaterial.UnregisterUser(Self);
    currentLibMaterial := newLibMaterial;
    // register with new
    if Assigned(currentLibMaterial) then
      currentLibMaterial.RegisterUser(Self);
    NotifyTexMapChange(Self);
  end;
end;

function TGLMaterial.GetTextureEx: TGLTextureEx;
begin
  if not Assigned(FTextureEx) then
    FTextureEx := TGLTextureEx.Create(Self);
  Result := FTextureEx;
end;

procedure TGLMaterial.SetTextureEx(const Value: TGLTextureEx);
begin
  if Assigned(Value) or Assigned(FTextureEx) then
    TextureEx.Assign(Value);
end;

function TGLMaterial.StoreTextureEx: Boolean;
begin
  Result := (Assigned(FTextureEx) and (TextureEx.Count > 0));
end;

procedure TGLMaterial.SetBlendingParams(const Value: TGLBlendingParameters);
begin
  FBlendingParams.Assign(Value);
  NotifyChange(Self);
end;

procedure TGLMaterial.NotifyLibMaterialDestruction;
begin
  FMaterialLibrary := nil;
  FLibMaterialName := '';
  currentLibMaterial := nil;
end;

procedure TGLMaterial.Loaded;
begin
  inherited;
  if Assigned(FTextureEx) then
    TextureEx.Loaded;
end;

function TGLMaterial.StoreMaterialProps: Boolean;
begin
  Result := not Assigned(currentLibMaterial);
end;

procedure TGLMaterial.PrepareBuildList;
begin
  if Assigned(FTexture) and (not FTexture.Disabled) then
    FTexture.PrepareBuildList;
end;

procedure TGLMaterial.Apply(var rci: TGLRenderContextInfo);
begin
  if Assigned(currentLibMaterial) then
    currentLibMaterial.Apply(rci)
  else
  with rci.GLStates do
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
      fcCull: Enable(stCullFace);
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

function TGLMaterial.UnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  if Assigned(currentLibMaterial) then
    Result := currentLibMaterial.UnApply(rci)
  else
  begin
    if Assigned(FTexture) and (not FTexture.Disabled) and (not
      FTextureEx.IsTextureEnabled(0)) then
      FTexture.UnApply(rci)
    else if Assigned(FTextureEx) then
      FTextureEx.UnApply(rci);
    Result := False;
  end;
end;

procedure TGLMaterial.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLMaterial) then
  begin
    if Assigned(TGLMaterial(Source).FBackProperties) then
      BackProperties.Assign(TGLMaterial(Source).BackProperties)
    else
      FreeAndNil(FBackProperties);
    FFrontProperties.Assign(TGLMaterial(Source).FFrontProperties);
    FPolygonMode := TGLMaterial(Source).FPolygonMode;
    FBlendingMode := TGLMaterial(Source).FBlendingMode;
    FMaterialOptions := TGLMaterial(Source).FMaterialOptions;
    if Assigned(TGLMaterial(Source).FTexture) then
      Texture.Assign(TGLMaterial(Source).FTexture)
    else
      FreeAndNil(FTexture);
    FFaceCulling := TGLMaterial(Source).FFaceCulling;
    FMaterialLibrary := TGLMaterial(Source).MaterialLibrary;
    SetLibMaterialName(TGLMaterial(Source).LibMaterialName);
    TextureEx.Assign(TGLMaterial(Source).TextureEx);
    FDepthProperties.Assign(TGLMaterial(Source).DepthProperties);
    NotifyChange(Self);
  end
  else
    inherited;
end;

procedure TGLMaterial.NotifyChange(Sender: TObject);
var
  intf: IGLNotifyAble;
begin
  if Supports(Owner, IGLNotifyAble, intf) then
    intf.NotifyChange(Self);
end;

procedure TGLMaterial.NotifyTexMapChange(Sender: TObject);
var
  intf: IGLTextureNotifyAble;
begin
  if Supports(Owner, IGLTextureNotifyAble, intf) then
    intf.NotifyTexMapChange(Self)
  else
    NotifyChange(Self);
end;

procedure TGLMaterial.DestroyHandles;
begin
  if Assigned(FTexture) then
    FTexture.DestroyHandles;
end;

function TGLMaterial.Blended: Boolean;
begin
  if Assigned(currentLibMaterial) then
  begin

    Result := currentLibMaterial.Blended
  end
  else
    Result := not (BlendingMode in [bmOpaque, bmAlphaTest50, bmAlphaTest100, bmCustom]);
end;

function TGLMaterial.HasSecondaryTexture: Boolean;
begin
  Result := Assigned(currentLibMaterial)
    and (currentLibMaterial is TGLLibMaterial)
    and Assigned(TGLLibMaterial(currentLibMaterial).libMatTexture2);
end;

function TGLMaterial.MaterialIsLinkedToLib: Boolean;
begin
  Result := Assigned(currentLibMaterial);
end;

function TGLMaterial.GetActualPrimaryTexture: TGLTexture;
begin
  if Assigned(currentLibMaterial) and (currentLibMaterial is TGLLibMaterial) then
    Result := TGLLibMaterial(currentLibMaterial).Material.Texture
  else
    Result := Texture;
end;

function TGLMaterial.GetActualPrimaryMaterial: TGLMaterial;
begin
  if Assigned(currentLibMaterial) and (currentLibMaterial is TGLLibMaterial) then
    Result := TGLLibMaterial(currentLibMaterial).Material
  else
    Result := Self;
end;

function TGLMaterial.GetLibMaterial: TGLLibMaterial;
begin
  if Assigned(currentLibMaterial) and (currentLibMaterial is TGLLibMaterial) then
    Result := TGLLibMaterial(currentLibMaterial)
  else
    Result := nil;
end;

procedure TGLMaterial.QuickAssignMaterial(const MaterialLibrary:
  TGLMaterialLibrary; const Material: TGLLibMaterial);
begin
  FMaterialLibrary := MaterialLibrary;
  FLibMaterialName := Material.FName;

  if Material <> CurrentLibMaterial then
  begin
    // unregister from old
    if Assigned(CurrentLibMaterial) then
      currentLibMaterial.UnregisterUser(Self);
    CurrentLibMaterial := Material;
    // register with new
    if Assigned(CurrentLibMaterial) then
      CurrentLibMaterial.RegisterUser(Self);

    NotifyTexMapChange(Self);
  end;
end;

procedure TGLMaterial.SetPolygonMode(AValue: TGLPolygonMode);
begin
  if AValue <> FPolygonMode then
  begin
    FPolygonMode := AValue;
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TGLAbstractLibMaterial ------------------
// ------------------

constructor TGLAbstractLibMaterial.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FUserList := TList.Create;
  if Assigned(ACollection) then
  begin
    FName := TGLAbstractLibMaterials(ACollection).MakeUniqueName('LibMaterial');
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;

destructor TGLAbstractLibMaterial.Destroy;
begin
  FUserList.Free;
  inherited Destroy;
end;

procedure TGLAbstractLibMaterial.Assign(Source: TPersistent);
begin
  if Source is TGLAbstractLibMaterial then
  begin
    FName :=
      TGLLibMaterials(Collection).MakeUniqueName(TGLLibMaterial(Source).Name);
    FNameHashKey := ComputeNameHashKey(FName);
  end
  else
    inherited; // Raise AssignError
end;

function TGLAbstractLibMaterial.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TGLAbstractLibMaterial._AddRef: Integer; stdcall;
begin
  Result := -1; //ignore
end;

function TGLAbstractLibMaterial._Release: Integer; stdcall;
begin
  Result := -1; //ignore
end;

procedure TGLAbstractLibMaterial.RegisterUser(obj: TGLUpdateAbleObject);
begin
  Assert(FUserList.IndexOf(obj) < 0);
  FUserList.Add(obj);
end;

procedure TGLAbstractLibMaterial.UnRegisterUser(obj: TGLUpdateAbleObject);
begin
  FUserList.Remove(obj);
end;

procedure TGLAbstractLibMaterial.RegisterUser(comp: TGLUpdateAbleComponent);
begin
  Assert(FUserList.IndexOf(comp) < 0);
  FUserList.Add(comp);
end;

procedure TGLAbstractLibMaterial.UnRegisterUser(comp: TGLUpdateAbleComponent);
begin
  FUserList.Remove(comp);
end;

procedure TGLAbstractLibMaterial.RegisterUser(libMaterial: TGLLibMaterial);
begin
  Assert(FUserList.IndexOf(libMaterial) < 0);
  FUserList.Add(libMaterial);
end;

procedure TGLAbstractLibMaterial.UnRegisterUser(libMaterial: TGLLibMaterial);
begin
  FUserList.Remove(libMaterial);
end;

procedure TGLAbstractLibMaterial.NotifyChange(Sender: TObject);
begin
  NotifyUsers();
end;

procedure TGLAbstractLibMaterial.NotifyUsers;
var
  i: Integer;
  obj: TObject;
begin
  if FNotifying then
    Exit;
  FNotifying := True;
  try
    for i := 0 to FUserList.Count - 1 do
    begin
      obj := TObject(FUserList[i]);
      if obj is TGLUpdateAbleObject then
        TGLUpdateAbleObject(FUserList[i]).NotifyChange(Self)
      else if obj is TGLUpdateAbleComponent then
        TGLUpdateAbleComponent(FUserList[i]).NotifyChange(Self)
      else
      begin
        Assert(obj is TGLAbstractLibMaterial);
        TGLAbstractLibMaterial(FUserList[i]).NotifyUsers;
      end;
    end;
  finally
    FNotifying := False;
  end;
end;

function TGLAbstractLibMaterial.IsUsed: Boolean;
begin
  Result := Assigned(Self) and (FUserlist.Count > 0);
end;

function TGLAbstractLibMaterial.GetDisplayName: string;
begin
  Result := Name;
end;

function TGLAbstractLibMaterial.GetMaterialLibrary: TGLAbstractMaterialLibrary;
var
  LOwner: TPersistent;
begin
  Result := nil;
  if Assigned(Collection) then
  begin
    LOwner := TGLAbstractLibMaterials(Collection).Owner;
    if LOwner is TGLAbstractMaterialLibrary then
      Result := TGLAbstractMaterialLibrary(LOwner);
  end;
end;

function TGLAbstractLibMaterial.Blended: Boolean;
begin
  Result := False;
end;

class function TGLAbstractLibMaterial.ComputeNameHashKey(const name: string): Integer;
var
  i, n: Integer;
begin
  n := Length(name);
  Result := n;
  for i := 1 to n do
    Result := (Result shl 1) + Byte(name[i]);
end;

procedure TGLAbstractLibMaterial.SetName(const val: TGLLibMaterialName);
begin
  if val <> FName then
  begin
    if not (csLoading in TComponent(Collection.Owner).ComponentState) then
    begin
      if TGLLibMaterials(Collection).GetLibMaterialByName(val) <> Self then
        FName := TGLLibMaterials(Collection).MakeUniqueName(val)
      else
        FName := val;
    end
    else
      FName := val;
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;



// ------------------
// ------------------ TGLLibMaterial ------------------
// ------------------

constructor TGLLibMaterial.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMaterial := TGLMaterial.Create(Self);
  FMaterial.Texture.OnTextureNeeded := DoOnTextureNeeded;
  FTextureOffset := TGLCoordinates.CreateInitialized(Self, NullHmgVector, csPoint);
  FTextureOffset.OnNotifyChange := OnNotifyChange;
  FTextureScale := TGLCoordinates.CreateInitialized(Self, XYZHmgVector, csPoint);
  FTextureScale.OnNotifyChange := OnNotifyChange;
  FTextureRotate := 0;
  FTextureOverride := False;
  FTextureMatrixIsIdentity := True;
end;

destructor TGLLibMaterial.Destroy;
var
  i: Integer;
  matObj: TObject;
begin
  Shader := nil; // drop dependency
  Texture2Name := ''; // drop dependency
  for i := 0 to FUserList.Count - 1 do
  begin
    matObj := TObject(FUserList[i]);
    if matObj is TGLMaterial then
      TGLMaterial(matObj).NotifyLibMaterialDestruction
    else if matObj is TGLLibMaterial then
    begin
      TGLLibMaterial(matObj).libMatTexture2 := nil;
      TGLLibMaterial(matObj).FTexture2Name := '';
    end;
  end;
  FMaterial.Free;
  FTextureOffset.Free;
  FTextureScale.Free;
  inherited;
end;

procedure TGLLibMaterial.Assign(Source: TPersistent);
begin
  if Source is TGLLibMaterial then
  begin
    FMaterial.Assign(TGLLibMaterial(Source).Material);
    FTextureOffset.Assign(TGLLibMaterial(Source).TextureOffset);
    FTextureScale.Assign(TGLLibMaterial(Source).TextureScale);
    FTextureRotate := TGLLibMaterial(Source).TextureRotate;
    TextureMatrix := TGLLibMaterial(Source).TextureMatrix;
    FTextureOverride := TGLLibMaterial(Source).FTextureOverride;
    FTexture2Name := TGLLibMaterial(Source).Texture2Name;
    FShader := TGLLibMaterial(Source).Shader;
  end;
  inherited;
end;

function TGLLibMaterial.Blended: Boolean;
begin
  Result := Material.Blended;
end;

procedure TGLLibMaterial.PrepareBuildList;
begin
  if Assigned(Self) then
    Material.PrepareBuildList;
end;

procedure TGLLibMaterial.Apply(var ARci: TGLRenderContextInfo);
var
  multitextured: Boolean;
begin
  xgl.BeginUpdate;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssHighLevel: Shader.Apply(ARci, Self);
      ssReplace:
        begin
          Shader.Apply(ARci, Self);
          Exit;
        end;
    end;
  end
  else
    ARci.GLStates.CurrentProgram := 0;
  if (Texture2Name <> '') and GL.ARB_multitexture and (not
    xgl.SecondTextureUnitForbidden) then
  begin
    if not Assigned(libMatTexture2) then
    begin
      libMatTexture2 :=
        TGLLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
      if Assigned(libMatTexture2) then
        libMatTexture2.RegisterUser(Self)
      else
        FTexture2Name := '';
    end;
    multitextured := Assigned(libMatTexture2)
      and (not libMatTexture2.Material.Texture.Disabled);
  end
  else
    multitextured := False;
  if not multitextured then
  begin
    // no multitexturing ("standard" mode)
    if not FTextureMatrixIsIdentity then
        ARci.GLStates.SetGLTextureMatrix(FTextureMatrix);
    Material.Apply(ARci);
  end
  else
  begin
    // multitexturing is ON
    if not FTextureMatrixIsIdentity then
      ARci.GLStates.SetGLTextureMatrix(FTextureMatrix);
    Material.Apply(ARci);

    if not libMatTexture2.FTextureMatrixIsIdentity then
      libMatTexture2.Material.Texture.ApplyAsTexture2(ARci,
        @libMatTexture2.FTextureMatrix.V[0].X)
    else
      libMatTexture2.Material.Texture.ApplyAsTexture2(ARci);

    if (not Material.Texture.Disabled) and (Material.Texture.MappingMode =
      tmmUser) then
      if libMatTexture2.Material.Texture.MappingMode = tmmUser then
        xgl.MapTexCoordToDual
      else
        xgl.MapTexCoordToMain
    else if libMatTexture2.Material.Texture.MappingMode = tmmUser then
      xgl.MapTexCoordToSecond
    else
      xgl.MapTexCoordToMain;

  end;
 
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssLowLevel: Shader.Apply(ARci, Self);
    end;
  end;
  xgl.EndUpdate;
end;

function TGLLibMaterial.UnApply(var ARci: TGLRenderContextInfo): Boolean;
begin
  Result := False;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssLowLevel: Result := Shader.UnApply(ARci);
      ssReplace:
        begin
          Result := Shader.UnApply(ARci);
          Exit;
        end;
    end;
  end;

  if not Result then
  begin
    if Assigned(libMatTexture2) and GL.ARB_multitexture and (not
      xgl.SecondTextureUnitForbidden) then
    begin
      libMatTexture2.Material.Texture.UnApplyAsTexture2(ARci, (not
        libMatTexture2.TextureMatrixIsIdentity));
      xgl.MapTexCoordToMain;
    end;
    Material.UnApply(ARci);
    if not Material.Texture.Disabled then
      if not FTextureMatrixIsIdentity then
        ARci.GLStates.ResetGLTextureMatrix;
    if Assigned(FShader) then
    begin
      case Shader.ShaderStyle of
        ssHighLevel: Result := Shader.UnApply(ARci);
      end;
    end;
  end;
end;

procedure TGLLibMaterial.NotifyTexMapChange(Sender: TObject);
begin
  NotifyUsersOfTexMapChange();
end;

procedure TGLLibMaterial.NotifyUsersOfTexMapChange;
var
  i: Integer;
  obj: TObject;
begin
  if FNotifying then
    Exit;
  FNotifying := True;
  try
    for i := 0 to FUserList.Count - 1 do
    begin
      obj := TObject(FUserList[i]);
      if obj is TGLMaterial then
        TGLMaterial(FUserList[i]).NotifyTexMapChange(Self)
      else if obj is TGLLibMaterial then
        TGLLibMaterial(FUserList[i]).NotifyUsersOfTexMapChange
      else if obj is TGLUpdateAbleObject then
        TGLUpdateAbleObject(FUserList[i]).NotifyChange(Self)
      else if obj is TGLUpdateAbleComponent then
        TGLUpdateAbleComponent(FUserList[i]).NotifyChange(Self);
    end;
  finally
    FNotifying := False;
  end;
end;

procedure TGLLibMaterial.Loaded;
begin
  CalculateTextureMatrix;
  Material.Loaded;
end;

procedure TGLLibMaterial.SetMaterial(const val: TGLMaterial);
begin
  FMaterial.Assign(val);
end;

procedure TGLLibMaterial.SetTextureOffset(const val: TGLCoordinates);
begin
  FTextureOffset.AsVector := val.AsVector;
  CalculateTextureMatrix;
end;

procedure TGLLibMaterial.SetTextureScale(const val: TGLCoordinates);
begin
  FTextureScale.AsVector := val.AsVector;
  CalculateTextureMatrix;
end;

procedure TGLLibMaterial.SetTextureMatrix(const Value: TGLMatrix);
begin
  FTextureMatrixIsIdentity := CompareMem(@Value.V[0], @IdentityHmgMatrix.V[0], SizeOf(TGLMatrix));
  FTextureMatrix := Value;
  FTextureOverride := True;
  NotifyUsers;
end;

procedure TGLLibMaterial.SetTextureRotate(Value: Single);
begin
  if Value <> FTextureRotate then
  begin
    FTextureRotate := Value;
    CalculateTextureMatrix;
  end;
end;

function TGLLibMaterial.StoreTextureRotate: Boolean;
begin
  Result := Abs(FTextureRotate) > EPSILON;
end;

procedure TGLLibMaterial.SetTexture2Name(const val: TGLLibMaterialName);
begin
  if val <> Texture2Name then
  begin
    if Assigned(libMatTexture2) then
    begin
      libMatTexture2.UnregisterUser(Self);
      libMatTexture2 := nil;
    end;
    FTexture2Name := val;
    NotifyUsers;
  end;
end;

procedure TGLLibMaterial.SetShader(const val: TGLShader);
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

procedure TGLLibMaterial.CalculateTextureMatrix;
begin
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
        CreateRotationMatrixZ(DegToRadian(FTextureRotate)));
  end;
  FTextureOverride := False;
  NotifyUsers;
end;

procedure TGLLibMaterial.DestroyHandles;
var
  libMat: TGLLibMaterial;
begin
  FMaterial.DestroyHandles;
  if FTexture2Name <> '' then
  begin
    libMat := TGLLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
    if Assigned(libMat) then
      libMat.DestroyHandles;
  end;
end;

procedure TGLLibMaterial.OnNotifyChange(Sender: TObject);
begin
  CalculateTextureMatrix;
end;

procedure TGLLibMaterial.DoOnTextureNeeded(Sender: TObject; var textureFileName:
  string);
var
  mLib: TGLMaterialLibrary;
  i: Integer;
  tryName: string;
begin
  if not Assigned(Collection) then
    exit;
  mLib := TGLMaterialLibrary((Collection as TGLLibMaterials).GetOwner);
  with mLib do
    if Assigned(FOnTextureNeeded) then
      FOnTextureNeeded(mLib, textureFileName);
  // if a ':' is present, or if it starts with a '\', consider it as an absolute path
  if (Pos(':', textureFileName) > 0) or (Copy(textureFileName, 1, 1) = PathDelim)
    then
    Exit;
  // ok, not an absolute path, try given paths
  with mLib do
  begin
    if FTexturePathList <> nil then
      for i := 0 to FTexturePathList.Count - 1 do
      begin
        tryName := IncludeTrailingPathDelimiter(FTexturePathList[i]) +
          textureFileName;
        if (Assigned(vAFIOCreateFileStream) and FileStreamExists(tryName)) or
          FileExists(tryName) then
        begin
          textureFileName := tryName;
          Break;
        end;
      end;
  end;
end;


// ------------------
// ------------------ TGLLibMaterials ------------------
// ------------------

function TGLAbstractLibMaterials.GetMaterial(const AName: TGLLibMaterialName):
  TGLAbstractLibMaterial;
var
  i, hk: Integer;
  lm: TGLAbstractLibMaterial;
begin
  hk := TGLAbstractLibMaterial.ComputeNameHashKey(AName);
  for i := 0 to Count - 1 do
  begin
    lm := TGLAbstractLibMaterial(inherited Items[i]);
    if (lm.NameHashKey = hk) and (lm.Name = AName) then
    begin
      Result := lm;
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TGLAbstractLibMaterials.Loaded;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    TGLAbstractLibMaterial(Items[I]).Loaded;
end;

function TGLAbstractLibMaterials.MakeUniqueName(const nameRoot: TGLLibMaterialName):
  TGLLibMaterialName;
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

constructor TGLLibMaterials.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TGLLibMaterial);
end;

procedure TGLLibMaterials.SetItems(index: Integer; const val: TGLLibMaterial);
begin
  inherited Items[index] := val;
end;

function TGLLibMaterials.GetItems(index: Integer): TGLLibMaterial;
begin
  Result := TGLLibMaterial(inherited Items[index]);
end;

procedure TGLLibMaterials.DestroyHandles;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].DestroyHandles;
end;

function TGLLibMaterials.Owner: TPersistent;
begin
  Result := GetOwner;
end;

function TGLLibMaterials.Add: TGLLibMaterial;
begin
  Result := (inherited Add) as TGLLibMaterial;
end;

function TGLLibMaterials.FindItemID(ID: Integer): TGLLibMaterial;
begin
  Result := (inherited FindItemID(ID)) as TGLLibMaterial;
end;

function TGLLibMaterials.GetLibMaterialByName(const AName: TGLLibMaterialName):
  TGLLibMaterial;
var
  LMaterial: TGLAbstractLibMaterial;
begin
  LMaterial := GetMaterial(AName);
  if Assigned(LMaterial) and (LMaterial is TGLLibMaterial) then
    Result := TGLLibMaterial(LMaterial)
  else
    Result := nil;
end;

function TGLLibMaterials.GetTextureIndex(const Texture: TGLTexture): Integer;
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I).Material.Texture = Texture then
      begin
        Result := I;
        Exit;
      end;
  Result := -1;
end;

function TGLLibMaterials.GetMaterialIndex(const Material: TGLMaterial): Integer;
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I).Material = Material then
      begin
        Result := I;
        Exit;
      end;
  Result := -1;
end;

function TGLLibMaterials.GetNameOfTexture(const Texture: TGLTexture):
  TGLLibMaterialName;
var
  MatIndex: Integer;
begin
  MatIndex := GetTextureIndex(Texture);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).Name
  else
    Result := '';
end;

function TGLLibMaterials.GetNameOfLibMaterial(const Material: TGLLibMaterial):
  TGLLibMaterialName;
var
  MatIndex: Integer;
begin
  MatIndex := IndexOf(Material);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).Name
  else
    Result := '';
end;

function TGLLibMaterials.IndexOf(const Item: TGLLibMaterial): Integer;
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

procedure TGLLibMaterials.PrepareBuildList;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TGLLibMaterial(inherited Items[i]).PrepareBuildList;
end;

procedure TGLLibMaterials.DeleteUnusedMaterials;
var
  i: Integer;
  gotNone: Boolean;
begin
  BeginUpdate;
  repeat
    gotNone := True;
    for i := Count - 1 downto 0 do
    begin
      if TGLLibMaterial(inherited Items[i]).FUserList.Count = 0 then
      begin
        TGLLibMaterial(inherited Items[i]).Free;
        gotNone := False;
      end;
    end;
  until gotNone;
  EndUpdate;
end;

 
//--------------------------------------------------------
// ---------------------TGLAbstractMaterialLibrary
//--------------------------------------------------------

procedure TGLAbstractMaterialLibrary.SetTexturePaths(const val: string);
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

function TGLAbstractMaterialLibrary.ApplyMaterial(const AName: string;
  var ARci: TGLRenderContextInfo): Boolean;
begin
  FLastAppliedMaterial := FMaterials.GetMaterial(AName);
  Result := Assigned(FLastAppliedMaterial);
  if Result then
    FLastAppliedMaterial.Apply(ARci);
end;

function TGLAbstractMaterialLibrary.UnApplyMaterial(
  var ARci: TGLRenderContextInfo): Boolean;
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

procedure TGLAbstractMaterialLibrary.SetNamesToTStrings(AStrings: TStrings);
var
  i: Integer;
  lm: TGLAbstractLibMaterial;
begin
  with AStrings do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to FMaterials.Count - 1 do
    begin
      lm := TGLAbstractLibMaterial(FMaterials.Items[i]);
      AddObject(lm.Name, lm);
    end;
    EndUpdate;
  end;
end;

procedure TGLAbstractMaterialLibrary.Loaded;
begin
  inherited;
  FMaterials.Loaded;
end;

// ------------------
// ------------------ TGLMaterialLibrary ------------------
// ------------------

constructor TGLMaterialLibrary.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TGLLibMaterials.Create(Self);
end;

destructor TGLMaterialLibrary.Destroy;
begin
  Assert(FLastAppliedMaterial = nil, 'Unbalanced material application');
  FTexturePathList.Free;
  FMaterials.Free;
  FMaterials := nil;
  inherited;
end;

procedure TGLMaterialLibrary.DestroyHandles;
begin
  if Assigned(FMaterials) then
    Materials.DestroyHandles;
end;

procedure TGLMaterialLibrary.SetMaterials(const val: TGLLibMaterials);
begin
  FMaterials.Assign(val);
end;

function TGLMaterialLibrary.StoreMaterials: Boolean;
begin
  Result := (FMaterials.Count > 0);
end;

procedure TGLMaterialLibrary.WriteToFiler(writer: TGLVirtualWriter);
var
  i, j: Integer;
  libMat: TGLLibMaterial;
  tex: TGLTexture;
  img: TGLTextureImage;
  pim: TGLPersistentImage;
  ss: TStringStream;
  bmp: TBitmap;
  texExItem: TGLTextureExItem;
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
      WriteString(libMat.Name);
      tex := libMat.Material.Texture;
      img := tex.Image;
      pim := TGLPersistentImage(img);
      if tex.Enabled and (img is TGLPersistentImage) and (pim.Picture.Graphic <>
        nil) then
      begin
        WriteBoolean(true);
        ss := TStringStream.Create('');
        try
          bmp := TBitmap.Create;
          try
            bmp.Assign(pim.Picture.Graphic);
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

      //version 1
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
          WriteBoolean(TRUE);
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
          WriteBoolean(FALSE);

        WriteInteger(Integer(FaceCulling));
      end;
      // version 3 end

      WriteInteger(SizeOf(TGLMaterialOptions));
      Write(libMat.Material.MaterialOptions, SizeOf(TGLMaterialOptions));
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
        pim := TGLPersistentImage(img);
        if texExItem.Texture.Enabled and (img is TGLPersistentImage)
          and (pim.Picture.Graphic <> nil) then
        begin
          WriteBoolean(True);
          ss := TStringStream.Create('');
          try
            bmp := TBitmap.Create;
            try
              bmp.Assign(pim.Picture.Graphic);
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

procedure TGLMaterialLibrary.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion: Integer;
  libMat: TGLLibMaterial;
  i, n, size, tex, texCount: Integer;
  LName: string;
  ss: TStringStream;
  bmp: TBitmap;
  texExItem: TGLTextureExItem;
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
            bmp := TBitmap.Create;
            try
              bmp.LoadFromStream(ss);
              if libMat = nil then
                libMat := AddTextureMaterial(LName, bmp)
              else
                libMat.Material.Texture.Image.Assign(bmp);
            finally
              bmp.Free;
            end;
          finally
            ss.Free;
          end;

          // version 3
          if archiveVersion >= 3 then
            with libMat.Material.Texture do
            begin
              Read(BorderColor.AsAddress^, SizeOf(Single) * 4);
              Compression := TGLTextureCompression(ReadInteger);
              DepthTextureMode := TGLDepthTextureMode(ReadInteger);
              Read(EnvColor.AsAddress^, SizeOf(Single) * 4);
              FilteringQuality := TGLTextureFilteringQuality(ReadInteger);
              ImageAlpha := TGLTextureImageAlpha(ReadInteger);
              ImageBrightness := ReadFloat;
              ImageGamma := ReadFloat;
              MagFilter := TGLMagFilter(ReadInteger);
              MappingMode := TGLTextureMappingMode(ReadInteger);
              Read(MappingSCoordinates.AsAddress^, SizeOf(Single) * 4);
              Read(MappingTCoordinates.AsAddress^, SizeOf(Single) * 4);
              Read(MappingRCoordinates.AsAddress^, SizeOf(Single) * 4);
              Read(MappingQCoordinates.AsAddress^, SizeOf(Single) * 4);
              MinFilter := TGLMinFilter(ReadInteger);
              NormalMapScale := ReadFloat;
              TextureCompareFunc := TGLDepthCompareFunc(ReadInteger);
              TextureCompareMode := TGLTextureCompareMode(ReadInteger);
              TextureFormat := TGLTextureFormat(ReadInteger);
              TextureMode := TGLTextureMode(ReadInteger);
              TextureWrap := TGLTextureWrap(ReadInteger);
              TextureWrapR := TGLSeparateTextureWrap(ReadInteger);
              TextureWrapS := TGLSeparateTextureWrap(ReadInteger);
              TextureWrapT := TGLSeparateTextureWrap(ReadInteger);
            end;
          // version 3 end

        end
        else
        begin
          if libMat = nil then
          begin
            libMat := Materials.Add;
            libMat.Name := LName;
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
            libMat.Material.PolygonMode := TGLPolygonMode(ReadInteger);
          end;
          with libMat.Material.BackProperties do
          begin
            Read(Ambient.AsAddress^, SizeOf(Single) * 3);
            Read(Diffuse.AsAddress^, SizeOf(Single) * 4);
            Read(Emission.AsAddress^, SizeOf(Single) * 3);
            Read(Specular.AsAddress^, SizeOf(Single) * 3);
            Read(FShininess, 1);
            {PolygonMode := TPolygonMode(} ReadInteger;
          end;
          libMat.Material.BlendingMode := TGLBlendingMode(ReadInteger);

          // version 3
          if archiveVersion >= 3 then
          begin
            if ReadBoolean then
              with libMat.Material.BlendingParams do
              begin
                AlphaFuncRef := ReadFloat;
                AlphaFunctType := TGlAlphaFunc(ReadInteger);
                BlendFuncDFactor := TGLBlendFunction(ReadInteger);
                BlendFuncSFactor := TGLBlendFunction(ReadInteger);
                UseAlphaFunc := ReadBoolean;
                UseBlendFunc := ReadBoolean;
              end;

            libMat.Material.FaceCulling := TGLFaceCulling(ReadInteger);
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
              bmp := TBitmap.Create;
              try
                bmp.LoadFromStream(ss);
                texExItem.Texture.Image.Assign(bmp);
                texExItem.Texture.Enabled := True;
              finally
                bmp.Free;
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

procedure TGLMaterialLibrary.SaveToStream(aStream: TStream);
var
  wr: TGLBinaryWriter;
begin
  wr := TGLBinaryWriter.Create(aStream);
  try
    Self.WriteToFiler(wr);
  finally
    wr.Free;
  end;
end;

procedure TGLMaterialLibrary.LoadFromStream(aStream: TStream);
var
  rd: TGLBinaryReader;
begin
  rd := TGLBinaryReader.Create(aStream);
  try
    Self.ReadFromFiler(rd);
  finally
    rd.Free;
  end;
end;

procedure TGLMaterialLibrary.AddMaterialsFromStream(aStream: TStream);
begin
  FDoNotClearMaterialsOnLoad := True;
  try
    LoadFromStream(aStream);
  finally
    FDoNotClearMaterialsOnLoad := False;
  end;
end;

procedure TGLMaterialLibrary.SaveToFile(const fileName: string);
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

procedure TGLMaterialLibrary.LoadFromFile(const fileName: string);
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

procedure TGLMaterialLibrary.AddMaterialsFromFile(const fileName: string);
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

function TGLMaterialLibrary.AddTextureMaterial(const materialName, fileName:
  string;
  persistent: Boolean = True): TGLLibMaterial;
begin
  Result := Materials.Add;
  with Result do
  begin
    Name := materialName;
    with Material.Texture do
    begin
      MinFilter := miLinearMipmapLinear;
      MagFilter := maLinear;
      TextureMode := tmModulate;
      Disabled := False;
      if persistent then
      begin
        ImageClassName := TGLPersistentImage.ClassName;
        if fileName <> '' then
          Image.LoadFromFile(fileName);
      end
      else
      begin
        ImageClassName := TGLPicFileImage.ClassName;
        TGLPicFileImage(Image).PictureFileName := fileName;
      end;
    end;
  end;
end;

function TGLMaterialLibrary.AddTextureMaterial(const materialName: string;
  graphic: TGraphic): TGLLibMaterial;
begin
  Result := Materials.Add;
  with Result do
  begin
    Name := materialName;
    with Material.Texture do
    begin
      MinFilter := miLinearMipmapLinear;
      MagFilter := maLinear;
      TextureMode := tmModulate;
      Disabled := False;
      Image.Assign(graphic);
    end;
  end;
end;

function TGLMaterialLibrary.LibMaterialByName(const AName: TGLLibMaterialName):
  TGLLibMaterial;
begin
  if Assigned(Self) then
    Result := Materials.GetLibMaterialByName(AName)
  else
    Result := nil;
end;

function TGLMaterialLibrary.TextureByName(const LibMatName: TGLLibMaterialName):
  TGLTexture;
var
  LibMat: TGLLibMaterial;
begin
  if Self = nil then
    raise ETexture.Create(strErrorEx + strMatLibNotDefined)
  else if LibMatName = '' then
    Result := nil
  else
  begin
    LibMat := LibMaterialByName(LibMatName);
    if LibMat = nil then
      raise ETexture.CreateFmt(strErrorEx + strMaterialNotFoundInMatlibEx,
        [LibMatName])
    else
      Result := LibMat.Material.Texture;
  end;
end;

function TGLMaterialLibrary.GetNameOfTexture(const Texture: TGLTexture):
  TGLLibMaterialName;
begin
  if (Self = nil) or (Texture = nil) then
    Result := ''
  else
    Result := Materials.GetNameOfTexture(Texture);
end;

function TGLMaterialLibrary.GetMaterials: TGLLibMaterials;
begin
  Result := TGLLibMaterials(FMaterials);
end;

function TGLMaterialLibrary.GetNameOfLibMaterial(const LibMat: TGLLibMaterial):
  TGLLibMaterialName;
begin
  if (Self = nil) or (LibMat = nil) then
    Result := ''
  else
    Result := Materials.GetNameOfLibMaterial(LibMat);
end;

 
procedure TGLBlendingParameters.Apply(var rci: TGLRenderContextInfo);
begin
  if FUseAlphaFunc then
  begin
    rci.GLStates.Enable(stAlphaTest);
    rci.GLStates.SetGLAlphaFunction(FAlphaFuncType, FAlphaFuncRef);
  end
  else
    rci.GLStates.Disable(stAlphaTest);
  if FUseBlendFunc then
  begin
    rci.GLStates.Enable(stBlend);
    if FSeparateBlendFunc then
      rci.GLStates.SetBlendFuncSeparate(FBlendFuncSFactor, FBlendFuncDFactor,
        FAlphaBlendFuncSFactor, FAlphaBlendFuncDFactor)
    else
      rci.GLStates.SetBlendFunc(FBlendFuncSFactor, FBlendFuncDFactor);
  end
  else
    rci.GLStates.Disable(stBlend);
end;

constructor TGLBlendingParameters.Create(AOwner: TPersistent);
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

procedure TGLBlendingParameters.SetAlphaFuncRef(const Value: Single);
begin
  if (FAlphaFuncRef <> Value) then
  begin
    FAlphaFuncRef := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLBlendingParameters.SetAlphaFuncType(
  const Value: TGlAlphaFunc);
begin
  if (FAlphaFuncType <> Value) then
  begin
    FAlphaFuncType := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLBlendingParameters.SetBlendFuncDFactor(
  const Value: TGLBlendFunction);
begin
  if (FBlendFuncDFactor <> Value) then
  begin
    FBlendFuncDFactor := Value;
    if not FSeparateBlendFunc then
      FAlphaBlendFuncDFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLBlendingParameters.SetBlendFuncSFactor(
  const Value: TGLBlendFunction);
begin
  if (FBlendFuncSFactor <> Value) then
  begin
    FBlendFuncSFactor := Value;
    if not FSeparateBlendFunc then
      FAlphaBlendFuncSFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLBlendingParameters.SetAlphaBlendFuncDFactor(const Value: TGLBlendFunction);
begin
  if FSeparateBlendFunc and (FAlphaBlendFuncDFactor <> Value) then
  begin
    FAlphaBlendFuncDFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLBlendingParameters.SetAlphaBlendFuncSFactor(const Value: TGLBlendFunction);
begin
  if FSeparateBlendFunc and (FAlphaBlendFuncSFactor <> Value) then
  begin
    FAlphaBlendFuncSFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLBlendingParameters.SetUseAlphaFunc(const Value: Boolean);
begin
  if (FUseAlphaFunc <> Value) then
  begin
    FUseAlphaFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLBlendingParameters.SetUseBlendFunc(const Value: Boolean);
begin
  if (FUseBlendFunc <> Value) then
  begin
    FUseBlendFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLBlendingParameters.SetSeparateBlendFunc(const Value: Boolean);
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

function TGLBlendingParameters.StoreAlphaFuncRef: Boolean;
begin
  Result := (Abs(AlphaFuncRef) > 0.001);
end;

//-----------------------------------------------
initialization
//-----------------------------------------------

  RegisterClasses([TGLMaterialLibrary, TGLMaterial, TGLShader]);

end.