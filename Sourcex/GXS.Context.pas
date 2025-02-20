//
// The graphics engine GLXEngine. The unit of GXScene for Delphi
//
unit GXS.Context;

(*
  Prototypes and base implementation of TgxContext.
  The history is logged in a former version of the unit.
*)

interface

{$I Stage.Defines.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.SyncObjs,
  System.StrUtils,
  FMX.Consts,
  FMX.Forms,
  FMX.Controls,
  FMX.Types,
  FMX.Dialogs,

  Stage.VectorTypes,
  Stage.VectorGeometry,
  Stage.Strings,
  Stage.PipelineTransform,
  Stage.TextureFormat,

 // GXS.OpenGLx,
  GXS.State;

// Buffer ID's for Multiple-Render-Targets (using GL_ATI_draw_buffers)
const
  MRT_BUFFERS: array [0 .. 3] of Cardinal = (GL_FRONT_LEFT, GL_AUX0, GL_AUX1, GL_AUX2);

type
  TgxRCOption = (rcoDoubleBuffered, rcoStereo, rcoDebug, rcoOGL_ES);
  TgxRCOptions = set of TgxRCOption;

  TgxContextLayer = (clUnderlay2, clUnderlay1, clMainPlane, clOverlay1, clOverlay2);

  TgxFinishTaskEvent = class(TEvent)
  public
    constructor Create; reintroduce;
  end;

  TgxTaskProcedure = procedure of object; stdcall;

  TgxServiceContextTask = record
    Task: TgxTaskProcedure;
    Event: TgxFinishTaskEvent;
  end;

  TgxContext = class;
  TgxContextManager = class;

  TgxAbstractMultitextureCoordinator = class(TObject)
  protected
    FOwner: TgxContext;
  public
    constructor Create(AOwner: TgxContext); virtual;
  end;

  TgxAbstractMultitextureCoordinatorClass = class of TgxAbstractMultitextureCoordinator;

  TgxContextAcceleration = (chaUnknown, chaHardware, chaSoftware);

  TgxAntiAliasing = (
    // Multisample Antialiasing
    aaDefault, aaNone, aa2x, aa2xHQ, aa4x, aa4xHQ, aa6x, aa8x, aa16x,
    // Coverage Sampling Antialiasing
    csa8x, csa8xHQ, csa16x, csa16xHQ);

  TgxSyncMode = (vsmSync, vsmNoSync);

  (* Wrapper around an OpenGL rendering context.
    The aim of this class is to offer platform-independant
    initialization, activation and management of OpenGL
    rendering context. The class also offers notifications
    event and error/problems detection.
    This is a virtual abstract a class, and platform-specific
    subclasses must be used. All rendering context share the same lists. *)
  TgxContext = class
  private
    FColorBits, FAlphaBits: Integer;
    FDepthBits: Integer;
    FStencilBits: Integer;
    FAccumBits: Integer;
    FAuxBuffers: Integer;
    FAntiAliasing: TgxAntiAliasing;
    FOptions: TgxRCOptions;
    FOnDestroyContext: TNotifyEvent;
    FManager: TgxContextManager;
    FActivationCount: Integer;
    FOwnedHandlesCount: Integer;
    FIsPraparationNeed: Boolean;
    procedure SetColorBits(const aColorBits: Integer); inline;
    procedure SetAlphaBits(const aAlphaBits: Integer); inline;
    procedure SetDepthBits(const val: Integer); inline;
    procedure SetStencilBits(const aStencilBits: Integer); inline;
    procedure SetAccumBits(const aAccumBits: Integer); inline;
    procedure SetAuxBuffers(const aAuxBuffers: Integer); inline;
    procedure SetOptions(const aOptions: TgxRCOptions); inline;
    procedure SetAntiAliasing(const val: TgxAntiAliasing); inline;
    procedure SetAcceleration(const val: TgxContextAcceleration); inline;
    function GetActive: Boolean; inline;
    procedure SetActive(const aActive: Boolean); inline;
    procedure SetLayer(const Value: TgxContextLayer); inline;
  protected
    FGXS: TgxAbstractMultitextureCoordinator;
    FgxStates: TgxStateCache;
    FTransformation: TgTransformation;
    FAcceleration: TgxContextAcceleration;
    FLayer: TgxContextLayer;
{$IFDEF USE_MULTITHREAD}
    FSharedContexts: TThreadList;
    FLock: TCriticalSection;
{$ELSE}
    FSharedContexts: TList;
    FOwnedHandles : TList;
{$ENDIF}
    procedure PropagateSharedContext;
    procedure DoCreateContext(ADeviceHandle: THandle); virtual; abstract; // VCL -> HDC
    procedure DoCreateMemoryContext(OutputDevice: THandle; Width, Height: // VCL ->HWND
      Integer; BufferCount: Integer = 1); virtual; abstract;
    function DoShareLists(aContext: TgxContext): Boolean; virtual; abstract;
    procedure DoDestroyContext; virtual; abstract;
    procedure DoActivate; virtual; abstract;
    procedure DoDeactivate; virtual; abstract;
    class function ServiceContext: TgxContext;
    procedure MakeGLCurrent;
    function GetGXS: TgxAbstractMultitextureCoordinator;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // An application-side cache of global per-context OpenGL states and parameters
    property GXStates: TgxStateCache read FgxStates;
    property PipelineTransformation: TgTransformation read FTransformation;
    // Context manager reference
    property Manager: TgxContextManager read FManager;
    // Color bits for the rendering context
    property ColorBits: Integer read FColorBits write SetColorBits;
    // Alpha bits for the rendering context
    property AlphaBits: Integer read FAlphaBits write SetAlphaBits;
    // Depth bits for the rendering context
    property DepthBits: Integer read FDepthBits write SetDepthBits;
    // Stencil bits for the rendering context
    property StencilBits: Integer read FStencilBits write SetStencilBits;
    // Accumulation buffer bits for the rendering context
    property AccumBits: Integer read FAccumBits write SetAccumBits;
    // Auxiliary buffers bits for the rendering context
    property AuxBuffers: Integer read FAuxBuffers write SetAuxBuffers;
    // AntiAliasing option.  Ignored if not hardware supported, currently based on ARB_multisample.
    property AntiAliasing: TgxAntiAliasing read FAntiAliasing write SetAntiAliasing;
    // Specifies the layer plane that the rendering context is bound to.
    property Layer: TgxContextLayer read FLayer write SetLayer;
    // Rendering context options.
    property Options: TgxRCOptions read FOptions write SetOptions;
    (* Allows reading and defining the activity for the context.
      The methods of this property are just wrappers around calls to Activate and Deactivate. *)
    property Active: Boolean read GetActive write SetActive;
    // Indicates if the context is hardware-accelerated.
    property Acceleration: TgxContextAcceleration read FAcceleration write SetAcceleration;
    (* Triggered whenever the context is destroyed.
      This events happens *before* the context has been
      actually destroyed, OpenGL resource cleanup can still occur here. *)
    property OnDestroyContext: TNotifyEvent read FOnDestroyContext write FOnDestroyContext;
    (* Creates the context.
      This method must be invoked before the context can be used. *)
    procedure CreateContext(ADeviceHandle: THandle); overload; // VCL -> HDC
    (* Creates an in-memory context.
      The function should fail if no hardware-accelerated memory context
      can be created (the CreateContext method can handle software OpenGL
      contexts). *)
    procedure CreateMemoryContext(OutputDevice: THandle; Width, Height: // VCL -> HWND
      Integer; BufferCount: Integer = 1);
    (* Setup display list sharing between two rendering contexts.
      Both contexts must have the same pixel format. *)
    procedure ShareLists(aContext: TgxContext);
    (* Destroy the context.
      Will fail if no context has been created.
      The method will first invoke the OnDestroyContext
      event, then attempts to deactivate the context
      (if it is active) before destroying it. *)
    procedure DestroyContext;
    (* Activates the context.
      A context can be activated multiple times (and must be
      deactivated the same number of times), but this function
      will fail if another context is already active. *)
    procedure Activate;
    (* Deactivates the context.
      Will fail if the context is not active or another
      context has been activated. *)
    procedure Deactivate;
    // Call OnPrepare for all handles.
    procedure PrepareHandlesData;
    (* Returns true if the context is valid.
      A context is valid from the time it has been successfully
      created to the time of its destruction. *)
    function IsValid: Boolean; virtual; abstract;
    // Request to swap front and back buffers if they were defined.
    procedure SwapBuffers; virtual; abstract;
    // Returns the first compatible context that isn't self in the shares.
    function FindCompatibleContext: TgxContext;
    procedure DestroyAllHandles;
    function RenderOutputDevice: Pointer; virtual; abstract;
    // Access to OpenGL command and extension.
    /// property GL: TGLExtensionsAndEntryPoints read FGL; depricated from OpenGLAdapter
    property MultitextureCoordinator: TgxAbstractMultitextureCoordinator read GetGXS;
    property IsPraparationNeed: Boolean read FIsPraparationNeed;
  end;

  TgxContextClass = class of TgxContext;

  (* Context with screen control property and methods.
    This variety of contexts is for drivers that access windows and OpenGL
    through an intermediate opaque cross-platform API.
    TgxSceneViewer won't use them, TgxMemoryViewer may be able to use them,
    but most of the time they will be accessed through a specific viewer
    class/subclass. *)
  TgxScreenControlingContext = class(TgxContext)
  strict private
    FWidth, FHeight: Integer;
    FFullScreen: Boolean;
  protected
  public
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property FullScreen: Boolean read FFullScreen write FFullScreen;
  end;

  PgxRCHandle = ^TgxRCHandle;

  TgxRCHandle = record
    FRenderingContext: TgxContext;
    FHandle: Cardinal;
    FChanged: Boolean;
  end;

  TOnPrepareHandleData = procedure(aContext: TgxContext) of object;

  (* Wrapper around an OpenGL context handle.
    This wrapper also takes care of context registrations and data releases
    related to context releases an cleanups. This is an abstract class,
    use the TgxListHandle and TgxTextureHandle subclasses. *)
  TgxContextHandle = class
  private
    FHandles: TList;
    FLastHandle: PgxRCHandle;
    FOnPrepare: TOnPrepareHandleData;
    function GetHandle: Cardinal; inline;
    function GetContext: TgxContext;
    function SearchRC(aContext: TgxContext): PgxRCHandle;
    function RCItem(AIndex: Integer): PgxRCHandle; inline;
    procedure CheckCurrentRC;
  protected
    // Invoked by when there is no compatible context left for relocation
    procedure ContextDestroying;
    // Specifies if the handle can be transfered across shared contexts
    class function Transferable: Boolean; virtual;
    class function IsValid(const ID: LongWord): BYTEBOOL; virtual;
    function DoAllocateHandle: LongWord; virtual; abstract;
    procedure DoDestroyHandle(var AHandle: LongWord); virtual; abstract;
  public
    constructor Create; virtual;
    constructor CreateAndAllocate(failIfAllocationFailed: Boolean = True);
    destructor Destroy; override;
    // Return OpenGL identifier in current context.
    property Handle: LongWord read GetHandle;
    (* Return current rendering context if handle is allocated in it
      or first context where handle is allocated. *)
    property RenderingContext: TgxContext read GetContext;
    // Return True is data need update in current context.
    function IsDataNeedUpdate: Boolean; inline;
    // Return True if data updated in all contexts.
    function IsDataComplitelyUpdated: Boolean;
    // Notify the data was updated in current context.
    procedure NotifyDataUpdated;
    // Notify the data was changed through all context.
    procedure NotifyChangesOfData;
    // Checks if required extensions / OpenGL version are met
    class function IsSupported: Boolean; virtual;
    function IsAllocatedForContext(aContext: TgxContext = nil): Boolean;
    function IsShared: Boolean;
    function AllocateHandle: Cardinal;
    procedure DestroyHandle;
    property OnPrapare: TOnPrepareHandleData read FOnPrepare write FOnPrepare;
  end;

  TgxVirtualHandle = class;
  TgxVirtualHandleEvent = procedure(Sender: TgxVirtualHandle; var Handle: LongWord) of object;

  // A context handle with event-based handle allocation and destruction.
  TgxVirtualHandle = class(TgxContextHandle)
  private
    FOnAllocate, FOnDestroy: TgxVirtualHandleEvent;
    FTag: Integer;
  protected
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: Cardinal); override;
    class function Transferable: Boolean; override;
  public
    property OnAllocate: TgxVirtualHandleEvent read FOnAllocate write FOnAllocate;
    property OnDestroy: TgxVirtualHandleEvent read FOnDestroy write FOnDestroy;
    property Tag: Integer read FTag write FTag;
  end;

  // Transferable virtual handle.
  TgxVirtualHandleTransf = class(TgxVirtualHandle)
  protected
    class function Transferable: Boolean; override;
  end;

  // Manages a handle to a display list.
  TgxListHandle = class(TgxContextHandle)
  protected
    function DoAllocateHandle: LongWord; override;
    procedure DoDestroyHandle(var AHandle: LongWord); override;
    class function IsValid(const ID: LongWord): BYTEBOOL; override;
  public
    procedure NewList(mode: Cardinal);
    procedure EndList;
    procedure CallList;
  end;

  // Manages a handle to a texture.
  TgxTextureHandle = class(TgxContextHandle)
  private
    FTarget: TglTextureTarget;
    procedure SetTarget(ATarget: TglTextureTarget);
  protected
    function DoAllocateHandle: LongWord; override;
    procedure DoDestroyHandle(var AHandle: LongWord); override;
    class function IsValid(const ID: LongWord): BYTEBOOL; override;
  public
    property Target: TglTextureTarget read FTarget write SetTarget;
  end;

  // Manages a handle to a sampler.
  TgxSamplerHandle = class(TgxContextHandle)
  protected
    function DoAllocateHandle: LongWord; override;
    procedure DoDestroyHandle(var AHandle: LongWord); override;
    class function IsValid(const ID: LongWord): BYTEBOOL; override;
  public
    class function IsSupported: Boolean; override;
  end;

  (* Manages a handle to a query.
    Do not use this class directly, use one of its subclasses instead. *)
  TgxQueryHandle = class(TgxContextHandle)
  private
    FActive: Boolean;
  protected
    class function Transferable: Boolean; override;
    function DoAllocateHandle: LongWord; override;
    procedure DoDestroyHandle(var AHandle: LongWord); override;
    function GetTarget: LongWord; virtual; abstract;
    function GetQueryType: TgxQueryType; virtual; abstract;
    class function IsValid(const ID: LongWord): BYTEBOOL; override;
  public
    procedure BeginQuery;
    procedure EndQuery;
    // Check if result is available from the query.  Result may not be available
    // immediately after ending the query
    function IsResultAvailable: Boolean;
    // Number of bits used to store the query result. eg. 32/64 bit
    function CounterBits: Integer;
    // Retrieve query result, may cause a stall if the result is not available yet
    function QueryResultInt: GLint;
    function QueryResultUInt: LongWord;
    function QueryResultInt64: GLint64EXT;
    function QueryResultUInt64: GLUint64EXT;
    function QueryResultBool: BYTEBOOL;
    property Target: LongWord read GetTarget;
    property QueryType: TgxQueryType read GetQueryType;
    // True if within a Begin/EndQuery.
    property Active: Boolean read FActive;
  end;

  (* Manages a handle to an occlusion query.
    Requires OpenGL 1.5+
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user. *)
  TgxOcclusionQueryHandle = class(TgxQueryHandle)
  protected
    function GetTarget: LongWord; override;
    function GetQueryType: TgxQueryType; override;
  public
    class function IsSupported: Boolean; override;
    (* Number of samples (pixels) drawn during the query, some pixels may
       be drawn to several times in the same query *)
    function PixelCount: Integer;
  end;

  TgxBooleanOcclusionQueryHandle = class(TgxQueryHandle)
  protected
    function GetTarget: LongWord; override;
    function GetQueryType: TgxQueryType; override;
  public
    class function IsSupported: Boolean; override;
  end;

  (* Manages a handle to a timer query.
    Requires GL_EXT_timer_query extension.
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user. *)
  TgxTimerQueryHandle = class(TgxQueryHandle)
  protected
    function GetTarget: LongWord; override;
    function GetQueryType: TgxQueryType; override;
  public
    class function IsSupported: Boolean; override;
    (* Time, in nanoseconds (1 ns = 10^-9 s) between starting + ending the query.
       with 32 bit integer can measure up to approximately 4 seconds, use
       QueryResultUInt64 if you may need longer *)
    function Time: Integer;
  end;

  (* Manages a handle to a primitive query.
    Requires OpenGL 3.0+
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user. *)
  TgxPrimitiveQueryHandle = class(TgxQueryHandle)
  protected
    function GetTarget: LongWord; override;
    function GetQueryType: TgxQueryType; override;
  public
    class function IsSupported: Boolean; override;
    (* Number of primitives (eg. Points, Triangles etc.) drawn whilst the
       query was active *)
    function PrimitivesGenerated: Integer;
  end;

  (* Manages a handle to a Buffer Object.
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user. *)
  TgxBufferObjectHandle = class(TgxContextHandle)
  private
    FSize: Integer;
  protected
    function DoAllocateHandle: LongWord; override;
    procedure DoDestroyHandle(var AHandle: LongWord); override;
    function GetTarget: LongWord; virtual; abstract;
    class function IsValid(const ID: LongWord): BYTEBOOL; override;
  public
    // Creates the buffer object buffer and initializes it.
    constructor CreateFromData(p: Pointer; size: Integer; bufferUsage: LongWord);
    procedure Bind; virtual; abstract;
    // Note that it is not necessary to UnBind before Binding another buffer.
    procedure UnBind; virtual; abstract;
    (* Bind a buffer object to an indexed target, used by transform feedback
      buffer objects and uniform buffer objects. (OpenGL 3.0+) *)
    procedure BindRange(index: LongWord; offset: PGLint; size: PGLsizei); virtual;
    // Equivalent to calling BindRange with offset = 0, and size = the size of buffer.
    procedure BindBase(index: LongWord); virtual;
    procedure UnBindBase(index: LongWord); virtual;
    (* Specifies buffer content.
      Common bufferUsage values are GL_STATIC_DRAW_ARB for data that will
      change rarely, but be used often, GL_STREAM_DRAW_ARB for data specified
      once but used only a few times, and GL_DYNAMIC_DRAW_ARB for data
      that is re-specified very often.
      Valid only if the buffer has been bound. *)
    procedure BufferData(p: Pointer; size: Integer; bufferUsage: LongWord);
    // Invokes Bind then BufferData
    procedure BindBufferData(p: Pointer; size: Integer; bufferUsage: LongWord);
    (* Updates part of an already existing buffer.
      offset and size indicate which part of the data in the buffer is
      to bo modified and p where the data should be taken from. *)
    procedure BufferSubData(offset, size: Integer; p: Pointer);
    (* Map buffer content to memory.
      Values for access are GL_READ_ONLY_ARB, GL_WRITE_ONLY_ARB and
      GL_READ_WRITE_ARB.
      Valid only if the buffer has been bound, must be followed by
      an UnmapBuffer, only one buffer may be mapped at a time. *)
    function MapBuffer(access: LongWord): Pointer;
    function MapBufferRange(offset: GLint; len: GLsizei; access: GLbitfield): Pointer;
    procedure Flush(offset: GLint; len: GLsizei);
    (* Unmap buffer content from memory.
      Must follow a MapBuffer, and happen before the buffer is unbound. *)
    function UnmapBuffer: BYTEBOOL;
    class function IsSupported: Boolean; override;
    property Target: LongWord read GetTarget;
    property BufferSize: Integer read FSize;
  end;

  (* Manages a handle to an Vertex Buffer Object.
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user.
    Do not use this class directly, use one of its subclasses instead. *)
  TgxVBOHandle = class(TgxBufferObjectHandle)
  private
    function GetVBOTarget: LongWord;
  public
    property VBOTarget: LongWord read GetVBOTarget;
  end;

  (* Manages a handle to VBO Array Buffer.
    Typically used to store vertices, normals, texcoords, etc. *)
  TgxVBOArrayBufferHandle = class(TgxVBOHandle)
  protected
    function GetTarget: GLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
  end;

  (* Manages a handle to VBO Element Array Buffer.
    Typically used to store vertex indices. *)
  TgxVBOElementArrayHandle = class(TgxVBOHandle)
  protected
    function GetTarget: GLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
  end;

  (* Manages a handle to PBO Pixel Pack Buffer.
    When bound, commands such as ReadPixels write
    their data into a buffer object. *)
  TgxPackPBOHandle = class(TgxBufferObjectHandle)
  protected
    function GetTarget: GLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    class function IsSupported: Boolean; override;
  end;

  (* Manages a handle to PBO Pixel Unpack Buffer.
    When bound, commands such as DrawPixels read 
	their data from a buffer object *)
  TgxUnpackPBOHandle = class(TgxBufferObjectHandle)
  protected
    function GetTarget: GLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    class function IsSupported: Boolean; override;
  end;

  (* Manages a handle to a Transform Feedback Buffer Object.
    Transform feedback buffers can be used to capture vertex data from the
    vertex or geometry shader stage to perform further processing without
    going on to the fragment shader stage. *)
  TgxTransformFeedbackBufferHandle = class(TgxBufferObjectHandle)
    /// FTransformFeedbackBufferBuffer: array[0..15] of LongWord; // (0, 0, 0, ...)
    /// FTransformFeedbackBufferStart: array[0..15] of LongWord64; // (0, 0, 0, ...)
    /// FTransformFeedbackBufferSize: array[0..15] of LongWord64; // (0, 0, 0, ...)
  protected
    function GetTarget: LongWord; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    procedure BeginTransformFeedback(primitiveMode: GLenum);
    procedure EndTransformFeedback();
    procedure BindRange(index: LongWord; offset: PGLint; size: PGLsizei); override;
    procedure BindBase(index: LongWord); override;
    procedure UnBindBase(index: LongWord); override;
    class function IsSupported: Boolean; override;
  end;

  // Manages a handle to a Buffer Texture. (TBO)
  TgxTextureBufferHandle = class(TgxBufferObjectHandle)
  protected
    function GetTarget: LongWord; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    class function IsSupported: Boolean; override;
  end;

  (* Manages a handle to a Uniform Buffer Object (UBO).
    Uniform buffer objects store "uniform blocks"; groups of uniforms
    that can be passed as a group into a GLSL program. *)
  TgxUniformBufferHandle = class(TgxBufferObjectHandle)
    /// FUniformBufferBuffer: array[0..15] of LongWord; // (0, 0, 0, ...)
    /// FUniformBufferStart: array[0..15] of LongWord64; // (0, 0, 0, ...)
    /// FUniformBufferSize: array[0..15] of LongWord64; // (0, 0, 0, ...)
  protected
    function GetTarget: LongWord; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    procedure BindRange(index: LongWord; offset: PGLint; size: PGLsizei); override;
    procedure BindBase(index: GLuint); override;
    procedure UnBindBase(index: GLuint); override;
    class function IsSupported: Boolean; override;
  end;

  (* Manages a handle to a Vertex Array Object (VAO).
    Vertex array objects are used to rapidly switch between large sets
    of array state. *)
  TgxVertexArrayHandle = class(TgxContextHandle)
  protected
    class function Transferable: Boolean; override;
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function IsValid(const ID: GLuint): BYTEBOOL; override;
  public
    procedure Bind;
    procedure UnBind;
    class function IsSupported: Boolean; override;
  end;

  TgxFramebufferStatus = (fsComplete, fsIncompleteAttachment, fsIncompleteMissingAttachment, fsIncompleteDuplicateAttachment,
    fsIncompleteDimensions, fsIncompleteFormats, fsIncompleteDrawBuffer, fsIncompleteReadBuffer, fsUnsupported,
    fsIncompleteMultisample, fsStatusError);

  (* Manages a handle to a Framebuffer Object (FBO).
    Framebuffer objects provide a way of drawing to rendering
    destinations other than the buffers provided to the GL by the
    window-system.  One or more "framebuffer-attachable images" can be attached
    to a Framebuffer for uses such as: offscreen rendering, "render to texture" +
    "multiple render targets" (MRT).
    There are several types of framebuffer-attachable images:
    - The image of a renderbuffer object, which is always 2D.
    - A single level of a 1D texture, which is treated as a 2D image with a height of one.
    - A single level of a 2D or rectangle texture.
    - A single face of a cube map texture level, which is treated as a 2D image.
    - A single layer of a 1D or 2D array texture or 3D texture, which is treated as a 2D image.
    Additionally, an entire level of a 3D texture, cube map texture,
    or 1D or 2D array texture can be attached to an attachment point.
    Such attachments are treated as an array of 2D images, arranged in
    layers, and the corresponding attachment point is considered to be layered. *)
  TgxFramebufferHandle = class(TgxContextHandle)
  protected
    class function Transferable: Boolean; override;
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function IsValid(const ID: GLuint): BYTEBOOL; override;
  public
    // Bind framebuffer for both drawing + reading
    procedure Bind;
    // Bind framebuffer for drawing
    procedure BindForDrawing;
    // Bind framebuffer for reading
    procedure BindForReading;
    // Note that it is not necessary to unbind before binding another framebuffer.
    procedure UnBind;
    procedure UnBindForDrawing;
    procedure UnBindForReading;
    // target = GL_DRAW_FRAMEBUFFER, GL_READ_FRAMEBUFFER, GL_FRAMEBUFFER (attach to both READ + DRAW)
    // attachment = COLOR_ATTACHMENTi, DEPTH_ATTACHMENT, STENCIL_ATTACHMENT, DEPTH_STENCIL_ATTACHMENT
    procedure Attach1DTexture(Target: GLenum; attachment: GLenum; textarget: GLenum; texture: LongWord; level: GLint);
    procedure Attach2DTexture(Target: GLenum; attachment: GLenum; textarget: GLenum; texture: LongWord; level: GLint);
    procedure Attach3DTexture(Target: GLenum; attachment: GLenum; textarget: GLenum; texture: LongWord; level: GLint; Layer: GLint);
    procedure AttachLayer(Target: GLenum; attachment: GLenum; texture: LongWord; level: GLint; Layer: GLint);
    procedure AttachRenderBuffer(Target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: LongWord);
    (* If texture is the name of a three-dimensional texture, cube map texture, one-or
      two-dimensional array texture, or two-dimensional multisample array texture, the
      texture level attached to the framebuffer attachment point is an array of images,
      and the framebuffer attachment is considered layered. *)
    procedure AttachTexture(Target: GLenum; attachment: GLenum; texture: LongWord; level: GLint);
    procedure AttachTextureLayer(Target: GLenum; attachment: GLenum; texture: LongWord; level: GLint; Layer: GLint);
    // copy rect from bound read framebuffer to bound draw framebuffer
    procedure Blit(srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint; dstX0: GLint; dstY0: GLint; dstX1: GLint;
      dstY1: GLint; mask: GLbitfield; filter: GLenum);
    (* target = GL_DRAW_FRAMEBUFFER, GL_READ_FRAMEBUFFER, GL_FRAMEBUFFER (equivalent to GL_DRAW_FRAMEBUFFER)
     If default framebuffer (0) is bound:
     attachment = GL_FRONT_LEFT, GL_FRONT_RIGHT, GL_BACK_LEFT, or GL_BACK_RIGHT, GL_DEPTH, GL_STENCIL
     if a framebuffer object is bound:
     attachment = GL_COLOR_ATTACHMENTi, GL_DEPTH_ATTACHMENT, GL_STENCIL_ATTACHMENT, GL_DEPTH_STENCIL_ATTACHMENT
     param = GL_FRAMEBUFFER_ATTACHMENT_(OBJECT_TYPE, OBJECT_NAME,
     RED_SIZE, GREEN_SIZE, BLUE_SIZE, ALPHA_SIZE, DEPTH_SIZE, STENCIL_SIZE,
     COMPONENT_TYPE, COLOR_ENCODING, TEXTURE_LEVEL, LAYERED, TEXTURE_CUBE_MAP_FACE, TEXTURE_LAYER *)
    function GetAttachmentParameter(Target: GLenum; attachment: GLenum; pname: GLenum): GLint;
    (* Returns the type of object bound to attachment point:
      GL_NONE, GL_FRAMEBUFFER_DEFAULT, GL_TEXTURE, or GL_RENDERBUFFER *)
    function GetAttachmentObjectType(Target: GLenum; attachment: GLenum): GLint;
    // Returns the name (ID) of the texture or renderbuffer attached to attachment point
    function GetAttachmentObjectName(Target: GLenum; attachment: GLenum): GLint;
    function GetStatus: TgxFramebufferStatus;
    function GetStringStatus(out clarification: string): TgxFramebufferStatus;
    class function IsSupported: Boolean; override;
  end;

  (* Manages a handle to a Renderbuffer Object.
    A Renderbuffer is a "framebuffer-attachable image" for generalized offscreen
    rendering and it also provides a means to support rendering to GL logical
    buffer types which have no corresponding texture format (stencil, accum, etc). *)
  TgxRenderbufferHandle = class(TgxContextHandle)
  protected
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: Cardinal); override;
    class function IsValid(const ID: LongWord): BYTEBOOL; override;
  public
    procedure Bind;
    procedure UnBind;
    procedure SetStorage(internalformat: GLenum; Width, Height: GLsizei);
    procedure SetStorageMultisample(internalformat: GLenum; samples: GLsizei; Width, Height: GLsizei);
    class function IsSupported: Boolean; override;
  end;

  TgxProgramHandleEXT = class(TgxContextHandle)
  private
    FReady: Boolean;
    FInfoLog: string;
  protected
    function DoAllocateHandle: LongWord; override;
    procedure DoDestroyHandle(var AHandle: LongWord); override;
    class function IsValid(const ID: LongWord): BYTEBOOL; override;
    class function GetTarget: GLenum; virtual; abstract;
  public
    procedure LoadARBProgram(AText: string);
    procedure Enable;
    procedure Disable;
    procedure Bind;
    property Ready: Boolean read FReady;
    property InfoLog: string read FInfoLog;
  end;

  TgxVertexProgramHandle = class(TgxProgramHandleEXT)
  protected
    class function GetTarget: GLenum; override;
  public
    class function IsSupported: Boolean; override;
  end;

  TgxFragmentProgramHandle = class(TgxProgramHandleEXT)
  protected
    class function GetTarget: GLenum; override;
  public
    class function IsSupported: Boolean; override;
  end;

  TgxGeometryProgramHandle = class(TgxProgramHandleEXT)
  protected
    class function GetTarget: GLenum; override;
  public
    class function IsSupported: Boolean; override;
  end;

  (* Base class for GLSL handles (programs and shaders).
    Do not use this class directly, use one of its subclasses instead. *)
  TgxSLHandle = class(TgxContextHandle)
  private
  protected
    procedure DoDestroyHandle(var AHandle: LongWord); override;
  public
    function InfoLog: string;
    class function IsSupported: Boolean; override;
  end;

  (* Manages a handle to a Shader Object.
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user.
    Do not use this class directly, use one of its subclasses instead. *)
  TgxShaderHandle = class(TgxSLHandle)
  private
    FShaderType: Cardinal;
  protected
    function DoAllocateHandle: LongWord; override;
    class function IsValid(const ID: LongWord): BYTEBOOL; override;
  public
    procedure ShaderSource(const source: AnsiString); overload;
    // Returns True if compilation sucessful
    function CompileShader: Boolean;
    property ShaderType: Cardinal read FShaderType;
  end;

  TgxShaderHandleClass = class of TgxShaderHandle;

  // Manages a handle to a Vertex Shader Object.
  TgxVertexShaderHandle = class(TgxShaderHandle)
  public
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // Manages a handle to a Geometry Shader Object.
  TgxGeometryShaderHandle = class(TgxShaderHandle)
  public
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // Manages a handle to a Fragment Shader Object.
  TgxFragmentShaderHandle = class(TgxShaderHandle)
  public
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // Manages a handle to a Tessellation Control Shader Object.
  TgxTessControlShaderHandle = class(TgxShaderHandle)
  public
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // Manages a handle to a Tessellation Evaluation Shader Object.
  TgxTessEvaluationShaderHandle = class(TgxShaderHandle)
  public
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  (* Manages a GLSL Program Object.
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user. *)
  TgxProgramHandle = class(TgxSLHandle)
  public
    class function IsValid(const ID: LongWord): BYTEBOOL; override;
  private
    FName: string;
    function GetUniform1i(const index: string): Integer;
    procedure SetUniform1i(const index: string; val: Integer);
    function GetUniform2i(const index: string): TVector2i;
    procedure SetUniform2i(const index: string; const Value: TVector2i);
    function GetUniform3i(const index: string): TVector3i;
    procedure SetUniform3i(const index: string; const Value: TVector3i);
    function GetUniform4i(const index: string): TVector4i;
    procedure SetUniform4i(const index: string; const Value: TVector4i);
    function GetUniform1f(const index: string): Single;
    procedure SetUniform1f(const index: string; val: Single);
    function GetUniform2f(const index: string): TVector2f;
    procedure SetUniform2f(const index: string; const val: TVector2f);
    function GetUniform3f(const index: string): TAffineVector;
    procedure SetUniform3f(const index: string; const val: TAffineVector);
    function GetUniform4f(const index: string): TVector4f;
    procedure SetUniform4f(const index: string; const val: TVector4f);
    function GetUniformMatrix2fv(const index: string): TMatrix2f;
    procedure SetUniformMatrix2fv(const index: string; const val: TMatrix2f);
    function GetUniformMatrix3fv(const index: string): TMatrix3f;
    procedure SetUniformMatrix3fv(const index: string; const val: TMatrix3f);
    function GetUniformMatrix4fv(const index: string): TMatrix4f;
    procedure SetUniformMatrix4fv(const index: string; const val: TMatrix4f);
    function GetUniformTextureHandle(const index: string; const TextureIndex: Integer; const TextureTarget: TglTextureTarget): Cardinal;
    procedure SetUniformTextureHandle(const index: string; const TextureIndex: Integer; const TextureTarget: TglTextureTarget; const Value: Cardinal);
    procedure SetUniformBuffer(const index: string; Value: TgxUniformBufferHandle);
  protected
    function DoAllocateHandle: LongWord; override;
  public
    property Name: string read FName write FName;
    constructor Create; override;
    (* Compile and attach a new shader.
      Raises an EGLShader exception in case of failure. *)
    procedure AddShader(ShaderType: TgxShaderHandleClass; const ShaderSource: string; treatWarningsAsErrors: Boolean = False);
    procedure AttachObject(shader: TgxShaderHandle);
    procedure DetachAllObject;
    procedure BindAttribLocation(index: Integer; const aName: string);
    procedure BindFragDataLocation(index: Integer; const aName: string);
    function LinkProgram: Boolean;
    function ValidateProgram: Boolean;
    function GetAttribLocation(const aName: string): GLint;
    function GetUniformLocation(const aName: string): GLint;
    function GetUniformOffset(const aName: string): GLint;
    function GetUniformBlockIndex(const aName: string): GLint;
    function GetVaryingLocation(const aName: string): GLint;
    // Currently, NVidia-specific.
    procedure AddActiveVarying(const aName: string);
    // Currently, NVidia-specific.
    function GetUniformBufferSize(const aName: string): GLint;
    procedure UseProgramObject;
    procedure EndUseProgramObject;
    procedure SetUniformi(const index: string; const val: GLint); overload;
    procedure SetUniformi(const index: string; const val: TVector2i); overload;
    procedure SetUniformi(const index: string; const val: TVector3i); overload;
    procedure SetUniformi(const index: string; const val: TVector4i); overload;
    procedure SetUniformf(const index: string; const val: Single); overload;
    procedure SetUniformf(const index: string; const val: TVector2f); overload;
    procedure SetUniformf(const index: string; const val: TVector3f); overload;
    procedure SetUniformf(const index: string; const val: TVector4f); overload;
    // Shader parameters.
    property Uniform1i[const index: string]: GLint read GetUniform1i write SetUniform1i;
    property Uniform2i[const index: string]: TVector2i read GetUniform2i write SetUniform2i;
    property Uniform3i[const index: string]: TVector3i read GetUniform3i write SetUniform3i;
    property Uniform4i[const index: string]: TVector4i read GetUniform4i write SetUniform4i;
    property Uniform1f[const index: string]: Single read GetUniform1f write SetUniform1f;
    property Uniform2f[const index: string]: TVector2f read GetUniform2f write SetUniform2f;
    property Uniform3f[const index: string]: TAffineVector read GetUniform3f write SetUniform3f;
    property Uniform4f[const index: string]: TVector4f read GetUniform4f write SetUniform4f;
    property UniformMatrix2fv[const index: string]: TMatrix2f read GetUniformMatrix2fv write SetUniformMatrix2fv;
    property UniformMatrix3fv[const index: string]: TMatrix3f read GetUniformMatrix3fv write SetUniformMatrix3fv;
    property UniformMatrix4fv[const index: string]: TMatrix4f read GetUniformMatrix4fv write SetUniformMatrix4fv;
    property UniformTextureHandle[const index: string; const TextureIndex: Integer; const TextureTarget: TglTextureTarget]
      : LongWord read GetUniformTextureHandle write SetUniformTextureHandle;
    property UniformBuffer[const index: string]: TgxUniformBufferHandle write SetUniformBuffer;
  end;

  TgxContextNotification = record
    obj: TObject;
    Event: TNotifyEvent;
  end;

  // Stores and manages all the TgxContext objects.
  TgxContextManager = class
  private
    FList: TThreadList;
    FTerminated: Boolean;
    FNotifications: array of TgxContextNotification;
    FCreatedRCCount: Integer;
    FHandles: TThreadList;
    FThread: TThread;
    FServiceStarter: TEvent;
    FServiceContext: TgxContext;
  protected
    procedure Lock;
    procedure UnLock;
    procedure RegisterContext(aContext: TgxContext);
    procedure UnRegisterContext(aContext: TgxContext);
    procedure ContextCreatedBy(aContext: TgxContext);
    procedure DestroyingContextBy(aContext: TgxContext);
    property ServiceContext: TgxContext read FServiceContext;
  public
    constructor Create;
    destructor Destroy; override;
    (* Returns an appropriate, ready-to use context.
      The returned context should be freed by caller. *)
    function CreateContext(AClass: TgxContextClass = nil): TgxContext;
    (* Returns the number of TgxContext object.
      This is *not* the number of OpenGL rendering contexts! *)
    function ContextCount: Integer;
    (* Registers a new object to notify when the last context is destroyed.
      When the last rendering context is destroyed, the 'anEvent' will
      be invoked with 'anObject' as parameter.
      Note that the registration is kept until the notification is triggered
      or a RemoveNotification on 'anObject' is issued. *)
    procedure LastContextDestroyNotification(anObject: TObject; anEvent: TNotifyEvent);
    // Unregisters an object from the notification lists.
    procedure RemoveNotification(anObject: TObject);
    // Marks the context manager for termination
    procedure Terminate;
    // Request all contexts to destroy all their handles.
    procedure DestroyAllHandles;
    // Notify all contexts about necessity of handles preparation.
    procedure NotifyPreparationNeed;
  end;

  EVXContext = class(Exception);
  EPBuffer = class(Exception);
  EVXShader = class(EVXContext);

// Drivers should register themselves via this function.
procedure RegisterContextClass(aContextClass: TgxContextClass);
(* The TgxContext that is the currently active context, if any.
  Returns nil if no context is active. *)
function CurrentContext: TgxContext;
function SafeCurrentContext: TgxContext;
function IsMainThread: Boolean;
function IsServiceContextAvaible: Boolean;
function GetServiceWindow: TForm;

var
  GXContextManager: TgxContextManager;
  vIgnoreOpenGXErrors: Boolean = False;
  vContextActivationFailureOccurred: Boolean = False;
  vMultitextureCoordinatorClass: TgxAbstractMultitextureCoordinatorClass;
  vCurrentContext: TgxContext;


// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

var
  vContextClasses: TList;
  vServiceWindow: TForm;
  vMainThread: Boolean;

function CurrentContext: TgxContext; inline;
begin
  Result := vCurrentContext;
end;

function SafeCurrentContext: TgxContext; inline;
begin
  Result := CurrentContext;
  if not Assigned(Result) then
  begin
{$IFDEF USE_LOGGING}
    ShowMessages(cNoActiveRC);
{$ENDIF}
    Abort;
  end;
end;

function IsMainThread: Boolean;
begin
  Result := vMainThread;
end;

function IsServiceContextAvaible: Boolean;
begin
  Result := GXContextManager.FHandles <> nil;
end;

function GetServiceWindow: TForm;
begin
  Result := vServiceWindow;
end;

procedure RegisterContextClass(aContextClass: TgxContextClass);
begin
  if not Assigned(vContextClasses) then
    vContextClasses := TList.Create;
  vContextClasses.Add(aContextClass);
end;

constructor TgxAbstractMultitextureCoordinator.Create(AOwner: TgxContext);
begin
  FOwner := AOwner;
end;

// ------------------
// ------------------ TgxContext ------------------
// ------------------

constructor TgxContext.Create;
begin
  inherited Create;
{$IFDEF USE_MULTITHREAD}
  FLock := TCriticalSection.Create;
{$ENDIF}
  FColorBits := 32;
  FStencilBits := 0;
  FAccumBits := 0;
  FAuxBuffers := 0;
  FLayer := clMainPlane;
  FOptions := [];
{$IFDEF USE_MULTITHREAD}
  FSharedContexts := TThreadList.Create;
{$ELSE}
  FSharedContexts := TList.Create;
  FOwnedHandles := TList.Create;
{$ENDIF}
  FSharedContexts.Add(Self);
  FAcceleration := chaUnknown;
  FgxStates := TgxStateCache.Create;
  FTransformation := TgTransformation.Create;
  FTransformation.LoadMatricesEnabled := True;
  GXContextManager.RegisterContext(Self);
  FIsPraparationNeed := True;
end;

destructor TgxContext.Destroy;
begin
  if IsValid then
    DestroyContext;
  GXContextManager.UnRegisterContext(Self);
  FOwnedHandles.Free;
  
  FSharedContexts.Free;
  FgxStates.Free;
  FGXS.Free;
  FTransformation.Free;
  FSharedContexts.Free;
{$IFDEF USE_MULTITHREAD}
  FLock.Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TgxContext.SetColorBits(const aColorBits: Integer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FColorBits := aColorBits;
end;

procedure TgxContext.SetAlphaBits(const aAlphaBits: Integer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FAlphaBits := aAlphaBits;
end;

procedure TgxContext.SetDepthBits(const val: Integer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FDepthBits := val;
end;

procedure TgxContext.SetLayer(const Value: TgxContextLayer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FLayer := Value;
end;

procedure TgxContext.SetStencilBits(const aStencilBits: Integer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FStencilBits := aStencilBits;
end;

procedure TgxContext.SetAccumBits(const aAccumBits: Integer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FAccumBits := aAccumBits;
end;

procedure TgxContext.SetAuxBuffers(const aAuxBuffers: Integer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FAuxBuffers := aAuxBuffers;
end;

procedure TgxContext.SetOptions(const aOptions: TgxRCOptions);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FOptions := aOptions;
end;

procedure TgxContext.SetAntiAliasing(const val: TgxAntiAliasing);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FAntiAliasing := val;
end;

procedure TgxContext.SetAcceleration(const val: TgxContextAcceleration);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FAcceleration := val;
end;

function TgxContext.GetActive: Boolean;
begin
  Result := (FActivationCount > 0);
end;

procedure TgxContext.SetActive(const aActive: Boolean);
begin
  // activation/deactivation can be nested...
  while aActive <> Active do
  begin
    if aActive then
      Activate
    else
      Deactivate;
  end;
end;

procedure TgxContext.CreateContext(ADeviceHandle: THandle);
begin
  if IsValid then
    raise EVXContext.Create(strContextAlreadyCreated);
  DoCreateContext(ADeviceHandle);
  Manager.ContextCreatedBy(Self);
end;

procedure TgxContext.CreateMemoryContext(OutputDevice: THandle; Width, Height: Integer; BufferCount: Integer);
begin
  if IsValid then
    raise EVXContext.Create(strContextAlreadyCreated);
  DoCreateMemoryContext(OutputDevice, Width, Height, BufferCount);
  Manager.ContextCreatedBy(Self);
end;

procedure TgxContext.PrepareHandlesData;
var
  I: Integer;
  LHandle: TgxContextHandle;
begin
  if vCurrentContext = Self then
  begin
{$IFNDEF USE_MULTITHREAD}
    for I := Manager.FHandles.LockList.Count - 1 downto 0 do
    begin
      LHandle := TgxContextHandle(Manager.FHandles.LockList[I]);
      if Assigned(LHandle.FOnPrepare) then
        LHandle.FOnPrepare(Self);
    end;
{$ELSE}
    with Manager.FHandles.LockList do
      try
        for I := Count - 1 downto 0 do
        begin
          LHandle := TgxContextHandle(Items[I]);
          if Assigned(LHandle.FOnPrepare) then
            LHandle.FOnPrepare(Self);
        end;
      finally
        Manager.FHandles.UnlockList;
      end;
{$ENDIF}
    FIsPraparationNeed := False;
  end;
end;

procedure TgxContext.PropagateSharedContext;
var
  I, j: Integer;
  otherContext: TgxContext;
  otherList: TList;
begin
{$IFNDEF USE_MULTITHREAD}
  with FSharedContexts do
  begin
    for I := 1 to Count - 1 do
    begin
      otherContext := TgxContext(Items[I]);
      otherList := otherContext.FSharedContexts;
      for j := 0 to otherList.Count - 1 do
        if IndexOf(otherList[j]) < 0 then
          Add(otherList[j]);
    end;
    for I := 1 to Count - 1 do
    begin
      otherContext := TgxContext(Items[I]);
      otherList := otherContext.FSharedContexts;
      if otherList.IndexOf(Self) < 0 then
        otherList.Add(Self);
    end;
  end;
{$ELSE}
  with FSharedContexts.LockList do
    try
      for I := 1 to Count - 1 do
      begin
        otherContext := TgxContext(Items[I]);
        otherList := otherContext.FSharedContexts.LockList;
        for j := 0 to otherList.Count - 1 do
          if IndexOf(otherList[j]) < 0 then
            Add(otherList[j]);
        otherContext.FSharedContexts.UnlockList;
      end;
      for I := 1 to Count - 1 do
      begin
        otherContext := TgxContext(Items[I]);
        otherList := otherContext.FSharedContexts.LockList;
        if otherList.IndexOf(Self) < 0 then
          otherList.Add(Self);
        otherContext.FSharedContexts.UnlockList;
      end;
    finally
      FSharedContexts.UnlockList;
    end;
{$ENDIF}
end;

procedure TgxContext.ShareLists(aContext: TgxContext);
begin
{$IFNDEF USE_MULTITHREAD}
  if FSharedContexts.IndexOf(aContext) < 0 then
  begin
    if DoShareLists(aContext) then
    begin
      FSharedContexts.Add(aContext);
      PropagateSharedContext;
    end;
  end;
{$ELSE}
  with FSharedContexts.LockList do
    try
      if IndexOf(aContext) < 0 then
      begin
        if DoShareLists(aContext) then
        begin
          Add(aContext);
          PropagateSharedContext;
        end;
      end;
    finally
      FSharedContexts.UnlockList;
    end;
{$ENDIF}
end;

procedure TgxContext.DestroyAllHandles;
var
  I: Integer;
begin
  Activate;
  try
{$IFNDEF USE_MULTITHREAD}
      for i:=FOwnedHandles.Count-1 downto 0 do
         TgxContextHandle(FOwnedHandles[i]).DestroyHandle;
{$ELSE}
    with Manager.FHandles.LockList do
      try
        for I := Count - 1 downto 0 do
          TgxContextHandle(Items[I]).ContextDestroying;
      finally
        Manager.FHandles.UnlockList;
      end;
{$ENDIF}
  finally
    Deactivate;
  end;
end;

procedure TgxContext.DestroyContext;
var
  I: Integer;
  oldContext, otherContext: TgxContext;
  contextHandle: TgxContextHandle;
  aList: TList;
begin

  if vCurrentContext <> Self then
  begin
    oldContext := vCurrentContext;
    if Assigned(oldContext) then
      oldContext.Deactivate;
  end
  else
    oldContext := nil;
  Activate;
  try
{$IFNDEF USE_MULTITHREAD}
    for I := Manager.FHandles.LockList.Count - 1 downto 0 do
    begin
      contextHandle := TgxContextHandle(Manager.FHandles.LockList[I]);
      contextHandle.ContextDestroying;
    end;
{$ELSE}
    aList := Manager.FHandles.LockList;
    try
      for I := aList.Count - 1 downto 0 do
      begin
        contextHandle := TgxContextHandle(aList[I]);
        contextHandle.ContextDestroying;
      end;
    finally
      Manager.FHandles.UnlockList;
    end;
{$ENDIF}
    Manager.DestroyingContextBy(Self);

{$IFDEF USE_MULTITHREAD}
    aList := FSharedContexts.LockList;
{$ELSE}
    aList := FSharedContexts;
{$ENDIF}
    for I := 1 to aList.Count - 1 do
    begin
      otherContext := TgxContext(aList[I]);
      otherContext.FSharedContexts.Remove(Self);
    end;
    FSharedContexts.Clear;
    FSharedContexts.Add(Self);
{$IFDEF USE_MULTITHREAD}
    FSharedContexts.UnlockList;
{$ENDIF}
    Active := False;
    DoDestroyContext;
  finally
    if Assigned(oldContext) then
      oldContext.Activate;
  end;
  FAcceleration := chaUnknown;
end;

procedure TgxContext.Activate;
begin
{$IFDEF USE_MULTITHREAD}
  FLock.Enter;
{$ENDIF}
  if FActivationCount = 0 then
  begin
    if not IsValid then
      raise EVXContext.Create(strContextNotCreated);

    vContextActivationFailureOccurred := False;
    try
      DoActivate;
    except
      vContextActivationFailureOccurred := True;
    end;
    vCurrentContext := Self;
  end
  else
    Assert(vCurrentContext = Self, 'vCurrentContext <> Self');
  Inc(FActivationCount);
end;

procedure TgxContext.Deactivate;
begin
  Assert(vCurrentContext = Self);
  Dec(FActivationCount);
  if FActivationCount = 0 then
  begin
    if not IsValid then
      raise EVXContext.Create(strContextNotCreated);
    if not vContextActivationFailureOccurred then
      DoDeactivate;
    vCurrentContext := nil;
  end
  else if FActivationCount < 0 then
    raise EVXContext.Create(strUnbalancedContexActivations);
{$IFDEF USE_MULTITHREAD}
  FLock.Leave;
{$ENDIF}
end;

function TgxContext.FindCompatibleContext: TgxContext;
var
  I: Integer;
begin
  Result := nil;
{$IFNDEF USE_MULTITHREAD}
  for I := 0 to FSharedContexts.Count - 1 do
    if TgxContext(FSharedContexts[I]) <> Self then
    begin
      Result := TgxContext(FSharedContexts[I]);
      Break;
    end;
{$ELSE}
  with FSharedContexts.LockList do
    try
      for I := 0 to Count - 1 do
        if TgxContext(Items[I]) <> Self then
        begin
          Result := TgxContext(Items[I]);
          Break;
        end;
    finally
      FSharedContexts.UnlockList;
    end;
{$ENDIF}
end;

class function TgxContext.ServiceContext: TgxContext;
begin
  Result := GXContextManager.FServiceContext;
end;

procedure TgxContext.MakeGLCurrent;
begin
//
end;

function TgxContext.GetGXS: TgxAbstractMultitextureCoordinator;
begin
  if FGXS = nil then
    FGXS := vMultitextureCoordinatorClass.Create(Self);
  Result := FGXS;
end;

// ------------------
// ------------------ TgxContextHandle ------------------
// ------------------

constructor TgxContextHandle.Create;
begin
  inherited Create;
  FHandles := TList.Create;
  // first is a dummy record
  new(FLastHandle);
  FillChar(FLastHandle^, sizeof(FLastHandle^), 0);
  FHandles.Add(FLastHandle);
  GXContextManager.FHandles.Add(Self);
end;

constructor TgxContextHandle.CreateAndAllocate(failIfAllocationFailed: Boolean = True);
begin
  Create;
  AllocateHandle;
  if failIfAllocationFailed and (Handle = 0) then
    raise EVXContext.Create('Auto-allocation failed');
end;

destructor TgxContextHandle.Destroy;
var
  I: Integer;
begin
  DestroyHandle;
  for I := 0 to FHandles.Count - 1 do
    Dispose(RCItem(I));
  FHandles.Free;
  if Assigned(GXContextManager) then
    GXContextManager.FHandles.Remove(Self);
  inherited Destroy;
end;

function TgxContextHandle.AllocateHandle: LongWord;
var
  I: Integer;
  bSucces: Boolean;
  aList: TList;
  p: PgxRCHandle;

begin
  // if handle aready allocated in current context
  Result := GetHandle;
  if Result <> 0 then
    exit;

  if vCurrentContext = nil then
  begin
    ShowMessage('Failed to allocate OpenGL identifier - no active rendering context!');
    exit;
  end;

  // add entry
  new(FLastHandle);
  FillChar(FLastHandle^, sizeof(FLastHandle^), 0);
  FHandles.Add(FLastHandle);
  FLastHandle.FRenderingContext := vCurrentContext;

  bSucces := False;
  if Transferable then
  begin
{$IFNDEF USE_MULTITHREAD}
    aList := vCurrentContext.FSharedContexts;
{$ELSE}
    aList := vCurrentGLContext.FSharedContexts.LockList;
    try
{$ENDIF}
      for I := aList.Count - 1 downto 0 do
      begin
        p := SearchRC(aList[I]);
        if (p.FHandle > 0) then
        begin
          // Copy shared handle
          // FLastHandle.FRenderingContext := vCurrentContext;
          FLastHandle.FHandle := p.FHandle;
          FLastHandle.FChanged := p.FChanged;
          Inc(vCurrentContext.FOwnedHandlesCount);
          bSucces := True;
          Break;
        end;
      end;
{$IFNDEF USE_MULTITHREAD}
{$ELSE}
    finally
      vCurrentContext.FSharedContexts.UnlockList;
    end;
{$ENDIF}
  end;
  if not bSucces then
  begin
    // Allocate handle in current context
    FLastHandle.FHandle := DoAllocateHandle;
    bSucces := FLastHandle.FHandle <> 0;
    FLastHandle.FChanged := bSucces;
    if bSucces then
      Inc(vCurrentContext.FOwnedHandlesCount);
  end;

  Result := FLastHandle.FHandle;
  if not bSucces then
    /// ShowMessages(cNoActiveRC)
  else if Assigned(FOnPrepare) then
    GXContextManager.NotifyPreparationNeed;
end;

function TgxContextHandle.IsAllocatedForContext(aContext: TgxContext = nil): Boolean;
begin
  Result := SearchRC(aContext).FHandle > 0;
end;

function TgxContextHandle.SearchRC(aContext: TgxContext): PgxRCHandle;
var
  I: Integer;
begin
  if aContext = nil then
    aContext := vCurrentContext;

  if aContext = FLastHandle.FRenderingContext then
  begin
    Result := FLastHandle;
    exit;
  end;

  for I := 1 to FHandles.Count - 1 do
    if RCItem(I).FRenderingContext = aContext then
    begin
      Result := RCItem(I);
      exit;
    end;

  // first handle is always a dummy
  Result := FHandles[0];
end;

procedure TgxContextHandle.CheckCurrentRC;
begin
  if vCurrentContext <> FLastHandle.FRenderingContext then
    FLastHandle := SearchRC(vCurrentContext);
end;

function TgxContextHandle.GetHandle: LongWord;
begin
  CheckCurrentRC;
  // inline doesn't always work... so optimize it here
  if vCurrentContext <> FLastHandle.FRenderingContext then
    FLastHandle := SearchRC(vCurrentContext);

  Result := FLastHandle.FHandle;
end;

procedure TgxContextHandle.DestroyHandle;
var
  oldContext: TgxContext;
  p: PgxRCHandle;
  I: Integer;
begin
  oldContext := vCurrentContext;
  if Assigned(oldContext) then
    oldContext.Deactivate;
  try
    for I := FHandles.Count - 1 downto 1 do
    begin
      p := FHandles[I];
      if p.FHandle > 0 then
      begin
        p.FRenderingContext.Activate;
        if IsValid(p.FHandle) then
          DoDestroyHandle(p.FHandle);
        Dec(p.FRenderingContext.FOwnedHandlesCount);
        p.FRenderingContext.Deactivate;
        p.FRenderingContext := nil;
        p.FHandle := 0;
        p.FChanged := True;
      end;
      Dispose(p);
    end;
    FHandles.Count := 1; // delete all in 1 step
    FLastHandle := FHandles[0];
  finally
    if Assigned(vCurrentContext) then
      vCurrentContext.Deactivate;
    if Assigned(oldContext) then
      oldContext.Activate;
  end;
end;

procedure TgxContextHandle.ContextDestroying;
var
  I: Integer;
  p: PgxRCHandle;
  aList: TList;
  bShared: Boolean;
begin
  if Assigned(vCurrentContext) then
  begin
    bShared := False;
    if Transferable then
    begin
{$IFNDEF USE_MULTITHREAD}
      aList := vCurrentContext.FSharedContexts;
{$ELSE}
      aList := vCurrentContext.FSharedContexts.LockList;
      try
{$ENDIF USE_MULTITHREAD}
        for I := FHandles.Count - 1 downto 1 do
        begin
          p := RCItem(I);
          if (p.FRenderingContext <> vCurrentContext) and (p.FHandle <> 0) and (aList.IndexOf(p.FRenderingContext) > -1) then
          begin
            bShared := True;
            Break;
          end;
        end;
{$IFDEF USE_MULTITHREAD}
      finally
        vCurrentContext.FSharedContexts.UnlockList;
      end;
{$ENDIF USE_MULTITHREAD}
    end;

    for I := FHandles.Count - 1 downto 1 do
    begin
      p := RCItem(I);
      if (p.FRenderingContext = vCurrentContext) and (p.FHandle <> 0) then
      begin
        if not bShared then
          if IsValid(p.FHandle) then
            DoDestroyHandle(p.FHandle);
        Dec(p.FRenderingContext.FOwnedHandlesCount);
        p.FHandle := 0;
        p.FRenderingContext := nil;
        p.FChanged := True;
        Dispose(p);
        FHandles.Delete(I);
        if FLastHandle = p then
          FLastHandle := FHandles[0];
        exit;
      end;
    end;
  end;
end;

function TgxContextHandle.GetContext: TgxContext;
var
  I: Integer;
  p: PgxRCHandle;
begin
  Result := nil;
  // Return first context where handle is allocated
  for I := FHandles.Count - 1 downto 1 do
  begin
    p := RCItem(I);
    if (p.FRenderingContext <> nil) and (p.FHandle <> 0) then
    begin
      Result := p.FRenderingContext;
      // If handle allocated in active context - return it
      if (Result = vCurrentContext) then
        exit;
    end;
  end;
end;

function TgxContextHandle.IsDataNeedUpdate: Boolean;
begin
  if GetHandle = 0 then
    CheckCurrentRC;
  Result := (FLastHandle.FHandle = 0) or FLastHandle.FChanged;
end;

function TgxContextHandle.IsDataComplitelyUpdated: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := FHandles.Count - 1 downto 1 do
  begin
    with RCItem(I)^ do
      if (FRenderingContext <> nil) and (FHandle <> 0) and FChanged then
        exit;
  end;
  Result := True;
end;

procedure TgxContextHandle.NotifyDataUpdated;
var
  I: Integer;
  aList: TList;
begin
  if Assigned(vCurrentContext) then
  begin
    if not Transferable then
    begin
      CheckCurrentRC();
      if FLastHandle.FHandle <> 0 then
      begin
        FLastHandle.FChanged := False;
        exit;
      end;
    end
    else
    begin
{$IFNDEF USE_MULTITHREAD}
      aList := vCurrentContext.FSharedContexts;
{$ELSE}
      aList := vCurrentContext.FSharedContexts.LockList;
      try
{$ENDIF}
        for I := 0 to aList.Count - 1 do
        begin
          with SearchRC(aList[I])^ do
            if (FHandle <> 0) then
              FChanged := False;
        end;
{$IFDEF USE_MULTITHREAD}
      finally
        vCurrentContext.FSharedContexts.UnlockList;
      end;
{$ENDIF}
    end;
  end
  else
    /// ShowMessages(cNoActiveRC);
end;

function TgxContextHandle.RCItem(AIndex: Integer): PgxRCHandle;
begin
  Result := FHandles[AIndex];
end;

procedure TgxContextHandle.NotifyChangesOfData;
var
  I: Integer;
begin
  for I := FHandles.Count - 1 downto 1 do
    RCItem(I).FChanged := True;
  if Assigned(FOnPrepare) then
    GXContextManager.NotifyPreparationNeed;
end;

function TgxContextHandle.IsShared: Boolean;
var
  I: Integer;
  vContext: TgxContext;
  aList: TList;
begin
  Result := False;
  // untransferable handles can't be shared
  if not Transferable then
    exit;
  Result := True;
{$IFNDEF USE_MULTITHREAD}
  aList := vCurrentContext.FSharedContexts;
{$ELSE}
  aList := vCurrentContext.FSharedContexts.LockList;
  try
{$ENDIF}
    for I := 0 to aList.Count - 1 do
    begin
      vContext := aList[I];
      if (vContext <> vCurrentContext) and
      // at least one context is friendly
        (SearchRC(vContext).FHandle <> 0) then
        exit;
    end;
{$IFDEF USE_MULTITHREAD}
  finally
    vCurrentContext.FSharedContexts.UnlockList;
  end;
{$ENDIF}
  Result := False;
end;

class function TgxContextHandle.Transferable: Boolean;
begin
  Result := True;
end;

class function TgxContextHandle.IsValid(const ID: LongWord): BYTEBOOL;
begin
  Result := True;
end;

class function TgxContextHandle.IsSupported: Boolean;
begin
  Result := True;
end;

// ------------------
// ------------------ TgxVirtualHandle ------------------
// ------------------

function TgxVirtualHandle.DoAllocateHandle: LongWord;
begin
  Result := 0;
  if Assigned(FOnAllocate) then
    FOnAllocate(Self, Result);
end;

procedure TgxVirtualHandle.DoDestroyHandle(var AHandle: LongWord);
begin
  if not vContextActivationFailureOccurred then
  begin
    glGetError;
    if Assigned(FOnDestroy) then
      FOnDestroy(Self, AHandle);
    /// CheckOpenGLError;
  end;
end;

class function TgxVirtualHandle.Transferable: Boolean;
begin
  Result := False;
end;

// ------------------
// TgxVirtualHandleTransf
// ------------------

class function TgxVirtualHandleTransf.Transferable: Boolean;
begin
  Result := True;
end;

// ------------------
// ------------------ TgxListHandle ------------------
// ------------------

function TgxListHandle.DoAllocateHandle: LongWord;
begin
  Result := glGenLists(1);
end;

procedure TgxListHandle.DoDestroyHandle(var AHandle: LongWord);
begin
  if not vContextActivationFailureOccurred then
  begin
    glGetError;
    glDeleteLists(AHandle, 1);
    /// CheckOpenGLError;
  end;
end;

class function TgxListHandle.IsValid(const ID: LongWord): BYTEBOOL;
begin
  Result := ByteBool(glIsList(ID));
end;

procedure TgxListHandle.NewList(mode: Cardinal);
begin
  vCurrentContext.gxStates.NewList(GetHandle, mode);
end;

procedure TgxListHandle.EndList;
begin
  vCurrentContext.gxStates.EndList;
end;

procedure TgxListHandle.CallList;
begin
  vCurrentContext.gxStates.CallList(GetHandle);
end;

// ------------------
// ------------------ TgxTextureHandle ------------------
// ------------------

function TgxTextureHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenTextures(1, @Result);
  FTarget := ttNoShape;
end;

procedure TgxTextureHandle.DoDestroyHandle(var AHandle: LongWord);
var
  a: GLint;
  t: TglTextureTarget;
begin
  if not vContextActivationFailureOccurred then
  // reset error status
  { Unbind identifier from all image selectors. }
  with GetContext.gxStates do
  begin
    for a := 0 to MaxTextureImageUnits - 1 do
      for t := Low(TglTextureTarget) to High(TglTextureTarget) do
        if TextureBinding[a, t] = AHandle then
          TextureBinding[a, t] := 0;
  end;
  glDeleteTextures(1, @AHandle);
  /// CheckOpenGLError;
end;

class function TgxTextureHandle.IsValid(const ID: LongWord): BYTEBOOL;
begin
  Result := ByteBool(glIsTexture(ID));
end;

procedure TgxTextureHandle.SetTarget(ATarget: TglTextureTarget);
begin
  if FTarget = ttNoShape then
    FTarget := ATarget;
end;

// ------------------
// ------------------ TgxSamplerHandle ------------------
// ------------------

function TgxSamplerHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenSamplers(1, @Result);
end;

procedure TgxSamplerHandle.DoDestroyHandle(var AHandle: LongWord);
begin
  if not vContextActivationFailureOccurred then
  glGetError;
  glDeleteSamplers(1, @AHandle);
  /// CheckOpenGLError;
end;

class function TgxSamplerHandle.IsSupported: Boolean;
begin
  Result := Boolean(GL_SAMPLER_OBJECT_AMD);
end;

class function TgxSamplerHandle.IsValid(const ID: LongWord): BYTEBOOL;
begin
  Result := ByteBool(glIsSampler(ID));
end;

// ------------------
// ------------------ TgxQueryHandle ------------------
// ------------------

procedure TgxQueryHandle.BeginQuery;
begin
  if vCurrentContext.gxStates.CurrentQuery[QueryType] = 0 then
    vCurrentContext.gxStates.BeginQuery(QueryType, GetHandle);
  FActive := True;
end;

function TgxQueryHandle.CounterBits: Integer;
begin
  glGetQueryiv(Target, GL_QUERY_COUNTER_BITS, @Result);
end;

function TgxQueryHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenQueries(1, @Result);
end;

procedure TgxQueryHandle.DoDestroyHandle(var AHandle: LongWord);
begin
  if not vContextActivationFailureOccurred then
  begin
    glDeleteQueries(1, @AHandle);
    ///CheckOpenGLError;
  end;
end;

class function TgxQueryHandle.IsValid(const ID: LongWord): BYTEBOOL;
begin
  Result := ByteBool(glIsQuery(ID));
end;

procedure TgxQueryHandle.EndQuery;
begin
  Assert(FActive = True, 'Cannot end a query before it begins');
  FActive := False;
  Assert(Handle <> 0);
  // glEndQuery(Target);
  vCurrentContext.gxStates.EndQuery(QueryType);
end;

function TgxQueryHandle.IsResultAvailable: Boolean;
begin
  glGetQueryObjectiv(Handle, GL_QUERY_RESULT_AVAILABLE, @Result);
end;

function TgxQueryHandle.QueryResultInt: GLint;
begin
  glGetQueryObjectiv(Handle, GL_QUERY_RESULT, @Result);
end;

function TgxQueryHandle.QueryResultInt64: Int64;
begin
  glGetQueryObjecti64v(Handle, GL_QUERY_RESULT, @Result);
end;

function TgxQueryHandle.QueryResultUInt: LongWord;
begin
  glGetQueryObjectuiv(Handle, GL_QUERY_RESULT, @Result);
end;

function TgxQueryHandle.QueryResultUInt64: UInt64;
begin
  glGetQueryObjectui64v(Handle, GL_QUERY_RESULT, @Result);
end;

function TgxQueryHandle.QueryResultBool: BYTEBOOL;
var
  I: LongWord;
begin
  glGetQueryObjectuiv(Handle, GL_QUERY_RESULT, @I);
  Result := True;
end;

class function TgxQueryHandle.Transferable: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TgxOcclusionQueryHandle ------------------
// ------------------

function TgxOcclusionQueryHandle.GetQueryType: TgxQueryType;
begin
  Result := qrySamplesPassed;
end;

function TgxOcclusionQueryHandle.GetTarget: LongWord;
begin
  Result := GL_SAMPLES_PASSED;
end;

class function TgxOcclusionQueryHandle.IsSupported: Boolean;
begin
  Result := True;
end;

function TgxOcclusionQueryHandle.PixelCount: Integer;
begin
  Result := QueryResultUInt;
end;

// ------------------
// ------------------ TgxBooleanOcclusionQueryHandle ------------------
// ------------------

function TgxBooleanOcclusionQueryHandle.GetQueryType: TgxQueryType;
begin
  Result := qryAnySamplesPassed;
end;

function TgxBooleanOcclusionQueryHandle.GetTarget: LongWord;
begin
  Result := GL_ANY_SAMPLES_PASSED;
end;

class function TgxBooleanOcclusionQueryHandle.IsSupported: Boolean;
begin
  Result := True;
end;

// ------------------
// ------------------ TgxTimerQueryHandle ------------------
// ------------------

function TgxTimerQueryHandle.GetQueryType: TgxQueryType;
begin
  Result := qryTimeElapsed;
end;

function TgxTimerQueryHandle.GetTarget: LongWord;
begin
  Result := GL_TIME_ELAPSED;
end;

class function TgxTimerQueryHandle.IsSupported: Boolean;
begin
  Result := True;
end;

function TgxTimerQueryHandle.Time: Integer;
begin
  Result := QueryResultUInt;
end;

// ------------------
// ------------------ TgxPrimitiveQueryHandle ------------------
// ------------------

function TgxPrimitiveQueryHandle.GetQueryType: TgxQueryType;
begin
  Result := qryPrimitivesGenerated;
end;

function TgxPrimitiveQueryHandle.GetTarget: LongWord;
begin
  Result := GL_PRIMITIVES_GENERATED;
end;

class function TgxPrimitiveQueryHandle.IsSupported: Boolean;
begin
  Result := True;
end;

function TgxPrimitiveQueryHandle.PrimitivesGenerated: Integer;
begin
  Result := QueryResultUInt;
end;

// ------------------
// ------------------ TgxBufferObjectHandle ------------------
// ------------------

constructor TgxBufferObjectHandle.CreateFromData(p: Pointer; size: Integer; bufferUsage: LongWord);
begin
  Create;
  AllocateHandle;
  Bind;
  BufferData(p, size, bufferUsage);
  UnBind;
end;

function TgxBufferObjectHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenBuffers(1, @Result);
end;

procedure TgxBufferObjectHandle.DoDestroyHandle(var AHandle: LongWord);
begin
  if not vContextActivationFailureOccurred then
  begin
    glGetError;
    UnBind;
    glDeleteBuffers(1, @AHandle);
    ///CheckOpenGLError;
  end;
end;

class function TgxBufferObjectHandle.IsValid(const ID: LongWord): BYTEBOOL;
begin
  Result := ByteBool(glIsBuffer(ID));
end;

class function TgxBufferObjectHandle.IsSupported: Boolean;
begin
  Result := True;
end;

procedure TgxBufferObjectHandle.BindRange(index: LongWord; offset: PGLint; size: PGLsizei);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

procedure TgxBufferObjectHandle.BindBase(index: LongWord);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

procedure TgxBufferObjectHandle.UnBindBase(index: LongWord);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

procedure TgxBufferObjectHandle.BufferData(p: Pointer; size: Integer; bufferUsage: LongWord);
begin
  FSize := size;
  glBufferData(Target, size, p, bufferUsage);
end;

procedure TgxBufferObjectHandle.BindBufferData(p: Pointer; size: Integer; bufferUsage: LongWord);
begin
  Bind;
  FSize := size;
  glBufferData(Target, size, p, bufferUsage);
end;

procedure TgxBufferObjectHandle.BufferSubData(offset, size: Integer; p: Pointer);
begin
  Assert(offset + size <= FSize);
  glBufferSubData(Target, offset, size, p);
end;

function TgxBufferObjectHandle.MapBuffer(access: LongWord): Pointer;
begin
  Result := glMapBuffer(Target, access);
end;

function TgxBufferObjectHandle.MapBufferRange(offset: GLint; len: GLsizei; access: GLbitfield): Pointer;
begin
  Result := glMapBufferRange(Target, offset, len, access);
end;

procedure TgxBufferObjectHandle.Flush(offset: GLint; len: GLsizei);
begin
  glFlushMappedBufferRange(Target, offset, len);
end;

function TgxBufferObjectHandle.UnmapBuffer: BYTEBOOL;
begin
  Result := ByteBool(glUnmapBuffer(Target));
end;

// ------------------
// ------------------ TgxVBOHandle ------------------
// ------------------

function TgxVBOHandle.GetVBOTarget: LongWord;
begin
  Result := Target;
end;

// ------------------
// ------------------ TgxVBOArrayBufferHandle ------------------
// ------------------

procedure TgxVBOArrayBufferHandle.Bind;
begin
  vCurrentContext.gxStates.ArrayBufferBinding := Handle;
end;

procedure TgxVBOArrayBufferHandle.UnBind;
begin
  vCurrentContext.gxStates.ArrayBufferBinding := 0;
end;

function TgxVBOArrayBufferHandle.GetTarget: LongWord;
begin
  Result := GL_ARRAY_BUFFER;
end;

// ------------------
// ------------------ TgxVBOElementArrayHandle ------------------
// ------------------

procedure TgxVBOElementArrayHandle.Bind;
begin
  vCurrentContext.gxStates.ElementBufferBinding := Handle;
end;

procedure TgxVBOElementArrayHandle.UnBind;
begin
  vCurrentContext.gxStates.ElementBufferBinding := 0;
end;

function TgxVBOElementArrayHandle.GetTarget: LongWord;
begin
  Result := GL_ELEMENT_ARRAY_BUFFER;
end;

// ------------------
// ------------------ TgxPackPBOHandle ------------------
// ------------------

procedure TgxPackPBOHandle.Bind;
begin
  vCurrentContext.gxStates.PixelPackBufferBinding := Handle;
end;

procedure TgxPackPBOHandle.UnBind;
begin
  vCurrentContext.gxStates.PixelPackBufferBinding := 0;
end;

function TgxPackPBOHandle.GetTarget: LongWord;
begin
  Result := GL_PIXEL_PACK_BUFFER;
end;

class function TgxPackPBOHandle.IsSupported: Boolean;
begin
  Result := True;
end;

// ------------------
// ------------------ TgxUnpackPBOHandle ------------------
// ------------------

procedure TgxUnpackPBOHandle.Bind;
begin
  vCurrentContext.gxStates.PixelUnpackBufferBinding := Handle;
end;

procedure TgxUnpackPBOHandle.UnBind;
begin
  vCurrentContext.gxStates.PixelUnpackBufferBinding := 0;
end;

function TgxUnpackPBOHandle.GetTarget: LongWord;
begin
  Result := GL_PIXEL_UNPACK_BUFFER;
end;

class function TgxUnpackPBOHandle.IsSupported: Boolean;
begin
  Result := True;
end;

// ------------------
// ------------------ TgxTransformFeedbackBufferHandle ------------------
// ------------------

procedure TgxTransformFeedbackBufferHandle.Bind;
begin
  vCurrentContext.gxStates.TransformFeedbackBufferBinding := Handle;
end;

procedure TgxTransformFeedbackBufferHandle.UnBind;
begin
  vCurrentContext.gxStates.TransformFeedbackBufferBinding := 0;
end;

function TgxTransformFeedbackBufferHandle.GetTarget: LongWord;
begin
  Result := GL_TRANSFORM_FEEDBACK_BUFFER;
end;

procedure TgxTransformFeedbackBufferHandle.BeginTransformFeedback(primitiveMode: GLenum);
begin
  glBeginTransformFeedback(primitiveMode);
end;

procedure TgxTransformFeedbackBufferHandle.EndTransformFeedback();
begin
  glEndTransformFeedback();
end;

procedure TgxTransformFeedbackBufferHandle.BindRange(index: LongWord; offset: PGLint; size: PGLsizei);
begin
///  vCurrentContext.gxStates.SetBufferIndexedBinding(Handle, bbtTransformFeedBack, index, offset, size);
end;

procedure TgxTransformFeedbackBufferHandle.BindBase(index: LongWord);
begin
  vCurrentContext.gxStates.SetBufferIndexedBinding(Handle, bbtTransformFeedBack, index, @BufferSize);
end;

procedure TgxTransformFeedbackBufferHandle.UnBindBase(index: LongWord);
begin
  vCurrentContext.gxStates.SetBufferIndexedBinding(0, bbtTransformFeedBack, index, 0);
end;

class function TgxTransformFeedbackBufferHandle.IsSupported: Boolean;
begin
  Result := True;
end;

// ------------------
// ------------------ TgxTextureBufferHandle ------------------
// ------------------

procedure TgxTextureBufferHandle.Bind;
begin
  vCurrentContext.gxStates.TextureBufferBinding := Handle;
end;

procedure TgxTextureBufferHandle.UnBind;
begin
  vCurrentContext.gxStates.TextureBufferBinding := 0;
end;

function TgxTextureBufferHandle.GetTarget: LongWord;
begin
  Result := GL_TEXTURE_BUFFER;
end;

class function TgxTextureBufferHandle.IsSupported: Boolean;
begin
  Result := True; // GL_VERSION_4_6;
end;

// ------------------
// ------------------ TgxUniformBufferHandle ------------------
// ------------------

procedure TgxUniformBufferHandle.Bind;
begin
  vCurrentContext.gxStates.UniformBufferBinding := Handle;
end;

procedure TgxUniformBufferHandle.UnBind;
begin
  vCurrentContext.gxStates.UniformBufferBinding := 0;
end;

procedure TgxUniformBufferHandle.BindRange(index: LongWord; offset: PGLint; size: PGLsizei);
begin
//  vCurrentContext.gxStates.SetBufferIndexedBinding(Handle, bbtUniform, index, offset, size);
  vCurrentContext.gxStates.SetBufferIndexedBinding(Handle, bbtUniform, index,
    GLInt(offset), @size);
end;

procedure TgxUniformBufferHandle.BindBase(index: LongWord);
begin
  vCurrentContext.gxStates.SetBufferIndexedBinding(Handle, bbtUniform, index, @BufferSize);
end;

procedure TgxUniformBufferHandle.UnBindBase(index: LongWord);
begin
  vCurrentContext.gxStates.SetBufferIndexedBinding(0, bbtUniform, index, 0);
end;

function TgxUniformBufferHandle.GetTarget: LongWord;
begin
  Result := GL_UNIFORM_BUFFER;
end;

class function TgxUniformBufferHandle.IsSupported: Boolean;
begin
  Result := True; // GL_ARB_uniform_buffer_object;
end;

// ------------------
// ------------------ TgxVertexArrayHandle ------------------
// ------------------

function TgxVertexArrayHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenVertexArrays(1, @Result);
end;

procedure TgxVertexArrayHandle.DoDestroyHandle(var AHandle: LongWord);
begin
  if not vContextActivationFailureOccurred then
  begin
    glGetError;
    glDeleteVertexArrays(1, @AHandle);
    //CheckOpenGLError;
  end;
end;

class function TgxVertexArrayHandle.IsValid(const ID: LongWord): BYTEBOOL;
begin
  Result := ByteBool(glIsVertexArray(ID));
end;

procedure TgxVertexArrayHandle.Bind;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.gxStates.VertexArrayBinding := Handle;
end;

procedure TgxVertexArrayHandle.UnBind;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.gxStates.VertexArrayBinding := 0;
end;

class function TgxVertexArrayHandle.IsSupported: Boolean;
begin
  Result := True;
end;

class function TgxVertexArrayHandle.Transferable: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TgxFramebufferHandle ------------------
// ------------------

function TgxFramebufferHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenFramebuffers(1, @Result)
end;

procedure TgxFramebufferHandle.DoDestroyHandle(var AHandle: LongWord);
begin
  if not vContextActivationFailureOccurred then
  begin
    glGetError;
    glDeleteFramebuffers(1, @AHandle);
    // CheckError;
  end;
end;

class function TgxFramebufferHandle.IsValid(const ID: LongWord): BYTEBOOL;
begin
  Result := ByteBool(glIsFramebuffer(ID));
end;

procedure TgxFramebufferHandle.Bind;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.gxStates.SetFrameBuffer(Handle);
end;

procedure TgxFramebufferHandle.BindForDrawing;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.gxStates.DrawFrameBuffer := Handle;
end;

procedure TgxFramebufferHandle.BindForReading;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.gxStates.ReadFrameBuffer := Handle;
end;

procedure TgxFramebufferHandle.UnBind;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.gxStates.SetFrameBuffer(0);
end;

procedure TgxFramebufferHandle.UnBindForDrawing;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.gxStates.DrawFrameBuffer := 0;
end;

procedure TgxFramebufferHandle.UnBindForReading;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.gxStates.ReadFrameBuffer := 0;
end;

procedure TgxFramebufferHandle.Attach1DTexture(Target: GLenum; attachment: GLenum; textarget: GLenum; texture: LongWord;
  level: GLint);
begin
  glFramebufferTexture1D(Target, attachment, textarget, texture, level);
end;

procedure TgxFramebufferHandle.Attach2DTexture(Target: GLenum; attachment: GLenum; textarget: GLenum; texture: LongWord;
  level: GLint);
begin
  glFramebufferTexture2D(Target, attachment, textarget, texture, level);
end;

procedure TgxFramebufferHandle.Attach3DTexture(Target: GLenum; attachment: GLenum; textarget: GLenum; texture: LongWord;
  level: GLint; Layer: GLint);
begin
  glFramebufferTexture3D(Target, attachment, textarget, texture, level, Layer);
end;

procedure TgxFramebufferHandle.AttachLayer(Target: GLenum; attachment: GLenum; texture: LongWord; level: GLint; Layer: GLint);
begin
  glFramebufferTextureLayer(Target, attachment, texture, level, Layer);
end;

procedure TgxFramebufferHandle.AttachRenderBuffer(Target: GLenum; attachment: GLenum; renderbuffertarget: GLenum;
  renderbuffer: LongWord);
begin
  glFramebufferRenderbuffer(Target, attachment, renderbuffertarget, renderbuffer);
end;

procedure TgxFramebufferHandle.AttachTexture(Target: GLenum; attachment: GLenum; texture: LongWord; level: GLint);
begin
  glFramebufferTexture(Target, attachment, texture, level);
end;

procedure TgxFramebufferHandle.AttachTextureLayer(Target: GLenum; attachment: GLenum; texture: LongWord; level: GLint;
  Layer: GLint);
begin
  glFramebufferTextureLayer(Target, attachment, texture, level, Layer);
end;

procedure TgxFramebufferHandle.Blit(srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint; dstX0: GLint; dstY0: GLint;
  dstX1: GLint; dstY1: GLint; mask: GLbitfield; filter: GLenum);
begin
  glBlitFramebuffer(srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1, mask, filter);
end;

function TgxFramebufferHandle.GetAttachmentParameter(Target: GLenum; attachment: GLenum; pname: GLenum): GLint;
begin
  glGetFramebufferAttachmentParameteriv(Target, attachment, pname, @Result)
end;

function TgxFramebufferHandle.GetAttachmentObjectType(Target: GLenum; attachment: GLenum): GLint;
begin
  glGetFramebufferAttachmentParameteriv(Target, attachment, GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE, @Result);
end;

function TgxFramebufferHandle.GetAttachmentObjectName(Target: GLenum; attachment: GLenum): GLint;
begin
  glGetFramebufferAttachmentParameteriv(Target, attachment, GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME, @Result);
end;

function TgxFramebufferHandle.GetStatus: TgxFramebufferStatus;
var
  Status: Cardinal;
begin
  Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);

  case Status of
    GL_FRAMEBUFFER_COMPLETE_EXT:
      Result := fsComplete;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT:
      Result := fsIncompleteAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT:
      Result := fsIncompleteMissingAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT:
      Result := fsIncompleteDimensions;
    GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT:
      Result := fsIncompleteFormats;
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT:
      Result := fsIncompleteDrawBuffer;
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT:
      Result := fsIncompleteReadBuffer;
    GL_FRAMEBUFFER_UNSUPPORTED_EXT:
      Result := fsUnsupported;
    GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE:
      Result := fsIncompleteMultisample;
  else
    Result := fsStatusError;
  end;
end;

function TgxFramebufferHandle.GetStringStatus(out clarification: string): TgxFramebufferStatus;
const
  cFBOStatus: array [TgxFramebufferStatus] of string = ('Complete', 'Incomplete attachment', 'Incomplete missing attachment',
    'IncompleteDuplicateAttachment', 'Incomplete dimensions', 'Incomplete formats', 'Incomplete draw buffer',
    'Incomplete read buffer', 'Unsupported', 'Incomplite multisample', 'Status Error');
begin
  Result := GetStatus;
  clarification := cFBOStatus[Result];
end;

class function TgxFramebufferHandle.IsSupported: Boolean;
begin
  Result := True; // GL_EXT_framebuffer_object or GL_ARB_framebuffer_object;
end;

class function TgxFramebufferHandle.Transferable: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TgxRenderbufferObject ------------------
// ------------------

function TgxRenderbufferHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenRenderbuffers(1, @Result);
end;

procedure TgxRenderbufferHandle.DoDestroyHandle(var AHandle: LongWord);
begin
  if not vContextActivationFailureOccurred then
  begin
    glGetError;
    glDeleteRenderbuffers(1, @AHandle);
    // CheckOpenGLError;
  end;
end;

class function TgxRenderbufferHandle.IsValid(const ID: LongWord): BYTEBOOL;
begin
  Result := ByteBool(glIsRenderbuffer(ID));
end;

procedure TgxRenderbufferHandle.Bind;
begin
  vCurrentContext.gxStates.renderbuffer := GetHandle;
end;

procedure TgxRenderbufferHandle.UnBind;
begin
  if vCurrentContext <> nil then
    vCurrentContext.gxStates.renderbuffer := 0;
end;

procedure TgxRenderbufferHandle.SetStorage(internalformat: GLenum; Width, Height: GLsizei);
begin
  glRenderbufferStorage(GL_RENDERBUFFER, internalformat, Width, Height);
end;

procedure TgxRenderbufferHandle.SetStorageMultisample(internalformat: GLenum; samples: GLsizei; Width, Height: GLsizei);
begin
  glRenderbufferStorageMultisample(GL_RENDERBUFFER, samples, internalformat, Width, Height);
end;

class function TgxRenderbufferHandle.IsSupported: Boolean;
begin
  Result := True; //GL_EXT_framebuffer_object or GL_ARB_framebuffer_object;
end;

// ------------------
// ------------------ TgxProgramHandleEXT ------------------
// ------------------

function TgxProgramHandleEXT.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenProgramsARB(1, @Result);
  FReady := False;
end;

procedure TgxProgramHandleEXT.DoDestroyHandle(var AHandle: LongWord);
begin
  if not vContextActivationFailureOccurred then
  begin
    glGetError;
    glDeleteProgramsARB(1, @AHandle);
    // CheckOpenGLError;
  end;
end;

class function TgxProgramHandleEXT.IsValid(const ID: LongWord): BYTEBOOL;
begin
  Result := ByteBool(glIsProgram(ID));
end;

procedure TgxProgramHandleEXT.LoadARBProgram(AText: string);
const
  cProgType: array [0 .. 2] of string = ('ARB vertex', 'ARB fragment', 'NV geometry');
var
  errPos, p: Integer;
begin
  Bind;
  glProgramStringARB(GetTarget, GL_PROGRAM_FORMAT_ASCII_ARB, Length(AText), PGLChar(AText));
  glGetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, @errPos);
  if errPos > -1 then
  begin
    FInfoLog := string(glGetString(GL_PROGRAM_ERROR_STRING_ARB));
    case GetTarget of
      GL_VERTEX_PROGRAM_ARB:
        p := 0;
      GL_FRAGMENT_PROGRAM_ARB:
        p := 1;
    else
      p := 2;
    end;
    /// ShowMessages(Format('%s Program Error - [Pos: %d][Error %s]', [cProgType[P], errPos, FInfoLog]));
    FReady := False;
  end
  else
  begin
    FReady := True;
    FInfoLog := '';
  end;
end;

procedure TgxProgramHandleEXT.Enable;
begin
  if FReady then
    glEnable(GetTarget)
  else
    Abort;
end;

procedure TgxProgramHandleEXT.Disable;
begin
  glDisable(GetTarget);
end;

procedure TgxProgramHandleEXT.Bind;
begin
  glBindProgramARB(GetTarget, Handle);
end;

class function TgxVertexProgramHandle.GetTarget: GLenum;
begin
  Result := GL_VERTEX_PROGRAM_ARB;
end;

class function TgxVertexProgramHandle.IsSupported: Boolean;
begin
  Result := True; // GL_ARB_vertex_program;
end;

class function TgxFragmentProgramHandle.GetTarget: GLenum;
begin
  Result := GL_FRAGMENT_PROGRAM_ARB;
end;

class function TgxFragmentProgramHandle.IsSupported: Boolean;
begin
  Result := True; // GL_ARB_vertex_program;
end;

class function TgxGeometryProgramHandle.GetTarget: GLenum;
begin
  Result := GL_GEOMETRY_PROGRAM_NV;
end;

class function TgxGeometryProgramHandle.IsSupported: Boolean;
begin
  Result := True; // GL_NV_geometry_program4;
end;

// ------------------
// ------------------ TgxSLHandle ------------------
// ------------------

procedure TgxSLHandle.DoDestroyHandle(var AHandle: LongWord);
begin
  if not vContextActivationFailureOccurred then
  begin
    glGetError;
    glDeleteObjectARB(@AHandle);
    // CheckError;
  end;
end;

function TgxSLHandle.InfoLog: string;
var
  maxLength: Integer;
  log: String;
  AHandle: LongWord;
begin
  maxLength := 0;
  AHandle := GetHandle;
  glGetObjectParameterivARB(@AHandle, GL_OBJECT_INFO_LOG_LENGTH_ARB, @maxLength);
  SetLength(log, maxLength);
  if maxLength > 0 then
  begin
    glGetInfoLogARB(@AHandle, maxLength, @maxLength, @log[1]);
    SetLength(log, maxLength);
  end;
  Result := string(log);
end;

class function TgxSLHandle.IsSupported: Boolean;
begin
  Result := True; //GL_ARB_shader_objects;
end;

// ------------------
// ------------------ TgxShaderHandle ------------------
// ------------------

function TgxShaderHandle.DoAllocateHandle: LongWord;
begin
  Result := glCreateShader(FShaderType)
end;

class function TgxShaderHandle.IsValid(const ID: LongWord): BYTEBOOL;
begin
  Result := ByteBool(glIsShader(ID));
end;

procedure TgxShaderHandle.ShaderSource(const source: AnsiString);
var
  p: PGLChar;
begin
  p := PGLChar(source);
  glShaderSource(GetHandle, 1, @p, nil);
end;

function TgxShaderHandle.CompileShader: Boolean;
var
  compiled: Integer;
  glH: LongWord;
begin
  glH := GetHandle;
  glCompileShader(glH);
  compiled := 0;
  glGetShaderiv(glH, GL_COMPILE_STATUS, @compiled);
  Result := (compiled <> 0);
end;

// ------------------
// ------------------ TgxVertexShaderHandle ------------------
// ------------------

constructor TgxVertexShaderHandle.Create;
begin
  FShaderType := GL_VERTEX_SHADER_ARB;
  inherited;
end;

class function TgxVertexShaderHandle.IsSupported: Boolean;
begin
  Result := True; // GL_ARB_vertex_shader;
end;

// ------------------
// ------------------ TgxGeometryShaderHandle ------------------
// ------------------

constructor TgxGeometryShaderHandle.Create;
begin
  FShaderType := GL_GEOMETRY_SHADER_EXT;
  inherited;
end;

class function TgxGeometryShaderHandle.IsSupported: Boolean;
begin
  Result := True; //GL_EXT_geometry_shader4;
end;

// ------------------
// ------------------ TgxFragmentShaderHandle ------------------
// ------------------

constructor TgxFragmentShaderHandle.Create;
begin
  FShaderType := GL_FRAGMENT_SHADER_ARB;
  inherited;
end;

class function TgxFragmentShaderHandle.IsSupported: Boolean;
begin
  Result := True; // GL_ARB_fragment_shader;
end;

// ------------------
// ------------------ TgxTessControlShaderHandle ------------------
// ------------------

constructor TgxTessControlShaderHandle.Create;
begin
  FShaderType := GL_TESS_CONTROL_SHADER;
  inherited;
end;

class function TgxTessControlShaderHandle.IsSupported: Boolean;
begin
  Result := True; // GL_ARB_tessellation_shader;
end;

// ------------------
// ------------------ TgxTessEvaluationShaderHandle ------------------
// ------------------

constructor TgxTessEvaluationShaderHandle.Create;
begin
  FShaderType := GL_TESS_EVALUATION_SHADER;
  inherited;
end;

class function TgxTessEvaluationShaderHandle.IsSupported: Boolean;
begin
  Result := True; // GL_ARB_tessellation_shader;
end;

// ------------------
// ------------------ TgxProgramHandle ------------------
// ------------------

function TgxProgramHandle.DoAllocateHandle: LongWord;
begin
  Result := glCreateProgram();
end;

class function TgxProgramHandle.IsValid(const ID: LongWord): BYTEBOOL;
begin
  Result := ByteBool(glIsProgram(ID));
end;

procedure TgxProgramHandle.AddShader(ShaderType: TgxShaderHandleClass; const ShaderSource: string;
  treatWarningsAsErrors: Boolean = False);
var
  shader: TgxShaderHandle;
begin
  shader := ShaderType.CreateAndAllocate;
  try
    if shader.Handle = 0 then
      raise EVXShader.Create('Couldn''t allocate ' + ShaderType.ClassName);
    shader.ShaderSource(AnsiString(ShaderSource));
    if (not shader.CompileShader) or (treatWarningsAsErrors and (Pos('warning', LowerCase(shader.InfoLog)) > 0)) then
      raise EVXShader.Create(FName + ' (' + shader.ClassName + '): '#13#10 + shader.InfoLog);
    AttachObject(shader);
  finally
    shader.Free;
  end;
  glGetError;
end;

procedure TgxProgramHandle.AttachObject(shader: TgxShaderHandle);
begin
  glAttachShader(GetHandle, shader.Handle);
end;

procedure TgxProgramHandle.DetachAllObject;
var
  glH: Cardinal;
  I: Integer;
  Count: Integer;
  buffer: array [0 .. 255] of Cardinal;
begin
  glH := GetHandle;
  if glH > 0 then
  begin
    glGetAttachedShaders(glH, Length(buffer), @Count, @buffer[0]);
    Count := MinInteger(Count, Length(buffer));
    for I := 0 to Count - 1 do
      glDetachShader(glH, buffer[I]);
    NotifyChangesOfData;
  end;
end;

procedure TgxProgramHandle.BindAttribLocation(index: Integer; const aName: string);
begin
  glBindAttribLocation(GetHandle, index, PGLChar(aName));
end;

procedure TgxProgramHandle.BindFragDataLocation(index: Integer; const aName: string);
begin
  glBindFragDataLocation(GetHandle, index, PGLChar(name));
end;

function TgxProgramHandle.LinkProgram: Boolean;
var
  Status: Integer;
  glH: Cardinal;
begin
  glH := GetHandle;
  glLinkProgram(glH);
  Status := 0;
  glGetProgramiv(glH, GL_LINK_STATUS, @Status);
  Result := (Status <> 0);
end;

function TgxProgramHandle.ValidateProgram: Boolean;
var
  validated: Integer;
  h: Cardinal;
begin
  h := GetHandle;
  glValidateProgram(h);
  validated := 0;
  glGetProgramiv(h, GL_VALIDATE_STATUS, @validated);
  Result := (validated <> 0);
end;

function TgxProgramHandle.GetAttribLocation(const aName: string): Integer;
begin
  Result := glGetAttribLocation(GetHandle, PGLChar(aName));
  Assert(Result >= 0, Format(strUnknownParam, ['attrib', aName, Name]));
end;

function TgxProgramHandle.GetUniformLocation(const aName: string): Integer;
begin
  Result := glGetUniformLocation(GetHandle, PGLChar(aName));
  Assert(Result >= 0, Format(strUnknownParam, ['uniform', aName, Name]));
end;

function TgxProgramHandle.GetVaryingLocation(const aName: string): Integer;
begin
  Result := glGetVaryingLocationNV(GetHandle, PGLChar(aName));
  Assert(Result >= 0, Format(strUnknownParam, ['varying', aName, Name]));
end;

procedure TgxProgramHandle.AddActiveVarying(const aName: string);
begin
  glActiveVaryingNV(GetHandle, PGLChar(aName));
end;

procedure TgxProgramHandle.UseProgramObject;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.gxStates.CurrentProgram := Handle;
end;

procedure TgxProgramHandle.EndUseProgramObject;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.gxStates.CurrentProgram := 0;
end;

function TgxProgramHandle.GetUniform1i(const index: string): Integer;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

function TgxProgramHandle.GetUniform2i(const index: string): TVector2i;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

function TgxProgramHandle.GetUniform3i(const index: string): TVector3i;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

function TgxProgramHandle.GetUniform4i(const index: string): TVector4i;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TgxProgramHandle.SetUniform1f(const index: string; val: Single);
begin
  glUniform1f(GetUniformLocation(index), val);
end;

function TgxProgramHandle.GetUniform1f(const index: string): Single;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TgxProgramHandle.SetUniform1i(const index: string; val: Integer);
begin
  glUniform1i(GetUniformLocation(index), val);
end;

procedure TgxProgramHandle.SetUniform2i(const index: string; const Value: TVector2i);
begin
  glUniform2i(GetUniformLocation(index), Value.X, Value.Y);
end;

procedure TgxProgramHandle.SetUniform3i(const index: string; const Value: TVector3i);
begin
  glUniform3i(GetUniformLocation(index), Value.X, Value.Y, Value.Z);
end;

procedure TgxProgramHandle.SetUniform4i(const index: string; const Value: TVector4i);
begin
  glUniform4i(GetUniformLocation(index), Value.X, Value.Y, Value.Z, Value.W);
end;

function TgxProgramHandle.GetUniform2f(const index: string): TVector2f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TgxProgramHandle.SetUniform2f(const index: string; const val: TVector2f);
begin
  glUniform2f(GetUniformLocation(index), val.X, val.Y);
end;

function TgxProgramHandle.GetUniform3f(const index: string): TAffineVector;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TgxProgramHandle.SetUniform3f(const index: string; const val: TAffineVector);
begin
  glUniform3f(GetUniformLocation(index), val.X, val.Y, val.Z);
end;

function TgxProgramHandle.GetUniform4f(const index: string): TVector4f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TgxProgramHandle.SetUniform4f(const index: string; const val: TVector4f);
begin
  glUniform4f(GetUniformLocation(index), val.X, val.Y, val.Z, val.W);
end;

function TgxProgramHandle.GetUniformMatrix2fv(const index: string): TMatrix2f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TgxProgramHandle.SetUniformMatrix2fv(const index: string; const val: TMatrix2f);
begin
  glUniformMatrix2fv(GetUniformLocation(index), 1,  0, @val);
end;

function TgxProgramHandle.GetUniformMatrix3fv(const index: string): TMatrix3f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TgxProgramHandle.SetUniformMatrix3fv(const index: string; const val: TMatrix3f);
begin
  glUniformMatrix3fv(GetUniformLocation(index), 1,  0, @val);
end;

function TgxProgramHandle.GetUniformMatrix4fv(const index: string): TMatrix4f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TgxProgramHandle.SetUniformMatrix4fv(const index: string; const val: TMatrix4f);
begin
  glUniformMatrix4fv(GetUniformLocation(index), 1,  0, @val);
end;

procedure TgxProgramHandle.SetUniformf(const index: string; const val: Single);
begin
  SetUniform1f(index, val);
end;

procedure TgxProgramHandle.SetUniformf(const index: string; const val: TVector2f);
begin
  SetUniform2f(index, val);
end;

procedure TgxProgramHandle.SetUniformf(const index: string; const val: TVector3f);
begin
  SetUniform3f(index, val);
end;

procedure TgxProgramHandle.SetUniformf(const index: string; const val: TVector4f);
begin
  SetUniform4f(index, val);
end;

procedure TgxProgramHandle.SetUniformi(const index: string; const val: Integer);
begin
  SetUniform1f(index, val);
end;

procedure TgxProgramHandle.SetUniformi(const index: string; const val: TVector2i);
begin
  SetUniform2i(index, val);
end;

procedure TgxProgramHandle.SetUniformi(const index: string; const val: TVector3i);
begin
  SetUniform3i(index, val);
end;

procedure TgxProgramHandle.SetUniformi(const index: string; const val: TVector4i);
begin
  SetUniform4i(index, val);
end;

function TgxProgramHandle.GetUniformTextureHandle(const index: string; const TextureIndex: Integer;
  const TextureTarget: TglTextureTarget): Cardinal;
begin
  Result := GetUniform1i(index);
end;

procedure TgxProgramHandle.SetUniformTextureHandle(const index: string; const TextureIndex: Integer;
  const TextureTarget: TglTextureTarget; const Value: LongWord);
begin
  vCurrentContext.gxStates.TextureBinding[0, TextureTarget] := Value;
  SetUniform1i(index, TextureIndex);
end;

procedure TgxProgramHandle.SetUniformBuffer(const index: string; Value: TgxUniformBufferHandle);
begin
  glUniformBufferEXT(Handle, GetUniformLocation(index), Value.Handle);
end;

function TgxProgramHandle.GetUniformBufferSize(const aName: string): GLint;
begin
  Result := glGetUniformBufferSizeEXT(Handle, GetUniformLocation(aName));
end;

function TgxProgramHandle.GetUniformOffset(const aName: string): GLint;
begin
  Result := glGetUniformOffsetEXT(Handle, GetUniformLocation(aName));
end;

function TgxProgramHandle.GetUniformBlockIndex(const aName: string): Integer;
begin
  Result := glGetUniformBlockIndex(Handle, PGLChar(aName));
  Assert(Result >= 0, Format(strUnknownParam, ['uniform block', aName, Name]));
end;

constructor TgxProgramHandle.Create;
begin
  inherited Create;
  FName := 'DefaultShaderName';
end;

// ------------------
// ------------------ TgxContextManager ------------------
// ------------------

constructor TgxContextManager.Create;
begin
  inherited Create;
  FHandles := TThreadList.Create;
  FList := TThreadList.Create;
end;

destructor TgxContextManager.Destroy;
begin
  FHandles.Free;
  FList.Free;
  inherited Destroy;
end;

function TgxContextManager.CreateContext(AClass: TgxContextClass): TgxContext;
begin
  if Assigned(AClass) then
  begin
    Result := AClass.Create;
    Result.FManager := Self;
  end
  else if Assigned(vContextClasses) and (vContextClasses.Count > 0) then
  begin
    Result := TgxContextClass(vContextClasses.Last).Create;
    Result.FManager := Self;
  end
  else
    Result := nil;
end;

{$IFDEF USE_SERVICE_CONTEXT}
procedure TgxContextManager.CreateContext(AClass: TgxContextClass): TgxContext;;
begin
  FServiceContext := CreateContext;
  FThreadTask := TgxServiceContextTaskList.Create;
  FServiceStarter := TFinishTaskEvent.Create;
  FThread := TServiceContextThread.Create;
  AddTaskForServiceContext(TServiceContextThread(FThread).DoCreateServiceContext);
end;

procedure TgxContextManager.QueueTaskDepleted;
var
  TaskRec: TServiceContextTask;
  I: Integer;
  nowTime: Double;
begin
  with FThreadTask.LockList do
    try
      for I := 0 to Count - 1 do
      begin
        TaskRec := Items[I];
        if Assigned(TaskRec.Task) then
        begin
          FThreadTask.UnlockList;
          // Task queue not empty
          FServiceStarter.SetEvent;
          exit;
        end;
      end;
    finally
      FThreadTask.UnlockList;
    end;

  FServiceStarter.ResetEvent;
  FThreadTask.Clear;
  nowTime := Now;
  with TServiceContextThread(FThread) do
    if (nowTime - FLastTaskStartTime > 30000) and not FReported then
    begin
      FReported := True;
      ShowMessage('Service context queue task depleted');
    end;
end;

{$ENDIF}

procedure TgxContextManager.Lock;
begin
  FList.LockList;
end;

procedure TgxContextManager.NotifyPreparationNeed;
var
  I: Integer;
  LList: TList;
begin
  LList := FList.LockList;
  try
    for I := LList.Count - 1 downto 0 do
      TgxContext(LList[I]).FIsPraparationNeed := True;
  finally
    FList.UnlockList;
  end;
end;

procedure TgxContextManager.UnLock;
begin
  FList.UnlockList;
end;

function TgxContextManager.ContextCount: Integer;
begin
  // try..finally just a waste of CPU here, if Count fails, the list is amok,
  // and so is the lock...
  Result := FList.LockList.Count;
  FList.UnlockList;
end;

procedure TgxContextManager.RegisterContext(aContext: TgxContext);
begin
  with FList.LockList do
    try
      if IndexOf(aContext) >= 0 then
        raise EVXContext.Create(strInvalidContextRegistration)
      else
        Add(aContext);
    finally
      FList.UnlockList;
    end;
end;

procedure TgxContextManager.UnRegisterContext(aContext: TgxContext);
begin
  with FList.LockList do
    try
      if IndexOf(aContext) < 0 then
        raise EVXContext.Create(strInvalidContextRegistration)
      else
        Remove(aContext);
    finally
      FList.UnlockList;
    end;
end;

procedure TgxContextManager.ContextCreatedBy(aContext: TgxContext);
begin
  Lock;
  try
    Inc(FCreatedRCCount);
  finally
    UnLock;
  end;
end;

procedure TgxContextManager.DestroyingContextBy(aContext: TgxContext);
var
  cn: TgxContextNotification;
begin
  Lock;
  try
    Dec(FCreatedRCCount);
    if FCreatedRCCount = 0 then
    begin
      // yes, slow and bulky, but allows for the triggered event to
      // cascade-remove notifications safely
      while Length(FNotifications) > 0 do
      begin
        cn := FNotifications[High(FNotifications)];
        SetLength(FNotifications, Length(FNotifications) - 1);
        cn.Event(cn.obj);
      end;
    end;
  finally
    UnLock;
  end;
end;

procedure TgxContextManager.LastContextDestroyNotification(anObject: TObject; anEvent: TNotifyEvent);
begin
  Lock;
  try
    SetLength(FNotifications, Length(FNotifications) + 1);
    with FNotifications[High(FNotifications)] do
    begin
      obj := anObject;
      Event := anEvent;
    end;
  finally
    UnLock;
  end;
end;

procedure TgxContextManager.RemoveNotification(anObject: TObject);
var
  I: Integer;
  found: Boolean;
begin
  Lock;
  try
    found := False;
    I := Low(FNotifications);
    while I <= High(FNotifications) do
    begin
      if FNotifications[I].obj = anObject then
      begin
        found := True;
        while I <= High(FNotifications) do
        begin
          FNotifications[I] := FNotifications[I + 1];
          Inc(I);
        end;
        SetLength(FNotifications, Length(FNotifications) - 1);
        Break;
      end;
      Inc(I);
    end;
    if not found then
      raise EVXContext.Create(strInvalidNotificationRemoval);
  finally
    UnLock;
  end;
end;

procedure TgxContextManager.Terminate;
begin
  FTerminated := True;
  if ContextCount = 0 then
  begin
    GXContextManager := nil;
    Free;
  end;
end;

procedure TgxContextManager.DestroyAllHandles;
var
  I: Integer;
begin
  with FList.LockList do
    try
      for I := Count - 1 downto 0 do
        TgxContext(Items[I]).DestroyAllHandles;
    finally
      FList.UnlockList;
    end;
end;

constructor TgxFinishTaskEvent.Create;
begin
  inherited Create(nil, True, False, '');
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

vMainThread := True;
GXContextManager := TgxContextManager.Create;

finalization

GXContextManager.Terminate;
vContextClasses.Free;
vContextClasses := nil;

end.
