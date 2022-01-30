//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.HeightData;

(*
  Classes for height data access.

  The components and classes in the unit are the core data providers for
  height-based objects (terrain rendering mainly), they are independant
  from the rendering stage.

  In short: access to raw height data is performed by a TGLHeightDataSource
  subclass, that must take care of performing all necessary data access,
  cacheing and manipulation to provide TGLHeightData objects. A TGLHeightData
  is basicly a square, power of two dimensionned raster heightfield, and
  holds the data a renderer needs.
*)

interface

{$I GLScene.inc}

uses
  Winapi.Windows, // to CreateMonochromeBitmap
  System.Classes,
  System.SysUtils,
  System.Types,
  Vcl.Graphics,

  GLS.ApplicationFileIO,
  GLS.Utils,
  GLS.VectorGeometry,
  GLS.Material,
  GLS.BaseClasses;

type
  TByteArray = array [0 .. MaxInt div (2 * SizeOf(Byte))] of Byte;
  TByteRaster = array [0 .. MaxInt div (2 * SizeOf(Pointer))] of PByteArray;
  PByteRaster = ^TByteRaster;
  TSmallintArray = array [0 .. MaxInt div (2 * SizeOf(SmallInt))] of SmallInt;
  PSmallIntArray = ^TSmallintArray;
  TSmallIntRaster = array [0 .. MaxInt div (2 * SizeOf(Pointer))
    ] of PSmallIntArray;
  PSmallIntRaster = ^TSmallIntRaster;
  TSingleRaster = array [0 .. MaxInt div (2 * SizeOf(Pointer))] of PSingleArray;
  PSingleRaster = ^TSingleRaster;

  TGLHeightData = class;
  TGLHeightDataClass = class of TGLHeightData;

  (* Determines the type of data stored in a TGLHeightData.
    There are 3 data types (8 bits unsigned, signed 16 bits and 32 bits).
    Conversions: (128*(ByteValue-128)) = SmallIntValue = Round(SingleValue).
    The 'hdtDefault' type is used for request only, and specifies that the
    default type for the source should be used. *)
  TGLHeightDataType = (hdtByte, hdtSmallInt, hdtSingle, hdtDefault);

  (* Base class for height datasources.
    This class is abstract and presents the standard interfaces for height
    data retrieval (TGLHeightData objects). The class offers the following
    features (that a subclass may decide to implement or not, what follow
    is the complete feature set, check subclass doc to see what is actually
    supported):
     Pooling / Cacheing (return a TGLHeightData with its "Release" method)
     Pre-loading : specify a list of TGLHeightData you want to preload
     Multi-threaded preload/queueing : specified list can be loaded in
    a background task. *)

  TGLHeightDataSource = class(TComponent)
  private
    FData: TThreadList; // stores all TGLHeightData, whatever their state/type
    FDataHash: array [0 .. 255] of TList; // X/Y hash references for HeightDatas
    FThread: TThread; // queue manager
    FMaxThreads: Integer;
    FMaxPoolSize: Integer;
    FHeightDataClass: TGLHeightDataClass;
    // FReleaseLatency : TDateTime;      //Not used anymore???
    FDefaultHeight: Single;
  protected
    procedure SetMaxThreads(const Val: Integer);
    function HashKey(XLeft, YTop: Integer): Integer;
    // Adjust this property in you subclasses.
    property HeightDataClass: TGLHeightDataClass read FHeightDataClass
      write FHeightDataClass;
    // Looks up the list and returns the matching TGLHeightData, if any.
    function FindMatchInList(XLeft, YTop, size: Integer;
      DataType: TGLHeightDataType): TGLHeightData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Access to currently pooled TGLHeightData objects, and Thread locking
    property Data: TThreadList read FData;
    (* Empties the Data list, terminating thread if necessary.
      If some TGLHeightData are hdsInUse, triggers an exception and does
      nothing. *)
    procedure Clear;
    (* Removes less used TDataHeight objects from the pool.
      Only removes objects whose state is hdsReady and UseCounter is zero,
      starting from the end of the list until total data size gets below
      MaxPoolSize (or nothing can be removed). *)
    procedure CleanUp;
    (* Base TGLHeightData requester method.
      Returns (by rebuilding it or from the cache) a TGLHeightData
      corresponding to the given area. Size must be a power of two.
      Subclasses may choose to publish it or just publish datasource-
      specific requester method using specific parameters. *)
    function GetData(XLeft, YTop, size: Integer; DataType: TGLHeightDataType)
      : TGLHeightData; virtual;
    // Preloading request. See GetData for details.
    function PreLoad(XLeft, YTop, size: Integer; DataType: TGLHeightDataType)
      : TGLHeightData; virtual;
    // Replacing dirty tiles.
    procedure PreloadReplacement(aHeightData: TGLHeightData);
    (* Notification that the data is no longer used by the renderer.
      Default behaviour is just to change DataState to hdsReady (ie. return
      the data to the pool) *)
    procedure Release(aHeightData: TGLHeightData); virtual;
    (* Marks the given area as "dirty" (ie source data changed).
      All loaded and in-cache tiles overlapping the area are flushed. *)
    procedure MarkDirty(const Area: TRect); overload; virtual;
    procedure MarkDirty(XLeft, YTop, xRight, yBottom: Integer); overload;
    procedure MarkDirty; overload;
    (* Maximum number of background threads.
      If 0 (zero), multithreading is disabled and StartPreparingData
      will be called from the mainthread, and all preload requirements
      (queued TGLHeightData objects) will be loaded in sequence from the main thread.
      If 1, basic multithreading and queueing gets enabled,
      ie. StartPreparingData will be called from a thread, but from one
      thread only (ie. there is no need to implement a TGLHeightDataThread,
      just make sure StartPreparingData code is thread-safe).
      Other values (2 and more) are relevant only if you implement
      a TGLHeightDataThread subclass and fire it in StartPreparingData. *)
    property MaxThreads: Integer read FMaxThreads write SetMaxThreads;
    (* Maximum Size of TDataHeight pool in bytes.
      The pool (cache) can actually get larger if more data than the pool
      can accomodate is used, but as soon as data gets released and returns
      to the pool, TDataHeight will be freed until total pool Size gets
      below this figure.
      The pool manager frees TDataHeight objects who haven't been requested
      for the longest time first.
      The default value of zero effectively disables pooling. *)
    property MaxPoolSize: Integer read FMaxPoolSize write FMaxPoolSize;
    // Height to return for undefined tiles.
    property DefaultHeight: Single read FDefaultHeight write FDefaultHeight;
    //  Interpolates height for the given point.
    function InterpolatedHeight(x, y: Single; TileSize: Integer) : Single; virtual;
    function Width: Integer; virtual; abstract;
    function Height: Integer; virtual; abstract;
    procedure ThreadIsIdle; virtual;
    // This is called BEFORE StartPreparing Data, but always from the main thread.
    procedure BeforePreparingData(HeightData: TGLHeightData); virtual;
    (* Request to start preparing data.
      If your subclass is thread-enabled, this is here that you'll create
      your thread and fire it (don't forget the requirements), if not,
      that'll be here you'll be doing your work.
      Either way, you are responsible for adjusting the DataState to
      hdsReady when you're done (DataState will be hdsPreparing when this
      method will be invoked). *)
    procedure StartPreparingData(HeightData: TGLHeightData); virtual;
    // This is called After "StartPreparingData", but always from the main thread
    procedure AfterPreparingData(HeightData: TGLHeightData); virtual;
    procedure TextureCoordinates(HeightData: TGLHeightData;
      Stretch: boolean = false);
  end;

  THDTextureCoordinatesMode = (tcmWorld, tcmLocal);

  (* Possible states for a TGLHeightData.
     hdsQueued : the data has been queued for loading
     hdsPreparing : the data is currently loading or being prepared for use
     hdsReady : the data is fully loaded and ready for use
     hdsNone : the height data does not exist for this tile *)
  TGLHeightDataState = (hdsQueued, hdsPreparing, hdsReady, hdsNone);

  TGLHeightDataThread = class;
  TOnHeightDataDirtyEvent = procedure(sender: TGLHeightData) of object;

  TGLHeightDataUser = record
    user: TObject;
    event: TOnHeightDataDirtyEvent;
  end;

  (* Base class for height data, stores a height-field raster.
    The raster is a square, whose Size must be a power of two. Data can be
    accessed through a base pointer ("ByteData[n]" f.i.), or through pointer
    indirections ("ByteRaster[y][x]" f.i.), this are the fastest way to access
    height data (and the most unsecure).
    Secure (with range checking) data access is provided by specialized
    methods (f.i. "ByteHeight"), in which coordinates (x & y) are always
    considered relative (like in raster access).
    The class offers conversion facility between the types (as a whole data
    conversion), but in any case, the TGLHeightData should be directly requested
    from the TGLHeightDataSource with the appropriate format.
    Though this class can be instantiated, you will usually prefer to subclass
    it in real-world cases, f.i. to add texturing data. *)
	/// TGLHeightData = class (TObject)
  TGLHeightData = class(TGLUpdateAbleObject)
  private
    FUsers: array of TGLHeightDataUser;
    FOwner: TGLHeightDataSource;
    FDataState: TGLHeightDataState;
    FSize: Integer;
    FXLeft, FYTop: Integer;
    FUseCounter: Integer;
    FDataType: TGLHeightDataType;
    FDataSize: Integer;
    FByteData: PByteArray;
    FByteRaster: PByteRaster;
    FSmallIntData: PSmallIntArray;
    FSmallIntRaster: PSmallIntRaster;
    FSingleData: PSingleArray;
    FSingleRaster: PSingleRaster;
    FTextureCoordinatesMode: THDTextureCoordinatesMode;
    FTCOffset, FTCScale: TTexPoint;
    FMaterialName: String; // Unsafe. Use FLibMaterial instead
    FLibMaterial: TGLLibMaterial;
    FObjectTag: TObject;
    FTag, FTag2: Integer;
    FOnDestroy: TNotifyEvent;
    FDirty: boolean;
    FHeightMin, FHeightMax: Single;
    procedure BuildByteRaster;
    procedure BuildSmallIntRaster;
    procedure BuildSingleRaster;
    procedure ConvertByteToSmallInt;
    procedure ConvertByteToSingle;
    procedure ConvertSmallIntToByte;
    procedure ConvertSmallIntToSingle;
    procedure ConvertSingleToByte;
    procedure ConvertSingleToSmallInt;
  protected
    FThread: TGLHeightDataThread;
    // thread used for multi-threaded processing (if any)
    procedure SetDataType(const Val: TGLHeightDataType);
    procedure SetMaterialName(const MaterialName: string);
    procedure SetLibMaterial(LibMaterial: TGLLibMaterial);
    function GetHeightMin: Single;
    function GetHeightMax: Single;
  public
    OldVersion: TGLHeightData; // previous version of this tile
    NewVersion: TGLHeightData; // the replacement tile
    DontUse: boolean; // Tells TerrainRenderer which version to use
    // constructor Create(AOwner : TComponent); override;
    constructor Create(AOwner: TGLHeightDataSource; aXLeft, aYTop, aSize: Integer;
      aDataType: TGLHeightDataType); reintroduce; virtual;
    destructor Destroy; override;
    // The component who created and maintains this data.
    property Owner: TGLHeightDataSource read FOwner;
    // Fired when the object is destroyed.
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    (* Counter for use registration.
      A TGLHeightData is not returned to the pool until this counter reaches
      a value of zero. *)
    property UseCounter: Integer read FUseCounter;
    (* Increments UseCounter.
      User objects should implement a method that will be notified when
      the data becomes dirty, when invoked they should release the heightdata
      immediately after performing their own cleanups. *)
    procedure RegisterUse;
    (*  Allocate memory and prepare lookup tables for current datatype.
      Fails if already allocated. Made Dynamic to allow descendants *)
    procedure Allocate(const Val: TGLHeightDataType); virtual;
    (*  Decrements UseCounter.
      When the counter reaches zero, notifies the Owner TGLHeightDataSource
      that the data is no longer used.
      The renderer should call Release when it no longer needs a THeighData,
      and never free/destroy the object directly. *)
    procedure Release;
    (*  Marks the tile as dirty.
      The immediate effect is currently the destruction of the tile. *)
    procedure MarkDirty;
    (*  World X coordinate of top left point. *)
    property XLeft: Integer read FXLeft;
    (*  World Y coordinate of top left point. *)
    property YTop: Integer read FYTop;
    (*  Type of the data.
      Assigning a new datatype will result in the data being converted. *)
    property DataType: TGLHeightDataType read FDataType write SetDataType;
    (*  Current state of the data. *)
    property DataState: TGLHeightDataState read FDataState write FDataState;
    (*  Size of the data square, in data units. *)
    property Size: Integer read FSize;
    (*  True if the data is dirty (ie. no longer up-to-date). *)
    property Dirty: boolean read FDirty write FDirty;
    (*  Memory Size of the raw data in bytes. *)
    property DataSize: Integer read FDataSize;
    (*  Access to data as a byte array (n = y*Size+x).
      If TGLHeightData is not of type hdtByte, this value is nil. *)
    property ByteData: PByteArray read FByteData;
    (*  Access to data as a byte raster (y, x).
      If TGLHeightData is not of type hdtByte, this value is nil. *)
    property ByteRaster: PByteRaster read FByteRaster;
    (*  Access to data as a SmallInt array (n = y*Size+x).
      If TGLHeightData is not of type hdtSmallInt, this value is nil. *)
    property SmallIntData: PSmallIntArray read FSmallIntData;
    (*  Access to data as a SmallInt raster (y, x).
      If TGLHeightData is not of type hdtSmallInt, this value is nil. *)
    property SmallIntRaster: PSmallIntRaster read FSmallIntRaster;
    (*  Access to data as a Single array (n = y*Size+x).
      If TGLHeightData is not of type hdtSingle, this value is nil. *)
    property SingleData: PSingleArray read FSingleData;
    (*  Access to data as a Single raster (y, x).
      If TGLHeightData is not of type hdtSingle, this value is nil. *)
    property SingleRaster: PSingleRaster read FSingleRaster;
    (*
      Name of material for the tile (if terrain uses multiple materials).
      property MaterialName : String read FMaterialName write FMaterialName;
      (WARNING: Unsafe when deleting textures! If possible, rather use LibMaterial.)
    *)
    property MaterialName: String read FMaterialName write SetMaterialName;
    (*
       property LibMaterial : Links directly to the tile's TGLLibMaterial.
       Unlike 'MaterialName', this property also registers the tile as
       a user of the texture.
       This prevents TGLLibMaterials.DeleteUnusedTextures from deleting the
       used texture by mistake and causing Access Violations.
       Use this instead of the old MaterialName property, to prevent AV's.
    *)
    property LibMaterial: TGLLibMaterial read FLibMaterial write SetLibMaterial;
    (*  Texture coordinates generation mode.
      Default is tcmWorld coordinates. *)
    property TextureCoordinatesMode: THDTextureCoordinatesMode
      read FTextureCoordinatesMode write FTextureCoordinatesMode;
    property TextureCoordinatesOffset: TTexPoint read FTCOffset write FTCOffset;
    property TextureCoordinatesScale: TTexPoint read FTCScale write FTCScale;
    (*  Height of point x, y as a Byte.  *)
    function ByteHeight(x, y: Integer): Byte;
    (*  Height of point x, y as a SmallInt.  *)
    function SmallIntHeight(x, y: Integer): SmallInt;
    (*  Height of point x, y as a Single.  *)
    function SingleHeight(x, y: Integer): Single;
    (*  Interopolated height of point x, y as a Single.  *)
    function InterpolatedHeight(x, y: Single): Single;
    (*  Minimum height in the tile.
      DataSources may assign a value to prevent automatic computation
      if they have a faster/already computed value. *)
    property HeightMin: Single read GetHeightMin write FHeightMin;
    (*  Maximum height in the tile.
      DataSources may assign a value to prevent automatic computation
      if they have a faster/already computed value. *)
    property HeightMax: Single read GetHeightMax write FHeightMax;
    (*  Returns the height as a single, whatever the DataType (slow). *)
    function Height(x, y: Integer): Single;
    (*  Calculates and returns the normal for vertex point x, y.
      Sub classes may provide normal cacheing, the default implementation
      being rather blunt. *)
    function Normal(x, y: Integer; const scale: TAffineVector): TAffineVector;
    (*  Calculates and returns the normal for cell x, y.(between vertexes)   *)
    function NormalAtNode(x, y: Integer; const scale: TAffineVector): TAffineVector;
    (*  Returns True if the data tile overlaps the area. *)
    function OverlapsArea(const Area: TRect): boolean;
    (*  Reserved for renderer use. *)
    property ObjectTag: TObject read FObjectTag write FObjectTag;
    (*  Reserved for renderer use. *)
    property Tag: Integer read FTag write FTag;
    (*  Reserved for renderer use. *)
    property Tag2: Integer read FTag2 write FTag2;
    (*  Used by perlin HDS. *)
    property Thread: TGLHeightDataThread read FThread write FThread;
  end;

  (*  A thread specialized for processing TGLHeightData in background.
    Requirements: 
     must have FreeOnTerminate set to true,
     must check and honour Terminated swiftly *)
  TGLHeightDataThread = class(TThread)
  protected
    FHeightData: TGLHeightData;
  public
    destructor Destroy; override;
    (*  The Height Data the thread is to prepare.  *)
    property HeightData: TGLHeightData read FHeightData write FHeightData;
  end;

  (*  Bitmap-based Height Data Source.
    The image is automatically wrapped if requested data is out of picture Size,
    or if requested data is larger than the picture. 
    The internal format is an 8 bit bitmap whose dimensions are a power of two,
    if the original image does not comply, it is StretchDraw'ed on a monochrome
    (gray) bitmap. *)
  TGLBitmapHDS = class(TGLHeightDataSource)
  private
    FScanLineCache: array of PByteArray;
    FBitmap: TBitmap;
    FPicture: TPicture;
    FInfiniteWrap: boolean;
    FInverted: boolean;
  protected
    procedure SetPicture(const Val: TPicture);
    procedure OnPictureChanged(sender: TObject);
    procedure SetInfiniteWrap(Val: boolean);
    procedure SetInverted(Val: boolean);
    procedure CreateMonochromeBitmap(size: Integer);
    procedure FreeMonochromeBitmap;
    function GetScanLine(y: Integer): PByteArray;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartPreparingData(HeightData: TGLHeightData); override;
    procedure MarkDirty(const Area: TRect); override;
    function Width: Integer; override;
    function Height: Integer; override;
  published
    (*  The picture serving as Height field data reference.
      The picture is (if not already) internally converted to a 8 bit
      bitmap (grayscale). For better performance and to save memory,
      feed it this format! *)
    property Picture: TPicture read FPicture write SetPicture;
    (*  If true the height field is wrapped indefinetely. *)
    property InfiniteWrap: boolean read FInfiniteWrap write SetInfiniteWrap
      default True;
    (*  If true, the rendered terrain is a mirror image of the input data. *)
    property Inverted: boolean read FInverted write SetInverted default True;
    property MaxPoolSize;
  end;

  TStartPreparingDataEvent = procedure(HeightData: TGLHeightData) of object;
  TMarkDirtyEvent = procedure(const Area: TRect) of object;

  // TTexturedHeightDataSource = class (TGLTexturedHeightDataSource)

  (*  An Height Data Source for custom use.
    Provides event handlers for the various requests to be implemented
    application-side (for application-specific needs). *)
  TGLCustomHDS = class(TGLHeightDataSource)
  private
    FOnStartPreparingData: TStartPreparingDataEvent;
    FOnMarkDirty: TMarkDirtyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartPreparingData(HeightData: TGLHeightData); override;
    procedure MarkDirty(const Area: TRect); override;
  published
    property MaxPoolSize;
    property OnStartPreparingData: TStartPreparingDataEvent
      read FOnStartPreparingData write FOnStartPreparingData;
    property OnMarkDirtyEvent: TMarkDirtyEvent read FOnMarkDirty
      write FOnMarkDirty;
  end;

  (*  TerrainBase-based Height Data Source.
    This component takes its data from the TerrainBase Gobal Terrain Model.
    Though it can be used directly, the resolution of the TerrainBase dataset
    isn't high enough for accurate short-range representation and the data
    should rather be used as basis for further (fractal) refinement. 
    TerrainBase is freely available from the National Geophysical Data Center
    and World Data Center web site (http://ngdc.noaa.com). 
    (this component expects to find "tbase.bin" in the current directory). *)
  TGLTerrainBaseHDS = class(TGLHeightDataSource)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartPreparingData(HeightData: TGLHeightData); override;
  published
    property MaxPoolSize;
  end;

  TGLHeightDataSourceFilter = Class;
  TSourceDataFetchedEvent = procedure(sender: TGLHeightDataSourceFilter;
    HeightData: TGLHeightData) of object;

  (*  Height Data Source Filter.
    This component sits between the TGLTerrainRenderer, and a real TGLHeightDataSource.
    i.e. TGLTerrainRenderer links to this. This links to the real TGLHeightDataSource.
    Use the 'HeightDataSource' property, to link to a source HDS.
    The 'OnSourceDataFetched' event then gives you the opportunity to make any changes,
    or link in a texture to the TGLHeightData object, BEFORE it is cached.
    It bypasses the cache of the source HDS, by calling the source's StartPreparingData procedure directly.
    The TGLHeightData objects are then cached by THIS component, AFTER you have made your changes.
    This eliminates the need to copy and release the TGLHeightData object from the Source HDS's cache,
    before linking your texture.  See the new version of TGLBumpmapHDS for an example. (LIN)
    To create your own HDSFilters, Derive from this component, and override the PreparingData procedure. *)
  TGLHeightDataSourceFilter = Class(TGLHeightDataSource)
  private
    FHDS: TGLHeightDataSource;
    FOnSourceDataFetched: TSourceDataFetchedEvent;
    FActive: boolean;
  protected
    (*  PreparingData:
      Override this function in your filter subclasses, to make any
      updates/changes to HeightData, before it goes into the cache.
      Make sure any code in this function is thread-safe, in case TAsyncHDS was used. *)
    procedure PreparingData(HeightData: TGLHeightData); virtual; abstract;
    procedure SetHDS(Val: TGLHeightDataSource);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Release(aHeightData: TGLHeightData); override;
    procedure StartPreparingData(HeightData: TGLHeightData); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function Width: Integer; override;
    function Height: Integer; override;
    property OnSourceDataFetched: TSourceDataFetchedEvent
      read FOnSourceDataFetched write FOnSourceDataFetched;
  published
    property MaxPoolSize;
    property HeightDataSource: TGLHeightDataSource read FHDS write SetHDS;
    property Active: boolean read FActive write FActive;
    // If Active=False, height data passes through unchanged
  end;


// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLHeightDataSourceThread ------------------
// ------------------

type
  TGLHeightDataSourceThread = class(TThread)
    FOwner: TGLHeightDataSource;
    FIdleLoops: Integer;
    procedure Execute; override;
    function WaitForTile(HD: TGLHeightData; seconds: Integer): boolean;
    procedure HDSIdle;
  end;

procedure TGLHeightDataSourceThread.Execute;
var
  i: Integer;
  lst: TList;
  HD: TGLHeightData;
  max: Integer;
  TdCtr: Integer;
begin
  while not Terminated do
  begin
    max := FOwner.MaxThreads;
    lst := FOwner.FData.LockList;

    // --count active threads--
    i := 0;
    TdCtr := 0;
    while (i < lst.Count) and (TdCtr < max) do
    begin
      if TGLHeightData(lst.Items[i]).FThread <> nil then
        Inc(TdCtr);
      Inc(i);
    end;
    // ------------------------

    // --Find the queued tiles, and Start preparing them--
    i := 0;
    While ((i < lst.Count) and (TdCtr < max)) do
    begin
      HD := TGLHeightData(lst.Items[i]);
      if HD.DataState = hdsQueued then
      begin
        FOwner.StartPreparingData(HD); // prepare
        Inc(TdCtr);
      end;
      Inc(i);
    end;
    // ---------------------------------------------------

    FOwner.FData.UnlockList;
    if (TdCtr = 0) then
      synchronize(HDSIdle);
    if (TdCtr = 0) then
      Sleep(10)
    else
      Sleep(0); // sleep longer if no Queued tiles were found
  end;
end;

// When Threading, wait a specified time, for the tile to finish preparing
function TGLHeightDataSourceThread.WaitForTile(HD: TGLHeightData;
  seconds: Integer): boolean;
var
  // i:integer;
  eTime: TDateTime;
begin
  eTime := now + (1000 * seconds);
  while (HD.FThread <> nil) and (now < eTime) do
  begin
    sleep(0);
  end;
  Result := (HD.FThread = nil); // true if the thread has finished
end;

// When using threads, HDSIdle is called in the main thread,
// whenever all HDS threads have finished, AND no queued tiles were found.
// (GLS.AsyncHDS uses this for the OnIdle event.)
procedure TGLHeightDataSourceThread.HDSIdle;
begin
  self.FOwner.ThreadIsIdle;
end;

// ------------------
// ------------------ TGLHeightDataSource ------------------
// ------------------

constructor TGLHeightDataSource.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  FHeightDataClass := TGLHeightData;
  FData := TThreadList.Create;
  for i := 0 to High(FDataHash) do
    FDataHash[i] := TList.Create;
  // FReleaseLatency:=15/(3600*24);
  FThread := TGLHeightDataSourceThread.Create(True);
  FThread.FreeOnTerminate := False;
  TGLHeightDataSourceThread(FThread).FOwner := self;
  if self.MaxThreads > 0 then
    FThread.Start;
end;

destructor TGLHeightDataSource.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.Start;
    FThread.WaitFor;
    FThread.Free;
  end;
  Clear;
  FData.Free;
  for i := 0 to High(FDataHash) do
    FDataHash[i].Free;
end;

procedure TGLHeightDataSource.Clear;
var
  i: Integer;
begin
  with FData.LockList do
  begin
    try
      for i := 0 to Count - 1 do
        if TGLHeightData(Items[i]).UseCounter > 0 then
          if not(csDestroying in ComponentState) then
            raise Exception.Create('ERR: HeightData still in use');
      for i := 0 to Count - 1 do
      begin
        TGLHeightData(Items[i]).FOwner := nil;
        TGLHeightData(Items[i]).Free;
      end;
      for i := 0 to High(FDataHash) do
        FDataHash[i].Clear;
      Clear;
    finally
      FData.UnlockList;
    end;
  end;
end;

function TGLHeightDataSource.HashKey(XLeft, YTop: Integer): Integer;
begin
  Result := (XLeft + (XLeft shr 8) + (YTop shl 1) + (YTop shr 7)) and
    High(FDataHash);
end;


function TGLHeightDataSource.FindMatchInList(XLeft, YTop, size: Integer;
  DataType: TGLHeightDataType): TGLHeightData;
var
  i: Integer;
  HD: TGLHeightData;
begin
  Result := nil;
  FData.LockList;
  try
    with FDataHash[HashKey(XLeft, YTop)] do
      for i := 0 to Count - 1 do
      begin
        HD := TGLHeightData(Items[i]);
        // if (not hd.Dirty) and (hd.XLeft=xLeft) and (hd.YTop=YTop) and (hd.Size=Size) and (hd.DataType=DataType) then begin
        if (HD.XLeft = XLeft) and (HD.YTop = YTop) and (HD.size = size) and
          (HD.DataType = DataType) and (HD.DontUse = false) then
        begin
          Result := HD;
          Break;
        end;
      end;
  finally
    FData.UnlockList;
  end;
end;

function TGLHeightDataSource.GetData(XLeft, YTop, size: Integer;
  DataType: TGLHeightDataType): TGLHeightData;
begin
  Result := FindMatchInList(XLeft, YTop, size, DataType);
  if not Assigned(Result) then
    Result := PreLoad(XLeft, YTop, size, DataType)
  else
    with FData.LockList do
    begin
      try
        Move(IndexOf(Result), 0); // Moves item to the beginning of the list.
      finally
        FData.UnlockList;
      end;
    end;
  // got one... can be used ?
  // while not (Result.DataState in [hdsReady, hdsNone]) do Sleep(0);
end;

function TGLHeightDataSource.PreLoad(XLeft, YTop, size: Integer;
  DataType: TGLHeightDataType): TGLHeightData;
begin
  Result := HeightDataClass.Create(self, XLeft, YTop, size, DataType);
  with FData.LockList do
    try
      Add(Result);
      BeforePreparingData(Result);
      FDataHash[HashKey(XLeft, YTop)].Add(Result);
    finally
      FData.UnlockList;
    end;

  // -- When NOT using Threads, fully prepare the tile immediately--
  if MaxThreads = 0 then
  begin
    StartPreparingData(Result);
    AfterPreparingData(Result);
  end;
  // ---------------------------------------------------------------
end;

// When Multi-threading, this queues a replacement for a dirty tile
// The Terrain renderer will continue to use the dirty tile, until the replacement is complete
procedure TGLHeightDataSource.PreloadReplacement(aHeightData: TGLHeightData);
var
  HD: TGLHeightData;
  NewHD: TGLHeightData;
begin
  Assert(MaxThreads > 0);
  HD := aHeightData;
  NewHD := HeightDataClass.Create(self, HD.XLeft, HD.YTop, HD.size,
    HD.DataType);
  with FData.LockList do
    try
      Add(NewHD);
      NewHD.OldVersion := HD; // link
      HD.NewVersion := NewHD; // link
      NewHD.DontUse := True;
      BeforePreparingData(NewHD);
      FDataHash[HashKey(HD.XLeft, HD.YTop)].Add(NewHD);
    finally
      FData.UnlockList;
    end;
end;

procedure TGLHeightDataSource.Release(aHeightData: TGLHeightData);
begin
  // nothing, yet
end;

procedure TGLHeightDataSource.MarkDirty(const Area: TRect);
var
  i: Integer;
  HD: TGLHeightData;
begin
  with FData.LockList do
  begin
    try
      for i := Count - 1 downto 0 do
      begin
        HD := TGLHeightData(Items[i]);
        if HD.OverlapsArea(Area) then
          HD.MarkDirty;
      end;
    finally
      FData.UnlockList;
    end;
  end;
end;

procedure TGLHeightDataSource.MarkDirty(XLeft, YTop, xRight, yBottom: Integer);
var
  r: TRect;
begin
  r.Left := XLeft;
  r.Top := YTop;
  r.Right := xRight;
  r.Bottom := yBottom;
  MarkDirty(r);
end;

procedure TGLHeightDataSource.MarkDirty;
const
  m = MaxInt - 1;
begin
  MarkDirty(-m, -m, m, m);
end;

procedure TGLHeightDataSource.CleanUp;
var
  packList: boolean;
  i, k: Integer;
  usedMemory: Integer;
  HD: TGLHeightData;
  ReleaseThis: boolean;
begin
  with FData.LockList do
  begin
    try
      usedMemory := 0;
      packList := false;
      // Cleanup dirty tiles and compute used memory
      for i := Count - 1 downto 0 do
      begin
        HD := TGLHeightData(Items[i]);
        if HD <> nil then
          with HD do
          begin
            // --Release criteria for dirty tiles--
            ReleaseThis := false;
            if HD.Dirty then
            begin // Only release dirty tiles
              if (MaxThreads = 0) then
                ReleaseThis := True
                // when not threading, delete ALL dirty tiles
              else if (HD.DataState <> hdsPreparing) then
              begin // Dont release Preparing tiles
                if (HD.UseCounter = 0) then
                  ReleaseThis := True; // This tile is unused
                if (HD.NewVersion = nil) then
                  ReleaseThis := True
                  // This tile has no queued replacement to wait for
                else if (HD.DontUse) then
                  ReleaseThis := True; // ??This tile has already been replaced.
              end;
            end;
            // ------------------------------------
            // if Dirty then ReleaseThis:=true;
            if ReleaseThis then
            begin
              FDataHash[HashKey(HD.XLeft, HD.YTop)].Remove(HD);
              Items[i] := nil;
              FOwner := nil;
              Free;
              packList := True;
            end
            else
              usedMemory := usedMemory + HD.DataSize;
          end;
      end;
      // If MaxPoolSize exceeded, release all that may be, and pack the list
      k := 0;
      if usedMemory > MaxPoolSize then
      begin
        for i := 0 to Count - 1 do
        begin
          HD := TGLHeightData(Items[i]);
          if HD <> nil then
            with HD do
            begin
              if (DataState <> hdsPreparing) and (UseCounter = 0) and
                (OldVersion = nil)
              // if (DataState=hdsReady)and(UseCounter=0)and(OldVersion=nil)
              then
              begin
                FDataHash[HashKey(HD.XLeft, HD.YTop)].Remove(HD);
                Items[i] := nil;
                FOwner := nil;
                Free;
                // packList:=True;
              end
              else
              begin
                Items[k] := HD;
                Inc(k);
              end;
            end;
        end;
        Count := k;
      end
      else if packList then
      begin
        for i := 0 to Count - 1 do
          if Items[i] <> nil then
          begin
            Items[k] := Items[i];
            Inc(k);
          end;
        Count := k;
      end;
    finally
      FData.UnlockList;
    end;
  end;
end;

procedure TGLHeightDataSource.SetMaxThreads(const Val: Integer);
begin
  if (Val <= 0) then
    FMaxThreads := 0
  else
  begin
    // If we didn't do threading, but will now
    // resume our thread
    if (FMaxThreads <= 0) then
      FThread.Start;
    FMaxThreads := Val;
  end;
end;

// Called BEFORE StartPreparingData, but always from the MAIN thread.
// Override this in subclasses, to prepare for Threading.
procedure TGLHeightDataSource.BeforePreparingData(HeightData: TGLHeightData);
begin
  //
end;

// When Threads are used, this runs from the sub-thread, so this MUST be thread-safe.
// Any Non-thread-safe code should be placed in "BeforePreparingData"
procedure TGLHeightDataSource.StartPreparingData(HeightData: TGLHeightData);
begin
  // Only the tile Owner may set the preparing tile to ready
  if (HeightData.Owner = self) and (HeightData.DataState = hdsPreparing) then
    HeightData.FDataState := hdsReady;
end;

// Called AFTER StartPreparingData, but always from the MAIN thread.
// Override this in subclasses, if needed.
procedure TGLHeightDataSource.AfterPreparingData(HeightData: TGLHeightData);
begin
  //
end;

procedure TGLHeightDataSource.ThreadIsIdle;
begin
  // TGLAsyncHDS overrides this
end;

// Calculates texture World texture coordinates for the current tile.
// Use Stretch for OpenGL1.1, to hide the seams when using linear filtering.
procedure TGLHeightDataSource.TextureCoordinates(HeightData: TGLHeightData;
  Stretch: boolean = false);
var
  w, h, size: Integer;
  scaleS, scaleT: Single;
  offsetS, offsetT: Single;
  HD: TGLHeightData;
  halfpixel: Single;
begin
  HD := HeightData;
  w := self.Width;
  h := self.Height;
  size := HD.FSize;
  // if GL_VERSION_1_2 then begin //OpenGL1.2 supports texture clamping, so seams dont show.
  if Stretch = false then
  begin // These are the real Texture coordinates
    scaleS := w / (size - 1);
    scaleT := h / (size - 1);
    offsetS := -((HD.XLeft / w) * scaleS);
    offsetT := -(h - (HD.YTop + size - 1)) / (size - 1);
  end
  else
  begin // --Texture coordinates: Stretched by 1 pixel, to hide seams on OpenGL-1.1(no Clamping)--
    scaleS := w / size;
    scaleT := h / size;
    halfpixel := 1 / (size shr 1);
    offsetS := -((HD.XLeft / w) * scaleS) + halfpixel;
    offsetT := -(h - (HD.YTop + size)) / size - halfpixel;
  end;
  HD.FTCScale.S := scaleS;
  HD.FTCScale.T := scaleT;
  HD.FTCOffset.S := offsetS;
  HD.FTCOffset.T := offsetT;
end;

function TGLHeightDataSource.InterpolatedHeight(x, y: Single;
  TileSize: Integer): Single;
var
  i: Integer;
  HD, foundHd: TGLHeightData;
begin
  with FData.LockList do
  begin
    try
      // first, lookup data list to find if aHeightData contains our point
      foundHd := nil;
      for i := 0 to Count - 1 do
      begin
        HD := TGLHeightData(Items[i]);
        if (HD.XLeft <= x) and (HD.YTop <= y) and (HD.XLeft + HD.size - 1 > x)
          and (HD.YTop + HD.size - 1 > y) then
        begin
          foundHd := HD;
          Break;
        end;
      end;
    finally
      FData.UnlockList;
    end;
  end;
  if (foundHd = nil) or foundHd.Dirty then
  begin
    // not found, request one... slowest mode (should be avoided)
    if TileSize > 1 then
      foundHd := GetData(Round(x / (TileSize - 1) - 0.5) * (TileSize - 1),
        Round(y / (TileSize - 1) - 0.5) * (TileSize - 1), TileSize, hdtDefault)
    else
    begin
      Result := DefaultHeight;
      Exit;
    end;
  end
  else
  begin
    // request it using "standard" way (takes care of threads)
    foundHd := GetData(foundHd.XLeft, foundHd.YTop, foundHd.size,
      foundHd.DataType);
  end;
  if foundHd.DataState = hdsNone then
    Result := DefaultHeight
  else
    Result := foundHd.InterpolatedHeight(x - foundHd.XLeft, y - foundHd.YTop);
end;

// ------------------
// ------------------ TGLHeightData ------------------
// ------------------

constructor TGLHeightData.Create(AOwner: TGLHeightDataSource;
  aXLeft, aYTop, aSize: Integer; aDataType: TGLHeightDataType);
begin
  inherited Create(AOwner);
  SetLength(FUsers, 0);
  FOwner := AOwner;
  FXLeft := aXLeft;
  FYTop := aYTop;
  FSize := aSize;
  FTextureCoordinatesMode := tcmWorld;
  FTCScale := XYTexPoint;
  FDataType := aDataType;
  FDataState := hdsQueued;
  FHeightMin := 1E30;
  FHeightMax := 1E30;

  OldVersion := nil;
  NewVersion := nil;
  DontUse := False;
end;

destructor TGLHeightData.Destroy;
begin
  Assert(Length(FUsers) = 0,
    'You should *not* free a TGLHeightData, use "Release" instead');
  Assert(not Assigned(FOwner),
    'You should *not* free a TGLHeightData, use "Release" instead');
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    if FThread.Suspended then
      FThread.Start;
    FThread.WaitFor;
  end;

  if Assigned(FOnDestroy) then
    FOnDestroy(self);
  case DataType of
    hdtByte:
      begin
        FreeMem(FByteData);
        FreeMem(FByteRaster);
      end;
    hdtSmallInt:
      begin
        FreeMem(FSmallIntData);
        FreeMem(FSmallIntRaster);
      end;
    hdtSingle:
      begin
        FreeMem(FSingleData);
        FreeMem(FSingleRaster);
      end;
    hdtDefault:
      ; // nothing
  else
    Assert(False);
  end;
  // ----------------------
  self.LibMaterial := nil; // release a used material

  // --Break any link with a new/old version of this tile--
  if Assigned(self.OldVersion) then
  begin
    self.OldVersion.NewVersion := nil;
    self.OldVersion := nil;
  end;
  if Assigned(self.NewVersion) then
  begin
    self.NewVersion.OldVersion := nil;
    self.NewVersion := nil;
  end;
  // ------------------------------------------------------

  // ----------------------
  inherited Destroy;
end;

procedure TGLHeightData.RegisterUse;
begin
  Inc(FUseCounter);
end;

// Release
//
procedure TGLHeightData.Release;
begin
  if FUseCounter > 0 then
    Dec(FUseCounter);
  if FUseCounter = 0 then
  begin
    Owner.Release(self); // ???
  end;
end;

// Release Dirty tiles, unless threading, and the tile is being used.
// In that case, start building a replacement tile instead.

procedure TGLHeightData.MarkDirty;
begin
  with Owner.Data.LockList do
    try
      if (not Dirty) and (DataState <> hdsQueued) then
      begin // dont mark queued tiles as dirty
        FDirty := True;
        if (Owner.MaxThreads > 0) and (FUseCounter > 0) then
          Owner.PreloadReplacement(self)
        else
        begin
          FUseCounter := 0;
          Owner.Release(self);
        end;
      end;
    finally
      Owner.Data.UnlockList;
    end;
end;

procedure TGLHeightData.Allocate(const Val: TGLHeightDataType);
begin
  Assert(FDataSize = 0);
  case Val of
    hdtByte:
      begin
        FDataSize := size * size * SizeOf(Byte);
        GetMem(FByteData, FDataSize);
        BuildByteRaster;
      end;
    hdtSmallInt:
      begin
        FDataSize := size * size * SizeOf(SmallInt);
        GetMem(FSmallIntData, FDataSize);
        BuildSmallIntRaster;
      end;
    hdtSingle:
      begin
        FDataSize := size * size * SizeOf(Single);
        GetMem(FSingleData, FDataSize);
        BuildSingleRaster;
      end;
  else
    Assert(false);
  end;
  FDataType := Val;
end;

// WARNING: SetMaterialName does NOT register the tile as a user of this texture.
// So, TGLLibMaterials.DeleteUnusedMaterials may see this material as unused, and delete it.
// This may lead to AV's the next time this tile is rendered.
// To be safe, rather assign the new TGLHeightData.LibMaterial property
procedure TGLHeightData.SetMaterialName(const MaterialName: string);
begin
  SetLibMaterial(nil);
  FMaterialName := MaterialName;
end;

procedure TGLHeightData.SetLibMaterial(LibMaterial: TGLLibMaterial);
begin
  if Assigned(FLibMaterial) then
    FLibMaterial.UnregisterUser(self); // detach from old texture
  FLibMaterial := LibMaterial; // Attach new Material
  if Assigned(LibMaterial) then
  begin
    LibMaterial.RegisterUser(self); // Mark new Material as 'used'
    FMaterialName := LibMaterial.Name; // sync up MaterialName property
  end
  else
    FMaterialName := '';
end;

procedure TGLHeightData.SetDataType(const Val: TGLHeightDataType);
begin
  if (Val <> FDataType) and (Val <> hdtDefault) then
  begin
    if DataState <> hdsNone then
    begin
      case FDataType of
        hdtByte:
          case Val of
            hdtSmallInt:
              ConvertByteToSmallInt;
            hdtSingle:
              ConvertByteToSingle;
          else
            Assert(False);
          end;
        hdtSmallInt:
          case Val of
            hdtByte:
              ConvertSmallIntToByte;
            hdtSingle:
              ConvertSmallIntToSingle;
          else
            Assert(False);
          end;
        hdtSingle:
          case Val of
            hdtByte:
              ConvertSingleToByte;
            hdtSmallInt:
              ConvertSingleToSmallInt;
          else
            Assert(False);
          end;
        hdtDefault:
          ; // nothing, assume StartPreparingData knows what it's doing
      else
        Assert(False);
      end;
    end;
    FDataType := Val;
  end;
end;

procedure TGLHeightData.BuildByteRaster;
var
  i: Integer;
begin
  GetMem(FByteRaster, size * SizeOf(PByteArray));
  for i := 0 to size - 1 do
    FByteRaster^[i] := @FByteData[i * size]
end;

procedure TGLHeightData.BuildSmallIntRaster;
var
  i: Integer;
begin
  GetMem(FSmallIntRaster, size * SizeOf(PSmallIntArray));
  for i := 0 to size - 1 do
    FSmallIntRaster^[i] := @FSmallIntData[i * size]
end;

procedure TGLHeightData.BuildSingleRaster;
var
  i: Integer;
begin
  GetMem(FSingleRaster, size * SizeOf(PSingleArray));
  for i := 0 to size - 1 do
    FSingleRaster^[i] := @FSingleData[i * size]
end;

procedure TGLHeightData.ConvertByteToSmallInt;
var
  i: Integer;
begin
  FreeMem(FByteRaster);
  FByteRaster := nil;
  FDataSize := size * size * SizeOf(SmallInt);
  GetMem(FSmallIntData, FDataSize);
  for i := 0 to size * size - 1 do
    FSmallIntData^[i] := (FByteData^[i] - 128) shl 7;
  FreeMem(FByteData);
  FByteData := nil;
  BuildSmallIntRaster;
end;

procedure TGLHeightData.ConvertByteToSingle;
var
  i: Integer;
begin
  FreeMem(FByteRaster);
  FByteRaster := nil;
  FDataSize := size * size * SizeOf(Single);
  GetMem(FSingleData, FDataSize);
  for i := 0 to size * size - 1 do
    FSingleData^[i] := (FByteData^[i] - 128) shl 7;
  FreeMem(FByteData);
  FByteData := nil;
  BuildSingleRaster;
end;

procedure TGLHeightData.ConvertSmallIntToByte;
var
  i: Integer;
begin
  FreeMem(FSmallIntRaster);
  FSmallIntRaster := nil;
  FByteData := Pointer(FSmallIntData);
  for i := 0 to size * size - 1 do
    FByteData^[i] := (FSmallIntData^[i] div 128) + 128;
  FDataSize := size * size * SizeOf(Byte);
  ReallocMem(FByteData, FDataSize);
  FSmallIntData := nil;
  BuildByteRaster;
end;

procedure TGLHeightData.ConvertSmallIntToSingle;
var
  i: Integer;
begin
  FreeMem(FSmallIntRaster);
  FSmallIntRaster := nil;
  FDataSize := size * size * SizeOf(Single);
  GetMem(FSingleData, FDataSize);
  for i := 0 to size * size - 1 do
    FSingleData^[i] := FSmallIntData^[i];
  FreeMem(FSmallIntData);
  FSmallIntData := nil;
  BuildSingleRaster;
end;

procedure TGLHeightData.ConvertSingleToByte;
var
  i: Integer;
begin
  FreeMem(FSingleRaster);
  FSingleRaster := nil;
  FByteData := Pointer(FSingleData);
  for i := 0 to size * size - 1 do
    FByteData^[i] := (Round(FSingleData^[i]) div 128) + 128;
  FDataSize := size * size * SizeOf(Byte);
  ReallocMem(FByteData, FDataSize);
  FSingleData := nil;
  BuildByteRaster;
end;

procedure TGLHeightData.ConvertSingleToSmallInt;
var
  i: Integer;
begin
  FreeMem(FSingleRaster);
  FSingleRaster := nil;
  FSmallIntData := Pointer(FSingleData);
  for i := 0 to size * size - 1 do
    FSmallIntData^[i] := Round(FSingleData^[i]);
  FDataSize := size * size * SizeOf(SmallInt);
  ReallocMem(FSmallIntData, FDataSize);
  FSingleData := nil;
  BuildSmallIntRaster;
end;

function TGLHeightData.ByteHeight(x, y: Integer): Byte;
begin
  Assert((Cardinal(x) < Cardinal(size)) and (Cardinal(y) < Cardinal(size)));
  Result := ByteRaster^[y]^[x];
end;

function TGLHeightData.SmallIntHeight(x, y: Integer): SmallInt;
begin
  Assert((Cardinal(x) < Cardinal(size)) and (Cardinal(y) < Cardinal(size)));
  Result := SmallIntRaster^[y]^[x];
end;

function TGLHeightData.SingleHeight(x, y: Integer): Single;
begin
  Assert((Cardinal(x) < Cardinal(size)) and (Cardinal(y) < Cardinal(size)));
  Result := SingleRaster^[y]^[x];
end;

function TGLHeightData.InterpolatedHeight(x, y: Single): Single;
var
  ix, iy, ixn, iyn: Integer;
  h1, h2, h3: Single;
begin
  if FDataState = hdsNone then
    Result := 0
  else
  begin
    ix := Trunc(x);
    x := Frac(x);
    iy := Trunc(y);
    y := Frac(y);
    ixn := ix + 1;
    if ixn >= size then
      ixn := ix;
    iyn := iy + 1;
    if iyn >= size then
      iyn := iy;
    if x > y then
    begin
      // top-right triangle
      h1 := Height(ixn, iy);
      h2 := Height(ix, iy);
      h3 := Height(ixn, iyn);
      Result := h1 + (h2 - h1) * (1 - x) + (h3 - h1) * y;
    end
    else
    begin
      // bottom-left triangle
      h1 := Height(ix, iyn);
      h2 := Height(ixn, iyn);
      h3 := Height(ix, iy);
      Result := h1 + (h2 - h1) * (x) + (h3 - h1) * (1 - y);
    end;
  end;
end;

function TGLHeightData.Height(x, y: Integer): Single;
begin
  case DataType of
    hdtByte:
      Result := (ByteHeight(x, y) - 128) shl 7;
    hdtSmallInt:
      Result := SmallIntHeight(x, y);
    hdtSingle:
      Result := SingleHeight(x, y);
  else
    Result := 0;
    Assert(false);
  end;
end;

function TGLHeightData.GetHeightMin: Single;
var
  i: Integer;
  b: Byte;
  sm: SmallInt;
  si: Single;
begin
  if FHeightMin = 1E30 then
  begin
    if DataState = hdsReady then
    begin
      case DataType of
        hdtByte:
          begin
            b := FByteData^[0];
            for i := 1 to size * size - 1 do
              if FByteData^[i] < b then
                b := FByteData^[i];
            FHeightMin := ((Integer(b) - 128) shl 7);
          end;
        hdtSmallInt:
          begin
            sm := FSmallIntData^[0];
            for i := 1 to size * size - 1 do
              if FSmallIntData^[i] < sm then
                sm := FSmallIntData^[i];
            FHeightMin := sm;
          end;
        hdtSingle:
          begin
            si := FSingleData^[0];
            for i := 1 to size * size - 1 do
              if FSingleData^[i] < si then
                si := FSingleData^[i];
            FHeightMin := si;
          end;
      else
        FHeightMin := 0;
      end;
    end
    else
      FHeightMin := 0;
  end;
  Result := FHeightMin;
end;

function TGLHeightData.GetHeightMax: Single;
var
  i: Integer;
  b: Byte;
  sm: SmallInt;
  si: Single;
begin
  if FHeightMax = 1E30 then
  begin
    if DataState = hdsReady then
    begin
      case DataType of
        hdtByte:
          begin
            b := FByteData^[0];
            for i := 1 to size * size - 1 do
              if FByteData^[i] > b then
                b := FByteData^[i];
            FHeightMax := ((Integer(b) - 128) shl 7);
          end;
        hdtSmallInt:
          begin
            sm := FSmallIntData^[0];
            for i := 1 to size * size - 1 do
              if FSmallIntData^[i] > sm then
                sm := FSmallIntData^[i];
            FHeightMax := sm;
          end;
        hdtSingle:
          begin
            si := FSingleData^[0];
            for i := 1 to size * size - 1 do
              if FSingleData^[i] > si then
                si := FSingleData^[i];
            FHeightMax := si;
          end;
      else
        FHeightMax := 0;
      end;
    end
    else
      FHeightMax := 0;
  end;
  Result := FHeightMax;
end;

// Calculates the normal at a vertex
function TGLHeightData.Normal(x, y: Integer; const scale: TAffineVector)
  : TAffineVector;
var
  dx, dy: Single;
begin
  if x > 0 then
    if x < size - 1 then
      dx := (Height(x + 1, y) - Height(x - 1, y))
    else
      dx := (Height(x, y) - Height(x - 1, y))
  else
    dx := (Height(x + 1, y) - Height(x, y));
  if y > 0 then
    if y < size - 1 then
      dy := (Height(x, y + 1) - Height(x, y - 1))
    else
      dy := (Height(x, y) - Height(x, y - 1))
  else
    dy := (Height(x, y + 1) - Height(x, y));
  Result.X := dx * scale.Y * scale.Z;
  Result.Y := dy * scale.X * scale.Z;
  Result.Z := 1 * scale.X * scale.Y;
  NormalizeVector(Result);
end;

// Calculates the normal at a surface cell (Between vertexes)
function TGLHeightData.NormalAtNode(x, y: Integer; const scale: TAffineVector)
  : TAffineVector;
var
  dx, dy, Hxy: Single;
begin
  MinInteger(MaxInteger(x, 0), size - 2); // clamp x to 0 -> Size-2
  MinInteger(MaxInteger(y, 0), size - 2); // clamp x to 0 -> Size-2
  Hxy := Height(x, y);
  dx := Height(x + 1, y) - Hxy;
  dy := Height(x, y + 1) - Hxy;
  Result.X := dx * scale.Y * scale.Z; // Result.X:=dx/scale.X;
  Result.Y := dy * scale.X * scale.Z; // Result.Y:=dy/scale.Y;
  Result.Z := 1 * scale.X * scale.Y;  // Result.Z:=1 /scale.Z;
  NormalizeVector(Result);
end;

function TGLHeightData.OverlapsArea(const Area: TRect): boolean;
begin
  Result := (XLeft <= Area.Right) and (YTop <= Area.Bottom) and
    (XLeft + size > Area.Left) and (YTop + size > Area.Top);
end;

// ------------------
// ------------------ TGLHeightDataThread ------------------
// ------------------

destructor TGLHeightDataThread.Destroy;
begin
  if Assigned(FHeightData) then
    FHeightData.FThread := nil;
  inherited;
end;

// ------------------
// ------------------ TGLBitmapHDS ------------------
// ------------------

constructor TGLBitmapHDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
  FPicture.OnChange := OnPictureChanged;
  FInfiniteWrap := True;
  FInverted := True;
end;

destructor TGLBitmapHDS.Destroy;
begin
  inherited Destroy;
  FreeMonochromeBitmap;
  FPicture.Free;
end;

procedure TGLBitmapHDS.SetPicture(const Val: TPicture);
begin
  FPicture.Assign(Val);
end;

procedure TGLBitmapHDS.OnPictureChanged(sender: TObject);
var
  oldPoolSize, size: Integer;
begin
  // cleanup pool
  oldPoolSize := MaxPoolSize;
  MaxPoolSize := 0;
  CleanUp;
  MaxPoolSize := oldPoolSize;
  // prepare MonoChromeBitmap
  FreeMonochromeBitmap;
  size := Picture.Width;
  if size > 0 then
    CreateMonochromeBitmap(size);
end;

procedure TGLBitmapHDS.SetInfiniteWrap(Val: boolean);
begin
  if FInfiniteWrap <> Val then
  begin
    FInfiniteWrap := Val;
    MarkDirty;
  end;
end;

procedure TGLBitmapHDS.SetInverted(Val: boolean);
begin
  if FInverted = Val then
    Exit;
  FInverted := Val;
  MarkDirty;
end;

procedure TGLBitmapHDS.MarkDirty(const Area: TRect);
begin
  inherited;
  FreeMonochromeBitmap;
  if Picture.Width > 0 then
    CreateMonochromeBitmap(Picture.Width);
end;

procedure TGLBitmapHDS.CreateMonochromeBitmap(size: Integer);
type
  TPaletteEntryArray = array [0 .. 255] of TPaletteEntry;
  PPaletteEntryArray = ^TPaletteEntryArray;

  TLogPal = record
    lpal: TLogPalette;
    pe: TPaletteEntryArray;
  end;

var
  x: Integer;
  logpal: TLogPal;
  hPal: HPalette;
begin
  size := RoundUpToPowerOf2(size);
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf8bit;
  FBitmap.Width := size;
  FBitmap.Height := size;
  for x := 0 to 255 do
    with PPaletteEntryArray(@logpal.lpal.palPalEntry[0])[x] do
    begin
      peRed := x;
      peGreen := x;
      peBlue := x;
      peFlags := 0;
    end;
  with logpal.lpal do
  begin
    palVersion := $300;
    palNumEntries := 256;
  end;
  hPal := CreatePalette(logpal.lpal);
  Assert(hPal <> 0);
  FBitmap.Palette := hPal;
  // some picture formats trigger a "change" when drawed
  Picture.OnChange := nil;
  try
    FBitmap.Canvas.StretchDraw(Rect(0, 0, size, size), Picture.Graphic);
  finally
    Picture.OnChange := OnPictureChanged;
  end;
  SetLength(FScanLineCache, 0); // clear the cache
  SetLength(FScanLineCache, size);
end;

procedure TGLBitmapHDS.FreeMonochromeBitmap;
begin
  SetLength(FScanLineCache, 0);
  FBitmap.Free;
  FBitmap := nil;
end;

function TGLBitmapHDS.GetScanLine(y: Integer): PByteArray;
begin
  Result := FScanLineCache[y];
  if not Assigned(Result) then
  begin
    Result := FBitmap.ScanLine[y];
    FScanLineCache[y] := Result;
  end;
end;

procedure TGLBitmapHDS.StartPreparingData(HeightData: TGLHeightData);
var
  y, x: Integer;
  bmpSize, wrapMask: Integer;
  bitmapLine, rasterLine: PByteArray;
  oldType: TGLHeightDataType;
  b: Byte;
  YPos: Integer;
begin
  if FBitmap = nil then
    Exit;
  HeightData.FDataState := hdsPreparing;
  bmpSize := FBitmap.Width;
  wrapMask := bmpSize - 1;
  // retrieve data
  with HeightData do
  begin
    if (not InfiniteWrap) and ((XLeft >= bmpSize) or (XLeft < 0) or
      (YTop >= bmpSize) or (YTop < 0)) then
    begin
      HeightData.FDataState := hdsNone;
      Exit;
    end;
    oldType := DataType;
    Allocate(hdtByte);
    if Inverted then
      YPos := YTop
    else
      YPos := 1 - size - YTop;
    for y := 0 to size - 1 do
    begin
      bitmapLine := GetScanLine((y + YPos) and wrapMask);
      if Inverted then
        rasterLine := ByteRaster^[y]
      else
        rasterLine := ByteRaster^[size - 1 - y];
      // *BIG CAUTION HERE* : Don't remove the intermediate variable here!!!
      // or Delphi compiler will "optimize" to 32 bits access with clamping
      // Resulting in possible reads of stuff beyon bitmapLine length!!!!
      for x := XLeft to XLeft + size - 1 do
      begin
        b := bitmapLine^[x and wrapMask];
        rasterLine^[x - XLeft] := b;
      end;
    end;
    if (oldType <> hdtByte) and (oldType <> hdtDefault) then
      DataType := oldType;
  end;
  TextureCoordinates(HeightData);
  inherited;
end;

function TGLBitmapHDS.Width: Integer;
begin
  if Assigned(self.FBitmap) then
    Result := self.FBitmap.Width
  else
    Result := 0;
end;

function TGLBitmapHDS.Height: Integer;
begin
  if Assigned(self.FBitmap) then
    Result := self.FBitmap.Height
  else
    Result := 0;
end;


// ------------------
// ------------------ TGLCustomHDS ------------------
// ------------------

constructor TGLCustomHDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TGLCustomHDS.Destroy;
begin
  inherited Destroy;
end;

procedure TGLCustomHDS.MarkDirty(const Area: TRect);
begin
  inherited;
  if Assigned(FOnMarkDirty) then
    FOnMarkDirty(Area);
end;

procedure TGLCustomHDS.StartPreparingData(HeightData: TGLHeightData);
begin
  if Assigned(FOnStartPreparingData) then
    FOnStartPreparingData(HeightData);
  if HeightData.DataState <> hdsNone then
    HeightData.DataState := hdsReady;
end;

// ------------------
// ------------------ TGLTerrainBaseHDS ------------------
// ------------------

constructor TGLTerrainBaseHDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TGLTerrainBaseHDS.Destroy;
begin
  inherited Destroy;
end;

procedure TGLTerrainBaseHDS.StartPreparingData(HeightData: TGLHeightData);
const
  cTBWidth: Integer = 4320;
  cTBHeight: Integer = 2160;
var
  y, x, offset: Integer;
  rasterLine: PSmallIntArray;
  oldType: TGLHeightDataType;
  b: SmallInt;
  fs: TStream;
begin
  if not FileExists('tbase.bin') then
    Exit;
  fs := TFileStream.Create('tbase.bin', fmOpenRead + fmShareDenyNone);
  try
    // retrieve data
    with HeightData do
    begin
      oldType := DataType;
      Allocate(hdtSmallInt);
      for y := YTop to YTop + size - 1 do
      begin
        offset := (y mod cTBHeight) * (cTBWidth * 2);
        rasterLine := SmallIntRaster^[y - YTop];
        for x := XLeft to XLeft + size - 1 do
        begin
          fs.Seek(offset + (x mod cTBWidth) * 2, soFromBeginning);
          fs.Read(b, 2);
          if b < 0 then
            b := 0;
          rasterLine^[x - XLeft] := SmallInt(b);
        end;
      end;
      if oldType <> hdtSmallInt then
        DataType := oldType;
    end;
    inherited;
  finally
    fs.Free;
  end;
end;


// ------------------
// ------------------ TGLHeightDataSourceFilter ------------------
// ------------------

constructor TGLHeightDataSourceFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
end;

destructor TGLHeightDataSourceFilter.Destroy;
begin
  HeightDataSource := nil;
  inherited Destroy;
end;

procedure TGLHeightDataSourceFilter.Release(aHeightData: TGLHeightData);
begin
  if Assigned(HeightDataSource) then
    HeightDataSource.Release(aHeightData);
end;

procedure TGLHeightDataSourceFilter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FHDS then
      HeightDataSource := nil
  end;
  inherited;
end;

procedure TGLHeightDataSourceFilter.SetHDS(Val: TGLHeightDataSource);
begin
  if Val = self then
    Val := nil; // prevent self-referencing
  if Val <> FHDS then
  begin
    if Assigned(FHDS) then
      FHDS.RemoveFreeNotification(self);
    FHDS := Val;
    if Assigned(FHDS) then
      FHDS.FreeNotification(self);
    // MarkDirty;
    self.Clear; // when removing the HDS, also remove all tiles from the cache
  end;
end;

function TGLHeightDataSourceFilter.Width: Integer;
begin
  if Assigned(FHDS) then
    Result := FHDS.Width
  else
    Result := 0;
end;

function TGLHeightDataSourceFilter.Height: Integer;
begin
  if Assigned(FHDS) then
    Result := FHDS.Height
  else
    Result := 0;
end;

procedure TGLHeightDataSourceFilter.StartPreparingData(HeightData: TGLHeightData);
begin
  // ---if there is no linked HDS then return an empty tile--
  if not Assigned(FHDS) then
  begin
    HeightData.Owner.Data.LockList;
    HeightData.DataState := hdsNone;
    HeightData.Owner.Data.UnlockList;
    Exit;
  end;
  // ---Use linked HeightDataSource to prepare height data--
  if HeightData.DataState = hdsQueued then
  begin
    HeightData.Owner.Data.LockList;
    HeightData.DataState := hdsPreparing;
    HeightData.Owner.Data.UnlockList;
  end;
  FHDS.StartPreparingData(HeightData);
  if Assigned(FOnSourceDataFetched) then
    FOnSourceDataFetched(self, HeightData);
  if HeightData.DataState = hdsNone then
    Exit;
  if FActive then
    PreparingData(HeightData);
  inherited; // HeightData.DataState:=hdsReady;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

// class registrations
RegisterClasses([TGLBitmapHDS, TGLCustomHDS, TGLHeightDataSourceFilter]);

end.
