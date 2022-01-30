//
// The graphics rendering engine GLScene http://glscene.org
//
unit CUDA.APIComps;

(* CUDA API routines implementation *)

interface

{$I GLScene.inc}

uses
  System.Types,
  System.Classes,
  System.SysUtils,

  GLS.PersistentClasses,
  GLS.BaseClasses,
  GLS.Context,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.VectorLists,
  GLS.Graphics,
  GLS.Strings,
  GLS.Utils,

  CUDA.Import,
  CUDA.RunTime,
  CUDA.Parser,
  CUDA.FourierTransform,
  CUDA.Compiler,
  CUDA.Context,
  CUDA.DataAccess;
type
  TCUDAChange = (cuchDevice, cuchContext, cuchSize, cuchAddresMode, cuchFlag,
    cuchFilterMode, cuchArray, cuchFormat, cuchMapping);
  TCUDAChanges = set of TCUDAChange;

  TCuAddresMode = (amWrap, amClamp, amMirror);
  TCuFilterMode = (fmPoint, fmLinear);

  TCUDAChannelType = (ctUndefined, ctUInt8, ctUInt16, ctUInt32, ctInt8, ctInt16,
    ctInt32, ctHalfFloat, ctFloat, ctDouble);

type

  TCUDAChannelNum = (cnOne, cnTwo, cnThree, cnFour);

  TChannelTypeAndNum = record
    F: TCUDAChannelType;
    C: TCUDAChannelNum;
  end;

  TCUDAMapping = (grmDefault, grmReadOnly, grmWriteDiscard);

  TCUDAComponent = class(TCUDAHandlesMaster)
  private
    FMaster: TCUDAComponent;
    FItems: TGLPersistentObjectList;
    procedure SetMaster(AMaster: TCUDAComponent);
    function GetItem(const i: Integer): TCUDAComponent;
    function GetItemsCount: Integer;
  protected
    FStatus: TCUresult;
    FChanges: TCUDAChanges;
    function GetContext: TCUDAContext; override;
    procedure CollectStatus(AStatus: TCUresult);
    procedure GetChildren(AProc: TGetChildProc; Root: TComponent); override;
    procedure AddItem(AItem: TCUDAComponent);
    procedure RemoveItem(AItem: TCUDAComponent);
    procedure DeleteItems;
    procedure SetName(const NewName: TComponentName); override;
    function GetIsAllocated: Boolean; virtual; abstract;
  public
    destructor Destroy; override;
    procedure CuNotifyChange(AChange: TCUDAChange); virtual;
    function GetParentComponent: TComponent; override;
    procedure SetParentComponent(Value: TComponent); override;
    function HasParent: Boolean; override;
    function GetItemByName(const name: string): TCUDAComponent;
    function MakeUniqueName(const BaseName: string): string;
    property Master: TCUDAComponent read FMaster write SetMaster;
    property Context: TCUDAContext read GetContext;
    property Items[const i: Integer]: TCUDAComponent read GetItem;
    property ItemsCount: Integer read GetItemsCount;
    property Status: TCUresult read FStatus;
    // Return true if handle is allocated (i.e. component has device object)
    property IsAllocated: Boolean read GetIsAllocated;
  end;

  TCUDAComponentClass = class of TCUDAComponent;

  TCUDAMemData = class;
  TCUDAFunction = class;
  TCUDATexture = class;
  TGLCUDA = class;
  TCUDAConstant = class;

  TCUDAModule = class(TCUDAComponent)
  private
    FHandle: PCUmodule;
    FCode: TStringList;
    FCodeType: TGLCUDACompilerOutput;
    FCompiler: TGLCUDACompiler;
    procedure SetCode(const Value: TStringList);
    procedure SetCompiler(const Value: TGLCUDACompiler);
    function GetKernelFunction(const AName: string): TCUDAFunction;
    function GetKernelTexture(const AName: string): TCUDATexture;
    function GetKernelConstant(const AName: string): TCUDAConstant;
  protected
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
    procedure OnChangeCode(Sender: TObject);
    procedure Loaded; override;
    function GetContext: TCUDAContext; override;
    function GetIsAllocated: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromSource;
    procedure Unload;
    procedure LoadAndCompile;
    property Context: TCUDAContext read GetContext;
    property CodeType: TGLCUDACompilerOutput read FCodeType;
    property KernelFunction[const AName: string]: TCUDAFunction
      read GetKernelFunction;
    property KernelTexture[const AName: string]: TCUDATexture
      read GetKernelTexture;
    property KernelConstant[const AName: string]: TCUDAConstant
      read GetKernelConstant;
  published
    property Code: TStringList read FCode write SetCode;
    property Compiler: TGLCUDACompiler read FCompiler write SetCompiler;
  end;

  TGLResourceType = (rtTexture, rtBuffer);

  //  Abstract class of graphic resources.
  TCUDAGraphicResource = class(TCUDAComponent)
  protected
    FHandle: array [0 .. 7] of PCUgraphicsResource;
    FMapping: TCUDAMapping;
    FResourceType: TGLResourceType;
    FGLContextHandle: TGLVirtualHandle;
    FMapCounter: Integer;
    function GetIsAllocated: Boolean; override;
    procedure OnGLHandleAllocate(Sender: TGLVirtualHandle;
      var Handle: Cardinal);
    procedure OnGLHandleDestroy(Sender: TGLVirtualHandle; var Handle: Cardinal);
    procedure BindArrayToTexture(var cudaArray: TCUDAMemData;
      ALeyer, ALevel: LongWord); virtual; abstract;
    procedure SetArray(var AArray: TCUDAMemData; AHandle: PCUarray;
      ForGLTexture, Volume: Boolean);
    function GetAttributeArraySize(const Attr: string): LongWord; virtual; abstract;
    function GetAttributeArrayAddress(const Attr: string): Pointer; virtual;
      abstract;
    function GetElementArrayDataSize: LongWord; virtual; abstract;
    function GetElementArrayAddress: Pointer; virtual; abstract;
    procedure SetMapping(const Value: TCUDAMapping); virtual;
    property Mapping: TCUDAMapping read FMapping write SetMapping
      default grmDefault;
  public
    procedure MapResources; virtual; abstract;
    procedure UnMapResources; virtual; abstract;
  end;

  TCUDAMemType = (mtHost, mtDevice, mtArray);
  TCUDAMemMapFlag =
  (
    mmfPortable, // Memory is shared between contexts
    mmfFastWrite // Fast write, slow read
  );
  TCUDAMemMapFlags = set of TCUDAMemMapFlag;

  TCUDAMemData = class(TCUDAComponent)
  private
    FData: TCUdeviceptr;
    FMappedMemory: TCUdeviceptr;
    FHandle: PCUarray;
    FWidth: Integer;
    FHeight: Integer;
    FDepth: Integer;
    FPitch: Cardinal;
    FElementSize: Integer;
    FDataSize: Integer;
    FChannelsType: TCUDAChannelType;
    fChannelsNum: TCUDAChannelNum;
    FMemoryType: TCUDAMemType;
    FTexture: TCUDATexture;
    FOpenGLRefArray: Boolean;
    FMapping: Boolean;
    procedure SetMemoryType(const AType: TCUDAMemType);
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetDepth(const Value: Integer);
    procedure SetChannelType(const Value: TCUDAChannelType);
    procedure SetChannelNum(const Value: TCUDAChannelNum);
    function GetData: TCUdeviceptr;
    function GetArrayHandle: PCUarray;
  protected
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
    function GetIsAllocated: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CuNotifyChange(AChange: TCUDAChange); override;
    (* Map device and array memory to host or host memory to device.
       Mapping is necessary for modifying device data.
       When mapped host memory - it can be accessed in device side
       via MappedHostAddress. *)
    procedure Map(const AFlags: TCUDAMemMapFlags = []);
    // Done mapping operation.
    procedure UnMap;
    function Data<EType>(X: Integer): GCUDAHostElementAccess<EType>; overload;
    function Data<EType>(X, Y: Integer): GCUDAHostElementAccess<EType>; overload;
    function Data<EType>(X, Y, Z: Integer): GCUDAHostElementAccess<EType>; overload;
    //  Fill device data
    procedure FillMem(const Value);
    procedure CopyTo(const ADstMemData: TCUDAMemData); overload;
    procedure CopyTo(const AGLImage: TGLImage); overload;
    //  Copy data to Graphic resource.
    procedure CopyTo(const AGLGraphic: TCUDAGraphicResource;
      aAttr: string = ''); overload;
    procedure CopyFrom(const ASrcMemData: TCUDAMemData); overload;
    procedure CopyFrom(const AGLImage: TGLBitmap32); overload;
    procedure CopyFrom(const AGLGraphic: TCUDAGraphicResource;
      aAttr: string = ''); overload;
    procedure SubCopyTo(const ADstMemData: TCUDAMemData;
      ASrcXYZ, ADstXYZ, ASizes: IntElement.TVector3);
    property ElementSize: Integer read FElementSize;
    property DataSize: Integer read FDataSize;
    property Pitch: Cardinal read fPitch;
    property RawData: TCUdeviceptr read GetData;
    property MappedMemoryAddress: TCUdeviceptr read FMappedMemory;
    property ArrayHandle: PCUarray read GetArrayHandle;
  published
    property Width: Integer read fWidth write SetWidth default 256;
    property Height: Integer read fHeight write SetHeight default 0;
    property Depth: Integer read fDepth write SetDepth default 0;
    property MemoryType: TCUDAMemType read FMemoryType write SetMemoryType
      default mtHost;
    property ChannelsType: TCUDAChannelType read fChannelsType
      write SetChannelType default ctInt8;
    property ChannelsNum: TCUDAChannelNum read fChannelsNum write SetChannelNum
      default cnOne;
  end;

  TCUDAUniform = class(TCUDAComponent)
  protected
    FHandle: TCUdeviceptr;
    FSize: Cardinal;
    FKernelName: string;
    FType: TCUDAType;
    FCustomType: string;
    FRef: Boolean;
    FDefined: Boolean;
    procedure SetKernelName(const AName: string);
    procedure SetType(AValue: TCUDAType);
    procedure SetCustomType(const AValue: string);
    procedure SetSize(const AValue: Cardinal);
    procedure SetRef(AValue: Boolean);
    procedure SetDefined(AValue: Boolean);

    property KernelName: string read FKernelName write SetKernelName;
    property DataType: TCUDAType read FType write SetType;
    property CustomType: string read FCustomType write SetCustomType;
    property Size: Cardinal read FSize write SetSize;
    property Reference: Boolean read FRef write SetRef;
    function GetIsAllocated: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IsValueDefined: Boolean read FDefined write SetDefined;
  end;


  TCUDAConstant = class(TCUDAUniform)
  protected
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
    function GetDeviceAddress: TCUdeviceptr;
  public
    property DeviceAddress: TCUdeviceptr read GetDeviceAddress;
  published
    property KernelName;
    property DataType;
    property CustomType;
    property Size;
    property Reference;
  end;

  TCUDAFuncParam = class(TCUDAUniform)
  protected
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property KernelName;
    property DataType;
    property CustomType;
    property Size;
    property Reference;
  end;

  TCUDAFunction = class(TCUDAComponent)
  private
    FKernelName: string;
    FHandle: PCUfunction;
    FAutoSync: Boolean;
    FBlockShape: TCUDADimensions;
    FGrid: TCUDADimensions;
    ParamOffset: Integer;
    FLaunching: Boolean;
    FOnParameterSetup: TNotifyEvent;
    procedure SetBlockShape(const AShape: TCUDADimensions);
    procedure SetGrid(const AGrid: TCUDADimensions);
    procedure SetKernelName(const AName: string);
    function GetHandle: PCUfunction;
    procedure SetSharedMemorySize(Value: Integer);
    function GetSharedMemorySize: Integer;
    function GetMaxThreadPerBlock: Integer;
    function GetConstMemorySize: Integer;
    function GetLocalMemorySize: Integer;
    function GetNumRegisters: Integer;
    function GetParameter(const AName: string): TCUDAFuncParam;
  protected
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
    function GetIsAllocated: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetParam(Value: Integer); overload;
    procedure SetParam(Value: Cardinal); overload;
    procedure SetParam(Value: Single); overload;
    procedure SetParam(Value: TVector2i); overload;
    procedure SetParam(Value: TVector3i); overload;
    procedure SetParam(Value: TVector4i); overload;
    procedure SetParam(Value: TVector2f); overload;
    procedure SetParam(Value: TVector3f); overload;
    procedure SetParam(Value: TVector4f); overload;
    procedure SetParam(MemData: TCUDAMemData); overload;
    procedure SetParam(TexRef: TCUDATexture); overload;
    procedure SetParam(Ptr: Pointer); overload;
    property Parameters[const AName: string]: TCUDAFuncParam read GetParameter;
    procedure Launch(Grided: Boolean = true);
    property Handle: PCUfunction read GetHandle;
    property SharedMemorySize: Integer read GetSharedMemorySize
      write SetSharedMemorySize;
    property MaxThreadPerBlock: Integer read GetMaxThreadPerBlock;
    property ConstMemorySize: Integer read GetConstMemorySize;
    property LocalMemorySize: Integer read GetLocalMemorySize;
    property NumRegisters: Integer read GetNumRegisters;
  published
    property KernelName: string read FKernelName write SetKernelName;
    property AutoSync: Boolean read FAutoSync write FAutoSync default true;
    property BlockShape: TCUDADimensions read FBlockShape write SetBlockShape;
    property Grid: TCUDADimensions read FGrid write SetGrid;
    property OnParameterSetup: TNotifyEvent read FOnParameterSetup
      write FOnParameterSetup;
  end;

  TCUDATexture = class(TCUDAComponent)
  private
    FKernelName: string;
    FHandle: PCUtexref;
    fArray: TCUDAMemData;
    fAddressModeS, fAddressModeT, fAddressModeR: TCuAddresMode;
    fNormalizedCoord: Boolean;
    fReadAsInteger: Boolean;
    fFilterMode: TCuFilterMode;
    fFormat: TCUDAChannelType;
    fChannelNum: TCUDAChannelNum;
    procedure SetKernelName(const AName: string);
    procedure SetAddressModeS(const AMode: TCuAddresMode);
    procedure SetAddressModeT(const AMode: TCuAddresMode);
    procedure SetAddressModeR(const AMode: TCuAddresMode);
    procedure SetNormalizedCoord(const flag: Boolean);
    procedure SetReadAsInteger(const flag: Boolean);
    procedure SetFilterMode(const mode: TCuFilterMode);
    procedure SetFormat(AValue: TCUDAChannelType);
    procedure SetChannelNum(AValue: TCUDAChannelNum);
    procedure SetArray(Value: TCUDAMemData);
    function GetHandle: PCUtexref;
  protected
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
    function GetIsAllocated: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Handle: PCUtexref read GetHandle;
  published
    property KernelName: string read FKernelName write SetKernelName;
    property AddressModeS: TCuAddresMode read fAddressModeS
      write SetAddressModeS default amClamp;
    property AddressModeT: TCuAddresMode read fAddressModeT
      write SetAddressModeT default amClamp;
    property AddressModeR: TCuAddresMode read fAddressModeR
      write SetAddressModeR default amClamp;
    property NormalizedCoord: Boolean read fNormalizedCoord
      write SetNormalizedCoord default true;
    property ReadAsInteger: Boolean read fReadAsInteger write SetReadAsInteger
      default false;
    property FilterMode: TCuFilterMode read fFilterMode write SetFilterMode
      default fmPoint;
    property Format: TCUDAChannelType read fFormat write SetFormat;
    property ChannelNum: TCUDAChannelNum read fChannelNum write SetChannelNum;
    property MemDataArray: TCUDAMemData read fArray write SetArray;
  end;

  TGLCUDA = class(TCUDAComponent)
  private
    fDevice: TGLCUDADevice;
    fContext: TCUDAContext;
    FOnOpenGLInteropInit: TOnOpenGLInteropInit;
    procedure SetDevice(const Value: TGLCUDADevice);
    procedure SetOnOpenGLInteropInit(AEvent: TOnOpenGLInteropInit);
    function GetModule(const i: Integer): TCUDAModule;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function GetContext: TCUDAContext; override;
    function GetIsAllocated: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Context: TCUDAContext read GetContext;
    property Modules[const i: Integer]: TCUDAModule read GetModule;
  published
    
    property ComputingDevice: TGLCUDADevice read fDevice write SetDevice;
    property OnOpenGLInteropInit: TOnOpenGLInteropInit read FOnOpenGLInteropInit
      write SetOnOpenGLInteropInit;
  end;

function GetChannelTypeAndNum(AType: TCUDAType): TChannelTypeAndNum;
procedure RegisterCUDAComponentNameChangeEvent(ANotifyEvent: TNotifyEvent);
procedure DeRegisterCUDAComponentNameChangeEvent;

//-----------------------------------------------------------------
implementation
//-----------------------------------------------------------------


const
  cAddressMode: array [TCuAddresMode] of TCUaddress_mode =
    (CU_TR_ADDRESS_MODE_WRAP, CU_TR_ADDRESS_MODE_CLAMP,
    CU_TR_ADDRESS_MODE_MIRROR);

  cFilterMode: array [TCuFilterMode] of TCUfilter_mode =
    (CU_TR_FILTER_MODE_POINT, CU_TR_FILTER_MODE_LINEAR);

const
  cCUDATypeToTexFormat: array [TCUDAType] of TChannelTypeAndNum =
    ((F: ctUndefined; C: cnOne), (F: ctInt8; C: cnOne), (F: ctUInt8; C: cnOne),
    (F: ctInt8; C: cnTwo), (F: ctUInt8; C: cnTwo), (F: ctInt8; C: cnThree),
    (F: ctUInt8; C: cnThree), (F: ctInt8; C: cnFour), (F: ctUInt8; C: cnFour),
    (F: ctInt16; C: cnOne), (F: ctUInt16; C: cnOne), (F: ctInt16; C: cnTwo),
    (F: ctUInt16; C: cnTwo), (F: ctInt16; C: cnThree), (F: ctUInt16;
    C: cnThree), (F: ctInt16; C: cnFour), (F: ctUInt16; C: cnFour), (F: ctInt32;
    C: cnOne), (F: ctUInt32; C: cnOne), (F: ctInt32; C: cnTwo), (F: ctUInt32;
    C: cnTwo), (F: ctInt32; C: cnThree), (F: ctUInt32; C: cnThree), (F: ctInt32;
    C: cnFour), (F: ctUInt32; C: cnFour), (F: ctUndefined; C: cnOne),
    (F: ctUndefined; C: cnOne), (F: ctUndefined; C: cnTwo), (F: ctUndefined;
    C: cnTwo), (F: ctUndefined; C: cnThree), (F: ctUndefined; C: cnThree),
    (F: ctUndefined; C: cnFour), (F: ctUndefined; C: cnFour), (F: ctFloat;
    C: cnOne), (F: ctFloat; C: cnTwo), (F: ctFloat; C: cnThree), (F: ctFloat;
    C: cnFour), (F: ctUndefined; C: cnOne), (F: ctUndefined; C: cnOne),
    (F: ctUndefined; C: cnTwo), (F: ctUndefined; C: cnTwo), (F: ctUndefined;
    C: cnThree), (F: ctUndefined; C: cnThree), (F: ctUndefined; C: cnFour),
    (F: ctUndefined; C: cnFour), (F: ctUndefined; C: cnOne), (F: ctUndefined;
    C: cnTwo), (F: ctUndefined; C: cnThree), (F: ctUndefined; C: cnFour),
    (F: ctInt8; C: cnOne), (F: ctInt16; C: cnOne), (F: ctInt32; C: cnOne),
    (F: ctUInt8; C: cnOne), (F: ctUInt16; C: cnOne), (F: ctUInt32; C: cnOne));

  cChannelTypeSize: array [TCUDAChannelType] of Integer =
      (0, 1, 2, 4, 1, 2, 4, 2, 4, 8);

var
  GLVirtualHandleCounter: Cardinal = 1;
  vCUDAComponentNameChangeEvent: TNotifyEvent;

function GetChannelTypeAndNum(AType: TCUDAType): TChannelTypeAndNum;
begin
  Result := cCUDATypeToTexFormat[AType];
end;

procedure CUDAEnumToChannelDesc(const Fmt: TCUarray_format; const nCh: LongWord;
  out oFormat: TCUDAChannelType; out oNum: TCUDAChannelNum);
begin
  case Fmt of
    CU_AD_FORMAT_UNSIGNED_INT8:
      oFormat := ctUInt8;
    CU_AD_FORMAT_UNSIGNED_INT16:
      oFormat := ctUInt16;
    CU_AD_FORMAT_UNSIGNED_INT32:
      oFormat := ctUInt32;
    CU_AD_FORMAT_SIGNED_INT8:
      oFormat := ctUInt8;
    CU_AD_FORMAT_SIGNED_INT16:
      oFormat := ctUInt16;
    CU_AD_FORMAT_SIGNED_INT32:
      oFormat := ctUInt32;
    CU_AD_FORMAT_HALF:
      oFormat := ctHalfFloat;
    CU_AD_FORMAT_FLOAT:
      oFormat := ctFloat;
  end;
  case nCh of
    1: oNum := cnOne;
    2: oNum := cnTwo;
    3: oNum := cnThree;
    4: oNum := cnFour;
  end;
end;

procedure RegisterCUDAComponentNameChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  vCUDAComponentNameChangeEvent := ANotifyEvent;
end;

procedure DeRegisterCUDAComponentNameChangeEvent;
begin
  vCUDAComponentNameChangeEvent := nil;
end;

// ------------------
// ------------------ TGLCUDA ------------------
// ------------------

constructor TGLCUDA.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDevice := nil;
  fContext := TCUDAContext.Create;
  FChanges := [];
end;

destructor TGLCUDA.Destroy;
begin
  ComputingDevice := nil;
  fContext.Destroy;
  inherited;
end;

procedure TGLCUDA.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = fDevice) then
    ComputingDevice := nil;
  inherited;
end;

procedure TGLCUDA.SetDevice(const Value: TGLCUDADevice);
begin
  if Value <> fDevice then
  begin
    if Assigned(Value) and not Value.Suitable then
      exit;
    if Assigned(fDevice) then
      fDevice.RemoveFreeNotification(Self);
    fDevice := Value;
    if Assigned(fDevice) then
    begin
      fDevice.FreeNotification(Self);
      CuNotifyChange(cuchDevice);
    end;
  end;
end;

procedure TGLCUDA.SetOnOpenGLInteropInit(AEvent: TOnOpenGLInteropInit);
begin
  FOnOpenGLInteropInit := AEvent;
  CuNotifyChange(cuchContext);
end;

function TGLCUDA.GetContext: TCUDAContext;
begin
  if cuchDevice in FChanges then
  begin
    if Assigned(fDevice) then
      fContext.Device := fDevice.Device
    else
      fContext.Device := nil;
    Exclude(FChanges, cuchDevice);
    Include(FChanges, cuchContext);
  end;

  if (cuchContext in FChanges) and Assigned(fDevice) then
  begin
    // Getting OpenGL context to make interoperability
    fContext.OnOpenGLInteropInit := FOnOpenGLInteropInit;
    CUDAContextManager.CreateContext(fContext);
    Exclude(FChanges, cuchContext);
  end;

  Result := fContext;
end;

function TGLCUDA.GetIsAllocated: Boolean;
begin
  Result := FContext.IsValid;
end;

function TGLCUDA.GetModule(const i: Integer): TCUDAModule;
var
  j, k: Integer;
begin
  Result := nil;
  k := 0;
  for j := 0 to FItems.Count - 1 do
  begin
    if FItems[j] is TCUDAModule then
    begin
      if k = i then
        exit(TCUDAModule(FItems[j]))
      else
        Inc(k);
    end;
  end;
end;

 
// ------------------
// ------------------ TCUDAModule ------------------
// ------------------

constructor TCUDAModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := nil;
  FCode := TStringList.Create;
  TStringList(FCode).OnChange := OnChangeCode;
end;

destructor TCUDAModule.Destroy;
begin
  Unload;
  FCode.Destroy;
  if Assigned(FCompiler) then
    FCompiler.Product := nil;
  inherited;
end;

procedure TCUDAModule.Assign(Source: TPersistent);
var
  module: TCUDAModule;
begin
  if Source is TCUDAModule then
  begin
    DestroyHandles;
    module := TCUDAModule(Source);
    FCode.Assign(module.FCode);
    FCodeType := module.FCodeType;
    AllocateHandles;
  end;
  inherited Assign(Source);
end;

procedure TCUDAModule.SetCompiler(const Value: TGLCUDACompiler);
begin
  if Value <> FCompiler then
  begin
    // Compiler must used by only one module
    if Assigned(Value) and Assigned(Value.Product) then
      exit;
    FCompiler := Value;
    if Assigned(FCompiler) then
      FCompiler.Product := FCode;
  end;
end;

function TCUDAModule.GetContext: TCUDAContext;
begin
  if Assigned(FMaster) and (FMaster is TGLCUDA) then
    Result := TGLCUDA(FMaster).Context
  else
  begin
    Result := nil;
    {$IFDEF USE_LOGGING}
      LogErrorFmt('Invalid master of module "%s"', [Name]);
    {$ENDIF}
    Abort;
  end;
end;

function TCUDAModule.GetIsAllocated: Boolean;
begin
  Result := Assigned(FHandle);
end;

procedure TCUDAModule.Loaded;
var
  I: Integer;
begin
  inherited Loaded;
  LoadFromSource;
  for i := ItemsCount - 1 downto 0 do
    Items[i].AllocateHandles;
end;

procedure TCUDAModule.AllocateHandles;
var
  func: TCUDAFunction;
  tex: TCUDATexture;
  cnst: TCUDAConstant;
  Param: TCUDAFuncParam;
  i, j: Integer;
  useless: array of TCUDAComponent;
  info: TCUDAModuleInfo;
  bFail: Boolean;
begin
  LoadFromSource;

  if Assigned(FCompiler) then
  begin
    info := FCompiler.ModuleInfo;
    info.Owner := Self;

    // Runtime module deployment
    if not(csDesigning in ComponentState) and Assigned(FCompiler) then
    begin

      // Redefine function and texture with same names
      for i := 0 to High(info.func) do
      begin
        func := GetKernelFunction(info.func[i].Name);
        if not Assigned(func) then
        begin
          func := TCUDAFunction.Create(Self);
          func.Master := Self;
          func.FKernelName := info.func[i].KernelName;
          func.Name := MakeUniqueName(info.func[i].Name);
        end
        else
          func.DeleteItems;

        try
          bFail := func.Handle = nil;
        except
          bFail := True;
        end;

        if bFail then
          func.Destroy
        else
        begin
          for j := 0 to High(info.func[i].Args) do
          begin
            Param := TCUDAFuncParam.Create(func);
            Param.Master := TCUDAComponent(func);
            Param.FKernelName := info.func[i].Args[j].Name;
            Param.Name := func.KernelName + '_' + Param.KernelName;
            Param.FType := info.func[i].Args[j].DataType;
            Param.FCustomType := info.func[i].Args[j].CustomType;
            Param.FRef := info.func[i].Args[j].Ref;
            // Lock properties
            Param.AllocateHandles;
          end;
        end;

      end;

      for i := 0 to High(info.TexRef) do
      begin
        tex := GetKernelTexture(info.TexRef[i].Name);
        if not Assigned(tex) then
        begin
          tex := TCUDATexture.Create(Self);
          tex.Master := Self;
          tex.FKernelName := info.TexRef[i].Name;
          tex.fReadAsInteger :=
            (info.TexRef[i].ReadMode = cudaReadModeElementType);
          tex.fFormat := cCUDATypeToTexFormat[info.TexRef[i].DataType].F;
          tex.fChannelNum := cCUDATypeToTexFormat[info.TexRef[i].DataType].C;
          tex.Name := MakeUniqueName(tex.FKernelName);
        end;

        try
          bFail := tex.Handle = nil;
        except
          bFail := True;
        end;

        if bFail then
          tex.Destroy;
      end;

      for i := 0 to High(info.Constant) do
      begin
        cnst := GetKernelConstant(info.Constant[i].Name);
        if not Assigned(cnst) then
        begin
          cnst := TCUDAConstant.Create(Self);
          cnst.Master := Self;
          cnst.FKernelName := info.Constant[i].Name;
          cnst.FType := info.Constant[i].DataType;
          cnst.FCustomType := info.Constant[i].CustomType;
          cnst.Name := MakeUniqueName(cnst.FKernelName);
          cnst.IsValueDefined := info.Constant[i].DefValue;
        end;

        try
          bFail := cnst.DeviceAddress = nil;
        except
          bFail := True;
        end;

        if bFail then
          cnst.Destroy;
      end;

      // Delete useless components
      SetLength(useless, ItemsCount);
      j := 0;
      for i := 0 to ItemsCount - 1 do
        if not Items[i].IsAllocated then
          begin
            useless[j] := Items[i];
            Inc(j);
          end;
      for i := 0 to j - 1 do
        useless[i].Destroy;
    end;
  end;
end;

procedure TCUDAModule.DestroyHandles;
var
  I: Integer;
begin
  for I := 0 to ItemsCount - 1 do
    TCUDAComponent(Items[I]).DestroyHandles;
end;

procedure TCUDAModule.LoadFromFile(const AFilename: string);
var
  Status: TCUresult;
  ext: string;
  AnsiFileName: AnsiString;
begin
  if FileExists(AFilename) then
  begin
    ext := ExtractFileExt(AFilename);
    System.Delete(ext, 1, 1);
    ext := AnsiLowerCase(ext);
    FCodeType := codeUndefined;
    if ext = 'ptx' then
      FCodeType := codePtx;
    if ext = 'cubin' then
      FCodeType := codeCubin;
    if ext = 'gpu' then
      FCodeType := codeGpu;

    if (FCodeType = codePtx) or (FCodeType = codeCubin) then
    begin
      Unload;
      Context.Requires;
      AnsiFileName := AnsiString(AFilename);
      Status := cuModuleLoad(FHandle, PAnsiChar(AnsiFileName));
      Context.Release;
      if Status <> CUDA_SUCCESS then
        Abort;
      FCode.LoadFromFile(AFilename);
      Compiler := nil;
      AllocateHandles;
    end
    else
      {$IFDEF USE_LOGGING}
        LogErrorFmt('%s.LoadFromFile: file extension must be ptx or cubin', [Self.ClassName]);
     {$ENDIF}
  end
  else
   {$IFDEF USE_LOGGING}
    LogErrorFmt(strFailedOpenFile, [AFilename]);
   {$ENDIF}
end;

procedure TCUDAModule.LoadFromSource;
var
  Text: AnsiString;
begin
  Text := AnsiString(FCode.Text);
  if Length(Text) > 0 then
  begin
    DestroyHandles;

    Text := Text + #00;
    Context.Requires;
    FStatus := cuModuleLoadData(FHandle, PAnsiChar(Text));
    Context.Release;
    if FStatus <> CUDA_SUCCESS then
      Abort;
  end;
end;

procedure TCUDAModule.LoadAndCompile;
begin
  AllocateHandles;
end;

procedure TCUDAModule.Unload;
begin
  if Assigned(FHandle) then
  begin
    DestroyHandles;
    DeleteItems;
    Context.Requires;
    FStatus := cuModuleUnload(FHandle);
    Context.Release;
    FHandle := nil;
  end;
end;

procedure TCUDAModule.OnChangeCode(Sender: TObject);
begin
  if not(csLoading in ComponentState) and (Sender is TGLCUDACompiler) then
  begin
    AllocateHandles;
  end;
end;

procedure TCUDAModule.SetCode(const Value: TStringList);
begin
  FCode.Assign(Value);
end;

function TCUDAModule.GetKernelFunction(const AName: string): TCUDAFunction;
var
  i: Integer;
  item: TComponent;
begin
  Result := nil;
  for i := 0 to Self.ItemsCount - 1 do
  begin
    item := Items[i];
    if item is TCUDAFunction then
      if TCUDAFunction(item).KernelName = AName then
        exit(TCUDAFunction(item));
  end;
end;

function TCUDAModule.GetKernelTexture(const AName: string): TCUDATexture;
var
  i: Integer;
  item: TComponent;
begin
  Result := nil;
  for i := 0 to Self.ItemsCount - 1 do
  begin
    item := Items[i];
    if item is TCUDATexture then
      if TCUDATexture(item).KernelName = AName then
        exit(TCUDATexture(item));
  end;
end;


function TCUDAModule.GetKernelConstant(const AName: string): TCUDAConstant;
var
  i: Integer;
  item: TComponent;
begin
  Result := nil;
  for i := 0 to Self.ItemsCount - 1 do
  begin
    item := Items[i];
    if item is TCUDAConstant then
      if TCUDAConstant(item).KernelName = AName then
        exit(TCUDAConstant(item));
  end;
end;

 
// ------------------
// ------------------ TCUDAComponent ------------------
// ------------------

destructor TCUDAComponent.Destroy;
begin
  if Assigned(FMaster) then
    FMaster.RemoveItem(Self);
  if Assigned(FItems) then
  begin
    DeleteItems;
    FItems.Free;
  end;
  inherited;
end;

procedure TCUDAComponent.CuNotifyChange(AChange: TCUDAChange);
begin
  Include(FChanges, AChange);
end;

function TCUDAComponent.GetContext: TCUDAContext;
begin
  if Self is TGLCUDA then
    Result := TGLCUDA(Self).Context
  else
    Result := TGLCUDA(FMaster).Context;
end;

procedure TCUDAComponent.CollectStatus(AStatus: TCUresult);
begin
  if AStatus <> CUDA_SUCCESS then
    FStatus := AStatus;
end;

procedure TCUDAComponent.GetChildren(AProc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  if Assigned(FItems) then
    for i := 0 to FItems.Count - 1 do
      if not IsSubComponent(TComponent(FItems.List^[i])) then
        AProc(TComponent(FItems.List^[i]));
end;

procedure TCUDAComponent.SetParentComponent(Value: TComponent);
begin
  inherited;
  if Self is TGLCUDA then
    exit;
  if Value <> FMaster then
    Master := TCUDAComponent(Value);
end;

function TCUDAComponent.GetParentComponent: TComponent;
begin
  Result := FMaster;
end;

function TCUDAComponent.HasParent: Boolean;
begin
  Result := Assigned(FMaster);
end;

procedure TCUDAComponent.SetMaster(AMaster: TCUDAComponent);
begin
  if Assigned(FMaster) then
    FMaster.RemoveItem(Self);
  FMaster := AMaster;
  if Assigned(FMaster) then
    FMaster.AddItem(Self);
end;

procedure TCUDAComponent.SetName(const NewName: TComponentName);
begin
  if Name <> NewName then
  begin
    inherited SetName(NewName);
    if Assigned(vCUDAComponentNameChangeEvent) then
      vCUDAComponentNameChangeEvent(Self);
  end;
end;

procedure TCUDAComponent.AddItem(AItem: TCUDAComponent);
begin
  if not Assigned(FItems) then
    FItems := TGLPersistentObjectList.Create;
  FItems.Add(AItem);
end;

procedure TCUDAComponent.RemoveItem(AItem: TCUDAComponent);
begin
  if not Assigned(FItems) then
    exit;
  if AItem.FMaster = Self then
  begin
    if AItem.Owner = Self then
      RemoveComponent(AItem);
    FItems.Remove(AItem);
    AItem.FMaster := nil;
  end;
end;

procedure TCUDAComponent.DeleteItems;
var
  child: TCUDAComponent;
begin
  if Assigned(FItems) then
    while FItems.Count > 0 do
    begin
      child := TCUDAComponent(FItems.Pop);
      child.Free;
    end;
end;

function TCUDAComponent.GetItem(const i: Integer): TCUDAComponent;
begin
  if Assigned(FItems) and (i < FItems.Count) then
    Result := TCUDAComponent(FItems[i])
  else
    Result := nil;
end;

function TCUDAComponent.GetItemsCount: Integer;
begin
  if Assigned(FItems) then
    Result := FItems.Count
  else
    Result := 0;
end;

function TCUDAComponent.GetItemByName(const name: string): TCUDAComponent;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to GetItemsCount - 1 do
  begin
    if Items[i].Name = name then
    begin
      Result := Items[i];
      exit;
    end;
  end;
end;

function TCUDAComponent.MakeUniqueName(const BaseName: string): string;
var
  i: Integer;
begin
  Result := BaseName + '1';
  i := 2;
  while GetItemByName(Result) <> nil do
  begin
    Result := BaseName + IntToStr(i);
    Inc(i);
  end;
end;

 
// ------------------
// ------------------ TCUDAFunction ------------------
// ------------------

constructor TCUDAFunction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := nil;
  FAutoSync := true;
  FBlockShape := TCUDADimensions.Create(Self);
  FGrid := TCUDADimensions.Create(Self);
  FLaunching := false;
end;

 
destructor TCUDAFunction.Destroy;
begin
  FBlockShape.Destroy;
  FGrid.Destroy;
  DestroyHandles;
  inherited;
end;

procedure TCUDAFunction.AllocateHandles;
var
  LModule: TCUDAModule;
  ansiname: AnsiString;
  pFunc: PCUfunction;
begin
  DestroyHandles;

  if not(FMaster is TCUDAModule) then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strModuleAbsent);
    {$ENDIF}
    Abort;
  end;

  if Length(FKernelName) = 0 then
    exit;

  LModule := TCUDAModule(FMaster);
  if not Assigned(LModule.FHandle) then
    exit;

  with LModule.Context.Device do
  begin
    FBlockShape.MaxSizeX := MaxThreadsDim.SizeX;
    FBlockShape.MaxSizeY := MaxThreadsDim.SizeY;
    FBlockShape.MaxSizeZ := MaxThreadsDim.SizeZ;
    FGrid.MaxSizeX := MaxGridSize.SizeX;
    FGrid.MaxSizeY := MaxGridSize.SizeY;
    FGrid.MaxSizeZ := MaxGridSize.SizeZ;
  end;

  ansiname := AnsiString(FKernelName);
  Context.Requires;
  FStatus := cuModuleGetFunction(pFunc, LModule.FHandle, PAnsiChar(ansiname));
  Context.Release;
  if FStatus = CUDA_SUCCESS then
    FHandle := pFunc
  else
    Abort;
  inherited;
end;

procedure TCUDAFunction.DestroyHandles;
var
  i: Integer;
  item: TComponent;
begin
  if Assigned(FHandle) then
  begin
    for i := 0 to ItemsCount - 1 do
    begin
      item := Items[i];
      if item is TCUDAFuncParam then
        TCUDAFuncParam(item).DestroyHandles;
    end;
    FHandle := nil;
    inherited;
  end;
end;

procedure TCUDAFunction.SetBlockShape(const AShape: TCUDADimensions);
begin
  FBlockShape.Assign(AShape);
end;

procedure TCUDAFunction.SetGrid(const AGrid: TCUDADimensions);
begin
  FGrid.Assign(AGrid);
end;

procedure TCUDAFunction.SetKernelName(const AName: string);
begin
  if csLoading in ComponentState then
    FKernelName := AName
  else if not Assigned(FHandle) then
  begin
    FKernelName := AName;
    AllocateHandles;
  end;
end;

procedure TCUDAFunction.SetParam(Value: Integer);
begin
  if not FLaunching then
  begin
   {$IFDEF USE_LOGGING}
     LogError(strWrongParamSetup);
   {$ENDIF}
    Abort;
  end;
  FStatus := cuParamSeti(FHandle, ParamOffset, PCardinal(@Value)^);
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(Cardinal));
end;

procedure TCUDAFunction.SetParam(Value: Cardinal);
begin
  if not FLaunching then
  begin
    {$IFDEF USE_LOGGING}
     LogError(strWrongParamSetup);
   {$ENDIF}
    Abort;
  end;
  FStatus := cuParamSeti(FHandle, ParamOffset, Value);
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(Cardinal));
end;

procedure TCUDAFunction.SetParam(Value: Single);
begin
  if not FLaunching then
  begin
   {$IFDEF USE_LOGGING}
     LogError(strWrongParamSetup);
   {$ENDIF}
    Abort;
  end;
  FStatus := cuParamSetf(FHandle, ParamOffset, Value);
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(Single));
end;

procedure TCUDAFunction.SetParam(Value: TVector2i);
begin
  if not FLaunching then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strWrongParamSetup);
    {$ENDIF}
    Abort;
  end;
  FStatus := cuParamSetv(FHandle, ParamOffset, Value, SizeOf(TVector2i));
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(TVector2i));
end;

procedure TCUDAFunction.SetParam(Value: TVector3i);
begin
  if not FLaunching then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strWrongParamSetup);
    {$ENDIF}
    Abort;
  end;
  FStatus := cuParamSetv(FHandle, ParamOffset, Value, SizeOf(TVector3i));
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(TVector3i));
end;

procedure TCUDAFunction.SetParam(Value: TVector4i);
begin
  if not FLaunching then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strWrongParamSetup);
    {$ENDIF}
    Abort;
  end;
  FStatus := cuParamSetv(FHandle, ParamOffset, Value, SizeOf(TVector4i));
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(TVector4i));
end;

procedure TCUDAFunction.SetParam(Value: TVector2f);
begin
  if not FLaunching then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strWrongParamSetup);
    {$ENDIF}
    Abort;
  end;
  FStatus := cuParamSetv(FHandle, ParamOffset, Value, SizeOf(TVector2f));
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(TVector2f));
end;

procedure TCUDAFunction.SetParam(Value: TVector3f);
begin
  if not FLaunching then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strWrongParamSetup);
    {$ENDIF}
    Abort;
  end;
  FStatus := cuParamSetv(FHandle, ParamOffset, Value, SizeOf(TVector3f));
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(TVector4f));
end;

procedure TCUDAFunction.SetParam(Value: TVector4f);
begin
  if not FLaunching then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strWrongParamSetup);
    {$ENDIF}
    Abort;
  end;
  FStatus := cuParamSetv(FHandle, ParamOffset, Value, SizeOf(TVector4f));
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(TVector4f));
end;

procedure TCUDAFunction.SetParam(MemData: TCUDAMemData);
begin
  if not FLaunching then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strWrongParamSetup);
    {$ENDIF}
    Abort;
  end;
  FStatus := cuParamSeti(FHandle, ParamOffset, Cardinal(MemData.RawData));
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(Cardinal));
end;

procedure TCUDAFunction.SetParam(TexRef: TCUDATexture);
var
  HTexRef: PCUtexref;
begin
  if not FLaunching then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strWrongParamSetup);
    {$ENDIF}
    Abort;
  end;
  HTexRef := TexRef.Handle;
  FStatus := cuParamSetTexRef(FHandle, CU_PARAM_TR_DEFAULT, HTexRef);
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

procedure TCUDAFunction.SetParam(Ptr: Pointer);
begin
  if not FLaunching then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strWrongParamSetup);
    {$ENDIF}
    Abort;
  end;
  FStatus := cuParamSeti(FHandle, ParamOffset, Cardinal(Ptr));
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(Cardinal));
end;

procedure TCUDAFunction.Launch(Grided: Boolean = true);
begin
  if not(FMaster is TCUDAModule) then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strModuleAbsent);
    {$ENDIF}
    Abort;
  end;

  if not Assigned(FHandle) then
  begin
    {$IFDEF USE_LOGGING}
      LogErrorFmt(strFuncNotConnected, [Self.ClassName]);
    {$ENDIF}
    Abort;
  end;

  if FLaunching then
    exit;

  ParamOffset := 0;

  Context.Requires;
  FLaunching := true;
  if Assigned(FOnParameterSetup) then
    try
      FOnParameterSetup(Self);
    except
      FLaunching := false;
      Context.Release;
      raise;
    end;
  FLaunching := false;

  FStatus := cuParamSetSize(FHandle, ParamOffset);
  CollectStatus(cuFuncSetBlockShape(FHandle, FBlockShape.SizeX,
    FBlockShape.SizeY, FBlockShape.SizeZ));

  if FStatus = CUDA_SUCCESS then
  begin
    // execute the kernel
    if Grided then
      FStatus := cuLaunchGrid(FHandle, FGrid.SizeX, FGrid.SizeY)
    else
      FStatus := cuLaunch(FHandle);
    if FAutoSync then
      CollectStatus(cuCtxSynchronize);
  end;
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
  begin
    {$IFDEF USE_LOGGING}
      LogErrorFmt(strLaunchFailed, [Self.Name]);
    {$ENDIF}
    Abort;
  end;
end;

function TCUDAFunction.GetHandle: PCUfunction;
begin
  if FHandle = nil then
    AllocateHandles;
  Result := FHandle;
end;

function TCUDAFunction.GetIsAllocated: Boolean;
begin
  Result := Assigned(FHandle);
end;

function TCUDAFunction.GetMaxThreadPerBlock: Integer;
begin
  Context.Requires;
  FStatus := cuFuncGetAttribute(Result,
    CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK, Handle);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

function TCUDAFunction.GetSharedMemorySize: Integer;
begin
  Context.Requires;
  FStatus := cuFuncGetAttribute(Result,
    CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES, Handle);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

procedure TCUDAFunction.SetSharedMemorySize(Value: Integer);
var
  MemPerBlock: NativeUInt;
begin
  Context.Requires;
  MemPerBlock := TGLCUDA(TCUDAModule(FMaster).FMaster)
    .fDevice.Device.SharedMemPerBlock;
  if Value < 0 then
    Value := 0
  else if Value > Integer(MemPerBlock) then
    Value := MemPerBlock;
  FStatus := cuFuncSetSharedSize(Handle, Value);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

function TCUDAFunction.GetConstMemorySize: Integer;
begin
  Context.Requires;
  FStatus := cuFuncGetAttribute(Result,
    CU_FUNC_ATTRIBUTE_CONST_SIZE_BYTES, Handle);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

function TCUDAFunction.GetLocalMemorySize: Integer;
begin
  Context.Requires;
  FStatus := cuFuncGetAttribute(Result,
    CU_FUNC_ATTRIBUTE_LOCAL_SIZE_BYTES, Handle);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

function TCUDAFunction.GetNumRegisters: Integer;
begin
  Context.Requires;
  FStatus := cuFuncGetAttribute(Result, CU_FUNC_ATTRIBUTE_NUM_REGS, Handle);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

function TCUDAFunction.GetParameter(const AName: string): TCUDAFuncParam;
var
  i: Integer;
  item: TComponent;
begin
  Result := nil;
  for i := 0 to Self.ItemsCount - 1 do
  begin
    item := Items[i];
    if item is TCUDAFuncParam then
      if TCUDAFuncParam(item).KernelName = AName then
        exit(TCUDAFuncParam(item));
  end;
end;

 
// ------------------
// ------------------ TCUDAMemData ------------------
// ------------------

constructor TCUDAMemData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fData := nil;
  FHandle := nil;
  FMemoryType := mtHost;
  fWidth := 256;
  fHeight := 0;
  fDepth := 0;
  fPitch := 0;
  fChannelsType := ctInt8;
  fChannelsNum := cnOne;
  FOpenGLRefArray := False;
  FMapping := False;
end;

function TCUDAMemData.Data<EType>(X: Integer): GCUDAHostElementAccess<EType>;
var
  ptr: PByte;
  size: Integer;
begin
  if (FMemoryType <> mtHost) and not FMapping then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strOnlyHostData);
    {$ENDIF}
    Abort;
  end;

  if FMapping then
    ptr := PByte(FMappedMemory)
  else
    ptr := PByte(GetData);
  size := ElementSize * X;
  if size > DataSize then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strOutOfRange);
    {$ENDIF}
    Abort;
  end;
  Inc(ptr, size);
  SetElementAccessAddress(ptr, ElementSize);
end;

function TCUDAMemData.Data<EType>(X, Y: Integer): GCUDAHostElementAccess<EType>;
var
  ptr: PByte;
  size: Integer;
begin
  if (FMemoryType <> mtHost) and not FMapping then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strOnlyHostData);
    {$ENDIF}
    Abort;
  end;

  if FMapping then
    ptr := PByte(FMappedMemory)
  else
    ptr := PByte(GetData);
  size := ElementSize * (X + fWidth*Y);
  if size > DataSize then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strOutOfRange);
    {$ENDIF}
    Abort;
  end;
  Inc(ptr, size);
  SetElementAccessAddress(ptr, ElementSize);
end;

function TCUDAMemData.Data<EType>(X, Y, Z: Integer): GCUDAHostElementAccess<EType>;
var
  ptr: PByte;
  size: Integer;
begin
  if (FMemoryType <> mtHost) and not FMapping then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strOnlyHostData);
    {$ENDIF}
    Abort;
  end;

  if FMapping then
    ptr := PByte(FMappedMemory)
  else
    ptr := PByte(GetData);
  size := ElementSize * (X + fWidth*(Y  + Z * fHeight));
  if size > DataSize then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strOutOfRange);
    {$ENDIF}
    Abort;
  end;
  Inc(ptr, size);
  SetElementAccessAddress(ptr, ElementSize);
end;

destructor TCUDAMemData.Destroy;
begin
  if Assigned(fTexture) then
    fTexture.MemDataArray := nil;
  DestroyHandles;
  inherited;
end;

procedure TCUDAMemData.CuNotifyChange(AChange: TCUDAChange);
begin
  inherited CuNotifyChange(AChange);
  if Assigned(fTexture) then
    fTexture.CuNotifyChange(cuchArray);
end;

procedure TCUDAMemData.SetMemoryType(const AType: TCUDAMemType);
begin
  if FMemoryType <> AType then
  begin
    FMemoryType := AType;
    if (AType = mtArray) and (fChannelsType = ctDouble) then
      SetChannelType(ctFloat);
    CuNotifyChange(cuchArray);
  end;
end;

procedure TCUDAMemData.SetWidth(const Value: Integer);
begin
  Assert(Value > 0);
  if Value <> fWidth then
  begin
    fWidth := Value;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAMemData.UnMap;
begin
  if not FMapping then
  begin
    {$IFDEF USE_LOGGING}
      LogErrorFmt(strFailUnmap, [Name]);
    {$ENDIF}
    Abort;
  end;

  Context.Requires;

  case FMemoryType of
    mtHost:
      begin
        FStatus := CUDA_SUCCESS;
      end;
    mtDevice:
      begin
        FStatus := cuMemcpyHtoD(GetData, FMappedMemory, DataSize);
        if FStatus = CUDA_SUCCESS then
          FStatus := cuMemFreeHost(FMappedMemory);
      end;
    mtArray:
      begin
        FStatus := cuMemcpyHtoA(GetArrayHandle, 0, FMappedMemory, DataSize);
        if FStatus = CUDA_SUCCESS then
          FStatus := cuMemFreeHost(FMappedMemory);
      end;
  end;

  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;

  FMapping := False;
  FMappedMemory := nil;
end;

procedure TCUDAMemData.SetHeight(const Value: Integer);
begin
  Assert(Value >= 0);
  if Value <> fHeight then
  begin
    fHeight := Value;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAMemData.SetDepth(const Value: Integer);
begin
  Assert(Value >= 0);
  if Value <> fDepth then
  begin
    fDepth := Value;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAMemData.SetChannelType(const Value: TCUDAChannelType);
begin
  Assert(Value <> ctUndefined);
  if (FMemoryType = mtArray) and (Value = ctDouble) then
    exit;
  if Value <> fChannelsType then
  begin
    fChannelsType := Value;
    CuNotifyChange(cuchFormat);
  end;
end;

procedure TCUDAMemData.SetChannelNum(const Value: TCUDAChannelNum);
begin
  if Value <> fChannelsNum then
  begin
    fChannelsNum := Value;
    CuNotifyChange(cuchFormat);
  end;
end;

function TCUDAMemData.GetData: TCUdeviceptr;
begin
  if not Assigned(fData) and (FChanges <> []) then
    AllocateHandles;
  Result := fData;
end;

function TCUDAMemData.GetArrayHandle: PCUarray;
begin
  if not Assigned(FHandle) and (FChanges <> []) then
    AllocateHandles;
  Result := FHandle;
end;

procedure TCUDAMemData.AllocateHandles;
const
  cArrayFormat: array [ctUInt8 .. ctFloat] of TCUarray_format =
    (CU_AD_FORMAT_UNSIGNED_INT8, CU_AD_FORMAT_UNSIGNED_INT16,
    CU_AD_FORMAT_UNSIGNED_INT32, CU_AD_FORMAT_SIGNED_INT8,
    CU_AD_FORMAT_SIGNED_INT16, CU_AD_FORMAT_SIGNED_INT32, CU_AD_FORMAT_HALF,
    CU_AD_FORMAT_FLOAT);
var
  h, d: Integer;
  Array2DDesc: TCUDA_ARRAY_DESCRIPTOR;
  // Array3DDesc: TCUDA_ARRAY3D_DESCRIPTOR;
  AlignedSize: Integer;
begin
  DestroyHandles;

  if cuchFormat in FChanges then
  begin
    FElementSize := cChannelTypeSize[fChannelsType] * (Ord(fChannelsNum) + 1);
  end;

  h := Height;
  if h = 0 then
    h := 1;
  d := Depth;
  if d = 0 then
    d := 1;
  FDataSize := Width * h * d * ElementSize;

  FStatus := CUDA_SUCCESS;
  Context.Requires;
  case FMemoryType of
    mtHost:
      FStatus := cuMemAllocHost(fData, DataSize);
    mtDevice:
      begin
        if fHeight > 1 then
        begin
          AlignedSize := RoundUpToPowerOf2(ElementSize);
          if AlignedSize < 4 then
            AlignedSize := 4;
          if AlignedSize > 16 then
            AlignedSize := 16;
          FStatus := cuMemAllocPitch(TCUdeviceptr(fData), fPitch,
            Width * ElementSize, fHeight, AlignedSize);
        end
        else
          FStatus := cuMemAlloc(TCUdeviceptr(fData), DataSize);
      end;
    mtArray:
      begin
        Array2DDesc.Width := fWidth;
        Array2DDesc.Height := fHeight;
        Array2DDesc.Format := cArrayFormat[fChannelsType];
        Array2DDesc.NumChannels := Ord(fChannelsNum) + 1;
        FStatus := cuArrayCreate(FHandle, Array2DDesc);
      end;
  end;
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
    Abort;

  FChanges := [];
  inherited;
end;

procedure TCUDAMemData.DestroyHandles;
begin
  case FMemoryType of
    mtHost, mtDevice:
      if fData = nil then
        exit;
    mtArray:
      if FHandle = nil then
        exit;
  end;

  inherited;

  if not FOpenGLRefArray then
  begin
    Context.Requires;
    case FMemoryType of
      mtHost:
        if Assigned(fData) then
          cuMemFreeHost(fData);

      mtDevice:
        if Assigned(fData) then
          cuMemFree(fData);

      mtArray:
        if Assigned(FHandle) then
        begin
          if Assigned(fTexture) then
            fTexture.MemDataArray := nil;
          cuArrayDestroy(FHandle);
        end;
    end;
    Context.Release;
  end;
  FHandle := nil;
  fData := nil;
  fPitch := 0;
  FDataSize := 0;
  FElementSize := 0;
  FOpenGLRefArray := False;
end;

procedure TCUDAMemData.FillMem(const Value);
var
  Ptr: TCUdeviceptr;
  RowSize: Integer;
begin
  if FMemoryType = mtDevice then
  begin
    Ptr := GetData;
    FStatus := CUDA_SUCCESS;
    Context.Requires;
    // 1D memory set
    if fHeight = 0 then
    begin
      case fChannelsType of
        ctUInt8, ctInt8:
          FStatus := cuMemsetD8(Ptr, Byte(Value), DataSize);
        ctUInt16, ctInt16, ctHalfFloat:
          FStatus := cuMemsetD16(Ptr, Word(Value), DataSize div SizeOf(Word));
        ctUInt32, ctInt32, ctFloat:
          FStatus := cuMemsetD32(Ptr, DWord(Value), DataSize div SizeOf(DWord));
      end;
    end
    // 2D memory set
    else
    begin
      RowSize := (1 + Ord(fChannelsNum)) * fWidth;
      case fChannelsType of
        ctUInt8, ctInt8:
          FStatus := cuMemsetD2D8(Ptr, fPitch, Byte(Value), RowSize, fHeight);
        ctUInt16, ctInt16, ctHalfFloat:
          FStatus := cuMemsetD2D16(Ptr, fPitch, Word(Value), RowSize,
            fHeight);
        ctUInt32, ctInt32, ctFloat:
          FStatus := cuMemsetD2D32(Ptr, fPitch, DWord(Value),
            RowSize, fHeight);
      end;
    end;
    Context.Release;
    if FStatus <> CUDA_SUCCESS then
      Abort
  end;
end;

procedure TCUDAMemData.CopyTo(const ADstMemData: TCUDAMemData);
var
  copyParam2D: TCUDA_MEMCPY2D;
  // copyParam3D: TCUDA_MEMCPY3D;
  Size: Integer;
begin
  if not Assigned(ADstMemData) then
    exit;

  Assert((fDepth = 0) and (ADstMemData.Depth = 0),
    'Volume copying not yet implemented');

  FStatus := CUDA_SUCCESS;

  if (Height = ADstMemData.Height) and (Height = 0) then
  begin
    // 1D copying
    Size := MinInteger(DataSize, ADstMemData.DataSize);
    Context.Requires;
    case MemoryType of
      mtHost:
        case ADstMemData.MemoryType of
          mtHost:
            Move(RawData^, ADstMemData.RawData^, Size);
          mtDevice:
            FStatus := cuMemcpyHtoD(ADstMemData.RawData, RawData, Size);
          mtArray:
            FStatus := cuMemcpyHtoA(ADstMemData.ArrayHandle, 0, RawData, Size);
        end;

      mtDevice:
        case ADstMemData.MemoryType of
          mtHost:
            FStatus := cuMemcpyDtoH(ADstMemData.RawData, RawData, Size);
          mtDevice:
            FStatus := cuMemcpyDtoD(ADstMemData.RawData, RawData, Size);
          mtArray:
            FStatus := cuMemcpyDtoA(ADstMemData.ArrayHandle, 0, RawData, Size);
        end;

      mtArray:
        case ADstMemData.MemoryType of
          mtHost:
            FStatus := cuMemcpyAtoH(ADstMemData.RawData, ArrayHandle, 0, Size);
          mtDevice:
            FStatus := cuMemcpyAtoD(ADstMemData.RawData, ArrayHandle, 0, Size);
          mtArray:
            FStatus := cuMemcpyAtoA(ADstMemData.ArrayHandle, 0,
              ArrayHandle, 0, Size);
        end;
    end;
    Context.Release;
  end
  else
  begin
    // 2D copying
    FillChar(copyParam2D, SizeOf(copyParam2D), 0);
    // Setup source copy parameters
    case MemoryType of
      mtHost:
        begin
          copyParam2D.srcMemoryType := CU_MEMORYTYPE_HOST;
          copyParam2D.srcHost := TCUdeviceptr(RawData);
        end;
      mtDevice:
        begin
          copyParam2D.srcMemoryType := CU_MEMORYTYPE_DEVICE;
          copyParam2D.srcDevice := TCUdeviceptr(RawData);
        end;
      mtArray:
        begin
          copyParam2D.srcMemoryType := CU_MEMORYTYPE_ARRAY;
          copyParam2D.srcArray := ArrayHandle;
        end;
    end;
    copyParam2D.srcPitch := fPitch;
    // Setup destination copy parameters
    case ADstMemData.FMemoryType of
      mtHost:
        begin
          copyParam2D.dstMemoryType := CU_MEMORYTYPE_HOST;
          copyParam2D.dstHost := TCUdeviceptr(ADstMemData.RawData);
        end;
      mtDevice:
        begin
          copyParam2D.dstMemoryType := CU_MEMORYTYPE_DEVICE;
          copyParam2D.dstDevice := TCUdeviceptr(ADstMemData.RawData);
        end;
      mtArray:
        begin
          copyParam2D.dstMemoryType := CU_MEMORYTYPE_ARRAY;
          copyParam2D.dstArray := ADstMemData.ArrayHandle;
        end;
    end;
    copyParam2D.dstPitch := ADstMemData.fPitch;

    copyParam2D.WidthInBytes := Cardinal(MinInteger(ElementSize * Width,
      ADstMemData.ElementSize * ADstMemData.Width));
    copyParam2D.Height := MinInteger(fHeight, ADstMemData.Height);

    Context.Requires;
    FStatus := cuMemcpy2D(@copyParam2D);
    Context.Release;
  end;

  if FStatus <> CUDA_SUCCESS then
    Abort
end;

procedure TCUDAMemData.SubCopyTo(const ADstMemData: TCUDAMemData;
  ASrcXYZ, ADstXYZ, ASizes: IntElement.TVector3);
var
  copyParam2D: TCUDA_MEMCPY2D;
  // copyParam3D: TCUDA_MEMCPY3D;
begin
  if not Assigned(ADstMemData) then
    exit;

  // Clamp sizes
  ASrcXYZ[0] := MinInteger(ASrcXYZ[0], Width - 1);
  ASrcXYZ[1] := MinInteger(ASrcXYZ[1], MaxInteger(Height - 1, 0));
  ASrcXYZ[2] := MinInteger(ASrcXYZ[2], MaxInteger(Depth - 1, 0));

  ADstXYZ[0] := MinInteger(ADstXYZ[0], ADstMemData.Width - 1);
  ADstXYZ[1] := MinInteger(ADstXYZ[1], MaxInteger(ADstMemData.Height - 1, 0));
  ADstXYZ[2] := MinInteger(ADstXYZ[2], MaxInteger(ADstMemData.Depth - 1, 0));

  ASizes[0] := MinInteger(ASizes[0], Width, ADstMemData.Width);
  ASizes[1] := MinInteger(ASizes[1], Height, ADstMemData.Height);
  ASizes[2] := MinInteger(ASizes[2], Depth, ADstMemData.Depth);

  Assert(ASizes[2] = 0, 'Volume copying not yet implemented');

  FStatus := CUDA_SUCCESS;

  if ASizes[2] = 0 then
  begin
    // 2D copying
    FillChar(copyParam2D, SizeOf(copyParam2D), 0);
    // Setup source copy parameters
    case MemoryType of
      mtHost:
        begin
          copyParam2D.srcMemoryType := CU_MEMORYTYPE_HOST;
          copyParam2D.srcHost := TCUdeviceptr(RawData);
        end;
      mtDevice:
        begin
          copyParam2D.srcMemoryType := CU_MEMORYTYPE_DEVICE;
          copyParam2D.srcDevice := TCUdeviceptr(RawData);
        end;
      mtArray:
        begin
          copyParam2D.srcMemoryType := CU_MEMORYTYPE_ARRAY;
          copyParam2D.srcArray := ArrayHandle;
        end;
    end;
    copyParam2D.srcXInBytes := ASrcXYZ[0] * FElementSize;
    copyParam2D.srcY := ASrcXYZ[1];
    copyParam2D.srcPitch := fPitch;
    // Setup destination copy parameters
    case ADstMemData.FMemoryType of
      mtHost:
        begin
          copyParam2D.dstMemoryType := CU_MEMORYTYPE_HOST;
          copyParam2D.dstHost := TCUdeviceptr(ADstMemData.RawData);
        end;
      mtDevice:
        begin
          copyParam2D.dstMemoryType := CU_MEMORYTYPE_DEVICE;
          copyParam2D.dstDevice := TCUdeviceptr(ADstMemData.RawData);
        end;
      mtArray:
        begin
          copyParam2D.dstMemoryType := CU_MEMORYTYPE_ARRAY;
          copyParam2D.dstArray := ADstMemData.ArrayHandle;
        end;
    end;
    copyParam2D.dstXInBytes := ADstXYZ[0] * ADstMemData.FElementSize;
    copyParam2D.dstY := ADstXYZ[1];
    copyParam2D.dstPitch := ADstMemData.fPitch;

    copyParam2D.WidthInBytes := Cardinal(MinInteger(ElementSize * ASizes[0],
      ADstMemData.ElementSize * ASizes[0]));
    copyParam2D.Height := MaxInteger(ASizes[1], 1);

    Context.Requires;
    FStatus := cuMemcpy2D(@copyParam2D);
    Context.Release;
  end;

  if FStatus <> CUDA_SUCCESS then
    Abort
end;

procedure TCUDAMemData.CopyTo(const AGLImage: TGLBitmap32);
var
  copyParam2D: TCUDA_MEMCPY2D;
  // copyParam3D: TCUDA_MEMCPY3D;
begin
  if not Assigned(AGLImage) then
    exit;

  Assert((fDepth = 0) and (AGLImage.Depth = 0),
    'Volume copying not yet implemented');

  FillChar(copyParam2D, SizeOf(copyParam2D), 0);
  // Setup source copy parameters
  case FMemoryType of
    mtHost:
      begin
        copyParam2D.srcMemoryType := CU_MEMORYTYPE_HOST;
        copyParam2D.srcHost := TCUdeviceptr(RawData);
      end;
    mtDevice:
      begin
        copyParam2D.srcMemoryType := CU_MEMORYTYPE_DEVICE;
        copyParam2D.srcDevice := TCUdeviceptr(RawData);
      end;
    mtArray:
      begin
        copyParam2D.srcMemoryType := CU_MEMORYTYPE_ARRAY;
        copyParam2D.srcArray := ArrayHandle;
      end;
  end;
  copyParam2D.srcPitch := fPitch;
  // Setup destination copy parameters
  copyParam2D.dstMemoryType := CU_MEMORYTYPE_HOST;
  copyParam2D.dstHost := AGLImage.Data;
  copyParam2D.dstPitch := AGLImage.ElementSize * AGLImage.Width;

  copyParam2D.WidthInBytes :=
    MinInteger(Cardinal(ElementSize * Width), copyParam2D.dstPitch);
  copyParam2D.Height := MinInteger(Height, AGLImage.Height);

  Context.Requires;
  FStatus := cuMemcpy2D(@copyParam2D);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

procedure TCUDAMemData.CopyTo(const AGLGraphic: TCUDAGraphicResource;
  aAttr: string);
var
  pMap: TCUdeviceptr;
  mapSize: Integer;
begin
  if not Assigned(AGLGraphic.FHandle[0]) then
    exit;

  Context.Requires;
  AGLGraphic.MapResources;

  if AGLGraphic.FResourceType = rtBuffer then
  begin
    if Length(aAttr) = 0 then
    begin
      mapSize := AGLGraphic.GetElementArrayDataSize;
      pMap := AGLGraphic.GetElementArrayAddress;
    end
    else
    begin
      mapSize := AGLGraphic.GetAttributeArraySize(aAttr);
      pMap := AGLGraphic.GetAttributeArrayAddress(aAttr);
    end;
  end
  else
  begin
    // TODO: image copying
    AGLGraphic.UnMapResources;
    Context.Release;
    exit;
  end;

  FStatus := CUDA_SUCCESS;

  case FMemoryType of
    mtHost:
      FStatus := cuMemcpyHtoD(pMap, RawData, MinInteger(DataSize, mapSize));
    mtDevice:
      FStatus := cuMemcpyDtoD(pMap, RawData, MinInteger(DataSize, mapSize));
    mtArray:
      FStatus := cuMemcpyAtoD(pMap, ArrayHandle, 0,
        MinInteger(DataSize, mapSize));
  end;

  AGLGraphic.UnMapResources;
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

procedure TCUDAMemData.CopyFrom(const ASrcMemData: TCUDAMemData);
begin
  ASrcMemData.CopyTo(Self);
end;

procedure TCUDAMemData.CopyFrom(const AGLImage: TGLBitmap32);
var
  copyParam2D: TCUDA_MEMCPY2D;
  // copyParam3D: TCUDA_MEMCPY3D;
begin
  if not Assigned(AGLImage) then
    exit;

  Assert((fDepth = 0) and (AGLImage.Depth = 0),
    'Volume copying not yet implemented');

  FillChar(copyParam2D, SizeOf(copyParam2D), 0);
  // Setup destination copy parameters
  case FMemoryType of
    mtHost:
      begin
        copyParam2D.dstMemoryType := CU_MEMORYTYPE_HOST;
        copyParam2D.dstHost := TCUdeviceptr(RawData);
      end;
    mtDevice:
      begin
        copyParam2D.dstMemoryType := CU_MEMORYTYPE_DEVICE;
        copyParam2D.dstDevice := TCUdeviceptr(RawData);
      end;
    mtArray:
      begin
        copyParam2D.dstMemoryType := CU_MEMORYTYPE_ARRAY;
        copyParam2D.dstArray := ArrayHandle;
      end;
  end;
  copyParam2D.dstPitch := fPitch;
  // Setup source copy parameters
  copyParam2D.srcMemoryType := CU_MEMORYTYPE_HOST;
  copyParam2D.srcHost := AGLImage.Data;
  copyParam2D.srcPitch := AGLImage.ElementSize * AGLImage.Width;

  copyParam2D.WidthInBytes := MinInteger(
    Cardinal(ElementSize * fWidth), copyParam2D.srcPitch);
  copyParam2D.Height := MinInteger(fHeight, AGLImage.Height);

  Context.Requires;
  FStatus := cuMemcpy2D(@copyParam2D);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

procedure TCUDAMemData.CopyFrom(const AGLGraphic: TCUDAGraphicResource;
  aAttr: string);
var
  pMap: TCUdeviceptr;
  mapSize: Integer;
begin
  if not Assigned(AGLGraphic.FHandle[0]) then
    exit;

  Assert(fDepth = 0, 'Volume copying not yet implemented');

  Context.Requires;
  AGLGraphic.MapResources;

  if AGLGraphic.fResourceType = rtBuffer then
  begin
    if Length(aAttr) = 0 then
    begin
      mapSize := AGLGraphic.GetElementArrayDataSize;
      pMap := AGLGraphic.GetElementArrayAddress;
    end
    else
    begin
      mapSize := AGLGraphic.GetAttributeArraySize(aAttr);
      pMap := AGLGraphic.GetAttributeArrayAddress(aAttr);
    end;
  end
  else
  begin
    // TODO: image copying
    AGLGraphic.UnMapResources;
    Context.Release;
    exit;
  end;

  FStatus := CUDA_SUCCESS;


  case FMemoryType of
    mtHost:
      FStatus := cuMemcpyDtoH(RawData, pMap,
        Cardinal(MinInteger(DataSize, mapSize)));
    mtDevice:
      FStatus := cuMemcpyDtoD(RawData, pMap,
        Cardinal(MinInteger(DataSize, mapSize)));
    mtArray:
      FStatus := cuMemcpyDtoA(ArrayHandle, 0, pMap,
        Cardinal(MinInteger(DataSize, mapSize)));
  end;
  AGLGraphic.UnMapResources;
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

function TCUDAMemData.GetIsAllocated: Boolean;
begin
  case FMemoryType of
    mtHost, mtDevice: Result := Assigned(FData);
    mtArray: Result := Assigned(FHandle);
    else
      Result := False;
  end;
end;

procedure TCUDAMemData.Map(const AFlags: TCUDAMemMapFlags);
var
  LFlag: Cardinal;
begin
  if FMapping then
  begin
    {$IFDEF USE_LOGGING}
      LogErrorFmt(strFailMap, [Name]);
    {$ENDIF}
    Abort;
  end;

  LFlag := 0;
  if mmfPortable in AFlags then
    LFlag := LFlag or CU_MEMHOSTALLOC_PORTABLE;
  if mmfFastWrite in AFlags then
    LFlag := LFlag or CU_MEMHOSTALLOC_WRITECOMBINED;

  Context.Requires;
  GetData;

  case FMemoryType of
    mtHost:
      begin
        FStatus := cuMemHostGetDevicePointer(
          FMappedMemory, GetData, 0);
      end;
    mtDevice:
      begin
        FStatus := cuMemHostAlloc(
          FMappedMemory, DataSize, LFlag);
        if FStatus = CUDA_SUCCESS then
          FStatus := cuMemcpyDtoH(
            FMappedMemory, GetData, DataSize);
      end;
    mtArray:
      begin
        FStatus := cuMemHostAlloc(
          FMappedMemory, DataSize, LFlag);
        if FStatus = CUDA_SUCCESS then
          FStatus := cuMemcpyAtoH(
            FMappedMemory, GetArrayHandle, 0, DataSize);
      end;
  end;

  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;

  FMapping := True;
end;

// ------------------
// ------------------ TCUDATexture ------------------
// ------------------

constructor TCUDATexture.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := nil;
  fArray := nil;
  AddressModeS := amClamp;
  AddressModeT := amClamp;
  AddressModeR := amClamp;
  NormalizedCoord := true;
  ReadAsInteger := false;
  FilterMode := fmPoint;
  fFormat := ctUndefined;
  fChannelNum := cnOne;
end;


destructor TCUDATexture.Destroy;
begin
  if Assigned(fArray) then
    fArray.fTexture := nil;
  DestroyHandles;
  inherited;
end;

function TCUDATexture.GetHandle: PCUtexref;
begin
  if not Assigned(FHandle) or (FChanges <> []) then
    AllocateHandles;
  Result := FHandle;
end;

function TCUDATexture.GetIsAllocated: Boolean;
begin
  Result := Assigned(FHandle);
end;

procedure TCUDATexture.AllocateHandles;
var
  pTex: PCUtexref;
  LName: AnsiString;
  LModule: TCUDAModule;
  LFlag: Cardinal;
  LFormat: TCUarray_format;
  LChanels: Integer;
begin
  if not(FMaster is TCUDAModule) then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strModuleAbsent);
    {$ENDIF}
    Abort;
  end;

  if Length(FKernelName) = 0 then
    exit;

  LModule := TCUDAModule(FMaster);

  LName := AnsiString(FKernelName);
  Context.Requires;
  FStatus := cuModuleGetTexRef(pTex, LModule.FHandle, PAnsiChar(LName));
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
  FHandle := pTex;

  Context.Requires;
  // Apply changes
  if (cuchArray in FChanges) and Assigned(fArray) then
  begin
    CollectStatus(cuTexRefSetArray(FHandle, fArray.ArrayHandle,
      CU_TRSA_OVERRIDE_FORMAT));
    fArray.fTexture := Self;
    // Update format
    if cuTexRefGetFormat(LFormat, LChanels, FHandle) = CUDA_SUCCESS then
      CUDAEnumToChannelDesc(LFormat, LChanels, fFormat, fChannelNum);
  end;

  if cuchAddresMode in FChanges then
  begin
    CollectStatus(cuTexRefSetAddressMode(FHandle, 0,
      cAddressMode[fAddressModeS]));
    CollectStatus(cuTexRefSetAddressMode(FHandle, 1,
      cAddressMode[fAddressModeT]));
    CollectStatus(cuTexRefSetAddressMode(FHandle, 2,
      cAddressMode[fAddressModeR]));
  end;

  if cuchFlag in FChanges then
  begin
    LFlag := 0;
    if fNormalizedCoord then
      LFlag := LFlag or CU_TRSF_NORMALIZED_COORDINATES;
    if fReadAsInteger then
      LFlag := LFlag or CU_TRSF_READ_AS_INTEGER;
    CollectStatus(cuTexRefSetFlags(FHandle, LFlag));
  end;

  if cuchFilterMode in FChanges then
    CollectStatus(cuTexRefSetFilterMode(FHandle, cFilterMode[fFilterMode]));

  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;

  FChanges := [];
  inherited;
end;

procedure TCUDATexture.DestroyHandles;
begin
  if Assigned(FHandle) then
  begin
    FHandle := nil;
    inherited;
  end;
end;

procedure TCUDATexture.SetKernelName(const AName: string);
begin
  if csLoading in ComponentState then
    FKernelName := AName
  else if not Assigned(FHandle) then
  begin
    FKernelName := AName;
    AllocateHandles;
  end;
end;

// SetAddressModeS
//

procedure TCUDATexture.SetAddressModeS(const AMode: TCuAddresMode);
begin
  if AMode <> fAddressModeS then
  begin
    fAddressModeS := AMode;
    CuNotifyChange(cuchAddresMode);
  end;
end;

procedure TCUDATexture.SetAddressModeT(const AMode: TCuAddresMode);
begin
  if AMode <> fAddressModeT then
  begin
    fAddressModeT := AMode;
    CuNotifyChange(cuchAddresMode);
  end;
end;

procedure TCUDATexture.SetAddressModeR(const AMode: TCuAddresMode);
begin
  if AMode <> fAddressModeR then
  begin
    fAddressModeR := AMode;
    CuNotifyChange(cuchAddresMode);
  end;
end;

procedure TCUDATexture.SetNormalizedCoord(const flag: Boolean);
begin
  if flag <> fNormalizedCoord then
  begin
    fNormalizedCoord := flag;
    CuNotifyChange(cuchFlag);
  end;
end;

procedure TCUDATexture.SetReadAsInteger(const flag: Boolean);
begin
  if flag <> fReadAsInteger then
  begin
    fReadAsInteger := flag;
    CuNotifyChange(cuchFlag);
  end;
end;

procedure TCUDATexture.SetFilterMode(const mode: TCuFilterMode);
begin
  if mode <> fFilterMode then
  begin
    fFilterMode := mode;
    CuNotifyChange(cuchFilterMode);
  end;
end;

procedure TCUDATexture.SetFormat(AValue: TCUDAChannelType);
begin
  if csLoading in ComponentState then
    fFormat := AValue
  else if not Assigned(FHandle) then
  begin
    fFormat := AValue;
    CuNotifyChange(cuchFormat);
  end;
end;

procedure TCUDATexture.SetArray(Value: TCUDAMemData);
begin
  if Value <> fArray then
  begin
    if Assigned(fArray) then
      fArray.fTexture := nil;
    if Assigned(Value) then
    begin
      if Value.MemoryType <> mtArray then
        Value := nil
      else
      begin
        fFormat := Value.fChannelsType;
        fChannelNum := Value.fChannelsNum;
        if Assigned(Value.fTexture) then
          Value.fTexture.MemDataArray := nil;
        Value.fTexture := Self;
      end;
    end
    else
    begin
      fFormat := ctUndefined;
      fChannelNum := cnOne;
    end;
    fArray := Value;
    CuNotifyChange(cuchArray);
  end;
end;

procedure TCUDATexture.SetChannelNum(AValue: TCUDAChannelNum);
begin
  if csLoading in ComponentState then
    fChannelNum := AValue
  else if not Assigned(FHandle) then
  begin
    fChannelNum := AValue;
    CuNotifyChange(cuchFormat);
  end;
end;

 
// ------------------
// ------------------ TCUDAGraphicResource ------------------
// ------------------

procedure TCUDAGraphicResource.SetMapping(const Value: TCUDAMapping);
begin
  if fMapping <> Value then
  begin
    fMapping := Value;
    CuNotifyChange(cuchMapping);
  end;
end;

function TCUDAGraphicResource.GetIsAllocated: Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FHandle) do
    if Assigned(FHandle[I]) then
      exit(True);
  Result := False;
end;

procedure TCUDAGraphicResource.OnGLHandleAllocate(Sender: TGLVirtualHandle;
  var Handle: Cardinal);
begin
  Handle := GLVirtualHandleCounter;
  Inc(GLVirtualHandleCounter);
end;

procedure TCUDAGraphicResource.OnGLHandleDestroy(Sender: TGLVirtualHandle;
  var Handle: Cardinal);
begin
  DestroyHandles;
end;

procedure TCUDAGraphicResource.SetArray(var AArray: TCUDAMemData;
  AHandle: PCUarray; ForGLTexture, Volume: Boolean);
var
  Desc2D: TCUDA_ARRAY_DESCRIPTOR;
  Desc3D: TCUDA_ARRAY3D_DESCRIPTOR;
begin
  Context.Requires;
  // Get array descriptor
  if Volume then
    FStatus := cuArray3DGetDescriptor(Desc3D, AHandle)
  else
    FStatus := cuArrayGetDescriptor(Desc2D, AHandle);
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
    Abort;

  // Set array parameters
  if not Assigned(AArray) then
    AArray := TCUDAMemData.Create(Owner);

  with AArray do
  begin
    if FHandle <> AHandle then
    begin
      DestroyHandles;
      FHandle := AHandle;
    end;
    FOpenGLRefArray := ForGLTexture;
    FMemoryType := mtArray;
    FPitch := 0;
    if Volume then
    begin
      fWidth := Desc3D.Width;
      fHeight := Desc3D.Height;
      fDepth := Desc3D.Depth;
      CUDAEnumToChannelDesc(Desc3D.Format, Desc3D.NumChannels, fChannelsType,
        fChannelsNum);
    end
    else
    begin
      fWidth := Desc2D.Width;
      fHeight := Desc2D.Height;
      fDepth := 0;
      CUDAEnumToChannelDesc(Desc2D.Format, Desc2D.NumChannels, fChannelsType,
        fChannelsNum);
    end;
    FElementSize := cChannelTypeSize[fChannelsType] * (Ord(fChannelsNum) + 1);
  end;
end;

 
// ------------------
// ------------------ TCUDAUniform ------------------
// ------------------

constructor TCUDAUniform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := nil;
  FSize := 0;
  FType := TCUDAType.CustomType;
  FDefined := false;
end;

destructor TCUDAUniform.Destroy;
begin
  DestroyHandles;
  inherited;
end;

function TCUDAUniform.GetIsAllocated: Boolean;
begin
  Result := Assigned(FHandle);
end;

procedure TCUDAUniform.SetCustomType(const AValue: string);
begin
  if csLoading in ComponentState then
    FCustomType := AValue
  else if not Assigned(FHandle) then
  begin
    FCustomType := AValue;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAUniform.SetDefined(AValue: Boolean);
begin
  if not Assigned(FHandle) then
    FDefined := AValue;
end;

procedure TCUDAUniform.SetKernelName(const AName: string);
begin
  if csLoading in ComponentState then
    FKernelName := AName
  else if not Assigned(FHandle) then
  begin
    FKernelName := AName;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAUniform.SetSize(const AValue: Cardinal);
begin
  if csLoading in ComponentState then
    FSize := AValue
  else if not Assigned(FHandle) then
  begin
    FSize := AValue;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAUniform.SetType(AValue: TCUDAType);
begin
  if csLoading in ComponentState then
    FType := AValue
  else if not Assigned(FHandle) then
  begin
    FType := AValue;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAUniform.SetRef(AValue: Boolean);
begin
  if csLoading in ComponentState then
    FRef := AValue
  else if not Assigned(FHandle) then
  begin
    FRef := AValue;
    CuNotifyChange(cuchSize);
  end;
end;
 

// ------------------
// ------------------ TCUDAConstant ------------------
// ------------------

procedure TCUDAConstant.AllocateHandles;
var
  LName: AnsiString;
  LModule: TCUDAModule;
begin
  if not(FMaster is TCUDAModule) then
  begin
    {$IFDEF USE_LOGGING}
      LogError(strModuleAbsent);
    {$ENDIF}
    Abort;
  end;

  if Length(FKernelName) = 0 then
    exit;

  LModule := TCUDAModule(FMaster);

  LName := AnsiString(FKernelName);
  DestroyHandles;

  Context.Requires;
  FStatus := cuModuleGetGlobal(FHandle, FSize, LModule.FHandle,
    PAnsiChar(LName));
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
    Abort;

  FChanges := [];
  inherited;
end;

procedure TCUDAConstant.DestroyHandles;
begin
  if Assigned(FHandle) then
  begin
    FHandle := nil;
    inherited;
  end;
end;

function TCUDAConstant.GetDeviceAddress: TCUdeviceptr;
begin
  if (FChanges <> []) or (FHandle = nil) then
    AllocateHandles;
  Result := FHandle;
end;

 

// ------------------
// ------------------ TCUDAFuncParam ------------------
// ------------------

procedure TCUDAFuncParam.AllocateHandles;
begin
  if Assigned(Master) and (Master is TCUDAFunction) then
  begin
    FHandle := TCUDAFunction(Master).FHandle;
    if Assigned(FHandle) then
      inherited;
  end;
end;

constructor TCUDAFuncParam.Create(AOwner: TComponent);
begin
  inherited;
  FHandle := nil;
  FRef := false;
end;

procedure TCUDAFuncParam.DestroyHandles;
begin
  if Assigned(FHandle) then
  begin
    FHandle := nil;
    inherited;
  end;
end;

 
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClasses([TGLCUDA, TGLCUDACompiler, TCUDAModule, TCUDAFunction,
    TCUDATexture, TCUDAMemData, TCUDAConstant, TCUDAFuncParam]);

end.
