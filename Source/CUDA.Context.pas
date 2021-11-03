//
// The graphics rendering engine GLScene http://glscene.org
//
unit CUDA.Context;

(* CUDA context *)

interface

uses
  System.Classes,
  System.SysUtils,
  GLS.Strings,
  GLS.BaseClasses,
  GLS.Context,
  GLS.Generics,

  CUDA.Import,
  CUDA.RunTime;

type

  TCUDADimensions = class(TGLUpdateAbleObject)
  private
    FXYZ: TDim3;
    FMaxXYZ: TDim3;
    FReadOnly: Boolean;
    function GetDimComponent(index: Integer): Integer;
    procedure SetDimComponent(index: Integer; Value: Integer);
    function GetMaxDimComponent(index: Integer): Integer;
    procedure SetMaxDimComponent(index: Integer; Value: Integer);
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    property MaxSizeX: Integer index 0 read GetMaxDimComponent
      write SetMaxDimComponent;
    property MaxSizeY: Integer index 1 read GetMaxDimComponent
      write SetMaxDimComponent;
    property MaxSizeZ: Integer index 2 read GetMaxDimComponent
      write SetMaxDimComponent;
    property ReadOnlyValue: Boolean read FReadOnly write FReadOnly;
  published
    { Published Properties }
    property SizeX: Integer index 0 read GetDimComponent write SetDimComponent
      default 1;
    property SizeY: Integer index 1 read GetDimComponent write SetDimComponent
      default 1;
    property SizeZ: Integer index 2 read GetDimComponent write SetDimComponent
      default 1;
  end;

  TCUDAContext = class;
  TOnOpenGLInteropInit = procedure(out Context: TGLContext) of object;

  TCUDADevice = class(TPersistent)
  private
    fID: Integer;
    fHandle: TCUdevice;
    fGFlops: Integer;
    fDeviceProperties: TCudaDeviceProp;
    FSuitable: Boolean;
    FUsed: Boolean;
    fMaxThreadsDim: TCUDADimensions;
    fMaxGridSize: TCUDADimensions;
  protected
    function GetName: string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {  Returns in bytes the total amount of memory
      available on the device dev in bytes. }
    function TotalMemory: Cardinal;
  published
    property Name: string read GetName;
    property TotalGlobalMem: NativeUInt read fDeviceProperties.TotalGlobalMem;
    property SharedMemPerBlock: NativeUInt read fDeviceProperties.SharedMemPerBlock;
    property RegsPerBlock: Integer read fDeviceProperties.RegsPerBlock;
    property WarpSize: Integer read fDeviceProperties.WarpSize;
    property MemPitch: NativeUInt read fDeviceProperties.MemPitch;
    property MaxThreadsPerBlock: Integer
      read fDeviceProperties.MaxThreadsPerBlock;
    property MaxThreadsDim: TCUDADimensions read fMaxThreadsDim;
    property MaxGridSize: TCUDADimensions read fMaxGridSize;
    property ClockRate: Integer read fDeviceProperties.ClockRate;
    property TotalConstMem: NativeUInt read fDeviceProperties.TotalConstMem;
    property Major: Integer read fDeviceProperties.Major;
    property Minor: Integer read fDeviceProperties.Minor;
    property TextureAlignment: NativeUInt read fDeviceProperties.TextureAlignment;
    property DeviceOverlap: Integer read fDeviceProperties.DeviceOverlap;
    property MultiProcessorCount: Integer
      read fDeviceProperties.MultiProcessorCount;
  end;

  TGLCUDADevice = class(TComponent)
  private
    FSelectDeviceName: string;
    function GetDevice: TCUDADevice;
    procedure SetDevice(AValue: TCUDADevice);
    procedure SetDeviceName(const AName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Suitable: Boolean;
  published
    property SelectDevice: string read FSelectDeviceName write SetDeviceName;
    property Device: TCUDADevice read GetDevice write SetDevice;
  end;

  TCUDAHandlesMaster = class(TComponent)
  protected
    function GetContext: TCUDAContext; virtual; abstract;
    procedure AllocateHandles; virtual;
    procedure DestroyHandles; virtual;
  end;

  TCUDAHandleList = GThreadList<TCUDAHandlesMaster>;

  TCUDAContext = class(TObject)
  private
    fHandle: PCUcontext;
    FDevice: TCUDADevice;
    FOnOpenGLInteropInit: TOnOpenGLInteropInit;
    FHandleList: TCUDAHandleList;
    procedure SetDevice(ADevice: TCUDADevice);
  public
    constructor Create;
    destructor Destroy; override;
    {  Destroy all handles based of this context. }
    procedure DestroyAllHandles;
    {  Pushes context onto CPU thread’s stack of current contexts. }
    procedure Requires;
    {  Pops context from current CPU thread. }
    procedure Release;
    function IsValid: Boolean; inline;
    property Device: TCUDADevice read FDevice write SetDevice;
    property OnOpenGLInteropInit: TOnOpenGLInteropInit read FOnOpenGLInteropInit
      write FOnOpenGLInteropInit;
  end;

  TCUDADeviceList = GList<TCUDADevice>;
  TCUDAContextList = GList<TCUDAContext>;

  {  Static class of CUDA contexts manager. }
   CUDAContextManager = class
  private
    class var fDeviceList: TCUDADeviceList;
    class var fContextList: TCUDAContextList;
    class var FContextStacks: array of TCUDAContextList;
  protected
    class function GetDevice(i: Integer): TCUDADevice;
    class function GetNextUnusedDevice: TCUDADevice;
    class procedure RegisterContext(aContext: TCUDAContext);
    class procedure UnRegisterContext(aContext: TCUDAContext);
    class function GetThreadStack: TCUDAContextList;
    class function GetContext(i: Integer): TCUDAContext;
  public
    //  Management
    class procedure Init;
    class procedure Done;
    class procedure CreateContext(aContext: TCUDAContext);
    class procedure DestroyContext(aContext: TCUDAContext);
    class procedure CreateContextOf(ADevice: TCUDADevice);
    class procedure DestroyContextOf(ADevice: TCUDADevice);
    class procedure PushContext(aContext: TCUDAContext);
    class function PopContext: TCUDAContext;
    //  Fill unused device list to show its in property.
    class procedure FillUnusedDeviceList(var AList: TStringList);
    //  Return device by name.
    class function GetDeviceByName(const AName: string): TCUDADevice;
    //  Returns the number of CUDA compatiable devices.
    class function DeviceCount: Integer;
    //  Access to devices list.
    property Devices[i: Integer]: TCUDADevice read GetDevice;
    //  Returns a device that has a maximum Giga flops.
    class function GetMaxGflopsDevice: TCUDADevice;
    //  Returns the number of TCUDAcontext object.
    class function ContextCount: Integer;
    //  Return CUDA context of current thread.
    class function GetCurrentThreadContext: TCUDAContext;
    {  Access to contexts list. }
    property Contexts[i: Integer]: TCUDAContext read GetContext;
  end;

//---------------------------------------------------------------------
implementation
//---------------------------------------------------------------------

threadvar
  vStackIndex: Cardinal;

// ------------------
// ------------------ TCUDADimensions ------------------
// ------------------

constructor TCUDADimensions.Create(AOwner: TPersistent);
const
  cXYZone: TDim3 = (1, 1, 1);
  cXYZmax: TDim3 = (MaxInt, MaxInt, MaxInt);
begin
  inherited Create(AOwner);
  FReadOnly := False;
  FXYZ := cXYZone;
  FMaxXYZ := cXYZmax;
end;

procedure TCUDADimensions.Assign(Source: TPersistent);
begin
  if Source is TCUDADimensions then
  begin
    FMaxXYZ[0] := TCUDADimensions(Source).FMaxXYZ[0];
    FMaxXYZ[1] := TCUDADimensions(Source).FMaxXYZ[1];
    FMaxXYZ[2] := TCUDADimensions(Source).FMaxXYZ[2];
    FXYZ[0] := TCUDADimensions(Source).FXYZ[0];
    FXYZ[1] := TCUDADimensions(Source).FXYZ[1];
    FXYZ[2] := TCUDADimensions(Source).FXYZ[2];
    NotifyChange(Self);
  end;
  inherited Assign(Source);
end;

function TCUDADimensions.GetDimComponent(index: Integer): Integer;
begin
  Result := FXYZ[index];
end;

procedure TCUDADimensions.SetDimComponent(index: Integer; Value: Integer);
var
  v: LongWord;
begin
  if not FReadOnly then
  begin
    if Value < 1 then
      v := 1
    else
      v := LongWord(Value);
    if v > FMaxXYZ[index] then
      v := FMaxXYZ[index];
    FXYZ[index] := v;
    NotifyChange(Self);
  end;
end;

function TCUDADimensions.GetMaxDimComponent(index: Integer): Integer;
begin
  Result := FMaxXYZ[index];
end;

procedure TCUDADimensions.SetMaxDimComponent(index: Integer; Value: Integer);
begin
  if not FReadOnly then
  begin
    if Value > 0 then
    begin
      FMaxXYZ[index] := LongWord(Value);
      if FXYZ[index] > FMaxXYZ[index] then
        FXYZ[index] := FMaxXYZ[index];
      NotifyChange(Self);
    end;
  end;
end;

// ------------------
// ------------------ TCUDADevice ------------------
// ------------------

constructor TCUDADevice.Create;
begin
  fMaxThreadsDim := TCUDADimensions.Create(Self);
  fMaxThreadsDim.ReadOnlyValue := True;
  fMaxGridSize := TCUDADimensions.Create(Self);
  fMaxGridSize.ReadOnlyValue := True;

  if IsCUDAInitialized then
  begin
    fID := CUDAContextManager.fDeviceList.Count;
    FUsed := False;

    FSuitable := cuDeviceGet(fHandle, fID) = CUDA_SUCCESS;
    if FSuitable then
    begin
      cuDeviceGetName(@fDeviceProperties.name[0], SizeOf(fDeviceProperties.name), fHandle);
      cuDeviceTotalMem(@fDeviceProperties.TotalGlobalMem, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.SharedMemPerBlock, CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.RegsPerBlock, CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.WarpSize, CU_DEVICE_ATTRIBUTE_WARP_SIZE, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.MemPitch, CU_DEVICE_ATTRIBUTE_MAX_PITCH, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.MaxThreadsPerBlock, CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.MaxThreadsDim[0], CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.MaxThreadsDim[1], CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.MaxThreadsDim[2], CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.MaxGridSize[0], CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.MaxGridSize[1], CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.MaxGridSize[2], CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.ClockRate, CU_DEVICE_ATTRIBUTE_CLOCK_RATE, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.TotalConstMem, CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY, fHandle);
      cuDeviceComputeCapability(fDeviceProperties.Major, fDeviceProperties.Minor, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.TextureAlignment, CU_DEVICE_ATTRIBUTE_TEXTURE_ALIGNMENT, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.DeviceOverlap, CU_DEVICE_ATTRIBUTE_GPU_OVERLAP, fHandle);
      cuDeviceGetAttribute(@fDeviceProperties.DeviceOverlap, CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT, fHandle);
      fGFlops := fDeviceProperties.MultiProcessorCount *
        fDeviceProperties.ClockRate;
      fMaxThreadsDim.FXYZ[0] := fDeviceProperties.MaxThreadsDim[0];
      fMaxThreadsDim.FXYZ[1] := fDeviceProperties.MaxThreadsDim[1];
      fMaxThreadsDim.FXYZ[2] := fDeviceProperties.MaxThreadsDim[2];
      fMaxGridSize.FXYZ[0] := fDeviceProperties.MaxGridSize[0];
      fMaxGridSize.FXYZ[1] := fDeviceProperties.MaxGridSize[1];
      fMaxGridSize.FXYZ[2] := fDeviceProperties.MaxGridSize[2];
    end;
  end;
end;

destructor TCUDADevice.Destroy;
begin
  fMaxThreadsDim.Destroy;
  fMaxGridSize.Destroy;
  inherited;
end;

procedure TCUDADevice.Assign(Source: TPersistent);
var
  dev: TCUDADevice;
begin
  if Source is TCUDADevice then
  begin
    dev := TCUDADevice(Source);
    fID := dev.fID;
    fHandle := dev.fHandle;
    fGFlops := dev.fGFlops;
    fDeviceProperties := dev.fDeviceProperties;
    FSuitable := dev.FSuitable;
    fMaxThreadsDim.Assign(dev.fMaxThreadsDim);
    fMaxGridSize.Assign(dev.fMaxGridSize);
  end;
  inherited Assign(Source);
end;

function TCUDADevice.GetName: string;
begin
  Result := Format('%s (%d)', [string(fDeviceProperties.name), fID + 1]);
end;

function TCUDADevice.TotalMemory: Cardinal;
begin
  cuDeviceTotalMem(@fDeviceProperties.TotalGlobalMem, fHandle);
  Result := fDeviceProperties.TotalGlobalMem;
end;

// ------------------
// ------------------ TGLCUDADevice ------------------
// ------------------

constructor TGLCUDADevice.Create(AOwner: TComponent);
var
  LDevice: TCUDADevice;
begin
  inherited Create(AOwner);
  LDevice := CUDAContextManager.GetNextUnusedDevice;
  if Assigned(LDevice) and LDevice.FSuitable then
  begin
    FSelectDeviceName := LDevice.name;
    LDevice.FUsed := True;
  end
  else
  begin
    FSelectDeviceName := '';
  end;
end;

destructor TGLCUDADevice.Destroy;
var
  Device: TCUDADevice;
begin
  inherited;
  Device := CUDAContextManager.GetDeviceByName(FSelectDeviceName);
  if Assigned(Device) then
    Device.FUsed := False;
end;

function TGLCUDADevice.GetDevice: TCUDADevice;
begin
  Result := CUDAContextManager.GetDeviceByName(FSelectDeviceName);
end;

function TGLCUDADevice.Suitable: Boolean;
var
  LDevice: TCUDADevice;
begin
  LDevice := GetDevice;
  Result := Assigned(LDevice);
  if Result then
    Result := LDevice.FSuitable;
end;

procedure TGLCUDADevice.SetDevice(AValue: TCUDADevice);
begin
end;

procedure TGLCUDADevice.SetDeviceName(const AName: string);
begin
  if FSelectDeviceName <> AName then
  begin
    CUDAContextManager.DestroyContextOf(Self.Device);
    FSelectDeviceName := AName;
    CUDAContextManager.CreateContextOf(Self.Device);
  end;
end;

// ------------------
// ------------------ TCUDAContextManager ------------------
// ------------------

class procedure CUDAContextManager.Init;
var
  dCount: Integer;
  status: TCUresult;
  i: Integer;
begin
  if InitCUDA and not Assigned(fDeviceList) then
  begin
    fDeviceList := TCUDADeviceList.Create;
    fContextList := TCUDAContextList.Create;
    dCount := 0;
    status := cuInit(0);
    if status = CUDA_SUCCESS then
      cuDeviceGetCount(dCount);

    // Fill devices list
    for i := 0 to dCount - 1 do
      fDeviceList.Add(TCUDADevice.Create);
  end;
end;

class procedure CUDAContextManager.Done;
var
  I, J: Integer;
begin
  if Assigned(fDeviceList) then
    for i := 0 to fDeviceList.Count - 1 do
      fDeviceList[i].Free;

  for I := 0 to High(FContextStacks) do
  begin
    if FContextStacks[I].Count > 0 then
    begin
      //Unbalansed Usage
      for J := FContextStacks[I].Count - 1 to 0 do
        FContextStacks[I][J].Release;
    end;
    FContextStacks[I].Destroy;
  end;

  fDeviceList.Free;
  fContextList.Free;
  CloseCUDA;
end;

class procedure CUDAContextManager.RegisterContext(aContext: TCUDAContext);
begin
  if fContextList.IndexOf(aContext) >= 0 then
  begin
    // Invalid Context Reg
    Abort;
  end
  else
    fContextList.Add(aContext);
end;

class procedure CUDAContextManager.UnRegisterContext(aContext: TCUDAContext);
begin
  if fContextList.IndexOf(aContext) < 0 then
  begin
    // Invalid Context Reg
    Abort;
  end
  else
  begin
    fContextList.Remove(aContext);
  end;
end;

class function CUDAContextManager.ContextCount: Integer;
begin
  Result := fContextList.Count;
end;

class function CUDAContextManager.DeviceCount: Integer;
begin
  Result := fDeviceList.Count;
end;

class function CUDAContextManager.GetDevice(i: Integer): TCUDADevice;
begin
  Result := nil;
  if i < fDeviceList.Count then
    Result := fDeviceList[i];
end;

class function CUDAContextManager.GetContext(i: Integer): TCUDAContext;
begin
  Result := nil;
  if i < fContextList.Count then
    Result := fContextList[i];
end;

class procedure CUDAContextManager.FillUnusedDeviceList(var AList: TStringList);
var
  i: Integer;
begin
  if not Assigned(AList) then
    AList := TStringList.Create
  else
    AList.Clear;
  for i := 0 to fDeviceList.Count - 1 do
    if not fDeviceList[i].FUsed then
      AList.Add(fDeviceList[i].name);
end;

class function CUDAContextManager.GetDeviceByName(const AName: string)
  : TCUDADevice;
var
  i: Integer;
  Device: TCUDADevice;
begin
  Result := nil;
  if Length(AName) = 0 then
    exit;

  for i := 0 to fDeviceList.Count - 1 do
  begin
    Device := fDeviceList[i];
    if Device.name = AName then
    begin
      Result := Device;
      exit;
    end;
  end;
end;

class function CUDAContextManager.GetMaxGflopsDevice: TCUDADevice;
var
  max_gflops: Integer;
  i: Integer;
  Device: TCUDADevice;
begin
  Device := nil;
  max_gflops := 0;
  for i := 0 to fDeviceList.Count - 1 do
  begin
    if max_gflops < fDeviceList.Items[i].fGFlops then
    begin
      Device := fDeviceList.Items[i];
      max_gflops := Device.fGFlops;
    end;
  end;
  Result := Device;
end;

class function CUDAContextManager.GetNextUnusedDevice: TCUDADevice;
var
  i: Integer;
  Device: TCUDADevice;
begin
  Result := nil;
  for i := 0 to fDeviceList.Count - 1 do
  begin
    Device := fDeviceList[i];
    if not Device.FUsed then
    begin
      Result := Device;
      exit;
    end;
  end;
end;

class procedure CUDAContextManager.CreateContext(aContext: TCUDAContext);
var
  status: TCUresult;
  cuOldContext, cuContext: PCUcontext;
  LGLContext: TGLContext;
  LStack: TCUDAContextList;
begin
  if not Assigned(aContext.FDevice)
    or not aContext.FDevice.FSuitable then
  begin
    // No Device To Create
    Abort;
  end;

  if GetThreadStack.Count > 0 then
  begin
    if cuCtxPopCurrent(cuOldContext) <> CUDA_SUCCESS then
    begin
      // Thread Busy
      Abort;
    end;
  end
  else
    cuOldContext := nil;

  if aContext.IsValid then
    DestroyContext(aContext);

  RegisterContext(aContext);

  status := CUDA_SUCCESS;
  if Assigned(aContext.FOnOpenGLInteropInit) then
  begin
    aContext.FOnOpenGLInteropInit(LGLContext);
    if Assigned(LGLContext) and LGLContext.IsValid then
    begin
      LGLContext.Activate;
      cuContext := nil;
      status := cuGLCtxCreate(cuContext, 0, aContext.FDevice.fHandle);
      LGLContext.Deactivate;
    end
    else
    begin
      // Invalid GL Context
      UnRegisterContext(aContext);
      Abort;
    end;
  end
  else
  begin
    status := cuCtxCreate(cuContext, 0, aContext.FDevice.fHandle);
  end;

  if (status <> CUDA_SUCCESS) then
  begin
    cudaGetLastErrorString;
    UnRegisterContext(aContext);
    cuCtxDetach(cuContext);
    Abort;
  end;

  aContext.fHandle := cuContext;

  // Make context be floating to use it in different thread
  if cuCtxPopCurrent(cuContext) <> CUDA_SUCCESS then
  begin
    // Make Floating Failed
    LStack := GetThreadStack;
    LStack.Insert(LStack.Count - 1, aContext);
  end;

  if Assigned(cuOldContext) then
    cuCtxPushCurrent(cuOldContext);
end;

class procedure CUDAContextManager.CreateContextOf(ADevice: TCUDADevice);
var
  i: Integer;
begin
  if Assigned(ADevice) and ADevice.FSuitable then
  begin
    for i := 0 to fContextList.Count do
      if fContextList[i].FDevice = ADevice then
        CreateContext(fContextList[i]);
  end;
end;

class procedure CUDAContextManager.DestroyContext(aContext: TCUDAContext);
begin
  if aContext.IsValid then
  begin
    aContext.DestroyAllHandles;
    cuCtxDestroy(aContext.fHandle);
    aContext.fHandle := nil;
    CUDAContextManager.UnRegisterContext(aContext);
  end;
end;

class procedure CUDAContextManager.DestroyContextOf(ADevice: TCUDADevice);
var
  i: Integer;
begin
  if Assigned(ADevice) and ADevice.FSuitable then
  begin
    for i := 0 to fContextList.Count - 1 do
      if fContextList[i].FDevice = ADevice then
        DestroyContext(fContextList[i]);
  end;
end;

class function CUDAContextManager.GetThreadStack: TCUDAContextList;
begin
  if vStackIndex = 0 then
  begin
    SetLength(FContextStacks, Length(FContextStacks)+1);
    FContextStacks[High(FContextStacks)] := TCUDAContextList.Create;
    vStackIndex := High(FContextStacks)+1;
  end;
  Result := FContextStacks[vStackIndex-1];
end;

class function CUDAContextManager.GetCurrentThreadContext: TCUDAContext;
begin
  if GetThreadStack.Count > 0 then
    Result := GetThreadStack.Last
  else
    Result := nil;
end;

class procedure CUDAContextManager.PushContext(aContext: TCUDAContext);
var
  LContext: TCUDAContext;
  cuContext: PCUcontext;
begin
  LContext := GetCurrentThreadContext;
  if LContext <> aContext then
  begin
    // Pop current
    if Assigned(LContext) then
      if cuCtxPopCurrent(cuContext) = CUDA_SUCCESS then
      begin
        if LContext.fHandle <> cuContext then
        begin
          // Unbalansed Usage
          Abort;
        end;
      end
      else
        Abort;
    // Push required
    if cuCtxPushCurrent(aContext.fHandle) <> CUDA_SUCCESS then
      Abort;
  end;
  GetThreadStack.Add(aContext);
end;

class function CUDAContextManager.PopContext: TCUDAContext;
var
  C: Integer;
  LContext: TCUDAContext;
  cuContext: PCUcontext;
begin
  C := GetThreadStack.Count;
  if C = 0 then
  begin
    // UnbalansedUsage
    Abort;
  end;

  Result := GetThreadStack.Last;
  GetThreadStack.Delete(C - 1);

  LContext := GetCurrentThreadContext;
  if Result <> LContext then
  begin
    if cuCtxPopCurrent(cuContext) = CUDA_SUCCESS then
    begin
      if Result.fHandle <> cuContext then
      begin
        // UnbalansedUsage
        Abort;
      end;
    end
    else
      Abort;

    if Assigned(LContext)
      and (cuCtxPushCurrent(LContext.fHandle) <> CUDA_SUCCESS) then
        Abort;
  end;
end;

// ------------------
// ------------------ TCUDAHandlesMaster ------------------
// ------------------

procedure TCUDAHandlesMaster.AllocateHandles;
var
  LList: TCUDAHandleList.TLockableList;
begin
  LList := GetContext.FHandleList.LockList;
  if LList.IndexOf(Self) < 0 then
    LList.Add(Self);
  GetContext.FHandleList.UnlockList;
end;

procedure TCUDAHandlesMaster.DestroyHandles;
begin
  GetContext.FHandleList.Remove(Self);
end;

// ------------------
// ------------------ TCUDAContext ------------------
// ------------------

constructor TCUDAContext.Create;
begin
  inherited Create;
  fHandle := nil;
  FDevice := nil;
  FHandleList := TCUDAHandleList.Create;
end;

destructor TCUDAContext.Destroy;
begin
  DestroyAllHandles;
  CUDAContextManager.DestroyContext(Self);
  FHandleList.Destroy;
  inherited;
end;

procedure TCUDAContext.SetDevice(ADevice: TCUDADevice);
begin
  if FDevice <> ADevice then
  begin
    CUDAContextManager.DestroyContext(Self);
    FDevice := ADevice;
  end;
end;

procedure TCUDAContext.Requires;
begin
  if not IsValid then
  begin
    // Context Not Initialized
    Abort;
  end;
  CUDAContextManager.PushContext(Self);
end;

procedure TCUDAContext.Release;
begin
  CUDAContextManager.PopContext;
end;

procedure TCUDAContext.DestroyAllHandles;
var
  i: Integer;
  LList: TCUDAHandleList.TLockableList;
begin
  Requires;
  LList := FHandleList.LockList;
  try
    for i := LList.Count - 1 downto 0 do
      LList[i].DestroyHandles;
  finally
    FHandleList.Clear;
    FHandleList.UnlockList;
    Release;
  end;
end;

function TCUDAContext.IsValid: Boolean;
begin
  Result := Assigned(fHandle);
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClasses([TGLCUDADevice]);
  CUDAContextManager.Init;

finalization

  CUDAContextManager.Done;

end.
