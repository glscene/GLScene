//
// The graphics platform GLScene https://github.com/glscene
//
unit CUDA.PropEditors;

interface

uses
  System.Classes,
  System.SysUtils,

  ToolsAPI,
  StrEdit,
  DesignEditors,
  DesignIntf,

  CUDA.APIComps,
  CUDA.Context,
  CUDA.Compiler,
  CUDA.Parser,
  CUDA.EditorFm;

type
  TGLCUDAEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TGLCUDACompilerEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TGLCUDACompilerSourceProperty = class(TStringProperty)
  private
    FModuleList: TStringList;
    procedure RefreshModuleList;
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: String); override;
  end;

  TGLCUDADeviceProperty = class(TStringProperty)
  private
    FDeviceList: TStringList;
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: String); override;
  end;


function FindCuFile(var AModuleName: string): Boolean;

//-----------------------------------------------
implementation
//-----------------------------------------------

uses
  CUDA.RunTime;

function FindCuFile(var AModuleName: string): Boolean;
var
  proj: IOTAProject;
  I: Integer;
  LModule: IOTAModuleInfo;
  LName: string;
begin
  proj := GetActiveProject;
  if proj <> nil then
  begin
    for I := 0 to proj.GetModuleCount - 1 do
    begin
      LModule := proj.GetModule(I);
      LName := ExtractFileName(LModule.FileName);
      if LName = AModuleName then
      begin
        AModuleName := LModule.FileName;
        exit(True);
      end;
    end;
  end;
  Result := False;
end;

// ------------------
// ------------------ TGLCUDAEditor ------------------
// ------------------

procedure TGLCUDAEditor.Edit;
begin
  with GLCUDAEditorForm do
  begin
    SetCUDAEditorClient(TGLCUDA(Self.Component), Self.Designer);
    Show;
  end;
end;

procedure TGLCUDAEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TGLCUDAEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show CUDA Items Editor';
  end;
end;

function TGLCUDAEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


// ------------------
// ------------------ TGLCUDACompilerEditor ------------------
// ------------------

procedure TGLCUDACompilerEditor.Edit;
var
  CUDACompiler: TGLCUDACompiler;
  I, J: Integer;
  func: TCUDAFunction;
  tex: TCUDATexture;
  cnst: TCUDAConstant;
  param: TCUDAFuncParam;
  parent: TCUDAModule;
  info: TCUDAModuleInfo;
  bUseless: Boolean;
  useless: array of TCUDAComponent;
  CTN: TChannelTypeAndNum;

  procedure CreateFuncParams;
  var
    K: Integer;
  begin
    for K := 0 to High(info.Func[I].Args) do
    begin
      param := TCUDAFuncParam(Designer.CreateComponent(TCUDAFuncParam,
        func, 0, 0, 0, 0));
      param.Master := TCUDAComponent(func);
      param.KernelName := info.Func[I].Args[K].Name;
      param.Name := func.KernelName+'_'+param.KernelName;
      param.DataType := info.Func[I].Args[K].DataType;
      param.CustomType := info.Func[I].Args[K].CustomType;
      param.Reference := info.Func[I].Args[K].Ref;
    end;
  end;

begin
  CUDACompiler := TGLCUDACompiler(Self.Component);
  if CUDACompiler.Compile then
  begin
    info := CUDACompiler.ModuleInfo;
    parent := TCUDAModule(info.Owner);

    // Creates kernel's functions
    for I := 0 to High(info.Func) do
    begin
      func := parent.KernelFunction[info.Func[I].KernelName];
      if not Assigned(func) then
      begin
        func := TCUDAFunction(Designer.CreateComponent(TCUDAFunction,
          info.Owner, 0, 0, 0, 0));
        func.Master := TCUDAComponent(info.Owner);
        func.KernelName := info.Func[I].KernelName;
        func.Name := TCUDAComponent(info.Owner).MakeUniqueName(info.Func[I].Name);
      end
      else
      begin
        // for old parameters
        while func.ItemsCount > 0 do
          func.Items[0].Destroy;
      end;

      try
        bUseless := func.Handle = nil;
      except
        bUseless := True;
      end;
      if bUseless then
      begin
        Designer.SelectComponent(func);
        Designer.DeleteSelection(True);
        func := nil;
      end
      else
        CreateFuncParams;
    end;

    // Creates kernel's textures
    for I := 0 to High(info.TexRef) do
    begin
      tex := parent.KernelTexture[info.TexRef[I].Name];
      if not Assigned(tex) then
      begin
        tex := TCUDATexture(Designer.CreateComponent(TCUDATexture,
          info.Owner, 0, 0, 0, 0));
        tex.Master := TCUDAComponent(info.Owner);
        tex.KernelName := info.TexRef[I].Name;
        tex.Name := tex.KernelName;
        tex.ReadAsInteger := (info.TexRef[I].ReadMode = cudaReadModeElementType);
        CTN := GetChannelTypeAndNum(info.TexRef[I].DataType);
        tex.Format := CTN.F;
      end;

      tex.ChannelNum := CTN.C;
      try
        bUseless := tex.Handle = nil;
      except
        bUseless := True;
      end;
      if bUseless then
      begin
        Designer.SelectComponent(tex);
        Designer.DeleteSelection(True);
      end;
    end;
    // Creates kernel's constants
    for I := 0 to High(info.Constant) do
    begin
      cnst := parent.KernelConstant[info.Constant[I].Name];
      if not Assigned(cnst) then
      begin
        cnst := TCUDAConstant(Designer.CreateComponent(TCUDAConstant,
          info.Owner, 0, 0, 0, 0));
        cnst.Master := TCUDAComponent(info.Owner);
        cnst.KernelName := info.Constant[I].Name;
        cnst.Name := cnst.KernelName;
        cnst.DataType := info.Constant[I].DataType;
        cnst.CustomType := info.Constant[I].CustomType;
        cnst.IsValueDefined := info.Constant[I].DefValue;
      end;

      try
        bUseless := cnst.DeviceAddress = nil;
      except
        bUseless := True;
      end;
      if bUseless then
      begin
        Designer.SelectComponent(cnst);
        Designer.DeleteSelection(True);
      end;
    end;

    // Delete useless components
    SetLength(useless, parent.ItemsCount);
    j := 0;
    for i := 0 to parent.ItemsCount - 1 do
    begin
      if not TCUDAComponent(parent.Items[i]).IsAllocated then
        begin
          useless[j] := parent.Items[i];
          inc(j);
        end;
    end;

    for i := 0 to j - 1 do
      useless[i].Destroy;
  end;
  Designer.Modified;
end;

procedure TGLCUDACompilerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TGLCUDACompilerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Compile Module';
  end;
end;

function TGLCUDACompilerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

// ------------------
// ------------------ TGLCUDACompilerSourceProperty ------------------
// ------------------

constructor TGLCUDACompilerSourceProperty.Create(
    const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited;
  FModuleList := TStringList.Create;
end;


destructor TGLCUDACompilerSourceProperty.Destroy;
begin
  FModuleList.Destroy;
  inherited;
end;

procedure TGLCUDACompilerSourceProperty.RefreshModuleList;
var
  proj: IOTAProject;
  I: Integer;
  LModule: IOTAModuleInfo;
  LName: string;
begin
  FModuleList.Clear;
  FModuleList.Add('none');
  proj := GetActiveProject;
  if proj <> nil then
  begin
    for I := 0 to proj.GetModuleCount - 1 do
    begin
      LModule := proj.GetModule(I);
      LName := UpperCase(ExtractFileExt(LModule.FileName));
      if LName = '.CU' then
        FModuleList.Add(LModule.FileName);
    end;
  end;
end;


function TGLCUDACompilerSourceProperty.GetAttributes;
begin
  Result := [paValueList];
end;

procedure TGLCUDACompilerSourceProperty.GetValues(Proc: TGetStrProc);
var
   I : Integer;
begin
  RefreshModuleList;
  for I := 0 to FModuleList.Count - 1 do
      Proc(ExtractFileName(FModuleList[I]));
end;

procedure TGLCUDACompilerSourceProperty.SetValue(const Value: String);
var
  I, J: Integer;
begin
  RefreshModuleList;
  J := -1;
  for I := 1 to FModuleList.Count - 1 do
    if Value = ExtractFileName(FModuleList[I]) then
    begin
      J := I;
      Break;
    end;

  if J > 0 then
  begin
    TGLCUDACompiler(GetComponent(0)).SetSourceCodeFile(FModuleList[J]);
    SetStrValue(ExtractFileName(Value));
  end
  else
  begin
    SetStrValue('none');
  end;
	Modified;
end;

// ------------------
// ------------------ TGLCUDADeviceProperty ------------------
// ------------------

constructor TGLCUDADeviceProperty.Create(const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited;
  FDeviceList := TStringList.Create;
end;

destructor TGLCUDADeviceProperty.Destroy;
begin
  FDeviceList.Destroy;
  inherited;
end;

function TGLCUDADeviceProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TGLCUDADeviceProperty.GetValues(Proc: TGetStrProc);
begin
  CUDAContextManager.FillUnusedDeviceList(FDeviceList);
end;

procedure TGLCUDADeviceProperty.SetValue(const Value: String);
var
  I: Integer;
begin
  for I := 0 to FDeviceList.Count - 1 do
    if Value = FDeviceList[I] then
    begin
      SetStrValue(Value);
      Break;
    end;
  Modified;
end;

end.
