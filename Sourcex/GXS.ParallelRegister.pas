//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.ParallelRegister;

(*  Registration unit for GPU Computing package *)

interface

uses
  System.Classes,
  System.SysUtils,

  DesignIntf,
  DesignEditors,

  {
  ToolsAPI,
  STREDIT,
  }

  GXS.Strings;

procedure Register;

type

  TCUDAEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TCUDACompilerEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TCUDACompilerSourceProperty = class(TStringProperty)
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

  TCUDADeviceProperty = class(TStringProperty)
  private
    FDeviceList: TStringList;
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: String); override;
  end;

//======================================
implementation
//======================================

uses
  FxCUDAEditor,
  CUDAx.RunTime,
  CUDAx.Parser,
  CUDAx.Context,
  CUDAx.API,
  CUDAx.Compiler,
  CUDAx.FFTPlan,
  CUDAx.Graphics;

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
// ------------------ TCUDAEditor ------------------
// ------------------

procedure TCUDAEditor.Edit;
begin
  with CUDAEditorForm do
  begin
    SetCUDAEditorClient(TCUDA(Self.Component), Self.Designer);
    Show;
  end;
end;

procedure TCUDAEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TCUDAEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show CUDA Items Editor';
  end;
end;

function TCUDAEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

// ------------------
// ------------------ TCUDACompilerEditor ------------------
// ------------------

procedure TCUDACompilerEditor.Edit;
var
  CUDACompiler: TCUDACompiler;
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
  CUDACompiler := TCUDACompiler(Self.Component);
  if CUDACompiler.Compile then
  begin
    info := CUDACompiler.ModuleInfo;
    parent := TCUDAModule(info.Owner);

    // Create kernel's functions
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
        // destroy old parameters
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

    // Create kernel's textures
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
    // Create kernel's constants
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

procedure TCUDACompilerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TCUDACompilerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Compile Module';
  end;
end;

function TCUDACompilerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

// ------------------
// ------------------ TCUDACompilerSourceProperty ------------------
// ------------------

constructor TCUDACompilerSourceProperty.Create(
    const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited;
  FModuleList := TStringList.Create;
end;

destructor TCUDACompilerSourceProperty.Destroy;
begin
  FModuleList.Destroy;
  inherited;
end;

procedure TCUDACompilerSourceProperty.RefreshModuleList;
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

function TCUDACompilerSourceProperty.GetAttributes;
begin
  Result := [paValueList];
end;

procedure TCUDACompilerSourceProperty.GetValues(Proc: TGetStrProc);
var
   I : Integer;
begin
  RefreshModuleList;
  for I := 0 to FModuleList.Count - 1 do
      Proc(ExtractFileName(FModuleList[I]));
end;

procedure TCUDACompilerSourceProperty.SetValue(const Value: String);
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
    TCUDACompiler(GetComponent(0)).SetSourceCodeFile(FModuleList[J]);
    SetStrValue(ExtractFileName(Value));
  end
  else
  begin
    SetStrValue('none');
  end;
	Modified;
end;

// ------------------
// ------------------ TCUDADeviceProperty ------------------
// ------------------
constructor TCUDADeviceProperty.Create(const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited;
  FDeviceList := TStringList.Create;
end;

destructor TCUDADeviceProperty.Destroy;
begin
  FDeviceList.Destroy;
  inherited;
end;

function TCUDADeviceProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TCUDADeviceProperty.GetValues(Proc: TGetStrProc);
begin
  CUDAContextManager.FillUnusedDeviceList(FDeviceList);
end;

procedure TCUDADeviceProperty.SetValue(const Value: String);
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

procedure Register;
begin
  RegisterComponents('GPU Computing', [TCUDA, TCUDADevice, TCUDACompiler]);
  RegisterComponentEditor(TCUDA, TCUDAEditor);
  RegisterComponentEditor(TCUDACompiler, TCUDACompilerEditor);
  RegisterPropertyEditor(TypeInfo(string), TCUDACompiler, 'ProjectModule',
    TCUDACompilerSourceProperty);
  RegisterPropertyEditor(TypeInfo(string), TCUDADevice, 'SelectDevice',
    TCUDADeviceProperty);
  RegisterNoIcon([TCUDAModule, TCUDAMemData, TCUDAFunction, TCUDATexture,
    TCUDAFFTPlan, TCUDAImageResource, TCUDAGeometryResource, TCUDAConstant, TCUDAFuncParam]);

  ObjectManager.RegisterSceneObject(TgxFeedBackMesh, 'GPU generated mesh', 'GPU Computing', HInstance);
end;

//-------------------------------------
initialization
//-------------------------------------

  vFindCuFileFunc := FindCuFile;

end.

