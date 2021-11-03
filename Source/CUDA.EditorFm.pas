//
// The graphics rendering engine GLScene http://glscene.org
//
unit CUDA.EditorFm;

(* Editor of TGLCUDA *)

interface

{$I GLScene.inc}

uses
  Winapi.Windows, 
  Winapi.Messages,
  System.SysUtils, 
  System.Variants,
  System.Classes,
  System.Win.Registry,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ImgList,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  DesignIntf,
  VCLEditors,
  GLS.Strings,

  CUDA.APIComps,
  CUDA.FFTPlan,
  CUDA.Graphics;

type
  TGLCUDAEditorForm = class(TForm)
    ToolBar1: TToolBar;
    AddModuleButton: TToolButton;
    DeleteButton: TToolButton;
    ListBox1: TListBox;
    ImageList1: TImageList;
    AddMemDataButton: TToolButton;
    AddFFTPlanButton: TToolButton;
    AddGeometryResButton: TToolButton;
    AddImageResButton: TToolButton;
    procedure AddItemButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FClassList: TList;
    FCUDA: TGLCUDA;
    FCurrentDesigner: IDesigner;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure OnCUDAComponentNameChanged(Sender : TObject);
  public
    procedure SetCUDAEditorClient(Client: TGLCUDA; Designer: IDesigner);
  end;

function GLCUDAEditorForm: TGLCUDAEditorForm;
procedure ReleaseGLCUDAEditorForm;

//--------------------------------------------------------
implementation
//--------------------------------------------------------

{$R *.dfm}

const
  cRegistryKey = 'Software\GLScene\CUDAEditor';

var
  vGLCUDAEditorForm: TGLCUDAEditorForm;

function GLCUDAEditorForm: TGLCUDAEditorForm;
begin
  if not Assigned(vGLCUDAEditorForm) then
    vGLCUDAEditorForm := TGLCUDAEditorForm.Create(nil);
  Result := vGLCUDAEditorForm;
end;

procedure ReleaseGLCUDAEditorForm;
begin
  if Assigned(vGLCUDAEditorForm) then
  begin
    vGLCUDAEditorForm.Free;
    vGLCUDAEditorForm := nil;
  end;
end;

function ReadRegistryInteger(reg: TRegistry; const name: string;
  defaultValue: Integer): Integer;
begin
  if reg.ValueExists(name) then
    Result := reg.ReadInteger(name)
  else
    Result := defaultValue;
end;

procedure TGLCUDAEditorForm.AddItemButtonClick(Sender: TObject);
var
  LClass: TCUDAComponentClass;
  obj: TCUDAComponent;
begin
  if Assigned(FCurrentDesigner) then
  begin
    LClass := TCUDAComponentClass(FClassList[TToolButton(Sender).Tag]);
    obj := TCUDAComponent(FCurrentDesigner.CreateComponent(LClass, FCUDA, 0, 0, 0, 0));
    obj.Master := FCUDA;
    ListBox1.AddItem(obj.Name, obj);
    FCurrentDesigner.Modified;
  end;
end;

procedure TGLCUDAEditorForm.DeleteButtonClick(Sender: TObject);
var
  obj: TCUDAComponent;
  i: Integer;
begin
  if ListBox1.SelCount = 0 then
    exit;
  for i := 0 to ListBox1.Count - 1 do
  begin
    if ListBox1.Selected[i] then
    begin
      obj := TCUDAComponent(ListBox1.Items.Objects[i]);
      obj.Destroy;
    end;
  end;
  ListBox1.DeleteSelected;
  FCurrentDesigner.Modified;
end;

procedure TGLCUDAEditorForm.FormCreate(Sender: TObject);
var
  reg: TRegistry;
begin
  RegisterCUDAComponentNameChangeEvent(OnCUDAComponentNameChanged);
  reg := TRegistry.Create;
  try
    if reg.OpenKey(cRegistryKey, True) then
    begin
      Left := ReadRegistryInteger(reg, 'Left', Left);
      Top := ReadRegistryInteger(reg, 'Top', Top);
      Width := ReadRegistryInteger(reg, 'Width', 250);
      Height := ReadRegistryInteger(reg, 'Height', Height);
    end;
  finally
    reg.Free;
  end;
  FClassList := TList.Create;
  AddModuleButton.Tag := FClassList.Add(TCUDAModule);
  AddMemDataButton.Tag := FClassList.Add(TCUDAMemData);
  AddFFTPlanButton.Tag := FClassList.Add(TCUDAFFTPlan);
  AddGeometryResButton.Tag := FClassList.Add(TCUDAGeometryResource);
  AddImageResButton.Tag := FClassList.Add(TCUDAImageResource);
end;

procedure TGLCUDAEditorForm.FormDestroy(Sender: TObject);
var
  reg: TRegistry;
begin
  DeRegisterCUDAComponentNameChangeEvent;
  FClassList.Destroy;
  reg := TRegistry.Create;
  try
    if reg.OpenKey(cRegistryKey, True) then
    begin
      reg.WriteInteger('Left', Left);
      reg.WriteInteger('Top', Top);
      reg.WriteInteger('Width', Width);
      reg.WriteInteger('Height', Height);
    end;
  finally
    reg.Free;
  end;
end;

procedure TGLCUDAEditorForm.ListBox1Click(Sender: TObject);
var
  obj: TCUDAComponent;
  i: Integer;
begin
  if not Assigned(FCurrentDesigner) then
    exit;
  obj := nil;
  if ListBox1.SelCount = 1 then
    for i := 0 to ListBox1.Count - 1 do
    begin
      if ListBox1.Selected[i] then
      begin
        obj := TCUDAComponent(ListBox1.Items.Objects[i]);
        break;
      end;
    end;
  if Assigned(obj) then
    FCurrentDesigner.SelectComponent(obj);
end;

procedure TGLCUDAEditorForm.SetCUDAEditorClient(Client: TGLCUDA; Designer: IDesigner);
var
  i: Integer;
  child: TCUDAComponent;
begin
  if Assigned(FCUDA) then
    FCUDA.RemoveFreeNotification(Self);
  FCUDA := Client;
  FCurrentDesigner := Designer;
  ListBox1.Clear;
  if Assigned(FCUDA) then
  begin
    FCUDA.FreeNotification(Self);
    Caption := strCUDAEditor + ' : ' + FCUDA.Name;
    for i := 0 to FCUDA.ItemsCount - 1 do
    begin
      child := FCUDA.Items[i];
      ListBox1.AddItem(child.Name, child);
    end;
  end
  else
    Caption := strCUDAEditor;
end;

procedure TGLCUDAEditorForm.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (FCUDA = AComponent) and (Operation = opRemove) then
  begin
    FCUDA := nil;
    SetCUDAEditorClient(nil, nil);
  end;
  inherited;
end;

procedure TGLCUDAEditorForm.OnCUDAComponentNameChanged(Sender: TObject);
var
  i: Integer;
  obj: TCUDAComponent;
begin
  for i := 0 to ListBox1.Count - 1 do
  begin
    obj := TCUDAComponent(ListBox1.Items.Objects[i]);
    if Sender = obj then
    begin
      ListBox1.Items[I]:= obj.Name;
      break;
    end;
  end;
end;

//-----------------------------------------------
initialization
//-----------------------------------------------

finalization

  ReleaseGLCUDAEditorForm;

end.

