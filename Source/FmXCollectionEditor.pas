//
// The graphics rendering engine GLScene http://glscene.org
//

unit FmXCollectionEditor;

(* Edits a TXCollection *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Actions,
  System.ImageList,
  VCL.Forms,
  VCL.ImgList,
  VCL.Controls,
  VCL.ActnList,
  VCL.Menus,
  VCL.ComCtrls,
  VCL.ToolWin,
  VCL.Dialogs,

  DesignIntf,

  GLS.Strings,
  GLS.Scene,
  GLS.Behaviours,
  GLS.MaterialEx,
  GLS.XCollection;

type
  TXCollectionEditorForm = class(TForm)
    ListView: TListView;
    PMListView: TPopupMenu;
    ActionList: TActionList;
    ACRemove: TAction;
    ACMoveUp: TAction;
    ACMoveDown: TAction;
    ImageList: TImageList;
    MIAdd: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Moveup1: TMenuItem;
    Movedown1: TMenuItem;
    ToolBar1: TToolBar;
    TBAdd: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    PMToolBar: TPopupMenu;
    procedure TBAddClick(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ACRemoveExecute(Sender: TObject);
    procedure ACMoveUpExecute(Sender: TObject);
    procedure ACMoveDownExecute(Sender: TObject);
    procedure PMToolBarPopup(Sender: TObject);
    procedure PMListViewPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    FXCollection: TXCollection;
    // ownerComponent : TComponent;
    FDesigner: IDesigner;
    UpdatingListView: Boolean;
    procedure PrepareListView;
    procedure PrepareXCollectionItemPopup(parent: TMenuItem);
    procedure OnAddXCollectionItemClick(Sender: TObject);
    procedure OnNameChanged(Sender: TObject);
    procedure OnXCollectionDestroyed(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    procedure SetXCollection(aXCollection: TXCollection; designer: IDesigner );
  end;

function XCollectionEditorForm: TXCollectionEditorForm;
procedure ReleaseXCollectionEditor;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

{$R *.dfm}

var
  vXCollectionEditorForm: TXCollectionEditorForm;

function XCollectionEditorForm: TXCollectionEditorForm;
begin
  if not Assigned(vXCollectionEditorForm) then
    vXCollectionEditorForm := TXCollectionEditorForm.Create(nil);
  Result := vXCollectionEditorForm;
end;

procedure ReleaseXCollectionEditor;
begin
  if Assigned(vXCollectionEditorForm) then
  begin
    vXCollectionEditorForm.Release;
    vXCollectionEditorForm := nil;
  end;
end;

//--------------------------------------------
// TXCollectionEditorForm
//--------------------------------------------

procedure TXCollectionEditorForm.FormCreate(Sender: TObject);
begin
  RegisterGLBehaviourNameChangeEvent(OnNameChanged);
  RegisterGLMaterialExNameChangeEvent(OnNameChanged);
  RegisterXCollectionDestroyEvent(OnXCollectionDestroyed);
end;

procedure TXCollectionEditorForm.FormDestroy(Sender: TObject);
begin
  DeRegisterGLBehaviourNameChangeEvent(OnNameChanged);
  DeRegisterGLMaterialExNameChangeEvent(OnNameChanged);
  DeRegisterXCollectionDestroyEvent(OnXCollectionDestroyed);
end;

procedure TXCollectionEditorForm.FormHide(Sender: TObject);
begin
  SetXCollection(nil, nil);
  ReleaseXCollectionEditor;
end;

procedure TXCollectionEditorForm.SetXCollection(aXCollection: TXCollection; designer: IDesigner);
begin
  // if Assigned(ownerComponent) then
  // ownerComponent.RemoveFreeNotification(Self);
  FXCollection := aXCollection;
  FDesigner := designer;
  if Assigned(FXCollection) then
  begin
    // if Assigned(FXCollection.Owner) and (FXCollection.Owner is TComponent) then
    // ownerComponent:=TComponent(FXCollection.Owner);
    // if Assigned(ownerComponent) then
    // ownerComponent.FreeNotification(Self);
    Caption := FXCollection.GetNamePath;
  end
  else
  begin
    // ownerComponent:=nil;
    Caption := strXCollectionEditor;
  end;
  PrepareListView;
end;

procedure TXCollectionEditorForm.TBAddClick(Sender: TObject);
begin
  TBAdd.CheckMenuDropdown;
end;

procedure TXCollectionEditorForm.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  sel: Boolean;
begin
  if (Change = ctState) and Assigned(FDesigner) and (not updatingListView) then
  begin
    // setup enablings
    sel := (ListView.Selected <> nil);
    TBAdd.Enabled := Assigned(FDesigner);
    ACRemove.Enabled := sel;
    ACMoveUp.Enabled := sel and (ListView.Selected.Index > 0);
    ACMoveDown.Enabled := sel and
      (ListView.Selected.Index < ListView.Items.Count - 1);
    if Assigned(FDesigner) then
      if sel then
        FDesigner.SelectComponent(TXCollectionItem(ListView.Selected.Data))
      else
        FDesigner.SelectComponent(nil);
  end;
end;

procedure TXCollectionEditorForm.PrepareListView;
var
  i: Integer;
  prevSelData: Pointer;
  XCollectionItem: TXCollectionItem;
  DisplayedName: String;
begin
  Assert(Assigned(ListView));
  updatingListView := True;
  try
    if ListView.Selected <> nil then
      prevSelData := ListView.Selected.Data
    else
      prevSelData := nil;
    with ListView.Items do
    begin
      BeginUpdate;
      Clear;
      if Assigned(FXCollection) then
      begin
        for i := 0 to FXCollection.Count - 1 do
          with Add do
          begin
            XCollectionItem := FXCollection[i];
            DisplayedName := XCollectionItem.Name;
            if DisplayedName = '' then
              DisplayedName := '(unnamed)';
            Caption := Format('%d - %s', [i, DisplayedName]);
            SubItems.Add(XCollectionItem.FriendlyName);
            Data := XCollectionItem;
          end;
        if prevSelData <> nil then
          ListView.Selected := ListView.FindData(0, prevSelData, True, False);
      end;
      EndUpdate;
    end;
  finally
    updatingListView := False;
  end;
  ListViewChange(Self, nil, ctState);
end;

procedure TXCollectionEditorForm.PrepareXCollectionItemPopup(parent: TMenuItem);
var
  i: Integer;
  list: TList;
  XCollectionItemClass: TXCollectionItemClass;
  mi, categoryItem: TMenuItem;
begin
  list := GetXCollectionItemClassesList(FXCollection.ItemsClass);
  try
    parent.Clear;
    for i := 0 to list.Count - 1 do
    begin
      XCollectionItemClass := TXCollectionItemClass(list[i]);
      if XCollectionItemClass.ItemCategory <> '' then
      begin
        categoryItem := parent.Find(XCollectionItemClass.ItemCategory);
        if categoryItem = nil then
        begin
          categoryItem := TMenuItem.Create(owner);
          categoryItem.Caption := XCollectionItemClass.ItemCategory;
          parent.Add(categoryItem);
        end;
      end
      else
        categoryItem := parent;

      mi := TMenuItem.Create(owner);
      mi.Caption := XCollectionItemClass.FriendlyName;
      mi.OnClick := OnAddXCollectionItemClick;
      mi.Tag := Integer(XCollectionItemClass);
      mi.Enabled := Assigned(FXCollection) and
        FXCollection.CanAdd(XCollectionItemClass);
      categoryItem.Add(mi);
    end;
  finally
    list.Free;
  end;
end;

procedure TXCollectionEditorForm.OnNameChanged(Sender: TObject);
begin
  if TXCollectionItem(Sender).owner = FXCollection then
    PrepareListView;
end;

procedure TXCollectionEditorForm.OnXCollectionDestroyed(Sender: TObject);
begin
  if TXCollection(Sender) = FXCollection then
    Close;
end;

procedure TXCollectionEditorForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  { if (Operation=opRemove) and (AComponent=ownerComponent) then begin
    ownerComponent:=nil;
    SetXCollection(nil, nil);
    Close;
    end;
  }
  inherited;
end;

procedure TXCollectionEditorForm.OnAddXCollectionItemClick(Sender: TObject);
var
  XCollectionItemClass: TXCollectionItemClass;
  XCollectionItem: TXCollectionItem;
begin
  XCollectionItemClass := TXCollectionItemClass((Sender as TMenuItem).Tag);
  XCollectionItem := XCollectionItemClass.Create(FXCollection);
  PrepareListView;
  ListView.Selected := ListView.FindData(0, XCollectionItem, True, False);
  FDesigner.Modified;
end;

procedure TXCollectionEditorForm.ACRemoveExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    FDesigner.Modified;
    FDesigner.SelectComponent(FXCollection.owner);

    TXCollectionItem(ListView.Selected.Data).Free;
    ListView.Selected.Free;
    ListViewChange(Self, nil, ctState);
  end;
end;

procedure TXCollectionEditorForm.ACMoveUpExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    TXCollectionItem(ListView.Selected.Data).MoveUp;
    PrepareListView;
    FDesigner.Modified;
  end;
end;

procedure TXCollectionEditorForm.ACMoveDownExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    TXCollectionItem(ListView.Selected.Data).MoveDown;
    PrepareListView;
    FDesigner.Modified;
  end;
end;

procedure TXCollectionEditorForm.PMToolBarPopup(Sender: TObject);
begin
  PrepareXCollectionItemPopup(PMToolBar.Items);
end;

procedure TXCollectionEditorForm.PMListViewPopup(Sender: TObject);
begin
  PrepareXCollectionItemPopup(MIAdd);
end;

//-----------------------------------
initialization
//-----------------------------------

finalization

ReleaseXCollectionEditor;

end.
