//
// The graphics rendering engine GLScene http://glscene.org
//
unit FmSceneEditor;

(* Scene Editor, for adding + removing scene objects *)

interface

{$I GLScene.inc}

uses
  WinApi.Windows,
  System.Classes,
  System.SysUtils,
  System.Win.Registry,
  System.ImageList, 
  System.Actions, 
  VCL.ActnList,
  VCL.Controls,
  VCL.Forms,
  VCL.ComCtrls,
  VCL.ImgList,
  VCL.Dialogs,
  VCL.Menus,
  VCL.ToolWin,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.ClipBrd,

  DesignIntf,
  VCLEditors,

  GLS.Scene,
  GLS.SceneViewer,
  GLS.Strings,
  FmInfo,
  GLS.XCollection,

  GLS.Utils,
  GLS.SceneRegister;

const
  SCENE_SELECTED = 0;
  BEHAVIOURS_SELECTED = 1;
  EFFECTS_SELECTED = 2;

type
  TSetSubItemsEvent = procedure(Sender: TObject) of object;

  TGLSceneEditorForm = class(TForm)
    PopupMenu: TPopupMenu;
    MIAddObject: TMenuItem;
    N1: TMenuItem;
    MIDelObject: TMenuItem;
    ToolBar: TToolBar;
    ActionList: TActionList;
    TBAddObjects: TToolButton;
    TBMoveUp: TToolButton;
    pmToolBar: TPopupMenu;
    TBDeleteObject: TToolButton;
    TBMoveDown: TToolButton;
    acAddObject: TAction;
    ImageList: TImageList;
    acDeleteObject: TAction;
    acMoveUp: TAction;
    acMoveDown: TAction;
    N2: TMenuItem;
    MIMoveUp: TMenuItem;
    MIMoveDown: TMenuItem;
    acSaveScene: TAction;
    acLoadScene: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    TBLoadScene: TToolButton;
    TBSaveScene: TToolButton;
    acInfo: TAction;
    acCopy: TAction;
    acCut: TAction;
    acPaste: TAction;
    MICopy: TMenuItem;
    MIPaste: TMenuItem;
    MICut: TMenuItem;
    TBCut: TToolButton;
    TBCopy: TToolButton;
    TBPaste: TToolButton;
    pmBehavioursToolbar: TPopupMenu;
    acAddBehaviour: TAction;
    MIAddBehaviour: TMenuItem;
    MIAddEffect: TMenuItem;
    MIBehaviourSeparator: TMenuItem;
    acDeleteBehaviour: TAction;
    pmBehaviours: TPopupMenu;
    Delete1: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    N4: TMenuItem;
    pmEffectsToolbar: TPopupMenu;
    acAddEffect: TAction;
    TBCharacterPanels: TToolButton;
    TBStayOnTop: TToolButton;
    acStayOnTop: TAction;
    TBSeparator4: TToolButton;
    TBExpand: TToolButton;
    acExpand: TAction;
    PAGallery: TPanel;
    PATree: TPanel;
    Tree: TTreeView;
    TBInfo: TToolButton;
    GalleryListView: TListView;
    PABehaviours: TPanel;
    ToolBarBehaviours: TToolBar;
    TBAddBehaviours: TToolButton;
    BehavioursListView: TListView;
    PAEffects: TPanel;
    EffectsListView: TListView;
    ToolBarEffects: TToolBar;
    TBAddEffects: TToolButton;
    TBGalleryPanel: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure TreeEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure TreeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure TreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeEnter(Sender: TObject);
    procedure TreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure acDeleteObjectExecute(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acAddObjectExecute(Sender: TObject);
    procedure acSaveSceneExecute(Sender: TObject);
    procedure acLoadSceneExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acInfoExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acCutExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure BehavioursListViewEnter(Sender: TObject);
    procedure EffectsListViewEnter(Sender: TObject);
    procedure acAddBehaviourExecute(Sender: TObject);
    procedure DeleteBaseBehaviour(ListView: TListView);
    procedure pmBehavioursToolbarPopup(Sender: TObject);
    procedure pmEffectsToolbarPopup(Sender: TObject);
    procedure BehavioursListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure acAddEffectExecute(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure TBCharacterPanelsClick(Sender: TObject);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure acStayOnTopExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure acExpandExecute(Sender: TObject);
    procedure TBGalleryPanelClick(Sender: TObject);
  private
    FSelectedItems: Integer; //
    FScene: TGLScene;
    FObjectNode, FSceneObjects: TTreeNode;
    FCurrentDesigner: IDesigner;
    FLastMouseDownPos: TPoint;
    FPasteOwner: TComponent;
    FPasteSelection: IDesignerSelections;
    procedure ReadScene;
    procedure ResetTree;
    // adds the given scene object as well as its children to the tree structure and returns
    // the last add node (e.g. for selection)
    function AddNodes(ANode: TTreeNode; AObject: TGLBaseSceneObject): TTreeNode;
    procedure AddObjectClick(Sender: TObject);
    procedure AddBehaviourClick(Sender: TObject);
    procedure AddEffectClick(Sender: TObject);
    procedure SetObjectsSubItems(parent: TMenuItem);
    procedure SetXCollectionSubItems(parent: TMenuItem; XCollection: TXCollection; Event: TSetSubItemsEvent);
    procedure SetBehavioursSubItems(parent: TMenuItem; XCollection: TXCollection);
    procedure SetEffectsSubItems(parent: TMenuItem; XCollection: TXCollection);
    procedure OnBaseSceneObjectNameChanged(Sender: TObject);
    function IsValidClipBoardNode: Boolean;
    function IsPastePossible: Boolean;
    procedure ShowBehaviours(BaseSceneObject: TGLBaseSceneObject);
    procedure ShowEffects(BaseSceneObject: TGLBaseSceneObject);
    procedure ShowGallery(BaseSceneObject: TGLBaseSceneObject);
    procedure ShowBehavioursAndEffects(BaseSceneObject: TGLBaseSceneObject);
    procedure EnableAndDisableActions();
    function CanPaste(obj, destination: TGLBaseSceneObject): Boolean;
    procedure CopyComponents(Root: TComponent; const Components: IDesignerSelections);
    procedure MethodError(Reader: TReader; const MethodName: string; var Address: Pointer; var Error: Boolean);
    function PasteComponents(AOwner, AParent: TComponent; const Components: IDesignerSelections): Boolean;
    procedure ReaderSetName(Reader: TReader; Component: TComponent; var Name: string);
    procedure ComponentRead(Component: TComponent);
    function UniqueName(Component: TComponent): string;
    (* We can not use the IDE to define this event because the
     prototype is not the same between Delphi and Kylix !! *)
    procedure TreeEdited(Sender: TObject; Node: TTreeNode; var S: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure SetScene(Scene: TGLScene; Designer: IDesigner);
  end;

function GLSceneEditorForm: TGLSceneEditorForm;
procedure ReleaseGLSceneEditorForm;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

{$R *.dfm}

const
  cRegistryKey = 'Software\GLScene\GLSceneEditor';

var
  vGLSceneEditorForm: TGLSceneEditorForm;

function GLSceneEditorForm: TGLSceneEditorForm;
begin
  if not Assigned(vGLSceneEditorForm) then
    vGLSceneEditorForm := TGLSceneEditorForm.Create(nil);
  Result := vGLSceneEditorForm;
end;

procedure ReleaseGLSceneEditorForm;
begin
  if Assigned(vGLSceneEditorForm) then
  begin
    vGLSceneEditorForm.Free;
    vGLSceneEditorForm := nil;
  end;
end;

function ReadRegistryInteger(reg: TRegistry; const Name: string;
  defaultValue: Integer): Integer;
begin
  if reg.ValueExists(name) then
    Result := reg.ReadInteger(name)
  else
    Result := defaultValue;
end;

function FindNodeByData(treeNodes: TTreeNodes; data: Pointer;
  baseNode: TTreeNode = nil): TTreeNode;
var
  n: TTreeNode;
begin
  Result := nil;
  if Assigned(baseNode) then
  begin
    n := baseNode.getFirstChild;
    while Assigned(n) do
    begin
      if n.data = data then
      begin
        Result := n;
        Break;
      end
      else if n.HasChildren then
      begin
        Result := FindNodeByData(treeNodes, data, n);
        if Assigned(Result) then
          Break;
      end;
      n := baseNode.GetNextChild(n);
    end;
  end
  else
  begin
    n := treeNodes.GetFirstNode;
    while Assigned(n) do
    begin
      if n.data = data then
      begin
        Result := n;
        Break;
      end
      else if n.HasChildren then
      begin
        Result := FindNodeByData(treeNodes, data, n);
        if Assigned(Result) then
          Break;
      end;
      n := n.getNextSibling;
    end;
  end;
end;

// ----------------- TGLSceneEditorForm ---------------------------------------------------------------------------------

procedure TGLSceneEditorForm.SetScene(Scene: TGLScene; Designer: IDesigner);
begin
  if Assigned(FScene) then
    FScene.RemoveFreeNotification(Self);
  FScene := Scene;
  FCurrentDesigner := Designer;
  ResetTree;
  BehavioursListView.Items.Clear;
  EffectsListView.Items.Clear;
  GalleryListView.Items.Clear;

  if Assigned(FScene) then
  begin
    FScene.FreeNotification(Self);
    ReadScene;
    Caption := strGLSceneEditor + ' : ' + FScene.Name;
  end
  else
    Caption := strGLSceneEditor;
  TreeChange(Self, nil);
  if Assigned(FScene) then
  begin
    Tree.Enabled := true;
    BehavioursListView.Enabled := true;
    EffectsListView.Enabled := true;
    ACLoadScene.Enabled := true;
    ACSaveScene.Enabled := true;
    FSelectedItems := SCENE_SELECTED;
    EnableAndDisableActions;
  end
  else
  begin
    Tree.Enabled := False;
    BehavioursListView.Enabled := False;
    EffectsListView.Enabled := False;
    ACLoadScene.Enabled := False;
    ACSaveScene.Enabled := False;
    ACAddObject.Enabled := False;
    ACAddBehaviour.Enabled := False;
    ACAddEffect.Enabled := False;
    ACDeleteObject.Enabled := False;
    ACMoveUp.Enabled := False;
    ACMoveDown.Enabled := False;
    ACCut.Enabled := False;
    ACCopy.Enabled := False;
    ACPaste.Enabled := False;
  end;
  ShowBehavioursAndEffects(nil);
end;

procedure TGLSceneEditorForm.FormCreate(Sender: TObject);
var
  CurrentNode: TTreeNode;
  reg: TRegistry;
begin
  RegisterGLBaseSceneObjectNameChangeEvent(OnBaseSceneObjectNameChanged);
  Tree.Images := ObjectManager.ObjectIcons;
  Tree.Indent := ObjectManager.ObjectIcons.Width;
  with Tree.Items do
  begin
    // first add the scene root
    CurrentNode := Add(nil, strSceneRoot);
    with CurrentNode do
    begin
      ImageIndex := ObjectManager.SceneRootIndex;
      SelectedIndex := ImageIndex;
    end;
    // and the root for all objects
    FObjectNode := AddChild(CurrentNode, strObjectRoot);
    FSceneObjects := FObjectNode;
    with FObjectNode do
    begin
      ImageIndex := ObjectManager.ObjectRootIndex;
      SelectedIndex := ImageIndex;
    end;
  end;
  // Build SubMenus
  SetObjectsSubItems(MIAddObject);
  MIAddObject.SubMenuImages := ObjectManager.ObjectIcons;
  SetObjectsSubItems(PMToolBar.Items);
  PMToolBar.Images := ObjectManager.ObjectIcons;

  SetBehavioursSubItems(MIAddBehaviour, nil);
  SetBehavioursSubItems(PMBehavioursToolbar.Items, nil);
  SetEffectsSubItems(MIAddEffect, nil);
  SetEffectsSubItems(PMEffectsToolbar.Items, nil);

  reg := TRegistry.Create;
  try
    if reg.OpenKey(cRegistryKey, true) then
    begin
      if reg.ValueExists('CharacterPanels') then
        TBCharacterPanels.Down := reg.ReadBool('CharacterPanels');
      TBCharacterPanelsClick(Self);

      if reg.ValueExists('ExpandTree') then
        TBExpand.Down := reg.ReadBool('ExpandTree');
      ACExpandExecute(Self);

      Left := ReadRegistryInteger(reg, 'Left', Left);
      Top := ReadRegistryInteger(reg, 'Top', Top);
      Width := ReadRegistryInteger(reg, 'Width', 250);
      Height := ReadRegistryInteger(reg, 'Height', Height);
    end;
  finally
    reg.Free;
  end;

  // Trigger the event OnEdited manualy
  Tree.OnEdited := TreeEdited;
end;

procedure TGLSceneEditorForm.FormDestroy(Sender: TObject);
var
  reg: TRegistry;
begin
  DeRegisterGLBaseSceneObjectNameChangeEvent(OnBaseSceneObjectNameChanged);

  reg := TRegistry.Create;
  try
    if reg.OpenKey(cRegistryKey, true) then
    begin
      reg.WriteBool('CharacterPanels', TBCharacterPanels.Down);
      reg.WriteBool('ExpandTree', TBExpand.Down);
      reg.WriteInteger('Left', Left);
      reg.WriteInteger('Top', Top);
      reg.WriteInteger('Width', Width);
      reg.WriteInteger('Height', Height);
    end;
  finally
    reg.Free;
  end;
end;

procedure TGLSceneEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F12 then
end;

// ----------------------------------------------------------------------------------------------------------------------
procedure TGLSceneEditorForm.ReadScene;

var
  I: Integer;

begin
  Tree.Items.BeginUpdate;
  with FScene do
  begin
    if Assigned(Objects) then
    begin
      FObjectNode.data := Objects;
      with Objects do
        for I := 0 to Count - 1 do
          AddNodes(FObjectNode, Children[I]);
      FObjectNode.Expand(False);
    end;
  end;
  Tree.Items.EndUpdate;
end;

// ----------------------------------------------------------------------------------------------------------------------
procedure TGLSceneEditorForm.ResetTree;
begin
  // delete all subtrees (empty tree)
  Tree.Items.BeginUpdate;
  try
    with FObjectNode do
    begin
      DeleteChildren;
      data := nil;
      parent.Expand(true);
    end;
  finally
    Tree.Items.EndUpdate;
  end;
end;

// ----------------------------------------------------------------------------------------------------------------------

function TGLSceneEditorForm.AddNodes(ANode: TTreeNode;
  AObject: TGLBaseSceneObject): TTreeNode;
var
  I: Integer;
  CurrentNode: TTreeNode;
begin
  if IsSubComponent(AObject) then
  begin
    Result := Tree.Selected;
    Exit;
  end
  else
  begin
    Result := Tree.Items.AddChildObject(ANode, AObject.Name, AObject);
    Result.ImageIndex := ObjectManager.GetImageIndex
      (TGLSceneObjectClass(AObject.ClassType));
    Result.SelectedIndex := Result.ImageIndex;
    CurrentNode := Result;
    for I := 0 to AObject.Count - 1 do
      Result := AddNodes(CurrentNode, AObject[I]);
  end;
end;

procedure TGLSceneEditorForm.SetObjectsSubItems(parent: TMenuItem);
var
  objectList: TStringList;
  i, j: Integer;
  Item, currentParent: TMenuItem;
  currentCategory: string;
  soc: TGLSceneObjectClass;
begin
  objectList := TStringList.Create;
  try
    ObjectManager.GetRegisteredSceneObjects(objectList);
    for i := 0 to objectList.Count - 1 do
      if objectList[i] <> '' then
      begin
        with ObjectManager do
          currentCategory :=
            GetCategory(TGLSceneObjectClass(objectList.Objects[i]));
        if currentCategory = '' then
          currentParent := parent
        else
        begin
          currentParent := NewItem(currentCategory, 0, False, true, nil, 0, '');
          parent.Add(currentParent);
        end;
        for j := I to objectList.Count - 1 do
          if objectList[j] <> '' then
            with ObjectManager do
            begin
              soc := TGLSceneObjectClass(objectList.Objects[j]);
              if currentCategory = GetCategory(soc) then
              begin
                Item := NewItem(objectList[j], 0, False, true,
                  AddObjectClick, 0, '');
                Item.ImageIndex := GetImageIndex(soc);
                Item.Tag := Integer(soc);
                currentParent.Add(Item);
                objectList[j] := '';
                if currentCategory = '' then
                  Break;
              end;
            end;
      end;
  finally
    objectList.Free;
  end;
end;

procedure TGLSceneEditorForm.SetXCollectionSubItems(parent: TMenuItem;
  XCollection: TXCollection; Event: TSetSubItemsEvent);
var
  i: Integer;
  list: TList;
  XCollectionItemClass: TXCollectionItemClass;
  mi: TMenuItem;
begin
  parent.Clear;
  if Assigned(XCollection) then
  begin
    list := GetXCollectionItemClassesList(XCollection.ItemsClass);
    try
      for i := 0 to list.Count - 1 do
      begin
        XCollectionItemClass := TXCollectionItemClass(list[i]);
        mi := TMenuItem.Create(owner);
        mi.Caption := XCollectionItemClass.FriendlyName;
        mi.OnClick := Event; // AddBehaviourClick;
        mi.Tag := Integer(XCollectionItemClass);
        if Assigned(XCollection) then
          mi.Enabled := XCollection.CanAdd(XCollectionItemClass)
        else
          mi.Enabled := TBAddBehaviours.Enabled;
        parent.Add(mi);
      end;
    finally
      list.Free;
    end;
  end;
end;


procedure TGLSceneEditorForm.SetBehavioursSubItems(parent: TMenuItem;
  XCollection: TXCollection);
begin
  SetXCollectionSubItems(parent, XCollection, AddBehaviourClick);
end;

procedure TGLSceneEditorForm.SetEffectsSubItems(parent: TMenuItem;
  XCollection: TXCollection);
begin
  SetXCollectionSubItems(parent, XCollection, AddEffectClick);
end;

procedure TGLSceneEditorForm.AddObjectClick(Sender: TObject);
var
  AParent, AObject: TGLBaseSceneObject;
  Node: TTreeNode;
begin
  if Assigned(FCurrentDesigner) then
    with Tree do
      if Assigned(Selected) and (Selected.Level > 0) then
      begin
        AParent := TGLBaseSceneObject(Selected.data);
        // FCurrentDesigner.cr
        AObject := TGLBaseSceneObject
          (FCurrentDesigner.CreateComponent
          (TGLSceneObjectClass(TMenuItem(Sender).Tag), AParent, 0, 0, 0, 0));
        TComponent(AObject).DesignInfo := 0;
        AParent.AddChild(AObject);
        Node := AddNodes(Selected, AObject);
        Node.Selected := true;
        FCurrentDesigner.Modified;
      end;
end;

procedure TGLSceneEditorForm.AddBehaviourClick(Sender: TObject);
var
  XCollectionItemClass: TXCollectionItemClass;
  AParent: TGLBaseSceneObject;
begin
  if Assigned(Tree.Selected) then
  begin
    AParent := TGLBaseSceneObject(Tree.Selected.data);
    XCollectionItemClass := TXCollectionItemClass((Sender as TMenuItem).Tag);
    XCollectionItemClass.Create(AParent.Behaviours);
    // PrepareListView;
    ShowBehaviours(AParent);
    // ListView.Selected:=ListView.FindData(0, XCollectionItem, True, False);
    FCurrentDesigner.Modified;
  end;
end;

procedure TGLSceneEditorForm.AddEffectClick(Sender: TObject);
var
  XCollectionItemClass: TXCollectionItemClass;
  AParent: TGLBaseSceneObject;
begin
  if Assigned(Tree.Selected) then
  begin

    AParent := TGLBaseSceneObject(Tree.Selected.data);
    XCollectionItemClass := TXCollectionItemClass((Sender as TMenuItem).Tag);
    XCollectionItemClass.Create(AParent.Effects);
    // PrepareListView;
    ShowEffects(AParent);
    // ListView.Selected:=ListView.FindData(0, XCollectionItem, True, False);
    FCurrentDesigner.Modified;
  end;
end;

procedure TGLSceneEditorForm.TreeDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Target: TTreeNode;
begin
  Accept := False;
  if Source = Tree then
    with Tree do
    begin
      Target := DropTarget;
      Accept := Assigned(Target) and (Selected <> Target) and
        Assigned(Target.data) and (not Target.HasAsParent(Selected));
    end;
end;

procedure TGLSceneEditorForm.TreeDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  SourceNode, DestinationNode: TTreeNode;
  SourceObject, DestinationObject: TGLBaseSceneObject;
begin
  if Assigned(FCurrentDesigner) then
  begin
    DestinationNode := Tree.DropTarget;
    if Assigned(DestinationNode) and (Source = Tree) then
    begin
      SourceNode := TTreeView(Source).Selected;
      SourceObject := SourceNode.data;
      DestinationObject := DestinationNode.data;
      DestinationObject.Insert(0, SourceObject);
      SourceNode.MoveTo(DestinationNode, naAddChildFirst);
      TreeChange(Self, nil);
      FCurrentDesigner.Modified;
    end;
  end;
end;

procedure TGLSceneEditorForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (FScene = AComponent) and (Operation = opRemove) then
  begin
    FScene := nil;
    SetScene(nil, nil);
  end;
  inherited;
end;

procedure TGLSceneEditorForm.OnBaseSceneObjectNameChanged(Sender: TObject);
var
  n: TTreeNode;
begin
  n := FindNodeByData(Tree.Items, Sender);
  if Assigned(n) then
    n.Text := (Sender as TGLBaseSceneObject).Name;
end;

procedure TGLSceneEditorForm.TreeChange(Sender: TObject; Node: TTreeNode);
var
  // selNode : TTreeNode;
  BaseSceneObject1: TGLBaseSceneObject;
begin
  if Assigned(FCurrentDesigner) then
  begin

    if Node <> nil then
    begin
      BaseSceneObject1 := TGLBaseSceneObject(Node.data);
      if BaseSceneObject1 <> nil then
      begin
        ShowBehavioursAndEffects(BaseSceneObject1);
      end;
    end;

    EnableAndDisableActions();
  end;
end;

procedure TGLSceneEditorForm.TreeEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := (Node.Level > 1);
end;

procedure TGLSceneEditorForm.ShowBehaviours(BaseSceneObject
  : TGLBaseSceneObject);
var
  I: Integer;
  DisplayedName: string;
begin
  BehavioursListView.Items.Clear;
  BehavioursListView.Items.BeginUpdate;
  if Assigned(BaseSceneObject) then
  begin
    for I := 0 to BaseSceneObject.Behaviours.Count - 1 do
    begin
      with BehavioursListView.Items.Add do
      begin
        DisplayedName := BaseSceneObject.Behaviours[I].Name;
        if DisplayedName = '' then
          DisplayedName := '(unnamed)';
        Caption := IntToStr(I) + ' - ' + DisplayedName;
        SubItems.Add(BaseSceneObject.Behaviours[I].FriendlyName);
        data := BaseSceneObject.Behaviours[I];
      end;
    end;
  end;
  BehavioursListView.Items.EndUpdate;
end;

procedure TGLSceneEditorForm.ShowEffects(BaseSceneObject: TGLBaseSceneObject);
var
  I: Integer;
  DisplayedName: string;
begin
  EffectsListView.Items.Clear;
  EffectsListView.Items.BeginUpdate;
  if Assigned(BaseSceneObject) then
  begin
    for I := 0 to BaseSceneObject.Effects.Count - 1 do
    begin
      with EffectsListView.Items.Add do
      begin
        DisplayedName := BaseSceneObject.Effects[I].Name;
        if DisplayedName = '' then
          DisplayedName := '(unnamed)';
        Caption := IntToStr(I) + ' - ' + DisplayedName;
        SubItems.Add(BaseSceneObject.Effects[I].FriendlyName);
        Data := BaseSceneObject.Effects[I];
      end;
    end;
  end;
  EffectsListView.Items.EndUpdate;
end;

procedure TGLSceneEditorForm.ShowGallery(BaseSceneObject: TGLBaseSceneObject);
var
  I: Integer;
  DisplayedName: string;
begin
 // GalleryListView.LargeImages := ObjectManager.ObjectIcons;
  GalleryListView.Items.Clear;
  GalleryListView.Items.BeginUpdate;
  if Assigned(BaseSceneObject) then
  begin
    for I := 0 to BaseSceneObject.Count - 1 do
    begin
      with GalleryListView.Items.Add do
      begin
        DisplayedName := BaseSceneObject.Name;
        if DisplayedName = '' then
          DisplayedName := '(unnamed)';
        Caption := IntToStr(I) + ' - ' + DisplayedName;
      end;
    end;
  end;
  GalleryListView.Items.EndUpdate;
end;

procedure TGLSceneEditorForm.ShowBehavioursAndEffects(BaseSceneObject
  : TGLBaseSceneObject);
begin
  ShowBehaviours(BaseSceneObject);
  ShowEffects(BaseSceneObject);
  ShowGallery(BaseSceneObject);
end;


procedure TGLSceneEditorForm.TreeEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
var
  BaseSceneObject1: TGLBaseSceneObject;
begin
  if Assigned(FCurrentDesigner) then
  begin
    // renaming a node means renaming a scene object
    BaseSceneObject1 := TGLBaseSceneObject(Node.data);
    if FScene.FindSceneObject(S) = nil then
      BaseSceneObject1.Name := S
    else
    begin
      Messagedlg('A component named ' + S + ' already exists', mtWarning,
        [mbok], 0);
      S := BaseSceneObject1.Name;
    end;
    ShowBehavioursAndEffects(BaseSceneObject1);

    FCurrentDesigner.Modified;
  end;
end;

procedure TGLSceneEditorForm.TreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FLastMouseDownPos := Point(X, Y);
end;

procedure TGLSceneEditorForm.TreeMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Node: TTreeNode;
begin
  if Shift = [ssLeft] then
  begin
    Node := Tree.Selected;
    if Assigned(Node) and (Node.Level > 1) then
      if (Abs(FLastMouseDownPos.X - X) > 4) or (Abs(FLastMouseDownPos.Y - Y) > 4)
      then
        Tree.BeginDrag(False);
  end;
end;

procedure TGLSceneEditorForm.TreeEnter(Sender: TObject);
begin
  if Assigned(FCurrentDesigner) and Assigned(Tree.Selected) then
    FCurrentDesigner.SelectComponent(TGLBaseSceneObject(Tree.Selected.data));
  FSelectedItems := SCENE_SELECTED;
  EnableAndDisableActions();
end;

procedure TGLSceneEditorForm.acDeleteObjectExecute(Sender: TObject);
var
  anObject: TGLBaseSceneObject;
  allowed, keepChildren: Boolean;
  confirmMsg: string;
  buttons: TMsgDlgButtons;
begin
  if FSelectedItems = BEHAVIOURS_SELECTED then
  begin
    DeleteBaseBehaviour(BehavioursListView);
    FCurrentDesigner.SelectComponent(TGLBaseSceneObject(Tree.Selected.data));
    ShowBehaviours(TGLBaseSceneObject(Tree.Selected.data));
  end
  else if FSelectedItems = EFFECTS_SELECTED then
  begin
    DeleteBaseBehaviour(EffectsListView);
    FCurrentDesigner.SelectComponent(TGLBaseSceneObject(Tree.Selected.data));
    ShowEffects(TGLBaseSceneObject(Tree.Selected.data));
  end
  else if FSelectedItems = SCENE_SELECTED then
  begin
    if Assigned(Tree.Selected) and (Tree.Selected.Level > 1) then
    begin
      anObject := TGLBaseSceneObject(Tree.Selected.data);
      // ask for confirmation
      if anObject.Name <> '' then
        confirmMsg := 'Delete ' + anObject.Name
      else
        confirmMsg := 'Delete the marked object';
      buttons := [mbok, mbCancel];
      // are there children to care for?
      // mbAll exist only on Windows ...
      if (anObject.Count > 0) and (not anObject.HasSubChildren) then
      begin
        confirmMsg := confirmMsg + ' only or with ALL its children?';
        buttons := [mbAll] + buttons;
      end
      else
        confirmMsg := confirmMsg + '?';
      case Messagedlg(confirmMsg, mtConfirmation, buttons, 0) of
        mrAll:
          begin
            keepChildren := False;
            allowed := true;
          end;
        mrOK:
          begin
            keepChildren := true;
            allowed := true;
          end;
        mrCancel:
          begin
            allowed := False;
            keepChildren := true;
          end;
      else
        allowed := False;
        keepChildren := true;
      end;
      // deletion allowed?
      if allowed then
      begin
        if keepChildren = true then
          while Tree.Selected.Count > 0 do
            Tree.Selected.Item[0].MoveTo(Tree.Selected, naAdd);
        // previous line should be "naInsert" if children are to remain in position of parent
        // (would require changes to TGLBaseSceneObject.Remove)
        Tree.Selected.Free;
        FCurrentDesigner.SelectComponent(nil);
        anObject.parent.Remove(anObject, keepChildren);
        anObject.Free;
      end
    end;
  end;
end;

procedure TGLSceneEditorForm.acExpandExecute(Sender: TObject);
begin
  if FSceneObjects <> nil then
    try
      Tree.Items.BeginUpdate;
      if TBExpand.Down then
        Tree.FullExpand
      else
      begin
        FSceneObjects.Collapse(True);
        FSceneObjects.Expand(False);
      end;
    finally
      Tree.Items.EndUpdate;
    end;
end;

procedure TGLSceneEditorForm.acMoveUpExecute(Sender: TObject);
var
  Node: TTreeNode;
  prevData: Pointer;
begin
  if FSelectedItems = BEHAVIOURS_SELECTED then
  begin
    prevData := BehavioursListView.Selected.data;
    TGLBaseBehaviour(prevData).MoveUp;
    ShowBehaviours(TGLBaseSceneObject(Tree.Selected.data));
    BehavioursListView.Selected := BehavioursListView.FindData(0, prevData,
      true, False);
    FCurrentDesigner.Modified;
  end
  else if FSelectedItems = EFFECTS_SELECTED then
  begin
    prevData := EffectsListView.Selected.data;
    TGLBaseBehaviour(prevData).MoveUp;
    ShowEffects(TGLBaseSceneObject(Tree.Selected.data));
    EffectsListView.Selected := EffectsListView.FindData(0, prevData,
      true, False);
    FCurrentDesigner.Modified;
  end
  else if FSelectedItems = SCENE_SELECTED then
  begin
    if ACMoveUp.Enabled then
    begin
      Node := Tree.Selected;
      if Assigned(Node) then
      begin
        Node.MoveTo(Node.GetPrevSibling, naInsert);
        with TGLBaseSceneObject(Node.data) do
        begin
          MoveUp;
          Update;
        end;
        TreeChange(Self, Node);
        FCurrentDesigner.Modified;
      end;
    end;
  end;
end;

procedure TGLSceneEditorForm.acMoveDownExecute(Sender: TObject);
var
  Node: TTreeNode;
  prevData: Pointer;
begin
  if FSelectedItems = BEHAVIOURS_SELECTED then
  begin
    prevData := BehavioursListView.Selected.data;
    TGLBaseBehaviour(prevData).MoveDown;
    ShowBehaviours(TGLBaseSceneObject(Tree.Selected.data));
    BehavioursListView.Selected := BehavioursListView.FindData(0, prevData,
      true, False);
    FCurrentDesigner.Modified;
  end
  else if FSelectedItems = EFFECTS_SELECTED then
  begin
    prevData := EffectsListView.Selected.data;
    TGLBaseBehaviour(prevData).MoveDown;
    ShowEffects(TGLBaseSceneObject(Tree.Selected.data));
    EffectsListView.Selected := EffectsListView.FindData(0, prevData,
      true, False);
    FCurrentDesigner.Modified;
  end
  else if FSelectedItems = SCENE_SELECTED then
  begin
    if ACMoveDown.Enabled then
    begin
      Node := Tree.Selected;
      if Assigned(Node) then
      begin
        Node.getNextSibling.MoveTo(Node, naInsert);
        with TGLBaseSceneObject(Node.data) do
        begin
          MoveDown;
          Update;
        end;
        TreeChange(Self, Node);
        FCurrentDesigner.Modified;
      end;
    end;
  end;
end;

procedure TGLSceneEditorForm.acAddObjectExecute(Sender: TObject);
begin
  TBAddObjects.CheckMenuDropdown;
end;

procedure TGLSceneEditorForm.acStayOnTopExecute(Sender: TObject);
begin
  if TBStayOnTop.Down then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TGLSceneEditorForm.acSaveSceneExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
    FScene.SaveToFile(SaveDialog.FileName);
end;

procedure TGLSceneEditorForm.acLoadSceneExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FScene.LoadFromFile(OpenDialog.FileName);
    ResetTree;
    ReadScene;
    ShowBehavioursAndEffects(nil);
  end;
end;

procedure TGLSceneEditorForm.acInfoExecute(Sender: TObject);
var
  AScene: TGLSceneViewer;
begin
  AScene := TGLSceneViewer.Create(Self);
  AScene.Name := 'GLSceneEditor';
  AScene.Width := 0;
  AScene.Height := 0;
  AScene.parent := Self;
  try
    AScene.Buffer.ShowInfo;
  finally
    AScene.Free;
  end;
end;

function TGLSceneEditorForm.IsValidClipBoardNode: Boolean;
var
  selNode: TTreeNode;
begin
  selNode := Tree.Selected;
  Result := ((selNode <> nil) and (selNode.parent <> nil) and
    (selNode.parent.parent <> nil));
end;

function TGLSceneEditorForm.IsPastePossible: Boolean;
  function PossibleStream(const S: string): Boolean;
  var
    I: Integer;
  begin
    Result := true;
    for I := 1 to Length(S) - 6 do
    begin
      if CharInSet(S[I], ['O', 'o']) and
        (CompareText(Copy(S, I, 6), 'OBJECT') = 0) then
        Exit;
      if not CharInSet(S[I], [' ', #9, #13, #10]) then
        Break;
    end;
    Result := False;
  end;

var
  selNode: TTreeNode;
  anObject, destination: TGLBaseSceneObject;
  ComponentList: IDesignerSelections;
  TmpContainer: TComponent;
begin
  selNode := Tree.Selected;

  if (selNode <> nil) and (selNode.parent <> nil)
    and (ClipBoard.HasFormat(CF_COMPONENT) or (ClipBoard.HasFormat(CF_TEXT) and
    PossibleStream(ClipBoard.AsText))) then
  begin
    TmpContainer := TComponent.Create(Self);
    try
      ComponentList := TDesignerSelections.Create;
      if PasteComponents(TmpContainer, TmpContainer, ComponentList) then
        if (ComponentList.Count > 0) and (ComponentList[0] is TGLBaseSceneObject)
        then
        begin
          anObject := TGLBaseSceneObject(ComponentList[0]);
          destination := TGLBaseSceneObject(selNode.data);
          Result := CanPaste(anObject, destination);
        end
        else
          Result := False
      else
        Result := False;
    finally
      TmpContainer.Free;
    end;
  end
  else
    Result := False;
end;

function TGLSceneEditorForm.CanPaste(obj, destination
  : TGLBaseSceneObject): Boolean;
begin
  Result := Assigned(obj) and Assigned(destination);
end;

procedure TGLSceneEditorForm.acCopyExecute(Sender: TObject);
var
  ComponentList: IDesignerSelections;
begin
  ComponentList := TDesignerSelections.Create;
  ComponentList.Add(TGLBaseSceneObject(Tree.Selected.data));
  CopyComponents(FScene.owner, ComponentList);
  ACPaste.Enabled := IsPastePossible;
end;

procedure TGLSceneEditorForm.acCutExecute(Sender: TObject);
var
  AObject: TGLBaseSceneObject;
  ComponentList: IDesignerSelections;
begin
  if IsValidClipBoardNode then
  begin
    AObject := TGLBaseSceneObject(Tree.Selected.data);
    ComponentList := TDesignerSelections.Create;
    ComponentList.Add(TGLBaseSceneObject(Tree.Selected.data));

    CopyComponents(FScene.owner, ComponentList);
    AObject.parent.Remove(AObject, False);
    AObject.Free;
    Tree.Selected.Free;
    ACPaste.Enabled := IsPastePossible;
  end;
end;

procedure TGLSceneEditorForm.acPasteExecute(Sender: TObject);
var
  selNode: TTreeNode;
  destination: TGLBaseSceneObject;
  ComponentList: IDesignerSelections;
  t: Integer;
begin
  selNode := Tree.Selected;
  if (selNode <> nil) and (selNode.parent <> nil) then
  begin
    destination := TGLBaseSceneObject(selNode.data);
    ComponentList := TDesignerSelections.Create;
    PasteComponents(FScene.owner, destination, ComponentList);
    if (ComponentList.Count > 0) and
      (CanPaste(TGLBaseSceneObject(ComponentList[0]), destination)) then
    begin
      for t := 0 to ComponentList.Count - 1 do
        AddNodes(selNode, TGLBaseSceneObject(ComponentList[t]));
      selNode.Expand(False);
    end;
    FCurrentDesigner.Modified;
  end;
end;

procedure TGLSceneEditorForm.CopyComponents(Root: TComponent;
  const Components: IDesignerSelections);
var
  S: TMemoryStream;
  W: TWriter;
  I: Integer;
begin
  S := TMemoryStream.Create;
  try
    W := TWriter.Create(S, 1024);
    try
      W.Root := Root;
      for I := 0 to Components.Count - 1 do
      begin
        W.WriteSignature;
        W.WriteComponent(TComponent(Components[I]));
      end;
      W.WriteListEnd;
    finally
      W.Free;
    end;
    CopyStreamToClipboard(S);
  finally
    S.Free;
  end;
end;

procedure TGLSceneEditorForm.MethodError(Reader: TReader;
  const MethodName: string; var Address: Pointer; var Error: Boolean);
begin
  // error is true because Address is nil in csDesigning
  Error := False;
end;

function TGLSceneEditorForm.PasteComponents(AOwner, AParent: TComponent;
  const Components: IDesignerSelections): Boolean;
var
  S: TStream;
  R: TReader;
begin
  // catch GetClipboardStream exceptions that can easilly occured
  try
    S := GetClipboardStream;
    try
      R := TReader.Create(S, 1024);
      try
        R.OnSetName := ReaderSetName;
        R.OnFindMethod := MethodError;
        FPasteOwner := AOwner;
        FPasteSelection := Components;
        R.ReadComponents(AOwner, AParent, ComponentRead);
        Result := true;
      finally
        R.Free;
      end;
    finally
      S.Free;
    end;
  finally
  end;
end;

procedure TGLSceneEditorForm.ReaderSetName(Reader: TReader;
  Component: TComponent; var Name: string);
begin
  if (Reader.Root = FPasteOwner) and (FPasteOwner.FindComponent(Name) <> nil)
  then
    Name := UniqueName(Component);
end;

function TGLSceneEditorForm.UniqueName(Component: TComponent): string;
begin
  Result := FCurrentDesigner.UniqueName(Component.ClassName);
end;

procedure TGLSceneEditorForm.ComponentRead(Component: TComponent);
begin
  FPasteSelection.Add(Component);
end;

procedure TGLSceneEditorForm.BehavioursListViewEnter(Sender: TObject);
begin
  if Assigned(FCurrentDesigner) and Assigned(BehavioursListView.Selected) then
    FCurrentDesigner.SelectComponent
      (TGLBaseBehaviour(BehavioursListView.Selected.data));
  FSelectedItems := BEHAVIOURS_SELECTED;
  EnableAndDisableActions();
end;

procedure TGLSceneEditorForm.EffectsListViewEnter(Sender: TObject);
begin
  if Assigned(FCurrentDesigner) and Assigned(EffectsListView.Selected) then
    FCurrentDesigner.SelectComponent
      (TGLBaseBehaviour(EffectsListView.Selected.data));
  FSelectedItems := EFFECTS_SELECTED;
  EnableAndDisableActions();
end;

procedure TGLSceneEditorForm.acAddBehaviourExecute(Sender: TObject);
begin
  TBAddBehaviours.CheckMenuDropdown
end;

procedure TGLSceneEditorForm.DeleteBaseBehaviour(ListView: TListView);
begin
  if ListView.Selected <> nil then
  begin
    FCurrentDesigner.Modified;
    FCurrentDesigner.NoSelection;
    TXCollectionItem(ListView.Selected.data).Free;
    ListView.Selected.Free;
    // ListViewChange(Self, nil, ctState);
    ShowBehavioursAndEffects(TGLBaseSceneObject(Tree.Selected.data));
  end;
end;

procedure TGLSceneEditorForm.pmBehavioursToolbarPopup(Sender: TObject);
var
  object1: TGLBaseSceneObject;
begin
  if (Tree.Selected) <> nil then
  begin
    object1 := TGLBaseSceneObject(Tree.Selected.data);
    SetBehavioursSubItems(PMBehavioursToolbar.Items, object1.Behaviours);
  end;
end;

procedure TGLSceneEditorForm.pmEffectsToolbarPopup(Sender: TObject);
var
  object1: TGLBaseSceneObject;
begin
  if (Tree.Selected) <> nil then
  begin
    object1 := TGLBaseSceneObject(Tree.Selected.data);
    SetEffectsSubItems(PMEffectsToolbar.Items, object1.Effects);
  end;
end;

procedure TGLSceneEditorForm.BehavioursListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  EnableAndDisableActions();
end;

procedure TGLSceneEditorForm.acAddEffectExecute(Sender: TObject);
begin
  TBAddEffects.CheckMenuDropdown;
end;

procedure TGLSceneEditorForm.EnableAndDisableActions();
var
  selNode: TTreeNode;
begin
  if FSelectedItems = SCENE_SELECTED then
  begin
    selNode := Tree.Selected;
    // select in Delphi IDE
    if Assigned(selNode) then
    begin
      if Assigned(selNode.data) then
        FCurrentDesigner.SelectComponent(TGLBaseSceneObject(selNode.data))
      else
        FCurrentDesigner.SelectComponent(FScene);
      // enablings
      ACAddObject.Enabled := ((selNode = FObjectNode) or selNode.HasAsParent(FObjectNode));
      ACAddBehaviour.Enabled := (selNode.HasAsParent(FObjectNode));
      ACAddEffect.Enabled := (selNode.HasAsParent(FObjectNode));
      ACDeleteObject.Enabled := (selNode.Level > 1);
      ACMoveUp.Enabled := ((selNode.Index > 0) and (selNode.Level > 1));
      ACMoveDown.Enabled := ((selNode.getNextSibling <> nil) and (selNode.Level > 1));
      ACCut.Enabled := IsValidClipBoardNode;
      ACPaste.Enabled := IsPastePossible;

    end
    else
    begin
      ACAddObject.Enabled := False;
      ACAddBehaviour.Enabled := False;
      ACAddEffect.Enabled := False;
      ACDeleteObject.Enabled := False;
      ACMoveUp.Enabled := False;
      ACMoveDown.Enabled := False;
      ACCut.Enabled := False;
      ACPaste.Enabled := False;
    end;
    // end;
    ACCopy.Enabled := ACCut.Enabled;

  end
  else if FSelectedItems = BEHAVIOURS_SELECTED then
  begin
    if (BehavioursListView.Selected <> nil) then
    begin
      FCurrentDesigner.SelectComponent
        (TGLBaseBehaviour(BehavioursListView.Selected.data));
      ACDeleteObject.Enabled := true;
      ACMoveUp.Enabled := (BehavioursListView.Selected.Index > 0);
      ACMoveDown.Enabled := (BehavioursListView.Selected.
        Index < BehavioursListView.Selected.owner.Count - 1);
      ACCut.Enabled := False;
      ACCopy.Enabled := False;
      ACPaste.Enabled := False;
    end
    else
    begin
      ACDeleteObject.Enabled := False;
      ACMoveUp.Enabled := False;
      ACMoveDown.Enabled := False;
      ACCut.Enabled := False;
      ACCopy.Enabled := False;
      ACPaste.Enabled := False;
    end;
  end
  else if FSelectedItems = EFFECTS_SELECTED then
  begin
    if (EffectsListView.Selected <> nil) then
    begin
      FCurrentDesigner.SelectComponent
        (TGLBaseBehaviour(EffectsListView.Selected.data));
      ACDeleteObject.Enabled := true;
      ACMoveUp.Enabled := (EffectsListView.Selected.Index > 0);
      ACMoveDown.Enabled := (EffectsListView.Selected.
        Index < EffectsListView.Selected.owner.Count - 1);
      ACCut.Enabled := False;
      ACCopy.Enabled := False;
      ACPaste.Enabled := False;
    end
    else
    begin
      ACDeleteObject.Enabled := False;
      ACMoveUp.Enabled := False;
      ACMoveDown.Enabled := False;
      ACCut.Enabled := False;
      ACCopy.Enabled := False;
      ACPaste.Enabled := False;
    end;
  end;
end;

procedure TGLSceneEditorForm.PopupMenuPopup(Sender: TObject);
var
  obj: TObject;
  sceneObj: TGLBaseSceneObject;
begin
  if (Tree.Selected) <> nil then
  begin
    obj := TObject(Tree.Selected.data);
    if Assigned(obj) and (obj is TGLBaseSceneObject) then
    begin
      sceneObj := TGLBaseSceneObject(obj);
      SetBehavioursSubItems(MIAddBehaviour, sceneObj.Behaviours);
      SetEffectsSubItems(MIAddEffect, sceneObj.Effects);
    end
    else
    begin
      SetBehavioursSubItems(MIAddBehaviour, nil);
      SetEffectsSubItems(MIAddEffect, nil);
    end;
  end;
end;

procedure TGLSceneEditorForm.TBCharacterPanelsClick(Sender: TObject);
begin
  PABehaviours.Visible := TBCharacterPanels.Down;
  PAEffects.Visible := TBCharacterPanels.Down;
  if PABehaviours.Visible then
    Height := Height + PABehaviours.Height + PAEffects.Height
  else
    Height := Height - PABehaviours.Height - PAEffects.Height;
end;

procedure TGLSceneEditorForm.TBGalleryPanelClick(Sender: TObject);
begin
  //yet not ready to populate with LargeImages 32x32
  PAGallery.Visible := TBGalleryPanel.Down;
  if PAGallery.Visible then
    Width := Width + PATree.Width
  else
    Width := Width - PATree.Width;

end;

procedure TGLSceneEditorForm.TreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  FNode: TTreeNode;
begin
  if (Key = VK_DELETE) and not Tree.IsEditing then
  begin
    Key := 0;
    ACDeleteObject.Execute;
  end;
  if Key = VK_F2 then
  begin
    FNode := Tree.Selected;
    if FNode.Level > 1 then
    begin
      FNode.EditText;
    end;
  end;
end;

//--------------------------------------------------------------
initialization
//--------------------------------------------------------------

finalization

ReleaseGLSceneEditorForm;

end.
