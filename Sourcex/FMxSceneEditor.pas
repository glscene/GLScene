//
// The graphics engine GXScene https://github.com/glscene
//
unit FMxSceneEditor;

(* Scene Editor, for adding + removing scene objects in Delphi IDE *)

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Actions,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Menus,
  FMX.ActnList,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.TreeView,
  FMX.ListView.Types,
  FMX.ListView,
  FMX.Objects,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation,

  FMxInfo,

  GXS.Scene,
  GXS.SceneViewer,
  GXS.SceneRegister,
  GXS.Strings,
  GXS.XCollection;

type
  TSceneEditorForm = class(TForm)
    ToolBar: TToolBar;
    PATree: TPanel;
    PAGallery: TPanel;
    PAEffects: TPanel;
    ActionList: TActionList;
    PMToolBar: TPopupMenu;
    PopupMenu: TPopupMenu;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    Tree: TTreeView;
    PABehaviours: TPanel;
    PMBehavioursToolBar: TPopupMenu;
    PMEffectsToolBar: TPopupMenu;
    BehavioursPopupMenu: TPopupMenu;
    ToolBarBehaviours: TToolBar;
    ToolBarEffects: TToolBar;
    GalleryListView: TListView;
    BehavioursListView: TListView;
    EffectsListView: TListView;
    ACLoadScene: TAction;
    ACSaveScene: TAction;
    ACStayOnTop: TAction;
    ACAddObject: TAction;
    ACAddBehaviour: TAction;
    ACAddEffect: TAction;
    ACMoveUp: TAction;
    ACMoveDown: TAction;
    ACExpand: TAction;
    ACDeleteObject: TAction;
    ACDeleteBehaviour: TAction;
    ACCut: TAction;
    ACCopy: TAction;
    ACPaste: TAction;
    ACInfo: TAction;
    TBLoadScene: TSpeedButton;
    ImLoadScene: TImage;
    TBInfo: TSpeedButton;
    ImInfo: TImage;
    TBPaste: TSpeedButton;
    ImPaste: TImage;
    TBCopy: TSpeedButton;
    ImCopy: TImage;
    TBCut: TSpeedButton;
    ImCut: TImage;
    TBDeleteObject: TSpeedButton;
    ImDeleteObject: TImage;
    TBExpand: TSpeedButton;
    ImExpand: TImage;
    TBMoveDown: TSpeedButton;
    ImMoveDown: TImage;
    TBMoveUp: TSpeedButton;
    ImMoveUp: TImage;
    TBCharacterPanels: TSpeedButton;
    ImCharacterPanels: TImage;
    TBGalleryPanel: TSpeedButton;
    ImGalleryPanel: TImage;
    TBAddObjects: TSpeedButton;
    ImAddObjects: TImage;
    TBStayOnTop: TSpeedButton;
    ImStayOnTop: TImage;
    TBSaveScene: TSpeedButton;
    ImSaveScene: TImage;
    TBSeparator1: TSpeedButton;
    TBSeparator2: TSpeedButton;
    TBSeparator3: TSpeedButton;
    TBSeparator4: TSpeedButton;
    ACGallery: TAction;
    ImArrowDown: TImage;
    ImArrowDownBeh: TImage;
    ImArrowDownEff: TImage;
    TBAddBehaviours: TSpeedButton;
    TBAddEffects: TSpeedButton;
    MIAddObject: TMenuItem;
    MIAddBehaviour: TMenuItem;
    MIAddEffect: TMenuItem;
    MICut: TMenuItem;
    MICopy: TMenuItem;
    MIPaste: TMenuItem;
    MIDelObject: TMenuItem;
    MIMoveUp: TMenuItem;
    MIMoveDown: TMenuItem;
    StyleBook: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ACInfoExecute(Sender: TObject);
  end;

function SceneEditorForm: TSceneEditorForm;
procedure ReleaseSceneEditorForm;

//==================================================================
implementation
//==================================================================

{$R *.fmx}

const
  cRegistryKey = 'Software\SceneEngine\SceneEditor';

var
  vSceneEditorForm: TSceneEditorForm;

function SceneEditorForm: TSceneEditorForm;
begin
  if not Assigned(vSceneEditorForm) then
    vSceneEditorForm := TSceneEditorForm.Create(nil);
  Result := vSceneEditorForm;
end;

procedure ReleaseSceneEditorForm;
begin
  if Assigned(vSceneEditorForm) then
  begin
    vSceneEditorForm.Free;
    vSceneEditorForm := nil;
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

procedure TSceneEditorForm.FormCreate(Sender: TObject);
var
  CurrentNode: TTreeNode;
  reg: TRegistry;
begin
  RegisterBaseSceneObjectNameChangeEvent(OnBaseSceneObjectNameChanged);
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

procedure TSceneEditorForm.FormDestroy(Sender: TObject);
var
  reg: TRegistry;
begin
  DeRegisterBaseSceneObjectNameChangeEvent(OnBaseSceneObjectNameChanged);

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

procedure TSceneEditorForm.ACInfoExecute(Sender: TObject);
var
  AScene: TgxSceneViewer;
begin
  AScene := TgxSceneViewer.Create(Self);
  AScene.Name := 'SceneEditor';
  AScene.Width := 0;
  AScene.Height := 0;
  AScene.parent := Self;
  try
    AScene.Buffer.ShowInfo;
  finally
    AScene.Free;
  end;
end;

end.
