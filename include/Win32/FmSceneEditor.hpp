// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FmSceneEditor.pas' rev: 35.00 (Windows)

#ifndef FmsceneeditorHPP
#define FmsceneeditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Win.Registry.hpp>
#include <System.ImageList.hpp>
#include <System.Actions.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.ToolWin.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Clipbrd.hpp>
#include <DesignIntf.hpp>
#include <VCLEditors.hpp>
#include <GLS.Scene.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.Strings.hpp>
#include <FmInfo.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.Utils.hpp>
#include <GLS.SceneRegister.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmsceneeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSceneEditorForm;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TSetSubItemsEvent)(System::TObject* Sender);

class PASCALIMPLEMENTATION TGLSceneEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Menus::TPopupMenu* PopupMenu;
	Vcl::Menus::TMenuItem* MIAddObject;
	Vcl::Menus::TMenuItem* N1;
	Vcl::Menus::TMenuItem* MIDelObject;
	Vcl::Comctrls::TToolBar* ToolBar;
	Vcl::Actnlist::TActionList* ActionList;
	Vcl::Comctrls::TToolButton* TBAddObjects;
	Vcl::Comctrls::TToolButton* TBMoveUp;
	Vcl::Menus::TPopupMenu* pmToolBar;
	Vcl::Comctrls::TToolButton* TBDeleteObject;
	Vcl::Comctrls::TToolButton* TBMoveDown;
	Vcl::Actnlist::TAction* acAddObject;
	Vcl::Controls::TImageList* ImageList;
	Vcl::Actnlist::TAction* acDeleteObject;
	Vcl::Actnlist::TAction* acMoveUp;
	Vcl::Actnlist::TAction* acMoveDown;
	Vcl::Menus::TMenuItem* N2;
	Vcl::Menus::TMenuItem* MIMoveUp;
	Vcl::Menus::TMenuItem* MIMoveDown;
	Vcl::Actnlist::TAction* acSaveScene;
	Vcl::Actnlist::TAction* acLoadScene;
	Vcl::Dialogs::TOpenDialog* OpenDialog;
	Vcl::Dialogs::TSaveDialog* SaveDialog;
	Vcl::Comctrls::TToolButton* TBLoadScene;
	Vcl::Comctrls::TToolButton* TBSaveScene;
	Vcl::Actnlist::TAction* acInfo;
	Vcl::Actnlist::TAction* acCopy;
	Vcl::Actnlist::TAction* acCut;
	Vcl::Actnlist::TAction* acPaste;
	Vcl::Menus::TMenuItem* MICopy;
	Vcl::Menus::TMenuItem* MIPaste;
	Vcl::Menus::TMenuItem* MICut;
	Vcl::Comctrls::TToolButton* TBCut;
	Vcl::Comctrls::TToolButton* TBCopy;
	Vcl::Comctrls::TToolButton* TBPaste;
	Vcl::Menus::TPopupMenu* pmBehavioursToolbar;
	Vcl::Actnlist::TAction* acAddBehaviour;
	Vcl::Menus::TMenuItem* MIAddBehaviour;
	Vcl::Menus::TMenuItem* MIAddEffect;
	Vcl::Menus::TMenuItem* MIBehaviourSeparator;
	Vcl::Actnlist::TAction* acDeleteBehaviour;
	Vcl::Menus::TPopupMenu* pmBehaviours;
	Vcl::Menus::TMenuItem* Delete1;
	Vcl::Menus::TMenuItem* MoveUp1;
	Vcl::Menus::TMenuItem* MoveDown1;
	Vcl::Menus::TMenuItem* N4;
	Vcl::Menus::TPopupMenu* pmEffectsToolbar;
	Vcl::Actnlist::TAction* acAddEffect;
	Vcl::Comctrls::TToolButton* TBCharacterPanels;
	Vcl::Comctrls::TToolButton* TBStayOnTop;
	Vcl::Actnlist::TAction* acStayOnTop;
	Vcl::Comctrls::TToolButton* TBSeparator4;
	Vcl::Comctrls::TToolButton* TBExpand;
	Vcl::Actnlist::TAction* acExpand;
	Vcl::Extctrls::TPanel* PATreeAll;
	Vcl::Extctrls::TPanel* PATree;
	Vcl::Comctrls::TTreeView* Tree;
	Vcl::Comctrls::TToolButton* TBInfo;
	Vcl::Extctrls::TPanel* PABehaviours;
	Vcl::Comctrls::TToolBar* ToolBarBehaviours;
	Vcl::Comctrls::TToolButton* TBAddBehaviours;
	Vcl::Comctrls::TListView* BehavioursListView;
	Vcl::Extctrls::TPanel* PAEffects;
	Vcl::Comctrls::TListView* EffectsListView;
	Vcl::Comctrls::TToolBar* ToolBarEffects;
	Vcl::Comctrls::TToolButton* TBAddEffects;
	Vcl::Comctrls::TToolButton* TBGalleryPanel;
	Vcl::Comctrls::TTreeView* TreeAll;
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall TreeEditing(System::TObject* Sender, Vcl::Comctrls::TTreeNode* Node, bool &AllowEdit);
	void __fastcall TreeDragOver(System::TObject* Sender, System::TObject* Source, int X, int Y, System::Uitypes::TDragState State, bool &Accept);
	void __fastcall TreeDragDrop(System::TObject* Sender, System::TObject* Source, int X, int Y);
	void __fastcall TreeAllChange(System::TObject* Sender, Vcl::Comctrls::TTreeNode* Node);
	void __fastcall TreeChange(System::TObject* Sender, Vcl::Comctrls::TTreeNode* Node);
	void __fastcall TreeMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall TreeEnter(System::TObject* Sender);
	void __fastcall TreeMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall acDeleteObjectExecute(System::TObject* Sender);
	void __fastcall acMoveUpExecute(System::TObject* Sender);
	void __fastcall acMoveDownExecute(System::TObject* Sender);
	void __fastcall acAddObjectExecute(System::TObject* Sender);
	void __fastcall acSaveSceneExecute(System::TObject* Sender);
	void __fastcall acLoadSceneExecute(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall acInfoExecute(System::TObject* Sender);
	void __fastcall acCopyExecute(System::TObject* Sender);
	void __fastcall acCutExecute(System::TObject* Sender);
	void __fastcall acPasteExecute(System::TObject* Sender);
	void __fastcall BehavioursListViewEnter(System::TObject* Sender);
	void __fastcall EffectsListViewEnter(System::TObject* Sender);
	void __fastcall acAddBehaviourExecute(System::TObject* Sender);
	void __fastcall DeleteBaseBehaviour(Vcl::Comctrls::TListView* ListView);
	void __fastcall pmBehavioursToolbarPopup(System::TObject* Sender);
	void __fastcall pmEffectsToolbarPopup(System::TObject* Sender);
	void __fastcall BehavioursListViewSelectItem(System::TObject* Sender, Vcl::Comctrls::TListItem* Item, bool Selected);
	void __fastcall acAddEffectExecute(System::TObject* Sender);
	void __fastcall PopupMenuPopup(System::TObject* Sender);
	void __fastcall TBCharacterPanelsClick(System::TObject* Sender);
	void __fastcall TreeKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall acStayOnTopExecute(System::TObject* Sender);
	void __fastcall FormKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall acExpandExecute(System::TObject* Sender);
	
private:
	int FSelectedItems;
	Gls::Scene::TGLScene* FScene;
	Vcl::Comctrls::TTreeNode* FObjectNode;
	Vcl::Comctrls::TTreeNode* FSceneObjects;
	Designintf::_di_IDesigner FCurrentDesigner;
	System::Types::TPoint FLastMouseDownPos;
	System::Classes::TComponent* FPasteOwner;
	Designintf::_di_IDesignerSelections FPasteSelection;
	void __fastcall ReadScene();
	void __fastcall ResetTree();
	Vcl::Comctrls::TTreeNode* __fastcall AddNodesAll(Vcl::Comctrls::TTreeNode* ANode, Gls::Scene::TGLBaseSceneObject* AObject);
	Vcl::Comctrls::TTreeNode* __fastcall AddNodes(Vcl::Comctrls::TTreeNode* ANode, Gls::Scene::TGLBaseSceneObject* AObject);
	void __fastcall AddObjectClick(System::TObject* Sender);
	void __fastcall AddBehaviourClick(System::TObject* Sender);
	void __fastcall AddEffectClick(System::TObject* Sender);
	void __fastcall SetObjectsSubItems(Vcl::Menus::TMenuItem* parent);
	void __fastcall SetXCollectionSubItems(Vcl::Menus::TMenuItem* parent, Gls::Xcollection::TXCollection* XCollection, TSetSubItemsEvent Event);
	void __fastcall SetBehavioursSubItems(Vcl::Menus::TMenuItem* parent, Gls::Xcollection::TXCollection* XCollection);
	void __fastcall SetEffectsSubItems(Vcl::Menus::TMenuItem* parent, Gls::Xcollection::TXCollection* XCollection);
	void __fastcall OnBaseSceneObjectNameChanged(System::TObject* Sender);
	bool __fastcall IsValidClipBoardNode();
	bool __fastcall IsPastePossible();
	void __fastcall ShowBehaviours(Gls::Scene::TGLBaseSceneObject* BaseSceneObject);
	void __fastcall ShowEffects(Gls::Scene::TGLBaseSceneObject* BaseSceneObject);
	void __fastcall ShowBehavioursAndEffects(Gls::Scene::TGLBaseSceneObject* BaseSceneObject);
	void __fastcall EnableAndDisableActions();
	bool __fastcall CanPaste(Gls::Scene::TGLBaseSceneObject* obj, Gls::Scene::TGLBaseSceneObject* destination);
	void __fastcall CopyComponents(System::Classes::TComponent* Root, const Designintf::_di_IDesignerSelections Components);
	void __fastcall MethodError(System::Classes::TReader* Reader, const System::UnicodeString MethodName, void * &Address, bool &Error);
	bool __fastcall PasteComponents(System::Classes::TComponent* AOwner, System::Classes::TComponent* AParent, const Designintf::_di_IDesignerSelections Components);
	void __fastcall ReaderSetName(System::Classes::TReader* Reader, System::Classes::TComponent* Component, System::UnicodeString &Name);
	void __fastcall ComponentRead(System::Classes::TComponent* Component);
	System::UnicodeString __fastcall UniqueName(System::Classes::TComponent* Component);
	void __fastcall TreeEdited(System::TObject* Sender, Vcl::Comctrls::TTreeNode* Node, System::UnicodeString &S);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	void __fastcall SetScene(Gls::Scene::TGLScene* Scene, Designintf::_di_IDesigner Designer);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLSceneEditorForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLSceneEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLSceneEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLSceneEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 SCENE_SELECTED = System::Int8(0x0);
static const System::Int8 BEHAVIOURS_SELECTED = System::Int8(0x1);
static const System::Int8 EFFECTS_SELECTED = System::Int8(0x2);
extern DELPHI_PACKAGE TGLSceneEditorForm* __fastcall GLSceneEditorForm(void);
extern DELPHI_PACKAGE void __fastcall ReleaseGLSceneEditorForm(void);
}	/* namespace Fmsceneeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMSCENEEDITOR)
using namespace Fmsceneeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FmsceneeditorHPP
