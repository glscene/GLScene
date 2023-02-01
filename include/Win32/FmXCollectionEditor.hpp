// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FmXCollectionEditor.pas' rev: 35.00 (Windows)

#ifndef FmxcollectioneditorHPP
#define FmxcollectioneditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Actions.hpp>
#include <System.ImageList.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ToolWin.hpp>
#include <Vcl.Dialogs.hpp>
#include <DesignIntf.hpp>
#include <GLS.Strings.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Behaviours.hpp>
#include <GLS.MaterialEx.hpp>
#include <GLS.XCollection.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmxcollectioneditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TXCollectionEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TXCollectionEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Comctrls::TListView* ListView;
	Vcl::Menus::TPopupMenu* PMListView;
	Vcl::Actnlist::TActionList* ActionList;
	Vcl::Actnlist::TAction* ACRemove;
	Vcl::Actnlist::TAction* ACMoveUp;
	Vcl::Actnlist::TAction* ACMoveDown;
	Vcl::Controls::TImageList* ImageList;
	Vcl::Menus::TMenuItem* MIAdd;
	Vcl::Menus::TMenuItem* N1;
	Vcl::Menus::TMenuItem* N2;
	Vcl::Menus::TMenuItem* Moveup1;
	Vcl::Menus::TMenuItem* Movedown1;
	Vcl::Comctrls::TToolBar* ToolBar1;
	Vcl::Comctrls::TToolButton* TBAdd;
	Vcl::Comctrls::TToolButton* ToolButton2;
	Vcl::Comctrls::TToolButton* ToolButton3;
	Vcl::Comctrls::TToolButton* ToolButton4;
	Vcl::Comctrls::TToolButton* ToolButton5;
	Vcl::Comctrls::TToolButton* ToolButton6;
	Vcl::Menus::TPopupMenu* PMToolBar;
	void __fastcall TBAddClick(System::TObject* Sender);
	void __fastcall ListViewChange(System::TObject* Sender, Vcl::Comctrls::TListItem* Item, Vcl::Comctrls::TItemChange Change);
	void __fastcall ACRemoveExecute(System::TObject* Sender);
	void __fastcall ACMoveUpExecute(System::TObject* Sender);
	void __fastcall ACMoveDownExecute(System::TObject* Sender);
	void __fastcall PMToolBarPopup(System::TObject* Sender);
	void __fastcall PMListViewPopup(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall FormHide(System::TObject* Sender);
	
private:
	Gls::Xcollection::TXCollection* FXCollection;
	Designintf::_di_IDesigner FDesigner;
	bool UpdatingListView;
	void __fastcall PrepareListView();
	void __fastcall PrepareXCollectionItemPopup(Vcl::Menus::TMenuItem* parent);
	void __fastcall OnAddXCollectionItemClick(System::TObject* Sender);
	void __fastcall OnNameChanged(System::TObject* Sender);
	void __fastcall OnXCollectionDestroyed(System::TObject* Sender);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	void __fastcall SetXCollection(Gls::Xcollection::TXCollection* aXCollection, Designintf::_di_IDesigner designer);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TXCollectionEditorForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TXCollectionEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TXCollectionEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TXCollectionEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TXCollectionEditorForm* __fastcall XCollectionEditorForm(void);
extern DELPHI_PACKAGE void __fastcall ReleaseXCollectionEditor(void);
}	/* namespace Fmxcollectioneditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMXCOLLECTIONEDITOR)
using namespace Fmxcollectioneditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FmxcollectioneditorHPP
