// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FmGuiLayoutEditor.pas' rev: 35.00 (Windows)

#ifndef FmguilayouteditorHPP
#define FmguilayouteditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ShellAPI.hpp>
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtDlgs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Samples.Spin.hpp>
#include <Vcl.Grids.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Gui.hpp>
#include <GLS.Strings.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmguilayouteditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLLayoutEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLLayoutEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Extctrls::TPanel* Panel1;
	Vcl::Extctrls::TPanel* Panel2;
	Vcl::Stdctrls::TListBox* items_list;
	Vcl::Stdctrls::TLabel* x_label;
	Vcl::Stdctrls::TLabel* y_label;
	Vcl::Buttons::TBitBtn* open_image_button;
	Vcl::Buttons::TBitBtn* open_button;
	Vcl::Buttons::TBitBtn* save_button;
	Vcl::Dialogs::TOpenDialog* OpenDialog1;
	Vcl::Dialogs::TSaveDialog* SaveDialog1;
	Vcl::Buttons::TBitBtn* delete_item_button;
	Vcl::Buttons::TBitBtn* add_button;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Samples::Spin::TSpinEdit* left_edit;
	Vcl::Samples::Spin::TSpinEdit* top_edit;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Samples::Spin::TSpinEdit* height_edit;
	Vcl::Samples::Spin::TSpinEdit* width_edit;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TEdit* name_edit;
	Vcl::Grids::TStringGrid* elements_grid;
	Vcl::Extctrls::TPanel* Panel3;
	Vcl::Buttons::TBitBtn* BitBtn4;
	Vcl::Buttons::TBitBtn* BitBtn5;
	Vcl::Forms::TScrollBox* ScrollBox1;
	Vcl::Extctrls::TImage* Image2;
	Vcl::Extctrls::TPaintBox* PaintBox1;
	Vcl::Extctrls::TImage* Image1;
	Vcl::Buttons::TBitBtn* BitBtn6;
	Vcl::Buttons::TBitBtn* BitBtn1;
	Gls::Gui::TGLGuiLayout* GLGuiLayout1;
	void __fastcall open_image_buttonClick(System::TObject* Sender);
	void __fastcall open_buttonClick(System::TObject* Sender);
	void __fastcall save_buttonClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall Image1MouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall Image1MouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall Image1MouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall add_buttonClick(System::TObject* Sender);
	void __fastcall delete_item_buttonClick(System::TObject* Sender);
	void __fastcall items_listClick(System::TObject* Sender);
	void __fastcall name_editExit(System::TObject* Sender);
	void __fastcall name_editKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall elements_gridClick(System::TObject* Sender);
	void __fastcall left_editChange(System::TObject* Sender);
	void __fastcall top_editChange(System::TObject* Sender);
	void __fastcall width_editChange(System::TObject* Sender);
	void __fastcall height_editChange(System::TObject* Sender);
	void __fastcall BitBtn4Click(System::TObject* Sender);
	void __fastcall BitBtn6Click(System::TObject* Sender);
	void __fastcall elements_gridDblClick(System::TObject* Sender);
	
private:
	void __fastcall SyncImages();
	void __fastcall DrawCurrentElement();
	void __fastcall RefreshComponentBox();
	bool __fastcall GetEnabledSpins();
	void __fastcall SetEnabledSpins(bool Value);
	
public:
	void __fastcall Execute(Gls::Gui::TGLGuiLayout* AGUILayout);
	__property bool EnabledSpins = {read=GetEnabledSpins, write=SetEnabledSpins, nodefault};
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLLayoutEditorForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLLayoutEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLLayoutEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLLayoutEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLLayoutEditorForm* __fastcall GUILayoutEditorForm(void);
extern DELPHI_PACKAGE void __fastcall ReleaseGUILayoutEditorForm(void);
}	/* namespace Fmguilayouteditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMGUILAYOUTEDITOR)
using namespace Fmguilayouteditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FmguilayouteditorHPP
