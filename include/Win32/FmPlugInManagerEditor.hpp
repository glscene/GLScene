// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FmPlugInManagerEditor.pas' rev: 35.00 (Windows)

#ifndef FmpluginmanagereditorHPP
#define FmpluginmanagereditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.ImageList.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ToolWin.hpp>
#include <GLS.PlugInManager.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmpluginmanagereditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLPlugInManagerEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLPlugInManagerEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Dialogs::TOpenDialog* OpenDialog;
	Vcl::Stdctrls::TListBox* ListBox;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TGroupBox* GroupBox;
	Vcl::Stdctrls::TMemo* DescriptionMemo;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* DateLabel;
	Vcl::Stdctrls::TLabel* SizeLabel;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TComboBox* ServiceBox;
	Vcl::Stdctrls::TComboBox* NameBox;
	Vcl::Comctrls::TToolBar* ToolBar1;
	Vcl::Comctrls::TToolButton* ToolButton1;
	Vcl::Comctrls::TToolButton* ToolButton2;
	Vcl::Comctrls::TToolButton* ToolButton3;
	Vcl::Controls::TImageList* ImageList;
	void __fastcall OKButtonClick(System::TObject* Sender);
	void __fastcall LoadButtonClick(System::TObject* Sender);
	void __fastcall ListBoxClick(System::TObject* Sender);
	void __fastcall UnloadButtonClick(System::TObject* Sender);
	void __fastcall ServiceBoxChange(System::TObject* Sender);
	
private:
	Gls::Pluginmanager::TGLPlugInManager* FManager;
	
public:
	__classmethod void __fastcall EditPlugIns(Gls::Pluginmanager::TGLPlugInManager* AManager);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLPlugInManagerEditorForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLPlugInManagerEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLPlugInManagerEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLPlugInManagerEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Fmpluginmanagereditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMPLUGINMANAGEREDITOR)
using namespace Fmpluginmanagereditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FmpluginmanagereditorHPP
