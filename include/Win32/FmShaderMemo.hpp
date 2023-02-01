// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FmShaderMemo.pas' rev: 35.00 (Windows)

#ifndef FmshadermemoHPP
#define FmshadermemoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#include <System.Classes.hpp>
#include <System.Win.Registry.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.ToolWin.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.Memo.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmshadermemo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TShaderMemoForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TShaderMemoForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Controls::TImageList* ImageList;
	Vcl::Comctrls::TToolBar* ToolBar;
	Vcl::Comctrls::TToolButton* TBOpen;
	Vcl::Comctrls::TToolButton* TBSave;
	Vcl::Comctrls::TToolButton* TBStayOnTop;
	Vcl::Comctrls::TToolButton* TBHelp;
	Vcl::Comctrls::TToolButton* ToolButton2;
	Vcl::Comctrls::TToolButton* TBCopy;
	Vcl::Comctrls::TToolButton* TBPaste;
	Vcl::Comctrls::TToolButton* TBCut;
	Vcl::Comctrls::TToolButton* ToolButton10;
	Vcl::Comctrls::TToolButton* TBTemplate;
	Vcl::Comctrls::TToolButton* TBUndo;
	Vcl::Comctrls::TToolButton* TBRedo;
	Vcl::Comctrls::TToolButton* ToolButton4;
	Gls::Memo::TGLSSynHiMemo* GLSLMemo;
	Vcl::Dialogs::TOpenDialog* OpenDialog;
	Vcl::Dialogs::TSaveDialog* SaveDialog;
	Vcl::Menus::TPopupMenu* TemplateMenu;
	Vcl::Menus::TMenuItem* GLSL120;
	Vcl::Menus::TMenuItem* GLSL330;
	Vcl::Menus::TMenuItem* GLSL400;
	Vcl::Menus::TMenuItem* N1;
	Vcl::Menus::TMenuItem* N2;
	Vcl::Stdctrls::TMemo* CompilatorLog;
	Vcl::Comctrls::TToolButton* TBIncIndent;
	Vcl::Comctrls::TToolButton* TBDecIndent;
	Vcl::Comctrls::TToolButton* TBComment;
	Vcl::Comctrls::TToolButton* TBUncoment;
	Vcl::Comctrls::TToolButton* ToolButton1;
	Vcl::Extctrls::TPanel* Panel1;
	Vcl::Stdctrls::TButton* CancelButton;
	Vcl::Stdctrls::TButton* OKButton;
	Vcl::Stdctrls::TButton* CheckButton;
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall GLSLMemoGutterClick(System::TObject* Sender, int LineNo);
	void __fastcall GLSLMemoGutterDraw(System::TObject* Sender, Vcl::Graphics::TCanvas* ACanvas, int LineNo, const System::Types::TRect &rct);
	void __fastcall TBOpenClick(System::TObject* Sender);
	void __fastcall TBSaveClick(System::TObject* Sender);
	void __fastcall TBStayOnTopClick(System::TObject* Sender);
	void __fastcall TBUndoClick(System::TObject* Sender);
	void __fastcall GLSLMemoUndoChange(System::TObject* Sender, bool CanUndo, bool CanRedo);
	void __fastcall TBRedoClick(System::TObject* Sender);
	void __fastcall TBCopyClick(System::TObject* Sender);
	void __fastcall TBPasteClick(System::TObject* Sender);
	void __fastcall TBCutClick(System::TObject* Sender);
	void __fastcall CheckButtonClick(System::TObject* Sender);
	void __fastcall TBIncIndentClick(System::TObject* Sender);
	void __fastcall TBDecIndentClick(System::TObject* Sender);
	void __fastcall TBCommentClick(System::TObject* Sender);
	void __fastcall TBUncomentClick(System::TObject* Sender);
	void __fastcall FormShow(System::TObject* Sender);
	
private:
	int FLightLineStyle;
	System::Classes::TNotifyEvent FOnCheck;
	void __fastcall OnTemplateClick(System::TObject* Sender);
	
public:
	__property System::Classes::TNotifyEvent OnCheck = {read=FOnCheck, write=FOnCheck};
public:
	/* TCustomForm.Create */ inline __fastcall virtual TShaderMemoForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TShaderMemoForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TShaderMemoForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TShaderMemoForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TShaderMemoForm* __fastcall GLShaderEditorForm(void);
extern DELPHI_PACKAGE void __fastcall ReleaseGLShaderEditor(void);
}	/* namespace Fmshadermemo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMSHADERMEMO)
using namespace Fmshadermemo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FmshadermemoHPP
