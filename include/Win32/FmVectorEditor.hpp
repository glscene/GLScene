// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FmVectorEditor.pas' rev: 35.00 (Windows)

#ifndef FmvectoreditorHPP
#define FmvectoreditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ToolWin.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Utils.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmvectoreditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLVectorEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLVectorEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TEdit* EDx;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TEdit* EDy;
	Vcl::Stdctrls::TEdit* EDz;
	Vcl::Buttons::TBitBtn* BBok;
	Vcl::Buttons::TBitBtn* BBcancel;
	Vcl::Extctrls::TImage* IMx;
	Vcl::Extctrls::TImage* IMy;
	Vcl::Extctrls::TImage* IMz;
	Vcl::Buttons::TSpeedButton* SpeedButton1;
	Vcl::Buttons::TSpeedButton* SBmX;
	Vcl::Buttons::TSpeedButton* SpeedButton3;
	Vcl::Buttons::TSpeedButton* SBmY;
	Vcl::Buttons::TSpeedButton* SpeedButton5;
	Vcl::Buttons::TSpeedButton* SBmZ;
	Vcl::Buttons::TSpeedButton* SpeedButton7;
	Vcl::Buttons::TSpeedButton* SBUnit;
	Vcl::Buttons::TSpeedButton* SpeedButton9;
	Vcl::Extctrls::TBevel* Bevel1;
	Vcl::Buttons::TSpeedButton* SBInvert;
	void __fastcall TBxClick(System::TObject* Sender);
	void __fastcall TByClick(System::TObject* Sender);
	void __fastcall TBzClick(System::TObject* Sender);
	void __fastcall TBnullClick(System::TObject* Sender);
	void __fastcall EDxChange(System::TObject* Sender);
	void __fastcall EDyChange(System::TObject* Sender);
	void __fastcall EDzChange(System::TObject* Sender);
	void __fastcall SBmXClick(System::TObject* Sender);
	void __fastcall SBmYClick(System::TObject* Sender);
	void __fastcall SBmZClick(System::TObject* Sender);
	void __fastcall SBUnitClick(System::TObject* Sender);
	void __fastcall SpeedButton9Click(System::TObject* Sender);
	void __fastcall SBInvertClick(System::TObject* Sender);
	
private:
	float vx;
	float vy;
	float vz;
	void __fastcall TestInput(Vcl::Stdctrls::TEdit* edit, Vcl::Extctrls::TImage* imError, float &dest);
	
public:
	bool __fastcall Execute(float &x, float &y, float &z);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLVectorEditorForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLVectorEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLVectorEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLVectorEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLVectorEditorForm* __fastcall GLVectorEditorForm(void);
extern DELPHI_PACKAGE void __fastcall ReleaseVectorEditorForm(void);
}	/* namespace Fmvectoreditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMVECTOREDITOR)
using namespace Fmvectoreditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FmvectoreditorHPP
