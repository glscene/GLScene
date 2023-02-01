// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FRColorEditor.pas' rev: 35.00 (Windows)

#ifndef FrcoloreditorHPP
#define FrcoloreditorHPP

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
#include <System.Types.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Color.hpp>
#include <GLS.Texture.hpp>
#include <GLS.VectorTypes.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Frcoloreditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TRColorEditor;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TRColorEditor : public Vcl::Forms::TFrame
{
	typedef Vcl::Forms::TFrame inherited;
	
	
private:
	enum DECLSPEC_DENUM _TRColorEditor__1 : unsigned char { None, Red, Green, Blue, Alpha };
	
	
__published:
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Extctrls::TPanel* PAPreview;
	Vcl::Dialogs::TColorDialog* ColorDialog;
	Vcl::Extctrls::TPanel* Panel1;
	Vcl::Extctrls::TPaintBox* ColorEditorPaintBox;
	Vcl::Stdctrls::TEdit* RedEdit;
	Vcl::Stdctrls::TEdit* GreenEdit;
	Vcl::Stdctrls::TEdit* BlueEdit;
	Vcl::Stdctrls::TEdit* AlphaEdit;
	void __fastcall TBEChange(System::TObject* Sender);
	void __fastcall PAPreviewDblClick(System::TObject* Sender);
	void __fastcall ColorEditorPaintBoxPaint(System::TObject* Sender);
	void __fastcall FrameResize(System::TObject* Sender);
	void __fastcall ColorEditorPaintBoxMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall ColorEditorPaintBoxMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall ColorEditorPaintBoxMouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall RedEditChange(System::TObject* Sender);
	void __fastcall GreenEditChange(System::TObject* Sender);
	void __fastcall BlueEditChange(System::TObject* Sender);
	void __fastcall AlphaEditChange(System::TObject* Sender);
	
private:
	System::Classes::TNotifyEvent FOnChange;
	bool updating;
	Vcl::Graphics::TBitmap* WorkBitmap;
	int RedValue;
	int GreenValue;
	int BlueValue;
	int AlphaVAlue;
	_TRColorEditor__1 DraggingValue;
	HIDESBASE void __fastcall SetColor(const Gls::Vectortypes::TVector4f &val);
	Gls::Vectortypes::TVector4f __fastcall GetColor();
	void __fastcall DrawContents();
	void __fastcall DragColorSliderToPosition(int XPos);
	void __fastcall ContentsChanged();
	
public:
	__fastcall virtual TRColorEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TRColorEditor();
	__property Gls::Vectortypes::TVector4f Color = {read=GetColor, write=SetColor};
	
__published:
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	/* TWinControl.CreateParented */ inline __fastcall TRColorEditor(HWND ParentWindow) : Vcl::Forms::TFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Frcoloreditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRCOLOREDITOR)
using namespace Frcoloreditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrcoloreditorHPP
