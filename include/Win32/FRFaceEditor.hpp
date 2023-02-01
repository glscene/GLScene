// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FRFaceEditor.pas' rev: 35.00 (Windows)

#ifndef FrfaceeditorHPP
#define FrfaceeditorHPP

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
#include <System.ImageList.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <FRTrackBarEdit.hpp>
#include <FRColorEditor.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Material.hpp>
#include <GLS.State.hpp>

//-- user supplied -----------------------------------------------------------

namespace Frfaceeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TRFaceEditor;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TRFaceEditor : public Vcl::Forms::TFrame
{
	typedef Vcl::Forms::TFrame inherited;
	
__published:
	Vcl::Comctrls::TPageControl* PageControl;
	Vcl::Comctrls::TTabSheet* TSAmbient;
	Vcl::Comctrls::TTabSheet* TSDiffuse;
	Vcl::Comctrls::TTabSheet* TSEmission;
	Vcl::Comctrls::TTabSheet* TSSpecular;
	Frcoloreditor::TRColorEditor* CEAmbiant;
	Vcl::Stdctrls::TLabel* Label1;
	Frtrackbaredit::TRTrackBarEdit* TBEShininess;
	Vcl::Controls::TImageList* ImageList;
	Frcoloreditor::TRColorEditor* CEDiffuse;
	Frcoloreditor::TRColorEditor* CEEmission;
	Frcoloreditor::TRColorEditor* CESpecular;
	void __fastcall TBEShininessTrackBarChange(System::TObject* Sender);
	
private:
	System::Classes::TNotifyEvent FOnChange;
	bool Updating;
	Gls::Material::TGLFaceProperties* FFaceProperties;
	void __fastcall SetGLFaceProperties(Gls::Material::TGLFaceProperties* const val);
	void __fastcall OnColorChange(System::TObject* Sender);
	
public:
	__fastcall virtual TRFaceEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TRFaceEditor();
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property Gls::Material::TGLFaceProperties* FaceProperties = {read=FFaceProperties, write=SetGLFaceProperties};
public:
	/* TWinControl.CreateParented */ inline __fastcall TRFaceEditor(HWND ParentWindow) : Vcl::Forms::TFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Frfaceeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRFACEEDITOR)
using namespace Frfaceeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrfaceeditorHPP
