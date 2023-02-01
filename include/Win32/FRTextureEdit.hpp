// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FRTextureEdit.pas' rev: 35.00 (Windows)

#ifndef FrtextureeditHPP
#define FrtextureeditHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.TypInfo.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Controls.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.Texture.hpp>
#include <GLS.State.hpp>
#include <GLS.TextureImageEditors.hpp>

//-- user supplied -----------------------------------------------------------

namespace Frtextureedit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TRTextureEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TRTextureEdit : public Vcl::Forms::TFrame
{
	typedef Vcl::Forms::TFrame inherited;
	
__published:
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Buttons::TSpeedButton* SBEditImage;
	Vcl::Stdctrls::TComboBox* CBMagFilter;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TComboBox* CBMinFilter;
	Vcl::Stdctrls::TComboBox* CBTextureMode;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TComboBox* CBTextureWrap;
	Vcl::Stdctrls::TCheckBox* CBDisabled;
	Vcl::Stdctrls::TComboBox* CBImageClass;
	Vcl::Stdctrls::TComboBox* CBImageAlpha;
	Vcl::Stdctrls::TLabel* Label6;
	Vcl::Stdctrls::TComboBox* CBFilteringQuality;
	Vcl::Stdctrls::TLabel* Label7;
	void __fastcall CBMagFilterChange(System::TObject* Sender);
	void __fastcall CBMinFilterChange(System::TObject* Sender);
	void __fastcall CBTextureModeChange(System::TObject* Sender);
	void __fastcall CBTextureWrapChange(System::TObject* Sender);
	void __fastcall CBDisabledClick(System::TObject* Sender);
	void __fastcall SBEditImageClick(System::TObject* Sender);
	void __fastcall CBImageClassChange(System::TObject* Sender);
	void __fastcall CBImageAlphaChange(System::TObject* Sender);
	void __fastcall CBFilteringQualityChange(System::TObject* Sender);
	
private:
	Gls::Texture::TGLTexture* FTexture;
	System::Classes::TNotifyEvent FOnChange;
	bool Changeing;
	
protected:
	void __fastcall SetTexture(Gls::Texture::TGLTexture* const val);
	virtual void __fastcall DoOnChange();
	
public:
	__fastcall virtual TRTextureEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TRTextureEdit();
	__property Gls::Texture::TGLTexture* Texture = {read=FTexture, write=SetTexture};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	/* TWinControl.CreateParented */ inline __fastcall TRTextureEdit(HWND ParentWindow) : Vcl::Forms::TFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Frtextureedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRTEXTUREEDIT)
using namespace Frtextureedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrtextureeditHPP
