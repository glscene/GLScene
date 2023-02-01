// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.WindowsFont.pas' rev: 35.00 (Windows)

#ifndef Gls_WindowsfontHPP
#define Gls_WindowsfontHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Texture.hpp>
#include <GLS.BitmapFont.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.Utils.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Windowsfont
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLWindowsBitmapFont;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLWindowsBitmapFont : public Gls::Bitmapfont::TGLCustomBitmapFont
{
	typedef Gls::Bitmapfont::TGLCustomBitmapFont inherited;
	
private:
	Vcl::Graphics::TFont* FFont;
	void __fastcall SetList(Gls::Vectorlists::TGLIntegerList* const AList);
	
protected:
	void __fastcall SetFont(Vcl::Graphics::TFont* value);
	virtual void __fastcall LoadWindowsFont();
	bool __fastcall StoreRanges();
	virtual void __fastcall PrepareImage(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	virtual int __fastcall TextureFormat();
	void __fastcall StreamlineRanges();
	
public:
	__fastcall virtual TGLWindowsBitmapFont(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLWindowsBitmapFont();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	int __fastcall FontTextureWidth();
	int __fastcall FontTextureHeight();
	void __fastcall EnsureString(const System::UnicodeString s)/* overload */;
	void __fastcall EnsureChars(const System::WideChar AStart, const System::WideChar AEnd);
	__property Glyphs;
	
__published:
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property HSpace = {default=1};
	__property VSpace = {default=1};
	__property MagFilter = {default=1};
	__property MinFilter = {default=1};
	__property Ranges = {stored=StoreRanges};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Windowsfont */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_WINDOWSFONT)
using namespace Gls::Windowsfont;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_WindowsfontHPP
