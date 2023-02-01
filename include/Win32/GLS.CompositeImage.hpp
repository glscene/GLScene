// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.CompositeImage.pas' rev: 35.00 (Windows)

#ifndef Gls_CompositeimageHPP
#define Gls_CompositeimageHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <GLS.Context.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.Texture.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Compositeimage
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCompositeImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCompositeImage : public Gls::Texture::TGLTextureImage
{
	typedef Gls::Texture::TGLTextureImage inherited;
	
private:
	Gls::Graphics::TGLImage* FBitmap;
	int FWidth;
	int FHeight;
	int FDepth;
	
protected:
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(int val);
	void __fastcall SetDepth(int val);
	virtual int __fastcall GetWidth();
	virtual int __fastcall GetHeight();
	virtual int __fastcall GetDepth();
	virtual Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	
public:
	__fastcall virtual TGLCompositeImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLCompositeImage();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Gls::Graphics::TGLImage* __fastcall GetBitmap32();
	virtual void __fastcall ReleaseBitmap32();
	virtual void __fastcall SaveToFile(const System::UnicodeString fileName);
	virtual void __fastcall LoadFromFile(const System::UnicodeString fileName);
	void __fastcall LoadFromStream(System::Classes::TStream* const AStream);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property NativeTextureTarget;
	
__published:
	__property int Width = {read=GetWidth, write=SetWidth, nodefault};
	__property int Height = {read=GetHeight, write=SetHeight, nodefault};
	__property int Depth = {read=GetDepth, write=SetDepth, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Compositeimage */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_COMPOSITEIMAGE)
using namespace Gls::Compositeimage;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_CompositeimageHPP
