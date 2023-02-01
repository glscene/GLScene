// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.MultiSampleImage.pas' rev: 35.00 (Windows)

#ifndef Gls_MultisampleimageHPP
#define Gls_MultisampleimageHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.OpenGLext.hpp>
#include <System.Classes.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Context.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Multisampleimage
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMultisampleImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLMultisampleImage : public Gls::Texture::TGLTextureImage
{
	typedef Gls::Texture::TGLTextureImage inherited;
	
private:
	Gls::Graphics::TGLImage* FBitmap;
	int FSamplesCount;
	int FWidth;
	int FHeight;
	int FDepth;
	bool FFixedSamplesLocation;
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(int val);
	void __fastcall SetDepth(int val);
	void __fastcall SetSamplesCount(int val);
	void __fastcall SetFixedSamplesLocation(bool val);
	
protected:
	virtual int __fastcall GetWidth();
	virtual int __fastcall GetHeight();
	virtual int __fastcall GetDepth();
	virtual Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	
public:
	__fastcall virtual TGLMultisampleImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLMultisampleImage();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual bool __fastcall IsSelfLoading();
	virtual void __fastcall LoadTexture(Gls::Textureformat::TGLInternalFormat AInternalFormat);
	virtual Gls::Graphics::TGLImage* __fastcall GetBitmap32();
	virtual void __fastcall ReleaseBitmap32();
	virtual void __fastcall SaveToFile(const System::UnicodeString fileName);
	virtual void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property NativeTextureTarget;
	
__published:
	__property int Width = {read=GetWidth, write=SetWidth, default=256};
	__property int Height = {read=GetHeight, write=SetHeight, default=256};
	__property int Depth = {read=GetDepth, write=SetDepth, default=0};
	__property int SamplesCount = {read=FSamplesCount, write=SetSamplesCount, default=0};
	__property bool FixedSamplesLocation = {read=FFixedSamplesLocation, write=SetFixedSamplesLocation, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Multisampleimage */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_MULTISAMPLEIMAGE)
using namespace Gls::Multisampleimage;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_MultisampleimageHPP
