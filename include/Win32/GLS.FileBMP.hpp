// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FileBMP.pas' rev: 35.00 (Windows)

#ifndef Gls_FilebmpHPP
#define Gls_FilebmpHPP

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
#include <System.SysUtils.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Context.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Filebmp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBMPImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLBMPImage : public Gls::Graphics::TGLBaseImage
{
	typedef Gls::Graphics::TGLBaseImage inherited;
	
private:
	bool FTopDown;
	unsigned RedMask;
	unsigned GreenMask;
	unsigned BlueMask;
	System::Int8 RedShift;
	System::Int8 GreenShift;
	System::Int8 BlueShift;
	System::Sysutils::TByteArray *FLineBuffer;
	int FReadSize;
	int FDeltaX;
	int FDeltaY;
	System::Int8 __fastcall CountBits(System::Byte Value);
	System::Int8 __fastcall ShiftCount(unsigned Mask);
	Gls::Graphics::TGLPixel32 __fastcall ExpandColor(unsigned Value);
	void __fastcall ExpandRLE4ScanLine(int Row, System::Classes::TStream* Stream);
	void __fastcall ExpandRLE8ScanLine(int Row, System::Classes::TStream* Stream);
	int __fastcall Monochrome(int N);
	int __fastcall Quadrochrome(int N);
	int __fastcall Octochrome(int N);
	
public:
	virtual void __fastcall LoadFromFile(const System::UnicodeString filename);
	virtual void __fastcall SaveToFile(const System::UnicodeString filename);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* stream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* stream);
	__classmethod virtual Gls::Applicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	HIDESBASE void __fastcall AssignFromTexture(Gls::Context::TGLContext* textureContext, const unsigned textureHandle, Gls::Textureformat::TGLTextureTarget textureTarget, const bool CurrentFormat, const Gls::Textureformat::TGLInternalFormat intFormat);
public:
	/* TGLBaseImage.Create */ inline __fastcall virtual TGLBMPImage() : Gls::Graphics::TGLBaseImage() { }
	/* TGLBaseImage.Destroy */ inline __fastcall virtual ~TGLBMPImage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Filebmp */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILEBMP)
using namespace Gls::Filebmp;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FilebmpHPP
