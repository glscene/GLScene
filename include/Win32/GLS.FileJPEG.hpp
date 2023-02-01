// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FileJPEG.pas' rev: 35.00 (Windows)

#ifndef Gls_FilejpegHPP
#define Gls_FilejpegHPP

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
#include <Vcl.Graphics.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Context.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Filejpeg
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLJPEGImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLJPEGImage : public Gls::Graphics::TGLBaseImage
{
	typedef Gls::Graphics::TGLBaseImage inherited;
	
private:
	bool FAbortLoading;
	unsigned FDivScale;
	bool FDither;
	bool FSmoothing;
	bool FProgressiveEncoding;
	void __fastcall SetSmoothing(const bool AValue);
	
public:
	__fastcall virtual TGLJPEGImage();
	__classmethod virtual Gls::Applicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromFile(const System::UnicodeString filename);
	virtual void __fastcall SaveToFile(const System::UnicodeString filename);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* AStream);
	HIDESBASE void __fastcall AssignFromTexture(Gls::Context::TGLContext* textureContext, const unsigned textureHandle, Gls::Textureformat::TGLTextureTarget textureTarget, const bool CurrentFormat, const Gls::Textureformat::TGLInternalFormat intFormat);
	__property unsigned DivScale = {read=FDivScale, write=FDivScale, nodefault};
	__property bool Dither = {read=FDither, write=FDither, nodefault};
	__property bool Smoothing = {read=FSmoothing, write=SetSmoothing, nodefault};
	__property bool ProgressiveEncoding = {read=FProgressiveEncoding, nodefault};
public:
	/* TGLBaseImage.Destroy */ inline __fastcall virtual ~TGLJPEGImage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Jpeg2Bmp(const System::UnicodeString BmpFileName, const System::UnicodeString JpgFileName);
}	/* namespace Filejpeg */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILEJPEG)
using namespace Gls::Filejpeg;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FilejpegHPP
