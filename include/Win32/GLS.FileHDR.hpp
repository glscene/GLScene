// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FileHDR.pas' rev: 35.00 (Windows)

#ifndef Gls_FilehdrHPP
#define Gls_FilehdrHPP

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
#include <GLS.TextureFormat.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.RGBE.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Filehdr
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLHDRImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLHDRImage : public Gls::Graphics::TGLBaseImage
{
	typedef Gls::Graphics::TGLBaseImage inherited;
	
private:
	System::AnsiString __fastcall GetProgramType();
	void __fastcall SetProgramType(System::AnsiString aval);
	
protected:
	float fGamma;
	float fExposure;
	System::SmallString<16> fProgramType;
	
public:
	__classmethod virtual Gls::Applicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromFile(const System::UnicodeString filename);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* stream);
	HIDESBASE void __fastcall AssignFromTexture(Gls::Context::TGLContext* textureContext, const unsigned textureHandle, Gls::Textureformat::TGLTextureTarget textureTarget, const bool CurrentFormat, const Gls::Textureformat::TGLInternalFormat intFormat);
	__property float Gamma = {read=fGamma};
	__property float Exposure = {read=fExposure};
	__property System::AnsiString ProgramType = {read=GetProgramType, write=SetProgramType};
public:
	/* TGLBaseImage.Create */ inline __fastcall virtual TGLHDRImage() : Gls::Graphics::TGLBaseImage() { }
	/* TGLBaseImage.Destroy */ inline __fastcall virtual ~TGLHDRImage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Filehdr */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILEHDR)
using namespace Gls::Filehdr;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FilehdrHPP
