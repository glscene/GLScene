// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FileOCT.pas' rev: 35.00 (Windows)

#ifndef Gls_FileoctHPP
#define Gls_FileoctHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Material.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.State.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <Formats.OCT.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Fileoct
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLOCTGLVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLOCTGLVectorFile : public Gls::Vectorfileobjects::TGLVectorFile
{
	typedef Gls::Vectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Gls::Applicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLOCTGLVectorFile(System::Classes::TPersistent* AOwner) : Gls::Vectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLOCTGLVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE float vGLFileOCTLightmapBrightness;
extern DELPHI_PACKAGE float vGLFileOCTLightmapGammaCorrection;
extern DELPHI_PACKAGE bool vGLFileOCTAllocateMaterials;
}	/* namespace Fileoct */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILEOCT)
using namespace Gls::Fileoct;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FileoctHPP
