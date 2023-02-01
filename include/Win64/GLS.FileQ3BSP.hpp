// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FileQ3BSP.pas' rev: 35.00 (Windows)

#ifndef Gls_Fileq3bspHPP
#define Gls_Fileq3bspHPP

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
#include <GLS.VectorFileObjects.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorLists.hpp>
#include <Formats.Q3BSP.hpp>
#include <GLS.MeshBSP.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.State.hpp>
#include <GLS.Utils.hpp>
#include <GLS.Material.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Fileq3bsp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLQ3BSPVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLQ3BSPVectorFile : public Gls::Vectorfileobjects::TGLVectorFile
{
	typedef Gls::Vectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Gls::Applicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLQ3BSPVectorFile(System::Classes::TPersistent* AOwner) : Gls::Vectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLQ3BSPVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE float vQ3BSPLightmapGammaCorrection;
extern DELPHI_PACKAGE float vQ3BSPLightmapBrightness;
extern DELPHI_PACKAGE bool vGLFileQ3BSPLoadMaterials;
}	/* namespace Fileq3bsp */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILEQ3BSP)
using namespace Gls::Fileq3bsp;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_Fileq3bspHPP
