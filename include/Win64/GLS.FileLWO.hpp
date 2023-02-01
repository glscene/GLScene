// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FileLWO.pas' rev: 35.00 (Windows)

#ifndef Gls_FilelwoHPP
#define Gls_FilelwoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.VectorLists.hpp>
#include <Formats.LWO.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Filelwo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLLWOVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLLWOVectorFile : public Gls::Vectorfileobjects::TGLVectorFile
{
	typedef Gls::Vectorfileobjects::TGLVectorFile inherited;
	
private:
	Formats::Lwo::TLWObjectFile* FLWO;
	Formats::Lwo::TLWPnts* FPnts;
	void __fastcall AddLayr(Formats::Lwo::TLWLayr* Layr, Formats::Lwo::TLWObjectFile* LWO);
	void __fastcall AddSurf(Formats::Lwo::TLWSurf* Surf, Formats::Lwo::TLWObjectFile* LWO);
	void __fastcall AddPnts(Formats::Lwo::TLWPnts* Pnts, Gls::Vectorfileobjects::TGLMeshObject* Mesh);
	void __fastcall AddPols(Formats::Lwo::TLWPols* Pols, Gls::Vectorfileobjects::TGLMeshObject* Mesh);
	void __fastcall AddVMap(Formats::Lwo::TLWVMap* VMap, Gls::Vectorfileobjects::TGLMeshObject* Mesh);
	
public:
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLLWOVectorFile(System::Classes::TPersistent* AOwner) : Gls::Vectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLLWOVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Filelwo */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILELWO)
using namespace Gls::Filelwo;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FilelwoHPP
