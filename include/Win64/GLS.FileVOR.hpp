// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FileVOR.pas' rev: 35.00 (Windows)

#ifndef Gls_FilevorHPP
#define Gls_FilevorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.Utils.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Filevor
{
//-- forward type declarations -----------------------------------------------
struct TVORHeader;
struct TVORFace;
class DELPHICLASS TGLVORVectorFile;
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TVORHeader
{
public:
	System::StaticArray<System::Byte, 80> dummy;
	int nbFaces;
};
#pragma pack(pop)


typedef Gls::Vectortypes::TVector3f TVORVertex;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TVORFace
{
public:
	Gls::Vectortypes::TVector3f normal;
	Gls::Vectortypes::TVector3f v1;
	Gls::Vectortypes::TVector3f v2;
	Gls::Vectortypes::TVector3f v3;
	System::StaticArray<System::Byte, 2> padding;
};
#pragma pack(pop)


class PASCALIMPLEMENTATION TGLVORVectorFile : public Gls::Vectorfileobjects::TGLVectorFile
{
	typedef Gls::Vectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Gls::Applicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLVORVectorFile(System::Classes::TPersistent* AOwner) : Gls::Vectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLVORVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool VORUseEmbeddedColors;
}	/* namespace Filevor */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILEVOR)
using namespace Gls::Filevor;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FilevorHPP
