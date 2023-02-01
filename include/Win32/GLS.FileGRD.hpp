// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FileGRD.pas' rev: 35.00 (Windows)

#ifndef Gls_FilegrdHPP
#define Gls_FilegrdHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.Graph.hpp>
#include <GLS.Utils.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Filegrd
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLGRDVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLGRDVectorFile : public Gls::Vectorfileobjects::TGLVectorFile
{
	typedef Gls::Vectorfileobjects::TGLVectorFile inherited;
	
	
private:
	typedef System::DynamicArray<Gls::Vectorgeometry::TSingleArray> _TGLGRDVectorFile__1;
	
	
public:
	Gls::Graph::TGLHeightField* GLHeightField;
	_TGLGRDVectorFile__1 Nodes;
	__classmethod virtual Gls::Applicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	
private:
	System::UnicodeString StrVal;
	System::UnicodeString StrLine;
	float MaxZ;
	System::UnicodeString __fastcall ExtractWord(int N, const System::UnicodeString S, const System::Sysutils::TSysCharSet &WordDelims);
	int __fastcall WordPosition(const int N, const System::UnicodeString S, const System::Sysutils::TSysCharSet &WordDelims);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLGRDVectorFile(System::Classes::TPersistent* AOwner) : Gls::Vectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLGRDVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Filegrd */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILEGRD)
using namespace Gls::Filegrd;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FilegrdHPP
