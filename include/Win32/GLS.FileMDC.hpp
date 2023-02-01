// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FileMDC.pas' rev: 35.00 (Windows)

#ifndef Gls_FilemdcHPP
#define Gls_FilemdcHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.Material.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Filemdc
{
//-- forward type declarations -----------------------------------------------
struct TMDCFileHeader;
struct TMDCBorderFrame;
struct TMDCTagName;
struct TMDCTagFrame;
struct TMDCTag;
struct TMDCSurfaceHeader;
struct TMDCSkin;
struct TMDCBaseFrame;
struct TMDCCompFrame;
class DELPHICLASS TGLMDCVectorFile;
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<float, 3> TMDCPoint;

typedef TMDCPoint TMDCAngle;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCFileHeader
{
public:
	System::StaticArray<char, 4> Ident;
	unsigned Version;
	System::StaticArray<char, 64> Name;
	unsigned Flags;
	unsigned NumFrames;
	unsigned NumTags;
	unsigned NumSurfaces;
	unsigned NumSkins;
	unsigned OffsetBorderFrames;
	unsigned OffsetTagNames;
	unsigned OffsetTagFrames;
	unsigned OffsetSurfaces;
	unsigned OffsetEnd;
};
#pragma pack(pop)


struct DECLSPEC_DRECORD TMDCBorderFrame
{
public:
	TMDCPoint BBMin;
	TMDCPoint BBMax;
	TMDCPoint LocalOrigin;
	float Radius;
	System::StaticArray<char, 16> Name;
};


typedef TMDCTagName *PMDCTagName;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCTagName
{
public:
	System::StaticArray<char, 64> Name;
};
#pragma pack(pop)


typedef TMDCTagFrame *PMDCTagFrame;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCTagFrame
{
public:
	System::StaticArray<System::Word, 3> TagPosition;
	System::StaticArray<System::Word, 3> TagAngle;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCTag
{
public:
	TMDCTagName *TagName;
	TMDCTagFrame *TagFrame;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCSurfaceHeader
{
public:
	System::StaticArray<char, 4> Ident;
	System::StaticArray<char, 64> Name;
	unsigned Flags;
	unsigned NumCompFrames;
	unsigned NumBaseFrames;
	unsigned NumSkins;
	unsigned NumVertices;
	unsigned NumTriangles;
	unsigned OffsetTriangles;
	unsigned OffsetSkins;
	unsigned OffsetTexCoords;
	unsigned OffsetBaseVerts;
	unsigned OffsetCompVerts;
	unsigned OffsetFrameBaseFrames;
	unsigned OffsetFrameCompFrames;
	unsigned OffsetEnd;
};
#pragma pack(pop)


typedef System::StaticArray<unsigned, 3> TMDCTriangle;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCSkin
{
public:
	System::StaticArray<char, 64> Shader;
	unsigned Flags;
};
#pragma pack(pop)


typedef System::StaticArray<float, 2> TMDCTexCoord;

typedef System::StaticArray<short, 4> TMDCBaseVertex;

typedef System::StaticArray<System::Byte, 4> TMDCCompVertex;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCBaseFrame
{
	
private:
	typedef System::DynamicArray<TMDCBaseVertex> _TMDCBaseFrame__1;
	
	
public:
	_TMDCBaseFrame__1 BaseVertices;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCCompFrame
{
	
private:
	typedef System::DynamicArray<TMDCCompVertex> _TMDCCompFrame__1;
	
	
public:
	_TMDCCompFrame__1 CompVertices;
};
#pragma pack(pop)


class PASCALIMPLEMENTATION TGLMDCVectorFile : public Gls::Vectorfileobjects::TGLVectorFile
{
	typedef Gls::Vectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Gls::Applicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* AStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLMDCVectorFile(System::Classes::TPersistent* AOwner) : Gls::Vectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLMDCVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define MDCFILE_IDENTITY L"IDPC"
static const System::Int8 MDCFILE_VERSION = System::Int8(0x2);
static const System::Extended MDC_BASEVERTEX_FACTOR = 1.562500E-02;
static const System::Extended MDC_COMPVERTEX_FACTOR = 4.687500E-02;
}	/* namespace Filemdc */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILEMDC)
using namespace Gls::Filemdc;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FilemdcHPP
