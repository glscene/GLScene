// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Formats.MD2.pas' rev: 35.00 (Windows)

#ifndef Formats_Md2HPP
#define Formats_Md2HPP

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

//-- user supplied -----------------------------------------------------------

namespace Formats
{
namespace Md2
{
//-- forward type declarations -----------------------------------------------
struct TMD2VertexIndex;
struct TMD2Triangle;
struct TMD2TriangleVertex;
struct TMD2AliasFrame;
struct TMD2Header;
class DELPHICLASS TFileMD2;
//-- type declarations -------------------------------------------------------
typedef TMD2VertexIndex *PMD2VertexIndex;

struct DECLSPEC_DRECORD TMD2VertexIndex
{
public:
	int A;
	int B;
	int C;
	float A_S;
	float A_T;
	float B_S;
	float B_T;
	float C_S;
	float C_T;
};


struct DECLSPEC_DRECORD TMD2Triangle
{
public:
	Gls::Vectortypes::TVector3s VertexIndex;
	Gls::Vectortypes::TVector3s TextureCoordIndex;
};


struct DECLSPEC_DRECORD TMD2TriangleVertex
{
public:
	System::StaticArray<System::Byte, 3> Vert;
	System::Byte LightnormalIndex;
};


typedef TMD2AliasFrame *PMD2AliasFrame;

struct DECLSPEC_DRECORD TMD2AliasFrame
{
public:
	Gls::Vectortypes::TVector3f Scale;
	Gls::Vectortypes::TVector3f Translate;
	System::StaticArray<char, 16> Name;
	System::StaticArray<TMD2TriangleVertex, 1> Vertices;
};


struct DECLSPEC_DRECORD TMD2Header
{
public:
	int Ident;
	int Version;
	int SkinWidth;
	int SkinHeight;
	int FrameSize;
	int Num_Skins;
	int Num_Vertices;
	int Num_TextureCoords;
	int Num_VertexIndices;
	int Num_GLCommdands;
	int Num_Frames;
	int Offset_skins;
	int Offset_st;
	int Offset_tris;
	int Offset_frames;
	int Offset_glcmds;
	int Offset_end;
};


typedef System::DynamicArray<TMD2VertexIndex> TIndexList;

typedef System::DynamicArray<Gls::Vectortypes::TVector3f> Formats_Md2__1;

typedef System::DynamicArray<System::DynamicArray<Gls::Vectortypes::TVector3f> > TGLVertexList;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFileMD2 : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FiFrames;
	int FiVertices;
	int FiTriangles;
	void __fastcall FreeLists();
	
public:
	TIndexList fIndexList;
	TGLVertexList fVertexList;
	System::Classes::TStrings* FrameNames;
	__fastcall virtual TFileMD2();
	__fastcall virtual ~TFileMD2();
	void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	__property int iFrames = {read=FiFrames, nodefault};
	__property int iVertices = {read=FiVertices, nodefault};
	__property int iTriangles = {read=FiTriangles, nodefault};
	__property TIndexList IndexList = {read=fIndexList};
	__property TGLVertexList VertexList = {read=fVertexList};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Word MAX_MD2_TRIANGLES = System::Word(0x1000);
static const System::Word MAX_MD2_VERTICES = System::Word(0x800);
static const System::Word MAX_MD2_FRAMES = System::Word(0x200);
static const System::Int8 MAX_MD2_SKINS = System::Int8(0x20);
static const System::Int8 MAX_MD2_SKINNAME = System::Int8(0x40);
}	/* namespace Md2 */
}	/* namespace Formats */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS_MD2)
using namespace Formats::Md2;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS)
using namespace Formats;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Formats_Md2HPP
