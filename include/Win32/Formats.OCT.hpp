// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Formats.OCT.pas' rev: 35.00 (Windows)

#ifndef Formats_OctHPP
#define Formats_OctHPP

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
#include <GLS.VectorLists.hpp>

//-- user supplied -----------------------------------------------------------

namespace Formats
{
namespace Oct
{
//-- forward type declarations -----------------------------------------------
struct TOCTHeader;
struct TOCTVertex;
struct TOCTFace;
struct TOCTTexture;
struct TOCTLightmap;
struct TOCTLight;
class DELPHICLASS TOCTFile;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TOCTHeader
{
public:
	int numVerts;
	int numFaces;
	int numTextures;
	int numLightmaps;
	int numLights;
};


struct DECLSPEC_DRECORD TOCTVertex
{
public:
	Gls::Vectorgeometry::TTexPoint tv;
	Gls::Vectorgeometry::TTexPoint lv;
	Gls::Vectortypes::TVector3f pos;
};


struct DECLSPEC_DRECORD TOCTFace
{
public:
	int start;
	int num;
	int id;
	int lid;
	Gls::Vectortypes::TVector4f p;
};


typedef TOCTFace *POCTFace;

struct DECLSPEC_DRECORD TOCTTexture
{
public:
	int id;
	System::StaticArray<char, 64> Name;
};


struct DECLSPEC_DRECORD TOCTLightmap
{
public:
	int id;
	System::StaticArray<System::Byte, 49152> map;
};


typedef TOCTLightmap *POCTLightmap;

struct DECLSPEC_DRECORD TOCTLight
{
public:
	Gls::Vectortypes::TVector3f pos;
	Gls::Vectortypes::TVector3f color;
	int intensity;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TOCTFile : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<TOCTVertex> _TOCTFile__1;
	
	typedef System::DynamicArray<TOCTFace> _TOCTFile__2;
	
	typedef System::DynamicArray<TOCTTexture> _TOCTFile__3;
	
	typedef System::DynamicArray<TOCTLightmap> _TOCTFile__4;
	
	typedef System::DynamicArray<TOCTLight> _TOCTFile__5;
	
	
public:
	TOCTHeader Header;
	_TOCTFile__1 Vertices;
	_TOCTFile__2 Faces;
	_TOCTFile__3 Textures;
	_TOCTFile__4 Lightmaps;
	_TOCTFile__5 Lights;
	Gls::Vectortypes::TVector3f PlayerPos;
	__fastcall TOCTFile()/* overload */;
	__fastcall TOCTFile(System::Classes::TStream* octStream)/* overload */;
	void __fastcall SaveToStream(System::Classes::TStream* aStream);
	void __fastcall AddTriangles(Gls::Vectorlists::TGLAffineVectorList* vertexCoords, Gls::Vectorlists::TGLAffineVectorList* texMapCoords, const System::UnicodeString textureName);
	void __fastcall AddLight(const Gls::Vectortypes::TVector3f &lightPos, const Gls::Vectortypes::TVector4f &lightColor, int lightIntensity);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TOCTFile() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Oct */
}	/* namespace Formats */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS_OCT)
using namespace Formats::Oct;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS)
using namespace Formats;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Formats_OctHPP
