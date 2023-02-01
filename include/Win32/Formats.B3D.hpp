// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Formats.B3D.pas' rev: 35.00 (Windows)

#ifndef Formats_B3dHPP
#define Formats_B3dHPP

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
namespace B3d
{
//-- forward type declarations -----------------------------------------------
struct TB3DChunk;
struct TBB3DChunk;
struct TTEXSChunk;
struct TBRUSChunk;
struct TVertexData;
struct TVRTSChunk;
struct TTRISChunk;
struct TMESHChunk;
struct TBONEChunk;
struct TKEYSChunk;
struct TANIMChunk;
struct TNODEChunk;
class DELPHICLASS TB3DMaterial;
class DELPHICLASS TB3DTexture;
class DELPHICLASS TB3DNode;
class DELPHICLASS TFileB3D;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TB3DChunkType : unsigned char { bctUnknown, bctHeader, bctTexture, bctBrush, bctNode, bctVertex, bctTriangle, bctMesh, bctBone, bctKeyFrame, bctAnimation };

typedef TB3DChunk *PB3DChunk;

struct DECLSPEC_DRECORD TB3DChunk
{
public:
	System::StaticArray<System::WideChar, 4> chunk;
	int length;
};


typedef TBB3DChunk *PBB3DChunk;

struct DECLSPEC_DRECORD TBB3DChunk
{
public:
	int Version;
};


typedef TTEXSChunk *PTEXSChunk;

struct DECLSPEC_DRECORD TTEXSChunk
{
public:
	System::StaticArray<System::WideChar, 256> fileName;
	int flags;
	int blend;
	float x_pos;
	float y_pos;
	float x_scale;
	float y_scale;
	float rotation;
};


typedef TBRUSChunk *PBRUSChunk;

struct DECLSPEC_DRECORD TBRUSChunk
{
	
private:
	typedef System::DynamicArray<int> _TBRUSChunk__1;
	
	
public:
	int n_texs;
	System::StaticArray<System::WideChar, 256> name;
	float red;
	float green;
	float blue;
	float alpha;
	float shininess;
	int blend;
	int fx;
	_TBRUSChunk__1 texture_id;
};


typedef TVertexData *PVertexData;

struct DECLSPEC_DRECORD TVertexData
{
	
private:
	typedef System::DynamicArray<float> _TVertexData__1;
	
	
public:
	TVertexData *next;
	float x;
	float y;
	float z;
	float nx;
	float ny;
	float nz;
	float red;
	float green;
	float blue;
	float alpha;
	_TVertexData__1 tex_coords;
};


typedef TVRTSChunk *PVRTSChunk;

struct DECLSPEC_DRECORD TVRTSChunk
{
public:
	int flags;
	int tex_coord_sets;
	int tex_coord_set_size;
	TVertexData *vertices;
};


typedef TTRISChunk *PTRISChunk;

struct DECLSPEC_DRECORD TTRISChunk
{
	
private:
	typedef System::DynamicArray<int> _TTRISChunk__1;
	
	
public:
	TTRISChunk *next;
	int brush_id;
	_TTRISChunk__1 vertex_id;
};


typedef TMESHChunk *PMESHChunk;

struct DECLSPEC_DRECORD TMESHChunk
{
public:
	int brush_id;
	TVRTSChunk vertices;
	TTRISChunk *triangles;
};


typedef TBONEChunk *PBONEChunk;

struct DECLSPEC_DRECORD TBONEChunk
{
public:
	int vertex_id;
	float weight;
};


typedef TKEYSChunk *PKEYSChunk;

struct DECLSPEC_DRECORD TKEYSChunk
{
public:
	TKEYSChunk *next;
	int flags;
	int frame;
	Gls::Vectortypes::TVector3f position;
	Gls::Vectortypes::TVector3f scale;
	Gls::Vectortypes::TVector4f rotation;
};


typedef TANIMChunk *PANIMChunk;

struct DECLSPEC_DRECORD TANIMChunk
{
public:
	int flags;
	int frames;
	float fps;
};


typedef TNODEChunk *PNODEChunk;

struct DECLSPEC_DRECORD TNODEChunk
{
public:
	System::StaticArray<System::WideChar, 256> name;
	Gls::Vectortypes::TVector3f position;
	Gls::Vectortypes::TVector3f scale;
	Gls::Vectortypes::TVector4f rotation;
	TMESHChunk *meshes;
	TKEYSChunk *keys;
	TNODEChunk *nodes;
	TANIMChunk animation;
	TNODEChunk *next;
	int level;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TB3DMaterial : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TBRUSChunk MaterialData;
	__fastcall TB3DMaterial();
	__fastcall virtual ~TB3DMaterial();
	System::UnicodeString __fastcall GetMaterialName();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TB3DTexture : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TTEXSChunk TextureData;
	__fastcall TB3DTexture();
	__fastcall virtual ~TB3DTexture();
	System::UnicodeString __fastcall GetTextureName();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TB3DNode : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TNODEChunk *NodeData;
	__fastcall TB3DNode();
	__fastcall virtual ~TB3DNode();
	System::UnicodeString __fastcall GetNodeName();
	void __fastcall DestroyNodeData(PNODEChunk Node);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFileB3D : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStringList* fTextures;
	System::Classes::TStringList* fMaterials;
	TB3DNode* fNodes;
	void __fastcall FreeLists();
	TB3DChunkType __fastcall GetChunkType(const TB3DChunk &aChunk);
	int __fastcall SkipChunk(System::Classes::TStream* aStream, const TB3DChunk &aChunk);
	int __fastcall ReadTextureChunk(System::Classes::TStream* aStream, const TB3DChunk &aChunk);
	int __fastcall ReadMaterialChunk(System::Classes::TStream* aStream, const TB3DChunk &aChunk);
	int __fastcall ReadNodeChunk(System::Classes::TStream* aStream, const TB3DChunk &aChunk, PNODEChunk Node, int level);
	int __fastcall ReadMeshChunk(System::Classes::TStream* aStream, const TB3DChunk &aChunk, PMESHChunk Mesh);
	int __fastcall ReadVerticesChunk(System::Classes::TStream* aStream, const TB3DChunk &aChunk, PVRTSChunk Vertices);
	int __fastcall ReadTrianglesChunk(System::Classes::TStream* aStream, const TB3DChunk &aChunk, PTRISChunk Triangle);
	
public:
	__fastcall virtual TFileB3D();
	__fastcall virtual ~TFileB3D();
	void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	void __fastcall Check();
	__property System::Classes::TStringList* Textures = {read=fTextures};
	__property System::Classes::TStringList* Materials = {read=fMaterials};
	__property TB3DNode* Nodes = {read=fNodes};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace B3d */
}	/* namespace Formats */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS_B3D)
using namespace Formats::B3d;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS)
using namespace Formats;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Formats_B3dHPP
