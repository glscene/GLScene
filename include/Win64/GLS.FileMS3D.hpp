// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FileMS3D.pas' rev: 35.00 (Windows)

#ifndef Gls_Filems3dHPP
#define Gls_Filems3dHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Math.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Material.hpp>
#include <GLS.Color.hpp>
#include <GLS.Texture.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Filems3d
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMS3DGroup;
struct TMS3DHeader;
struct TMS3DVertex;
struct TMS3DTriangle;
struct TMS3DMaterial;
struct TMS3DKeyframeRotation;
struct TMS3DKeyframePosition;
struct TMS3DJointBase;
struct TMS3DJoint;
struct TMS3DComment;
class DELPHICLASS TMS3DCommentList;
struct TMS3D_vertex_ex_t;
class DELPHICLASS TVertexWeightList;
class DELPHICLASS TGLMS3DVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TMS3DGroup : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::Byte Flags;
	System::StaticArray<char, 32> Name;
	System::Word NumTriangles;
	System::Classes::TList* TriangleIndices;
	System::Int8 MaterialIndex;
	__fastcall TMS3DGroup();
	__fastcall virtual ~TMS3DGroup();
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DHeader
{
public:
	System::StaticArray<char, 10> ID;
	int Version;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DVertex
{
public:
	System::Byte Flags;
	Gls::Vectortypes::TD3DVector Vertex;
	char BoneId;
	System::Byte ReferenceCount;
};
#pragma pack(pop)


typedef System::StaticArray<TMS3DVertex, 8192> TMS3DVertexArray;

typedef TMS3DVertexArray *PMS3DVertexArray;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DTriangle
{
public:
	System::Word Flags;
	System::StaticArray<System::Word, 3> VertexIndices;
	System::StaticArray<Gls::Vectortypes::TD3DVector, 3> VertexNormals;
	System::StaticArray<float, 3> S;
	System::StaticArray<float, 3> T;
	System::Byte SmoothingGroup;
	System::Byte GroupIndex;
};
#pragma pack(pop)


typedef System::StaticArray<TMS3DTriangle, 16384> TMS3DTriangleArray;

typedef TMS3DTriangleArray *PMS3DTriangleArray;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DMaterial
{
public:
	System::StaticArray<char, 32> Name;
	Gls::Vectortypes::TVector4f Ambient;
	Gls::Vectortypes::TVector4f Diffuse;
	Gls::Vectortypes::TVector4f Specular;
	Gls::Vectortypes::TVector4f Emissive;
	float Shininess;
	float Transparency;
	char Mode;
	System::StaticArray<char, 128> Texture;
	System::StaticArray<char, 128> Alphamap;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DKeyframeRotation
{
public:
	float Time;
	Gls::Vectortypes::TD3DVector Rotation;
};
#pragma pack(pop)


typedef System::StaticArray<TMS3DKeyframeRotation, 5000> TMS3DKeyframeRotationArray;

typedef TMS3DKeyframeRotationArray *PMS3DKeyframeRotationArray;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DKeyframePosition
{
public:
	float Time;
	Gls::Vectortypes::TD3DVector Position;
};
#pragma pack(pop)


typedef System::StaticArray<TMS3DKeyframePosition, 5000> TMS3DKeyframePositionArray;

typedef TMS3DKeyframePositionArray *PMS3DKeyframePositionArray;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DJointBase
{
public:
	System::Byte Flags;
	System::StaticArray<char, 32> Name;
	System::StaticArray<char, 32> ParentName;
	Gls::Vectortypes::TD3DVector Rotation;
	Gls::Vectortypes::TD3DVector Position;
	System::Word NumKeyFramesRot;
	System::Word NumKeyFramesTrans;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DJoint
{
public:
	TMS3DJointBase Base;
	TMS3DKeyframeRotationArray *KeyFramesRot;
	TMS3DKeyframePositionArray *KeyFramesTrans;
};
#pragma pack(pop)


typedef System::StaticArray<TMS3DJoint, 128> TMS3DJointArray;

typedef TMS3DJointArray *PMS3DJointArray;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DComment
{
	
private:
	typedef System::DynamicArray<char> _TMS3DComment__1;
	
	
public:
	int index;
	int commentLength;
	_TMS3DComment__1 comment;
};
#pragma pack(pop)


typedef TMS3DComment *pMS3DComment;

#pragma pack(push,1)
class PASCALIMPLEMENTATION TMS3DCommentList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	__fastcall virtual ~TMS3DCommentList();
	pMS3DComment __fastcall NewComment();
public:
	/* TObject.Create */ inline __fastcall TMS3DCommentList() : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3D_vertex_ex_t
{
public:
	System::StaticArray<System::Byte, 3> boneIds;
	System::StaticArray<System::Byte, 3> weights;
	unsigned extra;
	unsigned unknown;
};
#pragma pack(pop)


typedef TMS3D_vertex_ex_t *pMS3D_vertex_ex_t;

#pragma pack(push,1)
class PASCALIMPLEMENTATION TVertexWeightList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
private:
	int FsubVersion;
	pMS3D_vertex_ex_t __fastcall GetWeight(int idx);
	void __fastcall SetsubVersion(const int Value);
	
public:
	pMS3D_vertex_ex_t __fastcall newWeight();
	virtual void __fastcall Clear();
	__fastcall virtual ~TVertexWeightList();
	__property pMS3D_vertex_ex_t Weight[int idx] = {read=GetWeight};
	__property int subVersion = {read=FsubVersion, write=SetsubVersion, nodefault};
public:
	/* TObject.Create */ inline __fastcall TVertexWeightList() : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,1)
class PASCALIMPLEMENTATION TGLMS3DVectorFile : public Gls::Vectorfileobjects::TGLVectorFile
{
	typedef Gls::Vectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Gls::Applicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLMS3DVectorFile(System::Classes::TPersistent* AOwner) : Gls::Vectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLMS3DVectorFile() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Word MAX_MS3D_VERTICES = System::Word(0x2000);
static const System::Word MAX_MS3D_TRIANGLES = System::Word(0x4000);
static const System::Byte MAX_MS3D_GROUPS = System::Byte(0x80);
static const System::Byte MAX_MS3D_MATERIALS = System::Byte(0x80);
static const System::Byte MAX_MS3D_JOINTS = System::Byte(0x80);
static const System::Word MAX_MS3D_KEYFRAMES = System::Word(0x1388);
}	/* namespace Filems3d */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILEMS3D)
using namespace Gls::Filems3d;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_Filems3dHPP
