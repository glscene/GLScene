// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Octree.pas' rev: 35.00 (Windows)

#ifndef Gls_OctreeHPP
#define Gls_OctreeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Math.hpp>
#include <System.Classes.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.GeometryBB.hpp>
#include <GLS.Context.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Octree
{
//-- forward type declarations -----------------------------------------------
struct TOctreeTriangleInfo;
struct TGLOctreeNode;
class DELPHICLASS TGLOctree;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TProcInt)(int I);

typedef void __fastcall (__closure *TProcAffineAffineAffine)(const Gls::Vectortypes::TVector3f &V1, const Gls::Vectortypes::TVector3f &V2, const Gls::Vectortypes::TVector3f &V3);

typedef TOctreeTriangleInfo *POctreeTriangleInfo;

struct DECLSPEC_DRECORD TOctreeTriangleInfo
{
public:
	int Index;
	System::StaticArray<Gls::Vectortypes::TVector3f, 3> Vertex;
};


typedef TGLOctreeNode *PGLOctreeNode;

struct DECLSPEC_DRECORD TGLOctreeNode
{
	
private:
	typedef System::DynamicArray<int> _TGLOctreeNode__1;
	
	
public:
	Gls::Vectortypes::TVector3f MinExtent;
	Gls::Vectortypes::TVector3f MaxExtent;
	Gls::Context::TGLListHandle* ListHandle;
	bool WillDraw;
	_TGLOctreeNode__1 TriArray;
	System::StaticArray<PGLOctreeNode, 8> ChildArray;
};


class PASCALIMPLEMENTATION TGLOctree : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<PGLOctreeNode> _TGLOctree__1;
	
	
private:
	int Intersections;
	
protected:
	float __fastcall GetMidPoint(float Min, float Max);
	bool __fastcall PointInNode(const Gls::Vectortypes::TVector3f &Min, const Gls::Vectortypes::TVector3f &Max, const Gls::Vectortypes::TVector3f &APoint);
	bool __fastcall TriIntersectNode(const Gls::Vectortypes::TVector3f &MinExtent, const Gls::Vectortypes::TVector3f &MaxExtent, const Gls::Vectortypes::TVector3f &V1, const Gls::Vectortypes::TVector3f &V2, const Gls::Vectortypes::TVector3f &V3);
	bool __fastcall SphereInNode(const Gls::Vectortypes::TVector3f &MinExtent, const Gls::Vectortypes::TVector3f &MaxExtent, const Gls::Vectortypes::TVector4f &C, float Radius);
	void __fastcall WalkTriToLeafx(PGLOctreeNode Onode, const Gls::Vectortypes::TVector3f &V1, const Gls::Vectortypes::TVector3f &V2, const Gls::Vectortypes::TVector3f &V3);
	void __fastcall WalkPointToLeafx(PGLOctreeNode ONode, const Gls::Vectortypes::TVector3f &P);
	void __fastcall WalkSphereToLeafx(PGLOctreeNode Onode, const Gls::Vectortypes::TVector4f &P, float Radius);
	void __fastcall WalkRayToLeafx(PGLOctreeNode Onode, const Gls::Vectortypes::TVector4f &P, const Gls::Vectortypes::TVector4f &V);
	Gls::Vectortypes::TVector3f __fastcall GetExtent(const System::Byte *Flags, const int Flags_High, PGLOctreeNode ParentNode);
	void __fastcall Refine(PGLOctreeNode ParentNode, int Level);
	void __fastcall WalkPointToLeaf(PGLOctreeNode ONode, const Gls::Vectortypes::TVector3f &P);
	void __fastcall WalkTriToLeaf(PGLOctreeNode Onode, const Gls::Vectortypes::TVector3f &V1, const Gls::Vectortypes::TVector3f &V2, const Gls::Vectortypes::TVector3f &V3);
	void __fastcall WalkRayToLeaf(PGLOctreeNode Onode, const Gls::Vectortypes::TVector4f &P, const Gls::Vectortypes::TVector4f &V);
	void __fastcall ConvertR4(PGLOctreeNode ONode, const Gls::Vectortypes::TVector3f &Scale);
	void __fastcall CreateTree(int Depth);
	void __fastcall CutMesh();
	
public:
	Gls::Vectortypes::TVector3f WorldMinExtent;
	Gls::Vectortypes::TVector3f WorldMaxExtent;
	TGLOctreeNode *RootNode;
	int MaxOlevel;
	int NodeCount;
	int TriCountMesh;
	int TriCountOctree;
	int MeshCount;
	_TGLOctree__1 ResultArray;
	Gls::Vectorlists::TGLAffineVectorList* TriangleFiler;
	void __fastcall WalkSphereToLeaf(PGLOctreeNode Onode, const Gls::Vectortypes::TVector4f &P, float Radius);
	void __fastcall InitializeTree(const Gls::Vectortypes::TVector3f &AWorldMinExtent, const Gls::Vectortypes::TVector3f &AWorldMaxExtent, Gls::Vectorlists::TGLAffineVectorList* const ATriangles, const int ATreeDepth);
	void __fastcall DisposeTree();
	__fastcall virtual ~TGLOctree();
	bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &RayStart, const Gls::Vectortypes::TVector4f &RayVector, Gls::Vectortypes::PGLVector IntersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector IntersectNormal = (Gls::Vectortypes::PGLVector)(0x0), POctreeTriangleInfo TriangleInfo = (POctreeTriangleInfo)(0x0));
	bool __fastcall SphereSweepIntersect(const Gls::Vectortypes::TVector4f &RayStart, const Gls::Vectortypes::TVector4f &RayVector, const float Velocity, const float Radius, Gls::Vectortypes::PGLVector IntersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector IntersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	bool __fastcall TriangleIntersect(const Gls::Vectortypes::TVector3f &V1, const Gls::Vectortypes::TVector3f &V2, const Gls::Vectortypes::TVector3f &V3);
	Gls::Vectorlists::TGLAffineVectorList* __fastcall GetTrianglesFromNodesIntersectingAABB(const Gls::Geometrybb::TAABB &ObjAABB);
	Gls::Vectorlists::TGLAffineVectorList* __fastcall GetTrianglesFromNodesIntersectingCube(const Gls::Geometrybb::TAABB &ObjAABB, const Gls::Vectortypes::TMatrix4f &ObjToSelf, const Gls::Vectortypes::TMatrix4f &SelfToObj);
	bool __fastcall AABBIntersect(const Gls::Geometrybb::TAABB &AABB, const Gls::Vectortypes::TMatrix4f &M1to2, const Gls::Vectortypes::TMatrix4f &M2to1, Gls::Vectorlists::TGLAffineVectorList* Triangles = (Gls::Vectorlists::TGLAffineVectorList*)(0x0));
public:
	/* TObject.Create */ inline __fastcall TGLOctree() : System::TObject() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Octree */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_OCTREE)
using namespace Gls::Octree;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_OctreeHPP
