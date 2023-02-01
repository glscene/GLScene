// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.EllipseCollision.pas' rev: 35.00 (Windows)

#ifndef Gls_EllipsecollisionHPP
#define Gls_EllipsecollisionHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Octree.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Ellipsecollision
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TECPlane;
struct TECObjectInfo;
struct TECTriangle;
struct TECTriMesh;
struct TECFreeForm;
struct TECCollider;
struct TECContact;
struct TECCollisionPacket;
struct TECMovePack;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TECPlane : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::StaticArray<float, 4> Equation;
	Gls::Vectortypes::TVector3f Origin;
	Gls::Vectortypes::TVector3f Normal;
	void __fastcall MakePlane(const Gls::Vectortypes::TVector3f &nOrigin, const Gls::Vectortypes::TVector3f &nNormal)/* overload */;
	void __fastcall MakePlane(const Gls::Vectortypes::TVector3f &p1, const Gls::Vectortypes::TVector3f &p2, const Gls::Vectortypes::TVector3f &p3)/* overload */;
	bool __fastcall isFrontFacingTo(const Gls::Vectortypes::TVector3f &Direction);
	float __fastcall signedDistanceTo(const Gls::Vectortypes::TVector3f &Point);
public:
	/* TObject.Create */ inline __fastcall TECPlane() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TECPlane() { }
	
};


struct DECLSPEC_DRECORD TECObjectInfo
{
public:
	Gls::Vectortypes::TMatrix4f AbsoluteMatrix;
	bool Solid;
	bool IsDynamic;
	int ObjectID;
};


struct DECLSPEC_DRECORD TECTriangle
{
public:
	Gls::Vectortypes::TVector3f p1;
	Gls::Vectortypes::TVector3f p2;
	Gls::Vectortypes::TVector3f p3;
};


struct DECLSPEC_DRECORD TECTriMesh
{
	
private:
	typedef System::DynamicArray<TECTriangle> _TECTriMesh__1;
	
	
public:
	_TECTriMesh__1 Triangles;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECTriMesh> TECTriMeshList;

struct DECLSPEC_DRECORD TECFreeForm
{
	
private:
	typedef System::DynamicArray<Gls::Octree::PGLOctreeNode> _TECFreeForm__1;
	
	
public:
	_TECFreeForm__1 OctreeNodes;
	Gls::Vectorlists::TGLAffineVectorList* *triangleFiler;
	bool InvertedNormals;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECFreeForm> TECFreeFormList;

enum DECLSPEC_DENUM TECColliderShape : unsigned char { csEllipsoid, csPoint };

struct DECLSPEC_DRECORD TECCollider
{
public:
	Gls::Vectortypes::TVector3f Position;
	Gls::Vectortypes::TVector3f Radius;
	TECColliderShape Shape;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECCollider> TECColliderList;

struct DECLSPEC_DRECORD TECContact
{
public:
	Gls::Vectortypes::TVector3f Position;
	Gls::Vectortypes::TVector3f SurfaceNormal;
	float Distance;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECContact> TECContactList;

struct DECLSPEC_DRECORD TECCollisionPacket
{
public:
	Gls::Vectortypes::TVector3f velocity;
	Gls::Vectortypes::TVector3f normalizedVelocity;
	Gls::Vectortypes::TVector3f basePoint;
	bool foundCollision;
	float nearestDistance;
	int NearestObject;
	Gls::Vectortypes::TVector3f intersectionPoint;
	Gls::Vectortypes::TVector3f intersectionNormal;
};


struct DECLSPEC_DRECORD TECMovePack
{
public:
	TECTriMeshList TriMeshes;
	TECFreeFormList Freeforms;
	TECColliderList Colliders;
	Gls::Vectortypes::TVector3f Position;
	Gls::Vectortypes::TVector3f velocity;
	Gls::Vectortypes::TVector3f Gravity;
	Gls::Vectortypes::TVector3f Radius;
	TECObjectInfo ObjectInfo;
	float CollisionRange;
	double UnitScale;
	System::Byte MaxRecursionDepth;
	TECCollisionPacket CP;
	System::Byte collisionRecursionDepth;
	Gls::Vectortypes::TVector3f ResultPos;
	int NearestObject;
	bool VelocityCollided;
	bool GravityCollided;
	Gls::Vectortypes::TVector3f GroundNormal;
	TECContactList Contacts;
};


typedef System::DynamicArray<TECTriangle> Gls_Ellipsecollision__2;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE float cECCloseDistance;
extern DELPHI_PACKAGE Gls_Ellipsecollision__2 Debug_Tri;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector3f __fastcall VectorDivide(const Gls::Vectortypes::TVector3f &v, const Gls::Vectortypes::TVector3f &divider);
extern DELPHI_PACKAGE void __fastcall CollideAndSlide(TECMovePack &MP);
extern DELPHI_PACKAGE void __fastcall CollideWithWorld(TECMovePack &MP, const Gls::Vectortypes::TVector3f &pos, const Gls::Vectortypes::TVector3f &vel, bool &HasCollided);
}	/* namespace Ellipsecollision */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_ELLIPSECOLLISION)
using namespace Gls::Ellipsecollision;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_EllipsecollisionHPP
