// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.SpacePartition.pas' rev: 35.00 (Windows)

#ifndef Gls_SpacepartitionHPP
#define Gls_SpacepartitionHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.GeometryBB.hpp>
#include <GLS.Context.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.State.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Spacepartition
{
//-- forward type declarations -----------------------------------------------
struct TGLConeSP;
struct TGLExtendedFrustum;
class DELPHICLASS TGLSpacePartitionLeaf;
class DELPHICLASS TGLSpacePartitionLeafList;
class DELPHICLASS TGLSpacePartitionLeafS;
class DELPHICLASS TGLBaseSpacePartition;
class DELPHICLASS TGLLeavedSpacePartition;
class DELPHICLASS TGLSectorNode;
class DELPHICLASS TGLSectoredSpacePartition;
class DELPHICLASS TSPOctreeNode;
class DELPHICLASS TGLOctreeSpacePartition;
class DELPHICLASS TSPQuadtreeNode;
class DELPHICLASS TGLQuadtreeSpacePartition;
class DELPHICLASS TGLSceneObj;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TGLConeSP
{
public:
	Gls::Vectortypes::TVector3f Base;
	Gls::Vectortypes::TVector3f Axis;
	float Angle;
	float Length;
};


struct DECLSPEC_DRECORD TGLExtendedFrustum
{
public:
	Gls::Vectorgeometry::TFrustum Frustum;
	Gls::Geometrybb::TBSphere BSphere;
};


class PASCALIMPLEMENTATION TGLSpacePartitionLeaf : public Gls::Persistentclasses::TGLPersistentObject
{
	typedef Gls::Persistentclasses::TGLPersistentObject inherited;
	
private:
	TGLBaseSpacePartition* FSpacePartition;
	void __fastcall SetSpacePartition(TGLBaseSpacePartition* const Value);
	
public:
	void *FPartitionTag;
	Gls::Geometrybb::TAABB FCachedAABB;
	Gls::Geometrybb::TBSphere FCachedBSphere;
	virtual void __fastcall Changed();
	virtual void __fastcall UpdateCachedAABBAndBSphere();
	__property TGLBaseSpacePartition* SpacePartition = {read=FSpacePartition, write=SetSpacePartition};
	__property void * PartitionTag = {read=FPartitionTag};
	__fastcall TGLSpacePartitionLeaf(TGLBaseSpacePartition* SpacePartition);
	__fastcall virtual ~TGLSpacePartitionLeaf();
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TGLSpacePartitionLeaf() : Gls::Persistentclasses::TGLPersistentObject() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLSpacePartitionLeaf(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Persistentclasses::TGLPersistentObject(reader) { }
	
};


class PASCALIMPLEMENTATION TGLSpacePartitionLeafList : public Gls::Persistentclasses::TGLPersistentObjectList
{
	typedef Gls::Persistentclasses::TGLPersistentObjectList inherited;
	
public:
	TGLSpacePartitionLeaf* operator[](int I) { return this->Items[I]; }
	
private:
	TGLSpacePartitionLeaf* __fastcall GetItems(int I);
	void __fastcall SetItems(int I, TGLSpacePartitionLeaf* const Value);
	
public:
	__property TGLSpacePartitionLeaf* Items[int I] = {read=GetItems, write=SetItems/*, default*/};
	__fastcall virtual TGLSpacePartitionLeafList();
public:
	/* TGLPersistentObjectList.Destroy */ inline __fastcall virtual ~TGLSpacePartitionLeafList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLSpacePartitionLeafList(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Persistentclasses::TGLPersistentObjectList(reader) { }
	
};


class PASCALIMPLEMENTATION TGLSpacePartitionLeafS : public TGLSpacePartitionLeaf
{
	typedef TGLSpacePartitionLeaf inherited;
	
public:
	Gls::Scene::TGLBaseSceneObject* GLBaseSceneObject;
	Gls::Vectortypes::TVector3f Direction;
	virtual void __fastcall UpdateCachedAABBAndBSphere();
	__fastcall TGLSpacePartitionLeafS(TGLBaseSpacePartition* SpacePartition, Gls::Scene::TGLBaseSceneObject* aGLBaseSceneObject);
public:
	/* TGLSpacePartitionLeaf.CreateOwned */ inline __fastcall TGLSpacePartitionLeafS(TGLBaseSpacePartition* SpacePartition) : TGLSpacePartitionLeaf(SpacePartition) { }
	/* TGLSpacePartitionLeaf.Destroy */ inline __fastcall virtual ~TGLSpacePartitionLeafS() { }
	
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TGLSpacePartitionLeafS() : TGLSpacePartitionLeaf() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLSpacePartitionLeafS(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLSpacePartitionLeaf(reader) { }
	
};


enum DECLSPEC_DENUM TGLCullingMode : unsigned char { CmFineCulling, CmGrossCulling };

class PASCALIMPLEMENTATION TGLBaseSpacePartition : public Gls::Persistentclasses::TGLPersistentObject
{
	typedef Gls::Persistentclasses::TGLPersistentObject inherited;
	
private:
	TGLCullingMode FCullingMode;
	virtual int __fastcall QueryCone(const TGLConeSP &ACone);
	
protected:
	TGLSpacePartitionLeafList* FQueryResult;
	int FQueryInterObjectTests;
	virtual void __fastcall FlushQueryResult();
	
public:
	__property TGLSpacePartitionLeafList* QueryResult = {read=FQueryResult};
	virtual void __fastcall Clear();
	virtual void __fastcall AddLeaf(TGLSpacePartitionLeaf* ALeaf);
	virtual void __fastcall RemoveLeaf(TGLSpacePartitionLeaf* ALeaf);
	virtual void __fastcall LeafChanged(TGLSpacePartitionLeaf* ALeaf);
	virtual int __fastcall QueryAABB(const Gls::Geometrybb::TAABB &AAABB);
	virtual int __fastcall QueryBSphere(const Gls::Geometrybb::TBSphere &ABSphere);
	virtual int __fastcall QueryLeaf(TGLSpacePartitionLeaf* const ALeaf);
	virtual int __fastcall QueryPlane(const Gls::Vectortypes::TVector3f &Location, const Gls::Vectortypes::TVector3f &Normal);
	virtual int __fastcall QueryFrustum(const Gls::Vectorgeometry::TFrustum &Frustum);
	virtual int __fastcall QueryFrustumEx(const TGLExtendedFrustum &ExtendedFrustum);
	__property int QueryInterObjectTests = {read=FQueryInterObjectTests, nodefault};
	virtual void __fastcall ProcessUpdated();
	__property TGLCullingMode CullingMode = {read=FCullingMode, write=FCullingMode, nodefault};
	__fastcall virtual TGLBaseSpacePartition();
	__fastcall virtual ~TGLBaseSpacePartition();
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLBaseSpacePartition(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Persistentclasses::TGLPersistentObject(reader) { }
	
};


class PASCALIMPLEMENTATION TGLLeavedSpacePartition : public TGLBaseSpacePartition
{
	typedef TGLBaseSpacePartition inherited;
	
private:
	TGLSpacePartitionLeafList* FLeaves;
	virtual int __fastcall QueryCone(const TGLConeSP &ACone);
	
public:
	virtual void __fastcall Clear();
	virtual void __fastcall AddLeaf(TGLSpacePartitionLeaf* ALeaf);
	virtual void __fastcall RemoveLeaf(TGLSpacePartitionLeaf* ALeaf);
	virtual int __fastcall QueryAABB(const Gls::Geometrybb::TAABB &AAABB);
	virtual int __fastcall QueryBSphere(const Gls::Geometrybb::TBSphere &ABSphere);
	virtual int __fastcall QueryPlane(const Gls::Vectortypes::TVector3f &FLocation, const Gls::Vectortypes::TVector3f &FNormal);
	__fastcall virtual TGLLeavedSpacePartition();
	__fastcall virtual ~TGLLeavedSpacePartition();
	
__published:
	__property TGLSpacePartitionLeafList* Leaves = {read=FLeaves};
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLLeavedSpacePartition(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLBaseSpacePartition(reader) { }
	
};


typedef System::StaticArray<TGLSectorNode*, 8> TGLSectorNodeArray;

class PASCALIMPLEMENTATION TGLSectorNode : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLSpacePartitionLeafList* FLeaves;
	Gls::Geometrybb::TAABB FAABB;
	TGLSectoredSpacePartition* FSectoredSpacePartition;
	int FRecursiveLeafCount;
	TGLSectorNode* FParent;
	int FNodeDepth;
	int FChildCount;
	TGLSectorNodeArray FChildren;
	Gls::Geometrybb::TBSphere FBSphere;
	bool __fastcall GetNoChildren();
	void __fastcall SetAABB(const Gls::Geometrybb::TAABB &Value);
	Gls::Vectortypes::TVector3f __fastcall GetCenter();
	
protected:
	int __fastcall CalcRecursiveLeafCount();
	TGLSectorNode* __fastcall PlaceLeafInChild(TGLSpacePartitionLeaf* ALeaf);
	System::UnicodeString __fastcall VerifyRecursiveLeafCount();
	virtual void __fastcall ChildrenChanged();
	
public:
	void __fastcall Clear();
	__property Gls::Geometrybb::TAABB AABB = {read=FAABB, write=SetAABB};
	__property Gls::Geometrybb::TBSphere BSphere = {read=FBSphere};
	__property Gls::Vectortypes::TVector3f Center = {read=GetCenter};
	__property bool NoChildren = {read=GetNoChildren, nodefault};
	__property TGLSectorNodeArray Children = {read=FChildren};
	__property int ChildCount = {read=FChildCount, nodefault};
	virtual TGLSectorNode* __fastcall GetChildForAABB(const Gls::Geometrybb::TAABB &AABB);
	__property TGLSpacePartitionLeafList* Leaves = {read=FLeaves};
	__property TGLSectoredSpacePartition* SectoredSpacePartition = {read=FSectoredSpacePartition};
	__property TGLSectorNode* Parent = {read=FParent};
	__property int RecursiveLeafCount = {read=FRecursiveLeafCount, nodefault};
	__property int NodeDepth = {read=FNodeDepth, nodefault};
	virtual bool __fastcall AABBFitsInNode(const Gls::Geometrybb::TAABB &AAABB);
	virtual bool __fastcall AABBIntersectsNode(const Gls::Geometrybb::TAABB &AAABB);
	virtual bool __fastcall BSphereFitsInNode(const Gls::Geometrybb::TBSphere &BSphere);
	virtual bool __fastcall BSphereIntersectsNode(const Gls::Geometrybb::TBSphere &BSphere);
	virtual Gls::Geometrybb::TSpaceContains __fastcall AABBContainsSector(const Gls::Geometrybb::TAABB &AABB);
	virtual Gls::Geometrybb::TSpaceContains __fastcall BSphereContainsSector(const Gls::Geometrybb::TBSphere &BSphere);
	virtual Gls::Geometrybb::TSpaceContains __fastcall ContainsBSphere(const Gls::Geometrybb::TBSphere &ABSphere);
	virtual Gls::Geometrybb::TSpaceContains __fastcall ContainsAABB(const Gls::Geometrybb::TAABB &AAABB);
	TGLSectorNode* __fastcall AddLeaf(TGLSpacePartitionLeaf* ALeaf);
	bool __fastcall RemoveLeaf(TGLSpacePartitionLeaf* ALeaf, bool OwnerByThis);
	void __fastcall QueryAABB(const Gls::Geometrybb::TAABB &AAABB, TGLSpacePartitionLeafList* const QueryResult);
	void __fastcall QueryBSphere(const Gls::Geometrybb::TBSphere &ABSphere, TGLSpacePartitionLeafList* const QueryResult);
	void __fastcall QueryPlane(const Gls::Vectortypes::TVector3f &Location, const Gls::Vectortypes::TVector3f &Normal, TGLSpacePartitionLeafList* const QueryResult);
	void __fastcall QueryFrustum(const Gls::Vectorgeometry::TFrustum &Frustum, TGLSpacePartitionLeafList* const QueryResult);
	void __fastcall QueryFrustumEx(const TGLExtendedFrustum &ExtendedFrustum, TGLSpacePartitionLeafList* const QueryResult);
	void __fastcall AddAllLeavesRecursive(TGLSpacePartitionLeafList* const QueryResult);
	void __fastcall ExpandNode();
	virtual void __fastcall CreateChildren();
	void __fastcall CollapseNode();
	int __fastcall GetNodeCount();
	__fastcall TGLSectorNode(TGLSectoredSpacePartition* ASectoredSpacePartition, TGLSectorNode* AParent);
	__fastcall virtual ~TGLSectorNode();
};


enum DECLSPEC_DENUM TGLGrowMethod : unsigned char { gmNever, gmBestFit, gmIncreaseToFitAll };

class PASCALIMPLEMENTATION TGLSectoredSpacePartition : public TGLLeavedSpacePartition
{
	typedef TGLLeavedSpacePartition inherited;
	
private:
	TGLSectorNode* FRootNode;
	int FLeafThreshold;
	int FMaxTreeDepth;
	float FGrowGravy;
	TGLGrowMethod FGrowMethod;
	void __fastcall SetLeafThreshold(const int Value);
	void __fastcall SetMaxTreeDepth(const int Value);
	
protected:
	int FQueryNodeTests;
	virtual void __fastcall FlushQueryResult();
	
public:
	virtual void __fastcall AddLeaf(TGLSpacePartitionLeaf* ALeaf);
	virtual void __fastcall RemoveLeaf(TGLSpacePartitionLeaf* ALeaf);
	virtual void __fastcall LeafChanged(TGLSpacePartitionLeaf* ALeaf);
	virtual int __fastcall QueryAABB(const Gls::Geometrybb::TAABB &AAABB);
	virtual int __fastcall QueryBSphere(const Gls::Geometrybb::TBSphere &ABSphere);
	virtual int __fastcall QueryLeaf(TGLSpacePartitionLeaf* const ALeaf);
	virtual int __fastcall QueryPlane(const Gls::Vectortypes::TVector3f &Location, const Gls::Vectortypes::TVector3f &Normal);
	virtual int __fastcall QueryFrustum(const Gls::Vectorgeometry::TFrustum &Frustum);
	virtual int __fastcall QueryFrustumEx(const TGLExtendedFrustum &ExtendedFrustum);
	__property int QueryNodeTests = {read=FQueryNodeTests, nodefault};
	int __fastcall GetNodeCount();
	void __fastcall UpdateStructureSize(float Gravy);
	void __fastcall RebuildTree(const Gls::Geometrybb::TAABB &NewAABB);
	Gls::Geometrybb::TAABB __fastcall GetAABB();
	virtual TGLSectorNode* __fastcall CreateNewNode(TGLSectorNode* AParent);
	virtual void __fastcall Clear();
	__fastcall virtual TGLSectoredSpacePartition();
	__fastcall virtual ~TGLSectoredSpacePartition();
	
__published:
	__property TGLSectorNode* RootNode = {read=FRootNode};
	__property int MaxTreeDepth = {read=FMaxTreeDepth, write=SetMaxTreeDepth, nodefault};
	__property int LeafThreshold = {read=FLeafThreshold, write=SetLeafThreshold, nodefault};
	__property TGLGrowMethod GrowMethod = {read=FGrowMethod, write=FGrowMethod, nodefault};
	__property float GrowGravy = {read=FGrowGravy, write=FGrowGravy};
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLSectoredSpacePartition(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLLeavedSpacePartition(reader) { }
	
};


class PASCALIMPLEMENTATION TSPOctreeNode : public TGLSectorNode
{
	typedef TGLSectorNode inherited;
	
public:
	virtual void __fastcall CreateChildren();
	virtual bool __fastcall AABBFitsInNode(const Gls::Geometrybb::TAABB &AAABB);
	virtual bool __fastcall AABBIntersectsNode(const Gls::Geometrybb::TAABB &AAABB);
	virtual bool __fastcall BSphereFitsInNode(const Gls::Geometrybb::TBSphere &BSphere);
	virtual bool __fastcall BSphereIntersectsNode(const Gls::Geometrybb::TBSphere &BSphere);
public:
	/* TGLSectorNode.Create */ inline __fastcall TSPOctreeNode(TGLSectoredSpacePartition* ASectoredSpacePartition, TGLSectorNode* AParent) : TGLSectorNode(ASectoredSpacePartition, AParent) { }
	/* TGLSectorNode.Destroy */ inline __fastcall virtual ~TSPOctreeNode() { }
	
};


class PASCALIMPLEMENTATION TGLOctreeSpacePartition : public TGLSectoredSpacePartition
{
	typedef TGLSectoredSpacePartition inherited;
	
public:
	void __fastcall SetSize(const Gls::Vectortypes::TVector3f &Min, const Gls::Vectortypes::TVector3f &Max);
	virtual TGLSectorNode* __fastcall CreateNewNode(TGLSectorNode* AParent);
public:
	/* TGLSectoredSpacePartition.Create */ inline __fastcall virtual TGLOctreeSpacePartition() : TGLSectoredSpacePartition() { }
	/* TGLSectoredSpacePartition.Destroy */ inline __fastcall virtual ~TGLOctreeSpacePartition() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLOctreeSpacePartition(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLSectoredSpacePartition(reader) { }
	
};


class PASCALIMPLEMENTATION TSPQuadtreeNode : public TSPOctreeNode
{
	typedef TSPOctreeNode inherited;
	
protected:
	virtual void __fastcall ChildrenChanged();
	
public:
	virtual void __fastcall CreateChildren();
	virtual bool __fastcall AABBFitsInNode(const Gls::Geometrybb::TAABB &AAABB);
	virtual bool __fastcall AABBIntersectsNode(const Gls::Geometrybb::TAABB &AAABB);
	virtual bool __fastcall BSphereFitsInNode(const Gls::Geometrybb::TBSphere &BSphere);
	virtual bool __fastcall BSphereIntersectsNode(const Gls::Geometrybb::TBSphere &BSphere);
	virtual TGLSectorNode* __fastcall GetChildForAABB(const Gls::Geometrybb::TAABB &AABB);
public:
	/* TGLSectorNode.Create */ inline __fastcall TSPQuadtreeNode(TGLSectoredSpacePartition* ASectoredSpacePartition, TGLSectorNode* AParent) : TSPOctreeNode(ASectoredSpacePartition, AParent) { }
	/* TGLSectorNode.Destroy */ inline __fastcall virtual ~TSPQuadtreeNode() { }
	
};


class PASCALIMPLEMENTATION TGLQuadtreeSpacePartition : public TGLSectoredSpacePartition
{
	typedef TGLSectoredSpacePartition inherited;
	
public:
	void __fastcall SetSize(const Gls::Vectortypes::TVector3f &Min, const Gls::Vectortypes::TVector3f &Max);
	virtual TGLSectorNode* __fastcall CreateNewNode(TGLSectorNode* AParent);
public:
	/* TGLSectoredSpacePartition.Create */ inline __fastcall virtual TGLQuadtreeSpacePartition() : TGLSectoredSpacePartition() { }
	/* TGLSectoredSpacePartition.Destroy */ inline __fastcall virtual ~TGLQuadtreeSpacePartition() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLQuadtreeSpacePartition(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLSectoredSpacePartition(reader) { }
	
};


class PASCALIMPLEMENTATION TGLSceneObj : public TGLSpacePartitionLeaf
{
	typedef TGLSpacePartitionLeaf inherited;
	
public:
	Gls::Scene::TGLBaseSceneObject* Obj;
	virtual void __fastcall UpdateCachedAABBAndBSphere();
	__fastcall TGLSceneObj(TGLSectoredSpacePartition* Owner, Gls::Scene::TGLBaseSceneObject* aObj);
	__fastcall virtual ~TGLSceneObj();
public:
	/* TGLSpacePartitionLeaf.CreateOwned */ inline __fastcall TGLSceneObj(TGLBaseSpacePartition* SpacePartition) : TGLSpacePartitionLeaf(SpacePartition) { }
	
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TGLSceneObj() : TGLSpacePartitionLeaf() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLSceneObj(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLSpacePartitionLeaf(reader) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 COctree_LEAF_TRHESHOLD = System::Int8(0x1e);
static const System::Int8 COctree_MAX_TREE_DEPTH = System::Int8(0x8);
#define COctree_GROW_GRAVY  (1.000000E-01)
extern DELPHI_PACKAGE Gls::Geometrybb::TSpaceContains __fastcall ConeContainsBSphere(const TGLConeSP &Cone, const Gls::Geometrybb::TBSphere &BSphere);
extern DELPHI_PACKAGE bool __fastcall ExtendedFrustumIntersectsBSphere(const TGLExtendedFrustum &AExtendedFrustum, const Gls::Geometrybb::TBSphere &ABSphere);
extern DELPHI_PACKAGE TGLExtendedFrustum __fastcall ExtendedFrustumMake(const Gls::Vectorgeometry::TFrustum &AFrustum, const float ANearDist, const float AFarDist, const float AFieldOfViewRadians, const Gls::Vectortypes::TVector3f &ACameraPosition, const Gls::Vectortypes::TVector3f &ALookVector);
extern DELPHI_PACKAGE void __fastcall RenderAABB(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, const Gls::Geometrybb::TAABB &AABB)/* overload */;
extern DELPHI_PACKAGE void __fastcall RenderAABB(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, const Gls::Geometrybb::TAABB &AABB, float w, float r, float g, float b)/* overload */;
extern DELPHI_PACKAGE void __fastcall RenderSpatialPartitioning(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, TGLSectoredSpacePartition* const Space);
extern DELPHI_PACKAGE TGLExtendedFrustum __fastcall ExtendedFrustumMakeFromSceneViewer(const Gls::Vectorgeometry::TFrustum &AFrustum, Gls::Sceneviewer::TGLSceneViewer* const AGLSceneViewer)/* overload */;
extern DELPHI_PACKAGE TGLExtendedFrustum __fastcall ExtendedFrustumMakeFromSceneViewer(const Gls::Vectorgeometry::TFrustum &AFrustum, const int vWidth, const int vHeight, Gls::Scene::TGLCamera* AGLCamera)/* overload */;
}	/* namespace Spacepartition */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SPACEPARTITION)
using namespace Gls::Spacepartition;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_SpacepartitionHPP
