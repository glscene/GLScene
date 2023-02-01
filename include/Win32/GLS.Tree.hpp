// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Tree.pas' rev: 35.00 (Windows)

#ifndef Gls_TreeHPP
#define Gls_TreeHPP

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
#include <GLS.State.hpp>
#include <GLS.Material.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.XOpenGL.hpp>
#include <GLS.Context.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Utils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Tree
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTreeLeaves;
class DELPHICLASS TGLTreeBranch;
class DELPHICLASS TGLTreeBranches;
class DELPHICLASS TGLTreeBranchNoise;
class DELPHICLASS TGLTree;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTreeLeaves : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLTree* FOwner;
	int FCount;
	Gls::Vectorlists::TGLAffineVectorList* FVertices;
	Gls::Vectorlists::TGLAffineVectorList* FNormals;
	Gls::Vectorlists::TGLAffineVectorList* FTexCoords;
	
public:
	__fastcall TGLTreeLeaves(TGLTree* AOwner);
	__fastcall virtual ~TGLTreeLeaves();
	void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall AddNew(const Gls::Vectortypes::TMatrix4f &matrix);
	void __fastcall Clear();
	__property TGLTree* Owner = {read=FOwner};
	__property int Count = {read=FCount, nodefault};
	__property Gls::Vectorlists::TGLAffineVectorList* Vertices = {read=FVertices};
	__property Gls::Vectorlists::TGLAffineVectorList* Normals = {read=FNormals};
	__property Gls::Vectorlists::TGLAffineVectorList* TexCoords = {read=FTexCoords};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTreeBranch : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLTreeBranches* FOwner;
	TGLTreeBranch* FLeft;
	TGLTreeBranch* FCenter;
	TGLTreeBranch* FRight;
	TGLTreeBranch* FParent;
	int FBranchID;
	int FParentID;
	Gls::Vectortypes::TMatrix4f FMatrix;
	Gls::Vectorlists::TGLIntegerList* FLower;
	Gls::Vectorlists::TGLIntegerList* FUpper;
	bool FCentralLeader;
	void __fastcall BuildBranch(TGLTreeBranchNoise* branchNoise, const Gls::Vectortypes::TMatrix4f &matrix, float TexCoordY, float Twist, int Level);
	
public:
	__fastcall TGLTreeBranch(TGLTreeBranches* AOwner, TGLTreeBranch* AParent);
	__fastcall virtual ~TGLTreeBranch();
	__property TGLTreeBranches* Owner = {read=FOwner};
	__property TGLTreeBranch* Left = {read=FLeft};
	__property TGLTreeBranch* Center = {read=FCenter};
	__property TGLTreeBranch* Right = {read=FRight};
	__property TGLTreeBranch* Parent = {read=FParent};
	__property Gls::Vectortypes::TMatrix4f matrix = {read=FMatrix};
	__property Gls::Vectorlists::TGLIntegerList* Lower = {read=FLower};
	__property Gls::Vectorlists::TGLIntegerList* Upper = {read=FUpper};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTreeBranches : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLTree* FOwner;
	Gls::Vectorlists::TGLSingleList* FSinList;
	Gls::Vectorlists::TGLSingleList* FCosList;
	Gls::Vectorlists::TGLAffineVectorList* FVertices;
	Gls::Vectorlists::TGLAffineVectorList* FNormals;
	Gls::Vectorlists::TGLAffineVectorList* FTexCoords;
	Gls::Vectorlists::TGLIntegerList* FIndices;
	TGLTreeBranch* FRoot;
	int FCount;
	System::Classes::TList* FBranchCache;
	Gls::Vectorlists::TGLIntegerList* FBranchIndices;
	void __fastcall BuildBranches();
	
public:
	__fastcall TGLTreeBranches(TGLTree* AOwner);
	__fastcall virtual ~TGLTreeBranches();
	void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall Clear();
	__property TGLTree* Owner = {read=FOwner};
	__property Gls::Vectorlists::TGLSingleList* SinList = {read=FSinList};
	__property Gls::Vectorlists::TGLSingleList* CosList = {read=FCosList};
	__property Gls::Vectorlists::TGLAffineVectorList* Vertices = {read=FVertices};
	__property Gls::Vectorlists::TGLAffineVectorList* Normals = {read=FNormals};
	__property Gls::Vectorlists::TGLAffineVectorList* TexCoords = {read=FTexCoords};
	__property int Count = {read=FCount, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTreeBranchNoise : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	float FBranchNoise;
	TGLTreeBranchNoise* FLeft;
	TGLTreeBranchNoise* FRight;
	TGLTreeBranchNoise* FCenter;
	TGLTreeBranchNoise* __fastcall GetLeft();
	TGLTreeBranchNoise* __fastcall GetCenter();
	TGLTreeBranchNoise* __fastcall GetRight();
	
public:
	__fastcall TGLTreeBranchNoise();
	__fastcall virtual ~TGLTreeBranchNoise();
	__property TGLTreeBranchNoise* Left = {read=GetLeft};
	__property TGLTreeBranchNoise* Center = {read=GetCenter};
	__property TGLTreeBranchNoise* Right = {read=GetRight};
	__property float branchNoise = {read=FBranchNoise};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLTree : public Gls::Scene::TGLImmaterialSceneObject
{
	typedef Gls::Scene::TGLImmaterialSceneObject inherited;
	
private:
	int FDepth;
	int FBranchFacets;
	float FLeafSize;
	float FBranchSize;
	float FBranchNoise;
	float FBranchAngleBias;
	float FBranchAngle;
	float FBranchTwist;
	float FBranchRadius;
	float FLeafThreshold;
	float FCentralLeaderBias;
	bool FCentralLeader;
	int FSeed;
	bool FAutoCenter;
	bool FAutoRebuild;
	float FCenterBranchConstant;
	TGLTreeLeaves* FLeaves;
	TGLTreeBranches* FBranches;
	TGLTreeBranchNoise* FNoise;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FLeafMaterialName;
	System::UnicodeString FLeafBackMaterialName;
	System::UnicodeString FBranchMaterialName;
	bool FRebuildTree;
	Gls::Vectortypes::TVector4f FAxisAlignedDimensionsCache;
	
protected:
	void __fastcall SetDepth(const int Value);
	void __fastcall SetBranchFacets(const int Value);
	void __fastcall SetLeafSize(const float Value);
	void __fastcall SetBranchSize(const float Value);
	void __fastcall SetBranchNoise(const float Value);
	void __fastcall SetBranchAngleBias(const float Value);
	void __fastcall SetBranchAngle(const float Value);
	void __fastcall SetBranchTwist(const float Value);
	void __fastcall SetBranchRadius(const float Value);
	void __fastcall SetLeafThreshold(const float Value);
	void __fastcall SetCentralLeaderBias(const float Value);
	void __fastcall SetCentralLeader(const bool Value);
	void __fastcall SetSeed(const int Value);
	void __fastcall SetAutoCenter(const bool Value);
	void __fastcall SetAutoRebuild(const bool Value);
	void __fastcall SetCenterBranchConstant(const float Value);
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const Value);
	void __fastcall SetLeafMaterialName(const System::UnicodeString Value);
	void __fastcall SetLeafBackMaterialName(const System::UnicodeString Value);
	void __fastcall SetBranchMaterialName(const System::UnicodeString Value);
	virtual void __fastcall Loaded();
	
public:
	__fastcall virtual TGLTree(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTree();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall StructureChanged();
	void __fastcall BuildMesh(Gls::Vectorfileobjects::TGLBaseMesh* GLBaseMesh);
	void __fastcall RebuildTree();
	void __fastcall ForceTotalRebuild();
	void __fastcall Clear();
	void __fastcall GetExtents(Gls::Vectortypes::TVector3f &min, Gls::Vectortypes::TVector3f &max);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	void __fastcall SaveToStream(System::Classes::TStream* aStream);
	void __fastcall LoadFromFile(const System::UnicodeString aFileName);
	void __fastcall SaveToFile(const System::UnicodeString aFileName);
	__property TGLTreeLeaves* Leaves = {read=FLeaves};
	__property TGLTreeBranches* Branches = {read=FBranches};
	__property TGLTreeBranchNoise* Noise = {read=FNoise};
	
__published:
	__property int Depth = {read=FDepth, write=SetDepth, nodefault};
	__property int BranchFacets = {read=FBranchFacets, write=SetBranchFacets, nodefault};
	__property float LeafSize = {read=FLeafSize, write=SetLeafSize};
	__property float BranchSize = {read=FBranchSize, write=SetBranchSize};
	__property float BranchNoise = {read=FBranchNoise, write=SetBranchNoise};
	__property float BranchAngleBias = {read=FBranchAngleBias, write=SetBranchAngleBias};
	__property float BranchAngle = {read=FBranchAngle, write=SetBranchAngle};
	__property float BranchTwist = {read=FBranchTwist, write=SetBranchTwist};
	__property float BranchRadius = {read=FBranchRadius, write=SetBranchRadius};
	__property float LeafThreshold = {read=FLeafThreshold, write=SetLeafThreshold};
	__property float CentralLeaderBias = {read=FCentralLeaderBias, write=SetCentralLeaderBias};
	__property bool CentralLeader = {read=FCentralLeader, write=SetCentralLeader, nodefault};
	__property int Seed = {read=FSeed, write=SetSeed, nodefault};
	__property bool AutoCenter = {read=FAutoCenter, write=SetAutoCenter, nodefault};
	__property bool AutoRebuild = {read=FAutoRebuild, write=SetAutoRebuild, nodefault};
	__property float CenterBranchConstant = {read=FCenterBranchConstant, write=SetCenterBranchConstant};
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString LeafMaterialName = {read=FLeafMaterialName, write=SetLeafMaterialName};
	__property System::UnicodeString LeafBackMaterialName = {read=FLeafBackMaterialName, write=SetLeafBackMaterialName};
	__property System::UnicodeString BranchMaterialName = {read=FBranchMaterialName, write=SetBranchMaterialName};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTree(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tree */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_TREE)
using namespace Gls::Tree;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_TreeHPP
