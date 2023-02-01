// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.VerletClothify.pas' rev: 35.00 (Windows)

#ifndef Gls_VerletclothifyHPP
#define Gls_VerletclothifyHPP

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
#include <GLS.OpenGLTokens.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.VerletTypes.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Texture.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.Context.hpp>
#include <GLS.SpacePartition.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Verletclothify
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLFace;
class DELPHICLASS TGLFaceList;
class DELPHICLASS TGLFaceExtractor;
class DELPHICLASS TGLEdge;
class DELPHICLASS TGLEdgeList;
class DELPHICLASS TGLEdgeDetector;
class DELPHICLASS TGLMeshObjectVerletNode;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFace : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::StaticArray<int, 3> Vertices;
	Gls::Vectortypes::TVector3f Normal;
	Gls::Vectorfileobjects::TGLMeshObject* MeshObject;
	bool Active;
	void __fastcall UpdateNormal();
	__fastcall TGLFace(Gls::Vectorfileobjects::TGLMeshObject* aMeshObject);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLFace() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFaceList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TGLFace* operator[](int i) { return this->Items[i]; }
	
private:
	TGLFace* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TGLFace* const Value);
	
public:
	__property TGLFace* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TGLFaceList() { }
	
public:
	/* TObject.Create */ inline __fastcall TGLFaceList() : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFaceExtractor : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLFaceList* FFaceList;
	Gls::Vectorfileobjects::TGLBaseMesh* FGLBaseMesh;
	Gls::Verlettypes::TGLVerletNodeList* FNodeList;
	float FWeldDistance;
	int FEdgeDoublesSkipped;
	void __fastcall SetWeldDistance(const float Value);
	
protected:
	virtual void __fastcall ProcessMeshObject(Gls::Vectorfileobjects::TGLMeshObject* const MeshObject);
	
public:
	void __fastcall ExtractFacesFromVertexIndexList(Gls::Vectorfileobjects::TFGVertexIndexList* const FaceGroup, Gls::Vectorfileobjects::TGLMeshObject* const MeshObject);
	__property TGLFaceList* FaceList = {read=FFaceList};
	virtual void __fastcall Clear();
	virtual void __fastcall ProcessMesh();
	__property float WeldDistance = {read=FWeldDistance, write=SetWeldDistance};
	__property int EdgeDoublesSkipped = {read=FEdgeDoublesSkipped, nodefault};
	__property Gls::Vectorfileobjects::TGLBaseMesh* GLBaseMesh = {read=FGLBaseMesh};
	__property Gls::Verlettypes::TGLVerletNodeList* NodeList = {read=FNodeList};
	virtual TGLFace* __fastcall AddFace(const int Vi0, const int Vi1, const int Vi2, Gls::Vectorfileobjects::TGLMeshObject* const MeshObject);
	__fastcall virtual TGLFaceExtractor(Gls::Vectorfileobjects::TGLBaseMesh* const aGLBaseMesh);
	__fastcall virtual ~TGLFaceExtractor();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLEdge : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FSolid;
	float FLength;
	Gls::Vectorfileobjects::TGLMeshObject* FMeshObject;
	TGLEdgeDetector* FOwner;
	
public:
	System::StaticArray<int, 2> Vertices;
	System::StaticArray<TGLFace*, 2> Faces;
	void __fastcall Contract();
	__property TGLEdgeDetector* Owner = {read=FOwner};
	__property Gls::Vectorfileobjects::TGLMeshObject* MeshObject = {read=FMeshObject, write=FMeshObject};
	__property float Length = {read=FLength, write=FLength};
	__property bool Solid = {read=FSolid, write=FSolid, nodefault};
	void __fastcall UpdateEdgeLength();
	__fastcall TGLEdge(TGLEdgeDetector* const AOwner, int AVi0, int AVi1, TGLFace* AFace0, TGLFace* AFace1, Gls::Vectorfileobjects::TGLMeshObject* aMeshObject, bool ASolid);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLEdge() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLEdgeList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TGLEdge* operator[](int i) { return this->Items[i]; }
	
private:
	TGLEdge* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TGLEdge* const Value);
	
public:
	__property TGLEdge* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall SortByLength();
	int __fastcall InsertSorted(TGLEdge* AEdge);
public:
	/* TList.Destroy */ inline __fastcall virtual ~TGLEdgeList() { }
	
public:
	/* TObject.Create */ inline __fastcall TGLEdgeList() : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLEdgeDetector : public TGLFaceExtractor
{
	typedef TGLFaceExtractor inherited;
	
private:
	TGLEdgeList* FEdgeList;
	int FCurrentNodeOffset;
	bool FNodesAdded;
	void __fastcall BuildOpposingEdges();
	
protected:
	bool FCalcEdgeLength;
	
public:
	__property TGLEdgeList* EdgeList = {read=FEdgeList};
	virtual void __fastcall Clear();
	virtual void __fastcall ProcessMesh();
	TGLEdge* __fastcall AddEdge(const int Vi0, const int Vi1, TGLFace* const Face, Gls::Vectorfileobjects::TGLMeshObject* const aMeshObject);
	virtual TGLFace* __fastcall AddFace(const int Vi0, const int Vi1, const int Vi2, Gls::Vectorfileobjects::TGLMeshObject* const MeshObject);
	virtual Gls::Verlettypes::TGLVerletNode* __fastcall AddNode(Gls::Verlettypes::TGLVerletWorld* const VerletWorld, Gls::Vectorfileobjects::TGLMeshObject* const MeshObject, const int VertexIndex);
	void __fastcall AddNodes(Gls::Verlettypes::TGLVerletWorld* const VerletWorld);
	void __fastcall AddEdgesAsSticks(Gls::Verlettypes::TGLVerletWorld* const VerletWorld, const float Slack);
	void __fastcall AddEdgesAsSprings(Gls::Verlettypes::TGLVerletWorld* const VerletWorld, const float Strength, const float Damping, const float Slack);
	void __fastcall AddEdgesAsSolidEdges(Gls::Verlettypes::TGLVerletWorld* const VerletWorld);
	void __fastcall AddOuterEdgesAsSolidEdges(Gls::Verlettypes::TGLVerletWorld* const VerletWorld);
	void __fastcall RenderEdges(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property int CurrentNodeOffset = {read=FCurrentNodeOffset, nodefault};
	__property bool NodesAdded = {read=FNodesAdded, nodefault};
	void __fastcall ReplaceVertexIndex(const int ViRemove, const int ViReplaceWith);
	__fastcall virtual TGLEdgeDetector(Gls::Vectorfileobjects::TGLBaseMesh* const aGLBaseMesh);
	__fastcall virtual ~TGLEdgeDetector();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMeshObjectVerletNode : public Gls::Verlettypes::TGLVerletNode
{
	typedef Gls::Verlettypes::TGLVerletNode inherited;
	
private:
	Gls::Vectorfileobjects::TGLMeshObject* MeshObject;
	Gls::Vectorlists::TGLIntegerList* VertexIndices;
	
public:
	virtual void __fastcall AfterProgress();
	__fastcall virtual TGLMeshObjectVerletNode(Gls::Verlettypes::TGLVerletWorld* const AOwner);
	__fastcall virtual ~TGLMeshObjectVerletNode();
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TGLMeshObjectVerletNode() : Gls::Verlettypes::TGLVerletNode() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLMeshObjectVerletNode(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Verlettypes::TGLVerletNode(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Verletclothify */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_VERLETCLOTHIFY)
using namespace Gls::Verletclothify;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_VerletclothifyHPP
