// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Mesh.pas' rev: 35.00 (Windows)

#ifndef Gls_MeshHPP
#define Gls_MeshHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.OpenGLext.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Strings.hpp>
#include <GLS.XOpenGL.hpp>
#include <GLS.Context.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.State.hpp>
#include <GLS.Color.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Mesh
{
//-- forward type declarations -----------------------------------------------
struct TGLVertexData;
class DELPHICLASS TGLVertexList;
class DELPHICLASS TGLMesh;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLMeshMode : unsigned char { mmTriangleStrip, mmTriangleFan, mmTriangles, mmQuadStrip, mmQuads, mmPolygon };

enum DECLSPEC_DENUM TGLVertexMode : unsigned char { vmV, vmVN, vmVNC, vmVNCT, vmVNT, vmVT };

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLVertexData
{
public:
	Gls::Vectorgeometry::TTexPoint textCoord;
	Gls::Vectortypes::TVector4f color;
	Gls::Vectortypes::TVector3f normal;
	Gls::Vectortypes::TVector3f coord;
};
#pragma pack(pop)


typedef TGLVertexData *PGLVertexData;

typedef System::StaticArray<TGLVertexData, 33554432> TGLVertexDataArray;

typedef TGLVertexDataArray *PGLVertexDataArray;

class PASCALIMPLEMENTATION TGLVertexList : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
public:
	TGLVertexData operator[](int index) { return this->Vertices[index]; }
	
private:
	TGLVertexDataArray *FValues;
	int FCount;
	int FCapacity;
	int FGrowth;
	TGLVertexDataArray *FLockedOldValues;
	
protected:
	void __fastcall SetCapacity(const int val);
	void __fastcall SetGrowth(const int val);
	void __fastcall Grow();
	void __fastcall SetVertices(int index, const TGLVertexData &val);
	TGLVertexData __fastcall GetVertices(int index);
	void __fastcall SetVertexCoord(int index, const Gls::Vectortypes::TVector3f &val);
	Gls::Vectortypes::TVector3f __fastcall GetVertexCoord(int index);
	void __fastcall SetVertexNormal(int index, const Gls::Vectortypes::TVector3f &val);
	Gls::Vectortypes::TVector3f __fastcall GetVertexNormal(int index);
	void __fastcall SetVertexTexCoord(int index, const Gls::Vectorgeometry::TTexPoint &val);
	Gls::Vectorgeometry::TTexPoint __fastcall GetVertexTexCoord(int index);
	void __fastcall SetVertexColor(int index, const Gls::Vectortypes::TVector4f &val);
	Gls::Vectortypes::TVector4f __fastcall GetVertexColor(int index);
	System::PSingle __fastcall GetFirstEntry();
	System::PSingle __fastcall GetFirstColor();
	System::PSingle __fastcall GetFirstNormal();
	System::PSingle __fastcall GetFirstVertex();
	System::PSingle __fastcall GetFirstTexPoint();
	bool __fastcall GetLocked();
	void __fastcall SetLocked(bool val);
	
public:
	__fastcall virtual TGLVertexList(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLVertexList();
	TGLVertexList* __fastcall CreateInterpolatedCoords(TGLVertexList* list2, float lerpFactor);
	void __fastcall AddVertex(const TGLVertexData &vertexData)/* overload */;
	void __fastcall AddVertex3(const TGLVertexData &vd1, const TGLVertexData &vd2, const TGLVertexData &vd3)/* overload */;
	void __fastcall AddVertex(const Gls::Vectortypes::TVector3f &aVertex, const Gls::Vectortypes::TVector3f &aNormal, const Gls::Vectortypes::TVector4f &aColor, const Gls::Vectorgeometry::TTexPoint &aTexPoint)/* overload */;
	void __fastcall AddVertex(const Gls::Vectortypes::TVector3f &vertex, const Gls::Vectortypes::TVector3f &normal, const Gls::Vectortypes::TVector4f &color)/* overload */;
	void __fastcall AddVertex(const Gls::Vectortypes::TVector3f &vertex, const Gls::Vectortypes::TVector3f &normal)/* overload */;
	void __fastcall DuplicateVertex(int index);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Clear();
	__property TGLVertexData Vertices[int index] = {read=GetVertices, write=SetVertices/*, default*/};
	__property Gls::Vectortypes::TVector3f VertexCoord[int index] = {read=GetVertexCoord, write=SetVertexCoord};
	__property Gls::Vectortypes::TVector3f VertexNormal[int index] = {read=GetVertexNormal, write=SetVertexNormal};
	__property Gls::Vectorgeometry::TTexPoint VertexTexCoord[int index] = {read=GetVertexTexCoord, write=SetVertexTexCoord};
	__property Gls::Vectortypes::TVector4f VertexColor[int index] = {read=GetVertexColor, write=SetVertexColor};
	__property int Count = {read=FCount, nodefault};
	__property int Capacity = {read=FCapacity, write=SetCapacity, nodefault};
	__property int Growth = {read=FGrowth, write=SetGrowth, nodefault};
	Gls::Vectortypes::TVector3f __fastcall SumVertexCoords();
	void __fastcall GetExtents(Gls::Vectortypes::TVector3f &min, Gls::Vectortypes::TVector3f &max);
	void __fastcall NormalizeNormals();
	void __fastcall Translate(const Gls::Vectortypes::TVector3f &v);
	void __fastcall DefineOpenGLArrays();
	__property System::PSingle FirstColor = {read=GetFirstColor};
	__property System::PSingle FirstEntry = {read=GetFirstEntry};
	__property System::PSingle FirstNormal = {read=GetFirstNormal};
	__property System::PSingle FirstVertex = {read=GetFirstVertex};
	__property System::PSingle FirstTexPoint = {read=GetFirstTexPoint};
	__property bool Locked = {read=GetLocked, write=SetLocked, nodefault};
	void __fastcall EnterLockSection();
	void __fastcall LeaveLockSection();
};


class PASCALIMPLEMENTATION TGLMesh : public Gls::Scene::TGLSceneObject
{
	typedef Gls::Scene::TGLSceneObject inherited;
	
private:
	TGLVertexList* FVertices;
	TGLMeshMode FMode;
	TGLVertexMode FVertexMode;
	Gls::Vectortypes::TVector4f FAxisAlignedDimensionsCache;
	
protected:
	void __fastcall SetMode(TGLMeshMode AValue);
	void __fastcall SetVertices(TGLVertexList* AValue);
	void __fastcall SetVertexMode(TGLVertexMode AValue);
	void __fastcall VerticesChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TGLMesh(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMesh();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall CalcNormals(Gls::State::TGLFaceWinding Frontface);
	__property TGLVertexList* Vertices = {read=FVertices, write=SetVertices};
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual void __fastcall StructureChanged();
	float __fastcall Length();
	float __fastcall Area();
	float __fastcall Volume();
	
__published:
	__property TGLMeshMode Mode = {read=FMode, write=SetMode, nodefault};
	__property TGLVertexMode VertexMode = {read=FVertexMode, write=SetVertexMode, default=3};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMesh(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::StaticArray<unsigned, 6> cMeshModeToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 6> cVertexModeToGLEnum;
}	/* namespace Mesh */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_MESH)
using namespace Gls::Mesh;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_MeshHPP
