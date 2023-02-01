// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.MeshLines.pas' rev: 35.00 (Windows)

#ifndef Gls_MeshlinesHPP
#define Gls_MeshlinesHPP

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
#include <GLS.Scene.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.Context.hpp>
#include <GLS.Material.hpp>
#include <GLS.Color.hpp>
#include <GLS.State.hpp>
#include <GLS.Nodes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Spline.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Meshlines
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLLineNode;
class DELPHICLASS TGLLineNodes;
class DELPHICLASS TGLLineItem;
class DELPHICLASS TGLLineCollection;
class DELPHICLASS TGLLightmapBounds;
class DELPHICLASS TGLMeshLines;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLLineOperation : unsigned char { loSelectLine, loNewLine, loInsertNode, loMoveNode };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLineNode : public Gls::Nodes::TGLNode
{
	typedef Gls::Nodes::TGLNode inherited;
	
private:
	void *FData;
	
public:
	__fastcall virtual TGLLineNode(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLLineNode();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property void * Data = {read=FData, write=FData};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLineNodes : public Gls::Nodes::TGLNodes
{
	typedef Gls::Nodes::TGLNodes inherited;
	
public:
	__fastcall TGLLineNodes(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall virtual ~TGLLineNodes();
	virtual void __fastcall NotifyChange();
	int __fastcall IndexOf(TGLLineNode* LineNode);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLineItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	float FBreakAngle;
	int FDivision;
	TGLLineNodes* FNodes;
	Gls::Objects::TGLLineSplineMode FSplineMode;
	float FTextureLength;
	float FWidth;
	bool FTextureCorrection;
	bool FHide;
	void *FData;
	void __fastcall SetHide(const bool Value);
	void __fastcall SetTextureCorrection(const bool Value);
	void __fastcall SetBreakAngle(const float Value);
	void __fastcall SetDivision(const int Value);
	void __fastcall SetNodes(TGLLineNodes* const Value);
	void __fastcall SetSplineMode(const Gls::Objects::TGLLineSplineMode Value);
	void __fastcall SetTextureLength(const float Value);
	void __fastcall SetWidth(const float Value);
	
protected:
	virtual void __fastcall DoChanged();
	
public:
	__property void * Data = {read=FData, write=FData};
	
__published:
	__fastcall virtual TGLLineItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLLineItem();
	__property bool Hide = {read=FHide, write=SetHide, nodefault};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property bool TextureCorrection = {read=FTextureCorrection, write=SetTextureCorrection, nodefault};
	__property float BreakAngle = {read=FBreakAngle, write=SetBreakAngle};
	__property int Division = {read=FDivision, write=SetDivision, nodefault};
	__property TGLLineNodes* Nodes = {read=FNodes, write=SetNodes};
	__property Gls::Objects::TGLLineSplineMode SplineMode = {read=FSplineMode, write=SetSplineMode, nodefault};
	__property float TextureLength = {read=FTextureLength, write=SetTextureLength};
	__property float Width = {read=FWidth, write=SetWidth};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLineCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLLineItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	void __fastcall SetItems(int Index, TGLLineItem* const Val);
	TGLLineItem* __fastcall GetItems(int Index);
	
public:
	HIDESBASE TGLLineItem* __fastcall Add()/* overload */;
	HIDESBASE TGLLineItem* __fastcall Add(System::UnicodeString Name)/* overload */;
	__property TGLLineItem* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TOwnedCollection.Create */ inline __fastcall TGLLineCollection(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLLineCollection() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLLightmapBounds : public Gls::Coordinates::TGLCustomCoordinates
{
	typedef Gls::Coordinates::TGLCustomCoordinates inherited;
	
private:
	float __fastcall GetLeft();
	float __fastcall GetTop();
	float __fastcall GetRight();
	float __fastcall GetBottom();
	float __fastcall GetWidth();
	float __fastcall GetHeight();
	void __fastcall SetLeft(const float Value);
	void __fastcall SetTop(const float Value);
	void __fastcall SetRight(const float Value);
	void __fastcall SetBottom(const float Value);
	
__published:
	__property float Left = {read=GetLeft, write=SetLeft, stored=false};
	__property float Top = {read=GetTop, write=SetTop, stored=false};
	__property float Right = {read=GetRight, write=SetRight, stored=false};
	__property float Bottom = {read=GetBottom, write=SetBottom, stored=false};
	__property float Width = {read=GetWidth};
	__property float Height = {read=GetHeight};
public:
	/* TGLCustomCoordinates.CreateInitialized */ inline __fastcall TGLLightmapBounds(System::Classes::TPersistent* AOwner, const Gls::Vectortypes::TVector4f &AValue, const Gls::Coordinates::TGLCoordinatesStyle AStyle) : Gls::Coordinates::TGLCustomCoordinates(AOwner, AValue, AStyle) { }
	/* TGLCustomCoordinates.Destroy */ inline __fastcall virtual ~TGLLightmapBounds() { }
	
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLLightmapBounds(System::Classes::TPersistent* AOwner) : Gls::Coordinates::TGLCustomCoordinates(AOwner) { }
	
};


class PASCALIMPLEMENTATION TGLMeshLines : public Gls::Vectorfileobjects::TGLFreeForm
{
	typedef Gls::Vectorfileobjects::TGLFreeForm inherited;
	
private:
	TGLLineCollection* FLines;
	Gls::Vectorfileobjects::TGLMeshObject* FMesh;
	TGLLightmapBounds* FLightmapBounds;
	int FLightmapIndex;
	System::UnicodeString FLightmapMaterialName;
	Gls::Vectorfileobjects::TFGVertexIndexList* FFaceGroup;
	int FIndex;
	bool FNoZWrite;
	bool FShowNodes;
	int FUpdating;
	TGLLineItem* FSelectedLineItem;
	TGLLineNode* FSelectedNode;
	TGLLineNode* FNode1;
	TGLLineNode* FNode2;
	bool __fastcall GetUpdating();
	bool __fastcall PointNearLine(TGLLineItem* const LineItem, const float X, const float Z, float Tolerance = 1.000000E+00f);
	bool __fastcall PointNearSegment(TGLLineNode* const StartNode, TGLLineNode* const EndNode, const float X, const float Z, float LineWidth, float Tolerance = 1.000000E+00f);
	void __fastcall StitchStrips(Gls::Vectorlists::TGLIntegerList* idx);
	void __fastcall AddStitchMarker(Gls::Vectorlists::TGLIntegerList* idx);
	void __fastcall SetShowNodes(const bool Value);
	void __fastcall SetNoZWrite(const bool Value);
	void __fastcall SetLightmapIndex(const int Value);
	void __fastcall SetLightmapMaterialName(const System::UnicodeString Value);
	void __fastcall SetLightmapBounds(TGLLightmapBounds* const Value);
	void __fastcall DoChanged();
	void __fastcall AddIndex();
	void __fastcall AddVertices(const Gls::Vectortypes::TVector3f &Up, const Gls::Vectortypes::TVector3f &Inner, const Gls::Vectortypes::TVector3f &Outer, float S, float Correction, bool UseDegenerate, TGLLineItem* LineItem);
	void __fastcall BuildLineItem(TGLLineItem* LineItem);
	void __fastcall BuildGeometry();
	void __fastcall DrawNode(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, TGLLineNode* Node, float LineWidth);
	void __fastcall DrawCircle(float Radius);
	TGLLineNode* __fastcall SelectNode(TGLLineItem* LineItem, float X, float Z);
	
protected:
	virtual void __fastcall Loaded();
	
public:
	HIDESBASE void __fastcall BeginUpdate();
	HIDESBASE void __fastcall EndUpdate();
	void __fastcall Clear();
	TGLLineItem* __fastcall SelectLineItem(const float X, const float Z, float Tolerance = 1.000000E+00f)/* overload */;
	TGLLineItem* __fastcall SelectLineItem(TGLLineItem* LineItem)/* overload */;
	TGLLineItem* __fastcall SelectLineItem(TGLLineNode* LineNode)/* overload */;
	void __fastcall DeselectLineItem();
	void __fastcall DeselectLineNode();
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property TGLLineItem* SelectedLineItem = {read=FSelectedLineItem};
	__property TGLLineNode* SelectedNode = {read=FSelectedNode};
	__property TGLLineNode* Node1 = {read=FNode1};
	__property TGLLineNode* Node2 = {read=FNode2};
	
__published:
	__fastcall virtual TGLMeshLines(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMeshLines();
	__property bool Updating = {read=GetUpdating, nodefault};
	__property TGLLineCollection* Lines = {read=FLines};
	__property Material;
	__property TGLLightmapBounds* LightmapBounds = {read=FLightmapBounds, write=SetLightmapBounds};
	__property int LightmapIndex = {read=FLightmapIndex, write=SetLightmapIndex, nodefault};
	__property System::UnicodeString LightmapMaterialName = {read=FLightmapMaterialName, write=SetLightmapMaterialName};
	__property bool NoZWrite = {read=FNoZWrite, write=SetNoZWrite, nodefault};
	__property bool ShowNodes = {read=FShowNodes, write=SetShowNodes, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMeshLines(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Vectorfileobjects::TGLFreeForm(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Meshlines */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_MESHLINES)
using namespace Gls::Meshlines;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_MeshlinesHPP
