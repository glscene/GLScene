// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.GeomObjects.pas' rev: 35.00 (Windows)

#ifndef Gls_GeomobjectsHPP
#define Gls_GeomobjectsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Math.hpp>
#include <System.Classes.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.OpenGLAdapter.hpp>
#include <GLS.Scene.hpp>
#include <GLS.State.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.Polynomials.hpp>
#include <GLS.Silhouette.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.GeometryBB.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.Material.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Context.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Mesh.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.XOpenGL.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Geomobjects
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTetrahedron;
class DELPHICLASS TGLOctahedron;
class DELPHICLASS TGLHexahedron;
class DELPHICLASS TGLDodecahedron;
class DELPHICLASS TGLIcosahedron;
class DELPHICLASS TGLDisk;
class DELPHICLASS TGLCylinderBase;
class DELPHICLASS TGLCone;
class DELPHICLASS TGLCylinder;
class DELPHICLASS TGLCapsule;
class DELPHICLASS TGLAnnulus;
class DELPHICLASS TGLTorus;
class DELPHICLASS TGLArrowLine;
class DELPHICLASS TGLArrowArc;
class DELPHICLASS TGLPolygon;
class DELPHICLASS TGLFrustrum;
class DELPHICLASS TGLTeapot;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLTetrahedron : public Gls::Vectorfileobjects::TGLBaseMesh
{
	typedef Gls::Vectorfileobjects::TGLBaseMesh inherited;
	
public:
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TGLBaseMesh.Create */ inline __fastcall virtual TGLTetrahedron(System::Classes::TComponent* AOwner) : Gls::Vectorfileobjects::TGLBaseMesh(AOwner) { }
	/* TGLBaseMesh.Destroy */ inline __fastcall virtual ~TGLTetrahedron() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTetrahedron(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Vectorfileobjects::TGLBaseMesh(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLOctahedron : public Gls::Vectorfileobjects::TGLBaseMesh
{
	typedef Gls::Vectorfileobjects::TGLBaseMesh inherited;
	
public:
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TGLBaseMesh.Create */ inline __fastcall virtual TGLOctahedron(System::Classes::TComponent* AOwner) : Gls::Vectorfileobjects::TGLBaseMesh(AOwner) { }
	/* TGLBaseMesh.Destroy */ inline __fastcall virtual ~TGLOctahedron() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLOctahedron(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Vectorfileobjects::TGLBaseMesh(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLHexahedron : public Gls::Vectorfileobjects::TGLBaseMesh
{
	typedef Gls::Vectorfileobjects::TGLBaseMesh inherited;
	
public:
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TGLBaseMesh.Create */ inline __fastcall virtual TGLHexahedron(System::Classes::TComponent* AOwner) : Gls::Vectorfileobjects::TGLBaseMesh(AOwner) { }
	/* TGLBaseMesh.Destroy */ inline __fastcall virtual ~TGLHexahedron() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLHexahedron(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Vectorfileobjects::TGLBaseMesh(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLDodecahedron : public Gls::Vectorfileobjects::TGLBaseMesh
{
	typedef Gls::Vectorfileobjects::TGLBaseMesh inherited;
	
public:
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TGLBaseMesh.Create */ inline __fastcall virtual TGLDodecahedron(System::Classes::TComponent* AOwner) : Gls::Vectorfileobjects::TGLBaseMesh(AOwner) { }
	/* TGLBaseMesh.Destroy */ inline __fastcall virtual ~TGLDodecahedron() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLDodecahedron(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Vectorfileobjects::TGLBaseMesh(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLIcosahedron : public Gls::Vectorfileobjects::TGLBaseMesh
{
	typedef Gls::Vectorfileobjects::TGLBaseMesh inherited;
	
public:
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TGLBaseMesh.Create */ inline __fastcall virtual TGLIcosahedron(System::Classes::TComponent* AOwner) : Gls::Vectorfileobjects::TGLBaseMesh(AOwner) { }
	/* TGLBaseMesh.Destroy */ inline __fastcall virtual ~TGLIcosahedron() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLIcosahedron(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Vectorfileobjects::TGLBaseMesh(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLDisk : public Gls::Objects::TGLQuadricObject
{
	typedef Gls::Objects::TGLQuadricObject inherited;
	
private:
	float FStartAngle;
	float FSweepAngle;
	float FOuterRadius;
	float FInnerRadius;
	int FSlices;
	int FLoops;
	void __fastcall SetOuterRadius(const float aValue);
	void __fastcall SetInnerRadius(const float aValue);
	void __fastcall SetSlices(int aValue);
	void __fastcall SetLoops(int aValue);
	void __fastcall SetStartAngle(const float aValue);
	void __fastcall SetSweepAngle(const float aValue);
	
public:
	__fastcall virtual TGLDisk(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	
__published:
	__property float InnerRadius = {read=FInnerRadius, write=SetInnerRadius};
	__property int Loops = {read=FLoops, write=SetLoops, default=2};
	__property float OuterRadius = {read=FOuterRadius, write=SetOuterRadius};
	__property int Slices = {read=FSlices, write=SetSlices, default=16};
	__property float StartAngle = {read=FStartAngle, write=SetStartAngle};
	__property float SweepAngle = {read=FSweepAngle, write=SetSweepAngle};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLDisk() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLDisk(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Objects::TGLQuadricObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLCylinderBase : public Gls::Objects::TGLQuadricObject
{
	typedef Gls::Objects::TGLQuadricObject inherited;
	
private:
	float FBottomRadius;
	int FSlices;
	int FStacks;
	int FLoops;
	float FHeight;
	
protected:
	void __fastcall SetBottomRadius(const float aValue);
	void __fastcall SetHeight(const float aValue);
	void __fastcall SetSlices(int aValue);
	void __fastcall SetStacks(int aValue);
	void __fastcall SetLoops(int aValue);
	virtual float __fastcall GetTopRadius();
	
public:
	__fastcall virtual TGLCylinderBase(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Gls::Silhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Gls::Silhouette::TGLSilhouetteParameters &silhouetteParameters);
	
__published:
	__property float BottomRadius = {read=FBottomRadius, write=SetBottomRadius};
	__property float Height = {read=FHeight, write=SetHeight};
	__property int Slices = {read=FSlices, write=SetSlices, default=16};
	__property int Stacks = {read=FStacks, write=SetStacks, default=4};
	__property int Loops = {read=FLoops, write=SetLoops, default=1};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLCylinderBase() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCylinderBase(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Objects::TGLQuadricObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLConePart : unsigned char { coSides, coBottom };

typedef System::Set<TGLConePart, TGLConePart::coSides, TGLConePart::coBottom> TGLConeParts;

class PASCALIMPLEMENTATION TGLCone : public TGLCylinderBase
{
	typedef TGLCylinderBase inherited;
	
private:
	TGLConeParts FParts;
	
protected:
	void __fastcall SetParts(TGLConeParts aValue);
	virtual float __fastcall GetTopRadius();
	
public:
	__fastcall virtual TGLCone(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	
__published:
	__property TGLConeParts Parts = {read=FParts, write=SetParts, default=3};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLCone() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCone(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLCylinderBase(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLCylinderPart : unsigned char { cySides, cyBottom, cyTop };

typedef System::Set<TGLCylinderPart, TGLCylinderPart::cySides, TGLCylinderPart::cyTop> TGLCylinderParts;

enum DECLSPEC_DENUM TGLCylinderAlignment : unsigned char { caCenter, caTop, caBottom };

class PASCALIMPLEMENTATION TGLCylinder : public TGLCylinderBase
{
	typedef TGLCylinderBase inherited;
	
private:
	TGLCylinderParts FParts;
	float FTopRadius;
	TGLCylinderAlignment FAlignment;
	
protected:
	void __fastcall SetTopRadius(const float aValue);
	void __fastcall SetParts(TGLCylinderParts aValue);
	void __fastcall SetAlignment(TGLCylinderAlignment val);
	virtual float __fastcall GetTopRadius();
	
public:
	__fastcall virtual TGLCylinder(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	void __fastcall Align(const Gls::Vectortypes::TVector4f &startPoint, const Gls::Vectortypes::TVector4f &endPoint)/* overload */;
	void __fastcall Align(Gls::Scene::TGLBaseSceneObject* const startObj, Gls::Scene::TGLBaseSceneObject* const endObj)/* overload */;
	void __fastcall Align(const Gls::Vectortypes::TVector3f &startPoint, const Gls::Vectortypes::TVector3f &endPoint)/* overload */;
	
__published:
	__property float TopRadius = {read=FTopRadius, write=SetTopRadius};
	__property TGLCylinderParts Parts = {read=FParts, write=SetParts, default=7};
	__property TGLCylinderAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLCylinder() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCylinder(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLCylinderBase(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLCapsule : public Gls::Scene::TGLSceneObject
{
	typedef Gls::Scene::TGLSceneObject inherited;
	
private:
	TGLCylinderParts FParts;
	float FRadius;
	int FSlices;
	int FStacks;
	float FHeight;
	TGLCylinderAlignment FAlignment;
	
protected:
	void __fastcall SetHeight(const float aValue);
	void __fastcall SetRadius(const float aValue);
	void __fastcall SetSlices(const int aValue);
	void __fastcall SetStacks(const int aValue);
	void __fastcall SetParts(TGLCylinderParts aValue);
	void __fastcall SetAlignment(TGLCylinderAlignment val);
	
public:
	__fastcall virtual TGLCapsule(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	void __fastcall Align(const Gls::Vectortypes::TVector4f &startPoint, const Gls::Vectortypes::TVector4f &endPoint)/* overload */;
	void __fastcall Align(Gls::Scene::TGLBaseSceneObject* const startObj, Gls::Scene::TGLBaseSceneObject* const endObj)/* overload */;
	void __fastcall Align(const Gls::Vectortypes::TVector3f &startPoint, const Gls::Vectortypes::TVector3f &endPoint)/* overload */;
	
__published:
	__property float Height = {read=FHeight, write=SetHeight};
	__property int Slices = {read=FSlices, write=SetSlices, nodefault};
	__property int Stacks = {read=FStacks, write=SetStacks, nodefault};
	__property float Radius = {read=FRadius, write=SetRadius};
	__property TGLCylinderParts Parts = {read=FParts, write=SetParts, default=7};
	__property TGLCylinderAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLCapsule() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCapsule(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLSceneObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLAnnulusPart : unsigned char { anInnerSides, anOuterSides, anBottom, anTop };

typedef System::Set<TGLAnnulusPart, TGLAnnulusPart::anInnerSides, TGLAnnulusPart::anTop> TGLAnnulusParts;

class PASCALIMPLEMENTATION TGLAnnulus : public TGLCylinderBase
{
	typedef TGLCylinderBase inherited;
	
private:
	TGLAnnulusParts FParts;
	float FBottomInnerRadius;
	float FTopInnerRadius;
	float FTopRadius;
	
protected:
	void __fastcall SetTopRadius(const float aValue);
	void __fastcall SetTopInnerRadius(const float aValue);
	void __fastcall SetBottomInnerRadius(const float aValue);
	void __fastcall SetParts(TGLAnnulusParts aValue);
	
public:
	__fastcall virtual TGLAnnulus(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	
__published:
	__property float BottomInnerRadius = {read=FBottomInnerRadius, write=SetBottomInnerRadius};
	__property float TopInnerRadius = {read=FTopInnerRadius, write=SetTopInnerRadius};
	__property float TopRadius = {read=FTopRadius, write=SetTopRadius};
	__property TGLAnnulusParts Parts = {read=FParts, write=SetParts, default=15};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLAnnulus() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLAnnulus(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLCylinderBase(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLTorusPart : unsigned char { toSides, toStartDisk, toStopDisk };

typedef System::Set<TGLTorusPart, TGLTorusPart::toSides, TGLTorusPart::toStopDisk> TGLTorusParts;

class PASCALIMPLEMENTATION TGLTorus : public Gls::Scene::TGLSceneObject
{
	typedef Gls::Scene::TGLSceneObject inherited;
	
	
private:
	typedef System::DynamicArray<Gls::Objects::TGLVertexRec> _TGLTorus__1;
	
	typedef System::DynamicArray<System::DynamicArray<Gls::Objects::TGLVertexRec> > _TGLTorus__2;
	
	
private:
	TGLTorusParts FParts;
	unsigned FRings;
	unsigned FSides;
	float FStartAngle;
	float FStopAngle;
	float FMinorRadius;
	float FMajorRadius;
	_TGLTorus__2 FMesh;
	
protected:
	void __fastcall SetMajorRadius(const float aValue);
	void __fastcall SetMinorRadius(const float aValue);
	void __fastcall SetRings(unsigned aValue);
	void __fastcall SetSides(unsigned aValue);
	void __fastcall SetStartAngle(const float aValue);
	void __fastcall SetStopAngle(const float aValue);
	void __fastcall SetParts(TGLTorusParts aValue);
	
public:
	__fastcall virtual TGLTorus(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	
__published:
	__property float MajorRadius = {read=FMajorRadius, write=SetMajorRadius};
	__property float MinorRadius = {read=FMinorRadius, write=SetMinorRadius};
	__property unsigned Rings = {read=FRings, write=SetRings, default=25};
	__property unsigned Sides = {read=FSides, write=SetSides, default=15};
	__property float StartAngle = {read=FStartAngle, write=SetStartAngle};
	__property float StopAngle = {read=FStopAngle, write=SetStopAngle};
	__property TGLTorusParts Parts = {read=FParts, write=SetParts, default=1};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLTorus() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTorus(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLSceneObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLArrowLinePart : unsigned char { alLine, alTopArrow, alBottomArrow };

typedef System::Set<TGLArrowLinePart, TGLArrowLinePart::alLine, TGLArrowLinePart::alBottomArrow> TGLArrowLineParts;

enum DECLSPEC_DENUM TGLArrowHeadStyle : unsigned char { ahssStacked, ahssCentered, ahssIncluded };

class PASCALIMPLEMENTATION TGLArrowLine : public TGLCylinderBase
{
	typedef TGLCylinderBase inherited;
	
private:
	TGLArrowLineParts FParts;
	float FTopRadius;
	float fTopArrowHeadHeight;
	float fTopArrowHeadRadius;
	float fBottomArrowHeadHeight;
	float fBottomArrowHeadRadius;
	TGLArrowHeadStyle FHeadStackingStyle;
	
protected:
	void __fastcall SetTopRadius(const float aValue);
	void __fastcall SetTopArrowHeadHeight(const float aValue);
	void __fastcall SetTopArrowHeadRadius(const float aValue);
	void __fastcall SetBottomArrowHeadHeight(const float aValue);
	void __fastcall SetBottomArrowHeadRadius(const float aValue);
	void __fastcall SetParts(TGLArrowLineParts aValue);
	void __fastcall SetHeadStackingStyle(const TGLArrowHeadStyle val);
	
public:
	__fastcall virtual TGLArrowLine(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float TopRadius = {read=FTopRadius, write=SetTopRadius};
	__property TGLArrowHeadStyle HeadStackingStyle = {read=FHeadStackingStyle, write=SetHeadStackingStyle, default=0};
	__property TGLArrowLineParts Parts = {read=FParts, write=SetParts, default=3};
	__property float TopArrowHeadHeight = {read=fTopArrowHeadHeight, write=SetTopArrowHeadHeight};
	__property float TopArrowHeadRadius = {read=fTopArrowHeadRadius, write=SetTopArrowHeadRadius};
	__property float BottomArrowHeadHeight = {read=fBottomArrowHeadHeight, write=SetBottomArrowHeadHeight};
	__property float BottomArrowHeadRadius = {read=fBottomArrowHeadRadius, write=SetBottomArrowHeadRadius};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLArrowLine() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLArrowLine(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLCylinderBase(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLArrowArcPart : unsigned char { aaArc, aaTopArrow, aaBottomArrow };

typedef System::Set<TGLArrowArcPart, TGLArrowArcPart::aaArc, TGLArrowArcPart::aaBottomArrow> TGLArrowArcParts;

class PASCALIMPLEMENTATION TGLArrowArc : public TGLCylinderBase
{
	typedef TGLCylinderBase inherited;
	
	
private:
	typedef System::DynamicArray<Gls::Objects::TGLVertexRec> _TGLArrowArc__1;
	
	typedef System::DynamicArray<System::DynamicArray<Gls::Objects::TGLVertexRec> > _TGLArrowArc__2;
	
	
private:
	float fArcRadius;
	float FStartAngle;
	float FStopAngle;
	TGLArrowArcParts FParts;
	float FTopRadius;
	float fTopArrowHeadHeight;
	float fTopArrowHeadRadius;
	float fBottomArrowHeadHeight;
	float fBottomArrowHeadRadius;
	TGLArrowHeadStyle FHeadStackingStyle;
	_TGLArrowArc__2 FMesh;
	
protected:
	void __fastcall SetArcRadius(const float aValue);
	void __fastcall SetStartAngle(const float aValue);
	void __fastcall SetStopAngle(const float aValue);
	void __fastcall SetTopRadius(const float aValue);
	void __fastcall SetTopArrowHeadHeight(const float aValue);
	void __fastcall SetTopArrowHeadRadius(const float aValue);
	void __fastcall SetBottomArrowHeadHeight(const float aValue);
	void __fastcall SetBottomArrowHeadRadius(const float aValue);
	void __fastcall SetParts(TGLArrowArcParts aValue);
	void __fastcall SetHeadStackingStyle(const TGLArrowHeadStyle val);
	
public:
	__fastcall virtual TGLArrowArc(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float ArcRadius = {read=fArcRadius, write=SetArcRadius};
	__property float StartAngle = {read=FStartAngle, write=SetStartAngle};
	__property float StopAngle = {read=FStopAngle, write=SetStopAngle};
	__property float TopRadius = {read=FTopRadius, write=SetTopRadius};
	__property TGLArrowHeadStyle HeadStackingStyle = {read=FHeadStackingStyle, write=SetHeadStackingStyle, default=0};
	__property TGLArrowArcParts Parts = {read=FParts, write=SetParts, default=3};
	__property float TopArrowHeadHeight = {read=fTopArrowHeadHeight, write=SetTopArrowHeadHeight};
	__property float TopArrowHeadRadius = {read=fTopArrowHeadRadius, write=SetTopArrowHeadRadius};
	__property float BottomArrowHeadHeight = {read=fBottomArrowHeadHeight, write=SetBottomArrowHeadHeight};
	__property float BottomArrowHeadRadius = {read=fBottomArrowHeadRadius, write=SetBottomArrowHeadRadius};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLArrowArc() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLArrowArc(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLCylinderBase(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLPolygonPart : unsigned char { ppTop, ppBottom };

typedef System::Set<TGLPolygonPart, TGLPolygonPart::ppTop, TGLPolygonPart::ppBottom> TGLPolygonParts;

class PASCALIMPLEMENTATION TGLPolygon : public Gls::Objects::TGLPolygonBase
{
	typedef Gls::Objects::TGLPolygonBase inherited;
	
private:
	TGLPolygonParts FParts;
	
protected:
	void __fastcall SetParts(const TGLPolygonParts val);
	
public:
	__fastcall virtual TGLPolygon(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPolygon();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property TGLPolygonParts Parts = {read=FParts, write=SetParts, default=3};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPolygon(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Objects::TGLPolygonBase(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLFrustrumPart : unsigned char { fpTop, fpBottom, fpFront, fpBack, fpLeft, fpRight };

typedef System::Set<TGLFrustrumPart, TGLFrustrumPart::fpTop, TGLFrustrumPart::fpRight> TGLFrustrumParts;

class PASCALIMPLEMENTATION TGLFrustrum : public Gls::Scene::TGLSceneObject
{
	typedef Gls::Scene::TGLSceneObject inherited;
	
private:
	float FApexHeight;
	float FBaseDepth;
	float FBaseWidth;
	float FHeight;
	TGLFrustrumParts FParts;
	Gls::Scene::TGLNormalDirection FNormalDirection;
	void __fastcall SetApexHeight(const float aValue);
	void __fastcall SetBaseDepth(const float aValue);
	void __fastcall SetBaseWidth(const float aValue);
	void __fastcall SetHeight(const float aValue);
	void __fastcall SetParts(TGLFrustrumParts aValue);
	void __fastcall SetNormalDirection(Gls::Scene::TGLNormalDirection aValue);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadData(System::Classes::TStream* Stream);
	void __fastcall WriteData(System::Classes::TStream* Stream);
	
public:
	__fastcall virtual TGLFrustrum(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	float __fastcall TopDepth();
	float __fastcall TopWidth();
	HIDESBASE Gls::Geometrybb::TAABB __fastcall AxisAlignedBoundingBoxUnscaled();
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	
__published:
	__property float ApexHeight = {read=FApexHeight, write=SetApexHeight, stored=false};
	__property float BaseDepth = {read=FBaseDepth, write=SetBaseDepth, stored=false};
	__property float BaseWidth = {read=FBaseWidth, write=SetBaseWidth, stored=false};
	__property float Height = {read=FHeight, write=SetHeight, stored=false};
	__property Gls::Scene::TGLNormalDirection NormalDirection = {read=FNormalDirection, write=SetNormalDirection, default=1};
	__property TGLFrustrumParts Parts = {read=FParts, write=SetParts, default=63};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLFrustrum() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFrustrum(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLTeapot : public Gls::Scene::TGLSceneObject
{
	typedef Gls::Scene::TGLSceneObject inherited;
	
private:
	unsigned FGrid;
	
public:
	__fastcall virtual TGLTeapot(System::Classes::TComponent* AOwner);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLTeapot() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTeapot(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define cAllFrustrumParts (System::Set<TGLFrustrumPart, TGLFrustrumPart::fpTop, TGLFrustrumPart::fpRight>() << TGLFrustrumPart::fpTop << TGLFrustrumPart::fpBottom << TGLFrustrumPart::fpFront << TGLFrustrumPart::fpBack << TGLFrustrumPart::fpLeft << TGLFrustrumPart::fpRight )
}	/* namespace Geomobjects */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_GEOMOBJECTS)
using namespace Gls::Geomobjects;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_GeomobjectsHPP
