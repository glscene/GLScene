// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Extrusion.pas' rev: 35.00 (Windows)

#ifndef Gls_ExtrusionHPP
#define Gls_ExtrusionHPP

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
#include <GLS.Context.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Scene.hpp>
#include <GLS.MultiPolygon.hpp>
#include <GLS.Color.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Nodes.hpp>
#include <GLS.State.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Extrusion
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLRevolutionSolid;
class DELPHICLASS TGLExtrusionSolid;
class DELPHICLASS TGLPipeNode;
class DELPHICLASS TGLPipeNodes;
class DELPHICLASS TGLPipe;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLExtrusionSolidPart : unsigned char { espOutside, espInside, espStartPolygon, espStopPolygon };

typedef System::Set<TGLExtrusionSolidPart, TGLExtrusionSolidPart::espOutside, TGLExtrusionSolidPart::espStopPolygon> TGLExtrusionSolidParts;

enum DECLSPEC_DENUM TGLRevolutionSolidPart : unsigned char { rspOutside, rspInside, rspStartPolygon, rspStopPolygon };

typedef System::Set<TGLRevolutionSolidPart, TGLRevolutionSolidPart::rspOutside, TGLRevolutionSolidPart::rspStopPolygon> TGLRevolutionSolidParts;

class PASCALIMPLEMENTATION TGLRevolutionSolid : public Gls::Objects::TGLPolygonBase
{
	typedef Gls::Objects::TGLPolygonBase inherited;
	
private:
	int FSlices;
	float FStartAngle;
	float FStopAngle;
	Gls::Objects::TGLNormalSmoothing FNormals;
	float FYOffsetPerTurn;
	int FTriangleCount;
	Gls::Scene::TGLNormalDirection FNormalDirection;
	TGLRevolutionSolidParts FParts;
	Gls::Vectortypes::TVector4f FAxisAlignedDimensionsCache;
	
protected:
	void __fastcall SetStartAngle(const float val);
	void __fastcall SetStopAngle(const float val);
	bool __fastcall StoreStopAngle();
	void __fastcall SetSlices(const int val);
	void __fastcall SetNormals(const Gls::Objects::TGLNormalSmoothing val);
	void __fastcall SetYOffsetPerTurn(const float val);
	void __fastcall SetNormalDirection(const Gls::Scene::TGLNormalDirection val);
	void __fastcall SetParts(const TGLRevolutionSolidParts val);
	
public:
	__fastcall virtual TGLRevolutionSolid(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLRevolutionSolid();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property int TriangleCount = {read=FTriangleCount, nodefault};
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual void __fastcall StructureChanged();
	
__published:
	__property TGLRevolutionSolidParts Parts = {read=FParts, write=SetParts, default=1};
	__property float StartAngle = {read=FStartAngle, write=SetStartAngle};
	__property float StopAngle = {read=FStopAngle, write=SetStopAngle, stored=StoreStopAngle};
	__property float YOffsetPerTurn = {read=FYOffsetPerTurn, write=SetYOffsetPerTurn};
	__property int Slices = {read=FSlices, write=SetSlices, default=16};
	__property Gls::Objects::TGLNormalSmoothing Normals = {read=FNormals, write=SetNormals, default=0};
	__property Gls::Scene::TGLNormalDirection NormalDirection = {read=FNormalDirection, write=SetNormalDirection, default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLRevolutionSolid(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Objects::TGLPolygonBase(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLExtrusionSolid : public Gls::Multipolygon::TGLMultiPolygonBase
{
	typedef Gls::Multipolygon::TGLMultiPolygonBase inherited;
	
private:
	int FStacks;
	Gls::Objects::TGLNormalSmoothing FNormals;
	int FTriangleCount;
	Gls::Scene::TGLNormalDirection FNormalDirection;
	TGLExtrusionSolidParts FParts;
	float FHeight;
	float FMinSmoothAngle;
	float FMinSmoothAngleCos;
	Gls::Vectortypes::TVector4f FAxisAlignedDimensionsCache;
	void __fastcall SetHeight(const float Value);
	void __fastcall SetMinSmoothAngle(const float Value);
	
protected:
	void __fastcall SetStacks(const int val);
	void __fastcall SetNormals(const Gls::Objects::TGLNormalSmoothing val);
	void __fastcall SetNormalDirection(const Gls::Scene::TGLNormalDirection val);
	void __fastcall SetParts(const TGLExtrusionSolidParts val);
	
public:
	__fastcall virtual TGLExtrusionSolid(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLExtrusionSolid();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property int TriangleCount = {read=FTriangleCount, nodefault};
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual void __fastcall StructureChanged();
	
__published:
	__property TGLExtrusionSolidParts Parts = {read=FParts, write=SetParts, default=1};
	__property float Height = {read=FHeight, write=SetHeight};
	__property int Stacks = {read=FStacks, write=SetStacks, default=1};
	__property Gls::Objects::TGLNormalSmoothing Normals = {read=FNormals, write=SetNormals, default=0};
	__property Gls::Scene::TGLNormalDirection NormalDirection = {read=FNormalDirection, write=SetNormalDirection, default=1};
	__property float MinSmoothAngle = {read=FMinSmoothAngle, write=SetMinSmoothAngle};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLExtrusionSolid(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Multipolygon::TGLMultiPolygonBase(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLPipeNode : public Gls::Nodes::TGLNode
{
	typedef Gls::Nodes::TGLNode inherited;
	
private:
	float FRadiusFactor;
	Gls::Color::TGLColor* FColor;
	float FTexCoordT;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetRadiusFactor(const float val);
	bool __fastcall StoreRadiusFactor();
	void __fastcall SetColor(Gls::Color::TGLColor* const val);
	void __fastcall ColorChanged(System::TObject* sender);
	bool __fastcall StoreTexCoordT();
	
public:
	__fastcall virtual TGLPipeNode(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLPipeNode();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float RadiusFactor = {read=FRadiusFactor, write=SetRadiusFactor, stored=StoreRadiusFactor};
	__property Gls::Color::TGLColor* Color = {read=FColor, write=SetColor};
	__property float TexCoordT = {read=FTexCoordT, write=FTexCoordT, stored=StoreTexCoordT};
};


class PASCALIMPLEMENTATION TGLPipeNodes : public Gls::Objects::TGLLinesNodes
{
	typedef Gls::Objects::TGLLinesNodes inherited;
	
public:
	TGLPipeNode* operator[](int index) { return this->Items[index]; }
	
protected:
	HIDESBASE void __fastcall SetItems(int index, TGLPipeNode* const val);
	HIDESBASE TGLPipeNode* __fastcall GetItems(int index);
	
public:
	__fastcall TGLPipeNodes(System::Classes::TComponent* AOwner);
	HIDESBASE TGLPipeNode* __fastcall Add();
	HIDESBASE TGLPipeNode* __fastcall FindItemID(int ID);
	__property TGLPipeNode* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLPipeNodes() { }
	
};


enum DECLSPEC_DENUM TPipePart : unsigned char { ppOutside, ppInside, ppStartDisk, ppStopDisk };

typedef System::Set<TPipePart, TPipePart::ppOutside, TPipePart::ppStopDisk> TPipeParts;

enum DECLSPEC_DENUM TPipeNodesColorMode : unsigned char { pncmNone, pncmEmission, pncmAmbient, pncmDiffuse, pncmAmbientAndDiffuse };

enum DECLSPEC_DENUM TPipeTexCoordMode : unsigned char { ptcmDefault, ptcmManual };

enum DECLSPEC_DENUM TPipeNormalMode : unsigned char { pnmDefault, pnmAdvanced };

class PASCALIMPLEMENTATION TGLPipe : public Gls::Objects::TGLPolygonBase
{
	typedef Gls::Objects::TGLPolygonBase inherited;
	
private:
	int FSlices;
	TPipeParts FParts;
	int FTriangleCount;
	float FRadius;
	TPipeNodesColorMode FNodesColorMode;
	TPipeTexCoordMode FTextCoordMode;
	float FTextCoordTileS;
	float FTextCoordTileT;
	TPipeNormalMode FNormalMode;
	float FNormalSmoothAngle;
	
protected:
	DYNAMIC void __fastcall CreateNodes();
	void __fastcall SetSlices(const int val);
	void __fastcall SetParts(const TPipeParts val);
	void __fastcall SetRadius(const float val);
	bool __fastcall StoreRadius();
	void __fastcall SetNodesColorMode(const TPipeNodesColorMode val);
	void __fastcall SetTextCoordMode(const TPipeTexCoordMode val);
	void __fastcall SetTextCoordTileS(const float val);
	void __fastcall SetTextCoordTileT(const float val);
	bool __fastcall StoreTextCoordTileS();
	bool __fastcall StoreTextCoordTileT();
	void __fastcall SetNormalMode(const TPipeNormalMode val);
	void __fastcall SetNormalSmoothAngle(const float val);
	
public:
	__fastcall virtual TGLPipe(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPipe();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property int TriangleCount = {read=FTriangleCount, nodefault};
	
__published:
	__property TPipeParts Parts = {read=FParts, write=SetParts, default=1};
	__property int Slices = {read=FSlices, write=SetSlices, default=16};
	__property float Radius = {read=FRadius, write=SetRadius};
	__property TPipeNodesColorMode NodesColorMode = {read=FNodesColorMode, write=SetNodesColorMode, default=0};
	__property TPipeTexCoordMode TexCoordMode = {read=FTextCoordMode, write=SetTextCoordMode, default=0};
	__property float TexCoordTileS = {read=FTextCoordTileS, write=SetTextCoordTileS, stored=StoreTextCoordTileS};
	__property float TexCoordTileT = {read=FTextCoordTileT, write=SetTextCoordTileT, stored=StoreTextCoordTileT};
	__property TPipeNormalMode NormalMode = {read=FNormalMode, write=SetNormalMode, default=0};
	__property float NormalSmoothAngle = {read=FNormalSmoothAngle, write=SetNormalSmoothAngle};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPipe(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Objects::TGLPolygonBase(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Extrusion */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_EXTRUSION)
using namespace Gls::Extrusion;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ExtrusionHPP
