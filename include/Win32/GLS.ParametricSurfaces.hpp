// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.ParametricSurfaces.pas' rev: 35.00 (Windows)

#ifndef Gls_ParametricsurfacesHPP
#define Gls_ParametricsurfacesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.OpenGLAdapter.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.CurvesAndSurfaces.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.Texture.hpp>
#include <GLS.State.hpp>
#include <GLS.Context.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Parametricsurfaces
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMOParametricSurface;
class DELPHICLASS TFGBezierSurface;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TParametricSurfaceRenderer : unsigned char { psrGLScene, psrOpenGL };

enum DECLSPEC_DENUM TParametricSurfaceBasis : unsigned char { psbBezier, psbBSpline };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMOParametricSurface : public Gls::Vectorfileobjects::TGLMeshObject
{
	typedef Gls::Vectorfileobjects::TGLMeshObject inherited;
	
private:
	Gls::Vectorlists::TGLAffineVectorList* FControlPoints;
	Gls::Vectorlists::TGLAffineVectorList* FWeightedControlPoints;
	Gls::Vectorlists::TGLSingleList* FKnotsU;
	Gls::Vectorlists::TGLSingleList* FKnotsV;
	Gls::Vectorlists::TGLSingleList* FWeights;
	int FOrderU;
	int FOrderV;
	int FCountU;
	int FCountV;
	int FResolution;
	bool FAutoKnots;
	Gls::Curvesandsurfaces::TBSplineContinuity FContinuity;
	TParametricSurfaceRenderer FRenderer;
	TParametricSurfaceBasis FBasis;
	void __fastcall SetControlPoints(Gls::Vectorlists::TGLAffineVectorList* Value);
	void __fastcall SetKnotsU(Gls::Vectorlists::TGLSingleList* Value);
	void __fastcall SetKnotsV(Gls::Vectorlists::TGLSingleList* Value);
	void __fastcall SetWeights(Gls::Vectorlists::TGLSingleList* Value);
	void __fastcall SetRenderer(TParametricSurfaceRenderer Value);
	void __fastcall SetBasis(TParametricSurfaceBasis Value);
	
public:
	__fastcall virtual TMOParametricSurface();
	__fastcall virtual ~TMOParametricSurface();
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* reader);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &mrci);
	virtual void __fastcall Prepare();
	virtual void __fastcall Clear();
	void __fastcall GenerateMesh();
	__property Gls::Vectorlists::TGLAffineVectorList* ControlPoints = {read=FControlPoints, write=SetControlPoints};
	__property Gls::Vectorlists::TGLSingleList* KnotsU = {read=FKnotsU, write=SetKnotsU};
	__property Gls::Vectorlists::TGLSingleList* KnotsV = {read=FKnotsV, write=SetKnotsV};
	__property Gls::Vectorlists::TGLSingleList* Weights = {read=FWeights, write=SetWeights};
	__property int OrderU = {read=FOrderU, write=FOrderU, nodefault};
	__property int OrderV = {read=FOrderV, write=FOrderV, nodefault};
	__property int CountU = {read=FCountU, write=FCountU, nodefault};
	__property int CountV = {read=FCountV, write=FCountV, nodefault};
	__property int Resolution = {read=FResolution, write=FResolution, nodefault};
	__property bool AutoKnots = {read=FAutoKnots, write=FAutoKnots, nodefault};
	__property Gls::Curvesandsurfaces::TBSplineContinuity Continuity = {read=FContinuity, write=FContinuity, nodefault};
	__property TParametricSurfaceRenderer Renderer = {read=FRenderer, write=SetRenderer, nodefault};
	__property TParametricSurfaceBasis Basis = {read=FBasis, write=SetBasis, nodefault};
public:
	/* TGLMeshObject.CreateOwned */ inline __fastcall TMOParametricSurface(Gls::Vectorfileobjects::TGLMeshObjectList* AOwner) : Gls::Vectorfileobjects::TGLMeshObject(AOwner) { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TMOParametricSurface(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Vectorfileobjects::TGLMeshObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFGBezierSurface : public Gls::Vectorfileobjects::TGLFaceGroup
{
	typedef Gls::Vectorfileobjects::TGLFaceGroup inherited;
	
private:
	int FCountU;
	int FCountV;
	Gls::Vectorlists::TGLIntegerList* FControlPointIndices;
	Gls::Vectorlists::TGLIntegerList* FTexCoordIndices;
	int FResolution;
	float FMinU;
	float FMaxU;
	float FMinV;
	float FMaxV;
	Gls::Vectorlists::TGLAffineVectorList* FTempControlPoints;
	Gls::Vectorlists::TGLAffineVectorList* FTempTexCoords;
	
protected:
	void __fastcall SetControlPointIndices(Gls::Vectorlists::TGLIntegerList* const Value);
	void __fastcall SetTexCoordIndices(Gls::Vectorlists::TGLIntegerList* const Value);
	
public:
	__fastcall virtual TFGBezierSurface();
	__fastcall virtual ~TFGBezierSurface();
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* reader);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &mrci);
	virtual void __fastcall Prepare();
	__property int CountU = {read=FCountU, write=FCountU, nodefault};
	__property int CountV = {read=FCountV, write=FCountV, nodefault};
	__property int Resolution = {read=FResolution, write=FResolution, nodefault};
	__property float MinU = {read=FMinU, write=FMinU};
	__property float MaxU = {read=FMaxU, write=FMaxU};
	__property float MinV = {read=FMinV, write=FMinV};
	__property float MaxV = {read=FMaxV, write=FMaxV};
	__property Gls::Vectorlists::TGLIntegerList* ControlPointIndices = {read=FControlPointIndices, write=SetControlPointIndices};
	__property Gls::Vectorlists::TGLIntegerList* TexCoordIndices = {read=FTexCoordIndices, write=SetTexCoordIndices};
public:
	/* TGLFaceGroup.CreateOwned */ inline __fastcall virtual TFGBezierSurface(Gls::Vectorfileobjects::TGLFaceGroups* aOwner) : Gls::Vectorfileobjects::TGLFaceGroup(aOwner) { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TFGBezierSurface(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Vectorfileobjects::TGLFaceGroup(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Parametricsurfaces */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_PARAMETRICSURFACES)
using namespace Gls::Parametricsurfaces;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ParametricsurfacesHPP
