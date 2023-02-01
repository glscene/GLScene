// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.CurvesAndSurfaces.pas' rev: 35.00 (Windows)

#ifndef Gls_CurvesandsurfacesHPP
#define Gls_CurvesandsurfacesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Curvesandsurfaces
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TBSplineContinuity : unsigned char { bscUniformNonPeriodic, bscUniformPeriodic };

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Gls::Vectortypes::TVector3f __fastcall BezierCurvePoint(float t, int n, Gls::Vectorgeometry::PAffineVectorArray cp);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector3f __fastcall BezierSurfacePoint(float s, float t, int m, int n, Gls::Vectorgeometry::PAffineVectorArray cp);
extern DELPHI_PACKAGE void __fastcall GenerateBezierCurve(int Steps, Gls::Vectorlists::TGLAffineVectorList* ControlPoints, Gls::Vectorlists::TGLAffineVectorList* Vertices);
extern DELPHI_PACKAGE void __fastcall GenerateBezierSurface(int Steps, int Width, int Height, Gls::Vectorlists::TGLAffineVectorList* ControlPoints, Gls::Vectorlists::TGLAffineVectorList* Vertices);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector3f __fastcall BSplinePoint(float t, int n, int k, Gls::Vectorgeometry::PFloatVector knots, Gls::Vectorgeometry::PAffineVectorArray cp);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector3f __fastcall BSplineSurfacePoint(float s, float t, int m, int n, int k1, int k2, Gls::Vectorgeometry::PFloatVector uknots, Gls::Vectorgeometry::PFloatVector vknots, Gls::Vectorgeometry::PAffineVectorArray cp);
extern DELPHI_PACKAGE void __fastcall GenerateBSpline(int Steps, int Order, Gls::Vectorlists::TGLSingleList* KnotVector, Gls::Vectorlists::TGLAffineVectorList* ControlPoints, Gls::Vectorlists::TGLAffineVectorList* Vertices);
extern DELPHI_PACKAGE void __fastcall GenerateBSplineSurface(int Steps, int UOrder, int VOrder, int Width, int Height, Gls::Vectorlists::TGLSingleList* UKnotVector, Gls::Vectorlists::TGLSingleList* VKnotVector, Gls::Vectorlists::TGLAffineVectorList* ControlPoints, Gls::Vectorlists::TGLAffineVectorList* Vertices);
extern DELPHI_PACKAGE void __fastcall GenerateKnotVector(Gls::Vectorlists::TGLSingleList* KnotVector, int NumberOfPoints, int Order, TBSplineContinuity Continuity);
}	/* namespace Curvesandsurfaces */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_CURVESANDSURFACES)
using namespace Gls::Curvesandsurfaces;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_CurvesandsurfacesHPP
