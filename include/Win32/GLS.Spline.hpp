// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Spline.pas' rev: 35.00 (Windows)

#ifndef Gls_SplineHPP
#define Gls_SplineHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Spline
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCubicSpline;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<System::StaticArray<float, 4> > TCubicSplineMatrix;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCubicSpline : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCubicSplineMatrix matX;
	TCubicSplineMatrix matY;
	TCubicSplineMatrix matZ;
	TCubicSplineMatrix matW;
	int FNb;
	
public:
	__fastcall TCubicSpline(const Gls::Vectorgeometry::PFloatVector X, const Gls::Vectorgeometry::PFloatVector Y, const Gls::Vectorgeometry::PFloatVector Z, const Gls::Vectorgeometry::PFloatVector W, const int nb);
	__fastcall virtual ~TCubicSpline();
	float __fastcall SplineX(const float t);
	float __fastcall SplineY(const float t);
	float __fastcall SplineZ(const float t);
	float __fastcall SplineW(const float t);
	void __fastcall SplineXY(const float t, /* out */ float &X, /* out */ float &Y);
	void __fastcall SplineXYZ(const float t, /* out */ float &X, /* out */ float &Y, /* out */ float &Z);
	void __fastcall SplineXYZW(const float t, /* out */ float &X, /* out */ float &Y, /* out */ float &Z, /* out */ float &W);
	Gls::Vectortypes::TVector3f __fastcall SplineAffineVector(const float t)/* overload */;
	void __fastcall SplineAffineVector(const float t, Gls::Vectortypes::TVector3f &vector)/* overload */;
	Gls::Vectortypes::TVector4f __fastcall SplineVector(const float t)/* overload */;
	void __fastcall SplineVector(const float t, Gls::Vectortypes::TVector4f &vector)/* overload */;
	float __fastcall SplineSlopeX(const float t);
	float __fastcall SplineSlopeY(const float t);
	float __fastcall SplineSlopeZ(const float t);
	float __fastcall SplineSlopeW(const float t);
	Gls::Vectortypes::TVector3f __fastcall SplineSlopeVector(const float t)/* overload */;
	bool __fastcall SplineIntersecYZ(float X, float &Y, float &Z);
	bool __fastcall SplineIntersecXZ(float Y, float &X, float &Z);
	bool __fastcall SplineIntersecXY(float Z, float &X, float &Y);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Spline */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SPLINE)
using namespace Gls::Spline;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_SplineHPP
