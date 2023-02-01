// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Polynomials.pas' rev: 35.00 (Windows)

#ifndef Gls_PolynomialsHPP
#define Gls_PolynomialsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <GLS.VectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Polynomials
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<double> TDoubleArray;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE double __fastcall EvalPolynom(const TDoubleArray poly, const double x);
extern DELPHI_PACKAGE TDoubleArray __fastcall DerivatedPolynom(const TDoubleArray poly);
extern DELPHI_PACKAGE double __fastcall FindRoot(const TDoubleArray poly, double min, double max, double epsilon);
extern DELPHI_PACKAGE bool __fastcall MinPositiveCoef(const TDoubleArray coefs, double &aMin);
extern DELPHI_PACKAGE double __fastcall cbrt(const double x);
extern DELPHI_PACKAGE TDoubleArray __fastcall SolveQuadric(const Gls::Vectorgeometry::PDoubleVector c);
extern DELPHI_PACKAGE TDoubleArray __fastcall SolveCubic(const Gls::Vectorgeometry::PDoubleVector c);
extern DELPHI_PACKAGE TDoubleArray __fastcall SolveQuartic(const Gls::Vectorgeometry::PDoubleVector c);
}	/* namespace Polynomials */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_POLYNOMIALS)
using namespace Gls::Polynomials;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_PolynomialsHPP
