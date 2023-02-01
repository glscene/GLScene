// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.RGBE.pas' rev: 35.00 (Windows)

#ifndef Gls_RgbeHPP
#define Gls_RgbeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Rgbe
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Float2rgbe(Gls::Vectortypes::TVector4b &RGBE, const float Red, const float Green, const float Blue);
extern DELPHI_PACKAGE void __fastcall Rgbe2float(float &Red, float &Green, float &Blue, const Gls::Vectortypes::TVector4b RGBE);
extern DELPHI_PACKAGE void __fastcall LoadRLEpixels(System::Classes::TStream* Stream, System::PSingle Dst, int Scanline_width, int Num_scanlines);
extern DELPHI_PACKAGE void __fastcall LoadRGBEpixels(System::Classes::TStream* Stream, System::PSingle Dst, int Numpixels);
}	/* namespace Rgbe */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_RGBE)
using namespace Gls::Rgbe;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_RgbeHPP
