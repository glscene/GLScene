// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.AnimationUtils.pas' rev: 35.00 (Windows)

#ifndef Gls_AnimationutilsHPP
#define Gls_AnimationutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Animationutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TEaseType : unsigned char { etLinear, etQuadIn, etQuadOut, etQuadInOut, etQuadOutIn, etCubicIn, etCubicOut, etCubicInOut, etCubicOutIn, etQuintIn, etQuintOut, etQuintInOut, etQuintOutIn, etSineIn, etSineOut, etSineInOut, etSineOutIn, etCircIn, etCircOut, etCircInOut, etCircOutIn, etExpoIn, etExpoOut, etExpoInOut, etExpoOutIn, etElasticIn, etElasticOut, etElasticInOut, etElasticOutIn, etBackIn, etBackOut, etBackInOut, etBackOutIn, etBounceIn, etBounceOut, etBounceInOut, etBounceOutIn };

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Gls::Vectortypes::TVector3f __fastcall Tweener(const Gls::Vectortypes::TVector3f &Current, const Gls::Vectortypes::TVector3f &Target, float Time, float Duration, TEaseType EaseType)/* overload */;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall Tweener(const Gls::Vectortypes::TVector4f &Current, const Gls::Vectortypes::TVector4f &Target, float Time, float Duration, TEaseType EaseType)/* overload */;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector2f __fastcall Tweener(const Gls::Vectortypes::TVector2f &Current, const Gls::Vectortypes::TVector2f &Target, float Time, float Duration, TEaseType EaseType)/* overload */;
extern DELPHI_PACKAGE float __fastcall Tweener(float Current, float Target, float Time, float Duration, TEaseType EaseType)/* overload */;
}	/* namespace Animationutils */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_ANIMATIONUTILS)
using namespace Gls::Animationutils;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_AnimationutilsHPP
