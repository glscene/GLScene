// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.ImageUtils.pas' rev: 35.00 (Windows)

#ifndef Gls_ImageutilsHPP
#define Gls_ImageutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.OpenGLext.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Strings.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Utils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Imageutils
{
//-- forward type declarations -----------------------------------------------
struct TIntermediateFormat;
class DELPHICLASS EGLImageUtils;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TIntermediateFormat
{
public:
	float R;
	float G;
	float B;
	float A;
};


typedef System::DynamicArray<void *> TPointerArray;

typedef TIntermediateFormat *PRGBA32F;

typedef System::StaticArray<TIntermediateFormat, 67108864> TIntermediateFormatArray;

typedef TIntermediateFormatArray *PIntermediateFormatArray;

typedef System::StaticArray<System::StaticArray<System::Byte, 4>, 4> TU48BitBlock;

typedef System::StaticArray<System::StaticArray<short, 4>, 4> T48BitBlock;

#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLImageUtils : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLImageUtils(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLImageUtils(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLImageUtils(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLImageUtils(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLImageUtils(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLImageUtils(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLImageUtils(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLImageUtils(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLImageUtils(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLImageUtils(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLImageUtils(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLImageUtils(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLImageUtils() { }
	
};

#pragma pack(pop)

typedef float __fastcall (*TImageFilterFunction)(float Value);

typedef void __fastcall (*TImageAlphaProc)(TIntermediateFormat &AColor);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE int vImageScaleFilterWidth;
extern DELPHI_PACKAGE float __fastcall ImageBoxFilter(float Value);
extern DELPHI_PACKAGE float __fastcall ImageTriangleFilter(float Value);
extern DELPHI_PACKAGE float __fastcall ImageHermiteFilter(float Value);
extern DELPHI_PACKAGE float __fastcall ImageBellFilter(float Value);
extern DELPHI_PACKAGE float __fastcall ImageSplineFilter(float Value);
extern DELPHI_PACKAGE float __fastcall ImageLanczos3Filter(float Value);
extern DELPHI_PACKAGE float __fastcall ImageMitchellFilter(float Value);
extern DELPHI_PACKAGE void __fastcall ImageAlphaFromIntensity(TIntermediateFormat &AColor);
extern DELPHI_PACKAGE void __fastcall ImageAlphaSuperBlackTransparent(TIntermediateFormat &AColor);
extern DELPHI_PACKAGE void __fastcall ImageAlphaLuminance(TIntermediateFormat &AColor);
extern DELPHI_PACKAGE void __fastcall ImageAlphaLuminanceSqrt(TIntermediateFormat &AColor);
extern DELPHI_PACKAGE void __fastcall ImageAlphaOpaque(TIntermediateFormat &AColor);
extern DELPHI_PACKAGE void __fastcall ImageAlphaTopLeftPointColorTransparent(TIntermediateFormat &AColor);
extern DELPHI_PACKAGE void __fastcall ImageAlphaInverseLuminance(TIntermediateFormat &AColor);
extern DELPHI_PACKAGE void __fastcall ImageAlphaInverseLuminanceSqrt(TIntermediateFormat &AColor);
extern DELPHI_PACKAGE void __fastcall ImageAlphaBottomRightPointColorTransparent(TIntermediateFormat &AColor);
extern DELPHI_PACKAGE void __fastcall ConvertImage(const void * ASrc, const void * ADst, unsigned ASrcColorFormat, unsigned ADstColorFormat, unsigned ASrcDataType, unsigned ADstDataType, int AWidth, int AHeight);
extern DELPHI_PACKAGE void __fastcall RescaleImage(const void * ASrc, const void * ADst, unsigned AColorFormat, unsigned ADataType, TImageFilterFunction AFilter, int ASrcWidth, int ASrcHeight, int ADstWidth, int ADstHeight);
extern DELPHI_PACKAGE void __fastcall Build2DMipmap(const void * ASrc, const TPointerArray ADst, unsigned AColorFormat, unsigned ADataType, TImageFilterFunction AFilter, int ASrcWidth, int ASrcHeight);
extern DELPHI_PACKAGE void __fastcall AlphaGammaBrightCorrection(const void * ASrc, unsigned AColorFormat, unsigned ADataType, int ASrcWidth, int ASrcHeight, TImageAlphaProc anAlphaProc, float ABrightness, float AGamma);
}	/* namespace Imageutils */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_IMAGEUTILS)
using namespace Gls::Imageutils;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ImageutilsHPP
