// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Utils.pas' rev: 35.00 (Windows)

#ifndef Gls_UtilsHPP
#define Gls_UtilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ShellAPI.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtDlgs.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Strings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Utils
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLUtilsException;
//-- type declarations -------------------------------------------------------
typedef System::Word THalfFloat;

typedef THalfFloat *PHalfFloat;

typedef System::Sysutils::EOSError EGLOSError;

class PASCALIMPLEMENTATION EGLUtilsException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLUtilsException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLUtilsException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLUtilsException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLUtilsException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLUtilsException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLUtilsException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLUtilsException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLUtilsException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLUtilsException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLUtilsException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLUtilsException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLUtilsException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLUtilsException() { }
	
};


typedef System::StaticArray<System::Byte, 256> TSqrt255Array;

typedef TSqrt255Array *PSqrt255Array;

typedef System::UnicodeString __fastcall (*TProjectTargetNameFunc)(void);

//-- var, const, procedure ---------------------------------------------------
static const System::Word FONT_CHARS_COUNT = System::Word(0x7e8);
extern DELPHI_PACKAGE bool IsDesignTime;
extern DELPHI_PACKAGE TProjectTargetNameFunc vProjectTargetName;
extern DELPHI_PACKAGE void __fastcall WordToIntegerArray(System::Sysutils::PWordArray Source, Gls::Vectorgeometry::PIntegerVector Dest, unsigned Count);
extern DELPHI_PACKAGE int __fastcall RoundUpToPowerOf2(int value);
extern DELPHI_PACKAGE int __fastcall RoundDownToPowerOf2(int value);
extern DELPHI_PACKAGE bool __fastcall IsPowerOf2(int value);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ReadCRLFString(System::Classes::TStream* aStream);
extern DELPHI_PACKAGE void __fastcall WriteCRLFString(System::Classes::TStream* aStream, const System::UnicodeString aString);
extern DELPHI_PACKAGE System::Extended __fastcall StrToFloatDef(const System::UnicodeString strValue, System::Extended defValue = 0.000000E+00);
extern DELPHI_PACKAGE float __fastcall Str2Float(const System::UnicodeString S);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall StringToColorAdvancedSafe(const System::UnicodeString Str, const System::Uitypes::TColor Default);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall StringToColorAdvanced(const System::UnicodeString Str);
extern DELPHI_PACKAGE bool __fastcall TryStringToColorAdvanced(const System::UnicodeString Str, System::Uitypes::TColor &OutColor);
extern DELPHI_PACKAGE int __fastcall ParseInteger(System::WideChar * &p);
extern DELPHI_PACKAGE System::Extended __fastcall ParseFloat(System::WideChar * &p);
extern DELPHI_PACKAGE void __fastcall SaveAnsiStringToFile(const System::UnicodeString fileName, const System::AnsiString data);
extern DELPHI_PACKAGE System::AnsiString __fastcall LoadAnsiStringFromFile(const System::UnicodeString fileName);
extern DELPHI_PACKAGE void __fastcall SaveStringToFile(const System::UnicodeString fileName, const System::UnicodeString data);
extern DELPHI_PACKAGE System::UnicodeString __fastcall LoadStringFromFile(const System::UnicodeString fileName);
extern DELPHI_PACKAGE void __fastcall SaveComponentToFile(System::Classes::TComponent* const Component, const System::UnicodeString fileName, const bool AsText = true);
extern DELPHI_PACKAGE void __fastcall LoadComponentFromFile(System::Classes::TComponent* const Component, const System::UnicodeString fileName, const bool AsText = true);
extern DELPHI_PACKAGE __int64 __fastcall SizeOfFile(const System::UnicodeString fileName);
extern DELPHI_PACKAGE PSqrt255Array __fastcall GetSqrt255Array(void);
extern DELPHI_PACKAGE void __fastcall InformationDlg(const System::UnicodeString msg);
extern DELPHI_PACKAGE bool __fastcall QuestionDlg(const System::UnicodeString msg);
extern DELPHI_PACKAGE System::UnicodeString __fastcall InputDlg(const System::UnicodeString aCaption, const System::UnicodeString aPrompt, const System::UnicodeString aDefault);
extern DELPHI_PACKAGE bool __fastcall SavePictureDialog(System::UnicodeString &aFileName, const System::UnicodeString aTitle = System::UnicodeString());
extern DELPHI_PACKAGE bool __fastcall OpenPictureDialog(System::UnicodeString &aFileName, const System::UnicodeString aTitle = System::UnicodeString());
extern DELPHI_PACKAGE void __fastcall SetCurrentDirToAsset(void);
extern DELPHI_PACKAGE System::Sysutils::TFileName __fastcall GetCurrentAssetPath(void);
extern DELPHI_PACKAGE void __fastcall RaiseLastOSError(void);
extern DELPHI_PACKAGE bool __fastcall IsSubComponent(System::Classes::TComponent* const AComponent);
extern DELPHI_PACKAGE void __fastcall MakeSubComponent(System::Classes::TComponent* const AComponent, const bool value);
extern DELPHI_PACKAGE bool __fastcall AnsiStartsText(const System::UnicodeString ASubText, const System::UnicodeString AText);
extern DELPHI_PACKAGE int __fastcall GLOKMessageBox(const System::UnicodeString Text, const System::UnicodeString Caption);
extern DELPHI_PACKAGE void __fastcall GLLoadBitmapFromInstance(int Instance, Vcl::Graphics::TBitmap* ABitmap, const System::UnicodeString AName);
extern DELPHI_PACKAGE void __fastcall ShowHTMLUrl(const System::UnicodeString Url);
extern DELPHI_PACKAGE System::Types::TRect __fastcall GetGLRect(const int aLeft, const int aTop, const int aRight, const int aBottom);
extern DELPHI_PACKAGE void __fastcall InflateGLRect(System::Types::TRect &aRect, int dx, int dy);
extern DELPHI_PACKAGE void __fastcall IntersectGLRect(System::Types::TRect &aRect, const System::Types::TRect &rect2);
extern DELPHI_PACKAGE int __fastcall GetDeviceLogicalPixelsX(HDC device);
extern DELPHI_PACKAGE int __fastcall GetCurrentColorDepth(void);
extern DELPHI_PACKAGE int __fastcall PixelFormatToColorBits(Vcl::Graphics::TPixelFormat aPixelFormat);
extern DELPHI_PACKAGE void __fastcall FixPathDelimiter(System::UnicodeString &S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall RelativePath(const System::UnicodeString S);
extern DELPHI_PACKAGE void __fastcall QueryPerformanceCounter(/* out */ __int64 &val);
extern DELPHI_PACKAGE bool __fastcall QueryPerformanceFrequency(/* out */ __int64 &val);
extern DELPHI_PACKAGE __int64 __fastcall StartPrecisionTimer(void);
extern DELPHI_PACKAGE double __fastcall PrecisionTimerLap(const __int64 precisionTimer);
extern DELPHI_PACKAGE double __fastcall StopPrecisionTimer(const __int64 precisionTimer);
extern DELPHI_PACKAGE double __fastcall AppTime(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall FindUnitName(System::TObject* anObject)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall FindUnitName(System::TClass aClass)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetExeDirectory(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetValueFromStringsIndex(System::Classes::TStrings* const AStrings, const int AIndex);
extern DELPHI_PACKAGE bool __fastcall IsDirectoryWriteable(const System::UnicodeString AName);
extern DELPHI_PACKAGE System::WideChar __fastcall CharToWideChar(const char AChar);
extern DELPHI_PACKAGE float __fastcall HalfToFloat(THalfFloat Half);
extern DELPHI_PACKAGE THalfFloat __fastcall FloatToHalf(float Float);
extern DELPHI_PACKAGE System::Extended __fastcall GLStrToFloatDef(const System::UnicodeString S, const System::Extended Default, const System::Sysutils::TFormatSettings &fs)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall GLStrToFloatDef(const System::UnicodeString S, const System::Extended Default)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall GLStrToFloatDef(const System::UnicodeString S)/* overload */;
}	/* namespace Utils */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_UTILS)
using namespace Gls::Utils;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_UtilsHPP
