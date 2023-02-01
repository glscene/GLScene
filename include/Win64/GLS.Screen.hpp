// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Screen.pas' rev: 35.00 (Windows)

#ifndef Gls_ScreenHPP
#define Gls_ScreenHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Forms.hpp>
#include <GLS.VectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Screen
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLDisplayOptions;
struct TGLVideoMode;
//-- type declarations -------------------------------------------------------
typedef System::Byte TResolution;

enum DECLSPEC_DENUM TGLWindowAttribute : unsigned char { woDesktop, woStayOnTop, woTransparent };

typedef System::Set<TGLWindowAttribute, TGLWindowAttribute::woDesktop, TGLWindowAttribute::woTransparent> TGLWindowAttributes;

enum DECLSPEC_DENUM TGLWindowFitting : unsigned char { wfDefault, wfFitWindowToScreen, wfFitScreenToWindow };

class PASCALIMPLEMENTATION TGLDisplayOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FFullScreen;
	TResolution FScreenResolution;
	TGLWindowAttributes FWindowAttributes;
	TGLWindowFitting FWindowFitting;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property bool FullScreen = {read=FFullScreen, write=FFullScreen, default=0};
	__property TResolution ScreenResolution = {read=FScreenResolution, write=FScreenResolution, default=0};
	__property TGLWindowAttributes WindowAttributes = {read=FWindowAttributes, write=FWindowAttributes, default=0};
	__property TGLWindowFitting WindowFitting = {read=FWindowFitting, write=FWindowFitting, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLDisplayOptions() { }
	
public:
	/* TObject.Create */ inline __fastcall TGLDisplayOptions() : System::Classes::TPersistent() { }
	
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLVideoMode
{
public:
	System::Word Width;
	System::Word Height;
	System::Byte ColorDepth;
	System::Byte MaxFrequency;
	System::UnicodeString Description;
};
#pragma pack(pop)


typedef TGLVideoMode *PGLVideoMode;

typedef System::DynamicArray<TGLVideoMode> Gls_Screen__2;

//-- var, const, procedure ---------------------------------------------------
static const System::Byte MaxVideoModes = System::Byte(0xc8);
static const System::Int8 lcl_release = System::Int8(0x0);
extern DELPHI_PACKAGE int vNumberVideoModes;
extern DELPHI_PACKAGE int vCurrentVideoMode;
extern DELPHI_PACKAGE Gls_Screen__2 vVideoModes;
extern DELPHI_PACKAGE TResolution __fastcall GetIndexFromResolution(int XRes, int YRes, int BPP);
extern DELPHI_PACKAGE void __fastcall ReadVideoModes(void);
extern DELPHI_PACKAGE bool __fastcall SetFullscreenMode(TResolution modeIndex, int displayFrequency = 0x0);
extern DELPHI_PACKAGE void __fastcall ReadScreenImage(HDC Dest, int DestLeft, int DestTop, const Gls::Vectorgeometry::TRectangle &SrcRect);
extern DELPHI_PACKAGE void __fastcall RestoreDefaultMode(void);
extern DELPHI_PACKAGE void __fastcall GLShowCursor(bool AShow);
extern DELPHI_PACKAGE void __fastcall GLSetCursorPos(int AScreenX, int AScreenY);
extern DELPHI_PACKAGE void __fastcall GLGetCursorPos(System::Types::TPoint &point);
extern DELPHI_PACKAGE int __fastcall GLGetScreenWidth(void);
extern DELPHI_PACKAGE int __fastcall GLGetScreenHeight(void);
}	/* namespace Screen */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SCREEN)
using namespace Gls::Screen;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ScreenHPP
