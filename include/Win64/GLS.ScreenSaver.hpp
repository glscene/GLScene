// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.ScreenSaver.pas' rev: 35.00 (Windows)

#ifndef Gls_ScreensaverHPP
#define Gls_ScreensaverHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Win.Registry.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Screensaver
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLScreenSaver;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TScreenSaverOption : unsigned char { ssoAutoAdjustFormProperties, ssoAutoHookKeyboardEvents, ssoAutoHookMouseEvents, ssoEnhancedMouseMoveDetection };

typedef System::Set<TScreenSaverOption, TScreenSaverOption::ssoAutoAdjustFormProperties, TScreenSaverOption::ssoEnhancedMouseMoveDetection> TScreenSaverOptions;

typedef void __fastcall (__closure *TScreenSaverPreviewEvent)(System::TObject* Sender, HWND previewHwnd);

class PASCALIMPLEMENTATION TGLScreenSaver : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	int mouseEventsToIgnore;
	bool FHonourWindowsPassword;
	TScreenSaverOptions FOptions;
	System::Classes::TNotifyEvent FOnPropertiesRequested;
	System::Classes::TNotifyEvent FOnExecute;
	TScreenSaverPreviewEvent FOnPreview;
	Vcl::Forms::TCloseQueryEvent FOnCloseQuery;
	System::UnicodeString FAboutString;
	bool FInPreviewMode;
	Vcl::Extctrls::TTimer* mouseTimer;
	System::Types::TPoint lastMousePosition;
	NativeUInt FMutex;
	
protected:
	virtual void __fastcall Loaded();
	void __fastcall FormMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall FormKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall OnMouseTimer(System::TObject* Sender);
	void __fastcall ConfigureSaver();
	void __fastcall PreviewSaver();
	void __fastcall ExecuteSaver();
	
public:
	__fastcall virtual TGLScreenSaver(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLScreenSaver();
	void __fastcall SetPassword();
	bool __fastcall CloseSaver();
	__property bool InPreviewMode = {read=FInPreviewMode, nodefault};
	
__published:
	__property TScreenSaverOptions Options = {read=FOptions, write=FOptions, default=11};
	__property bool HonourWindowsPassword = {read=FHonourWindowsPassword, write=FHonourWindowsPassword, default=1};
	__property System::UnicodeString AboutString = {read=FAboutString, write=FAboutString};
	__property System::Classes::TNotifyEvent OnPropertiesRequested = {read=FOnPropertiesRequested, write=FOnPropertiesRequested};
	__property System::Classes::TNotifyEvent OnExecute = {read=FOnExecute, write=FOnExecute};
	__property TScreenSaverPreviewEvent OnPreview = {read=FOnPreview, write=FOnPreview};
	__property Vcl::Forms::TCloseQueryEvent OnCloseQuery = {read=FOnCloseQuery, write=FOnCloseQuery};
};


//-- var, const, procedure ---------------------------------------------------
#define cDefaultScreenSaverOptions (System::Set<TScreenSaverOption, TScreenSaverOption::ssoAutoAdjustFormProperties, TScreenSaverOption::ssoEnhancedMouseMoveDetection>() << TScreenSaverOption::ssoAutoAdjustFormProperties << TScreenSaverOption::ssoAutoHookKeyboardEvents << TScreenSaverOption::ssoEnhancedMouseMoveDetection )
extern DELPHI_PACKAGE void __fastcall SetScreenSaverPassword(void);
}	/* namespace Screensaver */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SCREENSAVER)
using namespace Gls::Screensaver;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ScreensaverHPP
