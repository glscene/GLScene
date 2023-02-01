// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Keyboard.pas' rev: 35.00 (Windows)

#ifndef Gls_KeyboardHPP
#define Gls_KeyboardHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Keyboard
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef int TVirtualKeyCode;

//-- var, const, procedure ---------------------------------------------------
static const System::Byte VK_MOUSEWHEELUP = System::Byte(0x86);
static const System::Byte VK_MOUSEWHEELDOWN = System::Byte(0x87);
extern DELPHI_PACKAGE int vLastWheelDelta;
extern DELPHI_PACKAGE bool __fastcall IsKeyDown(System::WideChar c)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsKeyDown(int vk)/* overload */;
extern DELPHI_PACKAGE int __fastcall KeyPressed(int minVkCode = 0x0);
extern DELPHI_PACKAGE System::UnicodeString __fastcall VirtualKeyCodeToKeyName(int vk);
extern DELPHI_PACKAGE int __fastcall KeyNameToVirtualKeyCode(const System::UnicodeString keyName);
extern DELPHI_PACKAGE int __fastcall CharToVirtualKeyCode(System::WideChar c);
extern DELPHI_PACKAGE void __fastcall KeyboardNotifyWheelMoved(int wheelDelta);
}	/* namespace Keyboard */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_KEYBOARD)
using namespace Gls::Keyboard;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_KeyboardHPP
