// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.ModuleLoader.pas' rev: 35.00 (Windows)

#ifndef Gls_ModuleloaderHPP
#define Gls_ModuleloaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Moduleloader
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef NativeUInt TModuleHandle;

//-- var, const, procedure ---------------------------------------------------
static const NativeUInt INVALID_MODULEHANDLE_VALUE = NativeUInt(0x0);
extern DELPHI_PACKAGE bool __stdcall LoadModule(NativeUInt &Module, System::WideChar * FileName);
extern DELPHI_PACKAGE bool __stdcall LoadModuleEx(NativeUInt &Module, System::WideChar * FileName, unsigned Flags);
extern DELPHI_PACKAGE void __stdcall UnloadModule(NativeUInt &Module);
extern DELPHI_PACKAGE void * __stdcall GetModuleSymbol(NativeUInt Module, System::WideChar * SymbolName);
extern DELPHI_PACKAGE void * __stdcall GetModuleSymbolEx(NativeUInt Module, System::WideChar * SymbolName, bool &Accu);
extern DELPHI_PACKAGE bool __stdcall ReadModuleData(NativeUInt Module, System::WideChar * SymbolName, void *Buffer, unsigned Size);
extern DELPHI_PACKAGE bool __stdcall WriteModuleData(NativeUInt Module, System::WideChar * SymbolName, void *Buffer, unsigned Size);
}	/* namespace Moduleloader */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_MODULELOADER)
using namespace Gls::Moduleloader;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ModuleloaderHPP
