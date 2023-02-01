// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CUDA.Utility.pas' rev: 35.00 (Windows)

#ifndef Cuda_UtilityHPP
#define Cuda_UtilityHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cuda
{
namespace Utility
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
#define CUTILDLL L"cutil64.dll"
extern DELPHI_PACKAGE char * __stdcall (*cutFindFilePath)(const char * filename, const char * executablePath);
extern DELPHI_PACKAGE bool __stdcall (*cutLoadPGMf)(const char * filename, System::PSingle &data, int &w, int &h);
extern DELPHI_PACKAGE bool __stdcall (*cutSavePGMf)(const char * filename, System::PSingle data, int w, int h);
extern DELPHI_PACKAGE bool __stdcall (*cutLoadPGMub)(const char * filename, System::PByte &data, int &w, int &h);
extern DELPHI_PACKAGE bool __stdcall (*cutLoadPPMub)(const char * filename, System::PByte &data, int &w, int &h);
extern DELPHI_PACKAGE bool __stdcall (*cutLoadPPM4ub)(const char * filename, System::PByte &data, int &w, int &h);
extern DELPHI_PACKAGE bool __stdcall (*cutLoadPGMi)(const char * filename, System::PInteger &data, int &w, int &h);
extern DELPHI_PACKAGE bool __stdcall (*cutLoadPGMs)(const char * filename, PWORD &data, int &w, int &h);
extern DELPHI_PACKAGE bool __stdcall (*cutSavePGMub)(const char * filename, System::PByte data, int w, int h);
extern DELPHI_PACKAGE bool __stdcall (*cutSavePPMub)(const char * filename, System::PByte data, int w, int h);
extern DELPHI_PACKAGE bool __stdcall (*cutSavePPM4ub)(const char * filename, System::PByte data, int w, int h);
extern DELPHI_PACKAGE bool __stdcall (*cutSavePGMi)(const char * filename, System::PInteger data, int w, int h);
extern DELPHI_PACKAGE bool __stdcall (*cutSavePGMs)(const char * filename, PWORD data, int w, int h);
extern DELPHI_PACKAGE bool __stdcall (*cutComparef)(const Winapi::Windows::PSingle reference, const Winapi::Windows::PSingle data, const unsigned len);
extern DELPHI_PACKAGE bool __stdcall (*cutComparei)(const System::PInteger reference, const System::PInteger data, const unsigned len);
extern DELPHI_PACKAGE bool __stdcall (*cutCompareuit)(const System::PInteger reference, const System::PInteger data, const unsigned len, const float epsilon, const float threshold);
extern DELPHI_PACKAGE bool __stdcall (*cutCompareub)(const System::PByte reference, const System::PByte data, const unsigned len);
extern DELPHI_PACKAGE bool __stdcall (*cutCompareubt)(const System::PByte reference, const System::PByte data, const unsigned len, const float epsilon, const float threshold);
extern DELPHI_PACKAGE bool __stdcall (*cutCompareube)(const System::PByte reference, const System::PByte data, const unsigned len, const float epsilon);
extern DELPHI_PACKAGE bool __stdcall (*cutComparefe)(const Winapi::Windows::PSingle reference, const Winapi::Windows::PSingle data, const unsigned len, const float epsilon);
extern DELPHI_PACKAGE bool __stdcall (*cutComparefet)(const Winapi::Windows::PSingle reference, const Winapi::Windows::PSingle data, const unsigned len, const float epsilon, const float threshold);
extern DELPHI_PACKAGE bool __stdcall (*cutCompareL2fe)(const Winapi::Windows::PSingle reference, const Winapi::Windows::PSingle data, const unsigned len, const float epsilon);
extern DELPHI_PACKAGE bool __stdcall (*cutCreateTimer)(unsigned &name);
extern DELPHI_PACKAGE bool __stdcall (*cutStartTimer)(const unsigned name);
extern DELPHI_PACKAGE bool __stdcall (*cutStopTimer)(const unsigned name);
extern DELPHI_PACKAGE bool __stdcall (*cutResetTimer)(const unsigned name);
extern DELPHI_PACKAGE bool __stdcall (*cutDeleteTimer)(const unsigned name);
extern DELPHI_PACKAGE float __stdcall (*cutGetTimerValue)(const unsigned name);
extern DELPHI_PACKAGE float __stdcall (*cutGetAverageTimerValue)(const unsigned name);
extern DELPHI_PACKAGE void __stdcall (*cutFree)(void * ptr);
extern DELPHI_PACKAGE bool __fastcall InitCUTIL(void);
extern DELPHI_PACKAGE void __fastcall CloseCUTIL(void);
extern DELPHI_PACKAGE bool __fastcall InitCUTILFromLibrary(const System::WideString LibName);
extern DELPHI_PACKAGE bool __fastcall IsCUTILInitialized(void);
}	/* namespace Utility */
}	/* namespace Cuda */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA_UTILITY)
using namespace Cuda::Utility;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA)
using namespace Cuda;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cuda_UtilityHPP
