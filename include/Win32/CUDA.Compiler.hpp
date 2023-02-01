// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CUDA.Compiler.pas' rev: 35.00 (Windows)

#ifndef Cuda_CompilerHPP
#define Cuda_CompilerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ShellAPI.hpp>
#include <Winapi.TlHelp32.hpp>
#include <System.UITypes.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <CUDA.Parser.hpp>
#include <GLS.ApplicationFileIO.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cuda
{
namespace Compiler
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCUDACompiler;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLCUDACompilerOutput : unsigned char { codeUndefined, codePtx, codeCubin, codeGpu };

enum DECLSPEC_DENUM TGLCUDAVirtArch : unsigned char { compute_10, compute_11, compute_12, compute_13, compute_20 };

enum DECLSPEC_DENUM TGLCUDARealArch : unsigned char { sm_10, sm_11, sm_12, sm_13, sm_20, sm_21 };

typedef System::Set<TGLCUDARealArch, TGLCUDARealArch::sm_10, TGLCUDARealArch::sm_21> TGLCUDARealArchs;

class PASCALIMPLEMENTATION TGLCUDACompiler : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FNVCCPath;
	System::UnicodeString FCppCompilerPath;
	System::Classes::TStringList* FProduct;
	System::UnicodeString FProjectModule;
	System::UnicodeString FSourceCodeFile;
	System::UnicodeString FConsoleContent;
	TGLCUDACompilerOutput FOutputCodeType;
	TGLCUDAVirtArch FVirtualArch;
	TGLCUDARealArchs FRealArch;
	int FMaxRegisterCount;
	Cuda::Parser::TCUDAModuleInfo* FModuleInfo;
	void __fastcall SetMaxRegisterCount(int Value);
	void __fastcall SetOutputCodeType(const TGLCUDACompilerOutput Value);
	bool __fastcall StoreProjectModule();
	void __fastcall SetRealArch(TGLCUDARealArchs AValue);
	void __fastcall SetNVCCPath(const System::UnicodeString AValue);
	void __fastcall SetCppCompilerPath(const System::UnicodeString AValue);
	
protected:
	virtual void __fastcall Loaded();
	
public:
	__fastcall virtual TGLCUDACompiler(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCUDACompiler();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall SetSourceCodeFile(const System::UnicodeString AFileName);
	bool __fastcall Compile();
	__property System::Classes::TStringList* Product = {read=FProduct, write=FProduct};
	__property Cuda::Parser::TCUDAModuleInfo* ModuleInfo = {read=FModuleInfo};
	__property System::UnicodeString ConsoleContent = {read=FConsoleContent};
	
__published:
	__property System::UnicodeString NVCCPath = {read=FNVCCPath, write=SetNVCCPath};
	__property System::UnicodeString CppCompilerPath = {read=FCppCompilerPath, write=SetCppCompilerPath};
	__property System::UnicodeString SourceCodeFile = {read=FSourceCodeFile};
	__property System::UnicodeString ProjectModule = {read=FProjectModule, write=FProjectModule, stored=StoreProjectModule};
	__property TGLCUDACompilerOutput OutputCodeType = {read=FOutputCodeType, write=SetOutputCodeType, default=1};
	__property TGLCUDARealArchs RealArchitecture = {read=FRealArch, write=SetRealArch, default=8};
	__property TGLCUDAVirtArch VirtualArchitecture = {read=FVirtualArch, write=FVirtualArch, default=3};
	__property int MaxRegisterCount = {read=FMaxRegisterCount, write=SetMaxRegisterCount, default=32};
};


typedef bool __fastcall (*TFindCuFileFunc)(System::UnicodeString &AModuleName);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TFindCuFileFunc vFindCuFileFunc;
}	/* namespace Compiler */
}	/* namespace Cuda */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA_COMPILER)
using namespace Cuda::Compiler;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA)
using namespace Cuda;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cuda_CompilerHPP
