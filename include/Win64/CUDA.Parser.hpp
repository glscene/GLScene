// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CUDA.Parser.pas' rev: 35.00 (Windows)

#ifndef Cuda_ParserHPP
#define Cuda_ParserHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <CUDA.Runtime.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cuda
{
namespace Parser
{
//-- forward type declarations -----------------------------------------------
struct TCUDATexRefInfo;
struct TCUDAFuncArgInfo;
struct TCUDAFuncInfo;
struct TCUDAConstantInfo;
class DELPHICLASS TCUDAModuleInfo;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TCUDAType : unsigned char { customType, char1, uchar1, char2, uchar2, char3, uchar3, char4, uchar4, short1, ushort1, short2, ushort2, short3, ushort3, short4, ushort4, int1, uint1, int2, uint2, int3, uint3, int4, uint4, long1, ulong1, long2, ulong2, long3, ulong3, long4, ulong4, float1, float2, float3, float4, longlong1, ulonglong1, longlong2, ulonglong2, longlong3, ulonglong3, longlong4, ulonglong4, double1, double2, double3, double4, int8, int16, int32, uint8, uint16, uint32 };

struct DECLSPEC_DRECORD TCUDATexRefInfo
{
public:
	System::UnicodeString Name;
	TCUDAType DataType;
	System::Byte Dim;
	Cuda::Runtime::TcudaTextureReadMode ReadMode;
};


struct DECLSPEC_DRECORD TCUDAFuncArgInfo
{
public:
	System::UnicodeString Name;
	TCUDAType DataType;
	System::UnicodeString CustomType;
	bool Ref;
};


struct DECLSPEC_DRECORD TCUDAFuncInfo
{
	
private:
	typedef System::DynamicArray<TCUDAFuncArgInfo> _TCUDAFuncInfo__1;
	
	
public:
	System::UnicodeString Name;
	System::UnicodeString KernelName;
	_TCUDAFuncInfo__1 Args;
};


struct DECLSPEC_DRECORD TCUDAConstantInfo
{
public:
	System::UnicodeString Name;
	TCUDAType DataType;
	System::UnicodeString CustomType;
	bool Ref;
	bool DefValue;
};


class PASCALIMPLEMENTATION TCUDAModuleInfo : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<TCUDATexRefInfo> _TCUDAModuleInfo__1;
	
	typedef System::DynamicArray<TCUDAFuncInfo> _TCUDAModuleInfo__2;
	
	typedef System::DynamicArray<TCUDAConstantInfo> _TCUDAModuleInfo__3;
	
	
private:
	System::Classes::TStrings* ping;
	System::Classes::TStrings* pong;
	void __fastcall Reset();
	void __fastcall BreakStrings(System::Classes::TStrings* inlist, System::Classes::TStrings* outlist);
	void __fastcall RemoveComents(System::Classes::TStrings* inlist, System::Classes::TStrings* outlist);
	void __fastcall RemoveSpaces(System::Classes::TStrings* inlist, System::Classes::TStrings* outlist);
	void __fastcall ReplaceUnsigned(System::Classes::TStrings* inlist, System::Classes::TStrings* outlist);
	void __fastcall FindTexRef(System::Classes::TStrings* inlist);
	void __fastcall FindConst(System::Classes::TStrings* inlist);
	void __fastcall FindFunc(System::Classes::TStrings* inlist);
	void __fastcall FindFuncKernelName(System::Classes::TStrings* inlist);
	
public:
	System::Classes::TComponent* Owner;
	_TCUDAModuleInfo__1 TexRef;
	_TCUDAModuleInfo__2 Func;
	_TCUDAModuleInfo__3 Constant;
	__fastcall TCUDAModuleInfo();
	__fastcall virtual ~TCUDAModuleInfo();
	void __fastcall ParseModule(System::Classes::TStrings* ASource, System::Classes::TStrings* AProduct);
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Parser */
}	/* namespace Cuda */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA_PARSER)
using namespace Cuda::Parser;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA)
using namespace Cuda;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cuda_ParserHPP
