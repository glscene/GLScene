// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'OpenCL.Platform.pas' rev: 35.00 (Windows)

#ifndef Opencl_PlatformHPP
#define Opencl_PlatformHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Opencl
{
namespace Platform
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::Int8 Tcl_char;

typedef System::Byte Tcl_uchar;

typedef short Tcl_short;

typedef System::Word Tcl_ushort;

typedef int Tcl_int;

typedef unsigned Tcl_uint;

typedef __int64 Tcl_long;

typedef unsigned __int64 Tcl_ulong;

typedef System::Word Tcl_half;

typedef float Tcl_float;

typedef double Tcl_double;

typedef System::Int8 *Pcl_char;

typedef System::Byte *Pcl_uchar;

typedef short *Pcl_short;

typedef System::Word *Pcl_ushort;

typedef int *Pcl_int;

typedef unsigned *Pcl_uint;

typedef __int64 *Pcl_long;

typedef unsigned __int64 *Pcl_ulong;

typedef System::Word *Pcl_half;

typedef float *Pcl_float;

typedef double *Pcl_double;

typedef NativeUInt Tsize_t;

typedef NativeUInt *Psize_t;

typedef NativeUInt intptr_t;

typedef NativeUInt *Pintptr_t;

typedef System::StaticArray<System::Int8, 2> Tcl_char2;

typedef System::StaticArray<System::Int8, 4> Tcl_char4;

typedef System::StaticArray<System::Int8, 8> Tcl_char8;

typedef System::StaticArray<System::Int8, 16> Tcl_char16;

typedef System::StaticArray<System::Byte, 2> Tcl_uchar2;

typedef System::StaticArray<System::Byte, 4> Tcl_uchar4;

typedef System::StaticArray<System::Byte, 8> Tcl_uchar8;

typedef System::StaticArray<System::Byte, 16> Tcl_uchar16;

typedef System::StaticArray<short, 2> Tcl_short2;

typedef System::StaticArray<short, 4> Tcl_short4;

typedef System::StaticArray<short, 8> Tcl_short8;

typedef System::StaticArray<short, 16> Tcl_short16;

typedef System::StaticArray<System::Word, 2> Tcl_ushort2;

typedef System::StaticArray<System::Word, 4> Tcl_ushort4;

typedef System::StaticArray<System::Word, 8> Tcl_ushort8;

typedef System::StaticArray<System::Word, 16> Tcl_ushort16;

typedef System::StaticArray<int, 2> Tcl_int2;

typedef System::StaticArray<int, 4> Tcl_int4;

typedef System::StaticArray<int, 8> Tcl_int8;

typedef System::StaticArray<int, 16> Tcl_int16;

typedef System::StaticArray<unsigned, 2> Tcl_uint2;

typedef System::StaticArray<unsigned, 4> Tcl_uint4;

typedef System::StaticArray<unsigned, 8> Tcl_uint8;

typedef System::StaticArray<unsigned, 16> Tcl_uint16;

typedef System::StaticArray<__int64, 2> Tcl_long2;

typedef System::StaticArray<__int64, 4> Tcl_long4;

typedef System::StaticArray<__int64, 8> Tcl_long8;

typedef System::StaticArray<__int64, 16> Tcl_long16;

typedef System::StaticArray<unsigned __int64, 2> Tcl_ulong2;

typedef System::StaticArray<unsigned __int64, 4> Tcl_ulong4;

typedef System::StaticArray<unsigned __int64, 8> Tcl_ulong8;

typedef System::StaticArray<unsigned __int64, 16> Tcl_ulong16;

typedef System::StaticArray<float, 2> Tcl_float2;

typedef System::StaticArray<float, 4> Tcl_float4;

typedef System::StaticArray<float, 8> Tcl_float8;

typedef System::StaticArray<float, 16> Tcl_float16;

typedef System::StaticArray<double, 2> Tcl_double2;

typedef System::StaticArray<double, 4> Tcl_double4;

typedef System::StaticArray<double, 8> Tcl_double8;

typedef System::StaticArray<double, 16> Tcl_double16;

//-- var, const, procedure ---------------------------------------------------
#define CL_APT_CALL L"stdcall"
#define CL_CALLBACK L"stdcall"
static const System::Int8 CL_CHAR_BIT = System::Int8(0x8);
static const System::Int8 CL_SCHAR_MAX = System::Int8(0x7f);
static const System::Int8 CL_SCHAR_MIN = System::Int8(-128);
static const System::Int8 CL_CHAR_MAX = System::Int8(0x7f);
static const System::Int8 CL_CHAR_MIN = System::Int8(-128);
static const System::Byte CL_UCHAR_MAX = System::Byte(0xff);
static const System::Word CL_SHRT_MAX = System::Word(0x7fff);
static const short CL_SHRT_MIN = short(-32768);
static const System::Word CL_USHRT_MAX = System::Word(0xffff);
static const int CL_INT_MAX = int(0x7fffffff);
static const int CL_INT_MIN = int(-2147483648);
static const unsigned CL_UINT_MAX = unsigned(0xffffffff);
static const __int64 CL_LONG_MAX = 0x7fffffffffffffffLL;
static const __int64 CL_LONG_MIN = (-0x7fffffffffffffffLL-1);
static const unsigned __int64 CL_ULONG_MAX = 0xffffffffffffffffULL;
static const System::Int8 CL_FLT_DIG = System::Int8(0x6);
static const System::Int8 CL_FLT_MANT_DIG = System::Int8(0x18);
static const int CL_FLT_MAX_10_EXP = int(0x26);
static const int CL_FLT_MAX_EXP = int(0x80);
static const System::Int8 CL_FLT_MIN_10_EXP = System::Int8(-37);
static const System::Int8 CL_FLT_MIN_EXP = System::Int8(-125);
static const System::Int8 CL_FLT_RADIX = System::Int8(0x2);
static const System::Extended CL_FLT_MAX = 1.700000E+38;
static const System::Extended CL_FLT_MIN = 1.170000E-38;
static const System::Extended CL_FLT_EPSILON = 1.000000E-07;
static const System::Int8 CL_DBL_DIG = System::Int8(0xf);
static const System::Int8 CL_DBL_MANT_DIG = System::Int8(0x35);
static const int CL_DBL_MAX_10_EXP = int(0x134);
static const int CL_DBL_MAX_EXP = int(0x400);
static const short CL_DBL_MIN_10_EXP = short(-307);
static const short CL_DBL_MIN_EXP = short(-1021);
static const System::Int8 CL_DBL_RADIX = System::Int8(0x2);
static const System::Extended CL_DBL_MAX = 8.980000E+307;
static const System::Extended CL_DBL_MIN = 2.200000E-308;
static const System::Extended CL_DBL_EPSILON = 2.200000E-16;
static const System::Extended CL_M_E = 2.718282E+00;
static const System::Extended CL_M_LOG2E = 1.442695E+00;
static const System::Extended CL_M_LOG10E = 4.342945E-01;
static const System::Extended CL_M_LN2 = 6.931472E-01;
static const System::Extended CL_M_LN10 = 2.302585E+00;
static const System::Extended CL_M_PI = 3.141593E+00;
static const System::Extended CL_M_PI_2 = 1.570796E+00;
static const System::Extended CL_M_PI_4 = 7.853982E-01;
static const System::Extended CL_M_1_PI = 3.183099E-01;
static const System::Extended CL_M_2_PI = 6.366198E-01;
static const System::Extended CL_M_2_SQRTPI = 1.128379E+00;
static const System::Extended CL_M_SQRT2 = 1.414214E+00;
static const System::Extended CL_M_SQRT1_2 = 7.071068E-01;
static const System::Extended CL_M_E_F = 2.718282E+00;
static const System::Extended CL_M_LOG2E_F = 1.442695E+00;
static const System::Extended CL_M_LOG10E_F = 4.342945E-01;
static const System::Extended CL_M_LN2_F = 6.931472E-01;
static const System::Extended CL_M_LN10_F = 2.302585E+00;
static const System::Extended CL_M_PI_F = 3.141593E+00;
static const System::Extended CL_M_PI_2_F = 1.570796E+00;
static const System::Extended CL_M_PI_4_F = 7.853982E-01;
static const System::Extended CL_M_1_PI_F = 3.183099E-01;
static const System::Extended CL_M_2_PI_F = 6.366197E-01;
static const System::Extended CL_M_2_SQRTPI_F = 1.128379E+00;
static const System::Extended CL_M_SQRT2_F = 1.414214E+00;
static const System::Extended CL_M_SQRT1_2_F = 7.071068E-01;
static const System::Int8 CL_NAN = System::Int8(0x0);
static const System::Extended CL_HUGE_VALF = 1.000000E+50;
static const System::Extended CL_HUGE_VAL = 1.000000E+500;
static const System::Extended CL_MAXFLOAT = 1.700000E+38;
static const System::Extended CL_INFINITY = 1.000000E+50;
}	/* namespace Platform */
}	/* namespace Opencl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_OPENCL_PLATFORM)
using namespace Opencl::Platform;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_OPENCL)
using namespace Opencl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Opencl_PlatformHPP
