// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Cg.Import.pas' rev: 35.00 (Windows)

#ifndef Cg_ImportHPP
#define Cg_ImportHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cg
{
namespace Import
{
//-- forward type declarations -----------------------------------------------
struct DECLSPEC_DRECORD _CGcontext
{
};


struct DECLSPEC_DRECORD _CGprogram
{
};


struct DECLSPEC_DRECORD _CGparameter
{
};


//-- type declarations -------------------------------------------------------
typedef char * PCharCG;

typedef char * *PPCharCG;

typedef System::AnsiString StringCG;

typedef int TCGbool;

typedef int CGbool;

typedef _CGcontext *PCGcontext;

typedef PCGcontext CGcontext;

typedef _CGprogram *PCGprogram;

typedef PCGprogram CGprogram;

typedef _CGparameter *PCGparameter;

typedef PCGparameter CGparameter;

enum DECLSPEC_DENUM TCGtype : unsigned int { CG_UNKNOWN_TYPE, CG_STRUCT, CG_ARRAY, CG_TYPE_START_ENUM = 1024, CG_HALF, CG_HALF2, CG_HALF3, CG_HALF4, CG_HALF1x1, CG_HALF1x2, CG_HALF1x3, CG_HALF1x4, CG_HALF2x1, CG_HALF2x2, CG_HALF2x3, CG_HALF2x4, CG_HALF3x1, CG_HALF3x2, CG_HALF3x3, CG_HALF3x4, CG_HALF4x1, CG_HALF4x2, CG_HALF4x3, CG_HALF4x4, CG_FLOAT, CG_FLOAT2, CG_FLOAT3, CG_FLOAT4, CG_FLOAT1x1, CG_FLOAT1x2, CG_FLOAT1x3, CG_FLOAT1x4, CG_FLOAT2x1, CG_FLOAT2x2, CG_FLOAT2x3, CG_FLOAT2x4, CG_FLOAT3x1, CG_FLOAT3x2, CG_FLOAT3x3, CG_FLOAT3x4, CG_FLOAT4x1, CG_FLOAT4x2, CG_FLOAT4x3, CG_FLOAT4x4, CG_SAMPLER1D, CG_SAMPLER2D, CG_SAMPLER3D, CG_SAMPLERRECT, CG_SAMPLERCUBE, CG_FIXED, CG_FIXED2, CG_FIXED3, CG_FIXED4, CG_FIXED1x1, CG_FIXED1x2, CG_FIXED1x3, CG_FIXED1x4, 
	CG_FIXED2x1, CG_FIXED2x2, CG_FIXED2x3, CG_FIXED2x4, CG_FIXED3x1, CG_FIXED3x2, CG_FIXED3x3, CG_FIXED3x4, CG_FIXED4x1, CG_FIXED4x2, CG_FIXED4x3, CG_FIXED4x4, CG_HALF1, CG_FLOAT1, CG_FIXED1, CG_INT, CG_INT1, CG_INT2, CG_INT3, CG_INT4, CG_INT1x1, CG_INT1x2, CG_INT1x3, CG_INT1x4, CG_INT2x1, CG_INT2x2, CG_INT2x3, CG_INT2x4, CG_INT3x1, CG_INT3x2, CG_INT3x3, CG_INT3x4, CG_INT4x1, CG_INT4x2, CG_INT4x3, CG_INT4x4, CG_BOOL, CG_BOOL1, CG_BOOL2, CG_BOOL3, CG_BOOL4, CG_BOOL1x1, CG_BOOL1x2, CG_BOOL1x3, CG_BOOL1x4, CG_BOOL2x1, CG_BOOL2x2, CG_BOOL2x3, CG_BOOL2x4, CG_BOOL3x1, CG_BOOL3x2, CG_BOOL3x3, CG_BOOL3x4, CG_BOOL4x1, CG_BOOL4x2, CG_BOOL4x3, CG_BOOL4x4 };

typedef TCGtype CGtype;

enum DECLSPEC_DENUM TCGresource : unsigned int { CG_TEXUNIT0 = 2048, CG_TEXUNIT1, CG_TEXUNIT2, CG_TEXUNIT3, CG_TEXUNIT4, CG_TEXUNIT5, CG_TEXUNIT6, CG_TEXUNIT7, CG_TEXUNIT8, CG_TEXUNIT9, CG_TEXUNIT10, CG_TEXUNIT11, CG_TEXUNIT12, CG_TEXUNIT13, CG_TEXUNIT14, CG_TEXUNIT15, CG_ATTR0 = 2113, CG_ATTR1, CG_ATTR2, CG_ATTR3, CG_ATTR4, CG_ATTR5, CG_ATTR6, CG_ATTR7, CG_ATTR8, CG_ATTR9, CG_ATTR10, CG_ATTR11, CG_ATTR12, CG_ATTR13, CG_ATTR14, CG_ATTR15, CG_C = 2178, CG_TEX0, CG_TEX1, CG_TEX2, CG_TEX3 = 2192, CG_TEX4, CG_TEX5, CG_TEX6, CG_TEX7, CG_HPOS = 2243, CG_COL0 = 2245, CG_COL1, CG_COL2, CG_COL3, CG_PSIZ = 2309, CG_WPOS = 2373, CG_POSITION0 = 2437, CG_POSITION1, CG_POSITION2, CG_POSITION3, CG_POSITION4, CG_POSITION5, CG_POSITION6, CG_POSITION7, CG_POSITION8, 
	CG_POSITION9, CG_POSITION10, CG_POSITION11, CG_POSITION12, CG_POSITION13, CG_POSITION14, CG_POSITION15, CG_DIFFUSE0 = 2501, CG_TANGENT0 = 2565, CG_TANGENT1, CG_TANGENT2, CG_TANGENT3, CG_TANGENT4, CG_TANGENT5, CG_TANGENT6, CG_TANGENT7, CG_TANGENT8, CG_TANGENT9, CG_TANGENT10, CG_TANGENT11, CG_TANGENT12, CG_TANGENT13, CG_TANGENT14, CG_TANGENT15, CG_SPECULAR0 = 2629, CG_BLENDINDICES0 = 2693, CG_BLENDINDICES1, CG_BLENDINDICES2, CG_BLENDINDICES3, CG_BLENDINDICES4, CG_BLENDINDICES5, CG_BLENDINDICES6, CG_BLENDINDICES7, CG_BLENDINDICES8, CG_BLENDINDICES9, CG_BLENDINDICES10, CG_BLENDINDICES11, CG_BLENDINDICES12, CG_BLENDINDICES13, CG_BLENDINDICES14, CG_BLENDINDICES15, CG_COLOR0 = 2757, CG_COLOR1, CG_COLOR2, CG_COLOR3, CG_COLOR4, CG_COLOR5, CG_COLOR6, CG_COLOR7, 
	CG_COLOR8, CG_COLOR9, CG_COLOR10, CG_COLOR11, CG_COLOR12, CG_COLOR13, CG_COLOR14, CG_COLOR15, CG_PSIZE0 = 2821, CG_PSIZE1, CG_PSIZE2, CG_PSIZE3, CG_PSIZE4, CG_PSIZE5, CG_PSIZE6, CG_PSIZE7, CG_PSIZE8, CG_PSIZE9, CG_PSIZE10, CG_PSIZE11, CG_PSIZE12, CG_PSIZE13, CG_PSIZE14, CG_PSIZE15, CG_BINORMAL0 = 2885, CG_BINORMAL1, CG_BINORMAL2, CG_BINORMAL3, CG_BINORMAL4, CG_BINORMAL5, CG_BINORMAL6, CG_BINORMAL7, CG_BINORMAL8, CG_BINORMAL9, CG_BINORMAL10, CG_BINORMAL11, CG_BINORMAL12, CG_BINORMAL13, CG_BINORMAL14, CG_BINORMAL15, CG_FOG0 = 2917, CG_FOG1, CG_FOG2, CG_FOG3, CG_FOG4, CG_FOG5, CG_FOG6, CG_FOG7, CG_FOG8, CG_FOG9, CG_FOG10, CG_FOG11, CG_FOG12, CG_FOG13, CG_FOG14, CG_FOG15, CG_DEPTH0, CG_DEPTH1, CG_DEPTH2, CG_DEPTH3, CG_DEPTH4, CG_DEPTH5, CG_DEPTH6, CG_DEPTH7, 
	CG_DEPTH8, CG_DEPTH9 = 29542, CG_DEPTH10 = 2943, CG_DEPTH11, CG_DEPTH12, CG_DEPTH13, CG_DEPTH14, CG_DEPTH15, CG_SAMPLE0, CG_SAMPLE1, CG_SAMPLE2, CG_SAMPLE3, CG_SAMPLE4, CG_SAMPLE5, CG_SAMPLE6, CG_SAMPLE7, CG_SAMPLE8, CG_SAMPLE9, CG_SAMPLE10, CG_SAMPLE11, CG_SAMPLE12, CG_SAMPLE13, CG_SAMPLE14, CG_SAMPLE15, CG_BLENDWEIGHT0 = 3028, CG_BLENDWEIGHT1, CG_BLENDWEIGHT2, CG_BLENDWEIGHT3, CG_BLENDWEIGHT4, CG_BLENDWEIGHT5, CG_BLENDWEIGHT6, CG_BLENDWEIGHT7, CG_BLENDWEIGHT8, CG_BLENDWEIGHT9, CG_BLENDWEIGHT10, CG_BLENDWEIGHT11, CG_BLENDWEIGHT12, CG_BLENDWEIGHT13, CG_BLENDWEIGHT14, CG_BLENDWEIGHT15, CG_NORMAL0 = 3092, CG_NORMAL1, CG_NORMAL2, CG_NORMAL3, CG_NORMAL4, CG_NORMAL5, CG_NORMAL6, CG_NORMAL7, CG_NORMAL8, CG_NORMAL9, CG_NORMAL10, CG_NORMAL11, CG_NORMAL12, 
	CG_NORMAL13, CG_NORMAL14, CG_NORMAL15, CG_FOGCOORD = 3156, CG_TEXCOORD0 = 3220, CG_TEXCOORD1, CG_TEXCOORD2, CG_TEXCOORD3, CG_TEXCOORD4, CG_TEXCOORD5, CG_TEXCOORD6, CG_TEXCOORD7, CG_TEXCOORD8, CG_TEXCOORD9, CG_TEXCOORD10, CG_TEXCOORD11, CG_TEXCOORD12, CG_TEXCOORD13, CG_TEXCOORD14, CG_TEXCOORD15, CG_COMBINER_CONST0 = 3284, CG_COMBINER_CONST1, CG_COMBINER_STAGE_CONST0, CG_COMBINER_STAGE_CONST1, CG_OFFSET_TEXTURE_MATRIX, CG_OFFSET_TEXTURE_SCALE, CG_OFFSET_TEXTURE_BIAS, CG_CONST_EYE, CG_TESSFACTOR = 3255, CG_UNDEFINED };

typedef TCGresource CGresource;

enum DECLSPEC_DENUM TCGprofile : unsigned int { CG_PROFILE_START = 6144, CG_PROFILE_UNKNOWN, CG_PROFILE_VP20, CG_PROFILE_FP20, CG_PROFILE_VP30, CG_PROFILE_FP30, CG_PROFILE_ARBVP1, CG_PROFILE_ARBFP1 = 7000, CG_PROFILE_VP40, CG_PROFILE_FP40 = 6151, CG_PROFILE_VS_1_1 = 6153, CG_PROFILE_VS_2_0, CG_PROFILE_VS_2_X, CG_PROFILE_PS_1_1 = 6159, CG_PROFILE_PS_1_2, CG_PROFILE_PS_1_3, CG_PROFILE_PS_2_0, CG_PROFILE_PS_2_X, CG_PROFILE_MAX = 7100 };

typedef TCGprofile CGprofile;

typedef unsigned *PCGerror;

typedef unsigned TCGerror;

typedef unsigned CGerror;

enum DECLSPEC_DENUM TCGenum : unsigned int { CG_UNKNOWN = 4096, CG_IN, CG_OUT, CG_INOUT, CG_MIXED, CG_VARYING, CG_UNIFORM, CG_CONSTANT, CG_PROGRAM_SOURCE, CG_PROGRAM_ENTRY, CG_COMPILED_PROGRAM, CG_PROGRAM_PROFILE, CG_GLOBAL, CG_PROGRAM, CG_DEFAULT, CG_ERROR, CG_SOURCE, CG_OBJECT, CG_COMPILE_MANUAL, CG_COMPILE_IMMEDIATE, CG_COMPILE_LAZY, CG_CURRENT, CG_LITERAL, CG_VERSION };

typedef TCGenum CGenum;

typedef void __cdecl (*TCGerrorCallbackFunc)(void);

typedef TCGerrorCallbackFunc CGerrorCallbackFunc;

//-- var, const, procedure ---------------------------------------------------
#define CgLibrary L"cg.dll"
static const System::Int8 CG_VERSION_1_2 = System::Int8(0x1);
static const System::Word CG_VERSION_NUM = System::Word(0x4b0);
static const int CG_FALSE = int(0);
static const int CG_TRUE = int(1);
static const System::Int8 CG_NO_ERROR = System::Int8(0x0);
static const System::Int8 CG_COMPILER_ERROR = System::Int8(0x1);
static const System::Int8 CG_INVALID_PARAMETER_ERROR = System::Int8(0x2);
static const System::Int8 CG_INVALID_PROFILE_ERROR = System::Int8(0x3);
static const System::Int8 CG_PROGRAM_LOAD_ERROR = System::Int8(0x4);
static const System::Int8 CG_PROGRAM_BIND_ERROR = System::Int8(0x5);
static const System::Int8 CG_PROGRAM_NOT_LOADED_ERROR = System::Int8(0x6);
static const System::Int8 CG_UNSUPPORTED_GL_EXTENSION_ERROR = System::Int8(0x7);
static const System::Int8 CG_INVALID_VALUE_TYPE_ERROR = System::Int8(0x8);
static const System::Int8 CG_NOT_MATRIX_PARAM_ERROR = System::Int8(0x9);
static const System::Int8 CG_INVALID_ENUMERANT_ERROR = System::Int8(0xa);
static const System::Int8 CG_NOT_4x4_MATRIX_ERROR = System::Int8(0xb);
static const System::Int8 CG_FILE_READ_ERROR = System::Int8(0xc);
static const System::Int8 CG_FILE_WRITE_ERROR = System::Int8(0xd);
static const System::Int8 CG_NVPARSE_ERROR = System::Int8(0xe);
static const System::Int8 CG_MEMORY_ALLOC_ERROR = System::Int8(0xf);
static const System::Int8 CG_INVALID_CONTEXT_HANDLE_ERROR = System::Int8(0x10);
static const System::Int8 CG_INVALID_PROGRAM_HANDLE_ERROR = System::Int8(0x11);
static const System::Int8 CG_INVALID_PARAM_HANDLE_ERROR = System::Int8(0x12);
static const System::Int8 CG_UNKNOWN_PROFILE_ERROR = System::Int8(0x13);
static const System::Int8 CG_VAR_ARG_ERROR = System::Int8(0x14);
static const System::Int8 CG_INVALID_DIMENSION_ERROR = System::Int8(0x15);
static const System::Int8 CG_ARRAY_PARAM_ERROR = System::Int8(0x16);
static const System::Int8 CG_OUT_OF_ARRAY_BOUNDS_ERROR = System::Int8(0x17);
static const System::Int8 CG_CONFLICTING_TYPES_ERROR = System::Int8(0x18);
static const System::Int8 CG_CONFLICTING_PARAMETER_TYPES_ERROR = System::Int8(0x19);
static const System::Int8 CG_PARAMETER_IS_NOT_SHARED_ERROR = System::Int8(0x1a);
static const System::Int8 CG_INVALID_PARAMETER_VARIABILITY_ERROR = System::Int8(0x1b);
static const System::Int8 CG_CANNOT_DESTROY_PARAMETER_ERROR = System::Int8(0x1c);
static const System::Int8 CG_NOT_ROOT_PARAMETER_ERROR = System::Int8(0x1d);
static const System::Int8 CG_PARAMETERS_DO_NOT_MATCH_ERROR = System::Int8(0x1e);
static const System::Int8 CG_IS_NOT_PROGRAM_PARAMETER_ERROR = System::Int8(0x1f);
static const System::Int8 CG_INVALID_PARAMETER_TYPE_ERROR = System::Int8(0x20);
static const System::Int8 CG_PARAMETER_IS_NOT_RESIZABLE_ARRAY_ERROR = System::Int8(0x21);
static const System::Int8 CG_INVALID_SIZE_ERROR = System::Int8(0x22);
static const System::Int8 CG_BIND_CREATES_CYCLE_ERROR = System::Int8(0x23);
static const System::Int8 CG_ARRAY_TYPES_DO_NOT_MATCH_ERROR = System::Int8(0x24);
static const System::Int8 CG_ARRAY_DIMENSIONS_DO_NOT_MATCH_ERROR = System::Int8(0x25);
static const System::Int8 CG_ARRAY_HAS_WRONG_DIMENSION_ERROR = System::Int8(0x26);
static const System::Int8 CG_TYPE_IS_NOT_DEFINED_IN_PROGRAM_ERROR = System::Int8(0x27);
extern "C" PCGcontext __cdecl cgCreateContext(void);
extern "C" void __cdecl cgDestroyContext(PCGcontext ctx);
extern "C" int __cdecl cgIsContext(PCGcontext ctx);
extern "C" System::WideChar * __cdecl cgGetLastListing(PCGcontext ctx);
extern "C" void __cdecl cgSetAutoCompile(PCGcontext ctx, TCGenum flag);
extern "C" PCGprogram __cdecl cgCreateProgram(PCGcontext ctx, TCGenum program_type, const char * _program, TCGprofile profile, const char * entry, const PPCharCG args);
extern "C" PCGprogram __cdecl cgCreateProgramFromFile(PCGcontext ctx, TCGenum program_type, const char * program_file, TCGprofile profile, const char * entry, const PPCharCG args);
extern "C" PCGprogram __cdecl cgCopyProgram(PCGprogram _program);
extern "C" void __cdecl cgDestroyProgram(PCGprogram _program);
extern "C" PCGprogram __cdecl cgGetFirstProgram(PCGcontext ctx);
extern "C" PCGprogram __cdecl cgGetNextProgram(PCGprogram current);
extern "C" PCGcontext __cdecl cgGetProgramContext(PCGprogram prog);
extern "C" int __cdecl cgIsProgram(PCGprogram _program);
extern "C" void __cdecl cgCompileProgram(PCGprogram _program);
extern "C" int __cdecl cgIsProgramCompiled(PCGprogram _program);
extern "C" char * __cdecl cgGetProgramString(PCGprogram prog, TCGenum pname);
extern "C" TCGprofile __cdecl cgGetProgramProfile(PCGprogram prog);
extern "C" PCGparameter __cdecl cgCreateParameter(PCGcontext ctx, TCGtype type_);
extern "C" PCGparameter __cdecl cgCreateParameterArray(PCGcontext ctx, TCGtype type_, int length);
extern "C" PCGparameter __cdecl cgCreateParameterMultiDimArray(PCGcontext ctx, TCGtype type_, int dim, const System::PInteger lengths);
extern "C" void __cdecl cgDestroyParameter(PCGparameter param);
extern "C" void __cdecl cgConnectParameter(PCGparameter from, PCGparameter to_);
extern "C" void __cdecl cgDisconnectParameter(PCGparameter param);
extern "C" PCGparameter __cdecl cgGetConnectedParameter(PCGparameter param);
extern "C" int __cdecl cgGetNumConnectedToParameters(PCGparameter param);
extern "C" PCGparameter __cdecl cgGetConnectedToParameter(PCGparameter param, int index);
extern "C" PCGparameter __cdecl cgGetNamedParameter(PCGprogram prog, const char * name);
extern "C" PCGparameter __cdecl cgGetNamedProgramParameter(PCGprogram prog, TCGenum name_space, const char * name);
extern "C" PCGparameter __cdecl cgGetFirstParameter(PCGprogram prog, TCGenum name_space);
extern "C" PCGparameter __cdecl cgGetNextParameter(PCGparameter current);
extern "C" PCGparameter __cdecl cgGetFirstLeafParameter(PCGprogram prog, TCGenum name_space);
extern "C" PCGparameter __cdecl cgGetNextLeafParameter(PCGparameter current);
extern "C" PCGparameter __cdecl cgGetFirstStructParameter(PCGparameter param);
extern "C" PCGparameter __cdecl cgGetNamedStructParameter(PCGparameter param, const char * name);
extern "C" PCGparameter __cdecl cgGetFirstDependentParameter(PCGparameter param);
extern "C" PCGparameter __cdecl cgGetArrayParameter(PCGparameter aparam, int index);
extern "C" int __cdecl cgGetArrayDimension(PCGparameter param);
extern "C" TCGtype __cdecl cgGetArrayType(PCGparameter param);
extern "C" int __cdecl cgGetArraySize(PCGparameter param, int dimension);
extern "C" void __cdecl cgSetArraySize(PCGparameter param, int size);
extern "C" void __cdecl cgSetMultiDimArraySize(PCGparameter param, const System::PInteger sizes);
extern "C" PCGprogram __cdecl cgGetParameterProgram(PCGparameter param);
extern "C" PCGcontext __cdecl cgGetParameterContext(PCGparameter param);
extern "C" int __cdecl cgIsParameter(PCGparameter param);
extern "C" char * __cdecl cgGetParameterName(PCGparameter param);
extern "C" TCGtype __cdecl cgGetParameterType(PCGparameter param);
extern "C" TCGtype __cdecl cgGetParameterNamedType(PCGparameter param);
extern "C" char * __cdecl cgGetParameterSemantic(PCGparameter param);
extern "C" TCGresource __cdecl cgGetParameterResource(PCGparameter param);
extern "C" TCGresource __cdecl cgGetParameterBaseResource(PCGparameter param);
extern "C" unsigned __cdecl cgGetParameterResourceIndex(PCGparameter param);
extern "C" TCGenum __cdecl cgGetParameterVariability(PCGparameter param);
extern "C" TCGenum __cdecl cgGetParameterDirection(PCGparameter param);
extern "C" int __cdecl cgIsParameterReferenced(PCGparameter param);
extern "C" System::PDouble __cdecl cgGetParameterValues(PCGparameter param, TCGenum value_type, /* out */ int &nvalues);
extern "C" int __cdecl cgGetParameterOrdinalNumber(PCGparameter param);
extern "C" int __cdecl cgIsParameterGlobal(PCGparameter param);
extern "C" int __cdecl cgGetParameterIndex(PCGparameter param);
extern "C" void __cdecl cgSetParameterVariability(PCGparameter param, TCGenum vary);
extern "C" void __cdecl cgSetParameterSemantic(PCGparameter param, const char * semantic);
extern "C" void __cdecl cgSetParameter1f(PCGparameter param, float x);
extern "C" void __cdecl cgSetParameter2f(PCGparameter param, float x, float y);
extern "C" void __cdecl cgSetParameter3f(PCGparameter param, float x, float y, float z);
extern "C" void __cdecl cgSetParameter4f(PCGparameter param, float x, float y, float z, float w);
extern "C" void __cdecl cgSetParameter1d(PCGparameter param, double x);
extern "C" void __cdecl cgSetParameter2d(PCGparameter param, double x, double y);
extern "C" void __cdecl cgSetParameter3d(PCGparameter param, double x, double y, double z);
extern "C" void __cdecl cgSetParameter4d(PCGparameter param, double x, double y, double z, double w);
extern "C" void __cdecl cgSetParameter1fv(PCGparameter param, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgSetParameter2fv(PCGparameter param, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgSetParameter3fv(PCGparameter param, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgSetParameter4fv(PCGparameter param, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgSetParameter1dv(PCGparameter param, const System::PDouble x);
extern "C" void __cdecl cgSetParameter2dv(PCGparameter param, const System::PDouble x);
extern "C" void __cdecl cgSetParameter3dv(PCGparameter param, const System::PDouble x);
extern "C" void __cdecl cgSetParameter4dv(PCGparameter param, const System::PDouble x);
extern "C" void __cdecl cgSetMatrixParameterdr(PCGparameter param, const System::PDouble matrix);
extern "C" void __cdecl cgSetMatrixParameterfr(PCGparameter param, const Winapi::Windows::PSingle matrix);
extern "C" void __cdecl cgSetMatrixParameterdc(PCGparameter param, const System::PDouble matrix);
extern "C" void __cdecl cgSetMatrixParameterfc(PCGparameter param, const Winapi::Windows::PSingle matrix);
extern "C" char * __cdecl cgGetTypeString(TCGtype _type);
extern "C" TCGtype __cdecl cgGetType(const char * type_string);
extern "C" TCGtype __cdecl cgGetNamedUserType(PCGprogram program_, const char * name);
extern "C" int __cdecl cgGetNumUserTypes(PCGprogram program_);
extern "C" TCGtype __cdecl cgGetUserType(PCGprogram program_, int index);
extern "C" int __cdecl cgGetNumParentTypes(TCGtype type_);
extern "C" TCGtype __cdecl cgGetParentType(TCGtype type_, int index);
extern "C" int __cdecl cgIsParentType(TCGtype parent, TCGtype child);
extern "C" int __cdecl cgIsInterfaceType(TCGtype type_);
extern "C" char * __cdecl cgGetResourceString(TCGresource resource);
extern "C" TCGresource __cdecl cgGetResource(const char * resource_string);
extern "C" char * __cdecl cgGetEnumString(TCGenum en);
extern "C" TCGenum __cdecl cgGetEnum(const char * enum_string);
extern "C" char * __cdecl cgGetProfileString(TCGprofile profile);
extern "C" TCGprofile __cdecl cgGetProfile(const char * profile_string);
extern "C" unsigned __cdecl cgGetError(void);
extern "C" char * __cdecl cgGetErrorString(unsigned error);
extern "C" char * __cdecl cgGetLastErrorString(PCGerror error);
extern "C" void __cdecl cgSetErrorCallback(TCGerrorCallbackFunc func);
extern "C" TCGerrorCallbackFunc __cdecl cgGetErrorCallback(void);
extern "C" char * __cdecl cgGetString(TCGenum sname);
extern "C" PCGparameter __cdecl cgGetNextParameter_depr1_1(PCGparameter current);
extern "C" PCGparameter __cdecl cgGetNextLeafParameter_depr1_1(PCGparameter current);
}	/* namespace Import */
}	/* namespace Cg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CG_IMPORT)
using namespace Cg::Import;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CG)
using namespace Cg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cg_ImportHPP
