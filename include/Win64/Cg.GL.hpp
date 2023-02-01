// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Cg.GL.pas' rev: 35.00 (Windows)

#ifndef Cg_GlHPP
#define Cg_GlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Cg.Import.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cg
{
namespace Gl
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef unsigned TCGGLenum;

typedef unsigned CGGLenum;

//-- var, const, procedure ---------------------------------------------------
#define CgGLlibrary L"cgGL.dll"
static const System::Int8 CG_GL_MATRIX_IDENTITY = System::Int8(0x0);
static const System::Int8 CG_GL_MATRIX_TRANSPOSE = System::Int8(0x1);
static const System::Int8 CG_GL_MATRIX_INVERSE = System::Int8(0x2);
static const System::Int8 CG_GL_MATRIX_INVERSE_TRANSPOSE = System::Int8(0x3);
static const System::Int8 CG_GL_MODELVIEW_MATRIX = System::Int8(0x4);
static const System::Int8 CG_GL_PROJECTION_MATRIX = System::Int8(0x5);
static const System::Int8 CG_GL_TEXTURE_MATRIX = System::Int8(0x6);
static const System::Int8 CG_GL_MODELVIEW_PROJECTION_MATRIX = System::Int8(0x7);
static const System::Int8 CG_GL_VERTEX = System::Int8(0x8);
static const System::Int8 CG_GL_FRAGMENT = System::Int8(0x9);
extern "C" int __cdecl cgGLIsProfileSupported(Cg::Import::TCGprofile profile);
extern "C" void __cdecl cgGLEnableProfile(Cg::Import::TCGprofile profile);
extern "C" void __cdecl cgGLDisableProfile(Cg::Import::TCGprofile profile);
extern "C" Cg::Import::TCGprofile __cdecl cgGLGetLatestProfile(unsigned profile_type);
extern "C" void __cdecl cgGLSetOptimalOptions(Cg::Import::TCGprofile profile);
extern "C" void __cdecl cgGLLoadProgram(Cg::Import::PCGprogram _program);
extern "C" int __cdecl cgGLIsProgramLoaded(Cg::Import::PCGprogram _program);
extern "C" void __cdecl cgGLBindProgram(Cg::Import::PCGprogram _program);
extern "C" void __cdecl cgGLUnbindProgram(Cg::Import::TCGprofile profile);
extern "C" unsigned __cdecl cgGLGetProgramID(Cg::Import::PCGprogram _program);
extern "C" void __cdecl cgGLSetParameter1f(Cg::Import::PCGparameter param, float x);
extern "C" void __cdecl cgGLSetParameter2f(Cg::Import::PCGparameter param, float x, float y);
extern "C" void __cdecl cgGLSetParameter3f(Cg::Import::PCGparameter param, float x, float y, float z);
extern "C" void __cdecl cgGLSetParameter4f(Cg::Import::PCGparameter param, float x, float y, float z, float w);
extern "C" void __cdecl cgGLSetParameter1fv(Cg::Import::PCGparameter param, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameter2fv(Cg::Import::PCGparameter param, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameter3fv(Cg::Import::PCGparameter param, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameter4fv(Cg::Import::PCGparameter param, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameter1d(Cg::Import::PCGparameter param, double x);
extern "C" void __cdecl cgGLSetParameter2d(Cg::Import::PCGparameter param, double x, double y);
extern "C" void __cdecl cgGLSetParameter3d(Cg::Import::PCGparameter param, double x, double y, double z);
extern "C" void __cdecl cgGLSetParameter4d(Cg::Import::PCGparameter param, double x, double y, double z, double w);
extern "C" void __cdecl cgGLSetParameter1dv(Cg::Import::PCGparameter param, const System::PDouble v);
extern "C" void __cdecl cgGLSetParameter2dv(Cg::Import::PCGparameter param, const System::PDouble v);
extern "C" void __cdecl cgGLSetParameter3dv(Cg::Import::PCGparameter param, const System::PDouble v);
extern "C" void __cdecl cgGLSetParameter4dv(Cg::Import::PCGparameter param, const System::PDouble v);
extern "C" void __cdecl cgGLGetParameter1f(Cg::Import::PCGparameter param, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameter2f(Cg::Import::PCGparameter param, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameter3f(Cg::Import::PCGparameter param, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameter4f(Cg::Import::PCGparameter param, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameter1d(Cg::Import::PCGparameter param, System::PDouble v);
extern "C" void __cdecl cgGLGetParameter2d(Cg::Import::PCGparameter param, System::PDouble v);
extern "C" void __cdecl cgGLGetParameter3d(Cg::Import::PCGparameter param, System::PDouble v);
extern "C" void __cdecl cgGLGetParameter4d(Cg::Import::PCGparameter param, System::PDouble v);
extern "C" void __cdecl cgGLSetParameterArray1f(Cg::Import::PCGparameter param, int offset, int nelements, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameterArray2f(Cg::Import::PCGparameter param, int offset, int nelements, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameterArray3f(Cg::Import::PCGparameter param, int offset, int nelements, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameterArray4f(Cg::Import::PCGparameter param, int offset, int nelements, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameterArray1d(Cg::Import::PCGparameter param, int offset, int nelements, const System::PDouble v);
extern "C" void __cdecl cgGLSetParameterArray2d(Cg::Import::PCGparameter param, int offset, int nelements, const System::PDouble v);
extern "C" void __cdecl cgGLSetParameterArray3d(Cg::Import::PCGparameter param, int offset, int nelements, const System::PDouble v);
extern "C" void __cdecl cgGLSetParameterArray4d(Cg::Import::PCGparameter param, int offset, int nelements, const System::PDouble v);
extern "C" void __cdecl cgGLGetParameterArray1f(Cg::Import::PCGparameter param, int offset, int nelements, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameterArray2f(Cg::Import::PCGparameter param, int offset, int nelements, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameterArray3f(Cg::Import::PCGparameter param, int offset, int nelements, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameterArray4f(Cg::Import::PCGparameter param, int offset, int nelements, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameterArray1d(Cg::Import::PCGparameter param, int offset, int nelements, System::PDouble v);
extern "C" void __cdecl cgGLGetParameterArray2d(Cg::Import::PCGparameter param, int offset, int nelements, System::PDouble v);
extern "C" void __cdecl cgGLGetParameterArray3d(Cg::Import::PCGparameter param, int offset, int nelements, System::PDouble v);
extern "C" void __cdecl cgGLGetParameterArray4d(Cg::Import::PCGparameter param, int offset, int nelements, System::PDouble v);
extern "C" void __cdecl cgGLSetParameterPointer(Cg::Import::PCGparameter param, int fsize, unsigned _type, int stride, const void * _pointer);
extern "C" void __cdecl cgGLEnableClientState(Cg::Import::PCGparameter param);
extern "C" void __cdecl cgGLDisableClientState(Cg::Import::PCGparameter param);
extern "C" void __cdecl cgGLSetMatrixParameterdr(Cg::Import::PCGparameter param, const System::PDouble matrix);
extern "C" void __cdecl cgGLSetMatrixParameterfr(Cg::Import::PCGparameter param, const Winapi::Windows::PSingle matrix);
extern "C" void __cdecl cgGLSetMatrixParameterdc(Cg::Import::PCGparameter param, const System::PDouble matrix);
extern "C" void __cdecl cgGLSetMatrixParameterfc(Cg::Import::PCGparameter param, const Winapi::Windows::PSingle matrix);
extern "C" void __cdecl cgGLGetMatrixParameterdr(Cg::Import::PCGparameter param, System::PDouble matrix);
extern "C" void __cdecl cgGLGetMatrixParameterfr(Cg::Import::PCGparameter param, Winapi::Windows::PSingle matrix);
extern "C" void __cdecl cgGLGetMatrixParameterdc(Cg::Import::PCGparameter param, System::PDouble matrix);
extern "C" void __cdecl cgGLGetMatrixParameterfc(Cg::Import::PCGparameter param, Winapi::Windows::PSingle matrix);
extern "C" void __cdecl cgGLSetStateMatrixParameter(Cg::Import::PCGparameter param, unsigned matrix, unsigned transform);
extern "C" void __cdecl cgGLSetMatrixParameterArrayfc(Cg::Import::PCGparameter param, int offset, int nelements, const Winapi::Windows::PSingle matrices);
extern "C" void __cdecl cgGLSetMatrixParameterArrayfr(Cg::Import::PCGparameter param, int offset, int nelements, const Winapi::Windows::PSingle matrices);
extern "C" void __cdecl cgGLSetMatrixParameterArraydc(Cg::Import::PCGparameter param, int offset, int nelements, const System::PDouble matrices);
extern "C" void __cdecl cgGLSetMatrixParameterArraydr(Cg::Import::PCGparameter param, int offset, int nelements, const System::PDouble matrices);
extern "C" void __cdecl cgGLGetMatrixParameterArrayfc(Cg::Import::PCGparameter param, int offset, int nelements, Winapi::Windows::PSingle matrices);
extern "C" void __cdecl cgGLGetMatrixParameterArrayfr(Cg::Import::PCGparameter param, int offset, int nelements, Winapi::Windows::PSingle matrices);
extern "C" void __cdecl cgGLGetMatrixParameterArraydc(Cg::Import::PCGparameter param, int offset, int nelements, System::PDouble matrices);
extern "C" void __cdecl cgGLGetMatrixParameterArraydr(Cg::Import::PCGparameter param, int offset, int nelements, System::PDouble matrices);
extern "C" void __cdecl cgGLSetTextureParameter(Cg::Import::PCGparameter param, unsigned texobj);
extern "C" unsigned __cdecl cgGLGetTextureParameter(Cg::Import::PCGparameter param);
extern "C" void __cdecl cgGLEnableTextureParameter(Cg::Import::PCGparameter param);
extern "C" void __cdecl cgGLDisableTextureParameter(Cg::Import::PCGparameter param);
extern "C" unsigned __cdecl cgGLGetTextureEnum(Cg::Import::PCGparameter param);
extern "C" void __cdecl cgGLSetManageTextureParameters(Cg::Import::PCGcontext ctx, int flag);
extern "C" int __cdecl cgGLGetManageTextureParameters(Cg::Import::PCGcontext ctx);
}	/* namespace Gl */
}	/* namespace Cg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CG_GL)
using namespace Cg::Gl;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CG)
using namespace Cg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cg_GlHPP
