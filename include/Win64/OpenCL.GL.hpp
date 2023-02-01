// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'OpenCL.GL.pas' rev: 35.00 (Windows)

#ifndef Opencl_GlHPP
#define Opencl_GlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <OpenCL.Import.hpp>
#include <OpenCL.Platform.hpp>

//-- user supplied -----------------------------------------------------------

namespace Opencl
{
namespace Gl
{
//-- forward type declarations -----------------------------------------------
struct DECLSPEC_DRECORD __GLsync
{
};


//-- type declarations -------------------------------------------------------
typedef unsigned *PGLuint;

typedef unsigned Tcl_gl_object_type;

typedef unsigned *Pcl_gl_object_type;

typedef unsigned Tcl_gl_texture_info;

typedef unsigned *Pcl_gl_texture_info;

typedef unsigned Tcl_gl_platform_info;

typedef unsigned *Pcl_gl_platform_info;

typedef __GLsync *Pcl_GLsync;

typedef unsigned Tcl_gl_context_info;

typedef unsigned *Pcl_gl_context_info;

//-- var, const, procedure ---------------------------------------------------
static const System::Word CL_GL_OBJECT_BUFFER = System::Word(0x2000);
static const System::Word CL_GL_OBJECT_TEXTURE2D = System::Word(0x2001);
static const System::Word CL_GL_OBJECT_TEXTURE3D = System::Word(0x2002);
static const System::Word CL_GL_OBJECT_RENDERBUFFER = System::Word(0x2003);
static const System::Word CL_GL_OBJECT_TEXTURE2D_ARRAY = System::Word(0x200e);
static const System::Word CL_GL_OBJECT_TEXTURE1D = System::Word(0x200f);
static const System::Word CL_GL_OBJECT_TEXTURE1D_ARRAY = System::Word(0x2010);
static const System::Word CL_GL_OBJECT_TEXTURE_BUFFER = System::Word(0x2011);
static const System::Word CL_GL_TEXTURE_TARGET = System::Word(0x2004);
static const System::Word CL_GL_MIPMAP_LEVEL = System::Word(0x2005);
static const System::Word CL_GL_NUM_SAMPLES = System::Word(0x2012);
extern "C" Opencl::Import::Tcl_mem __stdcall clCreateFromGLBuffer(Opencl::Import::Tcl_context context, unsigned __int64 flags, unsigned bufobj, Opencl::Platform::Pcl_int errcode_ret);
extern "C" Opencl::Import::Tcl_mem __stdcall clCreateFromGLTexture3D(Opencl::Import::Tcl_context context, unsigned __int64 flags, unsigned target, int miplevel, unsigned texture, Opencl::Platform::Pcl_int errcode_ret);
extern "C" Opencl::Import::Tcl_mem __stdcall clCreateFromGLRenderbuffer(Opencl::Import::Tcl_context context, unsigned __int64 flags, unsigned renderbuffer, Opencl::Platform::Pcl_int errcode_ret);
extern "C" int __stdcall clGetGLObjectInfo(Opencl::Import::Tcl_mem memobj, Pcl_gl_object_type gl_object_type, PGLuint gl_object_name);
extern "C" int __stdcall clGetGLTextureInfo(Opencl::Import::Tcl_mem memobj, unsigned param_name, NativeUInt param_value_size, void * param_value, Opencl::Platform::Psize_t param_value_size_ret);
extern "C" int __stdcall clEnqueueAcquireGLObjects(Opencl::Import::Tcl_command_queue command_queue, unsigned num_objects, Opencl::Import::Pcl_mem mem_objects, unsigned num_events_in_wait_list, Opencl::Import::Pcl_event event_wait_list, Opencl::Import::Pcl_event event);
extern "C" int __stdcall clEnqueueReleaseGLObjects(Opencl::Import::Tcl_command_queue command_queue, unsigned num_objects, Opencl::Import::Pcl_mem mem_objects, unsigned num_events_in_wait_list, Opencl::Import::Pcl_event event_wait_list, Opencl::Import::Pcl_event event);
static const System::Int8 cl_khr_gl_sharing = System::Int8(0x1);
static const short CL_INVALID_GL_SHAREGROUP_REFERENCE_KHR = short(-1000);
static const System::Word CL_CURRENT_DEVICE_FOR_GL_CONTEXT_KHR = System::Word(0x2006);
static const System::Word CL_DEVICES_FOR_GL_CONTEXT_KHR = System::Word(0x2007);
static const System::Word CL_GL_CONTEXT_KHR = System::Word(0x2008);
static const System::Word CL_EGL_DISPLAY_KHR = System::Word(0x2009);
static const System::Word CL_GLX_DISPLAY_KHR = System::Word(0x200a);
static const System::Word CL_WGL_HDC_KHR = System::Word(0x200b);
static const System::Word CL_CGL_SHAREGROUP_KHR = System::Word(0x200c);
extern "C" int __stdcall clGetGLContextInfoKHR(NativeUInt properties, unsigned param_name, NativeUInt param_value_size, void * param_value, Opencl::Platform::Psize_t param_value_size_ret);
}	/* namespace Gl */
}	/* namespace Opencl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_OPENCL_GL)
using namespace Opencl::Gl;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_OPENCL)
using namespace Opencl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Opencl_GlHPP
