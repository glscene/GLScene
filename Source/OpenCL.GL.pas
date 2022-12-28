//
// The graphics platform GLScene https://github.com/glscene
//
unit OpenCL.GL;

(*
   Conversion of OpenCL header file: cl_gl.h to CL_GL.pas,
   from http://www.khronos.org/registry/cl/.
*)

///*****************************************************************************
// * Copyright (c) 2008-2020 The Khronos Group Inc.
// *
// * Permission is hereby granted, free of charge, to any person obtaining a
// * copy of this software and/or associated documentation files (the
// * "Materials"), to deal in the Materials without restriction, including
// * without limitation the rights to use, copy, modify, merge, publish,
// * distribute, sublicense, and/or sell copies of the Materials, and to
// * permit persons to whom the Materials are furnished to do so, subject to
// * the following conditions:
// *
// * The above copyright notice and this permission notice shall be included
// * in all copies or substantial portions of the Materials.
// *
// * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
// ****************************************************************************/

interface

uses
  OpenCL.Import,
  OpenCL.Platform;

type
  PGLuint = ^Cardinal;

  Tcl_gl_object_type = Tcl_uint;
  Pcl_gl_object_type = ^Tcl_gl_object_type;

  Tcl_gl_texture_info = Tcl_uint;
  Pcl_gl_texture_info = ^Tcl_gl_texture_info;

  Tcl_gl_platform_info = Tcl_uint;
  Pcl_gl_platform_info = ^Tcl_gl_platform_info;

  __GLsync = record end; Pcl_GLsync = ^__GLsync;

  //* cl_gl_object_type = 0x2000 - 0x200F enum values are currently taken  *//

const

CL_GL_OBJECT_BUFFER =                     $2000;
CL_GL_OBJECT_TEXTURE2D =                  $2001;
CL_GL_OBJECT_TEXTURE3D =                  $2002;
CL_GL_OBJECT_RENDERBUFFER =               $2003;
CL_GL_OBJECT_TEXTURE2D_ARRAY =            $200E;
CL_GL_OBJECT_TEXTURE1D =                  $200F;
CL_GL_OBJECT_TEXTURE1D_ARRAY =            $2010;
CL_GL_OBJECT_TEXTURE_BUFFER =             $2011;

//* cl_gl_texture_info *//
CL_GL_TEXTURE_TARGET =                    $2004;
CL_GL_MIPMAP_LEVEL =                      $2005;
CL_GL_NUM_SAMPLES =                       $2012;

function clCreateFromGLBuffer(context: Tcl_context;
           flags: Tcl_mem_flags;
           bufobj: Cardinal;
           errcode_ret: Pcl_int): Tcl_mem; //< CL_API_SUFFIX__VERSION_1_0;
           stdcall; external LibOpenCL;

function clCreateFromGLTexture3D(context: Tcl_context;
           flags: Tcl_mem_flags;
           target: Cardinal;
           miplevel: Integer;
           texture: Cardinal;
           errcode_ret: Pcl_int): Tcl_mem; //< CL_API_SUFFIX__VERSION_1_2;
           stdcall; external LibOpenCL;

function clCreateFromGLRenderbuffer(context: Tcl_context;
           flags: Tcl_mem_flags;
           renderbuffer: Cardinal;
           errcode_ret: Pcl_int): Tcl_mem; //< CL_API_SUFFIX__VERSION_1_0;
           stdcall; external LibOpenCL;

function clGetGLObjectInfo(memobj: Tcl_mem;
           gl_object_type: Pcl_gl_object_type;
           gl_object_name: PGLuint): Tcl_int; //< CL_API_SUFFIX__VERSION_1_0;
           stdcall; external LibOpenCL;


function clGetGLTextureInfo(memobj: Tcl_mem;
           param_name: Tcl_gl_texture_info;
           param_value_size: NativeUInt;
           param_value: Pointer;
           param_value_size_ret: Psize_t): Tcl_int; //< CL_API_SUFFIX__VERSION_1_0;
           stdcall; external LibOpenCL;


function clEnqueueAcquireGLObjects(command_queue: Tcl_command_queue;
           num_objects: Tcl_uint;
           mem_objects: Pcl_mem;
           num_events_in_wait_list: Tcl_uint;
           event_wait_list: Pcl_event;
           event: Pcl_event): Tcl_int;  //< CL_API_SUFFIX__VERSION_1_0;
           stdcall; external LibOpenCL;


function clEnqueueReleaseGLObjects(command_queue: Tcl_command_queue;
           num_objects: Tcl_uint;
           mem_objects: Pcl_mem;
           num_events_in_wait_list: Tcl_uint;
           event_wait_list: Pcl_event;
           event: Pcl_event): Tcl_int; //< CL_API_SUFFIX__VERSION_1_0;
           stdcall; external LibOpenCL;

//* Deprecated OpenCL 1.1 APIs *//
(* function clCreateFromGLTexture2D(); *)
(* function clCreateFromGLTexture3D(); *)
const
//* cl_khr_gl_sharing extension  *//

  cl_khr_gl_sharing =                             1;
type
  Tcl_gl_context_info = Tcl_uint;
  Pcl_gl_context_info = ^Tcl_gl_context_info;

const
//* Additional Error Codes  *//
CL_INVALID_GL_SHAREGROUP_REFERENCE_KHR =       -1000;

//* cl_gl_context_info  *//
CL_CURRENT_DEVICE_FOR_GL_CONTEXT_KHR =          $2006;
CL_DEVICES_FOR_GL_CONTEXT_KHR =                 $2007;

//* Additional cl_context_properties  *//
CL_GL_CONTEXT_KHR =                             $2008;
CL_EGL_DISPLAY_KHR =                            $2009;
CL_GLX_DISPLAY_KHR =                            $200A;
CL_WGL_HDC_KHR =                                $200B;
CL_CGL_SHAREGROUP_KHR =                         $200C;

function clGetGLContextInfoKHR(properties: Tcl_context_properties;
           param_name: Tcl_gl_context_info;
           param_value_size: NativeUInt;
           param_value: Pointer;
           param_value_size_ret: Psize_t): Tcl_int;
           stdcall; external LibOpenCL;

(*
function clGetGLContextInfoKHR_fn(properties: Pcl_context_properties;
         param_name: Tcl_gl_context_info;
         param_value_size: Tsize_t;
         param_value: Pointer;
         param_value_size_ret: Psize_t): Tcl_int;
         stdcall; external LibOpenCL;
*)

//--------------------------------------------------------------------
implementation
//--------------------------------------------------------------------

end.
