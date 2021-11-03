//
// The graphics rendering engine GLScene http://glscene.org
//
(*
   Conversion of cg NVIDIA header files to cg.pas
*)
(******************************************************************************
 *                                                                            *
 *  Copyright (c) 2002, NVIDIA Corporation.                                   *
 *                                                                            *
 *  Files:    cg.h, cg_datatypes.h, cg_errors.h, cg_profiles.h,               *
 *            cgGL_profiles.h, cg_bindlocations.h                             *
 *  Content:  NVIDIA Cg core include files                                    *
 *                                                                            *
 *  NVIDIA "Cg" Release 1.2 ObjectPascal adaptation by Alexey Barkovoy        *
 *  E-Mail: clootie@ixbt.com                                                  *
 *                                                                            *
 *  Modified: 14-Mar-2004                                                     *
 *                                                                            *
 *  Latest version can be downloaded from:                                    *
 *     http://www.clootie.ru/                                                 *
 *       http://developer.nvidia.com/object/cg_download.html                  *
 *                                                                            *)
(******************************************************************************

 Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)

 The contents of this file are used with permission, subject to the Mozilla
 Public License Version 1.1 (the "License"); you may not use this file except
 in compliance with the License. You may obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1.1.html

 Software distributed under the License is distributed on an "AS IS" basis,
 WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 the specific language governing rights and limitations under the License.

 Alternatively, the contents of this file may be used under the terms of the
 GNU Lesser General Public License (the  "LGPL License"), in which case the
 provisions of the LGPL License are applicable instead of those above.
 If you wish to allow use of your version of this file only under the terms
 of the LGPL License and not to allow others to use your version of this file
 under the MPL, indicate your decision by deleting  the provisions above and
 replace  them with the notice and other provisions required by the LGPL
 License.  If you do not delete the provisions above, a recipient may use
 your version of this file under either the MPL or the LGPL License.

 For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html

******************************************************************************)

(*
 * Copyright (c) 2002, NVIDIA Corporation.
 *
 * NVIDIA Corporation("NVIDIA") supplies this software to you in consideration
 * of your agreement to the following terms, and your use, installation,
 * modification or redistribution of this NVIDIA software constitutes
 * acceptance of these terms.  If you do not agree with these terms, please do
 * not use, install, modify or redistribute this NVIDIA software.
 *
 * In consideration of your agreement to abide by the following terms, and
 * subject to these terms, NVIDIA grants you a personal, non-exclusive license,
 * under NVIDIAs copyrights in this original NVIDIA software (the "NVIDIA
 * Software"), to use, reproduce, modify and redistribute the NVIDIA
 * Software, with or without modifications, in source and/or binary forms;
 * provided that if you redistribute the NVIDIA Software, you must retain the
 * copyright notice of NVIDIA, this notice and the following text and
 * disclaimers in all such redistributions of the NVIDIA Software. Neither the
 * name, trademarks, service marks nor logos of NVIDIA Corporation may be used
 * to endorse or promote products derived from the NVIDIA Software without
 * specific prior written permission from NVIDIA.  Except as expressly stated
 * in this notice, no other rights or licenses express or implied, are granted
 * by NVIDIA herein, including but not limited to any patent rights that may be
 * infringed by your derivative works or by other works in which the NVIDIA
 * Software may be incorporated. No hardware is licensed hereunder.
 *
 * THE NVIDIA SOFTWARE IS BEING PROVIDED ON AN "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING
 * WITHOUT LIMITATION, WARRANTIES OR CONDITIONS OF TITLE, NON-INFRINGEMENT,
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR ITS USE AND OPERATION
 * EITHER ALONE OR IN COMBINATION WITH OTHER PRODUCTS.
 *
 * IN NO EVENT SHALL NVIDIA BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL,
 * EXEMPLARY, CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, LOST
 * PROFITS; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) OR ARISING IN ANY WAY OUT OF THE USE,
 * REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION OF THE NVIDIA SOFTWARE,
 * HOWEVER CAUSED AND WHETHER UNDER THEORY OF CONTRACT, TORT (INCLUDING
 * NEGLIGENCE), STRICT LIABILITY OR OTHERWISE, EVEN IF NVIDIA HAS BEEN ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *)
unit Cg.Import;

interface

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows;
{$ENDIF}

{$I GLScene.inc}

{$MINENUMSIZE 4}

const
  {$IFDEF MSWINDOWS}
  CgLibrary = 'cg.dll';
  {$ELSE}
  CgLibrary = 'libCg.so';
  {$ENDIF}

const
  CG_VERSION_1_2                = 1;
  CG_VERSION_NUM                = 1200;

//
// This #define forces the old API for now.  This will be removed soon, but
// the user will still have the ability to enable it.
//
{.$DEFINE CG_DEPRECATED_1_1_API}

(*************************************************************************)
(*** CG Run-Time Library API                                          ***)
(*************************************************************************)

(*************************************************************************)
(*** Data types and enumerants                                         ***)
(*************************************************************************)

type
  PCharCG = PAnsiChar;
  PPCharCG = ^PCharCG;

  StringCG = AnsiString;

  TCGbool = Integer;
  CGbool = TCGbool;

const
  CG_FALSE = TCGbool(0);
  CG_TRUE  = TCGbool(1);

type
  _CGcontext = record end;
  PCGcontext = ^_CGcontext;
  CGcontext = PCGcontext;

  _CGprogram = record end;
  PCGprogram = ^_CGprogram;
  CGprogram = PCGprogram;
        
  _CGparameter = record end;
  PCGparameter = ^_CGparameter;
  CGparameter = PCGparameter;
        

 (*
  * The following macro invocations define the supported CG basic data types.
  *
  * The macros have the form :
  *
  *   CG_DATATYPE_MACRO(name, compiler_name, nrows, ncols)
  *
  *     name          : The name of the data type.
  *     compiler_name : The name of the data type within the compiler syntax.
  *     enum_name     : The C enumerant.
  *     nrows         : Number of rows for matrix types.  Should be 0 other-wise.
  *     ncols         : Number of columns for matrix types.  Should be 0
  *                     other-wise.
  *
  *)

  TCGtype = (
    CG_UNKNOWN_TYPE,
    CG_STRUCT,
    CG_ARRAY,

    CG_TYPE_START_ENUM = 1024,

    //# define CG_DATATYPE_MACRO(name, compiler_name, enum_name, ncols, nrows) \
    //  enum_name ,

    CG_HALF,      // CG_DATATYPE_MACRO(Half,half,CG_HALF,0,0)
    CG_HALF2,     // CG_DATATYPE_MACRO(Half2,half2,CG_HALF2,0,0)
    CG_HALF3,     // CG_DATATYPE_MACRO(Half3,half3,CG_HALF3,0,0)
    CG_HALF4,     // CG_DATATYPE_MACRO(Half4,half4,CG_HALF4,0,0)
    CG_HALF1x1,   // CG_DATATYPE_MACRO(Half1x1,half1x1,CG_HALF1x1,1,1)
    CG_HALF1x2,   // CG_DATATYPE_MACRO(Half1x2,half1x2,CG_HALF1x2,1,2)
    CG_HALF1x3,   // CG_DATATYPE_MACRO(Half1x3,half1x3,CG_HALF1x3,1,3)
    CG_HALF1x4,   // CG_DATATYPE_MACRO(Half1x4,half1x4,CG_HALF1x4,1,4)
    CG_HALF2x1,   // CG_DATATYPE_MACRO(Half2x1,half2x1,CG_HALF2x1,2,1)
    CG_HALF2x2,   // CG_DATATYPE_MACRO(Half2x2,half2x2,CG_HALF2x2,2,2)
    CG_HALF2x3,   // CG_DATATYPE_MACRO(Half2x3,half2x3,CG_HALF2x3,2,3)
    CG_HALF2x4,   // CG_DATATYPE_MACRO(Half2x4,half2x4,CG_HALF2x4,2,4)
    CG_HALF3x1,   // CG_DATATYPE_MACRO(Half3x1,half3x1,CG_HALF3x1,3,1)
    CG_HALF3x2,   // CG_DATATYPE_MACRO(Half3x2,half3x2,CG_HALF3x2,3,2)
    CG_HALF3x3,   // CG_DATATYPE_MACRO(Half3x3,half3x3,CG_HALF3x3,3,3)
    CG_HALF3x4,   // CG_DATATYPE_MACRO(Half3x4,half3x4,CG_HALF3x4,3,4)
    CG_HALF4x1,   // CG_DATATYPE_MACRO(Half4x1,half4x1,CG_HALF4x1,4,1)
    CG_HALF4x2,   // CG_DATATYPE_MACRO(Half4x2,half4x2,CG_HALF4x2,4,2)
    CG_HALF4x3,   // CG_DATATYPE_MACRO(Half4x3,half4x3,CG_HALF4x3,4,3)
    CG_HALF4x4,   // CG_DATATYPE_MACRO(Half4x4,half4x4,CG_HALF4x4,4,4)
    CG_FLOAT,     // CG_DATATYPE_MACRO(Float,float,CG_FLOAT,0,0)
    CG_FLOAT2,    // CG_DATATYPE_MACRO(Float2,float2,CG_FLOAT2,0,0)
    CG_FLOAT3,    // CG_DATATYPE_MACRO(Float3,float3,CG_FLOAT3,0,0)
    CG_FLOAT4,    // CG_DATATYPE_MACRO(Float4,float4,CG_FLOAT4,0,0)
    CG_FLOAT1x1,  // CG_DATATYPE_MACRO(Float1x1,float1x1,CG_FLOAT1x1,1,1)
    CG_FLOAT1x2,  // CG_DATATYPE_MACRO(Float1x2,float1x2,CG_FLOAT1x2,1,2)
    CG_FLOAT1x3,  // CG_DATATYPE_MACRO(Float1x3,float1x3,CG_FLOAT1x3,1,3)
    CG_FLOAT1x4,  // CG_DATATYPE_MACRO(Float1x4,float1x4,CG_FLOAT1x4,1,4)
    CG_FLOAT2x1,  // CG_DATATYPE_MACRO(Float2x1,float2x1,CG_FLOAT2x1,2,1)
    CG_FLOAT2x2,  // CG_DATATYPE_MACRO(Float2x2,float2x2,CG_FLOAT2x2,2,2)
    CG_FLOAT2x3,  // CG_DATATYPE_MACRO(Float2x3,float2x3,CG_FLOAT2x3,2,3)
    CG_FLOAT2x4,  // CG_DATATYPE_MACRO(Float2x4,float2x4,CG_FLOAT2x4,2,4)
    CG_FLOAT3x1,  // CG_DATATYPE_MACRO(Float3x1,float3x1,CG_FLOAT3x1,3,1)
    CG_FLOAT3x2,  // CG_DATATYPE_MACRO(Float3x2,float3x2,CG_FLOAT3x2,3,2)
    CG_FLOAT3x3,  // CG_DATATYPE_MACRO(Float3x3,float3x3,CG_FLOAT3x3,3,3)
    CG_FLOAT3x4,  // CG_DATATYPE_MACRO(Float3x4,float3x4,CG_FLOAT3x4,3,4)
    CG_FLOAT4x1,  // CG_DATATYPE_MACRO(Float4x1,float4x1,CG_FLOAT4x1,4,1)
    CG_FLOAT4x2,  // CG_DATATYPE_MACRO(Float4x2,float4x2,CG_FLOAT4x2,4,2)
    CG_FLOAT4x3,  // CG_DATATYPE_MACRO(Float4x3,float4x3,CG_FLOAT4x3,4,3)
    CG_FLOAT4x4,  // CG_DATATYPE_MACRO(Float4x4,float4x4,CG_FLOAT4x4,4,4)
    CG_SAMPLER1D, // CG_DATATYPE_MACRO(Sampler1D,sampler1D,CG_SAMPLER1D,0,0)
    CG_SAMPLER2D, // CG_DATATYPE_MACRO(Sampler2D,sampler2D,CG_SAMPLER2D,0,0)
    CG_SAMPLER3D, // CG_DATATYPE_MACRO(Sampler3D,sampler3D,CG_SAMPLER3D,0,0)
    CG_SAMPLERRECT, // CG_DATATYPE_MACRO(SamplerRECT,samplerRECT,CG_SAMPLERRECT,0,0)
    CG_SAMPLERCUBE, // CG_DATATYPE_MACRO(SamplerCUBE,samplerCUBE,CG_SAMPLERCUBE,0,0)
    CG_FIXED,     // CG_DATATYPE_MACRO(Fixed,fixed,CG_FIXED,0,0)
    CG_FIXED2,    // CG_DATATYPE_MACRO(Fixed2,fixed2,CG_FIXED2,0,0)
    CG_FIXED3,    // CG_DATATYPE_MACRO(Fixed3,fixed3,CG_FIXED3,0,0)
    CG_FIXED4,    // CG_DATATYPE_MACRO(Fixed4,fixed4,CG_FIXED4,0,0)
    CG_FIXED1x1,  // CG_DATATYPE_MACRO(Fixed1x1,fixed1x1,CG_FIXED1x1,1,1)
    CG_FIXED1x2,  // CG_DATATYPE_MACRO(Fixed1x2,fixed1x2,CG_FIXED1x2,1,2)
    CG_FIXED1x3,  // CG_DATATYPE_MACRO(Fixed1x3,fixed1x3,CG_FIXED1x3,1,3)
    CG_FIXED1x4,  // CG_DATATYPE_MACRO(Fixed1x4,fixed1x4,CG_FIXED1x4,1,4)
    CG_FIXED2x1,  // CG_DATATYPE_MACRO(Fixed2x1,fixed2x1,CG_FIXED2x1,2,1)
    CG_FIXED2x2,  // CG_DATATYPE_MACRO(Fixed2x2,fixed2x2,CG_FIXED2x2,2,2)
    CG_FIXED2x3,  // CG_DATATYPE_MACRO(Fixed2x3,fixed2x3,CG_FIXED2x3,2,3)
    CG_FIXED2x4,  // CG_DATATYPE_MACRO(Fixed2x4,fixed2x4,CG_FIXED2x4,2,4)
    CG_FIXED3x1,  // CG_DATATYPE_MACRO(Fixed3x1,fixed3x1,CG_FIXED3x1,3,1)
    CG_FIXED3x2,  // CG_DATATYPE_MACRO(Fixed3x2,fixed3x2,CG_FIXED3x2,3,2)
    CG_FIXED3x3,  // CG_DATATYPE_MACRO(Fixed3x3,fixed3x3,CG_FIXED3x3,3,3)
    CG_FIXED3x4,  // CG_DATATYPE_MACRO(Fixed3x4,fixed3x4,CG_FIXED3x4,3,4)
    CG_FIXED4x1,  // CG_DATATYPE_MACRO(Fixed4x1,fixed4x1,CG_FIXED4x1,4,1)
    CG_FIXED4x2,  // CG_DATATYPE_MACRO(Fixed4x2,fixed4x2,CG_FIXED4x2,4,2)
    CG_FIXED4x3,  // CG_DATATYPE_MACRO(Fixed4x3,fixed4x3,CG_FIXED4x3,4,3)
    CG_FIXED4x4,  // CG_DATATYPE_MACRO(Fixed4x4,fixed4x4,CG_FIXED4x4,4,4)
    CG_HALF1,     // CG_DATATYPE_MACRO(Half1,half1,CG_HALF1,0,0)
    CG_FLOAT1,    // CG_DATATYPE_MACRO(Float1,float1,CG_FLOAT1,0,0)
    CG_FIXED1,    // CG_DATATYPE_MACRO(Fixed1,fixed1,CG_FIXED1,0,0)
    CG_INT,       // CG_DATATYPE_MACRO(Int,int,CG_INT,0,1)
    CG_INT1,      // CG_DATATYPE_MACRO(Int1,int1,CG_INT1,0,1)
    CG_INT2,      // CG_DATATYPE_MACRO(Int2,int2,CG_INT2,0,2)
    CG_INT3,      // CG_DATATYPE_MACRO(Int3,int3,CG_INT3,0,3)
    CG_INT4,      // CG_DATATYPE_MACRO(Int4,int4,CG_INT4,0,4)
    CG_INT1x1,    // CG_DATATYPE_MACRO(Int1x1,int1x1,CG_INT1x1,1,1)
    CG_INT1x2,    // CG_DATATYPE_MACRO(Int1x2,int1x2,CG_INT1x2,1,2)
    CG_INT1x3,    // CCG_DATATYPE_MACRO(Int1x3,int1x3,CG_INT1x3,1,3)
    CG_INT1x4,    // CCG_DATATYPE_MACRO(Int1x4,int1x4,CG_INT1x4,1,4)
    CG_INT2x1,    // CCG_DATATYPE_MACRO(Int2x1,int2x1,CG_INT2x1,2,1)
    CG_INT2x2,    // CCG_DATATYPE_MACRO(Int2x2,int2x2,CG_INT2x2,2,2)
    CG_INT2x3,    // CCG_DATATYPE_MACRO(Int2x3,int2x3,CG_INT2x3,2,3)
    CG_INT2x4,    // CCG_DATATYPE_MACRO(Int2x4,int2x4,CG_INT2x4,2,4)
    CG_INT3x1,    // CCG_DATATYPE_MACRO(Int3x1,int3x1,CG_INT3x1,3,1)
    CG_INT3x2,    // CCG_DATATYPE_MACRO(Int3x2,int3x2,CG_INT3x2,3,2)
    CG_INT3x3,    // CCG_DATATYPE_MACRO(Int3x3,int3x3,CG_INT3x3,3,3)
    CG_INT3x4,    // CCG_DATATYPE_MACRO(Int3x4,int3x4,CG_INT3x4,3,4)
    CG_INT4x1,    // CCG_DATATYPE_MACRO(Int4x1,int4x1,CG_INT4x1,4,1)
    CG_INT4x2,    // CCG_DATATYPE_MACRO(Int4x2,int4x2,CG_INT4x2,4,2)
    CG_INT4x3,    // CCG_DATATYPE_MACRO(Int4x3,int4x3,CG_INT4x3,4,3)
    CG_INT4x4,    // CCG_DATATYPE_MACRO(Int4x4,int4x4,CG_INT4x4,4,4)
    CG_BOOL,      // CG_DATATYPE_MACRO(Bool,bool,CG_BOOL,0,1)
    CG_BOOL1,     // CG_DATATYPE_MACRO(Bool1,bool1,CG_BOOL1,0,1)
    CG_BOOL2,     // CG_DATATYPE_MACRO(Bool2,bool2,CG_BOOL2,0,2)
    CG_BOOL3,     // CG_DATATYPE_MACRO(Bool3,bool3,CG_BOOL3,0,3)
    CG_BOOL4,     // CG_DATATYPE_MACRO(Bool4,bool4,CG_BOOL4,0,4)
    CG_BOOL1x1,   // CG_DATATYPE_MACRO(Bool1x1,bool1x1,CG_BOOL1x1,1,1)
    CG_BOOL1x2,   // CG_DATATYPE_MACRO(Bool1x2,bool1x2,CG_BOOL1x2,1,2)
    CG_BOOL1x3,   // CG_DATATYPE_MACRO(Bool1x3,bool1x3,CG_BOOL1x3,1,3)
    CG_BOOL1x4,   // CG_DATATYPE_MACRO(Bool1x4,bool1x4,CG_BOOL1x4,1,4)
    CG_BOOL2x1,   // CG_DATATYPE_MACRO(Bool2x1,bool2x1,CG_BOOL2x1,2,1)
    CG_BOOL2x2,   // CG_DATATYPE_MACRO(Bool2x2,bool2x2,CG_BOOL2x2,2,2)
    CG_BOOL2x3,   // CG_DATATYPE_MACRO(Bool2x3,bool2x3,CG_BOOL2x3,2,3)
    CG_BOOL2x4,   // CG_DATATYPE_MACRO(Bool2x4,bool2x4,CG_BOOL2x4,2,4)
    CG_BOOL3x1,   // CG_DATATYPE_MACRO(Bool3x1,bool3x1,CG_BOOL3x1,3,1)
    CG_BOOL3x2,   // CG_DATATYPE_MACRO(Bool3x2,bool3x2,CG_BOOL3x2,3,2)
    CG_BOOL3x3,   // CG_DATATYPE_MACRO(Bool3x3,bool3x3,CG_BOOL3x3,3,3)
    CG_BOOL3x4,   // CG_DATATYPE_MACRO(Bool3x4,bool3x4,CG_BOOL3x4,3,4)
    CG_BOOL4x1,   // CG_DATATYPE_MACRO(Bool4x1,bool4x1,CG_BOOL4x1,4,1)
    CG_BOOL4x2,   // CG_DATATYPE_MACRO(Bool4x2,bool4x2,CG_BOOL4x2,4,2)
    CG_BOOL4x3,   // CG_DATATYPE_MACRO(Bool4x3,bool4x3,CG_BOOL4x3,4,3)
    CG_BOOL4x4    // CG_DATATYPE_MACRO(Bool4x4,bool4x4,CG_BOOL4x4,4,4)
  );
  CGtype = TCGtype;


 (*
  * The following macro invocations define the supported CG basic hardware
  * bind locations.
  *
  * The macros have the form :
  *
  *   CG_BINDLOCATION_MACRO(name, compiler_name, enum_int)
  *
  *     name          : The name of the location.
  *     enum_name     : The C enumerant.
  *     compiler_name : The name of the location within the compiler syntax.
  *     int_id        : Integer enumerant associated with this bind location.
  *     addressable   : The bind location must have an integer address
  *                     associated with it.
  *     ParamType     : the cgParamType of this register.
  *
  *)
  TCGresource = (
    //# define CG_BINDLOCATION_MACRO(name,enum_name,compiler_name,\
    //                               enum_int,addressable,param_type) \
    //  enum_name = enum_int,

    CG_TEXUNIT0              =  2048, // CG_BINDLOCATION_MACRO(TexUnit0,CG_TEXUNIT0,"texunit 0",2048,0,cgTexObjParam)
    CG_TEXUNIT1              =  2049, // CG_BINDLOCATION_MACRO(TexUnit1,CG_TEXUNIT1,"texunit 1",2049,0,cgTexObjParam)
    CG_TEXUNIT2              =  2050, // CG_BINDLOCATION_MACRO(TexUnit2,CG_TEXUNIT2,"texunit 2",2050,0,cgTexObjParam)
    CG_TEXUNIT3              =  2051, // CG_BINDLOCATION_MACRO(TexUnit3,CG_TEXUNIT3,"texunit 3",2051,0,cgTexObjParam)
    CG_TEXUNIT4              =  2052, // CG_BINDLOCATION_MACRO(TexUnit4,CG_TEXUNIT4,"texunit 4",2052,0,cgTexObjParam)
    CG_TEXUNIT5              =  2053, // CG_BINDLOCATION_MACRO(TexUnit5,CG_TEXUNIT5,"texunit 5",2053,0,cgTexObjParam)
    CG_TEXUNIT6              =  2054, // CG_BINDLOCATION_MACRO(TexUnit6,CG_TEXUNIT6,"texunit 6",2054,0,cgTexObjParam)
    CG_TEXUNIT7              =  2055, // CG_BINDLOCATION_MACRO(TexUnit7,CG_TEXUNIT7,"texunit 7",2055,0,cgTexObjParam)
    CG_TEXUNIT8              =  2056, // CG_BINDLOCATION_MACRO(TexUnit8,CG_TEXUNIT8,"texunit 8",2056,0,cgTexObjParam)
    CG_TEXUNIT9              =  2057, // CG_BINDLOCATION_MACRO(TexUnit9,CG_TEXUNIT9,"texunit 9",2057,0,cgTexObjParam)
    CG_TEXUNIT10             =  2058, // CG_BINDLOCATION_MACRO(TexUnit10,CG_TEXUNIT10,"texunit 10",2058,0,cgTexObjParam)
    CG_TEXUNIT11             =  2059, // CG_BINDLOCATION_MACRO(TexUnit11,CG_TEXUNIT11,"texunit 11",2059,0,cgTexObjParam)
    CG_TEXUNIT12             =  2060, // CG_BINDLOCATION_MACRO(TexUnit12,CG_TEXUNIT12,"texunit 12",2060,0,cgTexObjParam)
    CG_TEXUNIT13             =  2061, // CG_BINDLOCATION_MACRO(TexUnit13,CG_TEXUNIT13,"texunit 13",2061,0,cgTexObjParam)
    CG_TEXUNIT14             =  2062, // CG_BINDLOCATION_MACRO(TexUnit14,CG_TEXUNIT14,"texunit 14",2062,0,cgTexObjParam)
    CG_TEXUNIT15             =  2063, // CG_BINDLOCATION_MACRO(TexUnit15,CG_TEXUNIT15,"texunit 15",2063,0,cgTexObjParam)

    CG_ATTR0                 =  2113, // CG_BINDLOCATION_MACRO(Attr0,CG_ATTR0,"ATTR0",2113,0,cgConnectorMemberParam)
    CG_ATTR1                 =  2114, // CG_BINDLOCATION_MACRO(Attr1,CG_ATTR1,"ATTR1",2114,0,cgConnectorMemberParam)
    CG_ATTR2                 =  2115, // CG_BINDLOCATION_MACRO(Attr2,CG_ATTR2,"ATTR2",2115,0,cgConnectorMemberParam)
    CG_ATTR3                 =  2116, // CG_BINDLOCATION_MACRO(Attr3,CG_ATTR3,"ATTR3",2116,0,cgConnectorMemberParam)
    CG_ATTR4                 =  2117, // CG_BINDLOCATION_MACRO(Attr4,CG_ATTR4,"ATTR4",2117,0,cgConnectorMemberParam)
    CG_ATTR5                 =  2118, // CG_BINDLOCATION_MACRO(Attr5,CG_ATTR5,"ATTR5",2118,0,cgConnectorMemberParam)
    CG_ATTR6                 =  2119, // CG_BINDLOCATION_MACRO(Attr6,CG_ATTR6,"ATTR6",2119,0,cgConnectorMemberParam)
    CG_ATTR7                 =  2120, // CG_BINDLOCATION_MACRO(Attr7,CG_ATTR7,"ATTR7",2120,0,cgConnectorMemberParam)
    CG_ATTR8                 =  2121, // CG_BINDLOCATION_MACRO(Attr8,CG_ATTR8,"ATTR8",2121,0,cgConnectorMemberParam)
    CG_ATTR9                 =  2122, // CG_BINDLOCATION_MACRO(Attr9,CG_ATTR9,"ATTR9",2122,0,cgConnectorMemberParam)
    CG_ATTR10                =  2123, // CG_BINDLOCATION_MACRO(Attr10,CG_ATTR10,"ATTR10",2123,0,cgConnectorMemberParam)
    CG_ATTR11                =  2124, // CG_BINDLOCATION_MACRO(Attr11,CG_ATTR11,"ATTR11",2124,0,cgConnectorMemberParam)
    CG_ATTR12                =  2125, // CG_BINDLOCATION_MACRO(Attr12,CG_ATTR12,"ATTR12",2125,0,cgConnectorMemberParam)
    CG_ATTR13                =  2126, // CG_BINDLOCATION_MACRO(Attr13,CG_ATTR13,"ATTR13",2126,0,cgConnectorMemberParam)
    CG_ATTR14                =  2127, // CG_BINDLOCATION_MACRO(Attr14,CG_ATTR14,"ATTR14",2127,0,cgConnectorMemberParam)
    CG_ATTR15                =  2128, // CG_BINDLOCATION_MACRO(Attr15,CG_ATTR15,"ATTR15",2128,0,cgConnectorMemberParam)

    CG_C                     =  2178, // CG_BINDLOCATION_MACRO(VertUniform,CG_C,"c",2178,1,cgUniformParam)

    CG_TEX0                  =  2179, // CG_BINDLOCATION_MACRO(Tex0,CG_TEX0,"TEX0",2179,0,cgConnectorMemberParam)
    CG_TEX1                  =  2180, // CG_BINDLOCATION_MACRO(Tex1,CG_TEX1,"TEX1",2180,0,cgConnectorMemberParam)
    CG_TEX2                  =  2181, // CG_BINDLOCATION_MACRO(Tex2,CG_TEX2,"TEX2",2181,0,cgConnectorMemberParam)
    CG_TEX3                  =  2192, // CG_BINDLOCATION_MACRO(Tex3,CG_TEX3,"TEX3",2192,0,cgConnectorMemberParam)
    CG_TEX4                  =  2193, // CG_BINDLOCATION_MACRO(Tex4,CG_TEX4,"TEX4",2193,0,cgConnectorMemberParam)
    CG_TEX5                  =  2194, // CG_BINDLOCATION_MACRO(Tex5,CG_TEX5,"TEX5",2194,0,cgConnectorMemberParam)
    CG_TEX6                  =  2195, // CG_BINDLOCATION_MACRO(Tex6,CG_TEX6,"TEX6",2195,0,cgConnectorMemberParam)
    CG_TEX7                  =  2196, // CG_BINDLOCATION_MACRO(Tex7,CG_TEX7,"TEX7",2196,0,cgConnectorMemberParam)

    CG_HPOS                  =  2243, // CG_BINDLOCATION_MACRO(HPos,CG_HPOS,"HPOS",2243,0,cgConnectorMemberParam)
    CG_COL0                  =  2245, // CG_BINDLOCATION_MACRO(Col0,CG_COL0,"COL0",2245,0,cgConnectorMemberParam)
    CG_COL1                  =  2246, // CG_BINDLOCATION_MACRO(Col1,CG_COL1,"COL1",2246,0,cgConnectorMemberParam)
    CG_COL2                  =  2247, // CG_BINDLOCATION_MACRO(Col2,CG_COL2,"COL2",2247,0,cgConnectorMemberParam)
    CG_COL3                  =  2248, // CG_BINDLOCATION_MACRO(Col3,CG_COL3,"COL3",2248,0,cgConnectorMemberParam)
    CG_PSIZ                  =  2309, // CG_BINDLOCATION_MACRO(PSiz,CG_PSIZ,"PSIZ",2309,0,cgConnectorMemberParam)
    CG_WPOS                  =  2373, // CG_BINDLOCATION_MACRO(WPos,CG_WPOS,"WPOS",2373,0,cgConnectorMemberParam)

    CG_POSITION0             =  2437, // CG_BINDLOCATION_MACRO(Position0,CG_POSITION0,"POSITION0",2437,0,cgConnectorMemberParam)
    CG_POSITION1             =  2438, // CG_BINDLOCATION_MACRO(Position1,CG_POSITION1,"POSITION1",2438,0,cgConnectorMemberParam)
    CG_POSITION2             =  2439, // CG_BINDLOCATION_MACRO(Position2,CG_POSITION2,"POSITION2",2439,0,cgConnectorMemberParam)
    CG_POSITION3             =  2440, // CG_BINDLOCATION_MACRO(Position3,CG_POSITION3,"POSITION3",2440,0,cgConnectorMemberParam)
    CG_POSITION4             =  2441, // CG_BINDLOCATION_MACRO(Position4,CG_POSITION4,"POSITION4",2441,0,cgConnectorMemberParam)
    CG_POSITION5             =  2442, // CG_BINDLOCATION_MACRO(Position5,CG_POSITION5,"POSITION5",2442,0,cgConnectorMemberParam)
    CG_POSITION6             =  2443, // CG_BINDLOCATION_MACRO(Position6,CG_POSITION6,"POSITION6",2443,0,cgConnectorMemberParam)
    CG_POSITION7             =  2444, // CG_BINDLOCATION_MACRO(Position7,CG_POSITION7,"POSITION7",2444,0,cgConnectorMemberParam)
    CG_POSITION8             =  2445, // CG_BINDLOCATION_MACRO(Position8,CG_POSITION8,"POSITION8",2445,0,cgConnectorMemberParam)
    CG_POSITION9             =  2446, // CG_BINDLOCATION_MACRO(Position9,CG_POSITION9,"POSITION9",2446,0,cgConnectorMemberParam)
    CG_POSITION10            =  2447, // CG_BINDLOCATION_MACRO(Position10,CG_POSITION10,"POSITION10",2447,0,cgConnectorMemberParam)
    CG_POSITION11            =  2448, // CG_BINDLOCATION_MACRO(Position11,CG_POSITION11,"POSITION11",2448,0,cgConnectorMemberParam)
    CG_POSITION12            =  2449, // CG_BINDLOCATION_MACRO(Position12,CG_POSITION12,"POSITION12",2449,0,cgConnectorMemberParam)
    CG_POSITION13            =  2450, // CG_BINDLOCATION_MACRO(Position13,CG_POSITION13,"POSITION13",2450,0,cgConnectorMemberParam)
    CG_POSITION14            =  2451, // CG_BINDLOCATION_MACRO(Position14,CG_POSITION14,"POSITION14",2451,0,cgConnectorMemberParam)
    CG_POSITION15            =  2452, // CG_BINDLOCATION_MACRO(Position15,CG_POSITION15,"POSITION15",2452,0,cgConnectorMemberParam)
    CG_DIFFUSE0              =  2501, // CG_BINDLOCATION_MACRO(Diffuse0,CG_DIFFUSE0,"DIFFUSE0",2501,0,cgConnectorMemberParam)
    CG_TANGENT0              =  2565, // CG_BINDLOCATION_MACRO(Tangent0,CG_TANGENT0,"TANGENT0",2565,0,cgConnectorMemberParam)
    CG_TANGENT1              =  2566, // CG_BINDLOCATION_MACRO(Tangent1,CG_TANGENT1,"TANGENT1",2566,0,cgConnectorMemberParam)
    CG_TANGENT2              =  2567, // CG_BINDLOCATION_MACRO(Tangent2,CG_TANGENT2,"TANGENT2",2567,0,cgConnectorMemberParam)
    CG_TANGENT3              =  2568, // CG_BINDLOCATION_MACRO(Tangent3,CG_TANGENT3,"TANGENT3",2568,0,cgConnectorMemberParam)
    CG_TANGENT4              =  2569, // CG_BINDLOCATION_MACRO(Tangent4,CG_TANGENT4,"TANGENT4",2569,0,cgConnectorMemberParam)
    CG_TANGENT5              =  2570, // CG_BINDLOCATION_MACRO(Tangent5,CG_TANGENT5,"TANGENT5",2570,0,cgConnectorMemberParam)
    CG_TANGENT6              =  2571, // CG_BINDLOCATION_MACRO(Tangent6,CG_TANGENT6,"TANGENT6",2571,0,cgConnectorMemberParam)
    CG_TANGENT7              =  2572, // CG_BINDLOCATION_MACRO(Tangent7,CG_TANGENT7,"TANGENT7",2572,0,cgConnectorMemberParam)
    CG_TANGENT8              =  2573, // CG_BINDLOCATION_MACRO(Tangent8,CG_TANGENT8,"TANGENT8",2573,0,cgConnectorMemberParam)
    CG_TANGENT9              =  2574, // CG_BINDLOCATION_MACRO(Tangent9,CG_TANGENT9,"TANGENT9",2574,0,cgConnectorMemberParam)
    CG_TANGENT10             =  2575, // CG_BINDLOCATION_MACRO(Tangent10,CG_TANGENT10,"TANGENT10",2575,0,cgConnectorMemberParam)
    CG_TANGENT11             =  2576, // CG_BINDLOCATION_MACRO(Tangent11,CG_TANGENT11,"TANGENT11",2576,0,cgConnectorMemberParam)
    CG_TANGENT12             =  2577, // CG_BINDLOCATION_MACRO(Tangent12,CG_TANGENT12,"TANGENT12",2577,0,cgConnectorMemberParam)
    CG_TANGENT13             =  2578, // CG_BINDLOCATION_MACRO(Tangent13,CG_TANGENT13,"TANGENT13",2578,0,cgConnectorMemberParam)
    CG_TANGENT14             =  2579, // CG_BINDLOCATION_MACRO(Tangent14,CG_TANGENT14,"TANGENT14",2579,0,cgConnectorMemberParam)
    CG_TANGENT15             =  2580, // CG_BINDLOCATION_MACRO(Tangent15,CG_TANGENT15,"TANGENT15",2580,0,cgConnectorMemberParam)
    CG_SPECULAR0             =  2629, // CG_BINDLOCATION_MACRO(Specular0,CG_SPECULAR0,"SPECULAR0",2629,0,cgConnectorMemberParam)
    CG_BLENDINDICES0         =  2693, // CG_BINDLOCATION_MACRO(BlendIndices0,CG_BLENDINDICES0,"BLENDINDICES0",2693,0,cgConnectorMemberParam)
    CG_BLENDINDICES1         =  2694, // CG_BINDLOCATION_MACRO(BlendIndices1,CG_BLENDINDICES1,"BLENDINDICES1",2694,0,cgConnectorMemberParam)
    CG_BLENDINDICES2         =  2695, // CG_BINDLOCATION_MACRO(BlendIndices2,CG_BLENDINDICES2,"BLENDINDICES2",2695,0,cgConnectorMemberParam)
    CG_BLENDINDICES3         =  2696, // CG_BINDLOCATION_MACRO(BlendIndices3,CG_BLENDINDICES3,"BLENDINDICES3",2696,0,cgConnectorMemberParam)
    CG_BLENDINDICES4         =  2697, // CG_BINDLOCATION_MACRO(BlendIndices4,CG_BLENDINDICES4,"BLENDINDICES4",2697,0,cgConnectorMemberParam)
    CG_BLENDINDICES5         =  2698, // CG_BINDLOCATION_MACRO(BlendIndices5,CG_BLENDINDICES5,"BLENDINDICES5",2698,0,cgConnectorMemberParam)
    CG_BLENDINDICES6         =  2699, // CG_BINDLOCATION_MACRO(BlendIndices6,CG_BLENDINDICES6,"BLENDINDICES6",2699,0,cgConnectorMemberParam)
    CG_BLENDINDICES7         =  2700, // CG_BINDLOCATION_MACRO(BlendIndices7,CG_BLENDINDICES7,"BLENDINDICES7",2700,0,cgConnectorMemberParam)
    CG_BLENDINDICES8         =  2701, // CG_BINDLOCATION_MACRO(BlendIndices8,CG_BLENDINDICES8,"BLENDINDICES8",2701,0,cgConnectorMemberParam)
    CG_BLENDINDICES9         =  2702, // CG_BINDLOCATION_MACRO(BlendIndices9,CG_BLENDINDICES9,"BLENDINDICES9",2702,0,cgConnectorMemberParam)
    CG_BLENDINDICES10        =  2703, // CG_BINDLOCATION_MACRO(BlendIndices10,CG_BLENDINDICES10,"BLENDINDICES10",2703,0,cgConnectorMemberParam)
    CG_BLENDINDICES11        =  2704, // CG_BINDLOCATION_MACRO(BlendIndices11,CG_BLENDINDICES11,"BLENDINDICES11",2704,0,cgConnectorMemberParam)
    CG_BLENDINDICES12        =  2705, // CG_BINDLOCATION_MACRO(BlendIndices12,CG_BLENDINDICES12,"BLENDINDICES12",2705,0,cgConnectorMemberParam)
    CG_BLENDINDICES13        =  2706, // CG_BINDLOCATION_MACRO(BlendIndices13,CG_BLENDINDICES13,"BLENDINDICES13",2706,0,cgConnectorMemberParam)
    CG_BLENDINDICES14        =  2707, // CG_BINDLOCATION_MACRO(BlendIndices14,CG_BLENDINDICES14,"BLENDINDICES14",2707,0,cgConnectorMemberParam)
    CG_BLENDINDICES15        =  2708, // CG_BINDLOCATION_MACRO(BlendIndices15,CG_BLENDINDICES15,"BLENDINDICES15",2708,0,cgConnectorMemberParam)
    CG_COLOR0                =  2757, // CG_BINDLOCATION_MACRO(Color0,CG_COLOR0,"COLOR0",2757,0,cgConnectorMemberParam)
    CG_COLOR1                =  2758, // CG_BINDLOCATION_MACRO(Color1,CG_COLOR1,"COLOR1",2758,0,cgConnectorMemberParam)
    CG_COLOR2                =  2759, // CG_BINDLOCATION_MACRO(Color2,CG_COLOR2,"COLOR2",2759,0,cgConnectorMemberParam)
    CG_COLOR3                =  2760, // CG_BINDLOCATION_MACRO(Color3,CG_COLOR3,"COLOR3",2760,0,cgConnectorMemberParam)
    CG_COLOR4                =  2761, // CG_BINDLOCATION_MACRO(Color4,CG_COLOR4,"COLOR4",2761,0,cgConnectorMemberParam)
    CG_COLOR5                =  2762, // CG_BINDLOCATION_MACRO(Color5,CG_COLOR5,"COLOR5",2762,0,cgConnectorMemberParam)
    CG_COLOR6                =  2763, // CG_BINDLOCATION_MACRO(Color6,CG_COLOR6,"COLOR6",2763,0,cgConnectorMemberParam)
    CG_COLOR7                =  2764, // CG_BINDLOCATION_MACRO(Color7,CG_COLOR7,"COLOR7",2764,0,cgConnectorMemberParam)
    CG_COLOR8                =  2765, // CG_BINDLOCATION_MACRO(Color8,CG_COLOR8,"COLOR8",2765,0,cgConnectorMemberParam)
    CG_COLOR9                =  2766, // CG_BINDLOCATION_MACRO(Color9,CG_COLOR9,"COLOR9",2766,0,cgConnectorMemberParam)
    CG_COLOR10               =  2767, // CG_BINDLOCATION_MACRO(Color10,CG_COLOR10,"COLOR10",2767,0,cgConnectorMemberParam)
    CG_COLOR11               =  2768, // CG_BINDLOCATION_MACRO(Color11,CG_COLOR11,"COLOR11",2768,0,cgConnectorMemberParam)
    CG_COLOR12               =  2769, // CG_BINDLOCATION_MACRO(Color12,CG_COLOR12,"COLOR12",2769,0,cgConnectorMemberParam)
    CG_COLOR13               =  2770, // CG_BINDLOCATION_MACRO(Color13,CG_COLOR13,"COLOR13",2770,0,cgConnectorMemberParam)
    CG_COLOR14               =  2771, // CG_BINDLOCATION_MACRO(Color14,CG_COLOR14,"COLOR14",2771,0,cgConnectorMemberParam)
    CG_COLOR15               =  2772, // CG_BINDLOCATION_MACRO(Color15,CG_COLOR15,"COLOR15",2772,0,cgConnectorMemberParam)
    CG_PSIZE0                =  2821, // CG_BINDLOCATION_MACRO(PSize0,CG_PSIZE0,"PSIZE0",2821,0,cgConnectorMemberParam)
    CG_PSIZE1                =  2822, // CG_BINDLOCATION_MACRO(PSize1,CG_PSIZE1,"PSIZE1",2822,0,cgConnectorMemberParam)
    CG_PSIZE2                =  2823, // CG_BINDLOCATION_MACRO(PSize2,CG_PSIZE2,"PSIZE2",2823,0,cgConnectorMemberParam)
    CG_PSIZE3                =  2824, // CG_BINDLOCATION_MACRO(PSize3,CG_PSIZE3,"PSIZE3",2824,0,cgConnectorMemberParam)
    CG_PSIZE4                =  2825, // CG_BINDLOCATION_MACRO(PSize4,CG_PSIZE4,"PSIZE4",2825,0,cgConnectorMemberParam)
    CG_PSIZE5                =  2826, // CG_BINDLOCATION_MACRO(PSize5,CG_PSIZE5,"PSIZE5",2826,0,cgConnectorMemberParam)
    CG_PSIZE6                =  2827, // CG_BINDLOCATION_MACRO(PSize6,CG_PSIZE6,"PSIZE6",2827,0,cgConnectorMemberParam)
    CG_PSIZE7                =  2828, // CG_BINDLOCATION_MACRO(PSize7,CG_PSIZE7,"PSIZE7",2828,0,cgConnectorMemberParam)
    CG_PSIZE8                =  2829, // CG_BINDLOCATION_MACRO(PSize8,CG_PSIZE8,"PSIZE8",2829,0,cgConnectorMemberParam)
    CG_PSIZE9                =  2830, // CG_BINDLOCATION_MACRO(PSize9,CG_PSIZE9,"PSIZE9",2830,0,cgConnectorMemberParam)
    CG_PSIZE10               =  2831, // CG_BINDLOCATION_MACRO(PSize10,CG_PSIZE10,"PSIZE10",2831,0,cgConnectorMemberParam)
    CG_PSIZE11               =  2832, // CG_BINDLOCATION_MACRO(PSize11,CG_PSIZE11,"PSIZE11",2832,0,cgConnectorMemberParam)
    CG_PSIZE12               =  2833, // CG_BINDLOCATION_MACRO(PSize12,CG_PSIZE12,"PSIZE12",2833,0,cgConnectorMemberParam)
    CG_PSIZE13               =  2834, // CG_BINDLOCATION_MACRO(PSize13,CG_PSIZE13,"PSIZE13",2834,0,cgConnectorMemberParam)
    CG_PSIZE14               =  2835, // CG_BINDLOCATION_MACRO(PSize14,CG_PSIZE14,"PSIZE14",2835,0,cgConnectorMemberParam)
    CG_PSIZE15               =  2836, // CG_BINDLOCATION_MACRO(PSize15,CG_PSIZE15,"PSIZE15",2836,0,cgConnectorMemberParam)
    CG_BINORMAL0             =  2885, // CG_BINDLOCATION_MACRO(BiNormal0,CG_BINORMAL0,"BINORMAL0",2885,0,cgConnectorMemberParam)
    CG_BINORMAL1             =  2886, // CG_BINDLOCATION_MACRO(BiNormal1,CG_BINORMAL1,"BINORMAL1",2886,0,cgConnectorMemberParam)
    CG_BINORMAL2             =  2887, // CG_BINDLOCATION_MACRO(BiNormal2,CG_BINORMAL2,"BINORMAL2",2887,0,cgConnectorMemberParam)
    CG_BINORMAL3             =  2888, // CG_BINDLOCATION_MACRO(BiNormal3,CG_BINORMAL3,"BINORMAL3",2888,0,cgConnectorMemberParam)
    CG_BINORMAL4             =  2889, // CG_BINDLOCATION_MACRO(BiNormal4,CG_BINORMAL4,"BINORMAL4",2889,0,cgConnectorMemberParam)
    CG_BINORMAL5             =  2890, // CG_BINDLOCATION_MACRO(BiNormal5,CG_BINORMAL5,"BINORMAL5",2890,0,cgConnectorMemberParam)
    CG_BINORMAL6             =  2891, // CG_BINDLOCATION_MACRO(BiNormal6,CG_BINORMAL6,"BINORMAL6",2891,0,cgConnectorMemberParam)
    CG_BINORMAL7             =  2892, // CG_BINDLOCATION_MACRO(BiNormal7,CG_BINORMAL7,"BINORMAL7",2892,0,cgConnectorMemberParam)
    CG_BINORMAL8             =  2893, // CG_BINDLOCATION_MACRO(BiNormal8,CG_BINORMAL8,"BINORMAL8",2893,0,cgConnectorMemberParam)
    CG_BINORMAL9             =  2894, // CG_BINDLOCATION_MACRO(BiNormal9,CG_BINORMAL9,"BINORMAL9",2894,0,cgConnectorMemberParam)
    CG_BINORMAL10            =  2895, // CG_BINDLOCATION_MACRO(BiNormal10,CG_BINORMAL10,"BINORMAL10",2895,0,cgConnectorMemberParam)
    CG_BINORMAL11            =  2896, // CG_BINDLOCATION_MACRO(BiNormal11,CG_BINORMAL11,"BINORMAL11",2896,0,cgConnectorMemberParam)
    CG_BINORMAL12            =  2897, // CG_BINDLOCATION_MACRO(BiNormal12,CG_BINORMAL12,"BINORMAL12",2897,0,cgConnectorMemberParam)
    CG_BINORMAL13            =  2898, // CG_BINDLOCATION_MACRO(BiNormal13,CG_BINORMAL13,"BINORMAL13",2898,0,cgConnectorMemberParam)
    CG_BINORMAL14            =  2899, // CG_BINDLOCATION_MACRO(BiNormal14,CG_BINORMAL14,"BINORMAL14",2899,0,cgConnectorMemberParam)
    CG_BINORMAL15            =  2900, // CG_BINDLOCATION_MACRO(BiNormal15,CG_BINORMAL15,"BINORMAL15",2900,0,cgConnectorMemberParam)
    CG_FOG0                  =  2917, // CG_BINDLOCATION_MACRO(FOG0,CG_FOG0,"FOG0",2917,0,cgConnectorMemberParam)
    CG_FOG1                  =  2918, // CG_BINDLOCATION_MACRO(FOG1,CG_FOG1,"FOG1",2918,0,cgConnectorMemberParam)
    CG_FOG2                  =  2919, // CG_BINDLOCATION_MACRO(FOG2,CG_FOG2,"FOG2",2919,0,cgConnectorMemberParam)
    CG_FOG3                  =  2920, // CG_BINDLOCATION_MACRO(FOG3,CG_FOG3,"FOG3",2920,0,cgConnectorMemberParam)
    CG_FOG4                  =  2921, // CG_BINDLOCATION_MACRO(FOG4,CG_FOG4,"FOG4",2921,0,cgConnectorMemberParam)
    CG_FOG5                  =  2922, // CG_BINDLOCATION_MACRO(FOG5,CG_FOG5,"FOG5",2922,0,cgConnectorMemberParam)
    CG_FOG6                  =  2923, // CG_BINDLOCATION_MACRO(FOG6,CG_FOG6,"FOG6",2923,0,cgConnectorMemberParam)
    CG_FOG7                  =  2924, // CG_BINDLOCATION_MACRO(FOG7,CG_FOG7,"FOG7",2924,0,cgConnectorMemberParam)
    CG_FOG8                  =  2925, // CG_BINDLOCATION_MACRO(FOG8,CG_FOG8,"FOG8",2925,0,cgConnectorMemberParam)
    CG_FOG9                  =  2926, // CG_BINDLOCATION_MACRO(FOG9,CG_FOG9,"FOG9",2926,0,cgConnectorMemberParam)
    CG_FOG10                 =  2927, // CG_BINDLOCATION_MACRO(FOG10,CG_FOG10,"FOG10",2927,0,cgConnectorMemberParam)
    CG_FOG11                 =  2928, // CG_BINDLOCATION_MACRO(FOG11,CG_FOG11,"FOG11",2928,0,cgConnectorMemberParam)
    CG_FOG12                 =  2929, // CG_BINDLOCATION_MACRO(FOG12,CG_FOG12,"FOG12",2929,0,cgConnectorMemberParam)
    CG_FOG13                 =  2930, // CG_BINDLOCATION_MACRO(FOG13,CG_FOG13,"FOG13",2930,0,cgConnectorMemberParam)
    CG_FOG14                 =  2931, // CG_BINDLOCATION_MACRO(FOG14,CG_FOG14,"FOG14",2931,0,cgConnectorMemberParam)
    CG_FOG15                 =  2932, // CG_BINDLOCATION_MACRO(FOG15,CG_FOG15,"FOG15",2932,0,cgConnectorMemberParam)
    CG_DEPTH0                =  2933, // CG_BINDLOCATION_MACRO(DEPTH0,CG_DEPTH0,"DEPTH0",2933,0,cgConnectorMemberParam)
    CG_DEPTH1                =  2934, // CG_BINDLOCATION_MACRO(DEPTH1,CG_DEPTH1,"DEPTH1",2934,0,cgConnectorMemberParam)
    CG_DEPTH2                =  2935, // CG_BINDLOCATION_MACRO(DEPTH2,CG_DEPTH2,"DEPTH2",2935,0,cgConnectorMemberParam)
    CG_DEPTH3                =  2936, // CG_BINDLOCATION_MACRO(DEPTH3,CG_DEPTH3,"DEPTH3",2936,0,cgConnectorMemberParam)
    CG_DEPTH4                =  2937, // CG_BINDLOCATION_MACRO(DEPTH4,CG_DEPTH4,"DEPTH4",2937,0,cgConnectorMemberParam)
    CG_DEPTH5                =  2938, // CG_BINDLOCATION_MACRO(DEPTH5,CG_DEPTH5,"DEPTH5",2938,0,cgConnectorMemberParam)
    CG_DEPTH6                =  2939, // CG_BINDLOCATION_MACRO(DEPTH6,CG_DEPTH6,"DEPTH6",2939,0,cgConnectorMemberParam)
    CG_DEPTH7                =  2940, // CG_BINDLOCATION_MACRO(DEPTH7,CG_DEPTH7,"DEPTH7",2940,0,cgConnectorMemberParam)
    CG_DEPTH8                =  2941, // CG_BINDLOCATION_MACRO(DEPTH8,CG_DEPTH8,"DEPTH8",2941,0,cgConnectorMemberParam)
//TODO: what THE @#$% !!! ('29542')
    CG_DEPTH9                = 29542, // CG_BINDLOCATION_MACRO(DEPTH9,CG_DEPTH9,"DEPTH9",29542,0,cgConnectorMemberParam)
    CG_DEPTH10               =  2943, // CG_BINDLOCATION_MACRO(DEPTH10,CG_DEPTH10,"DEPTH10",2943,0,cgConnectorMemberParam)
    CG_DEPTH11               =  2944, // CG_BINDLOCATION_MACRO(DEPTH11,CG_DEPTH11,"DEPTH11",2944,0,cgConnectorMemberParam)
    CG_DEPTH12               =  2945, // CG_BINDLOCATION_MACRO(DEPTH12,CG_DEPTH12,"DEPTH12",2945,0,cgConnectorMemberParam)
    CG_DEPTH13               =  2946, // CG_BINDLOCATION_MACRO(DEPTH13,CG_DEPTH13,"DEPTH13",2946,0,cgConnectorMemberParam)
    CG_DEPTH14               =  2947, // CG_BINDLOCATION_MACRO(DEPTH14,CG_DEPTH14,"DEPTH14",2947,0,cgConnectorMemberParam)
    CG_DEPTH15               =  2948, // CG_BINDLOCATION_MACRO(DEPTH15,CG_DEPTH15,"DEPTH15",2948,0,cgConnectorMemberParam)
    CG_SAMPLE0               =  2949, // CG_BINDLOCATION_MACRO(SAMPLE0,CG_SAMPLE0,"SAMPLE0",2949,0,cgConnectorMemberParam)
    CG_SAMPLE1               =  2950, // CG_BINDLOCATION_MACRO(SAMPLE1,CG_SAMPLE1,"SAMPLE1",2950,0,cgConnectorMemberParam)
    CG_SAMPLE2               =  2951, // CG_BINDLOCATION_MACRO(SAMPLE2,CG_SAMPLE2,"SAMPLE2",2951,0,cgConnectorMemberParam)
    CG_SAMPLE3               =  2952, // CG_BINDLOCATION_MACRO(SAMPLE3,CG_SAMPLE3,"SAMPLE3",2952,0,cgConnectorMemberParam)
    CG_SAMPLE4               =  2953, // CG_BINDLOCATION_MACRO(SAMPLE4,CG_SAMPLE4,"SAMPLE4",2953,0,cgConnectorMemberParam)
    CG_SAMPLE5               =  2954, // CG_BINDLOCATION_MACRO(SAMPLE5,CG_SAMPLE5,"SAMPLE5",2954,0,cgConnectorMemberParam)
    CG_SAMPLE6               =  2955, // CG_BINDLOCATION_MACRO(SAMPLE6,CG_SAMPLE6,"SAMPLE6",2955,0,cgConnectorMemberParam)
    CG_SAMPLE7               =  2956, // CG_BINDLOCATION_MACRO(SAMPLE7,CG_SAMPLE7,"SAMPLE7",2956,0,cgConnectorMemberParam)
    CG_SAMPLE8               =  2957, // CG_BINDLOCATION_MACRO(SAMPLE8,CG_SAMPLE8,"SAMPLE8",2957,0,cgConnectorMemberParam)
    CG_SAMPLE9               =  2958, // CG_BINDLOCATION_MACRO(SAMPLE9,CG_SAMPLE9,"SAMPLE9",2958,0,cgConnectorMemberParam)
    CG_SAMPLE10              =  2959, // CG_BINDLOCATION_MACRO(SAMPLE10,CG_SAMPLE10,"SAMPLE10",2959,0,cgConnectorMemberParam)
    CG_SAMPLE11              =  2960, // CG_BINDLOCATION_MACRO(SAMPLE11,CG_SAMPLE11,"SAMPLE11",2960,0,cgConnectorMemberParam)
    CG_SAMPLE12              =  2961, // CG_BINDLOCATION_MACRO(SAMPLE12,CG_SAMPLE12,"SAMPLE12",2961,0,cgConnectorMemberParam)
    CG_SAMPLE13              =  2962, // CG_BINDLOCATION_MACRO(SAMPLE13,CG_SAMPLE13,"SAMPLE13",2962,0,cgConnectorMemberParam)
    CG_SAMPLE14              =  2963, // CG_BINDLOCATION_MACRO(SAMPLE14,CG_SAMPLE14,"SAMPLE14",2963,0,cgConnectorMemberParam)
    CG_SAMPLE15              =  2964, // CG_BINDLOCATION_MACRO(SAMPLE15,CG_SAMPLE15,"SAMPLE15",2964,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT0          =  3028, // CG_BINDLOCATION_MACRO(BlendWeight0,CG_BLENDWEIGHT0,"BLENDWEIGHT0",3028,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT1          =  3029, // CG_BINDLOCATION_MACRO(BlendWeight1,CG_BLENDWEIGHT1,"BLENDWEIGHT1",3029,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT2          =  3030, // CG_BINDLOCATION_MACRO(BlendWeight2,CG_BLENDWEIGHT2,"BLENDWEIGHT2",3030,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT3          =  3031, // CG_BINDLOCATION_MACRO(BlendWeight3,CG_BLENDWEIGHT3,"BLENDWEIGHT3",3031,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT4          =  3032, // CG_BINDLOCATION_MACRO(BlendWeight4,CG_BLENDWEIGHT4,"BLENDWEIGHT4",3032,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT5          =  3033, // CG_BINDLOCATION_MACRO(BlendWeight5,CG_BLENDWEIGHT5,"BLENDWEIGHT5",3033,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT6          =  3034, // CG_BINDLOCATION_MACRO(BlendWeight6,CG_BLENDWEIGHT6,"BLENDWEIGHT6",3034,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT7          =  3035, // CG_BINDLOCATION_MACRO(BlendWeight7,CG_BLENDWEIGHT7,"BLENDWEIGHT7",3035,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT8          =  3036, // CG_BINDLOCATION_MACRO(BlendWeight8,CG_BLENDWEIGHT8,"BLENDWEIGHT8",3036,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT9          =  3037, // CG_BINDLOCATION_MACRO(BlendWeight9,CG_BLENDWEIGHT9,"BLENDWEIGHT9",3037,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT10         =  3038, // CG_BINDLOCATION_MACRO(BlendWeight10,CG_BLENDWEIGHT10,"BLENDWEIGHT10",3038,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT11         =  3039, // CG_BINDLOCATION_MACRO(BlendWeight11,CG_BLENDWEIGHT11,"BLENDWEIGHT11",3039,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT12         =  3040, // CG_BINDLOCATION_MACRO(BlendWeight12,CG_BLENDWEIGHT12,"BLENDWEIGHT12",3040,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT13         =  3041, // CG_BINDLOCATION_MACRO(BlendWeight13,CG_BLENDWEIGHT13,"BLENDWEIGHT13",3041,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT14         =  3042, // CG_BINDLOCATION_MACRO(BlendWeight14,CG_BLENDWEIGHT14,"BLENDWEIGHT14",3042,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT15         =  3043, // CG_BINDLOCATION_MACRO(BlendWeight15,CG_BLENDWEIGHT15,"BLENDWEIGHT15",3043,0,cgConnectorMemberParam)
    CG_NORMAL0               =  3092, // CG_BINDLOCATION_MACRO(Normal0,CG_NORMAL0,"NORMAL0",3092,0,cgConnectorMemberParam)
    CG_NORMAL1               =  3093, // CG_BINDLOCATION_MACRO(Normal1,CG_NORMAL1,"NORMAL1",3093,0,cgConnectorMemberParam)
    CG_NORMAL2               =  3094, // CG_BINDLOCATION_MACRO(Normal2,CG_NORMAL2,"NORMAL2",3094,0,cgConnectorMemberParam)
    CG_NORMAL3               =  3095, // CG_BINDLOCATION_MACRO(Normal3,CG_NORMAL3,"NORMAL3",3095,0,cgConnectorMemberParam)
    CG_NORMAL4               =  3096, // CG_BINDLOCATION_MACRO(Normal4,CG_NORMAL4,"NORMAL4",3096,0,cgConnectorMemberParam)
    CG_NORMAL5               =  3097, // CG_BINDLOCATION_MACRO(Normal5,CG_NORMAL5,"NORMAL5",3097,0,cgConnectorMemberParam)
    CG_NORMAL6               =  3098, // CG_BINDLOCATION_MACRO(Normal6,CG_NORMAL6,"NORMAL6",3098,0,cgConnectorMemberParam)
    CG_NORMAL7               =  3099, // CG_BINDLOCATION_MACRO(Normal7,CG_NORMAL7,"NORMAL7",3099,0,cgConnectorMemberParam)
    CG_NORMAL8               =  3100, // CG_BINDLOCATION_MACRO(Normal8,CG_NORMAL8,"NORMAL8",3100,0,cgConnectorMemberParam)
    CG_NORMAL9               =  3101, // CG_BINDLOCATION_MACRO(Normal9,CG_NORMAL9,"NORMAL9",3101,0,cgConnectorMemberParam)
    CG_NORMAL10              =  3102, // CG_BINDLOCATION_MACRO(Normal10,CG_NORMAL10,"NORMAL10",3102,0,cgConnectorMemberParam)
    CG_NORMAL11              =  3103, // CG_BINDLOCATION_MACRO(Normal11,CG_NORMAL11,"NORMAL11",3103,0,cgConnectorMemberParam)
    CG_NORMAL12              =  3104, // CG_BINDLOCATION_MACRO(Normal12,CG_NORMAL12,"NORMAL12",3104,0,cgConnectorMemberParam)
    CG_NORMAL13              =  3105, // CG_BINDLOCATION_MACRO(Normal13,CG_NORMAL13,"NORMAL13",3105,0,cgConnectorMemberParam)
    CG_NORMAL14              =  3106, // CG_BINDLOCATION_MACRO(Normal14,CG_NORMAL14,"NORMAL14",3106,0,cgConnectorMemberParam)
    CG_NORMAL15              =  3107, // CG_BINDLOCATION_MACRO(Normal15,CG_NORMAL15,"NORMAL15",3107,0,cgConnectorMemberParam)
    CG_FOGCOORD              =  3156, // CG_BINDLOCATION_MACRO(FogCoord,CG_FOGCOORD,"FOGCOORD",3156,0,cgConnectorMemberParam)
    CG_TEXCOORD0             =  3220, // CG_BINDLOCATION_MACRO(TexCoord0,CG_TEXCOORD0,"TEXCOORD0",3220,0,cgConnectorMemberParam)
    CG_TEXCOORD1             =  3221, // CG_BINDLOCATION_MACRO(TexCoord1,CG_TEXCOORD1,"TEXCOORD1",3221,0,cgConnectorMemberParam)
    CG_TEXCOORD2             =  3222, // CG_BINDLOCATION_MACRO(TexCoord2,CG_TEXCOORD2,"TEXCOORD2",3222,0,cgConnectorMemberParam)
    CG_TEXCOORD3             =  3223, // CG_BINDLOCATION_MACRO(TexCoord3,CG_TEXCOORD3,"TEXCOORD3",3223,0,cgConnectorMemberParam)
    CG_TEXCOORD4             =  3224, // CG_BINDLOCATION_MACRO(TexCoord4,CG_TEXCOORD4,"TEXCOORD4",3224,0,cgConnectorMemberParam)
    CG_TEXCOORD5             =  3225, // CG_BINDLOCATION_MACRO(TexCoord5,CG_TEXCOORD5,"TEXCOORD5",3225,0,cgConnectorMemberParam)
    CG_TEXCOORD6             =  3226, // CG_BINDLOCATION_MACRO(TexCoord6,CG_TEXCOORD6,"TEXCOORD6",3226,0,cgConnectorMemberParam)
    CG_TEXCOORD7             =  3227, // CG_BINDLOCATION_MACRO(TexCoord7,CG_TEXCOORD7,"TEXCOORD7",3227,0,cgConnectorMemberParam)
    CG_TEXCOORD8             =  3228, // CG_BINDLOCATION_MACRO(TexCoord8,CG_TEXCOORD8,"TEXCOORD8",3228,0,cgConnectorMemberParam)
    CG_TEXCOORD9             =  3229, // CG_BINDLOCATION_MACRO(TexCoord9,CG_TEXCOORD9,"TEXCOORD9",3229,0,cgConnectorMemberParam)
    CG_TEXCOORD10            =  3230, // CG_BINDLOCATION_MACRO(TexCoord10,CG_TEXCOORD10,"TEXCOORD10",3230,0,cgConnectorMemberParam)
    CG_TEXCOORD11            =  3231, // CG_BINDLOCATION_MACRO(TexCoord11,CG_TEXCOORD11,"TEXCOORD11",3231,0,cgConnectorMemberParam)
    CG_TEXCOORD12            =  3232, // CG_BINDLOCATION_MACRO(TexCoord12,CG_TEXCOORD12,"TEXCOORD12",3232,0,cgConnectorMemberParam)
    CG_TEXCOORD13            =  3233, // CG_BINDLOCATION_MACRO(TexCoord13,CG_TEXCOORD13,"TEXCOORD13",3233,0,cgConnectorMemberParam)
    CG_TEXCOORD14            =  3234, // CG_BINDLOCATION_MACRO(TexCoord14,CG_TEXCOORD14,"TEXCOORD14",3234,0,cgConnectorMemberParam)
    CG_TEXCOORD15            =  3235, // CG_BINDLOCATION_MACRO(TexCoord15,CG_TEXCOORD15,"TEXCOORD15",3235,0,cgConnectorMemberParam)
    CG_COMBINER_CONST0       =  3284, // CG_BINDLOCATION_MACRO(CombinerConst0,CG_COMBINER_CONST0,"COMBINER_CONST0",3284,0,cgUniformParam)
    CG_COMBINER_CONST1       =  3285, // CG_BINDLOCATION_MACRO(CombinerConst1,CG_COMBINER_CONST1,"COMBINER_CONST1",3285,0,cgUniformParam)
    CG_COMBINER_STAGE_CONST0 =  3286, // CG_BINDLOCATION_MACRO(CombinerStageConst0,CG_COMBINER_STAGE_CONST0,"COMBINER_STAGE_CONST0",3286,1,cgUniformParam)
    CG_COMBINER_STAGE_CONST1 =  3287, // CG_BINDLOCATION_MACRO(CombinerStageConst1,CG_COMBINER_STAGE_CONST1,"COMBINER_STAGE_CONST1",3287,1,cgUniformParam)
    CG_OFFSET_TEXTURE_MATRIX =  3288, // CG_BINDLOCATION_MACRO(OffsetTextureMatrix,CG_OFFSET_TEXTURE_MATRIX,"OFFSET_TEXTURE_MATRIX",3288,0,cgUniformParam)
    CG_OFFSET_TEXTURE_SCALE  =  3289, // CG_BINDLOCATION_MACRO(OffsetTextureScale,CG_OFFSET_TEXTURE_SCALE,"OFFSET_TEXTURE_SCALE",3289,0,cgUniformParam)
    CG_OFFSET_TEXTURE_BIAS   =  3290, // CG_BINDLOCATION_MACRO(OffsetTextureBias,CG_OFFSET_TEXTURE_BIAS,"OFFSET_TEXTURE_BIAS",3290,0,cgUniformParam)
    CG_CONST_EYE             =  3291, // CG_BINDLOCATION_MACRO(ConstEye,CG_CONST_EYE,"CONST_EYE",3291,0,cgUniformParam)
    CG_TESSFACTOR            =  3255, // CG_BINDLOCATION_MACRO(TessFactor,CG_TESSFACTOR,"TESSFACTOR",3255,0,cgConnectorMemberParam)

    CG_UNDEFINED
  );
  CGresource = TCGresource;

 (*
  * The following macro invocations define the supported CG profiles.
  *
  * The macros have the form :
  *
  *   CG_PROFILE_MACRO(name, compiler_id, compiler_opt)
  *
  *     name         : The name of the profile.  Used consistently with the API.
  *     compiler_id  : The identifier string for the profile used by the compiler.
  *     compiler_id_caps : compiler_id in caps.
  *     compiler_opt : The command-line switch used to force compilation into
  *                    the profile.
  *     int_id           : Integer enumerant associated with this bind location.
  *     vertex_profile   : Non-zero if this is a vertex profile, otherwise it
  *                        is considered to be a fragment profile.
  *
  *
  *)
  TCGprofile = (
    CG_PROFILE_START = 6144,
    CG_PROFILE_UNKNOWN,

    //# define CG_PROFILE_MACRO(name, compiler_id, compiler_id_caps, compiler_opt,int_id,vertex_profile) \
    //   CG_PROFILE_##compiler_id_caps = int_id,

    CG_PROFILE_VP20   = 6146, // CG_PROFILE_MACRO(Vertex,vp20,VP20,"vp20",6146,1)
    CG_PROFILE_FP20   = 6147, // CG_PROFILE_MACRO(Fragment20,fp20,FP20,"fp20",6147,0)
    CG_PROFILE_VP30   = 6148, // CG_PROFILE_MACRO(Vertex30,vp30,VP30,"vp30",6148,1)
    CG_PROFILE_FP30   = 6149, // CG_PROFILE_MACRO(Fragment,fp30,FP30,"fp30",6149,0)
    CG_PROFILE_ARBVP1 = 6150, // CG_PROFILE_MACRO(ARBVertex,arbvp1,ARBVP1,"arbvp1",6150,1)
    CG_PROFILE_ARBFP1 = 7000, // CG_PROFILE_MACRO(ARBFragment,arbfp1,ARBFP1,"arbfp1",7000,0)
    CG_PROFILE_VP40   = 7001, // CG_PROFILE_MACRO(Vertex40,vp40,VP40,"vp40",7001,1)
    CG_PROFILE_FP40   = 6151, // CG_PROFILE_MACRO(Fragment40,fp40,FP40,"fp40",6151,0)

    CG_PROFILE_VS_1_1 = 6153, // CG_PROFILE_MACRO(DX9Vertex11,vs_1_1,VS_1_1,"vs_1_1",6153,1)
    CG_PROFILE_VS_2_0 = 6154, // CG_PROFILE_MACRO(DX9Vertex20,vs_2_0,VS_2_0,"vs_2_0",6154,1)
    CG_PROFILE_VS_2_X = 6155, // CG_PROFILE_MACRO(DX9Vertex2x,vs_2_x,VS_2_X,"vs_2_x",6155,1)

    CG_PROFILE_PS_1_1 = 6159, // CG_PROFILE_MACRO(DX9Pixel11,ps_1_1,PS_1_1,"ps_1_1",6159,0)
    CG_PROFILE_PS_1_2 = 6160, // CG_PROFILE_MACRO(DX9Pixel12,ps_1_2,PS_1_2,"ps_1_2",6160,0)
    CG_PROFILE_PS_1_3 = 6161, // CG_PROFILE_MACRO(DX9Pixel13,ps_1_3,PS_1_3,"ps_1_3",6161,0)
    CG_PROFILE_PS_2_0 = 6162, // CG_PROFILE_MACRO(DX9Pixel20,ps_2_0,PS_2_0,"ps_2_0",6162,0)
    CG_PROFILE_PS_2_X = 6163, // CG_PROFILE_MACRO(DX9Pixel2x,ps_2_x,PS_2_X,"ps_2_x",6163,0)

    CG_PROFILE_MAX = 7100
  );
  CGprofile = TCGprofile;

 (*
  * The following macro invocations define error codes returned by various cg
  * API functions.
  *
  * The macros have the form :
  *
  *   CG_ERROR_MACRO(code, enum_name, message)
  *
  *     code      : The integer error code associated with the error.
  *     enum_name : The name of enumerant of the error code in the API.
  *     message   : A description string associated with the error.
  *
  *)
  PCGerror = ^TCGerror;
  TCGerror = DWORD;
  CGerror = TCGerror;

const
  //# define CG_ERROR_MACRO(code, enum_name, new_enum_name, message) \
  //   new_enum_name = code,
  CG_NO_ERROR                       = 0;  // "No error has occurred."
  CG_COMPILER_ERROR                 = 1;  // "The compile returned an error."
  CG_INVALID_PARAMETER_ERROR        = 2;  // "The parameter used is invalid."
  CG_INVALID_PROFILE_ERROR          = 3;  // "The profile is not supported."
  CG_PROGRAM_LOAD_ERROR             = 4;  // "The program did could not load."
  CG_PROGRAM_BIND_ERROR             = 5;  // "The program could not bind."
  CG_PROGRAM_NOT_LOADED_ERROR       = 6;  // "The program must be loaded before this operation may be used."
  CG_UNSUPPORTED_GL_EXTENSION_ERROR = 7;  // "An unsupported GL extension was required to perform this operation."
  CG_INVALID_VALUE_TYPE_ERROR       = 8;  // "An unknown value type was assigned to a parameter."
  CG_NOT_MATRIX_PARAM_ERROR         = 9;  // "The parameter is not of matrix type."
  CG_INVALID_ENUMERANT_ERROR        = 10; // "The enumerant parameter has an invalid value."
  CG_NOT_4x4_MATRIX_ERROR           = 11; // "The parameter must be a 4x4 matrix type."
  CG_FILE_READ_ERROR                = 12; // "The file could not be read."
  CG_FILE_WRITE_ERROR               = 13; // "The file could not be written."
  CG_NVPARSE_ERROR                  = 14; // "nvparse could not successfully parse the output from the Cg compiler backend."
  CG_MEMORY_ALLOC_ERROR             = 15; // "Memory allocation failed."
  CG_INVALID_CONTEXT_HANDLE_ERROR   = 16; // "Invalid context handle."
  CG_INVALID_PROGRAM_HANDLE_ERROR   = 17; // "Invalid program handle."
  CG_INVALID_PARAM_HANDLE_ERROR     = 18; // "Invalid parameter handle."
  CG_UNKNOWN_PROFILE_ERROR          = 19; // "The specified profile is unknown."
  CG_VAR_ARG_ERROR                  = 20; // "The variable arguments were specified incorrectly."
  CG_INVALID_DIMENSION_ERROR        = 21; // "The dimension value is invalid."
  CG_ARRAY_PARAM_ERROR              = 22; // "The parameter must be an array."
  CG_OUT_OF_ARRAY_BOUNDS_ERROR      = 23; // "Index into the array is out of bounds."
  CG_CONFLICTING_TYPES_ERROR        = 24; // "A type being added to the context conflicts with an existing type."
  CG_CONFLICTING_PARAMETER_TYPES_ERROR = 25; // "The parameters being bound have conflicting types."
  CG_PARAMETER_IS_NOT_SHARED_ERROR  = 26; // "The parameter must be global."
  CG_INVALID_PARAMETER_VARIABILITY_ERROR = 27; // "The parameter could not be changed to the given variability."
  CG_CANNOT_DESTROY_PARAMETER_ERROR = 28; // "Cannot destroy the parameter.  It is bound to other parameters or is not a root parameter."
  CG_NOT_ROOT_PARAMETER_ERROR       = 29; // "The parameter is not a root parameter."
  CG_PARAMETERS_DO_NOT_MATCH_ERROR  = 30; // "The two parameters being bound do not match."
  CG_IS_NOT_PROGRAM_PARAMETER_ERROR = 31; // "The parameter is not a program parameter."
  CG_INVALID_PARAMETER_TYPE_ERROR   = 32; // "The type of the parameter is invalid."
  CG_PARAMETER_IS_NOT_RESIZABLE_ARRAY_ERROR = 33; // "The parameter must be a resizable array."
  CG_INVALID_SIZE_ERROR             = 34; // "The size value is invalid."
  CG_BIND_CREATES_CYCLE_ERROR       = 35; // "Cannot bind the given parameters.  Binding will form a cycle."
  CG_ARRAY_TYPES_DO_NOT_MATCH_ERROR = 36; // "Cannot bind the given parameters.  Array types do not match."
  CG_ARRAY_DIMENSIONS_DO_NOT_MATCH_ERROR = 37; // "Cannot bind the given parameters.  Array dimensions do not match."
  CG_ARRAY_HAS_WRONG_DIMENSION_ERROR = 38; // "The array is has the wrong dimension."
  CG_TYPE_IS_NOT_DEFINED_IN_PROGRAM_ERROR = 39; // "Connecting the parameters failed because The type of the source parameter is not defined within the given program or does not match the type with the same name in the program."

type
  TCGenum = (
  //todo: Insert code from here: # include <Cg/cg_enums.h>
  //todo: FIX BCB6 issues with {$IFDEF BCB}
    CG_UNKNOWN = 4096,
    CG_IN,
    CG_OUT,
    CG_INOUT,
    CG_MIXED,
    CG_VARYING,
    CG_UNIFORM,
    CG_CONSTANT,
    CG_PROGRAM_SOURCE,
    CG_PROGRAM_ENTRY,
    CG_COMPILED_PROGRAM,
    CG_PROGRAM_PROFILE,

    CG_GLOBAL,
    CG_PROGRAM,

    CG_DEFAULT,
    CG_ERROR,

    CG_SOURCE,
    CG_OBJECT,

    CG_COMPILE_MANUAL,
    CG_COMPILE_IMMEDIATE,
    CG_COMPILE_LAZY,
    CG_CURRENT,
    CG_LITERAL,
    CG_VERSION
  );
  CGenum = TCGenum;

type
  TCGerrorCallbackFunc = procedure; cdecl;
  CGerrorCallbackFunc = TCGerrorCallbackFunc;


(*************************************************************************)
(*** Functions                                                         ***)
(*************************************************************************)

//{$IFNDEF CG_EXPLICIT}

(*** Context functions ***)

function cgCreateContext: PCGcontext; cdecl; external CgLibrary;
procedure cgDestroyContext(ctx: PCGcontext); cdecl; external CgLibrary;
function cgIsContext(ctx: PCGcontext): TCGbool; cdecl; external CgLibrary;
function cgGetLastListing(ctx: PCGcontext): PChar{ const }; cdecl; external CgLibrary;
procedure cgSetAutoCompile(ctx: PCGcontext; flag: TCGenum); cdecl; external CgLibrary;

  //PPChar = ^PChar; //Clootie: It's actually pointer to array of PChar strings

(*** Program functions ***)
function cgCreateProgram(ctx: PCGcontext;
  program_type: TCGenum; const _program: PCharCG;
  profile: TCGprofile; const entry: PCharCG;
  const args: PPCharCG): PCGprogram; cdecl; external CgLibrary;
function cgCreateProgramFromFile(ctx: PCGcontext;
  program_type: TCGenum; const program_file: PCharCG;
  profile: TCGprofile; const entry: PCharCG;
  const args: PPCharCG): PCGprogram; cdecl; external CgLibrary;
function cgCopyProgram(_program: PCGprogram): PCGprogram; cdecl; external CgLibrary;
procedure cgDestroyProgram(_program: PCGprogram); cdecl; external CgLibrary;

function cgGetFirstProgram(ctx: PCGcontext): PCGprogram; cdecl; external CgLibrary;
function cgGetNextProgram(current: PCGprogram): PCGprogram; cdecl; external CgLibrary;
function cgGetProgramContext(prog: PCGprogram): PCGcontext; cdecl; external CgLibrary;
function cgIsProgram(_program: PCGprogram): TCGbool; cdecl; external CgLibrary;

procedure cgCompileProgram(_program: PCGprogram); cdecl; external CgLibrary;
function cgIsProgramCompiled(_program: PCGprogram): TCGbool; cdecl; external CgLibrary;
function cgGetProgramString(prog: PCGprogram; pname: TCGenum): PCharCG{ const }; cdecl; external CgLibrary;
function cgGetProgramProfile(prog: PCGprogram): TCGprofile; cdecl; external CgLibrary;

(*** Parameter functions ***)

function cgCreateParameter(ctx: PCGcontext; type_: TCGtype): PCGparameter; cdecl; external CgLibrary;
function cgCreateParameterArray(ctx: PCGcontext; type_: TCGtype; length: Integer): PCGparameter; cdecl; external CgLibrary;
function cgCreateParameterMultiDimArray(ctx: PCGcontext; type_: TCGtype; dim: Integer; const lengths: PInteger): PCGparameter; cdecl; external CgLibrary;
procedure cgDestroyParameter(param: PCGparameter); cdecl; external CgLibrary;
procedure cgConnectParameter(from, to_: PCGparameter); cdecl; external CgLibrary;
procedure cgDisconnectParameter(param: PCGparameter); cdecl; external CgLibrary;
function cgGetConnectedParameter(param: PCGparameter): PCGparameter; cdecl; external CgLibrary;

function cgGetNumConnectedToParameters(param: PCGparameter): Integer; cdecl; external CgLibrary;
function cgGetConnectedToParameter(param: PCGparameter; index: Integer): PCGparameter; cdecl; external CgLibrary;

function cgGetNamedParameter(prog: PCGprogram; const name: PCharCG): PCGparameter; cdecl; external CgLibrary;
function cgGetNamedProgramParameter(prog: PCGprogram; name_space: TCGenum; const name: PCharCG): PCGparameter; cdecl; external CgLibrary;

function cgGetFirstParameter(prog: PCGprogram; name_space: TCGenum): PCGparameter; cdecl; external CgLibrary;
{$IFNDEF CG_DEPRECATED_1_1_API}
function cgGetNextParameter(current: PCGparameter): PCGparameter; cdecl; external CgLibrary;
{$ENDIF}
function cgGetFirstLeafParameter(prog: PCGprogram; name_space: TCGenum): PCGparameter; cdecl; external CgLibrary;
{$IFNDEF CG_DEPRECATED_1_1_API}
function cgGetNextLeafParameter(current: PCGparameter): PCGparameter; cdecl; external CgLibrary;
{$ENDIF}

function cgGetFirstStructParameter(param: PCGparameter): PCGparameter; cdecl; external CgLibrary;
function cgGetNamedStructParameter(param: PCGparameter; const name: PCharCG): PCGparameter; cdecl; external CgLibrary;

function cgGetFirstDependentParameter(param: PCGparameter): PCGparameter; cdecl; external CgLibrary;

function cgGetArrayParameter(aparam: PCGparameter; index: Integer): PCGparameter; cdecl; external CgLibrary;
function cgGetArrayDimension(param: PCGparameter): Integer; cdecl; external CgLibrary;
function cgGetArrayType(param: PCGparameter): TCGtype; cdecl; external CgLibrary;
function cgGetArraySize(param: PCGparameter; dimension: Integer): Integer; cdecl; external CgLibrary;
procedure cgSetArraySize(param: PCGparameter; size: Integer); cdecl; external CgLibrary;
procedure cgSetMultiDimArraySize(param: PCGparameter; const sizes: PInteger); cdecl; external CgLibrary;

function cgGetParameterProgram(param: PCGparameter): PCGprogram; cdecl; external CgLibrary;
function cgGetParameterContext(param: PCGparameter): PCGcontext; cdecl; external CgLibrary;
function cgIsParameter(param: PCGparameter): TCGbool; cdecl; external CgLibrary;
function cgGetParameterName(param: PCGparameter): PCharCG{ const }; cdecl; external CgLibrary;
function cgGetParameterType(param: PCGparameter): TCGtype; cdecl; external CgLibrary;
function cgGetParameterNamedType(param: PCGparameter): TCGtype; cdecl; external CgLibrary;
function cgGetParameterSemantic(param: PCGparameter): PCharCG{ const }; cdecl; external CgLibrary;
function cgGetParameterResource(param: PCGparameter): TCGresource; cdecl; external CgLibrary;
function cgGetParameterBaseResource(param: PCGparameter): TCGresource; cdecl; external CgLibrary;
function cgGetParameterResourceIndex(param: PCGparameter): LongWord; cdecl; external CgLibrary;
function cgGetParameterVariability(param: PCGparameter): TCGenum; cdecl; external CgLibrary;
function cgGetParameterDirection(param: PCGparameter): TCGenum; cdecl; external CgLibrary;
function cgIsParameterReferenced(param: PCGparameter): TCGbool; cdecl; external CgLibrary;
function cgGetParameterValues(param: PCGparameter; value_type: TCGenum;
  out nvalues: Integer): PDouble{ const }; cdecl; external CgLibrary;
function cgGetParameterOrdinalNumber(param: PCGparameter): Integer; cdecl; external CgLibrary;
function cgIsParameterGlobal(param: PCGparameter): TCGbool; cdecl; external CgLibrary;
function cgGetParameterIndex(param: PCGparameter): Integer; cdecl; external CgLibrary;

procedure cgSetParameterVariability(param: PCGparameter; vary: TCGenum); cdecl; external CgLibrary;
procedure cgSetParameterSemantic(param: PCGparameter; const semantic: PCharCG); cdecl; external CgLibrary;


procedure cgSetParameter1f(param: PCGparameter; x: Single); cdecl; external CgLibrary;
procedure cgSetParameter2f(param: PCGparameter; x, y: Single); cdecl; external CgLibrary;
procedure cgSetParameter3f(param: PCGparameter; x, y, z: Single); cdecl; external CgLibrary;
procedure cgSetParameter4f(param: PCGparameter; x, y, z, w: Single); cdecl; external CgLibrary;
procedure cgSetParameter1d(param: PCGparameter; x: Double); cdecl; external CgLibrary;
procedure cgSetParameter2d(param: PCGparameter; x, y: Double); cdecl; external CgLibrary;
procedure cgSetParameter3d(param: PCGparameter; x, y, z: Double); cdecl; external CgLibrary;
procedure cgSetParameter4d(param: PCGparameter; x, y, z, w: Double); cdecl; external CgLibrary;


procedure cgSetParameter1fv(param: PCGparameter; const v: PSingle); cdecl; external CgLibrary;
procedure cgSetParameter2fv(param: PCGparameter; const v: PSingle); cdecl; external CgLibrary;
procedure cgSetParameter3fv(param: PCGparameter; const v: PSingle); cdecl; external CgLibrary;
procedure cgSetParameter4fv(param: PCGparameter; const v: PSingle); cdecl; external CgLibrary;
procedure cgSetParameter1dv(param: PCGparameter; const x: PDouble); cdecl; external CgLibrary;
procedure cgSetParameter2dv(param: PCGparameter; const x: PDouble); cdecl; external CgLibrary;
procedure cgSetParameter3dv(param: PCGparameter; const x: PDouble); cdecl; external CgLibrary;
procedure cgSetParameter4dv(param: PCGparameter; const x: PDouble); cdecl; external CgLibrary;

procedure cgSetMatrixParameterdr(param: PCGparameter; const matrix: PDouble); cdecl; external CgLibrary;
procedure cgSetMatrixParameterfr(param: PCGparameter; const matrix: PSingle); cdecl; external CgLibrary;
procedure cgSetMatrixParameterdc(param: PCGparameter; const matrix: PDouble); cdecl; external CgLibrary;
procedure cgSetMatrixParameterfc(param: PCGparameter; const matrix: PSingle); cdecl; external CgLibrary;


(*** Type Functions ***)

function cgGetTypeString(_type: TCGtype): PCharCG{ const }; cdecl; external CgLibrary;
function cgGetType(const type_string: PCharCG): TCGtype; cdecl; external CgLibrary;

function cgGetNamedUserType(program_: CGprogram; const name: PCharCG): TCGtype; cdecl; external CgLibrary;

function cgGetNumUserTypes(program_: CGprogram): Integer; cdecl; external CgLibrary;
function cgGetUserType(program_: CGprogram; index: Integer): TCGtype; cdecl; external CgLibrary;

function cgGetNumParentTypes(type_: TCGtype): Integer; cdecl; external CgLibrary;
function cgGetParentType(type_: TCGtype; index: Integer): TCGtype; cdecl; external CgLibrary;

function cgIsParentType(parent, child: TCGtype): TCGbool; cdecl; external CgLibrary;
function cgIsInterfaceType(type_: TCGtype): TCGbool; cdecl; external CgLibrary;

(*** Resource Functions ***)

function cgGetResourceString(resource: TCGresource): PCharCG{ const }; cdecl; external CgLibrary;
function cgGetResource(const resource_string: PCharCG): TCGresource; cdecl; external CgLibrary;

{*** Enum Functions ***}

function cgGetEnumString(en: TCGenum): PCharCG{ const }; cdecl; external CgLibrary;
function cgGetEnum(const enum_string: PCharCG): TCGenum; cdecl; external CgLibrary;

(*** Profile Functions ***)

function cgGetProfileString(profile: TCGprofile): PCharCG{ const }; cdecl; external CgLibrary;
function cgGetProfile(const profile_string: PCharCG): TCGprofile; cdecl; external CgLibrary;

(*** Error Functions ***)

function cgGetError: TCGerror; cdecl; external CgLibrary;
function cgGetErrorString(error: TCGerror): PCharCG{ const }; cdecl; external CgLibrary;
function cgGetLastErrorString(error: PCGerror): PCharCG{ const }; cdecl; external CgLibrary;
procedure cgSetErrorCallback(func: TCGerrorCallbackFunc); cdecl; external CgLibrary;
function cgGetErrorCallback: TCGerrorCallbackFunc; cdecl; external CgLibrary;


{*** Misc Functions ***}

function cgGetString(sname: TCGenum): PCharCG{ const }; cdecl; external CgLibrary;


{*** Support for deprecated Cg 1.1 API ***}

function cgGetNextParameter_depr1_1(current: PCGparameter): PCGparameter; cdecl; external CgLibrary;
function cgGetNextLeafParameter_depr1_1(current: PCGparameter): PCGparameter; cdecl; external CgLibrary;

{$IFDEF CG_DEPRECATED_1_1_API}

function cgGetNextParameter(current: PCGparameter): PCGparameter; cdecl; external CgLibrary name 'cgGetNextParameter_depr1_1';
function cgGetNextLeafParameter(current: PCGparameter): PCGparameter; cdecl; external CgLibrary name 'cgGetNextLeafParameter_depr1_1';

{$ENDIF}

implementation

end.
