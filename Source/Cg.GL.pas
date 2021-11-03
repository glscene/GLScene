{******************************************************************************}
{*                                                                            *}
{*  Copyright (c) 2002, NVIDIA Corporation.                                   *}
{*                                                                            *}
{*  Files:    cgGL.h                                                          *}
{*  Content:  NVIDIA Cg OpenGL interface include files                        *}
{*                                                                            *}
{*  NVIDIA "Cg" Release 1.2 ObjectPascal adaptation by Alexey Barkovoy        *}
{*  E-Mail: clootie@ixbt.com                                                  *}
{*                                                                            *}
{*  Modified: 14-Mar-2004                                                     *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*     http://clootie.narod.ru                                                *}
{*       http://developer.nvidia.com/object/cg_download.html                  *}
{*                                                                            *}
{******************************************************************************}
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

(*
 *
 * Copyright (c) 2002, NVIDIA Corporation.
 *
 *
 *
 * NVIDIA Corporation("NVIDIA") supplies this software to you in consideration
 * of your agreement to the following terms, and your use, installation,
 * modification or redistribution of this NVIDIA software constitutes
 * acceptance of these terms.  If you do not agree with these terms, please do
 * not use, install, modify or redistribute this NVIDIA software.
 *
 * In consideration of your agreement to abide by the following terms, and
 * subject to these terms, NVIDIA grants you a personal, non-exclusive license,
 * under NVIDIARs copyrights in this original NVIDIA software (the "NVIDIA
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
 *
 *)
unit Cg.GL;

{$I GLScene.inc}

interface

uses
  Winapi.Windows,
  Cg.Import;

const
  {$IFDEF MSWINDOWS}
  CgGLlibrary = 'cgGL.dll';
  {$ELSE}
  CgGLlibrary = 'libCgGL.so';
  {$ENDIF}

(*****************************************************************************)
(*** cgGL Type Definitions                                                 ***)
(*****************************************************************************)

type
  TCGGLenum = Cardinal;
  CGGLenum = TCGGLenum;

const
  CG_GL_MATRIX_IDENTITY          = 0;
  CG_GL_MATRIX_TRANSPOSE         = 1;
  CG_GL_MATRIX_INVERSE           = 2;
  CG_GL_MATRIX_INVERSE_TRANSPOSE = 3;

  CG_GL_MODELVIEW_MATRIX         = 4;
  CG_GL_PROJECTION_MATRIX        = 5;
  CG_GL_TEXTURE_MATRIX           = 6;
  CG_GL_MODELVIEW_PROJECTION_MATRIX = 7;

  CG_GL_VERTEX                   = 8;
  CG_GL_FRAGMENT                 = 9;

(******************************************************************************
 *** Profile Functions
 *****************************************************************************)

function cgGLIsProfileSupported(profile: TCGprofile): TCGbool; cdecl; external CgGLlibrary;

procedure cgGLEnableProfile(profile: TCGprofile); cdecl; external CgGLlibrary;
procedure cgGLDisableProfile(profile: TCGprofile); cdecl; external CgGLlibrary;

function cgGLGetLatestProfile(profile_type: TCGGLenum): TCGprofile; cdecl; external CgGLlibrary;
procedure cgGLSetOptimalOptions(profile: TCGprofile); cdecl; external CgGLlibrary;

(******************************************************************************
 *** Program Managment Functions
 *****************************************************************************)

procedure cgGLLoadProgram(_program: PCGprogram); cdecl; external CgGLlibrary;
function cgGLIsProgramLoaded(_program: PCGprogram): TCGbool; cdecl; external CgGLlibrary;
procedure cgGLBindProgram(_program: PCGprogram); cdecl; external CgGLlibrary;
procedure cgGLUnbindProgram(profile: TCGprofile); cdecl; external CgGLlibrary;
function cgGLGetProgramID(_program: PCGprogram): Cardinal; cdecl; external CgGLlibrary;

(******************************************************************************
 *** Parameter Managment Functions
 *****************************************************************************)

procedure cgGLSetParameter1f(param: PCGparameter; x: Single); cdecl; external CgGLlibrary;

procedure cgGLSetParameter2f(param: PCGparameter; x, y: Single); cdecl; external CgGLlibrary;

procedure cgGLSetParameter3f(param: PCGparameter; x, y, z: Single); cdecl; external CgGLlibrary;

procedure cgGLSetParameter4f(param: PCGparameter; x, y, z, w: Single); cdecl; external CgGLlibrary;

procedure cgGLSetParameter1fv(param: PCGparameter; const v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLSetParameter2fv(param: PCGparameter; const v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLSetParameter3fv(param: PCGparameter; const v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLSetParameter4fv(param: PCGparameter; const v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLSetParameter1d(param: PCGparameter; x: Double); cdecl; external CgGLlibrary;

procedure cgGLSetParameter2d(param: PCGparameter; x, y: Double); cdecl; external CgGLlibrary;

procedure cgGLSetParameter3d(param: PCGparameter; x, y, z: Double); cdecl; external CgGLlibrary;

procedure cgGLSetParameter4d(param: PCGparameter; x, y, z, w: Double); cdecl; external CgGLlibrary;

procedure cgGLSetParameter1dv(param: PCGparameter; const v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLSetParameter2dv(param: PCGparameter; const v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLSetParameter3dv(param: PCGparameter; const v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLSetParameter4dv(param: PCGparameter; const v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLGetParameter1f(param: PCGparameter; v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLGetParameter2f(param: PCGparameter; v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLGetParameter3f(param: PCGparameter; v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLGetParameter4f(param: PCGparameter; v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLGetParameter1d(param: PCGparameter; v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLGetParameter2d(param: PCGparameter; v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLGetParameter3d(param: PCGparameter; v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLGetParameter4d(param: PCGparameter; v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLSetParameterArray1f(param: PCGparameter;
    offset, nelements: Longint; const v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLSetParameterArray2f(param: PCGparameter;
    offset, nelements: Longint; const v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLSetParameterArray3f(param: PCGparameter;
    offset, nelements: Longint; const v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLSetParameterArray4f(param: PCGparameter;
    offset, nelements: Longint; const v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLSetParameterArray1d(param: PCGparameter;
    offset, nelements: Longint; const v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLSetParameterArray2d(param: PCGparameter;
    offset, nelements: Longint; const v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLSetParameterArray3d(param: PCGparameter;
    offset, nelements: Longint; const v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLSetParameterArray4d(param: PCGparameter;
    offset, nelements: Longint; const v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLGetParameterArray1f(param: PCGparameter;
    offset, nelements: Longint; v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLGetParameterArray2f(param: PCGparameter;
    offset, nelements: Longint; v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLGetParameterArray3f(param: PCGparameter;
    offset, nelements: Longint; v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLGetParameterArray4f(param: PCGparameter;
    offset, nelements: Longint; v: PSingle); cdecl; external CgGLlibrary;

procedure cgGLGetParameterArray1d(param: PCGparameter;
    offset, nelements: Longint; v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLGetParameterArray2d(param: PCGparameter;
    offset, nelements: Longint; v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLGetParameterArray3d(param: PCGparameter;
    offset, nelements: Longint; v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLGetParameterArray4d(param: PCGparameter;
    offset, nelements: Longint; v: PDouble); cdecl; external CgGLlibrary;

procedure cgGLSetParameterPointer(param: PCGparameter; fsize: Integer;
    _type: Cardinal; stride: Integer; const _pointer: Pointer); cdecl; external CgGLlibrary;

procedure cgGLEnableClientState(param: PCGparameter); cdecl; external CgGLlibrary;
procedure cgGLDisableClientState(param: PCGparameter); cdecl; external CgGLlibrary;

(******************************************************************************
 *** Matrix Parameter Managment Functions
 *****************************************************************************)

procedure cgGLSetMatrixParameterdr(param: PCGparameter; const matrix: PDouble); cdecl; external CgGLlibrary;
procedure cgGLSetMatrixParameterfr(param: PCGparameter; const matrix: PSingle); cdecl; external CgGLlibrary;
procedure cgGLSetMatrixParameterdc(param: PCGparameter; const matrix: PDouble); cdecl; external CgGLlibrary;
procedure cgGLSetMatrixParameterfc(param: PCGparameter; const matrix: PSingle); cdecl; external CgGLlibrary;

procedure cgGLGetMatrixParameterdr(param: PCGparameter; matrix: PDouble); cdecl; external CgGLlibrary;
procedure cgGLGetMatrixParameterfr(param: PCGparameter; matrix: PSingle); cdecl; external CgGLlibrary;
procedure cgGLGetMatrixParameterdc(param: PCGparameter; matrix: PDouble); cdecl; external CgGLlibrary;
procedure cgGLGetMatrixParameterfc(param: PCGparameter; matrix: PSingle); cdecl; external CgGLlibrary;

procedure cgGLSetStateMatrixParameter(param: PCGparameter;
    matrix: TCGGLenum; transform: TCGGLenum); cdecl; external CgGLlibrary;

procedure cgGLSetMatrixParameterArrayfc(param: PCGparameter;
    offset, nelements: Longint; const matrices: PSingle); cdecl; external CgGLlibrary;

procedure cgGLSetMatrixParameterArrayfr(param: PCGparameter;
    offset, nelements: Longint; const matrices: PSingle); cdecl; external CgGLlibrary;

procedure cgGLSetMatrixParameterArraydc(param: PCGparameter;
    offset, nelements: Longint; const matrices: PDouble); cdecl; external CgGLlibrary;

procedure cgGLSetMatrixParameterArraydr(param: PCGparameter;
    offset, nelements: Longint; const matrices: PDouble); cdecl; external CgGLlibrary;

procedure cgGLGetMatrixParameterArrayfc(param: PCGparameter;
    offset, nelements: Longint; matrices: PSingle); cdecl; external CgGLlibrary;

procedure cgGLGetMatrixParameterArrayfr(param: PCGparameter;
    offset, nelements: Longint; matrices: PSingle); cdecl; external CgGLlibrary;

procedure cgGLGetMatrixParameterArraydc(param: PCGparameter;
    offset, nelements: Longint; matrices: PDouble); cdecl; external CgGLlibrary;

procedure cgGLGetMatrixParameterArraydr(param: PCGparameter;
    offset, nelements: Longint; matrices: PDouble); cdecl; external CgGLlibrary;

(******************************************************************************
 *** Texture Parameter Managment Functions
 *****************************************************************************)

procedure cgGLSetTextureParameter(param: PCGparameter; texobj: Cardinal); cdecl; external CgGLlibrary;
function cgGLGetTextureParameter(param: PCGparameter): Cardinal; cdecl; external CgGLlibrary;
procedure cgGLEnableTextureParameter(param: PCGparameter); cdecl; external CgGLlibrary;
procedure cgGLDisableTextureParameter(param: PCGparameter); cdecl; external CgGLlibrary;
function cgGLGetTextureEnum(param: PCGparameter): Cardinal; cdecl; external CgGLlibrary;
procedure cgGLSetManageTextureParameters(ctx: PCGcontext; flag: TCGbool); cdecl; external CgGLlibrary;
function cgGLGetManageTextureParameters(ctx: PCGcontext): TCGbool; cdecl; external CgGLlibrary;

implementation

end.
