//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.XOpenGL;

(*
   "Alternate" OpenGL functions to handle multi-texturing.
   Using this functions allows specifying none/one/multiple ARB multi-texture
   coordinates with standard texture specification call.
   Before using any of the xglTexCoordXxxx functions, call one of the
   xglMapTexCoordToXxxx functions to establish the redirectors.
   This unit is Open-Source under MPL
   The history is logged in a former GLS version of the unit.
*)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext;

type
  TMapTexCoordMode = (mtcmUndefined, mtcmNull, mtcmMain, mtcmDual, mtcmSecond, mtcmArbitrary);

// TexCoord functions will be ignored.
procedure xglMapTexCoordToNull;
// TexCoord functions will define the main texture coordinates.
procedure xglMapTexCoordToMain;
// TexCoord functions will define the second texture unit coordinates.
procedure xglMapTexCoordToSecond;
// TexCoord functions will define the two first texture units coordinates.
procedure xglMapTexCoordToDual;
// TexCoord functions will define the specified texture units coordinates.
procedure xglMapTexCoordToArbitrary(const units: array of Cardinal); overload;
procedure xglMapTexCoordToArbitrary(const bitWiseUnits: Cardinal); overload;
procedure xglMapTexCoordToArbitraryAdd(const bitWiseUnits: Cardinal);

// Defers xglMap calls execution until xglEndUpdate is met. Calls to Begin/EndUpdate may be nested.
procedure xglBeginUpdate;
// Applies xglMap calls if there were any since xglBeginUpdate was invoked. Calls to Begin/EndUpdate may be nested.
procedure xglEndUpdate;

// Saves State on the stack.
procedure xglPushState;
// Restores State from the stack.
procedure xglPopState;

(* Whenever called, 2nd texture units changes will be forbidden to .
  Use this function when you're using the 2nd texture unit for your own
  purposes and don't want to alter it. *)
procedure xglForbidSecondTextureUnit;
// Allow to use the second texture unit again.
procedure xglAllowSecondTextureUnit;
// Returns the complex mapping in bitwise form.
function xglGetBitWiseMapping: Cardinal;

{$IFDEF MULTITHREADOPENGL}
threadvar
{$ELSE}

var
{$ENDIF}
  xglMapTexCoordMode: TMapTexCoordMode;
  vSecondTextureUnitForbidden: Boolean;

  // Explicit texture coordinates specification
  xglTexCoord2f: procedure(s, t: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  xglTexCoord2fv: procedure(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  xglTexCoord3f: procedure(s, t, r: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  xglTexCoord3fv: procedure(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  xglTexCoord4f: procedure(s, t, r, q: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  xglTexCoord4fv: procedure(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  // TexGen texture coordinates specification
  xglTexGenf: procedure(coord, pname: Cardinal; param: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  xglTexGenfv: procedure(coord, pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  xglTexGeni: procedure(coord, pname: Cardinal; param: GLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  xglTexGeniv: procedure(coord, pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  // Vertex Arrays texture coordinates specification
  xglTexCoordPointer: procedure(size: GLint; atype: GLEnum; stride: GLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  xglEnableClientState: procedure(aarray: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  xglDisableClientState: procedure(aarray: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  // Misc
  xglEnable: procedure(cap: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  xglDisable: procedure(cap: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

//===================================================================
implementation
//===================================================================


var
  vUpdCount: Integer;
  vUpdNewMode: TMapTexCoordMode;
  vStateStack: array of TMapTexCoordMode;
  vComplexMapping: array of Cardinal;
  vComplexMappingN: Integer;

  // ------------------------------------------------------------------
  // Multitexturing coordinates duplication functions
  // ------------------------------------------------------------------

  // --------- Complex (arbitrary) mapping

procedure glTexCoord2f_Arbitrary(s, t: Single); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
    glMultiTexCoord2f(vComplexMapping[i], s, t);
end;

procedure glTexCoord2fv_Arbitrary(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
    glMultiTexCoord2fv(vComplexMapping[i], v);
end;

procedure glTexCoord3f_Arbitrary(s, t, r: Single); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
    glMultiTexCoord3f(vComplexMapping[i], s, t, r);
end;

procedure glTexCoord3fv_Arbitrary(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
    glMultiTexCoord3fv(vComplexMapping[i], v);
end;

procedure glTexCoord4f_Arbitrary(s, t, r, q: Single); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
    glMultiTexCoord4f(vComplexMapping[i], s, t, r, q);
end;

procedure glTexCoord4fv_Arbitrary(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
    glMultiTexCoord4fv(vComplexMapping[i], v);
end;

procedure glTexGenf_Arbitrary(coord, pname: GLEnum; param: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    glActiveTexture(vComplexMapping[i]);
    glTexGenf(coord, pname, param);
  end;
  glActiveTexture(GL_TEXTURE0_ARB);
end;

procedure glTexGenfv_Arbitrary(coord, pname: GLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    glActiveTexture(vComplexMapping[i]);
    glTexGenfv(coord, pname, params);
  end;
  glActiveTexture(GL_TEXTURE0_ARB);
end;

procedure glTexGeni_Arbitrary(coord, pname: GLEnum; param: GLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    glActiveTexture(vComplexMapping[i]);
    glTexGeni(coord, pname, param);
  end;
  glActiveTexture(GL_TEXTURE0_ARB);
end;

procedure glTexGeniv_Arbitrary(coord, pname: GLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    glActiveTexture(vComplexMapping[i]);
    glTexGeniv(coord, pname, params);
  end;
  glActiveTexture(GL_TEXTURE0_ARB);
end;

procedure glEnable_Arbitrary(cap: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    glActiveTexture(vComplexMapping[i]);
    glEnable(cap);
  end;
  glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glDisable_Arbitrary(cap: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    glActiveTexture(vComplexMapping[i]);
    glDisable(cap);
  end;
  glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure xglTexCoordPointer_Arbitrary(size: GLint; atype: GLEnum; stride: GLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl; {$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    glClientActiveTexture(vComplexMapping[i]);
    glTexCoordPointer(size, atype, stride, data);
  end;
  glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure xglEnableClientState_Arbitrary(aarray: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    glClientActiveTexture(vComplexMapping[i]);
    glEnableClientState(aarray);
  end;
  glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure xglDisableClientState_Arbitrary(aarray: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    glClientActiveTexture(vComplexMapping[i]);
    glDisableClientState(aarray);
  end;
  glActiveTextureARB(GL_TEXTURE0_ARB);
end;

// --------- Second unit Texturing

procedure glTexCoord2f_Second(s, t: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glMultiTexCoord2f(GL_TEXTURE1, s, t);
end;

procedure glTexCoord2fv_Second(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glMultiTexCoord2fv(GL_TEXTURE1, v);
end;

procedure glTexCoord3f_Second(s, t, r: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glMultiTexCoord3f(GL_TEXTURE1, s, t, r);
end;

procedure glTexCoord3fv_Second(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glMultiTexCoord3fv(GL_TEXTURE1, v);
end;

procedure glTexCoord4f_Second(s, t, r, q: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glMultiTexCoord4f(GL_TEXTURE1, s, t, r, q);
end;

procedure glTexCoord4fv_Second(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glMultiTexCoord4fv(GL_TEXTURE1, v);
end;

procedure glTexGenf_Second(coord, pname: GLEnum; param: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glActiveTexture(GL_TEXTURE1);
  glTexGenf(coord, pname, param);
  glActiveTexture(GL_TEXTURE0);
end;

procedure glTexGenfv_Second(coord, pname: GLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glActiveTexture(GL_TEXTURE1);
  glTexGenfv(coord, pname, params);
  glActiveTexture(GL_TEXTURE0);
end;

procedure glTexGeni_Second(coord, pname: GLEnum; param: GLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glTexGeni(coord, pname, param);
  glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glTexGeniv_Second(coord, pname: GLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glActiveTexture(GL_TEXTURE1_ARB);
  glTexGeniv(coord, pname, params);
  glActiveTexture(GL_TEXTURE0_ARB);
end;

procedure glEnable_Second(cap: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glActiveTexture(GL_TEXTURE1_ARB);
  glEnable(cap);
  glActiveTexture(GL_TEXTURE0_ARB);
end;

procedure glDisable_Second(cap: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glActiveTexture(GL_TEXTURE1_ARB);
  glDisable(cap);
  glActiveTexture(GL_TEXTURE0_ARB);
end;

procedure xglTexCoordPointer_Second(size: GLint; atype: GLEnum; stride: GLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glClientActiveTexture(GL_TEXTURE1);
  glTexCoordPointer(size, atype, stride, data);
  glClientActiveTexture(GL_TEXTURE0);
end;

procedure xglEnableClientState_Second(aArray: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glClientActiveTexture(GL_TEXTURE1);
  glEnableClientState(aarray);
  glClientActiveTexture(GL_TEXTURE0);
end;

procedure xglDisableClientState_Second(aArray: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glClientActiveTexture(GL_TEXTURE1);
  glDisableClientState(aArray);
  glClientActiveTexture(GL_TEXTURE0);
end;

// --------- Dual Texturing

procedure glTexCoord2f_Dual(s, t: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glTexCoord2f(s, t);
  glMultiTexCoord2f(GL_TEXTURE1, s, t);
end;

procedure glTexCoord2fv_Dual(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glTexCoord2fv(v);
  glMultiTexCoord2fv(GL_TEXTURE1, v);
end;

procedure glTexCoord3f_Dual(s, t, r: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glTexCoord3f(s, t, r);
  glMultiTexCoord3f(GL_TEXTURE1, s, t, r);
end;

procedure glTexCoord3fv_Dual(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glTexCoord3fv(v);
  glMultiTexCoord3fv(GL_TEXTURE1, v);
end;

procedure glTexCoord4f_Dual(s, t, r, q: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glTexCoord4f(s, t, r, q);
  glMultiTexCoord4f(GL_TEXTURE1, s, t, r, q);
end;

procedure glTexCoord4fv_Dual(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glTexCoord4fv(v);
  glMultiTexCoord4fv(GL_TEXTURE1, v);
end;

procedure glTexGenf_Dual(coord, pname: GLEnum; param: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glTexGenf(coord, pname, param);
  glActiveTexture(GL_TEXTURE1_ARB);
  glTexGenf(coord, pname, param);
  glActiveTexture(GL_TEXTURE0_ARB);
end;

procedure glTexGenfv_Dual(coord, pname: GLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glTexGenfv(coord, pname, params);
  glActiveTexture(GL_TEXTURE1_ARB);
  glTexGenfv(coord, pname, params);
  glActiveTexture(GL_TEXTURE0_ARB);
end;

procedure glTexGeni_Dual(coord, pname: GLEnum; param: GLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glTexGeni(coord, pname, param);
  glActiveTexture(GL_TEXTURE1_ARB);
  glTexGeni(coord, pname, param);
  glActiveTexture(GL_TEXTURE0_ARB);
end;

procedure glTexGeniv_Dual(coord, pname: GLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glTexGeniv(coord, pname, params);
  glActiveTexture(GL_TEXTURE1_ARB);
  glTexGeniv(coord, pname, params);
  glActiveTexture(GL_TEXTURE0_ARB);
end;

procedure glEnable_Dual(cap: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glEnable(cap);
  glActiveTexture(GL_TEXTURE1_ARB);
  glEnable(cap);
  glActiveTexture(GL_TEXTURE0_ARB);
end;

procedure glDisable_Dual(cap: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glDisable(cap);
  glActiveTexture(GL_TEXTURE1);
  glDisable(cap);
  glActiveTexture(GL_TEXTURE0);
end;

procedure xglTexCoordPointer_Dual(size: GLint; atype: GLEnum; stride: GLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glTexCoordPointer(size, atype, stride, data);
  glClientActiveTexture(GL_TEXTURE1);
  glTexCoordPointer(size, atype, stride, data);
  glClientActiveTexture(GL_TEXTURE0);
end;

procedure xglEnableClientState_Dual(aarray: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glEnableClientState(aarray);
  glClientActiveTexture(GL_TEXTURE1);
  glEnableClientState(aarray);
  glClientActiveTexture(GL_TEXTURE0);
end;

procedure xglDisableClientState_Dual(aArray: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  glDisableClientState(aArray);
  glClientActiveTexture(GL_TEXTURE1);
  glDisableClientState(aArray);
  glClientActiveTexture(GL_TEXTURE0);
end;

// --------- Null Texturing

procedure glTexCoord2f_Null(s, t: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure glTexCoord2fv_Null(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure glTexCoord3f_Null(s, t, r: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure glTexCoord3fv_Null(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure glTexCoord4f_Null(s, t, r, q: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure glTexCoord4fv_Null(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure glTexGenf_Null(coord, pname: GLEnum; param: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure glTexGenfv_Null(coord, pname: GLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure glTexGeni_Null(coord, pname: GLEnum; param: GLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure glTexGeniv_Null(coord, pname: GLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure glEnable_Null(cap: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure glDisable_Null(cap: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure xglTexCoordPointer_Null(size: GLint; atype: GLEnum; stride: GLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure xglEnableClientState_Null(aArray: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

procedure xglDisableClientState_Null(aArray: GLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
end;

// ------------------------------------------------------------------
// Redirections management functions
// ------------------------------------------------------------------

procedure xglBeginUpdate;
begin
  if vUpdCount = 0 then
  begin
    vUpdCount := 1;
    vUpdNewMode := xglMapTexCoordMode;
  end
  else
    Inc(vUpdCount);
end;

procedure xglEndUpdate;
begin
  Dec(vUpdCount);
  if (vUpdCount = 0) and (vUpdNewMode <> xglMapTexCoordMode) then
  begin
    case vUpdNewMode of
      mtcmNull:        xglMapTexCoordToNull;
      mtcmMain:        xglMapTexCoordToMain;
      mtcmDual:        xglMapTexCoordToDual;
      mtcmSecond:      xglMapTexCoordToSecond;
      mtcmArbitrary:   xglMapTexCoordToArbitrary(vComplexMapping);
    else
      Assert(False);
    end;
  end;
end;

procedure xglPushState;
var
  i: Integer;
begin
  Assert(vUpdCount = 0);
  i := Length(vStateStack);
  SetLength(vStateStack, i + 1);
  vStateStack[i] := xglMapTexCoordMode;
end;

procedure xglPopState;
var
  i: Integer;
begin
  Assert(vUpdCount = 0);
  i := Length(vStateStack) - 1;
  Assert(i >= 0);
  case vStateStack[i] of
    mtcmNull:
      xglMapTexCoordToNull;
    mtcmMain:
      xglMapTexCoordToMain;
    mtcmDual:
      xglMapTexCoordToDual;
    mtcmSecond:
      xglMapTexCoordToSecond;
    mtcmArbitrary:
      xglMapTexCoordToArbitrary(vComplexMapping);
  else
    Assert(False);
  end;
  SetLength(vStateStack, i);
end;

procedure xglForbidSecondTextureUnit;
begin
  vSecondTextureUnitForbidden := True;
end;

procedure xglAllowSecondTextureUnit;
begin
  vSecondTextureUnitForbidden := False;
end;

procedure xglMapTexCoordToNull;
begin
  if vUpdCount <> 0 then
    vUpdNewMode := mtcmNull
  else if xglMapTexCoordMode <> mtcmNull then
  begin
    xglMapTexCoordMode := mtcmNull;

    xglTexCoord2f := glTexCoord2f_Null;
    xglTexCoord2fv := glTexCoord2fv_Null;
    xglTexCoord3f := glTexCoord3f_Null;
    xglTexCoord3fv := glTexCoord3fv_Null;
    xglTexCoord4f := glTexCoord4f_Null;
    xglTexCoord4fv := glTexCoord4fv_Null;

    xglTexGenf := glTexGenf_Null;
    xglTexGenfv := glTexGenfv_Null;
    xglTexGeni := glTexGeni_Null;
    xglTexGeniv := glTexGeniv_Null;

    xglTexCoordPointer := xglTexCoordPointer_Null;
    xglEnableClientState := xglEnableClientState_Null;
    xglDisableClientState := xglDisableClientState_Null;

    xglEnable := glEnable_Null;
    xglDisable := glDisable_Null;
  end;
end;

procedure xglMapTexCoordToMain;
begin
  if vUpdCount <> 0 then
    vUpdNewMode := mtcmMain
  else if xglMapTexCoordMode <> mtcmMain then
  begin
    xglMapTexCoordMode := mtcmMain;
    xglTexCoord2f := glTexCoord2f;
    xglTexCoord2fv := glTexCoord2fv;
    xglTexCoord3f := glTexCoord3f;
    xglTexCoord3fv := glTexCoord3fv;
    xglTexCoord4f := glTexCoord4f;
    xglTexCoord4fv := glTexCoord4fv;

    xglTexGenf := glTexGenf;
    xglTexGenfv := glTexGenfv;
    xglTexGeni := glTexGeni;
    xglTexGeniv := glTexGeniv;

    xglTexCoordPointer := glTexCoordPointer;
    xglEnableClientState := glEnableClientState;
    xglDisableClientState := glDisableClientState;

    xglEnable := glEnable;
    xglDisable := glDisable;
  end;
end;

procedure xglMapTexCoordToSecond;
begin
  if vSecondTextureUnitForbidden then
  begin
    xglMapTexCoordToNull;
    Exit;
  end;
  if vUpdCount <> 0 then
    vUpdNewMode := mtcmSecond
  else if xglMapTexCoordMode <> mtcmSecond then
  begin
    xglMapTexCoordMode := mtcmSecond;
    Assert(True, 'GL_ARB_multitexture');

    xglTexCoord2f := glTexCoord2f_Second;
    xglTexCoord2fv := glTexCoord2fv_Second;
    xglTexCoord3f := glTexCoord3f_Second;
    xglTexCoord3fv := glTexCoord3fv_Second;
    xglTexCoord4f := glTexCoord4f_Second;
    xglTexCoord4fv := glTexCoord4fv_Second;

    xglTexGenf := glTexGenf_Second;
    xglTexGenfv := glTexGenfv_Second;
    xglTexGeni := glTexGeni_Second;
    xglTexGeniv := glTexGeniv_Second;

    xglTexCoordPointer := xglTexCoordPointer_Second;
    xglEnableClientState := xglEnableClientState_Second;
    xglDisableClientState := xglDisableClientState_Second;

    xglEnable := glEnable_Second;
    xglDisable := glDisable_Second;
  end;
end;

procedure xglMapTexCoordToDual;
begin
  if vSecondTextureUnitForbidden then
  begin
    xglMapTexCoordToMain;
    Exit;
  end;
  if vUpdCount <> 0 then
    vUpdNewMode := mtcmDual
  else if xglMapTexCoordMode <> mtcmDual then
  begin
    xglMapTexCoordMode := mtcmDual;
    Assert(True, 'GL_ARB_multitexture');

    xglTexCoord2f := glTexCoord2f_Dual;
    xglTexCoord2fv := glTexCoord2fv_Dual;
    xglTexCoord3f := glTexCoord3f_Dual;
    xglTexCoord3fv := glTexCoord3fv_Dual;
    xglTexCoord4f := glTexCoord4f_Dual;
    xglTexCoord4fv := glTexCoord4fv_Dual;

    xglTexGenf := glTexGenf_Dual;
    xglTexGenfv := glTexGenfv_Dual;
    xglTexGeni := glTexGeni_Dual;
    xglTexGeniv := glTexGeniv_Dual;

    xglTexCoordPointer := xglTexCoordPointer_Dual;
    xglEnableClientState := xglEnableClientState_Dual;
    xglDisableClientState := xglDisableClientState_Dual;

    xglEnable := glEnable_Dual;
    xglDisable := glDisable_Dual;
  end;
end;

procedure xglMapTexCoordToArbitrary(const units: array of Cardinal);
var
  i, j, n: Integer;
begin
  n := Length(units);
  SetLength(vComplexMapping, n);
  j := 0;
  vComplexMappingN := n - 1;
  for i := 0 to vComplexMappingN do
  begin
    if (not vSecondTextureUnitForbidden) or (units[i] <> GL_TEXTURE1) then
    begin
      vComplexMapping[j] := units[i];
      Inc(j);
    end;
  end;

  if vUpdCount <> 0 then
    vUpdNewMode := mtcmArbitrary
  else if xglMapTexCoordMode <> mtcmArbitrary then
  begin

    xglMapTexCoordMode := mtcmArbitrary;
    Assert(True, 'ARB_multitexture');

    xglTexCoord2f := glTexCoord2f_Arbitrary;
    xglTexCoord2fv := glTexCoord2fv_Arbitrary;
    xglTexCoord3f := glTexCoord3f_Arbitrary;
    xglTexCoord3fv := glTexCoord3fv_Arbitrary;
    xglTexCoord4f := glTexCoord4f_Arbitrary;
    xglTexCoord4fv := glTexCoord4fv_Arbitrary;

    xglTexGenf := glTexGenf_Arbitrary;
    xglTexGenfv := glTexGenfv_Arbitrary;
    xglTexGeni := glTexGeni_Arbitrary;
    xglTexGeniv := glTexGeniv_Arbitrary;

    xglTexCoordPointer := xglTexCoordPointer_Arbitrary;
    xglEnableClientState := xglEnableClientState_Arbitrary;
    xglDisableClientState := xglDisableClientState_Arbitrary;

    xglEnable := glEnable_Arbitrary;
    xglDisable := glDisable_Arbitrary;
  end;
end;

procedure xglMapTexCoordToArbitrary(const bitWiseUnits: Cardinal);
var
  i, n: Integer;
  units: array of Cardinal;
begin
  n := 0;
  for i := 0 to 7 do
  begin
    if (bitWiseUnits and (1 shl i)) <> 0 then
      Inc(n);
  end;
  SetLength(units, n);
  n := 0;
  for i := 0 to 7 do
  begin
    if (bitWiseUnits and (1 shl i)) <> 0 then
    begin
      units[n] := GL_TEXTURE0 + i;
      Inc(n);
    end;
  end;
  xglMapTexCoordToArbitrary(units);
end;

procedure xglMapTexCoordToArbitraryAdd(const bitWiseUnits: Cardinal);
var
  n: Cardinal;
begin
  n := xglGetBitWiseMapping;
  xglMapTexCoordToArbitrary(n or bitWiseUnits);
end;

function xglGetBitWiseMapping: Cardinal;
var
  i, n: Cardinal;
  mode: TMapTexCoordMode;
begin
  if vUpdCount > 0 then
    mode := vUpdNewMode
  else
    mode := xglMapTexCoordMode;
  n := 0;
  case mode of
    mtcmMain:      n := 1;
    mtcmDual:      n := 3;
    mtcmSecond:    n := 2;
    mtcmArbitrary:     
	  begin
        for i := 0 to vComplexMappingN do
          n := n or (1 shl (vComplexMapping[i] - GL_TEXTURE0));
      end;
  else
    Assert(False);
  end;
  Result := n;
end;

// ------------------------------------------------------------------------
initialization
// ------------------------------------------------------------------------

xglMapTexCoordMode := mtcmUndefined;
xglMapTexCoordToNull;

end.
