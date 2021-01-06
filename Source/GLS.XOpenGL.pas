//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.XOpenGL;

(*
   "Alternate" OpenGL functions to handle multi-texturing.

   Using this functions allows specifying none/one/multiple ARB multi-texture
   coordinates with standard texture specification call.
   Before using any of the xglTexCoordXxxx fonctions, call one of the
   xglMapTexCoordToXxxx functions to establish the redirectors.
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGLext,

  GLS.VectorTypes,
  GLS.OpenGLTokens,
  GLS.State;

type
  TMapTexCoordMode = (mtcmUndefined, mtcmNull, mtcmMain, mtcmDual, mtcmSecond,
    mtcmArbitrary);

  TGLMultitextureCoordinator = class
  private
    FMapTexCoordMode: TMapTexCoordMode;
    FSecondTextureUnitForbidden: Boolean;
    FUpdCount: Integer;
    FUpdNewMode: TMapTexCoordMode;
    FStateStack: array of TMapTexCoordMode;
    FComplexMapping: array of Cardinal;
    FComplexMappingN: Integer;
  public
    // Explicit texture coordinates specification
    TexCoord2f: procedure(s, t: TGLfloat);stdcall;
    TexCoord2fv: procedure(v: PGLfloat);stdcall;
    TexCoord3f: procedure(s, t, r: TGLfloat);stdcall;
    TexCoord3fv: procedure(v: PGLfloat);stdcall;
    TexCoord4f: procedure(s, t, r, q: TGLfloat);stdcall;
    TexCoord4fv: procedure(v: PGLfloat);stdcall;

    // TexGen texture coordinates specification
    TexGenf: procedure(coord, pname: Cardinal; param: TGLfloat);stdcall;
    TexGenfv: procedure(coord, pname: Cardinal; params: PGLfloat);stdcall;
    TexGeni: procedure(coord, pname: Cardinal; param: TGLint);stdcall;
    TexGeniv: procedure(coord, pname: Cardinal; params: PGLint);stdcall;

    // Vertex Arrays texture coordinates specification
    TexCoordPointer: procedure(size: TGLint; atype: Cardinal; stride: TGLsizei; data: pointer);stdcall;
    EnableClientState: procedure(aarray: Cardinal);stdcall;
    DisableClientState: procedure(aarray: Cardinal);stdcall;
    // Misc
    Enable: procedure(cap: Cardinal);stdcall;
    Disable: procedure(cap: Cardinal);stdcall;
    constructor Create;
    // TexCoord functions will be ignored.
    procedure MapTexCoordToNull;
    // TexCoord functions will define the main texture coordinates.
    procedure MapTexCoordToMain;
    // TexCoord functions will define the second texture unit coordinates.
    procedure MapTexCoordToSecond;
    // TexCoord functions will define the two first texture units coordinates.
    procedure MapTexCoordToDual;
    // TexCoord functions will define the specified texture units coordinates.
    procedure MapTexCoordToArbitrary(const units: array of Cardinal); overload;
    procedure MapTexCoordToArbitrary(const bitWiseUnits: Cardinal); overload;
    procedure MapTexCoordToArbitraryAdd(const bitWiseUnits: Cardinal);
    (* Defers Map calls execution until EndUpdate is met.
       Calls to Begin/EndUpdate may be nested. *)
    procedure BeginUpdate; inline;
    (* Applies Map calls if there were any since BeginUpdate was invoked.
       Calls to Begin/EndUpdate may be nested. *)
    procedure EndUpdate; inline;
    // Saves XOpenGL State on the stack.
    procedure PushState;
    // Restores XOpenGL State from the stack.
    procedure PopState;
    (* Whenever called, 2nd texture units changes will be forbidden to .
       Use this function when you're using the 2nd texture unit for your own
       purposes and don't want XOpenGL to alter it. *)
    procedure ForbidSecondTextureUnit;
    // Allow XOpenGL to use the second texture unit again.
    procedure AllowSecondTextureUnit;
    // Returns the complex mapping in bitwise form.
    function GetBitWiseMapping: Cardinal;
    property MapTexCoordMode: TMapTexCoordMode read FMapTexCoordMode write FMapTexCoordMode;
    property SecondTextureUnitForbidden: Boolean read FSecondTextureUnitForbidden;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

uses
  GLS.Context;

// ------------------------------------------------------------------
// Multitexturing coordinates duplication functions
// ------------------------------------------------------------------

// --------- Complex (arbitrary) mapping

procedure TexCoord2f_Arbitrary(s, t: TGLfloat);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
    gl.MultiTexCoord2f(xgl.FComplexMapping[i], s, t);
end;

procedure TexCoord2fv_Arbitrary(v: PGLfloat);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
    gl.MultiTexCoord2fv(xgl.FComplexMapping[i], v);
end;

procedure TexCoord3f_Arbitrary(s, t, r: TGLfloat);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
    gl.MultiTexCoord3f(xgl.FComplexMapping[i], s, t, r);
end;

procedure TexCoord3fv_Arbitrary(v: PGLfloat);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
    gl.MultiTexCoord3fv(xgl.FComplexMapping[i], v);
end;

procedure TexCoord4f_Arbitrary(s, t, r, q: TGLfloat);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
    gl.MultiTexCoord4f(xgl.FComplexMapping[i], s, t, r, q);
end;

procedure TexCoord4fv_Arbitrary(v: PGLfloat);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
    gl.MultiTexCoord4fv(xgl.FComplexMapping[i], v);
end;

procedure TexGenf_Arbitrary(coord, pname: Cardinal; param: TGLfloat);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := xgl.FComplexMapping[i];
    gl.TexGenf(coord, pname, param);
  end;
end;

procedure TexGenfv_Arbitrary(coord, pname: Cardinal; params: PGLfloat);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := xgl.FComplexMapping[i];
    gl.TexGenfv(coord, pname, params);
  end;
end;

procedure TexGeni_Arbitrary(coord, pname: Cardinal; param: TGLint);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := xgl.FComplexMapping[i];
    gl.TexGeni(coord, pname, param);
  end;
end;

procedure TexGeniv_Arbitrary(coord, pname: Cardinal; params: PGLint);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := xgl.FComplexMapping[i];
    gl.TexGeniv(coord, pname, params);
  end;
end;

procedure Enable_Arbitrary(cap: Cardinal);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := xgl.FComplexMapping[i];
    gl.Enable(cap);
  end;
end;

procedure Disable_Arbitrary(cap: Cardinal);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := xgl.FComplexMapping[i];
    gl.Disable(cap);
  end;
end;

procedure TexCoordPointer_Arbitrary(size: TGLint; atype: Cardinal; stride: TGLsizei; data: pointer);
stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    gl.ClientActiveTexture(xgl.FComplexMapping[i]);
    gl.TexCoordPointer(size, atype, stride, data);
  end;
end;

procedure EnableClientState_Arbitrary(aArray: Cardinal);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    gl.ClientActiveTexture(xgl.FComplexMapping[i]);
    gl.EnableClientState(aArray);
  end;
end;

procedure DisableClientState_Arbitrary(aArray: Cardinal);stdcall;
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    gl.ClientActiveTexture(xgl.FComplexMapping[i]);
    gl.DisableClientState(aArray);
  end;
end;

// --------- Second unit Texturing

procedure TexCoord2f_Second(s, t: TGLfloat);stdcall;
begin
  gl.MultiTexCoord2f(GL_TEXTURE1, s, t);
end;

procedure TexCoord2fv_Second(v: PGLfloat);stdcall;
begin
  gl.MultiTexCoord2fv(GL_TEXTURE1, v);
end;

procedure TexCoord3f_Second(s, t, r: TGLfloat);stdcall;
begin
  gl.MultiTexCoord3f(GL_TEXTURE1, s, t, r);
end;

procedure TexCoord3fv_Second(v: PGLfloat);stdcall;
begin
  gl.MultiTexCoord3fv(GL_TEXTURE1, v);
end;

procedure TexCoord4f_Second(s, t, r, q: TGLfloat);stdcall;
begin
  gl.MultiTexCoord4f(GL_TEXTURE1, s, t, r, q);
end;

procedure TexCoord4fv_Second(v: PGLfloat);stdcall;
begin
  gl.MultiTexCoord4fv(GL_TEXTURE1, v);
end;

procedure TexGenf_Second(coord, pname: Cardinal; param: TGLfloat);stdcall;
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  gl.TexGenf(coord, pname, param);
end;

procedure TexGenfv_Second(coord, pname: Cardinal; params: PGLfloat);stdcall;
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  gl.TexGenfv(coord, pname, params);
end;

procedure TexGeni_Second(coord, pname: Cardinal; param: TGLint);stdcall;
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  gl.TexGeni(coord, pname, param);
end;

procedure TexGeniv_Second(coord, pname: Cardinal; params: PGLint);stdcall;
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  gl.TexGeniv(coord, pname, params);
end;

procedure Enable_Second(cap: Cardinal);stdcall;
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  gl.Enable(cap);
end;

procedure Disable_Second(cap: Cardinal);stdcall;
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  gl.Disable(cap);
end;

procedure TexCoordPointer_Second(size: TGLint; atype: Cardinal; stride:
  TGLsizei; data: pointer);stdcall;
begin
  gl.ClientActiveTexture(GL_TEXTURE1);
  gl.TexCoordPointer(size, atype, stride, data);
  gl.ClientActiveTexture(GL_TEXTURE0);
end;

procedure EnableClientState_Second(aArray: Cardinal);stdcall;
begin
  gl.ClientActiveTexture(GL_TEXTURE1);
  gl.EnableClientState(aArray);
  gl.ClientActiveTexture(GL_TEXTURE0);
end;

procedure DisableClientState_Second(aArray: Cardinal);stdcall;
begin
  gl.ClientActiveTexture(GL_TEXTURE1);
  gl.DisableClientState(aArray);
  gl.ClientActiveTexture(GL_TEXTURE0);
end;

// --------- Dual Texturing

procedure TexCoord2f_Dual(s, t: TGLfloat);stdcall;
begin
  gl.TexCoord2f(s, t);
  gl.MultiTexCoord2f(GL_TEXTURE1, s, t);
end;

procedure TexCoord2fv_Dual(v: PGLfloat);stdcall;
begin
  gl.TexCoord2fv(v);
  gl.MultiTexCoord2fv(GL_TEXTURE1, v);
end;

procedure TexCoord3f_Dual(s, t, r: TGLfloat);stdcall;
begin
  gl.TexCoord3f(s, t, r);
  gl.MultiTexCoord3f(GL_TEXTURE1, s, t, r);
end;

procedure TexCoord3fv_Dual(v: PGLfloat);stdcall;
begin
  gl.TexCoord3fv(v);
  gl.MultiTexCoord3fv(GL_TEXTURE1, v);
end;

procedure TexCoord4f_Dual(s, t, r, q: TGLfloat);stdcall;
begin
  gl.TexCoord4f(s, t, r, q);
  gl.MultiTexCoord4f(GL_TEXTURE1, s, t, r, q);
end;

procedure TexCoord4fv_Dual(v: PGLfloat);stdcall;
begin
  gl.TexCoord4fv(v);
  gl.MultiTexCoord4fv(GL_TEXTURE1, v);
end;

procedure TexGenf_Dual(coord, pname: Cardinal; param: TGLfloat);stdcall;
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    gl.TexGenf(coord, pname, param);
    ActiveTexture := 1;
    gl.TexGenf(coord, pname, param);
  end;
end;

procedure TexGenfv_Dual(coord, pname: Cardinal; params: PGLfloat);
stdcall;
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    gl.TexGenfv(coord, pname, params);
    ActiveTexture := 1;
    gl.TexGenfv(coord, pname, params);
  end;
end;

procedure TexGeni_Dual(coord, pname: Cardinal; param: TGLint);stdcall;
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    gl.TexGeni(coord, pname, param);
    ActiveTexture := 1;
    gl.TexGeni(coord, pname, param);
  end;
end;

procedure TexGeniv_Dual(coord, pname: Cardinal; params: PGLint);stdcall;
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    gl.TexGeniv(coord, pname, params);
    ActiveTexture := 1;
    gl.TexGeniv(coord, pname, params);
  end;
end;

procedure Enable_Dual(cap: Cardinal);stdcall;
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    gl.Enable(cap);
    ActiveTexture := 1;
    gl.Enable(cap);
  end;
end;

procedure Disable_Dual(cap: Cardinal);stdcall;
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    gl.Disable(cap);
    ActiveTexture := 1;
    gl.Disable(cap);
  end;
end;

procedure TexCoordPointer_Dual(size: TGLint; atype: Cardinal; stride:
  TGLsizei; data: pointer);stdcall;
begin
  gl.TexCoordPointer(size, atype, stride, data);
  gl.ClientActiveTexture(GL_TEXTURE1);
  gl.TexCoordPointer(size, atype, stride, data);
  gl.ClientActiveTexture(GL_TEXTURE0);
end;

procedure EnableClientState_Dual(aArray: Cardinal);stdcall;
begin
  gl.EnableClientState(aArray);
  gl.ClientActiveTexture(GL_TEXTURE1);
  gl.EnableClientState(aArray);
  gl.ClientActiveTexture(GL_TEXTURE0);
end;

procedure DisableClientState_Dual(aArray: Cardinal);stdcall;
begin
  gl.DisableClientState(aArray);
  gl.ClientActiveTexture(GL_TEXTURE1);
  gl.DisableClientState(aArray);
  gl.ClientActiveTexture(GL_TEXTURE0);
end;

// --------- Null Texturing

procedure TexCoord2f_Null(s, t: TGLfloat);stdcall;
begin
end;

procedure TexCoord2fv_Null(v: PGLfloat);stdcall;
begin
end;

procedure TexCoord3f_Null(s, t, r: TGLfloat);stdcall;
begin
end;

procedure TexCoord3fv_Null(v: PGLfloat);stdcall;
begin
end;

procedure TexCoord4f_Null(s, t, r, q: TGLfloat);stdcall;
begin
end;

procedure TexCoord4fv_Null(v: PGLfloat);stdcall;
begin
end;

procedure TexGenf_Null(coord, pname: Cardinal; param: TGLfloat);stdcall;
begin
end;

procedure TexGenfv_Null(coord, pname: Cardinal; params: PGLfloat);stdcall;
begin
end;

procedure TexGeni_Null(coord, pname: Cardinal; param: TGLint);stdcall;
begin
end;

procedure TexGeniv_Null(coord, pname: Cardinal; params: PGLint);stdcall;
begin
end;

procedure Enable_Null(cap: Cardinal);stdcall;
begin
end;

procedure Disable_Null(cap: Cardinal);stdcall;
begin
end;

procedure TexCoordPointer_Null(size: TGLint; atype: Cardinal; stride: TGLsizei; data: pointer);stdcall;
begin
end;

procedure EnableClientState_Null(aArray: Cardinal);stdcall;
begin
end;

procedure DisableClientState_Null(aArray: Cardinal);stdcall;
begin
end;

// ------------------------------------------------------------------
// Redirections management functions
// ------------------------------------------------------------------


procedure TGLMultitextureCoordinator.BeginUpdate;
begin
  if FUpdCount = 0 then
  begin
    FUpdCount := 1;
    FUpdNewMode := MapTexCoordMode;
  end
  else
    Inc(FUpdCount);
end;


procedure TGLMultitextureCoordinator.EndUpdate;
begin
  Dec(FUpdCount);
  if (FUpdCount = 0) and (FUpdNewMode <> MapTexCoordMode) then
  begin
    case FUpdNewMode of
      mtcmNull: MapTexCoordToNull;
      mtcmMain: MapTexCoordToMain;
      mtcmDual: MapTexCoordToDual;
      mtcmSecond: MapTexCoordToSecond;
      mtcmArbitrary: MapTexCoordToArbitrary(FComplexMapping);
    else
      Assert(False);
    end;
  end;
end;


procedure TGLMultitextureCoordinator.PushState;
var
  i: Integer;
begin
  Assert(FUpdCount = 0);
  i := Length(FStateStack);
  SetLength(FStateStack, i + 1);
  FStateStack[i] := MapTexCoordMode;
end;


procedure TGLMultitextureCoordinator.PopState;
var
  i: Integer;
begin
  Assert(FUpdCount = 0);
  i := Length(FStateStack) - 1;
  Assert(i >= 0);
  case FStateStack[i] of
    mtcmNull: MapTexCoordToNull;
    mtcmMain: MapTexCoordToMain;
    mtcmDual: MapTexCoordToDual;
    mtcmSecond: MapTexCoordToSecond;
    mtcmArbitrary: MapTexCoordToArbitrary(FComplexMapping);
  else
    Assert(False);
  end;
  SetLength(FStateStack, i);
end;


procedure TGLMultitextureCoordinator.ForbidSecondTextureUnit;
begin
  FSecondTextureUnitForbidden := True;
end;


procedure TGLMultitextureCoordinator.AllowSecondTextureUnit;
begin
  FSecondTextureUnitForbidden := False;
end;

constructor TGLMultitextureCoordinator.Create;
begin
  inherited;
  FMapTexCoordMode := mtcmUndefined;
  MapTexCoordToNull;
end;


procedure TGLMultitextureCoordinator.MapTexCoordToNull;
begin
  if FUpdCount <> 0 then
    FUpdNewMode := mtcmNull
  else if MapTexCoordMode <> mtcmNull then
  begin
    MapTexCoordMode := mtcmNull;

    TexCoord2f := TexCoord2f_Null;
    TexCoord2fv := TexCoord2fv_Null;
    TexCoord3f := TexCoord3f_Null;
    TexCoord3fv := TexCoord3fv_Null;
    TexCoord4f := TexCoord4f_Null;
    TexCoord4fv := TexCoord4fv_Null;

    TexGenf := TexGenf_Null;
    TexGenfv := TexGenfv_Null;
    TexGeni := TexGeni_Null;
    TexGeniv := TexGeniv_Null;

    TexCoordPointer := TexCoordPointer_Null;
    EnableClientState := EnableClientState_Null;
    DisableClientState := DisableClientState_Null;

    Enable := Enable_Null;
    Disable := Disable_Null;
  end;
end;


procedure TGLMultitextureCoordinator.MapTexCoordToMain;
begin
  if FUpdCount <> 0 then
    FUpdNewMode := mtcmMain
  else if MapTexCoordMode <> mtcmMain then
  begin
    MapTexCoordMode := mtcmMain;

    TexCoord2f := gl.TexCoord2f;
    TexCoord2fv := gl.TexCoord2fv;
    TexCoord3f := gl.TexCoord3f;
    TexCoord3fv := gl.TexCoord3fv;
    TexCoord4f := gl.TexCoord4f;
    TexCoord4fv := gl.TexCoord4fv;

    TexGenf := gl.TexGenf;
    TexGenfv := gl.TexGenfv;
    TexGeni := gl.TexGeni;
    TexGeniv := gl.TexGeniv;

    TexCoordPointer := gl.TexCoordPointer;
    EnableClientState := gl.EnableClientState;
    DisableClientState := gl.DisableClientState;

    Enable := gl.Enable;
    Disable := gl.Disable;
  end;
end;


procedure TGLMultitextureCoordinator.MapTexCoordToSecond;
begin
  if FSecondTextureUnitForbidden then
  begin
    MapTexCoordToNull;
    Exit;
  end;
  if FUpdCount <> 0 then
    FUpdNewMode := mtcmSecond
  else if MapTexCoordMode <> mtcmSecond then
  begin
    MapTexCoordMode := mtcmSecond;
    Assert(gl.ARB_multitexture);

    TexCoord2f := TexCoord2f_Second;
    TexCoord2fv := TexCoord2fv_Second;
    TexCoord3f := TexCoord3f_Second;
    TexCoord3fv := TexCoord3fv_Second;
    TexCoord4f := TexCoord4f_Second;
    TexCoord4fv := TexCoord4fv_Second;

    TexGenf := TexGenf_Second;
    TexGenfv := TexGenfv_Second;
    TexGeni := TexGeni_Second;
    TexGeniv := TexGeniv_Second;

    TexCoordPointer := TexCoordPointer_Second;
    EnableClientState := EnableClientState_Second;
    DisableClientState := DisableClientState_Second;

    Enable := Enable_Second;
    Disable := Disable_Second;
  end;
end;


procedure TGLMultitextureCoordinator.MapTexCoordToDual;
begin
  if FSecondTextureUnitForbidden then
  begin
    MapTexCoordToMain;
    Exit;
  end;
  if FUpdCount <> 0 then
    FUpdNewMode := mtcmDual
  else if MapTexCoordMode <> mtcmDual then
  begin
    MapTexCoordMode := mtcmDual;
    Assert(GL.ARB_multitexture);

    TexCoord2f := TexCoord2f_Dual;
    TexCoord2fv := TexCoord2fv_Dual;
    TexCoord3f := TexCoord3f_Dual;
    TexCoord3fv := TexCoord3fv_Dual;
    TexCoord4f := TexCoord4f_Dual;
    TexCoord4fv := TexCoord4fv_Dual;

    TexGenf := TexGenf_Dual;
    TexGenfv := TexGenfv_Dual;
    TexGeni := TexGeni_Dual;
    TexGeniv := TexGeniv_Dual;

    TexCoordPointer := TexCoordPointer_Dual;
    EnableClientState := EnableClientState_Dual;
    DisableClientState := DisableClientState_Dual;

    Enable := Enable_Dual;
    Disable := Disable_Dual;
  end;
end;


procedure TGLMultitextureCoordinator.MapTexCoordToArbitrary(const units: array of Cardinal);
var
  i, j, n: Integer;
begin
  n := Length(units);
  SetLength(FComplexMapping, n);
  j := 0;
  FComplexMappingN := n - 1;
  for i := 0 to FComplexMappingN do
  begin
    if (not FSecondTextureUnitForbidden) or (units[i] <> GL_TEXTURE1) then
    begin
      FComplexMapping[j] := units[i];
      Inc(j);
    end;
  end;

  if FUpdCount <> 0 then
    FUpdNewMode := mtcmArbitrary
  else if MapTexCoordMode <> mtcmArbitrary then
  begin

    MapTexCoordMode := mtcmArbitrary;
    Assert(GL.ARB_multitexture);

    TexCoord2f := TexCoord2f_Arbitrary;
    TexCoord2fv := TexCoord2fv_Arbitrary;
    TexCoord3f := TexCoord3f_Arbitrary;
    TexCoord3fv := TexCoord3fv_Arbitrary;
    TexCoord4f := TexCoord4f_Arbitrary;
    TexCoord4fv := TexCoord4fv_Arbitrary;

    TexGenf := TexGenf_Arbitrary;
    TexGenfv := TexGenfv_Arbitrary;
    TexGeni := TexGeni_Arbitrary;
    TexGeniv := TexGeniv_Arbitrary;

    TexCoordPointer := TexCoordPointer_Arbitrary;
    EnableClientState := EnableClientState_Arbitrary;
    DisableClientState := DisableClientState_Arbitrary;

    Enable := Enable_Arbitrary;
    Disable := Disable_Arbitrary;
  end;
end;


procedure TGLMultitextureCoordinator.MapTexCoordToArbitrary(const bitWiseUnits: Cardinal);
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
  MapTexCoordToArbitrary(units);
end;


procedure TGLMultitextureCoordinator.MapTexCoordToArbitraryAdd(const bitWiseUnits: Cardinal);
var
  n: Cardinal;
begin
  n := GetBitWiseMapping;
  MapTexCoordToArbitrary(n or bitWiseUnits);
end;


function TGLMultitextureCoordinator.GetBitWiseMapping: Cardinal;
var
  i, n: Cardinal;
  mode: TMapTexCoordMode;
begin
  if FUpdCount > 0 then
    mode := FUpdNewMode
  else
    mode := MapTexCoordMode;
  n := 0;
  case mode of
    mtcmMain: n := 1;
    mtcmDual: n := 3;
    mtcmSecond: n := 2;
    mtcmArbitrary:
      begin
        for i := 0 to FComplexMappingN do
          n := n or (1 shl (FComplexMapping[i] - GL_TEXTURE0));
      end;
  else
    Assert(False);
  end;
  Result := n;
end;

// ------------------------------------------------------------------------
initialization
// ------------------------------------------------------------------------

end.
