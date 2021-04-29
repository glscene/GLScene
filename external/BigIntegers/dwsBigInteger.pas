{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsBigInteger;

{$I dws.inc}

interface

uses
   Classes, SysUtils, dwsUtils, gmp_lib;  // gmp_obj

type

   TBigInteger = class;

   IBigInteger = interface
      ['{C78B75B3-DF67-4E60-8B38-AD8719F175D4}']
      function Obj : TBigInteger;
      function Clone : TBigInteger;
   end;

   TBigInteger = class (TInterfacedObject, IBigInteger)
      private
         mpz : mpz_t;

         function Obj : TBigInteger;

      public
         constructor Create;
         destructor Destroy; override;

         function Clone : TBigInteger;
   end;

   BigInteger = record
      private
         FData : IBigInteger;

         procedure Allocate; inline;

      public
         procedure Assign(const v : Integer); overload; inline;
         procedure Assign(const v : Int64); overload; inline;

         function ToHexString : String;
         procedure FromHexString(const s : String);

         function ToFloat : Double;
(*
         class operator Explicit(const v : Int64) : BigInteger; inline;
*)
         class operator Add(const a, b : BigInteger) : BigInteger;
(*         class operator Add(const a : BigInteger; const b : Int64) : BigInteger;
         class operator Add(const a : Int64; const b : BigInteger) : BigInteger;
         class operator Subtract(const a, b : BigInteger) : BigInteger;
         class operator Subtract(const a : BigInteger; const b : Int64) : BigInteger;
         class operator Subtract(const a : Int64; const b : BigInteger) : BigInteger;

         class operator BitwiseAnd(const a, b : BigInteger) : BigInteger;
         class operator BitwiseOr(const a, b : BigInteger) : BigInteger;
         class operator BitwiseXor(const a, b : BigInteger) : BigInteger;
*)
         class operator Equal(const a, b : BigInteger) : Boolean;
         class operator NotEqual(const a, b : BigInteger) : Boolean; inline;
   end;

   EBigIntegerException = class (Exception);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TBigInteger ------------------
// ------------------

// Create
//
constructor TBigInteger.Create;
begin
   mpz_init(mpz);
end;

// Destroy
//
destructor TBigInteger.Destroy;
begin
   inherited;
   mpz_clear(mpz);
end;

// Obj
//
function TBigInteger.Obj : TBigInteger;
begin
   Result := Self;
end;

// Clone
//
function TBigInteger.Clone : TBigInteger;
begin
   Result := TBigInteger.Create;
   mpz_set(Result.mpz, mpz);
end;

// ------------------
// ------------------ BigInteger ------------------
// ------------------

// Allocate
//
procedure BigInteger.Allocate;
begin
   if FData=nil then FData := TBigInteger.Create;
end;

// Assign
//
procedure BigInteger.Assign(const v : Int32);
begin
   Allocate;
   mpz_set_si(FData.Obj.mpz, v);
end;

// Assign
//
procedure BigInteger.Assign(const v : Int64);
begin
   Allocate;
   mpz_set_int64(FData.Obj.mpz, v);
end;

// ToHexString
//
function BigInteger.ToHexString : String;
var
   n : Integer;
   obj : TBigInteger;
   buf : RawByteString;
begin
   if FData = nil then Exit('0');
   obj := FData.Obj;
   n := mpz_sizeinbase(obj.mpz, 16) + 1;
   SetLength(buf, n);
   mpz_get_str(PAnsiChar(buf), 16, obj.mpz);
   RawByteStringToScriptString(buf, Result);
end;

// FromHexString
//
procedure BigInteger.FromHexString(const s : String);
var
   i, len, err : Integer;
   nb32 : Integer;
   buf : RawByteString;
begin
   if s = '0' then begin
      FData := nil;
      Exit;
   end;
   ScriptStringToRawByteString(s, buf);
   Allocate;
   err := mpz_set_str(FData.Obj.mpz, PAnsiChar(s), 16);
   if err <> 0 then
      raise EBigIntegerException.CreateFmt('Conversion error from hexadecimal (%d)', [err]);
end;

// ToFloat
//
function BigInteger.ToFloat : Double;
begin
   if FData = nil then Exit(0);
   Result := mpz_get_d(FData.Obj.mpz);
end;

(*
// Explicit
//
class operator BigInteger.Explicit(const v : Int64) : BigInteger;
begin
   Result.Assign(v);
end;
*)
// Add
//
class operator BigInteger.Add(const a, b : BigInteger) : BigInteger;
begin
   if a.FData = nil then begin
      if b.FData = nil then
         Result.FData := nil
      else Result.FData := b.FData.Clone
   end else if b.FData = nil then
      Result.FData := a.FData.Clone
   else begin
      Result.Allocate;
      mpz_add(Result.FData.Obj.mpz, a.FData.Obj.mpz, b.FData.Obj.mpz);
   end;
end;
(*
// Add
//
class operator BigInteger.Add(const a : BigInteger; const b : Int64) : BigInteger;
begin
   Result := a + BigInteger(b);
end;

// Add
//
class operator BigInteger.Add(const a : Int64; const b : BigInteger) : BigInteger;
begin
   Result := BigInteger(a) + b;
end;

// Subtract
//
class operator BigInteger.Subtract(const a, b : BigInteger) : BigInteger;
begin
   Result.High := a.High - b.High;
   Result.Low := a.Low - b.Low;
   if Result.Low > a.Low then
      Dec(Result.High);
end;

// Subtract
//
class operator BigInteger.Subtract(const a : BigInteger; const b : Int64) : BigInteger;
begin
   Result := a - BigInteger(b);
end;

// Subtract
//
class operator BigInteger.Subtract(const a : Int64; const b : BigInteger) : BigInteger;
begin
   Result := BigInteger(a) - b;
end;

// BitwiseAnd
//
class operator BigInteger.BitwiseAnd(const a, b : BigInteger) : BigInteger;
begin
   Result.High := a.High and b.High;
   Result.Low := a.Low and b.Low;
end;

// BitwiseOr
//
class operator BigInteger.BitwiseOr(const a, b : BigInteger) : BigInteger;
begin
   Result.High := a.High or b.High;
   Result.Low := a.Low or b.Low;
end;

// BitwiseXor
//
class operator BigInteger.BitwiseXor(const a, b : BigInteger) : BigInteger;
begin
   Result.High := a.High xor b.High;
   Result.Low := a.Low xor b.Low;
end;
*)
// Equal
//
class operator BigInteger.Equal(const a, b : BigInteger) : Boolean;
var
   i, na : Integer;
begin
   mpz_cm
   Result:=(a.FHigh32=b.FHigh32);
   if Result then begin
      a.Pack;
      b.Pack;
      na:=Length(a.FData);
      if na = Length(b.FData) then begin
         for i := 0 to na-1 do begin
            if a.FData[i] <> b.FData[i] then begin
               Result := False;
               Break;
            end;
         end;
      end else Result := False;
   end;
end;

// NotEqual
//
class operator BigInteger.NotEqual(const a, b : BigInteger) : Boolean;
begin
   Result := not (a = b);
end;

end.
