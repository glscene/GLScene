(* *****************************************************************************
  *                               PasDblStrUtils                               *
  ******************************************************************************
  *                        Version 2021-06-21-01-13-0000                       *
  ******************************************************************************
  *                                zlib license                                *
  *============================================================================*
  *                                                                            *
  * Copyright (C) 2016-2021, Benjamin Rosseaux (benjamin@rosseaux.de)          *
  *                                                                            *
  * This software is provided 'as-is', without any express or implied          *
  * warranty. In no event will the authors be held liable for any damages      *
  * arising from the use of this software.                                     *
  *                                                                            *
  * Permission is granted to anyone to use this software for any purpose,      *
  * including commercial applications, and to alter it and redistribute it     *
  * freely, subject to the following restrictions:                             *
  *                                                                            *
  * 1. The origin of this software must not be misrepresented; you must not    *
  *    claim that you wrote the original software. If you use this software    *
  *    in a product, an acknowledgement in the product documentation would be  *
  *    appreciated but is not required.                                        *
  * 2. Altered source versions must be plainly marked as such, and must not be *
  *    misrepresented as being the original software.                          *
  * 3. This notice may not be removed or altered from any source distribution. *
  *                                                                            *
  ******************************************************************************
  *                  General guidelines for code contributors                  *
  *============================================================================*
  *                                                                            *
  * 1. Make sure you are legally allowed to make a contribution under the zlib *
  *    license.                                                                *
  * 2. The zlib license header goes at the top of each source file, with       *
  *    appropriate copyright notice.                                           *
  * 3. After a pull request, check the status of your pull request on          *
  http://github.com/BeRo1985/pasdblstrutils                               *
  * 4. Write code, which is compatible with Delphi >=XE7 and FreePascal        *
  *    >= 3.0                                                                  *
  * 5. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
  *    but if needed, make it out-ifdef-able.                                  *
  * 6. No use of third-party libraries/units as possible, but if needed, make  *
  *    it out-ifdef-able.                                                      *
  * 7. Try to use const when possible.                                         *
  * 8. Make sure to comment out writeln, used while debugging.                 *
  * 9. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,     *
  *    x86-64, ARM, ARM64, etc.).                                              *
  * 10. Make sure the code runs on platforms with weak and strong memory       *
  *     models without any issues.                                             *
  *                                                                            *
  ***************************************************************************** *)

unit PasDblStrUtils;

{$ifdef fpc}
{$mode delphi}
{$ifdef cpui386}
{$define cpu386}
{$endif}
{$ifdef cpu386}
{$asmmode intel}
{$endif}
{$ifdef cpuamd64}
{$asmmode intel}
{$define cpux86_64}
{$define cpux64}
{$endif}
{$ifdef FPC_LITTLE_ENDIAN}
{$define LITTLE_ENDIAN}
{$else}
{$ifdef FPC_BIG_ENDIAN}
{$define BIG_ENDIAN}
{$endif}
{$endif}
{ -$pic off }
{$define CanInline}
{$ifdef FPC_HAS_TYPE_EXTENDED}
{$define HAS_TYPE_EXTENDED}
{$else}
{$undef HAS_TYPE_EXTENDED}
{$endif}
{$ifdef FPC_HAS_TYPE_DOUBLE}
{$define HAS_TYPE_DOUBLE}
{$else}
{$undef HAS_TYPE_DOUBLE}
{$endif}
{$ifdef FPC_HAS_TYPE_SINGLE}
{$define HAS_TYPE_SINGLE}
{$else}
{$undef HAS_TYPE_SINGLE}
{$endif}
{$if declared(RawByteString)}
{$define HAS_TYPE_RAWBYTESTRING}
{$else}
{$undef HAS_TYPE_RAWBYTESTRING}
{$ifend}
{$if declared(UTF8String)}
{$define HAS_TYPE_UTF8STRING}
{$else}
{$undef HAS_TYPE_UTF8STRING}
{$ifend}
{$if declared(UnicodeString)}
{$define HAS_TYPE_UNICODESTRING}
{$else}
{$undef HAS_TYPE_UNICODESTRING}
{$ifend}
{$else}
{$realcompatibility off}
{$localsymbols on}
{$define LITTLE_ENDIAN}
{$ifndef cpu64}
{$define cpu32}
{$endif}
{$ifdef cpux64}
{$define cpuamd64}
{$define cpux86_64}
{$endif}
{$define HAS_TYPE_EXTENDED}
{$define HAS_TYPE_DOUBLE}
{$define HAS_TYPE_SINGLE}
{$ifdef conditionalexpressions}
{$if declared(RawByteString)}
{$define HAS_TYPE_RAWBYTESTRING}
{$else}
{$undef HAS_TYPE_RAWBYTESTRING}
{$ifend}
{$if declared(UTF8String)}
{$define HAS_TYPE_UTF8STRING}
{$else}
{$undef HAS_TYPE_UTF8STRING}
{$ifend}
{$if declared(UnicodeString)}
{$define HAS_TYPE_UNICODESTRING}
{$else}
{$undef HAS_TYPE_UNICODESTRING}
{$ifend}
{$else}
{$undef HAS_TYPE_RAWBYTESTRING}
{$undef HAS_TYPE_UTF8STRING}
{$undef HAS_TYPE_UNICODESTRING}
{$endif}
{$ifndef BCB}
{$ifdef ver120}
{$define Delphi4or5}
{$endif}
{$ifdef ver130}
{$define Delphi4or5}
{$endif}
{$ifdef ver140}
{$define Delphi6}
{$endif}
{$ifdef ver150}
{$define Delphi7}
{$endif}
{$ifdef ver170}
{$define Delphi2005}
{$endif}
{$else}
{$ifdef ver120}
{$define Delphi4or5}
{$define BCB4}
{$endif}
{$ifdef ver130}
{$define Delphi4or5}
{$endif}
{$endif}
{$ifdef conditionalexpressions}
{$if CompilerVersion>=24}
{$legacyifend on}
{$ifend}
{$if CompilerVersion>=14.0}
{$if CompilerVersion=14.0}
{$define Delphi6}
{$ifend}
{$define Delphi6AndUp}
{$ifend}
{$if CompilerVersion>=15.0}
{$if CompilerVersion=15.0}
{$define Delphi7}
{$ifend}
{$define Delphi7AndUp}
{$ifend}
{$if CompilerVersion>=17.0}
{$if CompilerVersion=17.0}
{$define Delphi2005}
{$ifend}
{$define Delphi2005AndUp}
{$ifend}
{$if CompilerVersion>=18.0}
{$if CompilerVersion=18.0}
{$define BDS2006}
{$define Delphi2006}
{$ifend}
{$define Delphi2006AndUp}
{$ifend}
{$if CompilerVersion>=18.5}
{$if CompilerVersion=18.5}
{$define Delphi2007}
{$ifend}
{$define Delphi2007AndUp}
{$ifend}
{$if CompilerVersion=19.0}
{$define Delphi2007Net}
{$ifend}
{$if CompilerVersion>=20.0}
{$if CompilerVersion=20.0}
{$define Delphi2009}
{$ifend}
{$define Delphi2009AndUp}
{$ifend}
{$if CompilerVersion>=21.0}
{$if CompilerVersion=21.0}
{$define Delphi2010}
{$ifend}
{$define Delphi2010AndUp}
{$ifend}
{$if CompilerVersion>=22.0}
{$if CompilerVersion=22.0}
{$define DelphiXE}
{$ifend}
{$define DelphiXEAndUp}
{$ifend}
{$if CompilerVersion>=23.0}
{$if CompilerVersion=23.0}
{$define DelphiXE2}
{$ifend}
{$define DelphiXE2AndUp}
{$ifend}
{$if CompilerVersion>=24.0}
{$if CompilerVersion=24.0}
{$define DelphiXE3}
{$ifend}
{$define DelphiXE3AndUp}
{$ifend}
{$if CompilerVersion>=25.0}
{$if CompilerVersion=25.0}
{$define DelphiXE4}
{$ifend}
{$define DelphiXE4AndUp}
{$ifend}
{$if CompilerVersion>=26.0}
{$if CompilerVersion=26.0}
{$define DelphiXE5}
{$ifend}
{$define DelphiXE5AndUp}
{$ifend}
{$if CompilerVersion>=27.0}
{$if CompilerVersion=27.0}
{$define DelphiXE6}
{$ifend}
{$define DelphiXE6AndUp}
{$ifend}
{$if CompilerVersion>=28.0}
{$if CompilerVersion=28.0}
{$define DelphiXE7}
{$ifend}
{$define DelphiXE7AndUp}
{$ifend}
{$if CompilerVersion>=29.0}
{$if CompilerVersion=29.0}
{$define DelphiXE8}
{$ifend}
{$define DelphiXE8AndUp}
{$ifend}
{$if CompilerVersion>=30.0}
{$if CompilerVersion=30.0}
{$define Delphi10Seattle}
{$ifend}
{$define Delphi10SeattleAndUp}
{$ifend}
{$if CompilerVersion>=31.0}
{$if CompilerVersion=31.0}
{$define Delphi10Berlin}
{$ifend}
{$define Delphi10BerlinAndUp}
{$ifend}
{$endif}
{$ifndef Delphi4or5}
{$ifndef BCB}
{$define Delphi6AndUp}
{$endif}
{$ifndef Delphi6}
{$define BCB6OrDelphi7AndUp}
{$ifndef BCB}
{$define Delphi7AndUp}
{$endif}
{$ifndef BCB}
{$ifndef Delphi7}
{$ifndef Delphi2005}
{$define BDS2006AndUp}
{$endif}
{$endif}
{$endif}
{$endif}
{$endif}
{$ifdef Delphi6AndUp}
{$warn symbol_platform off}
{$warn symbol_deprecated off}
{$endif}
{$endif}
{$ifdef win32}
{$define windows}
{$endif}
{$ifdef win64}
{$define windows}
{$endif}
{$ifdef wince}
{$define windows}
{$endif}
{$ifndef HAS_TYPE_DOUBLE}
{$error No double floating point precision}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst off}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}
{$ifdef fpc}
{$optimization level1}
{$endif}

interface

uses
  System.SysUtils,
  System.Math;

type
  PPasDblStrUtilsInt8 = ^TPasDblStrUtilsInt8;
  TPasDblStrUtilsInt8 = {$IFDEF fpc}Int8{$ELSE}ShortInt{$ENDIF};

  PPasDblStrUtilsUInt8 = ^TPasDblStrUtilsUInt8;
  TPasDblStrUtilsUInt8 = {$IFDEF fpc}UInt8{$ELSE}Byte{$ENDIF};

  PPasDblStrUtilsInt16 = ^TPasDblStrUtilsInt16;
  TPasDblStrUtilsInt16 = {$IFDEF fpc}Int16{$ELSE}SmallInt{$ENDIF};

  PPasDblStrUtilsUInt16 = ^TPasDblStrUtilsUInt16;
  TPasDblStrUtilsUInt16 = {$IFDEF fpc}UInt16{$ELSE}Word{$ENDIF};

  PPasDblStrUtilsInt32 = ^TPasDblStrUtilsInt32;
  TPasDblStrUtilsInt32 = {$IFDEF fpc}Int32{$ELSE}LongInt{$ENDIF};

  PPasDblStrUtilsUInt32 = ^TPasDblStrUtilsUInt32;
  TPasDblStrUtilsUInt32 = {$IFDEF fpc}UInt32{$ELSE}LongWord{$ENDIF};

  PPasDblStrUtilsInt64 = ^TPasDblStrUtilsInt64;
  TPasDblStrUtilsInt64 = Int64;

  PPasDblStrUtilsUInt64 = ^TPasDblStrUtilsUInt64;
  TPasDblStrUtilsUInt64 = UInt64;

  PPasDblStrUtilsDouble = ^TPasDblStrUtilsDouble;
  TPasDblStrUtilsDouble = Double;

  PPasDblStrUtilsBoolean = ^TPasDblStrUtilsBoolean;
  TPasDblStrUtilsBoolean = Boolean;

  PPasDblStrUtilsPtrUInt = ^TPasDblStrUtilsPtrUInt;
  PPasDblStrUtilsPtrInt = ^TPasDblStrUtilsPtrInt;

{$IFDEF fpc}
  TPasDblStrUtilsPtrUInt = PtrUInt;
  TPasDblStrUtilsPtrInt = PtrInt;
{$ELSE}
{$IF Declared(CompilerVersion) and (CompilerVersion>=23.0)}
  TPasDblStrUtilsPtrUInt = NativeUInt;
  TPasDblStrUtilsPtrInt = NativeInt;
{$ELSE}
{$IFDEF cpu64}
  TPasDblStrUtilsPtrUInt = TPasDblStrUtilsUInt64;
  TPasDblStrUtilsPtrInt = TPasDblStrUtilsInt64;
{$ELSE}
  TPasDblStrUtilsPtrUInt = TPasDblStrUtilsUInt32;
  TPasDblStrUtilsPtrInt = TPasDblStrUtilsInt32;
{$ENDIF}
{$IFEND}
{$ENDIF}
  PPasDblStrUtilsNativeUInt = ^TPasDblStrUtilsNativeUInt;
  PPasDblStrUtilsNativeInt = ^TPasDblStrUtilsNativeInt;
  TPasDblStrUtilsNativeUInt = TPasDblStrUtilsPtrUInt;
  TPasDblStrUtilsNativeInt = TPasDblStrUtilsPtrInt;

  PPasDblStrUtilsRawByteChar = PAnsiChar;
  TPasDblStrUtilsRawByteChar = AnsiChar;

  PPasDblStrUtilsRawByteCharSet = ^TPasDblStrUtilsRawByteCharSet;
  TPasDblStrUtilsRawByteCharSet = set of TPasDblStrUtilsRawByteChar;

  PPasDblStrUtilsRawByteString = ^TPasDblStrUtilsRawByteString;
  TPasDblStrUtilsRawByteString =
  {$IFDEF HAS_TYPE_RAWBYTESTRING}RawByteString{$ELSE}AnsiString{$ENDIF};

  PPasDblStrUtilsUTF8Char = PAnsiChar;
  TPasDblStrUtilsUTF8Char = AnsiChar;

  PPasDblStrUtilsUTF8String = ^TPasDblStrUtilsUTF8String;
  TPasDblStrUtilsUTF8String =
  {$IFDEF HAS_TYPE_UTF8STRING}UTF8String{$ELSE}AnsiString{$ENDIF};

  PPasDblStrUtilsUTF16Char =
  {$IFDEF HAS_TYPE_UNICODESTRING}{$IFDEF fpc}PUnicodeChar{$ELSE}PWideChar{$ENDIF}{$ELSE}PWideChar{$ENDIF};
  TPasDblStrUtilsUTF16Char =
  {$IFDEF HAS_TYPE_UNICODESTRING}{$IFDEF fpc}UnicodeChar{$ELSE}WideChar{$ENDIF}{$ELSE}WideChar{$ENDIF};

  PPasDblStrUtilsUTF16String = ^TPasDblStrUtilsUTF16String;
  TPasDblStrUtilsUTF16String =
  {$IFDEF HAS_TYPE_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF};

  PPasDblStrUtilsChar = PAnsiChar;
  TPasDblStrUtilsChar = AnsiChar;

  PPasDblStrUtilsString = ^TPasDblStrUtilsString;
  TPasDblStrUtilsString =
  {$IFDEF HAS_TYPE_RAWBYTESTRING}RawByteString{$ELSE}AnsiString{$ENDIF};

  PPasDblStrUtilsPointer = ^TPasDblStrUtilsPointer;
  TPasDblStrUtilsPointer = Pointer;

  PPasDblStrUtilsRoundingMode = ^TPasDblStrUtilsRoundingMode;
  TPasDblStrUtilsRoundingMode = type TFPURoundingMode;

  TPasDblStrUtilsOutputMode = (omStandard, omStandardExponential, omFixed,
    omExponential, omPrecision, omRadix);

function FallbackStringToDouble(const aStringValue: PPasDblStrUtilsChar;
  const aStringLength: TPasDblStrUtilsInt32;
  const aRoundingMode: TPasDblStrUtilsRoundingMode = rmNearest;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aBase: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsDouble; overload;
function FallbackStringToDouble(const aStringValue: TPasDblStrUtilsString;
  const aRoundingMode: TPasDblStrUtilsRoundingMode = rmNearest;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aBase: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsDouble; overload;

function AlgorithmMStringToDouble(const aStringValue: PPasDblStrUtilsChar;
  const aStringLength: TPasDblStrUtilsInt32;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aBase: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsDouble; overload;
function AlgorithmMStringToDouble(const aStringValue: TPasDblStrUtilsString;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aBase: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsDouble; overload;

function EiselLemireStringToDouble(const aStringValue: PPasDblStrUtilsChar;
  const aStringLength: TPasDblStrUtilsInt32;
  const aOK: PPasDblStrUtilsBoolean = nil): TPasDblStrUtilsDouble; overload;
function EiselLemireStringToDouble(const aStringValue: TPasDblStrUtilsString;
  const aOK: PPasDblStrUtilsBoolean = nil): TPasDblStrUtilsDouble; overload;

function RyuStringToDouble(const aStringValue: PPasDblStrUtilsChar;
  const aStringLength: TPasDblStrUtilsInt32;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aCountDigits: PPasDblStrUtilsInt32 = nil)
  : TPasDblStrUtilsDouble; overload;
function RyuStringToDouble(const aStringValue: TPasDblStrUtilsString;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aCountDigits: PPasDblStrUtilsInt32 = nil)
  : TPasDblStrUtilsDouble; overload;

function RyuDoubleToString(const aValue: TPasDblStrUtilsDouble;
  const aExponential: Boolean = true): TPasDblStrUtilsString;

function ConvertStringToDouble(const aStringValue: PPasDblStrUtilsChar;
  const aStringLength: TPasDblStrUtilsInt32;
  const aRoundingMode: TPasDblStrUtilsRoundingMode = rmNearest;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aBase: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsDouble; overload;
function ConvertStringToDouble(const aStringValue: TPasDblStrUtilsString;
  const aRoundingMode: TPasDblStrUtilsRoundingMode = rmNearest;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aBase: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsDouble; overload;

function ConvertDoubleToString(const aValue: TPasDblStrUtilsDouble;
  const aOutputMode: TPasDblStrUtilsOutputMode = omStandard;
  aRequestedDigits: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsString;

implementation

type
  PDoubleHiLo = ^TDoubleHiLo;

  TDoubleHiLo = packed record
{$IFDEF BIG_ENDIAN}
    Hi, Lo: TPasDblStrUtilsUInt32;
{$ELSE}
    Lo, Hi: TPasDblStrUtilsUInt32;
{$ENDIF}
  end;

  PDoubleBytes = ^TDoubleBytes;
  TDoubleBytes = array [0 .. sizeof(TPasDblStrUtilsDouble) - 1]
    of TPasDblStrUtilsUInt8;

{$IFDEF cpu64}

function IsNaN(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := ((PPasDblStrUtilsInt64(@aValue)^ and $7FF0000000000000)
    = $7FF0000000000000) and
    ((PPasDblStrUtilsInt64(@aValue)^ and $000FFFFFFFFFFFFF) <>
    $0000000000000000);
end;

function IsInfinite(const aValue: TPasDblStrUtilsDouble)
  : TPasDblStrUtilsBoolean; {$IFDEF caninline}inline; {$ENDIF}
begin
  result := (PPasDblStrUtilsInt64(@aValue)^ and $7FFFFFFFFFFFFFFF)
    = $7FF0000000000000;
end;

function IsFinite(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := (PPasDblStrUtilsInt64(@aValue)^ and $7FF0000000000000) <>
    $7FF0000000000000;
end;

function IsPosInfinite(const aValue: TPasDblStrUtilsDouble)
  : TPasDblStrUtilsBoolean; {$IFDEF caninline}inline; {$ENDIF}
begin
  result := PPasDblStrUtilsInt64(@aValue)^ = TPasDblStrUtilsInt64
    ($7FF0000000000000);
end;

function IsNegInfinite(const aValue: TPasDblStrUtilsDouble)
  : TPasDblStrUtilsBoolean; {$IFDEF caninline}inline; {$ENDIF}
begin
{$IFDEF fpc}
  result := TPasDblStrUtilsUInt64(Pointer(@aValue)^) = TPasDblStrUtilsUInt64
    ($FFF0000000000000);
{$ELSE}
  result := PPasDblStrUtilsInt64(@aValue)^ = TPasDblStrUtilsInt64
    ($FFF0000000000000);
{$ENDIF}
end;

function IsPosZero(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := PPasDblStrUtilsInt64(@aValue)^ = TPasDblStrUtilsInt64
    ($0000000000000000);
end;

function IsNegZero(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
begin
{$IFDEF fpc}
  result := TPasDblStrUtilsUInt64(Pointer(@aValue)^) = TPasDblStrUtilsUInt64
    ($8000000000000000);
{$ELSE}
  result := PPasDblStrUtilsInt64(@aValue)^ = TPasDblStrUtilsInt64
    ($8000000000000000);
{$ENDIF}
end;

function IsZero(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
begin
{$IFDEF fpc}
  result := (TPasDblStrUtilsUInt64(Pointer(@aValue)^) and
    TPasDblStrUtilsUInt64($7FFFFFFFFFFFFFFF)) = TPasDblStrUtilsUInt64
    ($0000000000000000);
{$ELSE}
  result := (PPasDblStrUtilsInt64(@aValue)^ and
    TPasDblStrUtilsInt64($7FFFFFFFFFFFFFFF)) = TPasDblStrUtilsInt64
    ($0000000000000000);
{$ENDIF}
end;

function IsNegative(const aValue: TPasDblStrUtilsDouble)
  : TPasDblStrUtilsBoolean; {$IFDEF caninline}inline; {$ENDIF}
begin
{$IFDEF fpc}
  result := (TPasDblStrUtilsUInt64(Pointer(@aValue)^) and
    TPasDblStrUtilsUInt64($8000000000000000)) <> 0;
{$ELSE}
  result := (PPasDblStrUtilsInt64(@aValue)^ shr 63) <> 0;
{$ENDIF}
end;
{$ELSE}
{$IFDEF TrickyNumberChecks}

function IsNaN(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
var
  l: TPasDblStrUtilsUInt32;
begin
  l := PDoubleHiLo(@aValue)^.Lo;
  result := (TPasDblStrUtilsUInt32($7FF00000 - TPasDblStrUtilsUInt32
    (TPasDblStrUtilsUInt32(PDoubleHiLo(@aValue)^.Hi and $7FFFFFFF) or
    ((l or (-l)) shr 31))) shr 31) <> 0;
end;

function IsInfinite(const aValue: TPasDblStrUtilsDouble)
  : TPasDblStrUtilsBoolean; {$IFDEF caninline}inline; {$ENDIF}
begin
  result := TPasDblStrUtilsUInt32
    ((TPasDblStrUtilsUInt32(PDoubleHiLo(@aValue)^.Hi and $7FFFFFFF)
    xor $7FF00000) or PDoubleHiLo(@aValue)^.Lo) = 0;
end;

function IsFinite(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := (TPasDblStrUtilsUInt32((PDoubleHiLo(@aValue)^.Hi and $7FFFFFFF) -
    $7FF00000) shr 31) <> 0;
end;

function IsPosInfinite(const aValue: TPasDblStrUtilsDouble)
  : TPasDblStrUtilsBoolean; {$IFDEF caninline}inline; {$ENDIF}
var
  h: TPasDblStrUtilsUInt32;
begin
  h := PDoubleHiLo(@aValue)^.Hi;
  result := TPasDblStrUtilsUInt32
    (((TPasDblStrUtilsUInt32(h and $7FFFFFFF) xor $7FF00000) or
    PDoubleHiLo(@aValue)^.Lo) or TPasDblStrUtilsUInt32(h shr 31)) = 0;
end;

function IsNegInfinite(const aValue: TPasDblStrUtilsDouble)
  : TPasDblStrUtilsBoolean; {$IFDEF caninline}inline; {$ENDIF}
var
  h: TPasDblStrUtilsUInt32;
begin
  h := PDoubleHiLo(@aValue)^.Hi;
  result := TPasDblStrUtilsUInt32
    (((TPasDblStrUtilsUInt32(h and $7FFFFFFF) xor $7FF00000) or
    PDoubleHiLo(@aValue)^.Lo) or TPasDblStrUtilsUInt32
    (TPasDblStrUtilsUInt32(not h) shr 31)) = 0;
end;

function IsPosZero(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
var
  h: TPasDblStrUtilsUInt32;
begin
  h := PDoubleHiLo(@aValue)^.Hi;
  result := TPasDblStrUtilsUInt32
    (TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(h and $7FFFFFFF) or
    PDoubleHiLo(@aValue)^.Lo) or TPasDblStrUtilsUInt32(h shr 31)) = 0;
end;

function IsNegZero(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
var
  h: TPasDblStrUtilsUInt32;
begin
  h := PDoubleHiLo(@aValue)^.Hi;
  result := TPasDblStrUtilsUInt32
    (TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(h and $7FFFFFFF) or
    PDoubleHiLo(@aValue)^.Lo) or TPasDblStrUtilsUInt32
    (TPasDblStrUtilsUInt32(not h) shr 31)) = 0;
end;

function IsZero(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(PDoubleHiLo(@aValue)
    ^.Hi and $7FFFFFFF) or PDoubleHiLo(@aValue)^.Lo) = 0;
end;

function IsNegative(const aValue: TPasDblStrUtilsDouble)
  : TPasDblStrUtilsBoolean; {$IFDEF caninline}inline; {$ENDIF}
begin
  result := TPasDblStrUtilsUInt32(PDoubleHiLo(@aValue)^.Hi and
    TPasDblStrUtilsUInt32($80000000)) <> 0;
end;
{$ELSE}

function IsNaN(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := ((PDoubleHiLo(@aValue)^.Hi and $7FF00000) = $7FF00000) and
    (((PDoubleHiLo(@aValue)^.Hi and $000FFFFF) or PDoubleHiLo(@aValue)
    ^.Lo) <> 0);
end;

function IsInfinite(const aValue: TPasDblStrUtilsDouble)
  : TPasDblStrUtilsBoolean; {$IFDEF caninline}inline; {$ENDIF}
begin
  result := ((PDoubleHiLo(@aValue)^.Hi and $7FFFFFFF) = $7FF00000) and
    (PDoubleHiLo(@aValue)^.Lo = 0);
end;

function IsFinite(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := (PDoubleHiLo(@aValue)^.Hi and $7FF00000) <> $7FF00000;
end;

function IsPosInfinite(const aValue: TPasDblStrUtilsDouble)
  : TPasDblStrUtilsBoolean; {$IFDEF caninline}inline; {$ENDIF}
begin
  result := (PDoubleHiLo(@aValue)^.Hi = $7FF00000) and
    (PDoubleHiLo(@aValue)^.Lo = 0);
end;

function IsNegInfinite(const aValue: TPasDblStrUtilsDouble)
  : TPasDblStrUtilsBoolean; {$IFDEF caninline}inline; {$ENDIF}
begin
  result := (PDoubleHiLo(@aValue)^.Hi = $FFF00000) and
    (PDoubleHiLo(@aValue)^.Lo = 0);
end;

function IsPosZero(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := (PDoubleHiLo(@aValue)^.Hi or PDoubleHiLo(@aValue)^.Lo) = 0;
end;

function IsNegZero(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := (PDoubleHiLo(@aValue)^.Hi = $80000000) and
    (PDoubleHiLo(@aValue)^.Lo = 0);
end;

function IsZero(const aValue: TPasDblStrUtilsDouble): TPasDblStrUtilsBoolean;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := ((PDoubleHiLo(@aValue)^.Hi and $7FFFFFFF) or
    PDoubleHiLo(@aValue)^.Lo) = 0;
end;

function IsNegative(const aValue: TPasDblStrUtilsDouble)
  : TPasDblStrUtilsBoolean; {$IFDEF caninline}inline; {$ENDIF}
begin
  result := (PDoubleHiLo(@aValue)^.Hi and $80000000) <> 0;
end;
{$ENDIF}
{$ENDIF}

function DoubleAbsolute(const aValue: TPasDblStrUtilsDouble)
  : TPasDblStrUtilsDouble; {$IFDEF caninline}inline; {$ENDIF}
begin
{$IFDEF cpu64}
  PPasDblStrUtilsInt64(@result)^ := PPasDblStrUtilsInt64(@aValue)^ and
    $7FFFFFFFFFFFFFFF;
{$ELSE}
  PDoubleHiLo(@result)^.Hi := PDoubleHiLo(@aValue)^.Hi and $7FFFFFFF;
  PDoubleHiLo(@result)^.Lo := PDoubleHiLo(@aValue)^.Lo;
{$ENDIF}
end;

function CLZQWord(aValue: TPasDblStrUtilsUInt64): TPasDblStrUtilsInt32;
{$IF declared(BSRQWord)}
begin
  if aValue = 0 then
  begin
    result := 0;
  end
  else
  begin
    result := 63 - BSRQWord(aValue);
  end;
end;
{$ELSE}

const
  CLZDebruijn64Multiplicator: TPasDblStrUtilsUInt64 = TPasDblStrUtilsUInt64
    ($03F79D71B4CB0A89);
  CLZDebruijn64Shift = 58;
  CLZDebruijn64Mask = 63;
  CLZDebruijn64Table: array [0 .. 63] of TPasDblStrUtilsInt32 = (63, 16, 62, 7,
    15, 36, 61, 3, 6, 14, 22, 26, 35, 47, 60, 2, 9, 5, 28, 11, 13, 21, 42, 19,
    25, 31, 34, 40, 46, 52, 59, 1, 17, 8, 37, 4, 23, 27, 48, 10, 29, 12, 43, 20,
    32, 41, 53, 18, 38, 24, 49, 30, 44, 33, 54, 39, 50, 45, 55, 51, 56,
    57, 58, 0);

begin
  if aValue = 0 then
  begin
    result := 64;
  end
  else
  begin
    aValue := aValue or (aValue shr 1);
    aValue := aValue or (aValue shr 2);
    aValue := aValue or (aValue shr 4);
    aValue := aValue or (aValue shr 8);
    aValue := aValue or (aValue shr 16);
    aValue := aValue or (aValue shr 32);
    result := CLZDebruijn64Table
      [((aValue * CLZDebruijn64Multiplicator) shr CLZDebruijn64Shift) and
      CLZDebruijn64Mask];
  end;
end;
{$IFEND}

function CTZQWord(aValue: TPasDblStrUtilsUInt64): TPasDblStrUtilsInt32;
{$IF declared(BSFQWord)}
begin
  if aValue = 0 then
  begin
    result := 64;
  end
  else
  begin
    result := BSFQWord(aValue);
  end;
end;
{$ELSE}

const
  CTZDebruijn64Multiplicator: TPasDblStrUtilsUInt64 = TPasDblStrUtilsUInt64
    ($07EDD5E59A4E28C2);
  CTZDebruijn64Shift = 58;
  CTZDebruijn64Mask = 63;
  CTZDebruijn64Table: array [0 .. 63] of TPasDblStrUtilsInt32 = (63, 0, 58, 1,
    59, 47, 53, 2, 60, 39, 48, 27, 54, 33, 42, 3, 61, 51, 37, 40, 49, 18, 28,
    20, 55, 30, 34, 11, 43, 14, 22, 4, 62, 57, 46, 52, 38, 26, 32, 41, 50, 36,
    17, 19, 29, 10, 13, 21, 56, 45, 25, 31, 35, 16, 9, 12, 44, 24, 15, 8,
    23, 7, 6, 5);
begin
  if aValue = 0 then
  begin
    result := 64;
  end
  else
  begin
    result := CTZDebruijn64Table
      [(((aValue and (-aValue)) * CTZDebruijn64Multiplicator)
      shr CTZDebruijn64Shift) and CTZDebruijn64Mask];
  end;
end;
{$IFEND}
{$IF not declared(BSRQWord)}

function BSRQWord(aValue: TPasDblStrUtilsUInt64): TPasDblStrUtilsInt32;
const
  BSRDebruijn64Multiplicator: TPasDblStrUtilsUInt64 = TPasDblStrUtilsUInt64
    ($03F79D71B4CB0A89);
  BSRDebruijn64Shift = 58;
  BSRDebruijn64Mask = 63;
  BSRDebruijn64Table: array [0 .. 63] of TPasDblStrUtilsInt32 = (0, 47, 1, 56,
    48, 27, 2, 60, 57, 49, 41, 37, 28, 16, 3, 61, 54, 58, 35, 52, 50, 42, 21,
    44, 38, 32, 29, 23, 17, 11, 4, 62, 46, 55, 26, 59, 40, 36, 15, 53, 34, 51,
    20, 43, 31, 22, 10, 45, 25, 39, 14, 33, 19, 30, 9, 24, 13, 18, 8, 12,
    7, 6, 5, 63);

begin
  if aValue = 0 then
  begin
    result := 255;
  end
  else
  begin
    aValue := aValue or (aValue shr 1);
    aValue := aValue or (aValue shr 2);
    aValue := aValue or (aValue shr 4);
    aValue := aValue or (aValue shr 8);
    aValue := aValue or (aValue shr 16);
    aValue := aValue or (aValue shr 32);
    result := BSRDebruijn64Table
      [((aValue * BSRDebruijn64Multiplicator) shr BSRDebruijn64Shift) and
      BSRDebruijn64Mask];
  end;
end;
{$IFEND}

function FloorLog2(const aValue: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt32;
{$IFDEF caninline}inline; {$ENDIF}
{$IF declared(BSRQWord)}
begin
  result := BSRQWord(aValue);
end;
{$ELSE}

begin
  result := 63 - CLZQWord(aValue);
end;
{$IFEND}
{$IF not declared(TPasDblStrUtilsUInt128)}

function UMul128(const a, b: TPasDblStrUtilsUInt64;
  out aProductHi: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
var
  u0, u1, v0, v1, t, w0, w1, w2: TPasDblStrUtilsUInt64;
begin
  u1 := a shr 32;
  u0 := a and UInt64($FFFFFFFF);
  v1 := b shr 32;
  v0 := b and UInt64($FFFFFFFF);
  t := u0 * v0;
  w0 := t and UInt64($FFFFFFFF);
  t := (u1 * v0) + (t shr 32);
  w1 := t and UInt64($FFFFFFFF);
  w2 := t shr 32;
  t := (u0 * v1) + w1;
  aProductHi := ((u1 * v1) + w2) + (t shr 32);
  result := (t shl 32) + w0;
end;

function ShiftRight128(const aLo, aHi: TPasDblStrUtilsUInt64;
  const aShift: TPasDblStrUtilsUInt32): TPasDblStrUtilsUInt64;
begin
  result := (aHi shl (64 - aShift)) or (aLo shr aShift);
end;
{$IFEND}

type
  TPasDblStrUtilsBigUnsignedInteger = record
  public type
    TWord = TPasDblStrUtilsUInt32;
    PWord = ^TWord;
    TWords = array of TWord;
  public
    Words: TWords;
    Count: TPasDblStrUtilsInt32;
  public
    class operator Implicit(const a: TPasDblStrUtilsUInt64)
      : TPasDblStrUtilsBigUnsignedInteger; {$IFDEF caninline}inline; {$ENDIF}
    class operator Implicit(const a: TPasDblStrUtilsBigUnsignedInteger)
      : TPasDblStrUtilsUInt64; {$IFDEF caninline}inline; {$ENDIF}
    class operator Explicit(const a: TPasDblStrUtilsUInt64)
      : TPasDblStrUtilsBigUnsignedInteger; {$IFDEF caninline}inline; {$ENDIF}
    class operator Explicit(const a: TPasDblStrUtilsBigUnsignedInteger)
      : TPasDblStrUtilsUInt64; {$IFDEF caninline}inline; {$ENDIF}
    procedure Clear;
    procedure Trim; {$IFDEF caninline}inline; {$ENDIF}
    procedure Dump;
    function Bits: TPasDblStrUtilsInt32;
    function IsZero: Boolean;
    procedure SetValue(const aWith: TPasDblStrUtilsBigUnsignedInteger);
      overload;
    procedure SetValue(const aWith: TPasDblStrUtilsUInt32); overload;
    procedure ShiftLeftByOne; overload;
    procedure ShiftLeft(const aBits: TPasDblStrUtilsUInt32); overload;
    procedure ShiftRightByOne; overload;
    procedure ShiftRight(const aBits: TPasDblStrUtilsUInt32); overload;
    procedure BitwiseAnd(const aWith
      : TPasDblStrUtilsBigUnsignedInteger); overload;
    procedure Add(const aWith: TPasDblStrUtilsUInt32); overload;
    procedure Add(const aWith: TPasDblStrUtilsBigUnsignedInteger); overload;
    procedure Sub(const aWith: TPasDblStrUtilsUInt32); overload;
    procedure Sub(const aWith: TPasDblStrUtilsBigUnsignedInteger); overload;
    procedure Mul(const aWith: TPasDblStrUtilsUInt32); overload;
    procedure Mul(const aWith: TPasDblStrUtilsBigUnsignedInteger); overload;
    procedure MulAdd(const aMul, aAdd: TPasDblStrUtilsUInt32); overload;
    procedure DivMod(const aDivisor: TPasDblStrUtilsBigUnsignedInteger;
      var aQuotient, aRemainder: TPasDblStrUtilsBigUnsignedInteger); overload;
    class function Power(const aBase, aExponent: TPasDblStrUtilsUInt32)
      : TPasDblStrUtilsBigUnsignedInteger; static;
    procedure MultiplyPower5(aExponent: TPasDblStrUtilsUInt32);
    procedure MultiplyPower10(aExponent: TPasDblStrUtilsUInt32);
    function Scale2Exp(const aI: TPasDblStrUtilsInt32): TPasDblStrUtilsInt32;
    function Compare(const aWith: TPasDblStrUtilsUInt64)
      : TPasDblStrUtilsInt32; overload;
    function Compare(const aWith: TPasDblStrUtilsBigUnsignedInteger)
      : TPasDblStrUtilsInt32; overload;
    class function Difference(const aA, aB: TPasDblStrUtilsBigUnsignedInteger;
      out aOut: TPasDblStrUtilsBigUnsignedInteger): Boolean; static;
  end;

  PPasDblStrUtilsBigUnsignedInteger = ^TPasDblStrUtilsBigUnsignedInteger;

class operator TPasDblStrUtilsBigUnsignedInteger.Implicit
  (const a: TPasDblStrUtilsUInt64): TPasDblStrUtilsBigUnsignedInteger;
begin
  if (a and TPasDblStrUtilsUInt64($FFFFFFFF00000000)) <> 0 then
  begin
    result.Count := 2;
    SetLength(result.Words, result.Count);
    result.Words[0] := a and TPasDblStrUtilsUInt32($FFFFFFFF);
    result.Words[1] := a shr 32;
  end
  else
  begin
    result.Count := 1;
    SetLength(result.Words, result.Count);
    result.Words[0] := a and TPasDblStrUtilsUInt32($FFFFFFFF);
  end;
end;

class operator TPasDblStrUtilsBigUnsignedInteger.Implicit
  (const a: TPasDblStrUtilsBigUnsignedInteger): TPasDblStrUtilsUInt64;
begin
  if a.Count > 0 then
  begin
    result := a.Words[0];
    if a.Count > 1 then
    begin
      result := result or (TPasDblStrUtilsUInt64(a.Words[1]) shl 32);
    end;
  end
  else
  begin
    result := 0;
  end;
end;

class operator TPasDblStrUtilsBigUnsignedInteger.Explicit
  (const a: TPasDblStrUtilsUInt64): TPasDblStrUtilsBigUnsignedInteger;
begin
  if (a and TPasDblStrUtilsUInt64($FFFFFFFF00000000)) <> 0 then
  begin
    result.Count := 2;
    SetLength(result.Words, result.Count);
    result.Words[0] := a and TPasDblStrUtilsUInt32($FFFFFFFF);
    result.Words[1] := a shr 32;
  end
  else
  begin
    result.Count := 1;
    SetLength(result.Words, result.Count);
    result.Words[0] := a and TPasDblStrUtilsUInt32($FFFFFFFF);
  end;
end;

class operator TPasDblStrUtilsBigUnsignedInteger.Explicit
  (const a: TPasDblStrUtilsBigUnsignedInteger): TPasDblStrUtilsUInt64;
begin
  if a.Count > 0 then
  begin
    result := a.Words[0];
    if a.Count > 1 then
    begin
      result := result or (TPasDblStrUtilsUInt64(a.Words[1]) shl 32);
    end;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.Clear;
begin
  Count := 0;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.Trim;
begin
  while (Count > 1) and (Words[Count - 1] = 0) do
  begin
    dec(Count);
  end;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.Dump;
var
  Index: TPasDblStrUtilsInt32;
begin
  for Index := 0 to Count - 1 do
  begin
    Write(Words[Index], ' ');
  end;
  WriteLn;
end;

function TPasDblStrUtilsBigUnsignedInteger.Bits: TPasDblStrUtilsInt32;
var
  Index: TPasDblStrUtilsInt32;
begin
  result := 0;
  for Index := Count - 1 downto 0 do
  begin
    if Words[Index] <> 0 then
    begin
      result := (Index shl 5) + FloorLog2(Words[Index]);
      break;
    end;
  end;
end;

function TPasDblStrUtilsBigUnsignedInteger.IsZero: Boolean;
var
  Index: TPasDblStrUtilsInt32;
begin
  result := true;
  for Index := 0 to Count - 1 do
  begin
    if Words[Index] <> 0 then
    begin
      result := false;
      exit;
    end;
  end;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.SetValue
  (const aWith: TPasDblStrUtilsBigUnsignedInteger);
begin
  Count := aWith.Count;
  if length(Words) < Count then
  begin
    SetLength(Words, Count + ((Count + 1) shr 1));
  end;
  if Count > 0 then
  begin
    Move(aWith.Words[0], Words[0], Count * sizeof(TWord));
    Trim;
  end
  else
  begin
    if length(Words) < 1 then
    begin
      SetLength(Words, 1);
    end;
    Words[0] := 0;
    Count := 1;
  end;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.SetValue
  (const aWith: TPasDblStrUtilsUInt32);
begin
  if length(Words) < 1 then
  begin
    SetLength(Words, 1);
  end;
  Words[0] := aWith;
  Count := 1;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.ShiftLeftByOne;
var
  Index, NewCount: TPasDblStrUtilsInt32;
begin
  if Count > 0 then
  begin
    if (Words[Count - 1] and TWord($80000000)) <> 0 then
    begin
      NewCount := Count + 1;
      if length(Words) < NewCount then
      begin
        SetLength(Words, NewCount + ((NewCount + 1) shr 1));
      end;
      for Index := Count to NewCount - 1 do
      begin
        Words[Index] := 0;
      end;
      Count := NewCount;
    end;
    if Count > 1 then
    begin
      for Index := Count - 1 downto 1 do
      begin
        Words[Index] := (Words[Index] shl 1) or
          ((Words[Index - 1] shr 31) and 1);
      end;
    end;
    Words[0] := Words[0] shl 1;
  end;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.ShiftLeft
  (const aBits: TPasDblStrUtilsUInt32);
var
  Index, NewCount, BitLength, ShiftOffset, BitShift, InverseBitShift
    : TPasDblStrUtilsInt32;
  Current, Next: TWord;
begin
  if Count > 0 then
  begin
    if aBits = 1 then
    begin
      ShiftLeftByOne;
    end
    else
    begin
      Current := Words[Count - 1];
      if Current <> 0 then
      begin
        BitLength := FloorLog2(Current);
      end
      else
      begin
        BitLength := 0;
      end;
      NewCount := Count + ((BitLength + aBits) shr 5);
      if Count < NewCount then
      begin
        if length(Words) < NewCount then
        begin
          SetLength(Words, NewCount + ((NewCount + 1) shr 1));
        end;
        for Index := Count to NewCount - 1 do
        begin
          Words[Index] := 0;
        end;
      end;
      Count := NewCount;
      ShiftOffset := aBits shr 5;
      BitShift := aBits and 31;
      InverseBitShift := (32 - BitShift) and 31;
      if ShiftOffset <> 0 then
      begin
        for Index := Count - 1 downto ShiftOffset do
        begin
          Words[Index] := Words[Index - ShiftOffset];
        end;
        for Index := 0 to ShiftOffset - 1 do
        begin
          Words[Index] := 0;
        end;
      end;
      if BitShift <> 0 then
      begin
        Next := 0;
        for Index := 0 to Count - 1 do
        begin
          Current := Words[Index];
          Words[Index] := (Current shl BitShift) or Next;
          Next := Current shr InverseBitShift;
        end;
      end;
      Trim;
    end;
  end;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.ShiftRightByOne;
var
  Index: TPasDblStrUtilsInt32;
begin
  if Count > 0 then
  begin
    for Index := 0 to Count - 2 do
    begin
      Words[Index] := (Words[Index] shr 1) or ((Words[Index + 1] and 1) shl 31);
    end;
    Words[Count - 1] := Words[Count - 1] shr 1;
    if (Count > 1) and (Words[Count - 1] = 0) then
    begin
      dec(Count);
    end;
  end;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.ShiftRight
  (const aBits: TPasDblStrUtilsUInt32);
var
  Index, ShiftOffset, BitShift, InverseBitShift: TPasDblStrUtilsInt32;
  Current, Next: TWord;
begin
  if Count > 0 then
  begin
    if aBits = 1 then
    begin
      ShiftRightByOne;
    end
    else
    begin
      ShiftOffset := aBits shr 5;
      BitShift := aBits and 31;
      InverseBitShift := (32 - BitShift) and 31;
      if ShiftOffset <> 0 then
      begin
        for Index := Count - 1 downto ShiftOffset do
        begin
          Words[Index - ShiftOffset] := Words[Index];
        end;
        for Index := Count - ShiftOffset to Count - 1 do
        begin
          Words[Index] := 0;
        end;
      end;
      if BitShift <> 0 then
      begin
        Next := 0;
        for Index := Count - 1 downto 0 do
        begin
          Current := Words[Index];
          Words[Index] := (Current shr BitShift) or Next;
          Next := Current shl InverseBitShift;
        end;
      end;
      Trim;
    end;
  end;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.BitwiseAnd
  (const aWith: TPasDblStrUtilsBigUnsignedInteger);
var
  Index: TPasDblStrUtilsInt32;
  Value: TWord;
begin
  for Index := 0 to Count - 1 do
  begin
    Value := Words[Index];
    if Index < aWith.Count then
    begin
      Value := Value and aWith.Words[Index];
    end
    else
    begin
      Value := 0;
    end;
    Words[Index] := Value;
  end;
  Trim;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.Add(const aWith
  : TPasDblStrUtilsUInt32);
var
  Index, OldCount, OtherIndex: TPasDblStrUtilsInt32;
  Carry: TPasDblStrUtilsUInt32;
  Temporary: TPasDblStrUtilsUInt64;
begin
  Index := 0;
  Carry := aWith;
  while (Carry <> 0) and (Index < Count) do
  begin
    Temporary := TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(Words[Index]) +
      TPasDblStrUtilsUInt64(Carry));
    Words[Index] := Temporary and TPasDblStrUtilsUInt32($FFFFFFFF);
    Carry := Temporary shr 32;
    inc(Index);
  end;
  if Carry <> 0 then
  begin
    if Count <= Index then
    begin
      OldCount := Count;
      Count := Index + 1;
      if length(Words) < Count then
      begin
        SetLength(Words, Count + ((Count + 1) shr 1));
      end;
      for OtherIndex := OldCount to Count - 1 do
      begin
        Words[OtherIndex] := 0;
      end;
    end;
    Words[Index] := Carry;
  end;
  Trim;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.Add(const aWith
  : TPasDblStrUtilsBigUnsignedInteger);
var
  NewCount, Index, CommonCount, OldCount, OtherIndex: TPasDblStrUtilsInt32;
  Carry: TPasDblStrUtilsUInt32;
  Temporary: TPasDblStrUtilsUInt64;
begin
  if Count < aWith.Count then
  begin
    NewCount := aWith.Count;
  end
  else
  begin
    NewCount := Count;
  end;
  inc(NewCount);
  if length(Words) < NewCount then
  begin
    SetLength(Words, NewCount + ((NewCount + 1) shr 1));
    for Index := Count to NewCount - 1 do
    begin
      Words[Index] := 0;
    end;
  end;
  Count := NewCount;
  if aWith.Count < Count then
  begin
    CommonCount := aWith.Count;
  end
  else
  begin
    CommonCount := Count;
  end;
  Carry := 0;
  for Index := 0 to CommonCount - 1 do
  begin
    Temporary := TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(Words[Index]) +
      TPasDblStrUtilsUInt64(aWith.Words[Index]) + TPasDblStrUtilsUInt64(Carry));
    Words[Index] := Temporary and TPasDblStrUtilsUInt32($FFFFFFFF);
    Carry := Temporary shr 32;
  end;
  Index := CommonCount;
  while (Carry <> 0) and (Index < Count) do
  begin
    Temporary := TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(Words[Index]) +
      TPasDblStrUtilsUInt64(Carry));
    Words[Index] := Temporary and TPasDblStrUtilsUInt32($FFFFFFFF);
    Carry := Temporary shr 32;
    inc(Index);
  end;
  if Carry <> 0 then
  begin
    if Count <= Index then
    begin
      OldCount := Count;
      Count := Index + 1;
      if length(Words) < Count then
      begin
        SetLength(Words, Count + ((Count + 1) shr 1));
      end;
      for OtherIndex := OldCount to Count - 1 do
      begin
        Words[OtherIndex] := 0;
      end;
    end;
    Words[Index] := Carry;
  end;
  Trim;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.Sub(const aWith
  : TPasDblStrUtilsUInt32);
var
  Index: TPasDblStrUtilsInt32;
  Borrow: TPasDblStrUtilsUInt32;
  Temporary: TPasDblStrUtilsUInt64;
begin
  Index := 0;
  Borrow := aWith;
  while (Borrow <> 0) and (Index < Count) do
  begin
    Temporary := TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(Words[Index]) -
      TPasDblStrUtilsUInt64(Borrow));
    Words[Index] := Temporary and TPasDblStrUtilsUInt32($FFFFFFFF);
    Borrow := Temporary shr 63;
    inc(Index);
  end;
  Trim;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.Sub(const aWith
  : TPasDblStrUtilsBigUnsignedInteger);
var
  Index, CommonCount: TPasDblStrUtilsInt32;
  Borrow: TPasDblStrUtilsUInt32;
  Temporary: TPasDblStrUtilsUInt64;
begin
  if aWith.Count < Count then
  begin
    CommonCount := aWith.Count;
  end
  else
  begin
    CommonCount := Count;
  end;
  Borrow := 0;
  for Index := 0 to CommonCount - 1 do
  begin
    Temporary := TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(Words[Index]) -
      TPasDblStrUtilsUInt64(aWith.Words[Index])) -
      TPasDblStrUtilsUInt64(Borrow);
    Words[Index] := Temporary and TPasDblStrUtilsUInt32($FFFFFFFF);
    Borrow := Temporary shr 63;
  end;
  Index := CommonCount;
  while (Borrow <> 0) and (Index < Count) do
  begin
    Temporary := TPasDblStrUtilsUInt64(Words[Index]) -
      TPasDblStrUtilsUInt64(Borrow);
    Words[Index] := Temporary and TPasDblStrUtilsUInt32($FFFFFFFF);
    Borrow := Temporary shr 63;
    inc(Index);
  end;
  Trim;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.Mul(const aWith
  : TPasDblStrUtilsUInt32);
var
  Index, OldCount, OtherIndex: TPasDblStrUtilsInt32;
  Carry: TPasDblStrUtilsUInt32;
  Temporary: TPasDblStrUtilsUInt64;
begin
  Index := 0;
  Carry := 0;
  for Index := 0 to Count - 1 do
  begin
    Temporary := (Words[Index] * TPasDblStrUtilsUInt64(aWith)) + Carry;
    Words[Index] := Temporary and TPasDblStrUtilsUInt32($FFFFFFFF);
    Carry := Temporary shr 32;
  end;
  if Carry <> 0 then
  begin
    Index := Count;
    OldCount := Count;
    inc(Count);
    if length(Words) < Count then
    begin
      SetLength(Words, Count + ((Count + 1) shr 1));
    end;
    for OtherIndex := OldCount to Count - 1 do
    begin
      Words[OtherIndex] := 0;
    end;
    Words[Index] := Carry;
  end;
  Trim;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.Mul(const aWith
  : TPasDblStrUtilsBigUnsignedInteger);
var
  Index, ShiftCount, BitIndex: TPasDblStrUtilsInt32;
  Value: TWord;
  Temporary, Sum: TPasDblStrUtilsBigUnsignedInteger;
begin
  ShiftCount := 0;
  Temporary := self;
  Sum.Clear;
  for Index := 0 to aWith.Count - 1 do
  begin
    Value := aWith.Words[Index];
    if Value <> 0 then
    begin
      for BitIndex := 0 to 31 do
      begin
        if (Value and (TPasDblStrUtilsUInt32(1) shl BitIndex)) <> 0 then
        begin
          if ShiftCount <> 0 then
          begin
            Temporary.ShiftLeft(ShiftCount);
            ShiftCount := 0;
          end;
          Sum.Add(Temporary);
        end;
        inc(ShiftCount);
      end;
    end
    else
    begin
      inc(ShiftCount, 32);
    end;
  end;
  while (Sum.Count > 1) and (Sum.Words[Sum.Count - 1] = 0) do
  begin
    dec(Sum.Count);
  end;
  if Sum.Count < length(Words) then
  begin
    Move(Sum.Words[0], Words[0], Sum.Count * sizeof(TWord));
  end
  else
  begin
    Words := copy(Sum.Words, 0, Sum.Count);
  end;
  Count := Sum.Count;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.MulAdd(const aMul,
  aAdd: TPasDblStrUtilsUInt32);
var
  Index, OldCount, OtherIndex: TPasDblStrUtilsInt32;
  Carry: TPasDblStrUtilsUInt32;
  Temporary: TPasDblStrUtilsUInt64;
begin
  Index := 0;
  Carry := aAdd;
  for Index := 0 to Count - 1 do
  begin
    Temporary := (Words[Index] * TPasDblStrUtilsUInt64(aMul)) + Carry;
    Words[Index] := Temporary and TPasDblStrUtilsUInt32($FFFFFFFF);
    Carry := Temporary shr 32;
  end;
  if Carry <> 0 then
  begin
    Index := Count;
    OldCount := Count;
    inc(Count);
    if length(Words) < Count then
    begin
      SetLength(Words, Count + ((Count + 1) shr 1));
    end;
    for OtherIndex := OldCount to Count - 1 do
    begin
      Words[OtherIndex] := 0;
    end;
    Words[Index] := Carry;
  end;
  Trim;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.DivMod(const aDivisor
  : TPasDblStrUtilsBigUnsignedInteger;
  var aQuotient, aRemainder: TPasDblStrUtilsBigUnsignedInteger);
var
  Index, BitLen, WordIndex, BitIndex, Cmp: TPasDblStrUtilsInt32;
  QuotientIsZero: Boolean;
begin
  Cmp := Compare(aDivisor);
  if Cmp < 0 then
  begin
    aQuotient.SetValue(0);
    aRemainder.SetValue(self);
  end
  else if Cmp = 0 then
  begin
    aQuotient.SetValue(1);
    aRemainder.SetValue(0);
  end
  else
  begin
    aQuotient.SetValue(0);
    aRemainder.SetValue(0);
    QuotientIsZero := true;
    BitLen := Bits;
    for Index := BitLen downto 0 do
    begin
      aRemainder.ShiftLeftByOne;
      aRemainder.Words[0] := aRemainder.Words[0] or
        ((Words[Index shr 5] shr (Index and 31)) and 1);
      if aRemainder.Compare(aDivisor) >= 0 then
      begin
        aRemainder.Sub(aDivisor);
        WordIndex := Index shr 5;
        BitIndex := Index and 31;
        if QuotientIsZero then
        begin
          QuotientIsZero := false;
          aQuotient.Count := WordIndex + 1;
          SetLength(aQuotient.Words, aQuotient.Count);
          FillChar(aQuotient.Words[0], aQuotient.Count * sizeof(TWord), #0);
        end;
        aQuotient.Words[WordIndex] := aQuotient.Words[WordIndex] or
          (TWord(1) shl BitIndex);
      end;
    end;
  end;
end;

class function TPasDblStrUtilsBigUnsignedInteger.Power(const aBase,
  aExponent: TPasDblStrUtilsUInt32): TPasDblStrUtilsBigUnsignedInteger;
var
  Exponent: TPasDblStrUtilsUInt32;
  Base: TPasDblStrUtilsBigUnsignedInteger;
begin
  result := 1;
  Base := aBase;
  Exponent := aExponent;
  repeat
    if (Exponent and 1) <> 0 then
    begin
      result.Mul(Base);
    end;
    Exponent := Exponent shr 1;
    if Exponent = 0 then
    begin
      break;
    end
    else
    begin
      Base.Mul(Base);
    end;
  until false;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.MultiplyPower5
  (aExponent: TPasDblStrUtilsUInt32);
const
  Pow5Table: array [0 .. 13] of TPasDblStrUtilsUInt32 = (1, 5, 5 * 5, 5 * 5 * 5,
    5 * 5 * 5 * 5, 5 * 5 * 5 * 5 * 5, 5 * 5 * 5 * 5 * 5 * 5,
    5 * 5 * 5 * 5 * 5 * 5 * 5, 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5,
    5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5, 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5,
    5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5, 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5
    * 5 * 5 * 5, 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5);
begin
  if aExponent > 0 then
  begin
    while aExponent >= 13 do
    begin
      dec(aExponent, 13);
      Mul(Pow5Table[13]);
    end;
    if aExponent > 0 then
    begin
      Mul(Pow5Table[aExponent]);
    end;
  end;
end;

procedure TPasDblStrUtilsBigUnsignedInteger.MultiplyPower10
  (aExponent: TPasDblStrUtilsUInt32);
begin
  if aExponent > 0 then
  begin
    MultiplyPower5(aExponent);
    ShiftLeft(aExponent);
  end;
end;

function TPasDblStrUtilsBigUnsignedInteger.Scale2Exp
  (const aI: TPasDblStrUtilsInt32): TPasDblStrUtilsInt32;
begin
  if aI >= 0 then
  begin
    // z = x * 2^i
    ShiftLeft(aI);
    result := 0;
  end
  else
  begin
    // x * 5^-i * 10^i
    Mul(TPasDblStrUtilsBigUnsignedInteger.Power(5, -aI));
    result := aI;
  end;
end;

function TPasDblStrUtilsBigUnsignedInteger.Compare
  (const aWith: TPasDblStrUtilsUInt64): TPasDblStrUtilsInt32;
var
  Value: TPasDblStrUtilsUInt64;
begin
  case Count of
    0:
      begin
        result := -1;
      end;
    1:
      begin
        Value := Words[0];
        if Value < aWith then
        begin
          result := -1;
        end
        else if Value > aWith then
        begin
          result := 1;
        end
        else
        begin
          result := 0;
        end;
      end;
    2:
      begin
{$IFDEF BIG_ENDIAN}
        Value := Words[0] or (TPasDblStrUtilsUInt64(Words[1]) shl 32);
{$ELSE}
        Value := PPasDblStrUtilsUInt64(@Words[0])^;
{$ENDIF}
        if Value < aWith then
        begin
          result := -1;
        end
        else if Value > aWith then
        begin
          result := 1;
        end
        else
        begin
          result := 0;
        end;
      end;
  else
    begin
      result := 1;
    end;
  end;
end;

function TPasDblStrUtilsBigUnsignedInteger.Compare
  (const aWith: TPasDblStrUtilsBigUnsignedInteger): TPasDblStrUtilsInt32;
var
  Index: TPasDblStrUtilsInt32;
  Difference: TPasDblStrUtilsInt64;
begin
  if Count < aWith.Count then
  begin
    result := -1;
    exit;
  end
  else if Count > aWith.Count then
  begin
    result := 1;
    exit;
  end;
  for Index := Count - 1 downto 0 do
  begin
    Difference := TPasDblStrUtilsInt64(Words[Index]) -
      TPasDblStrUtilsInt64(aWith.Words[Index]);
    if Difference <> 0 then
    begin
      if Difference < 0 then
      begin
        result := -1;
      end
      else
      begin
        result := 1;
      end;
      exit;
    end;
  end;
  result := 0;
end;

class function TPasDblStrUtilsBigUnsignedInteger.Difference(const aA,
  aB: TPasDblStrUtilsBigUnsignedInteger;
  out aOut: TPasDblStrUtilsBigUnsignedInteger): Boolean;
var
  Cmp, Index: TPasDblStrUtilsInt32;
  a, b: PPasDblStrUtilsBigUnsignedInteger;
  Borrow, d: TWord;
begin
  Cmp := aA.Compare(aB);
  if Cmp < 0 then
  begin
    a := @aB;
    b := @aA;
    result := true;
  end
  else
  begin
    a := @aA;
    b := @aB;
    result := false;
  end;
  Borrow := 0;
  aOut.Count := a^.Count;
  SetLength(aOut.Words, aOut.Count);
  for Index := 0 to a^.Count - 1 do
  begin
    d := a^.Words[Index] - Borrow;
    if Index < b^.Count then
    begin
      dec(d, b^.Words[Index]);
    end;
    if d > a^.Words[Index] then
    begin
      Borrow := 1;
    end
    else
    begin
      Borrow := 0;
    end;
    aOut.Words[Index] := d;
  end;
  aOut.Trim;
end;

type
  TPasDblStrUtilsUInt128 = packed record
  public
    constructor Create(const aHi, aLo: TPasDblStrUtilsUInt64);
    function CountLeadingZeroBits: TPasDblStrUtilsInt32;
    function CountTrailingZeroBits: TPasDblStrUtilsInt32;
    function FloorLog2: TPasDblStrUtilsInt32;
    class operator Implicit(const a: TPasDblStrUtilsUInt64)
      : TPasDblStrUtilsUInt128; {$IFDEF caninline}inline; {$ENDIF}
    class operator Implicit(const a: TPasDblStrUtilsUInt128)
      : TPasDblStrUtilsUInt64; {$IFDEF caninline}inline; {$ENDIF}
    class operator Explicit(const a: TPasDblStrUtilsUInt64)
      : TPasDblStrUtilsUInt128; {$IFDEF caninline}inline; {$ENDIF}
    class operator Explicit(const a: TPasDblStrUtilsUInt128)
      : TPasDblStrUtilsUInt64; {$IFDEF caninline}inline; {$ENDIF}
    class operator inc(const a: TPasDblStrUtilsUInt128): TPasDblStrUtilsUInt128;
    {$IFDEF caninline}inline; {$ENDIF}
    class operator dec(const a: TPasDblStrUtilsUInt128): TPasDblStrUtilsUInt128;
    {$IFDEF caninline}inline; {$ENDIF}
    class operator Add(const a, b: TPasDblStrUtilsUInt128)
      : TPasDblStrUtilsUInt128; {$IFDEF caninline}inline; {$ENDIF}
    class operator Subtract(const a, b: TPasDblStrUtilsUInt128)
      : TPasDblStrUtilsUInt128; {$IFDEF caninline}inline; {$ENDIF}
    class operator LeftShift(const a: TPasDblStrUtilsUInt128;
      Shift: TPasDblStrUtilsInt32): TPasDblStrUtilsUInt128;
    class operator RightShift(const a: TPasDblStrUtilsUInt128;
      Shift: TPasDblStrUtilsInt32): TPasDblStrUtilsUInt128;
    class operator BitwiseAnd(const a, b: TPasDblStrUtilsUInt128)
      : TPasDblStrUtilsUInt128; {$IFDEF caninline}inline; {$ENDIF}
    class operator BitwiseOr(const a, b: TPasDblStrUtilsUInt128)
      : TPasDblStrUtilsUInt128; {$IFDEF caninline}inline; {$ENDIF}
    class operator BitwiseXor(const a, b: TPasDblStrUtilsUInt128)
      : TPasDblStrUtilsUInt128; {$IFDEF caninline}inline; {$ENDIF}
    class operator LogicalNot(const a: TPasDblStrUtilsUInt128)
      : TPasDblStrUtilsUInt128; {$IFDEF caninline}inline; {$ENDIF}
    class operator Negative(const a: TPasDblStrUtilsUInt128)
      : TPasDblStrUtilsUInt128; {$IFDEF caninline}inline; {$ENDIF}
    class operator Positive(const a: TPasDblStrUtilsUInt128)
      : TPasDblStrUtilsUInt128; {$IFDEF caninline}inline; {$ENDIF}
    class operator Equal(const a, b: TPasDblStrUtilsUInt128): Boolean;
    {$IFDEF caninline}inline; {$ENDIF}
    class operator NotEqual(const a, b: TPasDblStrUtilsUInt128): Boolean;
    {$IFDEF caninline}inline; {$ENDIF}
    class operator GreaterThan(const a, b: TPasDblStrUtilsUInt128): Boolean;
    {$IFDEF caninline}inline; {$ENDIF}
    class operator GreaterThanOrEqual(const a, b: TPasDblStrUtilsUInt128)
      : Boolean; {$IFDEF caninline}inline; {$ENDIF}
    class operator LessThan(const a, b: TPasDblStrUtilsUInt128): Boolean;
    {$IFDEF caninline}inline; {$ENDIF}
    class operator LessThanOrEqual(const a, b: TPasDblStrUtilsUInt128): Boolean;
    {$IFDEF caninline}inline; {$ENDIF}
    class procedure Mul64(out r: TPasDblStrUtilsUInt128;
      const a, b: TPasDblStrUtilsUInt64); overload; static;
    {$IF defined(CPUx86_64)}register; {$IFEND}
    class function Mul64(const a, b: TPasDblStrUtilsUInt64)
      : TPasDblStrUtilsUInt128; overload; static;
    {$IF defined(CPUx86_64)}register; {$IFEND}
    class operator Multiply(const a, b: TPasDblStrUtilsUInt128)
      : TPasDblStrUtilsUInt128;
    class procedure BinaryDivMod128(Dividend, Divisor: TPasDblStrUtilsUInt128;
      out Quotient, Remainder: TPasDblStrUtilsUInt128); static;
    class procedure BinaryDivMod64(const Dividend: TPasDblStrUtilsUInt128;
      const Divisor: TPasDblStrUtilsUInt64;
      out Quotient: TPasDblStrUtilsUInt128;
      out Remainder: TPasDblStrUtilsUInt64); static;
    class procedure DivMod64(const Dividend: TPasDblStrUtilsUInt128;
      const Divisor: TPasDblStrUtilsUInt64;
      out Quotient, Remainder: TPasDblStrUtilsUInt64); static;
    class procedure DivMod128Ex(Dividend, Divisor: TPasDblStrUtilsUInt128;
      out Quotient, Remainder: TPasDblStrUtilsUInt128); static;
    class procedure DivMod128(Dividend, Divisor: TPasDblStrUtilsUInt128;
      out Quotient, Remainder: TPasDblStrUtilsUInt128); static;
    class operator IntDivide(const Dividend: TPasDblStrUtilsUInt128;
      const Divisor: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt128;
    class operator IntDivide(const Dividend, Divisor: TPasDblStrUtilsUInt128)
      : TPasDblStrUtilsUInt128;
    class operator Modulus(const Dividend: TPasDblStrUtilsUInt128;
      const Divisor: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt128;
    class operator Modulus(const Dividend, Divisor: TPasDblStrUtilsUInt128)
      : TPasDblStrUtilsUInt128;
{$IFDEF BIG_ENDIAN}
    case Byte of
      0:
        (Hi, Lo: TPasDblStrUtilsUInt64;
        );
      1:
        (Q3, Q2, Q1, Q0: TPasDblStrUtilsUInt32;
        );
{$ELSE}
    case Byte of
      0:
        (Lo, Hi: TPasDblStrUtilsUInt64;
        );
      1:
        (Q0, Q1, Q2, Q3: TPasDblStrUtilsUInt32;
        );
{$ENDIF}
  end;

constructor TPasDblStrUtilsUInt128.Create(const aHi,
  aLo: TPasDblStrUtilsUInt64);
begin
  Hi := aHi;
  Lo := aLo;
end;

function TPasDblStrUtilsUInt128.CountLeadingZeroBits: TPasDblStrUtilsInt32;
begin
  if Hi = 0 then
  begin
    if Lo = 0 then
    begin
      result := 64;
    end
    else
    begin
      result := CLZQWord(Lo) + 64;
    end;
  end
  else
  begin
    result := CLZQWord(Hi);
  end;
end;

function TPasDblStrUtilsUInt128.CountTrailingZeroBits: TPasDblStrUtilsInt32;
begin
  if Lo = 0 then
  begin
    result := CTZQWord(Hi) + 64;
  end
  else
  begin
    result := CTZQWord(Lo);
  end;
end;

function TPasDblStrUtilsUInt128.FloorLog2: TPasDblStrUtilsInt32;
begin
  result := 127 - CountLeadingZeroBits;
end;

class operator TPasDblStrUtilsUInt128.Implicit(const a: TPasDblStrUtilsUInt64)
  : TPasDblStrUtilsUInt128;
begin
  result.Hi := 0;
  result.Lo := a;
end;

class operator TPasDblStrUtilsUInt128.Implicit(const a: TPasDblStrUtilsUInt128)
  : TPasDblStrUtilsUInt64;
begin
  result := a.Lo;
end;

class operator TPasDblStrUtilsUInt128.Explicit(const a: TPasDblStrUtilsUInt64)
  : TPasDblStrUtilsUInt128;
begin
  result.Hi := 0;
  result.Lo := a;
end;

class operator TPasDblStrUtilsUInt128.Explicit(const a: TPasDblStrUtilsUInt128)
  : TPasDblStrUtilsUInt64;
begin
  result := a.Lo;
end;

class operator TPasDblStrUtilsUInt128.inc(const a: TPasDblStrUtilsUInt128)
  : TPasDblStrUtilsUInt128;
begin
  result.Lo := a.Lo + 1;
  result.Hi := a.Hi + (((a.Lo xor result.Lo) and a.Lo) shr 63);
end;

class operator TPasDblStrUtilsUInt128.dec(const a: TPasDblStrUtilsUInt128)
  : TPasDblStrUtilsUInt128;
begin
  result.Lo := a.Lo - 1;
  result.Hi := a.Hi - (((result.Lo xor a.Lo) and result.Lo) shr 63);
end;

class operator TPasDblStrUtilsUInt128.Add(const a, b: TPasDblStrUtilsUInt128)
  : TPasDblStrUtilsUInt128;
begin
  result.Hi := a.Hi + b.Hi +
    ((((a.Lo and b.Lo) and 1) + (a.Lo shr 1) + (b.Lo shr 1)) shr 63);
  result.Lo := a.Lo + b.Lo;
end;

class operator TPasDblStrUtilsUInt128.Subtract(const a,
  b: TPasDblStrUtilsUInt128): TPasDblStrUtilsUInt128;
begin
  result.Lo := a.Lo - b.Lo;
  result.Hi := a.Hi -
    (b.Hi + ((((result.Lo and b.Lo) and 1) + (b.Lo shr 1) + (result.Lo shr 1)
    ) shr 63));
end;

class operator TPasDblStrUtilsUInt128.LeftShift(const a: TPasDblStrUtilsUInt128;
  Shift: TPasDblStrUtilsInt32): TPasDblStrUtilsUInt128;
var
  m0, m1: TPasDblStrUtilsUInt64;
begin
  Shift := Shift and 127;
  m0 := ((((Shift + 127) or Shift) and 64) shr 6) - 1;
  m1 := (Shift shr 6) - 1;
  Shift := Shift and 63;
  result.Hi := (a.Lo shl Shift) and not m1;
  result.Lo := (a.Lo shl Shift) and m1;
  result.Hi := result.Hi or
    (((a.Hi shl Shift) or ((a.Lo shr (64 - Shift)) and m0)) and m1);
end;

class operator TPasDblStrUtilsUInt128.RightShift
  (const a: TPasDblStrUtilsUInt128; Shift: TPasDblStrUtilsInt32)
  : TPasDblStrUtilsUInt128;
var
  m0, m1: TPasDblStrUtilsUInt64;
begin
  Shift := Shift and 127;
  m0 := ((((Shift + 127) or Shift) and 64) shr 6) - 1;
  m1 := (Shift shr 6) - 1;
  Shift := Shift and 63;
  result.Lo := (a.Hi shr Shift) and not m1;
  result.Hi := (a.Hi shr Shift) and m1;
  result.Lo := result.Lo or
    (((a.Lo shr Shift) or ((a.Hi shl (64 - Shift)) and m0)) and m1);
end;

class operator TPasDblStrUtilsUInt128.BitwiseAnd(const a,
  b: TPasDblStrUtilsUInt128): TPasDblStrUtilsUInt128;
begin
  result.Hi := a.Hi and b.Hi;
  result.Lo := a.Lo and b.Lo;
end;

class operator TPasDblStrUtilsUInt128.BitwiseOr(const a,
  b: TPasDblStrUtilsUInt128): TPasDblStrUtilsUInt128;
begin
  result.Hi := a.Hi or b.Hi;
  result.Lo := a.Lo or b.Lo;
end;

class operator TPasDblStrUtilsUInt128.BitwiseXor(const a,
  b: TPasDblStrUtilsUInt128): TPasDblStrUtilsUInt128;
begin
  result.Hi := a.Hi xor b.Hi;
  result.Lo := a.Lo xor b.Lo;
end;

class operator TPasDblStrUtilsUInt128.LogicalNot
  (const a: TPasDblStrUtilsUInt128): TPasDblStrUtilsUInt128;
begin
  result.Hi := not a.Hi;
  result.Lo := not a.Lo;
end;

class operator TPasDblStrUtilsUInt128.Negative(const a: TPasDblStrUtilsUInt128)
  : TPasDblStrUtilsUInt128;
var
  Temporary: TPasDblStrUtilsUInt128;
begin
  Temporary.Hi := not a.Hi;
  Temporary.Lo := not a.Lo;
  result.Lo := Temporary.Lo + 1;
  result.Hi := Temporary.Hi +
    (((Temporary.Lo xor result.Lo) and Temporary.Lo) shr 63);
end;

class operator TPasDblStrUtilsUInt128.Positive(const a: TPasDblStrUtilsUInt128)
  : TPasDblStrUtilsUInt128;
begin
  result.Hi := a.Hi;
  result.Lo := a.Lo;
end;

class operator TPasDblStrUtilsUInt128.Equal(const a,
  b: TPasDblStrUtilsUInt128): Boolean;
begin
  result := (a.Hi = b.Hi) and (a.Lo = b.Lo);
end;

class operator TPasDblStrUtilsUInt128.NotEqual(const a,
  b: TPasDblStrUtilsUInt128): Boolean;
begin
  result := (a.Hi <> b.Hi) or (a.Lo <> b.Lo);
end;

class operator TPasDblStrUtilsUInt128.GreaterThan(const a,
  b: TPasDblStrUtilsUInt128): Boolean;
begin
  result := (a.Hi > b.Hi) or ((a.Hi = b.Hi) and (a.Lo > b.Lo));
end;

class operator TPasDblStrUtilsUInt128.GreaterThanOrEqual(const a,
  b: TPasDblStrUtilsUInt128): Boolean;
begin
  result := (a.Hi > b.Hi) or ((a.Hi = b.Hi) and (a.Lo >= b.Lo));
end;

class operator TPasDblStrUtilsUInt128.LessThan(const a,
  b: TPasDblStrUtilsUInt128): Boolean;
begin
  result := (a.Hi < b.Hi) or ((a.Hi = b.Hi) and (a.Lo < b.Lo));
end;

class operator TPasDblStrUtilsUInt128.LessThanOrEqual(const a,
  b: TPasDblStrUtilsUInt128): Boolean;
begin
  result := (a.Hi <= b.Hi) or ((a.Hi = b.Hi) and (a.Lo <= b.Lo));
end;

class procedure TPasDblStrUtilsUInt128.Mul64(out r: TPasDblStrUtilsUInt128;
  const a, b: TPasDblStrUtilsUInt64);
{$IF defined(CPUx86_64)}assembler; register; {$IFDEF fpc}nostackframe; {$ENDIF}
asm
  {$IFNDEF fpc}
  .noframe
  {$ENDIF}
  {$IF defined(Windows)}
  // Win64 ABI in-order: rcx rdx r8 r9
  mov rax,rdx
  mul r8
  mov qword ptr [rcx],rax
  mov qword ptr [rcx+8],rdx
  {$ELSE}
  // SysV ABI in-order: rdi rsi rdx rcx r8 r9
  mov rax,rsi
  mul rdx
  mov qword ptr [rdi],rax
  mov qword ptr [rdi+8],rdx
  {$IFEND}
end;
{$ELSE}

var
  u0, u1, v0, v1, t, w0, w1, w2: TPasDblStrUtilsUInt64;
begin
  u1 := a shr 32;
  u0 := a and TPasDblStrUtilsUInt64($FFFFFFFF);
  v1 := b shr 32;
  v0 := b and TPasDblStrUtilsUInt64($FFFFFFFF);
  t := u0 * v0;
  w0 := t and TPasDblStrUtilsUInt64($FFFFFFFF);
  t := (u1 * v0) + (t shr 32);
  w1 := t and TPasDblStrUtilsUInt64($FFFFFFFF);
  w2 := t shr 32;
  t := (u0 * v1) + w1;
  r.Hi := ((u1 * v1) + w2) + (t shr 32);
  r.Lo := (t shl 32) + w0;
end;
{$IFEND}

class function TPasDblStrUtilsUInt128.Mul64(const a, b: TPasDblStrUtilsUInt64)
  : TPasDblStrUtilsUInt128; {$IF defined(CPUx86_64)}assembler; register;
{$IFDEF fpc}nostackframe; {$ENDIF}
asm
  {$IFNDEF fpc}
  .noframe
  {$ENDIF}
  {$IF defined(Windows)}
  // Win64 ABI in-order: rcx rdx r8 r9
  mov rax,rdx
  mul r8
  mov qword ptr [rcx],rax
  mov qword ptr [rcx+8],rdx
  {$ELSE}
  // SysV ABI in-order: rdi rsi rdx rcx r8 r9
  mov rax,rdi
  mul rsi
  {$IFEND}
end;
{$ELSE}

var
  u0, u1, v0, v1, t, w0, w1, w2: TPasDblStrUtilsUInt64;
begin
  u1 := a shr 32;
  u0 := a and TPasDblStrUtilsUInt64($FFFFFFFF);
  v1 := b shr 32;
  v0 := b and TPasDblStrUtilsUInt64($FFFFFFFF);
  t := u0 * v0;
  w0 := t and TPasDblStrUtilsUInt64($FFFFFFFF);
  t := (u1 * v0) + (t shr 32);
  w1 := t and TPasDblStrUtilsUInt64($FFFFFFFF);
  w2 := t shr 32;
  t := (u0 * v1) + w1;
  result.Hi := ((u1 * v1) + w2) + (t shr 32);
  result.Lo := (t shl 32) + w0;
end;
{$IFEND}

class operator TPasDblStrUtilsUInt128.Multiply(const a,
  b: TPasDblStrUtilsUInt128): TPasDblStrUtilsUInt128;
begin
  Mul64(result, a.Lo, b.Lo);
  inc(result.Hi, (a.Hi * b.Lo) + (a.Lo * b.Hi));
end;

class procedure TPasDblStrUtilsUInt128.BinaryDivMod128(Dividend,
  Divisor: TPasDblStrUtilsUInt128;
  out Quotient, Remainder: TPasDblStrUtilsUInt128);
var
  Bit, Shift: TPasDblStrUtilsInt32;
begin
  Quotient := 0;
  Shift := Divisor.CountLeadingZeroBits - Dividend.CountLeadingZeroBits;
  Divisor := Divisor shl Shift;
  for Bit := 0 to Shift do
  begin
    Quotient := Quotient shl 1;
    if Dividend >= Divisor then
    begin
      Dividend := Dividend - Divisor;
      Quotient.Lo := Quotient.Lo or 1;
    end;
    Divisor := Divisor shr 1;
  end;
  Remainder := Dividend;
end;

class procedure TPasDblStrUtilsUInt128.BinaryDivMod64(const Dividend
  : TPasDblStrUtilsUInt128; const Divisor: TPasDblStrUtilsUInt64;
  out Quotient: TPasDblStrUtilsUInt128; out Remainder: TPasDblStrUtilsUInt64);
var
  Bit: TPasDblStrUtilsUInt32;
begin
  Quotient := Dividend;
  Remainder := 0;
  for Bit := 1 to 128 do
  begin
    Remainder := (Remainder shl 1) or
      (ord((Quotient.Hi and $8000000000000000) <> 0) and 1);
    Quotient.Hi := (Quotient.Hi shl 1) or (Quotient.Lo shr 63);
    Quotient.Lo := Quotient.Lo shl 1;
    if (TPasDblStrUtilsUInt32(Remainder shr 32) > TPasDblStrUtilsUInt32
      (Divisor shr 32)) or
      ((TPasDblStrUtilsUInt32(Remainder shr 32) = TPasDblStrUtilsUInt32
      (Divisor shr 32)) and (TPasDblStrUtilsUInt32(Remainder and $FFFFFFFF) >=
      TPasDblStrUtilsUInt32(Divisor and $FFFFFFFF))) then
    begin
      dec(Remainder, Divisor);
      Quotient.Lo := Quotient.Lo or 1;
    end;
  end;
end;

class procedure TPasDblStrUtilsUInt128.DivMod64(const Dividend
  : TPasDblStrUtilsUInt128; const Divisor: TPasDblStrUtilsUInt64;
  out Quotient, Remainder: TPasDblStrUtilsUInt64);
const
  b = TPasDblStrUtilsUInt64(1) shl 32;
var
  u0, u1, v, un1, un0, vn1, vn0, Q1, Q0, un32, un21, un10, rhat, left,
    right: TPasDblStrUtilsUInt64;
  s: NativeInt;
begin
  u0 := Dividend.Lo;
  u1 := Dividend.Hi;
  v := Divisor;
  s := 0;
  while (v and (TPasDblStrUtilsUInt64(1) shl 63)) = 0 do
  begin
    inc(s);
    v := v shl 1;
  end;
  v := Divisor shl s;
  vn1 := v shr 32;
  vn0 := v and $FFFFFFFF;
  if s > 0 then
  begin
    un32 := (u1 shl s) or (u0 shr (64 - s));
    un10 := u0 shl s;
  end
  else
  begin
    un32 := u1;
    un10 := u0;
  end;
  un1 := un10 shr 32;
  un0 := un10 and $FFFFFFFF;
  Q1 := un32 div vn1;
  rhat := un32 mod vn1;
  left := Q1 * vn0;
  right := (rhat shl 32) + un1;
  repeat
    if (Q1 >= b) or (left > right) then
    begin
      dec(Q1);
      inc(rhat, vn1);
      if rhat < b then
      begin
        dec(left, vn0);
        right := (rhat shl 32) or un1;
        continue;
      end;
    end;
    break;
  until false;
  un21 := (un32 shl 32) + (un1 - (Q1 * v));
  Q0 := un21 div vn1;
  rhat := un21 mod vn1;
  left := Q0 * vn0;
  right := (rhat shl 32) or un0;
  repeat
    if (Q0 >= b) or (left > right) then
    begin
      dec(Q0);
      inc(rhat, vn1);
      if rhat < b then
      begin
        dec(left, vn0);
        right := (rhat shl 32) or un0;
        continue;
      end;
    end;
    break;
  until false;
  Remainder := ((un21 shl 32) + (un0 - (Q0 * v))) shr s;
  Quotient := (Q1 shl 32) or Q0;
end;

class procedure TPasDblStrUtilsUInt128.DivMod128Ex(Dividend,
  Divisor: TPasDblStrUtilsUInt128;
  out Quotient, Remainder: TPasDblStrUtilsUInt128);
var
  DivisorLeadingZeroBits: TPasDblStrUtilsInt32;
  v, u, q: TPasDblStrUtilsUInt128;
begin
  if Divisor.Hi = 0 then
  begin
    if Dividend.Hi < Divisor.Lo then
    begin
      Quotient.Hi := 0;
      Remainder.Hi := 0;
      DivMod64(Dividend, Divisor.Lo, Quotient.Lo, Remainder.Lo);
    end
    else
    begin
      Quotient.Hi := Dividend.Hi div Divisor.Lo;
      Dividend.Hi := Dividend.Hi mod Divisor.Lo;
      DivMod64(Dividend, Divisor.Lo, Quotient.Lo, Remainder.Lo);
      Remainder.Hi := 0;
    end;
  end
  else
  begin
    DivisorLeadingZeroBits := Divisor.CountLeadingZeroBits;
    v := Divisor shl DivisorLeadingZeroBits;
    u := Dividend shr 1;
    DivMod64(u, v.Hi, q.Lo, q.Hi);
    q.Hi := 0;
    q := q shr (63 - DivisorLeadingZeroBits);
    if (q.Hi or q.Lo) <> 0 then
    begin
      dec(q);
    end;
    Quotient := q * Divisor;
    Remainder := Dividend - q;
    if Remainder >= Divisor then
    begin
      inc(Quotient);
      Remainder := Remainder - Divisor;
    end;
  end;
end;

class procedure TPasDblStrUtilsUInt128.DivMod128(Dividend,
  Divisor: TPasDblStrUtilsUInt128;
  out Quotient, Remainder: TPasDblStrUtilsUInt128);
var
  DivisorLeadingZeroBits, DividendLeadingZeroBits, DivisorTrailingZeroBits
    : TPasDblStrUtilsInt32;
begin
  DivisorLeadingZeroBits := Divisor.CountLeadingZeroBits;
  DividendLeadingZeroBits := Dividend.CountLeadingZeroBits;
  DivisorTrailingZeroBits := Divisor.CountTrailingZeroBits;
  if DivisorLeadingZeroBits = 128 then
  begin
    Assert(false);
    Quotient.Hi := 0;
    Quotient.Lo := 0;
    Remainder.Hi := 0;
    Remainder.Lo := 0;
  end
  else if (Dividend.Hi or Divisor.Hi) = 0 then
  begin
    Quotient.Hi := 0;
    Remainder.Hi := 0;
    Quotient.Lo := Dividend.Lo div Divisor.Lo;
    Remainder.Lo := Dividend.Lo mod Divisor.Lo;
  end
  else if DivisorLeadingZeroBits = 127 then
  begin
    Quotient := Dividend;
    Remainder.Hi := 0;
    Remainder.Lo := 0;
  end
  else if (DivisorTrailingZeroBits + DivisorLeadingZeroBits) = 127 then
  begin
    Quotient := Dividend shr DivisorTrailingZeroBits;
    dec(Divisor);
    Remainder := Divisor and Dividend;
  end
  else if Dividend < Divisor then
  begin
    Quotient.Hi := 0;
    Quotient.Lo := 0;
    Remainder := Dividend;
  end
  else if Dividend = Divisor then
  begin
    Quotient.Hi := 0;
    Quotient.Lo := 0;
    Remainder.Hi := 0;
    Remainder.Lo := 1;
  end
  else if (DivisorLeadingZeroBits - DividendLeadingZeroBits) > 5 then
  begin
    DivMod128Ex(Dividend, Divisor, Quotient, Remainder);
  end
  else
  begin
    BinaryDivMod128(Dividend, Divisor, Quotient, Remainder);
  end;
end;

class operator TPasDblStrUtilsUInt128.IntDivide(const Dividend
  : TPasDblStrUtilsUInt128; const Divisor: TPasDblStrUtilsUInt64)
  : TPasDblStrUtilsUInt128;
var
  Quotient: TPasDblStrUtilsUInt128;
  Remainder: TPasDblStrUtilsUInt64;
  Bit: TPasDblStrUtilsUInt32;
begin
  if Dividend.Hi = 0 then
  begin
    result.Hi := 0;
    if Dividend < Divisor then
    begin
      result.Lo := 0;
    end
    else
    begin
      result.Lo := Dividend.Lo div Divisor;
    end;
  end
  else
  begin
    Quotient := Dividend;
    Remainder := 0;
    for Bit := 1 to 128 do
    begin
      Remainder := (Remainder shl 1) or
        (ord((Quotient.Hi and $8000000000000000) <> 0) and 1);
      Quotient.Hi := (Quotient.Hi shl 1) or (Quotient.Lo shr 63);
      Quotient.Lo := Quotient.Lo shl 1;
      if (TPasDblStrUtilsUInt32(Remainder shr 32) > TPasDblStrUtilsUInt32
        (Divisor shr 32)) or
        ((TPasDblStrUtilsUInt32(Remainder shr 32) = TPasDblStrUtilsUInt32
        (Divisor shr 32)) and (TPasDblStrUtilsUInt32(Remainder and $FFFFFFFF) >=
        TPasDblStrUtilsUInt32(Divisor and $FFFFFFFF))) then
      begin
        dec(Remainder, Divisor);
        Quotient.Lo := Quotient.Lo or 1;
      end;
    end;
    result := Quotient;
  end;
end;

class operator TPasDblStrUtilsUInt128.IntDivide(const Dividend,
  Divisor: TPasDblStrUtilsUInt128): TPasDblStrUtilsUInt128;
var
  Remainder: TPasDblStrUtilsUInt128;
begin
  TPasDblStrUtilsUInt128.DivMod128(Dividend, Divisor, result, Remainder);
end;

class operator TPasDblStrUtilsUInt128.Modulus(const Dividend
  : TPasDblStrUtilsUInt128; const Divisor: TPasDblStrUtilsUInt64)
  : TPasDblStrUtilsUInt128;
var
  Quotient: TPasDblStrUtilsUInt128;
  Remainder: TPasDblStrUtilsUInt64;
  Bit: TPasDblStrUtilsUInt32;
begin
  if Dividend.Hi = 0 then
  begin
    result.Hi := 0;
    if Dividend < Divisor then
    begin
      result.Lo := Dividend.Lo;
    end
    else
    begin
      result.Lo := Dividend.Lo mod Divisor;
    end;
  end
  else
  begin
    Quotient := Dividend;
    Remainder := 0;
    for Bit := 1 to 128 do
    begin
      Remainder := (Remainder shl 1) or
        (ord((Quotient.Hi and $8000000000000000) <> 0) and 1);
      Quotient.Hi := (Quotient.Hi shl 1) or (Quotient.Lo shr 63);
      Quotient.Lo := Quotient.Lo shl 1;
      if (TPasDblStrUtilsUInt32(Remainder shr 32) > TPasDblStrUtilsUInt32
        (Divisor shr 32)) or
        ((TPasDblStrUtilsUInt32(Remainder shr 32) = TPasDblStrUtilsUInt32
        (Divisor shr 32)) and (TPasDblStrUtilsUInt32(Remainder and $FFFFFFFF) >=
        TPasDblStrUtilsUInt32(Divisor and $FFFFFFFF))) then
      begin
        dec(Remainder, Divisor);
        Quotient.Lo := Quotient.Lo or 1;
      end;
    end;
    result := Remainder;
  end;
end;

class operator TPasDblStrUtilsUInt128.Modulus(const Dividend,
  Divisor: TPasDblStrUtilsUInt128): TPasDblStrUtilsUInt128;
var
  Quotient: TPasDblStrUtilsUInt128;
begin
  TPasDblStrUtilsUInt128.DivMod128(Dividend, Divisor, Quotient, result);
end;

type
  TIEEEFormat = record
    Bytes: TPasDblStrUtilsInt32;
    Mantissa: TPasDblStrUtilsInt32;
    Explicit: TPasDblStrUtilsInt32;
    Exponent: TPasDblStrUtilsInt32;
  end;

  PIEEEFormat = ^TIEEEFormat;

const // exponent bits = round(4*log2(k)) - 13
  IEEEFormat8: TIEEEFormat = (Bytes: 1; Mantissa: 3; Explicit: 0; Exponent: 4);
  IEEEFormat16: TIEEEFormat = (Bytes: 2; Mantissa: 10; Explicit: 0;
    Exponent: 5);
  IEEEFormat32: TIEEEFormat = (Bytes: 4; Mantissa: 23; Explicit: 0;
    Exponent: 8);
  IEEEFormat64: TIEEEFormat = (Bytes: 8; Mantissa: 52; Explicit: 0;
    Exponent: 11);
  IEEEFormat80: TIEEEFormat = (Bytes: 10; Mantissa: 63; Explicit: 1;
    Exponent: 15);
  IEEEFormat128: TIEEEFormat = (Bytes: 16; Mantissa: 112; Explicit: 0;
    Exponent: 15);
  IEEEFormat256: TIEEEFormat = (Bytes: 32; Mantissa: 236; Explicit: 0;
    Exponent: 19);
  IEEEFormat512: TIEEEFormat = (Bytes: 64; Mantissa: 488; Explicit: 0;
    Exponent: 23);

function StringToFloat(const aFloatString: PPasDblStrUtilsChar;
  const aFloatStringLength: TPasDblStrUtilsInt32; out aFloatValue;
  const aIEEEFormat: TIEEEFormat;
  const RoundMode: TPasDblStrUtilsRoundingMode = rmNearest;
  const aDenormalsAreZero: TPasDblStrUtilsBoolean = false;
  const aBase: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsBoolean;
const
  LIMB_BITS = 32;
  LIMB_BYTES = 4;
  LIMB_BYTES_MASK = 3;
  LIMB_BYTES_SHIFT = 2; // 2^2 = 4
  LIMB_SHIFT = 5;
  LIMB_TOP_BIT = TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(1)
    shl (LIMB_BITS - 1));
  LIMB_MASK = TPasDblStrUtilsUInt32(not 0);
  LIMB_ALL_BYTES = TPasDblStrUtilsUInt32($01010101);
  MANT_LIMBS = 24; // 6;
  MANT_DIGITS = 208; // 52;
  FL_ZERO = 0;
  FL_DENORMAL = 1;
  FL_NORMAL = 2;
  FL_INFINITY = 3;
  FL_QNAN = 4;
  FL_SNAN = 5;
type
  PFPLimb = ^TFPLimb;
  TFPLimb = TPasDblStrUtilsUInt32;
  PFPLimbs = ^TFPLimbs;
  TFPLimbs = array [0 .. 65535] of TFPLimb;
  PFP2Limb = ^TFP2Limb;
  TFP2Limb = UInt64;
  PMantissa = ^TMantissa;
  TMantissa = array [0 .. MANT_LIMBS - 1] of TFPLimb;
  function MantissaMultiply(var aMantissaA, aMantissaB: TMantissa)
    : TPasDblStrUtilsInt32;
  var
    i, j: TPasDblStrUtilsInt32;
    n: TFP2Limb;
    Temp: array [0 .. (MANT_DIGITS * 2)] of TFP2Limb;
  begin
    for i := low(Temp) to high(Temp) do
    begin
      Temp[i] := 0;
    end;
    for i := 0 to MANT_LIMBS - 1 do
    begin
      for j := 0 to MANT_LIMBS - 1 do
      begin
        n := TFP2Limb(aMantissaA[i]) * TFP2Limb(aMantissaB[j]);
        inc(Temp[i + j], n shr LIMB_BITS);
        inc(Temp[i + j + 1], TFPLimb(n and LIMB_MASK));
      end;
    end;
    for i := (MANT_LIMBS * 2) downto 1 do
    begin
      inc(Temp[i - 1], Temp[i] shr LIMB_BITS);
      Temp[i] := Temp[i] and LIMB_MASK;
    end;
    if (Temp[0] and LIMB_TOP_BIT) <> 0 then
    begin
      for i := 0 to MANT_LIMBS - 1 do
      begin
        aMantissaA[i] := Temp[i] and LIMB_MASK;
      end;
      result := 0;
    end
    else
    begin
      for i := 0 to MANT_LIMBS - 1 do
      begin
        aMantissaA[i] := (Temp[i] shl 1) or
          (ord((Temp[i + 1] and LIMB_TOP_BIT) <> 0) and 1);
      end;
      result := -1;
    end;
  end;
  function ReadExponent(const aExponentStringValue: PPasDblStrUtilsChar;
    const aExponentStringLength, aExponentStringStartPosition,
    aMaxValue: TPasDblStrUtilsInt32): TPasDblStrUtilsInt32;
  var
    ExponentStringPosition: TPasDblStrUtilsInt32;
    Negative: TPasDblStrUtilsBoolean;
  begin
    result := 0;
    Negative := false;
    ExponentStringPosition := aExponentStringStartPosition;
    if (ExponentStringPosition < aExponentStringLength) and
      (aExponentStringValue[ExponentStringPosition] = '+') then
    begin
      inc(ExponentStringPosition);
    end
    else if (ExponentStringPosition < aExponentStringLength) and
      (aExponentStringValue[ExponentStringPosition] = '-') then
    begin
      inc(ExponentStringPosition);
      Negative := true;
    end;
    while ExponentStringPosition < aExponentStringLength do
    begin
      case aExponentStringValue[ExponentStringPosition] of
        '0' .. '9':
          begin
            if result < aMaxValue then
            begin
              result := (result * 10) +
                (TPasDblStrUtilsUInt8(AnsiChar(aExponentStringValue
                [ExponentStringPosition])) - TPasDblStrUtilsUInt8
                (AnsiChar('0')));
              if result > aMaxValue then
              begin
                result := aMaxValue;
              end;
            end;
          end;
      else
        begin
          result := $7FFFFFFF;
          exit;
        end;
      end;
      inc(ExponentStringPosition);
    end;
    if Negative then
    begin
      result := -result;
    end;
  end;
  function ProcessDecimal(const aFloatStringValue: PPasDblStrUtilsChar;
    const aFloatStringLength: TPasDblStrUtilsInt32;
    const aFloatStringStartPosition: TPasDblStrUtilsInt32;
    out aMantissa: TMantissa; var aExponent: TPasDblStrUtilsInt32)
    : TPasDblStrUtilsBoolean;
  var
    FloatStringPosition, TenPower, TwoPower, ExtraTwos, ExponentValue,
      MantissaPosition, DigitPos, StoredDigitPos, DigitPosBackwards,
      Value: TPasDblStrUtilsInt32;
    Bit, Carry: TFPLimb;
    Started, SeenDot { ,Warned } : TPasDblStrUtilsBoolean;
    // m:PFPLimb;
    Digits: array [0 .. MANT_DIGITS - 1] of TPasDblStrUtilsUInt8;
    Mult: TMantissa;
  begin
    // Warned:=false;
    TenPower := 0;
    DigitPos := 0;
    Started := false;
    SeenDot := false;
    FloatStringPosition := aFloatStringStartPosition;
    while FloatStringPosition < aFloatStringLength do
    begin
      case aFloatStringValue[FloatStringPosition] of
        '.':
          begin
            if SeenDot then
            begin
              result := false;
              exit;
            end
            else
            begin
              SeenDot := true;
            end;
          end;
        '0' .. '9':
          begin
            if (aFloatStringValue[FloatStringPosition] = '0') and not Started
            then
            begin
              if SeenDot then
              begin
                dec(TenPower);
              end;
            end
            else
            begin
              Started := true;
              if DigitPos < MANT_DIGITS then
              begin
                Digits[DigitPos] := TPasDblStrUtilsUInt8
                  (AnsiChar(aFloatStringValue[FloatStringPosition])) -
                  TPasDblStrUtilsUInt8(AnsiChar('0'));
                inc(DigitPos);
              end
              else
              begin
                // Warned:=true;
              end;
              if not SeenDot then
              begin
                inc(TenPower);
              end;
            end;
          end;
        'e', 'E':
          begin
            break;
          end;
      else
        begin
          result := false;
          exit;
        end;
      end;
      inc(FloatStringPosition);
    end;
    if FloatStringPosition < aFloatStringLength then
    begin
      if aFloatStringValue[FloatStringPosition] in ['e', 'E'] then
      begin
        inc(FloatStringPosition);
        ExponentValue := ReadExponent(aFloatStringValue, aFloatStringLength,
          FloatStringPosition, 5000);
        if ExponentValue = $7FFFFFFF then
        begin
          result := false;
          exit;
        end;
        inc(TenPower, ExponentValue);
      end
      else
      begin
        result := false;
        exit;
      end;
    end;
    for MantissaPosition := 0 to MANT_LIMBS - 1 do
    begin
      aMantissa[MantissaPosition] := 0;
    end;
    Bit := LIMB_TOP_BIT;
    StoredDigitPos := 0;
    Started := false;
    TwoPower := 0;
    MantissaPosition := 0;
    while MantissaPosition < MANT_LIMBS do
    begin
      Carry := 0;
      while (DigitPos > StoredDigitPos) and (Digits[DigitPos - 1] = 0) do
      begin
        dec(DigitPos);
      end;
      if DigitPos <= StoredDigitPos then
      begin
        break;
      end;
      DigitPosBackwards := DigitPos;
      while DigitPosBackwards > StoredDigitPos do
      begin
        dec(DigitPosBackwards);
        Value := (2 * Digits[DigitPosBackwards]) + Carry;
        if Value >= 10 then
        begin
          dec(Value, 10);
          Carry := 1;
        end
        else
        begin
          Carry := 0;
        end;
        Digits[DigitPosBackwards] := Value;
      end;
      if Carry <> 0 then
      begin
        aMantissa[MantissaPosition] := aMantissa[MantissaPosition] or Bit;
        Started := true;
      end;
      if Started then
      begin
        if Bit = 1 then
        begin
          Bit := LIMB_TOP_BIT;
          inc(MantissaPosition);
        end
        else
        begin
          Bit := Bit shr 1;
        end;
      end
      else
      begin
        dec(TwoPower);
      end;
    end;
    inc(TwoPower, TenPower);
    if TenPower < 0 then
    begin
      for MantissaPosition := 0 to MANT_LIMBS - 2 do
      begin
        Mult[MantissaPosition] := (TPasDblStrUtilsUInt32($CC) * LIMB_ALL_BYTES);
      end;
      Mult[MANT_LIMBS - 1] := (TPasDblStrUtilsUInt32($CC) * LIMB_ALL_BYTES) + 1;
      ExtraTwos := -2;
      TenPower := -TenPower;
    end
    else if TenPower > 0 then
    begin
      Mult[0] := TPasDblStrUtilsUInt32(5) shl (LIMB_BITS - 3);
      for MantissaPosition := 1 to MANT_LIMBS - 1 do
      begin
        Mult[MantissaPosition] := 0;
      end;
      ExtraTwos := 3;
    end
    else
    begin
      ExtraTwos := 0;
    end;
    while TenPower <> 0 do
    begin
      if (TenPower and 1) <> 0 then
      begin
        inc(TwoPower, ExtraTwos + MantissaMultiply(aMantissa, Mult));
      end;
      inc(ExtraTwos, ExtraTwos + MantissaMultiply(Mult, Mult));
      TenPower := TenPower shr 1;
    end;
    aExponent := TwoPower;
    result := true;
  end;
  function ProcessNonDecimal(const aFloatStringValue: PPasDblStrUtilsChar;
    const aFloatStringLength: TPasDblStrUtilsInt32;
    const aFloatStringStartPosition, aBits: TPasDblStrUtilsInt32;
    out aMantissa: TMantissa; var aExponent: TPasDblStrUtilsInt32)
    : TPasDblStrUtilsBoolean;
  const
    Log2Table: array [0 .. 15] of TPasDblStrUtilsInt32 = (-1, 0, 1, 1, 2, 2, 2,
      2, 3, 3, 3, 3, 3, 3, 3, 3);
  var
    FloatStringPosition, TwoPower, ExponentValue, MantissaPosition, Value,
      Radix, MantissaShift, l: TPasDblStrUtilsInt32;
    SeenDigit, SeenDot: TPasDblStrUtilsBoolean;
    MantissaPointer: PFPLimb;
    Mult: array [0 .. MANT_LIMBS] of TFPLimb;
  begin
    for MantissaPosition := 0 to MANT_LIMBS do
    begin
      Mult[MantissaPosition] := 0;
    end;
    Radix := 1 shl aBits;
    TwoPower := 0;
    MantissaShift := 0;
    MantissaPointer := @Mult[0];
    SeenDigit := false;
    SeenDot := false;
    FloatStringPosition := aFloatStringStartPosition;
    while FloatStringPosition < aFloatStringLength do
    begin
      case aFloatStringValue[FloatStringPosition] of
        '.':
          begin
            if SeenDot then
            begin
              result := false;
              exit;
            end
            else
            begin
              SeenDot := true;
            end;
          end;
        '0' .. '9', 'a' .. 'f', 'A' .. 'F':
          begin
            Value := TPasDblStrUtilsUInt8
              (AnsiChar(aFloatStringValue[FloatStringPosition]));
            if Value in [TPasDblStrUtilsUInt8(AnsiChar('0'))
              .. TPasDblStrUtilsUInt8(AnsiChar('9'))] then
            begin
              dec(Value, TPasDblStrUtilsUInt8(AnsiChar('0')));
            end
            else if Value in [TPasDblStrUtilsUInt8(AnsiChar('a'))
              .. TPasDblStrUtilsUInt8(AnsiChar('f'))] then
            begin
              Value := (Value - TPasDblStrUtilsUInt8(AnsiChar('a'))) + $A;
            end
            else if Value in [TPasDblStrUtilsUInt8(AnsiChar('A'))
              .. TPasDblStrUtilsUInt8(AnsiChar('F'))] then
            begin
              Value := (Value - TPasDblStrUtilsUInt8(AnsiChar('A'))) + $A;
            end
            else
            begin
              result := false;
              exit;
            end;
            if Value < Radix then
            begin
              if (Value <> 0) and not SeenDigit then
              begin
                l := Log2Table[Value];
                SeenDigit := true;
                MantissaPointer := @Mult[0];
                MantissaShift := (LIMB_BITS - 1) - l;
                if SeenDot then
                begin
                  TwoPower := (TwoPower - aBits) + l;
                end
                else
                begin
                  TwoPower := (l + 1) - aBits;
                end;
              end;
              if SeenDigit then
              begin
                if MantissaShift <= 0 then
                begin
                  MantissaPointer^ := MantissaPointer^ or
                    TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(Value)
                    shr TPasDblStrUtilsUInt32(-MantissaShift));
                  inc(MantissaPointer);
                  if TPasDblStrUtilsPtrUInt(MantissaPointer) >
                    TPasDblStrUtilsPtrUInt(Pointer(@Mult[MANT_LIMBS])) then
                  begin
                    MantissaPointer := @Mult[MANT_LIMBS];
                  end;
                  inc(MantissaShift, LIMB_BITS);
                end;
                MantissaPointer^ := MantissaPointer^ or
                  TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(Value)
                  shl TPasDblStrUtilsUInt32(MantissaShift));
                dec(MantissaShift, aBits);
                if not SeenDot then
                begin
                  inc(TwoPower, aBits);
                end;
              end
              else
              begin
                if SeenDot then
                begin
                  dec(TwoPower, aBits);
                end;
              end;
            end
            else
            begin
              result := false;
              exit;
            end;
          end;
        'p', 'P':
          begin
            break;
          end;
      else
        begin
          result := false;
          exit;
        end;
      end;
      inc(FloatStringPosition);
    end;
    if FloatStringPosition < aFloatStringLength then
    begin
      if aFloatStringValue[FloatStringPosition] in ['p', 'P'] then
      begin
        inc(FloatStringPosition);
        ExponentValue := ReadExponent(aFloatStringValue, aFloatStringLength,
          FloatStringPosition, 20000);
        if ExponentValue = $7FFFFFFF then
        begin
          result := false;
          exit;
        end;
        inc(TwoPower, ExponentValue);
      end
      else
      begin
        result := false;
        exit;
      end;
    end;
    if SeenDigit then
    begin
      for MantissaPosition := 0 to MANT_LIMBS - 1 do
      begin
        aMantissa[MantissaPosition] := Mult[MantissaPosition];
      end;
      aExponent := TwoPower;
    end
    else
    begin
      for MantissaPosition := 0 to MANT_LIMBS - 1 do
      begin
        aMantissa[MantissaPosition] := 0;
      end;
      aExponent := 0;
    end;
    result := true;
  end;
  procedure MantissaShiftRight(var aMantissa: TMantissa;
    const aShift: TPasDblStrUtilsInt32);
  var
    Next, Current: TFPLimb;
    Index, ShiftRight, ShiftLeft, ShiftOffset: TPasDblStrUtilsInt32;
  begin
    Index := 0;
    ShiftRight := aShift and (LIMB_BITS - 1);
    ShiftLeft := LIMB_BITS - ShiftRight;
    ShiftOffset := aShift shr LIMB_SHIFT;
    if ShiftRight = 0 then
    begin
      if ShiftOffset <> 0 then
      begin
        Index := MANT_LIMBS - 1;
        while Index >= ShiftOffset do
        begin
          aMantissa[Index] := aMantissa[Index - ShiftOffset];
          dec(Index);
        end;
      end;
    end
    else
    begin
      Next := aMantissa[(MANT_LIMBS - 1) - ShiftOffset] shr ShiftRight;
      Index := MANT_LIMBS - 1;
      while Index > ShiftOffset do
      begin
        Current := aMantissa[(Index - ShiftOffset) - 1];
        aMantissa[Index] := (Current shl ShiftLeft) or Next;
        Next := Current shr ShiftRight;
        dec(Index);
      end;
      aMantissa[Index] := Next;
      dec(Index);
    end;
    while Index >= 0 do
    begin
      aMantissa[Index] := 0;
      dec(Index);
    end;
  end;
  procedure MantissaSetBit(var aMantissa: TMantissa;
    const aBit: TPasDblStrUtilsInt32); {$IFDEF caninline}inline; {$ENDIF}
  begin
    aMantissa[aBit shr LIMB_SHIFT] := aMantissa[aBit shr LIMB_SHIFT] or
      (LIMB_TOP_BIT shr (aBit and (LIMB_BITS - 1)));
  end;
  function MantissaTestBit(const aMantissa: TMantissa;
    const aBit: TPasDblStrUtilsInt32): TPasDblStrUtilsBoolean;
  {$IFDEF caninline}inline; {$ENDIF}
  begin
    result := ((aMantissa[aBit shr LIMB_SHIFT] shr ((not aBit) and
      (LIMB_BITS - 1))) and 1) <> 0;
  end;
  function MantissaIsZero(const aMantissa: TMantissa): TPasDblStrUtilsBoolean;
  var
    Index: TPasDblStrUtilsInt32;
  begin
    result := true;
    for Index := 0 to MANT_LIMBS - 1 do
    begin
      if aMantissa[Index] <> 0 then
      begin
        result := false;
        exit;
      end;
    end;
  end;
  procedure MantissaRound(const Negative: TPasDblStrUtilsBoolean;
    var Mantissa: TMantissa; const BitPos: TPasDblStrUtilsInt32);
  var
    Index, IndexSubBitPos: TPasDblStrUtilsInt32;
    Bit: TFPLimb;
    function RoundAbsDown: TPasDblStrUtilsBoolean;
    var
      OtherIndex: TPasDblStrUtilsInt32;
    begin
      Mantissa[Index] := Mantissa[Index] and not(Bit - 1);
      for OtherIndex := Index + 1 to MANT_LIMBS - 1 do
      begin
        Mantissa[OtherIndex] := 0;
      end;
      result := false;
    end;
    function RoundAbsUp: TPasDblStrUtilsBoolean;
    var
      OtherIndex: TPasDblStrUtilsInt32;
    begin
      Mantissa[Index] := (Mantissa[Index] and not(Bit - 1)) + Bit;
      for OtherIndex := Index + 1 to MANT_LIMBS - 1 do
      begin
        Mantissa[OtherIndex] := 0;
      end;
      while (Index > 0) and (Mantissa[Index] = 0) do
      begin
        dec(Index);
        inc(Mantissa[Index]);
      end;
      result := Mantissa[0] = 0;
    end;
    function RoundTowardsInfinity: TPasDblStrUtilsBoolean;
    var
      OtherIndex: TPasDblStrUtilsInt32;
      m: TFPLimb;
    begin
      m := Mantissa[Index] and ((Bit shl 1) - 1);
      for OtherIndex := Index + 1 to MANT_LIMBS - 1 do
      begin
        m := m or Mantissa[OtherIndex];
      end;
      if m <> 0 then
      begin
        result := RoundAbsUp;
      end
      else
      begin
        result := RoundAbsDown;
      end;
    end;
    function RoundNear: TPasDblStrUtilsBoolean;
    var
      j: TPasDblStrUtilsInt32;
      m: TPasDblStrUtilsUInt32;
    begin
      if (Mantissa[Index] and Bit) <> 0 then
      begin
        Mantissa[Index] := Mantissa[Index] and not Bit;
        m := Mantissa[Index] and ((Bit shl 1) - 1);
        for j := Index + 1 to MANT_LIMBS - 1 do
        begin
          m := m or Mantissa[j];
        end;
        Mantissa[Index] := Mantissa[Index] or Bit;
        if m <> 0 then
        begin
          result := RoundAbsUp;
        end
        else
        begin
          if MantissaTestBit(Mantissa, BitPos - 1) then
          begin
            result := RoundAbsUp;
          end
          else
          begin
            result := RoundAbsDown;
          end;
        end;
      end
      else
      begin
        result := RoundAbsDown;
      end;
    end;

  begin
    Index := BitPos shr LIMB_SHIFT;
    IndexSubBitPos := BitPos and (LIMB_BITS - 1);
    Bit := LIMB_TOP_BIT shr IndexSubBitPos;
    case RoundMode of
      rmNearest:
        begin
          result := RoundNear;
        end;
      rmTruncate:
        begin
          result := RoundAbsDown;
        end;
      rmUp:
        begin
          if Negative then
          begin
            result := RoundAbsDown;
          end
          else
          begin
            result := RoundTowardsInfinity;
          end;
        end;
      rmDown:
        begin
          if Negative then
          begin
            result := RoundAbsUp;
          end
          else
          begin
            result := RoundTowardsInfinity;
          end;
        end;
    else
      begin
        result := false;
      end;
    end;
  end;
  function ProcessToPackedBCD(const aFloatStringValue: PPasDblStrUtilsChar;
    const aFloatStringLength: TPasDblStrUtilsInt32;
    const aFloatStringStartPosition: TPasDblStrUtilsInt32;
    aResultBytes: PPasDblStrUtilsUInt8; const aNegative: TPasDblStrUtilsBoolean)
    : TPasDblStrUtilsBoolean;
  var
    FloatStringPosition, Count, LoValue, Value: TPasDblStrUtilsInt32;
  begin
    result := false;
    if aIEEEFormat.Bytes <> 10 then
    begin
      exit;
    end;
    FloatStringPosition := aFloatStringStartPosition;
    while FloatStringPosition < aFloatStringLength do
    begin
      case aFloatString[FloatStringPosition] of
        '0' .. '9':
          begin
            inc(FloatStringPosition);
          end;
      else
        begin
          exit;
        end;
      end;
    end;
    LoValue := -1;
    Count := 0;
    while FloatStringPosition > aFloatStringStartPosition do
    begin
      dec(FloatStringPosition);
      Value := TPasDblStrUtilsUInt8
        (AnsiChar(aFloatStringValue[FloatStringPosition])) -
        TPasDblStrUtilsUInt8(AnsiChar('0'));
      if LoValue < 0 then
      begin
        LoValue := Value;
      end
      else
      begin
        if Count < 9 then
        begin
          aResultBytes^ := LoValue or (Value shl 4);
          inc(aResultBytes);
        end;
        inc(Count);
        LoValue := -1;
      end;
    end;
    if LoValue >= 0 then
    begin
      if Count < 9 then
      begin
        aResultBytes^ := LoValue;
        inc(aResultBytes);
      end;
      inc(Count);
    end;
    while Count < 9 do
    begin
      aResultBytes^ := 0;
      inc(aResultBytes);
      inc(Count);
    end;
    if aNegative then
    begin
      aResultBytes^ := $80;
    end
    else
    begin
      aResultBytes^ := 0;
    end;
    result := true;
  end;

var
  OK: TPasDblStrUtilsBoolean;
  FloatStringPosition, Exponent, ExpMax, FloatType, Shift, Bits, OnePos,
    i: TPasDblStrUtilsInt32;
  OneMask: TFPLimb;
  Negative: TPasDblStrUtilsBoolean;
  Mantissa: TMantissa;
  b: PPasDblStrUtilsUInt8;
begin
  result := false;
  Bits := aIEEEFormat.Bytes shl 3;
  OneMask := LIMB_TOP_BIT shr ((aIEEEFormat.Explicit + aIEEEFormat.Exponent) and
    (LIMB_BITS - 1));
  OnePos := (aIEEEFormat.Explicit + aIEEEFormat.Exponent) shr LIMB_SHIFT;
  FloatStringPosition := 0;
  while (FloatStringPosition < aFloatStringLength) and
    (aFloatString[FloatStringPosition] in [#1 .. #32]) do
  begin
    inc(FloatStringPosition);
  end;
  Negative := false;
  while (FloatStringPosition < aFloatStringLength) and
    (aFloatString[FloatStringPosition] in ['+', '-']) do
  begin
    if aFloatString[FloatStringPosition] = '-' then
    begin
      Negative := not Negative;
    end;
    inc(FloatStringPosition);
  end;
  ExpMax := 1 shl (aIEEEFormat.Exponent - 1);
  if ((FloatStringPosition + 2) < aFloatStringLength) and
    ((aFloatString[FloatStringPosition] in ['I', 'i']) and
    (aFloatString[FloatStringPosition + 1] in ['N', 'n']) and
    (aFloatString[FloatStringPosition + 2] in ['F', 'f'])) then
  begin
    FloatType := FL_INFINITY;
  end
  else if ((FloatStringPosition + 2) < aFloatStringLength) and
    ((aFloatString[FloatStringPosition] in ['N', 'n']) and
    (aFloatString[FloatStringPosition + 1] in ['A', 'a']) and
    (aFloatString[FloatStringPosition + 2] in ['N', 'n'])) then
  begin
    FloatType := FL_QNAN;
  end
  else if ((FloatStringPosition + 3) < aFloatStringLength) and
    ((aFloatString[FloatStringPosition] in ['S', 's']) and
    (aFloatString[FloatStringPosition + 1] in ['N', 'n']) and
    (aFloatString[FloatStringPosition + 2] in ['A', 'a']) and
    (aFloatString[FloatStringPosition + 3] in ['N', 'n'])) then
  begin
    FloatType := FL_SNAN;
  end
  else if ((FloatStringPosition + 3) < aFloatStringLength) and
    ((aFloatString[FloatStringPosition] in ['Q', 'q']) and
    (aFloatString[FloatStringPosition + 1] in ['N', 'n']) and
    (aFloatString[FloatStringPosition + 2] in ['A', 'a']) and
    (aFloatString[FloatStringPosition + 3] in ['N', 'n'])) then
  begin
    FloatType := FL_QNAN;
  end
  else
  begin
    case aBase of
      2:
        begin
          OK := ProcessNonDecimal(aFloatString, aFloatStringLength,
            FloatStringPosition, 1, Mantissa, Exponent);
        end;
      4:
        begin
          OK := ProcessNonDecimal(aFloatString, aFloatStringLength,
            FloatStringPosition, 2, Mantissa, Exponent);
        end;
      8:
        begin
          OK := ProcessNonDecimal(aFloatString, aFloatStringLength,
            FloatStringPosition, 3, Mantissa, Exponent);
        end;
      10:
        begin
          OK := ProcessDecimal(aFloatString, aFloatStringLength,
            FloatStringPosition, Mantissa, Exponent);
        end;
      16:
        begin
          OK := ProcessNonDecimal(aFloatString, aFloatStringLength,
            FloatStringPosition, 4, Mantissa, Exponent);
        end;
    else
      begin
        if ((FloatStringPosition + 1) < aFloatStringLength) and
          ((aFloatString[FloatStringPosition] = '0') and
          (aFloatString[FloatStringPosition + 1] in ['h', 'H', 'x', 'X'])) then
        begin
          inc(FloatStringPosition, 2);
          OK := ProcessNonDecimal(aFloatString, aFloatStringLength,
            FloatStringPosition, 4, Mantissa, Exponent);
        end
        else if ((FloatStringPosition + 1) < aFloatStringLength) and
          ((aFloatString[FloatStringPosition] = '0') and
          (aFloatString[FloatStringPosition + 1] in ['o', 'O', 'q', 'Q'])) then
        begin
          inc(FloatStringPosition, 2);
          OK := ProcessNonDecimal(aFloatString, aFloatStringLength,
            FloatStringPosition, 3, Mantissa, Exponent);
        end
        else if ((FloatStringPosition + 1) < aFloatStringLength) and
          ((aFloatString[FloatStringPosition] = '0') and
          (aFloatString[FloatStringPosition + 1] in ['b', 'B', 'y', 'Y'])) then
        begin
          inc(FloatStringPosition, 2);
          OK := ProcessNonDecimal(aFloatString, aFloatStringLength,
            FloatStringPosition, 1, Mantissa, Exponent);
        end
        else if ((FloatStringPosition + 1) < aFloatStringLength) and
          ((aFloatString[FloatStringPosition] = '0') and
          (aFloatString[FloatStringPosition + 1] in ['d', 'D', 't', 'T'])) then
        begin
          inc(FloatStringPosition, 2);
          OK := ProcessDecimal(aFloatString, aFloatStringLength,
            FloatStringPosition, Mantissa, Exponent);
        end
        else if ((FloatStringPosition + 1) < aFloatStringLength) and
          ((aFloatString[FloatStringPosition] = '0') and
          (aFloatString[FloatStringPosition + 1] in ['p', 'P'])) then
        begin
          inc(FloatStringPosition, 2);
          result := ProcessToPackedBCD(aFloatString, aFloatStringLength,
            FloatStringPosition, Pointer(@aFloatValue), Negative);
          exit;
        end
        else if (FloatStringPosition < aFloatStringLength) and
          (aFloatString[FloatStringPosition] = '$') then
        begin
          inc(FloatStringPosition);
          OK := ProcessNonDecimal(aFloatString, aFloatStringLength,
            FloatStringPosition, 4, Mantissa, Exponent);
        end
        else if (FloatStringPosition < aFloatStringLength) and
          (aFloatString[FloatStringPosition] = '&') then
        begin
          inc(FloatStringPosition);
          OK := ProcessNonDecimal(aFloatString, aFloatStringLength,
            FloatStringPosition, 3, Mantissa, Exponent);
        end
        else if (FloatStringPosition < aFloatStringLength) and
          (aFloatString[FloatStringPosition] = '%') then
        begin
          inc(FloatStringPosition);
          OK := ProcessNonDecimal(aFloatString, aFloatStringLength,
            FloatStringPosition, 1, Mantissa, Exponent);
        end
        else
        begin
          OK := ProcessDecimal(aFloatString, aFloatStringLength,
            FloatStringPosition, Mantissa, Exponent);
        end;
      end;
    end;
    if OK then
    begin
      if (Mantissa[0] and LIMB_TOP_BIT) <> 0 then
      begin
        dec(Exponent);
        if (Exponent >= (2 - ExpMax)) and (Exponent <= ExpMax) then
        begin
          FloatType := FL_NORMAL;
        end
        else if Exponent > 0 then
        begin
          FloatType := FL_INFINITY;
        end
        else
        begin
          FloatType := FL_DENORMAL;
        end;
      end
      else
      begin
        FloatType := FL_ZERO;
      end;
    end
    else
    begin
      FloatType := FL_QNAN;
    end;
  end;
  repeat
    case FloatType of
      FL_ZERO:
        begin
          FillChar(Mantissa, sizeof(Mantissa), #0);
        end;
      FL_DENORMAL:
        begin
          Shift := aIEEEFormat.Explicit -
            ((Exponent + ExpMax) - (aIEEEFormat.Exponent + 2));
          MantissaShiftRight(Mantissa, Shift);
          MantissaRound(Negative, Mantissa, Bits);
          if (Mantissa[OnePos] and OneMask) <> 0 then
          begin
            Exponent := 1;
            if aIEEEFormat.Explicit = 0 then
            begin
              Mantissa[OnePos] := Mantissa[OnePos] and not OneMask;
            end;
            Mantissa[0] := Mantissa[0] or
              (TPasDblStrUtilsUInt32(Exponent)
              shl ((LIMB_BITS - 1) - aIEEEFormat.Exponent));
          end
          else
          begin
            if aDenormalsAreZero or MantissaIsZero(Mantissa) then
            begin
              FloatType := FL_ZERO;
              continue;
            end;
          end;
        end;
      FL_NORMAL:
        begin
          inc(Exponent, ExpMax - 1);
          MantissaShiftRight(Mantissa, aIEEEFormat.Exponent +
            aIEEEFormat.Explicit);
          MantissaRound(Negative, Mantissa, Bits);
          if MantissaTestBit(Mantissa,
            (aIEEEFormat.Exponent + aIEEEFormat.Explicit) - 1) then
          begin
            MantissaShiftRight(Mantissa, 1);
            inc(Exponent);
            if Exponent >= ((ExpMax shl 1) - 1) then
            begin
              FloatType := FL_INFINITY;
              continue;
            end;
          end;
          if aIEEEFormat.Explicit = 0 then
          begin
            Mantissa[OnePos] := Mantissa[OnePos] and not OneMask;
          end;
          Mantissa[0] := Mantissa[0] or
            (TPasDblStrUtilsUInt32(Exponent) shl ((LIMB_BITS - 1) -
            aIEEEFormat.Exponent));
        end;
      FL_INFINITY, FL_QNAN, FL_SNAN:
        begin
          FillChar(Mantissa, sizeof(Mantissa), #0);
          Mantissa[0] := ((TPasDblStrUtilsUInt32(1) shl aIEEEFormat.Exponent) -
            1) shl ((LIMB_BITS - 1) - aIEEEFormat.Exponent);
          if aIEEEFormat.Explicit <> 0 then
          begin
            Mantissa[OnePos] := Mantissa[OnePos] or OneMask;
          end;
          case FloatType of
            FL_QNAN:
              begin
                MantissaSetBit(Mantissa, aIEEEFormat.Exponent +
                  aIEEEFormat.Explicit + 1);
              end;
            FL_SNAN:
              begin
                MantissaSetBit(Mantissa, aIEEEFormat.Exponent +
                  aIEEEFormat.Explicit + aIEEEFormat.Mantissa);
              end;
          end;
        end;
    end;
    break;
  until false;
  if Negative then
  begin
    Mantissa[0] := Mantissa[0] or LIMB_TOP_BIT;
  end;
  b := @aFloatValue;
  for i := aIEEEFormat.Bytes - 1 downto 0 do
  begin
    b^ := Mantissa[i shr LIMB_BYTES_SHIFT]
      shr ((LIMB_BYTES_MASK - (i and LIMB_BYTES_MASK)) shl 3);
    inc(b);
  end;
  result := true;
end;

{$IF defined(CPU64) or defined(CPUx86_64) or defined(CPUAArch64)}

function Div5(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := x div 5;
end;

function Div10(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := x div 10;
end;

function RoundDiv10(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := (x div 10) + (ord((x mod 10) >= 5) and 1);
end;

function Div100(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := x div 100;
end;

function Div1e8(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := x div TPasDblStrUtilsUInt64(100000000);
end;

function Div1e9(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := x div TPasDblStrUtilsUInt64(1000000000);
end;

function Mod1e9(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := TPasDblStrUtilsUInt32((x - (1000000000 * Div1e9(x))) and $FFFFFFFF);
end;
{$ELSE}

function UMulH(const a, b: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
var
  u0, u1, v0, v1, t, w1, w2: TPasDblStrUtilsUInt64;
begin
  u1 := a shr 32;
  u0 := a and UInt64($FFFFFFFF);
  v1 := b shr 32;
  v0 := b and UInt64($FFFFFFFF);
  t := u0 * v0;
  t := (u1 * v0) + (t shr 32);
  w1 := t and UInt64($FFFFFFFF);
  w2 := t shr 32;
  t := (u0 * v1) + w1;
  result := ((u1 * v1) + w2) + (t shr 32);
end;

function Div5(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
begin
  result := UMulH(x, TPasDblStrUtilsUInt64($CCCCCCCCCCCCCCCD)) shr 2;
end;

function Div10(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
begin
  result := UMulH(x, TPasDblStrUtilsUInt64($CCCCCCCCCCCCCCCD)) shr 3;
end;

function RoundDiv10(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
{$IFDEF caninline}inline; {$ENDIF}
begin
  result := UMulH(x, TPasDblStrUtilsUInt64($CCCCCCCCCCCCCCCD)) shr 3;
  inc(result, ord((x - (10 * result)) >= 5) and 1);
end;

function Div100(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
begin
  result := UMulH(x shr 2, TPasDblStrUtilsUInt64($28F5C28F5C28F5C3)) shr 2;
end;

function Div1e8(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
begin
  result := UMulH(x, TPasDblStrUtilsUInt64($ABCC77118461CEFD)) shr 26;
end;

function Div1e9(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
begin
  result := UMulH(x shr 9, TPasDblStrUtilsUInt64($44B82FA09B5A53)) shr 11;
end;

function Mod1e9(const x: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt64;
begin
  result := TPasDblStrUtilsUInt32(x and $FFFFFFFF) - TPasDblStrUtilsUInt32
    (1000000000 * TPasDblStrUtilsUInt32(Div1e9(x) and $FFFFFFFF));
end;
{$IFEND}

function Pow5Factor(aValue: TPasDblStrUtilsUInt64): TPasDblStrUtilsUInt32;
const
  Inv5: TPasDblStrUtilsUInt64 = TPasDblStrUtilsUInt64(14757395258967641293);
  nDiv5: TPasDblStrUtilsUInt64 = TPasDblStrUtilsUInt64(3689348814741910323);
begin
  result := 0;
  repeat
    Assert(aValue <> 0);
    aValue := aValue * Inv5;
    if aValue > nDiv5 then
    begin
      break;
    end
    else
    begin
      inc(result);
    end;
  until false;
end;

function MultipleOfPowerOf5(const aValue: TPasDblStrUtilsUInt64;
  const aP: TPasDblStrUtilsUInt32): Boolean;
begin
  result := Pow5Factor(aValue) >= aP;
end;

function MultipleOfPowerOf2(const aValue: TPasDblStrUtilsUInt64;
  const aP: TPasDblStrUtilsUInt32): Boolean;
begin
  Assert(aValue <> 0);
  Assert(aP < 64);
  result := (aValue and ((TPasDblStrUtilsUInt64(1) shl aP) - 1)) = 0;
end;

function MulShift64(const aM: TPasDblStrUtilsUInt64;
  const aMul: PPasDblStrUtilsUInt64; const aJ: TPasDblStrUtilsInt32)
  : TPasDblStrUtilsUInt64;
type
  TPasDblStrUtilsUInt64s = array [0 .. 1] of TPasDblStrUtilsUInt64;
  PPasDblStrUtilsUInt64s = ^TPasDblStrUtilsUInt64s;
{$IF declared(TPasDblStrUtilsUInt128)}
begin
  result := TPasDblStrUtilsUInt64
    (((TPasDblStrUtilsUInt128.Mul64(aM, PPasDblStrUtilsUInt64s(aMul)^[0])
    shr 64) + TPasDblStrUtilsUInt128.Mul64(aM, PPasDblStrUtilsUInt64s(aMul)^[1])
    ) shr (aJ - 64));
end;
{$ELSE}

var
  High0, High1, Low1, Sum: TPasDblStrUtilsUInt64;
begin
  Low1 := UMul128(aM, PPasDblStrUtilsUInt64s(aMul)^[1], High1);
  UMul128(aM, PPasDblStrUtilsUInt64s(aMul)^[0], High0);
  Sum := High0 + Low1;
  if Sum < High0 then
  begin
    inc(High1);
  end;
  result := ShiftRight128(Sum, High1, aJ - 64);
end;
{$IFEND}

function MulShiftAll64(const aM: TPasDblStrUtilsUInt64;
  const aMul: PPasDblStrUtilsUInt64; const aJ: TPasDblStrUtilsInt32;
  out aVP, aVM: TPasDblStrUtilsUInt64; const aMMShift: TPasDblStrUtilsUInt32)
  : TPasDblStrUtilsUInt64;
begin
  aVP := MulShift64((4 * aM) + 2, aMul, aJ);
  aVM := MulShift64(((4 * aM) - 1) - aMMShift, aMul, aJ);
  result := MulShift64(4 * aM, aMul, aJ);
end;

function Log10Pow2(const e: TPasDblStrUtilsInt32): TPasDblStrUtilsInt32;
begin
  Assert(e >= 0);
  Assert(e <= 32768);
  result := TPasDblStrUtilsUInt32
    ((TPasDblStrUtilsUInt64(e) * TPasDblStrUtilsUInt64(169464822037455)
    ) shr 49);
end;

function Log10Pow5(const e: TPasDblStrUtilsInt32): TPasDblStrUtilsInt32;
begin
  Assert(e >= 0);
  Assert(e <= 32768);
  result := TPasDblStrUtilsUInt32
    ((TPasDblStrUtilsUInt64(e) * TPasDblStrUtilsUInt64(196742565691928)
    ) shr 48);
end;

function Pow5Bits(const e: TPasDblStrUtilsInt32): TPasDblStrUtilsInt32;
begin
  Assert(e >= 0);
  Assert(e <= 32768);
  result := TPasDblStrUtilsUInt32
    ((TPasDblStrUtilsUInt64(e) * TPasDblStrUtilsUInt64(163391164108059))
    shr 46) + 1;
end;

function Log2Pow5(const e: TPasDblStrUtilsInt32): TPasDblStrUtilsInt32;
begin
  Assert(e >= 0);
  Assert(e <= 3528);
  result := TPasDblStrUtilsInt32
    (TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(e) *
    1217359) shr 19));
end;

function CeilLog2Pow5(const e: TPasDblStrUtilsInt32): TPasDblStrUtilsInt32;
begin
  result := Log2Pow5(e) + 1;
end;

const
  DOUBLE_POW5_INV_BITCOUNT = 125;
  DOUBLE_POW5_BITCOUNT = 125;
  DOUBLE_POW5_INV_TABLE_SIZE = 342;
  DOUBLE_POW5_TABLE_SIZE = 326;
  DOUBLE_POW5_INV_SPLIT: array [0 .. DOUBLE_POW5_INV_TABLE_SIZE - 1, 0 .. 1]
    of TPasDblStrUtilsUInt64 = ((TPasDblStrUtilsUInt64(1),
    TPasDblStrUtilsUInt64(2305843009213693952)),
    (TPasDblStrUtilsUInt64(11068046444225730970),
    TPasDblStrUtilsUInt64(1844674407370955161)),
    (TPasDblStrUtilsUInt64(5165088340638674453),
    TPasDblStrUtilsUInt64(1475739525896764129)),
    (TPasDblStrUtilsUInt64(7821419487252849886),
    TPasDblStrUtilsUInt64(1180591620717411303)),
    (TPasDblStrUtilsUInt64(8824922364862649494),
    TPasDblStrUtilsUInt64(1888946593147858085)),
    (TPasDblStrUtilsUInt64(7059937891890119595),
    TPasDblStrUtilsUInt64(1511157274518286468)),
    (TPasDblStrUtilsUInt64(13026647942995916322),
    TPasDblStrUtilsUInt64(1208925819614629174)),
    (TPasDblStrUtilsUInt64(9774590264567735146),
    TPasDblStrUtilsUInt64(1934281311383406679)),
    (TPasDblStrUtilsUInt64(11509021026396098440),
    TPasDblStrUtilsUInt64(1547425049106725343)),
    (TPasDblStrUtilsUInt64(16585914450600699399),
    TPasDblStrUtilsUInt64(1237940039285380274)),
    (TPasDblStrUtilsUInt64(15469416676735388068),
    TPasDblStrUtilsUInt64(1980704062856608439)),
    (TPasDblStrUtilsUInt64(16064882156130220778),
    TPasDblStrUtilsUInt64(1584563250285286751)),
    (TPasDblStrUtilsUInt64(9162556910162266299),
    TPasDblStrUtilsUInt64(1267650600228229401)),
    (TPasDblStrUtilsUInt64(7281393426775805432),
    TPasDblStrUtilsUInt64(2028240960365167042)),
    (TPasDblStrUtilsUInt64(16893161185646375315),
    TPasDblStrUtilsUInt64(1622592768292133633)),
    (TPasDblStrUtilsUInt64(2446482504291369283),
    TPasDblStrUtilsUInt64(1298074214633706907)),
    (TPasDblStrUtilsUInt64(7603720821608101175),
    TPasDblStrUtilsUInt64(2076918743413931051)),
    (TPasDblStrUtilsUInt64(2393627842544570617),
    TPasDblStrUtilsUInt64(1661534994731144841)),
    (TPasDblStrUtilsUInt64(16672297533003297786),
    TPasDblStrUtilsUInt64(1329227995784915872)),
    (TPasDblStrUtilsUInt64(11918280793837635165),
    TPasDblStrUtilsUInt64(2126764793255865396)),
    (TPasDblStrUtilsUInt64(5845275820328197809),
    TPasDblStrUtilsUInt64(1701411834604692317)),
    (TPasDblStrUtilsUInt64(15744267100488289217),
    TPasDblStrUtilsUInt64(1361129467683753853)),
    (TPasDblStrUtilsUInt64(3054734472329800808),
    TPasDblStrUtilsUInt64(2177807148294006166)),
    (TPasDblStrUtilsUInt64(17201182836831481939),
    TPasDblStrUtilsUInt64(1742245718635204932)),
    (TPasDblStrUtilsUInt64(6382248639981364905),
    TPasDblStrUtilsUInt64(1393796574908163946)),
    (TPasDblStrUtilsUInt64(2832900194486363201),
    TPasDblStrUtilsUInt64(2230074519853062314)),
    (TPasDblStrUtilsUInt64(5955668970331000884),
    TPasDblStrUtilsUInt64(1784059615882449851)),
    (TPasDblStrUtilsUInt64(1075186361522890384),
    TPasDblStrUtilsUInt64(1427247692705959881)),
    (TPasDblStrUtilsUInt64(12788344622662355584),
    TPasDblStrUtilsUInt64(2283596308329535809)),
    (TPasDblStrUtilsUInt64(13920024512871794791),
    TPasDblStrUtilsUInt64(1826877046663628647)),
    (TPasDblStrUtilsUInt64(3757321980813615186),
    TPasDblStrUtilsUInt64(1461501637330902918)),
    (TPasDblStrUtilsUInt64(10384555214134712795),
    TPasDblStrUtilsUInt64(1169201309864722334)),
    (TPasDblStrUtilsUInt64(5547241898389809503),
    TPasDblStrUtilsUInt64(1870722095783555735)),
    (TPasDblStrUtilsUInt64(4437793518711847602),
    TPasDblStrUtilsUInt64(1496577676626844588)),
    (TPasDblStrUtilsUInt64(10928932444453298728),
    TPasDblStrUtilsUInt64(1197262141301475670)),
    (TPasDblStrUtilsUInt64(17486291911125277965),
    TPasDblStrUtilsUInt64(1915619426082361072)),
    (TPasDblStrUtilsUInt64(6610335899416401726),
    TPasDblStrUtilsUInt64(1532495540865888858)),
    (TPasDblStrUtilsUInt64(12666966349016942027),
    TPasDblStrUtilsUInt64(1225996432692711086)),
    (TPasDblStrUtilsUInt64(12888448528943286597),
    TPasDblStrUtilsUInt64(1961594292308337738)),
    (TPasDblStrUtilsUInt64(17689456452638449924),
    TPasDblStrUtilsUInt64(1569275433846670190)),
    (TPasDblStrUtilsUInt64(14151565162110759939),
    TPasDblStrUtilsUInt64(1255420347077336152)),
    (TPasDblStrUtilsUInt64(7885109000409574610),
    TPasDblStrUtilsUInt64(2008672555323737844)),
    (TPasDblStrUtilsUInt64(9997436015069570011),
    TPasDblStrUtilsUInt64(1606938044258990275)),
    (TPasDblStrUtilsUInt64(7997948812055656009),
    TPasDblStrUtilsUInt64(1285550435407192220)),
    (TPasDblStrUtilsUInt64(12796718099289049614),
    TPasDblStrUtilsUInt64(2056880696651507552)),
    (TPasDblStrUtilsUInt64(2858676849947419045),
    TPasDblStrUtilsUInt64(1645504557321206042)),
    (TPasDblStrUtilsUInt64(13354987924183666206),
    TPasDblStrUtilsUInt64(1316403645856964833)),
    (TPasDblStrUtilsUInt64(17678631863951955605),
    TPasDblStrUtilsUInt64(2106245833371143733)),
    (TPasDblStrUtilsUInt64(3074859046935833515),
    TPasDblStrUtilsUInt64(1684996666696914987)),
    (TPasDblStrUtilsUInt64(13527933681774397782),
    TPasDblStrUtilsUInt64(1347997333357531989)),
    (TPasDblStrUtilsUInt64(10576647446613305481),
    TPasDblStrUtilsUInt64(2156795733372051183)),
    (TPasDblStrUtilsUInt64(15840015586774465031),
    TPasDblStrUtilsUInt64(1725436586697640946)),
    (TPasDblStrUtilsUInt64(8982663654677661702),
    TPasDblStrUtilsUInt64(1380349269358112757)),
    (TPasDblStrUtilsUInt64(18061610662226169046),
    TPasDblStrUtilsUInt64(2208558830972980411)),
    (TPasDblStrUtilsUInt64(10759939715039024913),
    TPasDblStrUtilsUInt64(1766847064778384329)),
    (TPasDblStrUtilsUInt64(12297300586773130254),
    TPasDblStrUtilsUInt64(1413477651822707463)),
    (TPasDblStrUtilsUInt64(15986332124095098083),
    TPasDblStrUtilsUInt64(2261564242916331941)),
    (TPasDblStrUtilsUInt64(9099716884534168143),
    TPasDblStrUtilsUInt64(1809251394333065553)),
    (TPasDblStrUtilsUInt64(14658471137111155161),
    TPasDblStrUtilsUInt64(1447401115466452442)),
    (TPasDblStrUtilsUInt64(4348079280205103483),
    TPasDblStrUtilsUInt64(1157920892373161954)),
    (TPasDblStrUtilsUInt64(14335624477811986218),
    TPasDblStrUtilsUInt64(1852673427797059126)),
    (TPasDblStrUtilsUInt64(7779150767507678651),
    TPasDblStrUtilsUInt64(1482138742237647301)),
    (TPasDblStrUtilsUInt64(2533971799264232598),
    TPasDblStrUtilsUInt64(1185710993790117841)),
    (TPasDblStrUtilsUInt64(15122401323048503126),
    TPasDblStrUtilsUInt64(1897137590064188545)),
    (TPasDblStrUtilsUInt64(12097921058438802501),
    TPasDblStrUtilsUInt64(1517710072051350836)),
    (TPasDblStrUtilsUInt64(5988988032009131678),
    TPasDblStrUtilsUInt64(1214168057641080669)),
    (TPasDblStrUtilsUInt64(16961078480698431330),
    TPasDblStrUtilsUInt64(1942668892225729070)),
    (TPasDblStrUtilsUInt64(13568862784558745064),
    TPasDblStrUtilsUInt64(1554135113780583256)),
    (TPasDblStrUtilsUInt64(7165741412905085728),
    TPasDblStrUtilsUInt64(1243308091024466605)),
    (TPasDblStrUtilsUInt64(11465186260648137165),
    TPasDblStrUtilsUInt64(1989292945639146568)),
    (TPasDblStrUtilsUInt64(16550846638002330379),
    TPasDblStrUtilsUInt64(1591434356511317254)),
    (TPasDblStrUtilsUInt64(16930026125143774626),
    TPasDblStrUtilsUInt64(1273147485209053803)),
    (TPasDblStrUtilsUInt64(4951948911778577463),
    TPasDblStrUtilsUInt64(2037035976334486086)),
    (TPasDblStrUtilsUInt64(272210314680951647),
    TPasDblStrUtilsUInt64(1629628781067588869)),
    (TPasDblStrUtilsUInt64(3907117066486671641),
    TPasDblStrUtilsUInt64(1303703024854071095)),
    (TPasDblStrUtilsUInt64(6251387306378674625),
    TPasDblStrUtilsUInt64(2085924839766513752)),
    (TPasDblStrUtilsUInt64(16069156289328670670),
    TPasDblStrUtilsUInt64(1668739871813211001)),
    (TPasDblStrUtilsUInt64(9165976216721026213),
    TPasDblStrUtilsUInt64(1334991897450568801)),
    (TPasDblStrUtilsUInt64(7286864317269821294),
    TPasDblStrUtilsUInt64(2135987035920910082)),
    (TPasDblStrUtilsUInt64(16897537898041588005),
    TPasDblStrUtilsUInt64(1708789628736728065)),
    (TPasDblStrUtilsUInt64(13518030318433270404),
    TPasDblStrUtilsUInt64(1367031702989382452)),
    (TPasDblStrUtilsUInt64(6871453250525591353),
    TPasDblStrUtilsUInt64(2187250724783011924)),
    (TPasDblStrUtilsUInt64(9186511415162383406),
    TPasDblStrUtilsUInt64(1749800579826409539)),
    (TPasDblStrUtilsUInt64(11038557946871817048),
    TPasDblStrUtilsUInt64(1399840463861127631)),
    (TPasDblStrUtilsUInt64(10282995085511086630),
    TPasDblStrUtilsUInt64(2239744742177804210)),
    (TPasDblStrUtilsUInt64(8226396068408869304),
    TPasDblStrUtilsUInt64(1791795793742243368)),
    (TPasDblStrUtilsUInt64(13959814484210916090),
    TPasDblStrUtilsUInt64(1433436634993794694)),
    (TPasDblStrUtilsUInt64(11267656730511734774),
    TPasDblStrUtilsUInt64(2293498615990071511)),
    (TPasDblStrUtilsUInt64(5324776569667477496),
    TPasDblStrUtilsUInt64(1834798892792057209)),
    (TPasDblStrUtilsUInt64(7949170070475892320),
    TPasDblStrUtilsUInt64(1467839114233645767)),
    (TPasDblStrUtilsUInt64(17427382500606444826),
    TPasDblStrUtilsUInt64(1174271291386916613)),
    (TPasDblStrUtilsUInt64(5747719112518849781),
    TPasDblStrUtilsUInt64(1878834066219066582)),
    (TPasDblStrUtilsUInt64(15666221734240810795),
    TPasDblStrUtilsUInt64(1503067252975253265)),
    (TPasDblStrUtilsUInt64(12532977387392648636),
    TPasDblStrUtilsUInt64(1202453802380202612)),
    (TPasDblStrUtilsUInt64(5295368560860596524),
    TPasDblStrUtilsUInt64(1923926083808324180)),
    (TPasDblStrUtilsUInt64(4236294848688477220),
    TPasDblStrUtilsUInt64(1539140867046659344)),
    (TPasDblStrUtilsUInt64(7078384693692692099),
    TPasDblStrUtilsUInt64(1231312693637327475)),
    (TPasDblStrUtilsUInt64(11325415509908307358),
    TPasDblStrUtilsUInt64(1970100309819723960)),
    (TPasDblStrUtilsUInt64(9060332407926645887),
    TPasDblStrUtilsUInt64(1576080247855779168)),
    (TPasDblStrUtilsUInt64(14626963555825137356),
    TPasDblStrUtilsUInt64(1260864198284623334)),
    (TPasDblStrUtilsUInt64(12335095245094488799),
    TPasDblStrUtilsUInt64(2017382717255397335)),
    (TPasDblStrUtilsUInt64(9868076196075591040),
    TPasDblStrUtilsUInt64(1613906173804317868)),
    (TPasDblStrUtilsUInt64(15273158586344293478),
    TPasDblStrUtilsUInt64(1291124939043454294)),
    (TPasDblStrUtilsUInt64(13369007293925138595),
    TPasDblStrUtilsUInt64(2065799902469526871)),
    (TPasDblStrUtilsUInt64(7005857020398200553),
    TPasDblStrUtilsUInt64(1652639921975621497)),
    (TPasDblStrUtilsUInt64(16672732060544291412),
    TPasDblStrUtilsUInt64(1322111937580497197)),
    (TPasDblStrUtilsUInt64(11918976037903224966),
    TPasDblStrUtilsUInt64(2115379100128795516)),
    (TPasDblStrUtilsUInt64(5845832015580669650),
    TPasDblStrUtilsUInt64(1692303280103036413)),
    (TPasDblStrUtilsUInt64(12055363241948356366),
    TPasDblStrUtilsUInt64(1353842624082429130)),
    (TPasDblStrUtilsUInt64(841837113407818570),
    TPasDblStrUtilsUInt64(2166148198531886609)),
    (TPasDblStrUtilsUInt64(4362818505468165179),
    TPasDblStrUtilsUInt64(1732918558825509287)),
    (TPasDblStrUtilsUInt64(14558301248600263113),
    TPasDblStrUtilsUInt64(1386334847060407429)),
    (TPasDblStrUtilsUInt64(12225235553534690011),
    TPasDblStrUtilsUInt64(2218135755296651887)),
    (TPasDblStrUtilsUInt64(2401490813343931363),
    TPasDblStrUtilsUInt64(1774508604237321510)),
    (TPasDblStrUtilsUInt64(1921192650675145090),
    TPasDblStrUtilsUInt64(1419606883389857208)),
    (TPasDblStrUtilsUInt64(17831303500047873437),
    TPasDblStrUtilsUInt64(2271371013423771532)),
    (TPasDblStrUtilsUInt64(6886345170554478103),
    TPasDblStrUtilsUInt64(1817096810739017226)),
    (TPasDblStrUtilsUInt64(1819727321701672159),
    TPasDblStrUtilsUInt64(1453677448591213781)),
    (TPasDblStrUtilsUInt64(16213177116328979020),
    TPasDblStrUtilsUInt64(1162941958872971024)),
    (TPasDblStrUtilsUInt64(14873036941900635463),
    TPasDblStrUtilsUInt64(1860707134196753639)),
    (TPasDblStrUtilsUInt64(15587778368262418694),
    TPasDblStrUtilsUInt64(1488565707357402911)),
    (TPasDblStrUtilsUInt64(8780873879868024632),
    TPasDblStrUtilsUInt64(1190852565885922329)),
    (TPasDblStrUtilsUInt64(2981351763563108441),
    TPasDblStrUtilsUInt64(1905364105417475727)),
    (TPasDblStrUtilsUInt64(13453127855076217722),
    TPasDblStrUtilsUInt64(1524291284333980581)),
    (TPasDblStrUtilsUInt64(7073153469319063855),
    TPasDblStrUtilsUInt64(1219433027467184465)),
    (TPasDblStrUtilsUInt64(11317045550910502167),
    TPasDblStrUtilsUInt64(1951092843947495144)),
    (TPasDblStrUtilsUInt64(12742985255470312057),
    TPasDblStrUtilsUInt64(1560874275157996115)),
    (TPasDblStrUtilsUInt64(10194388204376249646),
    TPasDblStrUtilsUInt64(1248699420126396892)),
    (TPasDblStrUtilsUInt64(1553625868034358140),
    TPasDblStrUtilsUInt64(1997919072202235028)),
    (TPasDblStrUtilsUInt64(8621598323911307159),
    TPasDblStrUtilsUInt64(1598335257761788022)),
    (TPasDblStrUtilsUInt64(17965325103354776697),
    TPasDblStrUtilsUInt64(1278668206209430417)),
    (TPasDblStrUtilsUInt64(13987124906400001422),
    TPasDblStrUtilsUInt64(2045869129935088668)),
    (TPasDblStrUtilsUInt64(121653480894270168),
    TPasDblStrUtilsUInt64(1636695303948070935)),
    (TPasDblStrUtilsUInt64(97322784715416134),
    TPasDblStrUtilsUInt64(1309356243158456748)),
    (TPasDblStrUtilsUInt64(14913111714512307107),
    TPasDblStrUtilsUInt64(2094969989053530796)),
    (TPasDblStrUtilsUInt64(8241140556867935363),
    TPasDblStrUtilsUInt64(1675975991242824637)),
    (TPasDblStrUtilsUInt64(17660958889720079260),
    TPasDblStrUtilsUInt64(1340780792994259709)),
    (TPasDblStrUtilsUInt64(17189487779326395846),
    TPasDblStrUtilsUInt64(2145249268790815535)),
    (TPasDblStrUtilsUInt64(13751590223461116677),
    TPasDblStrUtilsUInt64(1716199415032652428)),
    (TPasDblStrUtilsUInt64(18379969808252713988),
    TPasDblStrUtilsUInt64(1372959532026121942)),
    (TPasDblStrUtilsUInt64(14650556434236701088),
    TPasDblStrUtilsUInt64(2196735251241795108)),
    (TPasDblStrUtilsUInt64(652398703163629901),
    TPasDblStrUtilsUInt64(1757388200993436087)),
    (TPasDblStrUtilsUInt64(11589965406756634890),
    TPasDblStrUtilsUInt64(1405910560794748869)),
    (TPasDblStrUtilsUInt64(7475898206584884855),
    TPasDblStrUtilsUInt64(2249456897271598191)),
    (TPasDblStrUtilsUInt64(2291369750525997561),
    TPasDblStrUtilsUInt64(1799565517817278553)),
    (TPasDblStrUtilsUInt64(9211793429904618695),
    TPasDblStrUtilsUInt64(1439652414253822842)),
    (TPasDblStrUtilsUInt64(18428218302589300235),
    TPasDblStrUtilsUInt64(2303443862806116547)),
    (TPasDblStrUtilsUInt64(7363877012587619542),
    TPasDblStrUtilsUInt64(1842755090244893238)),
    (TPasDblStrUtilsUInt64(13269799239553916280),
    TPasDblStrUtilsUInt64(1474204072195914590)),
    (TPasDblStrUtilsUInt64(10615839391643133024),
    TPasDblStrUtilsUInt64(1179363257756731672)),
    (TPasDblStrUtilsUInt64(2227947767661371545),
    TPasDblStrUtilsUInt64(1886981212410770676)),
    (TPasDblStrUtilsUInt64(16539753473096738529),
    TPasDblStrUtilsUInt64(1509584969928616540)),
    (TPasDblStrUtilsUInt64(13231802778477390823),
    TPasDblStrUtilsUInt64(1207667975942893232)),
    (TPasDblStrUtilsUInt64(6413489186596184024),
    TPasDblStrUtilsUInt64(1932268761508629172)),
    (TPasDblStrUtilsUInt64(16198837793502678189),
    TPasDblStrUtilsUInt64(1545815009206903337)),
    (TPasDblStrUtilsUInt64(5580372605318321905),
    TPasDblStrUtilsUInt64(1236652007365522670)),
    (TPasDblStrUtilsUInt64(8928596168509315048),
    TPasDblStrUtilsUInt64(1978643211784836272)),
    (TPasDblStrUtilsUInt64(18210923379033183008),
    TPasDblStrUtilsUInt64(1582914569427869017)),
    (TPasDblStrUtilsUInt64(7190041073742725760),
    TPasDblStrUtilsUInt64(1266331655542295214)),
    (TPasDblStrUtilsUInt64(436019273762630246),
    TPasDblStrUtilsUInt64(2026130648867672343)),
    (TPasDblStrUtilsUInt64(7727513048493924843),
    TPasDblStrUtilsUInt64(1620904519094137874)),
    (TPasDblStrUtilsUInt64(9871359253537050198),
    TPasDblStrUtilsUInt64(1296723615275310299)),
    (TPasDblStrUtilsUInt64(4726128361433549347),
    TPasDblStrUtilsUInt64(2074757784440496479)),
    (TPasDblStrUtilsUInt64(7470251503888749801),
    TPasDblStrUtilsUInt64(1659806227552397183)),
    (TPasDblStrUtilsUInt64(13354898832594820487),
    TPasDblStrUtilsUInt64(1327844982041917746)),
    (TPasDblStrUtilsUInt64(13989140502667892133),
    TPasDblStrUtilsUInt64(2124551971267068394)),
    (TPasDblStrUtilsUInt64(14880661216876224029),
    TPasDblStrUtilsUInt64(1699641577013654715)),
    (TPasDblStrUtilsUInt64(11904528973500979224),
    TPasDblStrUtilsUInt64(1359713261610923772)),
    (TPasDblStrUtilsUInt64(4289851098633925465),
    TPasDblStrUtilsUInt64(2175541218577478036)),
    (TPasDblStrUtilsUInt64(18189276137874781665),
    TPasDblStrUtilsUInt64(1740432974861982428)),
    (TPasDblStrUtilsUInt64(3483374466074094362),
    TPasDblStrUtilsUInt64(1392346379889585943)),
    (TPasDblStrUtilsUInt64(1884050330976640656),
    TPasDblStrUtilsUInt64(2227754207823337509)),
    (TPasDblStrUtilsUInt64(5196589079523222848),
    TPasDblStrUtilsUInt64(1782203366258670007)),
    (TPasDblStrUtilsUInt64(15225317707844309248),
    TPasDblStrUtilsUInt64(1425762693006936005)),
    (TPasDblStrUtilsUInt64(5913764258841343181),
    TPasDblStrUtilsUInt64(2281220308811097609)),
    (TPasDblStrUtilsUInt64(8420360221814984868),
    TPasDblStrUtilsUInt64(1824976247048878087)),
    (TPasDblStrUtilsUInt64(17804334621677718864),
    TPasDblStrUtilsUInt64(1459980997639102469)),
    (TPasDblStrUtilsUInt64(17932816512084085415),
    TPasDblStrUtilsUInt64(1167984798111281975)),
    (TPasDblStrUtilsUInt64(10245762345624985047),
    TPasDblStrUtilsUInt64(1868775676978051161)),
    (TPasDblStrUtilsUInt64(4507261061758077715),
    TPasDblStrUtilsUInt64(1495020541582440929)),
    (TPasDblStrUtilsUInt64(7295157664148372495),
    TPasDblStrUtilsUInt64(1196016433265952743)),
    (TPasDblStrUtilsUInt64(7982903447895485668),
    TPasDblStrUtilsUInt64(1913626293225524389)),
    (TPasDblStrUtilsUInt64(10075671573058298858),
    TPasDblStrUtilsUInt64(1530901034580419511)),
    (TPasDblStrUtilsUInt64(4371188443704728763),
    TPasDblStrUtilsUInt64(1224720827664335609)),
    (TPasDblStrUtilsUInt64(14372599139411386667),
    TPasDblStrUtilsUInt64(1959553324262936974)),
    (TPasDblStrUtilsUInt64(15187428126271019657),
    TPasDblStrUtilsUInt64(1567642659410349579)),
    (TPasDblStrUtilsUInt64(15839291315758726049),
    TPasDblStrUtilsUInt64(1254114127528279663)),
    (TPasDblStrUtilsUInt64(3206773216762499739),
    TPasDblStrUtilsUInt64(2006582604045247462)),
    (TPasDblStrUtilsUInt64(13633465017635730761),
    TPasDblStrUtilsUInt64(1605266083236197969)),
    (TPasDblStrUtilsUInt64(14596120828850494932),
    TPasDblStrUtilsUInt64(1284212866588958375)),
    (TPasDblStrUtilsUInt64(4907049252451240275),
    TPasDblStrUtilsUInt64(2054740586542333401)),
    (TPasDblStrUtilsUInt64(236290587219081897),
    TPasDblStrUtilsUInt64(1643792469233866721)),
    (TPasDblStrUtilsUInt64(14946427728742906810),
    TPasDblStrUtilsUInt64(1315033975387093376)),
    (TPasDblStrUtilsUInt64(16535586736504830250),
    TPasDblStrUtilsUInt64(2104054360619349402)),
    (TPasDblStrUtilsUInt64(5849771759720043554),
    TPasDblStrUtilsUInt64(1683243488495479522)),
    (TPasDblStrUtilsUInt64(15747863852001765813),
    TPasDblStrUtilsUInt64(1346594790796383617)),
    (TPasDblStrUtilsUInt64(10439186904235184007),
    TPasDblStrUtilsUInt64(2154551665274213788)),
    (TPasDblStrUtilsUInt64(15730047152871967852),
    TPasDblStrUtilsUInt64(1723641332219371030)),
    (TPasDblStrUtilsUInt64(12584037722297574282),
    TPasDblStrUtilsUInt64(1378913065775496824)),
    (TPasDblStrUtilsUInt64(9066413911450387881),
    TPasDblStrUtilsUInt64(2206260905240794919)),
    (TPasDblStrUtilsUInt64(10942479943902220628),
    TPasDblStrUtilsUInt64(1765008724192635935)),
    (TPasDblStrUtilsUInt64(8753983955121776503),
    TPasDblStrUtilsUInt64(1412006979354108748)),
    (TPasDblStrUtilsUInt64(10317025513452932081),
    TPasDblStrUtilsUInt64(2259211166966573997)),
    (TPasDblStrUtilsUInt64(874922781278525018),
    TPasDblStrUtilsUInt64(1807368933573259198)),
    (TPasDblStrUtilsUInt64(8078635854506640661),
    TPasDblStrUtilsUInt64(1445895146858607358)),
    (TPasDblStrUtilsUInt64(13841606313089133175),
    TPasDblStrUtilsUInt64(1156716117486885886)),
    (TPasDblStrUtilsUInt64(14767872471458792434),
    TPasDblStrUtilsUInt64(1850745787979017418)),
    (TPasDblStrUtilsUInt64(746251532941302978),
    TPasDblStrUtilsUInt64(1480596630383213935)),
    (TPasDblStrUtilsUInt64(597001226353042382),
    TPasDblStrUtilsUInt64(1184477304306571148)),
    (TPasDblStrUtilsUInt64(15712597221132509104),
    TPasDblStrUtilsUInt64(1895163686890513836)),
    (TPasDblStrUtilsUInt64(8880728962164096960),
    TPasDblStrUtilsUInt64(1516130949512411069)),
    (TPasDblStrUtilsUInt64(10793931984473187891),
    TPasDblStrUtilsUInt64(1212904759609928855)),
    (TPasDblStrUtilsUInt64(17270291175157100626),
    TPasDblStrUtilsUInt64(1940647615375886168)),
    (TPasDblStrUtilsUInt64(2748186495899949531),
    TPasDblStrUtilsUInt64(1552518092300708935)),
    (TPasDblStrUtilsUInt64(2198549196719959625),
    TPasDblStrUtilsUInt64(1242014473840567148)),
    (TPasDblStrUtilsUInt64(18275073973719576693),
    TPasDblStrUtilsUInt64(1987223158144907436)),
    (TPasDblStrUtilsUInt64(10930710364233751031),
    TPasDblStrUtilsUInt64(1589778526515925949)),
    (TPasDblStrUtilsUInt64(12433917106128911148),
    TPasDblStrUtilsUInt64(1271822821212740759)),
    (TPasDblStrUtilsUInt64(8826220925580526867),
    TPasDblStrUtilsUInt64(2034916513940385215)),
    (TPasDblStrUtilsUInt64(7060976740464421494),
    TPasDblStrUtilsUInt64(1627933211152308172)),
    (TPasDblStrUtilsUInt64(16716827836597268165),
    TPasDblStrUtilsUInt64(1302346568921846537)),
    (TPasDblStrUtilsUInt64(11989529279587987770),
    TPasDblStrUtilsUInt64(2083754510274954460)),
    (TPasDblStrUtilsUInt64(9591623423670390216),
    TPasDblStrUtilsUInt64(1667003608219963568)),
    (TPasDblStrUtilsUInt64(15051996368420132820),
    TPasDblStrUtilsUInt64(1333602886575970854)),
    (TPasDblStrUtilsUInt64(13015147745246481542),
    TPasDblStrUtilsUInt64(2133764618521553367)),
    (TPasDblStrUtilsUInt64(3033420566713364587),
    TPasDblStrUtilsUInt64(1707011694817242694)),
    (TPasDblStrUtilsUInt64(6116085268112601993),
    TPasDblStrUtilsUInt64(1365609355853794155)),
    (TPasDblStrUtilsUInt64(9785736428980163188),
    TPasDblStrUtilsUInt64(2184974969366070648)),
    (TPasDblStrUtilsUInt64(15207286772667951197),
    TPasDblStrUtilsUInt64(1747979975492856518)),
    (TPasDblStrUtilsUInt64(1097782973908629988),
    TPasDblStrUtilsUInt64(1398383980394285215)),
    (TPasDblStrUtilsUInt64(1756452758253807981),
    TPasDblStrUtilsUInt64(2237414368630856344)),
    (TPasDblStrUtilsUInt64(5094511021344956708),
    TPasDblStrUtilsUInt64(1789931494904685075)),
    (TPasDblStrUtilsUInt64(4075608817075965366),
    TPasDblStrUtilsUInt64(1431945195923748060)),
    (TPasDblStrUtilsUInt64(6520974107321544586),
    TPasDblStrUtilsUInt64(2291112313477996896)),
    (TPasDblStrUtilsUInt64(1527430471115325346),
    TPasDblStrUtilsUInt64(1832889850782397517)),
    (TPasDblStrUtilsUInt64(12289990821117991246),
    TPasDblStrUtilsUInt64(1466311880625918013)),
    (TPasDblStrUtilsUInt64(17210690286378213644),
    TPasDblStrUtilsUInt64(1173049504500734410)),
    (TPasDblStrUtilsUInt64(9090360384495590213),
    TPasDblStrUtilsUInt64(1876879207201175057)),
    (TPasDblStrUtilsUInt64(18340334751822203140),
    TPasDblStrUtilsUInt64(1501503365760940045)),
    (TPasDblStrUtilsUInt64(14672267801457762512),
    TPasDblStrUtilsUInt64(1201202692608752036)),
    (TPasDblStrUtilsUInt64(16096930852848599373),
    TPasDblStrUtilsUInt64(1921924308174003258)),
    (TPasDblStrUtilsUInt64(1809498238053148529),
    TPasDblStrUtilsUInt64(1537539446539202607)),
    (TPasDblStrUtilsUInt64(12515645034668249793),
    TPasDblStrUtilsUInt64(1230031557231362085)),
    (TPasDblStrUtilsUInt64(1578287981759648052),
    TPasDblStrUtilsUInt64(1968050491570179337)),
    (TPasDblStrUtilsUInt64(12330676829633449412),
    TPasDblStrUtilsUInt64(1574440393256143469)),
    (TPasDblStrUtilsUInt64(13553890278448669853),
    TPasDblStrUtilsUInt64(1259552314604914775)),
    (TPasDblStrUtilsUInt64(3239480371808320148),
    TPasDblStrUtilsUInt64(2015283703367863641)),
    (TPasDblStrUtilsUInt64(17348979556414297411),
    TPasDblStrUtilsUInt64(1612226962694290912)),
    (TPasDblStrUtilsUInt64(6500486015647617283),
    TPasDblStrUtilsUInt64(1289781570155432730)),
    (TPasDblStrUtilsUInt64(10400777625036187652),
    TPasDblStrUtilsUInt64(2063650512248692368)),
    (TPasDblStrUtilsUInt64(15699319729512770768),
    TPasDblStrUtilsUInt64(1650920409798953894)),
    (TPasDblStrUtilsUInt64(16248804598352126938),
    TPasDblStrUtilsUInt64(1320736327839163115)),
    (TPasDblStrUtilsUInt64(7551343283653851484),
    TPasDblStrUtilsUInt64(2113178124542660985)),
    (TPasDblStrUtilsUInt64(6041074626923081187),
    TPasDblStrUtilsUInt64(1690542499634128788)),
    (TPasDblStrUtilsUInt64(12211557331022285596),
    TPasDblStrUtilsUInt64(1352433999707303030)),
    (TPasDblStrUtilsUInt64(1091747655926105338),
    TPasDblStrUtilsUInt64(2163894399531684849)),
    (TPasDblStrUtilsUInt64(4562746939482794594),
    TPasDblStrUtilsUInt64(1731115519625347879)),
    (TPasDblStrUtilsUInt64(7339546366328145998),
    TPasDblStrUtilsUInt64(1384892415700278303)),
    (TPasDblStrUtilsUInt64(8053925371383123274),
    TPasDblStrUtilsUInt64(2215827865120445285)),
    (TPasDblStrUtilsUInt64(6443140297106498619),
    TPasDblStrUtilsUInt64(1772662292096356228)),
    (TPasDblStrUtilsUInt64(12533209867169019542),
    TPasDblStrUtilsUInt64(1418129833677084982)),
    (TPasDblStrUtilsUInt64(5295740528502789974),
    TPasDblStrUtilsUInt64(2269007733883335972)),
    (TPasDblStrUtilsUInt64(15304638867027962949),
    TPasDblStrUtilsUInt64(1815206187106668777)),
    (TPasDblStrUtilsUInt64(4865013464138549713),
    TPasDblStrUtilsUInt64(1452164949685335022)),
    (TPasDblStrUtilsUInt64(14960057215536570740),
    TPasDblStrUtilsUInt64(1161731959748268017)),
    (TPasDblStrUtilsUInt64(9178696285890871890),
    TPasDblStrUtilsUInt64(1858771135597228828)),
    (TPasDblStrUtilsUInt64(14721654658196518159),
    TPasDblStrUtilsUInt64(1487016908477783062)),
    (TPasDblStrUtilsUInt64(4398626097073393881),
    TPasDblStrUtilsUInt64(1189613526782226450)),
    (TPasDblStrUtilsUInt64(7037801755317430209),
    TPasDblStrUtilsUInt64(1903381642851562320)),
    (TPasDblStrUtilsUInt64(5630241404253944167),
    TPasDblStrUtilsUInt64(1522705314281249856)),
    (TPasDblStrUtilsUInt64(814844308661245011),
    TPasDblStrUtilsUInt64(1218164251424999885)),
    (TPasDblStrUtilsUInt64(1303750893857992017),
    TPasDblStrUtilsUInt64(1949062802279999816)),
    (TPasDblStrUtilsUInt64(15800395974054034906),
    TPasDblStrUtilsUInt64(1559250241823999852)),
    (TPasDblStrUtilsUInt64(5261619149759407279),
    TPasDblStrUtilsUInt64(1247400193459199882)),
    (TPasDblStrUtilsUInt64(12107939454356961969),
    TPasDblStrUtilsUInt64(1995840309534719811)),
    (TPasDblStrUtilsUInt64(5997002748743659252),
    TPasDblStrUtilsUInt64(1596672247627775849)),
    (TPasDblStrUtilsUInt64(8486951013736837725),
    TPasDblStrUtilsUInt64(1277337798102220679)),
    (TPasDblStrUtilsUInt64(2511075177753209390),
    TPasDblStrUtilsUInt64(2043740476963553087)),
    (TPasDblStrUtilsUInt64(13076906586428298482),
    TPasDblStrUtilsUInt64(1634992381570842469)),
    (TPasDblStrUtilsUInt64(14150874083884549109),
    TPasDblStrUtilsUInt64(1307993905256673975)),
    (TPasDblStrUtilsUInt64(4194654460505726958),
    TPasDblStrUtilsUInt64(2092790248410678361)),
    (TPasDblStrUtilsUInt64(18113118827372222859),
    TPasDblStrUtilsUInt64(1674232198728542688)),
    (TPasDblStrUtilsUInt64(3422448617672047318),
    TPasDblStrUtilsUInt64(1339385758982834151)),
    (TPasDblStrUtilsUInt64(16543964232501006678),
    TPasDblStrUtilsUInt64(2143017214372534641)),
    (TPasDblStrUtilsUInt64(9545822571258895019),
    TPasDblStrUtilsUInt64(1714413771498027713)),
    (TPasDblStrUtilsUInt64(15015355686490936662),
    TPasDblStrUtilsUInt64(1371531017198422170)),
    (TPasDblStrUtilsUInt64(5577825024675947042),
    TPasDblStrUtilsUInt64(2194449627517475473)),
    (TPasDblStrUtilsUInt64(11840957649224578280),
    TPasDblStrUtilsUInt64(1755559702013980378)),
    (TPasDblStrUtilsUInt64(16851463748863483271),
    TPasDblStrUtilsUInt64(1404447761611184302)),
    (TPasDblStrUtilsUInt64(12204946739213931940),
    TPasDblStrUtilsUInt64(2247116418577894884)),
    (TPasDblStrUtilsUInt64(13453306206113055875),
    TPasDblStrUtilsUInt64(1797693134862315907)),
    (TPasDblStrUtilsUInt64(3383947335406624054),
    TPasDblStrUtilsUInt64(1438154507889852726)),
    (TPasDblStrUtilsUInt64(16482362180876329456),
    TPasDblStrUtilsUInt64(2301047212623764361)),
    (TPasDblStrUtilsUInt64(9496540929959153242),
    TPasDblStrUtilsUInt64(1840837770099011489)),
    (TPasDblStrUtilsUInt64(11286581558709232917),
    TPasDblStrUtilsUInt64(1472670216079209191)),
    (TPasDblStrUtilsUInt64(5339916432225476010),
    TPasDblStrUtilsUInt64(1178136172863367353)),
    (TPasDblStrUtilsUInt64(4854517476818851293),
    TPasDblStrUtilsUInt64(1885017876581387765)),
    (TPasDblStrUtilsUInt64(3883613981455081034),
    TPasDblStrUtilsUInt64(1508014301265110212)),
    (TPasDblStrUtilsUInt64(14174937629389795797),
    TPasDblStrUtilsUInt64(1206411441012088169)),
    (TPasDblStrUtilsUInt64(11611853762797942306),
    TPasDblStrUtilsUInt64(1930258305619341071)),
    (TPasDblStrUtilsUInt64(5600134195496443521),
    TPasDblStrUtilsUInt64(1544206644495472857)),
    (TPasDblStrUtilsUInt64(15548153800622885787),
    TPasDblStrUtilsUInt64(1235365315596378285)),
    (TPasDblStrUtilsUInt64(6430302007287065643),
    TPasDblStrUtilsUInt64(1976584504954205257)),
    (TPasDblStrUtilsUInt64(16212288050055383484),
    TPasDblStrUtilsUInt64(1581267603963364205)),
    (TPasDblStrUtilsUInt64(12969830440044306787),
    TPasDblStrUtilsUInt64(1265014083170691364)),
    (TPasDblStrUtilsUInt64(9683682259845159889),
    TPasDblStrUtilsUInt64(2024022533073106183)),
    (TPasDblStrUtilsUInt64(15125643437359948558),
    TPasDblStrUtilsUInt64(1619218026458484946)),
    (TPasDblStrUtilsUInt64(8411165935146048523),
    TPasDblStrUtilsUInt64(1295374421166787957)),
    (TPasDblStrUtilsUInt64(17147214310975587960),
    TPasDblStrUtilsUInt64(2072599073866860731)),
    (TPasDblStrUtilsUInt64(10028422634038560045),
    TPasDblStrUtilsUInt64(1658079259093488585)),
    (TPasDblStrUtilsUInt64(8022738107230848036),
    TPasDblStrUtilsUInt64(1326463407274790868)),
    (TPasDblStrUtilsUInt64(9147032156827446534),
    TPasDblStrUtilsUInt64(2122341451639665389)),
    (TPasDblStrUtilsUInt64(11006974540203867551),
    TPasDblStrUtilsUInt64(1697873161311732311)),
    (TPasDblStrUtilsUInt64(5116230817421183718),
    TPasDblStrUtilsUInt64(1358298529049385849)),
    (TPasDblStrUtilsUInt64(15564666937357714594),
    TPasDblStrUtilsUInt64(2173277646479017358)),
    (TPasDblStrUtilsUInt64(1383687105660440706),
    TPasDblStrUtilsUInt64(1738622117183213887)),
    (TPasDblStrUtilsUInt64(12174996128754083534),
    TPasDblStrUtilsUInt64(1390897693746571109)),
    (TPasDblStrUtilsUInt64(8411947361780802685),
    TPasDblStrUtilsUInt64(2225436309994513775)),
    (TPasDblStrUtilsUInt64(6729557889424642148),
    TPasDblStrUtilsUInt64(1780349047995611020)),
    (TPasDblStrUtilsUInt64(5383646311539713719),
    TPasDblStrUtilsUInt64(1424279238396488816)),
    (TPasDblStrUtilsUInt64(1235136468979721303),
    TPasDblStrUtilsUInt64(2278846781434382106)),
    (TPasDblStrUtilsUInt64(15745504434151418335),
    TPasDblStrUtilsUInt64(1823077425147505684)),
    (TPasDblStrUtilsUInt64(16285752362063044992),
    TPasDblStrUtilsUInt64(1458461940118004547)),
    (TPasDblStrUtilsUInt64(5649904260166615347),
    TPasDblStrUtilsUInt64(1166769552094403638)),
    (TPasDblStrUtilsUInt64(5350498001524674232),
    TPasDblStrUtilsUInt64(1866831283351045821)),
    (TPasDblStrUtilsUInt64(591049586477829062),
    TPasDblStrUtilsUInt64(1493465026680836657)),
    (TPasDblStrUtilsUInt64(11540886113407994219),
    TPasDblStrUtilsUInt64(1194772021344669325)),
    (TPasDblStrUtilsUInt64(18673707743239135),
    TPasDblStrUtilsUInt64(1911635234151470921)),
    (TPasDblStrUtilsUInt64(14772334225162232601),
    TPasDblStrUtilsUInt64(1529308187321176736)),
    (TPasDblStrUtilsUInt64(8128518565387875758),
    TPasDblStrUtilsUInt64(1223446549856941389)),
    (TPasDblStrUtilsUInt64(1937583260394870242),
    TPasDblStrUtilsUInt64(1957514479771106223)),
    (TPasDblStrUtilsUInt64(8928764237799716840),
    TPasDblStrUtilsUInt64(1566011583816884978)),
    (TPasDblStrUtilsUInt64(14521709019723594119),
    TPasDblStrUtilsUInt64(1252809267053507982)),
    (TPasDblStrUtilsUInt64(8477339172590109297),
    TPasDblStrUtilsUInt64(2004494827285612772)),
    (TPasDblStrUtilsUInt64(17849917782297818407),
    TPasDblStrUtilsUInt64(1603595861828490217)),
    (TPasDblStrUtilsUInt64(6901236596354434079),
    TPasDblStrUtilsUInt64(1282876689462792174)),
    (TPasDblStrUtilsUInt64(18420676183650915173),
    TPasDblStrUtilsUInt64(2052602703140467478)),
    (TPasDblStrUtilsUInt64(3668494502695001169),
    TPasDblStrUtilsUInt64(1642082162512373983)),
    (TPasDblStrUtilsUInt64(10313493231639821582),
    TPasDblStrUtilsUInt64(1313665730009899186)),
    (TPasDblStrUtilsUInt64(9122891541139893884),
    TPasDblStrUtilsUInt64(2101865168015838698)),
    (TPasDblStrUtilsUInt64(14677010862395735754),
    TPasDblStrUtilsUInt64(1681492134412670958)),
    (TPasDblStrUtilsUInt64(673562245690857633),
    TPasDblStrUtilsUInt64(1345193707530136767)));
  DOUBLE_POW5_SPLIT: array [0 .. DOUBLE_POW5_TABLE_SIZE - 1, 0 .. 1]
    of TPasDblStrUtilsUInt64 = ((TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1152921504606846976)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1441151880758558720)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1801439850948198400)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(2251799813685248000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1407374883553280000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1759218604441600000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(2199023255552000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1374389534720000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1717986918400000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(2147483648000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1342177280000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1677721600000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(2097152000000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1310720000000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1638400000000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(2048000000000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1280000000000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1600000000000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(2000000000000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1250000000000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1562500000000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1953125000000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1220703125000000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1525878906250000000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1907348632812500000)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1192092895507812500)), (TPasDblStrUtilsUInt64(0),
    TPasDblStrUtilsUInt64(1490116119384765625)),
    (TPasDblStrUtilsUInt64(4611686018427387904),
    TPasDblStrUtilsUInt64(1862645149230957031)),
    (TPasDblStrUtilsUInt64(9799832789158199296),
    TPasDblStrUtilsUInt64(1164153218269348144)),
    (TPasDblStrUtilsUInt64(12249790986447749120),
    TPasDblStrUtilsUInt64(1455191522836685180)),
    (TPasDblStrUtilsUInt64(15312238733059686400),
    TPasDblStrUtilsUInt64(1818989403545856475)),
    (TPasDblStrUtilsUInt64(14528612397897220096),
    TPasDblStrUtilsUInt64(2273736754432320594)),
    (TPasDblStrUtilsUInt64(13692068767113150464),
    TPasDblStrUtilsUInt64(1421085471520200371)),
    (TPasDblStrUtilsUInt64(12503399940464050176),
    TPasDblStrUtilsUInt64(1776356839400250464)),
    (TPasDblStrUtilsUInt64(15629249925580062720),
    TPasDblStrUtilsUInt64(2220446049250313080)),
    (TPasDblStrUtilsUInt64(9768281203487539200),
    TPasDblStrUtilsUInt64(1387778780781445675)),
    (TPasDblStrUtilsUInt64(7598665485932036096),
    TPasDblStrUtilsUInt64(1734723475976807094)),
    (TPasDblStrUtilsUInt64(274959820560269312),
    TPasDblStrUtilsUInt64(2168404344971008868)),
    (TPasDblStrUtilsUInt64(9395221924704944128),
    TPasDblStrUtilsUInt64(1355252715606880542)),
    (TPasDblStrUtilsUInt64(2520655369026404352),
    TPasDblStrUtilsUInt64(1694065894508600678)),
    (TPasDblStrUtilsUInt64(12374191248137781248),
    TPasDblStrUtilsUInt64(2117582368135750847)),
    (TPasDblStrUtilsUInt64(14651398557727195136),
    TPasDblStrUtilsUInt64(1323488980084844279)),
    (TPasDblStrUtilsUInt64(13702562178731606016),
    TPasDblStrUtilsUInt64(1654361225106055349)),
    (TPasDblStrUtilsUInt64(3293144668132343808),
    TPasDblStrUtilsUInt64(2067951531382569187)),
    (TPasDblStrUtilsUInt64(18199116482078572544),
    TPasDblStrUtilsUInt64(1292469707114105741)),
    (TPasDblStrUtilsUInt64(8913837547316051968),
    TPasDblStrUtilsUInt64(1615587133892632177)),
    (TPasDblStrUtilsUInt64(15753982952572452864),
    TPasDblStrUtilsUInt64(2019483917365790221)),
    (TPasDblStrUtilsUInt64(12152082354571476992),
    TPasDblStrUtilsUInt64(1262177448353618888)),
    (TPasDblStrUtilsUInt64(15190102943214346240),
    TPasDblStrUtilsUInt64(1577721810442023610)),
    (TPasDblStrUtilsUInt64(9764256642163156992),
    TPasDblStrUtilsUInt64(1972152263052529513)),
    (TPasDblStrUtilsUInt64(17631875447420442880),
    TPasDblStrUtilsUInt64(1232595164407830945)),
    (TPasDblStrUtilsUInt64(8204786253993389888),
    TPasDblStrUtilsUInt64(1540743955509788682)),
    (TPasDblStrUtilsUInt64(1032610780636961552),
    TPasDblStrUtilsUInt64(1925929944387235853)),
    (TPasDblStrUtilsUInt64(2951224747111794922),
    TPasDblStrUtilsUInt64(1203706215242022408)),
    (TPasDblStrUtilsUInt64(3689030933889743652),
    TPasDblStrUtilsUInt64(1504632769052528010)),
    (TPasDblStrUtilsUInt64(13834660704216955373),
    TPasDblStrUtilsUInt64(1880790961315660012)),
    (TPasDblStrUtilsUInt64(17870034976990372916),
    TPasDblStrUtilsUInt64(1175494350822287507)),
    (TPasDblStrUtilsUInt64(17725857702810578241),
    TPasDblStrUtilsUInt64(1469367938527859384)),
    (TPasDblStrUtilsUInt64(3710578054803671186),
    TPasDblStrUtilsUInt64(1836709923159824231)),
    (TPasDblStrUtilsUInt64(26536550077201078),
    TPasDblStrUtilsUInt64(2295887403949780289)),
    (TPasDblStrUtilsUInt64(11545800389866720434),
    TPasDblStrUtilsUInt64(1434929627468612680)),
    (TPasDblStrUtilsUInt64(14432250487333400542),
    TPasDblStrUtilsUInt64(1793662034335765850)),
    (TPasDblStrUtilsUInt64(8816941072311974870),
    TPasDblStrUtilsUInt64(2242077542919707313)),
    (TPasDblStrUtilsUInt64(17039803216263454053),
    TPasDblStrUtilsUInt64(1401298464324817070)),
    (TPasDblStrUtilsUInt64(12076381983474541759),
    TPasDblStrUtilsUInt64(1751623080406021338)),
    (TPasDblStrUtilsUInt64(5872105442488401391),
    TPasDblStrUtilsUInt64(2189528850507526673)),
    (TPasDblStrUtilsUInt64(15199280947623720629),
    TPasDblStrUtilsUInt64(1368455531567204170)),
    (TPasDblStrUtilsUInt64(9775729147674874978),
    TPasDblStrUtilsUInt64(1710569414459005213)),
    (TPasDblStrUtilsUInt64(16831347453020981627),
    TPasDblStrUtilsUInt64(2138211768073756516)),
    (TPasDblStrUtilsUInt64(1296220121283337709),
    TPasDblStrUtilsUInt64(1336382355046097823)),
    (TPasDblStrUtilsUInt64(15455333206886335848),
    TPasDblStrUtilsUInt64(1670477943807622278)),
    (TPasDblStrUtilsUInt64(10095794471753144002),
    TPasDblStrUtilsUInt64(2088097429759527848)),
    (TPasDblStrUtilsUInt64(6309871544845715001),
    TPasDblStrUtilsUInt64(1305060893599704905)),
    (TPasDblStrUtilsUInt64(12499025449484531656),
    TPasDblStrUtilsUInt64(1631326116999631131)),
    (TPasDblStrUtilsUInt64(11012095793428276666),
    TPasDblStrUtilsUInt64(2039157646249538914)),
    (TPasDblStrUtilsUInt64(11494245889320060820),
    TPasDblStrUtilsUInt64(1274473528905961821)),
    (TPasDblStrUtilsUInt64(532749306367912313),
    TPasDblStrUtilsUInt64(1593091911132452277)),
    (TPasDblStrUtilsUInt64(5277622651387278295),
    TPasDblStrUtilsUInt64(1991364888915565346)),
    (TPasDblStrUtilsUInt64(7910200175544436838),
    TPasDblStrUtilsUInt64(1244603055572228341)),
    (TPasDblStrUtilsUInt64(14499436237857933952),
    TPasDblStrUtilsUInt64(1555753819465285426)),
    (TPasDblStrUtilsUInt64(8900923260467641632),
    TPasDblStrUtilsUInt64(1944692274331606783)),
    (TPasDblStrUtilsUInt64(12480606065433357876),
    TPasDblStrUtilsUInt64(1215432671457254239)),
    (TPasDblStrUtilsUInt64(10989071563364309441),
    TPasDblStrUtilsUInt64(1519290839321567799)),
    (TPasDblStrUtilsUInt64(9124653435777998898),
    TPasDblStrUtilsUInt64(1899113549151959749)),
    (TPasDblStrUtilsUInt64(8008751406574943263),
    TPasDblStrUtilsUInt64(1186945968219974843)),
    (TPasDblStrUtilsUInt64(5399253239791291175),
    TPasDblStrUtilsUInt64(1483682460274968554)),
    (TPasDblStrUtilsUInt64(15972438586593889776),
    TPasDblStrUtilsUInt64(1854603075343710692)),
    (TPasDblStrUtilsUInt64(759402079766405302),
    TPasDblStrUtilsUInt64(1159126922089819183)),
    (TPasDblStrUtilsUInt64(14784310654990170340),
    TPasDblStrUtilsUInt64(1448908652612273978)),
    (TPasDblStrUtilsUInt64(9257016281882937117),
    TPasDblStrUtilsUInt64(1811135815765342473)),
    (TPasDblStrUtilsUInt64(16182956370781059300),
    TPasDblStrUtilsUInt64(2263919769706678091)),
    (TPasDblStrUtilsUInt64(7808504722524468110),
    TPasDblStrUtilsUInt64(1414949856066673807)),
    (TPasDblStrUtilsUInt64(5148944884728197234),
    TPasDblStrUtilsUInt64(1768687320083342259)),
    (TPasDblStrUtilsUInt64(1824495087482858639),
    TPasDblStrUtilsUInt64(2210859150104177824)),
    (TPasDblStrUtilsUInt64(1140309429676786649),
    TPasDblStrUtilsUInt64(1381786968815111140)),
    (TPasDblStrUtilsUInt64(1425386787095983311),
    TPasDblStrUtilsUInt64(1727233711018888925)),
    (TPasDblStrUtilsUInt64(6393419502297367043),
    TPasDblStrUtilsUInt64(2159042138773611156)),
    (TPasDblStrUtilsUInt64(13219259225790630210),
    TPasDblStrUtilsUInt64(1349401336733506972)),
    (TPasDblStrUtilsUInt64(16524074032238287762),
    TPasDblStrUtilsUInt64(1686751670916883715)),
    (TPasDblStrUtilsUInt64(16043406521870471799),
    TPasDblStrUtilsUInt64(2108439588646104644)),
    (TPasDblStrUtilsUInt64(803757039314269066),
    TPasDblStrUtilsUInt64(1317774742903815403)),
    (TPasDblStrUtilsUInt64(14839754354425000045),
    TPasDblStrUtilsUInt64(1647218428629769253)),
    (TPasDblStrUtilsUInt64(4714634887749086344),
    TPasDblStrUtilsUInt64(2059023035787211567)),
    (TPasDblStrUtilsUInt64(9864175832484260821),
    TPasDblStrUtilsUInt64(1286889397367007229)),
    (TPasDblStrUtilsUInt64(16941905809032713930),
    TPasDblStrUtilsUInt64(1608611746708759036)),
    (TPasDblStrUtilsUInt64(2730638187581340797),
    TPasDblStrUtilsUInt64(2010764683385948796)),
    (TPasDblStrUtilsUInt64(10930020904093113806),
    TPasDblStrUtilsUInt64(1256727927116217997)),
    (TPasDblStrUtilsUInt64(18274212148543780162),
    TPasDblStrUtilsUInt64(1570909908895272496)),
    (TPasDblStrUtilsUInt64(4396021111970173586),
    TPasDblStrUtilsUInt64(1963637386119090621)),
    (TPasDblStrUtilsUInt64(5053356204195052443),
    TPasDblStrUtilsUInt64(1227273366324431638)),
    (TPasDblStrUtilsUInt64(15540067292098591362),
    TPasDblStrUtilsUInt64(1534091707905539547)),
    (TPasDblStrUtilsUInt64(14813398096695851299),
    TPasDblStrUtilsUInt64(1917614634881924434)),
    (TPasDblStrUtilsUInt64(13870059828862294966),
    TPasDblStrUtilsUInt64(1198509146801202771)),
    (TPasDblStrUtilsUInt64(12725888767650480803),
    TPasDblStrUtilsUInt64(1498136433501503464)),
    (TPasDblStrUtilsUInt64(15907360959563101004),
    TPasDblStrUtilsUInt64(1872670541876879330)),
    (TPasDblStrUtilsUInt64(14553786618154326031),
    TPasDblStrUtilsUInt64(1170419088673049581)),
    (TPasDblStrUtilsUInt64(4357175217410743827),
    TPasDblStrUtilsUInt64(1463023860841311977)),
    (TPasDblStrUtilsUInt64(10058155040190817688),
    TPasDblStrUtilsUInt64(1828779826051639971)),
    (TPasDblStrUtilsUInt64(7961007781811134206),
    TPasDblStrUtilsUInt64(2285974782564549964)),
    (TPasDblStrUtilsUInt64(14199001900486734687),
    TPasDblStrUtilsUInt64(1428734239102843727)),
    (TPasDblStrUtilsUInt64(13137066357181030455),
    TPasDblStrUtilsUInt64(1785917798878554659)),
    (TPasDblStrUtilsUInt64(11809646928048900164),
    TPasDblStrUtilsUInt64(2232397248598193324)),
    (TPasDblStrUtilsUInt64(16604401366885338411),
    TPasDblStrUtilsUInt64(1395248280373870827)),
    (TPasDblStrUtilsUInt64(16143815690179285109),
    TPasDblStrUtilsUInt64(1744060350467338534)),
    (TPasDblStrUtilsUInt64(10956397575869330579),
    TPasDblStrUtilsUInt64(2180075438084173168)),
    (TPasDblStrUtilsUInt64(6847748484918331612),
    TPasDblStrUtilsUInt64(1362547148802608230)),
    (TPasDblStrUtilsUInt64(17783057643002690323),
    TPasDblStrUtilsUInt64(1703183936003260287)),
    (TPasDblStrUtilsUInt64(17617136035325974999),
    TPasDblStrUtilsUInt64(2128979920004075359)),
    (TPasDblStrUtilsUInt64(17928239049719816230),
    TPasDblStrUtilsUInt64(1330612450002547099)),
    (TPasDblStrUtilsUInt64(17798612793722382384),
    TPasDblStrUtilsUInt64(1663265562503183874)),
    (TPasDblStrUtilsUInt64(13024893955298202172),
    TPasDblStrUtilsUInt64(2079081953128979843)),
    (TPasDblStrUtilsUInt64(5834715712847682405),
    TPasDblStrUtilsUInt64(1299426220705612402)),
    (TPasDblStrUtilsUInt64(16516766677914378815),
    TPasDblStrUtilsUInt64(1624282775882015502)),
    (TPasDblStrUtilsUInt64(11422586310538197711),
    TPasDblStrUtilsUInt64(2030353469852519378)),
    (TPasDblStrUtilsUInt64(11750802462513761473),
    TPasDblStrUtilsUInt64(1268970918657824611)),
    (TPasDblStrUtilsUInt64(10076817059714813937),
    TPasDblStrUtilsUInt64(1586213648322280764)),
    (TPasDblStrUtilsUInt64(12596021324643517422),
    TPasDblStrUtilsUInt64(1982767060402850955)),
    (TPasDblStrUtilsUInt64(5566670318688504437),
    TPasDblStrUtilsUInt64(1239229412751781847)),
    (TPasDblStrUtilsUInt64(2346651879933242642),
    TPasDblStrUtilsUInt64(1549036765939727309)),
    (TPasDblStrUtilsUInt64(7545000868343941206),
    TPasDblStrUtilsUInt64(1936295957424659136)),
    (TPasDblStrUtilsUInt64(4715625542714963254),
    TPasDblStrUtilsUInt64(1210184973390411960)),
    (TPasDblStrUtilsUInt64(5894531928393704067),
    TPasDblStrUtilsUInt64(1512731216738014950)),
    (TPasDblStrUtilsUInt64(16591536947346905892),
    TPasDblStrUtilsUInt64(1890914020922518687)),
    (TPasDblStrUtilsUInt64(17287239619732898039),
    TPasDblStrUtilsUInt64(1181821263076574179)),
    (TPasDblStrUtilsUInt64(16997363506238734644),
    TPasDblStrUtilsUInt64(1477276578845717724)),
    (TPasDblStrUtilsUInt64(2799960309088866689),
    TPasDblStrUtilsUInt64(1846595723557147156)),
    (TPasDblStrUtilsUInt64(10973347230035317489),
    TPasDblStrUtilsUInt64(1154122327223216972)),
    (TPasDblStrUtilsUInt64(13716684037544146861),
    TPasDblStrUtilsUInt64(1442652909029021215)),
    (TPasDblStrUtilsUInt64(12534169028502795672),
    TPasDblStrUtilsUInt64(1803316136286276519)),
    (TPasDblStrUtilsUInt64(11056025267201106687),
    TPasDblStrUtilsUInt64(2254145170357845649)),
    (TPasDblStrUtilsUInt64(18439230838069161439),
    TPasDblStrUtilsUInt64(1408840731473653530)),
    (TPasDblStrUtilsUInt64(13825666510731675991),
    TPasDblStrUtilsUInt64(1761050914342066913)),
    (TPasDblStrUtilsUInt64(3447025083132431277),
    TPasDblStrUtilsUInt64(2201313642927583642)),
    (TPasDblStrUtilsUInt64(6766076695385157452),
    TPasDblStrUtilsUInt64(1375821026829739776)),
    (TPasDblStrUtilsUInt64(8457595869231446815),
    TPasDblStrUtilsUInt64(1719776283537174720)),
    (TPasDblStrUtilsUInt64(10571994836539308519),
    TPasDblStrUtilsUInt64(2149720354421468400)),
    (TPasDblStrUtilsUInt64(6607496772837067824),
    TPasDblStrUtilsUInt64(1343575221513417750)),
    (TPasDblStrUtilsUInt64(17482743002901110588),
    TPasDblStrUtilsUInt64(1679469026891772187)),
    (TPasDblStrUtilsUInt64(17241742735199000331),
    TPasDblStrUtilsUInt64(2099336283614715234)),
    (TPasDblStrUtilsUInt64(15387775227926763111),
    TPasDblStrUtilsUInt64(1312085177259197021)),
    (TPasDblStrUtilsUInt64(5399660979626290177),
    TPasDblStrUtilsUInt64(1640106471573996277)),
    (TPasDblStrUtilsUInt64(11361262242960250625),
    TPasDblStrUtilsUInt64(2050133089467495346)),
    (TPasDblStrUtilsUInt64(11712474920277544544),
    TPasDblStrUtilsUInt64(1281333180917184591)),
    (TPasDblStrUtilsUInt64(10028907631919542777),
    TPasDblStrUtilsUInt64(1601666476146480739)),
    (TPasDblStrUtilsUInt64(7924448521472040567),
    TPasDblStrUtilsUInt64(2002083095183100924)),
    (TPasDblStrUtilsUInt64(14176152362774801162),
    TPasDblStrUtilsUInt64(1251301934489438077)),
    (TPasDblStrUtilsUInt64(3885132398186337741),
    TPasDblStrUtilsUInt64(1564127418111797597)),
    (TPasDblStrUtilsUInt64(9468101516160310080),
    TPasDblStrUtilsUInt64(1955159272639746996)),
    (TPasDblStrUtilsUInt64(15140935484454969608),
    TPasDblStrUtilsUInt64(1221974545399841872)),
    (TPasDblStrUtilsUInt64(479425281859160394),
    TPasDblStrUtilsUInt64(1527468181749802341)),
    (TPasDblStrUtilsUInt64(5210967620751338397),
    TPasDblStrUtilsUInt64(1909335227187252926)),
    (TPasDblStrUtilsUInt64(17091912818251750210),
    TPasDblStrUtilsUInt64(1193334516992033078)),
    (TPasDblStrUtilsUInt64(12141518985959911954),
    TPasDblStrUtilsUInt64(1491668146240041348)),
    (TPasDblStrUtilsUInt64(15176898732449889943),
    TPasDblStrUtilsUInt64(1864585182800051685)),
    (TPasDblStrUtilsUInt64(11791404716994875166),
    TPasDblStrUtilsUInt64(1165365739250032303)),
    (TPasDblStrUtilsUInt64(10127569877816206054),
    TPasDblStrUtilsUInt64(1456707174062540379)),
    (TPasDblStrUtilsUInt64(8047776328842869663),
    TPasDblStrUtilsUInt64(1820883967578175474)),
    (TPasDblStrUtilsUInt64(836348374198811271),
    TPasDblStrUtilsUInt64(2276104959472719343)),
    (TPasDblStrUtilsUInt64(7440246761515338900),
    TPasDblStrUtilsUInt64(1422565599670449589)),
    (TPasDblStrUtilsUInt64(13911994470321561530),
    TPasDblStrUtilsUInt64(1778206999588061986)),
    (TPasDblStrUtilsUInt64(8166621051047176104),
    TPasDblStrUtilsUInt64(2222758749485077483)),
    (TPasDblStrUtilsUInt64(2798295147690791113),
    TPasDblStrUtilsUInt64(1389224218428173427)),
    (TPasDblStrUtilsUInt64(17332926989895652603),
    TPasDblStrUtilsUInt64(1736530273035216783)),
    (TPasDblStrUtilsUInt64(17054472718942177850),
    TPasDblStrUtilsUInt64(2170662841294020979)),
    (TPasDblStrUtilsUInt64(8353202440125167204),
    TPasDblStrUtilsUInt64(1356664275808763112)),
    (TPasDblStrUtilsUInt64(10441503050156459005),
    TPasDblStrUtilsUInt64(1695830344760953890)),
    (TPasDblStrUtilsUInt64(3828506775840797949),
    TPasDblStrUtilsUInt64(2119787930951192363)),
    (TPasDblStrUtilsUInt64(86973725686804766),
    TPasDblStrUtilsUInt64(1324867456844495227)),
    (TPasDblStrUtilsUInt64(13943775212390669669),
    TPasDblStrUtilsUInt64(1656084321055619033)),
    (TPasDblStrUtilsUInt64(3594660960206173375),
    TPasDblStrUtilsUInt64(2070105401319523792)),
    (TPasDblStrUtilsUInt64(2246663100128858359),
    TPasDblStrUtilsUInt64(1293815875824702370)),
    (TPasDblStrUtilsUInt64(12031700912015848757),
    TPasDblStrUtilsUInt64(1617269844780877962)),
    (TPasDblStrUtilsUInt64(5816254103165035138),
    TPasDblStrUtilsUInt64(2021587305976097453)),
    (TPasDblStrUtilsUInt64(5941001823691840913),
    TPasDblStrUtilsUInt64(1263492066235060908)),
    (TPasDblStrUtilsUInt64(7426252279614801142),
    TPasDblStrUtilsUInt64(1579365082793826135)),
    (TPasDblStrUtilsUInt64(4671129331091113523),
    TPasDblStrUtilsUInt64(1974206353492282669)),
    (TPasDblStrUtilsUInt64(5225298841145639904),
    TPasDblStrUtilsUInt64(1233878970932676668)),
    (TPasDblStrUtilsUInt64(6531623551432049880),
    TPasDblStrUtilsUInt64(1542348713665845835)),
    (TPasDblStrUtilsUInt64(3552843420862674446),
    TPasDblStrUtilsUInt64(1927935892082307294)),
    (TPasDblStrUtilsUInt64(16055585193321335241),
    TPasDblStrUtilsUInt64(1204959932551442058)),
    (TPasDblStrUtilsUInt64(10846109454796893243),
    TPasDblStrUtilsUInt64(1506199915689302573)),
    (TPasDblStrUtilsUInt64(18169322836923504458),
    TPasDblStrUtilsUInt64(1882749894611628216)),
    (TPasDblStrUtilsUInt64(11355826773077190286),
    TPasDblStrUtilsUInt64(1176718684132267635)),
    (TPasDblStrUtilsUInt64(9583097447919099954),
    TPasDblStrUtilsUInt64(1470898355165334544)),
    (TPasDblStrUtilsUInt64(11978871809898874942),
    TPasDblStrUtilsUInt64(1838622943956668180)),
    (TPasDblStrUtilsUInt64(14973589762373593678),
    TPasDblStrUtilsUInt64(2298278679945835225)),
    (TPasDblStrUtilsUInt64(2440964573842414192),
    TPasDblStrUtilsUInt64(1436424174966147016)),
    (TPasDblStrUtilsUInt64(3051205717303017741),
    TPasDblStrUtilsUInt64(1795530218707683770)),
    (TPasDblStrUtilsUInt64(13037379183483547984),
    TPasDblStrUtilsUInt64(2244412773384604712)),
    (TPasDblStrUtilsUInt64(8148361989677217490),
    TPasDblStrUtilsUInt64(1402757983365377945)),
    (TPasDblStrUtilsUInt64(14797138505523909766),
    TPasDblStrUtilsUInt64(1753447479206722431)),
    (TPasDblStrUtilsUInt64(13884737113477499304),
    TPasDblStrUtilsUInt64(2191809349008403039)),
    (TPasDblStrUtilsUInt64(15595489723564518921),
    TPasDblStrUtilsUInt64(1369880843130251899)),
    (TPasDblStrUtilsUInt64(14882676136028260747),
    TPasDblStrUtilsUInt64(1712351053912814874)),
    (TPasDblStrUtilsUInt64(9379973133180550126),
    TPasDblStrUtilsUInt64(2140438817391018593)),
    (TPasDblStrUtilsUInt64(17391698254306313589),
    TPasDblStrUtilsUInt64(1337774260869386620)),
    (TPasDblStrUtilsUInt64(3292878744173340370),
    TPasDblStrUtilsUInt64(1672217826086733276)),
    (TPasDblStrUtilsUInt64(4116098430216675462),
    TPasDblStrUtilsUInt64(2090272282608416595)),
    (TPasDblStrUtilsUInt64(266718509671728212),
    TPasDblStrUtilsUInt64(1306420176630260372)),
    (TPasDblStrUtilsUInt64(333398137089660265),
    TPasDblStrUtilsUInt64(1633025220787825465)),
    (TPasDblStrUtilsUInt64(5028433689789463235),
    TPasDblStrUtilsUInt64(2041281525984781831)),
    (TPasDblStrUtilsUInt64(10060300083759496378),
    TPasDblStrUtilsUInt64(1275800953740488644)),
    (TPasDblStrUtilsUInt64(12575375104699370472),
    TPasDblStrUtilsUInt64(1594751192175610805)),
    (TPasDblStrUtilsUInt64(1884160825592049379),
    TPasDblStrUtilsUInt64(1993438990219513507)),
    (TPasDblStrUtilsUInt64(17318501580490888525),
    TPasDblStrUtilsUInt64(1245899368887195941)),
    (TPasDblStrUtilsUInt64(7813068920331446945),
    TPasDblStrUtilsUInt64(1557374211108994927)),
    (TPasDblStrUtilsUInt64(5154650131986920777),
    TPasDblStrUtilsUInt64(1946717763886243659)),
    (TPasDblStrUtilsUInt64(915813323278131534),
    TPasDblStrUtilsUInt64(1216698602428902287)),
    (TPasDblStrUtilsUInt64(14979824709379828129),
    TPasDblStrUtilsUInt64(1520873253036127858)),
    (TPasDblStrUtilsUInt64(9501408849870009354),
    TPasDblStrUtilsUInt64(1901091566295159823)),
    (TPasDblStrUtilsUInt64(12855909558809837702),
    TPasDblStrUtilsUInt64(1188182228934474889)),
    (TPasDblStrUtilsUInt64(2234828893230133415),
    TPasDblStrUtilsUInt64(1485227786168093612)),
    (TPasDblStrUtilsUInt64(2793536116537666769),
    TPasDblStrUtilsUInt64(1856534732710117015)),
    (TPasDblStrUtilsUInt64(8663489100477123587),
    TPasDblStrUtilsUInt64(1160334207943823134)),
    (TPasDblStrUtilsUInt64(1605989338741628675),
    TPasDblStrUtilsUInt64(1450417759929778918)),
    (TPasDblStrUtilsUInt64(11230858710281811652),
    TPasDblStrUtilsUInt64(1813022199912223647)),
    (TPasDblStrUtilsUInt64(9426887369424876662),
    TPasDblStrUtilsUInt64(2266277749890279559)),
    (TPasDblStrUtilsUInt64(12809333633531629769),
    TPasDblStrUtilsUInt64(1416423593681424724)),
    (TPasDblStrUtilsUInt64(16011667041914537212),
    TPasDblStrUtilsUInt64(1770529492101780905)),
    (TPasDblStrUtilsUInt64(6179525747111007803),
    TPasDblStrUtilsUInt64(2213161865127226132)),
    (TPasDblStrUtilsUInt64(13085575628799155685),
    TPasDblStrUtilsUInt64(1383226165704516332)),
    (TPasDblStrUtilsUInt64(16356969535998944606),
    TPasDblStrUtilsUInt64(1729032707130645415)),
    (TPasDblStrUtilsUInt64(15834525901571292854),
    TPasDblStrUtilsUInt64(2161290883913306769)),
    (TPasDblStrUtilsUInt64(2979049660840976177),
    TPasDblStrUtilsUInt64(1350806802445816731)),
    (TPasDblStrUtilsUInt64(17558870131333383934),
    TPasDblStrUtilsUInt64(1688508503057270913)),
    (TPasDblStrUtilsUInt64(8113529608884566205),
    TPasDblStrUtilsUInt64(2110635628821588642)),
    (TPasDblStrUtilsUInt64(9682642023980241782),
    TPasDblStrUtilsUInt64(1319147268013492901)),
    (TPasDblStrUtilsUInt64(16714988548402690132),
    TPasDblStrUtilsUInt64(1648934085016866126)),
    (TPasDblStrUtilsUInt64(11670363648648586857),
    TPasDblStrUtilsUInt64(2061167606271082658)),
    (TPasDblStrUtilsUInt64(11905663298832754689),
    TPasDblStrUtilsUInt64(1288229753919426661)),
    (TPasDblStrUtilsUInt64(1047021068258779650),
    TPasDblStrUtilsUInt64(1610287192399283327)),
    (TPasDblStrUtilsUInt64(15143834390605638274),
    TPasDblStrUtilsUInt64(2012858990499104158)),
    (TPasDblStrUtilsUInt64(4853210475701136017),
    TPasDblStrUtilsUInt64(1258036869061940099)),
    (TPasDblStrUtilsUInt64(1454827076199032118),
    TPasDblStrUtilsUInt64(1572546086327425124)),
    (TPasDblStrUtilsUInt64(1818533845248790147),
    TPasDblStrUtilsUInt64(1965682607909281405)),
    (TPasDblStrUtilsUInt64(3442426662494187794),
    TPasDblStrUtilsUInt64(1228551629943300878)),
    (TPasDblStrUtilsUInt64(13526405364972510550),
    TPasDblStrUtilsUInt64(1535689537429126097)),
    (TPasDblStrUtilsUInt64(3072948650933474476),
    TPasDblStrUtilsUInt64(1919611921786407622)),
    (TPasDblStrUtilsUInt64(15755650962115585259),
    TPasDblStrUtilsUInt64(1199757451116504763)),
    (TPasDblStrUtilsUInt64(15082877684217093670),
    TPasDblStrUtilsUInt64(1499696813895630954)),
    (TPasDblStrUtilsUInt64(9630225068416591280),
    TPasDblStrUtilsUInt64(1874621017369538693)),
    (TPasDblStrUtilsUInt64(8324733676974063502),
    TPasDblStrUtilsUInt64(1171638135855961683)),
    (TPasDblStrUtilsUInt64(5794231077790191473),
    TPasDblStrUtilsUInt64(1464547669819952104)),
    (TPasDblStrUtilsUInt64(7242788847237739342),
    TPasDblStrUtilsUInt64(1830684587274940130)),
    (TPasDblStrUtilsUInt64(18276858095901949986),
    TPasDblStrUtilsUInt64(2288355734093675162)),
    (TPasDblStrUtilsUInt64(16034722328366106645),
    TPasDblStrUtilsUInt64(1430222333808546976)),
    (TPasDblStrUtilsUInt64(1596658836748081690),
    TPasDblStrUtilsUInt64(1787777917260683721)),
    (TPasDblStrUtilsUInt64(6607509564362490017),
    TPasDblStrUtilsUInt64(2234722396575854651)),
    (TPasDblStrUtilsUInt64(1823850468512862308),
    TPasDblStrUtilsUInt64(1396701497859909157)),
    (TPasDblStrUtilsUInt64(6891499104068465790),
    TPasDblStrUtilsUInt64(1745876872324886446)),
    (TPasDblStrUtilsUInt64(17837745916940358045),
    TPasDblStrUtilsUInt64(2182346090406108057)),
    (TPasDblStrUtilsUInt64(4231062170446641922),
    TPasDblStrUtilsUInt64(1363966306503817536)),
    (TPasDblStrUtilsUInt64(5288827713058302403),
    TPasDblStrUtilsUInt64(1704957883129771920)),
    (TPasDblStrUtilsUInt64(6611034641322878003),
    TPasDblStrUtilsUInt64(2131197353912214900)),
    (TPasDblStrUtilsUInt64(13355268687681574560),
    TPasDblStrUtilsUInt64(1331998346195134312)),
    (TPasDblStrUtilsUInt64(16694085859601968200),
    TPasDblStrUtilsUInt64(1664997932743917890)),
    (TPasDblStrUtilsUInt64(11644235287647684442),
    TPasDblStrUtilsUInt64(2081247415929897363)),
    (TPasDblStrUtilsUInt64(4971804045566108824),
    TPasDblStrUtilsUInt64(1300779634956185852)),
    (TPasDblStrUtilsUInt64(6214755056957636030),
    TPasDblStrUtilsUInt64(1625974543695232315)),
    (TPasDblStrUtilsUInt64(3156757802769657134),
    TPasDblStrUtilsUInt64(2032468179619040394)),
    (TPasDblStrUtilsUInt64(6584659645158423613),
    TPasDblStrUtilsUInt64(1270292612261900246)),
    (TPasDblStrUtilsUInt64(17454196593302805324),
    TPasDblStrUtilsUInt64(1587865765327375307)),
    (TPasDblStrUtilsUInt64(17206059723201118751),
    TPasDblStrUtilsUInt64(1984832206659219134)),
    (TPasDblStrUtilsUInt64(6142101308573311315),
    TPasDblStrUtilsUInt64(1240520129162011959)),
    (TPasDblStrUtilsUInt64(3065940617289251240),
    TPasDblStrUtilsUInt64(1550650161452514949)),
    (TPasDblStrUtilsUInt64(8444111790038951954),
    TPasDblStrUtilsUInt64(1938312701815643686)),
    (TPasDblStrUtilsUInt64(665883850346957067),
    TPasDblStrUtilsUInt64(1211445438634777304)),
    (TPasDblStrUtilsUInt64(832354812933696334),
    TPasDblStrUtilsUInt64(1514306798293471630)),
    (TPasDblStrUtilsUInt64(10263815553021896226),
    TPasDblStrUtilsUInt64(1892883497866839537)),
    (TPasDblStrUtilsUInt64(17944099766707154901),
    TPasDblStrUtilsUInt64(1183052186166774710)),
    (TPasDblStrUtilsUInt64(13206752671529167818),
    TPasDblStrUtilsUInt64(1478815232708468388)),
    (TPasDblStrUtilsUInt64(16508440839411459773),
    TPasDblStrUtilsUInt64(1848519040885585485)),
    (TPasDblStrUtilsUInt64(12623618533845856310),
    TPasDblStrUtilsUInt64(1155324400553490928)),
    (TPasDblStrUtilsUInt64(15779523167307320387),
    TPasDblStrUtilsUInt64(1444155500691863660)),
    (TPasDblStrUtilsUInt64(1277659885424598868),
    TPasDblStrUtilsUInt64(1805194375864829576)),
    (TPasDblStrUtilsUInt64(1597074856780748586),
    TPasDblStrUtilsUInt64(2256492969831036970)),
    (TPasDblStrUtilsUInt64(5609857803915355770),
    TPasDblStrUtilsUInt64(1410308106144398106)),
    (TPasDblStrUtilsUInt64(16235694291748970521),
    TPasDblStrUtilsUInt64(1762885132680497632)),
    (TPasDblStrUtilsUInt64(1847873790976661535),
    TPasDblStrUtilsUInt64(2203606415850622041)),
    (TPasDblStrUtilsUInt64(12684136165428883219),
    TPasDblStrUtilsUInt64(1377254009906638775)),
    (TPasDblStrUtilsUInt64(11243484188358716120),
    TPasDblStrUtilsUInt64(1721567512383298469)),
    (TPasDblStrUtilsUInt64(219297180166231438),
    TPasDblStrUtilsUInt64(2151959390479123087)),
    (TPasDblStrUtilsUInt64(7054589765244976505),
    TPasDblStrUtilsUInt64(1344974619049451929)),
    (TPasDblStrUtilsUInt64(13429923224983608535),
    TPasDblStrUtilsUInt64(1681218273811814911)),
    (TPasDblStrUtilsUInt64(12175718012802122765),
    TPasDblStrUtilsUInt64(2101522842264768639)),
    (TPasDblStrUtilsUInt64(14527352785642408584),
    TPasDblStrUtilsUInt64(1313451776415480399)),
    (TPasDblStrUtilsUInt64(13547504963625622826),
    TPasDblStrUtilsUInt64(1641814720519350499)),
    (TPasDblStrUtilsUInt64(12322695186104640628),
    TPasDblStrUtilsUInt64(2052268400649188124)),
    (TPasDblStrUtilsUInt64(16925056528170176201),
    TPasDblStrUtilsUInt64(1282667750405742577)),
    (TPasDblStrUtilsUInt64(7321262604930556539),
    TPasDblStrUtilsUInt64(1603334688007178222)),
    (TPasDblStrUtilsUInt64(18374950293017971482),
    TPasDblStrUtilsUInt64(2004168360008972777)),
    (TPasDblStrUtilsUInt64(4566814905495150320),
    TPasDblStrUtilsUInt64(1252605225005607986)),
    (TPasDblStrUtilsUInt64(14931890668723713708),
    TPasDblStrUtilsUInt64(1565756531257009982)),
    (TPasDblStrUtilsUInt64(9441491299049866327),
    TPasDblStrUtilsUInt64(1957195664071262478)),
    (TPasDblStrUtilsUInt64(1289246043478778550),
    TPasDblStrUtilsUInt64(1223247290044539049)),
    (TPasDblStrUtilsUInt64(6223243572775861092),
    TPasDblStrUtilsUInt64(1529059112555673811)),
    (TPasDblStrUtilsUInt64(3167368447542438461),
    TPasDblStrUtilsUInt64(1911323890694592264)),
    (TPasDblStrUtilsUInt64(1979605279714024038),
    TPasDblStrUtilsUInt64(1194577431684120165)),
    (TPasDblStrUtilsUInt64(7086192618069917952),
    TPasDblStrUtilsUInt64(1493221789605150206)),
    (TPasDblStrUtilsUInt64(18081112809442173248),
    TPasDblStrUtilsUInt64(1866527237006437757)),
    (TPasDblStrUtilsUInt64(13606538515115052232),
    TPasDblStrUtilsUInt64(1166579523129023598)),
    (TPasDblStrUtilsUInt64(7784801107039039482),
    TPasDblStrUtilsUInt64(1458224403911279498)),
    (TPasDblStrUtilsUInt64(507629346944023544),
    TPasDblStrUtilsUInt64(1822780504889099373)),
    (TPasDblStrUtilsUInt64(5246222702107417334),
    TPasDblStrUtilsUInt64(2278475631111374216)),
    (TPasDblStrUtilsUInt64(3278889188817135834),
    TPasDblStrUtilsUInt64(1424047269444608885)),
    (TPasDblStrUtilsUInt64(8710297504448807696),
    TPasDblStrUtilsUInt64(1780059086805761106)));

function UInt64Bits2Double(const Bits: TPasDblStrUtilsUInt64)
  : TPasDblStrUtilsDouble; {$IFDEF caninline}inline; {$ENDIF}
begin
  result := TPasDblStrUtilsDouble(Pointer(@Bits)^);
end;

function FallbackStringToDouble(const aStringValue: PPasDblStrUtilsChar;
  const aStringLength: TPasDblStrUtilsInt32;
  const aRoundingMode: TPasDblStrUtilsRoundingMode = rmNearest;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aBase: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsDouble;
const
  Bits = 512; // [64,128,256,512]
var
  OK: TPasDblStrUtilsBoolean;
  TemporaryFloat: array [0 .. 7] of TPasDblStrUtilsUInt64;
  IEEEExponent, Count, FullExp, ExpOfs: TPasDblStrUtilsInt32;
  IEEEMantissa: TPasDblStrUtilsUInt64;
  ui128: TPasDblStrUtilsUInt128;
  SignedMantissa, RoundNearestEven, HasResult: TPasDblStrUtilsBoolean;
  RoundIncrement, RoundBits: TPasDblStrUtilsInt16;
  IEEEFormat: PIEEEFormat;
begin
  case TPasDblStrUtilsInt32(Bits) of
    64:
      begin
        IEEEFormat := @IEEEFormat64;
      end;
    128:
      begin
        IEEEFormat := @IEEEFormat128;
      end;
    256:
      begin
        IEEEFormat := @IEEEFormat256;
      end;
    512:
      begin
        IEEEFormat := @IEEEFormat512;
      end;
  else
    begin
      result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
      // NaN
      if assigned(aOK) then
      begin
        aOK^ := false;
      end;
      exit;
    end;
  end;
  FillChar(TemporaryFloat, sizeof(TemporaryFloat), #0);
  if Bits = 64 then
  begin
    OK := StringToFloat(aStringValue, aStringLength, TemporaryFloat,
      IEEEFormat^, aRoundingMode, false, aBase);
  end
  else
  begin
    OK := StringToFloat(aStringValue, aStringLength, TemporaryFloat,
      IEEEFormat^, rmNearest, false, aBase);
  end;
  if OK then
  begin
    if Bits = 64 then
    begin
      result := UInt64Bits2Double(TemporaryFloat[0]);
    end
    else
    begin
      case TPasDblStrUtilsInt32(Bits) of
        128:
          begin
            ui128 := (TPasDblStrUtilsUInt128(TemporaryFloat[1] and
              TPasDblStrUtilsUInt64($0000FFFFFFFFFFFF)) shl 80) or
              (TPasDblStrUtilsUInt128(TemporaryFloat[0] and
              TPasDblStrUtilsUInt64($FFFFFFFFFFFFFFFF)) shl 16);
            IEEEExponent := (TemporaryFloat[1] shr 48) and $7FFF;
            SignedMantissa := (TemporaryFloat[1] shr 63) <> 0;
            FullExp := $7FFF;
            ExpOfs := $3C01;
          end;
        256:
          begin
            ui128 := (TPasDblStrUtilsUInt128(TemporaryFloat[3] and
              TPasDblStrUtilsUInt64($00000FFFFFFFFFFF)) shl 84) or
              (TPasDblStrUtilsUInt128(TemporaryFloat[2] and
              TPasDblStrUtilsUInt64($FFFFFFFFFFFFFFFF)) shl 20) or
              (TPasDblStrUtilsUInt128(TemporaryFloat[1] and
              TPasDblStrUtilsUInt64($FFFFF00000000000)) shr 44);
            IEEEExponent := (TemporaryFloat[3] shr 44) and $7FFFF;
            SignedMantissa := (TemporaryFloat[3] shr 63) <> 0;
            FullExp := $7FFFF;
            ExpOfs := $3FC01;
          end;
        512:
          begin
            ui128 := (TPasDblStrUtilsUInt128(TemporaryFloat[7] and
              TPasDblStrUtilsUInt64($000000FFFFFFFFFF)) shl 88) or
              (TPasDblStrUtilsUInt128(TemporaryFloat[6] and
              TPasDblStrUtilsUInt64($FFFFFFFFFFFFFFFF)) shl 24) or
              (TPasDblStrUtilsUInt128(TemporaryFloat[5] and
              TPasDblStrUtilsUInt64($FFFFFF0000000000)) shr 40);
            IEEEExponent := (TemporaryFloat[7] shr 40) and $7FFFFF;
            SignedMantissa := (TemporaryFloat[7] shr 63) <> 0;
            FullExp := $7FFFFF;
            ExpOfs := $3FFC01;
          end;
      else
        begin
          exit;
        end;
      end;
      if IEEEExponent = FullExp then
      begin
        if ui128 <> 0 then
        begin
          result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF0000000000000)
            or TPasDblStrUtilsUInt64(ui128.Hi) or
            (TPasDblStrUtilsUInt64(SignedMantissa) shl 63)); // -/+(Q|S)NaN
        end
        else
        begin
          result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF0000000000000)
            or (TPasDblStrUtilsUInt64(SignedMantissa) shl 63)); // -/+Inf
        end;
      end
      else
      begin
        ui128 := ui128 shr 2;
        IEEEMantissa := ui128.Hi or (ord(ui128.Lo <> 0) and 1);
        if (IEEEExponent <> 0) or (IEEEMantissa <> 0) then
        begin
          IEEEMantissa := IEEEMantissa or
            TPasDblStrUtilsUInt64($4000000000000000);
          dec(IEEEExponent, ExpOfs);
        end;
        case aRoundingMode of
          rmTruncate:
            begin
              RoundNearestEven := false;
              RoundIncrement := 0;
            end;
          rmUp:
            Begin
              RoundNearestEven := false;
              if SignedMantissa then
              begin
                RoundIncrement := 0;
              end
              else
              begin
                RoundIncrement := $3FF;
              end;
            end;
          rmDown:
            Begin
              RoundNearestEven := false;
              if SignedMantissa then
              begin
                RoundIncrement := $3FF;
              end
              else
              begin
                RoundIncrement := 0;
              end;
            end;
        else { rmNearest: }
          begin
            RoundNearestEven := true;
            RoundIncrement := $200;
          end;
        end;
        RoundBits := IEEEMantissa and $3FF;
        HasResult := false;
        if $7FD <= TPasDblStrUtilsUInt16(IEEEExponent) then
        begin
          if ($7FD < IEEEExponent) or
            ((IEEEExponent = $7FD) and
            (TPasDblStrUtilsInt64(IEEEMantissa + RoundIncrement) < 0)) then
          begin
            result := UInt64Bits2Double
              (((TPasDblStrUtilsUInt64(ord(SignedMantissa) and 1) shl 63) or
              (TPasDblStrUtilsUInt64($7FF) shl 52) or
              (0 and ((TPasDblStrUtilsUInt64(1) shl 52) - 1))) -
              (ord(RoundIncrement = 0) and 1));
            HasResult := true;
          end
          else if IEEEExponent < 0 then
          begin
            Count := -IEEEExponent;
            if Count <> 0 then
            begin
              if Count < 64 then
              begin
                IEEEMantissa := (IEEEMantissa shr Count) or
                  ord((IEEEMantissa shl ((-Count) and 63)) <> 0) and 1;
              end
              else
              begin
                IEEEMantissa := ord(IEEEMantissa <> 0) and 1;
              end;
            end;
            IEEEExponent := 0;
            RoundBits := IEEEMantissa and $3FF;
          end;
        end;
        if not HasResult then
        begin
          IEEEMantissa := (IEEEMantissa + RoundIncrement) shr 10;
          IEEEMantissa := IEEEMantissa and not TPasDblStrUtilsUInt64
            (ord(((RoundBits xor $200) = 0) and RoundNearestEven) and 1);
          if IEEEMantissa = 0 then
          begin
            IEEEExponent := 0;
          end;
          result := UInt64Bits2Double
            ((TPasDblStrUtilsUInt64(ord(SignedMantissa) and 1) shl 63) +
            (TPasDblStrUtilsUInt64(IEEEExponent) shl 52) + IEEEMantissa);
        end;
      end;
    end;
  end
  else
  begin
    result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
    // NaN
  end;
  if assigned(aOK) then
  begin
    aOK^ := OK;
  end;
end;

function FallbackStringToDouble(const aStringValue: TPasDblStrUtilsString;
  const aRoundingMode: TPasDblStrUtilsRoundingMode = rmNearest;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aBase: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsDouble;
begin
  result := FallbackStringToDouble(@aStringValue[1], length(aStringValue),
    aRoundingMode, aOK, aBase);
end;

function AlgorithmMStringToDouble(const aStringValue: PPasDblStrUtilsChar;
  const aStringLength: TPasDblStrUtilsInt32;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aBase: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsDouble; overload;
const
  DOUBLE_MANTISSA_BITS = 52;
  DOUBLE_EXPONENT_BITS = 11;
  DOUBLE_EXPONENT_BIAS = 1023;
  MAX_EXP = (1 shl (DOUBLE_EXPONENT_BITS - 1)) - 1;
  MIN_EXP = (-MAX_EXP) + 1;
  MIN_EXP_INT = MIN_EXP - DOUBLE_MANTISSA_BITS;
  MAX_EXP_INT = MAX_EXP - DOUBLE_MANTISSA_BITS;
  MAX_SIG = TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(1)
    shl (DOUBLE_MANTISSA_BITS + 1)) - 1;
  MIN_SIG = TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(1)
    shl DOUBLE_MANTISSA_BITS);
  TargetRatio = DOUBLE_MANTISSA_BITS + 1;
  RoundNone = 0;
  RoundByRemainder = 1;
  RoundUnderflow = 2;
type
  TPowerTable = array [1 .. 16] of TPasDblStrUtilsUInt32;
  PPowerTable = ^TPowerTable;
  TAllowedChars = set of TPasDblStrUtilsChar;
const
  Power2Table: TPowerTable = (2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048,
    4096, 8192, 16384, 32768, 65536);
  Power4Table: TPowerTable = (4, 16, 64, 256, 1024, 4096, 16384, 65536, 262144,
    1048576, 4194304, 16777216, 0, 0, 0, 0);
  Power8Table: TPowerTable = (8, 64, 512, 4096, 32768, 262144, 2097152,
    16777216, 134217728, 1073741824, 0, 0, 0, 0, 0, 0);
  Power10Table: TPowerTable = (10, 100, 1000, 10000, 100000, 1000000, 10000000,
    100000000, 1000000000, 0, 0, 0, 0, 0, 0, 0);
  Power16Table: TPowerTable = ($10, $100, $1000, $10000, $100000, $1000000,
    $10000000, 0, 0, 0, 0, 0, 0, 0, 0, 0);
var
  Index, Position, uParserBufferSize, uParserBufferLimit, RoundMode, Exponent,
    Cmp, Log2U, Log2V, UShift, VShift, Log2Ratio, BitLen, LeastSignificantBit
    : TPasDblStrUtilsInt32;
  Remainder, u, v, x: TPasDblStrUtilsBigUnsignedInteger;
  uParserBuffer, Base: TPasDblStrUtilsUInt32;
  uExponent, ExponentValue, IEEEExponent: TPasDblStrUtilsInt64;
  IEEEMantissa: TPasDblStrUtilsUInt64;
  HasDigits, SignedMantissa, SignedExponent, Underflow, Even: Boolean;
  c: TPasDblStrUtilsChar;
  PowerTable: PPowerTable;
  AllowedChars: TAllowedChars;
begin

  SignedMantissa := false;
  Position := 0;
  u := 0;
  uExponent := 0;

  while (Position < aStringLength) and
    (aStringValue[Position] in [#0 .. #32]) do
  begin
    inc(Position);
  end;

  while (Position < aStringLength) and (aStringValue[Position] in ['-', '+']) do
  begin
    SignedMantissa := SignedMantissa xor (aStringValue[Position] = '-');
    inc(Position);
  end;

  if (Position + 2) < aStringLength then
  begin
    if (aStringValue[Position] in ['n', 'N']) and
      (aStringValue[Position + 1] in ['a', 'A']) and
      (aStringValue[Position + 2] in ['n', 'N']) then
    begin
      if SignedMantissa then
      begin
        result := UInt64Bits2Double(TPasDblStrUtilsUInt64($FFF8000000000000));
        // -NaN
      end
      else
      begin
        result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
        // +NaN
      end;
      if assigned(aOK) then
      begin
        aOK^ := true;
      end;
      exit;
    end
    else if (aStringValue[Position] in ['i', 'I']) and
      (aStringValue[Position + 1] in ['n', 'N']) and
      (aStringValue[Position + 2] in ['f', 'F']) then
    begin
      if SignedMantissa then
      begin
        result := UInt64Bits2Double(TPasDblStrUtilsUInt64($FFF0000000000000));
        // -Inf
      end
      else
      begin
        result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF0000000000000));
        // +Inf
      end;
      if assigned(aOK) then
      begin
        aOK^ := true;
      end;
      exit;
    end;
  end;

  if ((Position + 3) < aStringLength) and
    (aStringValue[Position] in ['q', 'Q', 's', 'S']) and
    (aStringValue[Position + 1] in ['n', 'N']) and
    (aStringValue[Position + 2] in ['a', 'A']) and
    (aStringValue[Position + 3] in ['n', 'N']) then
  begin
    if aStringValue[Position] in ['q', 'Q'] then
    begin
      if SignedMantissa then
      begin
        result := UInt64Bits2Double(TPasDblStrUtilsUInt64($FFF8000000000000));
        // -QNaN
      end
      else
      begin
        result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
        // +QNaN
      end;
    end
    else
    begin
      if SignedMantissa then
      begin
        result := UInt64Bits2Double(TPasDblStrUtilsUInt64($FFFFFFFFFFFFFFFF));
        // -SNaN
      end
      else
      begin
        result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FFFFFFFFFFFFFFF));
        // +SNaN
      end;
    end;
    if assigned(aOK) then
    begin
      aOK^ := true;
    end;
    exit;
  end;

  if aBase < 0 then
  begin
    if ((Position + 1) < aStringLength) and (aStringValue[Position] = '0') and
      (aStringValue[Position + 1] in ['b', 'B', 'y', 'Y', 'o', 'O', 'q', 'Q',
      'd', 'D', 't', 'T', 'x', 'X', 'h', 'H']) then
    begin
      case aStringValue[Position + 1] of
        'b', 'B', 'y', 'Y':
          begin
            Base := 2;
          end;
        'o', 'O', 'q', 'Q':
          begin
            Base := 8;
          end;
        'd', 'D', 't', 'T':
          begin
            Base := 10;
          end;
        'x', 'X', 'h', 'H':
          begin
            Base := 16;
          end;
      else
        begin
          Base := 10;
        end;
      end;
      inc(Position, 2);
    end
    else if (Position < aStringLength) and
      (aStringValue[Position] in ['%', '&', '$']) then
    begin
      case aStringValue[Position] of
        '%':
          begin
            Base := 2;
          end;
        '&':
          begin
            Base := 8;
          end;
        '$':
          begin
            Base := 16;
          end;
      else
        begin
          Base := 10;
        end;
      end;
      inc(Position);
    end
    else
    begin
      Base := 10;
    end;
  end
  else
  begin
    Base := aBase;
  end;

  uParserBuffer := 0;
  uParserBufferSize := 0;

  case Base of
    2:
      begin
        PowerTable := @Power2Table;
        AllowedChars := ['0' .. '1'];
        uParserBufferLimit := 16;
      end;
    4:
      begin
        PowerTable := @Power4Table;
        AllowedChars := ['0' .. '3'];
        uParserBufferLimit := 12;
      end;
    8:
      begin
        PowerTable := @Power8Table;
        AllowedChars := ['0' .. '7'];
        uParserBufferLimit := 10;
      end;
    10:
      begin
        PowerTable := @Power10Table;
        AllowedChars := ['0' .. '9'];
        uParserBufferLimit := 9;
      end;
    16:
      begin
        PowerTable := @Power16Table;
        AllowedChars := ['0' .. '9', 'a' .. 'f', 'A' .. 'F'];
        uParserBufferLimit := 7;
      end;
  else
    begin
      result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
      // NaN
      if assigned(aOK) then
      begin
        aOK^ := false;
      end;
      exit;
    end;
  end;

  HasDigits := (Position < aStringLength) and
    (aStringValue[Position] in AllowedChars);
  if HasDigits then
  begin
    while Position < aStringLength do
    begin
      c := aStringValue[Position];
      if c in AllowedChars then
      begin
        case c of
          '0' .. '9':
            begin
              uParserBuffer := (uParserBuffer * Base) +
                TPasDblStrUtilsUInt8(TPasDblStrUtilsChar(aStringValue[Position])
                ) - TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'));
              inc(uParserBufferSize);
              if uParserBufferSize >= uParserBufferLimit then
              begin
                u.MulAdd(PowerTable^[uParserBufferSize], uParserBuffer);
                uParserBufferSize := 0;
                uParserBuffer := 0;
              end;
              inc(Position);
            end;
          'a' .. 'z':
            begin
              uParserBuffer := (uParserBuffer * Base) +
                (TPasDblStrUtilsUInt8(TPasDblStrUtilsChar(aStringValue[Position]
                )) - TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('a')) + $A);
              inc(uParserBufferSize);
              if uParserBufferSize >= uParserBufferLimit then
              begin
                u.MulAdd(PowerTable^[uParserBufferSize], uParserBuffer);
                uParserBufferSize := 0;
                uParserBuffer := 0;
              end;
              inc(Position);
            end;
          'A' .. 'Z':
            begin
              uParserBuffer := (uParserBuffer * Base) +
                (TPasDblStrUtilsUInt8(TPasDblStrUtilsChar(aStringValue[Position]
                )) - TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('A')) + $A);
              inc(uParserBufferSize);
              if uParserBufferSize >= uParserBufferLimit then
              begin
                u.MulAdd(PowerTable^[uParserBufferSize], uParserBuffer);
                uParserBufferSize := 0;
                uParserBuffer := 0;
              end;
              inc(Position);
            end;
        else
          begin
            break;
          end;
        end;
      end
      else
      begin
        break;
      end;
    end;
  end;

  if (Position < aStringLength) and (aStringValue[Position] = '.') then
  begin
    inc(Position);
    if (Position < aStringLength) and (aStringValue[Position] in AllowedChars)
    then
    begin
      HasDigits := true;
      while Position < aStringLength do
      begin
        c := aStringValue[Position];
        if c in AllowedChars then
        begin
          case c of
            '0' .. '9':
              begin
                uParserBuffer := (uParserBuffer * Base) +
                  TPasDblStrUtilsUInt8
                  (TPasDblStrUtilsChar(aStringValue[Position])) -
                  TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'));
                inc(uParserBufferSize);
                if uParserBufferSize >= uParserBufferLimit then
                begin
                  u.MulAdd(PowerTable^[uParserBufferSize], uParserBuffer);
                  uParserBufferSize := 0;
                  uParserBuffer := 0;
                end;
                inc(Position);
                dec(uExponent);
              end;
            'a' .. 'z':
              begin
                uParserBuffer := (uParserBuffer * Base) +
                  (TPasDblStrUtilsUInt8(TPasDblStrUtilsChar(aStringValue
                  [Position])) - TPasDblStrUtilsUInt8
                  (TPasDblStrUtilsChar('a')) + $A);
                inc(uParserBufferSize);
                if uParserBufferSize >= uParserBufferLimit then
                begin
                  u.MulAdd(PowerTable^[uParserBufferSize], uParserBuffer);
                  uParserBufferSize := 0;
                  uParserBuffer := 0;
                end;
                inc(Position);
                dec(uExponent);
              end;
            'A' .. 'Z':
              begin
                uParserBuffer := (uParserBuffer * Base) +
                  (TPasDblStrUtilsUInt8(TPasDblStrUtilsChar(aStringValue
                  [Position])) - TPasDblStrUtilsUInt8
                  (TPasDblStrUtilsChar('A')) + $A);
                inc(uParserBufferSize);
                if uParserBufferSize >= uParserBufferLimit then
                begin
                  u.MulAdd(PowerTable^[uParserBufferSize], uParserBuffer);
                  uParserBufferSize := 0;
                  uParserBuffer := 0;
                end;
                inc(Position);
                dec(uExponent);
              end;
          else
            begin
              break;
            end;
          end;
        end
        else
        begin
          break;
        end;
      end;
    end;
  end;

  if uParserBufferSize > 0 then
  begin
    u.MulAdd(PowerTable^[uParserBufferSize], uParserBuffer);
    uParserBufferSize := 0;
  end;

  if not HasDigits then
  begin
    result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
    // NaN
    if assigned(aOK) then
    begin
      aOK^ := false;
    end;
    exit;
  end;

  if (Position < aStringLength) and
    (aStringValue[Position] in ['e', 'E', 'p', 'P']) then
  begin
    inc(Position);
    if (Position < aStringLength) and (aStringValue[Position] in ['+', '-'])
    then
    begin
      SignedExponent := aStringValue[Position] = '-';
      inc(Position);
    end
    else
    begin
      SignedExponent := false;
    end;
    if (Position < aStringLength) and (aStringValue[Position] in ['0' .. '9'])
    then
    begin
      ExponentValue := 0;
      repeat
        ExponentValue := (ExponentValue * 10) + TPasDblStrUtilsInt32
          (TPasDblStrUtilsUInt8(TPasDblStrUtilsChar(aStringValue[Position])) -
          TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0')));
        inc(Position);
      until (Position >= aStringLength) or
        not(aStringValue[Position] in ['0' .. '9']);
      if SignedExponent then
      begin
        dec(uExponent, ExponentValue);
      end
      else
      begin
        inc(uExponent, ExponentValue);
      end;
    end
    else
    begin
      result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
      // NaN
      if assigned(aOK) then
      begin
        aOK^ := false;
      end;
      exit;
    end;
  end;

  if u.IsZero then
  begin
    result := UInt64Bits2Double
      (TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(ord(SignedMantissa) and 1)
      shl 63)); // +/- 0
    if assigned(aOK) then
    begin
      aOK^ := true;
    end;
    exit;
  end;

  if Position >= aStringLength then
  begin

    v := 1;

    if uExponent <> 0 then
    begin
      case Base of
        2:
          begin
            if uExponent > 0 then
            begin
              u.ShiftLeft(uExponent);
            end
            else
            begin
              v.ShiftLeft(-uExponent);
            end;
          end;
        10:
          begin
            if uExponent > 0 then
            begin
              u.MultiplyPower10(uExponent);
            end
            else
            begin
              v.MultiplyPower10(-uExponent);
            end;
          end;
      else
        begin
          if uExponent > 0 then
          begin
            u.Mul(TPasDblStrUtilsBigUnsignedInteger.Power(Base, uExponent));
          end
          else
          begin
            v.Mul(TPasDblStrUtilsBigUnsignedInteger.Power(Base, -uExponent));
          end;
        end;
      end;
    end;

    Remainder := 0;
    x := 0;

    Underflow := false;

    Exponent := 0;

    Log2U := u.Bits;
    Log2V := v.Bits;
    UShift := 0;
    VShift := 0;
    repeat
      case Exponent of
        MIN_EXP_INT, MAX_EXP_INT:
          begin
            break;
          end;
      else
        begin
          Log2Ratio := (Log2U + UShift) - (Log2V + VShift);
          if Log2Ratio < (TargetRatio - 1) then
          begin
            inc(UShift);
            dec(Exponent);
          end
          else if Log2Ratio > (TargetRatio + 1) then
          begin
            inc(VShift);
            inc(Exponent);
          end
          else
          begin
            break;
          end;
        end;
      end;
    until false;
    u.ShiftLeft(UShift);
    v.ShiftLeft(VShift);

    repeat
      u.DivMod(v, x, Remainder);
      if Exponent <= MIN_EXP_INT then
      begin
        if (x.Compare(MIN_SIG) >= 0) and (x.Compare(MAX_SIG) <= 0) then
        begin
          break;
        end;
        Underflow := true;
        break;
      end;
      if Exponent > MAX_EXP_INT then
      begin
        result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF0000000000000) or
          (TPasDblStrUtilsUInt64(ord(SignedMantissa) and 1) shl 63)); // -/+Inf
        exit;
      end;
      if x.Compare(MIN_SIG) < 0 then
      begin
        u.ShiftLeftByOne;
        dec(Exponent);
      end
      else if x.Compare(MAX_SIG) > 0 then
      begin
        v.ShiftLeftByOne;
        inc(Exponent);
      end
      else
      begin
        break;
      end;
    until false;

    RoundMode := RoundNone;

    LeastSignificantBit := 0;

    if Underflow then
    begin
      if x.Compare(MIN_SIG) < 0 then
      begin
        IEEEMantissa := TPasDblStrUtilsUInt64(x);
        IEEEExponent := 0;
        RoundMode := RoundByRemainder;
      end
      else
      begin
        BitLen := x.Bits;
        LeastSignificantBit := BitLen - (DOUBLE_MANTISSA_BITS + 1);
        IEEEMantissa := 0;
        for Index := LeastSignificantBit to BitLen do
        begin
          IEEEMantissa := (IEEEMantissa shl 1) or
            ((x.Words[Index shr 5] shr (Index and 31)) and 1);
        end;
        IEEEExponent := (MIN_EXP_INT + LeastSignificantBit) + MAX_EXP +
          DOUBLE_MANTISSA_BITS;
        RoundMode := RoundUnderflow;
      end;
    end
    else
    begin
      IEEEMantissa := TPasDblStrUtilsUInt64(x);
      IEEEExponent := Exponent + MAX_EXP + DOUBLE_MANTISSA_BITS;
      RoundMode := RoundByRemainder;
    end;

    result := UInt64Bits2Double((TPasDblStrUtilsUInt64(ord(SignedMantissa) and
      1) shl 63) or (TPasDblStrUtilsUInt64(IEEEExponent) shl 52) or
      (IEEEMantissa and not(TPasDblStrUtilsUInt64(1) shl 52)));

    case RoundMode of
      RoundByRemainder:
        begin
          x := v;
          x.Sub(Remainder);
          Cmp := Remainder.Compare(x);
          if (Cmp > 0) or ((Cmp = 0) and ((IEEEMantissa and 1) <> 0)) then
          begin
            inc(TPasDblStrUtilsUInt64(Pointer(@result)^));
          end;
        end;
      RoundUnderflow:
        begin
          Even := (IEEEMantissa and 1) = 0;
          if (LeastSignificantBit = 0) or
            (((x.Words[(LeastSignificantBit - 1) shr 5]
            shr ((LeastSignificantBit - 1) and 31)) and 1) = 0) then
          begin
            // Nothing, because less than 0.5 ULP
          end
          else
          begin
            Cmp := 0;
            for Index := 0 to LeastSignificantBit do
            begin
              if ((x.Words[Index shr 5] shr (Index and 31)) and 1) <> 0 then
              begin
                Cmp := 1;
                break;
              end;
            end;
            if (Cmp > 0) or ((Cmp = 0) and Remainder.IsZero and Even) then
            begin
              inc(TPasDblStrUtilsUInt64(Pointer(@result)^));
            end;
          end;
        end;
    else { RoundNone: }
      begin
      end;
    end;

    if assigned(aOK) then
    begin
      aOK^ := true;
    end;

  end
  else
  begin

    result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
    // NaN
    if assigned(aOK) then
    begin
      aOK^ := false;
    end;

  end;

end;

function AlgorithmMStringToDouble(const aStringValue: TPasDblStrUtilsString;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aBase: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsDouble; overload;
begin
  result := AlgorithmMStringToDouble(@aStringValue[1], length(aStringValue),
    aOK, aBase);
end;

const
  FASTFLOAT_SMALLEST_POWER = -325;
  FASTFLOAT_LARGEST_POWER = 308;

function ComputeFloat64(const aBase10Exponent: TPasDblStrUtilsInt64;
  aBase10Mantissa: TPasDblStrUtilsUInt64;
  const aNegative: TPasDblStrUtilsBoolean;
  const aSuccess: PPasDblStrUtilsBoolean): TPasDblStrUtilsDouble;
const
  PowerOfTen: array [0 .. 22] of TPasDblStrUtilsDouble = (1E0, 1E1, 1E2, 1E3,
    1E4, 1E5, 1E6, 1E7, 1E8, 1E9, 1E10, 1E11, 1E12, 1E13, 1E14, 1E15, 1E16,
    1E17, 1E18, 1E19, 1E20, 1E21, 1E22);
  Mantissa64: array [FASTFLOAT_SMALLEST_POWER .. FASTFLOAT_LARGEST_POWER]
    of TPasDblStrUtilsUInt64 = (TPasDblStrUtilsUInt64($A5CED43B7E3E9188),
    TPasDblStrUtilsUInt64($CF42894A5DCE35EA),
    TPasDblStrUtilsUInt64($818995CE7AA0E1B2),
    TPasDblStrUtilsUInt64($A1EBFB4219491A1F),
    TPasDblStrUtilsUInt64($CA66FA129F9B60A6),
    TPasDblStrUtilsUInt64($FD00B897478238D0),
    TPasDblStrUtilsUInt64($9E20735E8CB16382),
    TPasDblStrUtilsUInt64($C5A890362FDDBC62),
    TPasDblStrUtilsUInt64($F712B443BBD52B7B),
    TPasDblStrUtilsUInt64($9A6BB0AA55653B2D),
    TPasDblStrUtilsUInt64($C1069CD4EABE89F8),
    TPasDblStrUtilsUInt64($F148440A256E2C76),
    TPasDblStrUtilsUInt64($96CD2A865764DBCA),
    TPasDblStrUtilsUInt64($BC807527ED3E12BC),
    TPasDblStrUtilsUInt64($EBA09271E88D976B),
    TPasDblStrUtilsUInt64($93445B8731587EA3),
    TPasDblStrUtilsUInt64($B8157268FDAE9E4C),
    TPasDblStrUtilsUInt64($E61ACF033D1A45DF),
    TPasDblStrUtilsUInt64($8FD0C16206306BAB),
    TPasDblStrUtilsUInt64($B3C4F1BA87BC8696),
    TPasDblStrUtilsUInt64($E0B62E2929ABA83C),
    TPasDblStrUtilsUInt64($8C71DCD9BA0B4925),
    TPasDblStrUtilsUInt64($AF8E5410288E1B6F),
    TPasDblStrUtilsUInt64($DB71E91432B1A24A),
    TPasDblStrUtilsUInt64($892731AC9FAF056E),
    TPasDblStrUtilsUInt64($AB70FE17C79AC6CA),
    TPasDblStrUtilsUInt64($D64D3D9DB981787D),
    TPasDblStrUtilsUInt64($85F0468293F0EB4E),
    TPasDblStrUtilsUInt64($A76C582338ED2621),
    TPasDblStrUtilsUInt64($D1476E2C07286FAA),
    TPasDblStrUtilsUInt64($82CCA4DB847945CA),
    TPasDblStrUtilsUInt64($A37FCE126597973C),
    TPasDblStrUtilsUInt64($CC5FC196FEFD7D0C),
    TPasDblStrUtilsUInt64($FF77B1FCBEBCDC4F),
    TPasDblStrUtilsUInt64($9FAACF3DF73609B1),
    TPasDblStrUtilsUInt64($C795830D75038C1D),
    TPasDblStrUtilsUInt64($F97AE3D0D2446F25),
    TPasDblStrUtilsUInt64($9BECCE62836AC577),
    TPasDblStrUtilsUInt64($C2E801FB244576D5),
    TPasDblStrUtilsUInt64($F3A20279ED56D48A),
    TPasDblStrUtilsUInt64($9845418C345644D6),
    TPasDblStrUtilsUInt64($BE5691EF416BD60C),
    TPasDblStrUtilsUInt64($EDEC366B11C6CB8F),
    TPasDblStrUtilsUInt64($94B3A202EB1C3F39),
    TPasDblStrUtilsUInt64($B9E08A83A5E34F07),
    TPasDblStrUtilsUInt64($E858AD248F5C22C9),
    TPasDblStrUtilsUInt64($91376C36D99995BE),
    TPasDblStrUtilsUInt64($B58547448FFFFB2D),
    TPasDblStrUtilsUInt64($E2E69915B3FFF9F9),
    TPasDblStrUtilsUInt64($8DD01FAD907FFC3B),
    TPasDblStrUtilsUInt64($B1442798F49FFB4A),
    TPasDblStrUtilsUInt64($DD95317F31C7FA1D),
    TPasDblStrUtilsUInt64($8A7D3EEF7F1CFC52),
    TPasDblStrUtilsUInt64($AD1C8EAB5EE43B66),
    TPasDblStrUtilsUInt64($D863B256369D4A40),
    TPasDblStrUtilsUInt64($873E4F75E2224E68),
    TPasDblStrUtilsUInt64($A90DE3535AAAE202),
    TPasDblStrUtilsUInt64($D3515C2831559A83),
    TPasDblStrUtilsUInt64($8412D9991ED58091),
    TPasDblStrUtilsUInt64($A5178FFF668AE0B6),
    TPasDblStrUtilsUInt64($CE5D73FF402D98E3),
    TPasDblStrUtilsUInt64($80FA687F881C7F8E),
    TPasDblStrUtilsUInt64($A139029F6A239F72),
    TPasDblStrUtilsUInt64($C987434744AC874E),
    TPasDblStrUtilsUInt64($FBE9141915D7A922),
    TPasDblStrUtilsUInt64($9D71AC8FADA6C9B5),
    TPasDblStrUtilsUInt64($C4CE17B399107C22),
    TPasDblStrUtilsUInt64($F6019DA07F549B2B),
    TPasDblStrUtilsUInt64($99C102844F94E0FB),
    TPasDblStrUtilsUInt64($C0314325637A1939),
    TPasDblStrUtilsUInt64($F03D93EEBC589F88),
    TPasDblStrUtilsUInt64($96267C7535B763B5),
    TPasDblStrUtilsUInt64($BBB01B9283253CA2),
    TPasDblStrUtilsUInt64($EA9C227723EE8BCB),
    TPasDblStrUtilsUInt64($92A1958A7675175F),
    TPasDblStrUtilsUInt64($B749FAED14125D36),
    TPasDblStrUtilsUInt64($E51C79A85916F484),
    TPasDblStrUtilsUInt64($8F31CC0937AE58D2),
    TPasDblStrUtilsUInt64($B2FE3F0B8599EF07),
    TPasDblStrUtilsUInt64($DFBDCECE67006AC9),
    TPasDblStrUtilsUInt64($8BD6A141006042BD),
    TPasDblStrUtilsUInt64($AECC49914078536D),
    TPasDblStrUtilsUInt64($DA7F5BF590966848),
    TPasDblStrUtilsUInt64($888F99797A5E012D),
    TPasDblStrUtilsUInt64($AAB37FD7D8F58178),
    TPasDblStrUtilsUInt64($D5605FCDCF32E1D6),
    TPasDblStrUtilsUInt64($855C3BE0A17FCD26),
    TPasDblStrUtilsUInt64($A6B34AD8C9DFC06F),
    TPasDblStrUtilsUInt64($D0601D8EFC57B08B),
    TPasDblStrUtilsUInt64($823C12795DB6CE57),
    TPasDblStrUtilsUInt64($A2CB1717B52481ED),
    TPasDblStrUtilsUInt64($CB7DDCDDA26DA268),
    TPasDblStrUtilsUInt64($FE5D54150B090B02),
    TPasDblStrUtilsUInt64($9EFA548D26E5A6E1),
    TPasDblStrUtilsUInt64($C6B8E9B0709F109A),
    TPasDblStrUtilsUInt64($F867241C8CC6D4C0),
    TPasDblStrUtilsUInt64($9B407691D7FC44F8),
    TPasDblStrUtilsUInt64($C21094364DFB5636),
    TPasDblStrUtilsUInt64($F294B943E17A2BC4),
    TPasDblStrUtilsUInt64($979CF3CA6CEC5B5A),
    TPasDblStrUtilsUInt64($BD8430BD08277231),
    TPasDblStrUtilsUInt64($ECE53CEC4A314EBD),
    TPasDblStrUtilsUInt64($940F4613AE5ED136),
    TPasDblStrUtilsUInt64($B913179899F68584),
    TPasDblStrUtilsUInt64($E757DD7EC07426E5),
    TPasDblStrUtilsUInt64($9096EA6F3848984F),
    TPasDblStrUtilsUInt64($B4BCA50B065ABE63),
    TPasDblStrUtilsUInt64($E1EBCE4DC7F16DFB),
    TPasDblStrUtilsUInt64($8D3360F09CF6E4BD),
    TPasDblStrUtilsUInt64($B080392CC4349DEC),
    TPasDblStrUtilsUInt64($DCA04777F541C567),
    TPasDblStrUtilsUInt64($89E42CAAF9491B60),
    TPasDblStrUtilsUInt64($AC5D37D5B79B6239),
    TPasDblStrUtilsUInt64($D77485CB25823AC7),
    TPasDblStrUtilsUInt64($86A8D39EF77164BC),
    TPasDblStrUtilsUInt64($A8530886B54DBDEB),
    TPasDblStrUtilsUInt64($D267CAA862A12D66),
    TPasDblStrUtilsUInt64($8380DEA93DA4BC60),
    TPasDblStrUtilsUInt64($A46116538D0DEB78),
    TPasDblStrUtilsUInt64($CD795BE870516656),
    TPasDblStrUtilsUInt64($806BD9714632DFF6),
    TPasDblStrUtilsUInt64($A086CFCD97BF97F3),
    TPasDblStrUtilsUInt64($C8A883C0FDAF7DF0),
    TPasDblStrUtilsUInt64($FAD2A4B13D1B5D6C),
    TPasDblStrUtilsUInt64($9CC3A6EEC6311A63),
    TPasDblStrUtilsUInt64($C3F490AA77BD60FC),
    TPasDblStrUtilsUInt64($F4F1B4D515ACB93B),
    TPasDblStrUtilsUInt64($991711052D8BF3C5),
    TPasDblStrUtilsUInt64($BF5CD54678EEF0B6),
    TPasDblStrUtilsUInt64($EF340A98172AACE4),
    TPasDblStrUtilsUInt64($9580869F0E7AAC0E),
    TPasDblStrUtilsUInt64($BAE0A846D2195712),
    TPasDblStrUtilsUInt64($E998D258869FACD7),
    TPasDblStrUtilsUInt64($91FF83775423CC06),
    TPasDblStrUtilsUInt64($B67F6455292CBF08),
    TPasDblStrUtilsUInt64($E41F3D6A7377EECA),
    TPasDblStrUtilsUInt64($8E938662882AF53E),
    TPasDblStrUtilsUInt64($B23867FB2A35B28D),
    TPasDblStrUtilsUInt64($DEC681F9F4C31F31),
    TPasDblStrUtilsUInt64($8B3C113C38F9F37E),
    TPasDblStrUtilsUInt64($AE0B158B4738705E),
    TPasDblStrUtilsUInt64($D98DDAEE19068C76),
    TPasDblStrUtilsUInt64($87F8A8D4CFA417C9),
    TPasDblStrUtilsUInt64($A9F6D30A038D1DBC),
    TPasDblStrUtilsUInt64($D47487CC8470652B),
    TPasDblStrUtilsUInt64($84C8D4DFD2C63F3B),
    TPasDblStrUtilsUInt64($A5FB0A17C777CF09),
    TPasDblStrUtilsUInt64($CF79CC9DB955C2CC),
    TPasDblStrUtilsUInt64($81AC1FE293D599BF),
    TPasDblStrUtilsUInt64($A21727DB38CB002F),
    TPasDblStrUtilsUInt64($CA9CF1D206FDC03B),
    TPasDblStrUtilsUInt64($FD442E4688BD304A),
    TPasDblStrUtilsUInt64($9E4A9CEC15763E2E),
    TPasDblStrUtilsUInt64($C5DD44271AD3CDBA),
    TPasDblStrUtilsUInt64($F7549530E188C128),
    TPasDblStrUtilsUInt64($9A94DD3E8CF578B9),
    TPasDblStrUtilsUInt64($C13A148E3032D6E7),
    TPasDblStrUtilsUInt64($F18899B1BC3F8CA1),
    TPasDblStrUtilsUInt64($96F5600F15A7B7E5),
    TPasDblStrUtilsUInt64($BCB2B812DB11A5DE),
    TPasDblStrUtilsUInt64($EBDF661791D60F56),
    TPasDblStrUtilsUInt64($936B9FCEBB25C995),
    TPasDblStrUtilsUInt64($B84687C269EF3BFB),
    TPasDblStrUtilsUInt64($E65829B3046B0AFA),
    TPasDblStrUtilsUInt64($8FF71A0FE2C2E6DC),
    TPasDblStrUtilsUInt64($B3F4E093DB73A093),
    TPasDblStrUtilsUInt64($E0F218B8D25088B8),
    TPasDblStrUtilsUInt64($8C974F7383725573),
    TPasDblStrUtilsUInt64($AFBD2350644EEACF),
    TPasDblStrUtilsUInt64($DBAC6C247D62A583),
    TPasDblStrUtilsUInt64($894BC396CE5DA772),
    TPasDblStrUtilsUInt64($AB9EB47C81F5114F),
    TPasDblStrUtilsUInt64($D686619BA27255A2),
    TPasDblStrUtilsUInt64($8613FD0145877585),
    TPasDblStrUtilsUInt64($A798FC4196E952E7),
    TPasDblStrUtilsUInt64($D17F3B51FCA3A7A0),
    TPasDblStrUtilsUInt64($82EF85133DE648C4),
    TPasDblStrUtilsUInt64($A3AB66580D5FDAF5),
    TPasDblStrUtilsUInt64($CC963FEE10B7D1B3),
    TPasDblStrUtilsUInt64($FFBBCFE994E5C61F),
    TPasDblStrUtilsUInt64($9FD561F1FD0F9BD3),
    TPasDblStrUtilsUInt64($C7CABA6E7C5382C8),
    TPasDblStrUtilsUInt64($F9BD690A1B68637B),
    TPasDblStrUtilsUInt64($9C1661A651213E2D),
    TPasDblStrUtilsUInt64($C31BFA0FE5698DB8),
    TPasDblStrUtilsUInt64($F3E2F893DEC3F126),
    TPasDblStrUtilsUInt64($986DDB5C6B3A76B7),
    TPasDblStrUtilsUInt64($BE89523386091465),
    TPasDblStrUtilsUInt64($EE2BA6C0678B597F),
    TPasDblStrUtilsUInt64($94DB483840B717EF),
    TPasDblStrUtilsUInt64($BA121A4650E4DDEB),
    TPasDblStrUtilsUInt64($E896A0D7E51E1566),
    TPasDblStrUtilsUInt64($915E2486EF32CD60),
    TPasDblStrUtilsUInt64($B5B5ADA8AAFF80B8),
    TPasDblStrUtilsUInt64($E3231912D5BF60E6),
    TPasDblStrUtilsUInt64($8DF5EFABC5979C8F),
    TPasDblStrUtilsUInt64($B1736B96B6FD83B3),
    TPasDblStrUtilsUInt64($DDD0467C64BCE4A0),
    TPasDblStrUtilsUInt64($8AA22C0DBEF60EE4),
    TPasDblStrUtilsUInt64($AD4AB7112EB3929D),
    TPasDblStrUtilsUInt64($D89D64D57A607744),
    TPasDblStrUtilsUInt64($87625F056C7C4A8B),
    TPasDblStrUtilsUInt64($A93AF6C6C79B5D2D),
    TPasDblStrUtilsUInt64($D389B47879823479),
    TPasDblStrUtilsUInt64($843610CB4BF160CB),
    TPasDblStrUtilsUInt64($A54394FE1EEDB8FE),
    TPasDblStrUtilsUInt64($CE947A3DA6A9273E),
    TPasDblStrUtilsUInt64($811CCC668829B887),
    TPasDblStrUtilsUInt64($A163FF802A3426A8),
    TPasDblStrUtilsUInt64($C9BCFF6034C13052),
    TPasDblStrUtilsUInt64($FC2C3F3841F17C67),
    TPasDblStrUtilsUInt64($9D9BA7832936EDC0),
    TPasDblStrUtilsUInt64($C5029163F384A931),
    TPasDblStrUtilsUInt64($F64335BCF065D37D),
    TPasDblStrUtilsUInt64($99EA0196163FA42E),
    TPasDblStrUtilsUInt64($C06481FB9BCF8D39),
    TPasDblStrUtilsUInt64($F07DA27A82C37088),
    TPasDblStrUtilsUInt64($964E858C91BA2655),
    TPasDblStrUtilsUInt64($BBE226EFB628AFEA),
    TPasDblStrUtilsUInt64($EADAB0ABA3B2DBE5),
    TPasDblStrUtilsUInt64($92C8AE6B464FC96F),
    TPasDblStrUtilsUInt64($B77ADA0617E3BBCB),
    TPasDblStrUtilsUInt64($E55990879DDCAABD),
    TPasDblStrUtilsUInt64($8F57FA54C2A9EAB6),
    TPasDblStrUtilsUInt64($B32DF8E9F3546564),
    TPasDblStrUtilsUInt64($DFF9772470297EBD),
    TPasDblStrUtilsUInt64($8BFBEA76C619EF36),
    TPasDblStrUtilsUInt64($AEFAE51477A06B03),
    TPasDblStrUtilsUInt64($DAB99E59958885C4),
    TPasDblStrUtilsUInt64($88B402F7FD75539B),
    TPasDblStrUtilsUInt64($AAE103B5FCD2A881),
    TPasDblStrUtilsUInt64($D59944A37C0752A2),
    TPasDblStrUtilsUInt64($857FCAE62D8493A5),
    TPasDblStrUtilsUInt64($A6DFBD9FB8E5B88E),
    TPasDblStrUtilsUInt64($D097AD07A71F26B2),
    TPasDblStrUtilsUInt64($825ECC24C873782F),
    TPasDblStrUtilsUInt64($A2F67F2DFA90563B),
    TPasDblStrUtilsUInt64($CBB41EF979346BCA),
    TPasDblStrUtilsUInt64($FEA126B7D78186BC),
    TPasDblStrUtilsUInt64($9F24B832E6B0F436),
    TPasDblStrUtilsUInt64($C6EDE63FA05D3143),
    TPasDblStrUtilsUInt64($F8A95FCF88747D94),
    TPasDblStrUtilsUInt64($9B69DBE1B548CE7C),
    TPasDblStrUtilsUInt64($C24452DA229B021B),
    TPasDblStrUtilsUInt64($F2D56790AB41C2A2),
    TPasDblStrUtilsUInt64($97C560BA6B0919A5),
    TPasDblStrUtilsUInt64($BDB6B8E905CB600F),
    TPasDblStrUtilsUInt64($ED246723473E3813),
    TPasDblStrUtilsUInt64($9436C0760C86E30B),
    TPasDblStrUtilsUInt64($B94470938FA89BCE),
    TPasDblStrUtilsUInt64($E7958CB87392C2C2),
    TPasDblStrUtilsUInt64($90BD77F3483BB9B9),
    TPasDblStrUtilsUInt64($B4ECD5F01A4AA828),
    TPasDblStrUtilsUInt64($E2280B6C20DD5232),
    TPasDblStrUtilsUInt64($8D590723948A535F),
    TPasDblStrUtilsUInt64($B0AF48EC79ACE837),
    TPasDblStrUtilsUInt64($DCDB1B2798182244),
    TPasDblStrUtilsUInt64($8A08F0F8BF0F156B),
    TPasDblStrUtilsUInt64($AC8B2D36EED2DAC5),
    TPasDblStrUtilsUInt64($D7ADF884AA879177),
    TPasDblStrUtilsUInt64($86CCBB52EA94BAEA),
    TPasDblStrUtilsUInt64($A87FEA27A539E9A5),
    TPasDblStrUtilsUInt64($D29FE4B18E88640E),
    TPasDblStrUtilsUInt64($83A3EEEEF9153E89),
    TPasDblStrUtilsUInt64($A48CEAAAB75A8E2B),
    TPasDblStrUtilsUInt64($CDB02555653131B6),
    TPasDblStrUtilsUInt64($808E17555F3EBF11),
    TPasDblStrUtilsUInt64($A0B19D2AB70E6ED6),
    TPasDblStrUtilsUInt64($C8DE047564D20A8B),
    TPasDblStrUtilsUInt64($FB158592BE068D2E),
    TPasDblStrUtilsUInt64($9CED737BB6C4183D),
    TPasDblStrUtilsUInt64($C428D05AA4751E4C),
    TPasDblStrUtilsUInt64($F53304714D9265DF),
    TPasDblStrUtilsUInt64($993FE2C6D07B7FAB),
    TPasDblStrUtilsUInt64($BF8FDB78849A5F96),
    TPasDblStrUtilsUInt64($EF73D256A5C0F77C),
    TPasDblStrUtilsUInt64($95A8637627989AAD),
    TPasDblStrUtilsUInt64($BB127C53B17EC159),
    TPasDblStrUtilsUInt64($E9D71B689DDE71AF),
    TPasDblStrUtilsUInt64($9226712162AB070D),
    TPasDblStrUtilsUInt64($B6B00D69BB55C8D1),
    TPasDblStrUtilsUInt64($E45C10C42A2B3B05),
    TPasDblStrUtilsUInt64($8EB98A7A9A5B04E3),
    TPasDblStrUtilsUInt64($B267ED1940F1C61C),
    TPasDblStrUtilsUInt64($DF01E85F912E37A3),
    TPasDblStrUtilsUInt64($8B61313BBABCE2C6),
    TPasDblStrUtilsUInt64($AE397D8AA96C1B77),
    TPasDblStrUtilsUInt64($D9C7DCED53C72255),
    TPasDblStrUtilsUInt64($881CEA14545C7575),
    TPasDblStrUtilsUInt64($AA242499697392D2),
    TPasDblStrUtilsUInt64($D4AD2DBFC3D07787),
    TPasDblStrUtilsUInt64($84EC3C97DA624AB4),
    TPasDblStrUtilsUInt64($A6274BBDD0FADD61),
    TPasDblStrUtilsUInt64($CFB11EAD453994BA),
    TPasDblStrUtilsUInt64($81CEB32C4B43FCF4),
    TPasDblStrUtilsUInt64($A2425FF75E14FC31),
    TPasDblStrUtilsUInt64($CAD2F7F5359A3B3E),
    TPasDblStrUtilsUInt64($FD87B5F28300CA0D),
    TPasDblStrUtilsUInt64($9E74D1B791E07E48),
    TPasDblStrUtilsUInt64($C612062576589DDA),
    TPasDblStrUtilsUInt64($F79687AED3EEC551),
    TPasDblStrUtilsUInt64($9ABE14CD44753B52),
    TPasDblStrUtilsUInt64($C16D9A0095928A27),
    TPasDblStrUtilsUInt64($F1C90080BAF72CB1),
    TPasDblStrUtilsUInt64($971DA05074DA7BEE),
    TPasDblStrUtilsUInt64($BCE5086492111AEA),
    TPasDblStrUtilsUInt64($EC1E4A7DB69561A5),
    TPasDblStrUtilsUInt64($9392EE8E921D5D07),
    TPasDblStrUtilsUInt64($B877AA3236A4B449),
    TPasDblStrUtilsUInt64($E69594BEC44DE15B),
    TPasDblStrUtilsUInt64($901D7CF73AB0ACD9),
    TPasDblStrUtilsUInt64($B424DC35095CD80F),
    TPasDblStrUtilsUInt64($E12E13424BB40E13),
    TPasDblStrUtilsUInt64($8CBCCC096F5088CB),
    TPasDblStrUtilsUInt64($AFEBFF0BCB24AAFE),
    TPasDblStrUtilsUInt64($DBE6FECEBDEDD5BE),
    TPasDblStrUtilsUInt64($89705F4136B4A597),
    TPasDblStrUtilsUInt64($ABCC77118461CEFC),
    TPasDblStrUtilsUInt64($D6BF94D5E57A42BC),
    TPasDblStrUtilsUInt64($8637BD05AF6C69B5),
    TPasDblStrUtilsUInt64($A7C5AC471B478423),
    TPasDblStrUtilsUInt64($D1B71758E219652B),
    TPasDblStrUtilsUInt64($83126E978D4FDF3B),
    TPasDblStrUtilsUInt64($A3D70A3D70A3D70A),
    TPasDblStrUtilsUInt64($CCCCCCCCCCCCCCCC),
    TPasDblStrUtilsUInt64($8000000000000000),
    TPasDblStrUtilsUInt64($A000000000000000),
    TPasDblStrUtilsUInt64($C800000000000000),
    TPasDblStrUtilsUInt64($FA00000000000000),
    TPasDblStrUtilsUInt64($9C40000000000000),
    TPasDblStrUtilsUInt64($C350000000000000),
    TPasDblStrUtilsUInt64($F424000000000000),
    TPasDblStrUtilsUInt64($9896800000000000),
    TPasDblStrUtilsUInt64($BEBC200000000000),
    TPasDblStrUtilsUInt64($EE6B280000000000),
    TPasDblStrUtilsUInt64($9502F90000000000),
    TPasDblStrUtilsUInt64($BA43B74000000000),
    TPasDblStrUtilsUInt64($E8D4A51000000000),
    TPasDblStrUtilsUInt64($9184E72A00000000),
    TPasDblStrUtilsUInt64($B5E620F480000000),
    TPasDblStrUtilsUInt64($E35FA931A0000000),
    TPasDblStrUtilsUInt64($8E1BC9BF04000000),
    TPasDblStrUtilsUInt64($B1A2BC2EC5000000),
    TPasDblStrUtilsUInt64($DE0B6B3A76400000),
    TPasDblStrUtilsUInt64($8AC7230489E80000),
    TPasDblStrUtilsUInt64($AD78EBC5AC620000),
    TPasDblStrUtilsUInt64($D8D726B7177A8000),
    TPasDblStrUtilsUInt64($878678326EAC9000),
    TPasDblStrUtilsUInt64($A968163F0A57B400),
    TPasDblStrUtilsUInt64($D3C21BCECCEDA100),
    TPasDblStrUtilsUInt64($84595161401484A0),
    TPasDblStrUtilsUInt64($A56FA5B99019A5C8),
    TPasDblStrUtilsUInt64($CECB8F27F4200F3A),
    TPasDblStrUtilsUInt64($813F3978F8940984),
    TPasDblStrUtilsUInt64($A18F07D736B90BE5),
    TPasDblStrUtilsUInt64($C9F2C9CD04674EDE),
    TPasDblStrUtilsUInt64($FC6F7C4045812296),
    TPasDblStrUtilsUInt64($9DC5ADA82B70B59D),
    TPasDblStrUtilsUInt64($C5371912364CE305),
    TPasDblStrUtilsUInt64($F684DF56C3E01BC6),
    TPasDblStrUtilsUInt64($9A130B963A6C115C),
    TPasDblStrUtilsUInt64($C097CE7BC90715B3),
    TPasDblStrUtilsUInt64($F0BDC21ABB48DB20),
    TPasDblStrUtilsUInt64($96769950B50D88F4),
    TPasDblStrUtilsUInt64($BC143FA4E250EB31),
    TPasDblStrUtilsUInt64($EB194F8E1AE525FD),
    TPasDblStrUtilsUInt64($92EFD1B8D0CF37BE),
    TPasDblStrUtilsUInt64($B7ABC627050305AD),
    TPasDblStrUtilsUInt64($E596B7B0C643C719),
    TPasDblStrUtilsUInt64($8F7E32CE7BEA5C6F),
    TPasDblStrUtilsUInt64($B35DBF821AE4F38B),
    TPasDblStrUtilsUInt64($E0352F62A19E306E),
    TPasDblStrUtilsUInt64($8C213D9DA502DE45),
    TPasDblStrUtilsUInt64($AF298D050E4395D6),
    TPasDblStrUtilsUInt64($DAF3F04651D47B4C),
    TPasDblStrUtilsUInt64($88D8762BF324CD0F),
    TPasDblStrUtilsUInt64($AB0E93B6EFEE0053),
    TPasDblStrUtilsUInt64($D5D238A4ABE98068),
    TPasDblStrUtilsUInt64($85A36366EB71F041),
    TPasDblStrUtilsUInt64($A70C3C40A64E6C51),
    TPasDblStrUtilsUInt64($D0CF4B50CFE20765),
    TPasDblStrUtilsUInt64($82818F1281ED449F),
    TPasDblStrUtilsUInt64($A321F2D7226895C7),
    TPasDblStrUtilsUInt64($CBEA6F8CEB02BB39),
    TPasDblStrUtilsUInt64($FEE50B7025C36A08),
    TPasDblStrUtilsUInt64($9F4F2726179A2245),
    TPasDblStrUtilsUInt64($C722F0EF9D80AAD6),
    TPasDblStrUtilsUInt64($F8EBAD2B84E0D58B),
    TPasDblStrUtilsUInt64($9B934C3B330C8577),
    TPasDblStrUtilsUInt64($C2781F49FFCFA6D5),
    TPasDblStrUtilsUInt64($F316271C7FC3908A),
    TPasDblStrUtilsUInt64($97EDD871CFDA3A56),
    TPasDblStrUtilsUInt64($BDE94E8E43D0C8EC),
    TPasDblStrUtilsUInt64($ED63A231D4C4FB27),
    TPasDblStrUtilsUInt64($945E455F24FB1CF8),
    TPasDblStrUtilsUInt64($B975D6B6EE39E436),
    TPasDblStrUtilsUInt64($E7D34C64A9C85D44),
    TPasDblStrUtilsUInt64($90E40FBEEA1D3A4A),
    TPasDblStrUtilsUInt64($B51D13AEA4A488DD),
    TPasDblStrUtilsUInt64($E264589A4DCDAB14),
    TPasDblStrUtilsUInt64($8D7EB76070A08AEC),
    TPasDblStrUtilsUInt64($B0DE65388CC8ADA8),
    TPasDblStrUtilsUInt64($DD15FE86AFFAD912),
    TPasDblStrUtilsUInt64($8A2DBF142DFCC7AB),
    TPasDblStrUtilsUInt64($ACB92ED9397BF996),
    TPasDblStrUtilsUInt64($D7E77A8F87DAF7FB),
    TPasDblStrUtilsUInt64($86F0AC99B4E8DAFD),
    TPasDblStrUtilsUInt64($A8ACD7C0222311BC),
    TPasDblStrUtilsUInt64($D2D80DB02AABD62B),
    TPasDblStrUtilsUInt64($83C7088E1AAB65DB),
    TPasDblStrUtilsUInt64($A4B8CAB1A1563F52),
    TPasDblStrUtilsUInt64($CDE6FD5E09ABCF26),
    TPasDblStrUtilsUInt64($80B05E5AC60B6178),
    TPasDblStrUtilsUInt64($A0DC75F1778E39D6),
    TPasDblStrUtilsUInt64($C913936DD571C84C),
    TPasDblStrUtilsUInt64($FB5878494ACE3A5F),
    TPasDblStrUtilsUInt64($9D174B2DCEC0E47B),
    TPasDblStrUtilsUInt64($C45D1DF942711D9A),
    TPasDblStrUtilsUInt64($F5746577930D6500),
    TPasDblStrUtilsUInt64($9968BF6ABBE85F20),
    TPasDblStrUtilsUInt64($BFC2EF456AE276E8),
    TPasDblStrUtilsUInt64($EFB3AB16C59B14A2),
    TPasDblStrUtilsUInt64($95D04AEE3B80ECE5),
    TPasDblStrUtilsUInt64($BB445DA9CA61281F),
    TPasDblStrUtilsUInt64($EA1575143CF97226),
    TPasDblStrUtilsUInt64($924D692CA61BE758),
    TPasDblStrUtilsUInt64($B6E0C377CFA2E12E),
    TPasDblStrUtilsUInt64($E498F455C38B997A),
    TPasDblStrUtilsUInt64($8EDF98B59A373FEC),
    TPasDblStrUtilsUInt64($B2977EE300C50FE7),
    TPasDblStrUtilsUInt64($DF3D5E9BC0F653E1),
    TPasDblStrUtilsUInt64($8B865B215899F46C),
    TPasDblStrUtilsUInt64($AE67F1E9AEC07187),
    TPasDblStrUtilsUInt64($DA01EE641A708DE9),
    TPasDblStrUtilsUInt64($884134FE908658B2),
    TPasDblStrUtilsUInt64($AA51823E34A7EEDE),
    TPasDblStrUtilsUInt64($D4E5E2CDC1D1EA96),
    TPasDblStrUtilsUInt64($850FADC09923329E),
    TPasDblStrUtilsUInt64($A6539930BF6BFF45),
    TPasDblStrUtilsUInt64($CFE87F7CEF46FF16),
    TPasDblStrUtilsUInt64($81F14FAE158C5F6E),
    TPasDblStrUtilsUInt64($A26DA3999AEF7749),
    TPasDblStrUtilsUInt64($CB090C8001AB551C),
    TPasDblStrUtilsUInt64($FDCB4FA002162A63),
    TPasDblStrUtilsUInt64($9E9F11C4014DDA7E),
    TPasDblStrUtilsUInt64($C646D63501A1511D),
    TPasDblStrUtilsUInt64($F7D88BC24209A565),
    TPasDblStrUtilsUInt64($9AE757596946075F),
    TPasDblStrUtilsUInt64($C1A12D2FC3978937),
    TPasDblStrUtilsUInt64($F209787BB47D6B84),
    TPasDblStrUtilsUInt64($9745EB4D50CE6332),
    TPasDblStrUtilsUInt64($BD176620A501FBFF),
    TPasDblStrUtilsUInt64($EC5D3FA8CE427AFF),
    TPasDblStrUtilsUInt64($93BA47C980E98CDF),
    TPasDblStrUtilsUInt64($B8A8D9BBE123F017),
    TPasDblStrUtilsUInt64($E6D3102AD96CEC1D),
    TPasDblStrUtilsUInt64($9043EA1AC7E41392),
    TPasDblStrUtilsUInt64($B454E4A179DD1877),
    TPasDblStrUtilsUInt64($E16A1DC9D8545E94),
    TPasDblStrUtilsUInt64($8CE2529E2734BB1D),
    TPasDblStrUtilsUInt64($B01AE745B101E9E4),
    TPasDblStrUtilsUInt64($DC21A1171D42645D),
    TPasDblStrUtilsUInt64($899504AE72497EBA),
    TPasDblStrUtilsUInt64($ABFA45DA0EDBDE69),
    TPasDblStrUtilsUInt64($D6F8D7509292D603),
    TPasDblStrUtilsUInt64($865B86925B9BC5C2),
    TPasDblStrUtilsUInt64($A7F26836F282B732),
    TPasDblStrUtilsUInt64($D1EF0244AF2364FF),
    TPasDblStrUtilsUInt64($8335616AED761F1F),
    TPasDblStrUtilsUInt64($A402B9C5A8D3A6E7),
    TPasDblStrUtilsUInt64($CD036837130890A1),
    TPasDblStrUtilsUInt64($802221226BE55A64),
    TPasDblStrUtilsUInt64($A02AA96B06DEB0FD),
    TPasDblStrUtilsUInt64($C83553C5C8965D3D),
    TPasDblStrUtilsUInt64($FA42A8B73ABBF48C),
    TPasDblStrUtilsUInt64($9C69A97284B578D7),
    TPasDblStrUtilsUInt64($C38413CF25E2D70D),
    TPasDblStrUtilsUInt64($F46518C2EF5B8CD1),
    TPasDblStrUtilsUInt64($98BF2F79D5993802),
    TPasDblStrUtilsUInt64($BEEEFB584AFF8603),
    TPasDblStrUtilsUInt64($EEAABA2E5DBF6784),
    TPasDblStrUtilsUInt64($952AB45CFA97A0B2),
    TPasDblStrUtilsUInt64($BA756174393D88DF),
    TPasDblStrUtilsUInt64($E912B9D1478CEB17),
    TPasDblStrUtilsUInt64($91ABB422CCB812EE),
    TPasDblStrUtilsUInt64($B616A12B7FE617AA),
    TPasDblStrUtilsUInt64($E39C49765FDF9D94),
    TPasDblStrUtilsUInt64($8E41ADE9FBEBC27D),
    TPasDblStrUtilsUInt64($B1D219647AE6B31C),
    TPasDblStrUtilsUInt64($DE469FBD99A05FE3),
    TPasDblStrUtilsUInt64($8AEC23D680043BEE),
    TPasDblStrUtilsUInt64($ADA72CCC20054AE9),
    TPasDblStrUtilsUInt64($D910F7FF28069DA4),
    TPasDblStrUtilsUInt64($87AA9AFF79042286),
    TPasDblStrUtilsUInt64($A99541BF57452B28),
    TPasDblStrUtilsUInt64($D3FA922F2D1675F2),
    TPasDblStrUtilsUInt64($847C9B5D7C2E09B7),
    TPasDblStrUtilsUInt64($A59BC234DB398C25),
    TPasDblStrUtilsUInt64($CF02B2C21207EF2E),
    TPasDblStrUtilsUInt64($8161AFB94B44F57D),
    TPasDblStrUtilsUInt64($A1BA1BA79E1632DC),
    TPasDblStrUtilsUInt64($CA28A291859BBF93),
    TPasDblStrUtilsUInt64($FCB2CB35E702AF78),
    TPasDblStrUtilsUInt64($9DEFBF01B061ADAB),
    TPasDblStrUtilsUInt64($C56BAEC21C7A1916),
    TPasDblStrUtilsUInt64($F6C69A72A3989F5B),
    TPasDblStrUtilsUInt64($9A3C2087A63F6399),
    TPasDblStrUtilsUInt64($C0CB28A98FCF3C7F),
    TPasDblStrUtilsUInt64($F0FDF2D3F3C30B9F),
    TPasDblStrUtilsUInt64($969EB7C47859E743),
    TPasDblStrUtilsUInt64($BC4665B596706114),
    TPasDblStrUtilsUInt64($EB57FF22FC0C7959),
    TPasDblStrUtilsUInt64($9316FF75DD87CBD8),
    TPasDblStrUtilsUInt64($B7DCBF5354E9BECE),
    TPasDblStrUtilsUInt64($E5D3EF282A242E81),
    TPasDblStrUtilsUInt64($8FA475791A569D10),
    TPasDblStrUtilsUInt64($B38D92D760EC4455),
    TPasDblStrUtilsUInt64($E070F78D3927556A),
    TPasDblStrUtilsUInt64($8C469AB843B89562),
    TPasDblStrUtilsUInt64($AF58416654A6BABB),
    TPasDblStrUtilsUInt64($DB2E51BFE9D0696A),
    TPasDblStrUtilsUInt64($88FCF317F22241E2),
    TPasDblStrUtilsUInt64($AB3C2FDDEEAAD25A),
    TPasDblStrUtilsUInt64($D60B3BD56A5586F1),
    TPasDblStrUtilsUInt64($85C7056562757456),
    TPasDblStrUtilsUInt64($A738C6BEBB12D16C),
    TPasDblStrUtilsUInt64($D106F86E69D785C7),
    TPasDblStrUtilsUInt64($82A45B450226B39C),
    TPasDblStrUtilsUInt64($A34D721642B06084),
    TPasDblStrUtilsUInt64($CC20CE9BD35C78A5),
    TPasDblStrUtilsUInt64($FF290242C83396CE),
    TPasDblStrUtilsUInt64($9F79A169BD203E41),
    TPasDblStrUtilsUInt64($C75809C42C684DD1),
    TPasDblStrUtilsUInt64($F92E0C3537826145),
    TPasDblStrUtilsUInt64($9BBCC7A142B17CCB),
    TPasDblStrUtilsUInt64($C2ABF989935DDBFE),
    TPasDblStrUtilsUInt64($F356F7EBF83552FE),
    TPasDblStrUtilsUInt64($98165AF37B2153DE),
    TPasDblStrUtilsUInt64($BE1BF1B059E9A8D6),
    TPasDblStrUtilsUInt64($EDA2EE1C7064130C),
    TPasDblStrUtilsUInt64($9485D4D1C63E8BE7),
    TPasDblStrUtilsUInt64($B9A74A0637CE2EE1),
    TPasDblStrUtilsUInt64($E8111C87C5C1BA99),
    TPasDblStrUtilsUInt64($910AB1D4DB9914A0),
    TPasDblStrUtilsUInt64($B54D5E4A127F59C8),
    TPasDblStrUtilsUInt64($E2A0B5DC971F303A),
    TPasDblStrUtilsUInt64($8DA471A9DE737E24),
    TPasDblStrUtilsUInt64($B10D8E1456105DAD),
    TPasDblStrUtilsUInt64($DD50F1996B947518),
    TPasDblStrUtilsUInt64($8A5296FFE33CC92F),
    TPasDblStrUtilsUInt64($ACE73CBFDC0BFB7B),
    TPasDblStrUtilsUInt64($D8210BEFD30EFA5A),
    TPasDblStrUtilsUInt64($8714A775E3E95C78),
    TPasDblStrUtilsUInt64($A8D9D1535CE3B396),
    TPasDblStrUtilsUInt64($D31045A8341CA07C),
    TPasDblStrUtilsUInt64($83EA2B892091E44D),
    TPasDblStrUtilsUInt64($A4E4B66B68B65D60),
    TPasDblStrUtilsUInt64($CE1DE40642E3F4B9),
    TPasDblStrUtilsUInt64($80D2AE83E9CE78F3),
    TPasDblStrUtilsUInt64($A1075A24E4421730),
    TPasDblStrUtilsUInt64($C94930AE1D529CFC),
    TPasDblStrUtilsUInt64($FB9B7CD9A4A7443C),
    TPasDblStrUtilsUInt64($9D412E0806E88AA5),
    TPasDblStrUtilsUInt64($C491798A08A2AD4E),
    TPasDblStrUtilsUInt64($F5B5D7EC8ACB58A2),
    TPasDblStrUtilsUInt64($9991A6F3D6BF1765),
    TPasDblStrUtilsUInt64($BFF610B0CC6EDD3F),
    TPasDblStrUtilsUInt64($EFF394DCFF8A948E),
    TPasDblStrUtilsUInt64($95F83D0A1FB69CD9),
    TPasDblStrUtilsUInt64($BB764C4CA7A4440F),
    TPasDblStrUtilsUInt64($EA53DF5FD18D5513),
    TPasDblStrUtilsUInt64($92746B9BE2F8552C),
    TPasDblStrUtilsUInt64($B7118682DBB66A77),
    TPasDblStrUtilsUInt64($E4D5E82392A40515),
    TPasDblStrUtilsUInt64($8F05B1163BA6832D),
    TPasDblStrUtilsUInt64($B2C71D5BCA9023F8),
    TPasDblStrUtilsUInt64($DF78E4B2BD342CF6),
    TPasDblStrUtilsUInt64($8BAB8EEFB6409C1A),
    TPasDblStrUtilsUInt64($AE9672ABA3D0C320),
    TPasDblStrUtilsUInt64($DA3C0F568CC4F3E8),
    TPasDblStrUtilsUInt64($8865899617FB1871),
    TPasDblStrUtilsUInt64($AA7EEBFB9DF9DE8D),
    TPasDblStrUtilsUInt64($D51EA6FA85785631),
    TPasDblStrUtilsUInt64($8533285C936B35DE),
    TPasDblStrUtilsUInt64($A67FF273B8460356),
    TPasDblStrUtilsUInt64($D01FEF10A657842C),
    TPasDblStrUtilsUInt64($8213F56A67F6B29B),
    TPasDblStrUtilsUInt64($A298F2C501F45F42),
    TPasDblStrUtilsUInt64($CB3F2F7642717713),
    TPasDblStrUtilsUInt64($FE0EFB53D30DD4D7),
    TPasDblStrUtilsUInt64($9EC95D1463E8A506),
    TPasDblStrUtilsUInt64($C67BB4597CE2CE48),
    TPasDblStrUtilsUInt64($F81AA16FDC1B81DA),
    TPasDblStrUtilsUInt64($9B10A4E5E9913128),
    TPasDblStrUtilsUInt64($C1D4CE1F63F57D72),
    TPasDblStrUtilsUInt64($F24A01A73CF2DCCF),
    TPasDblStrUtilsUInt64($976E41088617CA01),
    TPasDblStrUtilsUInt64($BD49D14AA79DBC82),
    TPasDblStrUtilsUInt64($EC9C459D51852BA2),
    TPasDblStrUtilsUInt64($93E1AB8252F33B45),
    TPasDblStrUtilsUInt64($B8DA1662E7B00A17),
    TPasDblStrUtilsUInt64($E7109BFBA19C0C9D),
    TPasDblStrUtilsUInt64($906A617D450187E2),
    TPasDblStrUtilsUInt64($B484F9DC9641E9DA),
    TPasDblStrUtilsUInt64($E1A63853BBD26451),
    TPasDblStrUtilsUInt64($8D07E33455637EB2),
    TPasDblStrUtilsUInt64($B049DC016ABC5E5F),
    TPasDblStrUtilsUInt64($DC5C5301C56B75F7),
    TPasDblStrUtilsUInt64($89B9B3E11B6329BA),
    TPasDblStrUtilsUInt64($AC2820D9623BF429),
    TPasDblStrUtilsUInt64($D732290FBACAF133),
    TPasDblStrUtilsUInt64($867F59A9D4BED6C0),
    TPasDblStrUtilsUInt64($A81F301449EE8C70),
    TPasDblStrUtilsUInt64($D226FC195C6A2F8C),
    TPasDblStrUtilsUInt64($83585D8FD9C25DB7),
    TPasDblStrUtilsUInt64($A42E74F3D032F525),
    TPasDblStrUtilsUInt64($CD3A1230C43FB26F),
    TPasDblStrUtilsUInt64($80444B5E7AA7CF85),
    TPasDblStrUtilsUInt64($A0555E361951C366),
    TPasDblStrUtilsUInt64($C86AB5C39FA63440),
    TPasDblStrUtilsUInt64($FA856334878FC150),
    TPasDblStrUtilsUInt64($9C935E00D4B9D8D2),
    TPasDblStrUtilsUInt64($C3B8358109E84F07),
    TPasDblStrUtilsUInt64($F4A642E14C6262C8),
    TPasDblStrUtilsUInt64($98E7E9CCCFBD7DBD),
    TPasDblStrUtilsUInt64($BF21E44003ACDD2C),
    TPasDblStrUtilsUInt64($EEEA5D5004981478),
    TPasDblStrUtilsUInt64($95527A5202DF0CCB),
    TPasDblStrUtilsUInt64($BAA718E68396CFFD),
    TPasDblStrUtilsUInt64($E950DF20247C83FD),
    TPasDblStrUtilsUInt64($91D28B7416CDD27E),
    TPasDblStrUtilsUInt64($B6472E511C81471D),
    TPasDblStrUtilsUInt64($E3D8F9E563A198E5),
    TPasDblStrUtilsUInt64($8E679C2F5E44FF8F));
  Mantissa128: array [FASTFLOAT_SMALLEST_POWER .. FASTFLOAT_LARGEST_POWER]
    of TPasDblStrUtilsUInt64 = (TPasDblStrUtilsUInt64($419EA3BD35385E2D),
    TPasDblStrUtilsUInt64($52064CAC828675B9),
    TPasDblStrUtilsUInt64($7343EFEBD1940993),
    TPasDblStrUtilsUInt64($1014EBE6C5F90BF8),
    TPasDblStrUtilsUInt64($D41A26E077774EF6),
    TPasDblStrUtilsUInt64($8920B098955522B4),
    TPasDblStrUtilsUInt64($55B46E5F5D5535B0),
    TPasDblStrUtilsUInt64($EB2189F734AA831D),
    TPasDblStrUtilsUInt64($A5E9EC7501D523E4),
    TPasDblStrUtilsUInt64($47B233C92125366E),
    TPasDblStrUtilsUInt64($999EC0BB696E840A),
    TPasDblStrUtilsUInt64($C00670EA43CA250D),
    TPasDblStrUtilsUInt64($380406926A5E5728),
    TPasDblStrUtilsUInt64($C605083704F5ECF2),
    TPasDblStrUtilsUInt64($F7864A44C633682E),
    TPasDblStrUtilsUInt64($7AB3EE6AFBE0211D),
    TPasDblStrUtilsUInt64($5960EA05BAD82964),
    TPasDblStrUtilsUInt64($6FB92487298E33BD),
    TPasDblStrUtilsUInt64($A5D3B6D479F8E056),
    TPasDblStrUtilsUInt64($8F48A4899877186C),
    TPasDblStrUtilsUInt64($331ACDABFE94DE87),
    TPasDblStrUtilsUInt64($9FF0C08B7F1D0B14),
    TPasDblStrUtilsUInt64($7ECF0AE5EE44DD9),
    TPasDblStrUtilsUInt64($C9E82CD9F69D6150),
    TPasDblStrUtilsUInt64($BE311C083A225CD2),
    TPasDblStrUtilsUInt64($6DBD630A48AAF406),
    TPasDblStrUtilsUInt64($92CBBCCDAD5B108),
    TPasDblStrUtilsUInt64($25BBF56008C58EA5),
    TPasDblStrUtilsUInt64($AF2AF2B80AF6F24E),
    TPasDblStrUtilsUInt64($1AF5AF660DB4AEE1),
    TPasDblStrUtilsUInt64($50D98D9FC890ED4D),
    TPasDblStrUtilsUInt64($E50FF107BAB528A0),
    TPasDblStrUtilsUInt64($1E53ED49A96272C8),
    TPasDblStrUtilsUInt64($25E8E89C13BB0F7A),
    TPasDblStrUtilsUInt64($77B191618C54E9AC),
    TPasDblStrUtilsUInt64($D59DF5B9EF6A2417),
    TPasDblStrUtilsUInt64($4B0573286B44AD1D),
    TPasDblStrUtilsUInt64($4EE367F9430AEC32),
    TPasDblStrUtilsUInt64($229C41F793CDA73F),
    TPasDblStrUtilsUInt64($6B43527578C1110F),
    TPasDblStrUtilsUInt64($830A13896B78AAA9),
    TPasDblStrUtilsUInt64($23CC986BC656D553),
    TPasDblStrUtilsUInt64($2CBFBE86B7EC8AA8),
    TPasDblStrUtilsUInt64($7BF7D71432F3D6A9),
    TPasDblStrUtilsUInt64($DAF5CCD93FB0CC53),
    TPasDblStrUtilsUInt64($D1B3400F8F9CFF68),
    TPasDblStrUtilsUInt64($23100809B9C21FA1),
    TPasDblStrUtilsUInt64($ABD40A0C2832A78A),
    TPasDblStrUtilsUInt64($16C90C8F323F516C),
    TPasDblStrUtilsUInt64($AE3DA7D97F6792E3),
    TPasDblStrUtilsUInt64($99CD11CFDF41779C),
    TPasDblStrUtilsUInt64($40405643D711D583),
    TPasDblStrUtilsUInt64($482835EA666B2572),
    TPasDblStrUtilsUInt64($DA3243650005EECF),
    TPasDblStrUtilsUInt64($90BED43E40076A82),
    TPasDblStrUtilsUInt64($5A7744A6E804A291),
    TPasDblStrUtilsUInt64($711515D0A205CB36),
    TPasDblStrUtilsUInt64($D5A5B44CA873E03),
    TPasDblStrUtilsUInt64($E858790AFE9486C2),
    TPasDblStrUtilsUInt64($626E974DBE39A872),
    TPasDblStrUtilsUInt64($FB0A3D212DC8128F),
    TPasDblStrUtilsUInt64($7CE66634BC9D0B99),
    TPasDblStrUtilsUInt64($1C1FFFC1EBC44E80),
    TPasDblStrUtilsUInt64($A327FFB266B56220),
    TPasDblStrUtilsUInt64($4BF1FF9F0062BAA8),
    TPasDblStrUtilsUInt64($6F773FC3603DB4A9),
    TPasDblStrUtilsUInt64($CB550FB4384D21D3),
    TPasDblStrUtilsUInt64($7E2A53A146606A48),
    TPasDblStrUtilsUInt64($2EDA7444CBFC426D),
    TPasDblStrUtilsUInt64($FA911155FEFB5308),
    TPasDblStrUtilsUInt64($793555AB7EBA27CA),
    TPasDblStrUtilsUInt64($4BC1558B2F3458DE),
    TPasDblStrUtilsUInt64($9EB1AAEDFB016F16),
    TPasDblStrUtilsUInt64($465E15A979C1CADC),
    TPasDblStrUtilsUInt64($BFACD89EC191EC9),
    TPasDblStrUtilsUInt64($CEF980EC671F667B),
    TPasDblStrUtilsUInt64($82B7E12780E7401A),
    TPasDblStrUtilsUInt64($D1B2ECB8B0908810),
    TPasDblStrUtilsUInt64($861FA7E6DCB4AA15),
    TPasDblStrUtilsUInt64($67A791E093E1D49A),
    TPasDblStrUtilsUInt64($E0C8BB2C5C6D24E0),
    TPasDblStrUtilsUInt64($58FAE9F773886E18),
    TPasDblStrUtilsUInt64($AF39A475506A899E),
    TPasDblStrUtilsUInt64($6D8406C952429603),
    TPasDblStrUtilsUInt64($C8E5087BA6D33B83),
    TPasDblStrUtilsUInt64($FB1E4A9A90880A64),
    TPasDblStrUtilsUInt64($5CF2EEA09A55067F),
    TPasDblStrUtilsUInt64($F42FAA48C0EA481E),
    TPasDblStrUtilsUInt64($F13B94DAF124DA26),
    TPasDblStrUtilsUInt64($76C53D08D6B70858),
    TPasDblStrUtilsUInt64($54768C4B0C64CA6E),
    TPasDblStrUtilsUInt64($A9942F5DCF7DFD09),
    TPasDblStrUtilsUInt64($D3F93B35435D7C4C),
    TPasDblStrUtilsUInt64($C47BC5014A1A6DAF),
    TPasDblStrUtilsUInt64($359AB6419CA1091B),
    TPasDblStrUtilsUInt64($C30163D203C94B62),
    TPasDblStrUtilsUInt64($79E0DE63425DCF1D),
    TPasDblStrUtilsUInt64($985915FC12F542E4),
    TPasDblStrUtilsUInt64($3E6F5B7B17B2939D),
    TPasDblStrUtilsUInt64($A705992CEECF9C42),
    TPasDblStrUtilsUInt64($50C6FF782A838353),
    TPasDblStrUtilsUInt64($A4F8BF5635246428),
    TPasDblStrUtilsUInt64($871B7795E136BE99),
    TPasDblStrUtilsUInt64($28E2557B59846E3F),
    TPasDblStrUtilsUInt64($331AEADA2FE589CF),
    TPasDblStrUtilsUInt64($3FF0D2C85DEF7621),
    TPasDblStrUtilsUInt64($FED077A756B53A9),
    TPasDblStrUtilsUInt64($D3E8495912C62894),
    TPasDblStrUtilsUInt64($64712DD7ABBBD95C),
    TPasDblStrUtilsUInt64($BD8D794D96AACFB3),
    TPasDblStrUtilsUInt64($ECF0D7A0FC5583A0),
    TPasDblStrUtilsUInt64($F41686C49DB57244),
    TPasDblStrUtilsUInt64($311C2875C522CED5),
    TPasDblStrUtilsUInt64($7D633293366B828B),
    TPasDblStrUtilsUInt64($AE5DFF9C02033197),
    TPasDblStrUtilsUInt64($D9F57F830283FDFC),
    TPasDblStrUtilsUInt64($D072DF63C324FD7B),
    TPasDblStrUtilsUInt64($4247CB9E59F71E6D),
    TPasDblStrUtilsUInt64($52D9BE85F074E608),
    TPasDblStrUtilsUInt64($67902E276C921F8B),
    TPasDblStrUtilsUInt64($BA1CD8A3DB53B6),
    TPasDblStrUtilsUInt64($80E8A40ECCD228A4),
    TPasDblStrUtilsUInt64($6122CD128006B2CD),
    TPasDblStrUtilsUInt64($796B805720085F81),
    TPasDblStrUtilsUInt64($CBE3303674053BB0),
    TPasDblStrUtilsUInt64($BEDBFC4411068A9C),
    TPasDblStrUtilsUInt64($EE92FB5515482D44),
    TPasDblStrUtilsUInt64($751BDD152D4D1C4A),
    TPasDblStrUtilsUInt64($D262D45A78A0635D),
    TPasDblStrUtilsUInt64($86FB897116C87C34),
    TPasDblStrUtilsUInt64($D45D35E6AE3D4DA0),
    TPasDblStrUtilsUInt64($8974836059CCA109),
    TPasDblStrUtilsUInt64($2BD1A438703FC94B),
    TPasDblStrUtilsUInt64($7B6306A34627DDCF),
    TPasDblStrUtilsUInt64($1A3BC84C17B1D542),
    TPasDblStrUtilsUInt64($20CABA5F1D9E4A93),
    TPasDblStrUtilsUInt64($547EB47B7282EE9C),
    TPasDblStrUtilsUInt64($E99E619A4F23AA43),
    TPasDblStrUtilsUInt64($6405FA00E2EC94D4),
    TPasDblStrUtilsUInt64($DE83BC408DD3DD04),
    TPasDblStrUtilsUInt64($9624AB50B148D445),
    TPasDblStrUtilsUInt64($3BADD624DD9B0957),
    TPasDblStrUtilsUInt64($E54CA5D70A80E5D6),
    TPasDblStrUtilsUInt64($5E9FCF4CCD211F4C),
    TPasDblStrUtilsUInt64($7647C3200069671F),
    TPasDblStrUtilsUInt64($29ECD9F40041E073),
    TPasDblStrUtilsUInt64($F468107100525890),
    TPasDblStrUtilsUInt64($7182148D4066EEB4),
    TPasDblStrUtilsUInt64($C6F14CD848405530),
    TPasDblStrUtilsUInt64($B8ADA00E5A506A7C),
    TPasDblStrUtilsUInt64($A6D90811F0E4851C),
    TPasDblStrUtilsUInt64($908F4A166D1DA663),
    TPasDblStrUtilsUInt64($9A598E4E043287FE),
    TPasDblStrUtilsUInt64($40EFF1E1853F29FD),
    TPasDblStrUtilsUInt64($D12BEE59E68EF47C),
    TPasDblStrUtilsUInt64($82BB74F8301958CE),
    TPasDblStrUtilsUInt64($E36A52363C1FAF01),
    TPasDblStrUtilsUInt64($DC44E6C3CB279AC1),
    TPasDblStrUtilsUInt64($29AB103A5EF8C0B9),
    TPasDblStrUtilsUInt64($7415D448F6B6F0E7),
    TPasDblStrUtilsUInt64($111B495B3464AD21),
    TPasDblStrUtilsUInt64($CAB10DD900BEEC34),
    TPasDblStrUtilsUInt64($3D5D514F40EEA742),
    TPasDblStrUtilsUInt64($CB4A5A3112A5112),
    TPasDblStrUtilsUInt64($47F0E785EABA72AB),
    TPasDblStrUtilsUInt64($59ED216765690F56),
    TPasDblStrUtilsUInt64($306869C13EC3532C),
    TPasDblStrUtilsUInt64($1E414218C73A13FB),
    TPasDblStrUtilsUInt64($E5D1929EF90898FA),
    TPasDblStrUtilsUInt64($DF45F746B74ABF39),
    TPasDblStrUtilsUInt64($6B8BBA8C328EB783),
    TPasDblStrUtilsUInt64($66EA92F3F326564),
    TPasDblStrUtilsUInt64($C80A537B0EFEFEBD),
    TPasDblStrUtilsUInt64($BD06742CE95F5F36),
    TPasDblStrUtilsUInt64($2C48113823B73704),
    TPasDblStrUtilsUInt64($F75A15862CA504C5),
    TPasDblStrUtilsUInt64($9A984D73DBE722FB),
    TPasDblStrUtilsUInt64($C13E60D0D2E0EBBA),
    TPasDblStrUtilsUInt64($318DF905079926A8),
    TPasDblStrUtilsUInt64($FDF17746497F7052),
    TPasDblStrUtilsUInt64($FEB6EA8BEDEFA633),
    TPasDblStrUtilsUInt64($FE64A52EE96B8FC0),
    TPasDblStrUtilsUInt64($3DFDCE7AA3C673B0),
    TPasDblStrUtilsUInt64($6BEA10CA65C084E),
    TPasDblStrUtilsUInt64($486E494FCFF30A62),
    TPasDblStrUtilsUInt64($5A89DBA3C3EFCCFA),
    TPasDblStrUtilsUInt64($F89629465A75E01C),
    TPasDblStrUtilsUInt64($F6BBB397F1135823),
    TPasDblStrUtilsUInt64($746AA07DED582E2C),
    TPasDblStrUtilsUInt64($A8C2A44EB4571CDC),
    TPasDblStrUtilsUInt64($92F34D62616CE413),
    TPasDblStrUtilsUInt64($77B020BAF9C81D17),
    TPasDblStrUtilsUInt64($ACE1474DC1D122E),
    TPasDblStrUtilsUInt64($D819992132456BA),
    TPasDblStrUtilsUInt64($10E1FFF697ED6C69),
    TPasDblStrUtilsUInt64($CA8D3FFA1EF463C1),
    TPasDblStrUtilsUInt64($BD308FF8A6B17CB2),
    TPasDblStrUtilsUInt64($AC7CB3F6D05DDBDE),
    TPasDblStrUtilsUInt64($6BCDF07A423AA96B),
    TPasDblStrUtilsUInt64($86C16C98D2C953C6),
    TPasDblStrUtilsUInt64($E871C7BF077BA8B7),
    TPasDblStrUtilsUInt64($11471CD764AD4972),
    TPasDblStrUtilsUInt64($D598E40D3DD89BCF),
    TPasDblStrUtilsUInt64($4AFF1D108D4EC2C3),
    TPasDblStrUtilsUInt64($CEDF722A585139BA),
    TPasDblStrUtilsUInt64($C2974EB4EE658828),
    TPasDblStrUtilsUInt64($733D226229FEEA32),
    TPasDblStrUtilsUInt64($806357D5A3F525F),
    TPasDblStrUtilsUInt64($CA07C2DCB0CF26F7),
    TPasDblStrUtilsUInt64($FC89B393DD02F0B5),
    TPasDblStrUtilsUInt64($BBAC2078D443ACE2),
    TPasDblStrUtilsUInt64($D54B944B84AA4C0D),
    TPasDblStrUtilsUInt64($A9E795E65D4DF11),
    TPasDblStrUtilsUInt64($4D4617B5FF4A16D5),
    TPasDblStrUtilsUInt64($504BCED1BF8E4E45),
    TPasDblStrUtilsUInt64($E45EC2862F71E1D6),
    TPasDblStrUtilsUInt64($5D767327BB4E5A4C),
    TPasDblStrUtilsUInt64($3A6A07F8D510F86F),
    TPasDblStrUtilsUInt64($890489F70A55368B),
    TPasDblStrUtilsUInt64($2B45AC74CCEA842E),
    TPasDblStrUtilsUInt64($3B0B8BC90012929D),
    TPasDblStrUtilsUInt64($9CE6EBB40173744),
    TPasDblStrUtilsUInt64($CC420A6A101D0515),
    TPasDblStrUtilsUInt64($9FA946824A12232D),
    TPasDblStrUtilsUInt64($47939822DC96ABF9),
    TPasDblStrUtilsUInt64($59787E2B93BC56F7),
    TPasDblStrUtilsUInt64($57EB4EDB3C55B65A),
    TPasDblStrUtilsUInt64($EDE622920B6B23F1),
    TPasDblStrUtilsUInt64($E95FAB368E45ECED),
    TPasDblStrUtilsUInt64($11DBCB0218EBB414),
    TPasDblStrUtilsUInt64($D652BDC29F26A119),
    TPasDblStrUtilsUInt64($4BE76D3346F0495F),
    TPasDblStrUtilsUInt64($6F70A4400C562DDB),
    TPasDblStrUtilsUInt64($CB4CCD500F6BB952),
    TPasDblStrUtilsUInt64($7E2000A41346A7A7),
    TPasDblStrUtilsUInt64($8ED400668C0C28C8),
    TPasDblStrUtilsUInt64($728900802F0F32FA),
    TPasDblStrUtilsUInt64($4F2B40A03AD2FFB9),
    TPasDblStrUtilsUInt64($E2F610C84987BFA8),
    TPasDblStrUtilsUInt64($DD9CA7D2DF4D7C9),
    TPasDblStrUtilsUInt64($91503D1C79720DBB),
    TPasDblStrUtilsUInt64($75A44C6397CE912A),
    TPasDblStrUtilsUInt64($C986AFBE3EE11ABA),
    TPasDblStrUtilsUInt64($FBE85BADCE996168),
    TPasDblStrUtilsUInt64($FAE27299423FB9C3),
    TPasDblStrUtilsUInt64($DCCD879FC967D41A),
    TPasDblStrUtilsUInt64($5400E987BBC1C920),
    TPasDblStrUtilsUInt64($290123E9AAB23B68),
    TPasDblStrUtilsUInt64($F9A0B6720AAF6521),
    TPasDblStrUtilsUInt64($F808E40E8D5B3E69),
    TPasDblStrUtilsUInt64($B60B1D1230B20E04),
    TPasDblStrUtilsUInt64($B1C6F22B5E6F48C2),
    TPasDblStrUtilsUInt64($1E38AEB6360B1AF3),
    TPasDblStrUtilsUInt64($25C6DA63C38DE1B0),
    TPasDblStrUtilsUInt64($579C487E5A38AD0E),
    TPasDblStrUtilsUInt64($2D835A9DF0C6D851),
    TPasDblStrUtilsUInt64($F8E431456CF88E65),
    TPasDblStrUtilsUInt64($1B8E9ECB641B58FF),
    TPasDblStrUtilsUInt64($E272467E3D222F3F),
    TPasDblStrUtilsUInt64($5B0ED81DCC6ABB0F),
    TPasDblStrUtilsUInt64($98E947129FC2B4E9),
    TPasDblStrUtilsUInt64($3F2398D747B36224),
    TPasDblStrUtilsUInt64($8EEC7F0D19A03AAD),
    TPasDblStrUtilsUInt64($1953CF68300424AC),
    TPasDblStrUtilsUInt64($5FA8C3423C052DD7),
    TPasDblStrUtilsUInt64($3792F412CB06794D),
    TPasDblStrUtilsUInt64($E2BBD88BBEE40BD0),
    TPasDblStrUtilsUInt64($5B6ACEAEAE9D0EC4),
    TPasDblStrUtilsUInt64($F245825A5A445275),
    TPasDblStrUtilsUInt64($EED6E2F0F0D56712),
    TPasDblStrUtilsUInt64($55464DD69685606B),
    TPasDblStrUtilsUInt64($AA97E14C3C26B886),
    TPasDblStrUtilsUInt64($D53DD99F4B3066A8),
    TPasDblStrUtilsUInt64($E546A8038EFE4029),
    TPasDblStrUtilsUInt64($DE98520472BDD033),
    TPasDblStrUtilsUInt64($963E66858F6D4440),
    TPasDblStrUtilsUInt64($DDE7001379A44AA8),
    TPasDblStrUtilsUInt64($5560C018580D5D52),
    TPasDblStrUtilsUInt64($AAB8F01E6E10B4A6),
    TPasDblStrUtilsUInt64($CAB3961304CA70E8),
    TPasDblStrUtilsUInt64($3D607B97C5FD0D22),
    TPasDblStrUtilsUInt64($8CB89A7DB77C506A),
    TPasDblStrUtilsUInt64($77F3608E92ADB242),
    TPasDblStrUtilsUInt64($55F038B237591ED3),
    TPasDblStrUtilsUInt64($6B6C46DEC52F6688),
    TPasDblStrUtilsUInt64($2323AC4B3B3DA015),
    TPasDblStrUtilsUInt64($ABEC975E0A0D081A),
    TPasDblStrUtilsUInt64($96E7BD358C904A21),
    TPasDblStrUtilsUInt64($7E50D64177DA2E54),
    TPasDblStrUtilsUInt64($DDE50BD1D5D0B9E9),
    TPasDblStrUtilsUInt64($955E4EC64B44E864),
    TPasDblStrUtilsUInt64($BD5AF13BEF0B113E),
    TPasDblStrUtilsUInt64($ECB1AD8AEACDD58E),
    TPasDblStrUtilsUInt64($67DE18EDA5814AF2),
    TPasDblStrUtilsUInt64($80EACF948770CED7),
    TPasDblStrUtilsUInt64($A1258379A94D028D),
    TPasDblStrUtilsUInt64($96EE45813A04330),
    TPasDblStrUtilsUInt64($8BCA9D6E188853FC),
    TPasDblStrUtilsUInt64($775EA264CF55347D),
    TPasDblStrUtilsUInt64($95364AFE032A819D),
    TPasDblStrUtilsUInt64($3A83DDBD83F52204),
    TPasDblStrUtilsUInt64($C4926A9672793542),
    TPasDblStrUtilsUInt64($75B7053C0F178293),
    TPasDblStrUtilsUInt64($5324C68B12DD6338),
    TPasDblStrUtilsUInt64($D3F6FC16EBCA5E03),
    TPasDblStrUtilsUInt64($88F4BB1CA6BCF584),
    TPasDblStrUtilsUInt64($2B31E9E3D06C32E5),
    TPasDblStrUtilsUInt64($3AFF322E62439FCF),
    TPasDblStrUtilsUInt64($9BEFEB9FAD487C2),
    TPasDblStrUtilsUInt64($4C2EBE687989A9B3),
    TPasDblStrUtilsUInt64($F9D37014BF60A10),
    TPasDblStrUtilsUInt64($538484C19EF38C94),
    TPasDblStrUtilsUInt64($2865A5F206B06FB9),
    TPasDblStrUtilsUInt64($F93F87B7442E45D3),
    TPasDblStrUtilsUInt64($F78F69A51539D748),
    TPasDblStrUtilsUInt64($B573440E5A884D1B),
    TPasDblStrUtilsUInt64($31680A88F8953030),
    TPasDblStrUtilsUInt64($FDC20D2B36BA7C3D),
    TPasDblStrUtilsUInt64($3D32907604691B4C),
    TPasDblStrUtilsUInt64($A63F9A49C2C1B10F),
    TPasDblStrUtilsUInt64($FCF80DC33721D53),
    TPasDblStrUtilsUInt64($D3C36113404EA4A8),
    TPasDblStrUtilsUInt64($645A1CAC083126E9),
    TPasDblStrUtilsUInt64($3D70A3D70A3D70A3),
    TPasDblStrUtilsUInt64($CCCCCCCCCCCCCCCC), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($0),
    TPasDblStrUtilsUInt64($0), TPasDblStrUtilsUInt64($4000000000000000),
    TPasDblStrUtilsUInt64($5000000000000000),
    TPasDblStrUtilsUInt64($A400000000000000),
    TPasDblStrUtilsUInt64($4D00000000000000),
    TPasDblStrUtilsUInt64($F020000000000000),
    TPasDblStrUtilsUInt64($6C28000000000000),
    TPasDblStrUtilsUInt64($C732000000000000),
    TPasDblStrUtilsUInt64($3C7F400000000000),
    TPasDblStrUtilsUInt64($4B9F100000000000),
    TPasDblStrUtilsUInt64($1E86D40000000000),
    TPasDblStrUtilsUInt64($1314448000000000),
    TPasDblStrUtilsUInt64($17D955A000000000),
    TPasDblStrUtilsUInt64($5DCFAB0800000000),
    TPasDblStrUtilsUInt64($5AA1CAE500000000),
    TPasDblStrUtilsUInt64($F14A3D9E40000000),
    TPasDblStrUtilsUInt64($6D9CCD05D0000000),
    TPasDblStrUtilsUInt64($E4820023A2000000),
    TPasDblStrUtilsUInt64($DDA2802C8A800000),
    TPasDblStrUtilsUInt64($D50B2037AD200000),
    TPasDblStrUtilsUInt64($4526F422CC340000),
    TPasDblStrUtilsUInt64($9670B12B7F410000),
    TPasDblStrUtilsUInt64($3C0CDD765F114000),
    TPasDblStrUtilsUInt64($A5880A69FB6AC800),
    TPasDblStrUtilsUInt64($8EEA0D047A457A00),
    TPasDblStrUtilsUInt64($72A4904598D6D880),
    TPasDblStrUtilsUInt64($47A6DA2B7F864750),
    TPasDblStrUtilsUInt64($999090B65F67D924),
    TPasDblStrUtilsUInt64($FFF4B4E3F741CF6D),
    TPasDblStrUtilsUInt64($BFF8F10E7A8921A4),
    TPasDblStrUtilsUInt64($AFF72D52192B6A0D),
    TPasDblStrUtilsUInt64($9BF4F8A69F764490),
    TPasDblStrUtilsUInt64($2F236D04753D5B4),
    TPasDblStrUtilsUInt64($1D762422C946590),
    TPasDblStrUtilsUInt64($424D3AD2B7B97EF5),
    TPasDblStrUtilsUInt64($D2E0898765A7DEB2),
    TPasDblStrUtilsUInt64($63CC55F49F88EB2F),
    TPasDblStrUtilsUInt64($3CBF6B71C76B25FB),
    TPasDblStrUtilsUInt64($8BEF464E3945EF7A),
    TPasDblStrUtilsUInt64($97758BF0E3CBB5AC),
    TPasDblStrUtilsUInt64($3D52EEED1CBEA317),
    TPasDblStrUtilsUInt64($4CA7AAA863EE4BDD),
    TPasDblStrUtilsUInt64($8FE8CAA93E74EF6A),
    TPasDblStrUtilsUInt64($B3E2FD538E122B44),
    TPasDblStrUtilsUInt64($60DBBCA87196B616),
    TPasDblStrUtilsUInt64($BC8955E946FE31CD),
    TPasDblStrUtilsUInt64($6BABAB6398BDBE41),
    TPasDblStrUtilsUInt64($C696963C7EED2DD1),
    TPasDblStrUtilsUInt64($FC1E1DE5CF543CA2),
    TPasDblStrUtilsUInt64($3B25A55F43294BCB),
    TPasDblStrUtilsUInt64($49EF0EB713F39EBE),
    TPasDblStrUtilsUInt64($6E3569326C784337),
    TPasDblStrUtilsUInt64($49C2C37F07965404),
    TPasDblStrUtilsUInt64($DC33745EC97BE906),
    TPasDblStrUtilsUInt64($69A028BB3DED71A3),
    TPasDblStrUtilsUInt64($C40832EA0D68CE0C),
    TPasDblStrUtilsUInt64($F50A3FA490C30190),
    TPasDblStrUtilsUInt64($792667C6DA79E0FA),
    TPasDblStrUtilsUInt64($577001B891185938),
    TPasDblStrUtilsUInt64($ED4C0226B55E6F86),
    TPasDblStrUtilsUInt64($544F8158315B05B4),
    TPasDblStrUtilsUInt64($696361AE3DB1C721),
    TPasDblStrUtilsUInt64($3BC3A19CD1E38E9),
    TPasDblStrUtilsUInt64($4AB48A04065C723),
    TPasDblStrUtilsUInt64($62EB0D64283F9C76),
    TPasDblStrUtilsUInt64($3BA5D0BD324F8394),
    TPasDblStrUtilsUInt64($CA8F44EC7EE36479),
    TPasDblStrUtilsUInt64($7E998B13CF4E1ECB),
    TPasDblStrUtilsUInt64($9E3FEDD8C321A67E),
    TPasDblStrUtilsUInt64($C5CFE94EF3EA101E),
    TPasDblStrUtilsUInt64($BBA1F1D158724A12),
    TPasDblStrUtilsUInt64($2A8A6E45AE8EDC97),
    TPasDblStrUtilsUInt64($F52D09D71A3293BD),
    TPasDblStrUtilsUInt64($593C2626705F9C56),
    TPasDblStrUtilsUInt64($6F8B2FB00C77836C),
    TPasDblStrUtilsUInt64($B6DFB9C0F956447),
    TPasDblStrUtilsUInt64($4724BD4189BD5EAC),
    TPasDblStrUtilsUInt64($58EDEC91EC2CB657),
    TPasDblStrUtilsUInt64($2F2967B66737E3ED),
    TPasDblStrUtilsUInt64($BD79E0D20082EE74),
    TPasDblStrUtilsUInt64($ECD8590680A3AA11),
    TPasDblStrUtilsUInt64($E80E6F4820CC9495),
    TPasDblStrUtilsUInt64($3109058D147FDCDD),
    TPasDblStrUtilsUInt64($BD4B46F0599FD415),
    TPasDblStrUtilsUInt64($6C9E18AC7007C91A),
    TPasDblStrUtilsUInt64($3E2CF6BC604DDB0),
    TPasDblStrUtilsUInt64($84DB8346B786151C),
    TPasDblStrUtilsUInt64($E612641865679A63),
    TPasDblStrUtilsUInt64($4FCB7E8F3F60C07E),
    TPasDblStrUtilsUInt64($E3BE5E330F38F09D),
    TPasDblStrUtilsUInt64($5CADF5BFD3072CC5),
    TPasDblStrUtilsUInt64($73D9732FC7C8F7F6),
    TPasDblStrUtilsUInt64($2867E7FDDCDD9AFA),
    TPasDblStrUtilsUInt64($B281E1FD541501B8),
    TPasDblStrUtilsUInt64($1F225A7CA91A4226),
    TPasDblStrUtilsUInt64($3375788DE9B06958),
    TPasDblStrUtilsUInt64($52D6B1641C83AE),
    TPasDblStrUtilsUInt64($C0678C5DBD23A49A),
    TPasDblStrUtilsUInt64($F840B7BA963646E0),
    TPasDblStrUtilsUInt64($B650E5A93BC3D898),
    TPasDblStrUtilsUInt64($A3E51F138AB4CEBE),
    TPasDblStrUtilsUInt64($C66F336C36B10137),
    TPasDblStrUtilsUInt64($B80B0047445D4184),
    TPasDblStrUtilsUInt64($A60DC059157491E5),
    TPasDblStrUtilsUInt64($87C89837AD68DB2F),
    TPasDblStrUtilsUInt64($29BABE4598C311FB),
    TPasDblStrUtilsUInt64($F4296DD6FEF3D67A),
    TPasDblStrUtilsUInt64($1899E4A65F58660C),
    TPasDblStrUtilsUInt64($5EC05DCFF72E7F8F),
    TPasDblStrUtilsUInt64($76707543F4FA1F73),
    TPasDblStrUtilsUInt64($6A06494A791C53A8),
    TPasDblStrUtilsUInt64($487DB9D17636892),
    TPasDblStrUtilsUInt64($45A9D2845D3C42B6),
    TPasDblStrUtilsUInt64($B8A2392BA45A9B2),
    TPasDblStrUtilsUInt64($8E6CAC7768D7141E),
    TPasDblStrUtilsUInt64($3207D795430CD926),
    TPasDblStrUtilsUInt64($7F44E6BD49E807B8),
    TPasDblStrUtilsUInt64($5F16206C9C6209A6),
    TPasDblStrUtilsUInt64($36DBA887C37A8C0F),
    TPasDblStrUtilsUInt64($C2494954DA2C9789),
    TPasDblStrUtilsUInt64($F2DB9BAA10B7BD6C),
    TPasDblStrUtilsUInt64($6F92829494E5ACC7),
    TPasDblStrUtilsUInt64($CB772339BA1F17F9),
    TPasDblStrUtilsUInt64($FF2A760414536EFB),
    TPasDblStrUtilsUInt64($FEF5138519684ABA),
    TPasDblStrUtilsUInt64($7EB258665FC25D69),
    TPasDblStrUtilsUInt64($EF2F773FFBD97A61),
    TPasDblStrUtilsUInt64($AAFB550FFACFD8FA),
    TPasDblStrUtilsUInt64($95BA2A53F983CF38),
    TPasDblStrUtilsUInt64($DD945A747BF26183),
    TPasDblStrUtilsUInt64($94F971119AEEF9E4),
    TPasDblStrUtilsUInt64($7A37CD5601AAB85D),
    TPasDblStrUtilsUInt64($AC62E055C10AB33A),
    TPasDblStrUtilsUInt64($577B986B314D6009),
    TPasDblStrUtilsUInt64($ED5A7E85FDA0B80B),
    TPasDblStrUtilsUInt64($14588F13BE847307),
    TPasDblStrUtilsUInt64($596EB2D8AE258FC8),
    TPasDblStrUtilsUInt64($6FCA5F8ED9AEF3BB),
    TPasDblStrUtilsUInt64($25DE7BB9480D5854),
    TPasDblStrUtilsUInt64($AF561AA79A10AE6A),
    TPasDblStrUtilsUInt64($1B2BA1518094DA04),
    TPasDblStrUtilsUInt64($90FB44D2F05D0842),
    TPasDblStrUtilsUInt64($353A1607AC744A53),
    TPasDblStrUtilsUInt64($42889B8997915CE8),
    TPasDblStrUtilsUInt64($69956135FEBADA11),
    TPasDblStrUtilsUInt64($43FAB9837E699095),
    TPasDblStrUtilsUInt64($94F967E45E03F4BB),
    TPasDblStrUtilsUInt64($1D1BE0EEBAC278F5),
    TPasDblStrUtilsUInt64($6462D92A69731732),
    TPasDblStrUtilsUInt64($7D7B8F7503CFDCFE),
    TPasDblStrUtilsUInt64($5CDA735244C3D43E),
    TPasDblStrUtilsUInt64($3A0888136AFA64A7),
    TPasDblStrUtilsUInt64($88AAA1845B8FDD0),
    TPasDblStrUtilsUInt64($8AAD549E57273D45),
    TPasDblStrUtilsUInt64($36AC54E2F678864B),
    TPasDblStrUtilsUInt64($84576A1BB416A7DD),
    TPasDblStrUtilsUInt64($656D44A2A11C51D5),
    TPasDblStrUtilsUInt64($9F644AE5A4B1B325),
    TPasDblStrUtilsUInt64($873D5D9F0DDE1FEE),
    TPasDblStrUtilsUInt64($A90CB506D155A7EA),
    TPasDblStrUtilsUInt64($9A7F12442D588F2),
    TPasDblStrUtilsUInt64($C11ED6D538AEB2F),
    TPasDblStrUtilsUInt64($8F1668C8A86DA5FA),
    TPasDblStrUtilsUInt64($F96E017D694487BC),
    TPasDblStrUtilsUInt64($37C981DCC395A9AC),
    TPasDblStrUtilsUInt64($85BBE253F47B1417),
    TPasDblStrUtilsUInt64($93956D7478CCEC8E),
    TPasDblStrUtilsUInt64($387AC8D1970027B2),
    TPasDblStrUtilsUInt64($6997B05FCC0319E),
    TPasDblStrUtilsUInt64($441FECE3BDF81F03),
    TPasDblStrUtilsUInt64($D527E81CAD7626C3),
    TPasDblStrUtilsUInt64($8A71E223D8D3B074),
    TPasDblStrUtilsUInt64($F6872D5667844E49),
    TPasDblStrUtilsUInt64($B428F8AC016561DB),
    TPasDblStrUtilsUInt64($E13336D701BEBA52),
    TPasDblStrUtilsUInt64($ECC0024661173473),
    TPasDblStrUtilsUInt64($27F002D7F95D0190),
    TPasDblStrUtilsUInt64($31EC038DF7B441F4),
    TPasDblStrUtilsUInt64($7E67047175A15271),
    TPasDblStrUtilsUInt64($F0062C6E984D386),
    TPasDblStrUtilsUInt64($52C07B78A3E60868),
    TPasDblStrUtilsUInt64($A7709A56CCDF8A82),
    TPasDblStrUtilsUInt64($88A66076400BB691),
    TPasDblStrUtilsUInt64($6ACFF893D00EA435),
    TPasDblStrUtilsUInt64($583F6B8C4124D43),
    TPasDblStrUtilsUInt64($C3727A337A8B704A),
    TPasDblStrUtilsUInt64($744F18C0592E4C5C),
    TPasDblStrUtilsUInt64($1162DEF06F79DF73),
    TPasDblStrUtilsUInt64($8ADDCB5645AC2BA8),
    TPasDblStrUtilsUInt64($6D953E2BD7173692),
    TPasDblStrUtilsUInt64($C8FA8DB6CCDD0437),
    TPasDblStrUtilsUInt64($1D9C9892400A22A2),
    TPasDblStrUtilsUInt64($2503BEB6D00CAB4B),
    TPasDblStrUtilsUInt64($2E44AE64840FD61D),
    TPasDblStrUtilsUInt64($5CEAECFED289E5D2),
    TPasDblStrUtilsUInt64($7425A83E872C5F47),
    TPasDblStrUtilsUInt64($D12F124E28F77719),
    TPasDblStrUtilsUInt64($82BD6B70D99AAA6F),
    TPasDblStrUtilsUInt64($636CC64D1001550B),
    TPasDblStrUtilsUInt64($3C47F7E05401AA4E),
    TPasDblStrUtilsUInt64($65ACFAEC34810A71),
    TPasDblStrUtilsUInt64($7F1839A741A14D0D),
    TPasDblStrUtilsUInt64($1EDE48111209A050),
    TPasDblStrUtilsUInt64($934AED0AAB460432),
    TPasDblStrUtilsUInt64($F81DA84D5617853F),
    TPasDblStrUtilsUInt64($36251260AB9D668E),
    TPasDblStrUtilsUInt64($C1D72B7C6B426019),
    TPasDblStrUtilsUInt64($B24CF65B8612F81F),
    TPasDblStrUtilsUInt64($DEE033F26797B627),
    TPasDblStrUtilsUInt64($169840EF017DA3B1),
    TPasDblStrUtilsUInt64($8E1F289560EE864E),
    TPasDblStrUtilsUInt64($F1A6F2BAB92A27E2),
    TPasDblStrUtilsUInt64($AE10AF696774B1DB),
    TPasDblStrUtilsUInt64($ACCA6DA1E0A8EF29),
    TPasDblStrUtilsUInt64($17FD090A58D32AF3),
    TPasDblStrUtilsUInt64($DDFC4B4CEF07F5B0),
    TPasDblStrUtilsUInt64($4ABDAF101564F98E),
    TPasDblStrUtilsUInt64($9D6D1AD41ABE37F1),
    TPasDblStrUtilsUInt64($84C86189216DC5ED),
    TPasDblStrUtilsUInt64($32FD3CF5B4E49BB4),
    TPasDblStrUtilsUInt64($3FBC8C33221DC2A1),
    TPasDblStrUtilsUInt64($FABAF3FEAA5334A),
    TPasDblStrUtilsUInt64($29CB4D87F2A7400E),
    TPasDblStrUtilsUInt64($743E20E9EF511012),
    TPasDblStrUtilsUInt64($914DA9246B255416),
    TPasDblStrUtilsUInt64($1AD089B6C2F7548E),
    TPasDblStrUtilsUInt64($A184AC2473B529B1),
    TPasDblStrUtilsUInt64($C9E5D72D90A2741E),
    TPasDblStrUtilsUInt64($7E2FA67C7A658892),
    TPasDblStrUtilsUInt64($DDBB901B98FEEAB7),
    TPasDblStrUtilsUInt64($552A74227F3EA565),
    TPasDblStrUtilsUInt64($D53A88958F87275F),
    TPasDblStrUtilsUInt64($8A892ABAF368F137),
    TPasDblStrUtilsUInt64($2D2B7569B0432D85),
    TPasDblStrUtilsUInt64($9C3B29620E29FC73),
    TPasDblStrUtilsUInt64($8349F3BA91B47B8F),
    TPasDblStrUtilsUInt64($241C70A936219A73),
    TPasDblStrUtilsUInt64($ED238CD383AA0110),
    TPasDblStrUtilsUInt64($F4363804324A40AA),
    TPasDblStrUtilsUInt64($B143C6053EDCD0D5),
    TPasDblStrUtilsUInt64($DD94B7868E94050A),
    TPasDblStrUtilsUInt64($CA7CF2B4191C8326),
    TPasDblStrUtilsUInt64($FD1C2F611F63A3F0),
    TPasDblStrUtilsUInt64($BC633B39673C8CEC),
    TPasDblStrUtilsUInt64($D5BE0503E085D813),
    TPasDblStrUtilsUInt64($4B2D8644D8A74E18),
    TPasDblStrUtilsUInt64($DDF8E7D60ED1219E),
    TPasDblStrUtilsUInt64($CABB90E5C942B503),
    TPasDblStrUtilsUInt64($3D6A751F3B936243),
    TPasDblStrUtilsUInt64($0CC512670A783AD4),
    TPasDblStrUtilsUInt64($27FB2B80668B24C5),
    TPasDblStrUtilsUInt64($B1F9F660802DEDF6),
    TPasDblStrUtilsUInt64($5E7873F8A0396973),
    TPasDblStrUtilsUInt64($DB0B487B6423E1E8),
    TPasDblStrUtilsUInt64($91CE1A9A3D2CDA62),
    TPasDblStrUtilsUInt64($7641A140CC7810FB),
    TPasDblStrUtilsUInt64($A9E904C87FCB0A9D),
    TPasDblStrUtilsUInt64($546345FA9FBDCD44),
    TPasDblStrUtilsUInt64($A97C177947AD4095),
    TPasDblStrUtilsUInt64($49ED8EABCCCC485D),
    TPasDblStrUtilsUInt64($5C68F256BFFF5A74),
    TPasDblStrUtilsUInt64($73832EEC6FFF3111),
    TPasDblStrUtilsUInt64($C831FD53C5FF7EAB),
    TPasDblStrUtilsUInt64($BA3E7CA8B77F5E55),
    TPasDblStrUtilsUInt64($28CE1BD2E55F35EB),
    TPasDblStrUtilsUInt64($7980D163CF5B81B3),
    TPasDblStrUtilsUInt64($D7E105BCC332621F),
    TPasDblStrUtilsUInt64($8DD9472BF3FEFAA7),
    TPasDblStrUtilsUInt64($B14F98F6F0FEB951),
    TPasDblStrUtilsUInt64($6ED1BF9A569F33D3),
    TPasDblStrUtilsUInt64($0A862F80EC4700C8),
    TPasDblStrUtilsUInt64($CD27BB612758C0FA),
    TPasDblStrUtilsUInt64($8038D51CB897789C),
    TPasDblStrUtilsUInt64($E0470A63E6BD56C3),
    TPasDblStrUtilsUInt64($1858CCFCE06CAC74),
    TPasDblStrUtilsUInt64($0F37801E0C43EBC8),
    TPasDblStrUtilsUInt64($D30560258F54E6BA),
    TPasDblStrUtilsUInt64($47C6B82EF32A2069),
    TPasDblStrUtilsUInt64($4CDC331D57FA5441),
    TPasDblStrUtilsUInt64($E0133FE4ADF8E952),
    TPasDblStrUtilsUInt64($58180FDDD97723A6),
    TPasDblStrUtilsUInt64($570F09EAA7EA7648));
var
  FactorMantissa, Upper, Lower, FactorMantissaLow, ProductLow, ProductMiddle,
    ProductMiddle1, ProductMiddle2, ProductHigh, UpperBit, Mantissa,
    RealExponent: TPasDblStrUtilsUInt64;
  Exponent: TPasDblStrUtilsInt64;
  LeadingZeros: TPasDblStrUtilsInt32;
  Product: TPasDblStrUtilsUInt128;
begin
  if assigned(aSuccess) then
  begin
    aSuccess^ := false;
  end;
{$IF defined(CPUx86_64) or defined(CPUAArch64)}
  if ({$IF defined(PasDblStrUtilsDenormalsAreNotZeros)}(-22){$ELSE}0{$IFEND} <=
    aBase10Exponent) and (aBase10Exponent <= 22) and
    (aBase10Mantissa <= 9007199254740991)
{$IFNDEF PasDblStrUtilsNoFPUModeCheck}
    and (GetRoundMode = rmNearest) and
    (GetPrecisionMode in [pmDouble, pmExtended])
{$ENDIF} then
  begin
    result := aBase10Mantissa;
    if aBase10Exponent < 0 then
    begin
      result := result / PowerOfTen[-aBase10Exponent];
    end
    else
    begin
      result := result * PowerOfTen[aBase10Exponent];
    end;
    if aNegative then
    begin
      result := -result;
    end;
    if assigned(aSuccess) then
    begin
      aSuccess^ := true;
    end;
    exit;
  end;
{$IFEND}
  if aBase10Mantissa = 0 then
  begin
    if aNegative then
    begin
      result := UInt64Bits2Double(TPasDblStrUtilsUInt64($8000000000000000));
    end
    else
    begin
      result := UInt64Bits2Double(TPasDblStrUtilsUInt64($0000000000000000));
    end;
    if assigned(aSuccess) then
    begin
      aSuccess^ := true;
    end;
    exit;
  end;
  if (aBase10Exponent >= FASTFLOAT_SMALLEST_POWER) and
    (aBase10Exponent <= FASTFLOAT_LARGEST_POWER) then
  begin
    FactorMantissa := Mantissa64[aBase10Exponent];
{$IF declared(SARInt64)}
    Exponent := (SARInt64((152170 + 65536) * aBase10Exponent, 16)) +
      (1024 + 63);
{$ELSE}
    Exponent := (152170 + 65536) * aBase10Exponent;
    if (TPasDblStrUtilsUInt64(Exponent) and (TPasDblStrUtilsUInt64(1) shl 63))
      <> 0 then
    begin
      Exponent := TPasDblStrUtilsInt64
        (TPasDblStrUtilsUInt64((TPasDblStrUtilsUInt64(Exponent) shr 16) or
        TPasDblStrUtilsUInt64($FFFF000000000000)));
    end
    else
    begin
      Exponent := TPasDblStrUtilsInt64
        (TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(Exponent) shr 16));
    end;
    inc(Exponent, 1024 + 63);
{$IFEND}
    LeadingZeros := CLZQWord(aBase10Mantissa);
    aBase10Mantissa := aBase10Mantissa shl LeadingZeros;
    TPasDblStrUtilsUInt128.Mul64(Product, aBase10Mantissa, FactorMantissa);
    Upper := Product.Hi;
    Lower := Product.Lo;
    if ((Upper and $1FF) = $1FF) and ((Lower + aBase10Mantissa) < Lower) then
    begin
      FactorMantissaLow := Mantissa128[aBase10Exponent];
      TPasDblStrUtilsUInt128.Mul64(Product, aBase10Mantissa, FactorMantissaLow);
      ProductLow := Product.Lo;
      ProductMiddle2 := Product.Hi;
      ProductMiddle1 := Lower;
      ProductHigh := Upper;
      ProductMiddle := ProductMiddle1 + ProductMiddle2;
      if ProductMiddle < ProductMiddle1 then
      begin
        inc(ProductHigh);
      end;
      if (((ProductMiddle + 1) = 0) and ((ProductHigh and $1FF) = $1FF) and
        ((ProductLow + aBase10Mantissa) < ProductLow)) then
      begin
        result := 0;
        exit;
      end;
      Upper := ProductHigh;
      Lower := ProductMiddle;
    end;
    UpperBit := Upper shr 63;
    Mantissa := Upper shr (UpperBit + 9);
    inc(LeadingZeros, 1 xor UpperBit);
    if ((Lower = 0) and ((Upper and $1FF) = 0) and ((Mantissa and 3) = 1)) then
    begin
      result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
      // NaN
      exit;
    end;
    Mantissa := (Mantissa + (Mantissa and 1)) shr 1;
    if Mantissa >= (TPasDblStrUtilsUInt64(1) shl 53) then
    begin
      Mantissa := TPasDblStrUtilsUInt64(1) shl 52;
      dec(LeadingZeros);
    end;
    RealExponent := Exponent - LeadingZeros;
    if (RealExponent < 1) or (RealExponent > 2046) then
    begin
      result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
      // NaN
      exit;
    end;
    result := UInt64Bits2Double((TPasDblStrUtilsUInt64(ord(aNegative) and 1)
      shl 63) or (TPasDblStrUtilsUInt64(RealExponent) shl 52) or
      (Mantissa and not(TPasDblStrUtilsUInt64(1) shl 52)));
    if assigned(aSuccess) then
    begin
      aSuccess^ := true;
    end;
  end
  else
  begin
    result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
    // NaN
  end;
end;

function EiselLemireStringToDouble(const aStringValue: PPasDblStrUtilsChar;
  const aStringLength: TPasDblStrUtilsInt32;
  const aOK: PPasDblStrUtilsBoolean = nil): TPasDblStrUtilsDouble;
const
  Base10MantissaLimit = TPasDblStrUtilsUInt64(999999999999999990);
var
  StringPosition: TPasDblStrUtilsInt32;
  Base10Mantissa: TPasDblStrUtilsUInt64;
  Base10Exponent, ExponentValue: TPasDblStrUtilsInt64;
  HasDigits, Negative, ExponentNegative: Boolean;
  c: TPasDblStrUtilsChar;
begin

  Negative := false;
  StringPosition := 0;
  Base10Mantissa := 0;
  Base10Exponent := 0;

  while (StringPosition < aStringLength) and
    (aStringValue[StringPosition] in ['-', '+']) do
  begin
    Negative := Negative xor (aStringValue[StringPosition] = '-');
    inc(StringPosition);
  end;

  HasDigits := (StringPosition < aStringLength) and
    (aStringValue[StringPosition] in ['0' .. '9']);
  if HasDigits then
  begin
    while StringPosition < aStringLength do
    begin
      c := aStringValue[StringPosition];
      case c of
        '0' .. '9':
          begin
            Base10Mantissa := (Base10Mantissa * 10) + TPasDblStrUtilsUInt64
              (TPasDblStrUtilsUInt8(TPasDblStrUtilsChar(aStringValue
              [StringPosition])) - TPasDblStrUtilsUInt8
              (TPasDblStrUtilsChar('0')));
            if Base10Mantissa >= Base10MantissaLimit then
            begin
              result := UInt64Bits2Double
                (TPasDblStrUtilsUInt64($7FF8000000000000)); // NaN
              if assigned(aOK) then
              begin
                aOK^ := false;
              end;
              exit;
            end;
            inc(StringPosition);
          end;
      else
        begin
          break;
        end;
      end;
    end;
  end;

  if (StringPosition < aStringLength) and (aStringValue[StringPosition] = '.')
  then
  begin
    inc(StringPosition);
    if (StringPosition < aStringLength) and
      (aStringValue[StringPosition] in ['0' .. '9']) then
    begin
      HasDigits := true;
      while StringPosition < aStringLength do
      begin
        c := aStringValue[StringPosition];
        case c of
          '0' .. '9':
            begin
              Base10Mantissa := (Base10Mantissa * 10) + TPasDblStrUtilsUInt64
                (TPasDblStrUtilsUInt8(TPasDblStrUtilsChar(aStringValue
                [StringPosition])) - TPasDblStrUtilsUInt8
                (TPasDblStrUtilsChar('0')));
              if Base10Mantissa >= Base10MantissaLimit then
              begin
                result := UInt64Bits2Double
                  (TPasDblStrUtilsUInt64($7FF8000000000000)); // NaN
                if assigned(aOK) then
                begin
                  aOK^ := false;
                end;
                exit;
              end;
              inc(StringPosition);
              dec(Base10Exponent);
            end;
        else
          begin
            break;
          end;
        end;
      end;
    end;
  end;

  if not HasDigits then
  begin
    result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
    // NaN
    if assigned(aOK) then
    begin
      aOK^ := false;
    end;
    exit;
  end;

  if (StringPosition < aStringLength) and
    (aStringValue[StringPosition] in ['e', 'E']) then
  begin
    inc(StringPosition);
    if (StringPosition < aStringLength) and
      (aStringValue[StringPosition] in ['+', '-']) then
    begin
      ExponentNegative := aStringValue[StringPosition] = '-';
      inc(StringPosition);
    end
    else
    begin
      ExponentNegative := false;
    end;
    if (StringPosition < aStringLength) and
      (aStringValue[StringPosition] in ['0' .. '9']) then
    begin
      ExponentValue := 0;
      repeat
        ExponentValue := (ExponentValue * 10) + TPasDblStrUtilsInt32
          (TPasDblStrUtilsUInt8(TPasDblStrUtilsChar(aStringValue[StringPosition]
          )) - TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0')));
        inc(StringPosition);
      until (StringPosition >= aStringLength) or
        not(aStringValue[StringPosition] in ['0' .. '9']);
      if ExponentNegative then
      begin
        dec(Base10Exponent, ExponentValue);
      end
      else
      begin
        inc(Base10Exponent, ExponentValue);
      end;
    end
    else
    begin
      result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
      // NaN
      if assigned(aOK) then
      begin
        aOK^ := false;
      end;
      exit;
    end;
  end;

  if Base10Mantissa = 0 then
  begin
    Base10Exponent := 0;
  end;

  if StringPosition >= aStringLength then
  begin
    result := ComputeFloat64(Base10Exponent, Base10Mantissa, Negative, aOK);
  end
  else
  begin
    result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
    // NaN
    if assigned(aOK) then
    begin
      aOK^ := false;
    end;
  end;

end;

function EiselLemireStringToDouble(const aStringValue: TPasDblStrUtilsString;
  const aOK: PPasDblStrUtilsBoolean = nil): TPasDblStrUtilsDouble;
begin
  result := EiselLemireStringToDouble(@aStringValue[1],
    length(aStringValue), aOK);
end;

function RyuStringToDouble(const aStringValue: PPasDblStrUtilsChar;
  const aStringLength: TPasDblStrUtilsInt32;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aCountDigits: PPasDblStrUtilsInt32 = nil): TPasDblStrUtilsDouble;
const
  DOUBLE_MANTISSA_BITS = 52;
  DOUBLE_EXPONENT_BITS = 11;
  DOUBLE_EXPONENT_BIAS = 1023;
var
  CountBase10MantissaDigits, ExtraCountBase10MantissaDigits,
    CountBase10ExponentDigits, DotPosition, ExponentPosition,
    Base10MantissaBits, Base2MantissaBits, Base10Exponent, Position,
    Base2Exponent, Shift, Temporary, Exponent: TPasDblStrUtilsInt32;
  Base10Mantissa, Base2Mantissa, IEEEMantissa: TPasDblStrUtilsUInt64;
  IEEEExponent, LastRemovedBit: TPasDblStrUtilsUInt32;
  SignedMantissa, SignedExponent, TrailingZeros, RoundUp: Boolean;
  c: AnsiChar;
begin
  if assigned(aOK) then
  begin
    aOK^ := false;
  end;
  if aStringLength = 0 then
  begin
    result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
    // NaN
    exit;
  end;
  CountBase10MantissaDigits := 0;
  ExtraCountBase10MantissaDigits := 0;
  CountBase10ExponentDigits := 0;
  DotPosition := aStringLength;
  ExponentPosition := aStringLength;
  Base10Mantissa := 0;
  Base10Exponent := 0;
  SignedMantissa := false;
  SignedExponent := false;
  Position := 0;
  while (Position < aStringLength) and
    (aStringValue[Position] in [#0 .. #32]) do
  begin
    inc(Position);
  end;
  while (Position < aStringLength) and (aStringValue[Position] in ['-', '+']) do
  begin
    if aStringValue[Position] = '-' then
    begin
      SignedMantissa := not SignedMantissa;
    end;
    inc(Position);
  end;
  if (Position + 2) < aStringLength then
  begin
    if (aStringValue[Position] in ['n', 'N']) and
      (aStringValue[Position + 1] in ['a', 'A']) and
      (aStringValue[Position + 2] in ['n', 'N']) then
    begin
      if SignedMantissa then
      begin
        result := UInt64Bits2Double(TPasDblStrUtilsUInt64($FFF8000000000000));
        // -NaN
      end
      else
      begin
        result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
        // +NaN
      end;
      if assigned(aOK) then
      begin
        aOK^ := true;
      end;
      exit;
    end
    else if (aStringValue[Position] in ['i', 'I']) and
      (aStringValue[Position + 1] in ['n', 'N']) and
      (aStringValue[Position + 2] in ['f', 'F']) then
    begin
      if SignedMantissa then
      begin
        result := UInt64Bits2Double(TPasDblStrUtilsUInt64($FFF0000000000000));
        // -Inf
      end
      else
      begin
        result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF0000000000000));
        // +Inf
      end;
      if assigned(aOK) then
      begin
        aOK^ := true;
      end;
      exit;
    end;
  end;
  while Position < aStringLength do
  begin
    c := aStringValue[Position];
    case c of
      '.':
        begin
          if DotPosition <> aStringLength then
          begin
            result := 0.0;
            exit;
          end;
          DotPosition := Position;
        end;
      '0' .. '9':
        begin
          if CountBase10MantissaDigits < 17 then
          begin
            Base10Mantissa := (Base10Mantissa * 10) + TPasDblStrUtilsUInt64
              (TPasDblStrUtilsUInt8(AnsiChar(c)) - TPasDblStrUtilsUInt8
              (AnsiChar('0')));
            if Base10Mantissa <> 0 then
            begin
              inc(CountBase10MantissaDigits);
            end;
          end
          else
          begin
            inc(ExtraCountBase10MantissaDigits);
          end;
        end;
    else
      begin
        break;
      end;
    end;
    inc(Position);
  end;
  if (Position < aStringLength) and (aStringValue[Position] in ['e', 'E']) then
  begin
    ExponentPosition := Position;
    inc(Position);
    if (Position < aStringLength) and (aStringValue[Position] in ['-', '+'])
    then
    begin
      SignedExponent := aStringValue[Position] = '-';
      inc(Position);
    end;
    while Position < aStringLength do
    begin
      c := aStringValue[Position];
      case c of
        '0' .. '9':
          begin
            if CountBase10ExponentDigits > 3 then
            begin
              if SignedExponent or (Base10Mantissa = 0) then
              begin
                if SignedMantissa then
                begin
                  result := UInt64Bits2Double
                    (TPasDblStrUtilsUInt64($8000000000000000)); // -0
                end
                else
                begin
                  result := UInt64Bits2Double
                    (TPasDblStrUtilsUInt64($0000000000000000)); // +0
                end;
              end
              else
              begin
                if SignedMantissa then
                begin
                  result := UInt64Bits2Double
                    (TPasDblStrUtilsUInt64($FFF0000000000000)); // -Inf
                end
                else
                begin
                  result := UInt64Bits2Double
                    (TPasDblStrUtilsUInt64($7FF0000000000000)); // +Inf
                end;
              end;
              if assigned(aOK) then
              begin
                aOK^ := true;
              end;
              exit;
            end;
            Base10Exponent := (Base10Exponent * 10) +
              (TPasDblStrUtilsUInt8(AnsiChar(c)) - TPasDblStrUtilsUInt8
              (AnsiChar('0')));
            if Base10Exponent <> 0 then
            begin
              inc(CountBase10ExponentDigits);
            end;
          end;
      else
        begin
          result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
          // NaN
          exit;
        end;
      end;
      inc(Position);
    end;
  end;
  if Position < aStringLength then
  begin
    result := UInt64Bits2Double(TPasDblStrUtilsUInt64($7FF8000000000000));
    // NaN
    exit;
  end;
  if SignedExponent then
  begin
    Base10Exponent := -Base10Exponent;
  end;
  inc(Base10Exponent, ExtraCountBase10MantissaDigits);
  if DotPosition < ExponentPosition then
  begin
    dec(Base10Exponent, (ExponentPosition - DotPosition) - 1);
  end;
  if Base10Mantissa = 0 then
  begin
    if SignedMantissa then
    begin
      result := UInt64Bits2Double(TPasDblStrUtilsUInt64($8000000000000000));
      // -0
    end
    else
    begin
      result := UInt64Bits2Double(TPasDblStrUtilsUInt64($0000000000000000));
      // +0
    end;
    if assigned(aOK) then
    begin
      aOK^ := true;
    end;
    exit;
  end;
  if ((CountBase10MantissaDigits + Base10Exponent) <= -324) or
    (Base10Mantissa = 0) then
  begin
    result := UInt64Bits2Double(TPasDblStrUtilsUInt64((ord(SignedMantissa) and
      1)) shl (DOUBLE_EXPONENT_BITS + DOUBLE_MANTISSA_BITS));
    if assigned(aOK) then
    begin
      aOK^ := true;
    end;
    exit;
  end;
  if (CountBase10MantissaDigits + Base10Exponent) >= 310 then
  begin
    result := UInt64Bits2Double
      ((TPasDblStrUtilsUInt64((ord(SignedMantissa) and 1))
      shl (DOUBLE_EXPONENT_BITS + DOUBLE_MANTISSA_BITS)) or
      (TPasDblStrUtilsUInt64($7FF) shl DOUBLE_MANTISSA_BITS));
    if assigned(aOK) then
    begin
      aOK^ := false;
    end;
    exit;
  end;
  if Base10Exponent >= 0 then
  begin
    while Base10Exponent >= DOUBLE_POW5_TABLE_SIZE do
    begin
      Base10Mantissa := RoundDiv10(Base10Mantissa);
      dec(Base10Exponent);
    end;
    Base10MantissaBits := FloorLog2(Base10Mantissa);
    Base2Exponent :=
      ((TPasDblStrUtilsInt32(Base10MantissaBits) + Base10Exponent) +
      TPasDblStrUtilsInt32(Log2Pow5(Base10Exponent))) -
      (DOUBLE_MANTISSA_BITS + 1);
    Temporary := ((Base2Exponent - Base10Exponent) -
      CeilLog2Pow5(Base10Exponent)) + DOUBLE_POW5_BITCOUNT;
    Assert(Temporary >= 0);
    Base2Mantissa := MulShift64(Base10Mantissa,
      @DOUBLE_POW5_SPLIT[Base10Exponent], Temporary);
    TrailingZeros := (Base2Exponent < Base10Exponent) or
      (((Base2Exponent - Base10Exponent) < 64) and
      MultipleOfPowerOf2(Base10Mantissa, Base2Exponent - Base10Exponent));
  end
  else
  begin
    while (-Base10Exponent) >= DOUBLE_POW5_INV_TABLE_SIZE do
    begin
      Base10Mantissa := RoundDiv10(Base10Mantissa);
      inc(Base10Exponent);
    end;
    Base10MantissaBits := FloorLog2(Base10Mantissa);
    Base2Exponent :=
      ((TPasDblStrUtilsInt32(Base10MantissaBits) + Base10Exponent) -
      TPasDblStrUtilsInt32(CeilLog2Pow5(-Base10Exponent))) -
      (DOUBLE_MANTISSA_BITS + 1);
    Temporary :=
      (((Base2Exponent - Base10Exponent) + CeilLog2Pow5(-Base10Exponent)) - 1) +
      DOUBLE_POW5_INV_BITCOUNT;
    Assert((-Base10Exponent) < DOUBLE_POW5_INV_TABLE_SIZE);
    Base2Mantissa := MulShift64(Base10Mantissa,
      @DOUBLE_POW5_INV_SPLIT[-Base10Exponent], Temporary);
    TrailingZeros := MultipleOfPowerOf5(Base10Mantissa, -Base10Exponent);
  end;
  Exponent := Base2Exponent + DOUBLE_EXPONENT_BIAS + TPasDblStrUtilsInt32
    (FloorLog2(Base2Mantissa));
  if Exponent < 0 then
  begin
    IEEEExponent := 0;
  end
  else
  begin
    IEEEExponent := Exponent;
  end;
  if IEEEExponent > $7FE then
  begin
    result := UInt64Bits2Double
      ((TPasDblStrUtilsUInt64((ord(SignedMantissa) and 1))
      shl (DOUBLE_EXPONENT_BITS + DOUBLE_MANTISSA_BITS)) or
      (TPasDblStrUtilsUInt64($7FF) shl DOUBLE_MANTISSA_BITS));
    if assigned(aOK) then
    begin
      aOK^ := true;
    end;
    exit;
  end;
  if IEEEExponent = 0 then
  begin
    Shift := 1;
  end
  else
  begin
    Shift := IEEEExponent;
  end;
  Shift := (Shift - Base2Exponent) -
    (DOUBLE_EXPONENT_BIAS + DOUBLE_MANTISSA_BITS);
  Assert(Shift >= 0);
  TrailingZeros := TrailingZeros and
    ((Base2Mantissa and ((TPasDblStrUtilsUInt64(1) shl (Shift - 1)) - 1)) = 0);
  LastRemovedBit := (Base2Mantissa shr (Shift - 1)) and 1;
  RoundUp := (LastRemovedBit <> 0) and
    ((not TrailingZeros) or (((Base2Mantissa shr Shift) and 1) <> 0));
  IEEEMantissa := (Base2Mantissa shr Shift) + TPasDblStrUtilsUInt64
    (ord(RoundUp) and 1);
  Assert(IEEEMantissa <= (TPasDblStrUtilsUInt64(1)
    shl (DOUBLE_MANTISSA_BITS + 1)));
  IEEEMantissa := IEEEMantissa and
    ((TPasDblStrUtilsUInt64(1) shl DOUBLE_MANTISSA_BITS) - 1);
  if (IEEEMantissa = 0) and RoundUp then
  begin
    inc(IEEEExponent);
  end;
  result := UInt64Bits2Double((TPasDblStrUtilsUInt64(ord(SignedMantissa) and 1)
    shl (DOUBLE_EXPONENT_BITS + DOUBLE_MANTISSA_BITS)) or
    (TPasDblStrUtilsUInt64(IEEEExponent) shl DOUBLE_MANTISSA_BITS) or
    IEEEMantissa);
  if assigned(aOK) then
  begin
    aOK^ := true;
  end;
  if assigned(aCountDigits) then
  begin
    aCountDigits^ := CountBase10MantissaDigits +
      ExtraCountBase10MantissaDigits;;
  end;
end;

function RyuStringToDouble(const aStringValue: TPasDblStrUtilsString;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aCountDigits: PPasDblStrUtilsInt32 = nil): TPasDblStrUtilsDouble;
begin
  result := RyuStringToDouble(@aStringValue[1], length(aStringValue), aOK,
    aCountDigits);
end;

function RyuDoubleToString(const aValue: TPasDblStrUtilsDouble;
  const aExponential: Boolean = true): TPasDblStrUtilsString;
const
  DOUBLE_MANTISSA_BITS = 52;
  DOUBLE_EXPONENT_BITS = 11;
  DOUBLE_BIAS = 1023;
type
  TFloatingDecimal64 = record
    Mantissa: TPasDblStrUtilsUInt64;
    Exponent: TPasDblStrUtilsInt32;
  end;

  function DecimalLength17(const aValue: TPasDblStrUtilsUInt64)
    : TPasDblStrUtilsUInt32;
  begin
{$IFDEF fpc}
    case aValue of
      TPasDblStrUtilsUInt64(0) .. TPasDblStrUtilsUInt64(9):
        begin
          result := 1;
        end;
      TPasDblStrUtilsUInt64(10) .. TPasDblStrUtilsUInt64(99):
        begin
          result := 2;
        end;
      TPasDblStrUtilsUInt64(100) .. TPasDblStrUtilsUInt64(999):
        begin
          result := 3;
        end;
      TPasDblStrUtilsUInt64(1000) .. TPasDblStrUtilsUInt64(9999):
        begin
          result := 4;
        end;
      TPasDblStrUtilsUInt64(10000) .. TPasDblStrUtilsUInt64(99999):
        begin
          result := 5;
        end;
      TPasDblStrUtilsUInt64(100000) .. TPasDblStrUtilsUInt64(999999):
        begin
          result := 6;
        end;
      TPasDblStrUtilsUInt64(1000000) .. TPasDblStrUtilsUInt64(9999999):
        begin
          result := 7;
        end;
      TPasDblStrUtilsUInt64(10000000) .. TPasDblStrUtilsUInt64(99999999):
        begin
          result := 8;
        end;
      TPasDblStrUtilsUInt64(100000000) .. TPasDblStrUtilsUInt64(999999999):
        begin
          result := 9;
        end;
      TPasDblStrUtilsUInt64(1000000000) .. TPasDblStrUtilsUInt64(9999999999):
        begin
          result := 10;
        end;
      TPasDblStrUtilsUInt64(10000000000) .. TPasDblStrUtilsUInt64(99999999999):
        begin
          result := 11;
        end;
      TPasDblStrUtilsUInt64(100000000000) .. TPasDblStrUtilsUInt64
        (999999999999):
        begin
          result := 12;
        end;
      TPasDblStrUtilsUInt64(1000000000000) .. TPasDblStrUtilsUInt64
        (9999999999999):
        begin
          result := 13;
        end;
      TPasDblStrUtilsUInt64(10000000000000) .. TPasDblStrUtilsUInt64
        (99999999999999):
        begin
          result := 14;
        end;
      TPasDblStrUtilsUInt64(100000000000000) .. TPasDblStrUtilsUInt64
        (999999999999999):
        begin
          result := 15;
        end;
      TPasDblStrUtilsUInt64(1000000000000000) .. TPasDblStrUtilsUInt64
        (9999999999999999):
        begin
          result := 16;
        end;
    else
      begin
        Assert(aValue < TPasDblStrUtilsUInt64(100000000000000000));
        result := 17;
      end;
    end;
{$ELSE}
    if aValue < TPasDblStrUtilsUInt64(10) then
    begin
      result := 1;
    end
    else if aValue < TPasDblStrUtilsUInt64(100) then
    begin
      result := 2;
    end
    else if aValue < TPasDblStrUtilsUInt64(1000) then
    begin
      result := 3;
    end
    else if aValue < TPasDblStrUtilsUInt64(10000) then
    begin
      result := 4;
    end
    else if aValue < TPasDblStrUtilsUInt64(100000) then
    begin
      result := 5;
    end
    else if aValue < TPasDblStrUtilsUInt64(1000000) then
    begin
      result := 6;
    end
    else if aValue < TPasDblStrUtilsUInt64(10000000) then
    begin
      result := 7;
    end
    else if aValue < TPasDblStrUtilsUInt64(100000000) then
    begin
      result := 8;
    end
    else if aValue < TPasDblStrUtilsUInt64(1000000000) then
    begin
      result := 9;
    end
    else if aValue < TPasDblStrUtilsUInt64(10000000000) then
    begin
      result := 10;
    end
    else if aValue < TPasDblStrUtilsUInt64(100000000000) then
    begin
      result := 11;
    end
    else if aValue < TPasDblStrUtilsUInt64(1000000000000) then
    begin
      result := 12;
    end
    else if aValue < TPasDblStrUtilsUInt64(10000000000000) then
    begin
      result := 13;
    end
    else if aValue < TPasDblStrUtilsUInt64(100000000000000) then
    begin
      result := 14;
    end
    else if aValue < TPasDblStrUtilsUInt64(1000000000000000) then
    begin
      result := 15;
    end
    else if aValue < TPasDblStrUtilsUInt64(10000000000000000) then
    begin
      result := 16;
    end
    else
    begin
      Assert(aValue < TPasDblStrUtilsUInt64(100000000000000000));
      result := 17;
    end;
{$ENDIF}
  end;
  function DoubleToDecimal(const aIEEEMantissa: TPasDblStrUtilsUInt64;
    const aIEEEExponent: TPasDblStrUtilsUInt32): TFloatingDecimal64;
  var
    e2: TPasDblStrUtilsInt32;
    m2, mv, vr, vp, vm, Output, vpDiv10, vmDiv10, vrDiv10, vpDiv100, vmDiv100,
      vrDiv100: TPasDblStrUtilsUInt64;
    mmShift, q, mvMod5, vpMod10, vmMod10, vrMod10,
      vrMod100: TPasDblStrUtilsUInt32;
    e10, k, i, j, Removed: TPasDblStrUtilsInt32;
    LastRemovedDigit: TPasDblStrUtilsUInt8;
    Even, AcceptBounds, vmIsTrailingZeros, vrIsTrailingZeros, RoundUp: Boolean;
  begin
    if aIEEEExponent = 0 then
    begin
      e2 := 1 - (DOUBLE_BIAS + DOUBLE_MANTISSA_BITS + 2);
      m2 := aIEEEMantissa;
    end
    else
    begin
      e2 := aIEEEExponent - (DOUBLE_BIAS + DOUBLE_MANTISSA_BITS + 2);
      m2 := aIEEEMantissa or
        (TPasDblStrUtilsUInt64(1) shl DOUBLE_MANTISSA_BITS);
    end;
    Even := (m2 and 1) = 0;
    AcceptBounds := Even;
    mv := m2 shl 2;
    mmShift := ord((aIEEEMantissa <> 0) or (aIEEEExponent <= 1)) and 1;
    vmIsTrailingZeros := false;
    vrIsTrailingZeros := false;
    if e2 >= 0 then
    begin
      q := Log10Pow2(e2) - (ord(e2 > 3) and 1);
      e10 := q;
      k := Pow5Bits(q) + (DOUBLE_POW5_INV_BITCOUNT - 1);
      i := (TPasDblStrUtilsInt32(q) + TPasDblStrUtilsInt32(k)) -
        TPasDblStrUtilsInt32(e2);
      vr := MulShiftAll64(m2, @DOUBLE_POW5_INV_SPLIT[q], i, vp, vm, mmShift);
      if q <= 21 then
      begin
        mvMod5 := TPasDblStrUtilsUInt32(mv and $FFFFFFFF) -
          TPasDblStrUtilsUInt32(5 * (Div5(mv) and $FFFFFFFF));
        if mvMod5 = 0 then
        begin
          vrIsTrailingZeros := MultipleOfPowerOf5(mv, q);
        end
        else if AcceptBounds then
        begin
          vmIsTrailingZeros := MultipleOfPowerOf5(mv - (1 + mmShift), q);
        end
        else
        begin
          dec(vp, ord(MultipleOfPowerOf5(mv + 2, q)) and 1);
        end;
      end;
    end
    else
    begin
      q := Log10Pow5(-e2) - (ord((-e2) > 1) and 1);
      e10 := q + e2;
      i := (-e2) - TPasDblStrUtilsInt32(q);
      k := Pow5Bits(i) - DOUBLE_POW5_BITCOUNT;
      j := TPasDblStrUtilsInt32(q) - k;
      vr := MulShiftAll64(m2, @DOUBLE_POW5_SPLIT[i], j, vp, vm, mmShift);
      if q <= 1 then
      begin
        vrIsTrailingZeros := true;
        if AcceptBounds then
        begin
          vmIsTrailingZeros := mmShift = 1;
        end
        else
        begin
          dec(vp);
        end;
      end
      else if q < 63 then
      begin
        vrIsTrailingZeros := MultipleOfPowerOf2(mv, q);
      end;
    end;
    Removed := 0;
    LastRemovedDigit := 0;
    if vmIsTrailingZeros or vrIsTrailingZeros then
    begin
      repeat
        vpDiv10 := Div10(vp);
        vmDiv10 := Div10(vm);
        if vpDiv10 <= vmDiv10 then
        begin
          break;
        end;
        vmMod10 := TPasDblStrUtilsUInt32(vm and $FFFFFFFF) -
          (10 * TPasDblStrUtilsUInt32(vmDiv10 and $FFFFFFFF));
        vrDiv10 := Div10(vr);
        vrMod10 := TPasDblStrUtilsUInt32(vr and $FFFFFFFF) -
          (10 * TPasDblStrUtilsUInt32(vrDiv10 and $FFFFFFFF));
        vmIsTrailingZeros := vmIsTrailingZeros and (vmMod10 = 0);
        vrIsTrailingZeros := vrIsTrailingZeros and (LastRemovedDigit = 0);
        LastRemovedDigit := TPasDblStrUtilsUInt8(vrMod10 and $FF);
        vr := vrDiv10;
        vp := vpDiv10;
        vm := vmDiv10;
        inc(Removed);
      until false;
      if vmIsTrailingZeros then
      begin
        repeat
          vmDiv10 := Div10(vm);
          vmMod10 := TPasDblStrUtilsUInt32(vm and $FFFFFFFF) -
            (10 * TPasDblStrUtilsUInt32(vmDiv10 and $FFFFFFFF));
          if vmMod10 <> 0 then
          begin
            break;
          end;
          vpDiv10 := Div10(vp);
          vrDiv10 := Div10(vr);
          vrMod10 := TPasDblStrUtilsUInt32(vr and $FFFFFFFF) -
            (10 * TPasDblStrUtilsUInt32(vrDiv10 and $FFFFFFFF));
          vrIsTrailingZeros := vrIsTrailingZeros and (LastRemovedDigit = 0);
          LastRemovedDigit := TPasDblStrUtilsUInt8(vrMod10 and $FF);
          vr := vrDiv10;
          vp := vpDiv10;
          vm := vmDiv10;
          inc(Removed);
        until false;
      end;
      if vrIsTrailingZeros and (LastRemovedDigit = 5) and ((vr and 1) = 0) then
      begin
        LastRemovedDigit := 4;
      end;
      Output := vr + TPasDblStrUtilsUInt64
        (ord(((vr = vm) and ((not AcceptBounds) or not vmIsTrailingZeros)) or
        (LastRemovedDigit >= 5)) and 1);
    end
    else
    begin
      RoundUp := false;
      vpDiv100 := Div100(vp);
      vmDiv100 := Div100(vm);
      if vpDiv100 > vmDiv100 then
      begin
        vrDiv100 := Div100(vr);
        vrMod100 := TPasDblStrUtilsUInt32(vr and $FFFFFFFF) -
          (100 * TPasDblStrUtilsUInt32(vrDiv100 and $FFFFFFFF));
        RoundUp := vrMod100 >= 50;
        vr := vrDiv100;
        vp := vpDiv100;
        vm := vmDiv100;
        inc(Removed, 2);
      end;
      repeat
        vpDiv10 := Div10(vp);
        vmDiv10 := Div10(vm);
        if vpDiv10 <= vmDiv10 then
        begin
          break;
        end;
        vrDiv10 := Div10(vr);
        vrMod10 := TPasDblStrUtilsUInt32(vr and $FFFFFFFF) -
          (10 * TPasDblStrUtilsUInt32(vrDiv10 and $FFFFFFFF));
        RoundUp := vrMod10 >= 5;
        vr := vrDiv10;
        vp := vpDiv10;
        vm := vmDiv10;
        inc(Removed);
      until false;
      Output := vr + TPasDblStrUtilsUInt64(ord((vr = vm) or RoundUp) and 1);
    end;
    result.Exponent := e10 + Removed;
    result.Mantissa := Output;
  end;
  function DoubleToDecimalSmallInt(const aIEEEMantissa: TPasDblStrUtilsUInt64;
    const aIEEEExponent: TPasDblStrUtilsUInt32;
    out aResult: TFloatingDecimal64): Boolean;
  var
    m2: TPasDblStrUtilsUInt64;
    e2: TPasDblStrUtilsInt32;
  begin
    m2 := (TPasDblStrUtilsUInt64(1) shl DOUBLE_MANTISSA_BITS) or aIEEEMantissa;
    e2 := aIEEEExponent - (DOUBLE_BIAS + DOUBLE_MANTISSA_BITS);
    if (e2 > 0) or (e2 < -52) or
      ((m2 and ((TPasDblStrUtilsUInt64(1) shl (-e2)) - 1)) <> 0) then
    begin
      result := false;
    end
    else
    begin
      aResult.Mantissa := m2 shr (-e2);
      aResult.Exponent := 0;
      result := true;
    end;
  end;

var
  FloatingDecimal64: TFloatingDecimal64;
  Bits, IEEEMantissa, q, Output: TPasDblStrUtilsUInt64;
  IEEEExponent, r: TPasDblStrUtilsUInt32;
  IEEESign: Boolean;
  Len, OutputLen, Index, Anchor, Exponent, Position: TPasDblStrUtilsInt32;
  Digits: array [0 .. 31] of AnsiChar;
begin
  Bits := TPasDblStrUtilsUInt64(Pointer(@aValue)^);
  IEEESign := ((Bits shr (DOUBLE_MANTISSA_BITS + DOUBLE_EXPONENT_BITS))
    and 1) <> 0;
  IEEEMantissa := Bits and
    ((TPasDblStrUtilsUInt64(1) shl DOUBLE_MANTISSA_BITS) - 1);
  IEEEExponent := TPasDblStrUtilsUInt32((Bits shr DOUBLE_MANTISSA_BITS) and
    ((TPasDblStrUtilsUInt64(1) shl DOUBLE_EXPONENT_BITS) - 1));
  if (IEEEExponent = ((TPasDblStrUtilsUInt64(1) shl DOUBLE_EXPONENT_BITS) - 1))
    or ((IEEEExponent = 0) and (IEEEMantissa = 0)) then
  begin
    if IEEEMantissa <> 0 then
    begin
      result := 'NaN';
    end
    else if IEEEExponent <> 0 then
    begin
      if IEEESign then
      begin
        result := '-Infinity';
      end
      else
      begin
        result := 'Infinity';
      end;
    end
    else
    begin
      if aExponential then
      begin
        if IEEESign then
        begin
          result := '-0e0';
        end
        else
        begin
          result := '0e0';
        end;
      end
      else
      begin
        if IEEESign then
        begin
          result := '-0';
        end
        else
        begin
          result := '0';
        end;
      end;
    end;
  end
  else
  begin
    if DoubleToDecimalSmallInt(IEEEMantissa, IEEEExponent, FloatingDecimal64)
    then
    begin
      repeat
        q := Div10(FloatingDecimal64.Mantissa);
        r := TPasDblStrUtilsUInt32(FloatingDecimal64.Mantissa and $FFFFFFFF) -
          (10 * TPasDblStrUtilsUInt32(q and $FFFFFFFF));
        if r <> 0 then
        begin
          break;
        end;
        FloatingDecimal64.Mantissa := q;
        inc(FloatingDecimal64.Exponent);
      until false;
    end
    else
    begin
      FloatingDecimal64 := DoubleToDecimal(IEEEMantissa, IEEEExponent);
    end;
    result := '';
    Len := 0;
    try
      SetLength(result, 128);
      if IEEESign then
      begin
        inc(Len);
        result[Len] := '-';
      end;
      Output := FloatingDecimal64.Mantissa;
      OutputLen := DecimalLength17(Output);
      Exponent := (FloatingDecimal64.Exponent + TPasDblStrUtilsInt32
        (OutputLen)) - 1;
      if aExponential or (abs(Exponent) > 8) then
      begin
        if OutputLen > 1 then
        begin
          Anchor := Len + 1;
          inc(Len, OutputLen + 1);
          for Index := 0 to OutputLen - 2 do
          begin
            result[(Anchor + OutputLen) - Index] :=
              AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0'))
              + (Output mod 10)));
            Output := Output div 10;
          end;
          result[Anchor] :=
            AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0')) +
            (Output mod 10)));
          result[Anchor + 1] := '.';
        end
        else
        begin
          inc(Len);
          result[Len] :=
            AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0')) +
            (Output mod 10)));
        end;
        inc(Len);
        result[Len] := 'E';
        if Exponent < 0 then
        begin
          inc(Len);
          result[Len] := '-';
          Exponent := -Exponent;
        end;
        if Exponent = 0 then
        begin
          inc(Len);
          result[Len] := '0';
        end
        else
        begin
          inc(Len, DecimalLength17(Exponent));
          Index := Len;
          while Exponent > 0 do
          begin
            result[Index] :=
              AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0'))
              + (Exponent mod 10)));
            dec(Index);
            Exponent := Exponent div 10;
          end;
        end;
      end
      else
      begin
        if Exponent < 0 then
        begin
          if length(result) < ((OutputLen - Exponent) + 1) then
          begin
            SetLength(result, (OutputLen - Exponent) + 1);
          end;
          inc(Len);
          result[Len] := '0';
          inc(Len);
          result[Len] := '.';
          inc(Exponent);
          while Exponent < 0 do
          begin
            inc(Len);
            result[Len] := '0';
            inc(Exponent);
          end;
          inc(Len, OutputLen);
          for Index := 0 to OutputLen - 2 do
          begin
            result[Len - Index] :=
              AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0'))
              + (Output mod 10)));
            Output := Output div 10;
          end;
          result[(Len - OutputLen) + 1] :=
            AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0')) +
            (Output mod 10)));
        end
        else
        begin
          if length(result) < (Max(OutputLen, Exponent) + 1) then
          begin
            SetLength(result, Max(OutputLen, Exponent) + 1);
          end;
          Anchor := Len + 1;
          Position := OutputLen - 1;
          for Index := 0 to OutputLen - 1 do
          begin
            Digits[Position] :=
              AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0'))
              + (Output mod 10)));
            dec(Position);
            Output := Output div 10;
          end;
          for Index := 0 to OutputLen - 1 do
          begin
            inc(Len);
            result[Len] := Digits[Index];
            if (Exponent = Index) and (Index <> (OutputLen - 1)) then
            begin
              inc(Len);
              result[Len] := '.';
            end;
          end;
          if OutputLen <= Exponent then
          begin
            for Index := OutputLen to Exponent do
            begin
              inc(Len);
              result[Len] := '0';
            end;
          end;
        end;
      end;
    finally
      SetLength(result, Len);
    end;
  end;
end;

const
  DoubleToStringPowerOfTenTable: array [0 .. 86, 0 .. 2]
    of TPasDblStrUtilsInt64 = ((TPasDblStrUtilsInt64($FA8FD5A0081C0288), -1220,
    -348), (TPasDblStrUtilsInt64($BAAEE17FA23EBF76), -1193, -340),
    (TPasDblStrUtilsInt64($8B16FB203055AC76), -1166, -332),
    (TPasDblStrUtilsInt64($CF42894A5DCE35EA), -1140, -324),
    (TPasDblStrUtilsInt64($9A6BB0AA55653B2D), -1113, -316),
    (TPasDblStrUtilsInt64($E61ACF033D1A45DF), -1087, -308),
    (TPasDblStrUtilsInt64($AB70FE17C79AC6CA), -1060, -300),
    (TPasDblStrUtilsInt64($FF77B1FCBEBCDC4F), -1034, -292),
    (TPasDblStrUtilsInt64($BE5691EF416BD60C), -1007, -284),
    (TPasDblStrUtilsInt64($8DD01FAD907FFC3C), -980, -276),
    (TPasDblStrUtilsInt64($D3515C2831559A83), -954, -268),
    (TPasDblStrUtilsInt64($9D71AC8FADA6C9B5), -927, -260),
    (TPasDblStrUtilsInt64($EA9C227723EE8BCB), -901, -252),
    (TPasDblStrUtilsInt64($AECC49914078536D), -874, -244),
    (TPasDblStrUtilsInt64($823C12795DB6CE57), -847, -236),
    (TPasDblStrUtilsInt64($C21094364DFB5637), -821, -228),
    (TPasDblStrUtilsInt64($9096EA6F3848984F), -794, -220),
    (TPasDblStrUtilsInt64($D77485CB25823AC7), -768, -212),
    (TPasDblStrUtilsInt64($A086CFCD97BF97F4), -741, -204),
    (TPasDblStrUtilsInt64($EF340A98172AACE5), -715, -196),
    (TPasDblStrUtilsInt64($B23867FB2A35B28E), -688, -188),
    (TPasDblStrUtilsInt64($84C8D4DFD2C63F3B), -661, -180),
    (TPasDblStrUtilsInt64($C5DD44271AD3CDBA), -635, -172),
    (TPasDblStrUtilsInt64($936B9FCEBB25C996), -608, -164),
    (TPasDblStrUtilsInt64($DBAC6C247D62A584), -582, -156),
    (TPasDblStrUtilsInt64($A3AB66580D5FDAF6), -555, -148),
    (TPasDblStrUtilsInt64($F3E2F893DEC3F126), -529, -140),
    (TPasDblStrUtilsInt64($B5B5ADA8AAFF80B8), -502, -132),
    (TPasDblStrUtilsInt64($87625F056C7C4A8B), -475, -124),
    (TPasDblStrUtilsInt64($C9BCFF6034C13053), -449, -116),
    (TPasDblStrUtilsInt64($964E858C91BA2655), -422, -108),
    (TPasDblStrUtilsInt64($DFF9772470297EBD), -396, -100),
    (TPasDblStrUtilsInt64($A6DFBD9FB8E5B88F), -369, -92),
    (TPasDblStrUtilsInt64($F8A95FCF88747D94), -343, -84),
    (TPasDblStrUtilsInt64($B94470938FA89BCF), -316, -76),
    (TPasDblStrUtilsInt64($8A08F0F8BF0F156B), -289, -68),
    (TPasDblStrUtilsInt64($CDB02555653131B6), -263, -60),
    (TPasDblStrUtilsInt64($993FE2C6D07B7FAC), -236, -52),
    (TPasDblStrUtilsInt64($E45C10C42A2B3B06), -210, -44),
    (TPasDblStrUtilsInt64($AA242499697392D3), -183, -36),
    (TPasDblStrUtilsInt64($FD87B5F28300CA0E), -157, -28),
    (TPasDblStrUtilsInt64($BCE5086492111AEB), -130, -20),
    (TPasDblStrUtilsInt64($8CBCCC096F5088CC), -103, -12),
    (TPasDblStrUtilsInt64($D1B71758E219652C), -77, -4),
    (TPasDblStrUtilsInt64($9C40000000000000), -50, 4),
    (TPasDblStrUtilsInt64($E8D4A51000000000), -24, 12),
    (TPasDblStrUtilsInt64($AD78EBC5AC620000), 3, 20),
    (TPasDblStrUtilsInt64($813F3978F8940984), 30, 28),
    (TPasDblStrUtilsInt64($C097CE7BC90715B3), 56, 36),
    (TPasDblStrUtilsInt64($8F7E32CE7BEA5C70), 83, 44),
    (TPasDblStrUtilsInt64($D5D238A4ABE98068), 109, 52),
    (TPasDblStrUtilsInt64($9F4F2726179A2245), 136, 60),
    (TPasDblStrUtilsInt64($ED63A231D4C4FB27), 162, 68),
    (TPasDblStrUtilsInt64($B0DE65388CC8ADA8), 189, 76),
    (TPasDblStrUtilsInt64($83C7088E1AAB65DB), 216, 84),
    (TPasDblStrUtilsInt64($C45D1DF942711D9A), 242, 92),
    (TPasDblStrUtilsInt64($924D692CA61BE758), 269, 100),
    (TPasDblStrUtilsInt64($DA01EE641A708DEA), 295, 108),
    (TPasDblStrUtilsInt64($A26DA3999AEF774A), 322, 116),
    (TPasDblStrUtilsInt64($F209787BB47D6B85), 348, 124),
    (TPasDblStrUtilsInt64($B454E4A179DD1877), 375, 132),
    (TPasDblStrUtilsInt64($865B86925B9BC5C2), 402, 140),
    (TPasDblStrUtilsInt64($C83553C5C8965D3D), 428, 148),
    (TPasDblStrUtilsInt64($952AB45CFA97A0B3), 455, 156),
    (TPasDblStrUtilsInt64($DE469FBD99A05FE3), 481, 164),
    (TPasDblStrUtilsInt64($A59BC234DB398C25), 508, 172),
    (TPasDblStrUtilsInt64($F6C69A72A3989F5C), 534, 180),
    (TPasDblStrUtilsInt64($B7DCBF5354E9BECE), 561, 188),
    (TPasDblStrUtilsInt64($88FCF317F22241E2), 588, 196),
    (TPasDblStrUtilsInt64($CC20CE9BD35C78A5), 614, 204),
    (TPasDblStrUtilsInt64($98165AF37B2153DF), 641, 212),
    (TPasDblStrUtilsInt64($E2A0B5DC971F303A), 667, 220),
    (TPasDblStrUtilsInt64($A8D9D1535CE3B396), 694, 228),
    (TPasDblStrUtilsInt64($FB9B7CD9A4A7443C), 720, 236),
    (TPasDblStrUtilsInt64($BB764C4CA7A44410), 747, 244),
    (TPasDblStrUtilsInt64($8BAB8EEFB6409C1A), 774, 252),
    (TPasDblStrUtilsInt64($D01FEF10A657842C), 800, 260),
    (TPasDblStrUtilsInt64($9B10A4E5E9913129), 827, 268),
    (TPasDblStrUtilsInt64($E7109BFBA19C0C9D), 853, 276),
    (TPasDblStrUtilsInt64($AC2820D9623BF429), 880, 284),
    (TPasDblStrUtilsInt64($80444B5E7AA7CF85), 907, 292),
    (TPasDblStrUtilsInt64($BF21E44003ACDD2D), 933, 300),
    (TPasDblStrUtilsInt64($8E679C2F5E44FF8F), 960, 308),
    (TPasDblStrUtilsInt64($D433179D9C8CB841), 986, 316),
    (TPasDblStrUtilsInt64($9E19DB92B4E31BA9), 1013, 324),
    (TPasDblStrUtilsInt64($EB96BF6EBADF77D9), 1039, 332),
    (TPasDblStrUtilsInt64($AF87023B9BF0EE6B), 1066, 340));

  DoubleToStringPowerOfTenBinaryExponentTable: array [-1220 .. (1066 + 27) - 1]
    of TPasDblStrUtilsUInt8 = (0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9,
    9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
    10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
    11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17,
    17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
    17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18,
    18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 20,
    20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
    20, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,
    21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 22, 22,
    22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
    22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 25,
    25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
    25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27,
    27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
    27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29, 29, 29,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 30,
    30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30,
    30, 30, 30, 30, 30, 30, 30, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31,
    31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 32, 32, 32, 32, 32,
    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
    32, 32, 32, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33,
    33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 34, 34, 34, 34, 34, 34, 34, 34, 34,
    34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 35,
    35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
    35, 35, 35, 35, 35, 35, 35, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36,
    36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 37, 37, 37, 37, 37,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
    37, 37, 37, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
    38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 39, 39, 39, 39, 39, 39, 39, 39, 39,
    39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 40,
    40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
    40, 40, 40, 40, 40, 40, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41,
    41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 42, 42, 42, 42, 42,
    42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42,
    42, 42, 42, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
    43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 44, 44, 44, 44, 44, 44, 44, 44, 44,
    44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 45,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    45, 45, 45, 45, 45, 45, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46,
    46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 47, 47, 47, 47, 47,
    47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47,
    47, 47, 47, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
    48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51,
    51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 52, 52, 52, 52, 52,
    52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52,
    52, 52, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53,
    53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 54, 54, 54, 54, 54, 54, 54, 54, 54,
    54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 55,
    55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55,
    55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
    56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 57, 57, 57, 57, 57,
    57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
    57, 57, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58,
    58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 59, 59, 59, 59, 59, 59, 59, 59, 59,
    59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 60, 60,
    60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60,
    60, 60, 60, 60, 60, 60, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
    61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 62, 62, 62, 62, 62,
    62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62,
    62, 62, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 65, 65,
    65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
    65, 65, 65, 65, 65, 65, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
    66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 67, 67, 67, 67, 67, 67,
    67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67,
    67, 67, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68,
    68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 70, 70,
    70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70,
    70, 70, 70, 70, 70, 70, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
    71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 72, 72, 72, 72, 72,
    72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72,
    72, 72, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73,
    73, 73, 73, 73, 73, 73, 73, 73, 73, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 75, 75,
    75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75,
    75, 75, 75, 75, 75, 75, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76,
    76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 77, 77, 77, 77, 77, 77,
    77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77,
    77, 77, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78,
    78, 78, 78, 78, 78, 78, 78, 78, 78, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79,
    79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 80, 80,
    80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80,
    80, 80, 80, 80, 80, 80, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81,
    81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82,
    82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82,
    82, 82, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
    83, 83, 83, 83, 83, 83, 83, 83, 83, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
    84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 85, 85,
    85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85,
    85, 85, 85, 85, 85, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
    86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
    86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
    86, 86, 86);

  DoubleToStringPowerOfTenDecimalExponentTable: array [-348 .. (340 + 8) - 1]
    of TPasDblStrUtilsUInt8 = (0, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
    2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9,
    9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11,
    11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 14,
    14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16,
    16, 16, 16, 16, 17, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18,
    18, 19, 19, 19, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 21, 21,
    21, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23,
    23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25,
    26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 28, 28, 28,
    28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29, 29, 29, 30, 30, 30, 30, 30, 30,
    30, 30, 31, 31, 31, 31, 31, 31, 31, 31, 32, 32, 32, 32, 32, 32, 32, 32, 33,
    33, 33, 33, 33, 33, 33, 33, 34, 34, 34, 34, 34, 34, 34, 34, 35, 35, 35, 35,
    35, 35, 35, 35, 36, 36, 36, 36, 36, 36, 36, 36, 37, 37, 37, 37, 37, 37, 37,
    37, 38, 38, 38, 38, 38, 38, 38, 38, 39, 39, 39, 39, 39, 39, 39, 39, 40, 40,
    40, 40, 40, 40, 40, 40, 41, 41, 41, 41, 41, 41, 41, 41, 42, 42, 42, 42, 42,
    42, 42, 42, 43, 43, 43, 43, 43, 43, 43, 43, 44, 44, 44, 44, 44, 44, 44, 44,
    45, 45, 45, 45, 45, 45, 45, 45, 46, 46, 46, 46, 46, 46, 46, 46, 47, 47, 47,
    47, 47, 47, 47, 47, 48, 48, 48, 48, 48, 48, 48, 48, 49, 49, 49, 49, 49, 49,
    49, 49, 50, 50, 50, 50, 50, 50, 50, 50, 51, 51, 51, 51, 51, 51, 51, 51, 52,
    52, 52, 52, 52, 52, 52, 52, 53, 53, 53, 53, 53, 53, 53, 53, 54, 54, 54, 54,
    54, 54, 54, 54, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, 56,
    56, 57, 57, 57, 57, 57, 57, 57, 57, 58, 58, 58, 58, 58, 58, 58, 58, 59, 59,
    59, 59, 59, 59, 59, 59, 60, 60, 60, 60, 60, 60, 60, 60, 61, 61, 61, 61, 61,
    61, 61, 61, 62, 62, 62, 62, 62, 62, 62, 62, 63, 63, 63, 63, 63, 63, 63, 63,
    64, 64, 64, 64, 64, 64, 64, 64, 65, 65, 65, 65, 65, 65, 65, 65, 66, 66, 66,
    66, 66, 66, 66, 66, 67, 67, 67, 67, 67, 67, 67, 67, 68, 68, 68, 68, 68, 68,
    68, 68, 69, 69, 69, 69, 69, 69, 69, 69, 70, 70, 70, 70, 70, 70, 70, 70, 71,
    71, 71, 71, 71, 71, 71, 71, 72, 72, 72, 72, 72, 72, 72, 72, 73, 73, 73, 73,
    73, 73, 73, 73, 74, 74, 74, 74, 74, 74, 74, 74, 75, 75, 75, 75, 75, 75, 75,
    75, 76, 76, 76, 76, 76, 76, 76, 76, 77, 77, 77, 77, 77, 77, 77, 77, 78, 78,
    78, 78, 78, 78, 78, 78, 79, 79, 79, 79, 79, 79, 79, 79, 80, 80, 80, 80, 80,
    80, 80, 80, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82,
    83, 83, 83, 83, 83, 83, 83, 83, 84, 84, 84, 84, 84, 84, 84, 84, 85, 85, 85,
    85, 85, 85, 85, 85, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
    86, 86, 86);

  DoubleToStringEstimatePowerFactorTable: array [2 .. 36]
    of TPasDblStrUtilsInt64 = (4294967296,
    // round((ln(2)/ln(Radix))*4294967296.0);
    2709822658, 2147483648, 1849741732, 1661520155, 1529898219, 1431655765,
    1354911329, 1292913986, 1241523975, 1198050829, 1160664035, 1128071163,
    1099331346, 1073741824, 1050766077, 1029986701, 1011073584, 993761859,
    977836272, 963119891, 949465783, 936750801, 924870866, 913737342, 903274219,
    893415894, 884105413, 875293062, 866935226, 858993459, 851433729, 844225782,
    837342623, 830760078);

function ConvertStringToDouble(const aStringValue: PPasDblStrUtilsChar;
  const aStringLength: TPasDblStrUtilsInt32;
  const aRoundingMode: TPasDblStrUtilsRoundingMode = rmNearest;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aBase: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsDouble;
var
  TemporaryOK: TPasDblStrUtilsBoolean;
  CountDigits: TPasDblStrUtilsInt32;
begin

  TemporaryOK := false;

  repeat

    if (aRoundingMode = rmNearest) and ((aBase < 0) or (aBase = 10)) then
    begin

      begin
        // Very fast path
        result := EiselLemireStringToDouble(aStringValue, aStringLength,
          @TemporaryOK);
        if TemporaryOK then
        begin
          break;
        end;
      end;

      begin
        // Fast path
        CountDigits := 0;
        result := RyuStringToDouble(aStringValue, aStringLength, @TemporaryOK,
          @CountDigits);
        if TemporaryOK and (CountDigits <= 17) then
        begin
          break;
        end;
      end;

    end;

    if aRoundingMode = rmNearest then
    begin

      begin
        // Slow path
        result := AlgorithmMStringToDouble(aStringValue, aStringLength,
          @TemporaryOK, aBase);
        if TemporaryOK then
        begin
          break;
        end;
      end;

    end;

    begin
      // Damn slow path
      result := FallbackStringToDouble(aStringValue, aStringLength,
        aRoundingMode, @TemporaryOK, aBase);
    end;

    break;

  until true;

  if assigned(aOK) then
  begin
    aOK^ := TemporaryOK;
  end;

end;

function ConvertStringToDouble(const aStringValue: TPasDblStrUtilsString;
  const aRoundingMode: TPasDblStrUtilsRoundingMode = rmNearest;
  const aOK: PPasDblStrUtilsBoolean = nil;
  const aBase: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsDouble;
begin
  result := ConvertStringToDouble(@aStringValue[1], length(aStringValue),
    aRoundingMode, aOK, aBase);
end;

function ConvertDoubleToString(const aValue: TPasDblStrUtilsDouble;
  const aOutputMode: TPasDblStrUtilsOutputMode = omStandard;
  aRequestedDigits: TPasDblStrUtilsInt32 = -1): TPasDblStrUtilsString;
const
  SignificantMantissaSize = 64;
  MinimalTargetExponent = -60;
  MaximalTargetExponent = -32;
  ModeShortest = 0;
  ModeFixed = 1;
  ModePrecision = 2;
  BigNumMaxSignificantMantissaBits = 3584;
  BigitChunkSize = 32;
  BigitDoubleChunkSize = 64;
  BigitSize = 28;
  BigitMask = (1 shl BigitSize) - 1;
  BigNumCapacity = (BigNumMaxSignificantMantissaBits + (BigitSize - 1))
    div BigitSize;
type
  TDoubleValue = record
    SignificantMantissa: TPasDblStrUtilsUInt64;
    Exponent: TPasDblStrUtilsInt32;
  end;

  TBigNumChunk = TPasDblStrUtilsUInt32;
  TBigNumDoubleChunk = TPasDblStrUtilsUInt64;

  TBigNum = record
    Bigits: array [0 .. BigNumCapacity] of TBigNumChunk;
    UsedDigits: TPasDblStrUtilsInt32;
    Exponent: TPasDblStrUtilsInt32;
  end;

  function QWordLessOrEqual(a, b: TPasDblStrUtilsUInt64)
    : TPasDblStrUtilsBoolean;
  begin
    result := (a = b) or (((a shr 32) < (b shr 32)) or
      (((a shr 32) = (b shr 32)) and ((a and $FFFFFFFF) < (b and $FFFFFFFF))));
  end;
  function QWordGreaterOrEqual(a, b: TPasDblStrUtilsUInt64)
    : TPasDblStrUtilsBoolean;
  begin
    result := (a = b) or (((a shr 32) > (b shr 32)) or
      (((a shr 32) = (b shr 32)) and ((a and $FFFFFFFF) > (b and $FFFFFFFF))));
  end;
  function QWordLess(a, b: TPasDblStrUtilsUInt64): TPasDblStrUtilsBoolean;
  begin
    result := ((a shr 32) < (b shr 32)) or
      (((a shr 32) = (b shr 32)) and ((a and $FFFFFFFF) < (b and $FFFFFFFF)));
  end;
  function QWordGreater(a, b: TPasDblStrUtilsUInt64): TPasDblStrUtilsBoolean;
  begin
    result := ((a shr 32) > (b shr 32)) or
      (((a shr 32) = (b shr 32)) and ((a and $FFFFFFFF) > (b and $FFFFFFFF)));
  end;
  function DoubleValue(SignificantMantissa: TPasDblStrUtilsUInt64 = 0;
    Exponent: TPasDblStrUtilsInt32 = 0): TDoubleValue;
  begin
    result.SignificantMantissa := SignificantMantissa;
    result.Exponent := Exponent;
  end;
  procedure SplitDouble(Value: TPasDblStrUtilsDouble;
    var SignificantMantissa: TPasDblStrUtilsUInt64;
    var Exponent: TPasDblStrUtilsInt32);
  var
    Casted: TPasDblStrUtilsUInt64 absolute Value;
  begin
    SignificantMantissa := Casted and TPasDblStrUtilsUInt64($000FFFFFFFFFFFFF);
    if (Casted and TPasDblStrUtilsUInt64($7FF0000000000000)) <> 0 then
    begin
      inc(SignificantMantissa, TPasDblStrUtilsUInt64($0010000000000000));
      Exponent := ((Casted and TPasDblStrUtilsUInt64($7FF0000000000000)) shr 52)
        - ($3FF + 52);
    end
    else
    begin
      Exponent := (-($3FF + 52)) + 1;
    end;
  end;
  function DoubleValueGet(Value: TPasDblStrUtilsDouble): TDoubleValue;
  var
    SignificantMantissa: TPasDblStrUtilsUInt64;
    Exponent: TPasDblStrUtilsInt32;
  begin
    Assert(Value > 0);
    SplitDouble(Value, SignificantMantissa, Exponent);
    while (SignificantMantissa and TPasDblStrUtilsUInt64
      ($0010000000000000)) = 0 do
    begin
      SignificantMantissa := SignificantMantissa shl 1;
      dec(Exponent);
    end;
    SignificantMantissa := SignificantMantissa shl
      (SignificantMantissaSize - 53);
    dec(Exponent, SignificantMantissaSize - 53);
    result.SignificantMantissa := SignificantMantissa;
    result.Exponent := Exponent;
  end;
  procedure DoubleValueSubtract(var left: TDoubleValue;
    const right: TDoubleValue);
  begin
    Assert(left.Exponent = right.Exponent);
    Assert(QWordGreaterOrEqual(left.SignificantMantissa,
      right.SignificantMantissa));
    dec(left.SignificantMantissa, right.SignificantMantissa);
  end;
  function DoubleValueMinus(const left, right: TDoubleValue): TDoubleValue;
  begin
    Assert(left.Exponent = right.Exponent);
    Assert(QWordGreaterOrEqual(left.SignificantMantissa,
      right.SignificantMantissa));
    result.Exponent := left.Exponent;
    result.SignificantMantissa := left.SignificantMantissa -
      right.SignificantMantissa;
  end;
  procedure DoubleValueMuliply(var left: TDoubleValue;
    const right: TDoubleValue);
  var
    a, b, c, d, ac, bc, ad, bd: TPasDblStrUtilsUInt64;
  begin
    a := left.SignificantMantissa shr 32;
    b := left.SignificantMantissa and $FFFFFFFF;
    c := right.SignificantMantissa shr 32;
    d := right.SignificantMantissa and $FFFFFFFF;
    ac := a * c;
    bc := b * c;
    ad := a * d;
    bd := b * d;
    inc(left.Exponent, right.Exponent + 64);
    left.SignificantMantissa := ac + (ad shr 32) + (bc shr 32) +
      (TPasDblStrUtilsUInt64(((bd shr 32) + ((ad and $FFFFFFFF) + (bc and
      $FFFFFFFF))) + (TPasDblStrUtilsUInt64(1) shl 31)) shr 32);
  end;
  function DoubleValueMul(const left, right: TDoubleValue): TDoubleValue;
  var
    a, b, c, d, ac, bc, ad, bd: TPasDblStrUtilsUInt64;
  begin
    a := left.SignificantMantissa shr 32;
    b := left.SignificantMantissa and $FFFFFFFF;
    c := right.SignificantMantissa shr 32;
    d := right.SignificantMantissa and $FFFFFFFF;
    ac := a * c;
    bc := b * c;
    ad := a * d;
    bd := b * d;
    result.Exponent := left.Exponent + (right.Exponent + 64);
    a := ((bd shr 32) + ((ad and $FFFFFFFF) + (bc and $FFFFFFFF))) +
      (TPasDblStrUtilsUInt64(1) shl 31);
    result.SignificantMantissa := ac + (ad shr 32) + (bc shr 32) + (a shr 32);
  end;
  procedure DoubleValueNormalize(var Value: TDoubleValue);
  var
    SignificantMantissa: TPasDblStrUtilsUInt64;
    Exponent: TPasDblStrUtilsInt32;
  begin
    Assert(Value.SignificantMantissa <> 0);
    SignificantMantissa := Value.SignificantMantissa;
    Exponent := Value.Exponent;
    while (SignificantMantissa and TPasDblStrUtilsUInt64
      ($FFC0000000000000)) = 0 do
    begin
      SignificantMantissa := SignificantMantissa shl 10;
      dec(Exponent, 10);
    end;
    while (SignificantMantissa and TPasDblStrUtilsUInt64
      ($8000000000000000)) = 0 do
    begin
      SignificantMantissa := SignificantMantissa shl 1;
      dec(Exponent);
    end;
    Value.SignificantMantissa := SignificantMantissa;
    Value.Exponent := Exponent;
  end;
  function DoubleValueNorm(const Value: TDoubleValue): TDoubleValue;
  var
    SignificantMantissa: TPasDblStrUtilsUInt64;
    Exponent: TPasDblStrUtilsInt32;
  begin
    Assert(Value.SignificantMantissa <> 0);
    SignificantMantissa := Value.SignificantMantissa;
    Exponent := Value.Exponent;
    while (SignificantMantissa and TPasDblStrUtilsUInt64
      ($FFC0000000000000)) = 0 do
    begin
      SignificantMantissa := SignificantMantissa shl 10;
      dec(Exponent, 10);
    end;
    while (SignificantMantissa and TPasDblStrUtilsUInt64
      ($8000000000000000)) = 0 do
    begin
      SignificantMantissa := SignificantMantissa shl 1;
      dec(Exponent);
    end;
    result.SignificantMantissa := SignificantMantissa;
    result.Exponent := Exponent;
  end;
  function BigNumNew: TBigNum;
  begin
    FillChar(result, sizeof(TBigNum), #0);
  end;
  procedure BigNumZero(var BigNum: TBigNum);
  begin
    BigNum.UsedDigits := 0;
    BigNum.Exponent := 0;
  end;
  procedure BigNumEnsureCapacity(var BigNum: TBigNum;
    Size: TPasDblStrUtilsInt32);
  begin
  end;
  procedure BigNumClamp(var BigNum: TBigNum);
  begin
    while (BigNum.UsedDigits > 0) and
      (BigNum.Bigits[BigNum.UsedDigits - 1] = 0) do
    begin
      dec(BigNum.UsedDigits);
    end;
    if BigNum.UsedDigits = 0 then
    begin
      BigNum.Exponent := 0;
    end;
  end;
  function BigNumIsClamped(const BigNum: TBigNum): TPasDblStrUtilsBoolean;
  begin
    result := (BigNum.UsedDigits = 0) or
      (BigNum.Bigits[BigNum.UsedDigits - 1] <> 0);
  end;
  procedure BigNumAlign(var BigNum: TBigNum; const Other: TBigNum);
  var
    ZeroDigits, i: TPasDblStrUtilsInt32;
  begin
    if BigNum.Exponent > Other.Exponent then
    begin
      ZeroDigits := BigNum.Exponent - Other.Exponent;
      BigNumEnsureCapacity(BigNum, BigNum.UsedDigits + ZeroDigits);
      for i := BigNum.UsedDigits - 1 downto 0 do
      begin
        BigNum.Bigits[i + ZeroDigits] := BigNum.Bigits[i];
      end;
      for i := 0 to ZeroDigits - 1 do
      begin
        BigNum.Bigits[i] := 0;
      end;
      inc(BigNum.UsedDigits, ZeroDigits);
      dec(BigNum.Exponent, ZeroDigits);
      Assert(BigNum.UsedDigits >= 0);
      Assert(BigNum.Exponent >= 0);
    end;
  end;
  procedure BigNumAssignUInt16(var BigNum: TBigNum;
    Value: TPasDblStrUtilsUInt16);
  begin
    Assert(BigitSize >= (sizeof(TPasDblStrUtilsUInt16) * 8));
    BigNumZero(BigNum);
    if Value <> 0 then
    begin
      BigNumEnsureCapacity(BigNum, 1);
      BigNum.Bigits[0] := Value;
      BigNum.UsedDigits := 1;
    end;
  end;
  procedure BigNumAssignUInt64(var BigNum: TBigNum;
    Value: TPasDblStrUtilsUInt64);
  var
    i, j: TPasDblStrUtilsInt32;
  begin
    BigNumZero(BigNum);
    if Value <> 0 then
    begin
      j := (64 div BigitSize) + 1;
      BigNumEnsureCapacity(BigNum, j);
      for i := 0 to j - 1 do
      begin
        BigNum.Bigits[i] := Value and BigitMask;
        Value := Value shr BigitSize;
      end;
      BigNum.UsedDigits := j;
      BigNumClamp(BigNum);
    end;
  end;
  procedure BigNumAssignBigNum(var BigNum: TBigNum; const Other: TBigNum);
  begin
    BigNum.Exponent := Other.Exponent;
    BigNum.Bigits := Other.Bigits;
    BigNum.UsedDigits := Other.UsedDigits;
  end;
  procedure BigNumAddBigNum(var BigNum: TBigNum; const Other: TBigNum);
  var
    Carry, Sum: TBigNumChunk;
    BigitPos, i: TPasDblStrUtilsInt32;
  begin
    Assert(BigNumIsClamped(BigNum));
    Assert(BigNumIsClamped(Other));
    BigNumAlign(BigNum, Other);
    BigNumEnsureCapacity(BigNum, BigNum.UsedDigits + Other.UsedDigits);
    BigitPos := Other.Exponent - BigNum.Exponent;
    Assert(BigitPos >= 0);
    Carry := 0;
    for i := 0 to Other.UsedDigits - 1 do
    begin
      Sum := BigNum.Bigits[BigitPos] + Other.Bigits[i] + Carry;
      BigNum.Bigits[BigitPos] := Sum and BigitMask;
      Carry := Sum shr BigitSize;
      inc(BigitPos);
    end;
    while Carry <> 0 do
    begin
      Sum := BigNum.Bigits[BigitPos] + Carry;
      BigNum.Bigits[BigitPos] := Sum and BigitMask;
      Carry := Sum shr BigitSize;
      inc(BigitPos);
    end;
    if BigNum.UsedDigits < BigitPos then
    begin
      BigNum.UsedDigits := BigitPos;
    end;
    Assert(BigNumIsClamped(BigNum));
  end;
  procedure BigNumAddUInt64(var BigNum: TBigNum;
    const Value: TPasDblStrUtilsUInt64);
  var
    Other: TBigNum;
  begin
    Other := BigNumNew;
    BigNumAssignUInt64(Other, Value);
    BigNumAddBigNum(BigNum, Other);
  end;
  function BigNumBigitAt(const BigNum: TBigNum; Index: TPasDblStrUtilsInt32)
    : TBigNumChunk;
  begin
    if (Index < BigNum.Exponent) or
      (Index >= (BigNum.UsedDigits + BigNum.Exponent)) then
    begin
      result := 0;
    end
    else
    begin
      result := BigNum.Bigits[Index - BigNum.Exponent];
    end;
  end;
  function BigNumCompare(const a, b: TBigNum): TPasDblStrUtilsInt32;
  var
    la, lb, i, j: TPasDblStrUtilsInt32;
    ba, bb: TBigNumChunk;
  begin
    Assert(BigNumIsClamped(a));
    Assert(BigNumIsClamped(b));
    la := a.UsedDigits + a.Exponent;
    lb := b.UsedDigits + b.Exponent;
    if la < lb then
    begin
      result := -1;
    end
    else if la > lb then
    begin
      result := 1;
    end
    else
    begin
      if a.Exponent < b.Exponent then
      begin
        j := a.Exponent;
      end
      else
      begin
        j := b.Exponent;
      end;
      result := 0;
      for i := la - 1 downto j do
      begin
        ba := BigNumBigitAt(a, i);
        bb := BigNumBigitAt(b, i);
        if ba < bb then
        begin
          result := -1;
          break;
        end
        else if ba > bb then
        begin
          result := 1;
          break;
        end;
      end;
    end;
  end;
  function BigNumPlusCompare(const a, b, c: TBigNum): TPasDblStrUtilsInt32;
  var
    la, lb, lc, i, j: TPasDblStrUtilsInt32;
    ba, bb, bc, br, Sum: TBigNumChunk;
  begin
    Assert(BigNumIsClamped(a));
    Assert(BigNumIsClamped(b));
    Assert(BigNumIsClamped(c));
    la := a.UsedDigits + a.Exponent;
    lb := b.UsedDigits + b.Exponent;
    lc := c.UsedDigits + c.Exponent;
    if la < lb then
    begin
      result := BigNumPlusCompare(b, a, c);
    end
    else
    begin
      if (la + 1) < lc then
      begin
        result := -1;
      end
      else if la > lc then
      begin
        result := 1;
      end
      else if (a.Exponent >= lb) and (la < lc) then
      begin
        result := -1;
      end
      else
      begin
        if a.Exponent < b.Exponent then
        begin
          if a.Exponent < c.Exponent then
          begin
            j := a.Exponent;
          end
          else
          begin
            j := c.Exponent;
          end;
        end
        else
        begin
          if b.Exponent < c.Exponent then
          begin
            j := b.Exponent;
          end
          else
          begin
            j := c.Exponent;
          end;
        end;
        br := 0;
        for i := lc - 1 downto j do
        begin
          ba := BigNumBigitAt(a, i);
          bb := BigNumBigitAt(b, i);
          bc := BigNumBigitAt(c, i);
          Sum := ba + bb;
          if Sum > (bc + br) then
          begin
            result := 1;
            exit;
          end
          else
          begin
            br := (bc + br) - Sum;
            if br > 1 then
            begin
              result := -1;
              exit;
            end;
            br := br shl BigitSize;
          end;
        end;
        if br = 0 then
        begin
          result := 0;
        end
        else
        begin
          result := -1;
        end;
      end;
    end;
  end;
  procedure BigNumSubtractBigNum(var BigNum: TBigNum; const Other: TBigNum);
  var
    Borrow, Difference: TBigNumChunk;
    i, Offset: TPasDblStrUtilsInt32;
  begin
    Assert(BigNumIsClamped(BigNum));
    Assert(BigNumIsClamped(Other));
    Assert(BigNumCompare(Other, BigNum) <= 0);
    BigNumAlign(BigNum, Other);
    Offset := Other.Exponent - BigNum.Exponent;
    Borrow := 0;
    for i := 0 to Other.UsedDigits - 1 do
    begin
      Assert((Borrow = 0) or (Borrow = 1));
      Difference := (BigNum.Bigits[i + Offset] - Other.Bigits[i]) - Borrow;
      BigNum.Bigits[i + Offset] := Difference and BigitMask;
      Borrow := Difference shr (BigitChunkSize - 1);
    end;
    i := Other.UsedDigits;
    while Borrow <> 0 do
    begin
      Difference := BigNum.Bigits[i + Offset] - Borrow;
      BigNum.Bigits[i + Offset] := Difference and BigitMask;
      Borrow := Difference shr (BigitChunkSize - 1);
      inc(i);
    end;
    BigNumClamp(BigNum);
  end;
  procedure BigNumBigitsShiftLeft(var BigNum: TBigNum;
    Shift: TPasDblStrUtilsInt32);
  var
    Carry, NextCarry: TBigNumChunk;
    i: TPasDblStrUtilsInt32;
  begin
    Assert(Shift < BigitSize);
    Assert(Shift >= 0);
    Carry := 0;
    for i := 0 to BigNum.UsedDigits - 1 do
    begin
      NextCarry := BigNum.Bigits[i] shr (BigitSize - Shift);
      BigNum.Bigits[i] := ((BigNum.Bigits[i] shl Shift) + Carry) and BigitMask;
      Carry := NextCarry;
    end;
    if Carry <> 0 then
    begin
      BigNum.Bigits[BigNum.UsedDigits] := Carry;
      inc(BigNum.UsedDigits);
    end;
  end;
  procedure BigNumBigitsShiftRight(var BigNum: TBigNum;
    Shift: TPasDblStrUtilsInt32);
  var
    Carry, NextCarry: TBigNumChunk;
    i: TPasDblStrUtilsInt32;
  begin
    Assert(Shift < BigitSize);
    Assert(Shift >= 0);
    if BigNum.UsedDigits > 0 then
    begin
      Carry := 0;
      for i := BigNum.UsedDigits - 1 downto 1 do
      begin
        NextCarry := BigNum.Bigits[i] shl (BigitSize - Shift);
        BigNum.Bigits[i] := ((BigNum.Bigits[i] shr Shift) + Carry) and
          BigitMask;
        Carry := NextCarry;
      end;
      BigNum.Bigits[0] := (BigNum.Bigits[0] shr Shift) + Carry;
    end;
    BigNumClamp(BigNum);
  end;
  procedure BignumSubtractTimes(var BigNum: TBigNum; const Other: TBigNum;
    Factor: TPasDblStrUtilsInt32);
  var
    i, ExponentDiff: TPasDblStrUtilsInt32;
    Borrow, Difference: TBigNumChunk;
    Product, Remove: TBigNumDoubleChunk;
  begin
    Assert(BigNum.Exponent <= Other.Exponent);
    if Factor < 3 then
    begin
      for i := 1 to Factor do
      begin
        BigNumSubtractBigNum(BigNum, Other);
      end;
    end
    else
    begin
      Borrow := 0;
      ExponentDiff := Other.Exponent - BigNum.Exponent;
      for i := 0 to Other.UsedDigits - 1 do
      begin
        Product := TBigNumDoubleChunk(Factor) * Other.Bigits[i];
        Remove := Borrow + Product;
        Difference := BigNum.Bigits[i + ExponentDiff] -
          TBigNumChunk(Remove and BigitMask);
        BigNum.Bigits[i + ExponentDiff] := Difference and BigitMask;
        Borrow := TBigNumChunk((Difference shr (BigitChunkSize - 1)) +
          (Remove shr BigitSize));
      end;
      for i := Other.UsedDigits + ExponentDiff to BigNum.UsedDigits - 1 do
      begin
        if Borrow = 0 then
        begin
          exit;
        end;
        Difference := BigNum.Bigits[i] - Borrow;
        BigNum.Bigits[i] := Difference and BigitMask;
        Borrow := TBigNumChunk(Difference shr (BigitChunkSize - 1));
      end;
      BigNumClamp(BigNum);
    end;
  end;
  procedure BigNumShiftLeft(var BigNum: TBigNum; Shift: TPasDblStrUtilsInt32);
  begin
    if BigNum.UsedDigits <> 0 then
    begin
      inc(BigNum.Exponent, Shift div BigitSize);
      BigNumEnsureCapacity(BigNum, BigNum.UsedDigits + 1);
      BigNumBigitsShiftLeft(BigNum, Shift mod BigitSize);
    end;
  end;
  procedure BigNumShiftRight(var BigNum: TBigNum; Shift: TPasDblStrUtilsInt32);
  begin
    if BigNum.UsedDigits <> 0 then
    begin
      dec(BigNum.Exponent, Shift div BigitSize);
      BigNumEnsureCapacity(BigNum, BigNum.UsedDigits);
      BigNumBigitsShiftRight(BigNum, Shift mod BigitSize);
    end;
  end;
  procedure BigNumMultiplyByUInt32(var BigNum: TBigNum;
    Factor: TPasDblStrUtilsUInt16);
  var
    Carry, Product: TPasDblStrUtilsUInt64;
    i: TPasDblStrUtilsInt32;
  begin
    if Factor = 0 then
    begin
      BigNumZero(BigNum);
    end
    else if Factor <> 1 then
    begin
      Assert(BigitSize < 32);
      Carry := 0;
      for i := 0 to BigNum.UsedDigits - 1 do
      begin
        Product := (Factor * BigNum.Bigits[i]) + Carry;
        BigNum.Bigits[i] := Product and BigitMask;
        Carry := Product shr BigitSize;
      end;
      while Carry <> 0 do
      begin
        BigNumEnsureCapacity(BigNum, BigNum.UsedDigits + 1);
        BigNum.Bigits[BigNum.UsedDigits] := Carry and BigitMask;
        inc(BigNum.UsedDigits);
        Carry := Carry shr BigitSize;
      end;
    end;
  end;
  procedure BigNumMultiplyByUInt64(var BigNum: TBigNum;
    Factor: TPasDblStrUtilsUInt64);
  var
    Carry, Low, High, ProductLow, ProductHigh, Tmp: TPasDblStrUtilsUInt64;
    i: TPasDblStrUtilsInt32;
  begin
    if Factor = 0 then
    begin
      BigNumZero(BigNum);
    end
    else if Factor <> 1 then
    begin
      Assert(BigitSize < 32);
      Carry := 0;
      Low := Factor and $FFFFFFFF;
      High := Factor shr 32;
      for i := 0 to BigNum.UsedDigits - 1 do
      begin
        ProductLow := Low * BigNum.Bigits[i];
        ProductHigh := High * BigNum.Bigits[i];
        Tmp := (Carry and BigitMask) + ProductLow;
        BigNum.Bigits[i] := Tmp and BigitMask;
        Carry := (Carry shr BigitSize) + (Tmp shr BigitSize) +
          (ProductHigh shl (32 - BigitSize));
      end;
      while Carry <> 0 do
      begin
        BigNumEnsureCapacity(BigNum, BigNum.UsedDigits + 1);
        BigNum.Bigits[BigNum.UsedDigits] := Carry and BigitMask;
        inc(BigNum.UsedDigits);
        Carry := Carry shr BigitSize;
      end;
    end;
  end;
  procedure BigNumSquare(var BigNum: TBigNum);
  var
    ProductLength, CopyOffset, i, BigitIndex1, BigitIndex2
      : TPasDblStrUtilsInt32;
    Accumulator: TBigNumDoubleChunk;
    Chunk1, Chunk2: TBigNumChunk;
  begin
    Assert(BigNumIsClamped(BigNum));
    ProductLength := 2 * BigNum.UsedDigits;
    BigNumEnsureCapacity(BigNum, ProductLength);
    Assert(not((1 shl (2 * (BigitChunkSize - BigitSize))) <=
      BigNum.UsedDigits));
    Accumulator := 0;
    CopyOffset := BigNum.UsedDigits;
    for i := 0 to BigNum.UsedDigits - 1 do
    begin
      BigNum.Bigits[i + CopyOffset] := BigNum.Bigits[i];
    end;
    for i := 0 to BigNum.UsedDigits - 1 do
    begin
      BigitIndex1 := i;
      BigitIndex2 := 0;
      while BigitIndex1 >= 0 do
      begin
        Chunk1 := BigNum.Bigits[CopyOffset + BigitIndex1];
        Chunk2 := BigNum.Bigits[CopyOffset + BigitIndex2];
        inc(Accumulator, TBigNumDoubleChunk(Chunk1) * Chunk2);
        dec(BigitIndex1);
        inc(BigitIndex2);
      end;
      BigNum.Bigits[i] := Accumulator and BigitMask;
      Accumulator := Accumulator shr BigitSize;
    end;
    for i := BigNum.UsedDigits - 1 to ProductLength - 1 do
    begin
      BigitIndex1 := BigNum.UsedDigits - 1;
      BigitIndex2 := i - BigitIndex1;
      while BigitIndex2 < BigNum.UsedDigits do
      begin
        Chunk1 := BigNum.Bigits[CopyOffset + BigitIndex1];
        Chunk2 := BigNum.Bigits[CopyOffset + BigitIndex2];
        inc(Accumulator, TBigNumDoubleChunk(Chunk1) * Chunk2);
        dec(BigitIndex1);
        inc(BigitIndex2);
      end;
      BigNum.Bigits[i] := Accumulator and BigitMask;
      Accumulator := Accumulator shr BigitSize;
    end;
    Assert(Accumulator = 0);
    BigNum.UsedDigits := ProductLength;
    inc(BigNum.Exponent, BigNum.Exponent);
    BigNumClamp(BigNum);
  end;
  procedure BigNumAssignPowerUInt16(var BigNum: TBigNum;
    Base: TPasDblStrUtilsUInt16; PowerExponent: TPasDblStrUtilsInt32);
  var
    Shifts, BitSize, TmpBase, FinalSize, Mask: TPasDblStrUtilsInt32;
    ThisValue: TPasDblStrUtilsUInt64;
    DelayedMultipliciation: TPasDblStrUtilsBoolean;
  begin
    Assert(Base <> 0);
    Assert(PowerExponent >= 0);
    if PowerExponent = 0 then
    begin
      BigNumAssignUInt16(BigNum, 1);
    end
    else
    begin
      BigNumZero(BigNum);
      Shifts := 0;
      while (Base and 1) = 0 do
      begin
        Base := Base shr 1;
        inc(Shifts);
      end;
      BitSize := 0;
      TmpBase := Base;
      while TmpBase <> 0 do
      begin
        TmpBase := TmpBase shr 1;
        inc(BitSize);
      end;
      FinalSize := BitSize * PowerExponent;
      BigNumEnsureCapacity(BigNum, FinalSize);
      Mask := 1;
      while Mask <= PowerExponent do
      begin
        inc(Mask, Mask);
      end;
      Mask := Mask shr 2;
      ThisValue := Base;
      DelayedMultipliciation := false;
      while (Mask <> 0) and (ThisValue <= $FFFFFFFF) do
      begin
        ThisValue := ThisValue * ThisValue;
        if (PowerExponent and Mask) <> 0 then
        begin
          if (ThisValue and not((TPasDblStrUtilsUInt64(1) shl (64 - BitSize)) -
            1)) = 0 then
          begin
            ThisValue := ThisValue * Base;
          end
          else
          begin
            DelayedMultipliciation := true;
          end;
        end;
        Mask := Mask shr 1;
      end;
      BigNumAssignUInt64(BigNum, ThisValue);
      if DelayedMultipliciation then
      begin
        BigNumMultiplyByUInt32(BigNum, Base);
      end;
      while Mask <> 0 do
      begin
        BigNumSquare(BigNum);
        if (PowerExponent and Mask) <> 0 then
        begin
          BigNumMultiplyByUInt32(BigNum, Base);
        end;
        Mask := Mask shr 1;
      end;
      BigNumShiftLeft(BigNum, Shifts * PowerExponent);
    end;
  end;
  function BigNumDivideModuloIntBigNum(var BigNum: TBigNum;
    const Other: TBigNum): TPasDblStrUtilsUInt16;
  var
    ThisBigit, OtherBigit: TBigNumChunk;
    Quotient, DivisionEstimate: TPasDblStrUtilsUInt32;
  begin
    Assert(BigNumIsClamped(BigNum));
    Assert(BigNumIsClamped(Other));
    Assert(Other.UsedDigits > 0);
    result := 0;
    if (BigNum.UsedDigits + BigNum.Exponent) >=
      (Other.UsedDigits + Other.Exponent) then
    begin
      BigNumAlign(BigNum, Other);
      while (BigNum.UsedDigits + BigNum.Exponent) >
        (Other.UsedDigits + Other.Exponent) do
      begin
        Assert(Other.Bigits[Other.UsedDigits - 1] >=
          ((1 shl BigitSize) div 16));
        inc(result, BigNum.Bigits[BigNum.UsedDigits - 1]);
        BignumSubtractTimes(BigNum, Other,
          BigNum.Bigits[BigNum.UsedDigits - 1]);
      end;
      Assert((BigNum.UsedDigits + BigNum.Exponent)
        = (Other.UsedDigits + Other.Exponent));
      ThisBigit := BigNum.Bigits[BigNum.UsedDigits - 1];
      OtherBigit := Other.Bigits[Other.UsedDigits - 1];
      if Other.UsedDigits = 1 then
      begin
        Quotient := ThisBigit div OtherBigit;
        BigNum.Bigits[BigNum.UsedDigits - 1] := ThisBigit -
          (OtherBigit * Quotient);
        inc(result, Quotient);
        BigNumClamp(BigNum);
      end
      else
      begin
        DivisionEstimate := ThisBigit div (OtherBigit + 1);
        inc(result, DivisionEstimate);
        BignumSubtractTimes(BigNum, Other, DivisionEstimate);
        if (OtherBigit * (DivisionEstimate + 1)) <= ThisBigit then
        begin
          while BigNumCompare(Other, BigNum) <= 0 do
          begin
            BigNumSubtractBigNum(BigNum, Other);
            inc(result);
          end;
        end;
      end;
    end;
  end;
  function BigNumDivideModuloInt(var BigNum: TBigNum;
    Divisor: TPasDblStrUtilsUInt16): TPasDblStrUtilsUInt16;
  var
    Q0, r0, Q1, r1: TPasDblStrUtilsUInt64;
    i: integer;
  begin
    Assert(BigNumIsClamped(BigNum));
    Q0 := 0;
    for i := BigNum.UsedDigits - 1 downto 1 do
    begin
      Q1 := (BigNum.Bigits[i] div Divisor) + Q0;
      r1 := ((BigNum.Bigits[i] mod Divisor) shl 16) +
        (BigNum.Bigits[i - 1] shr 16);
      Q0 := ((r1 div Divisor) shl 16);
      r0 := r1 mod Divisor;
      BigNum.Bigits[i] := Q1;
      BigNum.Bigits[i - 1] := (r0 shl 16) + (BigNum.Bigits[i - 1] and $FFFF);
    end;
    Q1 := (BigNum.Bigits[0] div Divisor) + Q0;
    r1 := BigNum.Bigits[0] mod Divisor;
    BigNum.Bigits[0] := Q1;
    result := r1;
    BigNumClamp(BigNum);
  end;
  function NormalizedExponent(SignificantMantissa: TPasDblStrUtilsUInt64;
    Exponent: TPasDblStrUtilsInt32): TPasDblStrUtilsInt32;
  begin
    Assert(SignificantMantissa <> 0);
    while (SignificantMantissa and TPasDblStrUtilsUInt64
      ($0010000000000000)) = 0 do
    begin
      SignificantMantissa := SignificantMantissa shl 1;
      dec(Exponent);
    end;
    result := Exponent;
  end;
  function GetEstimatePower(Exponent: TPasDblStrUtilsInt32)
    : TPasDblStrUtilsInt32;
  begin
    result := TPasDblStrUtilsInt32
      (TPasDblStrUtilsInt64(((Exponent + 52) * TPasDblStrUtilsInt64(1292913986))
      - $1000) shr 32) + 1;
    // result:=System.Trunc(Math.Ceil(((Exponent+52)*0.30102999566398114)-(1e-10)));
  end;
  function GetEstimatePowerOf(Exponent, Radix: TPasDblStrUtilsInt32)
    : TPasDblStrUtilsInt32;
  begin
    result := TPasDblStrUtilsInt32
      (TPasDblStrUtilsInt64(((Exponent + 52) *
      DoubleToStringEstimatePowerFactorTable[Radix]) - $1000) shr 32) + 1;
    // result:=System.Trunc(Math.Ceil(((Exponent+52)*(ln(2)/ln(Radix)))-(1e-10)));
  end;
  procedure GenerateShortestDigits(var Numerator, Denominator, DeltaMinus,
    DeltaPlus: TBigNum; IsEven: TPasDblStrUtilsBoolean;
    var Buffer: TPasDblStrUtilsString; var Len: TPasDblStrUtilsInt32);
  var
    Digit, Compare: TPasDblStrUtilsInt32;
    InDeltaRoomMinus, InDeltaRoomPlus: TPasDblStrUtilsBoolean;
  begin
    Len := 0;
    while true do
    begin
      Digit := BigNumDivideModuloIntBigNum(Numerator, Denominator);
      Assert((Digit >= 0) and (Digit <= 9));
      inc(Len);
      if Len >= length(Buffer) then
      begin
        SetLength(Buffer, Len * 2);
      end;
      Buffer[Len] := TPasDblStrUtilsChar
        (TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))
        + Digit));
      if IsEven then
      begin
        InDeltaRoomMinus := BigNumCompare(Numerator, DeltaMinus) <= 0;
      end
      else
      begin
        InDeltaRoomMinus := BigNumCompare(Numerator, DeltaMinus) < 0;
      end;
      if IsEven then
      begin
        InDeltaRoomPlus := BigNumPlusCompare(Numerator, DeltaPlus,
          Denominator) >= 0;
      end
      else
      begin
        InDeltaRoomPlus := BigNumPlusCompare(Numerator, DeltaPlus,
          Denominator) > 0;
      end;
      if (not InDeltaRoomMinus) and (not InDeltaRoomPlus) then
      begin
        BigNumMultiplyByUInt32(Numerator, 10);
        BigNumMultiplyByUInt32(DeltaMinus, 10);
        BigNumMultiplyByUInt32(DeltaPlus, 10);
      end
      else if InDeltaRoomMinus and InDeltaRoomPlus then
      begin
        Compare := BigNumPlusCompare(Numerator, Numerator, Denominator);
        if Compare < 0 then
        begin
        end
        else if Compare > 0 then
        begin
          Assert(Buffer[Len] <> '9');
          inc(Buffer[Len]);
        end
        else
        begin
          if ((ord(Buffer[Len]) - ord('0')) and 1) <> 0 then
          begin
            Assert(Buffer[Len] <> '9');
            inc(Buffer[Len]);
          end;
        end;
        exit;
      end
      else if InDeltaRoomMinus then
      begin
        exit;
      end
      else
      begin
        Assert(Buffer[Len] <> '9');
        inc(Buffer[Len]);
        exit;
      end;
    end;
  end;
  procedure GenerateCountedDigits(Count: TPasDblStrUtilsInt32;
    var DecimalPoint: TPasDblStrUtilsInt32; var Numerator, Denominator: TBigNum;
    var Buffer: TPasDblStrUtilsString; var Len: TPasDblStrUtilsInt32);
  var
    i, Digit: TPasDblStrUtilsInt32;
  begin
    Assert(Count >= 0);
    for i := 1 to Count - 1 do
    begin
      Digit := BigNumDivideModuloIntBigNum(Numerator, Denominator);
      Assert((Digit >= 0) and (Digit <= 9));
      inc(Len);
      if Len >= length(Buffer) then
      begin
        SetLength(Buffer, Len * 2);
      end;
      Buffer[Len] := TPasDblStrUtilsChar
        (TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))
        + Digit));
      BigNumMultiplyByUInt32(Numerator, 10);
    end;
    Digit := BigNumDivideModuloIntBigNum(Numerator, Denominator);
    if BigNumPlusCompare(Numerator, Numerator, Denominator) >= 0 then
    begin
      inc(Digit);
    end;
    inc(Len);
    if Len >= length(Buffer) then
    begin
      SetLength(Buffer, Len * 2);
    end;
    Buffer[Len] := TPasDblStrUtilsChar
      (TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))
      + Digit));
    for i := Len downto 2 do
    begin
      if ord(Buffer[i]) <> (ord('0') + 10) then
      begin
        break;
      end;
      Buffer[i] := '0';
      inc(Buffer[i - 1]);
    end;
    if ord(Buffer[1]) = (ord('0') + 10) then
    begin
      Buffer[1] := '1';
      inc(DecimalPoint);
    end;
  end;
  procedure GenerateFixedDigits(RequestedDigits: TPasDblStrUtilsInt32;
    var DecimalPoint: TPasDblStrUtilsInt32; var Numerator, Denominator: TBigNum;
    var Buffer: TPasDblStrUtilsString; var Len: TPasDblStrUtilsInt32);
  begin
    if (-DecimalPoint) > RequestedDigits then
    begin
      DecimalPoint := -RequestedDigits;
      Len := 0;
    end
    else if (-DecimalPoint) = RequestedDigits then
    begin
      Assert(DecimalPoint = (-RequestedDigits));
      BigNumMultiplyByUInt32(Denominator, 10);
      if BigNumPlusCompare(Numerator, Numerator, Denominator) >= 0 then
      begin
        Buffer := '1';
        Len := 1;
      end
      else
      begin
        Len := 0;
      end;
    end
    else
    begin
      GenerateCountedDigits(DecimalPoint + RequestedDigits, DecimalPoint,
        Numerator, Denominator, Buffer, Len);
    end;
  end;
  procedure FixupMultiplyBase(EstimatedPower: TPasDblStrUtilsInt32;
    IsEven: TPasDblStrUtilsBoolean; var DecimalPoint: TPasDblStrUtilsInt32;
    var Numerator, Denominator, DeltaMinus, DeltaPlus: TBigNum;
    Base: TPasDblStrUtilsInt32);
  var
    InRange: TPasDblStrUtilsBoolean;
  begin
    if IsEven then
    begin
      InRange := BigNumPlusCompare(Numerator, DeltaPlus, Denominator) >= 0;
    end
    else
    begin
      InRange := BigNumPlusCompare(Numerator, DeltaPlus, Denominator) > 0;
    end;
    if InRange then
    begin
      DecimalPoint := EstimatedPower + 1;
    end
    else
    begin
      DecimalPoint := EstimatedPower;
      BigNumMultiplyByUInt32(Numerator, Base);
      if BigNumCompare(DeltaMinus, DeltaPlus) = 0 then
      begin
        BigNumMultiplyByUInt32(DeltaMinus, Base);
        BigNumAssignBigNum(DeltaPlus, DeltaMinus);
      end
      else
      begin
        BigNumMultiplyByUInt32(DeltaMinus, Base);
        BigNumMultiplyByUInt32(DeltaPlus, Base);
      end;
    end;
  end;
  procedure InitialScaledStartValuesPositiveExponent(Casted, SignificantMantissa
    : TPasDblStrUtilsUInt64; Exponent: TPasDblStrUtilsInt32;
    EstimatedPower: TPasDblStrUtilsInt32;
    NeedBoundaryDeltas: TPasDblStrUtilsBoolean;
    var Numerator, Denominator, DeltaMinus, DeltaPlus: TBigNum;
    Base: TPasDblStrUtilsInt32);
  begin
    Assert(EstimatedPower >= 0);

    BigNumAssignUInt64(Numerator, SignificantMantissa);
    BigNumShiftLeft(Numerator, Exponent);
    BigNumAssignPowerUInt16(Denominator, Base, EstimatedPower);

    if NeedBoundaryDeltas then
    begin
      BigNumShiftLeft(Numerator, 1);
      BigNumShiftLeft(Denominator, 1);

      BigNumAssignUInt16(DeltaPlus, 1);
      BigNumShiftLeft(DeltaPlus, Exponent);

      BigNumAssignUInt16(DeltaMinus, 1);
      BigNumShiftLeft(DeltaMinus, Exponent);

      if (Casted and TPasDblStrUtilsUInt64($000FFFFFFFFFFFFF)) = 0 then
      begin
        BigNumShiftLeft(Numerator, 1);
        BigNumShiftLeft(Denominator, 1);
        BigNumShiftLeft(DeltaPlus, 1);
      end;
    end;
  end;
  procedure InitialScaledStartValuesNegativeExponentPositivePower(Casted,
    SignificantMantissa: TPasDblStrUtilsUInt64; Exponent: TPasDblStrUtilsInt32;
    EstimatedPower: TPasDblStrUtilsInt32;
    NeedBoundaryDeltas: TPasDblStrUtilsBoolean;
    var Numerator, Denominator, DeltaMinus, DeltaPlus: TBigNum;
    Base: TPasDblStrUtilsInt32);
  begin
    BigNumAssignUInt64(Numerator, SignificantMantissa);
    BigNumAssignPowerUInt16(Denominator, Base, EstimatedPower);
    BigNumShiftLeft(Denominator, -Exponent);

    if NeedBoundaryDeltas then
    begin
      BigNumShiftLeft(Numerator, 1);
      BigNumShiftLeft(Denominator, 1);

      BigNumAssignUInt16(DeltaPlus, 1);
      BigNumAssignUInt16(DeltaMinus, 1);

      if (Casted and TPasDblStrUtilsUInt64($000FFFFFFFFFFFFF)) = 0 then
      begin
        BigNumShiftLeft(Numerator, 1);
        BigNumShiftLeft(Denominator, 1);
        BigNumShiftLeft(DeltaPlus, 1);
      end;
    end;
  end;
  procedure InitialScaledStartValuesNegativeExponentNegativePower(Casted,
    SignificantMantissa: TPasDblStrUtilsUInt64; Exponent: TPasDblStrUtilsInt32;
    EstimatedPower: TPasDblStrUtilsInt32;
    NeedBoundaryDeltas: TPasDblStrUtilsBoolean;
    var Numerator, Denominator, DeltaMinus, DeltaPlus: TBigNum;
    Base: TPasDblStrUtilsInt32);
  begin
    BigNumAssignPowerUInt16(Numerator, Base, -EstimatedPower);
    if NeedBoundaryDeltas then
    begin
      BigNumAssignBigNum(DeltaPlus, Numerator);
      BigNumAssignBigNum(DeltaMinus, Numerator);
    end;
    BigNumMultiplyByUInt64(Numerator, SignificantMantissa);

    BigNumAssignUInt16(Denominator, 1);
    BigNumShiftLeft(Denominator, -Exponent);

    if NeedBoundaryDeltas then
    begin
      BigNumShiftLeft(Numerator, 1);
      BigNumShiftLeft(Denominator, 1);
      if ((Casted and TPasDblStrUtilsUInt64($000FFFFFFFFFFFFF)) = 0) and
        ((Casted and TPasDblStrUtilsUInt64($7FF0000000000000)) <>
        TPasDblStrUtilsUInt64($0010000000000000)) then
      begin
        BigNumShiftLeft(Numerator, 1);
        BigNumShiftLeft(Denominator, 1);
        BigNumShiftLeft(DeltaPlus, 1);
      end;
    end;
  end;
  procedure InitialScaledStartValues(Casted, SignificantMantissa
    : TPasDblStrUtilsUInt64; Exponent: TPasDblStrUtilsInt32;
    EstimatedPower: TPasDblStrUtilsInt32;
    NeedBoundaryDeltas: TPasDblStrUtilsBoolean;
    var Numerator, Denominator, DeltaMinus, DeltaPlus: TBigNum;
    Base: TPasDblStrUtilsInt32);
  begin
    if Exponent >= 0 then
    begin
      InitialScaledStartValuesPositiveExponent(Casted, SignificantMantissa,
        Exponent, EstimatedPower, NeedBoundaryDeltas, Numerator, Denominator,
        DeltaMinus, DeltaPlus, Base);
    end
    else if EstimatedPower >= 0 then
    begin
      InitialScaledStartValuesNegativeExponentPositivePower(Casted,
        SignificantMantissa, Exponent, EstimatedPower, NeedBoundaryDeltas,
        Numerator, Denominator, DeltaMinus, DeltaPlus, Base);
    end
    else
    begin
      InitialScaledStartValuesNegativeExponentNegativePower(Casted,
        SignificantMantissa, Exponent, EstimatedPower, NeedBoundaryDeltas,
        Numerator, Denominator, DeltaMinus, DeltaPlus, Base);
    end;
  end;
  procedure DoubleToDecimal(Value: TPasDblStrUtilsDouble;
    Mode, RequestedDigits: TPasDblStrUtilsInt32;
    var Buffer: TPasDblStrUtilsString;
    var Len, DecimalPoint: TPasDblStrUtilsInt32);
  var
    Casted: TPasDblStrUtilsUInt64 absolute Value;
    SignificantMantissa: TPasDblStrUtilsUInt64;
    Exponent, EstimatedPower: TPasDblStrUtilsInt32;
    Numerator, Denominator, DeltaMinus, DeltaPlus: TBigNum;
    IsEven, NeedBoundaryDeltas: TPasDblStrUtilsBoolean;
  begin
    Assert(Value > 0);
    Assert(IsFinite(Value));
    SplitDouble(Value, SignificantMantissa, Exponent);
    IsEven := (SignificantMantissa and 1) = 0;
    EstimatedPower := GetEstimatePower(NormalizedExponent(SignificantMantissa,
      Exponent));
    if (Mode = ModeFixed) and (((-EstimatedPower) - 1) > RequestedDigits) then
    begin
      Buffer := '';
      Len := 0;
      DecimalPoint := -RequestedDigits;
    end
    else
    begin
      Assert(BigNumMaxSignificantMantissaBits >= (324 * 4));
      NeedBoundaryDeltas := Mode = ModeShortest;
      InitialScaledStartValues(Casted, SignificantMantissa, Exponent,
        EstimatedPower, NeedBoundaryDeltas, Numerator, Denominator, DeltaMinus,
        DeltaPlus, 10);
      FixupMultiplyBase(EstimatedPower, IsEven, DecimalPoint, Numerator,
        Denominator, DeltaMinus, DeltaPlus, 10);
      case Mode of
        ModeShortest:
          begin
            GenerateShortestDigits(Numerator, Denominator, DeltaMinus,
              DeltaPlus, IsEven, Buffer, Len);
          end;
        ModeFixed:
          begin
            GenerateFixedDigits(RequestedDigits, DecimalPoint, Numerator,
              Denominator, Buffer, Len);
          end;
      else { ModePrecision: }
        begin
          GenerateCountedDigits(RequestedDigits, DecimalPoint, Numerator,
            Denominator, Buffer, Len);
        end;
      end;
    end;
  end;
  procedure GenerateRadixDigits(var Numerator, Denominator, DeltaMinus,
    DeltaPlus: TBigNum; IsEven: TPasDblStrUtilsBoolean;
    var Buffer: TPasDblStrUtilsString; var Len: TPasDblStrUtilsInt32;
    Radix: TPasDblStrUtilsInt32);
  const
    Base36: array [0 .. 36] of TPasDblStrUtilsChar =
      '0123456789abcdefghijklmnopqrstuvwxyz{';
  var
    Digit, Compare, MaxDigit: TPasDblStrUtilsInt32;
    InDeltaRoomMinus, InDeltaRoomPlus: TPasDblStrUtilsBoolean;
    function ValueOf(c: TPasDblStrUtilsChar): TPasDblStrUtilsInt32;
    begin
      case c of
        '0' .. '9':
          begin
            result := ord(c) - ord('0');
          end;
      else
        begin
          result := (ord(c) - ord('a')) + $A;
        end;
      end;
    end;

  begin
    Len := 0;
    MaxDigit := Radix - 1;
    while true do
    begin
      Digit := BigNumDivideModuloIntBigNum(Numerator, Denominator);
      Assert((Digit >= 0) and (Digit <= MaxDigit));
      inc(Len);
      if Len >= length(Buffer) then
      begin
        SetLength(Buffer, Len * 2);
      end;
      Buffer[Len] := Base36[Digit];
      BigNumClamp(Numerator);
      BigNumClamp(DeltaMinus);
      BigNumClamp(DeltaPlus);
      if IsEven then
      begin
        InDeltaRoomMinus := BigNumCompare(Numerator, DeltaMinus) <= 0;
      end
      else
      begin
        InDeltaRoomMinus := BigNumCompare(Numerator, DeltaMinus) < 0;
      end;
      if IsEven then
      begin
        InDeltaRoomPlus := BigNumPlusCompare(Numerator, DeltaPlus,
          Denominator) >= 0;
      end
      else
      begin
        InDeltaRoomPlus := BigNumPlusCompare(Numerator, DeltaPlus,
          Denominator) > 0;
      end;
      if (not InDeltaRoomMinus) and (not InDeltaRoomPlus) then
      begin
        BigNumMultiplyByUInt32(Numerator, Radix);
        BigNumMultiplyByUInt32(DeltaMinus, Radix);
        BigNumMultiplyByUInt32(DeltaPlus, Radix);
      end
      else if InDeltaRoomMinus and InDeltaRoomPlus then
      begin
        Compare := BigNumPlusCompare(Numerator, Numerator, Denominator);
        if Compare < 0 then
        begin
        end
        else if Compare > 0 then
        begin
          Assert(ValueOf(Buffer[Len]) <> MaxDigit);
          Buffer[Len] := Base36[ValueOf(Buffer[Len]) + 1];
        end
        else
        begin
          if (ValueOf(Buffer[Len]) and 1) <> 0 then
          begin
            Assert(ValueOf(Buffer[Len]) <> MaxDigit);
            Buffer[Len] := Base36[ValueOf(Buffer[Len]) + 1];
          end;
        end;
        exit;
      end
      else if InDeltaRoomMinus then
      begin
        exit;
      end
      else
      begin
        Assert(ValueOf(Buffer[Len]) <> MaxDigit);
        Buffer[Len] := Base36[ValueOf(Buffer[Len]) + 1];
        exit;
      end;
    end;
  end;
  procedure DoubleToRadix(Value: TPasDblStrUtilsDouble;
    Radix: TPasDblStrUtilsInt32; var Buffer: TPasDblStrUtilsString;
    var Len, DecimalPoint: TPasDblStrUtilsInt32);
  var
    Casted: TPasDblStrUtilsUInt64 absolute Value;
    SignificantMantissa: TPasDblStrUtilsUInt64;
    Exponent, EstimatedPower: TPasDblStrUtilsInt32;
    Numerator, Denominator, DeltaMinus, DeltaPlus: TBigNum;
    IsEven, NeedBoundaryDeltas: TPasDblStrUtilsBoolean;
  begin
    Assert(Value > 0);
    Assert(IsFinite(Value));
    SplitDouble(Value, SignificantMantissa, Exponent);
    IsEven := (SignificantMantissa and 1) = 0;
    EstimatedPower := GetEstimatePowerOf(NormalizedExponent(SignificantMantissa,
      Exponent), Radix);
    Assert(BigNumMaxSignificantMantissaBits >= (324 * 4));
    NeedBoundaryDeltas := true;
    InitialScaledStartValues(Casted, SignificantMantissa, Exponent,
      EstimatedPower, NeedBoundaryDeltas, Numerator, Denominator, DeltaMinus,
      DeltaPlus, Radix);
    FixupMultiplyBase(EstimatedPower, IsEven, DecimalPoint, Numerator,
      Denominator, DeltaMinus, DeltaPlus, Radix);
    GenerateRadixDigits(Numerator, Denominator, DeltaMinus, DeltaPlus, IsEven,
      Buffer, Len, Radix);
  end;
{$WARNINGS off}
  procedure FastDoubleToRadix(v: TPasDblStrUtilsDouble;
    Radix: TPasDblStrUtilsInt32; var Buffer: TPasDblStrUtilsString;
    var Len, DecimalPoint: TPasDblStrUtilsInt32);
  const
    Base36: array [0 .. 35] of TPasDblStrUtilsChar =
      '0123456789abcdefghijklmnopqrstuvwxyz';
    DtoAFPUExceptionMask: TFPUExceptionMask = [exInvalidOp, exDenormalized,
      exZeroDivide, exOverflow, exUnderflow, exPrecision];
    DtoAFPUPrecisionMode: TFPUPrecisionMode = pmDouble;
    DtoAFPURoundingMode: TFPURoundingMode = rmNearest;
  var
    IntPart, FracPart, Old, Epsilon: TPasDblStrUtilsDouble;
    Digit, i, j: TPasDblStrUtilsInt32;
    TempBuffer: TPasDblStrUtilsString;
    OldFPUExceptionMask: TFPUExceptionMask;
    OldFPUPrecisionMode: TFPUPrecisionMode;
    OldFPURoundingMode: TFPURoundingMode;
    IntPart64: TPasDblStrUtilsInt64;
  begin
    if (Radix < 2) or (Radix > 36) then
    begin
      result := '';
    end
    else
    begin
      OldFPUExceptionMask := GetExceptionMask;
      OldFPUPrecisionMode := GetPrecisionMode;
      OldFPURoundingMode := GetRoundMode;
      try
        if OldFPUExceptionMask <> DtoAFPUExceptionMask then
        begin
          SetExceptionMask(DtoAFPUExceptionMask);
        end;
        if OldFPUPrecisionMode <> DtoAFPUPrecisionMode then
        begin
          SetPrecisionMode(DtoAFPUPrecisionMode);
        end;
        if OldFPURoundingMode <> DtoAFPURoundingMode then
        begin
          SetRoundMode(DtoAFPURoundingMode);
        end;
        try
          TempBuffer := '';
          IntPart := System.Int(v);
          FracPart := System.Frac(v);
          if IntPart = 0 then
          begin
            result := '0';
          end
          else
          begin
            if IntPart < 4294967295.0 then
            begin
              IntPart64 := trunc(IntPart);
              while IntPart64 > 0 do
              begin
                Digit := IntPart64 mod Radix;
                Assert((Digit >= 0) and (Digit < Radix));
                IntPart64 := IntPart64 div Radix;
                inc(Len);
                if Len >= length(TempBuffer) then
                begin
                  SetLength(TempBuffer, Len * 2);
                end;
                TempBuffer[Len] := Base36[Digit];
              end;
            end
            else
            begin
              while IntPart > 0 do
              begin
                Old := IntPart;
                IntPart := System.Int(IntPart / Radix);
                Digit := trunc(Old - (IntPart * Radix));
                Assert((Digit >= 0) and (Digit < Radix));
                inc(Len);
                if Len >= length(TempBuffer) then
                begin
                  SetLength(TempBuffer, Len * 2);
                end;
                TempBuffer[Len] := Base36[Digit];
              end;
            end;
            SetLength(Buffer, Len);
            j := 1;
            for i := Len downto 1 do
            begin
              Buffer[j] := TempBuffer[i];
              inc(j);
            end;
          end;
          if FracPart <> 0 then
          begin
            inc(Len);
            if Len >= length(Buffer) then
            begin
              SetLength(Buffer, Len * 2);
            end;
            Buffer[Len] := '.';
            Epsilon := 0.001 / Radix;
            while (FracPart >= Epsilon) and (Len < 32) do
            begin
              FracPart := FracPart * Radix;
              Digit := trunc(FracPart);
              FracPart := System.Frac(FracPart);
              Assert((Digit >= 0) and (Digit < Radix));
              inc(Len);
              if Len >= length(Buffer) then
              begin
                SetLength(Buffer, Len * 2);
              end;
              Buffer[Len] := Base36[Digit];
            end;
          end;
        finally
          TempBuffer := '';
        end;
      finally
        if OldFPUExceptionMask <> DtoAFPUExceptionMask then
        begin
          SetExceptionMask(OldFPUExceptionMask);
        end;
        if OldFPUPrecisionMode <> DtoAFPUPrecisionMode then
        begin
          SetPrecisionMode(OldFPUPrecisionMode);
        end;
        if OldFPURoundingMode <> DtoAFPURoundingMode then
        begin
          SetRoundMode(OldFPURoundingMode);
        end;
      end;
    end;
  end;
{$WARNINGS on}
  function GetCachedPowerForBinaryExponentRange(MinExponent,
    MaxExponent: TPasDblStrUtilsInt32; var Power: TDoubleValue;
    var DecimalExponent: TPasDblStrUtilsInt32): TPasDblStrUtilsBoolean;
  var
    Index: TPasDblStrUtilsInt32;
  begin
    result := false;
    if (low(DoubleToStringPowerOfTenBinaryExponentTable) <= MinExponent) and
      (MinExponent <= high(DoubleToStringPowerOfTenBinaryExponentTable)) then
    begin
      Index := DoubleToStringPowerOfTenBinaryExponentTable[MinExponent];
      if ((Index >= 0) and (Index < length(DoubleToStringPowerOfTenTable))) and
        ((MinExponent <= DoubleToStringPowerOfTenTable[Index, 1]) and
        (DoubleToStringPowerOfTenTable[Index, 1] <= MaxExponent)) then
      begin
        Power.SignificantMantissa := DoubleToStringPowerOfTenTable[Index, 0];
        Power.Exponent := DoubleToStringPowerOfTenTable[Index, 1];
        DecimalExponent := DoubleToStringPowerOfTenTable[Index, 2];
        result := true;
      end;
    end;
  end;
  function GetCachedPowerForDecimalExponent(RequestedExponent
    : TPasDblStrUtilsInt32; var Power: TDoubleValue;
    var FoundExponent: TPasDblStrUtilsInt32): TPasDblStrUtilsBoolean;
  var
    Index: TPasDblStrUtilsInt32;
  begin
    result := false;
    if (low(DoubleToStringPowerOfTenDecimalExponentTable) <= RequestedExponent)
      and (RequestedExponent <=
      high(DoubleToStringPowerOfTenDecimalExponentTable)) then
    begin
      Index := DoubleToStringPowerOfTenDecimalExponentTable[RequestedExponent];
      if (Index >= 0) and (Index < length(DoubleToStringPowerOfTenTable)) then
      begin
        Power.SignificantMantissa := DoubleToStringPowerOfTenTable[Index, 0];
        Power.Exponent := DoubleToStringPowerOfTenTable[Index, 1];
        FoundExponent := DoubleToStringPowerOfTenTable[Index, 2];
        result := true;
      end;
    end;
  end;
  function RoundWeed(var Buffer: TPasDblStrUtilsString;
    Len: TPasDblStrUtilsInt32; DistanceTooHighW, UnsafeInterval, Rest,
    TenCapacity, UnitValue: TPasDblStrUtilsUInt64): TPasDblStrUtilsBoolean;
  var
    SmallDistance, BigDistance: TPasDblStrUtilsUInt64;
  begin
    SmallDistance := DistanceTooHighW - UnitValue;
    BigDistance := DistanceTooHighW + UnitValue;
    Assert(QWordLessOrEqual(Rest, UnsafeInterval));
    while (QWordLess(Rest, SmallDistance) and
      (QWordGreaterOrEqual(UnsafeInterval - Rest, TenCapacity))) and
      (QWordLess(Rest + TenCapacity, SmallDistance) or
      QWordGreaterOrEqual(SmallDistance - Rest,
      ((Rest + TenCapacity) - SmallDistance))) do
    begin
      dec(Buffer[Len]);
      inc(Rest, TenCapacity);
    end;
    if ((QWordLess(Rest, BigDistance) and QWordGreaterOrEqual(UnsafeInterval -
      Rest, TenCapacity)) and (QWordLess(Rest + TenCapacity, BigDistance) or
      QWordGreater(BigDistance - Rest, ((Rest + TenCapacity) - BigDistance))))
    then
    begin
      result := false;
    end
    else
    begin
      result := (QWordLessOrEqual(2 * UnitValue, Rest) and
        QWordLessOrEqual(Rest, UnsafeInterval - (4 * UnitValue)));
    end;
  end;
  function RoundWeedCounted(var Buffer: TPasDblStrUtilsString;
    Len: TPasDblStrUtilsInt32; Rest, TenCapacity,
    UnitValue: TPasDblStrUtilsUInt64; var Capacity: TPasDblStrUtilsInt32)
    : TPasDblStrUtilsBoolean;
  var
    i: TPasDblStrUtilsInt32;
  begin
    Assert(QWordLess(Rest, TenCapacity));
    result := false;
    if QWordGreater(TenCapacity - UnitValue, UnitValue) then
    begin
      result := QWordGreater(TenCapacity - Rest, Rest) and
        QWordGreaterOrEqual(TenCapacity - (2 * Rest), 2 * UnitValue);
      if not result then
      begin
        result := QWordGreater(Rest, UnitValue) and
          QWordLessOrEqual(TenCapacity - (Rest - UnitValue), Rest - UnitValue);
        if result then
        begin
          inc(Buffer[Len]);
          for i := Len downto 2 do
          begin
            if ord(Buffer[i]) <> (ord('0') + 10) then
            begin
              break;
            end;
            Buffer[i] := '0';
            inc(Buffer[i - 1]);
          end;
        end;
        if ord(Buffer[1]) = (ord('0') + 10) then
        begin
          Buffer[1] := '1';
          inc(Capacity);
        end;
      end;
    end;
  end;
  function BiggestPowerTen(Number: TPasDblStrUtilsUInt32;
    NumberBits: TPasDblStrUtilsInt32; var Power: TPasDblStrUtilsUInt32;
    var Exponent: TPasDblStrUtilsInt32): TPasDblStrUtilsBoolean;
  label c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11;
  begin
    result := true;
    case NumberBits of
      30, 31, 32:
        begin
        c1:
          if 1000000000 <= Number then
          begin
            Power := 1000000000;
            Exponent := 9;
          end
          else
          begin
            goto c2;
          end;
        end;
      27, 28, 29:
        begin
        c2:
          if 100000000 <= Number then
          begin
            Power := 100000000;
            Exponent := 8;
          end
          else
          begin
            goto c3;
          end;
        end;
      24, 25, 26:
        begin
        c3:
          if 10000000 <= Number then
          begin
            Power := 10000000;
            Exponent := 7;
          end
          else
          begin
            goto c4;
          end;
        end;
      20, 21, 22, 23:
        begin
        c4:
          if 1000000 <= Number then
          begin
            Power := 1000000;
            Exponent := 6;
          end
          else
          begin
            goto c5;
          end;
        end;
      17, 18, 19:
        begin
        c5:
          if 100000 <= Number then
          begin
            Power := 100000;
            Exponent := 5;
          end
          else
          begin
            goto c6;
          end;
        end;
      14, 15, 16:
        begin
        c6:
          if 10000 <= Number then
          begin
            Power := 10000;
            Exponent := 4;
          end
          else
          begin
            goto c7;
          end;
        end;
      10, 11, 12, 13:
        begin
        c7:
          if 1000 <= Number then
          begin
            Power := 1000;
            Exponent := 3;
          end
          else
          begin
            goto c8;
          end;
        end;
      7, 8, 9:
        begin
        c8:
          if 100 <= Number then
          begin
            Power := 100;
            Exponent := 2;
          end
          else
          begin
            goto c9;
          end;
        end;
      4, 5, 6:
        begin
        c9:
          if 10 <= Number then
          begin
            Power := 10;
            Exponent := 1;
          end
          else
          begin
            goto c10;
          end;
        end;
      1, 2, 3:
        begin
        c10:
          if 1 <= Number then
          begin
            Power := 1;
            Exponent := 0;
          end
          else
          begin
            goto c11;
          end;
        end;
      0:
        begin
        c11:
          Power := 0;
          Exponent := -1;
        end;
    else
      begin
        Power := 0;
        Exponent := 0;
        result := false;
      end;
    end;
  end;
  function DigitGen(Low, w, High: TDoubleValue;
    var Buffer: TPasDblStrUtilsString; var Len, Capacity: TPasDblStrUtilsInt32)
    : TPasDblStrUtilsBoolean;
  var
    UnitValue, Fractionals, Rest: TPasDblStrUtilsUInt64;
    TooLow, TooHigh, UnsafeInterval, One: TDoubleValue;
    Integrals, Divisor, Digit: TPasDblStrUtilsUInt32;
    DivisorExponent: TPasDblStrUtilsInt32;
  begin
    result := false;
    if ((Low.Exponent = w.Exponent) and (w.Exponent = High.Exponent)) and
      (QWordLessOrEqual(Low.SignificantMantissa + 1, High.SignificantMantissa -
      1) and ((MinimalTargetExponent <= w.Exponent) and
      (w.Exponent <= MaximalTargetExponent))) then
    begin
      UnitValue := 1;
      TooLow.SignificantMantissa := Low.SignificantMantissa - UnitValue;
      TooLow.Exponent := Low.Exponent;
      TooHigh.SignificantMantissa := High.SignificantMantissa + UnitValue;
      TooHigh.Exponent := High.Exponent;
      UnsafeInterval := DoubleValueMinus(TooHigh, TooLow);
      One.SignificantMantissa := TPasDblStrUtilsUInt64(1) shl (-w.Exponent);
      One.Exponent := w.Exponent;
      Integrals := TooHigh.SignificantMantissa shr (-One.Exponent);
      Fractionals := TooHigh.SignificantMantissa and
        (One.SignificantMantissa - 1);
      Divisor := 0;
      DivisorExponent := 0;
      if BiggestPowerTen(Integrals, SignificantMantissaSize - (-One.Exponent),
        Divisor, DivisorExponent) then
      begin
        Capacity := DivisorExponent + 1;
        Len := 0;
        while Capacity > 0 do
        begin
          Digit := Integrals div Divisor;
          Integrals := Integrals mod Divisor;
          inc(Len);
          if Len >= length(Buffer) then
          begin
            SetLength(Buffer, Len * 2);
          end;
          Buffer[Len] := TPasDblStrUtilsChar
            (TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))
            + Digit));
          dec(Capacity);
          Rest := TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(Integrals)
            shl (-One.Exponent)) + Fractionals;
          if QWordLess(Rest, UnsafeInterval.SignificantMantissa) then
          begin
            result := RoundWeed(Buffer, Len, DoubleValueMinus(TooHigh, w)
              .SignificantMantissa, UnsafeInterval.SignificantMantissa, Rest,
              TPasDblStrUtilsUInt64(Divisor) shl (-One.Exponent), UnitValue);
            exit;
          end;
          Divisor := Divisor div 10;
        end;
        if (One.Exponent >= -60) and
          (QWordLess(Fractionals, One.SignificantMantissa) and
          QWordGreaterOrEqual(TPasDblStrUtilsUInt64($1999999999999999),
          One.SignificantMantissa)) then
        begin
          while true do
          begin
            Fractionals := Fractionals * 10;
            UnitValue := UnitValue * 10;
            UnsafeInterval.SignificantMantissa :=
              UnsafeInterval.SignificantMantissa * 10;
            Digit := Fractionals shr (-One.Exponent);
            inc(Len);
            if Len >= length(Buffer) then
            begin
              SetLength(Buffer, Len * 2);
            end;
            Buffer[Len] := TPasDblStrUtilsChar
              (TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar
              ('0')) + Digit));
            dec(Capacity);
            Fractionals := Fractionals and (One.SignificantMantissa - 1);
            if QWordLess(Fractionals, UnsafeInterval.SignificantMantissa) then
            begin
              result := RoundWeed(Buffer, Len, DoubleValueMinus(TooHigh, w)
                .SignificantMantissa * UnitValue,
                UnsafeInterval.SignificantMantissa, Fractionals,
                One.SignificantMantissa, UnitValue);
              exit;
            end;
          end;
        end;
      end;
    end;
  end;
  function DigitGenCounted(w: TDoubleValue;
    RequestedDigits: TPasDblStrUtilsInt32; var Buffer: TPasDblStrUtilsString;
    var Len, Capacity: TPasDblStrUtilsInt32): TPasDblStrUtilsBoolean;
  var
    wError, Fractionals, Rest: TPasDblStrUtilsUInt64;
    One: TDoubleValue;
    Integrals, Divisor, Digit: TPasDblStrUtilsUInt32;
    DivisorExponent: TPasDblStrUtilsInt32;
  begin
    result := false;
    if ((MinimalTargetExponent <= w.Exponent) and
      (w.Exponent <= MaximalTargetExponent)) and
      ((MinimalTargetExponent >= -60) and (MaximalTargetExponent <= -32)) then
    begin
      wError := 1;
      One.SignificantMantissa := TPasDblStrUtilsUInt64(1) shl (-w.Exponent);
      One.Exponent := w.Exponent;
      Integrals := w.SignificantMantissa shr (-One.Exponent);
      Fractionals := w.SignificantMantissa and (One.SignificantMantissa - 1);
      Divisor := 0;
      DivisorExponent := 0;
      if BiggestPowerTen(Integrals, SignificantMantissaSize - (-One.Exponent),
        Divisor, DivisorExponent) then
      begin
        Capacity := DivisorExponent + 1;
        Len := 0;
        while Capacity > 0 do
        begin
          Digit := Integrals div Divisor;
          Integrals := Integrals mod Divisor;
          inc(Len);
          if Len >= length(Buffer) then
          begin
            SetLength(Buffer, Len * 2);
          end;
          Buffer[Len] := TPasDblStrUtilsChar
            (TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))
            + Digit));
          dec(RequestedDigits);
          dec(Capacity);
          if RequestedDigits = 0 then
          begin
            break;
          end;
          Divisor := Divisor div 10;
        end;
        if RequestedDigits = 0 then
        begin
          Rest := TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(Integrals)
            shl (-One.Exponent)) + Fractionals;
          result := RoundWeedCounted(Buffer, Len, Rest,
            TPasDblStrUtilsUInt64(Divisor) shl (-One.Exponent), wError,
            Capacity);
          exit;
        end;
        if ((One.Exponent >= -60) and QWordLess(Fractionals,
          One.SignificantMantissa)) and
          QWordGreaterOrEqual(TPasDblStrUtilsUInt64($1999999999999999),
          One.SignificantMantissa) then
        begin
          while (RequestedDigits > 0) and (Fractionals > wError) do
          begin
            Fractionals := Fractionals * 10;
            Digit := Fractionals shr (-One.Exponent);
            inc(Len);
            if Len >= length(Buffer) then
            begin
              SetLength(Buffer, Len * 2);
            end;
            Buffer[Len] := TPasDblStrUtilsChar
              (TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar
              ('0')) + Digit));
            dec(RequestedDigits);
            dec(Capacity);
            Fractionals := Fractionals and (One.SignificantMantissa - 1);
          end;
          if RequestedDigits = 0 then
          begin
            result := RoundWeedCounted(Buffer, Len, Fractionals,
              One.SignificantMantissa, wError, Capacity);
          end
          else
          begin
            result := false;
          end;
        end;
      end;
    end;
  end;
  procedure NormalizedBoundaries(Value: TPasDblStrUtilsDouble;
    var BoundaryMinus, BoundaryPlus: TDoubleValue);
  var
    v: TDoubleValue;
    SignificantMantissaIsZero: TPasDblStrUtilsBoolean;
  begin
    Assert(not IsNegative(Value));
    Assert(IsFinite(Value));
    SplitDouble(Value, v.SignificantMantissa, v.Exponent);
    SignificantMantissaIsZero := v.SignificantMantissa = TPasDblStrUtilsUInt64
      ($0010000000000000);
    BoundaryPlus.SignificantMantissa := (v.SignificantMantissa shl 1) + 1;
    BoundaryPlus.Exponent := v.Exponent - 1;
    DoubleValueNormalize(BoundaryPlus);
    if SignificantMantissaIsZero and (v.Exponent <> ((-($3FF + 52)) + 1)) then
    begin
      BoundaryMinus.SignificantMantissa := (v.SignificantMantissa shl 2) - 1;
      BoundaryMinus.Exponent := v.Exponent - 2;
    end
    else
    begin
      BoundaryMinus.SignificantMantissa := (v.SignificantMantissa shl 1) - 1;
      BoundaryMinus.Exponent := v.Exponent - 1;
    end;
    BoundaryMinus.SignificantMantissa := BoundaryMinus.SignificantMantissa shl
      (BoundaryMinus.Exponent - BoundaryPlus.Exponent);
    BoundaryMinus.Exponent := BoundaryPlus.Exponent;
  end;
  function DoFastShortest(Value: TPasDblStrUtilsDouble;
    var Buffer: TPasDblStrUtilsString;
    var Len, DecimalExponent: TPasDblStrUtilsInt32): TPasDblStrUtilsBoolean;
  var
    w, BoundaryMinus, BoundaryPlus, TenMK, ScaledW, ScaledBoundaryMinus,
      ScaledBoundaryPlus: TDoubleValue;
    mK, TenMKMinimalBinaryExponent, TenMKMaximalBinaryExponent,
      Capacity: TPasDblStrUtilsInt32;
  begin
    result := false;
    w := DoubleValueGet(Value);
    NormalizedBoundaries(Value, BoundaryMinus, BoundaryPlus);
    Assert(BoundaryPlus.Exponent = w.Exponent);
    TenMKMinimalBinaryExponent := MinimalTargetExponent -
      (w.Exponent + SignificantMantissaSize);
    TenMKMaximalBinaryExponent := MaximalTargetExponent -
      (w.Exponent + SignificantMantissaSize);
    if GetCachedPowerForBinaryExponentRange(TenMKMinimalBinaryExponent,
      TenMKMaximalBinaryExponent, TenMK, mK) then
    begin
      if (MinimalTargetExponent <= (w.Exponent + TenMK.Exponent +
        SignificantMantissaSize)) and
        (MaximalTargetExponent >= (w.Exponent + TenMK.Exponent +
        SignificantMantissaSize)) then
      begin
        ScaledW := DoubleValueMul(w, TenMK);
        if ScaledW.Exponent = (BoundaryPlus.Exponent + TenMK.Exponent +
          SignificantMantissaSize) then
        begin
          ScaledBoundaryMinus := DoubleValueMul(BoundaryMinus, TenMK);
          ScaledBoundaryPlus := DoubleValueMul(BoundaryPlus, TenMK);
          Capacity := 0;
          result := DigitGen(ScaledBoundaryMinus, ScaledW, ScaledBoundaryPlus,
            Buffer, Len, Capacity);
          DecimalExponent := Capacity - mK;
        end;
      end;
    end;
  end;
  function DoFastPrecision(Value: TPasDblStrUtilsDouble;
    RequestedDigits: TPasDblStrUtilsInt32; var Buffer: TPasDblStrUtilsString;
    var Len, DecimalExponent: TPasDblStrUtilsInt32): TPasDblStrUtilsBoolean;
  var
    w, TenMK, ScaledW: TDoubleValue;
    mK, TenMKMinimalBinaryExponent, TenMKMaximalBinaryExponent,
      Capacity: TPasDblStrUtilsInt32;
  begin
    result := false;
    w := DoubleValueGet(Value);
    TenMKMinimalBinaryExponent := MinimalTargetExponent -
      (w.Exponent + SignificantMantissaSize);
    TenMKMaximalBinaryExponent := MaximalTargetExponent -
      (w.Exponent + SignificantMantissaSize);
    if GetCachedPowerForBinaryExponentRange(TenMKMinimalBinaryExponent,
      TenMKMaximalBinaryExponent, TenMK, mK) then
    begin
      if (MinimalTargetExponent <= (w.Exponent + TenMK.Exponent +
        SignificantMantissaSize)) and
        (MaximalTargetExponent >= (w.Exponent + TenMK.Exponent +
        SignificantMantissaSize)) then
      begin
        ScaledW := DoubleValueMul(w, TenMK);
        Capacity := 0;
        result := DigitGenCounted(ScaledW, RequestedDigits, Buffer, Len,
          Capacity);
        DecimalExponent := Capacity - mK;
      end;
    end;
  end;
  function DoFastFixed(Value: TPasDblStrUtilsDouble;
    FracitionalCount: TPasDblStrUtilsInt32; var Buffer: TPasDblStrUtilsString;
    var Len, DecimalPoint: TPasDblStrUtilsInt32): TPasDblStrUtilsBoolean;
  const
    Five17 = $B1A2BC2EC5; // 5^17
  type
    TInt128 = record
      High, Low: TPasDblStrUtilsUInt64;
    end;

    procedure Int128Mul(var a: TInt128;
      const Multiplicand: TPasDblStrUtilsUInt32);
    var
      Accumulator: TPasDblStrUtilsUInt64;
      Part: TPasDblStrUtilsUInt32;
    begin
      Accumulator := (a.Low and $FFFFFFFF) * Multiplicand;
      Part := Accumulator and $FFFFFFFF;
      Accumulator := (Accumulator shr 32) + ((a.Low shr 32) * Multiplicand);
      a.Low := (Accumulator shl 32) + Part;
      Accumulator := (Accumulator shr 32) +
        ((a.High and $FFFFFFFF) * Multiplicand);
      Part := Accumulator and $FFFFFFFF;
      Accumulator := (Accumulator shr 32) + ((a.High shr 32) * Multiplicand);
      a.High := (Accumulator shl 32) + Part;
      Assert((Accumulator shr 32) = 0);
    end;
    procedure Int128Shift(var a: TInt128; const Shift: TPasDblStrUtilsInt32);
    begin
      Assert(((-64) <= Shift) and (Shift <= 64));
      if Shift <> 0 then
      begin
        if Shift = -64 then
        begin
          a.High := a.Low;
          a.Low := 0;
        end
        else if Shift = 64 then
        begin
          a.Low := a.High;
          a.High := 0;
        end
        else if Shift <= 0 then
        begin
          a.High := (a.High shl (-Shift)) + (a.Low shr (64 + Shift));
          a.Low := a.Low shl (-Shift);
        end
        else
        begin
          a.Low := (a.Low shr Shift) + (a.High shl (64 - Shift));
          a.High := a.High shr Shift;
        end;
      end;
    end;
    function Int128DivModPowerOfTwo(var a: TInt128;
      const Power: TPasDblStrUtilsInt32): TPasDblStrUtilsInt32;
    begin
      if Power >= 64 then
      begin
        result := a.High shr (Power - 64);
        dec(a.High, result shl (Power - 64));
      end
      else
      begin
        result := (a.Low shr Power) + (a.High shl (64 - Power));
        a.High := 0;
        dec(a.Low, (a.Low shr Power) shl Power);
      end;
    end;
    function Int128IsZero(const a: TInt128): TPasDblStrUtilsBoolean;
    begin
      result := (a.High = 0) and (a.Low = 0);
    end;
    function Int128BitAt(const a: TInt128; const Position: TPasDblStrUtilsInt32)
      : TPasDblStrUtilsBoolean;
    begin
      if Position >= 64 then
      begin
        result := ((a.High shr (Position - 64)) and 1) <> 0;
      end
      else
      begin
        result := ((a.Low shr Position) and 1) <> 0;
      end;
    end;
    procedure FillDigits32FixedLength(Number: TPasDblStrUtilsUInt32;
      RequestedLength: TPasDblStrUtilsInt32; var Buffer: TPasDblStrUtilsString;
      var Len: TPasDblStrUtilsInt32);
    var
      i, l: TPasDblStrUtilsInt32;
    begin
      l := Len;
      inc(Len, RequestedLength);
      if Len >= length(Buffer) then
      begin
        SetLength(Buffer, Len * 2);
      end;
      for i := RequestedLength downto 1 do
      begin
        Buffer[l + i] := TPasDblStrUtilsChar
          (TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0')) +
          (Number mod 10)));
        Number := Number div 10;
      end;
    end;
    procedure FillDigits32(Number: TPasDblStrUtilsUInt32;
      var Buffer: TPasDblStrUtilsString; var Len: TPasDblStrUtilsInt32);
    var
      NumberLength, i, l: TPasDblStrUtilsInt32;
      OldNumber: TPasDblStrUtilsUInt32;
    begin
      OldNumber := Number;
      NumberLength := 0;
      while Number <> 0 do
      begin
        Number := Number div 10;
        inc(NumberLength);
      end;
      if NumberLength <> 0 then
      begin
        l := Len;
        inc(Len, NumberLength);
        if Len >= length(Buffer) then
        begin
          SetLength(Buffer, Len * 2);
        end;
        Number := OldNumber;
        for i := NumberLength downto 1 do
        begin
          Buffer[l + i] := TPasDblStrUtilsChar
            (TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))
            + (Number mod 10)));
          Number := Number div 10;
        end;
      end;
    end;
    procedure FillDigits64FixedLength(Number: TPasDblStrUtilsUInt64;
      RequestedLength: TPasDblStrUtilsInt32; var Buffer: TPasDblStrUtilsString;
      var Len: TPasDblStrUtilsInt32);
    var
      p0, p1, p2: TPasDblStrUtilsUInt32;
    begin
      p2 := Number mod 10000000;
      Number := Number div 10000000;
      p1 := Number mod 10000000;
      p0 := Number div 10000000;
      FillDigits32FixedLength(p0, 3, Buffer, Len);
      FillDigits32FixedLength(p1, 7, Buffer, Len);
      FillDigits32FixedLength(p2, 7, Buffer, Len);
    end;
    procedure FillDigits64(Number: TPasDblStrUtilsUInt64;
      var Buffer: TPasDblStrUtilsString; var Len: TPasDblStrUtilsInt32);
    var
      p0, p1, p2: TPasDblStrUtilsUInt32;
    begin
      p2 := Number mod 10000000;
      Number := Number div 10000000;
      p1 := Number mod 10000000;
      p0 := Number div 10000000;
      if p0 <> 0 then
      begin
        FillDigits32(p0, Buffer, Len);
        FillDigits32FixedLength(p1, 7, Buffer, Len);
        FillDigits32FixedLength(p2, 7, Buffer, Len);
      end
      else if p1 <> 0 then
      begin
        FillDigits32(p1, Buffer, Len);
        FillDigits32FixedLength(p2, 7, Buffer, Len);
      end
      else
      begin
        FillDigits32(p2, Buffer, Len);
      end;
    end;
    procedure RoundUp(var Buffer: TPasDblStrUtilsString;
      var Len, DecimalPoint: TPasDblStrUtilsInt32);
    var
      i: TPasDblStrUtilsInt32;
    begin
      if Len = 0 then
      begin
        Buffer := '1';
        Len := 1;
        DecimalPoint := 1;
      end
      else
      begin
        inc(Buffer[Len]);
        for i := Len downto 2 do
        begin
          if ord(Buffer[i]) <> (ord('0') + 10) then
          begin
            exit;
          end;
          Buffer[i] := '0';
          inc(Buffer[i - 1]);
        end;
        if ord(Buffer[1]) = (ord('0') + 10) then
        begin
          Buffer[1] := '1';
          inc(DecimalPoint);
        end;
      end;
    end;
    procedure FillFractionals(Fractionals: TPasDblStrUtilsUInt64;
      Exponent: TPasDblStrUtilsInt32; FractionalCount: TPasDblStrUtilsInt32;
      var Buffer: TPasDblStrUtilsString;
      var Len, DecimalPoint: TPasDblStrUtilsInt32);
    var
      Point, i, Digit: TPasDblStrUtilsInt32;
      Fractionals128: TInt128;
    begin
      Assert(((-128) <= Exponent) and (Exponent <= 0));
      if (-Exponent) <= 64 then
      begin
        Assert((Fractionals shr 56) = 0);
        Point := -Exponent;
        for i := 1 to FracitionalCount do
        begin
          Fractionals := Fractionals * 5;
          dec(Point);
          Digit := Fractionals shr Point;
          inc(Len);
          if Len >= length(Buffer) then
          begin
            SetLength(Buffer, Len * 2);
          end;
          Buffer[Len] := TPasDblStrUtilsChar
            (TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))
            + Digit));
          dec(Fractionals, TPasDblStrUtilsUInt64(Digit) shl Point);
        end;
        if ((Fractionals shr (Point - 1)) and 1) <> 0 then
        begin
          RoundUp(Buffer, Len, DecimalPoint);
        end;
      end
      else
      begin
        Assert((64 < (-Exponent)) and ((-Exponent) <= 128));
        Fractionals128.High := Fractionals;
        Fractionals128.Low := 0;
        Int128Shift(Fractionals128, (-Exponent) - 64);
        Point := 128;
        for i := 1 to FracitionalCount do
        begin
          if Int128IsZero(Fractionals128) then
          begin
            break;
          end;
          Int128Mul(Fractionals128, 5);
          dec(Point);
          Digit := Int128DivModPowerOfTwo(Fractionals128, Point);
          inc(Len);
          if Len >= length(Buffer) then
          begin
            SetLength(Buffer, Len * 2);
          end;
          Buffer[Len] := TPasDblStrUtilsChar
            (TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))
            + Digit));
        end;
        if Int128BitAt(Fractionals128, Point - 1) then
        begin
          RoundUp(Buffer, Len, DecimalPoint);
        end;
      end;
    end;
    procedure TrimZeros(var Buffer: TPasDblStrUtilsString;
      var Len, DecimalPoint: TPasDblStrUtilsInt32);
    var
      i: TPasDblStrUtilsInt32;
    begin
      while (Len > 0) and (Buffer[Len] = '0') do
      begin
        dec(Len);
      end;
      i := 0;
      while (i < Len) and (Buffer[i + 1] = '0') do
      begin
        inc(i);
      end;
      if i <> 0 then
      begin
        Delete(Buffer, 1, i);
        dec(Len, i);
        dec(DecimalPoint, i);
      end;
    end;

  var
    SignificantMantissa, Divisor, Dividend, Remainder, Integrals,
      Fractionals: TPasDblStrUtilsUInt64;
    Exponent, DivisorPower: TPasDblStrUtilsInt32;
    Quotient: TPasDblStrUtilsUInt32;
  begin
    result := false;
    SplitDouble(Value, SignificantMantissa, Exponent);
    if (Exponent <= 20) and (FracitionalCount <= 20) then
    begin
      Len := 0;
      if (Exponent + 53) > 74 then
      begin
        Divisor := Five17;
        DivisorPower := 17;
        Dividend := SignificantMantissa;
        if Exponent > DivisorPower then
        begin
          Dividend := Dividend shl (Exponent - DivisorPower);
          Quotient := Dividend div Divisor;
          Remainder := (Dividend mod Divisor) shl DivisorPower;
        end
        else
        begin
          Dividend := Dividend shl (DivisorPower - Exponent);
          Quotient := Dividend div Divisor;
          Remainder := (Dividend mod Divisor) shl Exponent;
        end;
        FillDigits32(Quotient, Buffer, Len);
        FillDigits64FixedLength(Remainder, DivisorPower, Buffer, Len);
        DecimalPoint := Len;
      end
      else if Exponent >= 0 then
      begin
        SignificantMantissa := SignificantMantissa shl Exponent;
        FillDigits64(SignificantMantissa, Buffer, Len);
        DecimalPoint := Len;
      end
      else if Exponent > -53 then
      begin
        Integrals := SignificantMantissa shr (-Exponent);
        Fractionals := SignificantMantissa - (Integrals shl (-Exponent));
        if Integrals > $FFFFFFFF then
        begin
          FillDigits64(Integrals, Buffer, Len);
        end
        else
        begin
          FillDigits32(Integrals, Buffer, Len);
        end;
        DecimalPoint := Len;
        FillFractionals(Fractionals, Exponent, FracitionalCount, Buffer, Len,
          DecimalPoint);
      end
      else if Exponent < -128 then
      begin
        Assert(FracitionalCount >= 20);
        Buffer := '';
        Len := 0;
        DecimalPoint := -FracitionalCount;
      end
      else
      begin
        DecimalPoint := 0;
        FillFractionals(SignificantMantissa, Exponent, FracitionalCount, Buffer,
          Len, DecimalPoint);
      end;
      TrimZeros(Buffer, Len, DecimalPoint);
      SetLength(Buffer, Len);
      if Len = 0 then
      begin
        DecimalPoint := -FracitionalCount;
      end;
      result := true;
    end;
  end;

var
  OK, Fast: TPasDblStrUtilsBoolean;
  Len, DecimalPoint, ZeroPrefixLength, ZeroPostfixLength,
    i: TPasDblStrUtilsInt32;
  LocalOutputMode: TPasDblStrUtilsOutputMode;
begin
  if IsNaN(aValue) then
  begin
    if IsNegative(aValue) then
    begin
      result := '-NaN';
    end
    else
    begin
      result := 'NaN';
    end;
  end
  else if IsZero(aValue) then
  begin
    result := '0';
  end
  else if IsNegInfinite(aValue) then
  begin
    result := '-Infinity';
  end
  else if IsInfinite(aValue) then
  begin
    result := 'Infinity';
  end
  else if IsNegative(aValue) then
  begin
    result := '-' + ConvertDoubleToString(DoubleAbsolute(aValue), aOutputMode,
      aRequestedDigits);
  end
  else
  begin
    result := '0';
    if aValue <> 0 then
    begin
      Len := 0;
      DecimalPoint := 0;
      OK := false;
      Fast := false;
      if ((aOutputMode = omFixed) and (aValue >= 1E21)) or
        ((aOutputMode = omRadix) and (aRequestedDigits = 10)) then
      begin
        LocalOutputMode := omStandard;
      end
      else
      begin
        LocalOutputMode := aOutputMode;
      end;
      case LocalOutputMode of
        omStandard:
          begin
            if aRequestedDigits < 0 then
            begin
              result := RyuDoubleToString(aValue, false);
              OK := true;
            end;
          end;
        omStandardExponential:
          begin
            if aRequestedDigits < 0 then
            begin
              result := RyuDoubleToString(aValue, true);
              OK := true;
            end;
          end;
      else
        begin
        end;
      end;
      if not OK then
      begin
        case LocalOutputMode of
          omStandard, omStandardExponential:
            begin
              OK := DoFastShortest(aValue, result, Len, DecimalPoint);
              inc(DecimalPoint, Len);
            end;
          omFixed:
            begin
              OK := DoFastFixed(aValue, aRequestedDigits, result, Len,
                DecimalPoint);
            end;
          omExponential, omPrecision:
            begin
              if aRequestedDigits <= 0 then
              begin
                OK := DoFastShortest(aValue, result, Len, DecimalPoint);
                inc(DecimalPoint, Len);
                aRequestedDigits := Len - 1;
              end
              else
              begin
                OK := DoFastPrecision(aValue, aRequestedDigits, result, Len,
                  DecimalPoint);
                inc(DecimalPoint, Len);
              end;
              Assert((Len > 0) and (Len <= (aRequestedDigits + 1)));
            end;
          omRadix:
            begin
              if ((aRequestedDigits >= 2) and (aRequestedDigits <= 36)) and
                (IsFinite(aValue) and (aValue < 4294967295.0) and
                (System.Int(aValue) = aValue)) then
              begin
                FastDoubleToRadix(aValue, aRequestedDigits, result, Len,
                  DecimalPoint);
                Fast := true;
                OK := true;
              end;
            end;
        end;
        if not OK then
        begin
          case LocalOutputMode of
            omStandard, omStandardExponential:
              begin
                DoubleToDecimal(aValue, ModeShortest, aRequestedDigits, result,
                  Len, DecimalPoint);
                OK := true;
              end;
            omFixed:
              begin
                DoubleToDecimal(aValue, ModeFixed, aRequestedDigits, result,
                  Len, DecimalPoint);
                OK := true;
              end;
            omExponential, omPrecision:
              begin
                if aRequestedDigits <= 0 then
                begin
                  DoubleToDecimal(aValue, ModeShortest, aRequestedDigits,
                    result, Len, DecimalPoint);
                  OK := true;
                  aRequestedDigits := Len - 1;
                end
                else
                begin
                  DoubleToDecimal(aValue, ModePrecision, aRequestedDigits,
                    result, Len, DecimalPoint);
                  OK := true;
                end;
                Assert((Len > 0) and (Len <= (aRequestedDigits + 1)));
              end;
            omRadix:
              begin
                if (aRequestedDigits >= 2) and (aRequestedDigits <= 36) then
                begin
                  DoubleToRadix(aValue, aRequestedDigits, result, Len,
                    DecimalPoint);
                  OK := true;
                end;
              end;
          end;
        end;
        if OK then
        begin
          SetLength(result, Len);
          case LocalOutputMode of
            omStandard:
              begin
                if (Len <= DecimalPoint) and (DecimalPoint <= 21) then
                begin
                  SetLength(result, DecimalPoint);
                  FillChar(result[Len + 1], DecimalPoint - Len, '0');
                end
                else if (0 < DecimalPoint) and (DecimalPoint <= 21) then
                begin
                  Insert('.', result, DecimalPoint + 1);
                end
                else if (DecimalPoint <= 0) and (DecimalPoint > -6) then
                begin
                  for i := 1 to -DecimalPoint do
                  begin
                    result := '0' + result;
                  end;
                  result := '0.' + result;
                end
                else
                begin
                  if Len <> 1 then
                  begin
                    Insert('.', result, 2);
                  end;
                  if DecimalPoint >= 0 then
                  begin
                    result := result + 'e+' + TPasDblStrUtilsString
                      (IntToStr(abs(DecimalPoint - 1)));
                  end
                  else
                  begin
                    result := result + 'e-' + TPasDblStrUtilsString
                      (IntToStr(abs(DecimalPoint - 1)));
                  end;
                end;
              end;
            omStandardExponential:
              begin
                if Len <> 1 then
                begin
                  Insert('.', result, 2);
                end;
                if DecimalPoint >= 0 then
                begin
                  result := result + 'e+' + TPasDblStrUtilsString
                    (IntToStr(abs(DecimalPoint - 1)));
                end
                else
                begin
                  result := result + 'e-' + TPasDblStrUtilsString
                    (IntToStr(abs(DecimalPoint - 1)));
                end;
              end;
            omFixed:
              begin
                ZeroPrefixLength := 0;
                ZeroPostfixLength := 0;
                if DecimalPoint <= 0 then
                begin
                  ZeroPrefixLength := (-DecimalPoint) + 1;
                  DecimalPoint := 1;
                end;
                if (ZeroPrefixLength + Len) < (DecimalPoint + aRequestedDigits)
                then
                begin
                  ZeroPostfixLength := ((DecimalPoint + aRequestedDigits) - Len)
                    - ZeroPrefixLength;
                end;
                for i := 1 to ZeroPrefixLength do
                begin
                  result := '0' + result;
                end;
                for i := 1 to ZeroPostfixLength do
                begin
                  result := result + '0';
                end;
                if (aRequestedDigits > 0) and (DecimalPoint > 0) and
                  (DecimalPoint <= length(result)) then
                begin
                  Insert('.', result, DecimalPoint + 1);
                end;
              end;
            omExponential:
              begin
                if aRequestedDigits < 1 then
                begin
                  aRequestedDigits := 1;
                end;
                if aRequestedDigits <> 1 then
                begin
                  Insert('.', result, 2);
                  for i := Len + 1 to aRequestedDigits do
                  begin
                    result := result + '0';
                  end;
                end
                else
                begin
                  SetLength(result, 1);
                end;
                if DecimalPoint >= 0 then
                begin
                  result := result + 'e+' + TPasDblStrUtilsString
                    (IntToStr(abs(DecimalPoint - 1)));
                end
                else
                begin
                  result := result + 'e-' + TPasDblStrUtilsString
                    (IntToStr(abs(DecimalPoint - 1)));
                end;
              end;
            omPrecision:
              begin
                if aRequestedDigits < 1 then
                begin
                  aRequestedDigits := 1;
                end;
                if (DecimalPoint < -6) or (DecimalPoint >= aRequestedDigits)
                then
                begin
                  if aRequestedDigits <> 1 then
                  begin
                    Insert('.', result, 2);
                    for i := Len + 1 to aRequestedDigits do
                    begin
                      result := result + '0';
                    end;
                  end
                  else
                  begin
                    SetLength(result, 1);
                  end;
                  if DecimalPoint >= 0 then
                  begin
                    result := result + 'e+' + TPasDblStrUtilsString
                      (IntToStr(abs(DecimalPoint - 1)));
                  end
                  else
                  begin
                    result := result + 'e-' + TPasDblStrUtilsString
                      (IntToStr(abs(DecimalPoint - 1)));
                  end;
                end
                else
                begin
                  if DecimalPoint <= 0 then
                  begin
                    for i := 1 to -DecimalPoint do
                    begin
                      result := '0' + result;
                    end;
                    result := '0.' + result;
                    for i := Len + 1 to aRequestedDigits do
                    begin
                      result := result + '0';
                    end;
                  end
                  else
                  begin
                    SetLength(result, aRequestedDigits);
                    for i := Len + 1 to aRequestedDigits do
                    begin
                      result[i] := '0';
                    end;
                    if DecimalPoint < aRequestedDigits then
                    begin
                      if Len <> 1 then
                      begin
                        Insert('.', result, DecimalPoint + 1);
                      end;
                    end;
                  end;
                end;
              end;
            omRadix:
              begin
                if not Fast then
                begin
                  if (Len <= DecimalPoint) and (DecimalPoint <= 21) then
                  begin
                    SetLength(result, DecimalPoint);
                    FillChar(result[Len + 1], DecimalPoint - Len, '0');
                  end
                  else if (0 < DecimalPoint) and (DecimalPoint <= 21) then
                  begin
                    Insert('.', result, DecimalPoint + 1);
                  end
                  else if (DecimalPoint <= 0) and (DecimalPoint > -6) then
                  begin
                    for i := 1 to -DecimalPoint do
                    begin
                      result := '0' + result;
                    end;
                    result := '0.' + result;
                  end
                  else
                  begin
                    if Len <> 1 then
                    begin
                      Insert('.', result, 2);
                    end;
                    if DecimalPoint >= 0 then
                    begin
                      result := result + 'p+' + TPasDblStrUtilsString
                        (IntToStr(abs(DecimalPoint - 1)));
                    end
                    else
                    begin
                      result := result + 'p-' + TPasDblStrUtilsString
                        (IntToStr(abs(DecimalPoint - 1)));
                    end;
                  end;
                  while (length(result) > 1) and
                    ((result[1] = '0') and (result[2] in ['0' .. '9',
                    'a' .. 'f'])) do
                  begin
                    Delete(result, 1, 1);
                  end;
                end;
              end;
          end;
        end
        else
        begin
          result := '';
        end;
      end;
    end;
  end;
end;

initialization

finalization

end.
