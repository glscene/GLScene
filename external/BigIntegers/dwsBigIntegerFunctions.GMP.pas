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
unit dwsBigIntegerFunctions.GMP;

{$I dws.inc}

interface

uses
   Classes, SysUtils,
   dwsXPlatform, dwsUtils, dwsStrings, dwsCompilerContext,
   dwsFunctions, dwsSymbols, dwsExprs, dwsCoreExprs, dwsExprList, dwsUnitSymbols,
   dwsConstExprs, dwsMagicExprs, dwsDataContext, dwsErrors, dwsRelExprs,
   dwsOperators, dwsTokenTypes, dwsCryptoXPlatform, dwsScriptSource,
   dwsMPIR;

const
   SYS_BIGINTEGER = 'BigInteger';

type

   TBaseBigIntegerSymbol = class (TBaseSymbol)
      public
         constructor Create;

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         procedure InitData(const data : TData; offset : Integer); override;
   end;

   IdwsBigInteger = interface
      ['{93A7FA32-DE99-44AB-A5B4-861FD50E9AAB}']
      function GetValue : pmpz_t;
      procedure SetValue(const v : pmpz_t);
      property Value : pmpz_t read GetValue write SetValue;

      function BitLength : Integer;
      function PopCount : Integer;
      function Sign : Integer;

      function ToStringBase(base : Integer) : String;
      function ToHexString : String;

      function ToInt64 : Int64;

      function ToNeg : IdwsBigInteger;
   end;

   TdwsBigIntegerWrapperPool = class;

   TBigIntegerWrapper = class (TInterfacedObject, IdwsBigInteger, IGetSelf)
      private
         FNext : TBigIntegerWrapper;

      protected
         function _Release: Integer; stdcall;

         function GetValue : pmpz_t; inline;
         procedure SetValue(const v : pmpz_t); inline;
         function GetSelf : TObject;

         constructor CreateNewZero;
         procedure Reset;

      public
         Value : mpz_t;

         class function CreateZero : TBigIntegerWrapper; static;
         class function CreateInt64(const i : Int64) : TBigIntegerWrapper; static;
         class function CreateFloat(const f : Double) : TBigIntegerWrapper; static;
         class function CreateString(const s : String; base : Integer) : TBigIntegerWrapper; static;
         destructor Destroy; override;

         function BitLength : Integer;
         function PopCount : Integer;
         function Sign : Integer;

         function ToStringBase(base : Integer) : String;
         function ToHexString : String;
         function ToString : String; override;

         function ToInt64 : Int64;

         function ToNeg : IdwsBigInteger;
   end;

   TdwsBigIntegerWrapperPool = class
      private
         FLock : TMultiReadSingleWrite;
         FHead : TBigIntegerWrapper;
         FSize : Integer;

      public
         constructor Create;
         destructor Destroy; override;

         function Pop : TBigIntegerWrapper; inline;
         procedure Push(ref : TBigIntegerWrapper); inline;
         procedure Cleanup;
   end;

   TBigIntegerNegateExpr = class(TUnaryOpExpr)
      constructor Create(context : TdwsBaseSymbolsContext; const aScriptPos : TScriptPos; expr : TTypedExpr); override;
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;

   TMPIRBinOpFunc = procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;

   TBigIntegerBinOpExpr = class(TBinaryOpExpr)
      constructor Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                         const anOp : TTokenType; aLeft, aRight : TTypedExpr); override;
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;

   TBigIntegerBinOpFuncExpr = class(TBigIntegerBinOpExpr)
      protected
         FOpFunc : TMPIRBinOpFunc;
         procedure InitOpFunc; virtual; abstract;

      public
         constructor Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                            const anOp : TTokenType; aLeft, aRight : TTypedExpr); override;

         procedure EvalAsInterface(exec : TdwsExecution; var result : IUnknown); override; final;
   end;

   TBigIntegerAddOpExpr = class(TBigIntegerBinOpFuncExpr)
      procedure InitOpFunc; override;
   end;
   TBigIntegerSubOpExpr = class(TBigIntegerBinOpFuncExpr)
      procedure InitOpFunc; override;
   end;
   TBigIntegerMultOpExpr = class(TBigIntegerBinOpFuncExpr)
      procedure InitOpFunc; override;
   end;
   TBigIntegerDivOpExpr = class(TBigIntegerBinOpFuncExpr)
      procedure InitOpFunc; override;
   end;
   TBigIntegerModOpExpr = class(TBigIntegerBinOpFuncExpr)
      procedure InitOpFunc; override;
   end;

   TBigIntegerAndOpExpr = class(TBigIntegerBinOpFuncExpr)
      procedure InitOpFunc; override;
   end;
   TBigIntegerOrOpExpr = class(TBigIntegerBinOpFuncExpr)
      procedure InitOpFunc; override;
   end;
   TBigIntegerXorOpExpr = class(TBigIntegerBinOpFuncExpr)
      procedure InitOpFunc; override;
   end;

   TBigIntegerShiftLeftExpr = class(TBigIntegerBinOpExpr)
      procedure EvalAsInterface(exec : TdwsExecution; var result : IUnknown); override;
   end;
   TBigIntegerShiftRightExpr = class(TBigIntegerBinOpExpr)
      procedure EvalAsInterface(exec : TdwsExecution; var result : IUnknown); override;
   end;

   TBigIntegerOpAssignExpr = class(TOpAssignExpr)
     procedure TypeCheckAssign(context : TdwsCompilerContext); override;
   end;

   TBigIntegerPlusAssignExpr = class(TBigIntegerOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBigIntegerPlusAssignIntExpr = class(TBigIntegerPlusAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBigIntegerMinusAssignExpr = class(TBigIntegerOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBigIntegerMinusAssignIntExpr = class(TBigIntegerMinusAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBigIntegerMultAssignExpr = class(TBigIntegerOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TBigIntegerRelOpExpr = class(TBoolRelOpExpr)
      protected
        function InternalCompare(exec : TdwsExecution) : Integer;
   end;
   TBigIntegerRelOpExprClass = class of TBigIntegerRelOpExpr;

   TBigIntegerEqualOpExpr = class(TBigIntegerRelOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TBigIntegerNotEqualOpExpr = class(TBigIntegerRelOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TBigIntegerGreaterOpExpr = class(TBigIntegerRelOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TBigIntegerGreaterEqualOpExpr = class(TBigIntegerRelOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TBigIntegerLessOpExpr = class(TBigIntegerRelOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TBigIntegerLessEqualOpExpr = class(TBigIntegerRelOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TBigIntegerUnaryOpExpr = class (TUnaryOpExpr)
      public
         constructor Create(context : TdwsBaseSymbolsContext; const aScriptPos : TScriptPos; expr : TTypedExpr); override;
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;

   TConvIntegerToBigIntegerExpr = class(TBigIntegerUnaryOpExpr)
      procedure EvalAsInterface(exec : TdwsExecution; var result : IUnknown); override;
   end;
   TConvStringToBigIntegerExpr = class(TBigIntegerUnaryOpExpr)
      procedure EvalAsInterface(exec : TdwsExecution; var result : IUnknown); override;
   end;
   TConvFloatToBigIntegerExpr = class(TBigIntegerUnaryOpExpr)
      procedure EvalAsInterface(exec : TdwsExecution; var result : IUnknown); override;
   end;
   TConvBigIntegerToIntegerExpr = class(TUnaryOpIntExpr)
      function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   TConvBigIntegerToFloatExpr = class(TUnaryOpFloatExpr)
      function  EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   TBigIntegerToStringFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;
   TStringToBigIntegerFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;
   TBigIntegerToHexFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;
   THexToBigIntegerFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerToBlobFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;
   TBlobToBigIntegerFunc = class(TInternalMagicInterfaceFunction)
      procedure DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown); override;
   end;

   TBigIntegerToFloatFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;
   TBigIntegerToIntegerFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TBigIntegerOddFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;
   TBigIntegerEvenFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TBigIntegerSignFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TBigIntegerAbsFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerBitLengthFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TBigIntegerTestBitFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TBigIntegerSetBitFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TBigIntegerSetBitValFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TBigIntegerClearBitFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TBigIntegerPopCountFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TBigIntegerGcdFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerLcmFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerIsPrimeFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;
   TBigIntegerNextPrimeFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerPowerFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerSqrFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerModPowFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerModInvFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerDivModFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TBigJacobiFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;
   TBigLegendreFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TBigIntegerFactorialFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;
   TBigIntegerPrimorialFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerRandomFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cLimbSize = SizeOf(NativeUInt);
   cPoolMaxSize = 256;

type
   TLimbArray = array [0..1024*1024*1024 div cLimbSize] of NativeUInt;
   PLimbArray = ^TLimbArray;

var
   vPool : TdwsBigIntegerWrapperPool;

// RegisterBigIntegerType                                      
//
procedure RegisterBigIntegerType(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                 unitTable : TSymbolTable);
var
   typBigInteger : TBaseBigIntegerSymbol;
begin
   if systemTable.FindLocal(SYS_BIGINTEGER)<>nil then exit;

   typBigInteger:=TBaseBigIntegerSymbol.Create;

   systemTable.AddSymbol(typBigInteger);
end;

// RegisterBigIntegerOperators
//
procedure RegisterBigIntegerOperators(systemTable : TSystemSymbolTable;
                                  unitTable : TSymbolTable; operators : TOperators);
var
   typBigInteger : TBaseBigIntegerSymbol;

   procedure RegisterOperators(token : TTokenType; exprClass : TBinaryOpExprClass);
   begin
      operators.RegisterOperator(token, exprClass, typBigInteger, typBigInteger);
      operators.RegisterOperator(token, exprClass, systemTable.TypInteger, typBigInteger);
      operators.RegisterOperator(token, exprClass, typBigInteger, systemTable.TypInteger);
   end;

begin
   typBigInteger:=systemTable.FindTypeSymbol(SYS_BIGINTEGER, cvMagic) as TBaseBigIntegerSymbol;

   if operators.FindCaster(typBigInteger, systemTable.TypInteger) <> nil then Exit;

   operators.RegisterUnaryOperator(ttMINUS, TBigIntegerNegateExpr, typBigInteger);

   RegisterOperators(ttPLUS,     TBigIntegerAddOpExpr);
   RegisterOperators(ttMINUS,    TBigIntegerSubOpExpr);
   RegisterOperators(ttTIMES,    TBigIntegerMultOpExpr);
   RegisterOperators(ttDIV,      TBigIntegerDivOpExpr);
   RegisterOperators(ttMOD,      TBigIntegerModOpExpr);
   RegisterOperators(ttAND,      TBigIntegerAndOpExpr);
   RegisterOperators(ttOR,       TBigIntegerOrOpExpr);
   RegisterOperators(ttXOR,      TBigIntegerXorOpExpr);

   operators.RegisterOperator(ttSHL, TBigIntegerShiftLeftExpr,   typBigInteger, systemTable.TypInteger);
   operators.RegisterOperator(ttSAR, TBigIntegerShiftRightExpr,  typBigInteger, systemTable.TypInteger);

   operators.RegisterOperator(ttPLUS_ASSIGN,  TBigIntegerPlusAssignExpr, typBigInteger, typBigInteger);
   operators.RegisterOperator(ttPLUS_ASSIGN,  TBigIntegerPlusAssignIntExpr, typBigInteger, systemTable.TypInteger);
   operators.RegisterOperator(ttMINUS_ASSIGN, TBigIntegerMinusAssignExpr, typBigInteger, typBigInteger);
   operators.RegisterOperator(ttMINUS_ASSIGN, TBigIntegerMinusAssignIntExpr, typBigInteger, systemTable.TypInteger);
   operators.RegisterOperator(ttTIMES_ASSIGN, TBigIntegerMultAssignExpr, typBigInteger, typBigInteger);
   operators.RegisterOperator(ttTIMES_ASSIGN, TBigIntegerMultAssignExpr, typBigInteger, systemTable.TypInteger);

   RegisterOperators(ttEQ,       TBigIntegerEqualOpExpr);
   RegisterOperators(ttNOT_EQ,   TBigIntegerNotEqualOpExpr);
   RegisterOperators(ttGTR,      TBigIntegerGreaterOpExpr);
   RegisterOperators(ttGTR_EQ,   TBigIntegerGreaterEqualOpExpr);
   RegisterOperators(ttLESS,     TBigIntegerLessOpExpr);
   RegisterOperators(ttLESS_EQ,  TBigIntegerLessEqualOpExpr);

   operators.RegisterCaster(typBigInteger, systemTable.TypInteger, TConvIntegerToBigIntegerExpr);
   operators.RegisterCaster(typBigInteger, systemTable.TypString,  TConvStringToBigIntegerExpr);
   operators.RegisterCaster(typBigInteger, systemTable.TypFloat,   TConvFloatToBigIntegerExpr);
   operators.RegisterCaster(systemTable.TypInteger, typBigInteger, TConvBigIntegerToIntegerExpr);
   operators.RegisterCaster(systemTable.TypFloat, typBigInteger,   TConvBigIntegerToFloatExpr);
end;

type
   TTypedExprBigIntegerHelper = class helper for TTypedExpr
      function EvalAsBigInteger(exec : TdwsExecution) : IdwsBigInteger;
   end;

function TTypedExprBigIntegerHelper.EvalAsBigInteger(exec : TdwsExecution) : IdwsBigInteger;
begin
   if Typ.UnAliasedType.ClassType = TBaseBigIntegerSymbol then begin
      EvalAsInterface(exec, IUnknown(Result));
      if Result = nil then
         Result := TBigIntegerWrapper.CreateZero;
   end else Result := TBigIntegerWrapper.CreateInt64( EvalAsInteger(exec) );
end;

// ArgBigInteger
//
function ArgBigInteger(const args : TExprBaseListExec; index : Integer) : IdwsBigInteger;
begin
   Result := (args.ExprBase[index] as TTypedExpr).EvalAsBigInteger(args.Exec);
end;

// ArgVarBigInteger
//
function ArgVarBigInteger(const args : TExprBaseListExec; index : Integer) : IdwsBigInteger;

   procedure Allocate(varExpr : TBaseTypeVarExpr; var result : IdwsBigInteger);
   var
      v : Variant;
   begin
      Result := TBigIntegerWrapper.CreateZero;
      v := IUnknown(Result);
      varExpr.AssignValue(args.Exec, v);
   end;

var
   varExpr : TBaseTypeVarExpr;
begin
   varExpr := (args.ExprBase[index] as TBaseTypeVarExpr);
   varExpr.EvalAsInterface(args.Exec, IUnknown(Result));
   if Result = nil then
      Allocate(varExpr, Result);
end;

// ------------------
// ------------------ TBaseBigIntegerSymbol ------------------
// ------------------

// Create
//
constructor TBaseBigIntegerSymbol.Create;
begin
   inherited Create(SYS_BIGINTEGER);
end;

// IsCompatible
//
function TBaseBigIntegerSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(typSym<>nil) and (typSym.UnAliasedType.ClassType=TBaseBigIntegerSymbol);
end;

// InitData
//
procedure TBaseBigIntegerSymbol.InitData(const data : TData; offset : Integer);
begin
   VarCopySafe(data[offset], IUnknown(nil));
end;

// ------------------
// ------------------ TBigIntegerWrapper ------------------
// ------------------

// CreateNewZero
//
constructor TBigIntegerWrapper.CreateNewZero;
begin
   Create;
   if not Bind_MPIR_DLL then
      raise Exception.Create('mpir.dll is required for BigInteger');

   mpz_init(Value);
end;

// Reset
//
procedure TBigIntegerWrapper.Reset;
begin
   mpz_set_ui(Value, 0);
end;

// _Release
//
function TBigIntegerWrapper._Release: Integer;
begin
   Result := InterlockedDecrement(FRefCount);
   if Result = 0 then
      vPool.Push(Self);
end;

// CreateZero
//
class function TBigIntegerWrapper.CreateZero : TBigIntegerWrapper;
begin
   Result := vPool.Pop;
end;

// CreateInt64
//
class function TBigIntegerWrapper.CreateInt64(const i : Int64) : TBigIntegerWrapper;
begin
   Result := vPool.Pop;
   mpz_set_int64(Result.Value, i);
end;

// CreateFloat
//
class function TBigIntegerWrapper.CreateFloat(const f : Double) : TBigIntegerWrapper;
begin
   Result := vPool.Pop;
   mpz_set_d(Result.Value, f);
end;

// CreateString
//
class function TBigIntegerWrapper.CreateString(const s : String; base : Integer) : TBigIntegerWrapper;
var
   buf : RawByteString;
   p : PAnsiChar;
begin
   Result := vPool.Pop;
   if s <> '' then begin
      ScriptStringToRawByteString(s, buf);
      p := Pointer(buf);
      if p^ = '+' then
         Inc(p);
      mpz_set_str(Result.Value, p, base);
   end;
end;

// Destroy
//
destructor TBigIntegerWrapper.Destroy;
begin
   if Value.mp_alloc <> 0 then
      mpz_clear(Value);
   inherited;
end;

// GetValue
//
function TBigIntegerWrapper.GetValue : pmpz_t;
begin
   Result := @Value;
end;

// SetValue
//
procedure TBigIntegerWrapper.SetValue(const v : pmpz_t);
begin
   mpz_set(Value, v^);
end;

// GetSelf
//
function TBigIntegerWrapper.GetSelf : TObject;
begin
   Result := Self;
end;

// BitLength
//
function TBigIntegerWrapper.BitLength : Integer;
begin
   if Value.mp_size = 0 then
      Result := 0
   else Result := mpz_sizeinbase(Value, 2);
end;

// PopCount
//
function TBigIntegerWrapper.PopCount : Integer;
begin
   if Value.mp_size = 0 then
      Result := 0
   else Result := mpz_popcount(Value);
end;

// Sign
//
function TBigIntegerWrapper.Sign : Integer;
begin
   Result := mpz_sgn(Value);
end;

// ToStringBase
//
function TBigIntegerWrapper.ToStringBase(base : Integer) : String;
var
   size : Integer;
   buf : RawByteString;
begin
   Assert(base in [2..62]);

   if Value.mp_size = 0 then Exit('0');

   size := mpz_sizeinbase(Value, base);
   Assert(size > 0);
   if Value.mp_size < 0 then
      Inc(size);
   SetLength(buf, size);
   mpz_get_str(Pointer(buf), base, Value);
   if (size > 1) and (buf[size] = #0) then
      SetLength(buf, size-1); // clear occasional trailing #0
   Result := RawByteStringToScriptString(buf);
end;

// ToHexString
//
function TBigIntegerWrapper.ToHexString : String;
begin
   Result := ToStringBase(16);
end;

// ToString
//
function TBigIntegerWrapper.ToString : String;
begin
   Result := ToStringBase(10);
end;

// ToInt64
//
function TBigIntegerWrapper.ToInt64 : Int64;
var
   n : Integer;
begin
   Result := 0;

   n := Abs(Value.mp_size);
   if n > 2 then n := 3;
   System.Move(Value.mp_d^, Result, n*4);

   if Value.mp_size < 0 then begin
      Result := -Result;
   end;
end;

// ToNeg
//
function TBigIntegerWrapper.ToNeg : IdwsBigInteger;
var
   biw : TBigIntegerWrapper;
begin
   biw := TBigIntegerWrapper.CreateZero;
   mpz_neg(biw.Value, Value);
   Result := biw;
end;

// ------------------
// ------------------ TdwsBigIntegerWrapperPool ------------------
// ------------------

// Create
//
constructor TdwsBigIntegerWrapperPool.Create;
begin
   inherited;
   FLock := TMultiReadSingleWrite.Create;
end;

// Destroy
//
destructor TdwsBigIntegerWrapperPool.Destroy;
begin
   inherited;
   Cleanup;
   FLock.Free;
end;

// Pop
//
function TdwsBigIntegerWrapperPool.Pop : TBigIntegerWrapper;
begin
   Result := nil;
   if Self <> nil then begin
      FLock.BeginWrite;
      try
         if FHead <> nil then begin
            Result := FHead;
            FHead := FHead.FNext;
            Result.FNext := nil;
            Dec(FSize);
         end;
      finally
         FLock.EndWrite;
      end;
   end;
   if Result = nil then
      Result := TBigIntegerWrapper.CreateNewZero
end;

// Push
//
procedure TdwsBigIntegerWrapperPool.Push(ref : TBigIntegerWrapper);
begin
   if (Self = nil) or (FSize >= cPoolMaxSize) then
      ref.Free
   else begin
      FLock.BeginWrite;
      try
         ref.FNext := FHead;
         FHead := ref;
         ref.Reset;
         Inc(FSize);
      finally
         FLock.EndWrite;
      end;
   end;
end;

// Cleanup
//
procedure TdwsBigIntegerWrapperPool.Cleanup;
var
   iter, next : TBigIntegerWrapper;
begin
   FLock.BeginWrite;
   try
      iter := FHead;
      while iter <> nil do begin
         next := iter.FNext;
         iter.Free;
         iter := next;
      end;
      FHead := nil;
      FSize := 0;
   finally
      FLock.EndWrite;
   end;
end;

// ------------------
// ------------------ TBigIntegerNegateExpr ------------------
// ------------------

// Create
//
constructor TBigIntegerNegateExpr.Create(context : TdwsBaseSymbolsContext; const aScriptPos : TScriptPos; expr : TTypedExpr);
begin
   inherited;
   Typ := expr.Typ;
end;

// EvalAsVariant
//
procedure TBigIntegerNegateExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   result := Expr.EvalAsBigInteger(exec).ToNeg;
end;

// ------------------
// ------------------ TBigIntegerBinOpExpr ------------------
// ------------------

// Create
//
constructor TBigIntegerBinOpExpr.Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                                        const anOp : TTokenType; aLeft, aRight : TTypedExpr);
begin
   inherited Create(context, aScriptPos, anOp, aLeft, aRight);
   if aLeft.Typ.UnAliasedTypeIs(TBaseIntegerSymbol) then
      Typ := aRight.Typ
   else Typ := aLeft.Typ;
end;


// EvalAsVariant
//
procedure TBigIntegerBinOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   intf : IUnknown;
begin
   EvalAsInterface(exec, intf);
   result := intf;
end;

// ------------------
// ------------------ TBigIntegerBinOpFuncExpr ------------------
// ------------------

// Create
//
constructor TBigIntegerBinOpFuncExpr.Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                                            const anOp : TTokenType; aLeft, aRight : TTypedExpr);
begin
   inherited Create(context, aScriptPos, anOp, aLeft, aRight);
   InitOpFunc;
end;

// EvalAsInterface
//
procedure TBigIntegerBinOpFuncExpr.EvalAsInterface(exec : TdwsExecution; var result : IUnknown);
var
   bi : TBigIntegerWrapper;
   biLeft, biRight : IdwsBigInteger;
begin
   biLeft := Left.EvalAsBigInteger(exec);
   biRight := Right.EvalAsBigInteger(exec);
   bi := TBigIntegerWrapper.CreateZero;
   FOpFunc(bi.Value, biLeft.Value^, biRight.Value^);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerAddOpExpr ------------------
// ------------------

procedure TBigIntegerAddOpExpr.InitOpFunc;
begin
   FOpFunc := mpz_add;
end;

// ------------------
// ------------------ TBigIntegerSubOpExpr ------------------
// ------------------

procedure TBigIntegerSubOpExpr.InitOpFunc;
begin
   FOpFunc := mpz_sub;
end;

// ------------------
// ------------------ TBigIntegerMultOpExpr ------------------
// ------------------

procedure TBigIntegerMultOpExpr.InitOpFunc;
begin
   FOpFunc := mpz_mul;
end;

// ------------------
// ------------------ TBigIntegerDivOpExpr ------------------
// ------------------

procedure TBigIntegerDivOpExpr.InitOpFunc;
begin
   FOpFunc := mpz_tdiv_q
end;

// ------------------
// ------------------ TBigIntegerModOpExpr ------------------
// ------------------

procedure TBigIntegerModOpExpr.InitOpFunc;
begin
   FOpFunc := mpz_mod;
end;

// ------------------
// ------------------ TBigIntegerAndOpExpr ------------------
// ------------------

procedure TBigIntegerAndOpExpr.InitOpFunc;
begin
   FOpFunc := mpz_and;
end;

// ------------------
// ------------------ TBigIntegerOrOpExpr ------------------
// ------------------

procedure TBigIntegerOrOpExpr.InitOpFunc;
begin
   FOpFunc := mpz_ior;
end;

// ------------------
// ------------------ TBigIntegerXorOpExpr ------------------
// ------------------

procedure TBigIntegerXorOpExpr.InitOpFunc;
begin
   FOpFunc := mpz_xor;
end;

// ------------------
// ------------------ TBigIntegerRelOpExpr ------------------
// ------------------

function TBigIntegerRelOpExpr.InternalCompare(exec : TdwsExecution) : Integer;
begin
   Result := mpz_cmp(Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
end;

// ------------------
// ------------------ TBigIntegerEqualOpExpr ------------------
// ------------------

function TBigIntegerEqualOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := InternalCompare(exec) = 0;
end;

// ------------------
// ------------------ TBigIntegerNotEqualOpExpr ------------------
// ------------------

function TBigIntegerNotEqualOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := InternalCompare(exec) <> 0;
end;

// ------------------
// ------------------ TBigIntegerGreaterOpExpr ------------------
// ------------------

function TBigIntegerGreaterOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := InternalCompare(exec) > 0;
end;

// ------------------
// ------------------ TBigIntegerGreaterEqualOpExpr ------------------
// ------------------

function TBigIntegerGreaterEqualOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := InternalCompare(exec) >= 0;
end;

// ------------------
// ------------------ TBigIntegerLessOpExpr ------------------
// ------------------

function TBigIntegerLessOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := InternalCompare(exec) < 0;
end;

// ------------------
// ------------------ TBigIntegerLessEqualOpExpr ------------------
// ------------------

function TBigIntegerLessEqualOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := InternalCompare(exec) <= 0;
end;

// ------------------
// ------------------ TBigIntegerUnaryOpExpr ------------------
// ------------------

constructor TBigIntegerUnaryOpExpr.Create(context : TdwsBaseSymbolsContext; const aScriptPos : TScriptPos; expr : TTypedExpr);
begin
   inherited Create(context, aScriptPos, expr);
   Typ := context.FindType(SYS_BIGINTEGER);
end;

procedure TBigIntegerUnaryOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   intf : IUnknown;
begin
   EvalAsInterface(exec, intf);
   result := intf;
end;

// ------------------
// ------------------ TConvIntegerToBigIntegerExpr ------------------
// ------------------

procedure TConvIntegerToBigIntegerExpr.EvalAsInterface(exec : TdwsExecution; var result : IUnknown);
begin
   result := TBigIntegerWrapper.CreateInt64( Expr.EvalAsInteger(exec) ) as IdwsBigInteger;
end;
// ------------------
// ------------------ TConvStringToBigIntegerExpr ------------------
// ------------------

procedure TConvStringToBigIntegerExpr.EvalAsInterface(exec : TdwsExecution; var result : IUnknown);
var
   s : String;
begin
   Expr.EvalAsString(exec, s);
   result := TBigIntegerWrapper.CreateString( s, 10 ) as IdwsBigInteger;
end;

// ------------------
// ------------------ TConvFloatToBigIntegerExpr ------------------
// ------------------

// EvalAsInterface
//
procedure TConvFloatToBigIntegerExpr.EvalAsInterface(exec : TdwsExecution; var result : IUnknown);
begin
   Result := TBigIntegerWrapper.CreateFloat(Expr.EvalAsFloat(exec)) as IdwsBigInteger;
end;

// ------------------
// ------------------ TConvBigIntegerToIntegerExpr ------------------
// ------------------

function TConvBigIntegerToIntegerExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := Expr.EvalAsBigInteger(exec).ToInt64;
end;

// ------------------
// ------------------ TConvBigIntegerToFloatExpr ------------------
// ------------------

function TConvBigIntegerToFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result := mpz_get_d(Expr.EvalAsBigInteger(exec).Value^);
end;

// ------------------
// ------------------ TBigIntegerToStringFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TBigIntegerToStringFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   Result := ArgBigInteger(args, 0).ToStringBase(args.AsInteger[1]);
end;

// ------------------
// ------------------ TStringToBigIntegerFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TStringToBigIntegerFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   result := TBigIntegerWrapper.CreateString( args.AsString[0], args.AsInteger[1] ) as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerToHexFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TBigIntegerToHexFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   Result := ArgBigInteger(args, 0).ToStringBase(16);
end;

// ------------------
// ------------------ THexToBigIntegerFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure THexToBigIntegerFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   result := TBigIntegerWrapper.CreateString( args.AsString[0], 16 ) as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerToFloatFunc ------------------
// ------------------

procedure TBigIntegerToFloatFunc.DoEvalAsFloat(const args : TExprBaseListExec; var result : Double);
begin
   result := mpz_get_d(ArgBigInteger(args, 0).Value^);
end;

// ------------------
// ------------------ TBigIntegerToIntegerFunc ------------------
// ------------------

function TBigIntegerToIntegerFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := ArgBigInteger(args, 0).ToInt64;
end;

// ------------------
// ------------------ TBigIntegerOddFunc ------------------
// ------------------

function TBigIntegerOddFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result := mpz_odd_p(ArgBigInteger(args, 0).Value^);
end;

// ------------------
// ------------------ TBigIntegerEvenFunc ------------------
// ------------------

function TBigIntegerEvenFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result := mpz_even_p(ArgBigInteger(args, 0).Value^);
end;

// ------------------
// ------------------ TBigIntegerSignFunc ------------------
// ------------------

function TBigIntegerSignFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := ArgBigInteger(args, 0).Sign;
end;

// ------------------
// ------------------ TBigIntegerAbsFunc ------------------
// ------------------

procedure TBigIntegerAbsFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   bi.SetValue(ArgBigInteger(args, 0).Value);
   bi.Value.mp_size := Abs(bi.Value.mp_size);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerGcdFunc ------------------
// ------------------

procedure TBigIntegerGcdFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_gcd(bi.Value, ArgBigInteger(args, 0).Value^, ArgBigInteger(args, 1).Value^);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerLcmFunc ------------------
// ------------------

procedure TBigIntegerLcmFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_lcm(bi.Value, ArgBigInteger(args, 0).Value^, ArgBigInteger(args, 1).Value^);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerIsPrimeFunc ------------------
// ------------------

function TBigIntegerIsPrimeFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   state : gmp_randstate_t;
begin
   gmp_randinit_mt(state);
   try
      Result := mpz_probable_prime_p(ArgBigInteger(args, 0).Value^, state, args.AsInteger[1], 0) > 0;
   finally
      gmp_randclear(state);
   end;
end;

// ------------------
// ------------------ TBigIntegerNextPrimeFunc ------------------
// ------------------

procedure TBigIntegerNextPrimeFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   base : IdwsBigInteger;
   bi : TBigIntegerWrapper;
   state : gmp_randstate_t;
   reps : Integer;
begin
   base := ArgBigInteger(args, 0);
   reps := args.AsInteger[1];

   bi := TBigIntegerWrapper.CreateZero;
   result := bi as IdwsBigInteger;

   if base.Value.mp_size <= 0 then begin
      mpz_set_ui(bi.Value, 1);
      Exit;
   end;

   if mpz_even_p(base.Value^) then
      mpz_add_ui(bi.Value, base.Value^, 1)
   else mpz_add_ui(bi.Value, base.Value^, 2);

   gmp_randinit_mt(state);
   try
      while mpz_probable_prime_p(bi.Value, state, reps, 0) <= 0 do begin
         if args.Exec.ProgramState = psRunningStopped then
            raise Exception.Create('NextPrime aborted');
         mpz_add_ui(bi.Value, bi.Value, 2);
      end;
   finally
      gmp_randclear(state);
   end;
end;

// ------------------
// ------------------ TBigIntegerPowerFunc ------------------
// ------------------

procedure TBigIntegerPowerFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_pow_ui(bi.Value, ArgBigInteger(args, 0).Value^, args.AsInteger[1]);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerSqrFunc ------------------
// ------------------

procedure TBigIntegerSqrFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_pow_ui(bi.Value, ArgBigInteger(args, 0).Value^, 2);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerDivModFunc ------------------
// ------------------

procedure TBigIntegerDivModFunc.DoEvalProc(const args : TExprBaseListExec);
var
   biQ, biR : TBigIntegerWrapper;
begin
   biQ := TBigIntegerWrapper.CreateZero;
   biR := TBigIntegerWrapper.CreateZero;

   mpz_tdiv_qr(biQ.Value, biR.Value, ArgBigInteger(args, 0).Value^, ArgBigInteger(args, 1).Value^);

   args.ExprBase[2].AssignValue(args.Exec, biQ as IdwsBigInteger);
   args.ExprBase[3].AssignValue(args.Exec, biR as IdwsBigInteger);
end;

// ------------------
// ------------------ TBigIntegerToBlobFunc ------------------
// ------------------

procedure TBigIntegerToBlobFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bufString : RawByteString;
   pDest, pSrc : PByte;
   n : Integer;
   gmp : pmpz_t;
begin
   gmp := ArgBigInteger(args, 0).Value;
   n := Abs(gmp.mp_size);
   if n = 0 then
      bufString := ''
   else begin
      SetLength(bufString, n*cLimbSize+1);
      pDest := Pointer(bufString);
      if gmp.mp_size < 0 then begin
         pDest^ := $ff;
         Inc(pDest);
      end;
      pSrc := @PLimbArray(gmp.mp_d)^[n-1];
      Inc(pSrc, cLimbSize-1);
      // skip zeroes
      while pSrc^ = 0 do begin
         Dec(pSrc);
         if pSrc = PByte(gmp.mp_d) then break;
      end;
      if (pSrc^ = $ff) and (gmp.mp_size > 0) then begin
         pDest^ := $00;
         Inc(pDest);
      end;
      repeat
         pDest^ := pSrc^;
         Dec(pSrc);
         Inc(pDest);
      until NativeUInt(pSrc) < NativeUInt(gmp.mp_d);
      SetLength(bufString, NativeUInt(pDest)-NativeUInt(Pointer(bufString)));
   end;
   Result := bufString;
end;

// ------------------
// ------------------ TBlobToBigIntegerFunc ------------------
// ------------------

// DoEvalAsInterface
//
procedure TBlobToBigIntegerFunc.DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown);
var
   bi : TBigIntegerWrapper;
   bufString : RawByteString;
   nbBytes, nbLimbs : Integer;
   pSrc, pDest : PByte;
   i : Integer;
begin
   bi := TBigIntegerWrapper.CreateZero;

   bufString := args.AsDataString[0];
   if bufString <> '' then begin

      nbBytes := Length(bufString);
      pSrc := Pointer(bufString);
      case Ord(bufString[1]) of
         $00, $ff : begin
            Inc(pSrc);
            Dec(nbBytes);
         end
      end;

      nbLimbs := (nbBytes+cLimbSize-1) div cLimbSize;
      mpz_realloc(bi.Value, nbLimbs);
      if Ord(bufString[1]) = $ff then
         bi.Value.mp_size := -nbLimbs
      else bi.Value.mp_size := nbLimbs;

      PLimbArray(bi.Value.mp_d)[nbLimbs-1] := 0;
      pDest := @PByteArray(bi.Value.mp_d)[nbBytes-1];
      for i := 1 to nbBytes do begin
         pDest^ := pSrc^;
         Dec(pDest);
         Inc(pSrc);
      end;

   end;

   Result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerShiftLeftExpr ------------------
// ------------------

procedure TBigIntegerShiftLeftExpr.EvalAsInterface(exec : TdwsExecution; var result : IUnknown);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_mul_2exp(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsInteger(exec));
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerShiftRightExpr ------------------
// ------------------

procedure TBigIntegerShiftRightExpr.EvalAsInterface(exec : TdwsExecution; var result : IUnknown);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_tdiv_q_2exp(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsInteger(exec));
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerOpAssignExpr ------------------
// ------------------

procedure TBigIntegerOpAssignExpr.TypeCheckAssign(context : TdwsCompilerContext);
begin
   // nothing here
end;

// ------------------
// ------------------ TBigIntegerPlusAssignExpr ------------------
// ------------------

procedure TBigIntegerPlusAssignExpr.EvalNoResult(exec : TdwsExecution);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_add(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   FLeft.AssignValue(exec, bi as IdwsBigInteger);
end;

// ------------------
// ------------------ TBigIntegerPlusAssignIntExpr ------------------
// ------------------

procedure TBigIntegerPlusAssignIntExpr.EvalNoResult(exec : TdwsExecution);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateInt64(Right.EvalAsInteger(exec));
   mpz_add(bi.Value, bi.Value, Left.EvalAsBigInteger(exec).Value^);
   FLeft.AssignValue(exec, bi as IdwsBigInteger);
end;

// ------------------
// ------------------ TBigIntegerMinusAssignExpr ------------------
// ------------------

procedure TBigIntegerMinusAssignExpr.EvalNoResult(exec : TdwsExecution);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_sub(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   FLeft.AssignValue(exec, bi as IdwsBigInteger);
end;

// ------------------
// ------------------ TBigIntegerMinusAssignIntExpr ------------------
// ------------------

procedure TBigIntegerMinusAssignIntExpr.EvalNoResult(exec : TdwsExecution);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateInt64(Right.EvalAsInteger(exec));
   mpz_sub(bi.Value, Left.EvalAsBigInteger(exec).Value^, bi.Value);
   FLeft.AssignValue(exec, bi as IdwsBigInteger);
end;

// ------------------
// ------------------ TBigIntegerMultAssignExpr ------------------
// ------------------

procedure TBigIntegerMultAssignExpr.EvalNoResult(exec : TdwsExecution);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_mul(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   FLeft.AssignValue(exec, bi as IdwsBigInteger);
end;

// ------------------
// ------------------ TBigIntegerRandomFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TBigIntegerRandomFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);

   function RandomBigIntegerOfBitLength(nb : Integer) : IdwsBigInteger;
   var
      mask : Integer;
      bytes : TBytes;
      bi : TBigIntegerWrapper;
   begin
      Assert(nb > 0);

      // adapted from BigInteger.Create(NumBits: Integer; const Random: IRandom)
      // uses cryptographic random
      bytes := RawByteStringToBytes(CryptographicRandom( (nb + 7) div 8 + 1 ));

      // One byte too many was allocated, to get a top byte of 0, i.e. always positive.
      bytes[High(bytes)] := 0;

      // Set bits above required bit length to 0.
      mask := $7F shr (7 - (nb and 7));
      bytes[High(bytes)-1] := bytes[High(bytes)-1] and mask;

      bi := TBigIntegerWrapper.CreateZero;
      mpz_realloc(bi.Value, (nb div (8*cLimbSize))+1);
      FillChar(bi.Value.mp_d^, bi.Value.mp_alloc*cLimbSize, 0);
      bi.Value.mp_size := bi.Value.mp_alloc;
      System.Move(bytes[0], bi.Value.mp_d^, Length(bytes));

      Result := bi as IdwsBigInteger;
   end;

var
   bi, limit : IdwsBigInteger;
   bits : Integer;
begin
   limit := ArgBigInteger(args, 0);
   if mpz_cmp_ui(limit.Value^, 1) <= 0 then begin
      result := TBigIntegerWrapper.CreateZero as IdwsBigInteger;
   end else begin
      bits := limit.BitLength;
      repeat
         bi := RandomBigIntegerOfBitLength(bits);
      until mpz_cmp(bi.Value^, limit.Value^) < 0;
   end;
   result := bi;
end;

// ------------------
// ------------------ TBigIntegerBitLengthFunc ------------------
// ------------------

function TBigIntegerBitLengthFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := ArgBigInteger(args, 0).BitLength;
end;

// ------------------
// ------------------ TBigIntegerTestBitFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TBigIntegerTestBitFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result := mpz_tstbit(ArgBigInteger(args, 0).Value^, Cardinal(args.AsInteger[1])) <> 0;
end;

// ------------------
// ------------------ TBigIntegerSetBitFunc ------------------
// ------------------

// DoEvalProc
//
procedure TBigIntegerSetBitFunc.DoEvalProc(const args : TExprBaseListExec);
begin
   mpz_setbit(ArgVarBigInteger(args, 0).Value^, Cardinal(args.AsInteger[1]));
end;

// ------------------
// ------------------ TBigIntegerSetBitValFunc ------------------
// ------------------

// DoEvalProc
//
procedure TBigIntegerSetBitValFunc.DoEvalProc(const args : TExprBaseListExec);
var
   bi : IdwsBigInteger;
   bit : Integer;
begin
   bi := ArgVarBigInteger(args, 0);
   bit := args.AsInteger[1];
   if args.AsBoolean[2] then
      mpz_setbit(bi.Value^, bit)
   else mpz_clrbit(bi.Value^, bit)
end;

// ------------------
// ------------------ TBigIntegerClearBitFunc ------------------
// ------------------

// DoEvalProc
//
procedure TBigIntegerClearBitFunc.DoEvalProc(const args : TExprBaseListExec);
begin
   mpz_clrbit(ArgVarBigInteger(args, 0).Value^, args.AsInteger[1]);
end;

// ------------------
// ------------------ TBigIntegerPopCountFunc ------------------
// ------------------

function TBigIntegerPopCountFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := ArgBigInteger(args, 0).PopCount;
end;

// ------------------
// ------------------ TBigIntegerModPowFunc ------------------
// ------------------

procedure TBigIntegerModPowFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   Result := bi as IdwsBigInteger;
   mpz_powm(bi.Value,
            ArgBigInteger(args, 0).Value^,
            ArgBigInteger(args, 1).Value^,
            ArgBigInteger(args, 2).Value^);
end;

// ------------------
// ------------------ TBigIntegerModInvFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TBigIntegerModInvFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   Result := bi as IdwsBigInteger;
   mpz_invert(bi.Value, ArgBigInteger(args, 0).Value^, ArgBigInteger(args, 1).Value^);
end;

// ------------------
// ------------------ TBigIntegerFactorialFunc ------------------
// ------------------

procedure TBigIntegerFactorialFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
   i : Int64;
begin
   i := args.AsInteger[0];
   bi := TBigIntegerWrapper.CreateZero;
   if i <= 1 then
      mpz_set_uint64(bi.Value, 1)
   else mpz_fac_ui(bi.Value, i);
   Result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerPrimorialFunc ------------------
// ------------------

procedure TBigIntegerPrimorialFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
   i : Int64;
begin
   i := args.AsInteger[0];
   bi := TBigIntegerWrapper.CreateZero;
   if i < 1 then
      mpz_set_uint64(bi.Value, 1)
   else mpz_primorial_ui(bi.Value, i);
   Result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigJacobiFunc ------------------
// ------------------

function TBigJacobiFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := mpz_jacobi(ArgBigInteger(args, 0).Value^, ArgBigInteger(args, 1).Value^);
end;

// ------------------
// ------------------ TBigLegendreFunc ------------------
// ------------------

function TBigLegendreFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := mpz_legendre(ArgBigInteger(args, 0).Value^, ArgBigInteger(args, 1).Value^);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vPool := TdwsBigIntegerWrapperPool.Create;

   dwsInternalUnit.AddSymbolsRegistrationProc(RegisterBigIntegerType);
   dwsInternalUnit.AddOperatorsRegistrationProc(RegisterBigIntegerOperators);

   RegisterInternalStringFunction(TBigIntegerToStringFunc,  'BigIntegerToString', ['v', SYS_BIGINTEGER, 'base=10', SYS_INTEGER], [iffStateLess], 'ToString');
   RegisterInternalFunction(TStringToBigIntegerFunc,        'StringToBigInteger', ['s', SYS_STRING, 'base=10', SYS_INTEGER], SYS_BIGINTEGER, [iffStateLess], 'ToBigInteger');
   RegisterInternalStringFunction(TBigIntegerToHexFunc,     'BigIntegerToHex', ['v', SYS_BIGINTEGER], [iffStateLess], 'ToHex');
   RegisterInternalFunction(THexToBigIntegerFunc,           'HexToBigInteger', ['h', SYS_STRING], SYS_BIGINTEGER, [iffStateLess], 'HexToBigInteger');

   RegisterInternalFunction(TBigIntegerToBlobFunc,          'BigIntegerToBlobParameter', ['v', SYS_BIGINTEGER], SYS_VARIANT, [iffStateLess], 'ToBlobParameter');
   RegisterInternalInterfaceFunction(TBlobToBigIntegerFunc, 'BlobFieldToBigInteger', ['b', SYS_STRING], SYS_BIGINTEGER, [iffStateLess]);

   RegisterInternalFloatFunction(TBigIntegerToFloatFunc,    '',   ['v', SYS_BIGINTEGER], [iffStateLess], 'ToFloat');
   RegisterInternalIntFunction(TBigIntegerToIntegerFunc,    '',   ['v', SYS_BIGINTEGER], [iffStateLess], 'ToInteger');


   RegisterInternalBoolFunction(TBigIntegerOddFunc,   'Odd',      ['i', SYS_BIGINTEGER], [iffStateLess, iffOverloaded], 'IsOdd');
   RegisterInternalBoolFunction(TBigIntegerEvenFunc,  'Even',     ['i', SYS_BIGINTEGER], [iffStateLess, iffOverloaded], 'IsEven');
   RegisterInternalIntFunction(TBigIntegerSignFunc,   'Sign',     ['v', SYS_BIGINTEGER], [iffStateLess, iffOverloaded], 'Sign');
   RegisterInternalFunction(TBigIntegerAbsFunc,       'Abs',      ['v', SYS_BIGINTEGER], SYS_BIGINTEGER, [iffStateLess, iffOverloaded], 'Abs');

   RegisterInternalIntFunction(TBigIntegerBitLengthFunc,  '',     ['v', SYS_BIGINTEGER], [iffStateLess], 'BitLength');
   RegisterInternalBoolFunction(TBigIntegerTestBitFunc,   '',     ['i', SYS_BIGINTEGER, 'bit', SYS_INTEGER], [iffStateLess], 'TestBit');
   RegisterInternalProcedure(TBigIntegerSetBitFunc,       '',     ['@i', SYS_BIGINTEGER, 'bit', SYS_INTEGER], 'SetBit', [iffOverloaded]);
   RegisterInternalProcedure(TBigIntegerSetBitValFunc,    '',     ['@i', SYS_BIGINTEGER, 'bit', SYS_INTEGER, 'v', SYS_BOOLEAN], 'SetBit', [iffOverloaded]);
   RegisterInternalProcedure(TBigIntegerClearBitFunc,     '',     ['@i', SYS_BIGINTEGER, 'bit', SYS_INTEGER], 'ClearBit', []);
   RegisterInternalIntFunction(TBigIntegerPopCountFunc,   '',     ['i', SYS_BIGINTEGER], [iffStateLess], 'PopCount');

   RegisterInternalFunction(TBigIntegerGcdFunc,        'Gcd',     ['a', SYS_BIGINTEGER, 'b', SYS_BIGINTEGER], SYS_BIGINTEGER, [iffStateLess, iffOverloaded]);
   RegisterInternalFunction(TBigIntegerLcmFunc,        'Lcm',     ['a', SYS_BIGINTEGER, 'b', SYS_BIGINTEGER], SYS_BIGINTEGER, [iffStateLess, iffOverloaded]);
   RegisterInternalBoolFunction(TBigIntegerIsPrimeFunc, 'IsPrime',['n', SYS_BIGINTEGER, 'prob=25', SYS_INTEGER], [iffStateLess, iffOverloaded], 'IsPrime');
   RegisterInternalFunction(TBigIntegerNextPrimeFunc,     '',     ['n', SYS_BIGINTEGER, 'prob=25', SYS_INTEGER], SYS_BIGINTEGER, [iffStateLess], 'NextPrime');

   RegisterInternalFunction(TBigIntegerPowerFunc,     'IntPower', ['base', SYS_BIGINTEGER, 'exponent', SYS_INTEGER], SYS_BIGINTEGER, [iffStateLess, iffOverloaded], 'Power');
   RegisterInternalFunction(TBigIntegerSqrFunc,       'Sqr',      ['v', SYS_BIGINTEGER], SYS_BIGINTEGER, [iffStateLess, iffOverloaded], 'Sqr');
   RegisterInternalProcedure(TBigIntegerDivModFunc,   'DivMod',
                             ['dividend', SYS_BIGINTEGER, 'divisor', SYS_BIGINTEGER,
                              '@result', SYS_BIGINTEGER, '@remainder', SYS_BIGINTEGER], '', [iffOverloaded]);
   RegisterInternalFunction(TBigIntegerModPowFunc,    'ModPow',   ['base', SYS_BIGINTEGER, 'exponent', SYS_BIGINTEGER, 'modulus', SYS_BIGINTEGER],
                                                                  SYS_BIGINTEGER, [iffStateLess, iffOverloaded], 'ModPow');
   RegisterInternalFunction(TBigIntegerModPowFunc,    'ModPow',   ['base', SYS_BIGINTEGER, 'exponent', SYS_INTEGER, 'modulus', SYS_BIGINTEGER],
                                                                  SYS_BIGINTEGER, [iffStateLess, iffOverloaded], 'ModPow');
   RegisterInternalFunction(TBigIntegerModInvFunc,    'ModInv',   ['base', SYS_BIGINTEGER, 'modulus', SYS_BIGINTEGER],
                                                                  SYS_BIGINTEGER, [iffStateLess], 'ModInv');
   RegisterInternalFunction(TBigIntegerFactorialFunc, 'BigFactorial', ['n', SYS_INTEGER], SYS_BIGINTEGER, [iffStateLess]);
   RegisterInternalFunction(TBigIntegerPrimorialFunc, 'BigPrimorial', ['n', SYS_INTEGER], SYS_BIGINTEGER, [iffStateLess]);

   RegisterInternalIntFunction(TBigJacobiFunc,        'BigJacobi', ['a', SYS_BIGINTEGER, 'b', SYS_BIGINTEGER], [iffStateLess], 'Jacobi');
   RegisterInternalIntFunction(TBigLegendreFunc,      'BigLegendre', ['a', SYS_BIGINTEGER, 'b', SYS_BIGINTEGER], [iffStateLess], 'Legendre');

   RegisterInternalFunction(TBigIntegerRandomFunc,    'RandomBigInteger', ['limitPlusOne', SYS_BIGINTEGER], SYS_BIGINTEGER);

finalization

   vPool.Cleanup;
   FreeAndNil(vPool);

end.

