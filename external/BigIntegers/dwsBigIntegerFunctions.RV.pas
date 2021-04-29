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
unit dwsBigIntegerFunctions;

{$I dws.inc}

interface

uses
   Classes, SysUtils,
   dwsXPlatform, dwsUtils, dwsStrings,
   dwsFunctions, dwsSymbols, dwsExprs, dwsCoreExprs, dwsExprList, dwsUnitSymbols,
   dwsConstExprs, dwsMagicExprs, dwsDataContext, dwsErrors, dwsRelExprs,
   dwsOperators, dwsTokenizer, dwsCryptoXPlatform,
   Velthuis.BigIntegers;

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
      function GetValue : BigInteger;
      procedure SetValue(const v : BigInteger);
      property Value : BigInteger read GetValue write SetValue;
   end;

   TBigIntegerWrapper = class (TInterfacedObject, IdwsBigInteger, IGetSelf)
      private
         FData : BigInteger;

      protected
         function GetValue : BigInteger;
         procedure SetValue(const v : BigInteger);
         function GetSelf : TObject;

      public
         constructor Create(const aBigInteger : BigInteger);
         function ToString : String; override;
   end;

   TBigIntegerOpExpr = class(TBinaryOpExpr)
      constructor Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr); override;
   end;

   TBigIntegerAddOpExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TBigIntegerSubOpExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TBigIntegerMultOpExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TBigIntegerDivOpExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TBigIntegerModOpExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;

   TBigIntegerShiftLeftExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TBigIntegerShiftRightExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;

   TBigIntegerOpAssignExpr = class(TOpAssignExpr)
     procedure TypeCheckAssign(prog : TdwsProgram; exec : TdwsExecution); override;
   end;

   TBigIntegerPlusAssignExpr = class(TBigIntegerOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBigIntegerMinusAssignExpr = class(TBigIntegerOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBigIntegerMultAssignExpr = class(TBigIntegerOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TBigIntegerRelOpExpr = class(TBoolRelOpExpr)
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
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

   TBigIntegerCompareZeroExpr = class(TUnaryOpBoolExpr)
      private
         FOp : TTokenType;
      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr; op : TTokenType); reintroduce;
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TBigIntegerUnaryOpExpr = class (TUnaryOpExpr)
      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr); override;
   end;

   TConvIntegerToBigIntegerExpr = class(TBigIntegerUnaryOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TConvStringToBigIntegerExpr = class(TBigIntegerUnaryOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TConvBigIntegerToIntegerExpr = class(TUnaryOpIntExpr)
      function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   TConvBigIntegerToFloatExpr = class(TUnaryOpFloatExpr)
      function  EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   TBigIntegerToStringFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;
   TStringToBigIntegerFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;
   TBigIntegerToHexFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;
   THexToBigIntegerFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerToBlobFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;
   TBlobToBigIntegerFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
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

   TBigIntegerBitLengthFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TBigIntegerAbsExpr = class(TBigIntegerUnaryOpExpr)
      public
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;

   TBigIntegerGcdFunc = class(TInternalMagicVariantFunction)
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

   TBigIntegerDivModFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
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

   RegisterOperators(ttPLUS,     TBigIntegerAddOpExpr);
   RegisterOperators(ttMINUS,    TBigIntegerSubOpExpr);
   RegisterOperators(ttTIMES,    TBigIntegerMultOpExpr);
   RegisterOperators(ttDIV,      TBigIntegerDivOpExpr);
   RegisterOperators(ttMOD,      TBigIntegerModOpExpr);

   operators.RegisterOperator(ttSHL, TBigIntegerShiftLeftExpr,   typBigInteger, systemTable.TypInteger);
   operators.RegisterOperator(ttSAR, TBigIntegerShiftRightExpr,  typBigInteger, systemTable.TypInteger);

   operators.RegisterOperator(ttPLUS_ASSIGN,  TBigIntegerPlusAssignExpr, typBigInteger, typBigInteger);
   operators.RegisterOperator(ttPLUS_ASSIGN,  TBigIntegerPlusAssignExpr, typBigInteger, systemTable.TypInteger);
   operators.RegisterOperator(ttMINUS_ASSIGN, TBigIntegerMinusAssignExpr, typBigInteger, typBigInteger);
   operators.RegisterOperator(ttMINUS_ASSIGN, TBigIntegerMinusAssignExpr, typBigInteger, systemTable.TypInteger);
   operators.RegisterOperator(ttTIMES_ASSIGN, TBigIntegerMultAssignExpr, typBigInteger, typBigInteger);
   operators.RegisterOperator(ttTIMES_ASSIGN, TBigIntegerMultAssignExpr, typBigInteger, systemTable.TypInteger);

   RegisterOperators(ttEQ,       TBigIntegerEqualOpExpr);
   RegisterOperators(ttNOTEQ,    TBigIntegerNotEqualOpExpr);
   RegisterOperators(ttGTR,      TBigIntegerGreaterOpExpr);
   RegisterOperators(ttGTREQ,    TBigIntegerGreaterEqualOpExpr);
   RegisterOperators(ttLESS,     TBigIntegerLessOpExpr);
   RegisterOperators(ttLESSEQ,   TBigIntegerLessEqualOpExpr);

   operators.RegisterCaster(typBigInteger, systemTable.TypInteger, TConvIntegerToBigIntegerExpr);
   operators.RegisterCaster(typBigInteger, systemTable.TypString,  TConvStringToBigIntegerExpr);
   operators.RegisterCaster(systemTable.TypInteger, typBigInteger, TConvBigIntegerToIntegerExpr);
   operators.RegisterCaster(systemTable.TypFloat, typBigInteger,   TConvBigIntegerToFloatExpr);
end;

// HandleBigIntegerAbs
//
function HandleBigIntegerAbs(prog : TdwsProgram; argExpr : TTypedExpr) : TTypedExpr;
begin
   if argExpr.Typ.UnAliasedTypeIs(TBaseBigIntegerSymbol) then
      Result:=TBigIntegerAbsExpr.Create(prog, argExpr)
   else Result:=nil;
end;

type
   TTypedExprBigIntegerHelper = class helper for TTypedExpr
      function EvalAsBigInteger(exec : TdwsExecution) : BigInteger;
   end;

function TTypedExprBigIntegerHelper.EvalAsBigInteger(exec : TdwsExecution) : BigInteger;
var
   v : Variant;
begin
   if Typ.UnAliasedType.ClassType = TBaseBigIntegerSymbol then begin
      EvalAsVariant(exec, v);
      Assert(TVarData(v).VType=varUnknown);
      if TVarData(v).VUnknown<>nil then
         Result := IdwsBigInteger(TVarData(v).VUnknown).GetValue
      else Result := BigInteger.Zero;
   end else Result := EvalAsInteger(exec);
end;

// ArgBigInteger
//
function ArgBigInteger(const args : TExprBaseListExec; index : Integer) : BigInteger;
begin
   Result := (args.ExprBase[index] as TTypedExpr).EvalAsBigInteger(args.Exec);
end;

// BigIntegerWrap            )
//
function BigIntegerWrap(const bi : BigInteger) : IInterface;
begin
   Result := TBigIntegerWrapper.Create(bi) as IdwsBigInteger;
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
   data[offset] := IUnknown(nil);
end;

// ------------------
// ------------------ TBigIntegerWrapper ------------------
// ------------------

// Create
//
constructor TBigIntegerWrapper.Create(const aBigInteger : BigInteger);
begin
   FData := aBigInteger;
end;

// GetValue
//
function TBigIntegerWrapper.GetValue : BigInteger;
begin
   Result := FData;
end;

// SetValue
//
procedure TBigIntegerWrapper.SetValue(const v : BigInteger);
begin
   FData := v;
end;

// GetSelf
//
function TBigIntegerWrapper.GetSelf : TObject;
begin
   Result := Self;
end;

// ToString
//
function TBigIntegerWrapper.ToString : String;
begin
   Result := FData.ToString;
end;

// ------------------
// ------------------ TBigIntegerOpExpr ------------------
// ------------------

// Create
//
constructor TBigIntegerOpExpr.Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr);
begin
   inherited;
   if aLeft.Typ.UnAliasedTypeIs(TBaseIntegerSymbol) then
      Typ := aRight.Typ
   else Typ := aLeft.Typ;
end;

// ------------------
// ------------------ TBigIntegerAddOpExpr ------------------
// ------------------

procedure TBigIntegerAddOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   result := BigIntegerWrap(Left.EvalAsBigInteger(exec) + Right.EvalAsBigInteger(exec));
end;

// ------------------
// ------------------ TBigIntegerSubOpExpr ------------------
// ------------------

procedure TBigIntegerSubOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   result := BigIntegerWrap(Left.EvalAsBigInteger(exec) - Right.EvalAsBigInteger(exec));
end;

// ------------------
// ------------------ TBigIntegerMultOpExpr ------------------
// ------------------

procedure TBigIntegerMultOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   result := BigIntegerWrap(Left.EvalAsBigInteger(exec) * Right.EvalAsBigInteger(exec));
end;

// ------------------
// ------------------ TBigIntegerDivOpExpr ------------------
// ------------------

procedure TBigIntegerDivOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   result := BigIntegerWrap(Left.EvalAsBigInteger(exec) div Right.EvalAsBigInteger(exec));
end;

// ------------------
// ------------------ TBigIntegerModOpExpr ------------------
// ------------------

procedure TBigIntegerModOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   result := BigIntegerWrap(Left.EvalAsBigInteger(exec) mod Right.EvalAsBigInteger(exec));
end;

// ------------------
// ------------------ TBigIntegerRelOpExpr ------------------
// ------------------

// Optimize
//
function TBigIntegerRelOpExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
type
   TRelOpConverter = record
      ro : TBigIntegerRelOpExprClass;
      opR, opL : TTokenType;
   end;
   PRelOpConverter = ^TRelOpConverter;

const
   cRelOpConverters : array [0..5] of TRelOpConverter = (
         ( ro : TBigIntegerEqualOpExpr;         opR : ttEQ;       opL : ttEQ     ),
         ( ro : TBigIntegerNotEqualOpExpr;      opR : ttNOTEQ;    opL : ttNOTEQ  ),
         ( ro : TBigIntegerGreaterOpExpr;       opR : ttGTR;      opL : ttLESS   ),
         ( ro : TBigIntegerGreaterEqualOpExpr;  opR : ttGTREQ;    opL : ttLESSEQ ),
         ( ro : TBigIntegerLessOpExpr;          opR : ttLESS;     opL : ttGTR    ),
         ( ro : TBigIntegerLessEqualOpExpr;     opR : ttLESSEQ;   opL : ttGTREQ  )
      );

   function IsZero(expr : TTypedExpr) : Boolean;
   begin
      Result := (expr.ClassType = TConstIntExpr) and (TConstIntExpr(expr).Value = 0);
   end;

   function RelOpConverter : PRelOpConverter;
   var
      ct : TClass;
      i : Integer;
   begin
      ct := ClassType;
      for i := 0 to High(cRelOpConverters) do begin
         Result := @cRelOpConverters[i];
         if Result.ro = ct then Exit;
      end;
      raise Exception.Create('Unsupported rel op');
   end;

begin
   if IsZero(Left) and Right.Typ.UnAliasedTypeIs(TBaseBigIntegerSymbol) then begin
      Result := TBigIntegerCompareZeroExpr.Create(prog, Right, RelOpConverter.opL);
      FRight := nil;
      Free;
      Exit;
   end else if IsZero(Right) and Left.Typ.UnAliasedTypeIs(TBaseBigIntegerSymbol) then begin
      Result := TBigIntegerCompareZeroExpr.Create(prog, Left, RelOpConverter.opR);
      FLeft := nil;
      Free;
   end else Result := Self;
end;

// ------------------
// ------------------ TBigIntegerEqualOpExpr ------------------
// ------------------

function TBigIntegerEqualOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := Left.EvalAsBigInteger(exec) = Right.EvalAsBigInteger(exec);
end;

// ------------------
// ------------------ TBigIntegerNotEqualOpExpr ------------------
// ------------------

function TBigIntegerNotEqualOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := Left.EvalAsBigInteger(exec) <> Right.EvalAsBigInteger(exec);
end;

// ------------------
// ------------------ TBigIntegerGreaterOpExpr ------------------
// ------------------

function TBigIntegerGreaterOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := Left.EvalAsBigInteger(exec) > Right.EvalAsBigInteger(exec);
end;

// ------------------
// ------------------ TBigIntegerGreaterEqualOpExpr ------------------
// ------------------

function TBigIntegerGreaterEqualOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := Left.EvalAsBigInteger(exec) >= Right.EvalAsBigInteger(exec);
end;

// ------------------
// ------------------ TBigIntegerLessOpExpr ------------------
// ------------------

function TBigIntegerLessOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := Left.EvalAsBigInteger(exec) < Right.EvalAsBigInteger(exec);
end;

// ------------------
// ------------------ TBigIntegerLessEqualOpExpr ------------------
// ------------------

function TBigIntegerLessEqualOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := Left.EvalAsBigInteger(exec) <= Right.EvalAsBigInteger(exec);
end;

// ------------------
// ------------------ TBigIntegerCompareZeroExpr ------------------
// ------------------

// Create
//
constructor TBigIntegerCompareZeroExpr.Create(prog : TdwsProgram; expr : TTypedExpr; op : TTokenType);
begin
   inherited Create(prog, expr);
   FOp := op;
end;

// EvalAsBoolean
//
function TBigIntegerCompareZeroExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   bi : BigInteger;
begin
   bi := Expr.EvalAsBigInteger(exec);
   case FOp of
      ttEQ :      Result := bi.IsZero;
      ttNOTEQ :   Result := not bi.IsZero;
      ttGTR :     Result := bi.IsPositive;
      ttGTREQ  :  Result := bi.IsPositive or bi.IsZero;
      ttLESS :    Result := bi.IsNegative;
      ttLESSEQ :  Result := bi.IsNegative or bi.IsZero;
   else
      Assert(False);
      Result := False;
   end;
end;

// ------------------
// ------------------ TBigIntegerUnaryOpExpr ------------------
// ------------------

constructor TBigIntegerUnaryOpExpr.Create(prog : TdwsProgram; expr : TTypedExpr);
begin
   inherited Create(prog, expr);
   Typ := prog.Root.SystemTable.SymbolTable.FindTypeSymbol(SYS_BIGINTEGER, cvMagic);
end;

// ------------------
// ------------------ TConvIntegerToBigIntegerExpr ------------------
// ------------------

procedure TConvIntegerToBigIntegerExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   result := BigIntegerWrap( BigInteger( Expr.EvalAsInteger(exec) ) );
end;

// ------------------
// ------------------ TConvStringToBigIntegerExpr ------------------
// ------------------

procedure TConvStringToBigIntegerExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   s : String;
begin
   Expr.EvalAsString(exec, s);
   result := BigIntegerWrap( BigInteger.Parse( s, 10 ) );
end;

// ------------------
// ------------------ TConvBigIntegerToIntegerExpr ------------------
// ------------------

function TConvBigIntegerToIntegerExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   result := Expr.EvalAsBigInteger(exec).AsInt64;
end;

// ------------------
// ------------------ TConvBigIntegerToFloatExpr ------------------
// ------------------

function TConvBigIntegerToFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   result := Expr.EvalAsBigInteger(exec).AsDouble;
end;

// ------------------
// ------------------ TBigIntegerToStringFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TBigIntegerToStringFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   Result := ArgBigInteger(args, 0).ToString(args.AsInteger[1]);
end;

// ------------------
// ------------------ TStringToBigIntegerFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TStringToBigIntegerFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   result := BigIntegerWrap( BigInteger.Parse( args.AsString[0], args.AsInteger[1] ) );
end;

// ------------------
// ------------------ TBigIntegerToHexFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TBigIntegerToHexFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   Result := ArgBigInteger(args, 0).ToHexString;
end;

// ------------------
// ------------------ THexToBigIntegerFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure THexToBigIntegerFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   result := BigIntegerWrap( BigInteger.Parse( args.AsString[0], 16 ) );
end;

// ------------------
// ------------------ TBigIntegerToFloatFunc ------------------
// ------------------

procedure TBigIntegerToFloatFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result := ArgBigInteger(args, 0).AsDouble;
end;

// ------------------
// ------------------ TBigIntegerToIntegerFunc ------------------
// ------------------

function TBigIntegerToIntegerFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := ArgBigInteger(args, 0).AsInt64;
end;

// ------------------
// ------------------ TBigIntegerOddFunc ------------------
// ------------------

function TBigIntegerOddFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result := not ArgBigInteger(args, 0).IsEven;
end;

// ------------------
// ------------------ TBigIntegerEvenFunc ------------------
// ------------------

function TBigIntegerEvenFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result := ArgBigInteger(args, 0).IsEven;
end;

// ------------------
// ------------------ TBigIntegerSignFunc ------------------
// ------------------

function TBigIntegerSignFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := ArgBigInteger(args, 0).Sign;
end;

// ------------------
// ------------------ TBigIntegerAbsExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TBigIntegerAbsExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   result := BigIntegerWrap( BigInteger.Abs(Expr.EvalAsBigInteger(exec)) );
end;

// ------------------
// ------------------ TBigIntegerGcdFunc ------------------
// ------------------

procedure TBigIntegerGcdFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   result := BigIntegerWrap( BigInteger.GreatestCommonDivisor(ArgBigInteger(args, 0), ArgBigInteger(args, 1)) );
end;

// ------------------
// ------------------ TBigIntegerPowerFunc ------------------
// ------------------

procedure TBigIntegerPowerFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   result := BigIntegerWrap( BigInteger.Pow(ArgBigInteger(args, 0), args.AsInteger[1]) );
end;

// ------------------
// ------------------ TBigIntegerSqrFunc ------------------
// ------------------

procedure TBigIntegerSqrFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   result := BigIntegerWrap( BigInteger.Sqr(ArgBigInteger(args, 0)) );
end;

// ------------------
// ------------------ TBigIntegerDivModFunc ------------------
// ------------------

procedure TBigIntegerDivModFunc.DoEvalProc(const args : TExprBaseListExec);
var
   q, r : BigInteger;
begin
   BigInteger.DivMod(ArgBigInteger(args, 0), ArgBigInteger(args, 1), q, r);
   args.ExprBase[2].AssignValue(args.Exec, BigIntegerWrap(q));
   args.ExprBase[3].AssignValue(args.Exec, BigIntegerWrap(r));
end;

// ------------------
// ------------------ TBigIntegerToBlobFunc ------------------
// ------------------

procedure TBigIntegerToBlobFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bufBytes : TArray<Byte>;
   bufString : RawByteString;
   pSrc, pDest : PByte;
   i, n : Integer;
begin
   bufBytes := ArgBigInteger(args, 0).ToByteArray;
   n := Length(bufBytes);
   SetLength(bufString, n);
   pSrc := @bufBytes[n-1];
   pDest := Pointer(bufString);
   for i := 1 to n do begin
      pDest^ := pSrc^;
      Inc(pDest);
      Dec(pSrc);
   end;
   Result := bufString;
end;

// ------------------
// ------------------ TBlobToBigIntegerFunc ------------------
// ------------------

procedure TBlobToBigIntegerFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bufBytes : TArray<Byte>;
   bufString : String;
   pSrc : PChar;
   pDest : PByte;
   i, n : Integer;
begin
   bufString := args.AsString[0];
   n := Length(bufString);
   SetLength(bufBytes, n);
   pDest := @bufBytes[n-1];
   pSrc := Pointer(bufString);
   for i := 1 to n do begin
      pDest^ := Byte(pSrc^);
      Dec(pDest);
      Inc(pSrc);
   end;
   Result := BigIntegerWrap( BigInteger.Create(bufBytes) );
end;

// ------------------
// ------------------ TBigIntegerShiftLeftExpr ------------------
// ------------------

procedure TBigIntegerShiftLeftExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   result := BigIntegerWrap( Left.EvalAsBigInteger(exec) shl Right.EvalAsInteger(exec) );
end;

// ------------------
// ------------------ TBigIntegerShiftRightExpr ------------------
// ------------------

procedure TBigIntegerShiftRightExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   result := BigIntegerWrap( Left.EvalAsBigInteger(exec) shr Right.EvalAsInteger(exec) );
end;

// ------------------
// ------------------ TBigIntegerOpAssignExpr ------------------
// ------------------

procedure TBigIntegerOpAssignExpr.TypeCheckAssign(prog : TdwsProgram; exec : TdwsExecution);
begin
   // nothing here
end;

// ------------------
// ------------------ TBigIntegerPlusAssignExpr ------------------
// ------------------

procedure TBigIntegerPlusAssignExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValue(exec, BigIntegerWrap( FLeft.EvalAsBigInteger(exec) + FRight.EvalAsBigInteger(exec) ));
end;

// ------------------
// ------------------ TBigIntegerMinusAssignExpr ------------------
// ------------------

procedure TBigIntegerMinusAssignExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValue(exec, BigIntegerWrap( FLeft.EvalAsBigInteger(exec) - FRight.EvalAsBigInteger(exec) ));
end;

// ------------------
// ------------------ TBigIntegerMultAssignExpr ------------------
// ------------------

procedure TBigIntegerMultAssignExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValue(exec, BigIntegerWrap( FLeft.EvalAsBigInteger(exec) * FRight.EvalAsBigInteger(exec) ));
end;

// ------------------
// ------------------ TBigIntegerRandomFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TBigIntegerRandomFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);

   function RandomBigIntegerOfBitLength(nb : Integer) : BigInteger;
   var
      mask : Integer;
      bytes : TBytes;
      rnd : RawByteString;
   begin
      // adapted from BigInteger.Create(NumBits: Integer; const Random: IRandom)
      // uses cryptographic random
      rnd := CryptographicRandom( (nb + 7) div 8 + 1 );
      Setlength(bytes, Length(rnd));
      System.Move(rnd[1], bytes[0], Length(rnd));

      // One byte too many was allocated, to get a top byte of 0, i.e. always positive.
      bytes[High(Bytes)] := 0;

      // Set bits above required bit length to 0.
      mask := $7F shr (7 - (nb and 7));
      bytes[High(bytes)-1] := bytes[High(bytes)-1] and mask;

      result := BigInteger.Create(bytes);
   end;

var
   bi, limit : BigInteger;
   bits : Integer;
begin
   limit := ArgBigInteger(args, 0);
   if limit.IsZero or limit.IsNegative or limit.IsOne then begin
      bi := BigInteger.Zero;
   end else begin
      bits := limit.BitLength;
      repeat
         bi := RandomBigIntegerOfBitLength(bits);
      until bi < limit;
   end;
   result := BigIntegerWrap( bi );
end;

// ------------------
// ------------------ TBigIntegerBitLengthFunc ------------------
// ------------------

function TBigIntegerBitLengthFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := ArgBigInteger(args, 0).BitLength;
end;

// ------------------
// ------------------ TBigIntegerModPowFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TBigIntegerModPowFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   Result := BigIntegerWrap(BigInteger.ModPow( ArgBigInteger(args, 0), ArgBigInteger(args, 1), ArgBigInteger(args, 2) ));
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddSymbolsRegistrationProc(RegisterBigIntegerType);
   dwsInternalUnit.AddOperatorsRegistrationProc(RegisterBigIntegerOperators);
   dwsInternalUnit.AddAbsHandler(HandleBigIntegerAbs);

   RegisterInternalStringFunction(TBigIntegerToStringFunc,  'BigIntegerToHex', ['v', SYS_BIGINTEGER, 'base=10', SYS_INTEGER], [iffStateLess], 'ToString');
   RegisterInternalFunction(TStringToBigIntegerFunc,        'StringToBigInteger', ['s', SYS_STRING, 'base=10', SYS_INTEGER], SYS_BIGINTEGER, [iffStateLess], 'ToBigInteger');
   RegisterInternalStringFunction(TBigIntegerToHexFunc,     'BigIntegerToHex', ['v', SYS_BIGINTEGER], [iffStateLess], 'ToHex');
   RegisterInternalFunction(THexToBigIntegerFunc,           'HexToBigInteger', ['h', SYS_STRING], SYS_BIGINTEGER, [iffStateLess], 'HexToBigInteger');

   RegisterInternalFunction(TBigIntegerToBlobFunc,          'BigIntegerToBlobParameter', ['v', SYS_BIGINTEGER], SYS_VARIANT, [iffStateLess], 'ToBlobParameter');
   RegisterInternalFunction(TBlobToBigIntegerFunc,          'BlobFieldToBigInteger', ['b', SYS_STRING], SYS_BIGINTEGER, [iffStateLess]);

   RegisterInternalFloatFunction(TBigIntegerToFloatFunc,    '',   ['v', SYS_BIGINTEGER], [iffStateLess], 'ToFloat');
   RegisterInternalIntFunction(TBigIntegerToIntegerFunc,    '',   ['v', SYS_BIGINTEGER], [iffStateLess], 'ToInteger');

   RegisterInternalBoolFunction(TBigIntegerOddFunc,   'Odd',      ['i', SYS_BIGINTEGER], [iffStateLess, iffOverloaded], 'IsOdd');
   RegisterInternalBoolFunction(TBigIntegerEvenFunc,  'Even',     ['i', SYS_BIGINTEGER], [iffStateLess, iffOverloaded], 'IsEven');
   RegisterInternalIntFunction(TBigIntegerSignFunc,   'Sign',     ['v', SYS_BIGINTEGER], [iffStateLess, iffOverloaded], 'Sign');
   RegisterInternalIntFunction(TBigIntegerBitLengthFunc, '',      ['v', SYS_BIGINTEGER], [iffStateLess], 'BitLength');
   RegisterInternalFunction(TBigIntegerGcdFunc,       'Gcd',      ['a', SYS_BIGINTEGER, 'b', SYS_BIGINTEGER], SYS_BIGINTEGER, [iffStateLess, iffOverloaded]);
   RegisterInternalFunction(TBigIntegerPowerFunc,     'IntPower', ['base', SYS_BIGINTEGER, 'exponent', SYS_INTEGER], SYS_BIGINTEGER, [iffStateLess, iffOverloaded], 'Power');
   RegisterInternalFunction(TBigIntegerSqrFunc,       'Sqr',      ['v', SYS_BIGINTEGER], SYS_BIGINTEGER, [iffStateLess, iffOverloaded], 'Sqr');
   RegisterInternalProcedure(TBigIntegerDivModFunc,   'DivMod',
                             ['dividend', SYS_BIGINTEGER, 'divisor', SYS_BIGINTEGER,
                              '@result', SYS_BIGINTEGER, '@remainder', SYS_BIGINTEGER], '', [iffOverloaded]);
   RegisterInternalFunction(TBigIntegerModPowFunc,    'ModPow',   ['base', SYS_BIGINTEGER, 'exponent', SYS_BIGINTEGER, 'modulus', SYS_BIGINTEGER],
                                                                  SYS_BIGINTEGER, [iffStateLess, iffOverloaded], 'ModPow');
   RegisterInternalFunction(TBigIntegerModPowFunc,    'ModPow',   ['base', SYS_BIGINTEGER, 'exponent', SYS_INTEGER, 'modulus', SYS_BIGINTEGER],
                                                                  SYS_BIGINTEGER, [iffStateLess, iffOverloaded], 'ModPow');

   RegisterInternalFunction(TBigIntegerRandomFunc,   'RandomBigInteger', ['limitPlusOne', SYS_BIGINTEGER], SYS_BIGINTEGER);

end.

