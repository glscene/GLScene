//
// The graphics rendering engine GLScene http://glscene.org
//
unit DWS.HelperFunc;

(*
  Helper functions for creating class, properties and
  method symbols in DelphiWebScript
*)

interface

uses
  System.Classes,
  System.SysUtils,
  dwsSymbols,
  dwsCompStrings;

procedure AddForwardDeclaration(ClassName: String; SymbolTable: TSymbolTable);
function AddClassSymbol(SymbolTable: TSymbolTable;
  Name, Ancestor: String): TSymbol;
procedure AddPropertyToClass(Name, DataType, ReadAccess, WriteAccess,
  IndexDataType: String; IsDefault: Boolean; ClassSym: TClassSymbol;
  Table: TSymbolTable);
procedure ValidateExternalObject(ExtObject: TObject; ObjClass: TClass);

// -------------------------------------------------------------------
implementation

// -------------------------------------------------------------------

procedure AddForwardDeclaration(ClassName: String; SymbolTable: TSymbolTable);
var
  Sym: TSymbol;
begin
  Sym := SymbolTable.FindSymbol(ClassName);
  if Assigned(Sym) then
    exit;
  Sym := TClassSymbol.Create(ClassName);
  TClassSymbol(Sym).IsForward := True;
  SymbolTable.AddSymbol(Sym);
end;

function AddClassSymbol(SymbolTable: TSymbolTable;
  Name, Ancestor: String): TSymbol;
var
  ancestorSym: TClassSymbol;
begin
  Result := SymbolTable.FindSymbol(Name);
  try
    if Assigned(Result) then
    begin
      if Result is TClassSymbol then
      begin
        if not(TClassSymbol(Result).IsForward) then
        begin
          exit;
        end;
      end
      else
      begin
        Result := nil;
        exit;
      end;
    end;

    try
      if not Assigned(Result) then
        Result := TClassSymbol.Create(Name);
      ancestorSym := TClassSymbol(SymbolTable.FindSymbol(Ancestor));
      if ancestorSym = nil then
        raise Exception.CreateFmt(UNT_SuperClassUnknwon, [Ancestor]);
      TClassSymbol(Result).InheritFrom(ancestorSym);
    except
      if not TClassSymbol(Result).IsForward then
        FreeAndNil(Result);
      raise;
    end;

    if TClassSymbol(Result).IsForward then
      TClassSymbol(Result).IsForward := False
    else
      SymbolTable.AddSymbol(Result);

  finally
    if not Assigned(Result) then
      raise Exception.CreateFmt('Unable to add %s to the symbol table', [Name]);
  end;
end;

procedure AddPropertyToClass(Name, DataType, ReadAccess, WriteAccess,
  IndexDataType: String; IsDefault: Boolean; ClassSym: TClassSymbol;
  Table: TSymbolTable);
var
  Sym: TSymbol;
  ParamSym: TParamSymbol;
  PropertySym: TPropertySymbol;
begin
  if Assigned(ClassSym.Members.FindLocal(Name)) then
    exit;
  Sym := Table.FindSymbol(DataType);
  PropertySym := TPropertySymbol.Create(Name, Sym);
  if ReadAccess <> '' then
    PropertySym.ReadSym := ClassSym.Members.FindLocal(ReadAccess);
  if WriteAccess <> '' then
    PropertySym.WriteSym := ClassSym.Members.FindLocal(WriteAccess);
  if IndexDataType <> '' then
  begin
    Sym := Table.FindSymbol(IndexDataType);
    ParamSym := TParamSymbol.Create('Index', Sym);
    PropertySym.ArrayIndices.AddSymbol(ParamSym);
  end;
  ClassSym.AddProperty(PropertySym);
  if IsDefault then
    ClassSym.DefaultProperty := PropertySym;
end;

procedure ValidateExternalObject(ExtObject: TObject; ObjClass: TClass);
var
  Valid: Boolean;
begin
  if Assigned(ExtObject) then
    Valid := (ExtObject is ObjClass)
  else
    Valid := False;
  if not Valid then
    raise Exception.Create('Invalid external object.');
end;

end.
