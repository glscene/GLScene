//
// The graphics rendering engine GLScene http://glscene.org
//
unit DWS.Classes;

(* DelphiWebScript symbol creation for base Delphi classes. *)

interface

uses
  System.Classes,
  System.SysUtils,

  dwsExprs,
  dwsSymbols,
  dwsComp,
  dwsCompStrings,
  dwsStack,
  dwsFunctions,
  dwsHelperFunc;

type
  TdwsClassesUnit = class(TdwsUnitComponent)
  private
    procedure AddClassTPersistent(SymbolTable: TSymbolTable);
    procedure AddClassTComponent(SymbolTable: TSymbolTable);
  protected
    procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

// ===============================================================
implementation
// ===============================================================

// ----------
// ---------- Internal class method class declarations ----------
// ----------

type
  TPersistentAssignMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TPersistentGetNamePathMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentCreateMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentSetTagMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentGetTagMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentSetNameMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentGetNameMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentGetOwnerMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentSetComponentIndexMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentGetComponentIndexMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentGetComponentCountMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentGetComponentMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentFindComponentMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentFreeNotificationMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentRemoveFreeNotificationMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentGetParentComponentMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentGetNamePathMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TComponentHasParentMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;


// ----------
// ---------- Internal class method execute procedures ----------
// ----------

procedure TPersistentAssignMethod.Execute(var ExternalObject: TObject);
var
  Source: TObject;
begin
  ValidateExternalObject(ExternalObject, TPersistent);
  Source := Info.GetExternalObjForVar('Source');
  if not Assigned(Source) then
    raise Exception.Create('Source parameter is unassigned.');
  if not(Source is TPersistent) then
    Exception.Create('Source parameter is not inheriting from TPersistent.');
  TPersistent(ExternalObject).Assign(TPersistent(Source));
end;

procedure TPersistentGetNamePathMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TPersistent);
  Info.Result.Value := TPersistent(ExternalObject).GetNamePath;
end;

procedure TComponentCreateMethod.Execute(var ExternalObject: TObject);
var
  AOwner: TComponent;
begin
  AOwner := TComponent(Info.GetExternalObjForVar('AOwner'));
  ExternalObject := TComponent.Create(AOwner);
end;

procedure TComponentSetTagMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TComponent);
  TComponent(ExternalObject).Tag := Info['Value'];
end;

procedure TComponentGetTagMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TComponent);
  Info.Result := TComponent(ExternalObject).Tag;
end;

procedure TComponentSetNameMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TComponent);
  TComponent(ExternalObject).Name := Info['Value'];
end;

procedure TComponentGetNameMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TComponent);
  Info.Result := TComponent(ExternalObject).Name;
end;

procedure TComponentGetOwnerMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TComponent);
  Info.Result := Info.RegisterExternalObject(TComponent(ExternalObject).Owner,
    False, False);
end;

procedure TComponentSetComponentIndexMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TComponent);
  TComponent(ExternalObject).ComponentIndex := Info['Value'];
end;

procedure TComponentGetComponentIndexMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TComponent);
  Info.Result := TComponent(ExternalObject).ComponentIndex;
end;

procedure TComponentGetComponentCountMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TComponent);
  Info.Result := TComponent(ExternalObject).ComponentCount;
end;

procedure TComponentGetComponentMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TComponent);
  Info.Result := Info.RegisterExternalObject(TComponent(ExternalObject)
    .Components[Info['Index']], False, False);
end;

procedure TComponentFindComponentMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TComponent);
  Info.Result := Info.RegisterExternalObject(TComponent(ExternalObject)
    .FindComponent(Info['AName']), False, False);
end;

procedure TComponentFreeNotificationMethod.Execute(var ExternalObject: TObject);
var
  AComponent: TComponent;
begin
  ValidateExternalObject(ExternalObject, TComponent);
  AComponent := TComponent(Info.GetExternalObjForVar('AComponent'));
  if Assigned(AComponent) then
    TComponent(ExternalObject).FreeNotification(AComponent);
end;

procedure TComponentRemoveFreeNotificationMethod.Execute(var ExternalObject
  : TObject);
var
  AComponent: TComponent;
begin
  ValidateExternalObject(ExternalObject, TComponent);
  AComponent := TComponent(Info.GetExternalObjForVar('AComponent'));
  if Assigned(AComponent) then
    TComponent(ExternalObject).RemoveFreeNotification(AComponent);
end;

procedure TComponentGetParentComponentMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TComponent);
  Info.Result := Info.RegisterExternalObject(TComponent(ExternalObject)
    .GetParentComponent, False, False);
end;

procedure TComponentGetNamePathMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TComponent);
  Info.Result := TComponent(ExternalObject).GetNamePath;
end;

procedure TComponentHasParentMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TComponent);
  Info.Result := TComponent(ExternalObject).HasParent;
end;


// ----------
// ---------- Global procedures/functions ----------
// ----------

procedure Register;
begin
  RegisterComponents('GLScene DWS2', [Tdws2ClassesUnit]);
end;


// ----------
// ---------- TdwsClassesUnit ----------
// ----------

constructor TdwsClassesUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName := 'Classes';
end;

procedure TdwsClassesUnit.AddClassTPersistent(SymbolTable: TSymbolTable);
var
  ClassSym: TClassSymbol;
begin
  ClassSym := TClassSymbol(AddClassSymbol(SymbolTable, 'TPersistent',
    'TObject'));

  if not Assigned(ClassSym.Members.FindLocal('Assign')) then
    TPersistentAssignMethod.Create(mkProcedure, [maVirtual], 0, 'Assign',
      ['Source', 'TPersistent'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetNamePath')) then
    TPersistentGetNamePathMethod.Create(mkFunction, [maVirtual], 0,
      'GetNamePath', [], 'String', ClassSym, SymbolTable);
end;

procedure TdwsClassesUnit.AddClassTComponent(SymbolTable: TSymbolTable);
var
  ClassSym: TClassSymbol;
begin
  ClassSym := TClassSymbol(AddClassSymbol(SymbolTable, 'TComponent',
    'TPersistent'));

  // Methods
  if not Assigned(ClassSym.Members.FindLocal('Create')) then
    TComponentCreateMethod.Create(mkConstructor, [maVirtual], 0, 'Create',
      ['AOwner', 'TComponent'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetTag')) then
    TComponentSetTagMethod.Create(mkProcedure, [], 0, 'SetTag',
      ['Value', 'Integer'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetTag')) then
    TComponentGetTagMethod.Create(mkFunction, [], 0, 'GetTag', [], 'Integer',
      ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetName')) then
    TComponentSetNameMethod.Create(mkProcedure, [], 0, 'SetName',
      ['Value', 'String'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetName')) then
    TComponentGetNameMethod.Create(mkFunction, [], 0, 'GetName', [], 'String',
      ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetOwner')) then
    TComponentGetOwnerMethod.Create(mkFunction, [], 0, 'GetOwner', [],
      'TComponent', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetComponentIndex')) then
    TComponentSetComponentIndexMethod.Create(mkProcedure, [], 0,
      'SetComponentIndex', ['Value', 'Integer'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetComponentIndex')) then
    TComponentGetComponentIndexMethod.Create(mkFunction, [], 0,
      'GetComponentIndex', [], 'Integer', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetComponentCount')) then
    TComponentGetComponentCountMethod.Create(mkFunction, [], 0,
      'GetComponentCount', [], 'Integer', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetComponent')) then
    TComponentGetComponentMethod.Create(mkFunction, [], 0, 'GetComponent',
      ['Index', 'Integer'], 'TComponent', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('FindComponent')) then
    TComponentFindComponentMethod.Create(mkFunction, [], 0, 'FindComponent',
      ['AName', 'String'], 'TComponent', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('FreeNotification')) then
    TComponentFreeNotificationMethod.Create(mkProcedure, [], 0,
      'FreeNotification', ['AComponent', 'TComponent'], '', ClassSym,
      SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('RemoveFreeNotification')) then
    TComponentRemoveFreeNotificationMethod.Create(mkProcedure, [], 0,
      'RemoveFreeNotification', ['AComponent', 'TComponent'], '', ClassSym,
      SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetParentComponent')) then
    TComponentGetParentComponentMethod.Create(mkFunction, [maVirtual], 0,
      'GetParentComponent', [], 'TComponent', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetNamePath')) then
    TComponentGetNamePathMethod.Create(mkFunction, [maOverride], 0,
      'GetNamePath', [], 'String', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('HasParent')) then
    TComponentHasParentMethod.Create(mkFunction, [maVirtual], 0, 'HasParent',
      [], 'Boolean', ClassSym, SymbolTable);

  // Properties
  AddPropertyToClass('Tag', 'Integer', 'GetTag', 'SetTag', '', False, ClassSym,
    SymbolTable);
  AddPropertyToClass('Name', 'String', 'GetName', 'SetName', '', False,
    ClassSym, SymbolTable);
  AddPropertyToClass('Owner', 'TComponent', 'GetOwner', '', '', False, ClassSym,
    SymbolTable);
  AddPropertyToClass('ComponentIndex', 'Integer', 'GetComponentIndex',
    'SetComponentIndex', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Components', 'TComponent', 'GetComponent', 'SetComponent',
    'Integer', True, ClassSym, SymbolTable);
end;

procedure TdwsClassesUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
begin
  // Forward class declaration
  AddForwardDeclaration('TPersistent', SymbolTable);
  AddForwardDeclaration('TComponent', SymbolTable);

  // Class types
  AddClassTPersistent(SymbolTable);
  AddClassTComponent(SymbolTable);
end;

end.
