//
// The graphics rendering engine GLScene http://glscene.org
//
unit DWS.Scene;
(*
  DelphiWebScript symbol creation for base GLScene classes.

  This unit is dependent on dws2Classes and dws2VectorGeometry.
  These components must be associated with the same compiler
  for the GLScene classes to inherit from.
*)
interface

uses
  System.Classes,
  System.SysUtils,

  GLS.Scene,
  GLS.VectorGeometry,
  GLS.Coordinates,

  DWS.HelperFunc,
  dwsExprs,
  dwsSymbols,
  dwsComp,
  dwsCompStrings,
  dwsStack,
  dwsFunctions;

type
  TdwsGLSceneUnit = class(TdwsUnitComponent)
  private
    procedure AddClassTGLCoordinates(SymbolTable: TSymbolTable);
    procedure AddClassTGLBaseSceneObject(SymbolTable: TSymbolTable);
  protected
    procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

// --------------------------------------------------------
implementation
// --------------------------------------------------------

// ----------
// ---------- Internal class method class declarations ----------
// ----------

type

// --------------------------
// -------- TGLCoordinates
// --------------------------

  TGLCoordinatesSetXMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesGetXMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesSetYMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesGetYMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesSetZMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesGetZMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesSetWMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesGetWMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesSetVectorMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesSetPointMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesSetToZeroMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesSetAsVectorMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesGetAsVectorMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesGetAsStringMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesTranslateMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesAddScaledVectorMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesRotateMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesNormalizeMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesInvertMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesScaleMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLCoordinatesEqualsMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  // --------------------------
  // TGLBaseSceneObject
  // --------------------------
  TGLBaseSceneObjectSetVisibleMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectGetVisibleMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectSetMatrixMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectGetMatrixMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectAbsoluteMatrixMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectInvAbsoluteMatrixMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectSetAbsolutePositionMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectGetAbsolutePositionMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectSetAbsoluteUpMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectGetAbsoluteUpMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectSetAbsoluteDirectionMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectGetAbsoluteDirectionMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectSetPositionMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectGetPositionMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectSetDirectionMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectGetDirectionMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectSetUpMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectGetUpMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectSetScaleMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectGetScaleMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectSetPitchAngleMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectGetPitchAngleMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectSetTurnAngleMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectGetTurnAngleMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectSetRollAngleMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectGetRollAngleMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectPitchMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectTurnMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectRollMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectMoveMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TGLBaseSceneObjectAddChildMethod = class(TInternalMethod)
  public
    procedure Execute(var ExternalObject: TObject); override;
  end;


  // ----------
  // ---------- Vector/Matrix to/from IInfo helper functions ----------
  // ----------

function GetVectorFromInfo(Info: IInfo): TGLVector;
begin
  Result := VectorMake(Info.Element([0]).Value, Info.Element([1]).Value,
    Info.Element([2]).Value, Info.Element([3]).Value);
end;

procedure SetInfoFromVector(Info: IInfo; vec: TGLVector);
var
  i: Integer;
begin
  for i := 0 to 3 do
    Info.Element([i]).Value := vec[i];
end;

function GetMatrixFromInfo(Info: IInfo): TGLMatrix;
var
  i: Integer;
begin
  for i := 0 to 3 do
    Result[i] := VectorMake(Info.Element([i]).Element([0]).Value,
      Info.Element([i]).Element([1]).Value, Info.Element([i]).Element([2])
      .Value, Info.Element([i]).Element([3]).Value);
end;

procedure SetInfoFromMatrix(Info: IInfo; mat: TGLMatrix);
var
  i, j: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Info.Element([i]).Element([j]).Value := mat[i][j];
end;


// ----------
// ---------- Internal class method execute procedures ----------
// ----------

// TGLCoordinates internal class methods

// TGLCoordinates.X write access
procedure TGLCoordinatesSetXMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  TGLCoordinates(ExternalObject).X := Info['Value'];
end;

// TGLCoordinates.X read access
procedure TGLCoordinatesGetXMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  Info.Result := TGLCoordinates(ExternalObject).X;
end;

// TGLCoordinates.Y write access
procedure TGLCoordinatesSetYMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  TGLCoordinates(ExternalObject).Y := Info['Value'];
end;

// TGLCoordinates.Y read access
procedure TGLCoordinatesGetYMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  Info.Result := TGLCoordinates(ExternalObject).Y;
end;

// TGLCoordinates.Z write access
procedure TGLCoordinatesSetZMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  TGLCoordinates(ExternalObject).Z := Info['Value'];
end;

// TGLCoordinates.Z read access
procedure TGLCoordinatesGetZMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  Info.Result := TGLCoordinates(ExternalObject).Z;
end;

// TGLCoordinates.W write access
procedure TGLCoordinatesSetWMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  TGLCoordinates(ExternalObject).W := Info['Value'];
end;

// TGLCoordinates.W read access
procedure TGLCoordinatesGetWMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  Info.Result := TGLCoordinates(ExternalObject).W;
end;

// TGLCoordinates.SetVector
procedure TGLCoordinatesSetVectorMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  TGLCoordinates(ExternalObject).SetVector(Info['x'], Info['y'], Info['z'],
    Info['w']);
end;

// TGLCoordinates.SetPoint
procedure TGLCoordinatesSetPointMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  TGLCoordinates(ExternalObject).SetPoint(Info['x'], Info['y'], Info['z']);
end;

// TGLCoordinates.AsVector write access
procedure TGLCoordinatesSetAsVectorMethod.Execute(var ExternalObject: TObject);
var
  v: TGLVector;
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  v := GetVectorFromInfo(Info.Vars['Value']);
  TGLCoordinates(ExternalObject).AsVector := v;
end;

// TGLCoordinates.AsVector read access
procedure TGLCoordinatesGetAsVectorMethod.Execute(var ExternalObject: TObject);
var
  v: TGLVector;
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  v := TGLCoordinates(ExternalObject).AsVector;
  SetInfoFromVector(Info.Vars['Result'], v);
end;

// TGLCoordinates.AsString read access
procedure TGLCoordinatesGetAsStringMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  Info.Result := TGLCoordinates(ExternalObject).AsString;
end;

// TGLCoordinates.Translate
procedure TGLCoordinatesTranslateMethod.Execute(var ExternalObject: TObject);
var
  v: TGLVector;
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  v := GetVectorFromInfo(Info.Vars['translationVector']);
  TGLCoordinates(ExternalObject).Translate(v);
end;

// TGLCoordinates.AddScaledVector
procedure TGLCoordinatesAddScaledVectorMethod.Execute(var ExternalObject
  : TObject);
var
  v: TGLVector;
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  v := GetVectorFromInfo(Info.Vars['translationVector']);
  TGLCoordinates(ExternalObject).AddScaledVector(Info['factor'], v);
end;

// TGLCoordinates.Rotate
procedure TGLCoordinatesRotateMethod.Execute(var ExternalObject: TObject);
var
  v: TGLVector;
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  v := GetVectorFromInfo(Info.Vars['anAxis']);
  TGLCoordinates(ExternalObject).Rotate(v, Info['anAngle']);
end;

// TGLCoordinates.Normalize
procedure TGLCoordinatesNormalizeMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  TGLCoordinates(ExternalObject).Normalize;
end;

// TGLCoordinates.Invert
procedure TGLCoordinatesInvertMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  TGLCoordinates(ExternalObject).Invert;
end;

// TGLCoordinates.Scale
procedure TGLCoordinatesScaleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  TGLCoordinates(ExternalObject).Scale(Info['factor']);
end;

// TGLCoordinates.Equals
procedure TGLCoordinatesEqualsMethod.Execute(var ExternalObject: TObject);
var
  v: TGLVector;
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  v := GetVectorFromInfo(Info.Vars['aVector']);
  Info.Result := TGLCoordinates(ExternalObject).Equals(v);
end;

// TGLCoordinates.SetToZero
procedure TGLCoordinatesSetToZeroMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLCoordinates);
  TGLCoordinates(ExternalObject).SetToZero;
end;


// TGLBaseSceneObject internal class methods

// TGLBaseSceneObject.SetVisible
procedure TGLBaseSceneObjectSetVisibleMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  TGLBaseSceneObject(ExternalObject).Visible := Info.Vars['Value'].Value;
end;

// TGLBaseSceneObject.GetVisible
procedure TGLBaseSceneObjectGetVisibleMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  Info.Result := TGLBaseSceneObject(ExternalObject).Visible;
end;

// TGLBaseSceneObject.SetMatrix
procedure TGLBaseSceneObjectSetMatrixMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  TGLBaseSceneObject(ExternalObject).Matrix :=
    GetMatrixFromInfo(Info.Vars['Value']);
end;

// TGLBaseSceneObject.GetMatrix
procedure TGLBaseSceneObjectGetMatrixMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  SetInfoFromMatrix(Info.Vars['Result'],
    TGLBaseSceneObject(ExternalObject).Matrix);
end;

// TGLBaseSceneObject.AbsoluteMatrix
procedure TGLBaseSceneObjectAbsoluteMatrixMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  SetInfoFromMatrix(Info.Vars['Result'], TGLBaseSceneObject(ExternalObject)
    .AbsoluteMatrix);
end;

// TGLBaseSceneObject.InvAbsoluteMatrix
procedure TGLBaseSceneObjectInvAbsoluteMatrixMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  SetInfoFromMatrix(Info.Vars['Result'], TGLBaseSceneObject(ExternalObject)
    .InvAbsoluteMatrix);
end;

// TGLBaseSceneObject.SetAbsolutePosition
procedure TGLBaseSceneObjectSetAbsolutePositionMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  TGLBaseSceneObject(ExternalObject).AbsolutePosition :=
    GetVectorFromInfo(Info.Vars['Value']);
end;

// TGLBaseSceneObject.GetAbsolutePosition
procedure TGLBaseSceneObjectGetAbsolutePositionMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  SetInfoFromVector(Info.Vars['Result'], TGLBaseSceneObject(ExternalObject)
    .AbsolutePosition);
end;

// TGLBaseSceneObject.SetAbsoluteUp
procedure TGLBaseSceneObjectSetAbsoluteUpMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  TGLBaseSceneObject(ExternalObject).AbsoluteUp :=
    GetVectorFromInfo(Info.Vars['Value']);
end;

// TGLBaseSceneObject.GetAbsoluteUp
procedure TGLBaseSceneObjectGetAbsoluteUpMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  SetInfoFromVector(Info.Vars['Result'], TGLBaseSceneObject(ExternalObject)
    .AbsoluteUp);
end;

// TGLBaseSceneObject.SetAbsoluteDirection
procedure TGLBaseSceneObjectSetAbsoluteDirectionMethod.Execute
  (var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  TGLBaseSceneObject(ExternalObject).AbsoluteDirection :=
    GetVectorFromInfo(Info.Vars['Value']);
end;

// TGLBaseSceneObject.GetAbsoluteDirection
procedure TGLBaseSceneObjectGetAbsoluteDirectionMethod.Execute
  (var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  SetInfoFromVector(Info.Vars['Result'], TGLBaseSceneObject(ExternalObject)
    .AbsoluteDirection);
end;

// TGLBaseSceneObject.Position write access
procedure TGLBaseSceneObjectSetPositionMethod.Execute(var ExternalObject
  : TObject);
var
  Value: TGLCoordinates;
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  Value := TGLCoordinates(Info.GetExternalObjForVar('Value'));
  TGLBaseSceneObject(ExternalObject).Position := Value;
end;

// TGLBaseSceneObject.Position read access
procedure TGLBaseSceneObjectGetPositionMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  Info.Result := Info.RegisterExternalObject(TGLBaseSceneObject(ExternalObject)
    .Position);
end;

// TGLBaseSceneObject.Direction write access
procedure TGLBaseSceneObjectSetDirectionMethod.Execute(var ExternalObject
  : TObject);
var
  Value: TGLCoordinates;
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  Value := TGLCoordinates(Info.GetExternalObjForVar('Value'));
  TGLBaseSceneObject(ExternalObject).Direction := Value;
end;

// TGLBaseSceneObject.Direction read access
procedure TGLBaseSceneObjectGetDirectionMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  Info.Result := Info.RegisterExternalObject(TGLBaseSceneObject(ExternalObject)
    .Direction);
end;

// TGLBaseSceneObject.Up write access
procedure TGLBaseSceneObjectSetUpMethod.Execute(var ExternalObject: TObject);
var
  Value: TGLCoordinates;
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  Value := TGLCoordinates(Info.GetExternalObjForVar('Value'));
  TGLBaseSceneObject(ExternalObject).Up := Value;
end;

// TGLBaseSceneObject.Up read access
procedure TGLBaseSceneObjectGetUpMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  Info.Result := Info.RegisterExternalObject
    (TGLBaseSceneObject(ExternalObject).Up);
end;

// TGLBaseSceneObject.Scale write access
procedure TGLBaseSceneObjectSetScaleMethod.Execute(var ExternalObject: TObject);
var
  Value: TGLCoordinates;
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  Value := TGLCoordinates(Info.GetExternalObjForVar('Value'));
  TGLBaseSceneObject(ExternalObject).Scale := Value;
end;

// TGLBaseSceneObject.Scale read access
procedure TGLBaseSceneObjectGetScaleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  Info.Result := Info.RegisterExternalObject
    (TGLBaseSceneObject(ExternalObject).Scale);
end;

// TGLBaseSceneObject.PitchAngle write access
procedure TGLBaseSceneObjectSetPitchAngleMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  TGLBaseSceneObject(ExternalObject).PitchAngle := Info.Vars['Value'].Value;
end;

// TGLBaseSceneObject.PitchAngle read access
procedure TGLBaseSceneObjectGetPitchAngleMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  Info.Result := TGLBaseSceneObject(ExternalObject).PitchAngle;
end;

// TGLBaseSceneObject.TurnAngle write access
procedure TGLBaseSceneObjectSetTurnAngleMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  TGLBaseSceneObject(ExternalObject).TurnAngle := Info.Vars['Value'].Value;
end;

// TGLBaseSceneObject.TurnAngle read access
procedure TGLBaseSceneObjectGetTurnAngleMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  Info.Result := TGLBaseSceneObject(ExternalObject).TurnAngle;
end;

// TGLBaseSceneObject.RollAngle write access
procedure TGLBaseSceneObjectSetRollAngleMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  TGLBaseSceneObject(ExternalObject).RollAngle := Info.Vars['Value'].Value;
end;

// TGLBaseSceneObject.RollAngle read access
procedure TGLBaseSceneObjectGetRollAngleMethod.Execute(var ExternalObject
  : TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  Info.Result := TGLBaseSceneObject(ExternalObject).RollAngle;
end;

// TGLBaseSceneObject.Pitch
procedure TGLBaseSceneObjectPitchMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  TGLBaseSceneObject(ExternalObject).Pitch(Info['angle']);
end;

// TGLBaseSceneObject.Turn
procedure TGLBaseSceneObjectTurnMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  TGLBaseSceneObject(ExternalObject).Turn(Info['angle']);
end;

// TGLBaseSceneObject.Roll
procedure TGLBaseSceneObjectRollMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  TGLBaseSceneObject(ExternalObject).Roll(Info['angle']);
end;

// TGLBaseSceneObject.Move
procedure TGLBaseSceneObjectMoveMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  TGLBaseSceneObject(ExternalObject).Move(Info['ADistance']);
end;

// TGLBaseSceneObject.AddChild
procedure TGLBaseSceneObjectAddChildMethod.Execute(var ExternalObject: TObject);
var
  AChild: TObject;
begin
  ValidateExternalObject(ExternalObject, TGLBaseSceneObject);
  AChild := Info.GetExternalObjForVar('AChild');
  if not Assigned(AChild) then
    raise Exception.Create('AChild parameter is unassigned.');
  if not(AChild is TGLBaseSceneObject) then
    Exception.Create
      ('AChild parameter is not inheriting from TGLBaseSceneObject.');
  TGLBaseSceneObject(ExternalObject).AddChild(TGLBaseSceneObject(AChild));
end;

// ----------
// ---------- Global procedures/functions ----------
// ----------
procedure Register;
begin
  RegisterComponents('GLScene DWS2', [Tdws2GLSceneUnit]);
end;

// ----------
// ---------- TdwsGLSceneUnit ----------
// ----------
constructor TdwsGLSceneUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName := 'GLScene';
  with FDependencies do
  begin
    Add('Classes');
    Add('GLS.VectorGeometry');
  end;
end;

procedure TdwsGLSceneUnit.AddClassTGLCoordinates(SymbolTable: TSymbolTable);
var
  ClassSym: TClassSymbol;
begin
  ClassSym := TClassSymbol(AddClassSymbol(SymbolTable, 'TGLCoordinates',
    'TPersistent'));

  // Methods
  if not Assigned(ClassSym.Members.FindLocal('SetX')) then
    TGLCoordinatesSetXMethod.Create(mkProcedure, [], 0, 'SetX',
      ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetX')) then
    TGLCoordinatesGetXMethod.Create(mkFunction, [], 0, 'GetX', [], 'Float',
      ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetY')) then
    TGLCoordinatesSetYMethod.Create(mkProcedure, [], 0, 'SetY',
      ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetY')) then
    TGLCoordinatesGetYMethod.Create(mkFunction, [], 0, 'GetY', [], 'Float',
      ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetZ')) then
    TGLCoordinatesSetZMethod.Create(mkProcedure, [], 0, 'SetZ',
      ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetZ')) then
    TGLCoordinatesGetZMethod.Create(mkFunction, [], 0, 'GetZ', [], 'Float',
      ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetW')) then
    TGLCoordinatesSetWMethod.Create(mkProcedure, [], 0, 'SetW',
      ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetW')) then
    TGLCoordinatesGetWMethod.Create(mkFunction, [], 0, 'GetW', [], 'Float',
      ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetVector')) then
    TGLCoordinatesSetVectorMethod.Create(mkProcedure, [], 0, 'SetVector',
      ['x', 'Float', 'y', 'Float', 'z', 'Float', 'w', 'Float'], '', ClassSym,
      SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetPoint')) then
    TGLCoordinatesSetPointMethod.Create(mkProcedure, [], 0, 'SetPoint',
      ['x', 'Float', 'y', 'Float', 'z', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetToZero')) then
    TGLCoordinatesSetToZeroMethod.Create(mkProcedure, [], 0, 'SetToZero', [],
      '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAsVector')) then
    TGLCoordinatesSetAsVectorMethod.Create(mkProcedure, [], 0, 'SetAsVector',
      ['Value', 'TGLVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAsVector')) then
    TGLCoordinatesGetAsVectorMethod.Create(mkFunction, [], 0, 'GetAsVector', [],
      'TGLVector', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAsString')) then
    TGLCoordinatesGetAsStringMethod.Create(mkFunction, [], 0, 'GetAsString', [],
      'String', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Translate')) then
    TGLCoordinatesTranslateMethod.Create(mkProcedure, [], 0, 'Translate',
      ['translationVector', 'TGLVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('AddScaledVector')) then
    TGLCoordinatesAddScaledVectorMethod.Create(mkProcedure, [], 0,
      'AddScaledVector', ['factor', 'Float', 'translationVector', 'TGLVector'],
      '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Rotate')) then
    TGLCoordinatesRotateMethod.Create(mkProcedure, [], 0, 'Rotate',
      ['anAxis', 'TGLVector', 'anAngle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Normalize')) then
    TGLCoordinatesNormalizeMethod.Create(mkProcedure, [], 0, 'Normalize', [],
      '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Invert')) then
    TGLCoordinatesInvertMethod.Create(mkProcedure, [], 0, 'Invert', [], '',
      ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Scale')) then
    TGLCoordinatesScaleMethod.Create(mkProcedure, [], 0, 'Scale',
      ['factor', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Equals')) then
    TGLCoordinatesEqualsMethod.Create(mkFunction, [], 0, 'Equals',
      ['aVector', 'TGLVector'], 'Boolean', ClassSym, SymbolTable);

  // Properties
  AddPropertyToClass('X', 'Float', 'GetX', 'SetX', '', False, ClassSym,
    SymbolTable);
  AddPropertyToClass('Y', 'Float', 'GetY', 'SetY', '', False, ClassSym,
    SymbolTable);
  AddPropertyToClass('Z', 'Float', 'GetZ', 'SetZ', '', False, ClassSym,
    SymbolTable);
  AddPropertyToClass('AsVector', 'TGLVector', 'GetAsVector', 'SetAsVector', '',
    False, ClassSym, SymbolTable);
  AddPropertyToClass('AsString', 'String', 'GetAsString', '', '', False,
    ClassSym, SymbolTable);
end;

procedure TdwsGLSceneUnit.AddClassTGLBaseSceneObject(SymbolTable: TSymbolTable);
var
  ClassSym: TClassSymbol;
begin
  ClassSym := TClassSymbol(AddClassSymbol(SymbolTable, 'TGLBaseSceneObject',
    'TComponent'));

  // Methods
  if not Assigned(ClassSym.Members.FindLocal('SetVisible')) then
    TGLBaseSceneObjectSetVisibleMethod.Create(mkProcedure, [], 0, 'SetVisible',
      ['Value', 'Boolean'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetVisible')) then
    TGLBaseSceneObjectGetVisibleMethod.Create(mkFunction, [], 0, 'GetVisible',
      [], 'Boolean', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetMatrix')) then
    TGLBaseSceneObjectSetMatrixMethod.Create(mkProcedure, [], 0, 'SetMatrix',
      ['Value', 'TGLMatrix'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetMatrix')) then
    TGLBaseSceneObjectGetMatrixMethod.Create(mkFunction, [], 0, 'GetMatrix', [],
      'TGLMatrix', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('AbsoluteMatrix')) then
    TGLBaseSceneObjectAbsoluteMatrixMethod.Create(mkFunction, [], 0,
      'AbsoluteMatrix', [], 'TGLMatrix', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('InvAbsoluteMatrix')) then
    TGLBaseSceneObjectInvAbsoluteMatrixMethod.Create(mkFunction, [], 0,
      'InvAbsoluteMatrix', [], 'TGLMatrix', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAbsolutePosition')) then
    TGLBaseSceneObjectSetAbsolutePositionMethod.Create(mkProcedure, [], 0,
      'SetAbsolutePosition', ['Value', 'TGLVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAbsolutePosition')) then
    TGLBaseSceneObjectGetAbsolutePositionMethod.Create(mkFunction, [], 0,
      'GetAbsolutePosition', [], 'TGLVector', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAbsoluteUp')) then
    TGLBaseSceneObjectSetAbsoluteUpMethod.Create(mkProcedure, [], 0,
      'SetAbsoluteUp', ['Value', 'TGLVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAbsoluteUp')) then
    TGLBaseSceneObjectGetAbsoluteUpMethod.Create(mkFunction, [], 0,
      'GetAbsoluteUp', [], 'TGLVector', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAbsoluteDirection')) then
    TGLBaseSceneObjectSetAbsoluteDirectionMethod.Create(mkProcedure, [], 0,
      'SetAbsoluteDirection', ['Value', 'TGLVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAbsoluteDirection')) then
    TGLBaseSceneObjectGetAbsoluteDirectionMethod.Create(mkFunction, [], 0,
      'GetAbsoluteDirection', [], 'TGLVector', ClassSym, SymbolTable);

  if not Assigned(ClassSym.Members.FindLocal('SetPosition')) then
    TGLBaseSceneObjectSetPositionMethod.Create(mkProcedure, [], 0,
      'SetPosition', ['Value', 'TGLCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetPosition')) then
    TGLBaseSceneObjectGetPositionMethod.Create(mkFunction, [], 0, 'GetPosition',
      [], 'TGLCoordinates', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetDirection')) then
    TGLBaseSceneObjectSetDirectionMethod.Create(mkProcedure, [], 0,
      'SetDirection', ['Value', 'TGLCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetDirection')) then
    TGLBaseSceneObjectGetDirectionMethod.Create(mkFunction, [], 0,
      'GetDirection', [], 'TGLCoordinates', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetUp')) then
    TGLBaseSceneObjectSetUpMethod.Create(mkProcedure, [], 0, 'SetUp',
      ['Value', 'TGLCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetUp')) then
    TGLBaseSceneObjectGetUpMethod.Create(mkFunction, [], 0, 'GetUp', [],
      'TGLCoordinates', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetScale')) then
    TGLBaseSceneObjectSetScaleMethod.Create(mkProcedure, [], 0, 'SetScale',
      ['Value', 'TGLCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetScale')) then
    TGLBaseSceneObjectGetScaleMethod.Create(mkFunction, [], 0, 'GetScale', [],
      'TGLCoordinates', ClassSym, SymbolTable);

  if not Assigned(ClassSym.Members.FindLocal('SetPitchAngle')) then
    TGLBaseSceneObjectSetPitchAngleMethod.Create(mkProcedure, [], 0,
      'SetPitchAngle', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetPitchAngle')) then
    TGLBaseSceneObjectGetPitchAngleMethod.Create(mkFunction, [], 0,
      'GetPitchAngle', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetTurnAngle')) then
    TGLBaseSceneObjectSetTurnAngleMethod.Create(mkProcedure, [], 0,
      'SetTurnAngle', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetTurnAngle')) then
    TGLBaseSceneObjectGetTurnAngleMethod.Create(mkFunction, [], 0,
      'GetTurnAngle', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetRollAngle')) then
    TGLBaseSceneObjectSetRollAngleMethod.Create(mkProcedure, [], 0,
      'SetRollAngle', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetRollAngle')) then
    TGLBaseSceneObjectGetRollAngleMethod.Create(mkFunction, [], 0,
      'GetRollAngle', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Pitch')) then
    TGLBaseSceneObjectPitchMethod.Create(mkProcedure, [], 0, 'Pitch',
      ['angle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Turn')) then
    TGLBaseSceneObjectTurnMethod.Create(mkProcedure, [], 0, 'Turn',
      ['angle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Roll')) then
    TGLBaseSceneObjectRollMethod.Create(mkProcedure, [], 0, 'Roll',
      ['angle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Move')) then
    TGLBaseSceneObjectMoveMethod.Create(mkProcedure, [], 0, 'Move',
      ['ADistance', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('AddChild')) then
    TGLBaseSceneObjectAddChildMethod.Create(mkProcedure, [], 0, 'AddChild',
      ['AChild', 'TGLBaseSceneObject'], '', ClassSym, SymbolTable);

  // Properties
  AddPropertyToClass('Visible', 'Boolean', 'GetVisible', 'SetVisible', '',
    False, ClassSym, SymbolTable);
  AddPropertyToClass('Matrix', 'TGLMatrix', 'GetMatrix', 'SetMatrix', '', False,
    ClassSym, SymbolTable);
  AddPropertyToClass('AbsolutePosition', 'TGLVector', 'GetAbsolutePosition',
    'SetAbsolutePosition', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AbsoluteUp', 'TGLVector', 'GetAbsoluteUp', 'SetAbsoluteUp',
    '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AbsoluteDirection', 'TGLVector', 'GetAbsoluteDirection',
    'SetAbsoluteDirection', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Position', 'TGLBaseSceneObject', 'GetPosition',
    'SetPosition', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Direction', 'TGLBaseSceneObject', 'GetDirection',
    'SetDirection', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Up', 'TGLBaseSceneObject', 'GetUp', 'SetUp', '', False,
    ClassSym, SymbolTable);
  AddPropertyToClass('Scale', 'TGLBaseSceneObject', 'GetScale', 'SetScale', '',
    False, ClassSym, SymbolTable);
  AddPropertyToClass('PitchAngle', 'Float', 'GetPitchAngle', 'SetPitchAngle',
    '', False, ClassSym, SymbolTable);
  AddPropertyToClass('TurnAngle', 'Float', 'GetTurnAngle', 'SetTurnAngle', '',
    False, ClassSym, SymbolTable);
  AddPropertyToClass('RollAngle', 'Float', 'GetRollAngle', 'SetRollAngle', '',
    False, ClassSym, SymbolTable);
end;

procedure TdwsGLSceneUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
begin
  // Forward class declaration
  AddForwardDeclaration('TGLCoordinates', SymbolTable);
  AddForwardDeclaration('TGLBaseSceneObject', SymbolTable);

  // Class types
  AddClassTGLCoordinates(SymbolTable);
  AddClassTGLBaseSceneObject(SymbolTable);
end;

end.
