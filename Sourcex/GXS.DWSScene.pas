//
// Graphic Scene Engine, http://glscene.org
//
{
   DelphiWebScript symbol creation for base GLScene classes. 

   This unit is dependent on DwsClasses and DwsVectorGeometry.
   These components must be associated with the same compiler
   for the GLScene classes to inherit from. 
}
unit VXS.DWSScene;

interface

uses
  System.Classes, 
  System.SysUtils,
  DwsExprs, 
  DwsSymbols, 
  DwsComp, 
  DwsCompStrings, 
  DwsStack, 
  DwsFunctions, 
  DwsHelperFunc, 
  GXS.Scene, 
  GXS.VectorGeometry,
  GXS.Coordinates;

type
  TDwsGLXceneUnit = class(TDwsUnitComponent)
    private
    procedure AddClassTgxCoordinates(SymbolTable : TSymbolTable);
      procedure AddClassTgxBaseSceneObject(SymbolTable : TSymbolTable);
    protected
      procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
    public
      constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

//============================================
implementation
//============================================

// ----------
// ---------- Internal class method class declarations ----------
// ----------

type

  
  TgxCoordinatesSetXMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesGetXMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesSetYMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesGetYMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesSetZMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesGetZMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesSetWMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesGetWMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesSetVectorMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesSetPointMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesSetToZeroMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesSetAsVectorMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesGetAsVectorMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesGetAsStringMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesTranslateMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesAddScaledVectorMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesRotateMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesNormalizeMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesInvertMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesScaleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxCoordinatesEqualsMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;


  // TgxBaseSceneObject

  TgxBaseSceneObjectSetVisibleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectGetVisibleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectSetMatrixMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectGetMatrixMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectAbsoluteMatrixMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectInvAbsoluteMatrixMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectSetAbsolutePositionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectGetAbsolutePositionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectSetAbsoluteUpMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectGetAbsoluteUpMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectSetAbsoluteDirectionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectGetAbsoluteDirectionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectSetPositionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectGetPositionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectSetDirectionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectGetDirectionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectSetUpMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectGetUpMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectSetScaleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectGetScaleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectSetPitchAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectGetPitchAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectSetTurnAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectGetTurnAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectSetRollAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectGetRollAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectPitchMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectTurnMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectRollMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectMoveMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TgxBaseSceneObjectAddChildMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;


// ----------
// ---------- Vector/Matrix to/from IInfo helper functions ----------
// ----------

// GetVectorFromInfo
//
function GetVectorFromInfo(Info : IInfo) : TVector4f;
begin
  Result:=VectorMake(Info.Element([0]).Value,
                     Info.Element([1]).Value,
                     Info.Element([2]).Value,
                     Info.Element([3]).Value);
end;

procedure SetInfoFromVector(Info : IInfo; vec : TVector4f);
var
  i : Integer;
begin
  for i:=0 to 3 do
    Info.Element([i]).Value:=vec[i];
end;

function GetMatrixFromInfo(Info : IInfo) : TMatrix4f;
var
  i : Integer;
begin
  for i:=0 to 3 do
    Result[i]:=VectorMake(Info.Element([i]).Element([0]).Value,
                          Info.Element([i]).Element([1]).Value,
                          Info.Element([i]).Element([2]).Value,
                          Info.Element([i]).Element([3]).Value);
end;

procedure SetInfoFromMatrix(Info : IInfo; mat : TMatrix4f);
var
  i,j : Integer;
begin
  for i:=0 to 3 do
    for j:=0 to 3 do
      Info.Element([i]).Element([j]).Value:=mat[i][j];
end;


// ----------
// ---------- Internal class method execute procedures ----------
// ----------

procedure TgxCoordinatesSetXMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  TgxCoordinates(ExternalObject).X:=Info['Value'];
end;

procedure TgxCoordinatesGetXMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  Info.Result:=TgxCoordinates(ExternalObject).X;
end;

// TgxCoordinates.Y write access
procedure TgxCoordinatesSetYMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  TgxCoordinates(ExternalObject).Y:=Info['Value'];
end;

// TgxCoordinates.Y read access
procedure TgxCoordinatesGetYMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  Info.Result:=TgxCoordinates(ExternalObject).Y;
end;

// TgxCoordinates.Z write access
procedure TgxCoordinatesSetZMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  TgxCoordinates(ExternalObject).Z:=Info['Value'];
end;

// TgxCoordinates.Z read access
procedure TgxCoordinatesGetZMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  Info.Result:=TgxCoordinates(ExternalObject).Z;
end;

// TgxCoordinates.W write access
procedure TgxCoordinatesSetWMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  TgxCoordinates(ExternalObject).W:=Info['Value'];
end;

// TgxCoordinates.W read access
procedure TgxCoordinatesGetWMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  Info.Result:=TgxCoordinates(ExternalObject).W;
end;

// TgxCoordinates.SetVector
procedure TgxCoordinatesSetVectorMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  TgxCoordinates(ExternalObject).SetVector(Info['x'],Info['y'],Info['z'],Info['w']);
end;

// TgxCoordinates.SetPoint
procedure TgxCoordinatesSetPointMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  TgxCoordinates(ExternalObject).SetPoint(Info['x'],Info['y'],Info['z']);
end;

// TgxCoordinates.AsVector write access
procedure TgxCoordinatesSetAsVectorMethod.Execute(var ExternalObject: TObject);
var
  v : TVector4f;
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  v:=GetVectorFromInfo(Info.Vars['Value']);
  TgxCoordinates(ExternalObject).AsVector:=v;
end;

// TgxCoordinates.AsVector read access
procedure TgxCoordinatesGetAsVectorMethod.Execute(var ExternalObject: TObject);
var
  v : TVector4f;
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  v:=TgxCoordinates(ExternalObject).AsVector;
  SetInfoFromVector(Info.Vars['Result'], v);
end;

// TgxCoordinates.AsString read access
procedure TgxCoordinatesGetAsStringMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  Info.Result:=TgxCoordinates(ExternalObject).AsString;
end;

// TgxCoordinates.Translate
procedure TgxCoordinatesTranslateMethod.Execute(var ExternalObject: TObject);
var
  v : TVector4f;
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  v:=GetVectorFromInfo(Info.Vars['translationVector']);
  TgxCoordinates(ExternalObject).Translate(v);
end;

// TgxCoordinates.AddScaledVector
procedure TgxCoordinatesAddScaledVectorMethod.Execute(var ExternalObject: TObject);
var
  v : TVector4f;
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  v:=GetVectorFromInfo(Info.Vars['translationVector']);
  TgxCoordinates(ExternalObject).AddScaledVector(Info['factor'],v);
end;

// TgxCoordinates.Rotate
procedure TgxCoordinatesRotateMethod.Execute(var ExternalObject: TObject);
var
  v : TVector4f;
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  v:=GetVectorFromInfo(Info.Vars['anAxis']);
  TgxCoordinates(ExternalObject).Rotate(v, Info['anAngle']);
end;

// TgxCoordinates.Normalize
procedure TgxCoordinatesNormalizeMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  TgxCoordinates(ExternalObject).Normalize;
end;

// TgxCoordinates.Invert
procedure TgxCoordinatesInvertMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  TgxCoordinates(ExternalObject).Invert;
end;

// TgxCoordinates.Scale
procedure TgxCoordinatesScaleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  TgxCoordinates(ExternalObject).Scale(Info['factor']);
end;

// TgxCoordinates.Equals
procedure TgxCoordinatesEqualsMethod.Execute(var ExternalObject: TObject);
var
  v : TVector4f;
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  v:=GetVectorFromInfo(Info.Vars['aVector']);
  Info.Result:=TgxCoordinates(ExternalObject).Equals(v);
end;

// TgxCoordinates.SetToZero
procedure TgxCoordinatesSetToZeroMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxCoordinates);
  TgxCoordinates(ExternalObject).SetToZero;
end;


// TgxBaseSceneObject internal class methods

procedure TgxBaseSceneObjectSetVisibleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  TgxBaseSceneObject(ExternalObject).Visible:=Info.Vars['Value'].Value;
end;

// TgxBaseSceneObject.GetVisible
procedure TgxBaseSceneObjectGetVisibleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  Info.Result:=TgxBaseSceneObject(ExternalObject).Visible;
end;

// TgxBaseSceneObject.SetMatrix
procedure TgxBaseSceneObjectSetMatrixMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  TgxBaseSceneObject(ExternalObject).Matrix:=GetMatrixFromInfo(Info.Vars['Value']);
end;

// TgxBaseSceneObject.GetMatrix
procedure TgxBaseSceneObjectGetMatrixMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  SetInfoFromMatrix(Info.Vars['Result'], TgxBaseSceneObject(ExternalObject).Matrix);
end;

// TgxBaseSceneObject.AbsoluteMatrix
procedure TgxBaseSceneObjectAbsoluteMatrixMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  SetInfoFromMatrix(Info.Vars['Result'], TgxBaseSceneObject(ExternalObject).AbsoluteMatrix);
end;

// TgxBaseSceneObject.InvAbsoluteMatrix
procedure TgxBaseSceneObjectInvAbsoluteMatrixMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  SetInfoFromMatrix(Info.Vars['Result'], TgxBaseSceneObject(ExternalObject).InvAbsoluteMatrix);
end;

// TgxBaseSceneObject.SetAbsolutePosition
procedure TgxBaseSceneObjectSetAbsolutePositionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  TgxBaseSceneObject(ExternalObject).AbsolutePosition:=GetVectorFromInfo(Info.Vars['Value']);
end;

// TgxBaseSceneObject.GetAbsolutePosition
procedure TgxBaseSceneObjectGetAbsolutePositionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  SetInfoFromVector(Info.Vars['Result'], TgxBaseSceneObject(ExternalObject).AbsolutePosition);
end;

// TgxBaseSceneObject.SetAbsoluteUp
procedure TgxBaseSceneObjectSetAbsoluteUpMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  TgxBaseSceneObject(ExternalObject).AbsoluteUp:=GetVectorFromInfo(Info.Vars['Value']);
end;

// TgxBaseSceneObject.GetAbsoluteUp
procedure TgxBaseSceneObjectGetAbsoluteUpMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  SetInfoFromVector(Info.Vars['Result'], TgxBaseSceneObject(ExternalObject).AbsoluteUp);
end;

// TgxBaseSceneObject.SetAbsoluteDirection
procedure TgxBaseSceneObjectSetAbsoluteDirectionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  TgxBaseSceneObject(ExternalObject).AbsoluteDirection:=GetVectorFromInfo(Info.Vars['Value']);
end;

// TgxBaseSceneObject.GetAbsoluteDirection
procedure TgxBaseSceneObjectGetAbsoluteDirectionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  SetInfoFromVector(Info.Vars['Result'], TgxBaseSceneObject(ExternalObject).AbsoluteDirection);
end;

// TgxBaseSceneObject.Position write access
procedure TgxBaseSceneObjectSetPositionMethod.Execute(var ExternalObject: TObject);
var
  Value : TgxCoordinates;
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  Value:=TgxCoordinates(Info.GetExternalObjForVar('Value'));
  TgxBaseSceneObject(ExternalObject).Position:=Value;
end;

// TgxBaseSceneObject.Position read access
procedure TgxBaseSceneObjectGetPositionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  Info.Result:=Info.RegisterExternalObject(TgxBaseSceneObject(ExternalObject).Position);
end;

// TgxBaseSceneObject.Direction write access
procedure TgxBaseSceneObjectSetDirectionMethod.Execute(var ExternalObject: TObject);
var
  Value : TgxCoordinates;
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  Value:=TgxCoordinates(Info.GetExternalObjForVar('Value'));
  TgxBaseSceneObject(ExternalObject).Direction:=Value;
end;

// TgxBaseSceneObject.Direction read access
procedure TgxBaseSceneObjectGetDirectionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  Info.Result:=Info.RegisterExternalObject(TgxBaseSceneObject(ExternalObject).Direction);
end;

// TgxBaseSceneObject.Up write access
procedure TgxBaseSceneObjectSetUpMethod.Execute(var ExternalObject: TObject);
var
  Value : TgxCoordinates;
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  Value:=TgxCoordinates(Info.GetExternalObjForVar('Value'));
  TgxBaseSceneObject(ExternalObject).Up:=Value;
end;

// TgxBaseSceneObject.Up read access
procedure TgxBaseSceneObjectGetUpMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  Info.Result:=Info.RegisterExternalObject(TgxBaseSceneObject(ExternalObject).Up);
end;

// TgxBaseSceneObject.Scale write access
procedure TgxBaseSceneObjectSetScaleMethod.Execute(var ExternalObject: TObject);
var
  Value : TgxCoordinates;
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  Value:=TgxCoordinates(Info.GetExternalObjForVar('Value'));
  TgxBaseSceneObject(ExternalObject).Scale:=Value;
end;

// TgxBaseSceneObject.Scale read access
procedure TgxBaseSceneObjectGetScaleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  Info.Result:=Info.RegisterExternalObject(TgxBaseSceneObject(ExternalObject).Scale);
end;

// TgxBaseSceneObject.PitchAngle write access
procedure TgxBaseSceneObjectSetPitchAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  TgxBaseSceneObject(ExternalObject).PitchAngle:=Info.Vars['Value'].Value;
end;

// TgxBaseSceneObject.PitchAngle read access
procedure TgxBaseSceneObjectGetPitchAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  Info.Result:=TgxBaseSceneObject(ExternalObject).PitchAngle;
end;

// TgxBaseSceneObject.TurnAngle write access
procedure TgxBaseSceneObjectSetTurnAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  TgxBaseSceneObject(ExternalObject).TurnAngle:=Info.Vars['Value'].Value;
end;

// TgxBaseSceneObject.TurnAngle read access
procedure TgxBaseSceneObjectGetTurnAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  Info.Result:=TgxBaseSceneObject(ExternalObject).TurnAngle;
end;

// TgxBaseSceneObject.RollAngle write access
procedure TgxBaseSceneObjectSetRollAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  TgxBaseSceneObject(ExternalObject).RollAngle:=Info.Vars['Value'].Value;
end;

// TgxBaseSceneObject.RollAngle read access
procedure TgxBaseSceneObjectGetRollAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  Info.Result:=TgxBaseSceneObject(ExternalObject).RollAngle;
end;

// TgxBaseSceneObject.Pitch
procedure TgxBaseSceneObjectPitchMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  TgxBaseSceneObject(ExternalObject).Pitch(Info['angle']);
end;

// TgxBaseSceneObject.Turn
procedure TgxBaseSceneObjectTurnMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  TgxBaseSceneObject(ExternalObject).Turn(Info['angle']);
end;

// TgxBaseSceneObject.Roll
procedure TgxBaseSceneObjectRollMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  TgxBaseSceneObject(ExternalObject).Roll(Info['angle']);
end;

// TgxBaseSceneObject.Move
procedure TgxBaseSceneObjectMoveMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  TgxBaseSceneObject(ExternalObject).Move(Info['ADistance']);
end;

// TgxBaseSceneObject.AddChild
procedure TgxBaseSceneObjectAddChildMethod.Execute(var ExternalObject: TObject);
var
  AChild : TObject;
begin
  ValidateExternalObject(ExternalObject, TgxBaseSceneObject);
  AChild:=Info.GetExternalObjForVar('AChild');
  if not Assigned(AChild) then raise Exception.Create('AChild parameter is unassigned.');
  if not (AChild is TgxBaseSceneObject) then Exception.Create('AChild parameter is not inheriting from TgxBaseSceneObject.');
  TgxBaseSceneObject(ExternalObject).AddChild(TgxBaseSceneObject(AChild));
end;


// ----------
// ---------- Global procedures/functions ----------
// ----------

procedure Register;
begin
  RegisterComponents('GXScene Dws', [TDwsGLXceneUnit]);
end;


// ----------
// ---------- TDwsGLXceneUnit ----------
// ----------

constructor TDwsGLXceneUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName:='GXScene';
  with FDependencies do begin
    Add('Classes');
    Add('GXS.VectorGeometry');
  end;
end;

procedure TDwsGLXceneUnit.AddClassTgxCoordinates(
  SymbolTable: TSymbolTable);
var
  ClassSym : TClassSymbol;
begin
  ClassSym:=TClassSymbol(AddClassSymbol(SymbolTable, 'TgxCoordinates', 'TPersistent'));

  // Methods
  if not Assigned(ClassSym.Members.FindLocal('SetX')) then
    TgxCoordinatesSetXMethod.Create(mkProcedure, [], 0, 'SetX', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetX')) then
    TgxCoordinatesGetXMethod.Create(mkFunction, [], 0, 'GetX', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetY')) then
    TgxCoordinatesSetYMethod.Create(mkProcedure, [], 0, 'SetY', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetY')) then
    TgxCoordinatesGetYMethod.Create(mkFunction, [], 0, 'GetY', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetZ')) then
    TgxCoordinatesSetZMethod.Create(mkProcedure, [], 0, 'SetZ', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetZ')) then
    TgxCoordinatesGetZMethod.Create(mkFunction, [], 0, 'GetZ', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetW')) then
    TgxCoordinatesSetWMethod.Create(mkProcedure, [], 0, 'SetW', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetW')) then
    TgxCoordinatesGetWMethod.Create(mkFunction, [], 0, 'GetW', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetVector')) then
    TgxCoordinatesSetVectorMethod.Create(mkProcedure, [], 0, 'SetVector', ['x', 'Float', 'y', 'Float', 'z', 'Float', 'w', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetPoint')) then
    TgxCoordinatesSetPointMethod.Create(mkProcedure, [], 0, 'SetPoint', ['x', 'Float', 'y', 'Float', 'z', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetToZero')) then
    TgxCoordinatesSetToZeroMethod.Create(mkProcedure, [], 0, 'SetToZero', [], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAsVector')) then
    TgxCoordinatesSetAsVectorMethod.Create(mkProcedure, [], 0, 'SetAsVector', ['Value', 'TVector4f'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAsVector')) then
    TgxCoordinatesGetAsVectorMethod.Create(mkFunction, [], 0, 'GetAsVector', [], 'TVector4f', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAsString')) then
    TgxCoordinatesGetAsStringMethod.Create(mkFunction, [], 0, 'GetAsString', [], 'String', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Translate')) then
    TgxCoordinatesTranslateMethod.Create(mkProcedure, [], 0, 'Translate', ['translationVector', 'TVector4f'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('AddScaledVector')) then
    TgxCoordinatesAddScaledVectorMethod.Create(mkProcedure, [], 0, 'AddScaledVector', ['factor', 'Float', 'translationVector', 'TVector4f'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Rotate')) then
    TgxCoordinatesRotateMethod.Create(mkProcedure, [], 0, 'Rotate', ['anAxis', 'TVector4f', 'anAngle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Normalize')) then
    TgxCoordinatesNormalizeMethod.Create(mkProcedure, [], 0, 'Normalize', [], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Invert')) then
    TgxCoordinatesInvertMethod.Create(mkProcedure, [], 0, 'Invert', [], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Scale')) then
    TgxCoordinatesScaleMethod.Create(mkProcedure, [], 0, 'Scale', ['factor', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Equals')) then
    TgxCoordinatesEqualsMethod.Create(mkFunction, [], 0, 'Equals', ['aVector', 'TVector4f'], 'Boolean', ClassSym, SymbolTable);

  // Properties
  AddPropertyToClass('X', 'Float', 'GetX', 'SetX', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Y', 'Float', 'GetY', 'SetY', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Z', 'Float', 'GetZ', 'SetZ', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AsVector', 'TVector4f', 'GetAsVector', 'SetAsVector', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AsString', 'String', 'GetAsString', '', '', False, ClassSym, SymbolTable);
end;

// AddClassTgxBaseSceneObject
//
procedure TDwsGLXceneUnit.AddClassTgxBaseSceneObject(
  SymbolTable: TSymbolTable);
var
  ClassSym : TClassSymbol;
begin
  ClassSym:=TClassSymbol(AddClassSymbol(SymbolTable, 'TgxBaseSceneObject', 'TComponent'));

  // Methods
  if not Assigned(ClassSym.Members.FindLocal('SetVisible')) then
    TgxBaseSceneObjectSetVisibleMethod.Create(mkProcedure, [], 0, 'SetVisible', ['Value', 'Boolean'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetVisible')) then
    TgxBaseSceneObjectGetVisibleMethod.Create(mkFunction, [], 0, 'GetVisible', [], 'Boolean', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetMatrix')) then
    TgxBaseSceneObjectSetMatrixMethod.Create(mkProcedure, [], 0, 'SetMatrix', ['Value', 'TMatrix4f'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetMatrix')) then
    TgxBaseSceneObjectGetMatrixMethod.Create(mkFunction, [], 0, 'GetMatrix', [], 'TMatrix4f', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('AbsoluteMatrix')) then
    TgxBaseSceneObjectAbsoluteMatrixMethod.Create(mkFunction, [], 0, 'AbsoluteMatrix', [], 'TMatrix4f', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('InvAbsoluteMatrix')) then
    TgxBaseSceneObjectInvAbsoluteMatrixMethod.Create(mkFunction, [], 0, 'InvAbsoluteMatrix', [], 'TMatrix4f', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAbsolutePosition')) then
    TgxBaseSceneObjectSetAbsolutePositionMethod.Create(mkProcedure, [], 0, 'SetAbsolutePosition', ['Value', 'TVector4f'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAbsolutePosition')) then
    TgxBaseSceneObjectGetAbsolutePositionMethod.Create(mkFunction, [], 0, 'GetAbsolutePosition', [], 'TVector4f', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAbsoluteUp')) then
    TgxBaseSceneObjectSetAbsoluteUpMethod.Create(mkProcedure, [], 0, 'SetAbsoluteUp', ['Value', 'TVector4f'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAbsoluteUp')) then
    TgxBaseSceneObjectGetAbsoluteUpMethod.Create(mkFunction, [], 0, 'GetAbsoluteUp', [], 'TVector4f', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAbsoluteDirection')) then
    TgxBaseSceneObjectSetAbsoluteDirectionMethod.Create(mkProcedure, [], 0, 'SetAbsoluteDirection', ['Value', 'TVector4f'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAbsoluteDirection')) then
    TgxBaseSceneObjectGetAbsoluteDirectionMethod.Create(mkFunction, [], 0, 'GetAbsoluteDirection', [], 'TVector4f', ClassSym, SymbolTable);

  if not Assigned(ClassSym.Members.FindLocal('SetPosition')) then
    TgxBaseSceneObjectSetPositionMethod.Create(mkProcedure, [], 0, 'SetPosition', ['Value', 'TgxCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetPosition')) then
    TgxBaseSceneObjectGetPositionMethod.Create(mkFunction, [], 0, 'GetPosition', [], 'TgxCoordinates', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetDirection')) then
    TgxBaseSceneObjectSetDirectionMethod.Create(mkProcedure, [], 0, 'SetDirection', ['Value', 'TgxCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetDirection')) then
    TgxBaseSceneObjectGetDirectionMethod.Create(mkFunction, [], 0, 'GetDirection', [], 'TgxCoordinates', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetUp')) then
    TgxBaseSceneObjectSetUpMethod.Create(mkProcedure, [], 0, 'SetUp', ['Value', 'TgxCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetUp')) then
    TgxBaseSceneObjectGetUpMethod.Create(mkFunction, [], 0, 'GetUp', [], 'TgxCoordinates', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetScale')) then
    TgxBaseSceneObjectSetScaleMethod.Create(mkProcedure, [], 0, 'SetScale', ['Value', 'TgxCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetScale')) then
    TgxBaseSceneObjectGetScaleMethod.Create(mkFunction, [], 0, 'GetScale', [], 'TgxCoordinates', ClassSym, SymbolTable);

  if not Assigned(ClassSym.Members.FindLocal('SetPitchAngle')) then
    TgxBaseSceneObjectSetPitchAngleMethod.Create(mkProcedure, [], 0, 'SetPitchAngle', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetPitchAngle')) then
    TgxBaseSceneObjectGetPitchAngleMethod.Create(mkFunction, [], 0, 'GetPitchAngle', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetTurnAngle')) then
    TgxBaseSceneObjectSetTurnAngleMethod.Create(mkProcedure, [], 0, 'SetTurnAngle', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetTurnAngle')) then
    TgxBaseSceneObjectGetTurnAngleMethod.Create(mkFunction, [], 0, 'GetTurnAngle', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetRollAngle')) then
    TgxBaseSceneObjectSetRollAngleMethod.Create(mkProcedure, [], 0, 'SetRollAngle', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetRollAngle')) then
    TgxBaseSceneObjectGetRollAngleMethod.Create(mkFunction, [], 0, 'GetRollAngle', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Pitch')) then
    TgxBaseSceneObjectPitchMethod.Create(mkProcedure, [], 0, 'Pitch', ['angle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Turn')) then
    TgxBaseSceneObjectTurnMethod.Create(mkProcedure, [], 0, 'Turn', ['angle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Roll')) then
    TgxBaseSceneObjectRollMethod.Create(mkProcedure, [], 0, 'Roll', ['angle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Move')) then
    TgxBaseSceneObjectMoveMethod.Create(mkProcedure, [], 0, 'Move', ['ADistance', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('AddChild')) then
    TgxBaseSceneObjectAddChildMethod.Create(mkProcedure, [], 0, 'AddChild', ['AChild', 'TgxBaseSceneObject'], '', ClassSym, SymbolTable);

  // Properties
  AddPropertyToClass('Visible', 'Boolean', 'GetVisible', 'SetVisible', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Matrix', 'TMatrix4f', 'GetMatrix', 'SetMatrix', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AbsolutePosition', 'TVector4f', 'GetAbsolutePosition', 'SetAbsolutePosition', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AbsoluteUp', 'TVector4f', 'GetAbsoluteUp', 'SetAbsoluteUp', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AbsoluteDirection', 'TVector4f', 'GetAbsoluteDirection', 'SetAbsoluteDirection', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Position', 'TgxBaseSceneObject', 'GetPosition', 'SetPosition', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Direction', 'TgxBaseSceneObject', 'GetDirection', 'SetDirection', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Up', 'TgxBaseSceneObject', 'GetUp', 'SetUp', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Scale', 'TgxBaseSceneObject', 'GetScale', 'SetScale', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('PitchAngle', 'Float', 'GetPitchAngle', 'SetPitchAngle', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('TurnAngle', 'Float', 'GetTurnAngle', 'SetTurnAngle', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('RollAngle', 'Float', 'GetRollAngle', 'SetRollAngle', '', False, ClassSym, SymbolTable);
end;

// AddUnitSymbols
//
procedure TDwsGLXceneUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
begin
  // Forward class declaration
  AddForwardDeclaration('TgxCoordinates', SymbolTable);
  AddForwardDeclaration('TgxBaseSceneObject', SymbolTable);

  // Class types
  AddClassTgxCoordinates(SymbolTable);
  AddClassTgxBaseSceneObject(SymbolTable);
end;

end.
