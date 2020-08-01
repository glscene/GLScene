//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLCoordinates;

(* Coordinate related classes *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  Scene.VectorGeometry,
  Scene.VectorTypes,
  OpenGLTokens,
  GLBaseClasses;

type

  (* Identifie le type de données stockées au sein d'un TGLCustomCoordinates.
     csPoint2D : a simple 2D point (Z=0, W=0)
     csPoint : un point (W=1)
     csVector : un vecteur (W=0)
     csUnknown : aucune contrainte *)
  TGLCoordinatesStyle = (csPoint2D, csPoint, csVector, csUnknown);

  (* Stores and homogeneous vector.
    This class is basicly a container for a TVector, allowing proper use of
    delphi property editors and editing in the IDE. Vector/Coordinates
    manipulation methods are only minimal.
    Handles dynamic default values to save resource file space.  *)
  TGLCustomCoordinates = class(TGLUpdateAbleObject)
  private
   FCoords: TVector;
    FStyle: TGLCoordinatesStyle; // NOT Persistent
    FPDefaultCoords: PVector;
    procedure SetAsPoint2D(const Value: TVector2f);
    procedure SetAsVector(const Value: TVector);
    procedure SetAsAffineVector(const Value: TAffineVector);
    function GetAsAffineVector: TAffineVector; inline;
    function GetAsPoint2D: TVector2f;
    function GetAsString: String;
    function GetCoordinate(const AIndex: Integer): TGLFloat; inline;
    procedure SetCoordinate(const AIndex: Integer; const AValue: TGLFloat); inline;
    function GetDirectCoordinate(const Index: Integer): TGLFloat; inline;
    procedure SetDirectCoordinate(const Index: Integer; const AValue: TGLFloat);
  protected
    procedure SetDirectVector(const V: TVector); inline;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  public
    constructor CreateInitialized(AOwner: TPersistent; const AValue: TVector;
      const AStyle: TGLCoordinatesStyle = CsUnknown);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TWriter);
    procedure ReadFromFiler(Reader: TReader);
    procedure Initialize(const Value: TVector);
    procedure NotifyChange(Sender: TObject); override;
    (* Identifies the coordinates styles.
      The property is NOT persistent, csUnknown by default, and should be
      managed by owner object only (internally).
      It is used by the TGLCustomCoordinates for internal "assertion" checks
      to detect "misuses" or "misunderstandings" of what the homogeneous
      coordinates system implies. *)
    property Style: TGLCoordinatesStyle read FStyle write FStyle;
    procedure Translate(const TranslationVector: TVector); overload;
    procedure Translate(const TranslationVector: TAffineVector); overload;
    procedure AddScaledVector(const Factor: Single; const TranslationVector: TVector); overload;
    procedure AddScaledVector(const Factor: Single; const TranslationVector: TAffineVector); overload;
    procedure Rotate(const AnAxis: TAffineVector; AnAngle: Single); overload;
    procedure Rotate(const AnAxis: TVector; AnAngle: Single); overload;
    procedure Normalize; inline;
    procedure Invert;
    procedure Scale(Factor: Single);
    function VectorLength: TGLFloat;
    function VectorNorm: TGLFloat;
    function MaxXYZ: Single;
    function Equals(const AVector: TVector): Boolean; reintroduce;
    procedure SetVector(const X, Y: Single; Z: Single = 0); overload;
    procedure SetVector(const X, Y, Z, W: Single); overload;
    procedure SetVector(const V: TAffineVector); overload;
    procedure SetVector(const V: TVector); overload;
    procedure SetPoint(const X, Y, Z: Single); overload;
    procedure SetPoint(const V: TAffineVector); overload;
    procedure SetPoint(const V: TVector); overload;
    procedure SetPoint2D(const X, Y: Single); overload;
    procedure SetPoint2D(const Vector: TAffineVector); overload;
    procedure SetPoint2D(const Vector: TVector); overload;
    procedure SetPoint2D(const Vector: TVector2f); overload;
    procedure SetToZero;
    function AsAddress: PGLFloat; inline;
    (* The coordinates viewed as a vector.
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. *)
    property AsVector: TVector read FCoords write SetAsVector;
    (* The coordinates viewed as an affine vector.
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead.
      The W component is automatically adjustes depending on style. *)
    property AsAffineVector: TAffineVector read GetAsAffineVector  write SetAsAffineVector;
    (*  The coordinates viewed as a 2D point.
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. *)
    property AsPoint2D: TVector2f read GetAsPoint2D write SetAsPoint2D;
    property X: TGLFloat index 0 read GetCoordinate write SetCoordinate;
    property Y: TGLFloat index 1 read GetCoordinate write SetCoordinate;
    property Z: TGLFloat index 2 read GetCoordinate write SetCoordinate;
    property W: TGLFloat index 3 read GetCoordinate write SetCoordinate;
    property Coordinate[const AIndex: Integer]: TGLFloat read GetCoordinate write SetCoordinate; default;
    // The coordinates, in-between brackets, separated by semi-colons.
    property AsString: String read GetAsString;
    // Similar to AsVector but does not trigger notification events
    property DirectVector: TVector read FCoords write SetDirectVector;
    property DirectX: TGLFloat index 0 read GetDirectCoordinate write SetDirectCoordinate;
    property DirectY: TGLFloat index 1 read GetDirectCoordinate write SetDirectCoordinate;
    property DirectZ: TGLFloat index 2 read GetDirectCoordinate write SetDirectCoordinate;
    property DirectW: TGLFloat index 3 read GetDirectCoordinate write SetDirectCoordinate;
  end;

  // A TGLCustomCoordinates that publishes X, Y properties.
  TGLCoordinates2 = class(TGLCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
  end;

  // A TGLCustomCoordinates that publishes X, Y, Z properties.
  TGLCoordinates3 = class(TGLCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
    property Z stored False;
  end;

  // A TGLCustomCoordinates that publishes X, Y, Z, W properties.
  TGLCoordinates4 = class(TGLCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
    property Z stored False;
    property W stored False;
  end;

  TGLCoordinates = TGLCoordinates3;

  (* Actually Sender should be TGLCustomCoordinates, but that would require
     changes in a some other GLScene units and some other projects that use
     TGLCoordinatesUpdateAbleComponent *)
  IGLCoordinatesUpdateAble = interface(IInterface)
    ['{ACB98D20-8905-43A7-AFA5-225CF5FA6FF5}']
    procedure CoordinateChanged(Sender: TGLCustomCoordinates);
  end;

  TGLCoordinatesUpdateAbleComponent = class(TGLUpdateAbleComponent, IGLCoordinatesUpdateAble)
  public
    procedure CoordinateChanged(Sender: TGLCustomCoordinates); virtual; abstract;
  end;

var
  (* Specifies if TGLCustomCoordinates should allocate memory for
    their default values (ie. design-time) or not (run-time) *)
  VUseDefaultCoordinateSets: Boolean = False;

//==================================================================  
implementation
//==================================================================  

const
  csVectorHelp = 'If you are getting assertions here, consider using the SetPoint procedure';
  csPointHelp = 'If you are getting assertions here, consider using the SetVector procedure';
  csPoint2DHelp = 'If you are getting assertions here, consider using one of the SetVector or SetPoint procedures';

  // ------------------
  // ------------------ TGLCustomCoordinates ------------------
  // ------------------

constructor TGLCustomCoordinates.CreateInitialized(AOwner: TPersistent;
  const AValue: TVector; const AStyle: TGLCoordinatesStyle = CsUnknown);
begin
  Create(AOwner);
  Initialize(AValue);
  FStyle := AStyle;
end;

destructor TGLCustomCoordinates.Destroy;
begin
  if Assigned(FPDefaultCoords) then
    Dispose(FPDefaultCoords);
  inherited;
end;

procedure TGLCustomCoordinates.Initialize(const Value: TVector);
begin
  FCoords := Value;
  if VUseDefaultCoordinateSets then
  begin
    if not Assigned(FPDefaultCoords) then
      New(FPDefaultCoords);
    FPDefaultCoords^ := Value;
  end;
end;

procedure TGLCustomCoordinates.Assign(Source: TPersistent);
begin
  if Source is TGLCustomCoordinates then
    FCoords := TGLCustomCoordinates(Source).FCoords
  else
    inherited;
end;

procedure TGLCustomCoordinates.WriteToFiler(Writer: TWriter);
var
  WriteCoords: Boolean;
begin
  with Writer do
  begin
    WriteInteger(0); // Archive Version 0
    if VUseDefaultCoordinateSets then
      WriteCoords := not VectorEquals(FPDefaultCoords^, FCoords)
    else
      WriteCoords := True;
    WriteBoolean(WriteCoords);
    if WriteCoords then
      Write(FCoords.X, SizeOf(FCoords));
  end;
end;

procedure TGLCustomCoordinates.ReadFromFiler(Reader: TReader);
var
  N: Integer;
begin
  with Reader do
  begin
    ReadInteger; // Ignore ArchiveVersion
    if ReadBoolean then
    begin
      N := SizeOf(FCoords);
      Assert(N = 4 * SizeOf(Single));
      Read(FCoords.X, N);
    end
    else if Assigned(FPDefaultCoords) then
      FCoords := FPDefaultCoords^;
  end;
end;

procedure TGLCustomCoordinates.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Coordinates', ReadData, WriteData,
    not(Assigned(FPDefaultCoords) and VectorEquals(FPDefaultCoords^, FCoords)));
end;

procedure TGLCustomCoordinates.ReadData(Stream: TStream);
begin
  Stream.Read(FCoords, SizeOf(FCoords));
end;

procedure TGLCustomCoordinates.WriteData(Stream: TStream);
begin
  Stream.Write(FCoords, SizeOf(FCoords));
end;

procedure TGLCustomCoordinates.NotifyChange(Sender: TObject);
var
  Int: IGLCoordinatesUpdateAble;
begin
  if Supports(Owner, IGLCoordinatesUpdateAble, Int) then
    Int.CoordinateChanged(TGLCoordinates(Self));
  inherited NotifyChange(Sender);
end;

procedure TGLCustomCoordinates.Translate(const TranslationVector: TVector);
begin
  FCoords.X := FCoords.X + TranslationVector.X;
  FCoords.Y := FCoords.Y + TranslationVector.Y;
  FCoords.Z := FCoords.Z + TranslationVector.Z;
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.Translate(const TranslationVector
  : TAffineVector);
begin
  FCoords.X := FCoords.X + TranslationVector.X;
  FCoords.Y := FCoords.Y + TranslationVector.Y;
  FCoords.Z := FCoords.Z + TranslationVector.Z;
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.AddScaledVector(const Factor: Single;
  const TranslationVector: TVector);
var
  F: Single;
begin
  F := Factor;
  CombineVector(FCoords, TranslationVector, F);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.AddScaledVector(const Factor: Single;
  const TranslationVector: TAffineVector);
var
  F: Single;
begin
  F := Factor;
  CombineVector(FCoords, TranslationVector, F);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.Rotate(const AnAxis: TAffineVector;
  AnAngle: Single);
begin
  RotateVector(FCoords, AnAxis, AnAngle);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.Rotate(const AnAxis: TVector; AnAngle: Single);
begin
  RotateVector(FCoords, AnAxis, AnAngle);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.Normalize;
begin
  NormalizeVector(FCoords);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.Invert;
begin
  NegateVector(FCoords);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.Scale(Factor: Single);
begin
  ScaleVector(PAffineVector(@FCoords)^, Factor);
  NotifyChange(Self);
end;

function TGLCustomCoordinates.VectorLength: TGLFloat;
begin
  Result := Scene.VectorGeometry.VectorLength(FCoords);
end;

function TGLCustomCoordinates.VectorNorm: TGLFloat;
begin
  Result := Scene.VectorGeometry.VectorNorm(FCoords);
end;

function TGLCustomCoordinates.MaxXYZ: Single;
begin
  Result := MaxXYZComponent(FCoords);
end;

function TGLCustomCoordinates.Equals(const AVector: TVector): Boolean;
begin
  Result := VectorEquals(FCoords, AVector);
end;

procedure TGLCustomCoordinates.SetVector(const X, Y: Single; Z: Single = 0);
begin
  Assert(FStyle = csVector, csVectorHelp);
  Scene.VectorGeometry.SetVector(FCoords, X, Y, Z);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetVector(const V: TAffineVector);
begin
  Assert(FStyle = csVector, csVectorHelp);
  Scene.VectorGeometry.SetVector(FCoords, V);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetVector(const V: TVector);
begin
  Assert(FStyle = csVector, csVectorHelp);
  Scene.VectorGeometry.SetVector(FCoords, V);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetVector(const X, Y, Z, W: Single);
begin
  Assert(FStyle = csVector, csVectorHelp);
  Scene.VectorGeometry.SetVector(FCoords, X, Y, Z, W);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetDirectCoordinate(const Index: Integer;
  const AValue: TGLFloat);
begin
  FCoords.V[index] := AValue;
end;

procedure TGLCustomCoordinates.SetDirectVector(const V: TVector);
begin
  FCoords.X := V.X;
  FCoords.Y := V.Y;
  FCoords.Z := V.Z;
  FCoords.W := V.W;
end;

procedure TGLCustomCoordinates.SetToZero;
begin
  FCoords.X := 0;
  FCoords.Y := 0;
  FCoords.Z := 0;
  if FStyle = CsPoint then
    FCoords.W := 1
  else
    FCoords.W := 0;
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetPoint(const X, Y, Z: Single);
begin
  Assert(FStyle = CsPoint, CsPointHelp);
  MakePoint(FCoords, X, Y, Z);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetPoint(const V: TAffineVector);
begin
  Assert(FStyle = CsPoint, CsPointHelp);
  MakePoint(FCoords, V);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetPoint(const V: TVector);
begin
  Assert(FStyle = CsPoint, CsPointHelp);
  MakePoint(FCoords, V);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetPoint2D(const X, Y: Single);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  Scene.VectorGeometry.MakeVector(FCoords, X, Y, 0);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetPoint2D(const Vector: TAffineVector);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  MakeVector(FCoords, Vector);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetPoint2D(const Vector: TVector);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  MakeVector(FCoords, Vector);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetPoint2D(const Vector: TVector2f);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  MakeVector(FCoords, Vector.X, Vector.Y, 0);
  NotifyChange(Self);
end;

function TGLCustomCoordinates.AsAddress: PGLFloat;
begin
  Result := @FCoords;
end;

procedure TGLCustomCoordinates.SetAsVector(const Value: TVector);
begin
  FCoords := Value;
  case FStyle of
    CsPoint2D:
      begin
        FCoords.Z := 0;
        FCoords.W := 0;
      end;
    CsPoint:
      FCoords.W := 1;
    CsVector:
      FCoords.W := 0;
  else
    Assert(False);
  end;
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetAsAffineVector(const Value: TAffineVector);
begin
  case FStyle of
    CsPoint2D:
      MakeVector(FCoords, Value);
    CsPoint:
      MakePoint(FCoords, Value);
    CsVector:
      MakeVector(FCoords, Value);
  else
    Assert(False);
  end;
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetAsPoint2D(const Value: TVector2f);
begin
  case FStyle of
    CsPoint2D, CsPoint, CsVector:
      begin
        FCoords.X := Value.X;
        FCoords.Y := Value.Y;
        FCoords.Z := 0;
        FCoords.W := 0;
      end;
  else
    Assert(False);
  end;
  NotifyChange(Self);
end;

function TGLCustomCoordinates.GetAsAffineVector: TAffineVector;
begin
  Scene.VectorGeometry.SetVector(Result, FCoords);
end;

function TGLCustomCoordinates.GetAsPoint2D: TVector2f;
begin
  Result.X := FCoords.X;
  Result.Y := FCoords.Y;
end;

procedure TGLCustomCoordinates.SetCoordinate(const AIndex: Integer;
  const AValue: TGLFloat);
begin
  FCoords.V[AIndex] := AValue;
  NotifyChange(Self);
end;

function TGLCustomCoordinates.GetCoordinate(const AIndex: Integer): TGLFloat;
begin
  Result := FCoords.V[AIndex];
end;

function TGLCustomCoordinates.GetDirectCoordinate(
  const Index: Integer): TGLFloat;
begin
  Result := FCoords.V[index]
end;

function TGLCustomCoordinates.GetAsString: String;
begin
  case Style of
    CsPoint2D:
      Result := Format('(%g; %g)', [FCoords.X, FCoords.Y]);
    CsPoint:
      Result := Format('(%g; %g; %g)', [FCoords.X, FCoords.Y, FCoords.Z]);
    CsVector:
      Result := Format('(%g; %g; %g; %g)', [FCoords.X, FCoords.Y, FCoords.Z,
        FCoords.W]);
  else
    Assert(False);
  end;
end;

//=====================================================================
initialization
//=====================================================================

RegisterClasses([TGLCoordinates2, TGLCoordinates3, TGLCoordinates4]);

end.
