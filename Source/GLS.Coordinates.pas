//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.Coordinates;

(* Coordinate related classes and functions *)

interface

{$I GLScene.inc}

uses
  System.Math,
  System.Classes,
  System.SysUtils,

  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.BaseClasses;

type

  (* Identifies the type of data stored within a TGLCustomCoordinates.
     csPoint2D : a simple 2D point (Z=0, W=0)
     csPoint : a point (W=1)
     csVector : a vector (W=0)
     csUnknown : no constraint *)
  TGLCoordinatesStyle = (csPoint2D, csPoint, csVector, csUnknown);

  (* Stores any homogeneous vector.
    This class is basicly a container for a TGLVector, allowing proper use of
    property editors and editing in the IDE. Vector/Coordinates
    manipulation methods are only minimal.
    Handles dynamic default values to save resource file space.  *)
  TGLCustomCoordinates = class(TGLUpdateAbleObject)
  private
   FCoords: TGLVector;
    FStyle: TGLCoordinatesStyle; // NOT Persistent
    FPDefaultCoords: PGLVector;
    procedure SetAsPoint2D(const Value: TVector2f);
    procedure SetAsVector(const Value: TGLVector);
    procedure SetAsAffineVector(const Value: TAffineVector);
    function GetAsAffineVector: TAffineVector; inline;
    function GetAsPoint2D: TVector2f;
    function GetAsString: String;
    function GetCoordinate(const AIndex: Integer): Single; inline;
    procedure SetCoordinate(const AIndex: Integer; const AValue: Single); inline;
    function GetDirectCoordinate(const Index: Integer): Single; inline;
    procedure SetDirectCoordinate(const Index: Integer; const AValue: Single);
  protected
    procedure SetDirectVector(const V: TGLVector); inline;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  public
    constructor CreateInitialized(AOwner: TPersistent; const AValue: TGLVector;
      const AStyle: TGLCoordinatesStyle = CsUnknown);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TWriter);
    procedure ReadFromFiler(Reader: TReader);
    procedure Initialize(const Value: TGLVector);
    procedure NotifyChange(Sender: TObject); override;
    (* Identifies the coordinates styles.
      The property is NOT persistent, csUnknown by default, and should be
      managed by owner object only (internally).
      It is used by the TGLCustomCoordinates for internal "assertion" checks
      to detect "misuses" or "misunderstandings" of what the homogeneous
      coordinates system implies. *)
    property Style: TGLCoordinatesStyle read FStyle write FStyle;
    procedure Translate(const TranslationVector: TGLVector); overload;
    procedure Translate(const TranslationVector: TAffineVector); overload;
    procedure AddScaledVector(const Factor: Single; const TranslationVector: TGLVector); overload;
    procedure AddScaledVector(const Factor: Single; const TranslationVector: TAffineVector); overload;
    procedure Rotate(const AnAxis: TAffineVector; AnAngle: Single); overload;
    procedure Rotate(const AnAxis: TGLVector; AnAngle: Single); overload;
    procedure Normalize; inline;
    procedure Invert;
    procedure Scale(Factor: Single);
    function VectorLength: Single;
    function VectorNorm: Single;
    function MaxXYZ: Single;
    function Equals(const AVector: TGLVector): Boolean; reintroduce;
    procedure SetVector(const X, Y: Single; Z: Single = 0); overload;
    procedure SetVector(const X, Y, Z, W: Single); overload;
    procedure SetVector(const V: TAffineVector); overload;
    procedure SetVector(const V: TGLVector); overload;
    procedure SetPoint(const X, Y, Z: Single); overload;
    procedure SetPoint(const V: TAffineVector); overload;
    procedure SetPoint(const V: TGLVector); overload;
    procedure SetPoint2D(const X, Y: Single); overload;
    procedure SetPoint2D(const Vector: TAffineVector); overload;
    procedure SetPoint2D(const Vector: TGLVector); overload;
    procedure SetPoint2D(const Vector: TVector2f); overload;
    procedure SetToZero;
    function AsAddress: PSingle; inline;
    (* The coordinates viewed as a vector.
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. *)
    property AsVector: TGLVector read FCoords write SetAsVector;
    (* The coordinates viewed as an affine vector.
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead.
      The W component is automatically adjustes depending on style. *)
    property AsAffineVector: TAffineVector read GetAsAffineVector  write SetAsAffineVector;
    (*  The coordinates viewed as a 2D point.
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. *)
    property AsPoint2D: TVector2f read GetAsPoint2D write SetAsPoint2D;
    property X: Single index 0 read GetCoordinate write SetCoordinate;
    property Y: Single index 1 read GetCoordinate write SetCoordinate;
    property Z: Single index 2 read GetCoordinate write SetCoordinate;
    property W: Single index 3 read GetCoordinate write SetCoordinate;
    property Coordinate[const AIndex: Integer]: Single read GetCoordinate write SetCoordinate; default;
    // The coordinates, in-between brackets, separated by semi-colons.
    property AsString: String read GetAsString;
    // Similar to AsVector but does not trigger notification events
    property DirectVector: TGLVector read FCoords write SetDirectVector;
    property DirectX: Single index 0 read GetDirectCoordinate write SetDirectCoordinate;
    property DirectY: Single index 1 read GetDirectCoordinate write SetDirectCoordinate;
    property DirectZ: Single index 2 read GetDirectCoordinate write SetDirectCoordinate;
    property DirectW: Single index 3 read GetDirectCoordinate write SetDirectCoordinate;
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

(* Calculates the barycentric coordinates for the point p on the triangle
  defined by the vertices v1, v2 and v3. That is, solves
  p = u * v1 + v * v2 + (1-u-v) * v3
  for u,v.
  Returns true if the point is inside the triangle, false otherwise.
  NOTE: This function assumes that the point lies on the plane defined by the triangle.
  If this is not the case, the function will not work correctly!
  https://mathworld.wolfram.com/BarycentricCoordinates.html *)
function BarycentricCoordinates(const V1, V2, V3, p: TAffineVector; var u, V: Single): Boolean;


//-------------------- Conversions of Coordinates --------------------
(*
  Helper functions to convert between different three dimensional coordinate
  systems. Room for optimisations.
*)

(* Convert Cylindrical to Cartesian [single] with no checks, theta in rad
  Ref: http://mathworld.wolfram.com/CylindricalCoordinates.html *)
procedure Cylindrical_Cartesian(const r, theta, z1: single; var x, y, z: single); overload;
(* Convert Cylindrical to Cartesian with no checks. Double version, theta in rads
  Ref: http://mathworld.wolfram.com/CylindricalCoordinates.html *)
procedure Cylindrical_Cartesian(const r, theta, z1: double; var x, y, z: double); overload;
(* Convert Cylindrical to Cartesian with checks. [single], theta in rad
  ierr: [0] = ok,
  [1] = r out of bounds. Acceptable r: [0,inf)
  [2] = theta out of bounds. Acceptable theta: [0,2pi)
  [3] = z1 out of bounds. Acceptable z1 : (-inf,inf)
  Ref: http://mathworld.wolfram.com/CylindricalCoordinates.html *)
procedure Cylindrical_Cartesian(const r, theta, z1: single; var x, y, z: single;
  var ierr: integer); overload;
(* Convert Cylindrical to Cartesian with checks. [double], theta in rad
  ierr: [0] = ok,
  [1] = r out of bounds. Acceptable r: [0,inf)
  [2] = theta out of bounds. Acceptable theta: [0,2pi)
  [3] = z1 out of bounds. Acceptable z1 : (-inf,inf)
  Ref: http://mathworld.wolfram.com/CylindricalCoordinates.html *)
procedure Cylindrical_Cartesian(const r, theta, z1: double; var x, y, z: double;
  var ierr: integer); overload;
(* Convert Cartesian to Cylindrical no checks. Single *)
procedure Cartesian_Cylindrical(const x, y, z1: single; var r, theta, z: single); overload;
(* Convert Cartesian to Cylindrical no checks. Duoble *)
procedure Cartesian_Cylindrical(const x, y, z1: double; var r, theta, z: double); overload;

(* Convert Spherical to Cartesian with no checks. [single] theta,phi in rads
   Ref: http://mathworld.wolfram.com/SphericalCoordinates.html *)
procedure Spherical_Cartesian(const r, theta, phi: single; var x, y, z: single); overload;
(* Convert Spherical to Cartesian with no checks. Double version. theta,phi in rads *)
procedure Spherical_Cartesian(const r, theta, phi: double; var x, y, z: double); overload;
(* Convert Spherical to Cartesian [single] (with error check).theta,phi in rad
  ierr: [0] = ok,
  [1] = r out of bounds
  [2] = theta out of bounds
  [3] = phi out of bounds
  Ref: http://mathworld.wolfram.com/SphericalCoordinates.html *)
procedure Spherical_Cartesian(const r, theta, phi: single; var x, y, z: single;
  var ierr: integer); overload;
// Convert spherical to cartesian [double] (with error check).theta,phi in rad
procedure Spherical_Cartesian(const r, theta, phi: double; var x, y, z: double;
  var ierr: integer); overload;
(* Convert Cartesian to Spherical, no checks, single
  Ref: http://mathworld.wolfram.com/SphericalCoordinates.html
  NB: Could be optimised by using jclmath.pas unit *)
procedure Cartesian_Spherical(const x, y, z: single; var r, theta, phi: single); overload;
procedure Cartesian_Spherical(const v: TAffineVector; var r, theta, phi: single); overload;
(* convert Cartesian to Spherical, no checks, double
  Ref: http://mathworld.wolfram.com/SphericalCoordinates.html
  NB: Could be optimised by using jclmath.pas unit? *)
procedure Cartesian_Spherical(const x, y, z: double; var r, theta, phi: double); overload;
(* Convert Prolate-Spheroidal to Cartesian with no checks. [single] eta, phi in rad
  A system of curvilinear coordinates in which two sets of coordinate surfaces are
  obtained by revolving the curves of the elliptic cylindrical coordinates about
  the x-axis, which is relabeled the z-axis. The third set of coordinates
  consists of planes passing through this axis.
  The coordinate system is parameterised by parameter a.
  A default value of a=1 is suggesed:
  Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html *)
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: single;
  var x, y, z: single); overload;
(* Convert Prolate-Spheroidal to Cartesian [double] eta,phi in rad
  A system of curvilinear coordinates in which two sets of coordinate surfaces are
  obtained by revolving the curves of the elliptic cylindrical coordinates about
  the x-axis, which is relabeled the z-axis. The third set of coordinates
  consists of planes passing through this axis.
  The coordinate system is parameterised by parameter a. A default value of a=1 is
  suggesed:
  Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html *)
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: double;
  var x, y, z: double); overload;
(* Convert Prolate-Spheroidal to Cartesian [single](with error check). eta,phi in rad
  ierr: [0] = ok,
  [1] = xi out of bounds. Acceptable xi: [0,inf)
  [2] = eta out of bounds. Acceptable eta: [0,pi]
  [3] = phi out of bounds. Acceptable phi: [0,2pi)
  Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html *)
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: single;
  var x, y, z: single; var ierr: integer); overload;
(* Convert Prolate-Spheroidal to Cartesian [double](with error check). eta,phi in rad
  ierr: [0] = ok,
  [1] = xi out of bounds. Acceptable xi: [0,inf)
  [2] = eta out of bounds. Acceptable eta: [0,pi]
  [3] = phi out of bounds. Acceptable phi: [0,2pi)
  Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html *)
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: double;
  var x, y, z: double;  var ierr: integer); overload;
// Convert Oblate-Spheroidal to Cartesian. [Single] eta, phi in rad
procedure OblateSpheroidal_Cartesian(const xi, eta, phi, a: single;
  var x, y, z: single); overload;
// Convert Oblate-Spheroidal to Cartesian. [Double] eta, phi in rad
procedure OblateSpheroidal_Cartesian(const xi, eta, phi, a: double;
  var x, y, z: double); overload;
// Convert Oblate-Spheroidal to Cartesian (with error check). eta,phi in rad
procedure OblateSpheroidal_Cartesian(const xi, eta, phi, a: single;
  var x, y, z: single; var ierr: integer); overload;
(* Convert Oblate-Spheroidal to Cartesian with checks. [Double] eta,phi in rad
  ierr: [0] = ok,
  [1] = xi out of bounds. Acceptable xi: [0,inf)
  [2] = eta out of bounds. Acceptable eta: [-0.5*pi,0.5*pi]
  [3] = phi out of bounds. Acceptable phi: [0,2*pi)
  Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html *)
procedure OblateSpheroidal_Cartesian(const xi, eta, phi, a: double;
  var x, y, z: double; var ierr: integer); overload;
// Convert Bipolar to Cartesian. u in rad
procedure BipolarCylindrical_Cartesian(const u, v, z1, a: single;
  var x, y, z: single); overload;
(* Convert BiPolarCylindrical to Cartesian with no checks. Double, u in rad
  http://mathworld.wolfram.com/BipolarCylindricalCoordinates.html *)
procedure BipolarCylindrical_Cartesian(const u, v, z1, a: double;
  var x, y, z: double); overload;
// Convert Bipolar to Cartesian (with error check). u in rad
procedure BipolarCylindrical_Cartesian(const u, v, z1, a: single;
  var x, y, z: single; var ierr: integer); overload;
(* Convert Oblate-Spheroidal to Cartesian with checks. Double, u in rad
  ierr: [0] = ok,
  [1] = u out of bounds. Acceptable u: [0,2*pi)
  [2] = v out of bounds. Acceptable v: (-inf,inf)
  [3] = z1 out of bounds. Acceptable z1: (-inf,inf)
  Ref: https://mathworld.wolfram.com/BipolarCylindricalCoordinates.html *)
procedure BipolarCylindrical_Cartesian(const u, v, z1, a: double;
  var x, y, z: double; var ierr: integer); overload;

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
  const AValue: TGLVector; const AStyle: TGLCoordinatesStyle = CsUnknown);
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

procedure TGLCustomCoordinates.Initialize(const Value: TGLVector);
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

procedure TGLCustomCoordinates.Translate(const TranslationVector: TGLVector);
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
  const TranslationVector: TGLVector);
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

procedure TGLCustomCoordinates.Rotate(const AnAxis: TGLVector; AnAngle: Single);
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

function TGLCustomCoordinates.VectorLength: Single;
begin
  Result := GLS.VectorGeometry.VectorLength(FCoords);
end;

function TGLCustomCoordinates.VectorNorm: Single;
begin
  Result := GLS.VectorGeometry.VectorNorm(FCoords);
end;

function TGLCustomCoordinates.MaxXYZ: Single;
begin
  Result := MaxXYZComponent(FCoords);
end;

function TGLCustomCoordinates.Equals(const AVector: TGLVector): Boolean;
begin
  Result := VectorEquals(FCoords, AVector);
end;

procedure TGLCustomCoordinates.SetVector(const X, Y: Single; Z: Single = 0);
begin
  Assert(FStyle = csVector, csVectorHelp);
  GLS.VectorGeometry.SetVector(FCoords, X, Y, Z);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetVector(const V: TAffineVector);
begin
  Assert(FStyle = csVector, csVectorHelp);
  GLS.VectorGeometry.SetVector(FCoords, V);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetVector(const V: TGLVector);
begin
  Assert(FStyle = csVector, csVectorHelp);
  GLS.VectorGeometry.SetVector(FCoords, V);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetVector(const X, Y, Z, W: Single);
begin
  Assert(FStyle = csVector, csVectorHelp);
  GLS.VectorGeometry.SetVector(FCoords, X, Y, Z, W);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetDirectCoordinate(const Index: Integer;
  const AValue: Single);
begin
  FCoords.V[index] := AValue;
end;

procedure TGLCustomCoordinates.SetDirectVector(const V: TGLVector);
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

procedure TGLCustomCoordinates.SetPoint(const V: TGLVector);
begin
  Assert(FStyle = CsPoint, CsPointHelp);
  MakePoint(FCoords, V);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetPoint2D(const X, Y: Single);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  GLS.VectorGeometry.MakeVector(FCoords, X, Y, 0);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetPoint2D(const Vector: TAffineVector);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  MakeVector(FCoords, Vector);
  NotifyChange(Self);
end;

procedure TGLCustomCoordinates.SetPoint2D(const Vector: TGLVector);
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

function TGLCustomCoordinates.AsAddress: PSingle;
begin
  Result := @FCoords;
end;

procedure TGLCustomCoordinates.SetAsVector(const Value: TGLVector);
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
  GLS.VectorGeometry.SetVector(Result, FCoords);
end;

function TGLCustomCoordinates.GetAsPoint2D: TVector2f;
begin
  Result.X := FCoords.X;
  Result.Y := FCoords.Y;
end;

procedure TGLCustomCoordinates.SetCoordinate(const AIndex: Integer;
  const AValue: Single);
begin
  FCoords.V[AIndex] := AValue;
  NotifyChange(Self);
end;

function TGLCustomCoordinates.GetCoordinate(const AIndex: Integer): Single;
begin
  Result := FCoords.V[AIndex];
end;

function TGLCustomCoordinates.GetDirectCoordinate(
  const Index: Integer): Single;
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

// ----------------- Conversions of coordinates --------------------

// ----------------- Cylindrical_Cartesian ----------------------
procedure Cylindrical_Cartesian(const r, theta, z1: single; var x, y, z: single);
begin
  SinCosine(theta, r, y, x);
  z := z1;
end;

// ----- Cylindrical_Cartesian -------------------------------------
procedure Cylindrical_Cartesian(const r, theta, z1: double; var x, y, z: double);
begin
  SinCosine(theta, r, y, x);
  z := z1;
end;

// ------------------ Cylindrical_Cartesian -----------------------
procedure Cylindrical_Cartesian(const r, theta, z1: single; var x, y, z: single;
  var ierr: integer);

begin
  // check input parameters
  if (r < 0.0) then
    ierr := 1
  else if ((theta < 0.0) or (theta >= 2 * pi)) then
    ierr := 2
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    SinCosine(theta, r, y, x);
    z := z1;
  end;
end;

// ----- Cylindrical_Cartesian -------------------------------------------------
procedure Cylindrical_Cartesian(const r, theta, z1: double; var x, y, z: double;
  var ierr: integer);

begin
  // check input parameters
  if (r < 0.0) then
    ierr := 1
  else if ((theta < 0.0) or (theta >= 2 * pi)) then
    ierr := 2
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    SinCosine(theta, r, y, x);
    z := z1;
  end;
end;

// ----- Cartesian_Cylindrical -------------------------------------------------
procedure Cartesian_Cylindrical(const x, y, z1: single; var r, theta, z: single);
begin
  r := sqrt(x * x + y * y);
  theta := ArcTan2(y, x);
  z := z1;
end;

// ----- Cartesian_Cylindrical -------------------------------------------------
procedure Cartesian_Cylindrical(const x, y, z1: double; var r, theta, z: double);
begin
  r := sqrt(x * x + y * y);
  theta := ArcTan2(y, x);
  z := z1;
end;

// ----- Spherical_Cartesian ---------------------------------------------------
procedure Spherical_Cartesian(const r, theta, phi: single; var x, y, z: single);
var
  a: single;
begin
  SinCosine(phi, r, a, z); // z = r*cos(phi), a = r*sin(phi)
  SinCosine(theta, a, y, x); // x = a*cos(theta), y = a*sin(theta)}
end;

// ----- Spherical_Cartesian ---------------------------------------------------
procedure Spherical_Cartesian(const r, theta, phi: double; var x, y, z: double);
var
  a: double;
begin
  SinCosine(phi, r, a, z); // z = r*cos(phi), a = r*sin(phi)
  SinCosine(theta, a, y, x); // x = a*cos(theta), y = a*sin(theta)}
end;

// ----- Spherical_Cartesian ---------------------------------------------------
procedure Spherical_Cartesian(const r, theta, phi: single; var x, y, z: single;
  var ierr: integer);
var
  a: single;
begin
  if (r < 0.0) then
    ierr := 1
  else if ((theta < 0.0) or (theta >= 2 * pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2 * pi)) then
    ierr := 3
  else
    ierr := 0;
  if (ierr = 0) then
  begin
    SinCosine(phi, r, a, z); // z = r*cos(phi), a = r*sin(phi)
    SinCosine(theta, a, y, x); // x = a*cos(theta), y = a*sin(theta)}
  end;
end;

// ----- Spherical_Cartesian ---------------------------------------------------
(* Convert Spherical to Cartesian with checks.
  ierr: [0] = ok,
  [1] = r out of bounds
  [2] = theta out of bounds
  [3] = phi out of bounds
  Ref: http://mathworld.wolfram.com/SphericalCoordinates.html *)
procedure Spherical_Cartesian(const r, theta, phi: double; var x, y, z: double;
  var ierr: integer);
var
  a: double;
begin
  if (r < 0.0) then
    ierr := 1
  else if ((theta < 0.0) or (theta >= 2 * pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2 * pi)) then
    ierr := 3
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    SinCosine(phi, r, a, z); // z = r*cos(phi), a=r*sin(phi)
    SinCosine(theta, a, y, x); // x = a*cos(theta), y = a*sin(theta)}
  end;
end;

// ----- Cartesian_Spherical ---------------------------------------------------
procedure Cartesian_Spherical(const x, y, z: single; var r, theta, phi: single);
begin
  r := sqrt((x * x) + (y * y) + (z * z));
  theta := ArcTan2(y, x);
  phi := ArcCosine(z / r);
end;

procedure Cartesian_Spherical(const v: TAffineVector; var r, theta, phi: single);
begin
  r := VectorLength(v);
  theta := ArcTan2(v.y, v.x);
  phi := ArcCosine(v.z / r);
end;

// ----- Cartesian_Spherical ---------------------------------------------------
procedure Cartesian_Spherical(const x, y, z: double; var r, theta, phi: double);
begin
  r := sqrt((x * x) + (y * y) + (z * z));
  theta := ArcTan2(y, x);
  phi := ArcCosine(z / r);
end;

// ----- ProlateSpheroidal_Cartesian -------------------------------------------
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: single; var x, y, z: single);
var
  sn, cs, snphi, csphi, shx, chx: single;
begin
  SinCosine(eta, a, sn, cs);
  SinCosine(phi, snphi, csphi);
  shx := sinh(xi);
  chx := cosh(xi);
  x := sn * shx * csphi; // x = a*sin(eta)*sinh(xi)*cos(phi)
  y := sn * shx * snphi; // y = a*sin(eta)*sinh(xi)*sin(phi)
  z := cs * chx; // z = a*cos(eta)*cosh(xi)
end;

// ----- ProlateSpheroidal_Cartesian -------------------------------------------
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: double; var x, y, z: double);
var
  sn, cs, snphi, csphi, shx, chx: double;
begin
  SinCosine(eta, a, sn, cs);
  SinCosine(phi, snphi, csphi);
  shx := sinh(xi);
  chx := cosh(xi);
  x := sn * shx * csphi; // x = a*sin(eta)*sinh(xi)*cos(phi)
  y := sn * shx * snphi; // y = a*sin(eta)*sinh(xi)*sin(phi)
  z := cs * chx; // z = a*cos(eta)*cosh(xi)
end;

// ----- ProlateSpheroidal_Cartesian -------------------------------------------
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: single;
  var x, y, z: single; var ierr: integer); overload;
var
  sn, cs, snphi, csphi, shx, chx: single;
begin
  if (xi < 0.0) then
    ierr := 1
  else if ((eta < 0.0) or (eta > pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2 * pi)) then
    ierr := 3
  else
    ierr := 0;
  if (ierr = 0) then
  begin
    SinCosine(eta, a, sn, cs);
    SinCosine(phi, snphi, csphi);
    shx := sinh(xi);
    chx := cosh(xi);
    x := sn * shx * csphi; // x = a*sin(eta)*sinh(xi)*cos(phi)
    y := sn * shx * snphi; // y = a*sin(eta)*sinh(xi)*sin(phi)
    z := cs * chx; // z = a*cos(eta)*cosh(xi)
  end;
end;

// ----- ProlateSpheroidal_Cartesian -------------------------------------------
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: double;
  var x, y, z: double; var ierr: integer); overload;
var
  sn, cs, snphi, csphi, shx, chx: double;
begin
  if (xi < 0.0) then
    ierr := 1
  else if ((eta < 0.0) or (eta > pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2 * pi)) then
    ierr := 3
  else
    ierr := 0;
  if (ierr = 0) then
  begin
    SinCosine(eta, a, sn, cs);
    SinCosine(phi, snphi, csphi);
    shx := sinh(xi);
    chx := cosh(xi);
    x := sn * shx * csphi; // x = a*sin(eta)*sinh(xi)*cos(phi)
    y := sn * shx * snphi; // y = a*sin(eta)*sinh(xi)*sin(phi)
    z := cs * chx; // z = a*cos(eta)*cosh(xi)
  end;
end;

// ----- OblateSpheroidal_Cartesian -------------------------------------------
(* Convert Oblate-Spheroidal to Cartesian with no checks.
  A system of curvilinear coordinates in which two sets of coordinate surfaces are
  obtained by revolving the curves of the elliptic cylindrical coordinates about
  the y-axis which is relabeled the z-axis. The third set of coordinates consists
  of planes passing through this axis.
  The coordinate system is parameterised by parameter a. A default value of a=1 is
  suggesed:
  http://documents.wolfram.com/v4/AddOns/StandardPackages/Calculus/VectorAnalysis.html
  Ref: http://mathworld.wolfram.com/OblateSpheroidalCoordinates.html *)
procedure OblateSpheroidal_Cartesian(const xi, eta, phi, a: single; var x, y, z: single);
var
  sn, cs, snphi, csphi, shx, chx: single;
begin
  SinCosine(eta, a, sn, cs);
  SinCosine(phi, snphi, csphi);
  shx := sinh(xi);
  chx := cosh(xi);
  x := cs * chx * csphi; // x = a*cos(eta)*cosh(xi)*cos(phi)
  y := cs * chx * snphi; // y = a*cos(eta)*cosh(xi)*sin(phi)
  z := sn * shx; // z = a*sin(eta)*sinh(xi)
end;

// ----- OblateSpheroidal_Cartesian -------------------------------------------
(* Convert Oblate-Spheroidal to Cartesian with no checks. Double Version.
  A system of curvilinear coordinates in which two sets of coordinate surfaces are
  obtained by revolving the curves of the elliptic cylindrical coordinates about
  the y-axis which is relabeled the z-axis. The third set of coordinates consists
  of planes passing through this axis.
  The coordinate system is parameterised by parameter a. A default value of a=1 is
  suggesed:
  http://documents.wolfram.com/v4/AddOns/StandardPackages/Calculus/VectorAnalysis.html
  Ref: http://mathworld.wolfram.com/OblateSpheroidalCoordinates.html *)
procedure OblateSpheroidal_Cartesian(const xi, eta, phi, a: double; var x, y, z: double);
var
  sn, cs, snphi, csphi, shx, chx: double;
begin
  SinCosine(eta, a, sn, cs);
  SinCosine(phi, snphi, csphi);
  shx := sinh(xi);
  chx := cosh(xi);
  x := cs * chx * csphi; // x = a*cos(eta)*cosh(xi)*cos(phi)
  y := cs * chx * snphi; // y = a*cos(eta)*cosh(xi)*sin(phi)
  z := sn * shx; // z = a*sin(eta)*sinh(xi)
end;

// ----- OblateSpheroidal_Cartesian -------------------------------------------
(* Convert Oblate-Spheroidal to Cartesian with checks.
  ierr: [0] = ok,
  [1] = xi out of bounds. Acceptable xi: [0,inf)
  [2] = eta out of bounds. Acceptable eta: [-0.5*pi,0.5*pi]
  [3] = phi out of bounds. Acceptable phi: [0,2*pi)
  Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html *)
procedure OblateSpheroidal_Cartesian(const xi, eta, phi, a: single;
  var x, y, z: single; var ierr: integer); overload;
var
  sn, cs, snphi, csphi, shx, chx: single;
begin
  if (xi < 0.0) then
    ierr := 1
  else if ((eta < -0.5 * pi) or (eta > 0.5 * pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2 * pi)) then
    ierr := 3
  else
    ierr := 0;
  if (ierr = 0) then
  begin
    SinCosine(eta, a, sn, cs);
    SinCosine(phi, snphi, csphi);
    shx := sinh(xi);
    chx := cosh(xi);
    x := cs * chx * csphi; // x = a*cos(eta)*cosh(xi)*cos(phi)
    y := cs * chx * snphi; // y = a*cos(eta)*cosh(xi)*sin(phi)
    z := sn * shx; // z = a*sin(eta)*sinh(xi)
  end;
end;

// ----- OblateSpheroidal_Cartesian -------------------------------------------
procedure OblateSpheroidal_Cartesian(const xi, eta, phi, a: double;
  var x, y, z: double; var ierr: integer); overload;
var
  sn, cs, snphi, csphi, shx, chx: double;
begin
  if (xi < 0.0) then
    ierr := 1
  else if ((eta < -0.5 * pi) or (eta > 0.5 * pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2 * pi)) then
    ierr := 3
  else
    ierr := 0;
  if (ierr = 0) then
  begin
    SinCosine(eta, a, sn, cs);
    SinCosine(phi, snphi, csphi);
    shx := sinh(xi);
    chx := cosh(xi);
    x := cs * chx * csphi; // x = a*cos(eta)*cosh(xi)*cos(phi)
    y := cs * chx * snphi; // y = a*cos(eta)*cosh(xi)*sin(phi)
    z := sn * shx; // z = a*sin(eta)*sinh(xi)
  end;
end;

// ----- BipolarCylindrical_Cartesian ------------------------------------------
(* Convert BiPolarCylindrical to Cartesian with no checks.
  http://mathworld.wolfram.com/BipolarCylindricalCoordinates.html *)
procedure BipolarCylindrical_Cartesian(const u, v, z1, a: single; var x, y, z: single);
var
  cs, sn, shx, chx: single;
begin
  SinCosine(u, sn, cs);
  shx := sinh(v);
  chx := cosh(v);
  x := a * shx / (chx - cs);
  y := a * sn / (chx - cs);
  z := z1;
end;

// ----- BipolarCylindrical_Cartesian ------------------------------------------
procedure BipolarCylindrical_Cartesian(const u, v, z1, a: double; var x, y, z: double);
var
  cs, sn, shx, chx: double;
begin
  SinCosine(u, sn, cs);
  shx := sinh(v);
  chx := cosh(v);
  x := a * shx / (chx - cs);
  y := a * sn / (chx - cs);
  z := z1;
end;

// ----- BipolarCylindrical_Cartesian ------------------------------------------
(* Convert Oblate-Spheroidal to Cartesian with checks.
  ierr: [0] = ok,
  [1] = u out of bounds. Acceptable u: [0,2*pi)
  [2] = v out of bounds. Acceptable v: (-inf,inf)
  [3] = z1 out of bounds. Acceptable z1: (-inf,inf)
  Ref: http://mathworld.wolfram.com/BiPolarCylindricalCoordinates.html *)
procedure BipolarCylindrical_Cartesian(const u, v, z1, a: single;
  var x, y, z: single; var ierr: integer); overload;
var
  cs, sn, shx, chx: single;
begin
  if ((u < 0.0) or (u >= 2 * pi)) then
    ierr := 1
  else
    ierr := 0;
  if (ierr = 0) then
  begin
    SinCosine(u, sn, cs);
    shx := sinh(v);
    chx := cosh(v);
    x := a * shx / (chx - cs);
    y := a * sn / (chx - cs);
    z := z1;
  end;
end;

// ----- BipolarCylindrical_Cartesian ------------------------------------------
procedure BipolarCylindrical_Cartesian(const u, v, z1, a: double;
  var x, y, z: double; var ierr: integer); overload;
var
  cs, sn, shx, chx: double;
begin
  if ((u < 0.0) or (u >= 2 * pi)) then
    ierr := 1
  else
    ierr := 0;
  if (ierr = 0) then
  begin
    SinCosine(u, sn, cs);
    shx := sinh(v);
    chx := cosh(v);
    x := a * shx / (chx - cs);
    y := a * sn / (chx - cs);
    z := z1;
  end;
end;

function BarycentricCoordinates(const V1, V2, V3, p: TAffineVector;
  var u, V: Single): Boolean;
var
  a1, a2: Integer;
  n, e1, e2, pt: TAffineVector;
begin
  // calculate edges
  VectorSubtract(V1, V3, e1);
  VectorSubtract(V2, V3, e2);

  // calculate p relative to v3
  VectorSubtract(p, V3, pt);

  // find the dominant axis
  n := VectorCrossProduct(e1, e2);
  AbsVector(n);
  a1 := 0;
  if n.Y > n.V[a1] then
    a1 := 1;
  if n.Z > n.V[a1] then
    a1 := 2;

  // use dominant axis for projection
  case a1 of
    0:
      begin
        a1 := 1;
        a2 := 2;
      end;
    1:
      begin
        a1 := 0;
        a2 := 2;
      end;
  else // 2:
    a1 := 0;
    a2 := 1;
  end;

  // solve for u and v
  u := (pt.V[a2] * e2.V[a1] - pt.V[a1] * e2.V[a2]) /
    (e1.V[a2] * e2.V[a1] - e1.V[a1] * e2.V[a2]);
  V := (pt.V[a2] * e1.V[a1] - pt.V[a1] * e1.V[a2]) /
    (e2.V[a2] * e1.V[a1] - e2.V[a1] * e1.V[a2]);

  result := (u >= 0) and (V >= 0) and (u + V <= 1);
end;


//=====================================================================
initialization
//=====================================================================

RegisterClasses([TGLCoordinates2, TGLCoordinates3, TGLCoordinates4]);

end.
