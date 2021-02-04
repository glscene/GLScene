//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.VerletSkeletonColliders;

(* Skeleton colliders for defining and controlling verlet constraints. *)

interface

uses
  System.Classes,
  GLS.PersistentClasses,
  GLS.VectorGeometry,
  GLS.VectorFileObjects,
  GLS.VerletTypes,
  GLS.VectorTypes;

type
  // Base Verlet Skeleton Collider class.
  TGLVerletBase = class(TGLSkeletonCollider)
  private
    FVerletConstraint: TGLVerletConstraint;
  public
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    procedure AddToVerletWorld(VerletWorld: TGLVerletWorld); virtual;
    // The verlet constraint is created through the AddToVerletWorld procedure
    property VerletConstraint: TGLVerletConstraint read FVerletConstraint;
  end;

  // Sphere shaped verlet constraint in a skeleton collider
  TGLVerletSphere = class(TGLVerletBase)
  private
    FRadius: Single;
  protected
    procedure SetRadius(const val: Single);
  public
    constructor Create; override;
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    procedure AddToVerletWorld(VerletWorld: TGLVerletWorld); override;
    procedure AlignCollider; override;
    property Radius: Single read FRadius write SetRadius;
  end;

  // Capsule shaped verlet constraint in a skeleton collider
  TGLVerletCapsule = class(TGLVerletBase)
  private
    FRadius, FLength: Single;
  protected
    procedure SetRadius(const val: Single);
    procedure SetLength(const val: Single);
  public
    constructor Create; override;
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    procedure AddToVerletWorld(VerletWorld: TGLVerletWorld); override;
    procedure AlignCollider; override;
    property Radius: Single read FRadius write SetRadius;
    property Length: Single read FLength write SetLength;
  end;

  (* After loading call this function to add all the constraints in a
    skeleton collider list to a given verlet world. *)
procedure AddSCVerletConstriantsToVerletWorld
  (colliders: TGLSkeletonColliderList; world: TGLVerletWorld);

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ Global methods ------------------
// ------------------

procedure AddSCVerletConstriantsToVerletWorld
  (colliders: TGLSkeletonColliderList; world: TGLVerletWorld);
var
  i: Integer;
begin
  for i := 0 to colliders.Count - 1 do
    if colliders[i] is TGLVerletBase then
      TGLVerletBase(colliders[i]).AddToVerletWorld(world);
end;

// ------------------
// ------------------ TGLVerletBase ------------------
// ------------------

procedure TGLVerletBase.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
  end;
end;

procedure TGLVerletBase.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
      // Nothing yet
    else
      RaiseFilerException(archiveVersion);
end;

procedure TGLVerletBase.AddToVerletWorld(VerletWorld: TGLVerletWorld);
begin
  AlignCollider;
end;

// ------------------
// ------------------ TGLVerletSphere ------------------
// ------------------
constructor TGLVerletSphere.Create;
begin
  inherited;
  Radius := 0.5;
  AlignCollider;
end;

procedure TGLVerletSphere.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FRadius);
  end;
end;

procedure TGLVerletSphere.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
      Radius := ReadFloat
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLVerletSphere.AddToVerletWorld(VerletWorld: TGLVerletWorld);
begin
  FVerletConstraint := TVCSphere.Create(VerletWorld);
  TVCSphere(FVerletConstraint).Radius := FRadius;
  inherited;
end;

procedure TGLVerletSphere.AlignCollider;
begin
  inherited;
  if Assigned(FVerletConstraint) then
    TVCSphere(FVerletConstraint).Location := AffineVectorMake(GlobalMatrix.W);
end;

procedure TGLVerletSphere.SetRadius(const val: Single);
begin
  if val <> FRadius then
  begin
    FRadius := val;
    if Assigned(FVerletConstraint) then
      TVCSphere(FVerletConstraint).Radius := FRadius;
  end;
end;

// ------------------
// ------------------ TGLVerletCapsule ------------------
// ------------------
constructor TGLVerletCapsule.Create;
begin
  inherited;
  Radius := 0.5;
  Length := 1;
  AlignCollider;
end;

procedure TGLVerletCapsule.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FRadius);
    WriteFloat(FLength);
  end;
end;

procedure TGLVerletCapsule.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      Radius := ReadFloat;
      Length := ReadFloat;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLVerletCapsule.AddToVerletWorld(VerletWorld: TGLVerletWorld);
begin
  FVerletConstraint := TVCCapsule.Create(VerletWorld);
  TVCCapsule(FVerletConstraint).Radius := FRadius;
  TVCCapsule(FVerletConstraint).Length := FLength;
  inherited;
end;

procedure TGLVerletCapsule.AlignCollider;
begin
  inherited;
  if Assigned(FVerletConstraint) then
  begin
    TVCCapsule(FVerletConstraint).Location := AffineVectorMake(GlobalMatrix.W);
    TVCCapsule(FVerletConstraint).Axis := AffineVectorMake(GlobalMatrix.Y);
  end;
end;

procedure TGLVerletCapsule.SetRadius(const val: Single);
begin
  if val <> FRadius then
  begin
    FRadius := val;
    if Assigned(FVerletConstraint) then
      TVCCapsule(FVerletConstraint).Radius := FRadius;
  end;
end;

procedure TGLVerletCapsule.SetLength(const val: Single);
begin
  if val <> FLength then
  begin
    FLength := val;
    if Assigned(FVerletConstraint) then
      TVCCapsule(FVerletConstraint).Length := FLength;
  end;
end;

// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------

RegisterClasses([TGLVerletBase, TGLVerletSphere, TGLVerletCapsule]);

end.
