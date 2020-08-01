//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLVerletSkeletonColliders;

(* Skeleton colliders for defining and controlling verlet constraints. *)

interface

uses
  System.Classes,
  Scene.PersistentClasses,
  Scene.VectorGeometry,
  GLVectorFileObjects,
  GLVerletTypes,
  Scene.VectorTypes;

type

  //Base verlet skeleton collider class.
  TSCVerletBase = class(TGLSkeletonCollider)
    private
      FVerletConstraint : TVerletConstraint;
    public
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;
      procedure AddToVerletWorld(VerletWorld : TGLVerletWorld); virtual;
      // The verlet constraint is created through the AddToVerletWorld procedure
      property VerletConstraint : TVerletConstraint read FVerletConstraint;
  end;

  // Sphere shaped verlet constraint in a skeleton collider
  TSCVerletSphere = class(TSCVerletBase)
    private
      FRadius : Single;
    protected
      procedure SetRadius(const val : Single);
    public
      constructor Create; override;
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;
      procedure AddToVerletWorld(VerletWorld : TGLVerletWorld); override;
      procedure AlignCollider; override;

      property Radius : Single read FRadius write SetRadius;
  end;

  // Capsule shaped verlet constraint in a skeleton collider
  TSCVerletCapsule = class(TSCVerletBase)
    private
      FRadius,
      FLength : Single;
    protected
      procedure SetRadius(const val : Single);
      procedure SetLength(const val : Single);
    public
      constructor Create; override;
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;
      procedure AddToVerletWorld(VerletWorld : TGLVerletWorld); override;
      procedure AlignCollider; override;
      property Radius : Single read FRadius write SetRadius;
      property Length : Single read FLength write SetLength;
  end;

(* After loading call this function to add all the constraints in a
   skeleton collider list to a given verlet world. *)
procedure AddSCVerletConstriantsToVerletWorld(
  colliders : TGLSkeletonColliderList; world : TGLVerletWorld);

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
    if colliders[i] is TSCVerletBase then
      TSCVerletBase(colliders[i]).AddToVerletWorld(world);
end;

// ------------------
// ------------------ TSCVerletBase ------------------
// ------------------

procedure TSCVerletBase.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
  end;
end;

procedure TSCVerletBase.ReadFromFiler(reader: TVirtualReader);
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

procedure TSCVerletBase.AddToVerletWorld(VerletWorld: TGLVerletWorld);
begin
  AlignCollider;
end;


// ------------------
// ------------------ TSCVerletSphere ------------------
// ------------------

constructor TSCVerletSphere.Create;
begin
  inherited;
  Radius := 0.5;
  AlignCollider;
end;

procedure TSCVerletSphere.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FRadius);
  end;
end;

procedure TSCVerletSphere.ReadFromFiler(reader: TVirtualReader);
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

procedure TSCVerletSphere.AddToVerletWorld(VerletWorld: TGLVerletWorld);
begin
  FVerletConstraint := TVCSphere.Create(VerletWorld);
  TVCSphere(FVerletConstraint).Radius := FRadius;
  inherited;
end;

procedure TSCVerletSphere.AlignCollider;
begin
  inherited;
  if Assigned(FVerletConstraint) then
    TVCSphere(FVerletConstraint).Location := AffineVectorMake(GlobalMatrix.W);
end;

procedure TSCVerletSphere.SetRadius(const val: Single);
begin
  if val <> FRadius then
  begin
    FRadius := val;
    if Assigned(FVerletConstraint) then
      TVCSphere(FVerletConstraint).Radius := FRadius;
  end;
end;

// ------------------
// ------------------ TSCVerletCapsule ------------------
// ------------------

constructor TSCVerletCapsule.Create;
begin
  inherited;
  Radius := 0.5;
  Length := 1;
  AlignCollider;
end;

procedure TSCVerletCapsule.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FRadius);
    WriteFloat(FLength);
  end;
end;

procedure TSCVerletCapsule.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion : integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion:=reader.ReadInteger;
  if archiveVersion=0 then with reader do begin
    Radius:=ReadFloat;
    Length:=ReadFloat;
  end else RaiseFilerException(archiveVersion);
end;

procedure TSCVerletCapsule.AddToVerletWorld(VerletWorld : TGLVerletWorld);
begin
  FVerletConstraint := TVCCapsule.Create(VerletWorld);
  TVCCapsule(FVerletConstraint).Radius := FRadius;
  TVCCapsule(FVerletConstraint).Length := FLength;
  inherited;
end;

procedure TSCVerletCapsule.AlignCollider;
begin
  inherited;
  if Assigned(FVerletConstraint) then
  begin
    TVCCapsule(FVerletConstraint).Location := AffineVectorMake(GlobalMatrix.W);
    TVCCapsule(FVerletConstraint).Axis := AffineVectorMake(GlobalMatrix.Y);
  end;
end;

procedure TSCVerletCapsule.SetRadius(const val : Single);
begin
  if val <> FRadius then
  begin
    FRadius := val;
    if Assigned(FVerletConstraint) then
      TVCCapsule(FVerletConstraint).Radius := FRadius;
  end;
end;

procedure TSCVerletCapsule.SetLength(const val : Single);
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

  RegisterClasses([TSCVerletBase,TSCVerletSphere,TSCVerletCapsule]);

end.
