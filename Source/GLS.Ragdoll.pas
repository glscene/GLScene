//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.Ragdoll;

(* Base abstract ragdoll class. Should be extended to use any physics system. *)

interface

{$I GLScene.inc}

uses
  GLS.Scene,
  GLS.PersistentClasses,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorFileObjects,
  GLS.VectorLists,
  GLS.Objects;

type
  TGLRagdoll = class;
  TGLRagdolBone = class;
  TGLRagdolJoint = class
  end;

  TGLRagdolBoneList = class(TPersistentObjectList)
  private
    FRagdoll: TGLRagdoll;
  protected
    function GetRagdollBone(Index: Integer): TGLRagdolBone;
  public
    constructor Create(Ragdoll: TGLRagdoll); reintroduce;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    property Ragdoll: TGLRagdoll read FRagdoll;
    property Items[Index: Integer]: TGLRagdolBone read GetRagdollBone; default;
  end;

  TGLRagdolBone = class(TGLRagdolBoneList)
  private
    FOwner: TGLRagdolBoneList;
    FName: String;
    FBoneID: Integer; // Refering to TGLActor Bone
    FBoundMax: TAffineVector;
    FBoundMin: TAffineVector;
    FBoundBoneDelta: TAffineVector;
    // Stores the diference from the bone.GlobalMatrix to the center of the bone's bounding box
    FOrigin: TAffineVector;
    FSize: TAffineVector;
    FBoneMatrix: TGLMatrix;
    FJoint: TGLRagdolJoint;
    FOriginalMatrix: TGLMatrix;
    // Stores the Bone.GlobalMatrix before the ragdoll start
    FReferenceMatrix: TGLMatrix;
    // Stores the first bone matrix to be used as reference
    FAnchor: TAffineVector; // The position of the joint
    procedure CreateBoundingBox;
    procedure SetAnchor(const Anchor: TAffineVector);
    procedure AlignToSkeleton;
    procedure CreateBoundsChild;
    procedure StartChild;
    procedure AlignChild;
    procedure UpdateChild;
    procedure StopChild;
  protected
    function GetRagdollBone(Index: Integer): TGLRagdolBone;
    procedure Start; virtual; abstract;
    procedure Align; virtual; abstract;
    procedure Update; virtual; abstract;
    procedure Stop; virtual; abstract;
  public
    constructor CreateOwned(aOwner: TGLRagdolBoneList);
    constructor Create(Ragdoll: TGLRagdoll);
    destructor Destroy; override;
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    property Owner: TGLRagdolBoneList read FOwner;
    property Name: String read FName write FName;
    property BoneID: Integer read FBoneID write FBoneID;
    property Origin: TAffineVector read FOrigin;
    property Size: TAffineVector read FSize;
    property BoneMatrix: TGLMatrix read FBoneMatrix;
    property ReferenceMatrix: TGLMatrix read FReferenceMatrix;
    property Anchor: TAffineVector read FAnchor;
    property Joint: TGLRagdolJoint read FJoint write FJoint;
    property Items[Index: Integer]: TGLRagdolBone read GetRagdollBone; default;
  end;

  TGLRagdoll = class(TPersistentObject)
  private
    FOwner: TGLBaseMesh;
    FRootBone: TGLRagdolBone;
    FEnabled: Boolean;
    FBuilt: Boolean;
  public
    constructor Create(aOwner: TGLBaseMesh); reintroduce;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    // Must be set before build the ragdoll
    procedure SetRootBone(RootBone: TGLRagdolBone);
    // Create the bounding box and setup the ragdoll do be started later
    procedure BuildRagdoll;
    procedure Start;
    procedure Update;
    procedure Stop;
    property Owner: TGLBaseMesh read FOwner;
    property RootBone: TGLRagdolBone read FRootBone;
    property Enabled: Boolean read FEnabled;
  end;

// ------------------------------------------------------------------------
implementation
// ------------------------------------------------------------------------


//--------------------------
// TGLRagdolBoneList
//--------------------------

constructor TGLRagdolBoneList.Create(Ragdoll: TGLRagdoll);
begin
  inherited Create;
  FRagdoll := Ragdoll;
end;

destructor TGLRagdolBoneList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Destroy;
  inherited;
end;

function TGLRagdolBoneList.GetRagdollBone(Index: Integer): TGLRagdolBone;
begin
  Result := TGLRagdolBone(List^[Index]);
end;

procedure TGLRagdolBoneList.ReadFromFiler(reader: TVirtualReader);
begin
  inherited;
  // Not implemented
end;

procedure TGLRagdolBoneList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited;
  // Not implemented
end;

//----------------------------------
// TGLRagdolBone
//----------------------------------

constructor TGLRagdolBone.Create(Ragdoll: TGLRagdoll);
begin
  inherited Create(Ragdoll);
end;

procedure TGLRagdolBone.CreateBoundingBox;
var
  bone: TGLSkeletonBone;
  i, j: Integer;
  BoneVertices: TAffineVectorList;
  BoneVertex, max, min: TAffineVector;
  invMat, mat: TGLMatrix;
begin
  bone := Ragdoll.Owner.Skeleton.BoneByID(FBoneID);

  // Get all vertices weighted to this bone
  BoneVertices := TAffineVectorList.Create;
  for i := 0 to Ragdoll.Owner.MeshObjects.Count - 1 do
    with TGLSkeletonMeshObject(Ragdoll.Owner.MeshObjects[i]) do
      for j := 0 to Vertices.Count - 1 do
        if bone.BoneID = VerticesBonesWeights[j][0].BoneID then
          BoneVertices.FindOrAdd(Vertices[j]);

  invMat := bone.GlobalMatrix;
  InvertMatrix(invMat);

  // For each vertex, get the max and min XYZ (Bounding box)
  if BoneVertices.Count > 0 then
  begin
    BoneVertex := VectorTransform(BoneVertices[0], invMat);
    max := BoneVertex;
    min := BoneVertex;
    for i := 1 to BoneVertices.Count - 1 do
    begin
      BoneVertex := VectorTransform(BoneVertices[i], invMat);
      if (BoneVertex.X > max.X) then
        max.X := BoneVertex.X;
      if (BoneVertex.Y > max.Y) then
        max.Y := BoneVertex.Y;
      if (BoneVertex.Z > max.Z) then
        max.Z := BoneVertex.Z;

      if (BoneVertex.X < min.X) then
        min.X := BoneVertex.X;
      if (BoneVertex.Y < min.Y) then
        min.Y := BoneVertex.Y;
      if (BoneVertex.Z < min.Z) then
        min.Z := BoneVertex.Z;
    end;

    FBoundMax := max;
    FBoundMin := min;
    // Get the origin and subtract from the bone matrix
    FBoundBoneDelta := VectorScale(VectorAdd(FBoundMax, FBoundMin), 0.5);
  end
  else
  begin
    FBoundMax := NullVector;
    FBoundMin := NullVector;
  end;

  AlignToSkeleton;
  FReferenceMatrix := FBoneMatrix;
  mat := MatrixMultiply(bone.GlobalMatrix, FRagdoll.Owner.AbsoluteMatrix);
  // Set Joint position
  SetAnchor(AffineVectorMake(mat.V[3]));

  BoneVertices.Free; // NEW1
end;

constructor TGLRagdolBone.CreateOwned(aOwner: TGLRagdolBoneList);
begin
  Create(aOwner.Ragdoll);
  FOwner := aOwner;
  aOwner.Add(Self);
end;

destructor TGLRagdolBone.Destroy;
begin
  inherited;
end;

procedure TGLRagdolBone.AlignToSkeleton;
var
  o: TAffineVector;
  bone: TGLSkeletonBone;
  mat, posMat: TGLMatrix;
  noBounds: Boolean;
begin
  bone := Ragdoll.Owner.Skeleton.BoneByID(FBoneID);
  noBounds := VectorIsNull(FBoundMax) and VectorIsNull(FBoundMin);
  // Get the bone matrix relative to the Actor matrix
  mat := MatrixMultiply(bone.GlobalMatrix, FRagdoll.Owner.AbsoluteMatrix);
  // Set Rotation
  FBoneMatrix := mat;
  NormalizeMatrix(FBoneMatrix);

  if (noBounds) then
  begin
    FOrigin := AffineVectorMake(mat.V[3]);
    FSize := AffineVectorMake(0.1, 0.1, 0.1);
  end
  else
  begin
    // Set Origin
    posMat := mat;
    posMat.V[3] := NullHmgVector;
    o := VectorTransform(FBoundBoneDelta, posMat);
    FOrigin := VectorAdd(AffineVectorMake(mat.V[3]), o);
    // Set Size
    FSize := VectorScale(VectorSubtract(FBoundMax, FBoundMin), 0.9);
    FSize.X := FSize.X * VectorLength(mat.V[0]);
    FSize.Y := FSize.Y * VectorLength(mat.V[1]);
    FSize.Z := FSize.Z * VectorLength(mat.V[2]);
  end;
  // Put the origin in the BoneMatrix
  FBoneMatrix.V[3] := VectorMake(FOrigin, 1);
end;

function TGLRagdolBone.GetRagdollBone(Index: Integer): TGLRagdolBone;
begin
  Result := TGLRagdolBone(List^[Index]);
end;

procedure TGLRagdolBone.ReadFromFiler(reader: TVirtualReader);
begin
  inherited;

end;

procedure TGLRagdolBone.StartChild;
var
  i: Integer;
begin
  FOriginalMatrix := Ragdoll.Owner.Skeleton.BoneByID(FBoneID).GlobalMatrix;
  AlignToSkeleton;
  Start;
  for i := 0 to Count - 1 do
    Items[i].StartChild;
end;

procedure TGLRagdolBone.UpdateChild;
var
  i: Integer;
begin
  Update;
  for i := 0 to Count - 1 do
    Items[i].UpdateChild;
end;

procedure TGLRagdolBone.WriteToFiler(writer: TVirtualWriter);
begin
  inherited;
end;

procedure TGLRagdolBone.StopChild;
var
  i: Integer;
begin
  Stop;
  Ragdoll.Owner.Skeleton.BoneByID(FBoneID).SetGlobalMatrix(FOriginalMatrix);
  for i := 0 to Count - 1 do
    Items[i].StopChild;
end;

procedure TGLRagdolBone.CreateBoundsChild;
var
  i: Integer;
begin
  CreateBoundingBox;
  for i := 0 to Count - 1 do
    Items[i].CreateBoundsChild;
end;

procedure TGLRagdolBone.SetAnchor(const Anchor: TAffineVector);
begin
  FAnchor := Anchor;
end;

procedure TGLRagdolBone.AlignChild;
var
  i: Integer;
begin
  Align;
  Update;
  for i := 0 to Count - 1 do
    Items[i].AlignChild;
end;

{ TGLRagdoll }

constructor TGLRagdoll.Create(aOwner: TGLBaseMesh);
begin
  FOwner := aOwner;
  FEnabled := False;
  FBuilt := False;
end;

destructor TGLRagdoll.Destroy;
begin
  if FEnabled then
    Stop;
  inherited Destroy;
end;

procedure TGLRagdoll.ReadFromFiler(reader: TVirtualReader);
begin
  inherited;
end;

procedure TGLRagdoll.SetRootBone(RootBone: TGLRagdolBone);
begin
  FRootBone := RootBone;
end;

procedure TGLRagdoll.Start;
begin
  Assert(FBuilt, 'First you need to build the ragdoll. BuildRagdoll;');
  if (FEnabled) then
    Exit;
  FEnabled := True;
  // First start the ragdoll in the reference position
  RootBone.StartChild;
  // Now align it to the animation
  RootBone.AlignChild;
  // Now it recalculate the vertices to use as reference
  FOwner.Skeleton.StartRagDoll;
end;

procedure TGLRagdoll.Update;
begin
  if FEnabled then
  begin
    RootBone.UpdateChild;
    FOwner.Skeleton.MorphMesh(True);
  end;
end;

procedure TGLRagdoll.Stop;
begin
  if not FEnabled then
    Exit;
  FEnabled := False;
  RootBone.StopChild;
  // Restore the old information
  FOwner.Skeleton.StopRagDoll;
  FOwner.Skeleton.MorphMesh(True);
end;

procedure TGLRagdoll.WriteToFiler(writer: TVirtualWriter);
begin
  inherited;

end;

procedure TGLRagdoll.BuildRagdoll;
begin
  Assert(RootBone <> nil,
    'First you need to set the root bone. SetRootBone();');
  RootBone.CreateBoundsChild;
  FBuilt := True;
end;

end.
