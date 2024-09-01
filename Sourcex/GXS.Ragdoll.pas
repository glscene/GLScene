//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Ragdoll;

(* Base abstract ragdoll class. Should be extended to use any physics system *)

interface

uses
  GXS.VectorTypes,
  GXS.PersistentClasses,
  GXS.VectorGeometry,
  GXS.VectorLists,
  GXS.Scene,
  GXS.Objects,
  GXS.VectorFileObjects;

type
  TgxRagdoll = class;
  TgxRagdolBone = class;

  TgxRagdolJoint = class
  end;

  TgxRagdolBoneList = class (TgxPersistentObjectList)
  private
     FRagdoll : TgxRagdoll;
  protected
    function GetRagdollBone(Index: Integer) : TgxRagdolBone;
  public
    constructor Create(Ragdoll: TgxRagdoll); reintroduce;
    destructor Destroy; override;
    procedure WriteToFiler(writer : TgxVirtualWriter); override;
    procedure ReadFromFiler(reader : TgxVirtualReader); override;
    property Ragdoll : TgxRagdoll read FRagdoll;
    property Items[Index: Integer] : TgxRagdolBone read GetRagdollBone; default;
	end;

	TgxRagdolBone = class (TgxRagdolBoneList)
  private
    FOwner : TgxRagdolBoneList;
    FName : String;
    FBoneID : Integer; //Refering to TgxActor Bone
    FBoundMax: TAffineVector;
    FBoundMin: TAffineVector;
    FBoundBoneDelta: TAffineVector; //Stores the diference from the bone.GlobalMatrix to the center of the bone's bounding box
    FOrigin: TAffineVector;
    FSize: TAffineVector;
    FBoneMatrix: TMatrix4f;
    FJoint: TgxRagdolJoint;
    FOriginalMatrix: TMatrix4f; //Stores the Bone.GlobalMatrix before the ragdoll start
    FReferenceMatrix: TMatrix4f; //Stores the first bone matrix to be used as reference
    FAnchor: TAffineVector; //The position of the joint
    procedure CreateBoundingBox;
    procedure SetAnchor(Anchor: TAffineVector);
    procedure AlignToSkeleton;
    procedure CreateBoundsChild;
    procedure StartChild;
    procedure AlignChild;
    procedure UpdateChild;
    procedure StopChild;
  protected
    function GetRagdollBone(Index: Integer) : TgxRagdolBone;
    procedure Start; virtual; abstract;
    procedure Align; virtual; abstract;
    procedure Update; virtual; abstract;
    procedure Stop; virtual; abstract;
  public
    constructor CreateOwned(aOwner : TgxRagdolBoneList);
    constructor Create(Ragdoll: TgxRagdoll);
    destructor Destroy; override;
    procedure WriteToFiler(writer : TgxVirtualWriter); override;
    procedure ReadFromFiler(reader : TgxVirtualReader); override;
    property Owner : TgxRagdolBoneList read FOwner;
    property Name : String read FName write FName;
    property BoneID : Integer read FBoneID write FBoneID;
    property Origin : TAffineVector read FOrigin;
    property Size : TAffineVector read FSize;
    property BoneMatrix : TMatrix4f read FBoneMatrix;
    property ReferenceMatrix : TMatrix4f read FReferenceMatrix;
    property Anchor : TAffineVector read FAnchor;
    property Joint : TgxRagdolJoint read FJoint write FJoint;
    property Items[Index: Integer] : TgxRagdolBone read GetRagdollBone; default;
	end;

  TgxRagdoll = class(TgxPersistentObject)
	private
    FOwner : TgxBaseMesh;
    FRootBone : TgxRagdolBone;
    FEnabled: Boolean;
    FBuilt: Boolean;
  public
    constructor Create(AOwner : TgxBaseMesh); reintroduce;
    destructor Destroy; override;
    procedure WriteToFiler(writer : TgxVirtualWriter); override;
    procedure ReadFromFiler(reader : TgxVirtualReader); override;
    // Must be set before build the ragdoll
    procedure SetRootBone(RootBone: TgxRagdolBone);
    // Create the bounding box and setup the ragdoll do be started later
    procedure BuildRagdoll;
    procedure Start;
    procedure Update;
    procedure Stop;
    property Owner : TgxBaseMesh read FOwner;
    property RootBone : TgxRagdolBone read FRootBone;
    property Enabled : Boolean read FEnabled;
	end;

//-----------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------

//--------------------------------
// TgxRagdolBoneList
//--------------------------------

constructor TgxRagdolBoneList.Create(Ragdoll: TgxRagdoll);
begin
  inherited Create;
  FRagdoll := Ragdoll;
end;

destructor TgxRagdolBoneList.Destroy;
var i: integer;
begin
  for i:=0 to Count-1 do Items[i].Destroy;
  inherited;
end;

function TgxRagdolBoneList.GetRagdollBone(Index: Integer): TgxRagdolBone;
begin
  Result:=TgxRagdolBone(List^[Index]);
end;

procedure TgxRagdolBoneList.ReadFromFiler(reader: TgxVirtualReader);
begin
  inherited;
  //Not implemented
end;

procedure TgxRagdolBoneList.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited;
  //Not implemented
end;

{ TgxRagdolBone }

constructor TgxRagdolBone.Create(Ragdoll: TgxRagdoll);
begin
  inherited Create(Ragdoll);
end;

procedure TgxRagdolBone.CreateBoundingBox;
var
  bone: TgxSkeletonBone;
  i, j: integer;
  BoneVertices : TgxAffineVectorList;
  BoneVertex, max,min: TAffineVector;
  invMat, mat: TMatrix4f;
begin
  bone := Ragdoll.Owner.Skeleton.BoneByID(FBoneID);

  //Get all vertices weighted to this bone
  BoneVertices:=TgxAffineVectorList.Create;
  for i:=0 to Ragdoll.Owner.MeshObjects.Count-1 do
  with TgxSkeletonMeshObject(Ragdoll.Owner.MeshObjects[i]) do
    for j:=0 to Vertices.Count-1 do
      if bone.BoneID = VerticesBonesWeights[j][0].BoneID then
        BoneVertices.FindOrAdd(Vertices[j]);

  invMat := bone.GlobalMatrix;
  InvertMatrix(invMat);

  //For each vertex, get the max and min XYZ (Bounding box)
  if BoneVertices.Count > 0 then
  begin
    BoneVertex := VectorTransform(BoneVertices[0], invMat);
    max := BoneVertex;
    min := BoneVertex;
    for i:=1 to BoneVertices.Count-1 do begin
      BoneVertex := VectorTransform(BoneVertices[i], invMat);
      if (BoneVertex.X > max.X) then max.X := BoneVertex.X;
      if (BoneVertex.Y > max.Y) then max.Y := BoneVertex.Y;
      if (BoneVertex.Z > max.Z) then max.Z := BoneVertex.Z;

      if (BoneVertex.X < min.X) then min.X := BoneVertex.X;
      if (BoneVertex.Y < min.Y) then min.Y := BoneVertex.Y;
      if (BoneVertex.Z < min.Z) then min.Z := BoneVertex.Z;
    end;

    FBoundMax := max;
    FBoundMin := min;
    //Get the origin and subtract from the bone matrix
    FBoundBoneDelta := VectorScale(VectorAdd(FBoundMax, FBoundMin), 0.5);
  end else begin
    FBoundMax := NullVector;
    FBoundMin := NullVector;
  end;

  AlignToSkeleton;
  FReferenceMatrix := FBoneMatrix;
  mat := MatrixMultiply(bone.GlobalMatrix,FRagdoll.Owner.AbsoluteMatrix);
  //Set Joint position
  SetAnchor(AffineVectorMake(mat.W));

  BoneVertices.Free; // NEW1
end;

constructor TgxRagdolBone.CreateOwned(aOwner: TgxRagdolBoneList);
begin
	Create(aOwner.Ragdoll);
  FOwner:=aOwner;
  aOwner.Add(Self);
end;

destructor TgxRagdolBone.Destroy;
begin
  inherited;
end;

procedure TgxRagdolBone.AlignToSkeleton;
var
  o: TAffineVector;
  bone: TgxSkeletonBone;
  mat, posMat: TMatrix4f;
  noBounds: Boolean;
begin
  bone := Ragdoll.Owner.Skeleton.BoneByID(FBoneID);
  noBounds := VectorIsNull(FBoundMax) and VectorIsNull(FBoundMin);
  //Get the bone matrix relative to the Actor matrix
  mat := MatrixMultiply(bone.GlobalMatrix,FRagdoll.Owner.AbsoluteMatrix);
  //Set Rotation
  FBoneMatrix := mat;
  NormalizeMatrix(FBoneMatrix);

  if (noBounds) then
  begin
    FOrigin := AffineVectorMake(mat.W);
    FSize := AffineVectorMake(0.1,0.1,0.1);
  end else begin
    //Set Origin
    posMat := mat;
    posMat.W := NullHmgVector;
    o := VectorTransform(FBoundBoneDelta, posMat);
    FOrigin := VectorAdd(AffineVectorMake(mat.W), o);
    //Set Size
    FSize := VectorScale(VectorSubtract(FBoundMax, FBoundMin),0.9);
    FSize.X := FSize.X*VectorLength(mat.X);
    FSize.Y := FSize.Y*VectorLength(mat.Y);
    FSize.Z := FSize.Z*VectorLength(mat.Z);
  end;
  //Put the origin in the BoneMatrix
  FBoneMatrix.W := VectorMake(FOrigin,1);
end;

function TgxRagdolBone.GetRagdollBone(Index: Integer): TgxRagdolBone;
begin
  Result:=TgxRagdolBone(List^[Index]);
end;

procedure TgxRagdolBone.ReadFromFiler(reader: TgxVirtualReader);
begin
  inherited;

end;

procedure TgxRagdolBone.StartChild;
var i: integer;
begin
  FOriginalMatrix := Ragdoll.Owner.Skeleton.BoneByID(FBoneID).GlobalMatrix;
  AlignToSkeleton;
  Start;
  for i := 0 to Count-1 do items[i].StartChild;
end;

procedure TgxRagdolBone.UpdateChild;
var i: integer;
begin
  Update;
  for i := 0 to Count-1 do items[i].UpdateChild;
end;

procedure TgxRagdolBone.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited;

end;

procedure TgxRagdolBone.StopChild;
var i: integer;
begin
  Stop;
  Ragdoll.Owner.Skeleton.BoneByID(FBoneID).SetGlobalMatrix(FOriginalMatrix);
  for i := 0 to Count-1 do items[i].StopChild;
end;

procedure TgxRagdolBone.CreateBoundsChild;
var i: integer;
begin
  CreateBoundingBox;
  for i := 0 to Count-1 do items[i].CreateBoundsChild;
end;

procedure TgxRagdolBone.SetAnchor(Anchor: TAffineVector);
begin
  FAnchor := Anchor;
end;

procedure TgxRagdolBone.AlignChild;
var i: integer;
begin
  Align;
  Update;
  for i := 0 to Count-1 do items[i].AlignChild;
end;

{ TgxRagdoll }

constructor TgxRagdoll.Create(AOwner : TgxBaseMesh);
begin
  FOwner := AOwner;
  FEnabled := False;
  FBuilt := False;
end;

destructor TgxRagdoll.Destroy;
begin
  if FEnabled then Stop;
  inherited Destroy;
end;

procedure TgxRagdoll.ReadFromFiler(reader: TgxVirtualReader);
begin
  inherited;
end;

procedure TgxRagdoll.SetRootBone(RootBone: TgxRagdolBone);
begin
  FRootBone := RootBone;
end;

procedure TgxRagdoll.Start;
begin
  Assert(FBuilt, 'First you need to build the ragdoll. BuildRagdoll;');
  if (FEnabled) then Exit;
  FEnabled:= True;
  //First start the ragdoll in the reference position
  RootBone.StartChild;
  //Now align it to the animation
  RootBone.AlignChild;
  //Now it recalculate the vertices to use as reference
  FOwner.Skeleton.StartRagDoll;
end;

procedure TgxRagdoll.Update;
begin
  if FEnabled then
  begin
    RootBone.UpdateChild;
    FOwner.Skeleton.MorphMesh(true);
  end;
end;

procedure TgxRagdoll.Stop;
begin
  if not FEnabled then Exit;
  FEnabled := False;
  RootBone.StopChild;
  //Restore the old information
  FOwner.Skeleton.StopRagDoll;
  FOwner.Skeleton.MorphMesh(true);
end;

procedure TgxRagdoll.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited;

end;

procedure TgxRagdoll.BuildRagdoll;
begin
  Assert(RootBone <> nil, 'First you need to set the root bone. SetRootBone();');
  RootBone.CreateBoundsChild;
  FBuilt := True;
end;

end.
