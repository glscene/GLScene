//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.Tree;

(*
  Dynamic tree generation in GLScene
  This code was adapted from the nVidia Tree Demo:
  http://developer.nvidia.com/object/Procedural_Tree.html

  Some info:
  CenterBranchConstant -
  Defines, how big the central branch is. When around 50%
  it makes a small branch inside the tree, for higher values
  much more branches and leaves are created, so either use it
  with low depth, or set it to zero, and have two-branched tree.
  Default value: 0.5
  "AutoRebuild" flag - Rebuild tree after property change.
  Default: True
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  System.Math,

  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.State,
  GLS.Material,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.VectorFileObjects,
  GLS.ApplicationFileIO,
  GLS.RenderContextInfo,
  GLS.PersistentClasses,
  GLS.XOpenGL,
  GLS.Context,
  GLS.VectorTypes,
  GLS.Utils;

type
  TGLTree = class;
  TGLTreeBranches = class;
  TGLTreeBranchNoise = class;

  TGLTreeLeaves = class
  private
    FOwner: TGLTree;
    FCount: Integer;
    FVertices: TGLAffineVectorList;
    FNormals: TGLAffineVectorList;
    FTexCoords: TGLAffineVectorList;
  public
   constructor Create(AOwner: TGLTree);
    destructor Destroy; override;
    procedure BuildList(var rci: TGLRenderContextInfo);
    procedure AddNew(matrix: TGLMatrix);
    procedure Clear;
    property Owner: TGLTree read FOwner;
    property Count: Integer read FCount;
    property Vertices: TGLAffineVectorList read FVertices;
    property Normals: TGLAffineVectorList read FNormals;
    property TexCoords: TGLAffineVectorList read FTexCoords;
  end;

  TGLTreeBranch = class
  private
    FOwner: TGLTreeBranches;
    FLeft: TGLTreeBranch;
    FCenter: TGLTreeBranch;
    FRight: TGLTreeBranch;
    FParent: TGLTreeBranch;
    FBranchID: Integer;
    FParentID: Integer;
    FMatrix: TGLMatrix;
    FLower: TGLIntegerList;
    FUpper: TGLIntegerList;
    FCentralLeader: Boolean;
    procedure BuildBranch(branchNoise: TGLTreeBranchNoise;
      const matrix: TGLMatrix; TexCoordY, Twist: Single; Level: Integer);
  public
    constructor Create(AOwner: TGLTreeBranches; AParent: TGLTreeBranch);
    destructor Destroy; override;
    property Owner: TGLTreeBranches read FOwner;
    property Left: TGLTreeBranch read FLeft;
    property Center: TGLTreeBranch read FCenter;
    property Right: TGLTreeBranch read FRight;
    property Parent: TGLTreeBranch read FParent;
    property matrix: TGLMatrix read FMatrix;
    property Lower: TGLIntegerList read FLower;
    property Upper: TGLIntegerList read FUpper;
  end;

  TGLTreeBranches = class
  private
    FOwner: TGLTree;
    FSinList: TGLSingleList;
    FCosList: TGLSingleList;
    FVertices: TGLAffineVectorList;
    FNormals: TGLAffineVectorList;
    FTexCoords: TGLAffineVectorList;
    FIndices: TGLIntegerList;
    FRoot: TGLTreeBranch;
    FCount: Integer;
    FBranchCache: TList;
    FBranchIndices: TGLIntegerList;
    procedure BuildBranches;
  public
    constructor Create(AOwner: TGLTree);
    destructor Destroy; override;
    procedure BuildList(var rci: TGLRenderContextInfo);
    procedure Clear;
    property Owner: TGLTree read FOwner;
    property SinList: TGLSingleList read FSinList;
    property CosList: TGLSingleList read FCosList;
    property Vertices: TGLAffineVectorList read FVertices;
    property Normals: TGLAffineVectorList read FNormals;
    property TexCoords: TGLAffineVectorList read FTexCoords;
    property Count: Integer read FCount;
  end;

  TGLTreeBranchNoise = class
  private
    FBranchNoise: Single;
    FLeft, FRight, FCenter: TGLTreeBranchNoise;
    function GetLeft: TGLTreeBranchNoise;
    function GetCenter: TGLTreeBranchNoise;
    function GetRight: TGLTreeBranchNoise;
  public
    constructor Create;
    destructor Destroy; override;
    property Left: TGLTreeBranchNoise read GetLeft;
    property Center: TGLTreeBranchNoise read GetCenter;
    property Right: TGLTreeBranchNoise read GetRight;
    property branchNoise: Single read FBranchNoise;
  end;

  TGLTree = class(TGLImmaterialSceneObject)
  private
    FDepth: Integer;
    FBranchFacets: Integer;
    FLeafSize: Single;
    FBranchSize: Single;
    FBranchNoise: Single;
    FBranchAngleBias: Single;
    FBranchAngle: Single;
    FBranchTwist: Single;
    FBranchRadius: Single;
    FLeafThreshold: Single;
    FCentralLeaderBias: Single;
    FCentralLeader: Boolean;
    FSeed: Integer;
    FAutoCenter: Boolean;
    FAutoRebuild: Boolean;
    FCenterBranchConstant: Single;
    FLeaves: TGLTreeLeaves;
    FBranches: TGLTreeBranches;
    FNoise: TGLTreeBranchNoise;
    FMaterialLibrary: TGLMaterialLibrary;
    FLeafMaterialName: TGLLibMaterialName;
    FLeafBackMaterialName: TGLLibMaterialName;
    FBranchMaterialName: TGLLibMaterialName;
    FRebuildTree: Boolean;
    FAxisAlignedDimensionsCache: TGLVector;
  protected
   procedure SetDepth(const Value: Integer);
    procedure SetBranchFacets(const Value: Integer);
    procedure SetLeafSize(const Value: Single);
    procedure SetBranchSize(const Value: Single);
    procedure SetBranchNoise(const Value: Single);
    procedure SetBranchAngleBias(const Value: Single);
    procedure SetBranchAngle(const Value: Single);
    procedure SetBranchTwist(const Value: Single);
    procedure SetBranchRadius(const Value: Single);
    procedure SetLeafThreshold(const Value: Single);
    procedure SetCentralLeaderBias(const Value: Single);
    procedure SetCentralLeader(const Value: Boolean);
    procedure SetSeed(const Value: Integer);
    procedure SetAutoCenter(const Value: Boolean);
    procedure SetAutoRebuild(const Value: Boolean);
    procedure SetCenterBranchConstant(const Value: Single);
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    procedure SetLeafMaterialName(const Value: TGLLibMaterialName);
    procedure SetLeafBackMaterialName(const Value: TGLLibMaterialName);
    procedure SetBranchMaterialName(const Value: TGLLibMaterialName);
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure StructureChanged; override;
    procedure BuildMesh(GLBaseMesh: TGLBaseMesh);
    procedure RebuildTree;
    procedure ForceTotalRebuild;
    procedure Clear;
    procedure GetExtents(var min, max: TAffineVector);
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromFile(const aFileName: String);
    procedure SaveToFile(const aFileName: String);
    property Leaves: TGLTreeLeaves read FLeaves;
    property Branches: TGLTreeBranches read FBranches;
    property Noise: TGLTreeBranchNoise read FNoise;
  published
    // The depth of tree branch recursion.
    property Depth: Integer read FDepth write SetDepth;
    // The number of facets for each branch in the tree.
    property BranchFacets: Integer read FBranchFacets write SetBranchFacets;
    // Leaf size modifier. Leaf size is also influenced by branch recursion scale.
    property LeafSize: Single read FLeafSize write SetLeafSize;
    // Branch length modifier.
    property BranchSize: Single read FBranchSize write SetBranchSize;
    // Overall branch noise influence. Relates to the 'fullness' of the tree.
    property BranchNoise: Single read FBranchNoise write SetBranchNoise;
    (* Effects the habit of the tree. Values from 0 to 1 refer to Upright to
      Spreading respectively. *)
    property BranchAngleBias: Single read FBranchAngleBias write SetBranchAngleBias;
    // Effects the balance of the tree.
    property BranchAngle: Single read FBranchAngle write SetBranchAngle;
    // Effects the rotation of each sub branch in recursion.
    property BranchTwist: Single read FBranchTwist write SetBranchTwist;
    // Effects the thickness of the branches.
    property BranchRadius: Single read FBranchRadius write SetBranchRadius;
    // Determines how thin a branch can get before a leaf is substituted.
    property LeafThreshold: Single read FLeafThreshold write SetLeafThreshold;
    // Determines how BranchAngle effects the central leader (CentralLeader must = True).
    property CentralLeaderBias: Single read FCentralLeaderBias
      write SetCentralLeaderBias;
    // Does this tree have a central leader?
    property CentralLeader: Boolean read FCentralLeader write SetCentralLeader;
    property Seed: Integer read FSeed write SetSeed;
    // Automatically center the tree's vertices after building them.
    property AutoCenter: Boolean read FAutoCenter write SetAutoCenter;
    // Automatically rebuild the tree after changing the settings
    property AutoRebuild: Boolean read FAutoRebuild write SetAutoRebuild;
    (* Central branch can be thinner(lower values)/thicker(->1) depending on this constant.
      The effect also depends on the BranchAngle variable. *)
    property CenterBranchConstant: Single read FCenterBranchConstant
      write SetCenterBranchConstant;
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary
      write SetMaterialLibrary;
    property LeafMaterialName: TGLLibMaterialName read FLeafMaterialName
      write SetLeafMaterialName;
    property LeafBackMaterialName: TGLLibMaterialName read FLeafBackMaterialName
      write SetLeafBackMaterialName;
    property BranchMaterialName: TGLLibMaterialName read FBranchMaterialName
      write SetBranchMaterialName;
  end;

// -----------------------------------------------------------------------------
implementation
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// TGLTreeLeaves
// -----------------------------------------------------------------------------

constructor TGLTreeLeaves.Create(AOwner: TGLTree);
begin
  FOwner := AOwner;
  FCount := 0;
  FVertices := TGLAffineVectorList.Create;
  FNormals := TGLAffineVectorList.Create;
  FTexCoords := TGLAffineVectorList.Create;
end;

destructor TGLTreeLeaves.Destroy;
begin
  FVertices.Free;
  FNormals.Free;
  FTexCoords.Free;
  inherited;
end;

procedure TGLTreeLeaves.AddNew(matrix: TGLMatrix);
var
  radius: Single;
  pos: TGLVector;
begin
  radius := Owner.LeafSize;
  Inc(FCount);

  pos := matrix.W;
  matrix.W := NullHMGPoint;
  matrix := Roll(matrix, FCount / 10);
  NormalizeMatrix(matrix);
  matrix.W := pos;

  FVertices.Add(VectorTransform(PointMake(0, -radius, 0), matrix));
  FVertices.Add(VectorTransform(PointMake(0, radius, 0), matrix));
  FVertices.Add(VectorTransform(PointMake(0, radius, 2 * radius), matrix));
  FVertices.Add(VectorTransform(PointMake(0, -radius, 2 * radius), matrix));
  FNormals.Add(VectorTransform(XHmgVector, matrix));
  FTexCoords.Add(XVector, NullVector);
  FTexCoords.Add(YVector, XYVector);
end;

procedure TGLTreeLeaves.BuildList(var rci: TGLRenderContextInfo);
var
  i: Integer;
  n: TAffineVector;
  libMat: TGLLibMaterial;
begin
  libMat := Owner.MaterialLibrary.LibMaterialByName(Owner.LeafMaterialName);
  if Assigned(libMat) then
    libMat.Apply(rci);

  gl.EnableClientState(GL_VERTEX_ARRAY);
  xgl.EnableClientState(GL_TEXTURE_COORD_ARRAY);

  gl.VertexPointer(3, GL_FLOAT, 0, @FVertices.List[0]);
  xgl.TexCoordPointer(3, GL_FLOAT, 0, @FTexCoords.List[0]);

  for i := 0 to (FVertices.Count div 4) - 1 do
  begin
    gl.Normal3fv(@FNormals.List[i]);
    gl.DrawArrays(GL_QUADS, 4 * i, 4);
  end;

  with Owner do
    if LeafMaterialName <> LeafBackMaterialName then
    begin
      if Assigned(libMat) then
        libMat.UnApply(rci);
      libMat := MaterialLibrary.LibMaterialByName(LeafBackMaterialName);
      if Assigned(libMat) then
        libMat.Apply(rci);
    end;

  rci.GLStates.InvertGLFrontFace;
  for i := 0 to (FVertices.Count div 4) - 1 do
  begin
    n := VectorNegate(FNormals[i]);
    gl.Normal3fv(@n);
    gl.DrawArrays(GL_QUADS, 4 * i, 4);
  end;
  rci.GLStates.InvertGLFrontFace;

  gl.DisableClientState(GL_VERTEX_ARRAY);
  xgl.DisableClientState(GL_TEXTURE_COORD_ARRAY);

  if Assigned(libMat) then
    libMat.UnApply(rci);
end;

procedure TGLTreeLeaves.Clear;
begin
  FVertices.Clear;
  FNormals.Clear;
  FTexCoords.Clear;
  FCount := 0;
end;

// -----------------------------------------------------------------------------
// TGLTreeBranch
// -----------------------------------------------------------------------------

constructor TGLTreeBranch.Create(AOwner: TGLTreeBranches;
  AParent: TGLTreeBranch);
begin
  FOwner := AOwner;
  FParent := AParent;
  FUpper := TGLIntegerList.Create;
  FLower := TGLIntegerList.Create;
  FCentralLeader := False;

  // Skeletal construction helpers
  if Assigned(FOwner) then
  begin
    FBranchID := FOwner.Count - 1;
    FOwner.FBranchCache.Add(Self);
  end
  else
    FBranchID := -1;
  if Assigned(FParent) then
    FParentID := FParent.FBranchID
  else
    FParentID := -1;
end;

destructor TGLTreeBranch.Destroy;
begin
  FUpper.Free;
  FLower.Free;
  FLeft.Free;
  FRight.Free;
  inherited;
end;

procedure TGLTreeBranch.BuildBranch(branchNoise: TGLTreeBranchNoise;
  const matrix: TGLMatrix; TexCoordY, Twist: Single; Level: Integer);
var
  i: Integer;
  Tree: TGLTree;
  Branches: TGLTreeBranches;
  Facets: Integer;
  t, c, s: Single;
  radius, LeftRadius, RightRadius, CenterRadius: Single;
  BranchAngle, LeftAngle, RightAngle, CenterAngle: Single;
  BranchAngleBias, BranchTwist, Taper: Single;
  LeftBranchNoiseValue, RightBranchNoiseValue, CenterBranchNoiseValue: Single;
  LeftBranchNoise: TGLTreeBranchNoise;
  CenterBranchNoise: TGLTreeBranchNoise;
  RightBranchNoise: TGLTreeBranchNoise;
  LeftMatrix, RightMatrix, CenterMatrix: TGLMatrix;
  central_leader: Boolean;
begin
  Assert(Assigned(FOwner), 'Incorrect use of TGLTreeBranch');
  Assert(Assigned(FOwner.FOwner), 'Incorrect use of TGLTreeBranches');

  FMatrix := matrix;

  Branches := FOwner;
  Tree := FOwner.FOwner;

  Facets := Tree.BranchFacets;
  radius := Tree.BranchRadius;

  FLower.Clear;
  FLower.Capacity := Facets + 1;
  FUpper.Clear;
  FUpper.Capacity := Facets + 1;

  BranchAngle := Tree.BranchAngle;
  BranchAngleBias := Tree.BranchAngleBias;
  BranchTwist := Twist + Tree.BranchTwist;

  LeftBranchNoise := branchNoise.Left;
  CenterBranchNoise := branchNoise.Center;
  RightBranchNoise := branchNoise.Right;

  LeftBranchNoiseValue := ((LeftBranchNoise.branchNoise * 0.4) - 0.1) *
    Tree.branchNoise;
  LeftRadius := Sqrt(1 - BranchAngle + LeftBranchNoiseValue);
  LeftRadius := ClampValue(LeftRadius, 0, 1);
  LeftAngle := BranchAngle * 90 * BranchAngleBias + 10 * LeftBranchNoiseValue;

  CenterBranchNoiseValue := ((CenterBranchNoise.branchNoise * 0.9) - 0.1) *
    Tree.branchNoise;
  CenterRadius := Sqrt(Tree.CenterBranchConstant - BranchAngle +
    CenterBranchNoiseValue);
  CenterRadius := ClampValue(CenterRadius, 0, 1);
  CenterAngle := (1 - BranchAngle) * 50 * CenterBranchNoiseValue *
    BranchAngleBias;

  RightBranchNoiseValue := ((RightBranchNoise.branchNoise * 0.6) - 0.1) *
    Tree.branchNoise;
  RightRadius := Sqrt(BranchAngle + RightBranchNoiseValue);
  RightRadius := ClampValue(RightRadius, 0, 1);
  RightAngle := (1 - BranchAngle) * -90 * BranchAngleBias + 10 *
    RightBranchNoiseValue;

  Taper := MaxFloat(LeftRadius, RightRadius, CenterRadius);

  // Build cylinder lower
  for i := 0 to Facets do
  begin
    t := 1 / Facets * i;
    c := Branches.CosList[i];
    s := Branches.SinList[i];
    Branches.Vertices.Add(VectorTransform(PointMake(c * radius, s * radius,
      radius), matrix));
    Branches.Normals.Add(VectorTransform(VectorMake(c, s, 0), matrix));
    Branches.TexCoords.Add(t, TexCoordY);
    FLower.Add(Branches.Vertices.Count - 1);
    Branches.FBranchIndices.Add(FBranchID);
  end;

  TexCoordY := TexCoordY + 1 - 2 * radius;

  // Build cylinder upper
  for i := 0 to Facets do
  begin
    t := 1 / Facets * i;
    c := Branches.CosList[i];
    s := Branches.SinList[i];
    Branches.Vertices.Add(VectorTransform(PointMake(c * radius * Taper,
      s * radius * Taper, 1 - radius), matrix));
    Branches.Normals.Add(VectorTransform(VectorMake(c, s, 0), matrix));
    Branches.TexCoords.Add(t, TexCoordY);
    FUpper.Add(Branches.Vertices.Count - 1);
    Branches.FBranchIndices.Add(FBranchID);
  end;

  TexCoordY := TexCoordY + 2 * radius;

  // BuildMatrices
  SinCos(DegToRad(BranchTwist), s, c);

  if Level = 0 then
    central_leader := FCentralLeader
  else
    central_leader := FParent.FCentralLeader;

  if central_leader then
  begin
    LeftMatrix := MatrixMultiply(CreateScaleMatrix(AffineVectorMake(LeftRadius,
      LeftRadius, LeftRadius)), CreateRotationMatrix(AffineVectorMake(s, c, 0),
      DegToRad(LeftAngle) * Tree.CentralLeaderBias));
  end
  else
  begin
    LeftMatrix := MatrixMultiply(CreateScaleMatrix(AffineVectorMake(LeftRadius,
      LeftRadius, LeftRadius)), CreateRotationMatrix(AffineVectorMake(s, c, 0),
      DegToRad(LeftAngle)));
  end;
  LeftMatrix := MatrixMultiply(LeftMatrix,
    MatrixMultiply(CreateTranslationMatrix(AffineVectorMake(0, 0,
    Tree.BranchSize * (1 - LeftBranchNoiseValue))), matrix));

  CenterMatrix := MatrixMultiply
    (CreateScaleMatrix(AffineVectorMake(CenterRadius, CenterRadius,
    CenterRadius)), CreateRotationMatrix(AffineVectorMake(s, c, 0),
    DegToRad(CenterAngle)));
  CenterMatrix := MatrixMultiply(CenterMatrix,
    MatrixMultiply(CreateTranslationMatrix(AffineVectorMake(0, 0,
    Tree.BranchSize * (1 - CenterBranchNoiseValue))), matrix));

  RightMatrix := MatrixMultiply(CreateScaleMatrix(AffineVectorMake(RightRadius,
    RightRadius, RightRadius)), CreateRotationMatrix(AffineVectorMake(s, c, 0),
    DegToRad(RightAngle)));
  RightMatrix := MatrixMultiply(RightMatrix,
    MatrixMultiply(CreateTranslationMatrix(AffineVectorMake(0, 0,
    Tree.BranchSize * (1 - RightBranchNoiseValue))), matrix));

  if (((Level + 1) >= Tree.Depth) or (LeftRadius < Tree.LeafThreshold)) then
  begin
    Tree.Leaves.AddNew(LeftMatrix);
  end
  else
  begin
    Inc(Branches.FCount);
    FLeft := TGLTreeBranch.Create(Owner, Self);
    FLeft.FCentralLeader := central_leader and (LeftRadius >= RightRadius);
    FLeft.BuildBranch(LeftBranchNoise, LeftMatrix, TexCoordY, BranchTwist,
      Level + 1);
  end;

  if (((Level + 1) >= Tree.Depth) or (CenterRadius < Tree.LeafThreshold)) then
  begin
    Tree.Leaves.AddNew(CenterMatrix);
  end
  else
  begin
    Inc(Branches.FCount);
    FCenter := TGLTreeBranch.Create(Owner, Self);
    FCenter.BuildBranch(CenterBranchNoise, CenterMatrix, TexCoordY, BranchTwist,
      Level + 1);
  end;

  if (((Level + 1) >= Tree.Depth) or (RightRadius < Tree.LeafThreshold)) then
  begin
    Tree.Leaves.AddNew(RightMatrix);
  end
  else
  begin
    Inc(Branches.FCount);
    FRight := TGLTreeBranch.Create(Owner, Self);
    FRight.BuildBranch(RightBranchNoise, RightMatrix, TexCoordY, BranchTwist,
      Level + 1);
  end;

  for i := 0 to Facets do
  begin
    Branches.FIndices.Add(Upper[i]);
    Branches.FIndices.Add(Lower[i]);
  end;

  if Assigned(FRight) then
  begin
    for i := 0 to Facets do
    begin
      Branches.FIndices.Add(Right.Lower[i]);
      Branches.FIndices.Add(Upper[i]);
    end;
  end;

  if Assigned(FCenter) then
  begin
    for i := 0 to Facets do
    begin
      Branches.FIndices.Add(Center.Lower[i]);
      Branches.FIndices.Add(Upper[i]);
    end;
  end;

  if Assigned(FLeft) then
  begin
    for i := 0 to Facets do
    begin
      Branches.FIndices.Add(Left.Lower[i]);
      Branches.FIndices.Add(Upper[i]);
    end;
  end;

end;

// -----------------------------------------------------------------------------
// TGLTreeBranches
// -----------------------------------------------------------------------------

constructor TGLTreeBranches.Create(AOwner: TGLTree);
begin
  FOwner := AOwner;
  FSinList := TGLSingleList.Create;
  FCosList := TGLSingleList.Create;
  FVertices := TGLAffineVectorList.Create;
  FNormals := TGLAffineVectorList.Create;
  FTexCoords := TGLAffineVectorList.Create;
  FIndices := TGLIntegerList.Create;
  FBranchCache := TList.Create;
  FBranchIndices := TGLIntegerList.Create;
  FCount := 0;
end;

destructor TGLTreeBranches.Destroy;
begin
  FSinList.Free;
  FCosList.Free;
  FVertices.Free;
  FNormals.Free;
  FTexCoords.Free;
  FIndices.Free;
  FRoot.Free;
  FBranchCache.Free;
  FBranchIndices.Free;
  inherited;
end;

procedure TGLTreeBranches.BuildBranches;
var
  i: Integer;
  u: Single;
  delta, min, max: TAffineVector;
begin
  RandSeed := Owner.FSeed;

  for i := 0 to Owner.BranchFacets do
  begin
    u := 1 / Owner.BranchFacets * i;
    SinList.Add(Sin(PI * 2 * u));
    CosList.Add(Cos(PI * 2 * u));
  end;

  Inc(FCount);
  FRoot := TGLTreeBranch.Create(Self, nil);
  FRoot.FCentralLeader := Owner.CentralLeader;
  FRoot.BuildBranch(Owner.Noise, IdentityHMGMatrix, 0, 0, 0);

  delta := AffineVectorMake(0, 0, -Owner.BranchRadius);
  Vertices.Translate(delta);
  Owner.Leaves.Vertices.Translate(delta);

  if Owner.AutoCenter then
  begin
    Owner.GetExtents(min, max);
    delta := VectorCombine(min, max, -0.5, -0.5);
    Vertices.Translate(delta);
    Owner.Leaves.Vertices.Translate(delta);
  end;

  Owner.FAxisAlignedDimensionsCache.X := -1;
end;

procedure TGLTreeBranches.BuildList(var rci: TGLRenderContextInfo);
var
  i, stride: Integer;
  libMat: TGLLibMaterial;
begin
  stride := (Owner.BranchFacets + 1) * 2;

  libMat := Owner.MaterialLibrary.LibMaterialByName(Owner.BranchMaterialName);
  if Assigned(libMat) then
    libMat.Apply(rci);

  gl.VertexPointer(3, GL_FLOAT, 0, @FVertices.List[0]);
  gl.NormalPointer(GL_FLOAT, 0, @FNormals.List[0]);
  xgl.TexCoordPointer(3, GL_FLOAT, 0, @FTexCoords.List[0]);

  gl.EnableClientState(GL_VERTEX_ARRAY);
  gl.EnableClientState(GL_NORMAL_ARRAY);
  xgl.EnableClientState(GL_TEXTURE_COORD_ARRAY);

  repeat
    for i := 0 to (FIndices.Count div stride) - 1 do
      gl.DrawElements(GL_TRIANGLE_STRIP, stride, GL_UNSIGNED_INT,
        @FIndices.List[stride * i]);
  until (not Assigned(libMat)) or (not libMat.UnApply(rci));

  xgl.DisableClientState(GL_TEXTURE_COORD_ARRAY);
  gl.DisableClientState(GL_NORMAL_ARRAY);
  gl.DisableClientState(GL_VERTEX_ARRAY);
end;

// Clear
//
procedure TGLTreeBranches.Clear;
begin
  FSinList.Clear;
  FCosList.Clear;
  FVertices.Clear;
  FNormals.Clear;
  FTexCoords.Clear;
  FIndices.Clear;
  FBranchCache.Clear;
  FBranchIndices.Clear;
  FreeAndNil(FRoot);
  FCount := 0;
end;

// -----------------------------------------------------------------------------
// TGLTreeBranchNoise
// -----------------------------------------------------------------------------

constructor TGLTreeBranchNoise.Create;
begin
  FBranchNoise := Random;
end;

destructor TGLTreeBranchNoise.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited;
end;

function TGLTreeBranchNoise.GetLeft: TGLTreeBranchNoise;
begin
  if not Assigned(FLeft) then
    FLeft := TGLTreeBranchNoise.Create;
  Result := FLeft;
end;

function TGLTreeBranchNoise.GetRight: TGLTreeBranchNoise;
begin
  if not Assigned(FRight) then
    FRight := TGLTreeBranchNoise.Create;
  Result := FRight;
end;

function TGLTreeBranchNoise.GetCenter: TGLTreeBranchNoise;
begin
  if not Assigned(FCenter) then
    FCenter := TGLTreeBranchNoise.Create;
  Result := FCenter;
end;

// -----------------------------------------------------------------------------
// TGLTree
// -----------------------------------------------------------------------------

constructor TGLTree.Create(AOwner: TComponent);
begin
  inherited;
  // Default tree setting
  FDepth := 5;
  FLeafThreshold := 0.02;
  FBranchAngleBias := 0.6;
  FBranchAngle := 0.4;
  FBranchTwist := 45;
  FBranchNoise := 0.7;
  FBranchSize := 1.0;
  FLeafSize := 0.1;
  FBranchRadius := 0.12;
  FBranchFacets := 6;
  FCentralLeader := False;
  FSeed := 0;
  FAutoCenter := False;
  FAutoRebuild := True;
  FCenterBranchConstant := 0.5;

  FLeaves := TGLTreeLeaves.Create(Self);
  FBranches := TGLTreeBranches.Create(Self);
  FNoise := TGLTreeBranchNoise.Create;
end;

destructor TGLTree.Destroy;
begin
  FLeaves.Free;
  FBranches.Free;
  FNoise.Free;
  inherited;
end;

procedure TGLTree.Loaded;
begin
  inherited;
  FBranches.BuildBranches;
end;

procedure TGLTree.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FMaterialLibrary) then
    MaterialLibrary := nil;
  inherited;
end;

procedure TGLTree.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
begin
  MaterialLibrary.LibMaterialByName(BranchMaterialName).PrepareBuildList;
  MaterialLibrary.LibMaterialByName(LeafMaterialName).PrepareBuildList;
  MaterialLibrary.LibMaterialByName(LeafBackMaterialName).PrepareBuildList;
  inherited;
end;

procedure TGLTree.BuildList(var rci: TGLRenderContextInfo);
begin
  if FRebuildTree then
  begin
    FBranches.BuildBranches;
    FRebuildTree := False;
  end;
  Branches.BuildList(rci);
  Leaves.BuildList(rci);
end;

procedure TGLTree.StructureChanged;
begin
  FAxisAlignedDimensionsCache.X := -1;
  inherited;
end;

procedure TGLTree.BuildMesh(GLBaseMesh: TGLBaseMesh);

  procedure RecursBranches(Branch: TGLTreeBranch; bone: TGLSkeletonBone;
    Frame: TGLSkeletonFrame);
  var
    trans: TTransformations;
    mat: TGLMatrix;
    rot, pos: TAffineVector;
  begin
    bone.Name := 'Branch' + IntToStr(Branch.FBranchID);
    bone.BoneID := Branch.FBranchID;

    // Construct base frame
    if Assigned(Branch.FParent) then
      mat := Branch.FParent.FMatrix
    else
      mat := IdentityHMGMatrix;
    InvertMatrix(mat);
    NormalizeMatrix(mat);
    if MatrixDecompose(mat, trans) then
    begin
      SetVector(rot, trans[ttRotateX], trans[ttRotateY], trans[ttRotateZ]);
      SetVector(pos, mat.W);
    end
    else
    begin
      rot := NullVector;
      pos := NullVector;
    end;
    Frame.Rotation.Add(rot);
    Frame.Position.Add(pos);

    // Recurse with child branches
    if Assigned(Branch.Left) then
      RecursBranches(Branch.Left, TGLSkeletonBone.CreateOwned(bone), Frame);
    if Assigned(Branch.Right) then
      RecursBranches(Branch.Right, TGLSkeletonBone.CreateOwned(bone), Frame);
  end;

var
  // SkelMesh : TGLSkeletonMeshObject;
  fg: TFGVertexIndexList;
  fg2: TFGVertexNormalTexIndexList;
  i, j, stride: Integer;
  // parent_id : integer;
  // bone : TGLSkeletonBone;
begin
  if not Assigned(GLBaseMesh) then
    exit;

  if FRebuildTree then
  begin
    FBranches.BuildBranches;
    FRebuildTree := False;
  end;

  GLBaseMesh.MeshObjects.Clear;
  GLBaseMesh.Skeleton.Clear;

  // if GLBaseMesh is TGLActor then
  // TGLSkeletonMeshObject.CreateOwned(GLBaseMesh.MeshObjects)
  // else
  TGLMeshObject.CreateOwned(GLBaseMesh.MeshObjects);
  GLBaseMesh.MeshObjects[0].Mode := momFaceGroups;

  // Branches
  GLBaseMesh.MeshObjects[0].Vertices.Add(Branches.Vertices);
  GLBaseMesh.MeshObjects[0].Normals.Add(Branches.Normals);
  GLBaseMesh.MeshObjects[0].TexCoords.Add(Branches.TexCoords);
  { if GLBaseMesh is TGLActor then begin
    TGLActor(GLBaseMesh).Reference:=aarSkeleton;
    RecursBranches(Branches.FRoot,
    TGLSkeletonBone.CreateOwned(GLBaseMesh.Skeleton.RootBones),
    TGLSkeletonFrame.CreateOwned(GLBaseMesh.Skeleton.Frames));
    SkelMesh:=TGLSkeletonMeshObject(GLBaseMesh.MeshObjects[0]);
    SkelMesh.BonesPerVertex:=1;
    SkelMesh.VerticeBoneWeightCount:=Branches.FBranchIndices.Count;
    for i:=0 to Branches.FBranchIndices.Count-1 do
    SkelMesh.AddWeightedBone(Branches.FBranchIndices[i],1);
    GLBaseMesh.Skeleton.RootBones.PrepareGlobalMatrices;
    SkelMesh.PrepareBoneMatrixInvertedMeshes;

    SkelMesh.ApplyCurrentSkeletonFrame(True);
    end;// }
  stride := (BranchFacets + 1) * 2;
  for i := 0 to (FBranches.FIndices.Count div stride) - 1 do
  begin
    fg := TFGVertexIndexList.CreateOwned(GLBaseMesh.MeshObjects[0].FaceGroups);
    fg.MaterialName := BranchMaterialName;
    fg.Mode := fgmmTriangleStrip;
    for j := 0 to stride - 1 do
      fg.VertexIndices.Add(Branches.FIndices[i * stride + j]);
  end;

  // Leaves
  // if GLBaseMesh is TGLActor then
  // TGLSkeletonMeshObject.CreateOwned(GLBaseMesh.MeshObjects)
  // else
  TGLMeshObject.CreateOwned(GLBaseMesh.MeshObjects);
  GLBaseMesh.MeshObjects[1].Mode := momFaceGroups;

  GLBaseMesh.MeshObjects[1].Vertices.Add(Leaves.Vertices);
  GLBaseMesh.MeshObjects[1].Normals.Add(Leaves.FNormals);
  for i := 0 to Leaves.Normals.Count - 1 do
    GLBaseMesh.MeshObjects[1].Normals.Add(VectorNegate(Leaves.FNormals[i]));
  GLBaseMesh.MeshObjects[1].TexCoords.Add(Leaves.TexCoords);

  for i := 0 to (Leaves.FVertices.Count div 4) - 1 do
  begin

    // Leaf front
    fg2 := TFGVertexNormalTexIndexList.CreateOwned
      (GLBaseMesh.MeshObjects[1].FaceGroups);
    fg2.MaterialName := LeafMaterialName;
    fg2.Mode := fgmmTriangleStrip;
    with fg2.VertexIndices do
    begin
      Add(i * 4);
      Add(i * 4 + 1);
      Add(i * 4 + 3);
      Add(i * 4 + 2);
    end;
    for j := 0 to 3 do
      fg2.NormalIndices.Add(i);
    with fg2.TexCoordIndices do
    begin
      Add(0);
      Add(1);
      Add(3);
      Add(2);
    end;

    // Leaf back
    fg2 := TFGVertexNormalTexIndexList.CreateOwned
      (GLBaseMesh.MeshObjects[1].FaceGroups);
    fg2.MaterialName := LeafBackMaterialName;
    fg2.Mode := fgmmTriangleStrip;
    with fg2.VertexIndices do
    begin
      Add(i * 4);
      Add(i * 4 + 3);
      Add(i * 4 + 1);
      Add(i * 4 + 2);
    end;
    for j := 0 to 3 do
      fg2.NormalIndices.Add(i);
    with fg2.TexCoordIndices do
    begin
      Add(0);
      Add(3);
      Add(1);
      Add(2);
    end;
  end;
end;

procedure TGLTree.Clear;
begin
  FLeaves.Clear;
  FBranches.Clear;
end;

procedure TGLTree.SetBranchAngle(const Value: Single);
begin
  if Value <> FBranchAngle then
  begin
    FBranchAngle := Value;
    if (FAutoRebuild) then
      RebuildTree;
  end;
end;

procedure TGLTree.SetBranchAngleBias(const Value: Single);
begin
  if Value <> FBranchAngleBias then
  begin
    FBranchAngleBias := Value;
    if (FAutoRebuild) then
      RebuildTree;
  end;
end;

procedure TGLTree.SetBranchNoise(const Value: Single);
begin
  if Value <> FBranchNoise then
  begin
    FBranchNoise := Value;
    if (FAutoRebuild) then
      RebuildTree;
  end;
end;

procedure TGLTree.SetBranchRadius(const Value: Single);
begin
  if Value <> FBranchRadius then
  begin
    FBranchRadius := Value;
    if (FAutoRebuild) then
      RebuildTree;
  end;
end;

procedure TGLTree.SetBranchSize(const Value: Single);
begin
  if Value <> FBranchSize then
  begin
    FBranchSize := Value;
    if (FAutoRebuild) then
      RebuildTree;
  end;
end;

procedure TGLTree.SetBranchTwist(const Value: Single);
begin
  if Value <> FBranchTwist then
  begin
    FBranchTwist := Value;
    if (FAutoRebuild) then
      RebuildTree;
  end;
end;

procedure TGLTree.SetDepth(const Value: Integer);
begin
  if Value <> FDepth then
  begin
    FDepth := Value;
    if (FAutoRebuild) then
      RebuildTree;
  end;
end;

procedure TGLTree.SetBranchFacets(const Value: Integer);
begin
  if Value <> FBranchFacets then
  begin
    FBranchFacets := Value;
    if (FAutoRebuild) then
      RebuildTree;
  end;
end;

procedure TGLTree.SetLeafSize(const Value: Single);
begin
  if Value <> FLeafSize then
  begin
    FLeafSize := Value;
    if (FAutoRebuild) then
      RebuildTree;
  end;
end;

procedure TGLTree.SetLeafThreshold(const Value: Single);
begin
  if Value <> FLeafThreshold then
  begin
    FLeafThreshold := Value;
    if (FAutoRebuild) then
      RebuildTree;
  end;
end;

procedure TGLTree.SetCentralLeaderBias(const Value: Single);
begin
  if Value <> FCentralLeaderBias then
  begin
    FCentralLeaderBias := Value;
    if (FAutoRebuild) then
      RebuildTree;
  end;
end;

procedure TGLTree.SetCentralLeader(const Value: Boolean);
begin
  if Value <> FCentralLeader then
  begin
    FCentralLeader := Value;
    if (FAutoRebuild) then
      RebuildTree;
  end;
end;

procedure TGLTree.SetSeed(const Value: Integer);
begin
  if Value <> FSeed then
  begin
    FSeed := Value;
    if (FAutoRebuild) then
      ForceTotalRebuild;
  end;
end;

procedure TGLTree.SetCenterBranchConstant(const Value: Single);
begin
  if Value <> CenterBranchConstant then
  begin
    FCenterBranchConstant := Value;
    if (FAutoRebuild) then
      ForceTotalRebuild;
  end;
end;

procedure TGLTree.SetBranchMaterialName(const Value: TGLLibMaterialName);
begin
  if Value <> FBranchMaterialName then
  begin
    FBranchMaterialName := Value;
    StructureChanged;
  end;
end;

procedure TGLTree.SetLeafBackMaterialName(const Value: TGLLibMaterialName);
begin
  if Value <> FLeafBackMaterialName then
  begin
    FLeafBackMaterialName := Value;
    StructureChanged;
  end;
end;

procedure TGLTree.SetLeafMaterialName(const Value: TGLLibMaterialName);
begin
  if Value <> FLeafMaterialName then
  begin
    FLeafMaterialName := Value;
    StructureChanged;
  end;
end;

procedure TGLTree.SetMaterialLibrary(const Value: TGLMaterialLibrary);
begin
  if Value <> FMaterialLibrary then
  begin
    FMaterialLibrary := Value;
    StructureChanged;
  end;
end;

procedure TGLTree.RebuildTree;
begin
  if not FRebuildTree then
  begin
    Clear;
    FRebuildTree := True;
    StructureChanged;
  end;
end;

procedure TGLTree.ForceTotalRebuild;
begin
  Clear;
  FNoise.Free;
  RandSeed := FSeed;
  FNoise := TGLTreeBranchNoise.Create;
  FRebuildTree := False;
  FBranches.BuildBranches;
  StructureChanged;
end;

procedure TGLTree.LoadFromStream(aStream: TStream);
var
  StrList, StrParse: TStringList;
  str: String;
  i: Integer;
begin
  StrList := TStringList.Create;
  StrParse := TStringList.Create;

  StrList.LoadFromStream(aStream);
  try
    for i := 0 to StrList.Count - 1 do
    begin
      str := StrList[i];
      if pos('#', str) > 0 then
        str := Copy(str, 0, pos('#', str) - 1);
      StrParse.CommaText := str;
      if StrParse.Count >= 2 then
      begin
        str := LowerCase(StrParse[0]);
        if str = 'depth' then
          FDepth := StrToInt(StrParse[1])
        else if str = 'branch_facets' then
          FBranchFacets := StrToInt(StrParse[1])
        else if str = 'leaf_size' then
          FLeafSize := GLStrToFloatDef(StrParse[1])
        else if str = 'branch_size' then
          FBranchSize := GLStrToFloatDef(StrParse[1])
        else if str = 'branch_noise' then
          FBranchNoise := GLStrToFloatDef(StrParse[1])
        else if str = 'branch_angle_bias' then
          FBranchAngleBias := GLStrToFloatDef(StrParse[1])
        else if str = 'branch_angle' then
          FBranchAngle := GLStrToFloatDef(StrParse[1])
        else if str = 'branch_twist' then
          FBranchTwist := GLStrToFloatDef(StrParse[1])
        else if str = 'branch_radius' then
          FBranchRadius := GLStrToFloatDef(StrParse[1])
        else if str = 'leaf_threshold' then
          FLeafThreshold := GLStrToFloatDef(StrParse[1])
        else if str = 'central_leader_bias' then
          FCentralLeaderBias := GLStrToFloatDef(StrParse[1])
        else if str = 'central_leader' then
          FCentralLeader := LowerCase(StrParse[1]) = 'true'
        else if str = 'seed' then
          FSeed := StrToInt(StrParse[1])
        else if str = 'leaf_front_material_name' then
          FLeafMaterialName := StrParse[1]
        else if str = 'leaf_back_material_name' then
          FLeafBackMaterialName := StrParse[1]
        else if str = 'branch_material_name' then
          FBranchMaterialName := StrParse[1];
      end;
    end;
    ForceTotalRebuild;
  finally
    StrList.Free;
    StrParse.Free;
  end;
end;

procedure TGLTree.SaveToStream(aStream: TStream);
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  StrList.Add(Format('depth, %d', [Depth]));
  StrList.Add(Format('branch_facets, %d', [BranchFacets]));
  StrList.Add(Format('leaf_size, %f', [LeafSize]));
  StrList.Add(Format('branch_size, %f', [BranchSize]));
  StrList.Add(Format('branch_noise, %f', [branchNoise]));
  StrList.Add(Format('branch_angle_bias, %f', [BranchAngleBias]));
  StrList.Add(Format('branch_angle, %f', [BranchAngle]));
  StrList.Add(Format('branch_twist, %f', [BranchTwist]));
  StrList.Add(Format('branch_radius, %f', [BranchRadius]));
  StrList.Add(Format('leaf_threshold, %f', [LeafThreshold]));
  StrList.Add(Format('central_leader_bias, %f', [CentralLeaderBias]));
  if CentralLeader then
    StrList.Add('central_leader, true')
  else
    StrList.Add('central_leader, false');
  StrList.Add(Format('seed, %d', [Seed]));
  StrList.Add('leaf_front_material_name, "' + LeafMaterialName + '"');
  StrList.Add('leaf_back_material_name, "' + LeafBackMaterialName + '"');
  StrList.Add('branch_material_name, "' + BranchMaterialName + '"');
  StrList.SaveToStream(aStream);
  StrList.Free;
end;

procedure TGLTree.LoadFromFile(const aFileName: String);
var
  stream: TStream;
begin
  stream := TFileStream.Create(aFileName, fmOpenRead);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TGLTree.SaveToFile(const aFileName: String);
var
  stream: TStream;
begin
  stream := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(stream);
  finally
    stream.Free;
  end;
end;


procedure TGLTree.GetExtents(var min, max: TAffineVector);
var
  lmin, lmax, bmin, bmax: TAffineVector;
begin
  if Branches.Vertices.Count = 0 then
  begin
    FBranches.BuildBranches;
    FRebuildTree := False;
  end;
  if Leaves.Vertices.Count > 0 then
    Leaves.Vertices.GetExtents(lmin, lmax)
  else
  begin
    lmin := NullVector;
    lmax := NullVector;
  end;
  if Branches.Vertices.Count > 0 then
    Branches.Vertices.GetExtents(bmin, bmax)
  else
  begin
    bmin := NullVector;
    bmax := NullVector;
  end;

  min.X := MinFloat([lmin.X, lmax.X, bmin.X, bmax.X]);
  min.Y := MinFloat([lmin.Y, lmax.Y, bmin.Y, bmax.Y]);
  min.Z := MinFloat([lmin.Z, lmax.Z, bmin.Z, bmax.Z]);

  max.X := MaxFloat([lmin.X, lmax.X, bmin.X, bmax.X]);
  max.Y := MaxFloat([lmin.Y, lmax.Y, bmin.Y, bmax.Y]);
  max.Z := MaxFloat([lmin.Z, lmax.Z, bmin.Z, bmax.Z]);
end;

function TGLTree.AxisAlignedDimensionsUnscaled: TGLVector;
var
  dMin, dMax: TAffineVector;
begin
  if FAxisAlignedDimensionsCache.X < 0 then
  begin
    GetExtents(dMin, dMax);
    FAxisAlignedDimensionsCache.X := MaxFloat(Abs(dMin.X), Abs(dMax.X));
    FAxisAlignedDimensionsCache.Y := MaxFloat(Abs(dMin.Y), Abs(dMax.Y));
    FAxisAlignedDimensionsCache.Z := MaxFloat(Abs(dMin.Z), Abs(dMax.Z));
  end;
  SetVector(Result, FAxisAlignedDimensionsCache);
end;

procedure TGLTree.SetAutoCenter(const Value: Boolean);
begin
  if Value <> FAutoCenter then
  begin
    FAutoCenter := Value;
    if (FAutoRebuild) then
      RebuildTree;
  end;
end;

procedure TGLTree.SetAutoRebuild(const Value: Boolean);
begin
  if Value <> FAutoRebuild then
  begin
    FAutoRebuild := Value;
  end;
end;

initialization

RegisterClasses([TGLTree]);

end.

