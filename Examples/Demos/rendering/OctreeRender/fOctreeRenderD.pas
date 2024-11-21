unit fOctreeRenderD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,

  GLS.Objects,
  GLS.Scene,
  GLS.Cadencer,
  GLS.SceneViewer,
  Stage.VectorGeometry,
  Stage.VectorTypes,
  GLS.GeometryBB,
  GLS.Texture,
  GLS.SpacePartition,

  GLS.Coordinates,
  GLS.RenderContextInfo,
  GLS.State,
  GLS.SimpleNavigation,
  GLS.Material,
  GLS.Context,
  GLS.BaseClasses;

const
  cBOX_SIZE = 14.2;

type
  TfrmOctreeDemo = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLCube1: TGLCube;
    GLCadencer1: TGLCadencer;
    GLSphere1: TGLSphere;
    GLPlane1: TGLPlane;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Panel1: TPanel;
    Label3: TLabel;
    TrackBar_LeafThreshold: TTrackBar;
    Label2: TLabel;
    Button_ResetOctreeSize: TButton;
    LabelCollisions: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure TrackBar_LeafThresholdChange(Sender: TObject);
    procedure Button_ResetOctreeSizeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    Octree: TGLOctreeSpacePartition;
    procedure CreateBox;
    procedure VerifySpacialMisc;
  end;

var
  frmOctreeDemo: TfrmOctreeDemo;

//========================================================
implementation
//========================================================

{$R *.dfm}

procedure TfrmOctreeDemo.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  randomize;

  Octree := TGLOctreeSpacePartition.Create;
  Octree.SetSize(AffineVectorMake(-15, -15, -15), AffineVectorMake(15, 15, 15));
  Octree.MaxTreeDepth := 6;
  Octree.LeafThreshold := 10;

  TrackBar_LeafThresholdChange(nil);

  for i := 0 to 300 do
    CreateBox;

  VerifySpacialMisc;
end;

procedure TfrmOctreeDemo.CreateBox;
  function randomPos: TAffineVector;
  const
    c1 = 10;
  begin
    // MakeVector(result, random*c1-c1/2, random*c1-c1/2, random*c1-c1/2);
    MakeVector(result, random, random, random);
  end;

  function randomSize: Single;
  begin
    result := 0.1 + random * 0.5;
  end;

var
  Cube: TGLCube;
begin
  Cube := TGLCube(GLScene1.Objects.AddNewChild(TGLCube));
  Cube.Position.AsAffineVector := randomPos;
  Cube.CubeWidth := randomSize;
  Cube.CubeHeight := randomSize;
  Cube.CubeDepth := randomSize;

  TGLSpacePartitionLeafS.CreateGLOwned(Octree, Cube);
end;

procedure TfrmOctreeDemo.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);

  procedure RenderAABB(AABB: TAABB; w, r, g, b: Single);
  begin
    glColor3f(r, g, b);
    rci.GLStates.LineWidth := w;

    glBegin(GL_LINE_STRIP);
    glVertex3f(AABB.Min.X, AABB.Min.Y, AABB.Min.Z);
    glVertex3f(AABB.Min.X, AABB.Max.Y, AABB.Min.Z);
    glVertex3f(AABB.Max.X, AABB.Max.Y, AABB.Min.Z);
    glVertex3f(AABB.Max.X, AABB.Min.Y, AABB.Min.Z);
    glVertex3f(AABB.Min.X, AABB.Min.Y, AABB.Min.Z);

    glVertex3f(AABB.Min.X, AABB.Min.Y, AABB.Max.Z);
    glVertex3f(AABB.Min.X, AABB.Max.Y, AABB.Max.Z);
    glVertex3f(AABB.Max.X, AABB.Max.Y, AABB.Max.Z);
    glVertex3f(AABB.Max.X, AABB.Min.Y, AABB.Max.Z);
    glVertex3f(AABB.Min.X, AABB.Min.Y, AABB.Max.Z);
    glEnd();

    glBegin(GL_LINES);
    glVertex3f(AABB.Min.X, AABB.Max.Y, AABB.Min.Z);
    glVertex3f(AABB.Min.X, AABB.Max.Y, AABB.Max.Z);

    glVertex3f(AABB.Max.X, AABB.Max.Y, AABB.Min.Z);
    glVertex3f(AABB.Max.X, AABB.Max.Y, AABB.Max.Z);

    glVertex3f(AABB.Max.X, AABB.Min.Y, AABB.Min.Z);
    glVertex3f(AABB.Max.X, AABB.Min.Y, AABB.Max.Z);
    glEnd;
  end;

  procedure RenderOctreeNode(Node: TGLSectorNode);
  var
    i: Integer;
    AABB: TAABB;
  begin
    if Node.NoChildren then
    begin
      AABB := Node.AABB;

      if Node.RecursiveLeafCount > 0 then
        RenderAABB(AABB, 1, 0, 0, 0)
      else
        RenderAABB(AABB, 1, 0.8, 0.8, 0.8) // }

    end
    else
    begin
      for i := 0 to Node.ChildCount - 1 do
        RenderOctreeNode(Node.Children[i]);
    end;
  end;

var
  AABB: TAABB;
begin
  rci.GLStates.Disable(stLighting);
  rci.GLStates.Enable(stColorMaterial);

  MakeVector(AABB.Min, -cBOX_SIZE, -cBOX_SIZE, -cBOX_SIZE);
  MakeVector(AABB.Max, cBOX_SIZE, cBOX_SIZE, cBOX_SIZE);
  RenderAABB(AABB, 2, 0, 0, 0);
  RenderOctreeNode(Octree.RootNode);
end;

procedure TfrmOctreeDemo.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
  Handled := true
end;

procedure TfrmOctreeDemo.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);

var
  AABB: TAABB;
  BSphere: TBSphere;
  Leaf, TestLeaf: TGLSpacePartitionLeafS;
  Cube: TGLCube;
  i, j, CollidingLeafCount: Integer;

  (*sub*)function TestMove(pos: Single; var dir: Single): Single;
  const
    cSPEED = 2;
  begin
    if (abs(pos + dir * deltaTime * cSPEED) >= cBOX_SIZE) then
      dir := -dir;
    result := pos + dir * deltaTime * cSPEED;
  end;

begin
  for i := 0 to Octree.Leaves.Count - 1 do
  begin
    Leaf := TGLSpacePartitionLeafS(Octree.Leaves[i]);
    Cube := TGLCube(Leaf.GLBaseSceneObject);
    Cube.Position.X := TestMove(Cube.Position.X, Leaf.Direction.X);
    Cube.Position.Y := TestMove(Cube.Position.Y, Leaf.Direction.Y);
    Cube.Position.Z := TestMove(Cube.Position.Z, Leaf.Direction.Z);

    Leaf.Changed;
  end; // }

  for i := 0 to Octree.Leaves.Count - 1 do
  begin
    Leaf := TGLSpacePartitionLeafS(Octree.Leaves[i]);
    TGLCube(Leaf.GLBaseSceneObject).Material.FrontProperties.Emission.Red := 0;
    TGLCube(Leaf.GLBaseSceneObject).Material.FrontProperties.Emission.
      Green := 0;
    TGLCube(Leaf.GLBaseSceneObject).Material.FrontProperties.Emission.Blue := 0;
  end; // }

  // AABB collision
  AABB := GLCube1.AxisAlignedBoundingBox;
  AABB.Min := GLCube1.LocalToAbsolute(AABB.Min);
  AABB.Max := GLCube1.LocalToAbsolute(AABB.Max);

  Octree.QueryAABB(AABB);

  for i := 0 to Octree.QueryResult.Count - 1 do
  begin
    Leaf := TGLSpacePartitionLeafS(Octree.QueryResult[i]);
    TGLCube(Leaf.GLBaseSceneObject).Material.FrontProperties.Emission.Red := 1;
  end; // }

  // BSphere collision
  BSphere.Center := GLSphere1.Position.AsAffineVector;
  BSphere.Radius := GLSphere1.Radius;

  Octree.QueryBSphere(BSphere);

  for i := 0 to Octree.QueryResult.Count - 1 do
  begin
    Leaf := TGLSpacePartitionLeafS(Octree.QueryResult[i]);
    TGLCube(Leaf.GLBaseSceneObject).Material.FrontProperties.Emission.Red := 1;
  end; // }

  // Plane collision
  if GLPlane1.Visible then
  begin
    Octree.QueryPlane(GLPlane1.Position.AsAffineVector,
      GLPlane1.Direction.AsAffineVector);

    for i := 0 to Octree.QueryResult.Count - 1 do
    begin
      Leaf := TGLSpacePartitionLeafS(Octree.QueryResult[i]);
      TGLCube(Leaf.GLBaseSceneObject)
        .Material.FrontProperties.Emission.Blue := 1;
    end; // }
  end;

  // Leaf - leaf collision
  CollidingLeafCount := 0;
  for i := 0 to Octree.Leaves.Count - 1 do
  begin
    Leaf := TGLSpacePartitionLeafS(Octree.Leaves[i]);

    Octree.QueryAABB(Leaf.FCachedAABB);

    for j := 0 to Octree.QueryResult.Count - 1 do
    begin
      TestLeaf := TGLSpacePartitionLeafS(Octree.QueryResult[j]);
      if TestLeaf <> Leaf then
      begin
        TGLCube(TestLeaf.GLBaseSceneObject).Material.FrontProperties.Emission.
          Green := 1;
        inc(CollidingLeafCount);
      end;
    end;
  end; // }//*)

  // Cone collision
  { Cone.Base := GLCone1.Position.AsAffineVector;
    Cone.Axis := VectorScale(GLCone1.Up.AsAffineVector, -1);
    Cone.Length := GLCone1.Height;
    Cone.Angle := ArcTan(GLCone1.BottomRadius/GLCone1.Height);

    Octree.QueryCone(Cone);

    for i := 0 to Octree.QueryResult.Count-1 do
    begin
    Leaf := TGLSpacePartitionLeaf(Octree.QueryResult[i]);
    TGLCube(Leaf.GLBaseSceneObject).Material.FrontProperties.Emission.Red := 1;
    end;// }

  LabelCollisions.Caption := Format('Nodes = %d, Colliding Leaves = %d',
    [Octree.GetNodeCount, CollidingLeafCount]);
end;

procedure TfrmOctreeDemo.TrackBar_LeafThresholdChange(Sender: TObject);
begin
  Label3.Caption := Format('Leaf Threshold : %d',
    [TrackBar_LeafThreshold.Position]);

  Octree.LeafThreshold := TrackBar_LeafThreshold.Position;
end;

procedure TfrmOctreeDemo.VerifySpacialMisc;
var
  AABBmajor, AABBfull, AABBoff, AABBpartial: TAABB;
begin
  MakeVector(AABBmajor.Min, 0, 0, 0);
  MakeVector(AABBmajor.Max, 5, 5, 5);

  MakeVector(AABBpartial.Min, 4, 4, 4);
  MakeVector(AABBpartial.Max, 6, 6, 6);

  MakeVector(AABBfull.Min, 1, 1, 1);
  MakeVector(AABBfull.Max, 2, 2, 2);

  MakeVector(AABBoff.Min, 7, 7, 7);
  MakeVector(AABBoff.Max, 8, 8, 8);

  Assert(AABBFitsInAABBAbsolute(AABBfull, AABBmajor),
    'AABBFitsInAABBAbsolute failed!');
  Assert(IntersectAABBsAbsolute(AABBfull, AABBmajor),
    'IntersectAABBsAbsolute failed!');
  Assert(AABBContainsAABB(AABBmajor, AABBfull) = scContainsFully,
    'AABBContainsAABB failed!');

  Assert(not AABBFitsInAABBAbsolute(AABBmajor, AABBfull),
    'AABBFitsInAABBAbsolute failed!');
  Assert(IntersectAABBsAbsolute(AABBmajor, AABBfull),
    'IntersectAABBsAbsolute failed!');
  Assert(AABBContainsAABB(AABBfull, AABBmajor) = scContainsPartially,
    'AABBContainsAABB failed!');

  Assert(not AABBFitsInAABBAbsolute(AABBfull, AABBoff),
    'AABBFitsInAABBAbsolute failed!');
  Assert(not IntersectAABBsAbsolute(AABBfull, AABBoff),
    'IntersectAABBsAbsolute failed!');
  Assert(AABBContainsAABB(AABBfull, AABBoff) = scNoOverlap,
    'AABBContainsAABB failed!');

  Assert(not AABBFitsInAABBAbsolute(AABBoff, AABBfull),
    'AABBFitsInAABBAbsolute failed!');
  Assert(not IntersectAABBsAbsolute(AABBoff, AABBfull),
    'IntersectAABBsAbsolute failed!');
  Assert(AABBContainsAABB(AABBoff, AABBfull) = scNoOverlap,
    'AABBContainsAABB failed!');

  Assert(not AABBFitsInAABBAbsolute(AABBmajor, AABBpartial),
    'AABBFitsInAABBAbsolute failed!');
  Assert(not AABBFitsInAABBAbsolute(AABBpartial, AABBmajor),
    'AABBFitsInAABBAbsolute failed!');

  Assert(IntersectAABBsAbsolute(AABBmajor, AABBpartial),
    'IntersectAABBsAbsolute failed!');
  Assert(IntersectAABBsAbsolute(AABBpartial, AABBmajor),
    'IntersectAABBsAbsolute failed!');

  Assert(AABBContainsAABB(AABBmajor, AABBpartial) = scContainsPartially,
    'AABBContainsAABB failed!');
  Assert(AABBContainsAABB(AABBpartial, AABBmajor) = scContainsPartially,
    'AABBContainsAABB failed!');
end;

procedure TfrmOctreeDemo.Button_ResetOctreeSizeClick(Sender: TObject);
begin
  Octree.GrowMethod := gmBestFit;
  Octree.UpdateStructureSize(0.05);
  Octree.GrowMethod := gmIncreaseToFitAll;
end;

procedure TfrmOctreeDemo.FormDestroy(Sender: TObject);
begin
  Octree.Free;
end;

end.
