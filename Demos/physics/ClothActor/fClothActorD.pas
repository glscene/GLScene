unit fClothActorD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.VectorFileObjects,
  GLS.Objects,
  GLS.Cadencer,
  GLS.Texture,
  GLS.SceneViewer,
  GLS.FileSMD,
  GLS.File3DS,
  GLS.VerletTypes,
  GLS.VerletClothify,
  GLS.ShadowVolume,
  GLS.Keyboard,
  GLS.VectorGeometry,
  GLS.GeometryBB,
  GLS.SpacePartition,

  GLS.Material,
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.Context,
  GLS.Utils,
  GLS.Coordinates,
  GLS.PersistentClasses;

type
  TFormClothActor = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLActor1: TGLActor;
    ActorDummy: TGLDummyCube;
    Timer1: TTimer;
    OctreeRenderer: TGLDirectOpenGL;
    cbShowOctree: TCheckBox;
    GLLightSource1: TGLLightSource;
    GLPlane1: TGLPlane;
    GLShadowVolume1: TGLShadowVolume;
    Cape: TGLActor;
    GLLightSource2: TGLLightSource;
    StaticTextFPS: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure OctreeRendererRender(Sender: TObject; var rci: TGLRenderContextInfo);
  public
    mx, my: Integer;
    VerletWorld: TGLVerletWorld;
    EdgeDetector: TGLEdgeDetector;
    AirResistance: TGLVerletAirResistance;
  end;

var
  FormClothActor: TFormClothActor;

implementation

{$R *.dfm}
// Mesh normal recalculation routines

procedure PrepareMeshForNormalsRecalc(BaseMesh: TGLBaseMesh);
var
  i, j, k: Integer;
  mo: TGLMeshObject;
  fg: TFGVertexNormalTexIndexList;
begin
  // update normals
  // (not very efficient, could use some work...)
  for i := 0 to BaseMesh.MeshObjects.Count - 1 do
  begin
    mo := BaseMesh.MeshObjects[i];
    for j := 0 to mo.FaceGroups.Count - 1 do
    begin
      if mo.FaceGroups[j] is TFGVertexNormalTexIndexList then
      begin
        fg := TFGVertexNormalTexIndexList(mo.FaceGroups[j]);
        for k := 0 to fg.VertexIndices.Count - 1 do
        begin
          fg.NormalIndices.List[k] := fg.VertexIndices.List[k];
        end;
      end;
    end;
  end;
  BaseMesh.StructureChanged;
end;

procedure RecalcMeshNormals(BaseMesh: TGLBaseMesh);
var
  i, j, k: Integer;
  mo: TGLMeshObject;
  fg: TFGVertexIndexList;
  n: TAffineVector;
begin
  // update normals
  // (not very efficient, could use some work...)
  for i := 0 to BaseMesh.MeshObjects.Count - 1 do
  begin
    mo := BaseMesh.MeshObjects[i];
    FillChar(mo.Normals.List[0], SizeOf(TAffineVector) * mo.Normals.Count, 0);
    for j := 0 to mo.FaceGroups.Count - 1 do
    begin
      if mo.FaceGroups[j] is TFGVertexIndexList then
      begin
        fg := TFGVertexIndexList(mo.FaceGroups[j]);
        k := 0;
        while k <= fg.VertexIndices.Count - 3 do
        begin
          n := CalcPlaneNormal(mo.Vertices.List[fg.VertexIndices.List[k]],
            mo.Vertices.List[fg.VertexIndices.List[k + 1]],
            mo.Vertices.List[fg.VertexIndices.List[k + 2]]);
          mo.Normals.TranslateItem(fg.VertexIndices.List[k], n);
          mo.Normals.TranslateItem(fg.VertexIndices.List[k + 1], n);
          mo.Normals.TranslateItem(fg.VertexIndices.List[k + 2], n); // }
          Inc(k, 3);
        end;
      end;
    end;
    mo.Normals.Normalize;
  end;
  BaseMesh.StructureChanged;
end;

procedure TFormClothActor.FormCreate(Sender: TObject);
var
  FloorVC: TGLVerletFloor;
begin
  var
    Path: TFileName := GetCurrentAssetPath();
  Randomize;

  // Load dynamic models of actors with textures and/or animations
  SetCurrentDir(Path + '\modelext');
  GLActor1.LoadFromFile('trinityRAGE.smd');
  GLActor1.AddDataFromFile('walk.smd');
  GLActor1.Animations[1].MakeSkeletalTranslationStatic;
  GLActor1.SwitchToAnimation('walk');
  GLActor1.BuildSilhouetteConnectivityData;

  // Load static models of objects
  SetCurrentDir(Path + '\model');

  // the cape must be loaded after trinityRAGE
  Cape.LoadFromFile('cape.3ds');
  Cape.Position.Y := GLActor1.BoundingSphereRadius - 10;
  PrepareMeshForNormalsRecalc(Cape);
  Cape.BuildSilhouetteConnectivityData;


  // Set up the floor texture and reposition to below the actors feet
  SetCurrentDir(Path + '\texture');
  GLPlane1.Material.Texture.Image.LoadFromFile('beigemarble.jpg');
  GLPlane1.Material.Texture.Disabled := False;
  GLPlane1.Position.Y := -GLActor1.BoundingSphereRadius * 0.9;

  // Setting up the verlet world using the optional dynamic octree can
  // give good perfamnce increases.
  VerletWorld := TGLVerletWorld.Create;
  VerletWorld.CreateOctree(AffineVectorMake(0, 0, 0), AffineVectorMake(0, 0, 0), 10, 6);

  VerletWorld.UpdateSpacePartion := uspEveryFrame;
  VerletWorld.Iterations := 3;

  // 'Clothify' the cape and add it to the verlet world
  EdgeDetector := TGLEdgeDetector.Create(Cape);
  EdgeDetector.ProcessMesh;
  EdgeDetector.AddEdgesAsSticks(VerletWorld, 0.15);
  EdgeDetector.AddEdgesAsSolidEdges(VerletWorld);
  // EdgeDetector.AddOuterEdgesAsSolidEdges(VerletWorld);

  // Set up verlet gravity and add the floor as a constraint
  TGLVerletGravity.Create(VerletWorld).Gravity := AffineVectorMake(0, -98.1, 0);
  FloorVC := TGLVerletFloor.Create(VerletWorld);
  FloorVC.Normal := GLPlane1.Direction.AsAffineVector;
  FloorVC.Location := VectorAdd(GLPlane1.Position.AsAffineVector,
    VectorScale(GLPlane1.Direction.AsAffineVector, 0.1));

  (* Load the skeleton colliders. Skeleton colliders define an
    approximate collision boundary for actors and are controlled
    by the actor's skeleton. *)
  SetCurrentDir(Path + '\scenery');
  GLActor1.Skeleton.Colliders.LoadFromFile('trinityRAGE.glsc');
  GLActor1.Skeleton.Colliders.AlignColliders;

  // Add the collider's verlet constraints to the verlet world
  AddVerletConstriantsToVerletWorld(GLActor1.Skeleton.Colliders, VerletWorld);
  (*
    AirResistance := TGLVerletAirResistance.Create(VerletWorld);
    AirResistance.DragCoeff := 0.001;
    AirResistance.WindDirection := AffineVectorMake(0,0,1);
    AirResistance.WindMagnitude := 15;
    AirResistance.WindChaos := 2;
    // *)
  FloorVC.Free;
end;

procedure TFormClothActor.FormDestroy(Sender: TObject);
begin
  VerletWorld.Free;
end;

procedure TFormClothActor.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
var
  i: Integer;
begin
  // Step the verlet world (this is where the magic happens)
  VerletWorld.Progress(deltaTime, newTime);
  // Recalculate the cape's normals
  RecalcMeshNormals(Cape);
  // Cycle the floor texture to make it look like it's moving
  GLPlane1.YOffset := GLPlane1.YOffset - 0.25 * deltaTime;
  if GLPlane1.YOffset < 0 then
    GLPlane1.YOffset := GLPlane1.YOffset + 1;
  // Orbit the light (to show off the pretty shadow volumes)
  GLLightSource1.MoveObjectAround(GLActor1, 0, -deltaTime * 20);
  GLLightSource1.PointTo(GLActor1, YHMGVector);
end;

procedure RenderAABB(AABB: TAABB; w, r, g, b: single);
begin
  glColor3f(r, g, b);
  glLineWidth(w);

  glBegin(GL_LINE_STRIP);
  glVertex3f(AABB.min.X, AABB.min.Y, AABB.min.Z);
  glVertex3f(AABB.min.X, AABB.max.Y, AABB.min.Z);
  glVertex3f(AABB.max.X, AABB.max.Y, AABB.min.Z);
  glVertex3f(AABB.max.X, AABB.min.Y, AABB.min.Z);
  glVertex3f(AABB.min.X, AABB.min.Y, AABB.min.Z);
  glVertex3f(AABB.min.X, AABB.min.Y, AABB.max.Z);
  glVertex3f(AABB.min.X, AABB.max.Y, AABB.max.Z);
  glVertex3f(AABB.max.X, AABB.max.Y, AABB.max.Z);
  glVertex3f(AABB.max.X, AABB.min.Y, AABB.max.Z);
  glVertex3f(AABB.min.X, AABB.min.Y, AABB.max.Z);
  glEnd;

  glBegin(GL_LINES);
  glVertex3f(AABB.min.X, AABB.max.Y, AABB.min.Z);
  glVertex3f(AABB.min.X, AABB.max.Y, AABB.max.Z);
  glVertex3f(AABB.max.X, AABB.max.Y, AABB.min.Z);
  glVertex3f(AABB.max.X, AABB.max.Y, AABB.max.Z);
  glVertex3f(AABB.max.X, AABB.min.Y, AABB.min.Z);
  glVertex3f(AABB.max.X, AABB.min.Y, AABB.max.Z);
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

procedure TFormClothActor.OctreeRendererRender(Sender: TObject; var rci: TGLRenderContextInfo);
begin
  if cbShowOctree.Checked then
  begin
    if VerletWorld.SpacePartition is TGLOctreeSpacePartition then
    begin
      glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
      glDisable(GL_LIGHTING);
      RenderOctreeNode(TGLOctreeSpacePartition(VerletWorld.SpacePartition).RootNode);
      glPopAttrib;
    end;
  end;
end;

procedure TFormClothActor.Timer1Timer(Sender: TObject);
begin
  StaticTextFPS.Caption := Format('%2.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormClothActor.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormClothActor.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

end.
