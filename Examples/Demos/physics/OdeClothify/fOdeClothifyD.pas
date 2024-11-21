unit fOdeClothifyD;

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
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,

  GLS.Objects,
  GLS.Scene,
  GLS.PersistentClasses,
  GLS.VectorFileObjects,
  GLS.SceneViewer,
  GLS.FileMS3D,
  GLS.VerletTypes,
  GLS.VerletClothify,
  Stage.VectorTypes,
  GLS.VectorLists,
  Stage.VectorGeometry,
  GLS.Texture,
  GLS.FileSMD,
  GLS.Cadencer,
  GLS.ShadowPlane,
  GLS.File3DS,
  GLS.GeometryBB,
  GLS.SpacePartition,
  GLS.GeomObjects,
  GLS.ShadowVolume,
  Stage.Utils,
  GLS.Material,
  GLS.Coordinates,
  GLS.RenderContextInfo,
  GLS.State,
  GLS.Context,
  GLS.BaseClasses,

  ODE.Import,
  GLS.ODEUtils;

type
  TFormClothify = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLActor1: TGLActor;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    GLSphere1: TGLSphere;
    GLCylinder1: TGLCylinder;
    GLShadowPlane1: TGLShadowPlane;
    GLCube1: TGLCube;
    GLCube_Stair1: TGLCube;
    GLDummyCube_Stairs: TGLDummyCube;
    GLCube_Stair2: TGLCube;
    GLCube_Stair3: TGLCube;
    GLCube_Stair4: TGLCube;
    GLDummyCube2: TGLDummyCube;
    GL_Capsule: TGLCylinder;
    GLSphere2: TGLSphere;
    GLSphere3: TGLSphere;
    GLDummyCube_Light: TGLDummyCube;
    GLActor2: TGLActor;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GroupBox_LoadForm: TGroupBox;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ComboBox_MeshName: TComboBox;
    ComboBox_ConstraintType: TComboBox;
    ComboBox_Collider: TComboBox;
    Button_LoadMesh: TButton;
    CheckBox_UseOctree: TCheckBox;
    CheckBox_SolidEdges: TCheckBox;
    CheckBox_Weld: TCheckBox;
    Button_CancelLoad: TButton;
    GLShadowVolume1: TGLShadowVolume;
    GLPlane1: TGLPlane;
    Panel1: TPanel;
    Label3: TLabel;
    TrackBar_Slack: TTrackBar;
    Label6: TLabel;
    TrackBar_Iterations: TTrackBar;
    Label7: TLabel;
    TrackBar_Friction: TTrackBar;
    CheckBox_ShowOctree: TCheckBox;
    Button_OpenLoadForm: TButton;
    Label8: TLabel;
    ComboBox_Shadow: TComboBox;
    Label1: TLabel;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure Button_LoadMeshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar_SlackChange(Sender: TObject);
    function GetSlack: single;
    procedure TrackBar_IterationsChange(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TrackBar_FrictionChange(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure Button_OpenLoadFormClick(Sender: TObject);
    procedure Button_CancelLoadClick(Sender: TObject);
    procedure ComboBox_ShadowChange(Sender: TObject);
  private
  public
    mx, my: Integer;
    VerletWorld: TGLVerletWorld;
    EdgeDetector: TGLEdgeDetector;
    world: PdxWorld;
    space: PdxSpace;
    ODESphere: PdxGeom;
    body: PdxBody;
    contactgroup: TdJointGroupID;
    VerletSphere: TGLVerletFrictionSphere;
  end;

procedure RecalcMeshNormals(BaseMesh: TGLBaseMesh);
procedure PrepareMeshForNormalsRecalc(BaseMesh: TGLBaseMesh);

var
  FormClothify: TFormClothify;

implementation

{$R *.dfm}

//-----------------------------------------------------------------
procedure TFormClothify.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\modelext');
  
  ComboBox_MeshName.ItemIndex := 0;
  ComboBox_ConstraintType.ItemIndex := 0;
  ComboBox_Collider.ItemIndex := 3;
  ComboBox_ShadowChange(nil);
  Button_LoadMesh.Click;
  TrackBar_IterationsChange(nil);
  GLShadowVolume1.Occluders.AddCaster(GLActor1);
end;

//-----------------------------------------------------------------
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

//-----------------------------------------------------------------
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

//----------------------------------------------------------------------
procedure TFormClothify.Button_LoadMeshClick(Sender: TObject);
var
  Floor: TGLVerletFloor;
  Capsule: TGLVerletFrictionCapsule;
  Cube: TGLVerletFrictionCube;
  Sides: TAffineVector;
  ColliderGravy: Single;
  s: string;
  f: single;
  p: Integer;

  procedure CreateCubeFromGLCube(GLCube: TGLCube);
  begin
    Cube := TGLVerletFrictionCube.Create(VerletWorld);
    Cube.Location := AffineVectorMake(GLCube.AbsolutePosition);
    Cube.FrictionRatio := 0.1;
    Sides.X := GLCube.CubeWidth * 1.1;
    Sides.Y := GLCube.CubeHeight * 1.1;
    Sides.Z := GLCube.CubeDepth * 1.1;
    Cube.Sides := Sides; // }
  end;

  procedure CreateODEWorld;
  var
    m: TdMass;

  begin
    GLSphere1.Visible := true;
    world := dWorldCreate;
    dWorldSetGravity(world, 0, -9.81, 0);

    contactgroup := dJointGroupCreate(0);
    space := dHashSpaceCreate(nil);
    body := dBodyCreate(world);
    dMassSetSphere(m, 0.1, GLSphere1.Radius);
    dCreatePlane(space, 0, 1, 0, GLShadowPlane1.Position.Y);

    ODESphere := dCreateSphere(space, GLSphere1.Radius);

    dGeomSetBody(ODESphere, body);
    dBodySetMass(body, @m);
    /// How to set centre of mass to origin ???

    ODESphere.data := GLSphere1;

    PositionSceneObjectForGeom(ODESphere);
  end;

begin
  randomize;

  if world <> nil then
  begin
    dWorldDestroy(world);
    world := nil;
    GLSphere1.Position.AsAffineVector := NullVector;
  end;

  FreeAndNil(VerletWorld);
  FreeAndNil(EdgeDetector);

  s := ComboBox_MeshName.Items[MaxInteger(ComboBox_MeshName.ItemIndex, 0)];

  FormatSettings.DecimalSeparator := '.';

  p := Pos(',', s);
  if p > 0 then
  begin
    f := StrToFloatDef(Trim(Copy(s, p + 1, MaxInt)), 1);
    GLActor1.Scale.AsVector := VectorMake(f, f, f, 0)
  end
  else
    GLActor1.Scale.AsVector := XYZHmgVector;

  GLActor1.AutoCentering := [macUseBarycenter];
  GLActor1.LoadFromFile(Trim(Copy(s, 1, p - 1)));
  PrepareMeshForNormalsRecalc(GLActor1);
  GLActor1.Reference := aarNone;

  // This is what allows for FAST shadow volumes when using actors. Without
  // this line, it'll be _very_ slow.
  GLActor1.BuildSilhouetteConnectivityData;

  GLActor1.Roll(random * 360);
  GLActor1.Turn(random * 360); // }

  GLSphere1.Visible := false;
  GLCylinder1.Visible := false;
  GLCube1.Visible := false;
  GLDummyCube_Stairs.Visible := false;
  GL_Capsule.Visible := false;

  case ComboBox_Collider.ItemIndex of
    0, -1:
      GLSphere1.Visible := true;
    1:
      GLCylinder1.Visible := true;
    2:
      GLCube1.Visible := true;
    3:
      GLDummyCube_Stairs.Visible := true;
    4:
      GL_Capsule.Visible := true;
    5:
      CreateODEWorld;
  end;

  EdgeDetector := TGLEdgeDetector.Create(GLActor1);

  if not CheckBox_Weld.Checked then
    EdgeDetector.WeldDistance := -1;

  EdgeDetector.ProcessMesh;

  VerletWorld := TGLVerletWorld.Create;

  if CheckBox_UseOctree.Checked then
    VerletWorld.CreateOctree(AffineVectorMake(-20, -5.5, -20),
      AffineVectorMake(20, 20, 20), 25, 5); // }

  if CheckBox_SolidEdges.Checked then
  begin
    ColliderGravy := 1;
    EdgeDetector.AddEdgesAsSolidEdges(VerletWorld);
  end
  else
    ColliderGravy := 1.1;

  if ComboBox_ConstraintType.ItemIndex = 0 then
    EdgeDetector.AddEdgesAsSticks(VerletWorld, GetSlack)
  else
    EdgeDetector.AddEdgesAsSprings(VerletWorld, 1000, 100, GetSlack); // }

  // VerletWorld.Nodes[0].NailedDown := true;

  TGLVerletGravity.Create(VerletWorld);

  Floor := TGLVerletFloor.Create(VerletWorld);
  Floor.Location := VectorAdd(GLShadowPlane1.Position.AsAffineVector,
    AffineVectorMake(0, 0.1, 0));
  Floor.Normal := GLShadowPlane1.Direction.AsAffineVector;

  Floor.FrictionRatio := 0.6; // }

  if GLSphere1.Visible then
  begin
    VerletSphere := TGLVerletFrictionSphere.Create(VerletWorld);
    VerletSphere.Radius := GLSphere1.Radius * ColliderGravy;
    VerletSphere.Location := AffineVectorMake(GLSphere1.AbsolutePosition);
  end;

  if GLCube1.Visible then
  begin
    CreateCubeFromGLCube(GLCube1);
  end;

  if GLCylinder1.Visible then
  begin
    Capsule := TGLVerletFrictionCapsule.Create(VerletWorld);
    Capsule.Radius := GLCylinder1.TopRadius * ColliderGravy;
    Capsule.Location := AffineVectorMake(GLCylinder1.AbsolutePosition);
    Capsule.Axis := AffineVectorMake(GLCylinder1.AbsoluteUp); // }
    Capsule.Length := 20;
    Capsule.FrictionRatio := 0.6;
  end;

  if GL_Capsule.Visible then
  begin
    Capsule := TGLVerletFrictionCapsule.Create(VerletWorld);
    Capsule.Radius := GL_Capsule.TopRadius * ColliderGravy;
    Capsule.Location := AffineVectorMake(GL_Capsule.AbsolutePosition);
    Capsule.Axis := AffineVectorMake(GL_Capsule.AbsoluteUp); // }
    Capsule.Length := GL_Capsule.Height * ColliderGravy;
    Capsule.FrictionRatio := 0.6;
  end;

  if GLDummyCube_Stairs.Visible then
  begin
    CreateCubeFromGLCube(GLCube_Stair1);
    CreateCubeFromGLCube(GLCube_Stair2);
    CreateCubeFromGLCube(GLCube_Stair3);
    CreateCubeFromGLCube(GLCube_Stair4);
  end;

  VerletWorld.SimTime := GLCadencer1.GetCurrentTime;
  VerletWorld.MaxDeltaTime := 0.01;
  VerletWorld.Iterations := TrackBar_Iterations.Position;

  TrackBar_FrictionChange(nil);

  GroupBox_LoadForm.Hide;

  CheckBox_ShowOctree.Enabled := CheckBox_UseOctree.Checked;
  TrackBar_Iterations.Enabled := (ComboBox_ConstraintType.ItemIndex = 0);
  TrackBar_Slack.Enabled := (ComboBox_ConstraintType.ItemIndex = 0);
end;

procedure TFormClothify.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);

  mx := X;
  my := Y
end;

procedure nearCallback(data: pointer; o1, o2: PdxGeom); cdecl;
const
  cCOL_MAX = 3;
var
  i: Integer;
  b1, b2: PdxBody;
  numc: Integer;
  contact: array [0 .. cCOL_MAX - 1] of TdContact;
  c: TdJointID;
begin
  // exit without doing anything if the two bodies are connected by a joint
  b1 := dGeomGetBody(o1);
  b2 := dGeomGetBody(o2);
  if (assigned(b1) and assigned(b2) and (dAreConnected(b1, b2) <> 0)) then
    exit; // }

  for i := 0 to cCOL_MAX - 1 do
  begin
    contact[i].surface.mode := dContactBounce;

    // This determines friction, play around with it!
    contact[i].surface.mu := 10E9; // dInfinity; SHOULD BE INFINITY!
    contact[i].surface.mu2 := 0;
    contact[i].surface.bounce := 0.5; // 0.5;
    contact[i].surface.bounce_vel := 0.1;
  end;

  numc := dCollide(o1, o2, cCOL_MAX, contact[0].geom, SizeOf(TdContact));
  if (numc > 0) then
  begin
    // dMatrix3 RI;
    // dRSetIdentity (RI);
    // const dReal ss[3] = {0.02,0.02,0.02};
    for i := 0 to numc - 1 do
    begin
      c := dJointCreateContact(FormClothify.world, FormClothify.contactgroup,
        @contact[i]);
      dJointAttach(c, b1, b2);
      // dsDrawBox (contact[i].geom.pos,RI,ss);
    end;
  end;
end;

//-------------------------------------------------------------
procedure TFormClothify.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  (* if CheckBox_Pause.Checked then
    VerletWorld.SimTime := newTime
    else// *)
  begin
    if world <> nil then
    begin
      PositionSceneObjectForGeom(ODESphere);
      VerletSphere.Location := GLSphere1.Position.AsAffineVector;
      dBodyAddForce(dGeomGetBody(ODESphere), VerletSphere.KickbackForce.X,
        VerletSphere.KickbackForce.Y, VerletSphere.KickbackForce.Z);
      dSpaceCollide(space, nil, nearCallback);
      dWorldStep(world, VerletWorld.MaxDeltaTime);
      dJointGroupEmpty(contactgroup);
    end;

    VerletWorld.Progress(VerletWorld.MaxDeltaTime, newTime);
    RecalcMeshNormals(GLActor1);
  end;
end;

//----------------------------------------------------------------
procedure TFormClothify.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := Format('%2.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormClothify.TrackBar_SlackChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to VerletWorld.Constraints.Count - 1 do
  begin
    if VerletWorld.Constraints[i] is TGLVerletStick then
      TGLVerletStick(VerletWorld.Constraints[i]).Slack := GetSlack;
  end;
end;

function TFormClothify.GetSlack: single;
begin
  result := TrackBar_Slack.Position / 500;
end;

procedure TFormClothify.TrackBar_IterationsChange(Sender: TObject);
begin
  VerletWorld.Iterations := TrackBar_Iterations.Position;

  Label6.Caption := Format('Iterations %d', [TrackBar_Iterations.Position]);
end;

//-----------------------------------------------------------------------
procedure TFormClothify.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TFormClothify.TrackBar_FrictionChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to VerletWorld.Constraints.Count - 1 do
    if VerletWorld.Constraints[i] is TGLVerletGlobalFrictionConstraint then
      TGLVerletGlobalFrictionConstraint(VerletWorld.Constraints[i])
        .FrictionRatio := TrackBar_Friction.Position / 100;
end;

//------------------------------------------------------------------
procedure TFormClothify.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
  procedure RenderAABB(AABB: TAABB; w, r, g, b: single);
  begin
    glColor3f(r, g, b);
    rci.GLStates.LineWidth := w;

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

begin
  if CheckBox_ShowOctree.Checked and
    (VerletWorld.SpacePartition is TGLOctreeSpacePartition) then
  begin
    rci.GLStates.PushAttrib([sttEnable, sttCurrent, sttLine, sttColorBuffer]);
    rci.GLStates.Disable(stLighting);

    RenderOctreeNode(TGLOctreeSpacePartition(VerletWorld.SpacePartition)
      .RootNode);
    rci.GLStates.PopAttrib;
  end;
end;

//--------------------------------------------------------------------
procedure TFormClothify.Button_OpenLoadFormClick(Sender: TObject);
begin
  GroupBox_LoadForm.Visible := true;
  GroupBox_LoadForm.SetFocus;
end;

procedure TFormClothify.Button_CancelLoadClick(Sender: TObject);
begin
  GroupBox_LoadForm.Hide;
end;

//-------------------------------------------------------------------
procedure TFormClothify.ComboBox_ShadowChange(Sender: TObject);
begin
  GLShadowVolume1.mode := svmOff;
  GLShadowPlane1.Visible := false;
  GLPlane1.Visible := true;
  GLSceneViewer1.Buffer.ContextOptions := GLSceneViewer1.Buffer.ContextOptions -
    [roStencilBuffer];

  case ComboBox_Shadow.ItemIndex of
    0:
      ;
    1:
      begin
        GLShadowVolume1.mode := svmDarkening;
        GLSceneViewer1.Buffer.ContextOptions :=
          GLSceneViewer1.Buffer.ContextOptions + [roStencilBuffer];
      end;
    2:
      begin
        GLShadowPlane1.Visible := true;
        GLPlane1.Visible := false;
        GLSceneViewer1.Buffer.ContextOptions :=
          GLSceneViewer1.Buffer.ContextOptions + [roStencilBuffer];
      end;
  end;
end;

end.
