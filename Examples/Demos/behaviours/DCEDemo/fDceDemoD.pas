unit fDceDemoD;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,

  GLS.Scene,
  GLS.Objects,
  GLS.PersistentClasses,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.DCE,
  GLS.Material,
  GLS.Texture,
  GLS.HeightData,
  GLS.TerrainRenderer,
  GLS.VectorFileObjects,
  GLS.BitmapFont,
  GLS.WindowsFont,
  GLS.HUDObjects,
  GLS.Coordinates,
  GLS.VectorGeometry,
  GLS.FileMD2,
  GLS.File3DS,
  GLS.Context,
  GLS.EllipseCollision,
  GLS.RenderContextInfo,
  GLS.Keyboard,
  GLS.ProxyObjects,
  GLS.State,
  GLS.Utils,
  GLS.BaseClasses,
  GLS.VectorTypes;

type
  TFormDCE = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    Player: TGLDummyCube;
    GLDCEManager1: TGLDCEManager;
    Terrain: TGLTerrainRenderer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLMatlLib: TGLMaterialLibrary;
    GLLightSource1: TGLLightSource;
    GLActor1: TGLActor;
    GLSphere1: TGLSphere;
    GLLightSource2: TGLLightSource;
    Balls: TGLDummyCube;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    Timer1: TTimer;
    GLHUDText1: TGLHUDText;
    Mushrooms: TGLDummyCube;
    moMushroom: TGLFreeForm;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLCube1: TGLCube;
    Help: TGLHUDText;
    HelpShadow: TGLHUDText;
    Ground: TGLPlane;
    procedure FormShow(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PlayerBehaviours0Collision(Sender: TObject;
      ObjectCollided: TGLBaseSceneObject; CollisionInfo: TDCECollision);
    procedure Timer1Timer(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
  public
    mx, my: Integer;
    Jumped: boolean;
    procedure Load;
    procedure HandleKeys;
    procedure HandleAnimation;
    procedure AddBall;
    procedure AddMushrooms;
  end;

var
  FormDCE: TFormDCE;

const
  cForce: Single = 250;
  cSpread = 200;
  cNbMushrooms = 20;

implementation

{$R *.dfm}
{ TForm1 }

procedure TFormDCE.Load;
begin
  var Path: TFileName := GetCurrentAssetPath();

  // Load texture material for terrain
  SetCurrentDir(Path  + '\texture');
  GLMatlLib.AddTextureMaterial('Terrain', 'snow512.jpg');

  // Load Terrain
  GLBitmapHDS1.MaxPoolSize := 8 * 1024 * 1024;
  GLBitmapHDS1.Picture.LoadFromFile('terrain.bmp');
  Terrain.Direction.SetVector(0, 1, 0);
  //Terrain.Material.LibMaterialName := 'Terrain';
  Terrain.TilesPerTexture := 256 / Terrain.TileSize;
  Terrain.Scale.SetVector(1, 1, 0.02);

  Ground.Material.LibMaterialName := 'Terrain';

  // Load static models
  SetCurrentDir(Path + '\model');
  // Mushroom mesh
  // Always use AutoScaling property or you may get some problems
  moMushroom.AutoScaling.SetPoint(0.1, 0.1, 0.1);
  moMushroom.LoadFromFile('Mushroom.3ds');
  moMushroom.Direction.SetVector(0, 1, 0);
  moMushroom.BuildOctree;

  // Load player
  Player.Position.SetPoint(0, 3, 0);

  // Load dyn modelexts with textures and animations
  SetCurrentDir(Path + '\modelext');
  GLMatlLib.AddTextureMaterial('Actor', 'waste.jpg');
  GLActor1.LoadFromFile('Waste.md2');
  GLActor1.Direction.SetVector(0, 1, 0);
  GLActor1.Up.SetVector(1, 0, 0);
  GLActor1.Scale.SetVector(0.05, 0.05, 0.05);
  GLActor1.Material.LibMaterialName := 'Actor';
  GLActor1.Animations.LoadFromFile('Quake2Animations.aaf');
  // Define animation properties
  GLActor1.AnimationMode := aamLoop;
  GLActor1.SwitchToAnimation('stand');
  GLActor1.FrameInterpolation := afpLinear;

  // DCE Behaviour
  GLSphere1.Scale.Assign(GetOrCreateDCEDynamic(Player).Size);
  GetOrCreateDCEDynamic(Player).OnCollision := PlayerBehaviours0Collision;
end;

procedure TFormDCE.HandleKeys;
var
  Force: TAffineVector;
begin

  Force := NullVector;
  if IsKeyDown('w') or IsKeyDown('z') then
    Force.Z := cForce;
  if IsKeyDown('s') then
    Force.Z := -cForce;
  if IsKeyDown('a') or IsKeyDown('q') then
    Force.X := cForce;
  if IsKeyDown('d') then
    Force.X := -cForce;

  GetOrCreateDCEDynamic(Player).ApplyAccel(Force);
end;

procedure TFormDCE.HandleAnimation;
var
  anim: string;
begin
  if VectorNorm(GetOrCreateDCEDynamic(Player).Speed) > 0.1 then
    anim := 'run'
  else
    anim := 'stand';

  if Jumped then
  begin
    if (not GetOrCreateDCEDynamic(Player).InGround) then
      anim := 'jump'
    else
      Jumped := False;
  end;

  if anim = 'jump' then
    GLActor1.Interval := 500
  else
    GLActor1.Interval := 100;

  if GLActor1.CurrentAnimation <> anim then
    GLActor1.SwitchToAnimation(anim);
end;

procedure TFormDCE.AddBall;
var
  Ball: TGLSphere;
  S: Single;
begin
  Ball := TGLSphere(Balls.AddNewChild(TGLSphere));
  with Ball do
  begin
    Tag := 1; // set the identifier of a ball
    Radius := 1;
    S := (100 + Random(900)) / 500;
    Scale.SetVector(S, S, S);
    Position.SetPoint(Random(40) - Random(40), 4 + Random(10),
      Random(40) - Random(40));
    Material.FrontProperties.Diffuse.SetColor((100 + Random(900)) / 1000,
      (100 + Random(900)) / 1000, (100 + Random(900)) / 1000);
  end;
  with GetOrCreateDCEDynamic(Ball) do
  begin
    Manager := GLDCEManager1;
    BounceFactor := 0.75;
    Friction := 0.1;
    SlideOrBounce := csbBounce;
    Size.Assign(Ball.Scale);
  end;
end;

procedure TFormDCE.AddMushrooms;
var
  i: Integer;
  proxy: TGLFreeFormProxy;
  S: TGLVector;
  f: Single;
begin
  // spawn some more mushrooms using proxy objects
  for i := 0 to cNbMushrooms - 1 do
  begin
    // create a new proxy and set its MasterObject property
    proxy := TGLFreeFormProxy(Mushrooms.AddNewChild(TGLFreeFormProxy));
    with proxy do
    begin
      ProxyOptions := [pooObjects];
      MasterObject := moMushroom;
      // retrieve reference attitude
      Direction := moMushroom.Direction;
      Up := moMushroom.Up;
      // randomize scale
      S := moMushroom.Scale.AsVector;
      f := (2 * Random + 1);
      ScaleVector(S, f);
      Scale.AsVector := S;
      // randomize position
      Position.SetPoint(Random(cSpread) - (cSpread / 2), moMushroom.Position.Z +
        1.5 * f, Random(cSpread) - (cSpread / 2));
      // randomize orientation
      RollAngle := Random(360);
      TransformationChanged;
    end;
    with GetOrCreateDCEStatic(proxy) do
    begin
      Manager := GLDCEManager1;
      BounceFactor := 0.75;
      Friction := 10;
      Shape := csFreeform;
    end;

  end;
end;

procedure TFormDCE.FormShow(Sender: TObject);
begin
  Load;
  GLCadencer1.Enabled := true;
  Help.Text := 'Mouse Drag - Look' + #13 + 'A,W,S,D - movement' + #13 +
    'SPACE - Jump' + #13 + 'F1 - Add one ball' + #13 + 'F2 - Add 10 balls' + #13
    + 'F3 - Add 20 mushrooms' + #13 + 'F4 - Change ground to box' + #13 +
    'F5 - Toggle step mode' + #13 + 'RETURN - Reset';
end;

procedure TFormDCE.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  // Mouse look
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget((my - Y), 0);
    Player.Turn(-(mx - X));
  end;
  mx := X;
  my := Y;
end;

procedure TFormDCE.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  HandleKeys;
  HandleAnimation;
  // This shows the manual progress, don't need this if you use the automatic mode
  if GLDCEManager1.ManualStep then
    GLDCEManager1.Step(deltaTime);

  Help.ModulateColor.Alpha := Help.ModulateColor.Alpha - (deltaTime * 0.05);
  if Help.ModulateColor.Alpha < 0.25 then
    Help.ModulateColor.Alpha := 0.25;
  HelpShadow.ModulateColor.Alpha := Help.ModulateColor.Alpha;
  HelpShadow.Text := Help.Text;
end;

procedure TFormDCE.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  if Key = VK_F1 then
    AddBall;
  if Key = VK_F2 then
    for i := 1 to 10 do
      AddBall;
  if Key = VK_F3 then
    AddMushrooms;
  if (Key = VK_Space) then
  begin
    GetOrCreateDCEDynamic(Player).Jump(1, 20);
    Jumped := true;
  end;
  if Key = VK_F4 then
  begin
    Terrain.Visible := False;
    Ground.Visible := true;
    GetOrCreateDCEStatic(Terrain).Active := False;
    GetOrCreateDCEStatic(Ground).Active := true;
  end;
  if Key = VK_F5 then
    GLDCEManager1.ManualStep := not GLDCEManager1.ManualStep;

  if (Key = VK_RETURN) then
  begin
    Player.Position.SetPoint(0, 3, 0);
    Balls.DeleteChildren;
    Mushrooms.DeleteChildren;
    Help.ModulateColor.Alpha := 1;
    Terrain.Visible := true;
    Ground.Visible := False;
    GetOrCreateDCEStatic(Terrain).Active := true;
    GetOrCreateDCEStatic(Ground).Active := False;
  end;
end;

procedure TFormDCE.PlayerBehaviours0Collision(Sender: TObject;
  ObjectCollided: TGLBaseSceneObject; CollisionInfo: TDCECollision);
var
  v: TAffineVector;
begin
  // Use some kind of identifier to know what object you are colliding
  // You can use the Tag, TagFloat, Name, Class
  if ObjectCollided.Tag = 1 then
  begin
    v := AffineVectorMake(VectorSubtract(ObjectCollided.AbsolutePosition,
      Player.AbsolutePosition));
    NormalizeVector(v);
    ScaleVector(v, 400);
    GetOrCreateDCEDynamic(ObjectCollided).StopAbsAccel;
    GetOrCreateDCEDynamic(ObjectCollided).ApplyAbsAccel(v);
  end;
end;

procedure TFormDCE.Timer1Timer(Sender: TObject);
var
  S: string;
begin
  if GLDCEManager1.ManualStep then
    S := 'Manual'
  else
    S := 'Automatic';
  GLHUDText1.Text :=
    Format('FPS: %.1f - Dynamics: %d - Statics: %d - Step mode: %s',
    [GLSceneViewer1.FramesPerSecond, GLDCEManager1.DynamicCount,
    GLDCEManager1.StaticCount, S]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormDCE.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  i: Integer;
  p, n: TAffineVector;
begin
  // To use this you will need to enable the debug define in the
  // GLEllipseCollision.pas, if you do, don't forget to clear the
  // triangle list! -> SetLength(debug_tri,0);

  rci.GLStates.PointSize := 5.0;
  glColor3f(0, 1, 0);

  for i := 0 to High(debug_tri) do
    with debug_tri[i] do
    begin
      glColor3f(0, 0, 0);
      glBegin(GL_LINE_STRIP);
      glVertex3f(p1.X, p1.Y, p1.Z);
      glVertex3f(p2.X, p2.Y, p2.Z);
      glVertex3f(p3.X, p3.Y, p3.Z);
      glEnd;
      CalcPlaneNormal(p1, p2, p3, n);
      ScaleVector(n, 0.25);
      p.X := (p1.X + p2.X + p3.X) / 3;
      p.Y := (p1.Y + p2.Y + p3.Y) / 3;
      p.Z := (p1.Z + p2.Z + p3.Z) / 3;
      glColor3f(0, 0, 1);
      glBegin(GL_LINE_STRIP);
      glVertex3f(p.X, p.Y, p.Z);
      glVertex3f(p.X + n.X, p.Y + n.Y, p.Z + n.Z);
      glEnd;
      glBegin(GL_POINTS);
      glVertex3f(p.X + n.X, p.Y + n.Y, p.Z + n.Z);
      glEnd;

    end; // }

  SetLength(debug_tri, 0);
end;

end.
