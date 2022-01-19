unit fActorTwocamD;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Vcl.Imaging.Jpeg,
  Vcl.Buttons,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Forms,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.Objects,
  GLS.Cadencer,
  GLS.VectorFileObjects,
  StdCtrls,
  Graphics,
  GLS.SkyDome,
  GLS.SceneViewer,
  GLS.Navigator,
  GLS.FileMD2,
  GLS.File3DS,
  GLS.GeomObjects,

  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Utils,
  GLS.VectorGeometry,
  GLS.Keyboard,
  GLS.PersistentClasses, GLS.DCE;

type
  TFormActorTwocam = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    dcMushroom: TGLDummyCube;
    diskClover: TGLDisk;
    GLSceneViewer1: TGLSceneViewer;
    Actor1: TGLActor;
    Actor2: TGLActor;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    Timer1: TTimer;
    GLCamera2: TGLCamera;
    Label3: TLabel;
    Label4: TLabel;
    DummyCube2: TGLDummyCube;
    ffMushroom: TGLFreeForm;
    GLLightSource2: TGLLightSource;
    DummyCube3: TGLDummyCube;
    Label1: TLabel;
    SkyDome1: TGLSkyDome;
    GLNavigator1: TGLNavigator;
    GLUserInterface1: TGLUserInterface;
    CBMouseLook: TCheckBox;
    GLDCEManager1: TGLDCEManager;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure CBMouseLookClick(Sender: TObject);
  private
    procedure AddMushrooms;
    procedure HandleKeys(const deltaTime: Double);
  end;

var
  FormActorTwocam: TFormActorTwocam;

implementation

{$R *.DFM}

const
  cWalkStep = 6; // this is our walking speed, in 3D units / second
  cStrafeStep = 6; // this is our strafing speed, in 3D units / second
  cRotAngle = 60; // this is our turning speed, in degrees / second
  cRunBoost = 2; // speed boost when running
  cSpread = 90;
  cNbMushrooms = 15;

procedure TFormActorTwocam.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  // Load mushroom mesh
  ffMushroom.LoadFromFile('mushroom.3ds');

  // Duplicate our reference mushroom (but not its mesh data !)
  AddMushrooms;

  // Load Actor into GLScene
  Actor1.LoadFromFile('waste.md2');
  Actor1.Material.Texture.Image.LoadFromFile('waste.jpg');
  Actor1.Animations.LoadFromFile('Quake2Animations.aaf');
  Actor1.Scale.SetVector(0.04, 0.04, 0.04, 0);
  // Load weapon model and texture
  Actor2.LoadFromFile('WeaponWaste.md2');
  Actor2.Material.Texture.Image.LoadFromFile('WeaponWaste.jpg');
  Actor2.Animations.Assign(Actor1.Animations);

  // Define animation properties
  Actor1.AnimationMode := aamLoop;
  Actor1.SwitchToAnimation('stand');
  Actor1.FrameInterpolation := afpLinear;
  Actor2.Synchronize(Actor1);

  // Load Texture for ground disk
  diskClover.Material.Texture.Disabled := False;
  diskClover.Material.Texture.Image.LoadFromFile('clover.jpg');
end;

procedure TFormActorTwocam.CBMouseLookClick(Sender: TObject);
begin
  GLUserInterface1.MouseLookActive := CBMouseLook.Checked;
end;

procedure TFormActorTwocam.HandleKeys(const deltaTime: Double);
var
  moving: String;
  boost: Single;
begin
  // This function uses asynchronous keyboard check (see Keyboard.pas)
  if IsKeyDown(VK_ESCAPE) then
    Close;
  if IsKeyDown('A') then
  begin
    CBMouseLook.Checked := True;
    CBMouseLookClick(Self);
  end;
  if IsKeyDown('D') then
  begin
    CBMouseLook.Checked := False;
    CBMouseLookClick(Self);
  end;

  // Change Cameras
  if IsKeyDown(VK_F7) then
  begin
    GLSceneViewer1.Camera := GLCamera1;
    Actor1.Visible := True;
    Label4.Font.Style := Label4.Font.Style - [fsBold];
    Label3.Font.Style := Label3.Font.Style + [fsBold];
  end;
  if IsKeyDown(VK_F8) then
  begin
    GLSceneViewer1.Camera := GLCamera2;
    Actor1.Visible := False;
    Label4.Font.Style := Label4.Font.Style + [fsBold];
    Label3.Font.Style := Label3.Font.Style - [fsBold];
  end;

  // Move Actor in the scene

  // if nothing specified, we are standing
  moving := 'stand';

  // first, are we running ? if yes give animation & speed a boost
  if IsKeyDown(VK_SHIFT) then
  begin
    Actor1.Interval := 100;
    boost := cRunBoost * deltaTime
  end
  else
  begin
    Actor1.Interval := 150;
    boost := deltaTime;
  end;
  Actor2.Interval := Actor1.Interval;

  // are we advaning/backpedaling ?
  if IsKeyDown(VK_UP) then
  begin
    GLNavigator1.MoveForward(cWalkStep * boost);
    moving := 'run';
  end;
  if IsKeyDown(VK_DOWN) then
  begin
    GLNavigator1.MoveForward(-cWalkStep * boost);
    moving := 'run';
  end;

  // slightly more complex, depending on CTRL key, we either turn or strafe
  if IsKeyDown(VK_LEFT) then
  begin
    if IsKeyDown(VK_CONTROL) then
      GLNavigator1.StrafeHorizontal(-cStrafeStep * boost)
    else
      GLNavigator1.TurnHorizontal(-cRotAngle * boost);
    moving := 'run';
  end;
  if IsKeyDown(VK_RIGHT) then
  begin
    if IsKeyDown(VK_CONTROL) then
      GLNavigator1.StrafeHorizontal(cStrafeStep * boost)
    else
      GLNavigator1.TurnHorizontal(cRotAngle * boost);
    moving := 'run';
  end;

  // update animation (if required)
  // you can use faster methods (such as storing the last value of "moving")
  // but this ones shows off the brand new "CurrentAnimation" function :)
  if Actor1.CurrentAnimation <> moving then
  begin
    Actor1.SwitchToAnimation(moving);
    Actor2.Synchronize(Actor1);
  end;
end;

procedure TFormActorTwocam.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  HandleKeys(deltaTime);
  GLUserInterface1.MouseLook;

  GLSceneViewer1.Invalidate;
  GLUserInterface1.MouseUpdate;
end;

// add a few mushrooms to make the "landscape"

procedure TFormActorTwocam.AddMushrooms;
var
  i: Integer;
  proxy: TGLProxyObject;
  s: TGLVector;
  f: Single;
begin
  // spawn some more mushrooms using proxy objects
  for i := 0 to cNbMushrooms - 1 do
  begin
    // create a new proxy and set its MasterObject property
    proxy := TGLProxyObject(dcMushroom.AddNewChild(TGLProxyObject));
    proxy.ProxyOptions := [pooObjects];
    proxy.MasterObject := ffMushroom;
    // retrieve reference attitude
    proxy.Direction := ffMushroom.Direction;
    proxy.Up := ffMushroom.Up;
    // randomize scale
    s := ffMushroom.Scale.AsVector;
    f := (1 * Random + 1);
    ScaleVector(s, f);
    proxy.Scale.AsVector := s;
    // randomize position
    proxy.Position.SetPoint(Random(cSpread) - (cSpread / 2),
      ffMushroom.Position.z + 0.8 * f, Random(cSpread) - (cSpread / 2));
    // randomize orientation
    proxy.RollAngle := Random(360);
    proxy.TransformationChanged;
  end;
end;

procedure TFormActorTwocam.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
