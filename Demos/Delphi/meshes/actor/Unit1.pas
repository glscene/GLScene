unit Unit1;

interface

uses
  Winapi.OpenGL,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,
  Vcl.ComCtrls,
  Vcl.Forms,

  
  GLCadencer,
  GLVectorFileObjects,
  GLScene,
  GLObjects,
  Scene.VectorGeometry,
  GLSceneViewer,
  GLFileMD2,
  GLGeomObjects,
  GLCrossPlatform,
  GLCoordinates,
  GLBaseClasses,
  GLS.Utils;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    Disk1: TGLDisk;
    GLSceneViewer1: TGLSceneViewer;
    Actor1: TGLActor;
    Actor2: TGLActor;
    GLCadencer1: TGLCadencer;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    SBPlay: TSpeedButton;
    SBStop: TSpeedButton;
    CBAnimations: TComboBox;
    BBLoadWeapon: TBitBtn;
    SBFrameToFrame: TSpeedButton;
    Label1: TLabel;
    CBSmooth: TCheckBox;
    Timer1: TTimer;
    LabelFPS: TLabel;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SBPlayClick(Sender: TObject);
    procedure SBStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BBLoadWeaponClick(Sender: TObject);
    procedure CBAnimationsChange(Sender: TObject);
    procedure SBFrameToFrameClick(Sender: TObject);
    procedure Actor1FrameChanged(Sender: TObject);
    procedure CBSmoothClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
  private
     
    mdx, mdy: Integer;
  public
     
  end;

var
  Form1: TForm1;
  i: Integer;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
   // Load Texture for ground disk
  Disk1.Material.Texture.Image.LoadFromFile('clover.jpg');

  // Load Actor into GLScene
  Actor1.LoadFromFile('waste.md2');
  Actor1.Material.Texture.Image.LoadFromFile('waste.jpg');

  // Load Quake2 animations defaults, for "waste.md2", this is not required
  // since the author did not renamed the frames, and thus, GLScene can
  // recover them from the .MD2, but other authors just made a mess...
  // Loading the default animations takes care of that
  Actor1.Animations.LoadFromFile('Quake2Animations.aaf');

  // Scale Actor for put in the Scene
  Actor1.Scale.SetVector(0.04, 0.04, 0.04, 0);

  // Send animation names to the combo, to allow user selection
  Actor1.Animations.SetToStrings(CBAnimations.Items);
  // Force state to stand (first in list)
  CBAnimations.ItemIndex := 0;
  CBAnimationsChange(Self);

end;

procedure TForm1.SBPlayClick(Sender: TObject);
begin
  // start playing
  Actor1.AnimationMode := aamLoop;
  Actor2.AnimationMode := aamLoop;

  // update buttons
  SBPlay.Enabled := False;
  SBStop.Enabled := True;
  SBFrameToFrame.Enabled := False;
end;

procedure TForm1.SBStopClick(Sender: TObject);
begin
  // stop playing
  Actor1.AnimationMode := aamNone;
  Actor2.AnimationMode := aamNone;

  // update buttons
  SBPlay.Enabled := True;
  SBStop.Enabled := False;
  SBFrameToFrame.Enabled := True;
end;

procedure TForm1.BBLoadWeaponClick(Sender: TObject);
begin
  // Load weapon model and texture
  Actor2.LoadFromFile('WeaponWaste.md2');
  Actor2.Material.Texture.Image.LoadFromFile('WeaponWaste.jpg');

  // Get animations frames from the main actor
  Actor2.Animations.Assign(Actor1.Animations);

  // Synch both actors
  Actor2.Synchronize(Actor1);
end;

procedure TForm1.CBAnimationsChange(Sender: TObject);
begin
  // Change animation
  Actor1.SwitchToAnimation(CBAnimations.Text, True);

  // Normally actors for Quake II Model have one number of frames
  // for all states 198 for actors and 172 for weapon,
  // frames 173 to 198 are for death
  // I use this for Hide and show weapon.
  Actor2.Visible := (Actor1.NextFrameIndex < 173);
  if Actor2.Visible then
    Actor2.Synchronize(Actor1);
end;

procedure TForm1.SBFrameToFrameClick(Sender: TObject);
begin
  // Animate Frame to Frame
  Actor1.NextFrame;
  Actor2.NextFrame;
end;

procedure TForm1.CBSmoothClick(Sender: TObject);
begin
  // Smooth movement is achieved by using linear frame interpolation
  if CBSmooth.Checked then
  begin
    Actor1.FrameInterpolation := afpLinear;
    Actor2.FrameInterpolation := afpLinear;
  end
  else
  begin
    Actor1.FrameInterpolation := afpNone;
    Actor2.FrameInterpolation := afpNone;
  end;
end;

procedure TForm1.Actor1FrameChanged(Sender: TObject);
begin
  StatusBar1.SimpleText := 'CurrentFrame = ' + IntToStr(Actor1.CurrentFrame);
end;

//
// events that follow handle camera movements and FPS rate
//

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // store mouse coordinates when a button went down
  mdx := X;
  mdy := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  // (we're moving around the parent and target dummycube)
  if Shift <> [] then
    GLCamera1.MoveAroundTarget(mdy - Y, mdx - X);
  mdx := X;
  mdy := Y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // Note that 1 wheel-step induces a WheelDelta of 120,
  // this code adjusts the distance to target with a 10% per wheel-step ratio
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  LabelFPS.Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

end.
