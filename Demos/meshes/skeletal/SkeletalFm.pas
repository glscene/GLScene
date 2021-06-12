unit SkeletalFm;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,

  GLS.VectorFileObjects,
  GLS.Scene,
  GLS.Objects,
  GLS.Texture,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.Graph,
  GLS.FileSMD,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.VectorGeometry,
  GLS.Utils;

type
  TFormSkeletal = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Actor1: TGLActor;
    DummyCube1: TGLDummyCube;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    BULongJump: TButton;
    CheckBox1: TCheckBox;
    LabelFPS: TLabel;
    BUHighJump: TButton;
    XYZGrid1: TGLXYZGrid;
    RBWalk: TRadioButton;
    RBRun: TRadioButton;
    AnimationControler1: TGLAnimationControler;
    Panel2: TPanel;
    TrackBar1: TTrackBar;
    CBBlend: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure BULongJumpClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Actor1EndFrameReached(Sender: TObject);
    procedure BUHighJumpClick(Sender: TObject);
    procedure RBWalkClick(Sender: TObject);
    procedure RBRunClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CBBlendClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
     
  public
     
    baseAnimation : String;
    mx, my : Integer;
  end;

var
  FormSkeletal: TFormSkeletal;

implementation

{$R *.DFM}

procedure TFormSkeletal.FormCreate(Sender: TObject);
begin
   SetGLSceneMediaDir();
   // We load the SMD model here
   // Note the actor was linked to a material library, and textures are loaded
   // automatically (4 textures are used by this model)
   //
   // Kind thanks to ~A.u.s.t.i.n. & Neal 'Guplik' Corbett for the model
   // and allowing its use ;)
   Actor1.LoadFromFile('trinityRage.smd');
   // Now we load the walk & run animations and "fix" their translation
   // (HL walk/run animations have a built-in "slide" that we don't want here)
   Actor1.AddDataFromFile('walk.smd');
   Actor1.Animations[1].MakeSkeletalTranslationStatic;
   Actor1.AddDataFromFile('run.smd');
   Actor1.Animations[2].MakeSkeletalTranslationStatic;
   // Then load the two jumps
   Actor1.AddDataFromFile('long_jump.smd');
   Actor1.AddDataFromFile('jump.smd');
   // And the 'look_left_right' blending animations, that we immediately
   // assign to the controler. The MakeSkeletalRotationDelta removes absolute
   // information from the SMD (which HL may use, but GLScene doesn't)
   Actor1.AddDataFromFile('look_left_right.smd');
   Actor1.Animations[5].MakeSkeletalRotationDelta;
   AnimationControler1.AnimationName:='look_left_right';
   // Skeleton visible, and start with walk animation
   // (pseudo-animation 0 is for the static model in its default attitude)
   Actor1.OverlaySkeleton:=True;
   baseAnimation:='walk';
   Actor1.SwitchToAnimation(baseAnimation);
end;

procedure TFormSkeletal.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1 := GLSceneViewer1.Camera;
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TFormSkeletal.RBWalkClick(Sender: TObject);
begin
   // user requested 'walk'
   baseAnimation:='walk';
   Actor1.SwitchToAnimation(baseAnimation, True);
end;

procedure TFormSkeletal.RBRunClick(Sender: TObject);
begin
   // user requested 'run'
   baseAnimation:='run';
   Actor1.SwitchToAnimation(baseAnimation, True);
end;

procedure TFormSkeletal.BULongJumpClick(Sender: TObject);
begin
   // Smoothly switch to Long Jump
   Actor1.SwitchToAnimation(3, True);
end;

procedure TFormSkeletal.BUHighJumpClick(Sender: TObject);
begin
   // Smoothly switch to High Jump
   Actor1.SwitchToAnimation(4, True);
end;

procedure TFormSkeletal.Actor1EndFrameReached(Sender: TObject);
begin
   // If we weren't walking, switch back to walk
   if Actor1.CurrentAnimation<>baseAnimation then
      Actor1.SwitchToAnimation(baseAnimation, True);
end;

procedure TFormSkeletal.CBBlendClick(Sender: TObject);
begin
   // Enable/disable blending by binding or unbinding the animation controler
   // to the actor 
   if CBBlend.Checked then begin
      AnimationControler1.Actor:=Actor1;
      TrackBar1Change(Self);
   end else AnimationControler1.Actor:=nil;
end;

procedure TFormSkeletal.TrackBar1Change(Sender: TObject);
begin
   // Blending along the controler's animation is just a matter of adjusting
   // the ratio, with 0 = first frame and 1 = last frame.
   AnimationControler1.Ratio:=TrackBar1.Position*0.01;
end;

// Nothing fancy below, just the same old stuff

procedure TFormSkeletal.CheckBox1Click(Sender: TObject);
begin
   Actor1.OverlaySkeleton:=CheckBox1.Checked;
end;

procedure TFormSkeletal.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TFormSkeletal.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x; my:=y;
   end;
end;

procedure TFormSkeletal.Timer1Timer(Sender: TObject);
begin
   LabelFPS.Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormSkeletal.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLScene1.NotifyChange(nil);
end;

end.
