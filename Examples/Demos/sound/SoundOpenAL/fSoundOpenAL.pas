unit fSoundOpenAL;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.StdCtrls,

  GLS.Cadencer,
  GLS.Scene,
  Stage.VectorTypes,
  GLS.Objects,
  GLS.SoundManager,
  GLS.SceneViewer,
  GLS.GeomObjects,
  GLS.Sounds.OpenAL,
  Stage.VectorGeometry,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.FileWAV,
  GLS.FileMP3,
  Stage.Utils;

type
  TForm1 = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube: TGLDummyCube;
    GLLightSource: TGLLightSource;
    GLSMOpenAL: TGLSMOpenAL;
    GLSoundLibrary: TGLSoundLibrary;
    GLCadencer1: TGLCadencer;
    Timer: TTimer;
    Mickey: TGLSphere;
    SphereSound: TGLSphere;
    Sphere2: TGLSphere;
    Sphere3: TGLSphere;
    Cone1: TGLCone;
    TrackBar: TTrackBar;
    Plane1: TGLPlane;
    Disk1: TGLDisk;
    Torus1: TGLTorus;
    TrackBar1: TTrackBar;
    Panel1: TPanel;
    btnChimes: TButton;
    btnHowl: TButton;
    LabelFPS: TLabel;
    procedure SphereSoundProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure TimerTimer(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnChimesClick(Sender: TObject);
    procedure btnHowlClick(Sender: TObject);
  private
    Path: TFileName;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Path := GetCurrentAssetPath() + '\audio';
  SetCurrentDir(Path);
  // Load our sound sample
  GLSoundLibrary.Samples.AddFile('drumloop.wav', 'drumloop.wav');
  GLSoundLibrary.Samples.AddFile('chimes.wav', 'chimes.wav');
  GLSoundLibrary.Samples.AddFile('howl.mp3', 'howl.mp3');
end;

procedure TForm1.SphereSoundProgress(Sender: TObject; const deltaTime, newTime: Double);
var
  alpha: Single;
begin
  // Move the red sphere (sound source) along an elliptic path
  alpha := 60 * DegToRad(newTime);
  TGLSphere(Sender).Position.SetPoint(sin(alpha) * 2, 0.5, cos(alpha) * 5);
end;

procedure TForm1.TrackBarChange(Sender: TObject);
begin
  // Rotate the listener around the vertical axis
  DummyCube.TurnAngle := TrackBar.Position;
  Application.ProcessMessages;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  // Move the listener forward/back
  Mickey.Position.Z := TrackBar1.Position / 10;
  Application.ProcessMessages;
end;

procedure TForm1.TimerTimer(Sender: TObject);
var
  mngName: String;
begin
  // some stats
  if ActiveSoundManager is TGLSMOpenAL then
    mngName := 'OpenAL'
  else
    mngName := '';
  if ActiveSoundManager <> nil then
    LabelFPS.Caption := Format('%.2f FPS, %s CPU use : %.2f%%',
      [GLSceneViewer.FramesPerSecond, mngName, ActiveSoundManager.CPUUsagePercent])
  else
    LabelFPS.Caption := 'No active sound manager.';
  GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TForm1.btnChimesClick(Sender: TObject);
begin
  with TGLBSoundEmitter.Create(SphereSound.Behaviours) do
  begin
    Source.SoundLibrary := GLSoundLibrary;
    Source.SoundName := 'chimes.wav';
    Playing := True;
  end;
end;

procedure TForm1.btnHowlClick(Sender: TObject);
begin
  with TGLBSoundEmitter.Create(SphereSound.Behaviours) do
  begin
    Source.SoundLibrary := GLSoundLibrary;
    Source.SoundName := 'howl.mp3';
    Playing := True;
  end;
end;

end.
