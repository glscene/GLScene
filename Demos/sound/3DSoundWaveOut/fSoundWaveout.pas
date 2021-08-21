unit fSoundWaveout;

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
  GLS.Objects,
  GLS.Scene,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Sound,
  Sounds.WaveOut,
  GLS.SceneViewer,
  GLS.GeomObjects,

  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.FileWAV,
  GLS.FileMP3,
  GLS.Utils;

type
  TFormSoundWaveout = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube: TGLDummyCube;
    Sphere: TGLSphere;
    GLLightSource: TGLLightSource;
    GLSoundLibrary: TGLSoundLibrary;
    GLCadencer1: TGLCadencer;
    Timer: TTimer;
    Mickey: TGLSphere;
    Sphere2: TGLSphere;
    Sphere3: TGLSphere;
    Cone1: TGLCone;
    TrackBar: TTrackBar;
    Plane1: TGLPlane;
    Disk1: TGLDisk;
    Torus1: TGLTorus;
    TrackBar1: TTrackBar;
    GLSMWaveOut: TGLSMWaveOut;
    Panel1: TPanel;
    Button1: TButton;
    btnHowl: TButton;
    LabelFPS: TLabel;
    procedure SphereProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure TimerTimer(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnHowlClick(Sender: TObject);
  private

  public

  end;

var
  FormSoundWaveout: TFormSoundWaveout;

implementation

{$R *.DFM}

procedure TFormSoundWaveout.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  // Load our sound sample
  GLSoundLibrary.Samples.AddFile('drumloop.wav', 'drumloop.wav');
  GLSoundLibrary.Samples.AddFile('chimes.wav', 'chimes.wav');
  GLSoundLibrary.Samples.AddFile('howl.mp3', 'howl.mp3');
end;

procedure TFormSoundWaveout.SphereProgress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  alpha: Single;
begin
  // Move the red sphere (sound source) along an elliptic path
  alpha := 60 * DegToRad(newTime);
  TGLSphere(Sender).Position.SetPoint(sin(alpha) * 2, 0.5, cos(alpha) * 5);
end;

procedure TFormSoundWaveout.TrackBarChange(Sender: TObject);
begin
  // Rotate the listener around the vertical axis
  DummyCube.TurnAngle := TrackBar.Position;
  Application.ProcessMessages;
end;

procedure TFormSoundWaveout.TrackBar1Change(Sender: TObject);
begin
  // Move the listener forward/back
  Mickey.Position.Z := TrackBar1.Position / 10;
  Application.ProcessMessages;
end;

procedure TFormSoundWaveout.TimerTimer(Sender: TObject);
var
  mngName: String;
begin
  // some stats
  if ActiveSoundManager is TGLSMWaveOut then
    mngName := 'WaveOut'
  else
    mngName := '';
  if ActiveSoundManager <> nil then
    LabelFPS.Caption := Format('%.2f FPS, %s CPU use : %.2f%%',
      [GLSceneViewer.FramesPerSecond, mngName,
      ActiveSoundManager.CPUUsagePercent])
  else
    LabelFPS.Caption := 'No active sound manager.';
  GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TFormSoundWaveout.Button1Click(Sender: TObject);
begin
  with TGLBSoundEmitter.Create(Sphere.Behaviours) do
  begin
    Source.SoundLibrary := GLSoundLibrary;
    Source.SoundName := 'chimes.wav';
    Playing := True;
  end;
end;

procedure TFormSoundWaveout.btnHowlClick(Sender: TObject);
begin
  with TGLBSoundEmitter.Create(Sphere.Behaviours) do
  begin
    Source.SoundLibrary := GLSoundLibrary;
    Source.SoundName := 'howl.mp3';
    Playing := True;
  end;
end;

end.
