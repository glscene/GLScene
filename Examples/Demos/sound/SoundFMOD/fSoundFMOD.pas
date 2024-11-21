unit fSoundFMOD;

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
  GLS.Sounds.FMOD,
  GLS.SceneViewer,
  GLS.GeomObjects,

  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.FileWAV,
  GLS.FileMP3,
  Stage.VectorGeometry,
  Stage.Utils;

type
  TFormSoundFMOD = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube: TGLDummyCube;
    Sphere: TGLSphere;
    GLLightSource: TGLLightSource;
    GLSMFMOD: TGLSMFMOD;
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
    Path: TFileName;
  public

  end;

var
  FormSoundFMOD: TFormSoundFMOD;

implementation

{$R *.DFM}

procedure TFormSoundFMOD.FormCreate(Sender: TObject);
begin
  Path := GetCurrentAssetPath()  + '\audio';
  SetCurrentDir(Path);

  // Load our sound sample
  GLSoundLibrary.Samples.AddFile('drumloop.wav', 'drumloop.wav');
  GLSoundLibrary.Samples.AddFile('chimes.wav', 'chimes.wav');
  GLSoundLibrary.Samples.AddFile('howl.mp3', 'howl.mp3');
end;

procedure TFormSoundFMOD.SphereProgress(Sender: TObject; const deltaTime, newTime: Double);
var
  alpha: Single;
begin
  // Move the red sphere (sound source) along an elliptic path
  alpha := 60 * DegToRad(newTime);
  TGLSphere(Sender).Position.SetPoint(sin(alpha) * 2, 0.5, cos(alpha) * 5);
end;

procedure TFormSoundFMOD.TrackBarChange(Sender: TObject);
begin
  // Rotate the listener around the vertical axis
  DummyCube.TurnAngle := TrackBar.Position;
  Application.ProcessMessages;
end;

procedure TFormSoundFMOD.TrackBar1Change(Sender: TObject);
begin
  // Move the listener forward/back
  Mickey.Position.Z := TrackBar1.Position / 10;
  Application.ProcessMessages;
end;

procedure TFormSoundFMOD.TimerTimer(Sender: TObject);
var
  mngName: String;
begin
  // some stats
  if ActiveSoundManager is TGLSMFMOD then
    mngName := 'FMOD'
  else
    mngName := '';
  if ActiveSoundManager <> nil then
    LabelFPS.Caption := Format('%.2f FPS, %s CPU use : %.2f%%',
      [GLSceneViewer.FramesPerSecond, mngName, ActiveSoundManager.CPUUsagePercent])
  else
    LabelFPS.Caption := 'No active sound manager.';
  GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TFormSoundFMOD.Button1Click(Sender: TObject);
begin
  with TGLBSoundEmitter.Create(Sphere.Behaviours) do
  begin
    Source.SoundLibrary := GLSoundLibrary;
    Source.SoundName := 'chimes.wav';
    Playing := True;
  end;
end;

procedure TFormSoundFMOD.btnHowlClick(Sender: TObject);
begin
  with TGLBSoundEmitter.Create(Sphere.Behaviours) do
  begin
    Source.SoundLibrary := GLSoundLibrary;
    Source.SoundName := 'howl.mp3';
    Playing := True;
  end;
end;

end.
