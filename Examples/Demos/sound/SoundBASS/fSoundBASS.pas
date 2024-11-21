unit fSoundBASS;

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

  GLS.Scene,
  Stage.VectorTypes,
  Stage.VectorGeometry,
  GLS.Cadencer,
  GLS.Objects,
  GLS.SoundManager,
  GLS.Sounds.BASS,
  GLS.SceneViewer,
  GLS.GeomObjects,

  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.FileWAV,
  GLS.FileMP3,
  Stage.Utils
  ;

type
  TFormSoundBASS = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube: TGLDummyCube;
    SphereSound: TGLSphere;
    GLLightSource: TGLLightSource;
    GLSoundLibrary: TGLSoundLibrary;
    GLSMBASS: TGLSMBASS;
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
  FormSoundBASS: TFormSoundBASS;

implementation

{$R *.DFM}

procedure TFormSoundBASS.FormCreate(Sender: TObject);
begin
  Path := GetCurrentAssetPath() + '\audio';
  SetCurrentDir(Path);
  // Load our sound samples
  GLSoundLibrary.Samples.AddFile('drumloop.wav', 'drumloop.wav');
  GLSoundLibrary.Samples.AddFile('chimes.wav', 'chimes.wav');
  GLSoundLibrary.Samples.AddFile('howl.mp3', 'howl.mp3');
end;

procedure TFormSoundBASS.SphereSoundProgress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  alpha: Single;
begin
  // Move the red sphere (sound source) along an elliptic path
  alpha := 60 * DegToRad(newTime);
  TGLSphere(Sender).Position.SetPoint(sin(alpha) * 1, 0.5, cos(alpha) * 5);
end;

procedure TFormSoundBASS.TrackBarChange(Sender: TObject);
begin
  // Rotate the listener around the vertical axis
  DummyCube.TurnAngle := TrackBar.Position;
  Application.ProcessMessages;
end;

procedure TFormSoundBASS.TrackBar1Change(Sender: TObject);
begin
  // Move the listener forward/back
  Mickey.Position.Z := TrackBar1.Position / 10;
  Application.ProcessMessages;
end;

procedure TFormSoundBASS.TimerTimer(Sender: TObject);
var
  mngName: String;
begin
  // some stats
  if ActiveSoundManager is TGLSMBASS then
    mngName := 'BASS'
  else
    mngName := '';
  if ActiveSoundManager <> nil then
    LabelFPS.Caption := Format('%.2f FPS, %s CPU use : %.2f%%',
      [GLSceneViewer.FramesPerSecond, mngName, ActiveSoundManager.CPUUsagePercent])
  else
    LabelFPS.Caption := 'No active sound manager.';
  GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TFormSoundBASS.btnChimesClick(Sender: TObject);
begin
  with TGLBSoundEmitter.Create(SphereSound.Behaviours) do
  begin
    Source.SoundLibrary := GLSoundLibrary;
    Source.SoundName := 'chimes.wav';
    Playing := True;
  end;
end;

procedure TFormSoundBASS.btnHowlClick(Sender: TObject);
begin
  with TGLBSoundEmitter.Create(SphereSound.Behaviours) do
  begin
    Source.SoundLibrary := GLSoundLibrary;
    Source.SoundName := 'howl.mp3';
    Playing := True;
  end;
end;

end.
