unit fSoundAroundD;

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
  GLS.Objects,
  GLS.Cadencer,
  Stage.VectorGeometry,
  GLS.SceneViewer,
  GLS.GeomObjects,
  GLS.SoundManager,

  GLS.Sounds.FMOD,
  GLS.Sounds.BASS,
  GLS.Sounds.OpenAL,
  GLS.Sounds.WaveOut,
  GLS.FileWAV,
  GLS.FileMP3,

  GLS.Coordinates,
  GLS.BaseClasses,
  Stage.Utils
  ;

type
  TFormSoundAround = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube: TGLDummyCube;
    Sphere: TGLSphere;
    GLLightSource: TGLLightSource;
    GLSoundLibrary: TGLSoundLibrary;
    GLSMBASS: TGLSMBASS;
    GLSMFMOD: TGLSMFMOD;
    GLSMOpenAL: TGLSMOpenAL;
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
    Label1: TLabel;
    RBBass: TRadioButton;
    RBFMOD: TRadioButton;
    Button1: TButton;
    btnHowl: TButton;
    RBOpenAL: TRadioButton;
    LabelFPS: TLabel;
    procedure SphereProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure TimerTimer(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBFMODClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnHowlClick(Sender: TObject);
  private
    Path: TFileName;
  public

  end;

var
  FormSoundAround: TFormSoundAround;

implementation

{$R *.DFM}

procedure TFormSoundAround.FormCreate(Sender: TObject);
begin
  Path := GetCurrentAssetPath() + '\audio';
  SetCurrentDir(Path);
  // Load our sound samples
  GLSoundLibrary.Samples.AddFile('drumloop.wav', 'drumloop.wav');
  GLSoundLibrary.Samples.AddFile('chimes.wav', 'chimes.wav');
  GLSoundLibrary.Samples.AddFile('howl.mp3', 'howl.mp3');
end;

procedure TFormSoundAround.SphereProgress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  alpha: Single;
begin
  // Move the red sphere (sound source) along an elliptic path
  alpha := 60 * DegToRad(newTime);
  TGLSphere(Sender).Position.SetPoint(sin(alpha) * 2, 0.5, cos(alpha) * 5);
end;

procedure TFormSoundAround.TrackBarChange(Sender: TObject);
begin
  // Rotate the listener around the vertical axis
  DummyCube.TurnAngle := TrackBar.Position;
  Application.ProcessMessages;
end;

procedure TFormSoundAround.TrackBar1Change(Sender: TObject);
begin
  // Move the listener forward/back
  Mickey.Position.Z := TrackBar1.Position / 10;
  Application.ProcessMessages;
end;

procedure TFormSoundAround.Button1Click(Sender: TObject);
begin
  with TGLBSoundEmitter.Create(Sphere.Behaviours) do
  begin
    Source.SoundLibrary := GLSoundLibrary;
    Source.SoundName := 'chimes.wav';
    Playing := True;
  end;
end;

procedure TFormSoundAround.TimerTimer(Sender: TObject);
var
  mngName: String;
begin
  // some stats
  if ActiveSoundManager is TGLSMBASS then
    mngName := 'BASS'
  else if ActiveSoundManager is TGLSMFMOD then
    mngName := 'FMOD'
  else if ActiveSoundManager is TGLSMOpenAL then
    mngName := 'OpenAL'
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

procedure TFormSoundAround.RBFMODClick(Sender: TObject);
var
  newManager: TGLSoundManager;
begin
  // This method switches managers. On a real world project, this would never
  // happen: you would choose an API and then cling to it, but the GLSS
  // completely wraps the underlying complexity and makes it a snap
  if RBFMOD.Checked then
  begin
    newManager := GLSMFMOD;
    btnHowl.Enabled := true;
  end
  else if RBBass.Checked then
  begin
    newManager := GLSMBASS;
    btnHowl.Enabled := true;
  end
  else
  begin
    newManager := GLSMOpenAL;
    btnHowl.Enabled := false; // mp3 not supported
  end;
  if newManager <> ActiveSoundManager then
  begin
    // shut down current one, and activate the new one
    if ActiveSoundManager <> nil then
      ActiveSoundManager.Active := false;
    if newManager <> nil then
      newManager.Active := true;
    // restart sound
    GetOrCreateSoundEmitter(Sphere).Playing := true;
  end;
end;

procedure TFormSoundAround.btnHowlClick(Sender: TObject);
begin
  with TGLBSoundEmitter.Create(Sphere.Behaviours) do
  begin
    Source.SoundLibrary := GLSoundLibrary;
    Source.SoundName := 'howl.mp3';
    Playing := True;
  end;
end;

end.
