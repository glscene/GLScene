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
  GLS.VectorTypes,
  GLS.Objects,
  GLS.Sound,
  GLS.SceneViewer,
  GLS.GeomObjects,
  Sounds.OpenAL,

  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.FileWAV,
  GLS.FileMP3,
  GLS.Utils;

type
  TForm1 = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube: TGLDummyCube;
    Sphere: TGLSphere;
    GLLightSource: TGLLightSource;
    GLSMOpenAL: TGLSMOpenAL;
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
    procedure SphereProgress(Sender: TObject; const deltaTime,
      newTime: Double);
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
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  GLS.VectorGeometry;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir;
  // Load our sound sample
  GLSoundLibrary.Samples.AddFile('drumloop.wav','drumloop.wav');
  GLSoundLibrary.Samples.AddFile('chimes.wav','chimes.wav');
  GLSoundLibrary.Samples.AddFile('howl.mp3','howl.mp3');
end;

procedure TForm1.SphereProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   alpha : Single;
begin
   // Move the red sphere (sound source) along an elliptic path
   alpha:=60*DegToRad(newTime);
   TGLSphere(Sender).Position.SetPoint(sin(alpha)*2, 0.5, cos(alpha)*5);
end;

procedure TForm1.TrackBarChange(Sender: TObject);
begin
   // Rotate the listener around the vertical axis
   DummyCube.TurnAngle:=TrackBar.Position;
   Application.ProcessMessages;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
   // Move the listener forward/back
   Mickey.Position.Z:=TrackBar1.Position/10;
   Application.ProcessMessages;
end;

procedure TForm1.TimerTimer(Sender: TObject);
var
   mngName : String;
begin
   // some stats
   if ActiveSoundManager is TGLSMOpenAL then
      mngName:='OpenAL'
   else mngName:='';
   if ActiveSoundManager<>nil then
      LabelFPS.Caption:=Format('%.2f FPS, %s CPU use : %.2f%%',
                      [GLSceneViewer.FramesPerSecond, mngName,
                       ActiveSoundManager.CPUUsagePercent])
   else LabelFPS.Caption:='No active sound manager.';
   GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   with TGLBSoundEmitter.Create(Sphere.Behaviours) do begin
      Source.SoundLibrary:=GLSoundLibrary;
      Source.SoundName:='chimes.wav';
      Playing:=True;
   end;
end;

procedure TForm1.btnHowlClick(Sender: TObject);
begin
   with TGLBSoundEmitter.Create(Sphere.Behaviours) do begin
      Source.SoundLibrary:=GLSoundLibrary;
      Source.SoundName:='howl.mp3';
      Playing:=True;
   end;       
end;

end.
