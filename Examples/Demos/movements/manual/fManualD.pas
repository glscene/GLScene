unit fManualD;

interface

uses
  System.SysUtils,
  System.Math,
  System.Classes,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,

  GLS.Scene,
  Stage.VectorTypes,
  GLS.Objects,
  GLS.Cadencer,
  GLS.SceneViewer,

  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormManual = class(TForm)
    Scene: TGLScene;
    SceneViewer: TGLSceneViewer;
    TrackBar: TTrackBar;
    CubeSun: TGLCube;
    CubeMoon: TGLCube;
    CubeEarth: TGLCube;
    Camera: TGLCamera;
    LightSource: TGLLightSource;
    CBPlay: TCheckBox;
    StaticText1: TStaticText;
    Cadencer: TGLCadencer;
    procedure TrackBarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CadencerProgress(Sender: TObject;
      const deltaTime, newTime: Double);
  private
  public
  end;

var
  FormManual: TFormManual;

implementation

{$R *.DFM}

procedure TFormManual.TrackBarChange(Sender: TObject);
var
  t: Integer;
begin
  t := TrackBar.Position;
  // the "sun" turns slowly around Y axis
  CubeSun.TurnAngle := t / 4;

  // "earth" rotates around the sun on the Y axis
  CubeEarth.Position.X := 3 * cos(DegToRad(t));
  CubeEarth.Position.Z := 3 * sin(DegToRad(t));

  // "moon" rotates around earth on the X axis
  CubeMoon.Position.X := CubeEarth.Position.X;
  CubeMoon.Position.Y := CubeEarth.Position.Y + 1 * cos(DegToRad(3 * t));
  CubeMoon.Position.Z := CubeEarth.Position.Z + 1 * sin(DegToRad(3 * t));

  // update FPS count
  StaticText1.Caption := IntToStr(Trunc(SceneViewer.FramesPerSecond)) + ' FPS';
end;

procedure TFormManual.CadencerProgress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if CBPlay.Checked and Visible then
  begin
    // simulate a user action on the trackbar...
    TrackBar.Position := ((TrackBar.Position + 1) mod 360);
  end;
end;

procedure TFormManual.FormResize(Sender: TObject);
begin
  SceneViewer.ResetPerformanceMonitor;
end;

end.
