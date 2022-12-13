unit fCandlesD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.Objects,
  GLS.Extrusion,
  GLS.Cadencer,
  GLS.FireFX,
  GLS.SceneViewer,
  GLS.GeomObjects,

  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormCandles = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    Candle: TGLCylinder;
    RevolutionSolid1: TGLRevolutionSolid;
    Lines1: TGLLines;
    GLFireFXManager1: TGLFireFXManager;
    GLCadencer1: TGLCadencer;
    GLProxyObject1: TGLProxyObject;
    GLProxyObject2: TGLProxyObject;
    TrackBar1: TTrackBar;
    Timer1: TTimer;
    Plane1: TGLPlane;
    DummyCube2: TGLDummyCube;
    procedure TrackBar1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
  private

  public

    mx, my: Integer;
  end;

var
  FormCandles: TFormCandles;

implementation

{$R *.DFM}

procedure TFormCandles.TrackBar1Change(Sender: TObject);
begin
  GLFireFXManager1.FireDir.Z := -TrackBar1.Position * 0.1;
end;

procedure TFormCandles.Timer1Timer(Sender: TObject);
var
  n: Integer;
begin
  Caption := 'Candles - ' + Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
  if TrackBar1.Position = 0 then
    GLFireFXManager1.Disabled := False
  else
  begin
    n := Abs(TrackBar1.Position) - 15;
    if n > 0 then
      if Random / n < 0.15 then
        GLFireFXManager1.Disabled := True;
  end;
end;

procedure TFormCandles.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormCandles.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Shift <> [] then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    GLCadencer1.Progress;
    mx := X;
    my := Y;
  end;
end;

procedure TFormCandles.FormResize(Sender: TObject);
begin
  GLCamera1.FocalLength := Height / 3;
end;

end.
