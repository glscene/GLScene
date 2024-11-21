unit fSplinesD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  
  GLS.Scene,
  GLS.Objects,
  GLS.SceneViewer,
 
  GLS.Coordinates,
  GLS.BaseClasses,
  Stage.VectorGeometry;

type
  TFormSplines = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    Lines1: TGLLines;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
  public
    procedure MoveCenterNodeTo(x, y : Integer);
  end;

var
  FormSplines: TFormSplines;

implementation

{$R *.DFM}

procedure TFormSplines.MoveCenterNodeTo(x, y : Integer);
begin
   Lines1.Nodes[1].AsAffineVector:=GLSceneViewer1.Buffer.ScreenToWorld(x, y);
end;

procedure TFormSplines.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MoveCenterNodeTo(x, y);
end;

procedure TFormSplines.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then
      MoveCenterNodeTo(x, y);
end;

end.
