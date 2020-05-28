unit Unit1;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  
  GLScene,
  GLObjects,
  GLWin32Viewer,
  GLCrossPlatform,
  GLCoordinates,
  GLBaseClasses,
  GLVectorGeometry;

type
  TForm1 = class(TForm)
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
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.MoveCenterNodeTo(x, y : Integer);
begin
   Lines1.Nodes[1].AsAffineVector:=GLSceneViewer1.Buffer.ScreenToWorld(x, y);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MoveCenterNodeTo(x, y);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then
      MoveCenterNodeTo(x, y);
end;

end.
