unit Unit1;

interface

uses
  System.Classes, System.SysUtils, System.Math,
  Vcl.Forms, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Controls,
  
  GLScene, GLObjects,
  GLCadencer, GLAsyncTimer, GLWin32Viewer, GLCrossPlatform,
  GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    TrackBar: TTrackBar;
    Cube1: TGLCube;
    Cube3: TGLCube;
    Cube2: TGLCube;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    CBPlay: TCheckBox;
    DummyCube1: TGLDummyCube;
    DummyCube2: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    procedure TrackBarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations  }
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.TrackBarChange(Sender: TObject);
var
   t : Integer;
begin
	t:=TrackBar.Position;
	// the "sun" spins slowly
	Cube1.TurnAngle:=t/4;
	// "earth" rotates around the sun and spins
	DummyCube1.TurnAngle:=-t;
	Cube2.TurnAngle:=t*2;
	// "moon" rotates around earth and spins
	DummyCube2.RollAngle:=3*t;
	Cube3.TurnAngle:=4*t;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
	if CBPlay.Checked and Visible then begin
		// simulate a user action on the trackbar...
		TrackBar.Position:=((TrackBar.Position+1) mod 360);
   end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
	GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	// We need to stop playing here :
	// 	since the timer is asynchronous, if we don't stop play,
	// 	it may get triggered during the form's destruction
	CBPlay.Checked:=False;
end;

end.
