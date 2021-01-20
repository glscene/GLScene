unit ManualFm;

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
  GLS.VectorTypes,
  GLS.Objects,
  GLS.Cadencer,
  GLS.SceneViewer,
 
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormManual = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    TrackBar: TTrackBar;
    Cube1: TGLCube;
    Cube3: TGLCube;
    Cube2: TGLCube;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    CBPlay: TCheckBox;
    StaticText1: TStaticText;
    GLCadencer1: TGLCadencer;
    procedure TrackBarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations  }
  public
     
  end;

var
  FormManual: TFormManual;

implementation

{$R *.DFM}

procedure TFormManual.TrackBarChange(Sender: TObject);
var
   t : Integer;
begin
	t:=TrackBar.Position;
	// the "sun" turns slowly around Y axis
	Cube1.TurnAngle:=t/4;
	// "earth" rotates around the sun on the Y axis
	with Cube2.Position do begin
		X:=3*cos(DegToRad(t));
		Z:=3*sin(DegToRad(t));
	end;
	// "moon" rotates around earth on the X axis
	with Cube3.Position do begin
		X:=Cube2.Position.X;
		Y:=Cube2.Position.Y+1*cos(DegToRad(3*t));
		Z:=Cube2.Position.Z+1*sin(DegToRad(3*t));
   end;
   // update FPS count
   StaticText1.Caption:=IntToStr(Trunc(GLSceneViewer1.FramesPerSecond))+' FPS';
end;

procedure TFormManual.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
	if CBPlay.Checked and Visible then begin
		// simulate a user action on the trackbar...
		TrackBar.Position:=((TrackBar.Position+1) mod 360);
	end;
end;

procedure TFormManual.FormResize(Sender: TObject);
begin
	GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
