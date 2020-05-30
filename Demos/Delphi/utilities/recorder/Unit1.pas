unit Unit1;

interface

uses
  System.Classes, System.SysUtils, System.Math,
  Vcl.Forms, Vcl.Graphics, Vcl.Controls,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls,
  
  GLScene, GLObjects, GLAsyncTimer,  GLCadencer, GLAVIRecorder, GLWin32Viewer,
  GLCrossPlatform, GLCoordinates, GLBaseClasses;

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
    StaticText1: TStaticText;
    DummyCube1: TGLDummyCube;
    DummyCube2: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    Button1: TButton;
    AVIRecorder1: TGLAVIRecorder;
    procedure TrackBarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure AVIRecorder1PostProcessEvent(Sender: TObject;
      frame: TBitmap);
  private
     
     UserAbort : boolean;
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
   // update FPS count
   StaticText1.Caption:=IntToStr(Trunc(GLSceneViewer1.FramesPerSecond))+' FPS';
end;

procedure TForm1.FormResize(Sender: TObject);
begin
	GLSceneViewer1.ResetPerformanceMonitor;
   AVIRecorder1.Width:=GLSceneViewer1.Width;
   AVIRecorder1.Height:=GLSceneViewer1.Height;
end;

procedure TForm1.Button1Click(Sender: TObject);
var i : integer;
    SavedCap : string;
begin
   if not AVIRecorder1.CreateAVIFile then Exit;
   // if AVIRecorder1.filename is empty, a dialog box will appear asking
   // for the filename. CreateAVIFile() will return a bool
   // indicating if user presses "cancel" in the dialog box.

   SavedCap:=caption;
   caption:='Press ESC to abort';
   UserAbort:=false;
   StaticText1.Visible:=false; // the FPS shown is not correct now,
                               // so just hide it for the time being.
   i:=0;

   Button1.enabled:=false;
   TrackBar.enabled:=false;

   try
      while (i<360) and not UserAbort do begin
         TrackBar.Position:=i;
         TrackBarChange(self);

         AVIRecorder1.AddAVIFrame;

         // you might want to update your progress bar here.

         Application.ProcessMessages; // so that our app. is not freezed,
                                      // and will accept user abort.
         inc(i);
      end;
   finally
      AVIRecorder1.CloseAVIFile(UserAbort); // if UserAbort, CloseAVIFile will
                                            // also delete the unfinished file.
      caption:=SavedCap;
      StaticText1.Visible:=true;
      Button1.enabled:=true;
      TrackBar.enabled:=true;
   end;

end;

procedure TForm1.AVIRecorder1PostProcessEvent(Sender: TObject;
  frame: TBitmap);
begin
   // PostProcess event is used to add a "watermark"
   // that will be in the AVI, but isn't visible on-screen
   with frame.Canvas do begin
      Font.Color:=clAqua;
      Font.Name:='Courrier New';
      Font.Size:=24;
      Font.Style:=[fsBold];
      Brush.Style:=bsClear;
      TextOut(20, 20, Format('GLScene %.3d', [TrackBar.Position]));
   end;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  UserAbort:=key=#27;
end;

end.
