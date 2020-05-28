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
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,

  
  GLScene, GLVectorFileObjects, GLObjects,
  GLWin32Viewer, GLFile3DS, GLCrossPlatform, GLCoordinates,
  GLBaseClasses, GLUtils;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    DummyCube2: TGLDummyCube;
    DummyCube3: TGLDummyCube;
    FreeForm1: TGLFreeForm;
    FreeForm2: TGLFreeForm;
    FreeForm3: TGLFreeForm;
    GLLightSource1: TGLLightSource;
    DCCamera: TGLDummyCube;
    TrackBar1: TTrackBar;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
const
   cFileName = 'polyhedron.3ds';

begin
   SetGLSceneMediaDir();
   // left one
   FreeForm3.AutoCentering:=[macCenterX, macCenterZ];
   FreeForm3.LoadFromFile(cFileName);
   // central one
   FreeForm2.AutoCentering:=[macCenterY];
   FreeForm2.LoadFromFile(cFileName);
   // right one
   FreeForm1.AutoCentering:=[macCenterX, macCenterY, macCenterZ];
   FreeForm1.LoadFromFile(cFileName);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
   DCCamera.PitchAngle:=TrackBar1.Position;
end;

end.
