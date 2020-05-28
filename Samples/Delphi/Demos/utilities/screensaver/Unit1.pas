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

  
  GLScene, GLScreenSaver, GLObjects, GLBehaviours, GLCadencer,
  GLWin32Viewer, GLGeomObjects, GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    DummyCube2: TGLDummyCube;
    DummyCube3: TGLDummyCube;
    DummyCube4: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLLightSource2: TGLLightSource;
    GLLightSource3: TGLLightSource;
    Torus1: TGLTorus;
    GLCadencer1: TGLCadencer;
    GLScreenSaver1: TGLScreenSaver;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLScreenSaver1PropertiesRequested(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  Unit2;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // read our option
  case GetMeshResolutions of
    0:
      begin // Low Res, this is one ugly torus
        Torus1.Rings := 8;
        Torus1.Sides := 6;
      end;
    1:
      begin // High Res, should still look smooth at high resolutions
        Torus1.Rings := 64;
        Torus1.Sides := 32;
      end;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // let the show begin :)
  GLCadencer1.Enabled := True;
end;

procedure TForm1.GLScreenSaver1PropertiesRequested(Sender: TObject);
begin
  // we create the dialog dans display it
  // we do not need to free it (TApplication will take care of this)
  Application.CreateForm(TForm2, Form2);
  Form2.ShowModal;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  // "Rescale" when form size is changed so our saver always looks the same
  GLCamera1.FocalLength := 50 * Width / 400;
end;

end.
