unit fSphereD;

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
  Vcl.StdCtrls,

  
  GLS.Scene,
  GLS.VectorTypes,
  GLS.Objects,
  GLS.Collision,
  GLS.SceneViewer,
 
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormSphere = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    CollisionManager1: TGLCollisionManager;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    Sphere1: TGLSphere;
    Sphere2: TGLSphere;
    TrackBar1: TTrackBar;
    Button1: TButton;
    procedure TrackBar1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CollisionManager1Collision(Sender: TObject; object1,
      object2: TGLBaseSceneObject);
  private
    { Private declarations  }
  public
     
  end;

var
  FormSphere: TFormSphere;

implementation

{$R *.DFM}

procedure TFormSphere.TrackBar1Change(Sender: TObject);
begin
   Sphere1.Position.Z:=TrackBar1.Position/10;
end;

procedure TFormSphere.Button1Click(Sender: TObject);
begin
   CollisionManager1.CheckCollisions;
end;

procedure TFormSphere.CollisionManager1Collision(Sender: TObject; object1,
  object2: TGLBaseSceneObject);
begin
   ShowMessage('Collision between '+object1.Name+' and '+object2.Name);
end;

end.
