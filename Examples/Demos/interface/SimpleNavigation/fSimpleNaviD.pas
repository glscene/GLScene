unit fSimpleNaviD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  
  GLS.Scene,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.Cadencer,
  GLS.SimpleNavigation,
 
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormSimpleNavigation = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    GLCube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
     
  public
     
  end;

var
  FormSimpleNavigation: TFormSimpleNavigation;

implementation

{$R *.dfm}

procedure TFormSimpleNavigation.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
  GLCube1.Turn(-0.05);
end;

end.
