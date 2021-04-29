unit Main;

interface

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Dialogs,

  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Scene,
  GLS.Objects,
  GLS.VectorFileObjects,
  GLS.Material, GLS.Cadencer,
  GLS.ArchiveManager,
  GLS.BaseClasses,
  GLS.FileMS3D,
  GLS.FileTGA,
  GLS.FileZLIB,
  GLS.Coordinates,
  GLS.SceneViewer,
  GLS.Utils;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLCadencer1: TGLCadencer;
    GLCamera: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLFreeForm: TGLFreeForm;
    GLFreeForm1: TGLFreeForm;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLPlane1: TGLPlane;
    GLSArchiveManager1: TGLSArchiveManager;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
  private
  end;

var
  Form1: TForm1;

//--------------------------------------------
implementation
//--------------------------------------------

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLCamera.Position.Rotate(VectorMake(0, 1, 0), deltaTime * 0.1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  GLMaterialLibrary1.TexturePaths := GetCurrentDir();
  with GLSArchiveManager1.Archives[0] do
  begin
    LoadFromFile('Chair.zlib');
    if FileName = '' then
      ShowMessage('Archive Can not be Loaded');
    (* Automatic loading from archive.
      If file is not in archive, then it's loaded from harddrive. *)
    GLFreeForm.LoadFromFile('Chair.ms3d');
    // Direct loading from archive
    GLFreeForm1.LoadFromStream('Chair.ms3d', GetContent('Chair.ms3d'));
  end;
  GLPlane1.Material.Texture.Image.LoadFromFile('GLScene.bmp');
end;

end.
