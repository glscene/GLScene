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
  GLS.Utils, GLS.SimpleNavigation;

type

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
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLLightSource2: TGLLightSource;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
  private
  end;

var
  Form1: TForm1;

  // --------------------------------------------
implementation

// --------------------------------------------

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  GLCamera.Position.Rotate(VectorMake(0, 1, 0), deltaTime * 0.1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  var
    Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path + '\texture');
  GLMaterialLibrary1.TexturePaths := GetCurrentDir();
  GLPlane1.Material.Texture.Image.LoadFromFile('grass.png');

  SetCurrentDir(Path + '\modelext');

  GLSArchiveManager1.Archives[0].LoadFromFile('Chair.zlib');
  if GLSArchiveManager1.Archives[0].FileName = '' then
    ShowMessage('Archive Can not be Loaded');
  (* Automatic loading from archive.
    If file is not in archive, then it's loaded from archive directory ! *)
  GLFreeForm.LoadFromFile('Chair.ms3d');
  // Direct loading from archive
  GLFreeForm1.LoadFromStream('Chair.ms3d', GLSArchiveManager1.Archives[0].GetContent('Chair.ms3d'));
end;

end.
