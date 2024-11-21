unit fMultiMaterialD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.Jpeg,

  GLS.Scene,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.Texture,
  Stage.VectorGeometry,
  GLS.Cadencer,
  GLSL.MultiMaterialShader,
  GLSL.TextureShaders,
  GLS.Material,
  GLS.Coordinates,

  Stage.Utils,
  GLS.BaseClasses,
  GLS.SimpleNavigation;

type
  TFormMultiMat = class(TForm)
    GLScene1: TGLScene;
    GLMatLib1: TGLMaterialLibrary;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLCube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLMatLib2: TGLMaterialLibrary;
    GLMultiMaterialShader1: TGLMultiMaterialShader;
    GLCadencer1: TGLCadencer;
    GLTexCombineShader1: TGLTexCombineShader;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private

  public
    mx, my: Integer;
  end;

var
  FormMultiMat: TFormMultiMat;

implementation

{$R *.dfm}

procedure TFormMultiMat.FormCreate(Sender: TObject);
var
  Path: TFileName;
  LibMat: TGLLibMaterial;
begin
  Path := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\texture');

	// GLMatLib1 is the source of the first image
	// Add the specular material using tmModulate for shiny text
  GLMatLib1.AddTextureMaterial('specular', 'glscene_alpha.bmp');
  GLMatLib1.Materials.GetLibMaterialByName('specular').Material.Texture.TextureMode := tmModulate;
  // use TextureMode := tmBlend; for shiny background
  GLMatLib1.Materials.GetLibMaterialByName('specular').Material.BlendingMode := bmAdditive;
  GLMatLib1.Materials.GetLibMaterialByName('specular').Texture2Name := 'specular_tex2';

  (* or with delphi with do more short
  with GLMatLib1.AddTextureMaterial('specular', 'glscene_alpha.bmp') do
  begin
    Material.Texture.TextureMode := tmModulate;
    Material.BlendingMode := bmAdditive;
    Texture2Name := 'specular_tex2';
  end;
  //*)

  GLMatLib1.AddTextureMaterial('specular_tex2', 'rainbow.bmp');
  GLMatLib1.Materials.GetLibMaterialByName('specular_tex2').Material.Texture.MappingMode := tmmCubeMapReflection;
  GLMatLib1.Materials.GetLibMaterialByName('specular_tex2').Material.Texture.ImageBrightness := 0.3;

  // GLMatLib2 is the source of the GLMultiMaterialShader passes.

  // Pass 1: Base texture
  GLMatLib2.AddTextureMaterial('Pass1', 'glscene.bmp'); // or use glscene_delphi.bmp  

  // Pass 2: Add a bit of detail
  GLMatLib2.AddTextureMaterial('Pass2', 'detailmap.jpg');
  GLMatLib2.Materials.GetLibMaterialByName('Pass2').Material.Texture.TextureMode := tmBlend;
  GLMatLib2.Materials.GetLibMaterialByName('Pass2').Material.BlendingMode := bmAdditive;

  // Pass 3: And a little specular reflection
  LibMat := TGLLibMaterial.Create(GLMatLib2.Materials);
  LibMat.Material.MaterialLibrary := GLMatLib1;
  LibMat.Material.LibMaterialName := 'specular';

  // This isn't limited to 3, try adding some more passes!
end;

procedure TFormMultiMat.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormMultiMat.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TFormMultiMat.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
  Handled := true
end;

procedure TFormMultiMat.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLCube1.Turn(deltaTime * 10);
end;

end.
