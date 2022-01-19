unit fSkyBoxD;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.Texture,
  GLS.SkyDome,

  GLS.Cadencer,
  GLS.Navigator,
  GLS.SceneViewer,
  GLS.Keyboard,
  GLS.LensFlare,
  GLS.Objects,
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.SimpleNavigation,
  GLS.Utils;

type
  TFormSkyBox = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLMaterialLibraryCM: TGLMaterialLibrary;
    GLLightSource1: TGLLightSource;
    Castle: TGLDummyCube;
    GLCube1: TGLCube;
    GLCube11: TGLCube;
    GLCube111: TGLCube;
    GLCube112: TGLCube;
    GLCube2: TGLCube;
    GLCube21: TGLCube;
    GLCube211: TGLCube;
    GLCube212: TGLCube;
    GLNavigator1: TGLNavigator;
    GLCadencer1: TGLCadencer;
    GLUserInterface1: TGLUserInterface;
    GLLensFlare1: TGLLensFlare;
    GLSceneViewer1: TGLSceneViewer;
    GLSkyBox1: TGLSkyBox;
    GLSkyBox2: TGLSkyBox;
    GLSphere1: TGLSphere;
    GLSphere2: TGLSphere;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
  private
    procedure HandleKeys(d: Double);
    function LoadTexture(Matname, Filename: string): TGLLibMaterial;
  public
  end;

var
  FormSkyBox: TFormSkyBox;

implementation

{$R *.dfm}

function TFormSkyBox.LoadTexture(Matname, Filename: string): TGLLibMaterial;
begin
  Result := GLMaterialLibraryCM.AddTextureMaterial(Matname, Filename);
  Result.Material.Texture.Disabled := False;
  Result.Material.Texture.TextureMode := tmDecal;
end;

procedure TFormSkyBox.FormCreate(Sender: TObject);
var
  PathCM: TFileName;
begin
  SetGLSceneMediaDir();
  PathCM := GetCurrentDir() + '\Cubemaps';
  SetCurrentDir(PathCM);
  // Skybox cubemaps
  LoadTexture('Left', 'icecraterlf.jpg');
  LoadTexture('Right', 'icecraterrt.jpg');
  LoadTexture('Top', 'icecraterup.jpg');
  LoadTexture('Bottom', 'icecraterdn.jpg');
  LoadTexture('Front', 'icecraterft.jpg');
  LoadTexture('Back', 'icecraterbk.jpg');
  SetGLSceneMediaDir(); // back to media folder with textures
  with LoadTexture('Clouds', 'Clouds.jpg') do
  begin
    // Add transparency to clouds
    Material.BlendingMode := bmTransparency;
    Material.FrontProperties.Diffuse.Alpha := 0.2;
    // scale the clouds texture
    TextureScale.X := 8;
    TextureScale.Y := 8;
  end;

  // bricks
  with LoadTexture('Bricks', 'rawwall.jpg') do
  begin
    TextureScale.X := 1;
    TextureScale.Y := 32;
    Material.Texture.TextureMode := tmModulate;
  end;
  with LoadTexture('Bricks2', 'marbletiles.jpg') do
  begin
    TextureScale.X := 6;
    TextureScale.Y := 1;
    Material.Texture.TextureMode := tmModulate;
  end;

  // Moon
  LoadTexture('Moon', 'unwrapped moon.jpg').Material.Texture.TextureMode :=
    tmModulate;

  // -----------------------------------------
  // Assign materials to objects
  // -----------------------------------------
  GLCube1.Material.LibMaterialName := 'Bricks';
  GLCube11.Material.LibMaterialName := 'Bricks';
  GLCube111.Material.LibMaterialName := 'Bricks';
  GLCube112.Material.LibMaterialName := 'Bricks';
  GLCube2.Material.LibMaterialName := 'Bricks2';
  GLCube21.Material.LibMaterialName := 'Bricks2';
  GLCube21.Material.LibMaterialName := 'Bricks2';
  GLCube211.Material.LibMaterialName := 'Bricks2';
  GLCube212.Material.LibMaterialName := 'Bricks2';
  GLSphere1.Material.LibMaterialName := 'Moon';
  GLSphere2.Material.LibMaterialName := 'Moon';

  //GLUserInterface1.MouseLookActive := true;
end;

procedure TFormSkyBox.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  // Make clouds Texture slide
  with GLMaterialLibraryCM.Materials.GetLibMaterialByName('Clouds') do
  begin
    TextureOffset.X := TextureOffset.X + deltaTime * 0.02;
    TextureOffset.y := TextureOffset.y + deltaTime * 0.03;
  end;

  // Rotate moons
  GLSphere1.Turn(deltaTime * 7);
  GLSphere2.Turn(deltaTime * 10);

  HandleKeys(deltaTime);
  GLUserInterface1.Mouselook;
  GLUserInterface1.MouseUpdate;
  GLSceneViewer1.Invalidate;
end;

procedure TFormSkyBox.HandleKeys(d: Double);
begin
  if IsKeyDown('W') or IsKeyDown('Z') then
    GLCamera1.Move(d);
  if IsKeyDown('S') then
    GLCamera1.Move(-d);
  if IsKeyDown('A') or IsKeyDown('A') then
    GLCamera1.Slide(-d);
  if IsKeyDown('D') then
    GLCamera1.Slide(d);

  if IsKeyDown(VK_ESCAPE) then
    Close;
end;

end.
