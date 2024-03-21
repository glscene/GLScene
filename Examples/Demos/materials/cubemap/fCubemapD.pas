unit fCubemapD;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Imaging.Jpeg,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  GLS.Scene,
  GLS.SceneViewer,
  GLS.Objects,
  GLS.Texture,
  GLS.FileDDS,
  GLS.Context,
  GLS.Utils,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.GeomObjects;

type
  TFormCubeMap = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    btnApply: TButton;
    Teapot1: TGLTeapot;
    Cylinder1: TGLCylinder;
    Cone1: TGLCone;
    Plane1: TGLPlane;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure btnApplyClick(Sender: TObject);
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Path: TFileName;
    CubmapSupported: Boolean;
    CubemapPath: TFileName;
    Cubemap: TGLTexture;
  public
    mx, my: Integer;
  end;

var
  FormCubeMap: TFormCubeMap;

implementation

{$R *.dfm}

procedure TFormCubeMap.FormCreate(Sender: TObject);
begin
  // Our cube map images are here
  Path := GetCurrentAssetPath();
  SetCurrentDir(Path + '\cubemap');
end;

procedure TFormCubeMap.GLSceneViewer1BeforeRender(Sender: TObject);
begin
  CubmapSupported := GL.ARB_texture_cube_map;
  GLSceneViewer1.BeforeRender := nil;
end;

procedure TFormCubeMap.btnApplyClick(Sender: TObject);
begin
  // Cube map warning message
  // If you don't check and turn off cube maps yourself in your apps when
  // cube maps aren't supported and will just turn off texturing
  // (ie. no error generated, just a different output)
  if not CubmapSupported then
  begin
    ShowMessage('Your graphics board does not support cube maps...');
    Exit;
  end;

  with Teapot1.Material.Texture do
  begin
    // We need a CubeMapImage, which unlike the "regular Images" stores
    // multiple images.
    ImageClassName := TGLCubeMapImage.ClassName;
    // The 'PX', 'NX', etc. refer to 'positive X', 'negative X', etc.
    // and follow the RenderMan specs/conventions

    (Image as TGLCubeMapImage).Picture[cmtPX].LoadFromFile('cm_left.jpg');
    (Image as TGLCubeMapImage).Picture[cmtNX].LoadFromFile('cm_right.jpg');
    (Image as TGLCubeMapImage).Picture[cmtPY].LoadFromFile('cm_top.jpg');
    (Image as TGLCubeMapImage).Picture[cmtNY].LoadFromFile('cm_bottom.jpg');
    (Image as TGLCubeMapImage).Picture[cmtPZ].LoadFromFile('cm_back.jpg');
    (Image as TGLCubeMapImage).Picture[cmtNZ].LoadFromFile('cm_front.jpg');
    // Select reflection cube map environment mapping
    // This is the mode you'll most commonly use with cube maps, normal cube
    // map generation is also supported (used for diffuse environment lighting)

    MappingMode := tmmCubeMapReflection;
    // That's all folks, let us see the thing!
    Disabled := False;
  end;
(* *)

//  DDStex(Teapot1.Material.Texture, 'skybox.dds');
//  Teapot1.Material.Texture.MappingMode := tmmEyeLinear;

  // apply .dds cubemaps to next objects
  DDStex(Cylinder1.Material.Texture, 'skybox.dds');
  Cylinder1.Material.Texture.MappingMode := tmmEyeLinear;
  DDStex(Cone1.Material.Texture, 'skybox.dds');
  Cone1.Material.Texture.MappingMode := tmmEyeLinear;
  DDStex(Plane1.Material.Texture, 'skybox.dds');
  Plane1.Material.Texture.MappingMode := tmmEyeLinear;

  btnApply.Visible := False;
end;

// standard issue handlers for mouse movement
procedure TFormCubeMap.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormCubeMap.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Shift <> [] then
  begin
    if ssLeft in Shift then
      GLCamera1.MoveAroundTarget(my - Y, mx - X)
    else
      GLCamera1.RotateTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

end.
