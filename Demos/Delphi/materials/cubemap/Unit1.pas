unit Unit1;

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

  GLScene,
  GLSceneViewer,
  GLObjects,
  GLTeapot,
  GLCrossPlatform,
  GLTexture,
  GLContext,
  GLUtils,
  GLCoordinates,
  GLBaseClasses,
  GLGeomObjects;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Button1: TButton;
    Teapot1: TGLTeapot;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    CubmapSupported: Boolean;
  public
    mx, my: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Our cube map images are here
  SetGLSceneMediaDir();
end;

procedure TForm1.GLSceneViewer1BeforeRender(Sender: TObject);
begin
  CubmapSupported := gl.ARB_texture_cube_map;
  GLSceneViewer1.BeforeRender := nil;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Cube map warning message
  // If you don't check and turn off cube maps yourself in your apps when
  // cube maps aren't supported, GLScene will just turn off texturing
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
    with Image as TGLCubeMapImage do
    begin
      // Load all 6 texture map components of the cube map
      // The 'PX', 'NX', etc. refer to 'positive X', 'negative X', etc.
      // and follow the RenderMan specs/conventions
      Picture[cmtPX].LoadFromFile('cm_left.jpg');
      Picture[cmtNX].LoadFromFile('cm_right.jpg');
      Picture[cmtPY].LoadFromFile('cm_top.jpg');
      Picture[cmtNY].LoadFromFile('cm_bottom.jpg');
      Picture[cmtPZ].LoadFromFile('cm_back.jpg');
      Picture[cmtNZ].LoadFromFile('cm_front.jpg');
    end;
    // Select reflection cube map environment mapping
    // This is the mode you'll most commonly use with cube maps, normal cube
    // map generation is also supported (used for diffuse environment lighting)
    MappingMode := tmmCubeMapReflection;
    // That's all folks, let us see the thing!
    Disabled := False;
  end;
  Button1.Visible := False;
end;

// standard issue handlers for mouse movement

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift <> [] then
  begin
    if ssLeft in Shift then
      GLCamera1.MoveAroundTarget(my - y, mx - x)
    else
      GLCamera1.RotateTarget(my - y, mx - x);
    mx := x;
    my := y;
  end;
end;

end.

