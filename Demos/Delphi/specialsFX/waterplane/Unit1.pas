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
  Vcl.Imaging.Jpeg,


  OpenGLTokens,
  GLCrossPlatform,
  GLScene,
  GLObjects,
  GLSceneViewer,
  GLWaterPlane,
  GLCadencer,
  GLTexture,
  GLUserShader,
  GLContext,
  Scene.VectorGeometry,
  GLGraph,
  Scene.VectorTypes,
  GLState,
  GLMaterial,
  GLCoordinates,
  GLRenderContextInfo,
  GLSimpleNavigation,
  GLColor,
  GLS.Utils,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DCTarget: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    GLWaterPlane1: TGLWaterPlane;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLUserShader1: TGLUserShader;
    GLSphere1: TGLSphere;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLHeightField1: TGLHeightField;
    GLLightSource1: TGLLightSource;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLUserShader1DoApply(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLHeightField1GetHeight(const X, Y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
    procedure GLUserShader1DoUnApply(Sender: TObject; Pass: Integer;
      var rci: TGLRenderContextInfo; var Continue: Boolean);
  private
     
  public
     
    mx, my: Integer;
    reflectionToggle: Boolean;
    procedure ClickWater(X, Y: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ClickWater(X, Y: Integer);
var
  ip: TVector;
begin
  // create a ripple in the pond on a right-mousebutton click

  GLSceneViewer1.Buffer.ScreenVectorIntersectWithPlaneXZ
    (VectorMake(X, GLSceneViewer1.Height - Y, 0), GLWaterPlane1.Position.Y, ip);
  GLWaterPlane1.CreateRippleAtWorldPos(ip);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  GLMaterialLibrary1.TexturePaths := GetCurrentDir();
  // Load the cube map which is used both for environment and as reflection texture
  with GLMaterialLibrary1.Materials[0].Material.Texture do
  begin
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
  end;

  GLWaterPlane1.Mask.LoadFromFile('basinMask.bmp');
  GLHeightField1.Material.Texture.Image.LoadFromFile('clover.jpg');
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  if ssRight in Shift then
    ClickWater(X, Y);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    // pseudo-fresnel
    with GLMaterialLibrary1.LibMaterialByName('CubeMap').Material do
      FrontProperties.Diffuse.Alpha := 0.3 + 0.5 *
        Sqr(1 - GLCamera1.Position.Y / GLCamera1.DistanceToTarget);
  end
  else if ssRight in Shift then
    ClickWater(X, Y);
  mx := X;
  my := Y;
end;

procedure TForm1.GLUserShader1DoApply(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  cubeMapMode: Integer;
begin
  // Here is the shader trick: the same cubemap is used in reflection mode
  // for the pond, and in normal mode for the environment sphere
  // Our basic user shader takes care of that.
  if reflectionToggle then
  begin
    cubeMapMode := GL_REFLECTION_MAP_ARB;
    rci.GLStates.Enable(stBlend);
  end
  else
  begin
    cubeMapMode := GL_NORMAL_MAP_ARB;
    rci.GLStates.Disable(stBlend);
  end;

  glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, cubeMapMode);
  glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, cubeMapMode);
  glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, cubeMapMode);

  rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
end;

procedure TForm1.GLUserShader1DoUnApply(Sender: TObject; Pass: Integer;
  var rci: TGLRenderContextInfo; var Continue: Boolean);
begin
  rci.GLStates.Disable(stBlend);
end;

procedure TForm1.GLSceneViewer1BeforeRender(Sender: TObject);
begin
  reflectionToggle := False; // toggle for environment sphere
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  reflectionToggle := True; // toggle for pond/water plane
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLHeightField1GetHeight(const X, Y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
begin
  z := 0.5 - (GLWaterPlane1.Mask.Bitmap.Canvas.Pixels[Round(X + 64),
    Round(Y + 64)] and $FF) / 255;
end;

end.
