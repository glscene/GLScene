unit WaterPlaneFm;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,


  GLS.OpenGLTokens,
 
  GLS.Scene,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.WaterPlane,
  GLS.Cadencer,
  GLS.Texture,
  GLSL.UserShader,
  GLS.Context,
  GLS.VectorGeometry,
  GLS.Graph,
  GLS.VectorTypes,
  GLS.State,
  GLS.Material,
  GLS.Coordinates,
  GLS.RenderContextInfo,
  GLS.SimpleNavigation,
  GLS.Color,
  GLS.Utils,
  GLS.BaseClasses;

type
  TFormWaterPlane = class(TForm)
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
  public
    mx, my: Integer;
    reflectionToggle: Boolean;
    procedure ClickWater(X, Y: Integer);
  end;

var
  FormWaterPlane: TFormWaterPlane;

implementation

{$R *.dfm}

procedure TFormWaterPlane.ClickWater(X, Y: Integer);
var
  ip: TGLVector;
begin
  // create a ripple in the pond on a right-mousebutton click

  GLSceneViewer1.Buffer.ScreenVectorIntersectWithPlaneXZ
    (VectorMake(X, GLSceneViewer1.Height - Y, 0), GLWaterPlane1.Position.Y, ip);
  GLWaterPlane1.CreateRippleAtWorldPos(ip);
end;

procedure TFormWaterPlane.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  GLMaterialLibrary1.TexturePaths := GetCurrentDir();
  GLWaterPlane1.Mask.LoadFromFile('basinMask.bmp');
  GLHeightField1.Material.Texture.Image.LoadFromFile('clover.jpg');
  SetCurrentDir(GLMaterialLibrary1.TexturePaths + '\Cubemaps');
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

end;

procedure TFormWaterPlane.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  if ssRight in Shift then
    ClickWater(X, Y);
end;

procedure TFormWaterPlane.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
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

procedure TFormWaterPlane.GLUserShader1DoApply(Sender: TObject;
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

procedure TFormWaterPlane.GLUserShader1DoUnApply(Sender: TObject; Pass: Integer;
  var rci: TGLRenderContextInfo; var Continue: Boolean);
begin
  rci.GLStates.Disable(stBlend);
end;

procedure TFormWaterPlane.GLSceneViewer1BeforeRender(Sender: TObject);
begin
  reflectionToggle := False; // toggle for environment sphere
end;

procedure TFormWaterPlane.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  reflectionToggle := True; // toggle for pond/water plane
end;

procedure TFormWaterPlane.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TFormWaterPlane.GLHeightField1GetHeight(const X, Y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
begin
  z := 0.5 - (GLWaterPlane1.Mask.Bitmap.Canvas.Pixels[Round(X + 64),
    Round(Y + 64)] and $FF) / 255;
end;

end.
