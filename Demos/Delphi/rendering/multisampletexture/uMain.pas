unit uMain;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  
  GLScene,
  GLObjects,
  GLSceneViewer,
  GLCrossPlatform,
  GLCoordinates,
  GLBaseClasses,
  GLBitmapFont,
  GLWindowsFont,
  GLMaterial,
  GLS.ShaderCustom,
  GLSL.Shader,
  GLCadencer,
  GLHUDObjects,
  GLGeomObjects,
  GLFBORenderer,
  Scene.VectorGeometry,
  Scene.VectorTypes,
  GLTexture,
  GLSimpleNavigation,
  GLState,
  GLContext,
  GLKeyboard,
  GLMultisampleIm, GLS.ShaderCustom, GLSL.Shaderage;

type
  TGLDemoForm = class(TForm)
    MainScene: TGLScene;
    MainCamera: TGLCamera;
    MainCadencer: TGLCadencer;
    MainMaterialLibrary: TGLMaterialLibrary;
    SceneObjects: TGLDummyCube;
    MainViewer: TGLSceneViewer;
    TestLight: TGLLightSource;
    MultisampleFBO: TGLFBORenderer;
    GLSLShader1: TGLSLShader;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLScreenQuad: TGLHUDSprite;
    GLSphere1: TGLSphere;
    GLTorus1: TGLTorus;
    GLTorus2: TGLTorus;
    GLCone1: TGLCone;
    GLLines1: TGLLines;
    FBOContainer: TGLDummyCube;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    procedure FormCreate(Sender: TObject);
    procedure MainCadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure FormResize(Sender: TObject);
    procedure GLSLShader1Apply(Shader: TGLCustomGLSLShader);
    procedure MainViewerBeforeRender(Sender: TObject);
  end;

var
  GLDemoForm: TGLDemoForm;

implementation

{$R *.dfm}

procedure TGLDemoForm.FormCreate(Sender: TObject);
begin
  width := Screen.width;
  height := Screen.height;
  WindowState := wsMaximized;

  MainMaterialLibrary.TextureByName('MultisampledColor').ImageClassName := 'TGLMultisampleImage';
  TGLMultiSampleImage(MainMaterialLibrary.TextureByName('MultisampledColor').Image).SamplesCount := 16;

  MainMaterialLibrary.TextureByName('Depth').ImageClassName := 'TGLMultisampleImage';
  TGLMultiSampleImage(MainMaterialLibrary.TextureByName('Depth').Image).SamplesCount := 16;

  GLSLShader1.Enabled := True;

  GLHUDText1.Text := 'Press F1/F2 to switch multisampling' + #10#13 +
                     'F3/F4 - Wireframe mode';
end;

procedure TGLDemoForm.MainCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if IsKeyDown(VK_F2) then
  begin
    FBOContainer.Visible := false;
    GLScreenQuad.Visible := false;
    SceneObjects.Visible := true;
  end;

  if IsKeyDown(VK_F1) then
  begin
    FBOContainer.Visible := true;
    GLScreenQuad.Visible := true;
    SceneObjects.Visible := false;
  end;

  if IsKeyDown(VK_F3) then
  begin
    GLTorus1.Material.PolygonMode := pmLines;
    GLTorus2.Material.PolygonMode := pmLines;
    GLCone1.Material.PolygonMode := pmLines;
  end;

  if IsKeyDown(VK_F4) then
  begin
    GLTorus1.Material.PolygonMode := pmFill;
    GLTorus2.Material.PolygonMode := pmFill;
    GLCone1.Material.PolygonMode := pmFill;
  end;

  MainViewer.Invalidate();
end;

procedure TGLDemoForm.FormResize(Sender: TObject);
begin
  MultisampleFBO.Width := MainViewer.Width;
  MultisampleFBO.Height := MainViewer.Height;
  GLScreenQuad.Width  := MainViewer.Width;
  GLScreenQuad.Height := MainViewer.Height;
  GLScreenQuad.Position.SetPoint(
    MainViewer.Width div 2,
    MainViewer.Height div 2, 0);
end;

procedure TGLDemoForm.GLSLShader1Apply(Shader: TGLCustomGLSLShader);
begin
  Shader.Param['TexUnit0'].AsTexture[0] :=
     MainMaterialLibrary.TextureByName('MultisampledColor');
  Shader.Param['ViewerSize'].AsVector2f :=
     Vector2fMake(MainViewer.Width, MainViewer.Height);
end;

procedure TGLDemoForm.MainViewerBeforeRender(Sender: TObject);
begin
  if not gl.EXT_framebuffer_multisample then
  begin
    ShowMessage
      ('Sorry, your hardware do not support Multisampling');
    Close;
  end;
end;

end.
