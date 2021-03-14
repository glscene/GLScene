unit ShadowFBOFm;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,

  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.VectorTypes,
  GLS.Context,
  GLS.Objects,
  GLS.HUDObjects,
  GLS.Material,
  GLS.Texture,
  GLS.SceneViewer,
  GLS.GeomObjects,
  GLS.FBORenderer,
  GLS.Cadencer,
  GLSL.CustomShader,
  GLSL.Shader,
  GLS.VectorGeometry,
  GLS.Coordinates,
 
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.SimpleNavigation,
  GLS.VectorFileObjects,
  GLS.PipelineTransformation,
  GLS.FileMD2,
  Formats.DDSImage,
  GLS.FileOBJ,
  GLS.Graphics,
  GLS.State,
  GLS.Utils;

type
  TFormShadowFBO = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    GLShadowTextureSprite: TGLHUDSprite;
    GLCamera2: TGLCamera;
    SceneRoot: TGLDummyCube;
    GLPlane1: TGLPlane;
    GLTorus1: TGLTorus;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    GLCylinder1: TGLCylinder;
    GLSLShader1: TGLSLShader;
    GLSLShader2: TGLSLShader;
    PrepareShadowMapping: TGLDirectOpenGL;
    GLNavigation: TGLSimpleNavigation;
    GLFreeForm1: TGLFreeForm;
    LightFBORenderer: TGLFBORenderer;
    GLSphere1: TGLSphere;
    procedure GLSLShader2Initialize(Shader: TGLCustomGLSLShader);
    procedure GLSLShader1UnApply(Shader: TGLCustomGLSLShader; var ThereAreMorePasses: Boolean);
    procedure FormResize(Sender: TObject);
    procedure PrepareShadowMappingRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure GLSLShader2Apply(Shader: TGLCustomGLSLShader);
    procedure GLSLShader1Apply(Shader: TGLCustomGLSLShader);
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure LightFBORendererBeforeRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure LightFBORendererAfterRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
  private
     

    FBiasMatrix: TGLMatrix;
    FLightModelViewMatrix: TGLMatrix;
    FLightProjMatrix: TGLMatrix;
    FInvCameraMatrix: TGLMatrix;
    FEyeToLightMatrix: TGLMatrix;
  public
     
  end;

var
  FormShadowFBO: TFormShadowFBO;

implementation

{$R *.dfm}

procedure TFormShadowFBO.PrepareShadowMappingRender(Sender: TObject; var rci: TGLRenderContextInfo);
begin
  // prepare shadow mapping matrix
  FInvCameraMatrix := rci.PipelineTransformation.InvModelViewMatrix^;
  // go from eye space to light's "eye" space
  FEyeToLightMatrix := MatrixMultiply(FInvCameraMatrix, FLightModelViewMatrix);
  // then to clip space
  FEyeToLightMatrix := MatrixMultiply(FEyeToLightMatrix, FLightProjMatrix);
  // and finally make the [-1..1] coordinates into [0..1]
  FEyeToLightMatrix := MatrixMultiply(FEyeToLightMatrix, FBiasMatrix);
end;

procedure TFormShadowFBO.FormCreate(Sender: TObject);
begin
  // Loading textures
  SetGLSceneMediaDir();
  with GLMaterialLibrary1 do
  begin

    with TextureByName('Chekers') do
    begin
      Image.LoadFromFile('marbletiles.jpg');
      Disabled := false;
    end;

    with TextureByName('Chekers2') do
    begin
      Image.LoadFromFile('Concrete.jpg');
      Disabled := false;
    end;

    with TextureByName('Lightspot') do
    begin
      Image.LoadFromFile('Flare1.bmp');
      Disabled := false;
    end;

    with TextureByName('bark') do
    begin
      Image.LoadFromFile('waste.jpg');
      Disabled := false;
    end;

    with TextureByName('mask') do
    begin
      Image.LoadFromFile('Masks.dds');
      Disabled := false;
    end;

  end;

  // Loading models
  GLFreeForm1.LoadFromFile('waste.md2');
  GLFreeForm1.Scale.Scale(0.05);
  GLFreeForm1.Position.Y := GLPlane1.Position.Y + 0.6;

  FBiasMatrix := CreateScaleAndTranslationMatrix(VectorMake(0.5, 0.5, 0.5), VectorMake(0.5, 0.5, 0.5));

  // Loading shader
  GLSLShader1.VertexProgram.LoadFromFile('Shaders\shadowmap_vp.glsl');
  GLSLShader1.FragmentProgram.LoadFromFile('Shaders\shadowmapvis_fp.glsl');
  GLSLShader1.Enabled := true;

  GLSLShader2.VertexProgram.LoadFromFile('Shaders\shadowmap_vp.glsl');
  GLSLShader2.FragmentProgram.LoadFromFile('Shaders\shadowmap_fp.glsl');
  GLSLShader2.Enabled := true;
end;

procedure TFormShadowFBO.FormResize(Sender: TObject);
begin
  GLSceneViewer1.Camera.SceneScale := GLSceneViewer1.ClientWidth / 400;
end;

procedure TFormShadowFBO.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLTorus1.Turn(deltaTime * 25);
  GLCylinder1.turn(deltaTime * 50);
  GLCamera2.Position.Rotate(VectorMake(0, 1, 0), deltaTime * 0.1);
end;

procedure TFormShadowFBO.GLSceneViewer1BeforeRender(Sender: TObject);
begin
  if not (GLSceneViewer1.Buffer.RenderingContext.gl.EXT_framebuffer_object) then
  begin
    ShowMessage('Sorry, this demo requires GL_EXT_framebuffer_object and either');
    Close;
  end;
end;

procedure TFormShadowFBO.GLSLShader1Apply(Shader: TGLCustomGLSLShader);
begin
  Shader.Param['ShadowMap'].AsTexture2D[0] := GLMaterialLibrary1.TextureByName(LightFBORenderer.DepthTextureName);
  // set compare to none so we can read off the depth value directly
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_NONE);
end;

procedure TFormShadowFBO.GLSLShader1UnApply(Shader: TGLCustomGLSLShader; var ThereAreMorePasses: Boolean);
begin
  // reset the compare mode to default
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_COMPARE_R_TO_TEXTURE_ARB);
end;

procedure TFormShadowFBO.GLSLShader2Apply(Shader: TGLCustomGLSLShader);
begin
  with Shader, GLMaterialLibrary1 do
  begin
    Param['ShadowMap'].AsTexture2D[1] := TextureByName(LightFBORenderer.DepthTextureName);
    Param['LightspotMap'].AsTexture2D[2] := TextureByName('Lightspot');
    Param['Scale'].AsFloat := 16.0;
    Param['Softly'].AsInteger := 1;
    Param['EyeToLightMatrix'].AsMatrix4f := FEyeToLightMatrix;
  end;
end;

procedure TFormShadowFBO.GLSLShader2Initialize(Shader: TGLCustomGLSLShader);
begin
  with Shader, GLMaterialLibrary1 do
  begin
    Param['TextureMap'].AsTexture2D[0] := TextureByName('Chekers2');
    Param['ShadowMap'].AsTexture2D[1] := TextureByName(LightFBORenderer.DepthTextureName);
    Param['LightspotMap'].AsTexture2D[2] := TextureByName('Lightspot');
  end;
end;

procedure TFormShadowFBO.LightFBORendererBeforeRender(Sender: TObject; var rci: TGLRenderContextInfo);
begin
  // get the modelview and projection matrices from the light's "camera"
  with rci.PipelineTransformation do
  begin
    FLightModelViewMatrix := ModelViewMatrix^;
    FLightProjMatrix := ProjectionMatrix^;
  end;

  // push geometry back a bit, prevents false self-shadowing
  with rci.GLStates do
  begin
    Enable(stPolygonOffsetFill);
    PolygonOffsetFactor := 2;
    PolygonOffsetUnits := 2;
  end;
end;

procedure TFormShadowFBO.LightFBORendererAfterRender(Sender: TObject; var rci: TGLRenderContextInfo);
begin
  rci.GLStates.Disable(stPolygonOffsetFill);
end;

end.

