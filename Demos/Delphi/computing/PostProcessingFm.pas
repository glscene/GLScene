unit PostProcessingFm;

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
  Vcl.ComCtrls,

  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.Coordinates,
  GLS.Objects,
  GLS.GeomObjects,
  GLS.Material,
  GLS.SimpleNavigation,
  GLS.Cadencer,
  GLS.SceneViewer,
 
  GLS.BaseClasses,
  GLS.FBORenderer,
  GLS.HUDObjects,

  CUDA.Api,
  CUDA.Graphics,
  CUDA.Compiler,
  CUDA.Context,

  GLS.State,
  GLS.RenderContextInfo,
  GLS.Context,
  GLSL.CustomShader,
  GLSL.Shader,
  GLS.Texture;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    GLTeapot1: TGLTeapot;
    GLLightSource1: TGLLightSource;
    RenderRoot: TGLDummyCube;
    GLCylinder1: TGLCylinder;
    RenderToTexture: TGLFBORenderer;
    GLCUDADevice1: TGLCUDADevice;
    GLCUDA1: TGLCUDA;
    GLCUDACompiler1: TGLCUDACompiler;
    MainModule: TCUDAModule;
    processedTextureMapper: TCUDAImageResource;
    CallPostProcess: TGLDirectOpenGL;
    GLCapsule1: TGLCapsule;
    ResultShader: TGLSLShader;
    processedTextureArray: TCUDAMemData;
    outputBuffer: TCUDAMemData;
    inputBuffer: TCUDAMemData;
    CommonShader: TGLSLShader;
    GLSphere1: TGLSphere;
    TrackBar1: TTrackBar;
    GLHUDSprite1: TGLHUDSprite;
    cudaProcess: TCUDAFunction;
    cudaProcess_k_g_data: TCUDAFuncParam;
    cudaProcess_k_g_odata: TCUDAFuncParam;
    cudaProcess_k_imgw: TCUDAFuncParam;
    cudaProcess_k_imgh: TCUDAFuncParam;
    cudaProcess_k_tilew: TCUDAFuncParam;
    cudaProcess_k_r: TCUDAFuncParam;
    cudaProcess_k_threshold: TCUDAFuncParam;
    cudaProcess_k_highlight: TCUDAFuncParam;
    procedure FormResize(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure cudaProcessParameterSetup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CallPostProcessRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure ResultShaderApply(Shader: TGLCustomGLSLShader);
    procedure RenderToTextureBeforeRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure RenderToTextureAfterRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure TrackBar1Change(Sender: TObject);
    procedure GLCUDA1OpenGLInteropInit(out Context: TGLContext);
  private

  public
    Radius: Integer;
    Threshold: Single;
    Highlight: SIngle;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Radius := 8;
  Threshold := 0.8;
  Highlight := 0.4;
  with GLMaterialLibrary1.TextureByName('processedTexture') do
  begin
    TGLBlankImage(Image).ColorFormat := GL_RGB_INTEGER;
    Disabled := false;
  end;
  GLHUDSprite1.Visible := True;
end;

procedure TForm1.CallPostProcessRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  processedTextureMapper.MapResources;
  processedTextureMapper.BindArrayToTexture(processedTextureArray, 0, 0);
  processedTextureArray.CopyTo(inputBuffer);
  cudaProcess.Launch;
  outputBuffer.CopyTo(processedTextureArray);
  processedTextureMapper.UnMapResources;
end;

procedure TForm1.cudaProcessParameterSetup(Sender: TObject);
begin
  with cudaProcess do
  begin
    SharedMemorySize :=
      (BlockShape.SizeX+(2*Radius))*(BlockShape.SizeY+(2*Radius))*sizeof(Integer);
    SetParam(inputBuffer);
    SetParam(outputBuffer);
    with GLMaterialLibrary1.TextureByName('processedTexture') do
    begin
      SetParam(TexWidth);
      SetParam(TexHeight);
    end;
    SetParam(BlockShape.SizeX + 2*Radius);
    SetParam(Radius);
    SetParam(Threshold);
    SetParam(Highlight);
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  GLCamera1.SceneScale := GLSceneViewer1.Width / GLSceneViewer1.Height;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.RenderToTextureBeforeRender(Sender: TObject; var rci: TGLRenderContextInfo);
begin
  CommonShader.Apply(rci, Self);
end;

procedure TForm1.GLCUDA1OpenGLInteropInit(out Context: TGLContext);
begin
  Context := GLSceneViewer1.Buffer.RenderingContext;
end;

procedure TForm1.RenderToTextureAfterRender(Sender: TObject; var rci: TGLRenderContextInfo);
begin
  CommonShader.UnApply(rci);
end;

procedure TForm1.ResultShaderApply(Shader: TGLCustomGLSLShader);
begin
  with CurrentGLContext.GLStates do
  begin
    Disable(stDepthTest);
    DepthWriteMask := False;
  end;
  Shader.Param['TexUnit0'].AsTexture[0] :=
    GLMaterialLibrary1.TextureByName('processedTexture');
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  Radius := TrackBar1.Position;
end;

end.
