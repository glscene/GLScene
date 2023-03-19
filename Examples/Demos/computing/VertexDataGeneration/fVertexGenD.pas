unit fVertexGenD;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.PipelineTransformation,
  GLS.Cadencer,
  GLS.SceneViewer,
 
  GLS.BaseClasses,
  GLS.SimpleNavigation,
  GLS.Objects,
  GLS.Coordinates,
  GLS.Context,

  CUDA.APIComps,
  CUDA.Compiler,
  CUDA.Context,
  CUDA.Graphics,

  GLS.Material,
  GLSL.CustomShader,
  GLSL.Shader;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLCUDADevice1: TGLCUDADevice;
    GLCUDA1: TGLCUDA;
    GLCUDACompiler1: TGLCUDACompiler;
    MainModule: TCUDAModule;
    DotFieldMapper: TCUDAGeometryResource;
    GLSLShader1: TGLSLShader;
    MakeDotField: TCUDAFunction;
    CUDAFeedbackMesh1: TCUDAFeedbackMesh;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure MakeVertexBufferParameterSetup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLSLShader1Apply(Shader: TGLCustomGLSLShader);
    procedure GLCUDA1OpenGLInteropInit(out Context: TGLContext);
  public
    FieldWidth: Integer;
    FieldHeight: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FieldWidth := 256;
  FieldHeight := 256;
  CUDAFeedbackMesh1.VertexNumber := FieldWidth * FieldHeight;
  CUDAFeedbackMesh1.Visible := True;
  MakeDotField.Grid.SizeX := FieldWidth div MakeDotField.BlockShape.SizeX;
  MakeDotField.Grid.SizeY := FieldWidth div MakeDotField.BlockShape.SizeY;
end;

procedure TForm1.GLCUDA1OpenGLInteropInit(out Context: TGLContext);
begin
  Context := GLSceneViewer1.Buffer.RenderingContext;
end;

procedure TForm1.GLSLShader1Apply(Shader: TGLCustomGLSLShader);
begin
  with GLSceneViewer1.Buffer.RenderingContext.PipelineTransformation do
    Shader.Param['ModelViewProjectionMatrix'].AsMatrix4f :=
      MatrixMultiply(ModelViewMatrix^, ProjectionMatrix^);
end;

procedure TForm1.MakeVertexBufferParameterSetup(Sender: TObject);
begin
  with MakeDotField do
  begin
    SetParam(DotFieldMapper.AttributeDataAddress[CUDAFeedbackMesh1.Attributes[0].Name]);
    SetParam(FieldWidth);
    SetParam(FieldHeight);
    SetParam(GLCadencer1.CurrentTime);
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

end.
