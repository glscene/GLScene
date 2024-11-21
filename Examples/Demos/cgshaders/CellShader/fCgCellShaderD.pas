unit fCgCellShaderD;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Imaging.Jpeg,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  Cg.GL,

  GLS.Scene,
  Stage.VectorTypes,
  Stage.VectorGeometry,
  GLS.Objects,
  GLS.Cadencer,
  GLS.Texture,
  GLS.SceneViewer,
  GLS.VectorFileObjects,
  GLS.AsyncTimer,

  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  Stage.Utils,
  GLS.FileMD2,

  GLS.CgShader;

type
  TFormCellShading = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    CgCellShader: TCgShader;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLActor1: TGLActor;
    AsyncTimer1: TGLAsyncTimer;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure CgCellShaderApplyVP(CgProgram: TCgProgram; Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CgCellShaderInitialize(CgShader: TCustomCgShader);
    procedure CgCellShaderApplyFP(CgProgram: TCgProgram; Sender: TObject);
    procedure CgCellShaderUnApplyFP(CgProgram: TCgProgram);
    procedure AsyncTimer1Timer(Sender: TObject);
  public
    mx, my: Integer;
  end;

var
  FormCellShading: TFormCellShading;

implementation

{$R *.dfm}

procedure TFormCellShading.FormCreate(Sender: TObject);
var
  r: Single;
begin
  var Path: TFileName := GetCurrentAssetPath();

  // Load the vertex and fragment Cg programs from Shaders dir
  SetCurrentDir(Path + '\shader');
  CgCellShader.VertexProgram.LoadFromFile('cellshading_vp.cg');
  CgCellShader.FragmentProgram.LoadFromFile('cellshading_fp.cg');

  // Load and scale the aminated actor
  SetCurrentDir(Path + '\modelext');
  GLActor1.LoadFromFile('waste.md2');
  // Load the texture
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('wastecell.jpg');

  r := GLActor1.BoundingSphereRadius;
  GLActor1.Scale.SetVector(2.5 / r, 2.5 / r, 2.5 / r);
  GLActor1.AnimationMode := aamLoop;
end;

procedure TFormCellShading.CgCellShaderApplyVP(CgProgram: TCgProgram; Sender: TObject);
begin
  // Apply the per frame uniform parameters
  with CgProgram do
  begin
    ParamByName('LightDir').SetAsVector(GLLightSource1.AbsoluteDirection);
    ParamByName('ModelViewProj').SetAsStateMatrix(CG_GL_MODELVIEW_PROJECTION_MATRIX,
      CG_GL_MATRIX_IDENTITY);
    ParamByName('ModelViewIT').SetAsStateMatrix(CG_GL_MODELVIEW_MATRIX,
      CG_GL_MATRIX_INVERSE_TRANSPOSE);
  end;
end;

procedure TFormCellShading.CgCellShaderInitialize(CgShader: TCustomCgShader);
begin
  // Set up the texture sampler parameter
  CgCellShader.FragmentProgram.ParamByName('Map0')
    .SetAsTexture2D(GLMaterialLibrary1.Materials[0].Material.Texture.Handle);
end;

procedure TFormCellShading.CgCellShaderApplyFP(CgProgram: TCgProgram; Sender: TObject);
begin
  // Enable the texture map sampler for use in the fragment
  // program
  CgProgram.ParamByName('Map0').EnableTexture();
end;

procedure TFormCellShading.CgCellShaderUnApplyFP(CgProgram: TCgProgram);
begin
  // Disable the texture map sampler
  CgProgram.ParamByName('Map0').DisableTexture();
end;

procedure TFormCellShading.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormCellShading.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TFormCellShading.AsyncTimer1Timer(Sender: TObject);
begin
  FormCellShading.Caption := Format('Cg Cell Shader - %.2f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;

end;

end.
