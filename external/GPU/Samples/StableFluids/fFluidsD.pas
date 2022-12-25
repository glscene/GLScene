unit fFluidsD;

interface

uses
  System.SysUtils, 
  System.Classes,
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.StdCtrls, 
  Vcl.Graphics,

  GLS.Scene,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.BaseClasses,
  GLS.Coordinates,
  GLS.Context,
  GLS.Gui,
  GLS.BitmapFont,
  GLS.WindowsFont,
  GLSL.CustomShader,
  GLS.RenderContextInfo,
  GLS.State,
  GLS.Material,
  GLSL.Shader,
  GLS.Windows,
  GLSL.ShaderCombiner,


  CUDA.Context,
  CUDA.APIComps,
  CUDA.Compiler,
  CUDA.FFTPlan,
  CUDA.Graphics,
  CUDA.DataAccess;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLCUDADevice1: TGLCUDADevice;
    GLCUDA1: TGLCUDA;
    GLCUDACompiler1: TGLCUDACompiler;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    clicked: Boolean;
    lastx: Integer;
    lasty: Integer;
    ComplexPadWidth: Integer;
    RealPadWidth: Integer;
    PaddedDomainSize: Integer;
    SpeedX: Integer;
    SpeedY: Integer;
    ForceX: Single;
    ForceY: Single;
  public
    MainModule: TCUDAModule;
    ArrayOfTexture: TCUDAMemData;
    TextureOfVelocityField: TCUDATexture;
    VelocityField: TCUDAMemData;
    ComplexVXField: TCUDAMemData;
    ComplexVYField: TCUDAMemData;
    InitialPosition: TCUDAMemData;
    FluidShader: TGLSLShader;
    GLMaterialLibrary1: TGLMaterialLibrary;
    ForwardFFT: TCUDAFFTPlan;
    InverseFFT: TCUDAFFTPlan;
    ParticleMapper: TCUDAGeometryResource;
    ResetButton: TGLButton;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    GLGuiLayout1: TGLGuiLayout;
    ParticleRenderer: TCUDAFeedbackMesh;
    addForces: TCUDAFunction;
    advectVelocity: TCUDAFunction;
    diffuseProject: TCUDAFunction;
    updateVelocity: TCUDAFunction;
    advectParticles: TCUDAFunction;
    addForces_k_v: TCUDAFuncParam;
    addForces_k_dx: TCUDAFuncParam;
    addForces_k_dy: TCUDAFuncParam;
    addForces_k_spx: TCUDAFuncParam;
    addForces_k_spy: TCUDAFuncParam;
    addForces_k_fx: TCUDAFuncParam;
    addForces_k_fy: TCUDAFuncParam;
    addForces_k_r: TCUDAFuncParam;
    addForces_k_pitch: TCUDAFuncParam;
    advectVelocity_k_vx: TCUDAFuncParam;
    advectVelocity_k_vy: TCUDAFuncParam;
    advectVelocity_k_dx: TCUDAFuncParam;
    advectVelocity_k_pdx: TCUDAFuncParam;
    advectVelocity_k_dy: TCUDAFuncParam;
    advectVelocity_k_dt: TCUDAFuncParam;
    advectVelocity_k_lb: TCUDAFuncParam;
    diffuseProject_k_vx: TCUDAFuncParam;
    diffuseProject_k_vy: TCUDAFuncParam;
    diffuseProject_k_dx: TCUDAFuncParam;
    diffuseProject_k_dy: TCUDAFuncParam;
    diffuseProject_k_dt: TCUDAFuncParam;
    diffuseProject_k_visc: TCUDAFuncParam;
    diffuseProject_k_lb: TCUDAFuncParam;
    updateVelocity_k_v: TCUDAFuncParam;
    updateVelocity_k_vx: TCUDAFuncParam;
    updateVelocity_k_vy: TCUDAFuncParam;
    updateVelocity_k_dx: TCUDAFuncParam;
    updateVelocity_k_pdx: TCUDAFuncParam;
    updateVelocity_k_dy: TCUDAFuncParam;
    updateVelocity_k_lb: TCUDAFuncParam;
    updateVelocity_k_pitch: TCUDAFuncParam;
    updateVelocity_k_scale: TCUDAFuncParam;
    advectParticles_k_part: TCUDAFuncParam;
    advectParticles_k_v: TCUDAFuncParam;
    advectParticles_k_dx: TCUDAFuncParam;
    advectParticles_k_dy: TCUDAFuncParam;
    advectParticles_k_dt: TCUDAFuncParam;
    advectParticles_k_lb: TCUDAFuncParam;
    advectParticles_k_pitch: TCUDAFuncParam;

    ParticlesDim: Integer;
    DeltaTime: Single;
    ViscosityConst: Single;
    ForceScaleFactor: Single;
    ForceUpdateRadius: Integer;
    ParticlesPerThread: Integer;

    procedure addForcesParameterSetup(Sender: TObject);
    procedure advectParticlesParameterSetup(Sender: TObject);
    procedure advectVelocityParameterSetup(Sender: TObject);
    procedure updateVelocityParameterSetup(Sender: TObject);
    procedure diffuseProjectParameterSetup(Sender: TObject);
    procedure FluidShaderApply(Shader: TGLCustomGLSLShader);
    procedure ResetButtonButtonClick(Sender: TObject);
    procedure BeforeKernelLaunch(Sender: TGLVertexAttribute);
    procedure GLCUDA1OpenGLInteropInit(out Context: TGLContext);

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  InitPosition : Boolean = False;

procedure TForm1.FormCreate(Sender: TObject);
var
  i, j: Integer;
  pos: FloatElement.TVector2;
begin
  ParticlesDim := 512;
  ComplexPadWidth := ParticlesDim div 2 + 1;
  RealPadWidth := 2 * ComplexPadWidth;
  PaddedDomainSize := ParticlesDim * ComplexPadWidth;
  ViscosityConst := 0.0025;
  ForceScaleFactor := 5.8 * ParticlesDim;
  ForceUpdateRadius := 4;
  ParticlesPerThread := 16;

  ComplexVXField.Width := PaddedDomainSize;
  ComplexVYField.Width := PaddedDomainSize;

  advectVelocity.BlockShape.SizeX := 64;
  advectVelocity.BlockShape.SizeY := 4;
  advectVelocity.Grid.SizeX := 8;
  advectVelocity.Grid.SizeY := 8;

  diffuseProject.BlockShape.SizeX := 64;
  diffuseProject.BlockShape.SizeY := 4;
  diffuseProject.Grid.SizeX := 8;
  diffuseProject.Grid.SizeY := 8;

  updateVelocity.BlockShape.SizeX := 64;
  updateVelocity.BlockShape.SizeY := 4;
  updateVelocity.Grid.SizeX := 8;
  updateVelocity.Grid.SizeY := 8;

  advectParticles.BlockShape.SizeX := 64;
  advectParticles.BlockShape.SizeY := 4;
  advectParticles.Grid.SizeX := 8;
  advectParticles.Grid.SizeY := 8;

  // Create initial position data at host side
  for i := 0 to InitialPosition.Height - 1 do
    for j := 0 to InitialPosition.Width - 1 do
    begin
      pos[0] := ((j + 0.5) / InitialPosition.Width) + (random - 0.5) /
        InitialPosition.Width;
      pos[1] := ((i + 0.5) / InitialPosition.Height) + (random - 0.5) /
        InitialPosition.Height;
      InitialPosition.Data<Single>(j, i).Vector2 := pos;
    end;

  ParticleRenderer.VertexNumber := ParticlesDim * ParticlesDim;
  clicked := false;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  lastX := X;
  lastY := Y;
  clicked := true;
  ResetButton.MouseDown(Sender, TMouseButton(Button), Shift, X, Y);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  nx, ny: Integer;
  ddx, ddy: Integer;
begin
  // Convert motion coordinates to domain
  ForceX := lastx / GLSceneViewer1.Width;
  ForceY := lasty / GLSceneViewer1.Height;
  nx := Round(ForceX * InitialPosition.Width);
  ny := Round(ForceY * InitialPosition.Height);

  if clicked and (nx < InitialPosition.Width - ForceUpdateRadius)
    and (nx > ForceUpdateRadius - 1)
    and (ny < InitialPosition.Height - ForceUpdateRadius)
    and (ny > ForceUpdateRadius - 1) then
  begin
    ddx := X - lastx;
    ddy := Y - lasty;
    SpeedX := nx - ForceUpdateRadius;
    SpeedY := ny - ForceUpdateRadius;
    ForceX := DeltaTime * ForceScaleFactor * (ddx / GLSceneViewer1.Width);
    ForceY := DeltaTime * ForceScaleFactor * (ddy / GLSceneViewer1.Height);
    addForces.Launch(false);
    lastx := X;
    lasty := Y;
  end;

  ResetButton.MouseMove(Sender, Shift, X, Y);
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  clicked := false;
  ResetButton.MouseUp(Sender, TMouseButton(Button), Shift, X, Y);
end;

procedure TForm1.GLCUDA1OpenGLInteropInit(out Context: TGLContext);
begin
  Context := GLSceneViewer1.Buffer.RenderingContext;
end;

procedure TForm1.BeforeKernelLaunch(
  Sender: TGLVertexAttribute);
begin
  if not InitPosition then
  begin
    InitialPosition.CopyTo(
      ParticleMapper,
      ParticleRenderer.Attributes[0].Name);
    VelocityField.FillMem(NullVector);
    InitPosition := true;
  end;
  // Simulate fluid
  advectVelocity.Launch;
  ForwardFFT.Execute(ComplexVXField, ComplexVXField);
  ForwardFFT.Execute(ComplexVYField, ComplexVYField);
  diffuseProject.Launch;
  InverseFFT.Execute(ComplexVXField, ComplexVXField);
  InverseFFT.Execute(ComplexVYField, ComplexVYField);
  updateVelocity.Launch;
  // advectParticles will be launched automaticaly by ParticleRenderer
  // Look at ParticleRenderer.VertexAttributes[0].KernelFunction
end;

procedure TForm1.addForcesParameterSetup(Sender: TObject);
begin
  with addForces do
  begin
    BlockShape.SizeX := 2 * ForceUpdateRadius + 1;
    BlockShape.SizeY := 2 * ForceUpdateRadius + 1;
    SetParam(VelocityField);
    SetParam(ParticlesDim);
    SetParam(ParticlesDim);
    SetParam(SpeedX);
    SetParam(SpeedY);
    SetParam(ForceX);
    SetParam(ForceY);
    SetParam(ForceUpdateRadius);
    SetParam(VelocityField.Pitch);
  end;
end;

procedure TForm1.advectVelocityParameterSetup(Sender: TObject);
begin
  VelocityField.CopyTo(ArrayOfTexture);
  with advectVelocity do
  begin
    SetParam(ComplexVXField);
    SetParam(ComplexVYField);
    SetParam(ParticlesDim);
    SetParam(RealPadWidth);
    SetParam(ParticlesDim);
    SetParam(DeltaTime);
    SetParam(ParticlesPerThread);
    // For texture parameter order does not matter
    SetParam(TextureOfVelocityField);
  end;
end;

procedure TForm1.diffuseProjectParameterSetup(Sender: TObject);
begin
  with diffuseProject do
  begin
    SetParam(ComplexVXField);
    SetParam(ComplexVYField);
    SetParam(ComplexPadWidth);
    SetParam(ParticlesDim);
    SetParam(DeltaTime);
    SetParam(ViscosityConst);
    SetParam(ParticlesPerThread);
  end;
end;

procedure TForm1.updateVelocityParameterSetup(Sender: TObject);
begin
  with updateVelocity do
  begin
    SetParam(VelocityField);
    SetParam(ComplexVXField);
    SetParam(ComplexVYField);
    SetParam(ParticlesDim);
    SetParam(RealPadWidth);
    SetParam(ParticlesDim);
    SetParam(ParticlesPerThread);
    SetParam(VelocityField.Pitch);
    SetParam(1.0 / (ParticlesDim * ParticlesDim));
  end;
end;

procedure TForm1.advectParticlesParameterSetup(Sender: TObject);
begin
  with advectParticles do
  begin
    SetParam(ParticleMapper.AttributeDataAddress[ParticleRenderer.Attributes[0].Name]);
    SetParam(VelocityField);
    SetParam(ParticlesDim);
    SetParam(ParticlesDim);
    SetParam(DeltaTime);
    SetParam(ParticlesPerThread);
    SetParam(VelocityField.Pitch);
  end;
end;

procedure TForm1.ResetButtonButtonClick(Sender: TObject);
begin
  InitPosition := false;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const DeltaTime,
  newTime: Double);
begin
  Self.DeltaTime := 5*DeltaTime;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FluidShaderApply(Shader: TGLCustomGLSLShader);
begin
  with CurrentGLContext.GLStates do
  begin
    Enable(stPointSmooth);
    Enable(stBlend);
    Disable(stCullFace);
    Disable(stDepthTest);
    PointSize := 1;
    SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
  end;
end;

end.

