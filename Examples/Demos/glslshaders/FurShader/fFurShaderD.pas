unit fFurShaderD;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Imaging.jpeg,
  GLS.Texture,
  GLS.FileTGA,
  GLS.VectorLists,
  VectorTypes,
  Stage.Utils,
  GLS.Context,
  GLS.FileOBJ,
  Stage.VectorGeometry,
  Stage.TextureFormat
  GLS.XOpenGL,
  GLS.Graphics,
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.Material,
  GLS.Cadencer,
  GLS.Scene,
  GLS.VectorFileObjects,
  GLS.Objects,
  GLS.Coordinates,
  GLS.SceneViewer;

type
  TFormFur = class(TForm)
    GLScene1: TGLScene;
    GLViewer: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    Camera: TGLCamera;
    Scene: TGLDummyCube;
    Timer1: TTimer;
    CamBox: TGLDummyCube;
    MatLib: TGLMaterialLibrary;
    RenderDirectGL: TGLDirectOpenGL;
    GLLightSource1: TGLLightSource;
    SceneMesh: TGLFreeForm;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
    procedure GLViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure GLViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint; var Handled: Boolean);
    procedure RenderDirectGLRender(Sender: TObject; var rci: TGLRenderContextInfo);
  private
    Path: TFileName;
    mx, my: Integer;
    InitDGL: Boolean;
    AltDGL: Boolean;
    GLSLProg: TGLProgramHandle;
    procedure PrepareTextures;
    procedure DummyRender(dummy: TGLDummyCube; rci: TGLRenderContextInfo);
  public
  end;

var
  FormFur: TFormFur;

implementation

{$R *.dfm}

procedure TFormFur.PrepareTextures;
begin
  SetCurrentDir(Path + '\texture');
  with MatLib.LibMaterialByName('Fur').Material.Texture do
  begin
    Image.LoadFromFile('fur.tga');
  end;
  with MatLib.LibMaterialByName('FurColor').Material.Texture do
  begin
    Image.LoadFromFile('rainbowfilm_smooth.jpg');
  end;
end;

procedure TFormFur.FormCreate(Sender: TObject);
begin
  Path := GetCurrentAssetPath();
  SetCurrentDir(Path + '\model');
  InitDGL := False;
  AltDGL := False;
  GLViewer.Buffer.FaceCulling := False;
  SceneMesh.LoadFromFile('Torus.obj');
  GLCadencer1.Enabled := True;
  Timer1.Enabled := True;
end;

procedure TFormFur.DummyRender(dummy: TGLDummyCube; rci: TGLRenderContextInfo);
var
  i: Integer;
begin
  if (dummy.Count > 0) then
  begin
    for i := 0 to dummy.Count - 1 do
    begin
      if TGLSceneObject(dummy.Children[i]).Tag <> 1 then
      begin
        if dummy.Children[i].Visible then
          dummy.Children[i].Visible := False;
        gl.PushMatrix();
        gl.MultMatrixf(PGLFloat(TGLSceneObject(dummy.Children[i]).AbsoluteMatrixAsAddress));
        if dummy.Children[i].Count > 0 then
          dummy.Children[i].DoRender(rci, True, True)
        else
          dummy.Children[i].DoRender(rci, True, False);
        gl.PopMatrix;
      end;
    end;
  end;
end;

procedure TFormFur.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  SceneMesh.Pitch(0.5);
  GLViewer.Invalidate;
end;

procedure TFormFur.GLViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormFur.GLViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (ssright in Shift) then
    Camera.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TFormFur.Timer1Timer(Sender: TObject);
begin
  Caption := Format('Fur Shader. [%.2f] FPS', [GLViewer.FramesPerSecond]);
  GLViewer.ResetPerformanceMonitor;
end;

procedure TFormFur.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AltDGL := True;
  InitDGL := False;
  GLCadencer1.Enabled := False;
  Timer1.Enabled := False;
  GLSLProg.Free;
end;

procedure TFormFur.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TFormFur.RenderDirectGLRender(Sender: TObject; var rci: TGLRenderContextInfo);
var
  v: TVector4f;
begin
  if (not InitDGL) then
  begin
    (*
      if (not(GL_ARB_shader_objects and GL_ARB_vertex_program and GL_ARB_vertex_shader and
      GL_ARB_fragment_shader)) then
      begin
      ShowMessage('Your hardware/driver doesn''t support GLSL and can''t execute this demo!');
      Halt;
      end;
    *)
    SetCurrentDir(Path + '\shader');
    GLSLProg := TGLProgramHandle.CreateAndAllocate;
    GLSLProg.AddShader(TGLVertexShaderHandle, LoadAnsiStringFromFile('Fur_vp.glsl'));
    GLSLProg.AddShader(TGLFragmentShaderHandle,
      LoadAnsiStringFromFile('Fur_fp.glsl'));
    if (not GLSLProg.LinkProgram) then
      raise Exception.Create(GLSLProg.InfoLog);
    if (not GLSLProg.ValidateProgram) then
      raise Exception.Create(GLSLProg.InfoLog);
    gl.CheckError;
    PrepareTextures;
    InitDGL := True;
  end;
  v.X := 0.2196;
  v.Y := 0.2202;
  v.Z := 0.2202;
  v.W := 1.0000;
  if (InitDGL) and (not AltDGL) then
  begin
    with GLSLProg do
    begin
      UseProgramObject;
      gl.ActiveTexture(GL_TEXTURE0_ARB);
      gl.BindTexture(GL_TEXTURE_2D, MatLib.Materials[0].Material.Texture.Handle);
      gl.ActiveTexture(GL_TEXTURE1_ARB);
      gl.BindTexture(GL_TEXTURE_2D, MatLib.Materials[1].Material.Texture.Handle);
      Uniform1f['shell_distance'] := 0.30000;
      Uniform1f['pass_index'] := 0;
      Uniform4f['furColorScale'] := v;
      Uniform1i['FurColor'] := 1;
      Uniform1i['Fur'] := 0;
      DummyRender(Scene, rci);

      gl.Enable(GL_BLEND);
      gl.BlendFunc(GL_SRC_ALPHA, GL_ONE);
      gl.BlendFunc(GL_DST_ALPHA, GL_ONE);
      Uniform1f['pass_index'] := 1;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 2;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 3;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 4;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 5;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 6;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 7;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 8;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 9;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 10;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 11;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 12;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 13;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 14;
      DummyRender(Scene, rci);

      Uniform1f['pass_index'] := 15;
      DummyRender(Scene, rci);
      EndUseProgramObject;
    end;
  end;
end;

end.
