unit u_Main;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Controls,

  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.Scene,
  GLS.VectorTypes,
  GLS.Objects,
  GLS.GeomObjects,
  GLS.Texture,
  GLS.TextureFormat,
  GLS.CompositeImage,
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  
  GLS.Context,
  GLS.RenderContextInfo,
  GLS.Utils,

  GLS.FileDDS;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    Vp: TGLSceneViewer;
    Cad: TGLCadencer;
    Cam: TGLCamera;
    DC_Cam: TGLDummyCube;
    Tor: TGLTorus;
    Sph: TGLSphere;
    Sky: TGLSphere;
    Dogl: TGLDirectOpenGL;
    procedure CadProgress(Sender: TObject; const DeltaTime, NewTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure DoglRender(Sender: TObject; var rci: TGLRenderContextInfo);
  public
    procedure LoadCubemap;
    procedure InitGLSL;
  end;

var
  Form1: TForm1;
  GLSL_Sky: TGLProgramHandle;
  GLSL_Obj: TGLProgramHandle;
  CubeMap: TGLTexture;

implementation

{$R *.dfm}

//
// setup
//
procedure TForm1.FormCreate;
begin
  loadCubemap;
  sky.Radius := cam.DepthOfView;
  sky.Material.Texture := cubemap;
  sph.Material.Texture := cubemap;
  tor.Material.Texture := cubemap;

end;

//
// LoadCubemap
//
procedure TForm1.LoadCubemap;
begin
  cubemap := TGLTexture.Create(self);
  with cubemap do
  begin
    ImageClassName := 'TGLCompositeImage';
    Image.LoadFromFile('data/sky.dds');
    TextureWrap := twNone;
    FilteringQuality := tfAnisotropic;
    disabled := false;
  end;
end;

//
// cadProgress
//
procedure TForm1.CadProgress;
begin
  dc_cam.Turn(deltaTime * 30);
  tor.Pitch(deltaTime * 50);
  cam.Position.Y := 3 * cos(NewTime / 3);
  Sky.AbsolutePosition := cam.AbsolutePosition;
end;

//
// doglRender
//
procedure TForm1.DoglRender;
begin
  if not cad.Enabled then
    initGLSL;
  // sky cubemap
  glsl_sky.UseProgramObject;
  sky.Render(rci);
  glsl_sky.EndUseProgramObject;

  // object cubemap
  glsl_obj.UseProgramObject;
  glsl_obj.UniformMatrix4fv['m4'] := sph.AbsoluteMatrix;
  sph.Render(rci);
  glsl_obj.UniformMatrix4fv['m4'] := tor.AbsoluteMatrix;
  tor.Render(rci);
  glsl_obj.EndUseProgramObject;
end;

//
// initGLSL
//
procedure TForm1.initGLSL;

  function load(vp, fp: String): TGLProgramHandle;
  begin
    result := TGLProgramHandle.CreateAndAllocate;
    result.AddShader(TGLVertexShaderHandle, LoadAnsiStringFromFile(vp));
    result.AddShader(TGLFragmentShaderHandle, LoadAnsiStringFromFile(fp));
    if not result.LinkProgram then
      raise Exception.Create(result.InfoLog);
    if not result.ValidateProgram then
      raise Exception.Create(result.InfoLog);
  end;

begin
  if not(GL.ARB_shader_objects and GL.ARB_vertex_program and
    GL.ARB_vertex_shader and GL.ARB_fragment_shader and GL.ARB_texture_cube_map)
  then
  begin
    ShowMessage('shader not supported by your hardware');
    Halt;
  end;

  cad.Enabled := true;

  glsl_sky := load('data/sky.vp', 'data/sky.fp');
  glsl_obj := load('data/obj.vp', 'data/obj.fp');

end;

end.
