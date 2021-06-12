unit OceanFm;

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
  Vcl.ComCtrls,

  GLS.OpenGLTokens,
  GLS.SceneViewer,
  GLS.Scene,
  GLS.Texture,
  GLS.Objects,
  GLS.Context,
  GLS.VectorGeometry,
  GLS.GeomObjects,
  GLS.Cadencer,
  GLSL.UserShader,
  GLS.Utils,
  GLS.Graph,
  GLS.VectorTypes,
  GLS.SkyDome,
  GLS.VectorLists,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.SimpleNavigation,
  GLS.TextureFormat,
  GLS.Color;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera: TGLCamera;
    MatLib: TGLMaterialLibrary;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    GLSphere1: TGLSphere;
    DOInitialize: TGLDirectOpenGL;
    GLUserShader1: TGLUserShader;
    GLHeightField1: TGLHeightField;
    GLMemoryViewer1: TGLMemoryViewer;
    GLScene2: TGLScene;
    CameraCubeMap: TGLCamera;
    GLEarthSkyDome1: TGLEarthSkyDome;
    GLSphere2: TGLSphere;
    DOOceanPlane: TGLDirectOpenGL;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure FormCreate(Sender: TObject);
    procedure DOInitializeRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLUserShader1DoApply(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLUserShader1DoUnApply(Sender: TObject; Pass: Integer;
      var rci: TGLRenderContextInfo; var Continue: Boolean);
    procedure GLHeightField1GetHeight(const x, y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
    procedure DOOceanPlaneRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLMemoryViewer1BeforeRender(Sender: TObject);
  public
    mx, my, dmx, dmy: Integer;
    programObject: TGLProgramHandle;
  end;

var
  Form1: TForm1;
  PathCM: TFileName;
  CubeMap: TGLTexture;


implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  CubeMap := TGLTexture.Create(Self);
  // Load the cube map which is used both for environment and as reflection texture
  MatLib.LibMaterialByName('water').Material.Texture.Image.LoadFromFile('noise.bmp');

  PathCM := GetCurrentDir() + '\Cubemaps';
  SetCurrentDir(PathCM);

//  Cubemap.ImageClassName := 'TGLCompositeImage';
//  Cubemap.Image.LoadFromFile('Cubemaps/Skybox.dds');
  Cubemap.TextureWrap := twNone;
  Cubemap.FilteringQuality := tfAnisotropic;
  Cubemap.Disabled := False;

  with MatLib.LibMaterialByName('cubeMap').Material.Texture do
  begin
    ImageClassName := TGLCubeMapImage.ClassName;
    with Image as TGLCubeMapImage do
    with Cubemap do
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
  SetGLSceneMediaDir();
end;

procedure TForm1.DoInitializeRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  if not (GL.ARB_shader_objects and
          GL.ARB_vertex_program and GL.ARB_vertex_shader
      and GL.ARB_fragment_shader) then
  begin
    ShowMessage('Your hardware/driver doesn''t support GLSL and can''t execute this demo!');
    Halt;
  end;

  if DOInitialize.Tag <> 0 then
    Exit;
  DOInitialize.Tag := 1;

  GLSceneViewer1.Buffer.RenderingContext.Deactivate;
  GLMemoryViewer1.RenderCubeMapTextures(matLib.LibMaterialByName('cubeMap').Material.Texture);
  GLSceneViewer1.Buffer.RenderingContext.Activate;

  programObject := TGLProgramHandle.CreateAndAllocate;

  programObject.AddShader(TGLVertexShaderHandle, String(LoadAnsiStringFromFile('Shaders\ocean_vp.glsl')), True);
  programObject.AddShader(TGLFragmentShaderHandle, String(LoadAnsiStringFromFile('Shaders\ocean_fp.glsl')), True);
  if not programObject.LinkProgram then
    raise Exception.Create(programObject.InfoLog);
  programObject.UseProgramObject;
  programObject.Uniform1i['NormalMap'] := 0;
  programObject.Uniform1i['EnvironmentMap'] := 1;
  programObject.EndUseProgramObject;
  // initialize the heightmap
  with MatLib.LibMaterialByName('water') do
    rci.GLStates.TextureBinding[0, ttTexture2D] := Material.Texture.Handle;
  // initialize the heightmap
  with MatLib.LibMaterialByName('cubeMap') do
    rci.GLStates.TextureBinding[1, ttTextureCube] := Material.Texture.Handle;
  if not programObject.ValidateProgram then
    raise Exception.Create(programObject.InfoLog);
end;

procedure TForm1.GLUserShader1DoApply(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  camPos: TGLVector;
begin
  programObject.UseProgramObject;

  programObject.Uniform1f['Time'] := GLCadencer1.CurrentTime * 0.05;

  camPos := GLCamera.AbsolutePosition;
  programObject.Uniform4f['EyePos'] := camPos;
end;

procedure TForm1.GLUserShader1DoUnApply(Sender: TObject; Pass: Integer;
  var rci: TGLRenderContextInfo; var Continue: Boolean);
begin
  programObject.EndUseProgramObject;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    Inc(dmx, mx - x);
    Inc(dmy, my - y);
  end;
  mx := x;
  my := y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if (dmx <> 0) or (dmy <> 0) then
  begin
    GLCamera.MoveAroundTarget(dmy * 0.3, dmx * 0.3);
    dmx := 0;
    dmy := 0;
  end;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLHeightField1GetHeight(const x, y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
begin
  z := 0;
end;

const
  cExtent = 200;
var
  vbo: TGLVBOArrayBufferHandle;
  nbVerts: Integer;

procedure TForm1.DOOceanPlaneRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  x, y: Integer;
  v: TTexPointList;
  cont: Boolean;
begin
  GLUserShader1DoApply(Self, rci);
  gl.EnableClientState(GL_VERTEX_ARRAY);
  if not Assigned(vbo) then
  begin
    v := TTexPointList.Create;
    v.Capacity := Sqr(cExtent + 1);
    y := -cExtent;
    while y < cExtent do
    begin
      x := -cExtent;
      while x <= cExtent do
      begin
        v.Add(y, x);
        v.Add(y + 2, x);
        Inc(x, 2);
      end;
      Inc(y, 2);
      v.Add(y, cExtent);
      v.Add(y, -cExtent);
    end;
    vbo := TGLVBOArrayBufferHandle.CreateAndAllocate();
    vbo.Bind;
    vbo.BufferData(v.List, v.DataSize, GL_STATIC_DRAW_ARB);
    nbVerts := v.Count;
    gl.VertexPointer(2, GL_FLOAT, 0, nil);
    gl.DrawArrays(GL_QUAD_STRIP, 0, nbVerts);
    vbo.UnBind;
    v.Free;
  end
  else
  begin
    vbo.Bind;
    gl.VertexPointer(2, GL_FLOAT, 0, nil);
    gl.DrawArrays(GL_TRIANGLE_STRIP, 0, nbVerts);
    vbo.UnBind;
  end;
  gl.DisableClientState(GL_VERTEX_ARRAY);
  GLUserShader1DoUnApply(Self, 0, rci, cont);
end;

procedure TForm1.GLMemoryViewer1BeforeRender(Sender: TObject);
begin
  GLMemoryViewer1.Buffer.RenderingContext.ShareLists(GLSceneViewer1.Buffer.RenderingContext);
  GLMemoryViewer1.BeforeRender := nil;
end;

end.

