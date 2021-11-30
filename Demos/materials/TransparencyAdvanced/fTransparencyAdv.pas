unit fTransparencyAdv;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs,
  Vcl.Imaging.Jpeg,

  GLS.OpenGLTokens,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.Context,
  GLS.State,
  GLS.Color,
  GLS.Scene,
  GLS.Objects,
  GLS.Coordinates,
  GLS.FileJPEG,
  GLS.SimpleNavigation,
  GLS.Material,
  GLS.Cadencer,
  GLS.BaseClasses,
  GLS.SceneViewer,
  GLSL.Shader,
  GLSL.CustomShader,
  GLS.Texture,
  GLS.FBORenderer,
  GLS.RenderContextInfo,
  GLS.GeomObjects,
  GLS.Mesh,
  GLS.HUDObjects,
  GLS.BitmapFont,
  GLS.WindowsFont,
  GLS.Keyboard,
  GLS.CompositeImage,

  GLS.Utils;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    GLSLShader1: TGLSLShader;              
    CustomRederer: TGLDirectOpenGL;
    LayeredFrameBuffer: TGLFBORenderer;
    GLLightSource1: TGLLightSource;
    GLDisk1: TGLDisk;
    ObjectContainer: TGLDummyCube;
    GLMesh1: TGLMesh;
    GLMesh2: TGLMesh;
    GLMesh3: TGLMesh;
    GLMesh4: TGLMesh;
    GLMesh5: TGLMesh;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLCylinder1: TGLCylinder;
    Surround: TGLDummyCube;
    ScreenQuad: TGLHUDSprite;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    ClearFrameBuffer: TGLDirectOpenGL;
    procedure ClearFrameBufferRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLSLShader1Apply(Shader: TGLCustomGLSLShader);
    procedure CustomRedererRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure GLSceneViewer1AfterRender(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FOITEnabled: Boolean;
    procedure CreateShapes;
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  img: TGLBlankImage;
  NativeDir: string;
begin
  SetGLSceneMediaDir();
  GLMaterialLibrary1.TextureByName('Surround').Image.LoadFromFile('WheatFld.JPG');
  GLSLShader1.LoadShaderPrograms('Shaders\OIT_vtx.glsl','Shaders\OIT_frag.glsl');
  GLSLShader1.Enabled := true;

  // Setup texture arrays
  img := TGLBlankImage(GLMaterialLibrary1.TextureByName('ColorLayers').Image);
  img.TextureArray := True;
  img.Depth := 6;
  img := TGLBlankImage(GLMaterialLibrary1.TextureByName('DepthLayers').Image);
  img.TextureArray := True;
  img.Depth := 6;

  // Create transparent shapes
  CreateShapes;

  GLHUDText1.Text := 'Press 1-7 to apply different blending functions'+#10#13+
  '8 to apply order independed transparency based on rendering to texture array';
end;

procedure TForm1.ClearFrameBufferRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  gl.Clear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

procedure TForm1.CreateShapes;
const
  vLtBlue: TGLColorVector = (X: 0.00; Y: 0.00; Z: 1.00; W:0.90);
  vLtPink: TGLColorVector = (X: 0.40; Y:0.00; Z:0.20; W:0.50);
  vLtYellow: TGLColorVector = (X: 0.98; Y:0.96; Z:0.14; W:0.30);
  vLtMagenta: TGLColorVector = (X: 0.83; Y:0.04; Z:0.83; W:0.70);
  vLtGreen: TGLColorVector = (X: 0.05; Y:0.98; Z:0.14; W:0.30);

var
  vd: array [0 .. 3] of TGLVertexData;

begin
  vd[0].coord := Vector3fMake(-0.2, -0.4, 0.0);
  vd[1].coord := Vector3fMake(-0.2, 0.4, 0.0);
  vd[2].coord := Vector3fMake(0.2, -0.4, 0.0);
  vd[3].coord := Vector3fMake(0.2, 0.4, 0.0);
  vd[0].color := vLtYellow;
  vd[1].color := vLtYellow;
  vd[2].color := vLtYellow;
  vd[3].color := vLtYellow;
  with GLMesh1 do
  begin
    Vertices.Clear;
    Vertices.AddVertex3(vd[0], vd[1], vd[2]);
    Vertices.AddVertex(vd[3]);
    CalcNormals(fwCounterClockWise);
  end;

  vd[0].coord := Vector3fMake(-0.24, -0.35, 0.0);
  vd[1].coord := Vector3fMake(-0.24, 0.45, 0.0);
  vd[2].coord := Vector3fMake(0.24, -0.45, 0.0);
  vd[3].coord := Vector3fMake(0.24, 0.35, 0.0);
  vd[0].color := vLtBlue;
  vd[1].color := vLtBlue;
  vd[2].color := vLtBlue;
  vd[3].color := vLtBlue;
  with GLMesh2 do
  begin
    Vertices.Clear;
    Vertices.AddVertex3(vd[0], vd[1], vd[2]);
    Vertices.AddVertex(vd[3]);
    CalcNormals(fwCounterClockWise);
  end;

  vd[0].coord := Vector3fMake(-0.20, -0.35, 0.0);
  vd[1].coord := Vector3fMake(-0.20, 0.25, 0.0);
  vd[2].coord := Vector3fMake(0.20, -0.25, 0.0);
  vd[3].coord := Vector3fMake(0.20, 0.35, 0.0);
  vd[0].color := vLtPink;
  vd[1].color := vLtPink;
  vd[2].color := vLtPink;
  vd[3].color := vLtPink;
  with GLMesh3 do
  begin
    Vertices.Clear;
    Vertices.AddVertex3(vd[0], vd[1], vd[2]);
    Vertices.AddVertex(vd[3]);
    CalcNormals(fwCounterClockWise);
  end;

  vd[0].coord := Vector3fMake(0.0, -0.45, 0.0);
  vd[1].coord := Vector3fMake(-0.3, 0.0, 0.0);
  vd[2].coord := Vector3fMake(0.3, 0.0, 0.0);
  vd[3].coord := Vector3fMake(0.0, 0.45, 0.0);
  vd[0].color := vLtGreen;
  vd[1].color := vLtGreen;
  vd[2].color := vLtGreen;
  vd[3].color := vLtGreen;
  with GLMesh4 do
  begin
    Vertices.Clear;
    Vertices.AddVertex3(vd[0], vd[1], vd[2]);
    Vertices.AddVertex(vd[3]);
    CalcNormals(fwCounterClockWise);
  end;

  vd[0].coord := Vector3fMake(-0.3, -0.4, 0.0);
  vd[1].coord := Vector3fMake(-0.0, 0.5, 0.0);
  vd[2].coord := Vector3fMake(0.3, -0.4, 0.0);
  vd[0].color := vLtMagenta;
  vd[1].color := vLtMagenta;
  vd[2].color := vLtMagenta;
  with GLMesh5 do
  begin
    Vertices.Clear;
    Vertices.AddVertex3(vd[0], vd[1], vd[2]);
    CalcNormals(fwCounterClockWise);
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);

  procedure TurnOffOIT;
  begin
    ObjectContainer.Visible := True;
    ClearFrameBuffer.Visible := True;
    CustomRederer.Visible := False;
    LayeredFrameBuffer.Active := False;
    ScreenQuad.Visible := False;
  end;

begin
  with GLMaterialLibrary1.Materials[0].Material.BlendingParams do
  begin
    if IsKeyDown('1') then
    begin
      SeparateBlendFunc := False;
      BlendFuncSFactor := bfSrcAlpha;
      BlendFuncDFactor := bfOneMinusSrcAlpha;
      TurnOffOIT;
    end
    else if IsKeyDown('2') then
    begin
      SeparateBlendFunc := False;
      BlendFuncSFactor := bfSrcAlpha;
      BlendFuncDFactor := bfOneMinusDstAlpha;
      TurnOffOIT;
    end
    else if IsKeyDown('3') then
    begin
      SeparateBlendFunc := False;
      BlendFuncSFactor := bfOne;
      BlendFuncDFactor := bfOneMinusSrcAlpha;
      TurnOffOIT;
    end
    else if IsKeyDown('4') then
    begin
      SeparateBlendFunc := False;
      BlendFuncSFactor := bfSrcAlpha;
      BlendFuncDFactor := bfOne;
      TurnOffOIT;
    end
    else if IsKeyDown('5') then
    begin
      SeparateBlendFunc := False;
      BlendFuncSFactor := bfSrcAlpha;
      BlendFuncDFactor := bfDstColor;
      TurnOffOIT;
    end
    else if IsKeyDown('6') then
    begin
      SeparateBlendFunc := True;
      BlendFuncSFactor := bfSrcAlpha;
      BlendFuncDFactor := bfDstAlpha;
      AlphaBlendFuncSFactor := bfSrcAlpha;
      AlphaBlendFuncDFactor := bfOneMinusSrcAlpha;
      TurnOffOIT;
    end
    else if IsKeyDown('7') then
    begin
      SeparateBlendFunc := True;
      BlendFuncSFactor := bfSrcColor;
      BlendFuncDFactor := bfDstColor;
      AlphaBlendFuncSFactor := bfSrcAlpha;
      AlphaBlendFuncDFactor := bfOneMinusSrcAlpha;
      TurnOffOIT;
    end
    else if IsKeyDown('8') and FOITEnabled then
    begin
      ObjectContainer.Visible := False;
      ClearFrameBuffer.Visible := False;
      CustomRederer.Visible := True;
      LayeredFrameBuffer.Active := True;
      ScreenQuad.Visible := True;
    end;

    GLSceneViewer1.Invalidate;
  end;
end;

procedure TForm1.CustomRedererRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  rci.ignoreBlendingRequests := True;
  rci.GLStates.Disable(stBlend);

  LayeredFrameBuffer.Layer := 0;
  gl.Clear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  Surround.Render(rci);
  GlDisk1.Render(rci);

  rci.GLStates.ColorClearValue := clrTransparent;
  LayeredFrameBuffer.Layer := 1;
  gl.Clear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  GLMesh1.Render(rci);

  LayeredFrameBuffer.Layer := 2;
  gl.Clear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  GLMesh2.Render(rci);

  LayeredFrameBuffer.Layer := 3;
  gl.Clear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  GLMesh3.Render(rci);

  LayeredFrameBuffer.Layer := 4;
  gl.Clear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  GLMesh4.Render(rci);

  LayeredFrameBuffer.Layer := 5;
  gl.Clear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  GLMesh5.Render(rci);

  rci.ignoreBlendingRequests := True;
end;

procedure TForm1.GLSceneViewer1AfterRender(Sender: TObject);
begin
  with GLSceneViewer1.Buffer.RenderingContext.GL do
    FOITEnabled := VERSION_3_0 or EXT_texture_array;
  GLSceneViewer1.AfterRender := nil;
end;

procedure TForm1.GLSLShader1Apply(Shader: TGLCustomGLSLShader);
begin
  with Shader, GLMaterialLibrary1 do
  begin
    Param['ColorLayers'].AsTexture[0] := TextureByName('ColorLayers');
    Param['DepthLayers'].AsTexture[1] := TextureByName('DepthLayers');
  end;
  CurrentGLContext.GLStates.Disable(stBlend);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  LayeredFrameBuffer.Width := GLSceneViewer1.Width;
  LayeredFrameBuffer.Height := GLSceneViewer1.Height;
end;

end.
