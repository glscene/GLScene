unit fIntensityMeshD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  GLS.Scene,
  Stage.VectorTypes,
  GLS.VectorFileObjects,
  GLS.VectorLists,
  GLS.SceneViewer,
  GLS.Mesh,
  GLS.Texture,
  GLSL.UserShader,
  GLS.HUDObjects,
  Stage.VectorGeometry,
  GLS.Context,
  GLS.Objects,
  GLS.BitmapFont,
  GLS.WindowsFont,
  Stage.Utils,
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.Graphics,
  GLS.State,
  Stage.TextureFormat;

type
  TFormIntensutyMesh = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera: TGLCamera;
    DCTarget: TGLDummyCube;
    GLFreeForm: TGLFreeForm;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLUserShader: TGLUserShader;
    HSPalette: TGLHUDSprite;
    GLWindowsBitmapFont: TGLWindowsBitmapFont;
    HTPaletteLeft: TGLHUDText;
    HTPaletteRight: TGLHUDText;
    Panel1: TPanel;
    CBWireFrame: TCheckBox;
    CBSmooth: TCheckBox;
    TBScale: TTrackBar;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLUserShaderDoUnApply(Sender: TObject; Pass: Integer;
      var rci: TGLRenderContextInfo; var Continue: Boolean);
    procedure CBSmoothClick(Sender: TObject);
    procedure CBWireFrameClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TBScaleChange(Sender: TObject);
  private
    mx, my: Integer;
  public

  end;

var
  FormIntensutyMesh: TFormIntensutyMesh;

implementation

{$R *.dfm}

type
  // Structures used in our binary file
  // The structure is quite simplified here, original data came from a FEM
  // package and was in (huge) text files, and parsing text files is not the
  // purpose of this demo, so data was simplified ;)
  TDataNode = record
    X, Y, Z: Single;
    Intensity: Single;
  end;

  TDataPrimitive = record
    Node1, Node2, Node3, Node4: Word; // if Node4 is $FFFF, codes a triangle
  end;

var
  DataNodes: array of TDataNode;
  DataPrimitives: array of TDataPrimitive;

procedure TFormIntensutyMesh.FormCreate(Sender: TObject);
var
  mo: TGLMeshObject;
  fgQuads, fgTris: TFGVertexIndexList;
  i: Integer;
  str: TFileStream;
begin
  // load our raw data
  str := TFileStream.Create('IntensityMesh.data', fmOpenRead);
  str.Read(i, 4);
  SetLength(DataNodes, i);
  str.Read(i, 4);
  SetLength(DataPrimitives, i);
  str.Read(DataNodes[0], Length(DataNodes) * SizeOf(TDataNode));
  str.Read(DataPrimitives[0], Length(DataPrimitives) * SizeOf(TDataPrimitive));
  str.Free;

  // fill the freeform with our data

  // first create a mesh object
  mo := TGLMeshObject.CreateOwned(GLFreeForm.MeshObjects);
  mo.Mode := momFaceGroups;
  // Specify vertex and texcoords data (intensity is stored a texcoord)
  for i := 0 to High(DataNodes) do
  begin
    mo.Vertices.Add(DataNodes[i].X, DataNodes[i].Y, DataNodes[i].Z);
    mo.TexCoords.Add(DataNodes[i].Intensity * 0.001, 0);
  end;
  // Then create the facegroups that will hold our quads and triangles
  fgQuads := TFGVertexIndexList.CreateOwned(mo.FaceGroups);
  fgQuads.Mode := fgmmQuads;
  fgTris := TFGVertexIndexList.CreateOwned(mo.FaceGroups);
  fgTris.Mode := fgmmTriangles;
  // and fill them with our primitives
  for i := 1 to High(DataPrimitives) do
    with DataPrimitives[i] do
    begin
      if Node4 <> $FFFF then
      begin
        fgQuads.VertexIndices.Add(Node1, Node2);
        fgQuads.VertexIndices.Add(Node4, Node3);
      end
      else
      begin
        fgTris.VertexIndices.Add(Node1, Node2, Node3);
      end;
    end;
  // auto center
  GLFreeForm.PerformAutoCentering;
  // and initialize scale
  TBScaleChange(Self);
end;

procedure TFormIntensutyMesh.GLUserShaderDoUnApply(Sender: TObject; Pass: Integer;
  var rci: TGLRenderContextInfo; var Continue: Boolean);
begin
  if not CBWireFrame.Checked then
    Pass := 2; // skip wireframe pass
  case Pass of
    1:
      begin
        // 2nd pass is a wireframe pass (two-sided)
        rci.GLStates.ActiveTextureEnabled[ttTexture2D] := False;
        rci.GLStates.Enable(stLineSmooth);
        rci.GLStates.Enable(stBlend);
        rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
        rci.GLStates.LineWidth := 0.5;
        rci.GLStates.PolygonMode := pmLines;
        rci.GLStates.PolygonOffsetFactor := -1;
        rci.GLStates.PolygonOffsetUnits := -1;
        rci.GLStates.Enable(stPolygonOffsetLine);
        gl.Color3f(0, 0, 0);
        Continue := True;
      end;
  else
    // restore states or mark them dirty
    if CBWireFrame.Checked then
    begin
      rci.GLStates.Disable(stPolygonOffsetLine);
    end;

    Continue := False;
  end;
end;

procedure TFormIntensutyMesh.CBSmoothClick(Sender: TObject);
var
  tex: TGLTexture;
begin
  // switch between linear and nearest filtering
  tex := GLMaterialLibrary1.Materials[0].Material.Texture;
  if CBSmooth.Checked then
  begin
    tex.MagFilter := maLinear;
    tex.MinFilter := miLinear;
  end
  else
  begin
    tex.MagFilter := maNearest;
    tex.MinFilter := miNearest;
  end;
end;

procedure TFormIntensutyMesh.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  GLSceneViewer1.SetFocus;
end;

procedure TFormIntensutyMesh.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera.MoveAroundTarget(my - Y, mx - X);
  if ssRight in Shift then
  begin
    DCTarget.Position.AddScaledVector((mx - X) / 30,
      GLCamera.AbsoluteRightVectorToTarget);
    DCTarget.Position.AddScaledVector((Y - my) / 30,
      GLCamera.AbsoluteUpVectorToTarget);
  end;
  mx := X;
  my := Y;
end;

procedure TFormIntensutyMesh.TBScaleChange(Sender: TObject);
begin
  with GLMaterialLibrary1.Materials[0] do
    TextureScale.X := TBScale.Position / 100;
  HTPaletteRight.Text := Format('%d', [TBScale.Position * 10]);
  GLSceneViewer1.Invalidate;
end;

procedure TFormIntensutyMesh.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera.AdjustDistanceToTarget(Power(1.03, WheelDelta / 120));
end;

procedure TFormIntensutyMesh.CBWireFrameClick(Sender: TObject);
begin
  GLSceneViewer1.Invalidate;
end;

end.
