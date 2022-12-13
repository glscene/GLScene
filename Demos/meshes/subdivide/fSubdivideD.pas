unit fSubdivideD;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,

  GLS.Scene,
  GLS.VectorFileObjects,
  GLS.PersistentClasses,
  GLS.Objects,
  GLS.Texture,
  GLS.SceneViewer,
  GLS.MeshUtils,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.Cadencer,

  GLS.Material,
  GLS.Coordinates,
  GLS.State,
  GLS.BaseClasses,
  GLS.Utils,
  GLS.File3DS,
  GLS.FileMD2,
  GLS.FileSMD;

type
  TFormSubdivide = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    BULoad: TButton;
    BUSubdivide: TButton;
    TrackBar1: TTrackBar;
    RBWireFrame: TRadioButton;
    RBSolid: TRadioButton;
    CBAnimate: TCheckBox;
    GLActor1: TGLActor;
    Label1: TLabel;
    LASubdivideTime: TLabel;
    GLLightSource1: TGLLightSource;
    procedure BULoadClick(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BUSubdivideClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure RBWireFrameClick(Sender: TObject);
    procedure RBSolidClick(Sender: TObject);
    procedure CBAnimateClick(Sender: TObject);
  public
    mx, my: Integer;
  end;

var
  FormSubdivide: TFormSubdivide;

implementation

{$R *.dfm}

procedure TFormSubdivide.BULoadClick(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\model');
  
  BUSubdivide.Enabled := True;

  // GLFreeForm1.LoadFromFile('polyhedron.3ds');
  // GLFreeForm1.LoadFromFile('mushroom.3ds');
  // GLFreeForm1.LoadFromFile('trinityrage.smd');
  // GLFreeForm1.LoadFromFile('HighPolyObject.3ds');

  {
    GLActor1.LoadFromFile('trinityrage.smd');
    GLActor1.AddDataFromFile('run.smd');
    GLActor1.Animations[1].MakeSkeletalTranslationStatic;
    GLActor1.SwitchToAnimation(GLActor1.Animations[1]);
  }

  GLActor1.LoadFromFile('waste.md2');
  
  SetCurrentDir(Path  + '\texture');
  
  GLActor1.Material.Texture.Image.LoadFromFile('waste.jpg');
  GLActor1.Material.Texture.Enabled := True;
  GLActor1.SwitchToAnimation(GLActor1.Animations[0]);

  CBAnimateClick(Self);
end;

procedure TFormSubdivide.BUSubdivideClick(Sender: TObject);
var
  i, j: Integer;
  tris, norms, tex, buf, morphTris, morphNorms: TGLAffineVectorList;
  indices, texIndices: TGLIntegerList;
  firstRemap, subdivideRemap, bufRemap: TGLIntegerList;
  t: Int64;
begin
  BUSubdivide.Enabled := False;

  Screen.Cursor := crHourGlass;
  t := StartPrecisionTimer;

  for i := 0 to GLActor1.MeshObjects.Count - 1 do
  begin
    tex := TGLAffineVectorList.Create;
    with GLActor1.MeshObjects[i] do
      tris := ExtractTriangles(tex);
    indices := BuildVectorCountOptimizedIndices(tris);
    firstRemap := TGLIntegerList(indices.CreateClone);
    RemapAndCleanupReferences(tris, indices);

    norms := BuildNormals(tris, indices);

    // subdivide geometry
    SubdivideTriangles(TrackBar1.Position * 0.1, tris, indices, norms);

    texIndices := BuildVectorCountOptimizedIndices(tex);
    RemapAndCleanupReferences(tex, texIndices);

    // subdivide texture space
    SubdivideTriangles(0, tex, texIndices);

    // Re-expand everything
    buf := TGLAffineVectorList.Create;
    try
      ConvertIndexedListToList(tris, indices, buf);
      tris.Assign(buf);
      buf.Count := 0;
      ConvertIndexedListToList(norms, indices, buf);
      norms.Assign(buf);
      buf.Count := 0;
      ConvertIndexedListToList(tex, texIndices, buf);
      tex.Assign(buf);
    finally
      buf.Free;
    end;

    // Pack & Optimize the expanded stuff
    indices.Free;
    indices := BuildVectorCountOptimizedIndices(tris, norms, tex);
    subdivideRemap := TGLIntegerList(indices.CreateClone);
    RemapReferences(norms, indices);
    RemapReferences(tex, indices);
    RemapAndCleanupReferences(tris, indices);

    IncreaseCoherency(indices, 13);

    with GLActor1.MeshObjects[i] as TGLMorphableMeshObject do
    begin

      bufRemap := TGLIntegerList.Create;
      for j := 0 to MorphTargets.Count - 1 do
      begin
        MorphTo(j);

        morphTris := ExtractTriangles;
        bufRemap.Assign(firstRemap);
        RemapAndCleanupReferences(morphTris, bufRemap);

        morphNorms := GLS.MeshUtils.BuildNormals(morphTris, bufRemap);

        SubdivideTriangles(TrackBar1.Position * 0.1, morphTris, bufRemap,
          morphNorms);

        buf := TGLAffineVectorList.Create;
        try
          ConvertIndexedListToList(morphTris, bufRemap, buf);
          morphTris.Assign(buf);
          ConvertIndexedListToList(morphNorms, bufRemap, buf);
          morphNorms.Assign(buf);
        finally
          buf.Free;
        end;
        RemapReferences(morphTris, subdivideRemap);
        RemapReferences(morphNorms, subdivideRemap);

        MorphTargets[j].Vertices := morphTris;
        MorphTargets[j].Normals := morphNorms;

        morphTris.Free;
        morphNorms.Free;
      end;
      bufRemap.Free;

      Vertices := tris;
      Normals := norms;
      TexCoords := tex;
      FaceGroups.Clear;
      with TFGVertexIndexList.CreateOwned(FaceGroups) do
      begin
        VertexIndices := indices;
        Mode := fgmmTriangles;
      end;
    end;

    texIndices.Free;
    subdivideRemap.Free;
    firstRemap.Free;
    tex.Free;
    indices.Free;
    norms.Free;
    tris.Free;
  end;

  // (GLActor1.MeshObjects[0] as TGLSkeletonMeshObject).PrepareBoneMatrixInvertedMeshes;

  LASubdivideTime.Caption := Format('%.1f ms', [StopPrecisionTimer(t) * 1000]);
  // Initial perf: 1412 ms
  // Basic Edges Hash: 464 ms
  // Several transfer optims: 377 ms
  // morph & subdivide normals too : 527 ms
  Screen.Cursor := crDefault;

  GLActor1.StructureChanged;
end;

procedure TFormSubdivide.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormSubdivide.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  end
  else if Shift = [ssRight] then
  begin
    GLCamera1.RotateTarget(my - Y, mx - X);
  end;
  mx := X;
  my := Y;
end;

procedure TFormSubdivide.RBWireFrameClick(Sender: TObject);
begin
  GLActor1.Material.PolygonMode := pmLines;
end;

procedure TFormSubdivide.RBSolidClick(Sender: TObject);
begin
  GLActor1.Material.PolygonMode := pmFill;
end;

procedure TFormSubdivide.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS -  %d Triangles', [GLSceneViewer1.FramesPerSecond,
    GLActor1.MeshObjects.TriangleCount]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormSubdivide.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TFormSubdivide.CBAnimateClick(Sender: TObject);
begin
  // not only turns on/off animation, but also forces the TGLActor
  // to generate a display list when animation is off
  if CBAnimate.Checked then
  begin
    GLActor1.AnimationMode := aamLoop;
    GLActor1.ObjectStyle := GLActor1.ObjectStyle + [osDirectDraw];
    GLActor1.Reference := aarMorph;
  end
  else
  begin
    GLActor1.AnimationMode := aamNone;
    GLActor1.MeshObjects.MorphTo(0);
    GLActor1.Reference := aarNone;
    GLActor1.StructureChanged;
    GLActor1.ObjectStyle := GLActor1.ObjectStyle - [osDirectDraw];
  end;
end;

end.
