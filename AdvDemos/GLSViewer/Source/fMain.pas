unit fMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.Win.Registry,
  System.ImageList,
  System.Math,
  System.Actions,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ActnList,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  Vcl.ExtDlgs,
  Vcl.ExtCtrls,
  Vcl.ActnMan,
  Vcl.ActnCtrls,
  Vcl.ActnMenus,
  Vcl.StdActns,
  Vcl.BandActn,
  Vcl.PlatformDefaultStyleActnCtrls,

  GLS.Material,
  GLS.Scene,
  GLS.SceneViewer,
  GLS.VectorFileObjects,
  GLS.Objects,
  GLS.VectorGeometry,
  GLS.Texture,
  GLS.Context,
  GLS.VectorLists,
  GLS.Cadencer,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.State,
  GLS.RenderContextInfo,
  GLS.TextureFormat,
  GLS.Color,
  GLS.Keyboard,
  GLS.Graphics,
  GLS.PersistentClasses,
  GLS.MeshUtils,
  GLS.VectorTypes,
  GnuGettext,
  GLS.AsyncTimer,
  GLS.Graph,
  GLS.MeshBuilder,
  GLS.Navigator,
  GLS.Utils,

  fGLForm,
  fGLAbout,
  fGLOptions,
  fGLDialog,
  dGLSViewer,
  GLS.SimpleNavigation;

type
  TMainForm = class(TGLForm)
    ImageList: TImageList;
    StatusBar: TStatusBar;
    Scene: TGLScene;
    ffObject: TGLFreeForm;
    LightSource: TGLLightSource;
    MaterialLib: TGLMaterialLibrary;
    CubeExtents: TGLCube;
    dcTarget: TGLDummyCube;
    Camera: TGLCamera;
    dcAxis: TGLDummyCube;
    Cadencer: TGLCadencer;
    Timer: TTimer;
    LightmapLib: TGLMaterialLibrary;
    snViewer: TGLSceneViewer;
    ActionManager: TActionManager;
    acOptimizeMesh: TAction;
    acProcessInvertNormals: TAction;
    acReverseRendering: TAction;
    acConvertToTriangles: TAction;
    acProcessStripify: TAction;
    acToolsOptions: TAction;
    acToolsFaceCulling: TAction;
    acToolsTexturing: TAction;
    acToolsLighting: TAction;
    acToolsCustomize: TCustomizeActionBars;
    acToolsShowFPS: TAction;
    acViewSmoothShading: TAction;
    acViewFlatShading: TAction;
    acViewFlatLines: TAction;
    acViewHiddenLines: TAction;
    acViewWireFrame: TAction;
    acViewZoomIn: TAction;
    acViewZoomOut: TAction;
    acViewReset: TAction;
    acFileOpen: TAction;
    acFilePick: TAction;
    acFileOpenTexLib: TAction;
    acFileSaveAs: TAction;
    acFileSaveTextures: TAction;
    acFileExit: TAction;
    acHelpContents: THelpContents;
    acHelpTopicSearch: THelpTopicSearch;
    acHelpOnHelp: THelpOnHelp;
    acHelpGLSHomePage: TAction;
    acHelpAbout: TAction;
    acAADefault: TAction;
    acAA2X: TAction;
    acAA4X: TAction;
    acEditUndo: TEditUndo;
    acEditCut: TEditCut;
    acEditCopy: TEditCopy;
    acEditPaste: TEditPaste;
    acEditSelectAll: TEditSelectAll;
    acEditDelete: TEditDelete;
    ImageListMenu: TImageList;
    ControlBar: TControlBar;
    amMenuBar: TActionMainMenuBar;
    acAA8X: TAction;
    acAA16X: TAction;
    acCSA8X: TAction;
    acCSA16X: TAction;
    atbTools: TActionToolBar;
    atbView: TActionToolBar;
    atbFile: TActionToolBar;
    acPoints: TAction;
    AsyncTimer: TGLAsyncTimer;
    dcWorld: TGLDummyCube;
    grdXYZ: TGLXYZGrid;
    acToolsNaviCube: TAction;
    GLPoints: TGLPoints;
    acToolsInfo: TAction;
    GLSimpleNavigation: TGLSimpleNavigation;
    acSpheres: TAction;
    procedure FormCreate(Sender: TObject);
    procedure snViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure snViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure snViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure snViewerBeforeRender(Sender: TObject);
    procedure snViewerAfterRender(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MaterialLibTextureNeeded(Sender: TObject;
      var textureFileName: String);
    procedure acInvertNormalsExecute(Sender: TObject);
    procedure acSaveAsUpdate(Sender: TObject);
    procedure acReverseRenderingOrderExecute(Sender: TObject);
    procedure acConvertToIndexedTrianglesExecute(Sender: TObject);
    procedure CadencerProgress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure TimerTimer(Sender: TObject);
    procedure acOptimizeExecute(Sender: TObject);
    procedure acStripifyExecute(Sender: TObject);
    procedure acHelpAboutExecute(Sender: TObject);
    procedure acFilePickExecute(Sender: TObject);
    procedure acFileOpenTexLibExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acFileSaveAsExecute(Sender: TObject);
    procedure acFileSaveTexturesExecute(Sender: TObject);
    procedure acFileExitExecute(Sender: TObject);
    procedure acToolsOptionsExecute(Sender: TObject);
    procedure acToolsTexturingExecute(Sender: TObject);
    procedure acToolsFaceCullingExecute(Sender: TObject);
    procedure acToolsLightingExecute(Sender: TObject);
    procedure acToolsShowFPSExecute(Sender: TObject);
    procedure acAADefaultExecute(Sender: TObject);
    procedure acViewSmoothShadingExecute(Sender: TObject);
    procedure acViewFlatShadingExecute(Sender: TObject);
    procedure acViewFlatLinesExecute(Sender: TObject);
    procedure acViewHiddenLinesExecute(Sender: TObject);
    procedure acViewWireFrameExecute(Sender: TObject);
    procedure acViewResetExecute(Sender: TObject);
    procedure acViewZoomOutExecute(Sender: TObject);
    procedure acViewZoomInExecute(Sender: TObject);
    procedure acPointsExecute(Sender: TObject);
    procedure AsyncTimerTimer(Sender: TObject);
    procedure acToolsNaviCubeExecute(Sender: TObject);
    procedure acToolsInfoExecute(Sender: TObject);
    procedure snViewerMouseLeave(Sender: TObject);
  private
    procedure DoResetCamera;
    procedure SetupFreeFormShading;
    procedure ApplyShadeModeToMaterial(aMaterial: TGLMaterial);
    procedure ApplyShadeMode;
    procedure ApplyFSAA;
    procedure ApplyFaceCull;
    procedure ApplyTexturing;
    procedure ApplyFPS;
    procedure DoOpen(const FileName: String);
  public
    md, nthShow: Boolean;
    mx, my: Integer;
    hlShader: TGLShader;
    lastFileName: String;
    lastLoadWithTextures: Boolean;
    Points: TGLPoints;
    procedure ApplyBgColor;
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  end;

var
  MainForm: TMainForm;
  NaviCube: TGLNaviCube;

//=======================================================================
implementation
//=======================================================================

{$R *.dfm}

uses
  GLS.FileOBJ,
  GLS.FileSTL,
  GLS.FileLWO,
  GLS.FileQ3BSP,
  GLS.FileOCT,
  GLS.FileMS3D,
  GLS.FileNMF,
  GLS.FileMD3,
  GLS.File3DS,
  GLS.FileMD2,
  GLS.FileSMD,
  GLS.FilePLY,
  GLS.FileGTS,
  GLS.FileVRML,
  GLS.FileMD5,
  GLS.FileTIN,
  GLS.FileDXF,
  GLS.FileGRD,
  GLS.FileX;

type
  // Hidden line shader (specific implem for the viewer, *not* generic)
  THiddenLineShader = class(TGLShader)
  private
    LinesColor: TGLColorVector;
    BackgroundColor: TGLColorVector;
    PassCount: Integer;
  public
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  end;

procedure THiddenLineShader.DoApply(var rci: TGLRenderContextInfo;
  Sender: TObject);
begin
  PassCount := 1;
  with rci.GLStates do
  begin
    PolygonMode := pmFill;
    gl.Color3fv(@BackgroundColor);
    ActiveTextureEnabled[ttTexture2D] := False;
    Enable(stPolygonOffsetFill);
    PolygonOffsetFactor := 1;
    PolygonOffsetUnits := 2;
  end;
end;

function THiddenLineShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  case PassCount of
    1:
      with rci.GLStates do
      begin
        PassCount := 2;
        PolygonMode := pmLines;
        glColor3fv(@LinesColor);
        Disable(stLighting);
        Result := True;
      end;
    2:
      begin
        rci.GLStates.Disable(stPolygonOffsetFill);
        Result := False;
      end;
  else
    // doesn't hurt to be cautious
    Assert(False);
    Result := False;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  inherited;
 // SetCurrentDir(ExtractFilePath(ParamStr(0)));
  SetGLSceneMediaDir();

  NaviCube := TGLNaviCube.CreateAsChild(Scene.Objects);
  NaviCube.SceneViewer := snViewer;
  NaviCube.FPS := 30;

 // instantiate our specific hidden-lines shader
  hlShader := THiddenLineShader.Create(Self);
  ffObject.IgnoreMissingTextures := True;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if not nthShow then
  begin
    dmGLSViewer.OpenDialog.Filter := VectorFileFormatsFilter;
    dmGLSViewer.SaveDialog.Filter := VectorFileFormatsSaveFilter;
    ApplyFSAA;
    ApplyFaceCull;
    ApplyFPS;
    if ParamCount > 0 then
      DoOpen(ParamStr(1));
    nthShow := True;
  end;
end;

procedure TMainForm.acFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.acFileOpenExecute(Sender: TObject);
begin
  NaviCube.ActiveMouse := False;
  if dmGLSViewer.OpenDialog.Execute then
    DoOpen(dmGLSViewer.OpenDialog.FileName);
end;

procedure TMainForm.acFileOpenTexLibExecute(Sender: TObject);
var
  I: Integer;
begin
  if dmGLSViewer.ODTextures.Execute then
    with MaterialLib do
    begin
      LoadFromFile(dmGLSViewer.ODTextures.FileName);
      for I := 0 to Materials.Count - 1 do
        with Materials[I].Material do
          BackProperties.Assign(FrontProperties);
      ApplyShadeMode;
      ApplyTexturing;
    end;
end;

procedure TMainForm.acFilePickExecute(Sender: TObject);
begin
  if dmGLSViewer.opDialog.Execute then
  begin
    with MaterialLib.Materials do
    begin
      with Items[Count - 1] do
      begin
        Tag := 1;
        Material.Texture.Image.LoadFromFile(dmGLSViewer.opDialog.FileName);
        Material.Texture.Enabled := True;
      end;
    end;
    ApplyTexturing;
  end;
end;

procedure TMainForm.acFileSaveAsExecute(Sender: TObject);
var
  ext: String;
begin
  if dmGLSViewer.SaveDialog.Execute then
  begin
    ext := ExtractFileExt(dmGLSViewer.SaveDialog.FileName);
    if ext = '' then
      dmGLSViewer.SaveDialog.FileName :=
        ChangeFileExt(dmGLSViewer.SaveDialog.FileName,
        '.' + GetVectorFileFormats.FindExtByIndex
        (dmGLSViewer.SaveDialog.FilterIndex, False, True));
    if GetVectorFileFormats.FindFromFileName(dmGLSViewer.SaveDialog.FileName) = nil
    then
      ShowMessage(_('Unsupported or unspecified file extension.'))
    else
      ffObject.SaveToFile(dmGLSViewer.SaveDialog.FileName);
  end;
end;

procedure TMainForm.acFileSaveTexturesExecute(Sender: TObject);
begin
  if dmGLSViewer.SDTextures.Execute then
    MaterialLib.SaveToFile(dmGLSViewer.SDTextures.FileName);
end;

procedure TMainForm.snViewerBeforeRender(Sender: TObject);
begin
  THiddenLineShader(hlShader).LinesColor := VectorMake(107 / 256, 123 / 256,
    173 / 256, 1);
  THiddenLineShader(hlShader).BackgroundColor :=
    ConvertWinColor(snViewer.Buffer.BackgroundColor);
  if not gl.ARB_multisample then
  begin
    acAADefault.Checked := True;
    acAA2X.Enabled := False;
    acAA4X.Enabled := False;
    acAA8X.Enabled := False;
    acAA16X.Enabled := False;
    acCSA8X.Enabled := False;
    acCSA16X.Enabled := False;
  end;
end;

procedure TMainForm.snViewerAfterRender(Sender: TObject);
begin
  ApplyFSAA;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.DoResetCamera;
var
  objSize: Single;
begin
  dcTarget.Position.AsVector := NullHmgPoint;
  Camera.Position.SetPoint(0, 4, 5);
  ffObject.Position.AsVector := NullHmgPoint;
  ffObject.Up.Assign(dcAxis.Up);
  ffObject.Direction.Assign(dcAxis.Direction);

  objSize := ffObject.BoundingSphereRadius;
  if objSize > 0 then
  begin
    if objSize < 1 then
    begin
      Camera.SceneScale := 1 / objSize;
      objSize := 1;
    end
    else
      Camera.SceneScale := 1;
    Camera.AdjustDistanceToTarget(objSize * 0.27);
    Camera.DepthOfView := 1.5 * Camera.DistanceToTarget + 2 * objSize;
  end;
end;

procedure TMainForm.ApplyShadeModeToMaterial(aMaterial: TGLMaterial);
begin
  if acViewSmoothShading.Checked then
  begin
    snViewer.Buffer.Lighting := True;
    snViewer.Buffer.ShadeModel := smSmooth;
    aMaterial.PolygonMode := pmFill;
  end
  else if acViewFlatShading.Checked then
  begin
    snViewer.Buffer.Lighting := True;
    snViewer.Buffer.ShadeModel := smFlat;
    aMaterial.PolygonMode := pmFill;
  end
  else if acViewFlatLines.Checked then
  begin
    snViewer.Buffer.Lighting := True;
    snViewer.Buffer.ShadeModel := smFlat;
    aMaterial.PolygonMode := pmLines;
  end
  else if acViewHiddenLines.Checked then
  begin
    snViewer.Buffer.Lighting := False;
    snViewer.Buffer.ShadeModel := smSmooth;
    aMaterial.PolygonMode := pmLines;
  end
  else if acViewWireFrame.Checked then
  begin
    snViewer.Buffer.Lighting := False;
    snViewer.Buffer.ShadeModel := smSmooth;
    aMaterial.PolygonMode := pmLines;
  end;
end;

procedure TMainForm.ApplyShadeMode;
var
  I: Integer;
begin
  with MaterialLib.Materials do
    for I := 0 to Count - 1 do
    begin
      ApplyShadeModeToMaterial(Items[I].Material);
      if (acViewHiddenLines.Checked) or (acViewFlatLines.Checked) then
        Items[I].Shader := hlShader
      else
        Items[I].Shader := nil;
    end;
  snViewer.Buffer.Lighting := acToolsLighting.Checked;
  ffObject.StructureChanged;
end;

procedure TMainForm.ApplyFSAA;
begin
  with snViewer.Buffer do
  begin
    if acAADefault.Checked then
      AntiAliasing := aaDefault
    else if acAA2X.Checked then
      AntiAliasing := aa2x
    else if acAA4X.Checked then
      AntiAliasing := aa4x
    else if acAA8X.Checked then
      AntiAliasing := aa8x
    else if acAA16X.Checked then
      AntiAliasing := aa16x
    else if acCSA8X.Checked then
      AntiAliasing := csa8x
    else if acCSA16X.Checked then
      AntiAliasing := csa16x;
  end;
end;

procedure TMainForm.ApplyFaceCull;
begin
  with snViewer.Buffer do
  begin
    if acToolsFaceCulling.Checked then
    begin
      FaceCulling := True;
      ContextOptions := ContextOptions - [roTwoSideLighting];
    end
    else
    begin
      FaceCulling := False;
      ContextOptions := ContextOptions + [roTwoSideLighting];
    end;
  end;
end;

procedure TMainForm.ApplyBgColor;
var
  bmp: TBitmap;
  col: TColor;
begin
  bmp := TBitmap.Create;
  try
    bmp.Width := 16;
    bmp.Height := 16;
    col := ColorToRGB(dmGLSViewer.ColorDialog.Color);
    snViewer.Buffer.BackgroundColor := col;
    bmp.Canvas.Pen.Color := col xor $FFFFFF;
    bmp.Canvas.Rectangle(0, 0, 16, 16);
    bmp.Canvas.Brush.Color := col;
  finally
    bmp.Free;
  end;
end;

procedure TMainForm.ApplyTexturing;
var
  I: Integer;
begin
  with MaterialLib.Materials do
    for I := 0 to Count - 1 do
    begin
      with Items[I].Material.Texture do
      begin
        if Enabled then
          Items[I].Tag := Integer(True);
        Enabled := Boolean(Items[I].Tag) and acToolsTexturing.Checked;
      end;
    end;
  ffObject.StructureChanged;
end;

procedure TMainForm.AsyncTimerTimer(Sender: TObject);
begin
  snViewer.ResetPerformanceMonitor;
end;

procedure TMainForm.ApplyFPS;
begin
  if acToolsShowFPS.Checked then
  begin
    Timer.Enabled := True;
    Cadencer.Enabled := True;
  end
  else
  begin
    Timer.Enabled := False;
    Cadencer.Enabled := False;
//    StatusBar.Panels[3].Text := ' FPS';
  end;
end;

procedure TMainForm.SetupFreeFormShading;
var
  I: Integer;
  LibMat: TGLLibMaterial;
begin
  if MaterialLib.Materials.Count = 0 then
  begin
    ffObject.Material.MaterialLibrary := MaterialLib;
    LibMat := MaterialLib.Materials.Add;
    ffObject.Material.LibMaterialName := LibMat.Name;
    LibMat.Material.FrontProperties.Diffuse.Red := 0;
  end;
  for I := 0 to MaterialLib.Materials.Count - 1 do
    with MaterialLib.Materials[I].Material do
      BackProperties.Assign(FrontProperties);
  ApplyShadeMode;
  ApplyTexturing;
  ApplyFPS;
end;

procedure TMainForm.DoOpen(const FileName: String);
var
  min, max: TAffineVector;
  Name: TFileName;
begin
  if not FileExists(FileName) then
    Exit;
  Screen.Cursor := crHourGlass;
  MainForm.Caption := 'GLSViewer - ' + FileName;
  MaterialLib.Materials.Clear;
  ffObject.MeshObjects.Clear;
  ffObject.LoadFromFile(FileName);
  SetupFreeFormShading;
  acFileSaveTextures.Enabled := (MaterialLib.Materials.Count > 0);
  acFileOpenTexLib.Enabled := (MaterialLib.Materials.Count > 0);
  lastFileName := FileName;
  lastLoadWithTextures := acToolsTexturing.Enabled;
  ffObject.GetExtents(min, max);
  CubeExtents.CubeWidth := max.X - min.X;
  CubeExtents.CubeHeight := max.Y - min.Y;
  CubeExtents.CubeDepth := max.Z - min.Z;
  CubeExtents.Position.AsAffineVector := VectorLerp(min, max, 0.5);
  StatusBar.Panels[0].Text := 'X: ' + ' ';
  StatusBar.Panels[1].Text := 'Y: ' + ' ';
  StatusBar.Panels[2].Text := 'Z: ' + ' ';
  Name := ExtractFileName(FileName);
  StatusBar.Panels[3].Text := Name;
  DoResetCamera;
end;

procedure TMainForm.snViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  md := True;
end;

procedure TMainForm.snViewerMouseLeave(Sender: TObject);
begin
  Cadencer.Enabled := False;
end;

procedure TMainForm.snViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  d: Single;
begin
  if md and (Shift <> []) then
  begin
    if ssLeft in Shift then
      if ssShift in Shift then
        Camera.MoveAroundTarget((my - Y) * 0.1, (mx - X) * 0.1)
      else
        Camera.MoveAroundTarget(my - Y, mx - X)
    else if ssRight in Shift then
    begin
      d := Camera.DistanceToTarget * 0.01 * (X - mx + Y - my);
      if IsKeyDown('x') then
        ffObject.Translate(d, 0, 0)
      else if IsKeyDown('y') then
        ffObject.Translate(0, d, 0)
      else if IsKeyDown('z') then
        ffObject.Translate(0, 0, d)
      else
      begin
        if ssShift in Shift then
          Camera.RotateObject(ffObject, (my - Y) * 0.1, (mx - X) * 0.1)
        else
          Camera.RotateObject(ffObject, my - Y, mx - X);
      end;
    end;
    mx := X;
    my := Y;
  end;
end;

procedure TMainForm.snViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  md := False;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ffObject.MeshObjects.Count > 0 then
  begin
    Camera.AdjustDistanceToTarget(Power(1.05, WheelDelta / 120));
    Camera.DepthOfView := 2 * Camera.DistanceToTarget + 2 *
      ffObject.BoundingSphereRadius;
  end;
  Handled := True;
end;

procedure TMainForm.MaterialLibTextureNeeded(Sender: TObject;
  var textureFileName: String);
begin
  if not acToolsTexturing.Enabled then
    textureFileName := '';
end;

procedure TMainForm.acInvertNormalsExecute(Sender: TObject);
var
  I: Integer;
begin
  with ffObject.MeshObjects do
    for I := 0 to Count - 1 do
      Items[I].Normals.Scale(-1);
  ffObject.StructureChanged;
end;

procedure TMainForm.acReverseRenderingOrderExecute(Sender: TObject);
var
  I, j, n: Integer;
  fg: TGLFaceGroup;
begin
  with ffObject.MeshObjects do
  begin
    // invert meshobjects order
    for I := 0 to (Count div 2) do
      Exchange(I, Count - 1 - I);
    // for each mesh object
    for I := 0 to Count - 1 do
      with Items[I] do
      begin
        // invert facegroups order
        n := FaceGroups.Count;
        for j := 0 to (n div 2) do
          Exchange(j, n - 1 - j);
        // for each facegroup
        for j := 0 to n - 1 do
        begin
          fg := FaceGroups[j];
          fg.Reverse;
        end;
      end;
  end;
  ffObject.StructureChanged;
end;

procedure TMainForm.acSaveAsUpdate(Sender: TObject);
begin
  acFileSaveAs.Enabled := (ffObject.MeshObjects.Count > 0);
end;

procedure TMainForm.acHelpAboutExecute(Sender: TObject);
begin
  with TGLAbout.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.acAADefaultExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := True;
  ApplyFSAA;
end;

procedure TMainForm.acConvertToIndexedTrianglesExecute(Sender: TObject);
var
  v: TGLAffineVectorList;
  I: TGLIntegerList;
  m: TGLMeshObject;
  fg: TFGVertexIndexList;
begin
  v := ffObject.MeshObjects.ExtractTriangles;
  try
    I := BuildVectorCountOptimizedIndices(v);
    try
      RemapAndCleanupReferences(v, I);
      IncreaseCoherency(I, 12);
      I.Capacity := I.Count;
      ffObject.MeshObjects.Clean;
      m := TGLMeshObject.CreateOwned(ffObject.MeshObjects);
      m.Vertices := v;
      m.BuildNormals(I, momTriangles);
      m.Mode := momFaceGroups;
      fg := TFGVertexIndexList.CreateOwned(m.FaceGroups);
      fg.VertexIndices := I;
      fg.Mode := fgmmTriangles;
      ffObject.StructureChanged;
    finally
      I.Free;
    end;
  finally
    v.Free;
  end;
  MaterialLib.Materials.Clear;
  SetupFreeFormShading;
end;

procedure TMainForm.acStripifyExecute(Sender: TObject);
var
  I: Integer;
  mo: TGLMeshObject;
  fg: TFGVertexIndexList;
  strips: TGLPersistentObjectList;
begin
  acConvertToTriangles.Execute;
  mo := ffObject.MeshObjects[0];
  fg := (mo.FaceGroups[0] as TFGVertexIndexList);
  strips := StripifyMesh(fg.VertexIndices, mo.Vertices.Count, True);
  try
    fg.Free;
    for I := 0 to strips.Count - 1 do
    begin
      fg := TFGVertexIndexList.CreateOwned(mo.FaceGroups);
      fg.VertexIndices := (strips[I] as TGLIntegerList);
      if I = 0 then
        fg.Mode := fgmmTriangles
      else
        fg.Mode := fgmmTriangleStrip;
    end;
  finally
    strips.Free;
  end;
end;

procedure TMainForm.acViewFlatShadingExecute(Sender: TObject);
begin
  ApplyShadeMode;
end;

procedure TMainForm.acViewHiddenLinesExecute(Sender: TObject);
begin
  ApplyShadeMode;
end;

procedure TMainForm.acViewResetExecute(Sender: TObject);
begin
  DoResetCamera;
end;

procedure TMainForm.acViewFlatLinesExecute(Sender: TObject);
begin
  ApplyShadeMode;
end;

procedure TMainForm.acViewSmoothShadingExecute(Sender: TObject);
begin
  ApplyShadeMode;
end;

procedure TMainForm.acViewWireFrameExecute(Sender: TObject);
begin
  ApplyShadeMode;
end;

procedure TMainForm.acViewZoomInExecute(Sender: TObject);
var
  h: Boolean;
begin
  FormMouseWheel(Self, [], -120 * 4, Point(0, 0), h);
end;

procedure TMainForm.acViewZoomOutExecute(Sender: TObject);
var
  h: Boolean;
begin
  FormMouseWheel(Self, [], 120 * 4, Point(0, 0), h);
end;

procedure TMainForm.acOptimizeExecute(Sender: TObject);
begin
  OptimizeMesh(ffObject.MeshObjects, [mooVertexCache, mooSortByMaterials]);
  ffObject.StructureChanged;
  SetupFreeFormShading;
end;

procedure TMainForm.acToolsOptionsExecute(Sender: TObject);
begin
  with TGLOptions.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.acToolsFaceCullingExecute(Sender: TObject);
begin
  acToolsFaceCulling.Checked := not acToolsFaceCulling.Checked;
  ApplyFaceCull;
end;

procedure TMainForm.acToolsInfoExecute(Sender: TObject);
begin
  with TGLDialog.Create(Self) do
    try
      Memo.Lines[0] := 'Triangles: ' +
        IntToStr(ffObject.MeshObjects.TriangleCount);
      Memo.Lines[1] := 'Area: ' + FloatToStr(ffObject.MeshObjects.Area);
      Memo.Lines[2] := 'Volume: ' + FloatToStr(ffObject.MeshObjects.Volume);
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.acToolsLightingExecute(Sender: TObject);
begin
  acToolsLighting.Checked := not acToolsLighting.Checked;
  // TBLighting
  ApplyShadeMode;
end;

procedure TMainForm.acToolsShowFPSExecute(Sender: TObject);
begin
  acToolsShowFPS.Checked := not acToolsShowFPS.Checked;
  ApplyFPS;
end;

procedure TMainForm.acToolsTexturingExecute(Sender: TObject);
begin
  acToolsTexturing.Checked := not acToolsTexturing.Checked;
  if acToolsTexturing.Checked then
    if lastLoadWithTextures then
      ApplyTexturing
    else
    begin
      DoOpen(lastFileName);
    end
  else
    ApplyTexturing;
end;

procedure TMainForm.acToolsNaviCubeExecute(Sender: TObject);
begin
  acToolsNaviCube.Checked := not acToolsNaviCube.Checked;
  if acToolsNaviCube.Checked = True then
  begin
    NaviCube.Visible := True;
    Cadencer.Enabled := True;
  end
  else
  begin
    NaviCube.Visible := False;
    Cadencer.Enabled := False;
  end;
  snViewer.Invalidate;
end;


// Show Base and Additional Objects
procedure TMainForm.acPointsExecute(Sender: TObject);
var
  I: Integer;
  Color: TVector3f;
  NumPoints: Integer;
  X, Y, Z: Single;

begin
  NumPoints := 10000;
  GLPoints := TGLPoints(dcWorld.AddNewChild(TGLPoints));
  GLPoints.Size := 5.0;
  GLPoints.Style := psSmooth;
  for I := 0 to NumPoints - 1 do
  begin
    Color.X := Random();
    Color.Y := Random();
    Color.Z := Random();

    X := Random(10) - 5;
    Y := Random(10) - 5;
    Z := Random(10) - 5;

    GLPoints.Positions.Add(X * 0.05, Y * 0.05, Z * 0.05);
    // Fill array of GLPoints
    GLPoints.Colors.AddPoint(Color);
  end;
//  dcWorld.Remove(GLPoints, False);
//  GLPoints := TGLPoints(dcWorld.AddNewChild(TGLPoints));
end;

(*
procedure TMainForm.acDeletePoints(Sender: TObject);
var
  I: Integer;
  Color: TVector3f;
  NumPoints: Integer;
  X, Y, Z: Single;

begin
  NumPoints := 10000;
  GLPoints := TGLPoints(dcWorld.AddNewChild(TGLPoints));
  for I := 0 to NumPoints - 1 do
  begin
    Color.X := Random();
    Color.Y := Random();
    Color.Z := Random();

    X := Random(100) - 50;
    Y := Random(100) - 50;
    Z := Random(100) - 50;

    GLPoints.Positions.Add(X * 0.05, Y * 0.05, Z * 0.05);
    // Fill array of GLPoints
  end;
end;
*)

procedure TMainForm.CadencerProgress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if NaviCube.InactiveTime > 5 then
  begin
    if NaviCube.InactiveTime < 8 then
      Camera.TurnAngle := Camera.TurnAngle + (NaviCube.InactiveTime - 5) *
        deltaTime * 2
    else
      Camera.TurnAngle := Camera.TurnAngle + deltaTime * 6;
  end;
  snViewer.Refresh;
  if Self.Focused then
    snViewer.Invalidate;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
//  StatusBar.Panels[3].Text := Format('%.1f  FPS', [snViewer.FramesPerSecond]);
  snViewer.ResetPerformanceMonitor;
end;

procedure TMainForm.ReadIniFile;
begin
  inherited;
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    Top := IniFile.ReadInteger(Name, 'Top', 100);
    Left := IniFile.ReadInteger(Name, 'Left', 200);
  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, 'Top', Top);
      WriteInteger(Name, 'Left', Left);
      // WriteBool(Name, 'InitMax', WindowState = wsMaximized);
    finally
      IniFile.Free;
    end;
  inherited;
end;

end.
