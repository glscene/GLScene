unit fArchipelagoD;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes, 
  System.Math,
  Vcl.StdCtrls, 
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.ExtCtrls,
  Vcl.ComCtrls, 
  Vcl.Forms, 
  Vcl.Dialogs, 
  Vcl.Imaging.Jpeg,

  GLS.Scene, 
  GLS.Cadencer, 
  GLS.Objects, 
  GLS.TerrainRenderer,
  GLS.HeightData,
  GLS.HeightTileFileHDS,
  GLS.Texture,
  GLS.HUDObjects, 
  GLS.Material,
  GLS.SkyDome, 
  GLS.SceneViewer, 
  GLS.WindowsFont,
  GLS.BitmapFont, 
  GLS.Coordinates, 
  GLS.RenderContextInfo, 
  GLS.Color,
  GLS.VectorFileObjects, 
  GLS.BaseClasses, 
  GLS.VectorLists, 
  GLS.VectorTypes,
  GLS.VectorGeometry, 
  GLS.Keyboard, 
  GLS.OpenGLTokens, 
  GLS.Context, 
  GLS.State, 
  GLS.TextureFormat,
  GLS.File3DS;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera: TGLCamera;
    DCCamera: TGLDummyCube;
    TerrainRenderer: TGLTerrainRenderer;
    Timer1: TTimer;
    GLCadencer: TGLCadencer;
    MaterialLibrary: TGLMaterialLibrary;
    HTFPS: TGLHUDText;
    SkyDome: TGLSkyDome;
    GLHeightTileFileHDS1: TGLHeightTileFileHDS;
    BFSmall: TGLWindowsBitmapFont;
    GLCustomHDS1: TGLCustomHDS;
    PAProgress: TPanel;
    ProgressBar: TProgressBar;
    Label1: TLabel;
    GLMemoryViewer1: TGLMemoryViewer;
    MLSailBoat: TGLMaterialLibrary;
    FFSailBoat: TGLFreeForm;
    LSSun: TGLLightSource;
    BFLarge: TGLWindowsBitmapFont;
    HTHelp: TGLHUDText;
    DOWake: TGLDirectOpenGL;
    GLDummyCube1: TGLDummyCube;
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure GLCustomHDS1MarkDirtyEvent(const area: TRect);
    procedure GLCustomHDS1StartPreparingData(heightData: TGLHeightData);
    procedure GLSceneViewerBeforeRender(Sender: TObject);
    procedure DOWakeProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure DOWakeRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure TerrainRendererHeightDataPostRender(var rci: TGLRenderContextInfo;
      var HeightDatas: TList);
  public
    FullScreen: Boolean;
    CamHeight: Single;
    WaterPolyCount: Integer;
    WaterPlane: Boolean;
    WasAboveWater: Boolean;
    HelpOpacity: Single;
    DataPath : String;
    WakeVertices: TGLAffineVectorList;
    WakeStretch: TGLAffineVectorList;
    WakeTime: TGLSingleList;
    procedure ResetMousePos;
    function WaterPhase(const px, py: Single): Single;
    function WaterHeight(const px, py: Single): Single;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

const
  cWaterLevel = -10000;
  cWaterOpaqueDepth = 2000;
  cWaveAmplitude = 120;

procedure TForm1.FormCreate(Sender: TObject);
var
  i, j: Integer;
  name: string;
  libMat: TGLLibMaterial;
begin
  DataPath := ExtractFilePath(ParamStr(0));
  //Delete(DataPath, Length(DataPath) - 12, 12); // del if Win32\Debug\
  DataPath := DataPath + 'Data\';
  SetCurrentDir(DataPath);

  MaterialLibrary.TexturePaths := DataPath;
  MLSailBoat.TexturePaths := DataPath;

  GLCustomHDS1.MaxPoolSize := 8 * 1024 * 1024;
  GLCustomHDS1.DefaultHeight := cWaterLevel;

  // load texmaps
  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      name := Format('Tex_%d_%d.bmp', [i, j]);
      if not FileExists(name) then
      begin
        ShowMessage('Texture file ' + name + ' not found...'#13#10
          + 'Did you run "splitter.exe" as said in the readme.txt?');
        Application.Terminate;
        Exit;
      end;
      libMat := MaterialLibrary.AddTextureMaterial(name, name, False);
      with libMat.Material.Texture do
      begin
        TextureMode := tmReplace;
        TextureWrap := twNone;
        Compression := tcStandard; // comment out to turn off texture compression
         //        FilteringQuality := tfAnisotropic;
      end;
      libMat.Texture2Name := 'detail';
    end;

  // Initial camera height offset (controled with pageUp/pageDown)
  CamHeight := 20;

  // Water plane active
  WaterPlane := True;

  // load the sailboat
  (*  lost material for sailboat
  FFSailBoat.LoadFromFile('sailboat.glsm');
  MLSailBoat.LoadFromFile('sailboat.glml');
  *)
  FFSailBoat.LoadFromFile('boat.3ds');
  FFSailBoat.Position.SetPoint(-125 * TerrainRenderer.Scale.X, 0, -100 * TerrainRenderer.Scale.Z);
  FFSailBoat.TurnAngle := -30;
  // boost ambient
  for i := 0 to MLSailBoat.Materials.Count - 1 do
    with MLSailBoat.Materials[i].Material.FrontProperties do
      Ambient.Color := Diffuse.Color;

  // Move camera starting point near the sailboat
  DCCamera.Position := FFSailBoat.Position;
  DCCamera.Translate(25, 0, -15);
  DCCamera.Turn(200);

  // Help text
  HTHelp.Text := 'Archipelago Demo'#13#10#13#10
    + '* : Increase CLOD precision'#13#10
    + '/ : decrease CLOD precision'#13#10
    + 'W : wireframe on/off'#13#10
    + 'S : sea surface on/off'#13#10
    + 'B : sailboat visible on/off'#13#10
    + 'Num4 & Num6 : steer the sailboat'#13#10
    + 'F1: show this help';
  HTHelp.Position.SetPoint(Screen.Width div 2 - 100,
    Screen.Height div 2 - 150, 0);
  HelpOpacity := 4;
  GLSceneViewer1.Cursor := crNone;
end;

procedure TForm1.ResetMousePos;
begin
  if GLSceneViewer1.Cursor = crNone then
    SetCursorPos(Screen.Width div 2, Screen.Height div 2);
end;

procedure TForm1.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
  speed, alpha, f: Single;
  terrainHeight, surfaceHeight: Single;
  sbp: TGLVector;
  newMousePos: TPoint;
begin
  // handle keypresses
  if IsKeyDown(VK_SHIFT) then
    speed := 100 * deltaTime
  else
    speed := 20 * deltaTime;
  with GLCamera.Position do
  begin
    if IsKeyDown(VK_UP) then
      DCCamera.Position.AddScaledVector(speed, GLCamera.AbsoluteVectorToTarget);
    if IsKeyDown(VK_DOWN) then
      DCCamera.Position.AddScaledVector(-speed, GLCamera.AbsoluteVectorToTarget);
    if IsKeyDown(VK_LEFT) then
      DCCamera.Position.AddScaledVector(-speed, GLCamera.AbsoluteRightVectorToTarget);
    if IsKeyDown(VK_RIGHT) then
      DCCamera.Position.AddScaledVector(speed, GLCamera.AbsoluteRightVectorToTarget);
    if IsKeyDown(VK_PRIOR) then
      CamHeight := CamHeight + speed;
    if IsKeyDown(VK_NEXT) then
      CamHeight := CamHeight - speed;
    if IsKeyDown(VK_ESCAPE) then
      Close;
  end;
  if IsKeyDown(VK_F1) then
    HelpOpacity := ClampValue(HelpOpacity + deltaTime * 3, 0, 3);
  if IsKeyDown(VK_NUMPAD4) then
    FFSailBoat.Turn(-deltaTime * 3);
  if IsKeyDown(VK_NUMPAD6) then
    FFSailBoat.Turn(deltaTime * 3);

  // mouse movements and actions
  if IsKeyDown(VK_LBUTTON) then
  begin
    alpha := DCCamera.Position.Y;
    DCCamera.Position.AddScaledVector(speed, GLCamera.AbsoluteVectorToTarget);
    CamHeight := CamHeight + DCCamera.Position.Y - alpha;
  end;
  if IsKeyDown(VK_RBUTTON) then
  begin
    alpha := DCCamera.Position.Y;
    DCCamera.Position.AddScaledVector(-speed, GLCamera.AbsoluteVectorToTarget);
    CamHeight := CamHeight + DCCamera.Position.Y - alpha;
  end;
  GetCursorPos(newMousePos);
  GLCamera.MoveAroundTarget((Screen.Height div 2 - newMousePos.Y) * 0.25,
    (Screen.Width div 2 - newMousePos.X) * 0.25);
  ResetMousePos;

  // don't drop our target through terrain!
  with DCCamera.Position do
  begin
    terrainHeight := TerrainRenderer.InterpolatedHeight(AsVector);
    surfaceHeight := TerrainRenderer.Scale.Z * cWaterLevel / 128;
    if terrainHeight < surfaceHeight then
      terrainHeight := surfaceHeight;
    Y := terrainHeight + CamHeight;
  end;
  // adjust fog distance/color for air/water
  if (GLCamera.AbsolutePosition.Y > surfaceHeight) or (not WaterPlane) then
  begin
    if not WasAboveWater then
    begin
      SkyDome.Visible := True;
      with GLSceneViewer1.Buffer.FogEnvironment do
      begin
        FogColor.Color := clrWhite;
        FogEnd := 1000;
        FogStart := 500;
      end;
      GLSceneViewer1.Buffer.BackgroundColor := clWhite;
      GLCamera.DepthOfView := 1000;
      WasAboveWater := True;
    end;
  end
  else
  begin
    if WasAboveWater then
    begin
      SkyDome.Visible := False;
      with GLSceneViewer1.Buffer.FogEnvironment do
      begin
        FogColor.AsWinColor := clNavy;
        FogEnd := 100;
        FogStart := 0;
      end;
      GLSceneViewer1.Buffer.BackgroundColor := clNavy;
      GLCamera.DepthOfView := 100;
      WasAboveWater := False;
    end;
  end;
  // help visibility
  if HelpOpacity > 0 then
  begin
    HelpOpacity := HelpOpacity - deltaTime;
    alpha := ClampValue(HelpOpacity, 0, 1);
    if alpha > 0 then
    begin
      HTHelp.Visible := True;
      HTHelp.ModulateColor.Alpha := alpha;
    end
    else
      HTHelp.Visible := False;
  end;
  // rock the sailboat
  sbp := TerrainRenderer.AbsoluteToLocal(FFSailBoat.AbsolutePosition);
  alpha := WaterPhase(sbp.X + TerrainRenderer.TileSize * 0.5, sbp.Y + TerrainRenderer.TileSize * 0.5);
  FFSailBoat.Position.Y := (cWaterLevel + Sin(alpha) * cWaveAmplitude) * (TerrainRenderer.Scale.Z / 128)
    + 4;
  f := cWaveAmplitude * 0.01;
  FFSailBoat.Up.SetVector(Cos(alpha) * 0.02 * f, 1, (Sin(alpha) * 0.02 - 0.005) * f);
  FFSailBoat.Move(deltaTime * 2);
end;


procedure TForm1.TerrainRendererHeightDataPostRender(
  var rci: TGLRenderContextInfo; var HeightDatas: TList);
var
  i, x, y, s, s2: Integer;
  t: Single;
  hd: TGLHeightData;
const
  r = 0.75;
  g = 0.75;
  b = 1;

  procedure IssuePoint(rx, ry: Integer);
  var
    px, py: Single;
    alpha, colorRatio, ca, sa: Single;
  begin
    px := x + rx + s2;
    py := y + ry + s2;
    if hd.DataState = hdsNone then
    begin
      alpha := 1;
    end
    else
    begin
      alpha := (cWaterLevel - hd.SmallIntHeight(rx, ry)) * (1 / cWaterOpaqueDepth);
      alpha := ClampValue(alpha, 0.5, 1);
    end;
    SinCos(WaterPhase(px, py), sa, ca);
    colorRatio := 1 - alpha * 0.1;
    glColor4f(r * colorRatio, g * colorRatio, b, alpha);
    glTexCoord2f(px * 0.01 + 0.002 * sa, py * 0.01 + 0.0022 * ca - t * 0.002);
    glVertex3f(px, py, cWaterLevel + cWaveAmplitude * sa);
  end;

begin
  if not WaterPlane then
    Exit;
  t := GLCadencer.CurrentTime;
  MaterialLibrary.ApplyMaterial('water', rci);
  repeat
    with rci.GLStates do
    begin
      if not WasAboveWater then
        InvertGLFrontFace;
      Disable(stLighting);
      Disable(stNormalize);
      SetStencilFunc(cfAlways, 1, 255);
      StencilWriteMask := 255;
      Enable(stStencilTest);
      SetStencilOp(soKeep, soKeep, soReplace);

      glNormal3f(0, 0, 1);

      for i := 0 to heightDatas.Count - 1 do
      begin
        hd := TGLHeightData(heightDatas.List[i]);
        if (hd.DataState = hdsReady) and (hd.HeightMin > cWaterLevel) then
          continue;
        x := hd.XLeft;
        y := hd.YTop;
        s := hd.Size - 1;
        s2 := s div 2;
        glBegin(GL_TRIANGLE_FAN);
        IssuePoint(s2, s2);
        IssuePoint(0, 0);
        IssuePoint(s2, 0);
        IssuePoint(s, 0);
        IssuePoint(s, s2);
        IssuePoint(s, s);
        IssuePoint(s2, s);
        IssuePoint(0, s);
        IssuePoint(0, s2);
        IssuePoint(0, 0);
        glEnd;
      end;
      SetStencilOp(soKeep, soKeep, soKeep);
      Disable(stStencilTest);
    end;

    if not WasAboveWater then
      rci.GLStates.InvertGLFrontFace;
    WaterPolyCount := heightDatas.Count * 8;
  until not MaterialLibrary.UnApplyMaterial(rci);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  HTFPS.Text := Format('%.1f FPS - %d - %d',
    [GLSceneViewer1.FramesPerSecond,
    TerrainRenderer.LastTriangleCount,
      WaterPolyCount]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
var
  i: Integer;
  pm: TGLPolygonMode;
begin
  case Key of
    'w', 'W':
      begin
        with MaterialLibrary do
        begin
          if Materials[0].Material.PolygonMode = pmLines then
            pm := pmFill
          else
            pm := pmLines;
          for i := 0 to Materials.Count - 1 do
            Materials[i].Material.PolygonMode := pm;
        end;
        with MLSailBoat do
          for i := 0 to Materials.Count - 1 do
            Materials[i].Material.PolygonMode := pm;
        FFSailBoat.StructureChanged;
      end;
    's', 'S': WaterPlane := not WaterPlane;
    'b', 'B': FFSailBoat.Visible := not FFSailBoat.Visible;
    '*': with TerrainRenderer do
        if CLODPrecision > 1 then
          CLODPrecision := Round(CLODPrecision * 0.8);
    '/': with TerrainRenderer do
        if CLODPrecision < 1000 then
          CLODPrecision := Round(CLODPrecision * 1.2 + 1);
  end;
  Key := #0;
end;

procedure TForm1.GLCustomHDS1MarkDirtyEvent(const area: TRect);
begin
  GLHeightTileFileHDS1.MarkDirty(area);
end;

procedure TForm1.GLCustomHDS1StartPreparingData(heightData: TGLHeightData);
var
  htfHD: TGLHeightData;
  i, j, n: Integer;
  offset: TTexPoint;
begin

  htfHD := GLHeightTileFileHDS1.GetData(heightData.XLeft, heightData.YTop, heightData.Size, heightData.DataType);
  if (htfHD.DataState = hdsNone) then //or (htfHD.HeightMax<=cWaterLevel-cWaterOpaqueDepth) then
    heightData.DataState := hdsNone
  else
  begin
    i := (heightData.XLeft div 128);
    j := (heightData.YTop div 128);
    if (Cardinal(i) < 4) and (Cardinal(j) < 4) then
    begin
      heightData.MaterialName := format('Tex_%d_%d.bmp', [i, j]);
      heightData.TextureCoordinatesMode := tcmLocal;
      n := ((heightData.XLeft div 32) and 3);
      offset.S := n * 0.25;
      n := ((heightData.YTop div 32) and 3);
      offset.T := -n * 0.25;
      heightData.TextureCoordinatesOffset := offset;
      heightData.TextureCoordinatesScale := TexPointMake(0.25, 0.25);
      heightData.DataType := hdtSmallInt;
      htfHD.DataType := hdtSmallInt;
      heightData.Allocate(hdtSmallInt);
      Move(htfHD.SmallIntData^, heightData.SmallIntData^, htfHD.DataSize);
      heightData.DataState := hdsReady;
      heightData.HeightMin := htfHD.HeightMin;
      heightData.HeightMax := htfHD.HeightMax;
    end
    else
      heightData.DataState := hdsNone
  end;
  GLHeightTileFileHDS1.Release(htfHD);
end;

procedure TForm1.GLSceneViewerBeforeRender(Sender: TObject);
var
  i, n: Integer;
begin
  PAProgress.Left := (Width - PAProgress.Width) div 2;
  PAProgress.Visible := True;
  n := MaterialLibrary.Materials.Count;
  ProgressBar.Max := n - 1;
  try
    for i := 0 to n - 1 do
    begin
      ProgressBar.Position := i;
      MaterialLibrary.Materials[i].Material.Texture.Handle;
      PAProgress.Repaint;
    end;
  finally
    ResetMousePos;
    PAProgress.Visible := False;
    GLSceneViewer1.BeforeRender := nil;
  end;
end;


function TForm1.WaterPhase(const px, py: Single): Single;
begin
  Result := GLCadencer.CurrentTime * 1 + px * 0.16 + py * 0.09;
end;

function TForm1.WaterHeight(const px, py: Single): Single;
var
  alpha: Single;
begin
  alpha := WaterPhase(px + TerrainRenderer.TileSize * 0.5,
    py + TerrainRenderer.TileSize * 0.5);
  Result := (cWaterLevel + Sin(alpha) * cWaveAmplitude) * (TerrainRenderer.Scale.Z * (1 / 128));
end;

procedure TForm1.DOWakeProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
  i: Integer;
  sbp, sbr: TGLVector;
begin
  if WakeVertices = nil then
  begin
    WakeVertices := TGLAffineVectorList.Create;
    WakeStretch := TGLAffineVectorList.Create;
    WakeTime := TGLSingleList.Create;
  end;

  // enlarge current vertices
  with WakeVertices do
  begin
    i := 0;
    while i < Count do
    begin
      CombineItem(i, WakeStretch.List[i shr 1], -0.45 * deltaTime);
      CombineItem(i + 1, WakeStretch.List[i shr 1], 0.45 * deltaTime);
      Inc(i, 2);
    end;
  end;

  // Progress wake
  if newTime > DOWake.TagFloat then
  begin
    if DOWake.TagFloat = 0 then
    begin
      DOWake.TagFloat := newTime + 0.2;
      Exit;
    end;
    DOWake.TagFloat := newTime + 1;
    sbp := VectorCombine(FFSailBoat.AbsolutePosition, FFSailBoat.AbsoluteDirection, 1, 3);
    sbr := FFSailBoat.AbsoluteRight;
    // add new
    WakeVertices.Add(VectorCombine(sbp, sbr, 1, -2));
    WakeVertices.Add(VectorCombine(sbp, sbr, 1, 2));
    WakeStretch.Add(VectorScale(sbr, (0.95 + Random * 0.1)));
    WakeTime.Add(newTime * 0.1);
    if WakeVertices.Count >= 80 then
    begin
      WakeVertices.Delete(0);
      WakeVertices.Delete(0);
      WakeStretch.Delete(0);
      WakeTime.Delete(0);
    end;
  end;
end;

procedure TForm1.DOWakeRender(Sender: TObject; var rci: TGLRenderContextInfo);
var
  i: Integer;
  p: PAffineVector;
  sbp: TGLVector;
  c: Single;
begin
  if not Assigned(WakeVertices) then
    Exit;
  if (not FFSailBoat.Visible) or (not WaterPlane) then
    Exit;

  MaterialLibrary.ApplyMaterial('wake', rci);
  repeat
    with rci.GLStates do
    begin
      Disable(stLighting);
      Disable(stFog);
      Enable(stBlend);
      SetBlendFunc(bfOne, bfOne);

      SetStencilFunc(cfEqual, 1, 255);
      StencilWriteMask := 255;
      Enable(stStencilTest);
      SetStencilOp(soKeep, soKeep, soKeep);
      Disable(stDepthTest);

      if not WasAboveWater then
        InvertGLFrontFace;

      glBegin(GL_TRIANGLE_STRIP);
      for i := 0 to WakeVertices.Count - 1 do
      begin
        p := @WakeVertices.List[i xor 1];
        sbp := TerrainRenderer.AbsoluteToLocal(VectorMake(p^));
        if (i and 1) = 0 then
        begin
          c := (i and $FFE) * 0.2 / WakeVertices.Count;
          glColor3f(c, c, c);
          glTexCoord2f(0, WakeTime[i div 2]);
        end
        else
          glTexCoord2f(1, WakeTime[i div 2]);
        glVertex3f(p.X, WaterHeight(sbp.X, sbp.Y), p.Z);
      end;
      glEnd;

      if not WasAboveWater then
        InvertGLFrontFace;
      Disable(stStencilTest);
    end;

  until not MaterialLibrary.UnApplyMaterial(rci);
end;

end.

