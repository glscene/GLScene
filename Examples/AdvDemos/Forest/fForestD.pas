unit fForestD;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.Jpeg,
  Vcl.ExtCtrls,

  Stage.VectorTypes,
  Stage.VectorGeometry,
  Stage.OpenGLTokens,
  GLS.XCollection,
  GLS.VectorLists,
  GLS.PersistentClasses,
  GLS.BaseClasses,
  Stage.TextureFormat,
  Stage.Keyboard,

  GLS.SceneViewer,
  GLS.Cadencer,
  GLS.Texture,
  GLS.Scene,
  GLS.Objects,
  GLS.Tree,
  GLS.BitmapFont,
  GLS.Context,
  GLS.WindowsFont,
  GLS.HUDObjects,
  GLS.SkyDome,
  GLS.Imposter,
  GLS.ParticleFX,
  GLS.Graphics,
  GLS.XOpenGL,
  Stage.PipelineTransform,
  GLS.TextureCombiners,
  GLS.Material,
  GLS.Coordinates,
  GLS.TerrainRenderer,
  GLS.HeightData,
  GLS.HeightTileFileHDS,
  GLS.RenderContextInfo,
  GLS.Screen,
  GLS.State,
  GLS.FileTGA,
  Stage.Utils;

type
  TFormForest = class(TForm)
    SceneViewer: TGLSceneViewer;
    GLScene: TGLScene;
    MLTrees: TGLMaterialLibrary;
    MLTerrain: TGLMaterialLibrary;
    GLCadencer: TGLCadencer;
    Terrain: TGLTerrainRenderer;
    Camera: TGLCamera;
    Light: TGLLightSource;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    EarthSkyDome: TGLEarthSkyDome;
    GLRenderPoint: TGLRenderPoint;
    SIBTree: TGLStaticImposterBuilder;
    DOTrees: TGLDirectOpenGL;
    PFXTrees: TGLCustomPFXManager;
    RenderTrees: TGLParticleFXRenderer;
    Timer1: TTimer;
    MLWater: TGLMaterialLibrary;
    DOInitializeReflection: TGLDirectOpenGL;
    DOGLSLWaterPlane: TGLDirectOpenGL;
    DOClassicWaterPlane: TGLDirectOpenGL;
    GLHeightTileFileHDS: TGLHeightTileFileHDS;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TerrainGetTerrainBounds(var l, t, r, b: Single);
    procedure GLCadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DOTreesRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure PFXTreesBeginParticles(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure PFXTreesCreateParticle(Sender: TObject; aParticle: TGLParticle);
    procedure PFXTreesEndParticles(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure PFXTreesRenderParticle(Sender: TObject; aParticle: TGLParticle;
      var rci: TGLRenderContextInfo);
    procedure SIBTreeImposterLoaded(Sender: TObject; impostoredObject: TGLBaseSceneObject;
      destImposter: TImposter);
    function SIBTreeLoadingImposter(Sender: TObject; impostoredObject: TGLBaseSceneObject;
      destImposter: TImposter): TGLBitmap32;
    procedure Timer1Timer(Sender: TObject);
    procedure PFXTreesProgress(Sender: TObject; const progressTime: TGLProgressTimes;
      var defaultProgress: Boolean);
    function PFXTreesGetParticleCountEvent(Sender: TObject): Integer;
    procedure FormResize(Sender: TObject);
    procedure DOInitializeReflectionRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure DOGLSLWaterPlaneRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure DOClassicWaterPlaneRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    // hscale, mapwidth, mapheight : Single;
    lmp: TPoint;
    camPitch, camTurn, camTime, curPitch, curTurn: Single;
    function GetTextureReflectionMatrix: TGLMatrix;
  public
    Path: TFileName;
    TestTree: TGLTree;
    TreesShown: Integer;
    nearTrees: TGLPersistentObjectList;
    Imposter: TImposter;
    densityBitmap: TBitmap;
    mirrorTexture: TGLTextureHandle;
    mirrorTexType: TGLTextureTarget;
    reflectionProgram: TGLProgramHandle;
    supportsGLSL: Boolean;
    enableGLSL: Boolean;
    enableRectReflection, enableTex2DReflection: Boolean;
  end;

var
  FormForest: TFormForest;

implementation // ------------------------------------------------------------

{$R *.dfm}

const
  cImposterCacheFile: string = 'imposters.bmp';
  cMapWidth: Integer = 1024;
  cMapHeight: Integer = 1024;
  cBaseSpeed: Single = 50;

procedure TFormForest.FormCreate(Sender: TObject);
var
  density: TPicture;

begin
  // go to 1024x768x32
  // SetFullscreenMode(GetIndexFromResolution(1024, 768, 32), 85);
  Application.OnDeactivate := FormDeactivate;

  Path := GetCurrentAssetPath();

  // Load volcano textures
  SetCurrentDir(Path + '\texture');

  MLTerrain.AddTextureMaterial('Terrain', 'volcano_TX_low.jpg').Texture2Name := 'Detail';
  MLTerrain.AddTextureMaterial('Detail', 'detailmap.jpg').Material.Texture.TextureMode :=
    tmModulate;
  MLTerrain.AddTextureMaterial('Detail', 'detailmap.jpg').TextureScale.SetPoint(128, 128, 128);
  Terrain.Material.MaterialLibrary := MLTerrain;
  Terrain.Material.LibMaterialName := 'Terrain';

  // Load textures for trees
  MLTrees.AddTextureMaterial('Leaf', 'leaf.tga').Material.Texture.TextureFormat := tfRGBA;
  MLTrees.AddTextureMaterial('Leaf', 'leaf.tga').Material.Texture.TextureMode := tmModulate;
  MLTrees.AddTextureMaterial('Leaf', 'leaf.tga').Material.Texture.MinFilter :=
    miNearestMipmapNearest;
  MLTrees.AddTextureMaterial('Leaf', 'leaf.tga').Material.BlendingMode := bmAlphaTest50;

  MLTrees.AddTextureMaterial('Bark', 'zbark_016.jpg').Material.Texture.TextureMode := tmModulate;

  // Create test tree
  Randomize;
  TestTree := TGLTree(GLScene.Objects.AddNewChild(TGLTree));
  with TestTree do
  begin
    Visible := False;
    MaterialLibrary := MLTrees;
    LeafMaterialName := 'Leaf';
    LeafBackMaterialName := 'Leaf';
    BranchMaterialName := 'Bark';
    Up.SetVector(ZHmgVector);
    Direction.SetVector(YHmgVector);
    Depth := 9;
    BranchFacets := 6;
    LeafSize := 0.50;
    BranchAngle := 0.65;
    BranchTwist := 135;
    ForceTotalRebuild;
  end;

  SIBTree.RequestImposterFor(TestTree);

  densityBitmap := TBitmap.Create;
  try
    densityBitmap.PixelFormat := pf24bit;
    density := TPicture.Create;
    try
      density.LoadFromFile('volcano_trees.jpg');
      densityBitmap.Width := density.Width;
      densityBitmap.Height := density.Height;
      densityBitmap.Canvas.Draw(0, 0, density.Graphic);
    finally
      density.Free;
    end;

    // Set directory to load landscapes
    SetCurrentDir(Path + '\landscape');
      PFXTrees.CreateParticles(10000);
  finally
    densityBitmap.Free;
  end;
  TreesShown := 2000;

  Light.Pitch(30);
  Camera.Position.Y := Terrain.InterpolatedHeight(Camera.Position.AsVector) + 10;

  lmp := ClientToScreen(Point(Width div 2, Height div 2));
  SetCursorPos(lmp.X, lmp.Y);
  ShowCursor(False);

  nearTrees := TGLPersistentObjectList.Create;

  camTurn := -60;
  enableRectReflection := False;
  enableTex2DReflection := False;

end;

//----------------------------------------------------------------

procedure TFormForest.FormDestroy(Sender: TObject);
begin
  // RestoreDefaultMode;

  ShowCursor(True);
  nearTrees.Free;
end;

procedure TFormForest.FormResize(Sender: TObject);
begin
  Camera.FocalLength := Width * 50 / 800;
end;

procedure TFormForest.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TFormForest.FormShow(Sender: TObject);
begin
  SetFocus;
end;

procedure TFormForest.GLCadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
var
  speed, z: Single;
  nmp: TPoint;
begin
  // Camera movement
  if IsKeyDown(VK_SHIFT) then
    speed := deltaTime * cBaseSpeed * 10
  else
    speed := deltaTime * cBaseSpeed;

  if IsKeyDown(VK_UP) or IsKeyDown('W') or IsKeyDown('Z') then
    Camera.Move(speed)
  else if IsKeyDown(VK_DOWN) or IsKeyDown('S') then
    Camera.Move(-speed);

  if IsKeyDown(VK_LEFT) or IsKeyDown('A') or IsKeyDown('Q') then
    Camera.Slide(-speed)
  else if IsKeyDown(VK_RIGHT) or IsKeyDown('D') then
    Camera.Slide(speed);

  z := Terrain.Position.Y + Terrain.InterpolatedHeight(Camera.Position.AsVector);
  if z < 0 then
    z := 0;
  z := z + 10;
  if Camera.Position.Y < z then
    Camera.Position.Y := z;

  GetCursorPos(nmp);
  camTurn := camTurn - (lmp.X - nmp.X) * 0.2;
  camPitch := camPitch + (lmp.Y - nmp.Y) * 0.2;
  camTime := camTime + deltaTime;
  while camTime > 0 do
  begin
    curTurn := Lerp(curTurn, camTurn, 0.2);
    curPitch := Lerp(curPitch, camPitch, 0.2);
    Camera.Position.Y := Lerp(Camera.Position.Y, z, 0.2);
    camTime := camTime - 0.01;
  end;
  Camera.ResetRotations;
  Camera.Turn(curTurn);
  Camera.Pitch(curPitch);
  SetCursorPos(lmp.X, lmp.Y);

  SceneViewer.Invalidate;
end;

procedure TFormForest.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      FormForest.Close;
    VK_ADD:
      if TreesShown < PFXTrees.Particles.ItemCount then
        TreesShown := TreesShown + 100;
    VK_SUBTRACT:
      if TreesShown > 0 then
        TreesShown := TreesShown - 100;
    Word('R'):
      enableTex2DReflection := not enableTex2DReflection;
    Word('G'):
      if supportsGLSL then
      begin
        enableGLSL := not enableGLSL;
        enableTex2DReflection := True;
      end;
  end;
end;

procedure TFormForest.Timer1Timer(Sender: TObject);
var
  hud: string;
begin
  hud := Format('%.1f FPS - %d trees'#13#10'Tree sort: %f ms',
    [SceneViewer.FramesPerSecond, TreesShown, RenderTrees.LastSortTime]);
  if enableTex2DReflection then
  begin
    hud := hud + #13#10 + 'Water reflections';
    if enableRectReflection then
      hud := hud + ' (RECT)';
  end;
  if enableGLSL and enableTex2DReflection then
    hud := hud + #13#10 + 'GLSL water';
  GLHUDText1.Text := hud;
  SceneViewer.ResetPerformanceMonitor;
  Caption := Format('%.2f', [RenderTrees.LastSortTime]);
end;

procedure TFormForest.PFXTreesCreateParticle(Sender: TObject; aParticle: TGLParticle);
var
  u, v, p: Single;
  // x, y, i, j, dark : Integer;
  pixelX, pixelY: Integer;
begin
  repeat
    repeat
      u := Random * 0.88 + 0.06;
      v := Random * 0.88 + 0.06;
      pixelX := Round(u * densityBitmap.Width);
      pixelY := Round(v * densityBitmap.Height);
      p := ((densityBitmap.Canvas.Pixels[pixelX, pixelY] shr 8) and 255) / 255;
    until p > Random;
    aParticle.PosX := (0.5 - u) * Terrain.Scale.X * cMapWidth;
    aParticle.PosY := 0;
    aParticle.PosZ := (0.5 - (1 - v)) * Terrain.Scale.Y * cMapHeight;
    aParticle.PosY := Terrain.Position.Y + Terrain.InterpolatedHeight(aParticle.Position);
  until aParticle.PosY >= 0;
  aParticle.Tag := Random(360);

end;

procedure TFormForest.PFXTreesBeginParticles(Sender: TObject; var rci: TGLRenderContextInfo);
begin
  Imposter := SIBTree.ImposterFor(TestTree);
  Imposter.BeginRender(rci);
end;

procedure TFormForest.PFXTreesRenderParticle(Sender: TObject; aParticle: TGLParticle;
  var rci: TGLRenderContextInfo);
const
  cTreeCenteringOffset: TAffineVector = (X: 0; Y: 30; z: 0);
var
  d: Single;
  camPos: TGLVector;
begin
  if not IsVolumeClipped(VectorAdd(aParticle.Position, cTreeCenteringOffset), 30, rci.rcci.frustum)
  then
  begin;
    VectorSubtract(rci.cameraPosition, aParticle.Position, camPos);
    d := VectorNorm(camPos);
    if d > Sqr(180) then
    begin
      RotateVectorAroundY(PAffineVector(@camPos)^, aParticle.Tag * cPIdiv180);
      Imposter.Render(rci, VectorMake(aParticle.Position), camPos, 10);
    end
    else
    begin
      nearTrees.Add(aParticle);
    end;
  end;
end;

procedure TFormForest.PFXTreesEndParticles(Sender: TObject; var rci: TGLRenderContextInfo);
var
  aParticle: TGLParticle;
  camPos: TGLVector;
begin
  // Only 20 trees max rendered at full res, force imposter'ing the others
  while nearTrees.Count > 20 do
  begin
    aParticle := TGLParticle(nearTrees.First);
    VectorSubtract(rci.cameraPosition, aParticle.Position, camPos);
    RotateVectorAroundY(PAffineVector(@camPos)^, aParticle.Tag * cPIdiv180);
    Imposter.Render(rci, VectorMake(aParticle.Position), camPos, 10);
    nearTrees.Delete(0);
  end;

  Imposter.EndRender(rci);
end;

procedure TFormForest.DOTreesRender(Sender: TObject; var rci: TGLRenderContextInfo);
var
  i: Integer;
  particle: TGLParticle;
  TreeModelMatrix: TGLMatrix;
begin
  rci.GLStates.Disable(stBlend);
  for i := 0 to nearTrees.Count - 1 do
  begin
    particle := TGLParticle(nearTrees[i]);
    TreeModelMatrix := MatrixMultiply(CreateTranslationMatrix(particle.Position),
      rci.PipelineTransformation.ViewMatrix^);
    TreeModelMatrix := MatrixMultiply(CreateScaleMatrix(VectorMake(10, 10, 10)), TreeModelMatrix);
    TreeModelMatrix := MatrixMultiply(CreateRotationMatrixY(DegToRad(-particle.Tag)),
      TreeModelMatrix);
    TreeModelMatrix := MatrixMultiply
      (CreateRotationMatrixX(DegToRad(Cos(GLCadencer.CurrentTime + particle.ID * 15) * 0.2)),
      TreeModelMatrix);
    TreeModelMatrix := MatrixMultiply
      (CreateRotationMatrixZ(DegToRad(Cos(GLCadencer.CurrentTime * 1.3 + particle.ID * 15) * 0.2)),
      TreeModelMatrix);
    TestTree.AbsoluteMatrix := TreeModelMatrix;
    TestTree.Render(rci);
  end;
  nearTrees.Clear;
end;

procedure TFormForest.TerrainGetTerrainBounds(var l, t, r, b: Single);
begin
  l := 0;
  t := cMapHeight;
  r := cMapWidth;
  b := 0;
end;

function TFormForest.SIBTreeLoadingImposter(Sender: TObject; impostoredObject: TGLBaseSceneObject;
  destImposter: TImposter): TGLBitmap32;
var
  bmp: TBitmap;
  cacheAge, exeAge: TDateTime;
begin
  Tag := 1;
  Result := nil;
  if not FileExists(cImposterCacheFile) then
    Exit;
  FileAge(cImposterCacheFile, cacheAge, True);
  FileAge(ParamStr(0), exeAge, True);
  if cacheAge < exeAge then
    Exit;

  Tag := 0;
  bmp := TBitmap.Create;
  bmp.LoadFromFile(cImposterCacheFile);
  Result := TGLBitmap32.Create;
  Result.Assign(bmp);
  bmp.Free;
end;

procedure TFormForest.SIBTreeImposterLoaded(Sender: TObject; impostoredObject: TGLBaseSceneObject;
  destImposter: TImposter);
var
  bmp32: TGLBitmap32;
  bmp: TBitmap;
begin
  if Tag = 1 then
  begin
    bmp32 := TGLBitmap32.Create;
    bmp32.AssignFromTexture2D(SIBTree.ImposterFor(TestTree).Texture);
    bmp := bmp32.Create32BitsBitmap;
    bmp.SaveToFile(cImposterCacheFile);
    bmp.Free;
    bmp32.Free;
  end;
end;

function TFormForest.PFXTreesGetParticleCountEvent(Sender: TObject): Integer;
begin
  Result := TreesShown;
end;

procedure TFormForest.PFXTreesProgress(Sender: TObject; const progressTime: TGLProgressTimes;
  var defaultProgress: Boolean);
begin
  defaultProgress := False;
end;

procedure TFormForest.DOInitializeReflectionRender(Sender: TObject; var rci: TGLRenderContextInfo);
var
  w, h: Integer;
  refMat: TGLMatrix;
  cameraPosBackup, cameraDirectionBackup: TGLVector;
  frustumBackup: TFrustum;
  clipPlane: TDoubleHmgPlane;
  glTarget: GLEnum;
begin
  supportsGLSL := GL.ARB_shader_objects and GL.ARB_fragment_shader and GL.ARB_vertex_shader;
  enableRectReflection := GL.NV_texture_rectangle and ((not enableGLSL) or GL.EXT_Cg_shader);

  if not enableTex2DReflection then
    Exit;

  if not Assigned(mirrorTexture) then
    mirrorTexture := TGLTextureHandle.Create;

  rci.PipelineTransformation.Push;

  // Mirror coordinates
  refMat := MakeReflectionMatrix(NullVector, YVector);
  rci.PipelineTransformation.ViewMatrix^ :=
    MatrixMultiply(refMat, rci.PipelineTransformation.ViewMatrix^);

  rci.GLStates.FrontFace := fwClockWise;

  GL.Enable(GL_CLIP_PLANE0);
  SetPlane(clipPlane, PlaneMake(AffineVectorMake(0, 1, 0), VectorNegate(YVector)));
  GL.clipPlane(GL_CLIP_PLANE0, @clipPlane);

  cameraPosBackup := rci.cameraPosition;
  cameraDirectionBackup := rci.cameraDirection;
  frustumBackup := rci.rcci.frustum;
  rci.cameraPosition := VectorTransform(rci.cameraPosition, refMat);
  rci.cameraDirection := VectorTransform(rci.cameraDirection, refMat);
  with rci.rcci.frustum do
  begin
    pLeft := VectorTransform(pLeft, refMat);
    pRight := VectorTransform(pRight, refMat);
    pTop := VectorTransform(pTop, refMat);
    pBottom := VectorTransform(pBottom, refMat);
    pNear := VectorTransform(pNear, refMat);
    pFar := VectorTransform(pFar, refMat);
  end;

  rci.PipelineTransformation.ViewMatrix^ := IdentityHmgMatrix;
  Camera.Apply;
  rci.PipelineTransformation.ViewMatrix^ :=
    MatrixMultiply(refMat, rci.PipelineTransformation.ViewMatrix^);

  EarthSkyDome.DoRender(rci, True, False);
  rci.PipelineTransformation.ModelMatrix^ := Terrain.AbsoluteMatrix;
  Terrain.DoRender(rci, True, False);

  rci.cameraPosition := cameraPosBackup;
  rci.cameraDirection := cameraDirectionBackup;
  rci.rcci.frustum := frustumBackup;

  // Restore to "normal"
  rci.PipelineTransformation.Pop;
  GLScene.SetupLights(TGLSceneBuffer(rci.buffer).LimitOf[limLights]);

  rci.GLStates.FrontFace := fwCounterClockWise;

  if enableRectReflection then
  begin
    mirrorTexType := ttTextureRect;
    w := SceneViewer.Width;
    h := SceneViewer.Height;
  end
  else
  begin
    mirrorTexType := ttTexture2D;
    w := RoundUpToPowerOf2(SceneViewer.Width);
    h := RoundUpToPowerOf2(SceneViewer.Height);
  end;
  glTarget := DecodeTextureTarget(mirrorTexType);

  mirrorTexture.AllocateHandle;
  if mirrorTexture.IsDataNeedUpdate then
  begin
    rci.GLStates.TextureBinding[0, mirrorTexType] := mirrorTexture.Handle;
    GL.TexParameteri(glTarget, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    GL.TexParameteri(glTarget, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    GL.TexParameteri(glTarget, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    GL.TexParameteri(glTarget, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    GL.CopyTexImage2d(glTarget, 0, GL_RGBA8, 0, 0, w, h, 0);
    mirrorTexture.NotifyDataUpdated;
  end
  else
  begin
    rci.GLStates.TextureBinding[0, mirrorTexType] := mirrorTexture.Handle;
    GL.CopyTexSubImage2D(glTarget, 0, 0, 0, 0, 0, w, h);
  end;

  GL.Clear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT + GL_STENCIL_BUFFER_BIT);
end;

procedure TFormForest.DOClassicWaterPlaneRender(Sender: TObject; var rci: TGLRenderContextInfo);
const
  cWaveScale = 7;
  cWaveSpeed = 0.02;
  cSinScale = 0.02;
var
  tex0Matrix, tex1Matrix: TGLMatrix;
  tWave: Single;
  pos: TAffineVector;
  tex: TTexPoint;
  X, Y: Integer;
begin
  if enableGLSL and enableTex2DReflection then
    Exit;

  tWave := GLCadencer.CurrentTime * cWaveSpeed;

  rci.GLStates.ActiveTexture := 0;
  rci.GLStates.TextureBinding[0, ttTexture2D] := MLWater.Materials[0].Material.Texture.Handle;
  rci.GLStates.ActiveTextureEnabled[ttTexture2D] := True;

  tex0Matrix := IdentityHmgMatrix;
  tex0Matrix.X.X := 3 * cWaveScale;
  tex0Matrix.Y.Y := 4 * cWaveScale;
  tex0Matrix.w.X := tWave * 1.1;
  tex0Matrix.w.Y := tWave * 1.06;
  rci.GLStates.SetTextureMatrix(tex0Matrix);

  rci.GLStates.ActiveTexture := 1;
  rci.GLStates.TextureBinding[0, ttTexture2D] := MLWater.Materials[0].Material.Texture.Handle;
  rci.GLStates.ActiveTextureEnabled[ttTexture2D] := True;

  tex1Matrix := IdentityHmgMatrix;
  tex1Matrix.X.X := cWaveScale;
  tex1Matrix.Y.Y := cWaveScale;
  tex1Matrix.w.X := tWave * 0.83;
  tex1Matrix.w.Y := tWave * 0.79;
  rci.GLStates.SetTextureMatrix(tex1Matrix);

  if enableTex2DReflection then
  begin
    rci.GLStates.ActiveTexture := 2;
    rci.GLStates.TextureBinding[2, mirrorTexType] := mirrorTexture.Handle;
    rci.GLStates.ActiveTextureEnabled[ttTexture2D] := True;
    rci.GLStates.SetTextureMatrix(GetTextureReflectionMatrix);
  end;

  rci.GLStates.ActiveTexture := 0;

  {
    if enableTex2DReflection then
    begin
    //SetupTextureCombiners('Tex0:=Tex1*Tex0;'#13#10
    GetTextureCombiners('Tex0:=Tex1*Tex0;'#13#10
    + 'Tex1 := Tex0+Col;'#13#10
    + 'Tex2 := Tex1+Tex2-0.5;');
    GL.Color4f(0.0, 0.3, 0.3, 1);
    end
    else
    begin
    //SetupTextureCombiners('Tex0:=Tex1*Tex0;'#13#10
    GetTextureCombiners('Tex0:=Tex1*Tex0;'#13#10
    + 'Tex1:=Tex0+Col;');
    GL.Color4f(0.0, 0.4, 0.7, 1);
    end;
  }
  GL.Color4f(0.0, 0.4, 0.7, 1);

  rci.GLStates.Disable(stCullFace);
  for Y := -10 to 10 - 1 do
  begin
    GL.Begin_(GL_QUAD_STRIP);
    for X := -10 to 10 do
    begin
      SetVector(pos, X * 1500, 0, Y * 1500);
      tex := TexPointMake(X, Y);
      GL.MultiTexCoord2fv(GL_TEXTURE0, @tex);
      GL.MultiTexCoord2fv(GL_TEXTURE1, @tex);
      GL.MultiTexCoord3fv(GL_TEXTURE2, @pos);
      GL.Vertex3fv(@pos);
      SetVector(pos, X * 1500, 0, (Y + 1) * 1500);
      tex := TexPointMake(X, (Y + 1));
      GL.MultiTexCoord3fv(GL_TEXTURE0, @tex);
      GL.MultiTexCoord3fv(GL_TEXTURE1, @tex);
      GL.MultiTexCoord3fv(GL_TEXTURE2, @pos);
      GL.Vertex3fv(@pos);
    end;
    GL.End_;
  end;

  rci.GLStates.ResetTextureMatrix;
end;

procedure TFormForest.DOGLSLWaterPlaneRender(Sender: TObject; var rci: TGLRenderContextInfo);
var
  X, Y: Integer;
begin
  if not(enableGLSL and enableTex2DReflection) then
    Exit;
  if not Assigned(reflectionProgram) then
  begin
    SetCurrentDir(Path + '\shader');
    reflectionProgram := TGLProgramHandle.CreateAndAllocate;
    reflectionProgram.AddShader(TGLVertexShaderHandle,
      string(LoadAnsiStringFromFile('water_vp.glsl')), True);
    reflectionProgram.AddShader(TGLFragmentShaderHandle,
      string(LoadAnsiStringFromFile('water_fp.glsl')), True);
    if not reflectionProgram.LinkProgram then
      raise Exception.Create(reflectionProgram.InfoLog);
    if not reflectionProgram.ValidateProgram then
      raise Exception.Create(reflectionProgram.InfoLog);
  end;

  reflectionProgram.UseProgramObject;
  reflectionProgram.Uniform1f['Time'] := GLCadencer.CurrentTime;
  reflectionProgram.Uniform4f['EyePos'] := Camera.AbsolutePosition;

  rci.GLStates.TextureBinding[0, mirrorTexType] := mirrorTexture.Handle;
  rci.GLStates.SetTextureMatrix(GetTextureReflectionMatrix);
  reflectionProgram.Uniform1i['ReflectionMap'] := 0;

  rci.GLStates.TextureBinding[1, ttTexture2D] := MLWater.Materials[1].Material.Texture.Handle;
  reflectionProgram.Uniform1i['WaveMap'] := 1;

  for Y := -10 to 10 - 1 do
  begin
    GL.Begin_(GL_QUAD_STRIP);
    for X := -10 to 10 do
    begin
      GL.Vertex3f(X * 1500, 0, Y * 1500);
      GL.Vertex3f(X * 1500, 0, (Y + 1) * 1500);
    end;
    GL.End_;
  end;

  reflectionProgram.EndUseProgramObject;

end;

// SetupReflectionMatrix
//
function TFormForest.GetTextureReflectionMatrix: TGLMatrix;
const
  cBaseMat: TGLMatrix = (v: ((X: 0.5; Y: 0; z: 0; w: 0), (X: 0; Y: 0.5; z: 0; w: 0), (X: 0; Y: 0;
    z: 1; w: 0), (X: 0.5; Y: 0.5; z: 0; w: 1)));

var
  w, h: Single;
begin
  if mirrorTexType = ttTexture2D then
  begin
    w := 0.5 * SceneViewer.Width / RoundUpToPowerOf2(SceneViewer.Width);
    h := 0.5 * SceneViewer.Height / RoundUpToPowerOf2(SceneViewer.Height);
  end
  else
  begin
    w := 0.5 * SceneViewer.Width;
    h := 0.5 * SceneViewer.Height;
  end;

  Result := CreateTranslationMatrix(VectorMake(w, h, 0));
  Result := MatrixMultiply(CreateScaleMatrix(VectorMake(w, h, 0)), Result);
  with CurrentGLContext.PipelineTransformation do
    Result := MatrixMultiply(ViewProjectionMatrix^, Result);
  // Camera.ApplyPerspective(SceneViewer.Buffer.ViewPort, SceneViewer.Width, SceneViewer.Height, 96);
  // Camera.Apply;
  Result := MatrixMultiply(CreateScaleMatrix(VectorMake(1, -1, 1)), Result);
end;

end.
