unit fShadedTerrainD;


interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Imaging.Jpeg,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,

  GLS.Scene,
  GLS.BaseClasses,
  Stage.VectorTypes,
  Stage.VectorGeometry,
  Stage.Keyboard,
  GLS.XCollection,
  GLS.Coordinates,

  GLS.Objects,
  GLS.TerrainRenderer,
  GLS.ROAMPatch,
  GLS.HeightData,
  GLS.Cadencer,
  GLS.Texture,
  GLS.SkyDome,
  GLS.SceneViewer,
  GLS.LensFlare,
  GLS.BumpMapHDS,
  GLSL.TextureShaders,
  GLS.Material,

  GLS.State,
  Stage.Utils;

type
  TFormShadedTerrain = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    TerrainRenderer1: TGLTerrainRenderer;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    SkyDome1: TGLSkyDome;
    SPSun: TGLSprite;
    GLLensFlare: TGLLensFlare;
    GLDummyCube1: TGLDummyCube;
    GLTexCombineShader1: TGLTexCombineShader;
    GLBumpmapHDS1: TGLBumpmapHDS;
    Panel1: TPanel;
    Label1: TLabel;
    TBSubSampling: TTrackBar;
    LASubFactor: TLabel;
    Label2: TLabel;
    TBIntensity: TTrackBar;
    LABumpIntensity: TLabel;
    TBContourInterval: TTrackBar;
    TBScaleZ: TTrackBar;
    LaScaleZ: TLabel;
    LabelZ: TLabel;
    LabelContInterval: TLabel;
    CBContourIntervals: TCheckBox;
    
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
    procedure GLBumpmapHDS1NewTilePrepared(Sender: TGLBumpmapHDS;
      heightData: TGLHeightData; normalMapMaterial: TGLLibMaterial);
    procedure TBSubSamplingChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TBIntensityChange(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TBScaleZChange(Sender: TObject);
    procedure TBContourIntervalChange(Sender: TObject);
    procedure CBContourIntervalsClick(Sender: TObject);
  public
    mx, my: Integer;
    fullScreen: Boolean;
    FCamHeight: Single;
  end;

var
  FormShadedTerrain: TFormShadedTerrain;

implementation

{$R *.DFM}

procedure TFormShadedTerrain.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\texture');

  // Load Terrain in 8 MB height data cache
  // Note this is the data size in terms of elevation samples, it does not
  // take into account all the data required/allocated by the renderer
  GLBitmapHDS1.MaxPoolSize := 8 * 1024 * 1024;

  // specify a map for height field data
  GLBitmapHDS1.Picture.LoadFromFile('terrain.bmp');

  GLMaterialLibrary1.LibMaterialByName('details').Material.Texture.Image.LoadFromFile('detailmap.jpg');

  // GLMaterialLibrary1.LibMaterialByName('texture').Material.Texture.Image.LoadFromFile('snow512.jpg');


  SPSun.Material.Texture.Image.LoadFromFile('flare1.bmp');

  // Could've been done at design time, but then it hurts the eyes ;)
  GLSceneViewer1.Buffer.BackgroundColor := clWhite;
  // Initial camera height offset (controled with pageUp/pageDown)
  FCamHeight := 20;

  // apply texture map scale (our heightmap size is 256)
  TerrainRenderer1.TilesPerTexture := 4; // 256/TerrainRenderer1.TileSize;
  // TerrainRenderer1.MaterialLibrary := GLMaterialLibrary1;
  TerrainRenderer1.ContourWidth := 2;

  TBIntensityChange(Self);
  TBScaleZChange(Self);
end;

procedure TFormShadedTerrain.FormShow(Sender: TObject);
begin
  TBSubSamplingChange(Self);
  TBContourIntervalChange(Self);
end;

procedure TFormShadedTerrain.GLBumpmapHDS1NewTilePrepared(Sender: TGLBumpmapHDS;
  heightData: TGLHeightData; normalMapMaterial: TGLLibMaterial);
var
  Vec: TGLVector;
begin
  heightData.MaterialName := normalMapMaterial.Name;
  normalMapMaterial.Texture2Name := 'contrast';//'details', 'texture' or 'contrast';
  normalMapMaterial.Shader := GLTexCombineShader1;
  normalMapMaterial.Material.MaterialOptions := [moNoLighting];
  Vec := VectorNormalize(SPSun.AbsolutePosition);
  ScaleVector(Vec, 0.5);
  Vec.Y := -Vec.Y;
  Vec.Z := -Vec.Z;
  AddVector(Vec, 0.5);
  normalMapMaterial.Material.FrontProperties.Diffuse.Color := Vec;
end;

procedure TFormShadedTerrain.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  speed: Single;
begin
  // handle keypresses
  if IsKeyDown(VK_SHIFT) then
    speed := 10 * deltaTime
  else
    speed := deltaTime;
  with GLCamera1.Position do
  begin
    if IsKeyDown(VK_UP) then
      DummyCube1.Translate(-X * speed, 0, -Z * speed);
    if IsKeyDown(VK_DOWN) then
      DummyCube1.Translate(X * speed, 0, Z * speed);
    if IsKeyDown(VK_LEFT) then
      DummyCube1.Translate(-Z * speed, 0, X * speed);
    if IsKeyDown(VK_RIGHT) then
      DummyCube1.Translate(Z * speed, 0, -X * speed);
    if IsKeyDown(VK_PRIOR) then
      FCamHeight := FCamHeight + 10 * speed;
    if IsKeyDown(VK_NEXT) then
      FCamHeight := FCamHeight - 10 * speed;
    if IsKeyDown(VK_ESCAPE) then
      Close;
  end;
  // don't drop through terrain!
  with DummyCube1.Position do
    Y := TerrainRenderer1.InterpolatedHeight(AsVector) + FCamHeight;
end;

// Standard mouse rotation & FPS code below

procedure TFormShadedTerrain.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GLSceneViewer1.SetFocus;
  mx := X;
  my := Y;
end;

procedure TFormShadedTerrain.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget((my - Y) * 0.5, (mx - X) * 0.5);
    mx := X;
    my := Y;
  end;
end;


procedure TFormShadedTerrain.Timer1Timer(Sender: TObject);
begin
  Caption := 'Shaded Terrain ' + GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormShadedTerrain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    'w', 'W':
      with GLMaterialLibrary1.Materials[0].Material do
      begin
        if PolygonMode = pmLines then
          PolygonMode := pmFill
        else
          PolygonMode := pmLines;
      end;
    '+':
      if GLCamera1.DepthOfView < 2000 then
      begin
        GLCamera1.DepthOfView := GLCamera1.DepthOfView * 1.2;
        with GLSceneViewer1.Buffer.FogEnvironment do
        begin
          FogEnd := FogEnd * 1.2;
          FogStart := FogStart * 1.2;
        end;
      end;
    '-':
      if GLCamera1.DepthOfView > 300 then
      begin
        GLCamera1.DepthOfView := GLCamera1.DepthOfView / 1.2;
        with GLSceneViewer1.Buffer.FogEnvironment do
        begin
          FogEnd := FogEnd / 1.2;
          FogStart := FogStart / 1.2;
        end;
      end;
    '*':
      with TerrainRenderer1 do
        if CLODPrecision > 10 then
          CLODPrecision := Round(CLODPrecision * 0.8);
    '/':
      with TerrainRenderer1 do
        if CLODPrecision < 1000 then
          CLODPrecision := Round(CLODPrecision * 1.2);
    'l':
      with GLLensFlare do
        Visible := (not Visible) and SPSun.Visible;
  end;
  Key := #0;
end;

procedure TFormShadedTerrain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if GLSceneViewer1.Focused then
    GLCamera1.AdjustDistanceToTarget(Power(1.02, WheelDelta / 120));
end;

procedure TFormShadedTerrain.GLSceneViewer1BeforeRender(Sender: TObject);
var
  TexUnits: Cardinal;
begin
  if GLTexCombineShader1.Enabled then
  begin
    GLSceneViewer1.Buffer.RenderingContext.Activate;
    TexUnits := GLSceneViewer1.Buffer.LimitOf[limNbTextureUnits];
    GLSceneViewer1.Buffer.RenderingContext.Deactivate;
    if TexUnits < 4 then
    begin
      Application.MessageBox
        ('Not enough texture units! The shader will be disabled.',
        'Error', MB_OK);
      GLTexCombineShader1.Enabled := False;
    end;
  end;
  GLLensFlare.PreRender(Sender as TGLSceneBuffer);
end;

procedure TFormShadedTerrain.TBSubSamplingChange(Sender: TObject);
begin
  GLBumpmapHDS1.SubSampling := (1 shl TBSubSampling.Position);
  LASubFactor.Caption := Format('(%d) -> %dx%1:d', [GLBumpmapHDS1.SubSampling,
    TerrainRenderer1.TileSize div GLBumpmapHDS1.SubSampling]);
  // don't leave the focus to the trackbar, otherwise it'll keep some keystrokes
  // for itself, like the arrow keys
  SetFocus;
end;

procedure TFormShadedTerrain.TBIntensityChange(Sender: TObject);
var
  i: Integer;
  bmp: TBitmap;
begin
  with GLMaterialLibrary1.LibMaterialByName('contrast').Material do
  begin
    bmp := TBitmap.Create;
    try
      bmp.PixelFormat := pf24bit;
      bmp.Width := 1;
      bmp.Height := 1;
      i := 255;
      bmp.Canvas.Pixels[0, 0] := RGB(i, i, i);
      Texture.Image.Assign(bmp);
    finally
      bmp.Free;
    end;
    i := (TBIntensity.Position * 255) div 100;
    Texture.EnvColor.AsWinColor := RGB(i, i, i);
  end;
  LABumpIntensity.Caption := IntToStr(TBIntensity.Position) + ' %';
end;

procedure TFormShadedTerrain.TBContourIntervalChange(Sender: TObject);
begin
  TerrainRenderer1.ContourInterval := TBContourInterval.Position;
  LabelContInterval.Caption := IntToStr(TerrainRenderer1.ContourInterval);
end;

procedure TFormShadedTerrain.TBScaleZChange(Sender: TObject);
begin
  TerrainRenderer1.Scale.Z := TBScaleZ.Position / 20;
  LabelZ.Caption := FloatToStrF(TerrainRenderer1.Scale.Z, ffFixed, 5, 2);
end;

procedure TFormShadedTerrain.CBContourIntervalsClick(Sender: TObject);
begin
  if CBContourIntervals.Checked = True then
    TerrainRenderer1.ContourInterval := TBContourInterval.Position
  else
    TerrainRenderer1.ContourInterval := 0;
  TBContourinterval.SetFocus;
end;

end.

