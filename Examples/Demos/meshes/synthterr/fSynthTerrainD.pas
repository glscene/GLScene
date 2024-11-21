unit fSynthTerrainD;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Imaging.Jpeg,

  GLS.Scene,
  GLS.Objects,
  GLS.TerrainRenderer,
  GLS.HeightData,
  GLS.Cadencer,
  Stage.VectorTypes,
  GLS.Texture,
  GLS.SceneViewer,
  GLS.Material,

  Stage.Keyboard,
  Stage.VectorGeometry,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.XCollection,

  GLS.ShadowHDS;

type
  TFormSynthTerrain = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    TerrainRenderer1: TGLTerrainRenderer;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCustomHDS: TGLCustomHDS;
    GLShadowHDS: TGLShadowHDS;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure GLCustomHDSStartPreparingData(HeightData: TGLHeightData);
  public
    mx, my: Integer;
    fullScreen: Boolean;
    FCamHeight: Single;
  end;

var
  FormSynthTerrain: TFormSynthTerrain;

implementation

{$R *.DFM}

procedure TFormSynthTerrain.FormCreate(Sender: TObject);
var
  i: Integer;
  bmp: TBitmap;
begin
  // 8 MB height data cache
  // Note this is the data size in terms of elevation samples, it does not
  // take into account all the data required/allocated by the renderer
  GLCustomHDS.MaxPoolSize := 8 * 1024 * 1024;
  // Move camera starting point to an interesting hand-picked location
  DummyCube1.Position.X := 50;
  DummyCube1.Position.Z := 150;
  // Initial camera height offset (controled with pageUp/pageDown)
  FCamHeight := 20;
  // We build several basic 1D textures which are just color ramps
  // all use automatic texture mapping corodinates, in ObjectLinear method
  // (ie. texture coordinates for a vertex depend on that vertex coordinates)
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24bit;
  bmp.Width := 256;
  bmp.Height := 1;
  // Black-White ramp, autotexture maps to Z coordinate
  // This one changes with altitude, this is a quick way to obtain
  // altitude-dependant coloring
  for i := 0 to 255 do
    bmp.Canvas.Pixels[i, 0] := RGB(i, i, i);
  with GLMaterialLibrary1.AddTextureMaterial('BW', bmp) do
  begin
    Material.Texture.MappingMode := tmmObjectLinear;
    Material.Texture.MappingSCoordinates.AsVector := VectorMake(0, 0, 0.0001, 0);
  end;
  // Red, Blue map linearly to X and Y axis respectively
  for i := 0 to 255 do
    bmp.Canvas.Pixels[i, 0] := RGB(i, 0, 0);
  with GLMaterialLibrary1.AddTextureMaterial('Red', bmp) do
  begin
    Material.Texture.MappingMode := tmmObjectLinear;
    Material.Texture.MappingSCoordinates.AsVector := VectorMake(0.1, 0, 0, 0);
  end;
  for i := 0 to 255 do
    bmp.Canvas.Pixels[i, 0] := RGB(0, 0, i);
  with GLMaterialLibrary1.AddTextureMaterial('Blue', bmp) do
  begin
    Material.Texture.MappingMode := tmmObjectLinear;
    Material.Texture.MappingSCoordinates.AsVector := VectorMake(0, 0.1, 0, 0);
  end;
  bmp.Free;
  TerrainRenderer1.MaterialLibrary := GLMaterialLibrary1;
end;

//
// The beef : this event does all the interesting elevation data stuff
//

procedure TFormSynthTerrain.GLCustomHDSStartPreparingData(HeightData: TGLHeightData);
var
  Y, X: Integer;
  rasterLine: PByteArray;
  oldType: TGLHeightDataType;
  b: Byte;
  d, dy: Single;
begin
  HeightData.DataState := hdsPreparing;
  // retrieve data
  with HeightData do
  begin
    oldType := DataType;
    Allocate(hdtByte);
    // Cheap texture changed (32 is our tileSize = 2^5)
    // This basicly picks a texture for each tile depending on the tile's position
    case (((XLeft xor YTop) shr 5) and 3) of
      0, 3: HeightData.MaterialName := 'BW';
         1: HeightData.MaterialName := 'Blue';
         2: HeightData.MaterialName := 'Red';
    end;
    // 'Cheap' elevation data : this is just a formula z=f(x, y)
    for Y := YTop to YTop + Size - 1 do
    begin
      rasterLine := ByteRaster[Y - YTop];
      dy := Sqr(Y);
      for X := XLeft to XLeft + Size - 1 do
      begin
        d := Sqrt(Sqr(X) + dy);
        b := Round(128 + 128 * Sin(d * 0.2) / (d * 0.1 + 1));
        rasterLine[X - XLeft] := b;
      end;
    end;
    if oldType <> hdtByte then
      DataType := oldType;
  end;
  inherited;
end;

// Movement, mouse handling etc.

procedure TFormSynthTerrain.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormSynthTerrain.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

procedure TFormSynthTerrain.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS - %d', [GLSceneViewer1.FramesPerSecond,
    TerrainRenderer1.LastTriangleCount]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormSynthTerrain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '+':
      if GLCamera1.DepthOfView < 4000 then
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
        if CLODPrecision > 5 then
          CLODPrecision := Round(CLODPrecision * 0.8);
    '/':
      with TerrainRenderer1 do
        if CLODPrecision < 500 then
          CLODPrecision := Round(CLODPrecision * 1.2);
    '8':
      with TerrainRenderer1 do
        if QualityDistance > 40 then
          QualityDistance := Round(QualityDistance * 0.8);
    '9':
      with TerrainRenderer1 do
        if QualityDistance < 1000 then
          QualityDistance := Round(QualityDistance * 1.2);
  end;
  Key := #0;
end;

procedure TFormSynthTerrain.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  speed: Single;
begin
  // handle keypresses
  if IsKeyDown(VK_SHIFT) then
    speed := 5 * deltaTime
  else
    speed := deltaTime;
  with GLCamera1.Position do
  begin
    if IsKeyDown(VK_RIGHT) then
      DummyCube1.Translate(Z * speed, 0, -X * speed);
    if IsKeyDown(VK_LEFT) then
      DummyCube1.Translate(-Z * speed, 0, X * speed);
    if IsKeyDown(VK_UP) then
      DummyCube1.Translate(-X * speed, 0, -Z * speed);
    if IsKeyDown(VK_DOWN) then
      DummyCube1.Translate(X * speed, 0, Z * speed);
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

end.
