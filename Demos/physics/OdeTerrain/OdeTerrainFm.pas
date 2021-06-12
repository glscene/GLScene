
unit OdeTerrainFm;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.UITypes,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Imaging.Jpeg,

  GLS.VectorTypes,
  GLS.Scene,
  GLS.TerrainRenderer,
  GLS.Objects,
  GLS.HeightData,
  GLS.Keyboard,
  GLS.Cadencer,
  GLS.Texture,
  GLS.HUDObjects,
  GLS.BitmapFont,
  GLS.SkyDome,
  GLS.SceneViewer,
  GLS.VectorGeometry,
  GLS.LensFlare,
  Physics.ODEManager,
  GLS.Navigator,
  GLS.GeomObjects,
  GLS.Color,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.State,
  GLS.Utils;

type
  TFormOdeTerrain = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    TerrainRenderer1: TGLTerrainRenderer;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    BitmapFont1: TGLBitmapFont;
    HUDText1: TGLHUDText;
    SkyDome1: TGLSkyDome;
    SPMoon: TGLSprite;
    SPSun: TGLSprite;
    GLLensFlare: TGLLensFlare;
    GLDummyCube1: TGLDummyCube;
    GLODEManager1: TGLODEManager;
    GLNavigator1: TGLNavigator;
    ODEDrop: TGLDummyCube;
    GLUserInterface1: TGLUserInterface;
    ODEObjects: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    ODERenderPoint: TGLRenderPoint;
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
  public

    procedure DropODEObject(anElementClass : TGLODEElementClass);
  end;

var
  FormOdeTerrain: TFormOdeTerrain;

implementation

{$R *.DFM}

procedure TFormOdeTerrain.FormCreate(Sender: TObject);
begin
   SetGLSceneMediaDir();
   // Set up the visuals
   GLBitmapHDS1.MaxPoolSize:=8*1024*1024;
   GLBitmapHDS1.Picture.LoadFromFile('terrain.bmp');
   GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('snow512.jpg');
   GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile('detailmap.jpg');
   SPMoon.Material.Texture.Image.LoadFromFile('moon.bmp');
   SPSun.Material.Texture.Image.LoadFromFile('flare1.bmp');
   TerrainRenderer1.TilesPerTexture:=256/TerrainRenderer1.TileSize;
   BitmapFont1.Glyphs.LoadFromFile('darkgold_font.bmp');
   GLSceneViewer1.Buffer.BackgroundColor:=clWhite;
   GLNavigator1.TurnHorizontal(90);

   GLUserInterface1.MouseLookActivate;
end;

procedure TFormOdeTerrain.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   speed, interpolated_height : Single;
begin
   // handle keypresses
   if IsKeyDown(VK_SHIFT) then
      speed:=50*deltaTime
   else speed:=10*deltaTime;
   with GLCamera1.Position do begin
      if IsKeyDown(VK_UP) then
        GLNavigator1.MoveForward(speed);
      if IsKeyDown(VK_DOWN) then
        GLNavigator1.MoveForward(-speed);
      if IsKeyDown(VK_LEFT) then
        GLNavigator1.StrafeHorizontal(-speed);
      if IsKeyDown(VK_RIGHT) then
        GLNavigator1.StrafeHorizontal(speed);
      if IsKeyDown(VK_PRIOR) then
         GLNavigator1.StrafeVertical(speed);
      if IsKeyDown(VK_NEXT) then
         GLNavigator1.StrafeVertical(-speed);
      if IsKeyDown(VK_ESCAPE) then Close;

      interpolated_height:=TerrainRenderer1.InterpolatedHeight(AsVector);
      if Z<interpolated_height+5 then
         Z:=interpolated_height+5;
   end;

   GLODEManager1.Step(deltaTime);
   GLUserInterface1.MouseUpdate;
   GLUserInterface1.MouseLook;
end;

procedure TFormOdeTerrain.Timer1Timer(Sender: TObject);
begin
   HUDText1.Text:=Format('%.1f FPS - %d',
                         [GLSceneViewer1.FramesPerSecond, TerrainRenderer1.LastTriangleCount]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormOdeTerrain.FormKeyPress(Sender: TObject; var Key: Char);
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
        if CLODPrecision > 20 then
          CLODPrecision := Round(CLODPrecision * 0.8);
    '/':
      with TerrainRenderer1 do
        if CLODPrecision < 1000 then
          CLODPrecision := Round(CLODPrecision * 1.2);
    '8':
      with TerrainRenderer1 do
        if QualityDistance > 40 then
          QualityDistance := Round(QualityDistance * 0.8);
    '9':
      with TerrainRenderer1 do
        if QualityDistance < 1000 then
          QualityDistance := Round(QualityDistance * 1.2);
    'n', 'N':
      with SkyDome1 do
        if Stars.Count = 0 then
        begin
          // turn on 'night' mode
          Bands[1].StopColor.AsWinColor := RGB(0, 0, 16);
          Bands[1].StartColor.AsWinColor := RGB(0, 0, 8);
          Bands[0].StopColor.AsWinColor := RGB(0, 0, 8);
          Bands[0].StartColor.AsWinColor := RGB(0, 0, 0);
          with Stars do
          begin
            AddRandomStars(700, clWhite, True); // many white stars
            AddRandomStars(100, RGB(255, 200, 200), True); // some redish ones
            AddRandomStars(100, RGB(200, 200, 255), True); // some blueish ones
            AddRandomStars(100, RGB(255, 255, 200), True);
            // some yellowish ones
          end;
          GLSceneViewer1.Buffer.BackgroundColor := clBlack;
          with GLSceneViewer1.Buffer.FogEnvironment do
          begin
            FogColor.AsWinColor := clBlack;
            FogStart := -FogStart; // Fog is used to make things darker
          end;
          SPMoon.Visible := True;
          SPSun.Visible := False;
          GLLensFlare.Visible := False;
        end;
    'd', 'D':
      with SkyDome1 do
        if Stars.Count > 0 then
        begin
          // turn on 'day' mode
          Bands[1].StopColor.Color := clrNavy;
          Bands[1].StartColor.Color := clrBlue;
          Bands[0].StopColor.Color := clrBlue;
          Bands[0].StartColor.Color := clrWhite;
          Stars.Clear;
          GLSceneViewer1.Buffer.BackgroundColor := clWhite;
          with GLSceneViewer1.Buffer.FogEnvironment do
          begin
            FogColor.AsWinColor := clWhite;
            FogStart := -FogStart;
          end;
          GLSceneViewer1.Buffer.FogEnvironment.FogStart := 0;
          SPMoon.Visible := False;
          SPSun.Visible := True;
        end;
    't':
      with SkyDome1 do
      begin
        if sdoTwinkle in Options then
          Options := Options - [sdoTwinkle]
        else
          Options := Options + [sdoTwinkle];
      end;
    'l':
      with GLLensFlare do
        Visible := (not Visible) and SPSun.Visible;
    '1':
      DropODEObject(TGLODEElementSphere);
    '2':
      DropODEObject(TGLODEElementBox);
    '3':
      DropODEObject(TGLODEElementCapsule);
    '4':
      DropODEObject(TGLODEElementCylinder);
    // '5' : DropODEObject(TGLODEElementCone); CONE is currently unsupported
  end;
  Key := #0;
end;

procedure TFormOdeTerrain.GLSceneViewer1BeforeRender(Sender: TObject);
begin
   GLLensFlare.PreRender(Sender as TGLSceneBuffer);
end;

procedure TFormOdeTerrain.DropODEObject(anElementClass : TGLODEElementClass);
var
  dummy : TGLBaseSceneObject;
  dyn : TGLODEDynamic;
begin
  dummy:=ODEObjects.AddNewChild(TGLDummyCube);
  dummy.Position.AsVector:=ODEDrop.AbsolutePosition;
  dyn:=TGLODEDynamic.Create(dummy.Behaviours);
  dyn.AddNewElement(anElementClass);  // mrqzzz : AddNewElement must come *before* setting manager!
  dyn.Manager:=GLODEManager1;
end;

end.
