unit fChrismasD;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,
  Vcl.Menus,

  GLS.Scene,
  GLS.BaseClasses,
  GLS.VectorFileObjects,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.Texture,
  GLS.Cadencer,
  GLS.SkyDome,
  GLS.ParticleFX,
  Stage.VectorGeometry,
  GLS.LensFlare,
  GLS.BitmapFont,
  GLS.WindowsFont,
  GLS.HUDObjects,
  Stage.VectorTypes,
  GLS.ScreenSaver,
  GLS.ShadowPlane,
  GLS.File3DS,
  GLS.FileOBJ,
  GLS.GeomObjects,
  GLS.Material,
  GLS.Coordinates,
  GLS.Color,
  GLS.SoundManager,
  GLS.Sounds.BASS,
  BASS.Import,
  GLS.FireFX,
  GLS.FileWAV,
  Stage.Utils;

type
  TMain = class(TForm)
    Scene: TGLScene;
    Viewer: TGLSceneViewer;
    Camera: TGLCamera;
    dcFirTree: TGLDummyCube;
    FFFirTree: TGLFreeForm;
    LSRoom: TGLLightSource;
    POFirTree2: TGLProxyObject;
    POFirTree3: TGLProxyObject;
    Cadencer: TGLCadencer;
    dcCameraTarget: TGLDummyCube;
    FFFirePlace: TGLFreeForm;
    MaterialLibrary: TGLMaterialLibrary;
    LSFire: TGLLightSource;
    PFXFire: TGLPolygonPFXManager;
    dcFireSource: TGLDummyCube;
    ParticleFXRenderer: TGLParticleFXRenderer;
    cyLog: TGLCylinder;
    DCLensFlares: TGLDummyCube;
    LensFlare1: TGLLensFlare;
    LensFlare2: TGLLensFlare;
    LensFlare3: TGLLensFlare;
    LensFlare4: TGLLensFlare;
    SMBASS: TGLSMBASS;
    SoundLibrary: TGLSoundLibrary;
    DCDecoWhite: TGLDummyCube;
    dcBalls: TGLDummyCube;
    SPWhiteBall: TGLSphere;
    POWhiteBall1: TGLProxyObject;
    SPGoldBall: TGLSphere;
    POGoldBall1: TGLProxyObject;
    DCDecoGold: TGLDummyCube;
    POGoldBall2: TGLProxyObject;
    LFFireLens: TGLLensFlare;
    LensFlare5: TGLLensFlare;
    POWhiteBall2: TGLProxyObject;
    POGoldBall3: TGLProxyObject;
    POWhiteBall3: TGLProxyObject;
    LensFlare6: TGLLensFlare;
    PFXTree: TGLPolygonPFXManager;
    WindowsBitmapFont: TGLWindowsBitmapFont;
    Cube1: TGLCube;
    dcGifts: TGLDummyCube;
    Cube2: TGLCube;
    ShadowPlane: TGLShadowPlane;
    DCTree: TGLDummyCube;
    Cube3: TGLCube;
    Cube4: TGLCube;
    dcFire: TGLDummyCube;
    ScreenSaver: TGLScreenSaver;
    Timer: TTimer;
    HUDSprite: TGLHUDSprite;
    ftCountDown: TGLFlatText;
    ftYear: TGLFlatText;
    ftCongratulations: TGLFlatText;
    FireFXManager: TGLFireFXManager;
    procedure FormCreate(Sender: TObject);
    procedure CadencerProgress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormResize(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure TimerTimer(Sender: TObject);
    procedure ScreenSaverCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ScreenSaverExecute(Sender: TObject);
    procedure ScreenSaverPreview(Sender: TObject; previewHwnd: HWND);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerDblClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    AssetPath: TFileName;
    mx, my: Integer;
    FireLight: Single;
    inPreview, inSaver: Boolean;
    bStream: Cardinal;
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

procedure TMain.FormCreate(Sender: TObject);
begin
  AssetPath := GetCurrentAssetPath();
  Randomize;
  // Load static models
  SetCurrentDir(AssetPath + '\model');
  ffFirTree.LoadFromFile('firtree.3ds');
  ffFirePlace.LoadFromFile('fireplace.3ds');

  FireLight := 0.5;
  FTYear.Text := '';

  // Set current dir for audio files
  SetCurrentDir(AssetPath + '\audio');
end;

procedure TMain.FormResize(Sender: TObject);
begin
  Camera.SceneScale := Width / 640;
  if Visible then
    HUDSprite.Position.X := Self.Width - 200;
  if (Width >= Screen.Width) then
    ViewerDblClick(Self);
end;

procedure TMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Key := #0;
  Application.Terminate;
end;

procedure TMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TMain.ViewerDblClick(Sender: TObject);
begin
  if (not inPreview) and (not inSaver) and (not Application.Terminated) and
    (BorderStyle <> bsNone) then
  begin
    BorderStyle := bsNone;
    FormStyle := fsStayOnTop;
    Align := alClient;
  end;
end;

procedure TMain.ViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TMain.ViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
    Camera.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

procedure TMain.CadencerProgress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  FireLight := ClampValue(FireLight + Random * 0.4 - 0.2, 0, 1);
  LSFire.Diffuse.Color := VectorLerp(clrYellow, VectorMake(0.5, 0, 0, 1),
    FireLight);
  LSFire.Position.Y := FireLight * 0.1;

  if inPreview then
    HUDSprite.Visible := False;

  if HUDSprite.Visible then
  begin
    HUDSprite.Material.FrontProperties.Diffuse.Alpha :=
      HUDSprite.Material.FrontProperties.Diffuse.Alpha - deltaTime * 0.03;
    if HUDSprite.Material.FrontProperties.Diffuse.Alpha < 0.01 then
      HUDSprite.Visible := False;
  end;
  DCFirTree.Turn(deltaTime);
  Viewer.Invalidate();
end;

procedure TMain.TimerTimer(Sender: TObject);
var
  i: Integer;
  t: TDateTime;
  buf: String;
  Y, m, d: Word;
  TheChristmas,
  isArrived: Boolean;

begin
  TheChristmas := false;   // Merry Christmas or Happy New Year!
  Caption := Format('%.1f FPS', [Viewer.FramesPerSecond]);
  Viewer.ResetPerformanceMonitor;
  if SMBASS.Active and (bStream = 0) then
  begin
    bStream := BASS_StreamCreateFile(False, PAnsiChar('Jingle_Bells_64.mp3'), 0,
      0, BASS_STREAM_AUTOFREE);
    BASS_ChannelPlay(bStream, True);
  end;
  DecodeDate(Now(), Y, m, d);
  if TheChristmas then
  begin
    t := EncodeDate(Y, 12, 25) - Now();
    ftCongratulations.Text := 'Merry Christmas!';
  end
  else
  begin
    t := EncodeDate(Y + 1, 01, 01) - Now();
    ftCongratulations.Text := 'Happy New Year!';
    ftYear.Text := IntToStr(Y + 1);
  end;
  if (t < 1) and (t > -1) then
    dcGifts.Visible := True;
  if t >= 2 then
  begin
    buf := IntToStr(Trunc(t)) + ' days, ';
    i := Round(Frac(t) * 24);
    if i > 1 then
      buf := buf + IntToStr(i) + ' hours...'
    else
      buf := buf + IntToStr(i) + ' hour...';
    ftCountDown.Text := buf;
  end
  else
  begin
    t := t * 24;
    if t > 1 then
    begin
      buf := IntToStr(Trunc(t)) + ' hours, ';
      i := Round(Frac(t) * 60);
      if i > 1 then
        buf := buf + IntToStr(i) + ' minutes...'
      else
        buf := buf + IntToStr(i) + ' minute...';
      ftCountDown.Text := buf;
    end
    else
    begin
      t := t * 60;
      ftCountDown.Text := IntToStr(Trunc(t)) + ' minutes, ' +
        IntToStr(Round(Frac(t) * 60)) + ' seconds...';
    end;
  end;
end;

procedure TMain.ScreenSaverCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Application.Terminate;
  CanClose := False;
end;

procedure TMain.ScreenSaverExecute(Sender: TObject);
begin
  inSaver := True;
end;

procedure TMain.ScreenSaverPreview(Sender: TObject; previewHwnd: HWND);
begin
  inPreview := True;
end;

end.
