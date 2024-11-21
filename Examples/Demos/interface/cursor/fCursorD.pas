unit fCursorD;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtDlgs,
  Vcl.Menus,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,

  GLS.Scene,
  GLS.Objects,
  Stage.VectorTypes,
  GLS.PersistentClasses,
  GLS.Particles,
  GLS.Texture,
  GLS.Cadencer,
  GLS.HUDObjects,
  GLS.SceneViewer,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  Stage.Utils;

type
  TFormCursor = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLParticles1: TGLParticles;
    HSBitmap: TGLHUDSprite;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    MIFile: TMenuItem;
    MILoadImage: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    HSCursor: TGLHUDSprite;
    GLCadencer1: TGLCadencer;
    Bevel1: TBevel;
    HSParticle: TGLHUDSprite;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Timer1: TTimer;
    O1: TMenuItem;
    MITrail: TMenuItem;
    N1: TMenuItem;
    MIExit: TMenuItem;
    miFPS: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLParticles1ActivateParticle(Sender: TObject;
      particle: TGLBaseSceneObject);
    procedure HSParticleProgress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure MITrailClick(Sender: TObject);
    procedure MILoadImageClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure GLSceneViewer1AfterRender(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
  private
     
    handleMouseMoves: Boolean;
  public
     
  end;

var
  FormCursor: TFormCursor;

implementation

{$R *.DFM}

procedure TFormCursor.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\button');
  // hide the Windows cursor for the GLSceneViewer
  GLSceneViewer1.Cursor := crNone;
  // and load an ugly cursor (size adjusted in design props)
  with GLMaterialLibrary1.Materials[0] do
    Material.Texture.Image.LoadFromFile('cursor.bmp');
end;

procedure TFormCursor.MILoadImageClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    // use the hourglass cursor, it may take some time to load the bitmap,
    // rescale it and generate mipmaps before sending it to OpenGL
    Screen.Cursor := crHourGlass;
    with (HSBitmap.Material.Texture.Image as TGLPictureImage).Picture do
    begin
      LoadFromFile(OpenPictureDialog1.FileName);
      // adjust hud sprite size to match that of the picture
      HSBitmap.SetSize(Width, Height);
      // adjust position, hudsprites are centered on their x, y coords
      HSBitmap.Position.X := Width / 2;
      HSBitmap.Position.Y := Height / 2;
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormCursor.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  color: TColor;
begin
  // Prevents event floods on slow hardware
  if not handleMouseMoves then
    Exit;
  handleMouseMoves := False;
  // Mouse moved, adjust the position of our cursor
  HSCursor.Position.X := X;
  HSCursor.Position.Y := Y;
  // Update the status bar with some misc. info
  color := GLSceneViewer1.Buffer.GetPixelColor(X, Y);
  StatusBar1.SimpleText := Format('X:%4d Y:%4d, R:%3d G:%3d B:%3d',
    [X, Y, GetRValue(color), GetGValue(color), GetBValue(color)]);
  // Add a trail particle
  if MITrail.Checked then
    GLParticles1.CreateParticle();
  // Update things now
  GLCadencer1.Progress();
end;

procedure TFormCursor.GLSceneViewer1AfterRender(Sender: TObject);
begin
  handleMouseMoves := True;
end;

procedure TFormCursor.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate();
end;

procedure TFormCursor.HSParticleProgress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  with (Sender as TGLHUDSprite) do
  begin
    // decrease life time / alpha
    TagFloat := TagFloat - deltaTime;
    // update alpha channel, but if no more life is left, then suicide
    if TagFloat < 0 then
      GLParticles1.KillParticle(TGLProxyObject(Sender))
    else
      AlphaChannel := TagFloat * 0.2;
  end;
end;

procedure TFormCursor.GLParticles1ActivateParticle(Sender: TObject;
  particle: TGLBaseSceneObject);
begin
  with (particle as TGLHUDSprite) do
  begin
    // we are cadencing real-time, so these are 5 seconds
    TagFloat := 5;
    // new particle stands where cursor is
    Position.AsVector := HSCursor.Position.AsVector;
  end;
end;

procedure TFormCursor.Timer1Timer(Sender: TObject);
begin
  // update FPS and sprite count
  miFPS.Caption := Format('%.1f FPS - %d Cursor Sprites',
    [GLSceneViewer1.FramesPerSecond, GLParticles1.Count]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormCursor.MITrailClick(Sender: TObject);
begin
  // turn trails on/off
  MITrail.Checked := not MITrail.Checked;
end;

procedure TFormCursor.MIExitClick(Sender: TObject);
begin
  Close;
end;

end.
