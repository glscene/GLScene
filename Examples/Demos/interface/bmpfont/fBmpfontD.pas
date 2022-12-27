unit fBmpfontD;

interface

uses
  Winapi.Opengl,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  
  GLS.Scene,
  GLS.VectorTypes,
  GLS.HUDObjects,
  GLS.Objects,
  GLS.Cadencer,
  GLS.BitmapFont,
  GLS.GeomObjects,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Utils,
  GLS.SceneViewer;

type
  TFormBmpFont = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    BitmapFont1: TGLBitmapFont;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    HUDText1: TGLHUDText;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    HUDText2: TGLHUDText;
    HUDText3: TGLHUDText;
    Teapot1: TGLTeapot;
    HUDTextFPS: TGLHUDText;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1Click(Sender: TObject);
  private
     
  public
     
  end;

var
  FormBmpFont: TFormBmpFont;

implementation

{$R *.DFM}

procedure TFormBmpFont.FormCreate(Sender: TObject);
begin
  // Load the font bitmap from asset dir
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\texture');
  BitmapFont1.Glyphs.LoadFromFile('darkgold_font.bmp');
  // sorry, couldn't resist...
  {$IFDEF WIN32}
  HUDText1.Text := 'Hello WIN 32!'#13#10#13#10 + 'This is me, '#13#10 +
    'the HUD Text.'#13#10#13#10 + 'Bitmap Fonts!';
  {$ENDIF}
 {$IFDEF WIN64}
  HUDText1.Text := 'Hello WIN 64!'#13#10#13#10 + 'This is me, '#13#10 +
    'the HUD Text.'#13#10#13#10 + 'Bitmap Fonts!';
  {$ENDIF}
end;

procedure TFormBmpFont.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  // make things move a little
  HUDText2.Rotation := HUDText2.Rotation + 15 * deltaTime;
  HUDText3.Scale.X := 0.5 * sin(newTime) + 1;
  HUDText3.Scale.Y := 0.5 * cos(newTime) + 1;
  GLSceneViewer1.Invalidate;
end;

procedure TFormBmpFont.Timer1Timer(Sender: TObject);
begin
  FormatSettings.DecimalSeparator := ',';
  HUDTextFPS.Text := FloatToStr(-2.01);
  HUDTextFPS.Text := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormBmpFont.GLSceneViewer1Click(Sender: TObject);
begin
  Teapot1.Visible := not Teapot1.Visible;
end;

end.
