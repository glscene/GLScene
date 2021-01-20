unit WinFontFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Menus,

  
  GLS.Scene,
  GLS.VectorTypes,
  GLS.HUDObjects,
  GLS.Objects,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.WindowsFont,
  GLS.GeomObjects,
  GLS.Coordinates,
 
  GLS.BaseClasses,
  GLS.BitmapFont;

type
  TFormWinFont = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    HUDText1: TGLHUDText;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    HUDText2: TGLHUDText;
    HUDText3: TGLHUDText;
    Teapot1: TGLTeapot;
    WindowsBitmapFont1: TGLWindowsBitmapFont;
    MainMenu1: TMainMenu;
    MIPickFont: TMenuItem;
    FontDialog1: TFontDialog;
    MIViewTexture: TMenuItem;
    MIFPS: TMenuItem;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1Click(Sender: TObject);
    procedure MIPickFontClick(Sender: TObject);
    procedure MIViewTextureClick(Sender: TObject);
  private
     
  public
     
  end;

var
  FormWinFont: TFormWinFont;

implementation

uses
  WinFontTextureFm;

{$R *.DFM}

procedure TFormWinFont.FormCreate(Sender: TObject);
begin
   // sorry, couldn't resist again...
   HUDText1.Text:= 'Lorem ipsum dolor sit amer, consectetaur adipisicing elit,'#13#10
                  +'sed do eiusmod tempor incididunt ut labore et dolore magna'#13#10
                  +'aliqua. Ut enim ad minim veniam, quis nostrud exercitation'#13#10
                  +'ullamco laboris nisi ut aliquip ex ea commodo consequat.'#13#10
                  +'Duis aute irure dolor in reprehenderit in voluptate velit'#13#10
                  +'esse cillum dolore eu fugiat nulla pariatur. Excepteur sint'#13#10
                  +'occaecat cupidatat non proident, sunt in culpa qui officia'#13#10
                  +'deserunt mollit anim id est laborum.'#13#10
                  +'Woblis ten caracuro Zapothek it Setag!'; // I needed an uppercase 'W' too...

  HUDText1.Text := HUDText1.Text + #13#10'Unicode text...' +
    WideChar($0699)+WideChar($069A)+WideChar($963f)+WideChar($54c0);
  WindowsBitmapFont1.EnsureString(HUDText1.Text);
end;

procedure TFormWinFont.MIPickFontClick(Sender: TObject);
begin
   FontDialog1.Font:=WindowsBitmapFont1.Font;
   if FontDialog1.Execute then begin
      WindowsBitmapFont1.Font:=FontDialog1.Font;
      HUDText1.ModulateColor.AsWinColor:=FontDialog1.Font.Color;
   end;
end;

procedure TFormWinFont.MIViewTextureClick(Sender: TObject);
begin
   with FormFontTexture.Image1 do begin
      Picture:=WindowsBitmapFont1.Glyphs;
      FormFontTexture.Width:=Picture.Width;
      FormFontTexture.Height:=Picture.Height;
   end;
   FormFontTexture.Show;
end;

procedure TFormWinFont.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   // make things move a little
   HUDText2.Rotation:=HUDText2.Rotation+15*deltaTime;
   HUDText3.Scale.X:=sin(newTime)+1.5;
   HUDText3.Scale.Y:=cos(newTime)+1.5;
   GLSceneViewer1.Invalidate;
end;

procedure TFormWinFont.Timer1Timer(Sender: TObject);
begin
   miFPS.Caption:=Format('%.1f FPS - %d x %d Font Texture',
                   [GLSceneViewer1.FramesPerSecond,
                    WindowsBitmapFont1.FontTextureWidth,
                    WindowsBitmapFont1.FontTextureHeight]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormWinFont.GLSceneViewer1Click(Sender: TObject);
begin
   Teapot1.Visible:=not Teapot1.Visible;
end;

end.
