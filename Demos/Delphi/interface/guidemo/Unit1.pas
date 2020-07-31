unit Unit1;

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
  
  GLScene,
  GLHUDObjects,
  GLObjects,
  GLCadencer,
  GLBitmapFont,
  GLSceneViewer,
  GLWindowsFont,
  GLWindows,
  GLGui,
  GLTexture,
  GLCrossPlatform,
  GLMaterial,
  GLCoordinates,
  GLBaseClasses,
  GLS.Utils;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    WindowsBitmapFont1: TGLWindowsBitmapFont;
    MainMenu1: TMainMenu;
    Font1: TMenuItem;
    WindowsFont1: TMenuItem;
    FontDialog1: TFontDialog;
    GLGuiLayout1: TGLGuiLayout;
    GLForm1: TGLForm;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLButton1: TGLButton;
    GLEdit1: TGLEdit;
    GLLabel1: TGLLabel;
    miFPS: TMenuItem;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure WindowsFont1Click(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GLButton1ButtonClick(Sender: TObject);
  private
     
  public
     
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  SetGLSceneMediaDir();
  GLForm1.Caption := 'Unicode caption...'#$0699#$069A#$963f#$54c0;
  WindowsBitmapFont1.EnsureString(GLForm1.Caption);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLForm1.DoChanges;
  // make things move a little
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  miFPS.Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.WindowsFont1Click(Sender: TObject);
begin
  FontDialog1.Font := WindowsBitmapFont1.Font;
  if FontDialog1.Execute then
    WindowsBitmapFont1.Font := FontDialog1.Font;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GLForm1.MouseDown(Sender, TMouseButton(Button), Shift, X, Y);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  GLForm1.MouseMove(Sender, Shift, X, Y);
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GLForm1.MouseUp(Sender, TMouseButton(Button), Shift, X, Y);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  GLForm1.KeyDown(Sender, Key, Shift);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  GLForm1.KeyPress(Sender, Key);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  GLForm1.KeyUp(Sender, Key, Shift);
end;

procedure TForm1.GLButton1ButtonClick(Sender: TObject);
Var
  OldCaption: String;
begin
  OldCaption := GLForm1.Caption;
  GLForm1.Caption := GLEdit1.Caption;
  GLEdit1.Caption := OldCaption;
end;

end.
