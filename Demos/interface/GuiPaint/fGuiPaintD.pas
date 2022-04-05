unit fGuiPaintD;

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
  GLS.BitmapFont,
  GLS.SceneViewer,
  GLS.WindowsFont,
  GLS.Windows,
  GLS.Gui,
  GLS.Texture,
  GLS.Canvas,

  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Utils;

type
  TFormGuiPaint = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    WindowsBitmapFont1: TGLWindowsBitmapFont;
    MainMenu1: TMainMenu;
    Font1: TMenuItem;
    miWindowsFont1: TMenuItem;
    FontDialog1: TFontDialog;
    GLGuiLayout1: TGLGuiLayout;
    GLForm1: TGLForm;
    GLMaterialLibrary1: TGLMaterialLibrary;
    BrushButton: TGLButton;
    PenButton: TGLButton;
    GLPanel1: TGLPanel;
    GLCanvas: TGLCustomControl;
    WhiteButton: TGLButton;
    BlackButton: TGLButton;
    RedButton: TGLButton;
    GreenButton: TGLButton;
    BlueButton: TGLButton;
    GuiRoot: TGLBaseControl;
    File1: TMenuItem;
    miOpen1: TMenuItem;
    miSave1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    miFPS: TMenuItem;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure miWindowsFont1Click(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GLCanvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLCanvasMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCanvasMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLCanvasRender(Sender: TGLCustomControl; Bitmap: TBitmap);
    procedure FormCreate(Sender: TObject);
    procedure WhiteButtonButtonClick(Sender: TObject);
    procedure BlackButtonButtonClick(Sender: TObject);
    procedure RedButtonButtonClick(Sender: TObject);
    procedure GreenButtonButtonClick(Sender: TObject);
    procedure BlueButtonButtonClick(Sender: TObject);
    procedure PenButtonButtonClick(Sender: TObject);
    procedure BrushButtonButtonClick(Sender: TObject);
    procedure GLCanvasAcceptMouseQuery(Sender: TGLBaseControl;
      Shift: TShiftState; Action: TGLMouseAction; Button: TMouseButton;
      X, Y: Integer; var accept: Boolean);
    procedure GLForm1Moving(Sender: TGLForm; var Left, Top: Single);
    procedure miOpen1Click(Sender: TObject);
    procedure miSave1Click(Sender: TObject);
  public
    StartX: Integer;
    StartY: Integer;
    CurrentX: Integer;
    CurrentY: Integer;
  end;

var
  FormGuiPaint: TFormGuiPaint;

implementation

{$R *.DFM}

procedure TFormGuiPaint.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir(); // go to media dir
  GLMaterialLibrary1.TexturePaths := GetCurrentDir();
  GLCanvas.MaxInvalidRenderCount := 40;
  StartX := -1;
end;

procedure TFormGuiPaint.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  // set frame rate to 10 when program is not focused to reduce cpu usage...
  If FormGuiPaint.Focused then
    GLCadencer1.SleepLength := 0
  else
    GLCadencer1.SleepLength := 100;

  // make things move a little
  GLForm1.DoChanges;
end;

procedure TFormGuiPaint.Timer1Timer(Sender: TObject);
begin
  miFPS.Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormGuiPaint.miWindowsFont1Click(Sender: TObject);
begin
  FontDialog1.Font := WindowsBitmapFont1.Font;
  if FontDialog1.Execute then
    WindowsBitmapFont1.Font := FontDialog1.Font;
end;

procedure TFormGuiPaint.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GuiRoot.MouseDown(Sender, TMouseButton(Button), Shift, X, Y);
end;

procedure TFormGuiPaint.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  GuiRoot.MouseMove(Sender, Shift, X, Y);
end;

procedure TFormGuiPaint.GLSceneViewer1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GuiRoot.MouseUp(Sender, TMouseButton(Button), Shift, X, Y);
end;

procedure TFormGuiPaint.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  GuiRoot.KeyDown(Sender, Key, Shift);
end;

procedure TFormGuiPaint.FormKeyPress(Sender: TObject; var Key: Char);
begin
  GuiRoot.KeyPress(Sender, Key);
end;

procedure TFormGuiPaint.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  GuiRoot.KeyUp(Sender, Key, Shift);
end;

procedure TFormGuiPaint.GLCanvasMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    // Make sure all mouse events are sent to the canvas before other GuiComponents, see GLCanvasAcceptMouseQuery.
    GuiRoot.ActiveControl := GLCanvas;
    // Set a status not to send mouse message to child components if any, see GLCanvasAcceptMouseQuery.
    GLCanvas.KeepMouseEvents := True;
    StartX := X;
    StartY := Y;
  end;
end;

procedure TFormGuiPaint.GLCanvasMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  CurrentX := X;
  CurrentY := Y;
end;

procedure TFormGuiPaint.GLCanvasMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    StartX := -1;
    StartY := -1;
    // Set normal mouse message handling, see GLCanvasAcceptMouseQuery.
    GuiRoot.ActiveControl := Nil;
    // Set that childs are allowed to get mouse events, meant for then, see GLCanvasAcceptMouseQuery.
    GLCanvas.KeepMouseEvents := False;
  end;
end;

procedure TFormGuiPaint.GLCanvasRender(Sender: TGLCustomControl;
  Bitmap: TBitmap);
begin
  Bitmap.Width := Round(GLCanvas.Width);
  Bitmap.Height := Round(GLCanvas.Height);
  if StartX <> -1 then
  begin
    Bitmap.Canvas.MoveTo(StartX - Round(Sender.Position.X),
      StartY - Round(Sender.Position.Y));
    Bitmap.Canvas.LineTo(CurrentX - Round(Sender.Position.X),
      CurrentY - Round(Sender.Position.Y));
    StartX := CurrentX;
    StartY := CurrentY;
  end;
end;

procedure TFormGuiPaint.PenButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Width := 1;
end;

procedure TFormGuiPaint.BrushButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Width := 5;
end;

procedure TFormGuiPaint.WhiteButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Color := clWhite;
end;

procedure TFormGuiPaint.BlackButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Color := clBlack;
end;

procedure TFormGuiPaint.RedButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Color := clRed;
end;

procedure TFormGuiPaint.GreenButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Color := clGreen;
end;

procedure TFormGuiPaint.BlueButtonButtonClick(Sender: TObject);
begin
  GLCanvas.Bitmap.Canvas.Pen.Color := clBlue;
end;

procedure TFormGuiPaint.GLCanvasAcceptMouseQuery(Sender: TGLBaseControl;
  Shift: TShiftState; Action: TGLMouseAction; Button: TMouseButton;
  X, Y: Integer; var accept: Boolean);
begin
  // Sender.KeepMouseEvents is set when drawing,
  // if drawing this component, gets mouse events even if they are out of bounds!
  if Sender.KeepMouseEvents then
    accept := True;
end;

procedure TFormGuiPaint.GLForm1Moving(Sender: TGLForm; var Left, Top: Single);
begin
  // make sure the form isn't moved out of bounds...

  if Left > GLSceneViewer1.Width - 32 then
    Left := GLSceneViewer1.Width - 32;

  if Left + Sender.Width < 32 then
    Left := 32 - Sender.Width;

  if Top > GLSceneViewer1.Height - 32 then
    Top := GLSceneViewer1.Height - 32;

  if Top < 0 then
    Top := 0;
end;

procedure TFormGuiPaint.miOpen1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    GLCanvas.Bitmap.LoadFromFile(OpenDialog1.FileName);
end;

procedure TFormGuiPaint.miSave1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    GLCanvas.Bitmap.SaveToFile(SaveDialog1.FileName);
end;

end.
