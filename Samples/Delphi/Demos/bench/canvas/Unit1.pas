unit Unit1;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,

  
  GLScene,
  GLWin32Viewer,
  GLBitmapFont,
  GLWindowsFont,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLCanvas,
  GLTexture,
  GLRenderContextInfo;

type
  TForm1 = class(TForm)
    BULines: TButton;
    BUEllipses: TButton;
    GLSceneViewer: TGLSceneViewer;
    PaintBox: TPaintBox;
    LAGLCanvas: TLabel;
    LAGDI: TLabel;
    Bevel1: TBevel;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    RBPenWidth1: TRadioButton;
    RBPenWidth2: TRadioButton;
    BUPoints: TButton;
    BURects: TButton;
    BUTextOut: TButton;
    WindowsBitmapFont: TGLWindowsBitmapFont;
    GLDirectOpenGL1: TGLDirectOpenGL;
    procedure BULinesClick(Sender: TObject);
    procedure BUEllipsesClick(Sender: TObject);
    procedure BUPointsClick(Sender: TObject);
    procedure BURectsClick(Sender: TObject);
    procedure BUTextOutClick(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure BUArcClick(Sender: TObject);
  private
     
    procedure PaintTheBox;
    procedure Bench;
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TWhat = (wLines, wEllipses, wRects, wPoints, wTextOut, wArcs);

var
  vWhat: TWhat;
  vPenWidth: Integer;

const
  cNbLines = 20000;
  cNbEllipses = 20000;
  cNbRects = 5000;
  cNbPoints = 200000;
  cNbTextOuts = 20000;
  cNbArcs = 20000;

procedure TForm1.BULinesClick(Sender: TObject);
begin
  vWhat := wLines;
  Bench;
end;

procedure TForm1.BUArcClick(Sender: TObject);
begin
  vWhat := wArcs;
  Bench;
end;

procedure TForm1.BUEllipsesClick(Sender: TObject);
begin
  vWhat := wEllipses;
  Bench;
end;

procedure TForm1.BURectsClick(Sender: TObject);
begin
  vWhat := wRects;
  Bench;
end;

procedure TForm1.BUPointsClick(Sender: TObject);
begin
  vWhat := wPoints;
  Bench;
end;

procedure TForm1.BUTextOutClick(Sender: TObject);
begin
  vWhat := wTextOut;
  Bench;
end;

procedure TForm1.Bench;
var
  t: Int64;
begin
  if RBPenWidth1.Checked then
    vPenWidth := 1
  else
    vPenWidth := 2;

  Application.ProcessMessages;
  RandSeed := 0;

  t := StartPrecisionTimer;
  GLSceneViewer.Refresh;
  LAGLCanvas.Caption := Format('GLCanvas: %.2f msec',
    [StopPrecisionTimer(t) * 1000]);

  Application.ProcessMessages;
  RandSeed := 0;

  t := StartPrecisionTimer;
  PaintTheBox;
  LAGDI.Caption := Format('GDI: %.1f msec', [StopPrecisionTimer(t) * 1000]);
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  i, x, y: Integer;
  glc: TGLCanvas;
  r: TRect;
  color: TColor;
begin
  glc := TGLCanvas.Create(256, 256);
  with glc do
  begin
    PenWidth := vPenWidth;
    case vWhat of
      wLines:
        begin
          for i := 1 to cNbLines do
          begin
            PenColor := Random(256 * 256 * 256);
            MoveTo(Random(256), Random(256));
            LineTo(Random(256), Random(256));
          end;
        end;
      wEllipses:
        for i := 1 to cNbEllipses do
        begin
          PenColor := Random(256 * 256 * 256);
          EllipseBB(Random(256), Random(256), Random(256), Random(256));
        end;
      wRects:
        for i := 1 to cNbRects do
        begin
          PenColor := Random(256 * 256 * 256);
          r := Rect(Random(256), Random(256), Random(256), Random(256));
          FillRect(r.Left, r.Top, r.Right, r.Bottom);
        end;
      wPoints:
        begin
          for i := 1 to cNbPoints do
          begin
            PenColor := Random(256 * 256 * 256);
            PlotPixel(Random(256), Random(256));
          end;
        end;
      wTextOut:
        begin
          for i := 1 to cNbTextOuts do
          begin
            color := Random(256 * 256 * 256);
            x := Random(256);
            y := Random(256);
            WindowsBitmapFont.TextOut(rci, x, y, 'Hello', color);
          end;
        end;
      wArcs:
        begin
          for i := 1 to cNbEllipses do
          begin
            PenColor := Random(256 * 256 * 256);
            Arc(Random(256), Random(256), Random(256), Random(256), Random(256),
              Random(256), Random(256), Random(256))
          end;
        end;
    end;
  end;
  glc.Free;
end;

procedure TForm1.PaintTheBox;
var
  i, x, y: Integer;
  r: TRect;
  b: TBitmap;
begin
  // to be fair, use offscreen painting...
  b := TBitmap.Create;
  b.Width := 256;
  b.Height := 256;
  with b.Canvas do
  begin
    Brush.Style := bsClear;
    Pen.Width := vPenWidth;
    case vWhat of
      wLines:
        begin
          for i := 1 to cNbLines do
          begin
            Pen.color := Random(256 * 256 * 256);
            MoveTo(Random(256), Random(256));
            LineTo(Random(256), Random(256));
          end;
        end;
      wEllipses:
        begin
          for i := 1 to cNbEllipses do
          begin
            Pen.color := Random(256 * 256 * 256);
            Ellipse(Random(256), Random(256), Random(256), Random(256));
          end;
        end;
      wRects:
        begin
          Brush.Style := bsSolid;
          for i := 1 to cNbRects do
          begin
            Brush.color := Random(256 * 256 * 256);
            r := Rect(Random(256), Random(256), Random(256), Random(256));
            FillRect(r);
          end;
        end;
      wPoints:
        begin
          for i := 1 to cNbPoints do
          begin
            Pixels[Random(256), Random(256)] := Random(256 * 256 * 256);
          end;
        end;
      wTextOut:
        begin
          Font := WindowsBitmapFont.Font;
          for i := 1 to cNbTextOuts do
          begin
            Font.color := Random(256 * 256 * 256);
            x := Random(256);
            y := Random(256);
            TextOut(x, y, 'Hello');
          end
        end;
      wArcs:
        begin
          for i := 1 to cNbEllipses do
          begin
            Pen.color := Random(256 * 256 * 256);
            Arc(Random(256), Random(256), Random(256), Random(256), Random(256),
              Random(256), Random(256), Random(256))
          end;
        end;
    end;
  end;
  PaintBox.Canvas.Draw(0, 0, b);
  b.Free;
end;

end.
