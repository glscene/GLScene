unit fCanvasD;

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


  GLS.Scene,
  GLS.SceneViewer,
  GLS.BitmapFont,
  GLS.WindowsFont,
  GLS.Coordinates,

  GLS.BaseClasses,
  GLS.Canvas,
  GLS.Texture,
  GLS.RenderContextInfo,
  GLS.Utils;

type
  TFormCanvas = class(TForm)
    BULines: TButton;
    BUEllipses: TButton;
    GLSceneViewer: TGLSceneViewer;
    PaintBox: TPaintBox;
    lbGLCanvas: TLabel;
    lbGDI: TLabel;
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
  FormCanvas: TFormCanvas;

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

procedure TFormCanvas.BULinesClick(Sender: TObject);
begin
  vWhat := wLines;
  Bench;
end;

procedure TFormCanvas.BUArcClick(Sender: TObject);
begin
  vWhat := wArcs;
  Bench;
end;

procedure TFormCanvas.BUEllipsesClick(Sender: TObject);
begin
  vWhat := wEllipses;
  Bench;
end;

procedure TFormCanvas.BURectsClick(Sender: TObject);
begin
  vWhat := wRects;
  Bench;
end;

procedure TFormCanvas.BUPointsClick(Sender: TObject);
begin
  vWhat := wPoints;
  Bench;
end;

procedure TFormCanvas.BUTextOutClick(Sender: TObject);
begin
  vWhat := wTextOut;
  Bench;
end;

procedure TFormCanvas.Bench;
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
  lbGLCanvas.Caption := Format('GLCanvas: %.2f msec',
    [StopPrecisionTimer(t) * 1000]);

  Application.ProcessMessages;
  RandSeed := 0;

  t := StartPrecisionTimer;
  PaintTheBox;
  lbGDI.Caption := Format('GDI: %.1f msec', [StopPrecisionTimer(t) * 1000]);
end;

procedure TFormCanvas.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  i, x, y: Integer;
  GLCanvas: TGLCanvas;
  r: TRect;
  Color: TColor;
begin
  GLCanvas := TGLCanvas.Create(256, 256);
  GLCanvas.PenWidth := vPenWidth;
  case vWhat of
    wLines:
      begin
        for i := 1 to cNbLines do
        begin
          GLCanvas.PenColor := Random(256 * 256 * 256);
          GLCanvas.MoveTo(Random(256), Random(256)); // first point
          GLCanvas.LineTo(Random(256), Random(256)); // second point
        end;
      end;
    wEllipses:
      for i := 1 to cNbEllipses do
      begin
        GLCanvas.PenColor := Random(256 * 256 * 256);
        GLCanvas.EllipseBB(Random(256), Random(256), Random(256), Random(256));
      end;
    wRects:
      for i := 1 to cNbRects do
      begin
        GLCanvas.PenColor := Random(256 * 256 * 256);
        r := Rect(Random(256), Random(256), Random(256), Random(256));
        GLCanvas.FillRect(r.Left, r.Top, r.Right, r.Bottom);
      end;
    wPoints:
      begin
        for i := 1 to cNbPoints do
        begin
          GLCanvas.PenColor := Random(256 * 256 * 256);
          GLCanvas.PlotPixel(Random(256), Random(256));
        end;
      end;
    wTextOut:
      begin
        for i := 1 to cNbTextOuts do
        begin
          Color := Random(256 * 256 * 256);
          x := Random(256);
          y := Random(256);
          WindowsBitmapFont.TextOut(rci, x, y, 'Hello', Color);
        end;
      end;
    wArcs:
      begin
        for i := 1 to cNbEllipses do
        begin
          GLCanvas.PenColor := Random(256 * 256 * 256);
          GLCanvas.Arc(Random(256), Random(256), Random(256), Random(256),
            Random(256), Random(256), Random(256), Random(256))
        end;
      end;
  end;
  GLCanvas.Free;
end;

procedure TFormCanvas.PaintTheBox;
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
