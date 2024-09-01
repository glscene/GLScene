//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Canvas;

(*
   Implements a basic Canvas-like interface over for OpenGL
   This class can be used for generic OpenGL applications and has no dependencies
   to the GXScene core units (only to base units).
*)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  System.Types,
  System.Classes,
  System.UITypes,
  System.Math,
  FMX.Graphics,

  GXS.VectorGeometry,
  GXS.Color,
  GXS.Context,
  GXS.VectorTypes,
  GXS.State;

type

  TgxArcDirection = (adCounterClockWise, adClockWise);

  (* A simple Canvas-like interface for OpenGL.
     This class implements a small "shell" for 2D operations in OpenGL,
     it operates over the current OpenGL context and provides methods
     for drawing lines, ellipses and points.
     This class is typically used by creating an instance, using it for drawing,
     and freeing the instance. When drawing (0, 0) is the top left corner.
     All coordinates are internally maintained with floating point precision.
     Several states are cached and it is of primary importance not to invoke
     OpenGL directly throughout the life of an instance (at the cost of
     unespected behaviour). *)
  
  TgxCanvas = class
  private
    FBufferSizeX, FBufferSizeY: Integer;
    FLastPrimitive: Integer;
    FCurrentPos: TAffineVector;
    FPenColor: TColor;
    FPenWidth: Integer;
    FCurrentPenColorVector: TVector4f;
    FArcDirection: TgxArcDirection;
  protected
    procedure BackupOpenGLStates;
    procedure StartPrimitive(const primitiveType: Integer);
    procedure EllipseVertices(x, y, xRadius, yRadius: Single);
    procedure SetPenColor(const val: TColor);
    function GetPenAlpha: Single;
    procedure SetPenAlpha(const val: Single);
    procedure SetPenWidth(const val: Integer);
    procedure SwapSingle(pX, pY: PSingle);
    procedure NormalizePoint(const x1, y1, x2, y2: Single;
      const x, y: Single; pX, pY: PSingle);
    procedure DrawArc(x1, y1, x2, y2, x3, y3, x4, y4: Single;
      UpdateCurrentPos: Boolean); overload;
    procedure DrawArc(x1, y1, x2, y2: Single;
      AngleBegin, AngleEnd: Single;
      UpdateCurrentPos: Boolean); overload;
  public
    constructor Create(bufferSizeX, bufferSizeY: Integer;
      const baseTransform: TMatrix4f); overload;
    constructor Create(bufferSizeX, bufferSizeY: Integer); overload;
    destructor Destroy; override;
    (* Stops the current internal primitive.
      This function is invoked automatically by TGLCanvas when changeing
      primitives, you should directly call if you want to render your
      own stuff intertwined with TgxCanvas drawings. In that case, call
      it before your own OpenGL calls. *)
    procedure StopPrimitive;
    (* Inverts the orientation of the Y Axis.
      If (0, 0) was in the top left corner, it will move to the bottom
      left corner or vice-versa. *)
    procedure InvertYAxis;
    property CanvasSizeX: Integer read FBufferSizeX;
    property CanvasSizeY: Integer read FBufferSizeY;
    // Current Pen Color. 
    property PenColor: TColor read FPenColor write SetPenColor;
    // Current Pen Alpha channel (from 0.0 to 1.0) 
    property PenAlpha : Single read GetPenAlpha write SetPenAlpha;
    // Current Pen Width. 
    property PenWidth: Integer read FPenWidth write SetPenWidth;
    // Updates the current position (absolute coords). 
    procedure MoveTo(const x, y: Integer); overload;
    procedure MoveTo(const x, y: Single); overload;
    // Updates the current position (relative coords). 
    procedure MoveToRel(const x, y: Integer); overload;
    procedure MoveToRel(const x, y: Single); overload;
    (* Draws a line from current position to given coordinate.
     Current position is updated. *)
    procedure LineTo(const x, y: Integer); overload;
    procedure LineTo(const x, y: Single); overload;
    procedure LineToRel(const x, y: Integer); overload;
    procedure LineToRel(const x, y: Single); overload;
    (* Draws a line from (x1, y1) to (x2, y2).
     The current position is NOT updated. *)
    procedure Line(const x1, y1, x2, y2: Integer); overload;
    procedure Line(const x1, y1, x2, y2: Single); overload;
    (* Draws the set of lines defined by connecting the points.
       Similar to invoking MoveTo on the first point, then LineTo
       on all the following points. *)
    procedure Polyline(const points: array of TPoint);
    // Similar to Polyline but also connects the last point to the first. 
    procedure Polygon(const points: array of TPoint);
    (* Plots a pixel at given coordinate. PenWidth affects pixel size.
     The current position is NOT updated. *)
    procedure PlotPixel(const x, y: Integer); overload;
    procedure PlotPixel(const x, y: Single); overload;
    // Draw the (x1,y1)-(x2, y2) rectangle's frame (border). 
    procedure FrameRect(const x1, y1, x2, y2: Integer); overload;
    procedure FrameRect(const x1, y1, x2, y2: Single); overload;
    // Draw the (x1,y1)-(x2, y2) rectangle (filled with PenColor). 
    procedure FillRect(const x1, y1, x2, y2: Integer); overload;
    procedure FillRect(const x1, y1, x2, y2: Single); overload;
    // Draw the (x1,y1)-(x2, y2) rectangle (filled with given gradient's color). 
    procedure FillRectGradient(const x1, y1, x2, y2: Single;
      const x1y1Color, x2y1Color, x2y2Color, x1y2Color: TgxColorVector); overload;
    procedure FillRectGradient(const x1, y1, x2, y2: Integer;
      const x1y1Color, x2y1Color, x2y2Color, x1y2Color: TgxColorVector); overload;
    {Draws an ellipse with (x1,y1)-(x2, y2) bounding rectangle. }
    procedure EllipseBB(const x1, y1, x2, y2: Integer); overload;
    procedure EllipseBB(const x1, y1, x2, y2: Single); overload;
    {Draws and ellipse centered at (x, y) with given radiuses. }
    procedure Ellipse(const x, y: Integer; const xRadius, yRadius: Single); overload;
    procedure Ellipse(const x, y: Single; const xRadius, yRadius: Single); overload;
    procedure Ellipse(const x, y: Single; const Radius: Single); overload;
    { Draw a filled ellipse. }
    procedure FillEllipse(const x, y: Integer; const xRadius, yRadius: Single); overload;
    procedure FillEllipse(const x, y: Single; const xRadius, yRadius: Single); overload;
    procedure FillEllipse(const x, y: Single; const Radius: Single); overload;
    { Draw a filled gradient ellipse.
    OpenGL will use the last PenColor and PenAlpha as the center color and do gradient to edge of ellipse using the edgeColor parameter. }
    procedure FillEllipseGradient(const x, y, xRadius, yRadius: Single;
      const edgeColor: TgxColorVector); overload;
    procedure FillEllipseGradient(const x, y: Integer;
      const xRadius, yRadius: Integer; const edgeColor: TgxColorVector); overload;
    procedure FillEllipseGradient(const x, y, Radius: Single;
      const edgeColor: TgxColorVector); overload;
    { Draw an elliptical arc. 
       The points (x1, y1) and (x2, y2) specify the bounding rectangle. 
       An ellipse formed by the specified bounding rectangle defines the curve of the arc. 
       The arc extends in the current drawing direction from the point where it intersects the radial from the center of the bounding rectangle to the (x3, y3) point. 
       The arc ends where it intersects the radial from the center of the bounding rectangle to the (x4, y4) point. 
       If the starting point and ending point are the same, a complete ellipse is drawn. 
       Use the ArcDirection property to get and set the current drawing direction for a device context. 
       The default drawing direction is counterclockwise. }
    procedure Arc(const x1, y1, x2, y2, x3, y3, x4, y4: Integer); overload;
    procedure Arc(const x1, y1, x2, y2, x3, y3, x4, y4: Single); overload;
    procedure Arc(const x1, y1, x2, y2: Single; AngleBegin, AngleEnd: Single); overload;
    { Same as Arc but update the current position. }
    procedure ArcTo(const x1, y1, x2, y2, x3, y3, x4, y4: Integer); overload;
    procedure ArcTo(const x1, y1, x2, y2, x3, y3, x4, y4: Single); overload;
    procedure ArcTo(const x1, y1, x2, y2: Single; AngleBegin, AngleEnd: Single); overload;
    procedure RoundRect(const x1, y1, x2, y2, xr, yr: Integer); overload;
    procedure RoundRect(const x1, y1, x2, y2, xr, yr: Single); overload;
    property ArcDirection: TgxArcDirection read FArcDirection write FArcDirection;
  end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------

const
  cNoPrimitive = MaxInt;
  pion2 = pi/2;
  pi3on2 = 3*pion2;

// ------------------
// ------------------ TgxCanvas ------------------
// ------------------

constructor TgxCanvas.Create(bufferSizeX, bufferSizeY: Integer;
  const baseTransform: TMatrix4f);
var
  PM: TMatrix4f;
begin
  FBufferSizeX := bufferSizeX;
  FBufferSizeY := bufferSizeY;

  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  PM := CreateOrthoMatrix(0, bufferSizeX, bufferSizeY, 0, -1, 1);
  glLoadMatrixf(@PM);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadMatrixf(@baseTransform);
  BackupOpenGLStates;
  FLastPrimitive := cNoPrimitive;
  FArcDirection := adCounterClockWise;
end;

constructor TgxCanvas.Create(bufferSizeX, bufferSizeY: Integer);
begin
  Create(bufferSizeX, bufferSizeY, IdentityHmgMatrix);
end;

destructor TgxCanvas.Destroy;
begin
  StopPrimitive;
  glMatrixMode(GL_PROJECTION);
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
end;

procedure TgxCanvas.BackupOpenGLStates;
begin
  with CurrentContext.gxStates do
  begin
    Disable(stLighting);
    Disable(stFog);
    Disable(stCullFace);
    Disable(stColorMaterial);
    Disable(stDepthTest);
    Disable(stLineSmooth);
    Disable(stLineStipple);
    Disable(stPointSmooth);
    Enable(stBlend);
    SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);

    // Setup and backup pen stuff
    FPenColor := TColorRec.Black;
    SetVector(FCurrentPenColorVector, NullHmgPoint);
    glColor4fv(@FCurrentPenColorVector);
    FPenWidth := 1;
    LineWidth := 1;
    PointSize := 1;
  end;
end;

procedure TgxCanvas.StartPrimitive(const primitiveType: Integer);
begin
  if primitiveType <> FLastPrimitive then
  begin
    if FLastPrimitive <> cNoPrimitive then
      glEnd;
    if primitiveType <> cNoPrimitive then
      glBegin(primitiveType);
    FLastPrimitive := primitiveType;
  end;
end;

procedure TgxCanvas.StopPrimitive;
begin
  StartPrimitive(cNoPrimitive);
end;

procedure TgxCanvas.InvertYAxis;
var
  mat: TMatrix4f;
begin
  mat := IdentityHmgMatrix;
  mat.Y.Y := -1;
  mat.W.Y := FBufferSizeY;
  glMultMatrixf(@mat);
end;

procedure TgxCanvas.SetPenColor(const val: TColor);
begin
  SetVector(FCurrentPenColorVector, ConvertWinColor(val,
    FCurrentPenColorVector.W));
  FPenColor := val;
  glColor4fv(@FCurrentPenColorVector);
end;

procedure TgxCanvas.SetPenAlpha(const val: Single);
begin
  FCurrentPenColorVector.W := val;
  glColor4fv(@FCurrentPenColorVector);
end;

procedure TgxCanvas.SetPenWidth(const val: Integer);
begin
  if val < 1 then
    Exit;
  if val <> FPenWidth then
    with CurrentContext.gxStates do
    begin
      FPenWidth := val;
      StopPrimitive;
      LineWidth := val;
      PointSize := val;
    end;
end;

procedure TgxCanvas.MoveTo(const x, y: Integer);
begin
  FCurrentPos.X := x;
  FCurrentPos.Y := y;
end;

procedure TgxCanvas.MoveTo(const x, y: Single);
begin
  FCurrentPos.X := x;
  FCurrentPos.Y := y;
end;

procedure TgxCanvas.MoveToRel(const x, y: Integer);
begin
  FCurrentPos.X := FCurrentPos.X + x;
  FCurrentPos.Y := FCurrentPos.Y + y;
end;

procedure TgxCanvas.MoveToRel(const x, y: Single);
begin
  FCurrentPos.X := FCurrentPos.X + x;
  FCurrentPos.Y := FCurrentPos.Y + y;
end;

procedure TgxCanvas.LineTo(const x, y: Integer);
begin
  StartPrimitive(GL_LINES);
  glVertex2fv(@FCurrentPos);
  MoveTo(x, y);
  glVertex2fv(@FCurrentPos);
end;

procedure TgxCanvas.LineTo(const x, y: Single);
begin
  StartPrimitive(GL_LINES);
  glVertex2fv(@FCurrentPos);
  MoveTo(x, y);
  glVertex2fv(@FCurrentPos);
end;

procedure TgxCanvas.LineToRel(const x, y: Integer);
begin
  LineTo(FCurrentPos.X + x, FCurrentPos.Y + y);
end;

procedure TgxCanvas.LineToRel(const x, y: Single);
begin
  LineTo(FCurrentPos.X + x, FCurrentPos.Y + y);
end;

procedure TgxCanvas.Line(const x1, y1, x2, y2: Integer);
begin
  StartPrimitive(GL_LINES);
  glVertex2i(x1, y1);
  glVertex2i(x2, y2);
end;

procedure TgxCanvas.Line(const x1, y1, x2, y2: Single);
begin
  StartPrimitive(GL_LINES);
  glVertex2f(x1, y1);
  glVertex2f(x2, y2);
end;

procedure TgxCanvas.Polyline(const points: array of TPoint);
var
  i, n: Integer;
begin
  n := Length(Points);
  if n > 1 then
  begin
    StartPrimitive(GL_LINE_STRIP);
    glVertex2iv(@points[Low(points)]);
    for i := Low(points) + 1 to High(points) do
      glVertex2iv(@points[i]);
    StopPrimitive;
  end;
end;

procedure TgxCanvas.Polygon(const points: array of TPoint);
var
  i, n: Integer;
begin
  n := Length(Points);
  if n > 1 then
  begin
    StartPrimitive(GL_LINE_LOOP);
    glVertex2iv(@points[Low(points)]);
    for i := Low(points) + 1 to High(points) do
      glVertex2iv(@points[i]);
    StopPrimitive;
  end;
end;

procedure TgxCanvas.PlotPixel(const x, y: Integer);
begin
  StartPrimitive(GL_POINTS);
  glVertex2i(x, y);
end;

procedure TgxCanvas.PlotPixel(const x, y: Single);
begin
  StartPrimitive(GL_POINTS);
  glVertex2f(x, y);
end;

procedure TgxCanvas.FrameRect(const x1, y1, x2, y2: Integer);
begin
  StartPrimitive(GL_LINE_LOOP);
  glVertex2i(x1, y1);
  glVertex2i(x2, y1);
  glVertex2i(x2, y2);
  glVertex2i(x1, y2);
  StopPrimitive;
end;

procedure TgxCanvas.FrameRect(const x1, y1, x2, y2: Single);
begin
  StartPrimitive(GL_LINE_LOOP);
  glVertex2f(x1, y1);
  glVertex2f(x2, y1);
  glVertex2f(x2, y2);
  glVertex2f(x1, y2);
  StopPrimitive;
end;

function TgxCanvas.GetPenAlpha: Single;
begin
  Result := FCurrentPenColorVector.W;
end;

procedure TgxCanvas.FillRect(const x1, y1, x2, y2: Integer);
begin
  StartPrimitive(GL_QUADS);
  glVertex2i(x1, y1);
  glVertex2i(x2, y1);
  glVertex2i(x2, y2);
  glVertex2i(x1, y2);
  StopPrimitive;
end;

procedure TgxCanvas.FillRect(const x1, y1, x2, y2: Single);
begin
  StartPrimitive(GL_QUADS);
  glVertex2f(x1, y1);
  glVertex2f(x2, y1);
  glVertex2f(x2, y2);
  glVertex2f(x1, y2);
  StopPrimitive;
end;

procedure TgxCanvas.EllipseVertices(x, y, xRadius, yRadius: Single);
var
  i, n: Integer;
  s, c: TSingleArray;
begin
  n := Round(MaxFloat(xRadius, yRadius) * 0.1) + 5;
  SetLength(s, n);
  SetLength(c, n);
  Dec(n);
  PrepareSinCosCache(s, c, 0, 90);
  ScaleFloatArray(s, yRadius);
  ScaleFloatArray(c, xRadius);
  // first quadrant (top right)
  for i := 0 to n do
    glVertex2f(x + c[i], y - s[i]);
  // second quadrant (top left)
  for i := n - 1 downto 0 do
    glVertex2f(x - c[i], y - s[i]);
  // third quadrant (bottom left)
  for i := 1 to n do
    glVertex2f(x - c[i], y + s[i]);
  // fourth quadrant (bottom right)
  for i := n - 1 downto 0 do
    glVertex2f(x + c[i], y + s[i]);
end;

procedure TgxCanvas.EllipseBB(const x1, y1, x2, y2: Integer);
begin
  Ellipse((x1 + x2) * 0.5, (y1 + y2) * 0.5, Abs(x2 - x1) * 0.5, Abs(y2 - y1) *
    0.5);
end;

procedure TgxCanvas.EllipseBB(const x1, y1, x2, y2: Single);
begin
  Ellipse((x1 + x2) * 0.5, (y1 + y2) * 0.5, Abs(x2 - x1) * 0.5, Abs(y2 - y1) *
    0.5);
end;

procedure TgxCanvas.Ellipse(const x, y: Single; const Radius: Single);
begin
  Ellipse(x, y, Radius, Radius);
end;

procedure TgxCanvas.Ellipse(const x, y: Integer; const xRadius, yRadius:
  Single);
var
  sx, sy: Single;
begin
  sx := x;
  sy := y;
  Ellipse(sx, sy, xRadius, yRadius);
end;

procedure TgxCanvas.Ellipse(const x, y: Single; const xRadius, yRadius: Single);
begin
  StartPrimitive(GL_LINE_STRIP);
  EllipseVertices(x, y, xRadius, yRadius);
  StopPrimitive;
end;

procedure TgxCanvas.FillEllipse(const x, y: Integer; const xRadius, yRadius:
  Single);
begin
  StartPrimitive(GL_TRIANGLE_FAN);
  glVertex2f(x, y); // not really necessary, but may help with memory stride
  EllipseVertices(x, y, xRadius, yRadius);
  StopPrimitive;
end;

procedure TgxCanvas.FillEllipse(const x, y, xRadius, yRadius: Single);
begin
  StartPrimitive(GL_TRIANGLE_FAN);
  glVertex2f(x, y); // not really necessary, but may help with memory stride
  EllipseVertices(x, y, xRadius, yRadius);
  StopPrimitive;
end;

procedure TgxCanvas.FillEllipse(const x, y, Radius: Single);
begin
  FillEllipse(x, y, Radius, Radius);
end;

procedure TgxCanvas.FillRectGradient(const x1, y1, x2, y2: Single;
  const x1y1Color, x2y1Color, x2y2Color, x1y2Color: TgxColorVector);
begin
  StartPrimitive(GL_QUADS);
  glColor4f(x1y1Color.X, x1y1Color.Y, x1y1Color.Z, x1y1Color.W);
  glVertex2f(x1, y1);
  glColor4f(x2y1Color.X, x2y1Color.Y, x2y1Color.Z, x2y1Color.W);
  glVertex2f(x2, y1);
  glColor4f(x2y2Color.X, x2y2Color.Y, x2y2Color.Z, x2y2Color.W);
  glVertex2f(x2, y2);
  glColor4f(x1y2Color.X, x1y2Color.Y, x1y2Color.Z, x1y2Color.W);
  glVertex2f(x1, y2);
  StopPrimitive;

  // restore pen color
  glColor4fv(@FCurrentPenColorVector);
end;

procedure TgxCanvas.FillRectGradient(const x1, y1, x2, y2: Integer;
  const x1y1Color, x2y1Color, x2y2Color, x1y2Color: TgxColorVector);
begin
  StartPrimitive(GL_QUADS);
  glColor4f(x1y1Color.X, x1y1Color.Y, x1y1Color.Z, x1y1Color.W);
  glVertex2i(x1, y1);
  glColor4f(x2y1Color.X, x2y1Color.Y, x2y1Color.Z, x2y1Color.W);
  glVertex2i(x2, y1);
  glColor4f(x2y2Color.X, x2y2Color.Y, x2y2Color.Z, x2y2Color.W);
  glVertex2i(x2, y2);
  glColor4f(x1y2Color.X, x1y2Color.Y, x1y2Color.Z, x1y2Color.W);
  glVertex2i(x1, y2);
  StopPrimitive;

  // restore pen color
  glColor4fv(@FCurrentPenColorVector);
end;

procedure TgxCanvas.FillEllipseGradient(const x, y: Integer; const xRadius, yRadius: Integer; const edgeColor: TgxColorVector);
begin
  StartPrimitive(GL_TRIANGLE_FAN);

  // the center will use the last set PenColor and PenAlpha
  glVertex2f(x, y); // really necessary now :)

  // then OpenGL will do a gradient from the center to the edge using the edgeColor
  glColor4f(edgeColor.X, edgeColor.Y, edgeColor.Z, edgeColor.W);
  EllipseVertices(x, y, xRadius, yRadius);
  StopPrimitive;

  // restore pen color
  glColor4fv(@FCurrentPenColorVector);
end;

procedure TgxCanvas.FillEllipseGradient(const x, y, xRadius, yRadius: Single; const edgeColor: TgxColorVector);
begin
  StartPrimitive(GL_TRIANGLE_FAN);
  glVertex2f(x, y); // really necessary now :)
  glColor4f(edgeColor.X, edgeColor.Y, edgeColor.Z, edgeColor.W);
  EllipseVertices(x, y, xRadius, yRadius);
  StopPrimitive;

  // restore pen color
  glColor4fv(@FCurrentPenColorVector);
end;

procedure TgxCanvas.FillEllipseGradient(const x, y, Radius: Single; const edgeColor: TgxColorVector);
begin
  FillEllipseGradient(x, y, Radius, Radius, edgeColor);
end;

procedure TgxCanvas.Arc(const x1, y1, x2, y2, x3, y3, x4, y4: Integer);
begin
  DrawArc(x1, y1, x2, y2, x3, y3, x4, y4, False);
end;

procedure TgxCanvas.Arc(const x1, y1, x2, y2, x3, y3, x4, y4: Single);
begin
  DrawArc(x1, y1, x2, y2, x3, y3, x4, y4, False);
end;

procedure TgxCanvas.Arc(const x1, y1, x2, y2: Single; AngleBegin, AngleEnd: Single);
begin
  DrawArc(x1, y1, x2, y2, AngleBegin, AngleEnd, False);
end;

procedure TgxCanvas.ArcTo(const x1, y1, x2, y2, x3, y3, x4, y4: Integer);
begin
  DrawArc(x1, y1, x2, y2, x3, y3, x4, y4, True);
end;

procedure TgxCanvas.ArcTo(const x1, y1, x2, y2, x3, y3, x4, y4: Single);
begin
  DrawArc(x1, y1, x2, y2, x3, y3, x4, y4, True);
end;

procedure TgxCanvas.ArcTo(const x1, y1, x2, y2: Single; AngleBegin, AngleEnd: Single);
begin
  DrawArc(x1, y1, x2, y2, AngleBegin, AngleEnd, True);
end;

procedure TgxCanvas.RoundRect(const x1, y1, x2, y2, xr, yr: Integer);
var
  x2r, y2r, x, y: integer;
begin
  x2r := 2*xr;
  y2r := 2*yr;
  x := x1 -1;
  y := y2 +1;
  Arc(x, y1, x + x2r, y1 + y2r, pi3on2, pi);
  Line(x1, y1 + yr, x1, y - yr);
  Arc(x, y, x + x2r,  y - y2r, pi, pion2);
  Line(x + xr, y2, x2 - xr, y2);
  Arc(x2, y, x2 - x2r, y - y2r, pion2, 0);
  Line(x2, y1 + yr, x2, y - yr);
  Arc(x2, y1, x2 - x2r, y1 + y2r, 0, pi3on2);
  Line(x + xr, y1, x2 - xr, y1);
end;

procedure TgxCanvas.RoundRect(const x1, y1, x2, y2, xr, yr: Single);
var
  x2r, y2r, x, y: Single;
begin
  x2r := 2*xr;
  y2r := 2*yr;
  x := x1 -1;
  y := y2 +1;
  Arc(x, y1, x + x2r, y1 + y2r, pi3on2, pi);
  Line(x1, y1 + yr, x1, y - yr);
  Arc(x, y, x + x2r,  y - y2r, pi, pion2);
  Line(x + xr, y2, x2 - xr, y2);
  Arc(x2, y, x2 - x2r, y - y2r, pion2, 0);
  Line(x2, y1 + yr, x2, y - yr);
  Arc(x2, y1, x2 - x2r, y1 + y2r, 0, pi3on2);
  Line(x + xr, y1, x2 - xr, y1);
end;

// wrapper from "ByPoints" methode

procedure TgxCanvas.DrawArc(x1, y1, x2, y2, x3, y3, x4, y4: Single; UpdateCurrentPos: Boolean);
var
  x, y: Single;
  AngleBegin, AngleEnd: Single;
begin
  if x1 > x2 then
    SwapSingle(@x1, @x2);
  if y1 > y2 then
    SwapSingle(@y1, @y2);

  NormalizePoint(x1, y1, x2, y2, x3, y3, @x, @y);
  AngleBegin := ArcTan2(y, x);

  NormalizePoint(x1, y1, x2, y2, x4, y4, @x, @y);
  AngleEnd := ArcTan2(y, x);

  DrawArc(x1, y1, x2, y2, AngleBegin, AngleEnd, UpdateCurrentPos);
end;

// Real work is here

procedure TgxCanvas.DrawArc(x1, y1, x2, y2: Single; AngleBegin, AngleEnd: Single; UpdateCurrentPos: Boolean);
var
  Xc, Yc, Rx, Ry, x, y, s, c: Single;
  AngleCurrent, AngleDiff, AngleStep: Single;
begin
  // check that our box is well set (as the original Arc function do)
  if x1 > x2 then
    SwapSingle(@x1, @x2);
  if y1 > y2 then
    SwapSingle(@y1, @y2);

  if (x1 = x2) or (y1 = y2) then
    exit;

  Xc := (x1 + x2) * 0.5;
  Yc := (y1 + y2) * 0.5;

  Rx := Abs(x2 - x1) * 0.5;
  Ry := Abs(y2 - y1) * 0.5;

  // if ClockWise then swap AngleBegin and AngleEnd to simulate it.
  if FArcDirection = adClockWise then
  begin
    AngleCurrent := AngleBegin;
    AngleBegin := AngleEnd;
    AngleEnd := AngleCurrent;
  end;

  if (AngleEnd >= AngleBegin) then
  begin // if end sup to begin, remove 2*Pi (360°)
    AngleEnd := AngleEnd - 2 * Pi;
  end;

  AngleDiff := Abs(AngleEnd - AngleBegin); // the amount radian to travel
  AngleStep := AngleDiff / Round(MaxFloat(Rx, Ry) * 0.1 + 5); // granulity of drawing, not too much, not too less

  AngleCurrent := AngleBegin;

  StartPrimitive(GL_LINE_STRIP);
  while AngleCurrent >= AngleBegin - AngleDiff do
  begin
    SinCosine(AngleCurrent, s, c);
    x := Xc + (Rx * c);
    y := Yc + (Ry * s);

    glVertex2f(x, y);

    AngleCurrent := AngleCurrent - AngleStep; // always step down, rotate only one way to draw it
  end;

  SinCosine(AngleEnd, s, c);
  x := Xc + (Rx * c);
  y := Yc + (Ry * s);

  glVertex2f(x, y);

  StopPrimitive();

  if UpdateCurrentPos then
    MoveTo(x, y); //FCurrentPos := CurrentPos;
end;

// for internal need

procedure TgxCanvas.NormalizePoint(const x1, y1, x2, y2: Single; const x, y: Single; pX, pY: PSingle);
begin
  pX^ := (x - x1) / (x2 - x1) * 2.0 - 1.0;
  pY^ := (y - y1) / (y2 - y1) * 2.0 - 1.0;
end;

procedure TgxCanvas.SwapSingle(pX, pY: PSingle);
var
  tmp: Single;
begin
  tmp := pX^;
  pX^ := pY^;
  pY^ := tmp;
end;

end.

