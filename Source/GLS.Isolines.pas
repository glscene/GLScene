//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.Isolines;

(* Class and routines to output isolines *)

interface

{$I GLScene.inc}

uses
  System.SysUtils, 
  System.Classes, 
  System.Math,
  System.Generics.Collections,

  GLS.VectorGeometry, 
  GLS.VectorLists, 
  GLS.Objects, 
  GLS.MultiPolygon,  
  GLS.Coordinates,
  GLS.VectorTypesExt,
  GLS.Color, 
  GLS.Spline, 
  GLS.SpaceText, 
  GLS.VectorTypes,
  GLS.VectorFileObjects;

type
  TVectorArr = array of Single;
  TByteVectorArr = array of Byte;

  TMatrixArr = array of array of Single;
  TByteMatrixArr = array of array of Byte;

  TVectorL4D = array [0 .. 4] of Single;
  TVectorL4I = array [0 .. 4] of Integer;
  TCastArray = array [0 .. 2, 0 .. 2, 0 .. 2] of Integer;

  TVertex2DArr = array [0 .. 32767] of TPoint2DRec;
  PVertex2DArr = ^TVertex2DArr;

  PGLIsoline = ^TGLIsoline;
  TGLIsoline = class (TObject)
    NP: Integer;
    Line: PVertex2DArr;
    constructor Create(LineSize: integer); virtual;
    destructor Destroy; override;
  end;

  TGLIsolineState = (ilsEmpty, ilsCalculating, ilsReady);

  TGLIsolines = class(TGLLines)
  public
    IsoVertex: TAffineVector;
    GLSpaceTextSF: array of TGLSpaceText;
    procedure MakeIsolines(var Depths: TMatrixArr; bmSize: Integer;
      StartDepth, EndDepth: Single; Interval: Integer);
    procedure FreeList;
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;

  (*
    CONREC is a contouring routine for rectangular spaced data or regular 2D grids
    It takes each rectangle of adjacent data points and splits it
    into 4 triangles after choosing the height at the centre of the rectangle.
    For each of the triangles the line segment resulting from the intersection
    with each contour plane.
    A routine is then called with the starting and stopping coordinates
    of the line segment that make up a contour curve and then output these
    isolines. See details in http://paulbourke.net/papers/conrec/

    The input parameters are as follows :
    PlaneSFindex -
    PlaneSF -
    Data -  Scalar field in 2D grid
    ilb - lower bound in west - east direction
    iub - upper bound in west - east direction
    jlb - lower bound in north - south direction
    jub upper bound in north - south direction
    X - coord. vector for west - east
    Y - coord. vector for north - south
    NC - number of cut levels
    HgtL - values of cut levels
    Z_Kfix -
    res3Dmin -
  *)
   procedure Conrec(PlaneSFindex:Integer; PlaneSF: TGLFreeForm; Data: TMatrixArr; ilb, iub, jlb, jub: Integer;
         X: TVectorArr; Y: TVectorArr; NC: Integer; HgtL: TVectorArr; Z_Kfix: Single; res3Dmax, res3Dmin: Single);
   private
     CoordRange: Integer;
     LineList: TList;
     IsolineState: TGLIsolineState;
  end;

procedure Initialize_Contouring(var DataGrid: TMatrixArr;
  NXpoints, NYpoints: integer; CurrentIsoline: Single);
procedure Release_Memory_Isoline;
function GetNextIsoline(var Isoline: TGLIsoline): Boolean;

// Defines contouring segments inside a triangle using elevations
procedure TriangleElevationSegments(const p1, p2, p3: TAffineVector;
  ElevationDelta: Single; Segments: TGLAffineVectorList);

//----------------------------------------------------------------------
implementation
//----------------------------------------------------------------------

var
  ii, jj: Integer;
  Visited: TByteMatrixArr; // 0 = not visited
  // 1 = visited
  // if it is a saddle points, then bits 1-4 also encode
  // which exit and entry points were used.
  Grid: TMatrixArr;
  NX, NY: Integer;
  LineX1, LineY1, LineX2, LineY2: TVectorArr;


function EqAdd(a, b: integer): integer;
begin
  if a = b then
    Result := 1
  else
    Result := 0;
end;

procedure Initialize_Contouring;

var
  i, j: Integer;
  Maxnp: Integer;
begin
  ii := 1;
  jj := 1;
  NX := NXpoints;
  NY := NYpoints;
  Maxnp := NX * NY div 256;
  SetLength(Visited, NX, NY);
  for i := 0 to NX - 1 do
    for j := 0 to NY - 1 do
      Visited[i, j] := 0;
  SetLength(Grid, NX + 1, NY + 1);
  SetLength(LineX1, Maxnp);
  SetLength(LineY1, Maxnp);
  SetLength(LineX2, Maxnp);
  SetLength(LineY2, Maxnp);
  // Generate a grid of data relative to the Isoline level
  for i := 1 to NX do
  begin
    for j := 1 to NY do
    begin
      Grid[i][j] := DataGrid[i - 1][j - 1] - CurrentIsoline;
      (* Don't want any grid points exactly zero *)
      if Grid[i][j] = 0 then
      begin
        Grid[i][j] := 1E-8;
      end;
    end;
  end;
end;

procedure Release_Memory_Isoline;
begin
  SetLength(Visited, 0);
  SetLength(Grid, 0);
  SetLength(LineX1, 0);
  SetLength(LineY1, 0);
  SetLength(LineX2, 0);
  SetLength(LineY2, 0);
end;

procedure Cuts(const g: TMatrixArr; i, j: Integer; var s: array of Integer);
begin
  s[0] := 0;
  if g[i][j + 1] * g[i + 1][j + 1] < 0 then
  begin
    Inc(s[0]);
    s[s[0]] := 1;
  end;
  if g[i + 1][j + 1] * g[i + 1][j] < 0 then
  begin
    Inc(s[0]);
    s[s[0]] := 2;
  end;
  if g[i + 1][j] * g[i][j] < 0 then
  begin
    Inc(s[0]);
    s[s[0]] := 3;
  end;
  if g[i][j] * g[i][j + 1] < 0 then
  begin
    Inc(s[0]);
    s[s[0]] := 4;
  end;
end;

procedure Intercept(const g: TMatrixArr; i, j, s: Integer; var x, y: Single);
begin
  case s of
    1:
      begin
        x := Abs(g[i][j + 1] / (g[i + 1][j + 1] - g[i][j + 1])) + i;
        y := 1 + j;
      end;
    2:
      begin
        y := Abs(g[i + 1][j] / (g[i + 1][j + 1] - g[i + 1][j])) + j;
        x := 1 + i;
      end;
    3:
      begin
        x := Abs(g[i][j] / (g[i + 1][j] - g[i][j])) + i;
        y := j;
      end;
    4:
      begin
        y := Abs(g[i][j] / (g[i][j + 1] - g[i][j])) + j;
        x := i;
      end;
  end;
end;

function Free_Exit(const Visited: TByteMatrixArr;
  i, j, NX, NY, Lexit: Integer): Boolean;
var
  ni, nj: Integer;
  Entry: Integer;
begin
  nj := j + EqAdd(Lexit, 1) - EqAdd(Lexit, 3);
  ni := i + EqAdd(Lexit, 2) - EqAdd(Lexit, 4);
  if (ni < 1) or (ni >= NX) or (nj < 1) or (nj >= NY) or (Visited[ni][nj] = 0)
  then
    Result := True // can always exit on an edge
  else
  begin
    Entry := ((Lexit + 1) mod 4) + 1;
    Result := (((Visited[ni][nj] shr Entry) and 1) = 0);
  end;
end;

procedure TraceIsoline(i, j, Lexit, NX, NY: Integer; const Grid: TMatrixArr;
  const Visited: TByteMatrixArr; var LineX, LineY: TVectorArr;
  var NP: Integer; var OffGrid: Boolean);
var
  ni, nj, si, sj: Integer;
  p, q: Integer;
  s: array [0 .. 5] of Integer;
  Entry: integer;

begin
  ni := i;
  nj := j;
  si := i;
  sj := j;
  NP := 0;
  offgrid := False;
  Visited[i][j] := 1;
  Intercept(Grid, i, j, Lexit, LineX[NP], LineY[NP]);
  NP := 1;
  while True do
  begin
    nj := nj + EqAdd(lexit, 1) - EqAdd(lexit, 3);
    ni := ni + EqAdd(lexit, 2) - EqAdd(lexit, 4);
    if (ni < 1) or (ni > NX - 1) or (nj < 1) or (nj > NY - 1) then
    begin
      offgrid := True;
      break;
    end;
    Visited[ni][nj] := 1;
    entry := ((lexit + 1) mod 4) + 1;
    Cuts(Grid, ni, nj, s);
    // Have come to a new point on the Isoline
    Lexit := 0;
    if (s[0] = 2) then
    begin
      // If there are two cuts then choose the one that is not the entry
      if entry = s[1] then
        lexit := s[2]
      else
        lexit := s[1];
    end
    else
    begin
      // If there are four cuts (saddle) then work round from the left
      p := (entry mod 4) + 1;
      while p <> entry do
      begin
        for q := 1 to s[0] do
        begin
          if (s[q] = p) and Free_Exit(Visited, NX, NY, ni, nj, p) then
          begin
            lexit := p;
            break;
          end;
        end;
        // Aim is to find first
        if lexit <> 0 then
          break;
        p := (p mod 4) + 1;
      end;
      (* exit from cell, going *)
      // We found a way out, make a note of way in and way out.
      // Need to do this as saddle may be visited twice.
      Visited[ni][nj] := (Visited[ni][nj]) or (1 shl entry) or (1 shl lexit);
    end;
    // clockwise from entry point
    Assert(lexit > 0, 'Contour routine caught in a loop');
    if (lexit = 0) then
      break;
    Intercept(Grid, ni, nj, lexit, LineX[NP], LineY[NP]);
    Inc(NP);
    if (ni = si) and (nj = sj) then
      break;
  end;
  // Have finished loop
end;

(* LineX and LineY are (pointers to) zero-offset vectors, to which
  sufficient space has been allocated to store the coordinates of
  any feasible Isoline *)
function GetNextIsoline(var Isoline: TGLIsoline): Boolean;
var
  OffGrid: boolean;
  Lexit: integer;
  np1, np2: integer;
  i, j, k: integer;
  s: array [0 .. 4] of integer;
begin
  for i := ii to NX - 1 do
  begin
    for j := 1 + (jj - 1) * EqAdd(i, ii) to NY - 1 do
    begin
      if (Visited[i][j] = 0) then
      begin
        Cuts(Grid, i, j, s);
        if s[0] = 2 then
        begin
          Lexit := s[2];
          TraceIsoline(i, j, lexit, NX, NY, Grid, Visited, LineX1, LineY1,
            np1, offgrid);
          // Follow the Isoline along
          if offgrid then
          begin
            // Go back to start of Isoline and trace in opposite direction
            Lexit := s[1];
            TraceIsoline(i, j, Lexit, NX, NY, Grid, Visited, LineX2, LineY2,
              np2, offgrid);
            // Copy both bits of line into Isoline
            Isoline := TGLIsoline.Create(np1 + np2);
            for k := 0 to np2 - 1 do
            begin
              Isoline.Line^[k].x := LineX2[np2 - k - 1];
              Isoline.Line^[k].y := LineY2[np2 - k - 1];
            end;
            for k := 0 to np1 - 1 do
            begin
              Isoline.Line^[k + np2].x := LineX1[k];
              Isoline.Line^[k + np2].y := LineY1[k];
            end;
          end
          else
          begin
            // Just copy the single Isoline loop into LineX & LineY
            Isoline := TGLIsoline.Create(np1);
            for k := 0 to np1 - 1 do
            begin
              Isoline.Line^[k].x := LineX1[k];
              Isoline.Line^[k].y := LineY1[k];
            end;
          end;
          // scale Isoline into true units
          { for k:=1 to np do
            begin
            LineX[k-1]:= xlo+(LineX[k]-1)*(xhi-xlo) / (nx-1);
            LineY[k-1]:= ylo+(LineY[k]-1)*(yhi-ylo) / (ny-1);
            // LineX and LineY are zero offset vectors
            end; }
          ii := i;
          jj := j;
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
  Result := False;
end;

procedure TriangleElevationSegments(const p1, p2, p3: TAffineVector;
  ElevationDelta: Single; Segments: TGLAffineVectorList);

  function SegmentIntersect(const a, b: TAffineVector; e: Single): Integer;
  var
    f: Single;
  begin
    if a.Z < b.Z then
    begin
      if (e >= a.Z) and (e < b.Z) then
      begin
        f := (e - a.Z) / (b.Z - a.Z);
        Segments.Add(VectorLerp(a, b, f));
        Result := 1;
      end
      else
        Result := 0;
    end
    else if a.Z > b.Z then
    begin
      if (e > b.Z) and (e <= a.Z) then
      begin
        f := (e - b.Z) / (a.Z - b.Z);
        Segments.Add(VectorLerp(b, a, f));
        Result := 1;
      end
      else
        Result := 0;
    end
    else
      Result := 0;
  end;

var
  i, n, LowElev, HighElev: Integer;
  e: Single;

begin
  LowElev := Round(MinFloat(p1.Z, p2.Z, p3.Z) / ElevationDelta - 0.5);
  HighElev := Round(MaxFloat(p1.Z, p2.Z, p3.Z) / ElevationDelta + 0.5);
  for i := LowElev to HighElev do
  begin
    e := i * ElevationDelta + 0.1;
    // add a real offset - this avoids all the special cases
    n := SegmentIntersect(p1, p2, e);
    if n < 2 then
      n := n + SegmentIntersect(p2, p3, e);
    if n < 2 then
      n := n + SegmentIntersect(p3, p1, e);
    Assert((n = 2) or (n = 0));
  end;
end;

constructor TGLIsolines.Create(AOwner: TComponent);
begin
  LineList := TList.Create;
  IsolineState := ilsEmpty;
  Nodes.Create(Self);
end;

destructor TGLIsolines.Destroy;
begin
  FreeList;
  Nodes.Free;
  inherited;
end;

procedure TGLIsolines.FreeList;
var
  i: integer;
begin
 if LineList<>nil then
  begin
    for i := LineList.Count - 1 downto 0 do
    begin
      TGLIsoline(LineList.Items[i]).Free;
    end;
    LineList.Clear;
    IsolineState := ilsEmpty;
  end;
end;

procedure TGLIsolines.MakeIsolines(var Depths: TMatrixArr; bmSize: integer;
  StartDepth, EndDepth: Single; Interval: Integer);
var
  Isoline: TGLIsoline;

begin
  IsolineState := ilsCalculating;
  CoordRange := bmSize;
  FreeList;
  repeat
    Initialize_Contouring(Depths, bmSize, bmSize, StartDepth);
    while GetNextIsoline(Isoline) do
    begin
      LineList.Add(Isoline);
    end;
    Release_Memory_Isoline;
    StartDepth := StartDepth + Interval;
  until StartDepth > EndDepth;
  IsolineState := ilsReady;
end;

constructor TGLIsoline.Create(LineSize: Integer);
begin
  inherited Create;
  NP := LineSize;
  Getmem(Line, NP * 2 * Sizeof(Single));
end;

destructor TGLIsoline.Destroy;
begin
  inherited;
  if Assigned(Line) then
    Freemem(Line);
  NP := 0;
end;

procedure TGLIsolines.Conrec(PlaneSFindex:Integer;PlaneSF:TGLfreeForm; Data: TMatrixArr; ilb, iub, jlb, jub: Integer;
  X: TVectorArr; Y: TVectorArr;  NC: Integer; HgtL: TVectorArr;
  Z_Kfix: Single; res3Dmax,res3Dmin: Single);
// ------------------------------------------------------------------------------
const
  im: array [0 .. 3] of Integer = (0, 1, 1, 0); // coord. cast array west - east
  jm: array [0 .. 3] of Integer = (0, 0, 1, 1);
  // coord. cast array north - south
  // ------------------------------------------------------------------------------
var
  m1, m2, m3, Deside: Integer;
  dmin, dmax, x1, x2, y1, y2: Single;
  minY1, maxY1, minX1, maxX1, ScaleFont, ActualValue: Single;
  I, J, K, lcnt, m: Integer;
  CastTab: TCastArray;
  h: TVectorL4D;
  sh: TVectorL4I;
  xh, yh: TVectorL4D;
  temp1, temp2: Single;
  IUniqueList: TList<Single>;

  // ------- service xsec west east lin. interpol -------------------------------
  function Xsec(p1, p2: Integer): Single;
  begin
    Result := (h[p2] * xh[p1] - h[p1] * xh[p2]) / (h[p2] - h[p1]);
  end;

// ------- service ysec north south lin interpol -------------------------------
  function Ysec(p1, p2: Integer): Single;
  begin
    Result := (h[p2] * yh[p1] - h[p1] * yh[p2]) / (h[p2] - h[p1]);
  end;

begin
 SetLength(GLSpaceTextSF, NC-1);
 IUniqueList := TList<Single>.Create;
 ScaleFont:= 0.025 * MaxValue(Y);      // 050515

  // set casting array
  CastTab[0, 0, 0] := 0;
  CastTab[0, 0, 1] := 0;
  CastTab[0, 0, 2] := 8;
  CastTab[0, 1, 0] := 0;
  CastTab[0, 1, 1] := 2;
  CastTab[0, 1, 2] := 5;
  CastTab[0, 2, 0] := 7;
  CastTab[0, 2, 1] := 6;
  CastTab[0, 2, 2] := 9;

  CastTab[1, 0, 0] := 0;
  CastTab[1, 0, 1] := 3;
  CastTab[1, 0, 2] := 4;
  CastTab[1, 1, 0] := 1;
  CastTab[1, 1, 1] := 3;
  CastTab[1, 1, 2] := 1;
  CastTab[1, 2, 0] := 4;
  CastTab[1, 2, 1] := 3;
  CastTab[1, 2, 2] := 0;

  CastTab[2, 0, 0] := 9;
  CastTab[2, 0, 1] := 6;
  CastTab[2, 0, 2] := 7;
  CastTab[2, 1, 0] := 5;
  CastTab[2, 1, 1] := 2;
  CastTab[2, 1, 2] := 0;
  CastTab[2, 2, 0] := 8;
  CastTab[2, 2, 1] := 0;
  CastTab[2, 2, 2] := 0;

  // set line counter
  lcnt := 0;
  // ------- Create the level curves ----------------------------------------------
  for j := jub - 1 downto jlb do
  begin // over all north - south and              +for j
    for i := ilb to iub - 1 do
    begin // east - west coordinates of datafield    +for i
          // set casting bounds from array
      temp1 := Min(Data[i, j], Data[i, j + 1]);
      temp2 := Min(Data[i + 1, j], Data[i + 1, j + 1]);
      dmin := Min(temp1, temp2);
      temp1 := Max(Data[i, j], Data[i, j + 1]);
      temp2 := Max(Data[i + 1, j], Data[i + 1, j + 1]);
      dmax := Max(temp1, temp2);
      if (dmax >= HgtL[0]) and (dmin <= HgtL[nc - 1]) then
      begin // ask horizontal cut available ----  +If dmin && dmax in z[0] .. z[nc-1]

        for k := 0 to NC - 1 do
        begin // over all possible cuts ---- +for k
          if (HgtL[k] > dmin) and (HgtL[k] <= dmax) then
          begin // ask for cut interval ----- +if z[k] in dmin .. dmax
            // -----------------------------------------------------------------------
            for m := 4 downto 0 do
            begin // deteriening the cut casts and set the ---- +for m
              if (m > 0) then
                  begin // height and coordinate vectors
                    h[m] := Data[i + im[m - 1], j + jm[m - 1]] - HgtL[k];
                    xh[m] := X[i + im[m - 1]];
                    yh[m] := Y[j + jm[m - 1]];
                  end
              else
                  begin
                    h[0] := (h[1] + h[2] + h[3] + h[4]) / 4;
                    xh[0] := (X[i] + X[i + 1]) / 2;
                    yh[0] := (Y[j] + Y[j + 1]) / 2;
                  end; // if m>0 then else
              if h[m] > 0 then      
			    sh[m] := 1
              else If h[m] < 0 then 
			    sh[m] := -1
              else
                sh[m] := 0;
            end; // --- -for m

            // -----------------------------------------------------------
            for m := 1 to 4 do
            begin // set directional CastTable
              //
              // Note: at this stage the relative heights of the corners and the
              // centre are in the h array, and the corresponding coordinates are
              // in the xh and yh arrays. The centre of the box is indexed by 0
              // and the 4 corners by 1 to 4 as shown below.
              // Each triangle is then indexed by the parameter m, and the 3
              // vertices of each triangle are indexed by parameters m1,m2,and
              // m3.
              // It is assumed that the centre of the box is always vertex 2
              // though this isimportant only when all 3 vertices lie exactly on
              // the same contour level, in which case only the side of the box
              // is drawn.
              //
              // AS ANY BODY NOWS IST FROM THE ORIGINAL
              //
              // vertex 4 +-------------------+ vertex 3
              // | \               / |
              // |   \    m-3    /   |
              // |     \       /     |
              // |       \   /       |
              // |  m=2    X   m=2   |       the centre is vertex 0
              // |       /   \       |
              // |     /       \     |
              // |   /    m=1    \   |
              // | /               \ |
              // vertex 1 +-------------------+ vertex 2
              //
              //
              // Scan each triangle in the box
              //
              m1 := m;
              m2 := 0;
              if not(m = 4) then
                m3 := m + 1
              else
                m3 := 1;
              Deside := CastTab[sh[m1] + 1, sh[m2] + 1, sh[m3] + 1];
              if not(Deside = 0) then
              begin // ask if there a decision available ---+if if not(Deside=0)
                case Deside of
                // ---- determine by desided cast cuts ---- +Case deside;
                  1:
                    begin
                      x1 := xh[m1];
                      y1 := yh[m1];
                      x2 := xh[m2];
                      y2 := yh[m2];
                    end;
                  2:
                    begin
                      x1 := xh[m2];
                      y1 := yh[m2];
                      x2 := xh[m3];
                      y2 := yh[m3];
                    end;
                  3:
                    begin
                      x1 := xh[m3];
                      y1 := yh[m3];
                      x2 := xh[m1];
                      y2 := yh[m1];
                    end;
                  4:
                    begin
                      x1 := xh[m1];
                      y1 := yh[m1];
                      x2 := xsec(m2, m3);
                      y2 := ysec(m2, m3);
                    end;
                  5:
                    begin
                      x1 := xh[m2];
                      y1 := yh[m2];
                      x2 := xsec(m3, m1);
                      y2 := ysec(m3, m1);
                    end;
                  6:
                    begin
                      x1 := xh[m3];
                      y1 := yh[m3];
                      x2 := Xsec(m1, m2);
                      y2 := Ysec(m1, m2);
                    end;
                  7:
                    begin
                      x1 := Xsec(m1, m2);
                      y1 := Ysec(m1, m2);
                      x2 := Xsec(m2, m3);
                      y2 := Ysec(m2, m3);
                    end;
                  8:
                    begin
                      x1 := Xsec(m2, m3);
                      y1 := Ysec(m2, m3);
                      x2 := Xsec(m3, m1);
                      y2 := Ysec(m3, m1);
                    end;
                  9:
                    begin
                      x1 := Xsec(m3, m1);
                      y1 := Ysec(m3, m1);
                      x2 := Xsec(m1, m2);
                      y2 := Ysec(m1, m2);
                    end;
                end; // ---  -Case deside;

                // -------Output results ---------------------
                case PlaneSFindex of              // suggestion3Planes
                   0:  begin
                        Nodes.AddNode(x1, y1, Z_kfix);
                        Nodes.AddNode(x2, y2, Z_kfix);
                       end ;
                   1: begin
                        Nodes.AddNode(Z_kfix,x1, y1);
                        Nodes.AddNode(Z_kfix,x2, y2);
                       end ;
                   2: begin
                        Nodes.AddNode(y1, Z_kfix, x1);
                        Nodes.AddNode(y2, Z_kfix, x2);
                       end ;
                end;
                if ODD(K) then
                   begin
                     MinY1:= 0.1*MaxValue(Y) ; MaxY1:= 0.6*MaxValue(Y);
                     MinX1:= 0.2*MaxValue(X) ; MaxX1:= 0.4*MaxValue(X);
                   end
                else
                   begin
                     MinY1:= 0.55*MaxValue(Y) ; MaxY1:= 0.9*MaxValue(Y);
                     MinX1:= 0.3*MaxValue(X) ; MaxX1:= 0.7*MaxValue(X);
                   end ;

                 if (not IUniqueList.Contains(HgtL[K])) and
                    ( (y1<MaxY1) and (y1>MinY1) and
                      (x1<MaxX1) and (x1>MinX1)) then
                   begin
                     GlSpaceTextSF[K].Free;
                     GlSpaceTextSF[K]:= TGlspacetext.CreateAsChild(self);

                     with GlspaceTextSF[K] do
                     begin
                       Scale.AsVector := VectorMake(scaleFont, scaleFont, scaleFont);
                       Material.FrontProperties.Emission.Color :=  clryellow;
                       Material.FrontProperties.Ambient.SetColor(1, 1, 1, 1);

                       ActualValue:= HgtL[K]* (res3Dmax - res3Dmin) +  res3Dmin;
                       Extrusion:= 0.5;
                       Text:= FloatToStrF(ActualValue, ffFixed, 4, 0)  ;

                       case PlaneSFindex of              // suggestion3Planes
                         0:  begin
                             Position.AsVector :=  VectorMake(x1,0.99*y1,1.01*Z_kfix);
                             Direction.AsVector :=  VectorMake(0,0,1);
                             end ;
                         1: begin
                              Position.AsVector :=  VectorMake(1.01*Z_kfix,x1,0.99*y1);
                              Direction.AsVector :=  VectorMake(1,0,0);
                             end ;
                         2: begin
                             Position.AsVector :=  VectorMake(y1,1.01*Z_kfix,0.99*x1);
                              Direction.AsVector :=  VectorMake(0,1,0);
                             end ;
                       end;
                       StructureChanged;
                     end;
                     IUniqueList.Add(HgtL[k]);
                   end;
                // ---------------------------------------------------------
              end; // ----  -if Not(deside=0)
            end; // ----  -for m
          end; // ----  -if z[k] in dmin .. dmax
        end; // ----  -for k
      end; // ----  -if dmin && dmax in z[0] .. z[nc-1]
    end; // ----  -for i
  end; // ----  -for j
end;
// ------ End of ----------------------------------------------------------------

end.


