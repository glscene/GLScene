unit GXS.SailSurface;
(*
  TgxSailSurface - Sail surface 3d object by oMAR
*)
interface

uses
  System.SysUtils,
  System.Classes,
  System.Math.Vectors,
  System.Threading,
  System.Types,
  System.Generics.Collections,
  System.RTLConsts,
  FMX.Types3D,
  FMX.Types,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.MaterialSources;

type
  TSailShape = (ShapeMain, ShapeSimetric, ShapeJib); // sail plans

  (* --------------------------------------------------------------------------
   Sail construction params

   ^ h                                    ^ h                           ^ h
   |      Main                            |        Simetric             |     Jib
   Ht  +----+                          Ht   +---+ Lt                    Ht  +---+
   | Lt  \                             /  |  \                          |Lt  \
   |      \                           /   |   \                         |     \
   |       \                         /    |    \                        |      \
   |        \                       /     |     \                       |       \
   |         \                     /      |      \                      |        \
   |          \                   /       |       \                     |         \
   +-----------+                 +-----------------+                    +----------+
   |    Lm      \               /         | Lm      \                   |    Lm     \
   |            |               |         |         |                   |            \
   |             \              |         |         |                   |    Lx       \
   |             |              \         |         /               Hx  +--------------+  <-- h of max chord
   |              \              |        |        |                    |         ____/
   0 |      Lb      |              |      0 | Lb     |                  0 |    ____/
   ----+--------------+--> L        -+--------+--------+--> L           ----+___/-------------> L
   Lb
   nh subdivisions in height
   nl subdivisions in chord
   ---------------------------------------------------------------------------
  *)

  TPointF_Array = array of TPointF;

  TSailParams = record // Sail w/ quadratic leech
    Lb: Single; // sail chord at the bottom
    Lt: Single; // chord at the top
    Lm: Single; // chord at the middle   ( for a quadratic leech )
    Ht: Single; // sail height
    // for Jib Shape only
    Hx: Single; // height of Jib max chord
    Lx: Single; // Jib max chord

    // NsubH:integer;   // subdivisions in height       default = 16hx8w
    // NsubL:integer;  // subdivisions in chord

    // chord at a certain height ( H between 0 and Ht )
    function GetSailChord(aType: TSailShape; const H: Single): Single;
  end;

  (*
    TgxSailSurface morphs a TPlane grid into a triangular sail with camber
    ( or any other polygonal Shape)
  *)
  TgxSailSurface = class(TPlane)
  private
    fSailShape: TSailShape;
    fVersion: integer;
    function GetChordBottom: Single;
    function GetChordMid: Single;
    function GetChordTop: Single;
    function GetSailHeight: Single;
    procedure SetChordBottom(const Value: Single);
    procedure SetChordMid(const Value: Single);
    procedure SetChordTop(const Value: Single);
    procedure SetSailHeight(const Value: Single);
    procedure SetSailShape(const Value: TSailShape);
    procedure CalcSailSurfaceMesh;
    function GetChordX: Single;
    function GetHeightX: Single;
    procedure SetChordX(const Value: Single);
    procedure SetHeightX(const Value: Single);
    procedure setVersion(const Value: integer);
  protected
    fTime: Single; // Om: movd stuff to protected
    fNbMesh: integer; // number of tiles in the mesh
    fSailParams: TSailParams;
    fShowlines, fUseTasks: boolean;
    fMaterialLignes: TColorMaterialSource;
    fCenter: TPoint3D;
    fCamberRight: boolean; // true = sail camber to the right
    procedure SetDepth(const Value: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    // Property    Data;  //om: publica
    function Altura(P: TPoint3D): Single; // Om: calc wave amplitude on a point
    procedure SetSailParams(aSailParams: TSailParams);
    procedure SetMeshWith2Dline(aPtArray: TPointF_Array);
  published
    property ShowLines: boolean read fShowlines write fShowlines;
    property UseTasks: boolean read fUseTasks write fUseTasks;
    property MaterialLines: TColorMaterialSource read fMaterialLignes
      write fMaterialLignes;
    property SailShape: TSailShape read fSailShape write SetSailShape
      default ShapeMain; // main sail, jib or simetric shape
    property CamberRight: boolean read fCamberRight write fCamberRight;
    property SailHeight: Single read GetSailHeight write SetSailHeight;
    property ChordTop: Single read GetChordTop write SetChordTop;
    property ChordMid: Single read GetChordMid write SetChordMid;
    property ChordBottom: Single read GetChordBottom write SetChordBottom;
    // Jib style only
    property HeightX: Single read GetHeightX write SetHeightX;
    property ChordX: Single read GetChordX write SetChordX;
    property version: integer read fVersion write setVersion;
  end;

  // Quadratic interpolation with 3 points
function CalcQuadraticInterpolation(const x0, y0, x1, y1, x2, y2: Single;
  const x: Single; { out: } var y: Single): boolean;

procedure Register;

implementation // ------------------------------------------------------------

// quadratic interpolation w/ 3 points
// from: https://slideplayer.com/slide/4897028/   search Quadratic interpolation slide
//
// y |     --*--
// |    /  1  \
// |   *0      *2
// |  /         \   x
// +------------------>
//
// y(x) = b0+b1(x-x0)+b2(x-x0)(x-x1)
// b0 =  y0
// b1 = (y1-y0)/(x1-x0)
// b2 = ((y2-y1)/(x2-x1)-(y1-y0)/(x1-x0)) / (x2-x0)

function CalcQuadraticInterpolation(const x0, y0, x1, y1, x2, y2: Single;
  const x: Single; var y: Single): boolean;
var
  b0, b1, b2, dx10, dx21, dx20: Single;
begin
  dx10 := (x1 - x0);
  dx21 := (x2 - x1);
  dx20 := (x2 - x0);
  Result := (dx10 <> 0) and (dx21 <> 0) and (dx20 <> 0);
  // sanity test. Cannot calc if two of the points have same x
  if Result then
  begin
    b0 := y0;
    b1 := (y1 - y0) / dx10;
    b2 := ((y2 - y1) / dx21 - (y1 - y0) / dx10) / dx20;
    y := b0 + b1 * (x - x0) + b2 * (x - x0) * (x - x1); // return y
  end;
end;

{ TSailParams }

// calc chord at a certain height, using a quadratic leech
function TSailParams.GetSailChord(aType: TSailShape; const H: Single): Single;
// in m
var
  y: Single;
begin
  Result := 0;
  if (Ht <= 0) or (H < 0) or (H > Ht) then
    exit; // sanity test

  case aType of
    ShapeMain:
      begin
        // was Result := Lb-(Lb-Lt)*H/Ht           // was:  linear leech from bot to top
        // quadratic interpolation of L in H using points Lt,Lm,Lb    ( top, mid, bottom chords)
        if CalcQuadraticInterpolation( { 0: } 0, Lb, { 1: } Ht / 2, Lm,
          { 2: } Ht, Lt, { H: } H, { out: } y) then
          Result := y // quadratic leech
        else
          Result := 0;
      end;
    ShapeJib:
      begin // 'jibs' have max sail width at a certain height 'x'
        if (H < Hx) then
          Result := Lb - (Lb - Lx) * H / Hx // below Hx  linear from Lb to Lx
        else
        begin
          if CalcQuadraticInterpolation( { 0: } Hx, Lx, { 1: } Ht / 2, Lm,
            { 2: } Ht, Lt, { H: } H, { out: } y) then
            Result := y // quadratic leech
          else
            Result := 0;
        end;
      end;
    ShapeSimetric:
      begin
        // quadratic interpolation of L in H using points Lt,Lm,Lb    ( top, mid, bottom chords)
        if CalcQuadraticInterpolation( { 0: } 0, Lb, { 1: } Ht / 2, Lm,
          { {2: } Ht, Lt, { H: } H, { out: } y) then
          Result := y // quadratic leech
        else
          Result := 0;
      end;
  else
    Result := 0; // wtf ??
  end;
end;

// TgxSailSurface

constructor TgxSailSurface.Create(AOwner: TComponent);
begin
  inherited;

  fSailShape := ShapeMain; // default sail type = main sail
  // set default sail params ( see diagrams on the top )
  // chords
  fSailParams.Lt := 0.8; // Lt:sail chord at the top
  fSailParams.Lm := 2.65; // Lm chord at mid (0.8+3.5)/2+0.5=2.65
  fSailParams.Lb := 3.5; // Lb:sail chord at the bottom
  // sail height
  fSailParams.Ht := 10.0; // Ht:sail height
  // params for Jibs only
  fSailParams.Hx := 1.0;
  fSailParams.Lx := 3.5;

  // number of subds more or less to have square sail "tiles"
  // fSailParams.NsubH := 10;           // 1x1 aspect sail
  // fSailParams.NsubL := 10;

  // set plane subdivisions
  // self.Width  := fSailParams.Lb;     // dont mess with original size
  // self.Height := fSailParams.Ht;
  // self.Depth  := 5; //??

  // h=10 w=16 comes from OPYC simulation
  // actually OPYC sails have varied number of segments, according to sail size ( from 8 jib to 16 spinaker )
  self.SubdivisionsHeight := 10; // set plane subdivisions
  self.SubdivisionsWidth := 16;

  fNbMesh := (SubdivisionsWidth + 1) * (SubdivisionsHeight + 1);
  // mesh number of vertices
  // what fCenter center means is a question
  // = NSubDiv/Width  --> unit = subdiv/m
  fCenter := Point3D(SubdivisionsWidth / self.Width,
    SubdivisionsHeight / self.Height, 0);

  fCamberRight := true; // side of the fake sail canvas

  fUseTasks := true; // default= using tasks
  fVersion := 1;
end;

destructor TgxSailSurface.Destroy;
begin
  inherited;
end;

function TgxSailSurface.GetChordBottom: Single;
begin
  Result := fSailParams.Lb;
end;

function TgxSailSurface.GetChordMid: Single;
begin
  Result := fSailParams.Lm;
end;

function TgxSailSurface.GetChordTop: Single;
begin
  Result := fSailParams.Lt;
end;

function TgxSailSurface.GetChordX: Single;
begin
  Result := fSailParams.Lx;
end;

function TgxSailSurface.GetHeightX: Single;
begin
  Result := fSailParams.Hx;
end;

function TgxSailSurface.GetSailHeight: Single;
begin
  Result := fSailParams.ht;
end;

procedure TgxSailSurface.SetDepth(const Value: Single);
// override TPlane tendency to set Depth to 0.01
begin
  if (self.fDepth <> Value) then // this copies what TPlane removed
  begin
    self.fDepth := Value;
    Resize3D;
    if (fDepth < 0) and (csDesigning in ComponentState) then
    begin
      fDepth := abs(fDepth);
      FScale.Z := -FScale.Z;
    end;
    if not(csLoading in ComponentState) then
      Repaint;
  end;
end;

// Calc sail surface mesh based on a line of points in 2D (planta)

Procedure TgxSailSurface.SetMeshWith2Dline(aPtArray: TPointF_Array);
var
  M: TMeshData;
  x, y, np: integer;
  somme: Single;
  front, back: PPoint3D;
  h, hinM, Dh, chord, chordFrac, L, Z, maxChord, sh2, sw2, ax, ay,
    aSailWidth: Single;
  aPt: PPointF;

begin
  // sail params sanity test
  if (SubdivisionsHeight <= 0) or (SubdivisionsWidth <= 0) then
    exit; // invalid subdiv values
  if Width = 0 then
    exit;

  M := self.Data; // use default TPlane mesh
  fNbMesh := (SubdivisionsWidth + 1) * (SubdivisionsHeight + 1);
  // recalc mesh number of vertices

  // mesh is calculated to fit into  [-0.5,-0.5..0.5,0.5] interval.    Actual sail dimensions are set with objects Width,Height,Depth

  if (fSailParams.Lm > fSailParams.Lb) then
    aSailWidth := fSailParams.Lm // get max width
  else
    aSailWidth := fSailParams.Lb;

  sh2 := fSailParams.Ht / 2;
  sw2 := aSailWidth / 2;

  h := -0.5; // start at the foot
  Dh := 1.0 / SubdivisionsHeight; // was Dh := fSailParams.Ht/fSailParams.NsubH;
  np := Length(aPtArray); // number of pts received
  // set maxChord ( max sail width )
  case fSailShape of
    ShapeMain:
      maxChord := fSailParams.Lb;
    ShapeJib:
      maxChord := fSailParams.Lx;
    ShapeSimetric:
      maxChord := fSailParams.Lm; // or Lb?
  else
    maxChord := 1;
  end;

  for y := 0 to SubdivisionsHeight do
  begin
    // calc DL
    hinM := (h + 0.5) * fSailParams.Ht; // h in m
    chord := fSailParams.GetSailChord(fSailShape, hinM);
    // sail chord for h in m
    // if (chord<=0) then continue;             //??

    chordFrac := chord / maxChord; // frac of maxChord in m

    // sail simetry is controlled by where the mesh line starts
    case fSailShape of // x of sail mesh start
      ShapeMain, ShapeJib:
        L := 0;
      ShapeSimetric:
        L := -chord / 2; // 0 is the sail middle. Start line at x=0-chord/2
    else
      L := -0.5; // wtf ??
    end;

    for x := 0 to SubdivisionsWidth do
    begin
      front := M.VertexBuffer.VerticesPtr[x + (y * (SubdivisionsWidth + 1))];
      back := M.VertexBuffer.VerticesPtr
        [fNbMesh + x + (y * (SubdivisionsWidth + 1))];

      if (x < np) then
        aPt := @aPtArray[x]
        // np-1 must be = to SubdivisionsWidth ( remesh if necessary )
      else
        aPt := @aPtArray[np - 1];

      ax := L - aPt^.y * chordFrac; // x <--> y scale conversion
      ay := L - aPt^.x * chordFrac; // y negative means sail pointing backw

      front^.x := ax; // x,y of the mesh always in -0.5,-0.5 .. 0.5,0.5
      front^.y := h;
      front^.Z := ay; // x,y of the mesh always in -0.5,-0.5 .. 0.5,0.5

      back^.x := ax;
      back^.y := h;
      back^.Z := ay;
    end;
    h := h + Dh; // inc h
  end;

  M.CalcTangentBinormals;
end;

procedure TgxSailSurface.CalcSailSurfaceMesh; // create default sail mesh
var
  M: TMeshData;
  x, y: integer;
  somme: Single;
  front, back: PPoint3D;
  h, hinM, L, Dh, DL, chord, chordFrac, maxChord, Z, ah, al: Single;
  aSailHeight, aSailWidth, sh2, sw2: Single;
begin
  // sail params sanity test
  if (SubdivisionsHeight <= 0) or (SubdivisionsWidth <= 0) then
    exit; // invalid subdiv values

  M := self.Data; // use default TPlane mesh
  fNbMesh := (SubdivisionsWidth + 1) * (SubdivisionsHeight + 1);
  // recalc mesh number of vertices

  // mesh is calculated to fit into  [-0.5,-0.5..0.5,0.5] interval.    Actual sail dimensions are set with objects Width,Height,Depth

  aSailHeight := fSailParams.Ht; // Height = H top

  // set maxChord ( max sail width ) and SailWidth
  case fSailShape of
    ShapeMain:
      begin
        maxChord := fSailParams.Lb;
        aSailWidth := fSailParams.Lb;
      end;
    ShapeJib:
      begin
        maxChord := fSailParams.Lx;
        aSailWidth := fSailParams.Lx;
      end;
    ShapeSimetric:
      begin
        maxChord := fSailParams.Lb;
        aSailWidth := fSailParams.Lb;
      end; // or Lb?
  else
    maxChord := 1;
    aSailWidth := 1;
  end;

  if (fSailParams.Lm > maxChord) then
  begin
    maxChord := fSailParams.Lm;
    aSailWidth := maxChord;
  end;

  sh2 := aSailHeight / 2;
  sw2 := aSailWidth / 2;

  h := -0.5; // start at the foot
  Dh := 1.0 / SubdivisionsHeight;
  // was Dh := fSailParams.ht/fSailParams.NsubH;  // subd h increment
  // this will create a mesh in h range -0.5 .. 0.5

  for y := 0 to SubdivisionsHeight do
  begin
    // calc DL
    hinM := (h + 0.5) * aSailHeight; // hinM = h in meters
    chord := fSailParams.GetSailChord(fSailShape, hinM);
    // sail chord for h (  in 0..1 range )
    chordFrac := chord / maxChord;
    if (chord <= 0) then
      continue; // ??
    DL := 0.5 * chordFrac / SubdivisionsWidth;
    // subd horizontal chord increment

    // sail simetry is controlled by where the mesh line starts
    case fSailShape of // x of sail mesh start
      ShapeMain, ShapeJib:
        L := 0;
      ShapeSimetric:
        L := -chord / 2; // 0 is the sail middle. Start line at x=0-chord/2
    else
      L := 0; // wtf ??
    end;

    for x := 0 to SubdivisionsWidth do
    begin
      front := M.VertexBuffer.VerticesPtr[x + (y * (SubdivisionsWidth + 1))];
      back := M.VertexBuffer.VerticesPtr
        [fNbMesh + x + (y * (SubdivisionsWidth + 1))];

      al := L;
      ah := h;

      front^.x := al;
      front^.y := ah;

      back^.x := al;
      back^.y := ah;

      // add some sail side movement ( camber )
      Z := x * (SubdivisionsWidth - x) * DL * 0.10;
      // (Random(10)-5)*0.003;  // test...

      if fCamberRight then
        Z := -Z;
      front^.Z := Z;
      back^.Z := Z;

      L := L + DL; // inc L
    end;
    h := h + Dh; // inc h
  end;

  M.CalcTangentBinormals;
  // fTime := fTime + 0.01;  //??
end;

function TgxSailSurface.Altura(P: TPoint3D): Single; // Om:
var
  M: TMeshData;
  x, y: integer;
  front, back: PPoint3D;
begin
  M := self.Data;
  x := Round(P.x);
  y := Round(P.y);
  if (x >= 0) and (x < SubdivisionsWidth) and (y > 0) and
    (y < SubdivisionsHeight) then
  begin
    fNbMesh := (SubdivisionsWidth + 1) * (SubdivisionsHeight + 1);
    // recalc mesh number of vertices

    front := M.VertexBuffer.VerticesPtr[x + (y * (SubdivisionsWidth + 1))];
    back := M.VertexBuffer.VerticesPtr
      [fNbMesh + x + (y * (SubdivisionsWidth + 1))];

    Result := (front^.Z + back^.Z) / 2; // ??
  end
  else
    Result := 0;
end;

procedure TgxSailSurface.SetSailParams(aSailParams: TSailParams);
begin
  fSailParams.Lb := aSailParams.Lb;
  fSailParams.Lt := aSailParams.Lt;
  fSailParams.Ht := aSailParams.Ht;
  // fSailParams.NsubH := aSailParams.NsubH;
  // fSailParams.NsubL := aSailParams.NsubL;

  // set plane subdivisions
  // self.SubdivisionsHeight := fSailParams.NsubH;  // plane subdivisions
  // self.SubdivisionsWidth  := fSailParams.NsubL;

  fNbMesh := (SubdivisionsWidth + 1) * (SubdivisionsHeight + 1);
  fCenter := Point3D(SubdivisionsWidth / self.Width,
    SubdivisionsHeight / self.Height, 0);

  // CalcSailSurfaceMesh;
end;

procedure TgxSailSurface.SetSailShape(const Value: TSailShape);
begin
  if fSailShape <> Value then
  begin
    fSailShape := Value;
    // should remesh here..
  end;
end;

procedure TgxSailSurface.setVersion(const Value: integer);
begin
  if (fVersion <> Value) then
  begin
    fVersion := Value;
    CalcSailSurfaceMesh; // recalc default mesh, based on fSailParams
  end;
end;

procedure TgxSailSurface.Render;
begin
  inherited;
  // each render recalcs the mesh !
  if fUseTasks then
  begin
    TTask.Create(
      procedure
      begin
        // CalcSailSurfaceMesh;   // recalc mesh
      end).start;
  end
  else
  begin
    // CalcSailSurfaceMesh;
  end;

  if ShowLines then
    Context.DrawLines(self.Data.VertexBuffer, self.Data.IndexBuffer,
      TMaterialSource.ValidMaterial(fMaterialLignes), 1);
end;

procedure TgxSailSurface.SetChordBottom(const Value: Single);
begin
  if fSailParams.Lb <> Value then
  begin
    fSailParams.Lb := Value;
    // CalcSailSurfaceMesh; // remesh
  end;
end;

procedure TgxSailSurface.SetChordMid(const Value: Single);
begin
  if fSailParams.Lm <> Value then
  begin
    fSailParams.Lm := Value;
    // CalcSailSurfaceMesh; // remesh
  end;
end;

procedure TgxSailSurface.SetChordTop(const Value: Single);
begin
  if fSailParams.Lt <> Value then
  begin
    fSailParams.Lt := Value;
    // CalcSailSurfaceMesh; // remesh
  end;
end;

procedure TgxSailSurface.SetChordX(const Value: Single);
begin
  if fSailParams.Lx <> Value then
  begin
    fSailParams.Lx := Value;
    // CalcSailSurfaceMesh; // remesh
  end;
end;

procedure TgxSailSurface.SetHeightX(const Value: Single);
begin
  if fSailParams.Hx <> Value then
  begin
    fSailParams.Hx := Value;
    // CalcSailSurfaceMesh; // remesh
  end;
end;

procedure TgxSailSurface.SetSailHeight(const Value: Single);
begin
  if fSailParams.ht <> Value then
  begin
    fSailParams.ht := Value;
    // CalcSailSurfaceMesh; // remesh
  end;
end;

//----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLXEngine', [TgxSailSurface]);
end;

end.
