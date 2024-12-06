unit GBE.PlaneExtend;
(*
  Om:
  GBEPlaneExtend implements a rectangular mesh with oe source of sin wave
  Extended TWaveRec to calc amplitude and pitch
  Added excessive comments to help understanding Gregorys code
*)

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Threading,
  System.Math.Vectors,
  System.RTLConsts,

  FMX.Types,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Types3D,
  FMX.MaterialSources;

type
  TWaveRec = record // calculates sin() wave amplitude at a given P (x,y)
    P, // wave origin
    D: TPoint3D;
    // wave params D = Point3D( MaxAmplitude, WaveLenght, WaveSpeed )
    function Wave(aSum, aX, aY, aT: single): single;
    function calcWaveAmplitudeAndPitch(aCap, aX, aY, aT: single;
      var aSumAmplitude, aSumDerivative: single): boolean;
  end;

  TGBEPlaneExtend = class(TPlane)
  private
    fAmplitude, fLongueur, fVitesse: single; // wave 1
    fOrigine: TPoint3D;
    procedure CalcWaves(D: TPoint3D);  // D = Point3D(fAmplitude, fLongueur, fVitesse)
  protected
    fTime: single; // Om: movd stuff to protected
    fNbMesh: integer; // number of tiles in the mesh
    fActiveWaves, fShowlines, fUseTasks: boolean;
    fMaterialLignes: TColorMaterialSource;
    fCenter: TPoint3D;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    // Property    Data;  //om: public
    function Altura(P: TPoint3D): single; // Om: calc wave amplitude on a point
  published
    property ActiveWaves: boolean read fActiveWaves write fActiveWaves;
    property Origine: TPoint3D read fOrigine write fOrigine;
    property Amplitude: single read fAmplitude write fAmplitude;
    property Longueur: single read fLongueur write fLongueur;
    property Vitesse: single read fVitesse write fVitesse;
    property ShowLines: boolean read fShowlines write fShowlines;
    property UseTasks: boolean read fUseTasks write fUseTasks;
    property MaterialLines: TColorMaterialSource read fMaterialLignes
      write fMaterialLignes;
  end;

procedure Register;

implementation //--------------------------------------------------------------

// TWaveRec

// wave world is x,y .  z is the wave height ( amplitude )
// /y
// +---------/---------+
// / ^    ^  /  ^   ^  /
// /---------+---------+----x
// /    ^    /    ^    /
// +---------/---------+
//

function TWaveRec.Wave(aSum, aX, aY, aT: single): single;
// sums. Here aX,aY are in div units ( not m )
var
  L, Ph: single;
begin
  L := P.Distance(Point3d(aX, aY, 0)); // L= dist to sin wave origin
  Result := aSum;
  // start w/ previous sum, so we add wave amplitudes of different waves
  // D.x = MaxAmplitude in div ?
  // D.y = WaveLenght   in div
  // D.z = WaveSpeed    in div/s
  if (D.Y > 0) and (D.x > 0) then
  // ignore if wave length<=0 Om: ..or Amplitude=0 ( which produces no effect )
  begin
    Ph := L / D.Y - D.z * aT; // calc wave phase at the point
    Result := Result + D.x * sin(Ph) * 0.001;
    // sum sin() wave amplitude / 1000  ( result inside +- MaxAmplitude*0.001 )
  end;
end;

// here wave world is in x,y        z is the wave height ( amplitude )
function TWaveRec.calcWaveAmplitudeAndPitch(aCap, aX, aY, aT: single;
  var aSumAmplitude, aSumDerivative: single): boolean;
var
  L, L0, L1, Ph, Ph0, Ph1, aAng, aAmp, aAmp0, aAmp1, aDeriv, DzaT: single;
  aP, P0, P1, DP: TPoint3D;
begin
  Result := true; // always
  aP := Point3d(aX, aY, 0); // requested coordinate
  L := P.Distance(aP); // L= dist to sin wave origin P

  if (D.Y > 0) and (D.x > 0) then
  // ignore if wave length D.Y <= 0  or Amplitude D.x <=0
  begin
    Result := true;
    DzaT := D.z * aT; // memoise speed*DT (=wave displacement)

    Ph := L / D.Y - DzaT; // calc wave phase at the point P
    aAmp := D.x * sin(Ph) * 0.001;
    // add sin() wave amplitude / 1000    ( result inside +- MaxAmplitude*0.001 )
    // P.z    := aAmp;   //dont do that !

    // calc directional derivative
    aAng := -aCap * Pi / 180; // cap to radians
    DP := Point3d(sin(aAng), cos(aAng), 0) / 2;
    // semi displacement vector in boat direction dir
    // P0=pt 1/2 div before
    P0 := aP - DP; // move .5 unit in -cap direction
    L0 := P.Distance(P0); // calc amplitude for P0
    Ph0 := L0 / D.Y - DzaT;
    aAmp0 := D.x * sin(Ph0) * 0.001;
    // add sin() wave amplitude / 1000    ( result inside +- MaxAmplitude*0.001 )
    // P1=pt 1/2 div after
    P1 := aP + DP / 2; // move 0.5 unit in cap direction
    L1 := P.Distance(P1); // calc amplitude for P1
    Ph1 := L1 / D.Y - DzaT;
    aAmp1 := D.x * sin(Ph1) * 0.001;

    // derivative calculated from -0.5 to +0.5 div
    aDeriv := (aAmp1 - aAmp0); // directional derivative calculated in P0-P1
    // add amplitude to sum

    aSumAmplitude := aSumAmplitude + aAmp; // accumulate
    aSumDerivative := aSumDerivative + aDeriv;
    // derivative of sum = sum of derivatives
  end;
end;

// TGBEPlaneExtend

procedure TGBEPlaneExtend.CalcWaves(D: TPoint3D);
// D = Point3D(Amplitude, Longueur, Vitesse)
var
  M: TMeshData;
  x, Y: integer;
  somme: single;
  front, back: PPoint3D;
  waveRec: TWaveRec;

begin
  M := self.Data;
  // init waveRec

  waveRec.P := Point3d(SubdivisionsWidth, SubdivisionsHeight, 0) * 0.5 +
    fOrigine * fCenter;
  waveRec.D := D;

  for Y := 0 to SubdivisionsHeight do // 0..30   ( 30 divisions )
    for x := 0 to SubdivisionsWidth do
    begin
      front := M.VertexBuffer.VerticesPtr[x + (Y * (SubdivisionsWidth + 1))];
      back := M.VertexBuffer.VerticesPtr
        [fNbMesh + x + (Y * (SubdivisionsWidth + 1))];
      somme := 0;
      somme := waveRec.Wave(somme, x, Y, fTime);
      somme := somme * 100;
      front^.z := somme;
      back^.z := somme;
    end;

  M.CalcTangentBinormals;

  fTime := fTime + 0.01;
  // adv wave time by 0.01 seg. Should be 0.2s ? ( the animation speed )
end;

function TGBEPlaneExtend.Altura(P: TPoint3D): single; // Om:
var
  M: TMeshData;
  x, Y: integer;
  front, back: PPoint3D;
begin
  M := self.Data;
  x := Round(P.x);
  Y := Round(P.Y);
  if (x >= 0) and (x < SubdivisionsWidth) and (Y > 0) and
    (Y < SubdivisionsHeight) then
  begin
    front := M.VertexBuffer.VerticesPtr[x + (Y * (SubdivisionsWidth + 1))];
    back := M.VertexBuffer.VerticesPtr
      [fNbMesh + x + (Y * (SubdivisionsWidth + 1))];

    Result := (front^.z + back^.z) / 2; // ??
  end
  else
    Result := 0;
end;

constructor TGBEPlaneExtend.Create(AOwner: TComponent);
begin
  inherited;
  fTime := 0; // wave time

  fAmplitude := 1; // wave params
  fLongueur := 1; // Om: only 1 ?
  fVitesse := 5; //

  self.SubdivisionsHeight := 30; // plane subdivisions
  self.SubdivisionsWidth := 30;

  fOrigine := Point3d(self.SubdivisionsWidth / self.Width,
    self.SubdivisionsHeight / self.Height, 2);
  fNbMesh := (SubdivisionsWidth + 1) * (SubdivisionsHeight + 1);
  // fCenter = SubD / width  ( unit div/m  )
  fCenter := Point3d(SubdivisionsWidth / self.Width,
    SubdivisionsHeight / self.Height, 0);
  fUseTasks := true; // default= using tasks
end;

destructor TGBEPlaneExtend.Destroy;
begin
  inherited;
end;

procedure TGBEPlaneExtend.Render;
var
  W1: TPoint3D;

begin
  inherited;
  if fActiveWaves then
  begin
    W1 := Point3d(fAmplitude, fLongueur, fVitesse);
    if fUseTasks then
    begin
      TTask.Create(
        procedure
        begin
          CalcWaves(W1); // recalc mesh
        end).start;
    end
    else
    begin
      CalcWaves(W1);
    end;

  end;

  if ShowLines then
    Context.DrawLines(self.Data.VertexBuffer, self.Data.IndexBuffer,
      TMaterialSource.ValidMaterial(fMaterialLignes), 1);
end;

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBEPlaneExtend]);
end;

end.
