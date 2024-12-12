unit GXS.OceanWaves;
(*
  Omar:  TgxTwoWavesOceanSurface
  TGBEPlaneExtend has one wave. Added a second to TgxTwoWavesOceanSurface
  actually 3 waves... and counting
  set21: increased the wave system from 3 to 5 waves
*)
interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Types,
  System.Threading,
  System.Math.Vectors,
  System.Generics.Collections,
  System.RTLConsts,

  FMX.Types,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Types3D,
  FMX.MaterialSources,
  GBE.PlaneExtend; // TGBEPlaneExtend and WaveRec

type
  TgxWaveSystem = class(TComponent)
  // collection of sea surface sinoid waves (3 for now)
  private
    fTime: Single; // Om: movd stuff to protected
    // wave  params
    fAmplitude, fLongueur, fVitesse: Single; // wave 1
    fOrigine: TPoint3D;

    fAmplitude2, fLongueur2, fVitesse2: Single;
    fOrigine2: TPoint3D;

    fAmplitude3, fLongueur3, fVitesse3: Single;
    fOrigine3: TPoint3D;

    fAmplitude4, fLongueur4, fVitesse4: Single;
    fOrigine4: TPoint3D;

    fAmplitude5, fLongueur5, fVitesse5: Single;
    fOrigine5: TPoint3D;
  protected
    function GetWaveParams1: TPoint3D;
    function GetWaveParams2: TPoint3D;
    function GetWaveParams3: TPoint3D;
    function GetWaveParams4: TPoint3D;
    function GetWaveParams5: TPoint3D;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure IncTime;

    Property WaveTime: Single read fTime write fTime;

  published
    property Origine: TPoint3D read fOrigine write fOrigine; // wave 1
    property Amplitude: Single read fAmplitude write fAmplitude;
    property Longueur: Single read fLongueur write fLongueur;
    property Vitesse: Single read fVitesse write fVitesse;

    property Origine2: TPoint3D read fOrigine2 write fOrigine2; // wave 2
    property Amplitude2: Single read fAmplitude2 write fAmplitude2;
    property Longueur2: Single read fLongueur2 write fLongueur2;
    property Vitesse2: Single read fVitesse2 write fVitesse2;

    property Origine3: TPoint3D read fOrigine3 write fOrigine3; // wave 3
    property Amplitude3: Single read fAmplitude3 write fAmplitude3;
    property Longueur3: Single read fLongueur3 write fLongueur3;
    property Vitesse3: Single read fVitesse3 write fVitesse3;

    property Origine4: TPoint3D read fOrigine4 write fOrigine4; // wave 4
    property Amplitude4: Single read fAmplitude4 write fAmplitude4;
    property Longueur4: Single read fLongueur4 write fLongueur4;
    property Vitesse4: Single read fVitesse4 write fVitesse4;

    property Origine5: TPoint3D read fOrigine5 write fOrigine5; // wave 5
    property Amplitude5: Single read fAmplitude5 write fAmplitude5;
    property Longueur5: Single read fLongueur5 write fLongueur5;
    property Vitesse5: Single read fVitesse5 write fVitesse5;
  end;

  TgxOceanSurface = class(TPlane)
  private
    fWaveSystem: TgxWaveSystem;
    procedure CalcWaves;
  protected
    fNbMesh: integer; // number of tiles in the mesh
    fActiveWaves, fShowlines, fUseTasks: boolean;
    fDivPerM: TPoint3D;
    fMaterialLignes: TColorMaterialSource;
  public
    fVirtualSeaOrigin: TPoint3D; // position of the origin of the virtual sea

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Property    Data;  //om: publica
    function calcWaveAmplitudeAndPitch(P: TPoint3D; const aCap: Single;
      var aAmplitude, aPitch: Single): boolean; // Om:
    procedure Render; override;
    procedure MoveTextureBy(var dx, dy: Single);
    procedure GetPointsTexCoordinates(var P, TC: String);
    // W1,W2 = Point3D(Amplitude, Longueur, Vitesse)
  published
    property ActiveWaves: boolean read fActiveWaves write fActiveWaves;
    property ShowLines: boolean read fShowlines write fShowlines;
    property WaveSystem: TgxWaveSystem read fWaveSystem write fWaveSystem;
    property MaterialLines: TColorMaterialSource read fMaterialLignes
      write fMaterialLignes;
    property DivPerM: TPoint3D read fDivPerM;
  end;

  TgxWindArrowSurface = class(TPlane)
  // Ondulating wind arrow. Less CPU intensive then OceanSurface
  private
    fVersion: integer;
    fWaveSystem: TgxWaveSystem; // using only wave 1 here
    procedure CalcArrowMesh;
    procedure setVersion(const Value: integer);
  protected
    fNbMesh: integer; // number of tiles in the mesh
    fActiveWaves, fShowlines, fUseTasks: boolean;
    fDivPerM: TPoint3D;
    fMaterialLignes: TColorMaterialSource;
    procedure SetDepth(const Value: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Property    Data;  //om: publica
    procedure Render; override;
  published
    property ActiveWaves: boolean read fActiveWaves write fActiveWaves;
    property ShowLines: boolean read fShowlines write fShowlines;
    property MaterialLines: TColorMaterialSource read fMaterialLignes
      write fMaterialLignes;
    property DivPerM: TPoint3D read fDivPerM;

    Property version: integer read fVersion write setVersion;
  end;

  TgxTwoWavesOceanSurface = class(TgxOceanSurface); // compatibility w/ old forms

procedure Register;

implementation //--------------------------------------------------------------

// TgxWaveSystem

constructor TgxWaveSystem.Create(AOwner: TComponent);
begin
  inherited;

  fTime := 0;

  fAmplitude := 2.5; // wave params
  fLongueur := 5; // Om: only 1 ?
  fVitesse := 5; //
  fOrigine := Point3D(1, 1, 2);

  fAmplitude2 := 1.1; // 2.5;      //wave 2 params
  fLongueur2 := 2.1;
  fVitesse2 := 3;
  fOrigine2 := Point3D(3, 10, 2);
  // (SubdivisionsWidth/Width, SubdivisionsHeight/Height, 2)

  fAmplitude3 := 1.5; // wave 3 params
  fLongueur3 := 4.2;
  fVitesse3 := 3.2;
  fOrigine3 := Point3D(7, -8, 1);

  fAmplitude4 := 0.5; // wave 4 params
  fLongueur4 := 2.2;
  fVitesse4 := 2.2;
  fOrigine4 := Point3D(7, -8, 1);

  fAmplitude5 := 0.7; // wave 5 params
  fLongueur5 := 4.2;
  fVitesse5 := 2.2;
  fOrigine5 := Point3D(7, -8, 1);
end;

destructor TgxWaveSystem.Destroy;
begin
  inherited;
end;

function TgxWaveSystem.GetWaveParams1: TPoint3D;
begin
  Result := Point3D(Amplitude, Longueur, Vitesse); // pack wave params
end;

function TgxWaveSystem.GetWaveParams2: TPoint3D;
begin
  Result := Point3D(fAmplitude2, fLongueur2, fVitesse2);
end;

function TgxWaveSystem.GetWaveParams3: TPoint3D;
begin
  Result := Point3D(fAmplitude3, fLongueur3, fVitesse3);
end;

function TgxWaveSystem.GetWaveParams4: TPoint3D;
begin
  Result := Point3D(fAmplitude4, fLongueur4, fVitesse4);
end;

function TgxWaveSystem.GetWaveParams5: TPoint3D;
begin
  Result := Point3D(fAmplitude5, fLongueur5, fVitesse5);
end;

procedure TgxWaveSystem.IncTime;
begin
  fTime := fTime + 0.010; // advance wave time.. slow advance
end;

{ TgxOceanSurface }

constructor TgxOceanSurface.Create(AOwner: TComponent);
begin
  inherited;

  fWaveSystem := nil;
  self.SubdivisionsHeight := 30; // plane subdivisions
  self.SubdivisionsWidth := 30;

  fNbMesh := (SubdivisionsWidth + 1) * (SubdivisionsHeight + 1);
  // fDivPerM = SubD / width  -->  units div/m
  fDivPerM := Point3D(SubdivisionsWidth / self.Width,
    SubdivisionsHeight / self.Height, 0);

  fUseTasks := true; // default= using tasks instead of inline mesh builds
  fVirtualSeaOrigin := Point3D(0, 0, 0);
end;

destructor TgxOceanSurface.Destroy;
begin
  inherited;
end;

Function MyFrac(const n: Single): Single;
begin
  Result := Frac(n);
  if (Result < 0) then
    Result := Result + 1.0;
end;

// MoveTextureBy uses existent mesh, by just displacing the tex pts
procedure TgxOceanSurface.MoveTextureBy(var dx, dy: Single);
// dx,dy in  3d units (m)
var
  M: TMeshData;
  S: String;
  x, y: integer;
  front, back: TPointF;
  ixf, ixb: integer;
  du, dv: Single;

begin
  M := self.Data; // get mesh

  fVirtualSeaOrigin := fVirtualSeaOrigin - Point3D(dx, dy, 0);
  // move by virtual sea coordinate system

  du := dx / Width; // in 0..1 range
  dv := dy / Height;

  fNbMesh := (SubdivisionsWidth + 1) * (SubdivisionsHeight + 1);

  for y := 0 to SubdivisionsHeight do
    for x := 0 to SubdivisionsWidth do
    begin
      ixf := x + (y * (SubdivisionsWidth + 1));
      // calc front and back point indexes
      ixb := fNbMesh + x + (y * (SubdivisionsWidth + 1));

      front := M.VertexBuffer.TexCoord0[ixf];
      // TexCoord0 must be in 0.0 .. 1.0  range  ( UV coordinates )
      back := M.VertexBuffer.TexCoord0[ixb];

      front.x := MyFrac(front.x + du);
      // Frac() wraps around the texture coordinates
      front.y := MyFrac(front.y + dv);
      // TODO: this leaves the last row,column w/ inverted texture ..

      back.x := MyFrac(back.x + du);
      back.y := MyFrac(back.y + dv);

      M.VertexBuffer.TexCoord0[ixf] := front;
      M.VertexBuffer.TexCoord0[ixb] := back;
    end;

  // format TexCoordinates as a str
  // '0.0 0.0, 1 0, 0.0 1, 1 1';
  // S := FloatToStr(u)  +' '+FloatToStr(v)   +','+
  // FloatToStr(u+1)+' '+FloatToStr(v)   +','+
  // FloatToStr(u)  +' '+FloatToStr(v+1) +','+
  // FloatToStr(u+1)+' '+FloatToStr(v+1);
  // M.Points         :=
  // M.TexCoordinates :=
end;

procedure TgxOceanSurface.GetPointsTexCoordinates(var P, TC: String);
var
  M: TMeshData;
begin
  M := self.Data; // get mesh

  P := M.Points;
  TC := M.TexCoordinates;
end;

procedure TgxOceanSurface.CalcWaves; // Wx = Point3D(Amplitude, Longueur, Vitesse)
var
  M: TMeshData;
  x, y: integer;
  ax, ay: Single;
  somme: Single;
  PCenter: TPoint3D;
  front, back: PPoint3D;
  waveRec1, waveRec2, waveRec3, waveRec4, waveRec5: TWaveRec;
begin
  if not Assigned(fWaveSystem) then
    exit;

  M := self.Data; // get mesh
  // init waveRecs
  PCenter := Point3D(SubdivisionsWidth, SubdivisionsHeight, 0) * 0.5;
  fDivPerM := Point3D(SubdivisionsWidth / self.Width,
    SubdivisionsHeight / self.Height, 0);
  fNbMesh := (SubdivisionsWidth + 1) * (SubdivisionsHeight + 1);

  // Waves 1 and 2 move with OceanSurface, like the floating stuff
  waveRec1.P := fWaveSystem.Origine + fVirtualSeaOrigin * fDivPerM;
  // calc sin wave origin position
  waveRec1.D := fWaveSystem.GetWaveParams1;
  // D = Point3D( Amplitude, Longueur, Vitesse)

  waveRec2.P := fWaveSystem.fOrigine2 + fVirtualSeaOrigin * fDivPerM;
  waveRec2.D := fWaveSystem.GetWaveParams2;

  // waves 3,4,5 move with the boat
  waveRec3.P := fWaveSystem.fOrigine3;
  waveRec3.D := fWaveSystem.GetWaveParams3;

  waveRec4.P := fWaveSystem.fOrigine4;
  waveRec4.D := fWaveSystem.GetWaveParams4;

  waveRec5.P := fWaveSystem.fOrigine5;
  waveRec5.D := fWaveSystem.GetWaveParams5;

  for y := 0 to SubdivisionsHeight do
  begin
    ay := y - PCenter.y; // + PCenter.y;
    for x := 0 to SubdivisionsWidth do
    begin
      ax := x - PCenter.x; // + PCenter.x;  //ax,ay in div
      // preserve original TPlane vertice's x,y.   Apply wave amplitude to z coordinate
      front := M.VertexBuffer.VerticesPtr[x + (y * (SubdivisionsWidth + 1))];
      back := M.VertexBuffer.VerticesPtr
        [fNbMesh + x + (y * (SubdivisionsWidth + 1))];
      // calc sum of wave amplitudes.  Here x,y is in division units ! ( not m )

      somme := 0; // sum, effect of waves
      somme := waveRec1.Wave(somme, ax, ay, fWaveSystem.fTime); // 1 st
      somme := waveRec2.Wave(somme, ax, ay, fWaveSystem.fTime); // 2 nd
      somme := waveRec3.Wave(somme, ax, ay, fWaveSystem.fTime); // 3 rd
      somme := waveRec4.Wave(somme, ax, ay, fWaveSystem.fTime); // 4 nd
      somme := waveRec5.Wave(somme, ax, ay, fWaveSystem.fTime); // 5 nd

      somme := somme * 100; // scale amplitude to div x 100 ?!

      front^.Z := somme;
      back^.Z := somme;
    end;
  end;

  M.CalcTangentBinormals;

  fWaveSystem.IncTime; // adv time by 0.01 sec

end;

// P in m
function TgxOceanSurface.calcWaveAmplitudeAndPitch(P: TPoint3D;
  const aCap: Single; var aAmplitude, aPitch: Single): boolean; // Om:
var
  waveRec1, waveRec2, waveRec3, waveRec4, waveRec5: TWaveRec;
  aSumAmpl, aSumDeriv, D, AbsD, ax, ay: Single;
  // PCenter:TPoint3d;

begin
  Result := false;
  if not Assigned(fWaveSystem) then
    exit;

  // init waveRecs
  // PCenter := Point3d( SubdivisionsWidth, SubdivisionsHeight, 0)*0.5;
  fDivPerM := Point3D(SubdivisionsWidth / self.Width,
    SubdivisionsHeight / self.Height, 0);

  // waves 1 n 2 Oringines move with the sea surface
  waveRec1.P := fWaveSystem.Origine + fVirtualSeaOrigin * fDivPerM;
  // calc sin wave origin position
  waveRec1.D := fWaveSystem.GetWaveParams1;
  // D = Point3D( Amplitude, Longueur, Vitesse)

  waveRec2.P := fWaveSystem.fOrigine2 + fVirtualSeaOrigin * fDivPerM;
  // + fVirtualSeaOrigin * fDivPerM;
  waveRec2.D := fWaveSystem.GetWaveParams2;

  // waves 3,4,5 move with the boat ( stationary )
  waveRec3.P := fWaveSystem.fOrigine3; // ugly array :(
  waveRec3.D := fWaveSystem.GetWaveParams3;

  waveRec4.P := fWaveSystem.fOrigine4;
  waveRec4.D := fWaveSystem.GetWaveParams4;

  waveRec5.P := fWaveSystem.fOrigine5;
  waveRec5.D := fWaveSystem.GetWaveParams5;

  // Sum amplitudes. Note that the derivative of a sum is the sum of the derivatives
  // f(x)=g(x)+h(x)    ---> f'(x)=g'(x)+h'(x)
  ax := (P.x - Position.x) * fDivPerM.x; // ax,ay in div
  ay := (P.Z + Position.Z) * fDivPerM.y;

  aSumAmpl := 0;
  aSumDeriv := 0;

  if waveRec1.calcWaveAmplitudeAndPitch(aCap, ax, ay, fWaveSystem.fTime,
    { out: } aSumAmpl, aSumDeriv) and // x,y in divs
    waveRec2.calcWaveAmplitudeAndPitch(aCap, ax, ay, fWaveSystem.fTime,
    { out: } aSumAmpl, aSumDeriv) and waveRec3.calcWaveAmplitudeAndPitch(aCap,
    ax, ay, fWaveSystem.fTime, { out: } aSumAmpl, aSumDeriv) and
    waveRec4.calcWaveAmplitudeAndPitch(aCap, ax, ay, fWaveSystem.fTime,
    { out: } aSumAmpl, aSumDeriv) and waveRec5.calcWaveAmplitudeAndPitch(aCap,
    ax, ay, fWaveSystem.fTime, { out: } aSumAmpl, aSumDeriv) then
  begin
    Result := true;
    aAmplitude := aSumAmpl * 100;
    // scale amplitude x 100, as was done creating the mesh
    // calc pitch in degrees
    D := aSumDeriv * 100 * 3; // scale deriv by 1000 ( cause amplitudes )
    AbsD := Abs(D);
    if (AbsD > 1) then
      D := D / AbsD;
    if (D >= -1.0) and (D <= 1.0) then
      aPitch := ArcSin(D) * 180 / Pi; // ad hoc formula
    if aPitch < -25 then
      aPitch := -25 // limit pitch to 25 deg
    else if aPitch > 25 then
      aPitch := 25;
  end;
end;


// function TgxOceanSurface.calcWaveAmplitudeAndPitch(P:TPoint3d; const aCap:Single; var aAmplitude,aPitch:Single ):boolean; //Om:
// var
// M:TMeshData;
// x,y,z,x1,y1,z1 : integer;
// front, back, next : PPoint3D;
// P1:TPoint3d;
// aAng,D,AbsD:Single;
//
// begin
// M := Data;
//
// x := Round( P.x - Position.x + SubdivisionsHeight/2 );
// y := Round( P.z - Position.z + SubdivisionsWidth/2  );
//
// aAng := -aCap*Pi/180;  // cap to radians
//
// P1 := Point3D(x,y,0) + 1.0 * Point3d( sin(aAng), cos(aAng), 0);  // P1 = pto futuro, for pitch calculation
// x1 := Round( P1.x );
// y1 := Round( P1.y );
//
// if (x>=0) and (x<SubdivisionsWidth) and (y>0) and (y<SubdivisionsHeight) then
// begin
// front      := M.VertexBuffer.VerticesPtr[x + (y * (SubdivisionsWidth+1))];
// // back  := M.VertexBuffer.VerticesPtr[fNbMesh + X + (Y * (SubdivisionsWidth+1))];
// aAmplitude := front^.Z;   // +back^.Z)/2; //??
// Result     := true;
//
// if (x1>=0) and (x1<SubdivisionsWidth) and (y1>0) and (y1<SubdivisionsHeight) then
// begin
// next := M.VertexBuffer.VerticesPtr[x1 + (y1 * (SubdivisionsWidth+1))];
// D    := (next^.Z-aAmplitude)/1.5;
// // AbsD := Abs(D);
// // if (AbsD>1) then D:=D/AbsD;
// //   0.8 is dynamic dampening
// if (D>=-1.0) and (D<=1.0) then aPitch := ArcSin( D )*180/Pi * 1.0;     // ad hoc formula
// if aPitch<-25 then  aPitch:=-25         // limit pitch to 25 deg
// else if aPitch>25 then  aPitch:=25;
// end
// else aPitch:=0;  //no pitch
// end
// else Result := false;
// end;

procedure TgxOceanSurface.Render;
begin
  inherited;

  if Assigned(fWaveSystem) and fActiveWaves then
  begin
    if fUseTasks then
    begin
      TTask.Create(
        procedure
        begin
          CalcWaves; // recalc mesh
        end).start;
    end
    else
    begin
      CalcWaves;
    end;
  end;

  if ShowLines then
    Context.DrawLines(self.Data.VertexBuffer, self.Data.IndexBuffer,
      TMaterialSource.ValidMaterial(fMaterialLignes), 1);
end;

{ TgxWindArrowSurface }

constructor TgxWindArrowSurface.Create(AOwner: TComponent);
begin
  inherited; // TPlane.Create

  // create an own wave system
  fWaveSystem := TgxWaveSystem.Create(nil);

  fWaveSystem.Amplitude := 0.8; // set wave 1
  fWaveSystem.Vitesse := 20.0;
  fWaveSystem.Longueur := 1.1;
  fWaveSystem.Origine := Point3D(0, -10, 0); // to have ondulation on y axis

  fWaveSystem.Amplitude2 := 0; // don't need those
  fWaveSystem.Amplitude3 := 0;
  fWaveSystem.Amplitude4 := 0;
  fWaveSystem.Amplitude5 := 0;

  SubdivisionsHeight := 25; // default plane subdivisions
  SubdivisionsWidth := 2;

  fNbMesh := (SubdivisionsWidth + 1) * (SubdivisionsHeight + 1);
  // fDivPerM = SubD / width  -->  units div/m
  fDivPerM := Point3D(SubdivisionsWidth / self.Width,
    SubdivisionsHeight / self.Height, 0);

  fUseTasks := true; // default= using tasks instead of inline mesh builds
  fVersion := 0;
end;

destructor TgxWindArrowSurface.Destroy;
begin
  fWaveSystem.Free;
  inherited;
end;

// transforms a plane into an arrow, by squeezing the x coordinate
procedure TgxWindArrowSurface.CalcArrowMesh; //
var // 0.0
  M: TMeshData; // Y       /\    0.5
  x, y: integer; // /  \
  front, back: PPoint3D; // /    \
  somme, h, Dh, Z, ah, al, ax, ay: Single; // +--  --+ 0.3
  waveRec1: TWaveRec; // |  |
  PCenter: TPoint3D; // |  |
  // |  |   0
  function _xArrowFunction(const aAx, aAy: Single): Single;
  // takes a plane mesh //         |  |
  begin // and turns it into   //         |  |
    if (aAy >= 0.3) then
      Result := aAx * (0.5 - aAy) / 0.1
      // an arrow             //  +    + +--+  -0.5    X >
    else
      Result := aAx / 1.6;
    // for y in -0.5..0.4, slim the mesh              //     -0.5  0 0.5
  end;

begin
  // sail params sanity test
  if (SubdivisionsHeight <= 0) or (SubdivisionsWidth <= 0) then
    exit; // invalid subdiv values

  M := self.Data; // use default TPlane mesh
  fNbMesh := (SubdivisionsWidth + 1) * (SubdivisionsHeight + 1);
  // recalc mesh number of vertices

  // mesh is calculated to fit into  [-0.5,-0.5..0.5,0.5] interval.
  h := -0.5;
  Dh := 1.0 / SubdivisionsHeight;
  // this will create an arrow mesh in h range -0.5 .. 0.5

  PCenter := Point3D(SubdivisionsWidth / self.Width,
    SubdivisionsHeight / self.Height, 0);

  // Wave 1
  waveRec1.P := fWaveSystem.Origine; // calc sin wave origin position
  waveRec1.D := fWaveSystem.GetWaveParams1;
  // D = Point3D( Amplitude, Longueur, Vitesse)

  for y := 0 to SubdivisionsHeight do
  begin

    somme := 0; // arrow ondulation
    somme := waveRec1.Wave(somme, 0, y, fWaveSystem.fTime) * 1000;

    for x := 0 to SubdivisionsWidth do // L
    begin //
      front := M.VertexBuffer.VerticesPtr[x + (y * (SubdivisionsWidth + 1))];
      back := M.VertexBuffer.VerticesPtr
        [fNbMesh + x + (y * (SubdivisionsWidth + 1))];

      ax := -0.5 + x / SubdivisionsWidth;
      ax := _xArrowFunction(ax, h); // L,h in -0.5..0.5 range
      ay := h;

      front^.x := ax;
      front^.y := ay;

      back^.x := ax;
      back^.y := ay;

      // add some sail side movement ( camber )    //

      front^.Z := somme;
      back^.Z := somme;
    end;
    h := h + Dh; // inc h
  end;

  M.CalcTangentBinormals;
  // fTime := fTime + 0.01;  //??
  fWaveSystem.IncTime; // adv time by 0.01 sec
end;

procedure TgxWindArrowSurface.Render;
begin
  inherited;

  if Assigned(fWaveSystem) and fActiveWaves then
  begin
    if fUseTasks then
    begin
      TTask.Create(
        procedure
        begin
          CalcArrowMesh; // recalc mesh
        end).start;
    end
    else
    begin
      CalcArrowMesh;
    end;
  end;

  if ShowLines then
    Context.DrawLines(self.Data.VertexBuffer, self.Data.IndexBuffer,
      TMaterialSource.ValidMaterial(fMaterialLignes), 1);
end;

procedure TgxWindArrowSurface.setVersion(const Value: integer);
begin
  if (fVersion <> Value) then
  begin
    CalcArrowMesh; // recalc mesh
    fVersion := Value;
  end;
end;

procedure TgxWindArrowSurface.SetDepth(const Value: Single);
// override TPlane tendency to set Depth to 0.01
begin
  if (self.fDepth <> Value) then // this copies what TPlane removed
  begin
    self.fDepth := Value;
    Resize3D;
    if (fDepth < 0) and (csDesigning in ComponentState) then
    begin
      fDepth := Abs(fDepth);
      FScale.Z := -FScale.Z;
    end;
    if not(csLoading in ComponentState) then
      Repaint;
  end;
end;

//----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLXEngine', [TgxWaveSystem, TgxOceanSurface,
    TgxTwoWavesOceanSurface, TgxWindArrowSurface]);
end;

end.
