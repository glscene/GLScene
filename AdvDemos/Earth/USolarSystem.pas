//
// The graphics rendering engine GLScene http://glscene.org
//
unit USolarSystem;
(*
   Solar system planetary elements and positions utility unit.

   Based on document by Paul Schlyter (Stockholm, Sweden)
   http://www.stjarnhimlen.se/comp/ppcomp.html

   Coordinates system takes Z as "up", ie. normal to the ecliptic plane,
   "axis" around which the planets turn.

   Eric Grange
   http://glscene.org
*)
interface

uses
  System.SysUtils,
  System.Math,
  GLS.VectorGeometry;

type

   TOrbitalElements = record
      N : Double;    // longitude of the ascending node
      i : Double;    // inclination to the ecliptic (plane of the Earth's orbit)
      w : Double;    // argument of perihelion
      a : Double;    // semi-major axis, or mean distance from Sun
      e : Double;    // eccentricity (0=circle, 0-1=ellipse, 1=parabola)
      M : Double;    // mean anomaly (0 at perihelion; increases uniformly with time)
   end;

   TOrbitalElementsData = record
      NConst, NVar : Double;     // longitude of the ascending node
      iConst, iVar : Double;     // inclination to the ecliptic (plane of the Earth's orbit)
      wConst, wVar : Double;     // argument of perihelion
      aConst, aVar : Double;     // semi-major axis, or mean distance from Sun
      eConst, eVar : Double;     // eccentricity (0=circle, 0-1=ellipse, 1=parabola)
      MConst, MVar : Double;     // mean anomaly (0 at perihelion; increases uniformly with time)
   end;

const

   // geocentric sun elements (?)
   cSunOrbitalElements : TOrbitalElementsData = (
      NConst : 0.0;        NVar : 0.0;
      iConst : 0.0;        iVar : 0.0;
      wConst : 282.9404;   wVar : 4.70935E-5;
      aConst : 1.000000;   aVar : 0.0; // (AU)
      eConst : 0.016709;   eVar : -1.151E-9;
      MConst : 356.0470;   MVar : 0.9856002585  );

   // geocentric moon elements
   cMoonOrbitalElements : TOrbitalElementsData = (
      NConst : 125.1228;   NVar : -0.0529538083;
      iConst : 5.1454;     iVar : 0.0;
      wConst : 318.0634;   wVar : 0.1643573223;
      aConst : 60.2666;    aVar : 0.0; // (Earth radii)
      eConst : 0.054900;   eVar : 0.0;
      MConst : 115.3654;   MVar : 13.0649929509  );

   // heliocentric mercury elements
   cMercuryOrbitalElements : TOrbitalElementsData = (
      NConst : 48.3313;    NVar : 3.24587E-5;
      iConst : 7.0047;     iVar : 5.00E-8;
      wConst : 29.1241;    wVar : 1.01444E-5;
      aConst : 0.387098;   aVar : 0.0; // (AU)
      eConst : 0.205635;   eVar : 5.59E-10;
      MConst : 168.6562;   MVar : 4.0923344368  );

   // heliocentric venus elements
   cVenusOrbitalElements : TOrbitalElementsData = (
      NConst : 76.6799;    NVar : 2.46590E-5;
      iConst : 3.3946;     iVar : 2.75E-8;
      wConst : 54.8910;    wVar : 1.38374E-5;
      aConst : 0.723330;   aVar : 0.0; // (AU)
      eConst : 0.006773;   eVar : -1.302E-9;
      MConst : 48.0052;    MVar : 1.6021302244  );

   // heliocentric mars elements
   cMarsOrbitalElements : TOrbitalElementsData = (
      NConst : 49.5574;    NVar : 2.11081E-5;
      iConst : 1.8497;     iVar : -1.78E-8;
      wConst : 286.5016;   wVar : 2.92961E-5;
      aConst : 1.523688;   aVar : 0.0; // (AU)
      eConst : 0.093405;   eVar : 2.516E-9;
      MConst : 18.6021;    MVar : 0.5240207766  );

   // heliocentric jupiter elements
   cJupiterOrbitalElements : TOrbitalElementsData = (
      NConst : 100.4542;   NVar : 2.76854E-5;
      iConst : 1.3030;     iVar : -1.557E-7;
      wConst : 273.8777;   wVar : 1.64505E-5;
      aConst : 5.20256;    aVar : 0.0; // (AU)
      eConst : 0.048498;   eVar : 4.469E-9;
      MConst : 19.8950;    MVar : 0.0830853001  );

   // heliocentric saturn elements
   cSaturnOrbitalElements : TOrbitalElementsData = (
      NConst : 113.6634;   NVar : 2.38980E-5;
      iConst : 2.4886;     iVar : -1.081E-7;
      wConst : 339.3939;   wVar : 2.97661E-5;
      aConst : 9.55475;    aVar : 0.0; // (AU)
      eConst : 0.055546;   eVar : -9.499E-9;
      MConst : 316.9670;   MVar : 0.0334442282  );

   // heliocentric uranus elements
   cUranusOrbitalElements : TOrbitalElementsData = (
      NConst : 74.0005;    NVar : 1.3978E-5;
      iConst : 0.7733;     iVar : 1.9E-8;
      wConst : 96.6612;    wVar : 3.0565E-5;
      aConst : 19.18171;   aVar : -1.55E-8; // (AU)
      eConst : 0.047318;   eVar : 7.45E-9;
      MConst : 142.5905;   MVar : 0.011725806  );

   // heliocentric neptune elements
   cNeptuneOrbitalElements : TOrbitalElementsData = (
      NConst : 131.7806;   NVar : 3.0173E-5;
      iConst : 1.7700;     iVar : -2.55E-7;
      wConst : 272.8461;   wVar : -6.027E-6;
      aConst : 30.05826;   aVar : 3.313E-8; // (AU)
      eConst : 0.008606;   eVar : 2.15E-9;
      MConst : 260.2471;   MVar : 0.005995147  );

   cAUToKilometers = 149.6e6; // astronomical units to kilometers
   cEarthRadius = 6400; // earth radius in kilometers

{: Converts a TDateTime (GMT+0) into the julian day used for computations. }
function GMTDateTimeToJulianDay(const dt : TDateTime) : Double;
{: Compute orbital elements for given julian day. }
function ComputeOrbitalElements(const oeData : TOrbitalElementsData;
                                const d : Double) : TOrbitalElements;

{: Compute the planet position for given julian day (in AU). }
function ComputePlanetPosition(const orbitalElements : TOrbitalElements) : TAffineVector; overload;
function ComputePlanetPosition(const orbitalElementsData : TOrbitalElementsData;
                               const d : Double) : TAffineVector; overload;

//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------

function GMTDateTimeToJulianDay(const dt : TDateTime) : Double;
begin
   Result:=dt-EncodeDate(2000, 1, 1);
end;

function ComputeOrbitalElements(const oeData : TOrbitalElementsData;
                                const d : Double) : TOrbitalElements;
begin
   with Result, oeData do begin
      N:=NConst+NVar*d;
      i:=iConst+iVar*d;
      w:=wConst+wVar*d;
      a:=aConst+aVar*d;
      e:=eConst+eVar*d;
      M:=MConst+MVar*d;
   end;
end;

function ComputePlanetPosition(const orbitalElements : TOrbitalElements) : TAffineVector;
var
   eccentricAnomaly, eA0 : Double;
   sm, cm, se, ce, si, ci, cn, sn, cvw, svw : Double;
   xv, yv, v, r : Double;
   nn : Integer; // numerical instability bailout
begin
   with orbitalElements do begin
      // E = M + e*(180/pi) * sin(M) * ( 1.0 + e * cos(M) )
      SinCos(M*cPIdiv180, sm, cm);
      eccentricAnomaly:=M+e*c180divPI*sm*(1.0+e*cm);

      nn:=0;
      repeat
         eA0:=eccentricAnomaly;
         // E1 = E0 - ( E0 - e*(180/pi) * sin(E0) - M ) / ( 1 - e * cos(E0) )
         SinCos(eA0*cPIdiv180, se, ce);
         eccentricAnomaly:=eA0-(eA0-e*c180divPI*se-M)/(1-e*ce);
         Inc(nn);
      until (nn>10) or (Abs(eccentricAnomaly-eA0)<1e-4);

      SinCos(eccentricAnomaly*cPIdiv180, se, ce);
      xv:=a*(ce-e);
      yv:=a*(Sqrt(1.0-e*e)*se);

      v:=ArcTan2(yv, xv)*c180divPI;
      r:=Sqrt(xv*xv+yv*yv);

      SinCos(i*cPIdiv180, si, ci);
      SinCos(N*cPIdiv180, sn, cn);
      SinCos((v+w)*cPIdiv180, svw, cvw);
   end;

   // xh = r * ( cos(N) * cos(v+w) - sin(N) * sin(v+w) * cos(i) )
   Result.X:=r*(cn*cvw-sn*svw*ci);
   // yh = r * ( sin(N) * cos(v+w) + cos(N) * sin(v+w) * cos(i) )
   Result.Y:=r*(sn*cvw+cn*svw*ci);
   // zh = r * ( sin(v+w) * sin(i) )
   Result.Z:=r*(svw*si);
end;

function ComputePlanetPosition(const orbitalElementsData : TOrbitalElementsData;
                               const d : Double) : TAffineVector;
var
   oe : TOrbitalElements;
begin
   oe:=ComputeOrbitalElements(orbitalElementsData, d);
   Result:=ComputePlanetPosition(oe);
end;

end.
