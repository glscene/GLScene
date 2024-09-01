//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.GeometryCoordinates;

(*
  Helper functions to convert between different three dimensional coordinate
  systems. Room for optimisations.
*)

interface

uses
  System.Math,
  GXS.VectorGeometry;

(* Convert Cylindrical to Cartesian with no checks. Single version with theta in rad
  Ref: http://mathworld.wolfram.com/CylindricalCoordinates.html *)
procedure Cylindrical_Cartesian(const r, theta, z1: single; var x, y, z: single); overload;
(* Convert Cylindrical to Cartesian with no checks. Double version with theta in rads
  Ref: http://mathworld.wolfram.com/CylindricalCoordinates.html *)
procedure Cylindrical_Cartesian(const r, theta, z1: double; var x, y, z: double); overload;
(* Convert Cylindrical to Cartesian with checks. Single version with theta in rad
  ierr: [0] = ok,
  [1] = r out of bounds. Acceptable r: [0,inf)
  [2] = theta out of bounds. Acceptable theta: [0,2pi)
  [3] = z1 out of bounds. Acceptable z1 : (-inf,inf)
  Ref: http://mathworld.wolfram.com/CylindricalCoordinates.html *)
procedure Cylindrical_Cartesian(const r, theta, z1: single; var x, y, z: single;
  var ierr: integer); overload;
(* Convert Cylindrical to Cartesian with checks. Double version with theta in rad
  ierr: [0] = ok,
  [1] = r out of bounds. Acceptable r: [0,inf)
  [2] = theta out of bounds. Acceptable theta: [0,2pi)
  [3] = z1 out of bounds. Acceptable z1 : (-inf,inf)
  Ref: http://mathworld.wolfram.com/CylindricalCoordinates.html *)
procedure Cylindrical_Cartesian(const r, theta, z1: double; var x, y, z: double;
  var ierr: integer); overload;

(* Convert Cartesian to Cylindrical no checks. Single *)
procedure Cartesian_Cylindrical(const x, y, z1: single; var r, theta, z: single); overload;
(* Convert Cartesian to Cylindrical no checks. Duoble *)
procedure Cartesian_Cylindrical(const x, y, z1: double; var r, theta, z: double); overload;

(* Convert Spherical to Cartesian with no checks. [single] theta,phi in rads
  Ref: http://mathworld.wolfram.com/SphericalCoordinates.html *)
procedure Spherical_Cartesian(const r, theta, phi: single; var x, y, z: single); overload;
(* Convert Spherical to Cartesian with no checks. Double version theta,phi in rads.
  Ref: http://mathworld.wolfram.com/SphericalCoordinates.html *)
procedure Spherical_Cartesian(const r, theta, phi: double; var x, y, z: double); overload;
(* Convert Spherical to Cartesian with checks. theta,phi in rad
  ierr: [0] = ok,
  [1] = r out of bounds
  [2] = theta out of bounds
  [3] = phi out of bounds
  Ref: http://mathworld.wolfram.com/SphericalCoordinates.html *)
procedure Spherical_Cartesian(const r, theta, phi: single; var x, y, z: single;
  var ierr: integer); overload;
(* Convert Spherical to Cartesian with checks. theta,phi in rad
  ierr: [0] = ok,
  [1] = r out of bounds
  [2] = theta out of bounds
  [3] = phi out of bounds
  Ref: http://mathworld.wolfram.com/SphericalCoordinates.html *)
procedure Spherical_Cartesian(const r, theta, phi: double; var x, y, z: double;
  var ierr: integer); overload;

(* convert Cartesian to Spherical, no checks, single
  Ref: http://mathworld.wolfram.com/SphericalCoordinates.html
  NB: Could be optimised by using jclmath.pas unit? *)
procedure Cartesian_Spherical(const x, y, z: single; var r, theta, phi: single); overload;
procedure Cartesian_Spherical(const v: TAffineVector; var r, theta, phi: single); overload;
// Convert cartesion to spherical [double]
(* convert Cartesian to Spherical, no checks, double
  Ref: http://mathworld.wolfram.com/SphericalCoordinates.html
  NB: Could be optimised by using jclmath.pas unit? *)
procedure Cartesian_Spherical(const x, y, z: double; var r, theta, phi: double); overload;

(* Convert Prolate-Spheroidal to Cartesian with no checks. [single] eta, phi in rad
  A system of curvilinear coordinates in which two sets of coordinate surfaces are
  obtained by revolving the curves of the elliptic cylindrical coordinates about
  the x-axis, which is relabeled the z-axis. The third set of coordinates
  consists of planes passing through this axis.
  The coordinate system is parameterised by parameter a. A default value of a=1 is
  suggesed:
  http://documents.wolfram.com/v4/AddOns/StandardPackages/Calculus/VectorAnalysis.html
  Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html *)
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: single;
  var x, y, z: single); overload;
(* Convert Prolate-Spheroidal to Cartesian with no checks. Double version. eta,phi in rad
  A system of curvilinear coordinates in which two sets of coordinate surfaces are
  obtained by revolving the curves of the elliptic cylindrical coordinates about
  the x-axis, which is relabeled the z-axis. The third set of coordinates
  consists of planes passing through this axis.
  The coordinate system is parameterised by parameter a. A default value of a=1 is
  suggesed:
  http://documents.wolfram.com/v4/AddOns/StandardPackages/Calculus/VectorAnalysis.html
  Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html *)
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: double;
  var x, y, z: double); overload;
(* Convert Prolate-Spheroidal to Cartesian with checks. [single] eta,phi in rad
ierr: [0] = ok,
      [1] = xi out of bounds. Acceptable xi: [0,inf)
      [2] = eta out of bounds. Acceptable eta: [0,pi]
      [3] = phi out of bounds. Acceptable phi: [0,2pi)
Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html *)
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: single;
  var x, y, z: single; var ierr: integer); overload;
(* Convert Prolate-Spheroidal to Cartesian with checks. Double Version. eta,phi in rad
ierr: [0] = ok,
      [1] = xi out of bounds. Acceptable xi: [0,inf)
      [2] = eta out of bounds. Acceptable eta: [0,pi]
      [3] = phi out of bounds. Acceptable phi: [0,2pi)
Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html *)
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: double;
  var x, y, z: double;  var ierr: integer); overload;
(* Convert Oblate-Spheroidal to Cartesian with no checks. [Single] eta, phi in rad
A system of curvilinear coordinates in which two sets of coordinate surfaces are
 obtained by revolving the curves of the elliptic cylindrical coordinates about
the y-axis which is relabeled the z-axis. The third set of coordinates consists
of planes passing through this axis.
The coordinate system is parameterised by parameter a. A default value of a=1 is
suggesed:
http://documents.wolfram.com/v4/AddOns/StandardPackages/Calculus/VectorAnalysis.html
Ref: http://mathworld.wolfram.com/OblateSpheroidalCoordinates.html *)
procedure OblateSpheroidal_Cartesian(const xi, eta, phi, a: single;
  var x, y, z: single); overload;
(* Convert Oblate-Spheroidal to Cartesian with no checks. Double Version eta, phi in rad.
A system of curvilinear coordinates in which two sets of coordinate surfaces are
 obtained by revolving the curves of the elliptic cylindrical coordinates about
the y-axis which is relabeled the z-axis. The third set of coordinates consists
of planes passing through this axis.
The coordinate system is parameterised by parameter a. A default value of a=1 is
suggesed:
http://documents.wolfram.com/v4/AddOns/StandardPackages/Calculus/VectorAnalysis.html
Ref: http://mathworld.wolfram.com/OblateSpheroidalCoordinates.html *)
procedure OblateSpheroidal_Cartesian(const xi, eta, phi, a: double;
  var x, y, z: double); overload;
(* Convert Oblate-Spheroidal to Cartesian with checks. eta,phi in rad
ierr: [0] = ok,
      [1] = xi out of bounds. Acceptable xi: [0,inf)
      [2] = eta out of bounds. Acceptable eta: [-0.5*pi,0.5*pi]
      [3] = phi out of bounds. Acceptable phi: [0,2*pi)
Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html *)
procedure OblateSpheroidal_Cartesian(const xi, eta, phi, a: single;
  var x, y, z: single; var ierr: integer); overload;
(* Convert Oblate-Spheroidal to Cartesian with checks. Double Version eta,phi in rad.
  ierr: [0] = ok,
  [1] = xi out of bounds. Acceptable xi: [0,inf)
  [2] = eta out of bounds. Acceptable eta: [-0.5*pi,0.5*pi]
  [3] = phi out of bounds. Acceptable phi: [0,2*pi)
  Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html *)
procedure OblateSpheroidal_Cartesian(const xi, eta, phi, a: double;
  var x, y, z: double; var ierr: integer); overload;
(* Convert BiPolarCylindrical to Cartesian with no checks. u in rad
http://mathworld.wolfram.com/BipolarCylindricalCoordinates.html *)
procedure BipolarCylindrical_Cartesian(const u, v, z1, a: single;
  var x, y, z: single); overload;
(* Convert BiPolarCylindrical to Cartesian with no checks. Double Version u in rad
http://mathworld.wolfram.com/BipolarCylindricalCoordinates.html *)
procedure BipolarCylindrical_Cartesian(const u, v, z1, a: double;
  var x, y, z: double); overload;
(* Convert Oblate-Spheroidal to Cartesian with checks. u in rad
ierr: [0] = ok,
      [1] = u out of bounds. Acceptable u: [0,2*pi)
      [2] = v out of bounds. Acceptable v: (-inf,inf)
      [3] = z1 out of bounds. Acceptable z1: (-inf,inf)
Ref: http://mathworld.wolfram.com/BiPolarCylindricalCoordinates.html *)
procedure BipolarCylindrical_Cartesian(const u, v, z1, a: single;
  var x, y, z: single; var ierr: integer); overload;
(* Convert Oblate-Spheroidal to Cartesian with checks. Double Version u in rad
ierr: [0] = ok,
      [1] = u out of bounds. Acceptable u: [0,2*pi)
      [2] = v out of bounds. Acceptable v: (-inf,inf)
      [3] = z1 out of bounds. Acceptable z1: (-inf,inf)
Ref: http://mathworld.wolfram.com/BiPolarCylindricalCoordinates.html *)
procedure BipolarCylindrical_Cartesian(const u, v, z1, a: double;
  var x, y, z: double; var ierr: integer); overload;

// --------------------------------------------------------------------------
implementation
// --------------------------------------------------------------------------

// ----- Cylindrical_Cartesian ---------------------------------------------
procedure Cylindrical_Cartesian(const r, theta, z1: single; var x, y, z: single);

begin
  SinCosine(theta, r, y, x);
  z := z1;
end;

// ----- Cylindrical_Cartesian -------------------------------------------------
procedure Cylindrical_Cartesian(const r, theta, z1: double; var x, y, z: double);

begin
  SinCosine(theta, r, y, x);
  z := z1;
end;

// ----- Cylindrical_Cartesian -------------------------------------------------
procedure Cylindrical_Cartesian(const r, theta, z1: single; var x, y, z: single;
  var ierr: integer);

begin
  { ** check input parameters }
  if (r < 0.0) then
    ierr := 1
  else if ((theta < 0.0) or (theta >= 2 * pi)) then
    ierr := 2
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    SinCosine(theta, r, y, x);
    z := z1;
  end;
end;

// ----- Cylindrical_Cartesian -------------------------------------------------
procedure Cylindrical_Cartesian(const r, theta, z1: double; var x, y, z: double;
  var ierr: integer);

begin
  // check input parameters
  if (r < 0.0) then
    ierr := 1
  else if ((theta < 0.0) or (theta >= 2 * pi)) then
    ierr := 2
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    SinCosine(theta, r, y, x);
    z := z1;
  end;
end;

// ----- Cartesian_Cylindrical -------------------------------------------------
procedure Cartesian_Cylindrical(const x, y, z1: single; var r, theta, z: single);
begin
  r := sqrt(x * x + y * y);
  theta := ArcTan2(y, x);
  z := z1;
end;

// ----- Cartesian_Cylindrical -------------------------------------------------
procedure Cartesian_Cylindrical(const x, y, z1: double; var r, theta, z: double);
begin
  r := sqrt(x * x + y * y);
  theta := ArcTan2(y, x);
  z := z1;
end;

// ----- Spherical_Cartesian ---------------------------------------------------
procedure Spherical_Cartesian(const r, theta, phi: single; var x, y, z: single);
var
  a: single;
begin
  SinCosine(phi, r, a, z); // z = r*cos(phi), a=r*sin(phi)
  SinCosine(theta, a, y, x); // x = a*cos(theta), y = a*sin(theta)}
end;

// ----- Spherical_Cartesian ---------------------------------------------------
procedure Spherical_Cartesian(const r, theta, phi: double; var x, y, z: double);
var
  a: double;
begin
  SinCosine(phi, r, a, z); // z = r*cos(phi), a=r*sin(phi)
  SinCosine(theta, a, y, x); // x = a*cos(theta), y = a*sin(theta)}
end;

// ----- Spherical_Cartesian ---------------------------------------------------
procedure Spherical_Cartesian(const r, theta, phi: single; var x, y, z: single;
  var ierr: integer);
var
  a: single;

begin
  if (r < 0.0) then
    ierr := 1
  else if ((theta < 0.0) or (theta >= 2 * pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2 * pi)) then
    ierr := 3
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    SinCosine(phi, r, a, z); // z = r*cos(phi), a=r*sin(phi)
    SinCosine(theta, a, y, x); // x = a*cos(theta), y = a*sin(theta)}
  end;
end;

// ----- Spherical_Cartesian ---------------------------------------------------
procedure Spherical_Cartesian(const r, theta, phi: double; var x, y, z: double;
  var ierr: integer);
var
  a: double;
begin
  if (r < 0.0) then
    ierr := 1
  else if ((theta < 0.0) or (theta >= 2 * pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2 * pi)) then
    ierr := 3
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    SinCosine(phi, r, a, z); // z = r*cos(phi), a=r*sin(phi)
    SinCosine(theta, a, y, x); // x = a*cos(theta), y = a*sin(theta)}
  end;
end;

// ----- Cartesian_Spherical ---------------------------------------------------
procedure Cartesian_Spherical(const x, y, z: single; var r, theta, phi: single);
begin
  r := sqrt((x * x) + (y * y) + (z * z));
  theta := ArcTan2(y, x);
  phi := ArcCosine(z / r);
end;

procedure Cartesian_Spherical(const v: TAffineVector; var r, theta, phi: single);
begin
  r := VectorLength(v);
  theta := ArcTan2(v.y, v.x);
  phi := ArcCosine(v.z / r);
end;

// ----- Cartesian_Spherical ---------------------------------------------------
procedure Cartesian_Spherical(const x, y, z: double; var r, theta, phi: double);
begin
  r := sqrt((x * x) + (y * y) + (z * z));
  theta := ArcTan2(y, x);
  phi := ArcCosine(z / r);
end;

// ----- ProlateSpheroidal_Cartesian -------------------------------------------
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: single; var x, y, z: single);
var
  sn, cs, snphi, csphi, shx, chx: single;
begin
  SinCosine(eta, a, sn, cs);
  SinCosine(phi, snphi, csphi);
  shx := sinh(xi);
  chx := cosh(xi);
  x := sn * shx * csphi; // x = a*sin(eta)*sinh(xi)*cos(phi)
  y := sn * shx * snphi; // y = a*sin(eta)*sinh(xi)*sin(phi)
  z := cs * chx; // z = a*cos(eta)*cosh(xi)
end;

// ----- ProlateSpheroidal_Cartesian -------------------------------------------
procedure ProlateSpheroidal_Cartesian(const xi, eta, phi, a: double; var x, y, z: double);
var
  sn, cs, snphi, csphi, shx, chx: double;
begin
  SinCosine(eta, a, sn, cs);
  SinCosine(phi, snphi, csphi);
  shx := sinh(xi);
  chx := cosh(xi);
  x := sn * shx * csphi; // x = a*sin(eta)*sinh(xi)*cos(phi)
  y := sn * shx * snphi; // y = a*sin(eta)*sinh(xi)*sin(phi)
  z := cs * chx; // z = a*cos(eta)*cosh(xi)
end;

// ----- ProlateSpheroidal_Cartesian -------------------------------------------
procedure ProlateSpheroidal_Cartesian(const xi,eta,phi,a:single;
  var x,y,z:single; var ierr:integer);overload;
var
  sn, cs, snphi, csphi, shx, chx: single;
begin
  if (xi < 0.0) then
    ierr := 1
  else if ((eta < 0.0) or (eta > pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2 * pi)) then
    ierr := 3
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    SinCosine(eta, a, sn, cs);
    SinCosine(phi, snphi, csphi);

    shx := sinh(xi);
    chx := cosh(xi);

    x := sn * shx * csphi; // x = a*sin(eta)*sinh(xi)*cos(phi)
    y := sn * shx * snphi; // y = a*sin(eta)*sinh(xi)*sin(phi)
    z := cs * chx; // z = a*cos(eta)*cosh(xi)
  end;
end;

// ----- ProlateSpheroidal_Cartesian -------------------------------------------
procedure ProlateSpheroidal_Cartesian(const xi,eta,phi,a:double;
  var x,y,z:double; var ierr:integer);overload;
var
  sn, cs, snphi, csphi, shx, chx: double;
begin
  if (xi < 0.0) then
    ierr := 1
  else if ((eta < 0.0) or (eta > pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2 * pi)) then
    ierr := 3
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    SinCosine(eta, a, sn, cs);
    SinCosine(phi, snphi, csphi);

    shx := sinh(xi);
    chx := cosh(xi);

    x := sn * shx * csphi; // x = a*sin(eta)*sinh(xi)*cos(phi)
    y := sn * shx * snphi; // y = a*sin(eta)*sinh(xi)*sin(phi)
    z := cs * chx; // z = a*cos(eta)*cosh(xi)
  end;
end;

// ----- OblateSpheroidal_Cartesian -------------------------------------------
procedure OblateSpheroidal_Cartesian(const xi,eta,phi,a:single;var x,y,z:single);
var
  sn, cs, snphi, csphi, shx, chx: single;
begin
  SinCosine(eta, a, sn, cs);
  SinCosine(phi, snphi, csphi);

  shx := sinh(xi);
  chx := cosh(xi);

  x := cs * chx * csphi; // x = a*cos(eta)*cosh(xi)*cos(phi)
  y := cs * chx * snphi; // y = a*cos(eta)*cosh(xi)*sin(phi)
  z := sn * shx; // z = a*sin(eta)*sinh(xi)
end;

// ----- OblateSpheroidal_Cartesian -------------------------------------------
procedure OblateSpheroidal_Cartesian(const xi,eta,phi,a:double;var x,y,z:double);
var
  sn,cs,snphi,csphi,shx,chx : double;
begin
  SinCosine(eta,a,sn,cs);
  SinCosine(phi,snphi,csphi);

  shx := sinh(xi);
  chx := cosh(xi);

  x := cs * chx * csphi; // x = a*cos(eta)*cosh(xi)*cos(phi)
  y := cs * chx * snphi; // y = a*cos(eta)*cosh(xi)*sin(phi)
  z := sn * shx; // z = a*sin(eta)*sinh(xi)
end;

// ----- OblateSpheroidal_Cartesian -------------------------------------------
procedure OblateSpheroidal_Cartesian(const xi,eta,phi,a:single;
  var x,y,z:single; var ierr:integer);overload;
var
  sn, cs, snphi, csphi, shx, chx: single;
begin
  if (xi < 0.0) then
    ierr := 1
  else if ((eta < -0.5 * pi) or (eta > 0.5 * pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2 * pi)) then
    ierr := 3
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    SinCosine(eta, a, sn, cs);
    SinCosine(phi, snphi, csphi);

    shx := sinh(xi);
    chx := cosh(xi);

    x := cs * chx * csphi; // x = a*cos(eta)*cosh(xi)*cos(phi)
    y := cs * chx * snphi; // y = a*cos(eta)*cosh(xi)*sin(phi)
    z := sn * shx; // z = a*sin(eta)*sinh(xi)
  end;
end;

// ----- OblateSpheroidal_Cartesian -------------------------------------------
procedure OblateSpheroidal_Cartesian(const xi, eta, phi, a: double;
  var x, y, z: double; var ierr: integer); overload;
var
  sn, cs, snphi, csphi, shx, chx: double;
begin
  if (xi < 0.0) then
    ierr := 1
  else if ((eta < -0.5 * pi) or (eta > 0.5 * pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2 * pi)) then
    ierr := 3
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    SinCosine(eta, a, sn, cs);
    SinCosine(phi, snphi, csphi);

    shx := sinh(xi);
    chx := cosh(xi);

    x := cs * chx * csphi; // x = a*cos(eta)*cosh(xi)*cos(phi)
    y := cs * chx * snphi; // y = a*cos(eta)*cosh(xi)*sin(phi)
    z := sn * shx; // z = a*sin(eta)*sinh(xi)
  end;
end;

// ----- BipolarCylindrical_Cartesian ------------------------------------------
procedure BipolarCylindrical_Cartesian(const u,v,z1,a:single;var x,y,z:single);
var
  cs, sn, shx, chx: single;
begin
  SinCosine(u, sn, cs);
  shx := sinh(v);
  chx := cosh(v);

  x := a * shx / (chx - cs);
  y := a * sn / (chx - cs);
  z := z1;
end;

// ----- BipolarCylindrical_Cartesian ------------------------------------------
procedure BipolarCylindrical_Cartesian(const u,v,z1,a:double;var x,y,z:double);
var
  cs, sn, shx, chx: double;
begin
  SinCosine(u, sn, cs);
  shx := sinh(v);
  chx := cosh(v);

  x := a * shx / (chx - cs);
  y := a * sn / (chx - cs);
  z := z1;
end;

// ----- BipolarCylindrical_Cartesian ------------------------------------------
procedure BipolarCylindrical_Cartesian(const u,v,z1,a:single;
  var x,y,z:single; var ierr:integer);overload;
var
  cs, sn, shx, chx: single;
begin
  if ((u < 0.0) or (u >= 2 * pi)) then
    ierr := 1
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    SinCosine(u, sn, cs);

    shx := sinh(v);
    chx := cosh(v);

    x := a * shx / (chx - cs);
    y := a * sn / (chx - cs);
    z := z1;
  end;
end;

// ----- BipolarCylindrical_Cartesian ------------------------------------------
procedure BipolarCylindrical_Cartesian(const u,v,z1,a:double;
  var x,y,z:double; var ierr:integer);overload;
var
  cs, sn, shx, chx: double;
begin
  if ((u < 0.0) or (u >= 2 * pi)) then
    ierr := 1
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    SinCosine(u, sn, cs);
    shx := sinh(v);
    chx := cosh(v);

    x := a * shx / (chx - cs);
    y := a * sn / (chx - cs);
    z := z1;
  end;
end;

// =============================================================================
end.


