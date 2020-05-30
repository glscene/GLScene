{:
  06 June 2009
  This FFT demo uses Borland Delpi 7 on Windows XP. 
  Code from http://www.vssoft.bg.
  Contact at vstoykov@consultant.bg.
}
unit CPUFFT;

interface

uses
  SysUtils, Types, Math;

type
  TProcessMode = (module, phase, real, imag);

  TComplex = record
    real, imag: Double;
  end;

  PComplexArray = ^TComplexDynArray;
  TComplexDynArray = array of TComplex;
  TComplex2DArray = array of TComplexDynArray;
  TByte2DArray = array of TByteDynArray;

  // perform fft (sgn=1) or inverse fft (sgn=-1) on cx[nx]
procedure ftu(sgn: TValueSign; nx: integer; cx: TComplexDynArray); overload;
// perform 2Dfft (sgn=1) or inverse 2Dfft (sgn=-1) on cx[len,len]
procedure ftu(sgn: TValueSign; len: integer; cArr: TComplex2DArray); overload;
// reorder quadrants to move 0 to the center (after fft on real signal)
procedure Reorder(inp: TComplexDynArray); overload; // 1D
procedure Reorder(inp: TComplex2DArray);
overload // 2D
// normalize cArr to [0,255] using function defined by mode and store result in buf
// on return, cArr.Real contains result before normalization
  procedure Normalize(cArr: TComplexDynArray; buf: TByteDynArray;
  mode: TProcessMode);
overload; // 1D
procedure Normalize(cArr: TComplex2DArray; buf: TByte2DArray;
  mode: TProcessMode); overload; // 2D

implementation

// some complex arithmetic
function CAdd(c1, c2: TComplex): TComplex;
begin
  Result.Real := c1.Real + c2.Real;
  Result.imag := c1.imag + c2.imag;
end;

function CSubt(c1, c2: TComplex): TComplex;
begin
  Result.Real := c1.Real - c2.Real;
  Result.imag := c1.imag - c2.imag;
end;

function CMult(c1, c2: TComplex): TComplex; overload;
begin
  Result.Real := c1.Real * c2.Real - c1.imag * c2.imag;
  Result.imag := c1.Real * c2.imag + c1.imag * c2.Real;
end;

function CMult(c: TComplex; i: integer): TComplex; overload;
begin
  Result.Real := c.Real * i;
  Result.imag := c.imag * i;
end;

function CMult(c: TComplex; d: Double): TComplex; overload;
begin
  Result.Real := c.Real * d;
  Result.imag := c.imag * d;
end;

// return y = 2^i >= n
function pad2(n: integer): integer;
begin
  Result := 1;
  while (Result < n) do
    Result := Result * 2;
end;

{
  Complex fourier transform with unitary scaling:

  1       nx-1          sgn*2*pi*i * j*(k-1)/nx
  cx(k)  =  -------- * sum cx(j) * e
  sqrt(nx)   j=0                          for k=1,2,...,nx=2**integer

  In some applications the scale factor 1/sqrt(nx) is omitted in fft: factor=1,
  wheras in the inverse fft, it is set to: factor=1/nx.
}
// Apply fft/ifft on cArr[nx] and store result in cArr:
procedure ftu(sgn: TValueSign; nx: integer; cx: TComplexDynArray);
var
  scale, arg: Double;
  cw, cdel, ct: TComplex;
  i, j, k, m, istep: integer;
begin
  if (nx <> pad2(nx)) then
    raise Exception.Create('ftu: number of samples is not a power of 2');
  scale := 1 / sqrt(nx);
  for i := 0 to nx - 1 do
    cx[i] := CMult(cx[i], scale);

  j := 0;
  for i := 0 to nx - 1 do
  begin
    if (i <= j) then
    begin
      ct := cx[j];
      cx[j] := cx[i];
      cx[i] := ct;
    end;
    m := nx div 2;
    while ((j >= m) and (m > 1)) do
    begin
      j := j - m;
      m := m div 2;
    end;
    j := j + m;
  end;

  k := 1;
  repeat
    istep := 2 * k;
    cw.Real := 1;
    cw.imag := 0;
    arg := sgn * Pi / k;
    cdel.Real := cos(arg);
    cdel.imag := sin(arg);

    for m := 0 to k - 1 do
    begin
      i := m;
      while i < nx do
      begin // for i=m to nx step istep
        ct := CMult(cw, cx[i + k]);
        cx[i + k] := CSubt(cx[i], ct);
        cx[i] := CAdd(cx[i], ct);
        Inc(i, istep);
      end;
      cw := CMult(cw, cdel);
    end;

    k := istep;
  until (k >= nx);
end;

{ first apply 1D ftu on cArr rows and then on cArr cols; store result in cArr }
procedure ftu(sgn: TValueSign; len: integer; cArr: TComplex2DArray);
var
  i, j: integer;
  tmp: TComplexDynArray;
begin
  // apply forward fft
  for i := 0 to len - 1 do // fft rows: up-down
    ftu(sgn, len, cArr[i]);
  // fft cols: left-right; uses tmp[rows]
  SetLength(tmp, len);
  for i := 0 to len - 1 do
  begin // for each col
    for j := 0 to len - 1 do // copy the i-th col of inp[rows,cols] to tmp[rows]
      tmp[j] := cArr[j, i];
    ftu(sgn, len, tmp); // fft on the i-th col
    for j := 0 to len - 1 do // copy result back from tmp to inp
      cArr[j, i] := tmp[j];
  end;
end;

procedure Reorder(inp: TComplexDynArray); overload; // 1D
var
  tmp: TComplexDynArray;
  i, h, w: integer;
begin
  w := Length(inp);
  h := w div 2;
  SetLength(tmp, h);
  for i := 0 to h - 1 do
    tmp[i] := inp[i + h];
  for i := 0 to h - 1 do
    inp[i + h] := inp[i];
  for i := 0 to h - 1 do
    inp[i] := tmp[i];
end;

procedure Reorder(inp: TComplex2DArray);
overload // 2D
var
  tmp: TComplex2DArray;
  i, j, h, w: integer;
begin
  w := Length(inp);
  h := w div 2;
  SetLength(tmp, h, h);
  // left,up->tmp
  for i := 0 to h - 1 do
    for j := 0 to h - 1 do
      tmp[i, j] := inp[i, j];
  // down,right->left,up
  for i := 0 to h - 1 do
    for j := 0 to h - 1 do
      inp[i, j] := inp[i + h, j + h];
  // tmp->down,right
  for i := 0 to h - 1 do
    for j := 0 to h - 1 do
      inp[i + h, j + h] := tmp[i, j];

  // right,up->tmp
  for i := 0 to h - 1 do
    for j := 0 to h - 1 do
      tmp[i, j] := inp[i + h, j];
  // down,left->right,up
  for i := 0 to h - 1 do
    for j := 0 to h - 1 do
      inp[i + h, j] := inp[i, j + h];
  // tmp->down,left
  for i := 0 to h - 1 do
    for j := 0 to h - 1 do
      inp[i, j + h] := tmp[i, j];

end;

procedure Normalize(cArr: TComplexDynArray; buf: TByteDynArray;
  mode: TProcessMode); // 1D
var
  j, len: integer;
  m, n, r: Double;
begin
  len := Length(cArr); // len := Length(cArr)-1;
  // find max and min values of cArr to normalize result
  m := MinDouble;
  n := MaxDouble;
  for j := 0 to len - 1 do
  begin // for j:=1 to len do begin
    case mode of
      module:
        r := Hypot(cArr[j].Real, cArr[j].imag);
      phase:
        r := ArcTan2(cArr[j].imag, cArr[j].Real);
      real:
        r := cArr[j].Real;
    else
      r := cArr[j].imag;
    end;
    if m < r then
      m := r;
    if n > r then
      n := r;
    // reuse real part of cArr to store result
    cArr[j].Real := r;
  end;
  // normalize result in cArr.Real to [0,255] and copy it to buf
  for j := 0 to len - 1 do
    buf[j] := Trunc(255 * (cArr[j].Real - n) / (m - n));
end;

procedure Normalize(cArr: TComplex2DArray; buf: TByte2DArray;
  mode: TProcessMode); // 2D
var
  i, j, len: integer;
  m, n, r: Double;
begin
  len := Length(cArr);
  m := MinDouble;
  n := MaxDouble;
  for i := 0 to len - 1 do
    for j := 0 to len - 1 do
    begin
      case mode of
        module:
          r := Hypot(cArr[i, j].Real, cArr[i, j].imag);
        phase:
          r := ArcTan2(cArr[i, j].imag, cArr[i, j].Real);
        real:
          r := cArr[i, j].Real;
      else
        r := cArr[i, j].imag;
      end;
      if m < r then
        m := r;
      if n > r then
        n := r;
      cArr[i, j].Real := r;
    end;
  for i := 0 to len - 1 do // move cArr[Real] -> buf
    for j := 0 to len - 1 do
      buf[i, j] := Trunc(255 * (cArr[i, j].Real - n) / (m - n));
end;

end.
