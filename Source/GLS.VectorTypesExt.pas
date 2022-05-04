//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.VectorTypesExt;

interface

{ .$I GLScene.inc }

uses
  System.Types,
  System.TypInfo,
  System.SysUtils,
  System.Rtti,
  System.Math,
  System.Math.Vectors,
  GLS.VectorTypes;

type
  // Extended may be replaced with BigDecimals
  TIntegerArray = array of Integer;
  TVertexArray = array of TVector3f;

  TVectorExt = array of Extended;
  TMatrixExt = array of array of Extended;

  TArray3DExt = array of array of array of Extended;

  TQuaternionRec = record
  private
    FData: array [0 .. 3] of Extended;
    procedure SetElement(Index: Byte; Value: Extended);
    function GetElement(Index: Byte): Extended;
  public
    constructor Create(Q: TVectorExt);
    class operator Multiply(Q1, Q2: TQuaternionRec): TQuaternionRec;
    class operator Multiply(Q: TQuaternionRec; Sc: Extended): TQuaternionRec;
    class operator Multiply(Scalar: Extended; Q: TQuaternionRec)
      : TQuaternionRec;
    class operator Implicit(V: TVectorExt): TQuaternionRec;
    function Inv: TQuaternionRec;
    function TruncateSTI: TQuaternionRec;
    property Element[index: Byte]: Extended read GetElement
      write SetElement; default;
  end;

  PVectorRec = ^TVectorRec;

  TVectorRec = record
  private
    FData: TVectorExt;
    FCount: Word;
    procedure SetElement(Index: Word; Value: Extended);
    function GetElement(Index: Word): Extended;
    procedure CheckUnique;
  public
    constructor Create(ElementsCount: Word); overload;
    constructor Create(V: TVectorExt); overload;
    class operator Add(V1, V2: TVectorRec): TVectorRec;
    class operator Add(V: TVectorRec; Scalar: Extended): TVectorRec;
    class operator Add(Scalar: Extended; V: TVectorRec): TVectorRec;
    class operator Subtract(V1, V2: TVectorRec): TVectorRec;
    class operator Subtract(Scalar: Extended; V: TVectorRec): TVectorRec;
    class operator Subtract(V: TVectorRec; Scalar: Extended): TVectorRec;
    class operator Multiply(V1, V2: TVectorRec): TVectorRec;
    class operator Multiply(V: TVectorRec; Scalar: Extended): TVectorRec;
    class operator Multiply(Scalar: Extended; V: TVectorRec): TVectorRec;
    class operator Divide(V: TVectorRec; Scalar: Extended): TVectorRec;
    class operator Divide(V1, V2: TVectorRec): TVectorRec;
    class operator Implicit(V: TVectorExt): TVectorRec;
    function Norm: Extended;
    function SumOfSquares: Extended;
    function SumOfElments: Extended;
    function TruncateSTI: TVectorRec;
    function ToQuat: TQuaternionRec;
    procedure Fill(Value: Extended);
    function ScalarMult(V: TVectorRec): Extended;
    property Count: Word read FCount;
    property Elements[index: Word]: Extended read GetElement
      write SetElement; default;
  end;

  PMatrixRec = ^TMatrixRec;

  TMatrixRec = record
  private
    FData: TMatrixExt;
    FRowsCount: Word;
    FColsCount: Word;
    procedure SetElement(Row, Col: Word; Value: Extended);
    function GetElement(Row, Col: Word): Extended;
    function GetRow(Row: Word): TVectorRec;
    procedure SetRow(Row: Word; Value: TVectorRec);
    function GetCol(Col: Word): TVectorRec;
    procedure SetCol(Col: Word; Value: TVectorRec);
    function Del(A: TMatrixRec; I, J: Integer; M: Integer): TMatrixRec;
    function Det(A: TMatrixRec; M: Integer): Extended;
    procedure CheckUnique;
  public
    constructor Create(RowsCount, ColsCount: Word); overload;
    constructor CreateDiag(Dim: Word; Value: Extended = 1.0);
    constructor Create(M: TMatrixExt); overload;
    class operator Add(M1, M2: TMatrixRec): TMatrixRec;
    class operator Subtract(M1, M2: TMatrixRec): TMatrixRec;
    class operator Multiply(M1, M2: TMatrixRec): TMatrixRec;
    class operator Multiply(M: TMatrixRec; V: TVectorRec): TVectorRec;
    class operator Multiply(V: TVectorRec; M: TMatrixRec): TVectorRec;
    class operator Multiply(M: TMatrixRec; Scalar: Extended): TMatrixRec;
    class operator Multiply(Scalar: Extended; M: TMatrixRec): TMatrixRec;
    class operator Multiply(M: TMatrixRec; Q: TQuaternionRec): TQuaternionRec;
    class operator Implicit(M: TMatrixExt): TMatrixRec;
    function Transp: TMatrixRec;
    function Inv: TMatrixRec;
    function ToQuat: TQuaternionRec;
    function Determinant: Extended;
    function TruncateSTI: TMatrixRec;
    function Trace: Extended; // sum on diagonal elements
    procedure Fill(Scalar: Extended);
    property RowCount: Word read FRowsCount;
    property ColCount: Word read FColsCount;
    property Row[Row: Word]: TVectorRec read GetRow write SetRow;
    property Col[Col: Word]: TVectorRec read GetCol write SetCol;
    property Elements[Row, Col: Word]: Extended read GetElement
      write SetElement; default;
  end;

  TQuaternionHelper = record helper for TQuaternionRec
    function ToMatrix: TMatrixRec;
  end;

  TVectorHelper = record helper for TVectorRec
    function ToDiagMatrix: TMatrixRec;
  end;

  TDim = class(TCustomAttribute)
  private
    FRowCount: Integer;
    FColCount: Integer;
  public
    constructor Create(ARowCount: Integer; AColCount: Integer = 0); overload;
    property RowCount: Integer read FRowCount;
    property ColCount: Integer read FColCount;
  end;

function GetVectorRec(V: TVectorExt): TVectorRec;
function GetMatrixRec(M: TMatrixExt): TMatrixRec;
function GetQuaternionRec(Q: TVectorExt): TQuaternionRec;
procedure Init(Obj, TypeInfoOfObj: Pointer; Offset: Integer = 0);

// -----------------------
// Point types
// -----------------------
type
  TGLScalarValue = Extended;
  TGLScalarField = function(X, Y, Z: Extended): TGLScalarValue;

  // if data are made on integer XYZ index may be replaced with BigIntegers
  TGLScalarFieldInt = function(iX, iY, iZ: Integer): TGLScalarValue of object;

  TVertexRec = record
    P, N: TVector3f; // Position and Normal
    Density: Extended;
  end;

  TFaceRec = record
    Normal: TVector3f;
    V1: TVector3f; // vertex 1
    V2: TVector3f; // vertex 2
    V3: TVector3f; // vertex 3
    Padding: array [0 .. 1] of Byte;
  end;

  PPoint2DRec = ^TPoint2DRec;

  TPoint2DRec = record
    X: Extended;
    Y: Extended;
  public
    function Create(X, Y: Extended): TPoint2DRec;
    procedure SetPosition(const X, Y: Extended);
    function Add(const APoint2D: TPoint2DRec): TPoint2DRec;
    function Length: Extended; // distance to origin
    function Distance(const APoint2D: TPoint2DRec): Extended;
    class function PointInCircle(const Point, Center: TPoint2DRec;
      const Radius: Integer): Boolean; static; inline;
    procedure Offset(const ADeltaX, ADeltaY: Extended);
  end;

  PPoint3DRec = ^TPoint3DRec;

  TPoint3DRec = record
    X: Extended;
    Y: Extended;
    Z: Extended;
  public
    function Create(X, Y, Z: Extended): TPoint3DRec;
    procedure SetPosition(const X, Y, Z: Extended);
    function Add(const AGLPoint3D: TPoint3DRec): TPoint3DRec;
    function Length: Single; // distance to origin
    function Distance(const APoint3D: TPoint3DRec): Extended;
    procedure Offset(const ADeltaX, ADeltaY, ADeltaZ: Extended);
  end;

  TPoint2DArray = array of TPoint2DRec;
  TPoint3DArray = array of TPoint3DRec;

  // Voxel types
  TVoxelStatus = (bpExternal, bpInternal);

  PVoxelRec = ^TVoxelRec;

  TVoxelRec = record
    P: TVector3f;
    Density: TGLScalarValue;
    Status: TVoxelStatus;
  end;

  TVoxelData = array [0 .. (MaxInt shr 8)] of TVoxelRec;
  PVoxelData = ^TVoxelData;

  // -----------------------
  // Vector types
  // -----------------------

  TVector2DType = array [0 .. 1] of Extended;
  TVector3DType = array [0 .. 2] of Extended;

  TVector2DRec = record
    function Create(const AX, AY, AW: Single): TVector2DRec;
    function Add(const AVector2D: TVector2DRec): TVector2DRec;
    function Length: Extended;
    function Norm: Extended;
    function Normalize: TVector2DRec;
    function CrossProduct(const AVector: TVector2DRec): TVector2DRec;
    function DotProduct(const AVector: TVector2DRec): Extended;
    case Integer of
      0:
        (V: TVector2DType;);
      1:
        (X: Extended;
          Y: Extended;
          W: Extended;)
  end;

  TVector3DRec = record
    function Create(const AX, AY, AZ, AW: Single): TVector3DRec;
    function Add(const AVector3D: TVector3DRec): TVector3DRec;
    function Length: Single;
    function Norm: Single;
    function Normalize: TVector3DRec;
    function CrossProduct(const AVector3D: TVector3D): TVector3D;
    function DotProduct(const AVector3D: TVector3D): Single; inline;
    case Integer of
      0:
        (V: TVector3DType;);
      1:
        (X: Extended;
          Y: Extended;
          Z: Extended;
          W: Extended;)
  end;

  // Vector Arrays
  TVector2DArray = array of TVector2DRec;
  TVector3DArray = array of TVector3DRec;

  // -----------------------
  // Matrix types
  // -----------------------
  TMatrix2DType = array [0 .. 3] of TVector2DRec;
  TMatrix3DType = array [0 .. 3] of TVector3DRec;

  TMatrix2DRec = record
  private
  public
    case Integer of
      0:
        (M: TMatrix2DType;);
      1:
        (e11, e12, e13: Single;
          e21, e22, e23: Single;
          e31, e32, e33: Single);
  end;

  TMatrix3DRec = record
  private
  public
    case Integer of
      0:
        (M: TMatrix3DType;);
      1:
        (e11, e12, e13, e14: Single;
          e21, e22, e23, e24: Single;
          e31, e32, e33, e34: Single;
          e41, e42, e43, e44: Single);
  end;

  // Matrix Arrays
  TMatrix2DArray = array of TMatrix2DRec;
  TMatrix3DArray = array of TMatrix3DRec;


  // -----------------------
  // Polygon types
  // -----------------------

  TPolygon2D = TPoint2DArray;
  TPolygon3D = TPoint3DArray;

  (*
    TPolygon3D = record
    Vertices: array of TPoint3DRec;
    function Length;
    end;
  *)

const
  ClosedPolygon2D: TPoint2DRec = (X: $FFFF; Y: $FFFF);
  ClosedPolygon3D: TPoint3DRec = (X: $FFFF; Y: $FFFF; Z: $FFFF);

type
  PVertArray = ^TVertArray;
  TVertArray = array [0 .. (MaxInt shr 8)] of TVertexRec;

type
  TTriangleRec = record
    V1, V2, V3: Integer;
    /// Vertices: array[0..2] of TPoint3DRec;
    /// function Area;
  end;

  PTriangleRecArray = ^TTriangleRecArray;
  TTriangleRecArray = array [0 .. (MaxInt shr 8)] of TTriangleRec;

  // -----------------------
  // Polyhedron types
  // -----------------------
type
  TPolyhedronArray = array of TPolygon3D;

  (*
    TPolyhedron = record
    Facets: array of TGLPolygon3D;
    function NetLength;
    function Area;
    function Volume;
    end;
  *)

  // --------------------------
  // Mesh simple record types
  // --------------------------
type
  TMesh2DVert = record
    X, Y: Single;
    NX, NY: Single;
    tU, tV: Single;
  end;

  TMesh3DVert = packed record
    X, Y, Z: Single;
    NX, NY, NZ: Single;
    tU, tV: Single;
  end;

  TMesh2DArray = array of TMesh2DVert;
  TMesh3DArray = array of TMesh3DVert;

  // --------------------------
  // Quaternion simple record types
  // --------------------------
type
  TQuat3DRec = record
    ImPart: TVector3DRec;
    RePart: Single;
  end;

  TQuatArray = array of TQuat3DRec;

type
  TBoxRec = record
    ALeft, ATop, ANear, ARight, ABottom, AFar: Single;
  end;

const
  sWRONG_ELEMENT = 'Wrong element';
  sWRONG_SIZE = 'Wrong size';
  sNOT_QUAD = 'Matrix not quadratic';
  sSINGULAR = 'Singular matrix founded';

  // ---------------------------------------------------------------
implementation

// ---------------------------------------------------------------

function GetVectorRec(V: TVectorExt): TVectorRec;
begin
  Result.Create(V);
end;

function GetMatrixRec(M: TMatrixExt): TMatrixRec;
begin
  Result.Create(M);
end;

function GetQuaternionRec(Q: TVectorExt): TQuaternionRec;
begin
  Result.Create(Q);
end;

{$POINTERMATH ON}

function NotUnique(PArr: PCardinal): Boolean;
begin
  Result := (PArr - 2)^ > 1;
end;

// -------------------------------------
// TMatrixRec
// -------------------------------------

// Removing i-th row and j-th col
function TMatrixRec.Del(A: TMatrixRec; I, J: Integer; M: Integer): TMatrixRec;
var
  K, G: Integer;
begin
  for G := I to M - 1 do
    for K := 1 to M do
      A[G, K] := A[G + 1, K];
  for G := J to M - 1 do
    for K := 1 to M - 1 do
      A[K, G] := A[K, G + 1];
  Result := A;
end;

// Recursive calculation of det for matrix
function TMatrixRec.Det(A: TMatrixRec; M: Integer): Extended;
var
  I: Integer;
  Buf: Extended;
begin
  Buf := 0;
  if M = 1 then
    Buf := A[1, 1]
  else
    for I := 1 to M do
      Buf := Buf + Power10(-1, I + 1) * A[I, 1] * Det(Del(A, I, 1, M), M - 1);
  Result := Buf;
end;

class operator TMatrixRec.Add(M1, M2: TMatrixRec): TMatrixRec;
var
  I, J: Integer;
begin
  if (M1.FRowsCount <> M2.FRowsCount) or (M1.FColsCount <> M2.FColsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(M1.FRowsCount, M1.FColsCount);
  for I := 0 to M1.FRowsCount - 1 do
    for J := 0 to M1.FColsCount - 1 do
      Result.FData[I, J] := M1.FData[I, J] + M2.FData[I, J];
end;

procedure TMatrixRec.CheckUnique;
var
  I: Integer;
begin
  if NotUnique(@FData) then
  begin
    FData := Copy(FData);
    for I := 0 to Pred(FRowsCount) do
      FData[I] := Copy(FData[I]);
  end;
end;

constructor TMatrixRec.Create(RowsCount, ColsCount: Word);
begin
  FRowsCount := RowsCount;
  FColsCount := ColsCount;
  FData := nil;
  SetLength(FData, FRowsCount, FColsCount);
end;

constructor TMatrixRec.Create(M: TMatrixExt);
var
  I: Integer;
begin
  FRowsCount := Length(M);
  FColsCount := Length(M[0]);
  FData := nil;
  SetLength(FData, FRowsCount, FColsCount);
  for I := 0 to Pred(FRowsCount) do
  begin
    if Length(M[I]) <> FColsCount then
      raise EMathError.Create('Wrong matrix proportions');
    FData[I] := Copy(M[I]);
  end;
end;

constructor TMatrixRec.CreateDiag(Dim: Word; Value: Extended = 1.0);
var
  I: Integer;
begin
  Create(Dim, Dim);
  for I := 0 to Dim - 1 do
    FData[I, I] := Value;
end;

function TMatrixRec.Determinant: Extended;
begin
  if (FRowsCount <> FColsCount) then
    raise EMathError.Create(sNOT_QUAD);
  Result := Det(Self, FRowsCount);
end;

procedure TMatrixRec.Fill(Scalar: Extended);
var
  I, J: Integer;
begin
  if Scalar = 0 then
  begin
    FData := nil;
    SetLength(FData, FRowsCount, FColsCount);
  end
  else
    for I := 0 to FRowsCount - 1 do
      for J := 0 to FColsCount - 1 do
        FData[I, J] := Scalar;
end;

function TMatrixRec.GetCol(Col: Word): TVectorRec;
var
  I: Integer;
begin
  if (Col = 0) or (Col > FColsCount) then
    raise EMathError.Create(sWRONG_ELEMENT);
  Result.Create(FRowsCount);
  for I := 0 to FRowsCount - 1 do
    Result.FData[I] := FData[I, Col - 1];
end;

function TMatrixRec.GetElement(Row, Col: Word): Extended;
begin
{$R+}
  Result := FData[Pred(Row), Pred(Col)];
end;

function TMatrixRec.GetRow(Row: Word): TVectorRec;
var
  I: Integer;
begin
  if (Row = 0) or (Row > FRowsCount) then
    raise EMathError.Create(sWRONG_ELEMENT);
  Result.Create(FColsCount);
  for I := 0 to FColsCount - 1 do
    Result.FData[I] := FData[Row - 1, I];
end;

class operator TMatrixRec.Implicit(M: TMatrixExt): TMatrixRec;
begin
  Result.Create(M);
end;

// --------------------------------------------------------

function TMatrixRec.Inv: TMatrixRec;
var
  Ipiv, Indxr, Indxc: array of Integer;
  DimMat, I, J, K, L, N, ICol, IRow: Integer;
  Big, Dum, Pivinv: Extended;
begin
  // Jordan algorithm
  if (FRowsCount <> FColsCount) then
    raise EMathError.Create(sNOT_QUAD);
  Result := Self;
  DimMat := FRowsCount;
  SetLength(Ipiv, DimMat);
  SetLength(Indxr, DimMat);
  SetLength(Indxc, DimMat);
  IRow := 1;
  ICol := 1;
  for I := 1 to DimMat do
  begin
    Big := 0;
    for J := 1 to DimMat do
      if (Ipiv[J - 1] <> 1) then
        for K := 1 to DimMat do
          if (Ipiv[K - 1] = 0) then
            if (Abs(Result[J, K]) >= Big) then
            begin
              Big := Abs(Result[J, K]);
              IRow := J;
              ICol := K;
            end;
    Ipiv[ICol - 1] := Ipiv[ICol - 1] + 1;
    if (IRow <> ICol) then
      for L := 1 to DimMat do
      begin
        Dum := Result[IRow, L];
        Result[IRow, L] := Result[ICol, L];
        Result[ICol, L] := Dum;
      end;
    Indxr[I - 1] := IRow;
    Indxc[I - 1] := ICol;
    if Result[ICol, ICol] = 0 then
      raise EMathError.Create(sSINGULAR);
    Pivinv := 1.0 / Result[ICol, ICol];
    Result[ICol, ICol] := 1.0;
    for L := 1 to DimMat do
      Result[ICol, L] := Result[ICol, L] * Pivinv;
    for N := 1 to DimMat do
      if (N <> ICol) then
      begin
        Dum := Result[N, ICol];
        Result[N, ICol] := 0.0;
        for L := 1 to DimMat do
          Result[N, L] := Result[N, L] - Result[ICol, L] * Dum;
      end;
  end;
  for L := DimMat downto 1 do
    if (Indxr[L - 1] <> Indxc[L - 1]) then
      for K := 1 to DimMat do
      begin
        Dum := Result[K, Indxr[L - 1]];
        Result[K, Indxr[L - 1]] := Result[K, Indxc[L - 1]];
        Result[K, Indxc[L - 1]] := Dum;
      end;
end;

function TMatrixRec.ToQuat: TQuaternionRec;
begin
  Result[0] := 0.5 * Sqrt(Abs(1 + Self[1, 1] + Self[2, 2] + Self[3, 3]));
  Result[1] := 0.5 * Sqrt(Abs(1 + Self[1, 1] - Self[2, 2] - Self[3, 3]));
  if Self[3, 2] < Self[2, 3] then
    Result[1] := -Result[1];
  Result[2] := 0.5 * Sqrt(Abs(1 - Self[1, 1] + Self[2, 2] - Self[3, 3]));
  if Self[1, 3] < Self[3, 1] then
    Result[2] := -Result[2];
  Result[3] := 0.5 * Sqrt(Abs(1 - Self[1, 1] - Self[2, 2] + Self[3, 3]));
  if Self[2, 1] < Self[1, 2] then
    Result[3] := -Result[3];
end;

class operator TMatrixRec.Multiply(M: TMatrixRec; Q: TQuaternionRec)
  : TQuaternionRec;
var
  I, J: Integer;
begin
  if (M.FRowsCount <> 4) or (M.FRowsCount <> M.FColsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  FillChar(Result.FData, SizeOf(Result.FData), 0);
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result.FData[I] := Result.FData[I] + M.FData[I, J] * Q.FData[J];
end;

class operator TMatrixRec.Multiply(Scalar: Extended; M: TMatrixRec): TMatrixRec;
begin
  Result := M * Scalar;
end;

class operator TMatrixRec.Multiply(V: TVectorRec; M: TMatrixRec): TVectorRec;
var
  I, J: Integer;
begin
  if (V.FCount <> M.FRowsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(V.FCount);
  for I := 0 to V.FCount - 1 do
    for J := 0 to V.FCount - 1 do
      Result.FData[I] := Result.FData[I] + V.FData[J] * M.FData[J, I];
end;

class operator TMatrixRec.Multiply(M: TMatrixRec; V: TVectorRec): TVectorRec;
var
  I, J: Integer;
begin
  if (M.FColsCount <> V.FCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(M.FRowsCount);
  for I := 0 to M.FRowsCount - 1 do
    for J := 0 to M.FColsCount - 1 do
      Result.FData[I] := Result.FData[I] + M.FData[I, J] * V.FData[J];
end;

class operator TMatrixRec.Multiply(M: TMatrixRec; Scalar: Extended): TMatrixRec;
var
  I, J: Integer;
begin
  Result.Create(M.FRowsCount, M.FColsCount);
  for I := 0 to M.FRowsCount - 1 do
    for J := 0 to M.FColsCount - 1 do
      Result.FData[I, J] := M.FData[I, J] * Scalar;
end;

class operator TMatrixRec.Multiply(M1, M2: TMatrixRec): TMatrixRec;
var
  I, J, K: Integer;
begin
  if (M1.FColsCount <> M2.FRowsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(M1.FRowsCount, M2.FColsCount);
  for I := 0 to M1.FRowsCount - 1 do
    for J := 0 to M2.FColsCount - 1 do
      for K := 0 to M1.FColsCount - 1 do
        Result.FData[I, J] := Result.FData[I, J] + M1.FData[I, K] *
          M2.FData[K, J];
end;

procedure TMatrixRec.SetCol(Col: Word; Value: TVectorRec);
var
  I: Integer;
begin
  if (Col = 0) or (Col > FColsCount) then
    raise EMathError.Create(sWRONG_ELEMENT);
  if (Value.Count <> FRowsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  for I := 0 to FRowsCount - 1 do
    FData[I, Col - 1] := Value.FData[I];
end;

procedure TMatrixRec.SetElement(Row, Col: Word; Value: Extended);
begin
{$R+}
  CheckUnique;
  FData[Pred(Row), Pred(Col)] := Value;
end;

procedure TMatrixRec.SetRow(Row: Word; Value: TVectorRec);
var
  I: Integer;
begin
  if (Row = 0) or (Row > FRowsCount) then
    raise EMathError.Create(sWRONG_ELEMENT);
  if (Value.Count <> FColsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  for I := 0 to FColsCount - 1 do
    FData[Row - 1, I] := Value.FData[I];
end;

class operator TMatrixRec.Subtract(M1, M2: TMatrixRec): TMatrixRec;
var
  I, J: Integer;
begin
  if (M1.FColsCount <> M2.FColsCount) or (M1.FRowsCount <> M2.FRowsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(M1.FRowsCount, M1.FColsCount);
  for I := 0 to M1.FRowsCount - 1 do
    for J := 0 to M1.FColsCount - 1 do
      Result.FData[I, J] := M1.FData[I, J] - M2.FData[I, J];
end;

function TMatrixRec.Trace: Extended;
var
  I: Integer;
begin
  Result := 0;
  if FColsCount <> FRowsCount then
    raise EMathError.Create(sNOT_QUAD);
  for I := 0 to FColsCount - 1 do
    Result := Result + FData[I, I];
end;

function TMatrixRec.Transp: TMatrixRec;
var
  I, J: Integer;
begin
  Result.Create(FColsCount, FRowsCount);
  for I := 0 to FColsCount - 1 do
    for J := 0 to FRowsCount - 1 do
      Result.FData[I, J] := FData[J, I];
end;

function TMatrixRec.TruncateSTI: TMatrixRec;
const
  Int32Max: Double = Integer.MaxValue;
  Int32Min: Double = Integer.MinValue;
var
  I, J: Integer;
begin
  Result.Create(FRowsCount, FColsCount);
  for I := 0 to FRowsCount - 1 do
    for J := 0 to FColsCount - 1 do
    begin
      if (FData[I, J] >= Int32Min) and (FData[I, J] <= Int32Max) then
        Result.FData[I, J] := Trunc(FData[I, J])
      else if (FData[I, J] < Int32Min) then
        Result.FData[I, J] := Int32Min
      else
        Result.FData[I, J] := Int32Max;
    end;
end;


// -----------------------------
// TVectorRec
// -----------------------------

constructor TVectorRec.Create(V: TVectorExt);
begin
  FCount := Length(V);
  FData := Copy(V);
end;

constructor TVectorRec.Create(ElementsCount: Word);
begin
  FCount := ElementsCount;
  FData := nil;
  SetLength(FData, FCount);
end;

class operator TVectorRec.Add(V1, V2: TVectorRec): TVectorRec;
var
  I: Integer;
begin
  if (V1.FCount <> V2.FCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result := TVectorRec.Create(V1.FCount);
  for I := 0 to V1.FCount - 1 do
    Result.FData[I] := V1.FData[I] + V2.FData[I];
end;

class operator TVectorRec.Add(V: TVectorRec; Scalar: Extended): TVectorRec;
var
  I: Integer;
begin
  Result.Create(V.FCount);
  for I := 0 to V.FCount - 1 do
    Result.FData[I] := V.FData[I] + Scalar;
end;

class operator TVectorRec.Add(Scalar: Extended; V: TVectorRec): TVectorRec;
begin
  Result := V + Scalar;
end;

procedure TVectorRec.CheckUnique;
begin
  if NotUnique(@FData) then
    FData := Copy(FData);
end;

class operator TVectorRec.Divide(V1, V2: TVectorRec): TVectorRec;
var
  I: Integer;
begin
  if (V1.FCount <> V2.FCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(V1.FCount);
  for I := 0 to V1.FCount - 1 do
    Result.FData[I] := V1.FData[I] / V2.FData[I];
end;

class operator TVectorRec.Divide(V: TVectorRec; Scalar: Extended): TVectorRec;
begin
  Result := V * (1 / Scalar);
end;

class operator TVectorRec.Implicit(V: TVectorExt): TVectorRec;
begin
  Result.Create(V);
end;

procedure TVectorRec.Fill(Value: Extended);
var
  I: Integer;
begin
  if Value = 0 then
  begin
    FData := nil;
    SetLength(FData, FCount);
  end
  else
    for I := 0 to FCount - 1 do
      FData[I] := Value;
end;

function TVectorRec.GetElement(Index: Word): Extended;
begin
  if (Index = 0) or (Index > FCount) then
    raise EMathError.Create(sWRONG_ELEMENT);
  Result := FData[Pred(Index)];
end;

class operator TVectorRec.Multiply(V: TVectorRec; Scalar: Extended): TVectorRec;
var
  I: Integer;
begin
  Result.Create(V.FCount);
  for I := 0 to V.FCount - 1 do
    Result.FData[I] := V.FData[I] * Scalar;
end;

class operator TVectorRec.Multiply(Scalar: Extended; V: TVectorRec): TVectorRec;
begin
  Result := V * Scalar;
end;

function TVectorRec.Norm: Extended;
begin
  Result := System.Math.Norm(FData);
end;

class operator TVectorRec.Multiply(V1, V2: TVectorRec): TVectorRec;
begin
  if (V1.FCount <> 3) or (V2.FCount <> 3) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(V1.FCount);
  Result.FData[0] := V1.FData[1] * V2.FData[2] - V1.FData[2] * V2.FData[1];
  Result.FData[1] := V1.FData[2] * V2.FData[0] - V1.FData[0] * V2.FData[2];
  Result.FData[2] := V1.FData[0] * V2.FData[1] - V1.FData[1] * V2.FData[0];
end;

function TVectorRec.ScalarMult(V: TVectorRec): Extended;
var
  I: Integer;
begin
  if V.FCount <> FCount then
    raise EMathError.Create(sWRONG_SIZE);
  Result := 0.0;
  for I := 0 to FCount - 1 do
    Result := Result + FData[I] * V.FData[I];
end;

procedure TVectorRec.SetElement(Index: Word; Value: Extended);
begin
  if (Index = 0) or (Index > FCount) then
    raise EMathError.Create(sWRONG_ELEMENT);
  CheckUnique;
  FData[Pred(Index)] := Value;
end;

class operator TVectorRec.Subtract(V1, V2: TVectorRec): TVectorRec;
var
  I: Integer;
begin
  if (V1.FCount <> V2.FCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(V1.FCount);
  for I := 0 to V1.FCount - 1 do
    Result.FData[I] := V1.FData[I] - V2.FData[I];
end;

class operator TVectorRec.Subtract(Scalar: Extended; V: TVectorRec): TVectorRec;
var
  I: Integer;
begin
  Result.Create(V.FCount);
  for I := 0 to V.FCount - 1 do
    Result.FData[I] := Scalar - V.FData[I];
end;

class operator TVectorRec.Subtract(V: TVectorRec; Scalar: Extended): TVectorRec;
var
  I: Integer;
begin
  Result.Create(V.FCount);
  for I := 0 to V.Count - 1 do
    Result.FData[I] := V.FData[I] - Scalar;
end;

function TVectorRec.SumOfElments: Extended;
begin
  Result := Sum(FData);
end;

function TVectorRec.SumOfSquares: Extended;
begin
  Result := System.Math.SumOfSquares(FData);
end;

function TVectorRec.ToQuat: TQuaternionRec;
var
  ModVec: Extended;
  C1, C2: Extended;
begin
  if (FCount <> 3) then
    raise EMathError.Create(sWRONG_SIZE);
  ModVec := Norm;
  C1 := Cos(ModVec / 2);
  if ModVec > 1E-15 then
    C2 := Sin(ModVec / 2) / ModVec
  else
    C2 := 1;
  Result := [C1, FData[0] * C2, FData[1] * C2, FData[2] * C2];
end;

function TVectorRec.TruncateSTI: TVectorRec;
const
  Int32Max: Double = Integer.MaxValue;
  Int32Min: Double = Integer.MinValue;
var
  I: Integer;
begin
  Result.Create(FCount);
  for I := 0 to FCount - 1 do
  begin
    if (FData[I] >= Int32Min) and (FData[I] <= Int32Max) then
      Result.FData[I] := Trunc(FData[I])
    else if (FData[I] < Int32Min) then
      Result.FData[I] := Int32Min
    else
      Result.FData[I] := Int32Max;
  end;
end;

// -----------------------------
// TQuaternionHelper
// -----------------------------

function TQuaternionHelper.ToMatrix: TMatrixRec;
begin
  Result.Create(3, 3);
  Result[1, 1] := Sqr(FData[0]) + Sqr(FData[1]) - Sqr(FData[2]) - Sqr(FData[3]);
  Result[1, 2] := 2 * (FData[1] * FData[2] - FData[0] * FData[3]);
  Result[1, 3] := 2 * (FData[1] * FData[3] + FData[0] * FData[2]);
  Result[2, 1] := 2 * (FData[1] * FData[2] + FData[0] * FData[3]);
  Result[2, 2] := Sqr(FData[0]) - Sqr(FData[1]) + Sqr(FData[2]) - Sqr(FData[3]);
  Result[2, 3] := 2 * (FData[2] * FData[3] - FData[0] * FData[1]);
  Result[3, 1] := 2 * (FData[1] * FData[3] - FData[0] * FData[2]);
  Result[3, 2] := 2 * (FData[2] * FData[3] + FData[0] * FData[1]);
  Result[3, 3] := Sqr(FData[0]) - Sqr(FData[1]) - Sqr(FData[2]) + Sqr(FData[3]);
end;

// -----------------------------
// TVectorHelper
// -----------------------------

function TVectorHelper.ToDiagMatrix: TMatrixRec;
var
  I: Integer;
begin
  Result.Create(FCount, FCount);
  for I := 0 to FCount - 1 do
    Result.FData[I, I] := FData[I];
end;

procedure Init(Obj, TypeInfoOfObj: Pointer; Offset: Integer = 0);
const
  DefaultRowCount = 3;
  DefaultColCount = 3;
  VectorTypeName = 'TVectorRec';
  MatrixTypeName = 'TMatrixRec';
var
  RTTIContext: TRttiContext;
  Field: TRttiField;
  ArrFld: TRttiArrayType;
  I: Integer;
  Dim: TCustomAttribute;
  RowCount, ColCount: Integer;
  OffsetFromArray: Integer;
begin
  for Field in RTTIContext.GetType(TypeInfoOfObj).GetFields do
  begin
    if Field.FieldType <> nil then
    begin
      RowCount := DefaultRowCount;
      ColCount := DefaultColCount;
      for Dim in Field.GetAttributes do
      begin
        RowCount := (Dim as TDim).RowCount;
        ColCount := (Dim as TDim).ColCount;
      end;
      if Field.FieldType.TypeKind = tkArray then
      begin
        ArrFld := TRttiArrayType(Field.FieldType);
        if ArrFld.ElementType.TypeKind = tkRecord then
        begin
          for I := 0 to ArrFld.TotalElementCount - 1 do
          begin
            OffsetFromArray := I * ArrFld.ElementType.TypeSize;
            if ArrFld.ElementType.Name = VectorTypeName then
              PVectorRec(Integer(Obj) + Field.Offset + OffsetFromArray + Offset)
                ^ := TVectorRec.Create(RowCount)
            else if ArrFld.ElementType.Name = MatrixTypeName then
              PMatrixRec(Integer(Obj) + Field.Offset + OffsetFromArray + Offset)
                ^ := TMatrixRec.Create(RowCount, ColCount)
            else
              Init(Obj, ArrFld.ElementType.Handle,
                Field.Offset + OffsetFromArray);
          end;
        end;
      end
      else if Field.FieldType.TypeKind = tkRecord then
      begin
        if Field.FieldType.Name = VectorTypeName then
          PVectorRec(Integer(Obj) + Field.Offset + Offset)^ :=
            TVectorRec.Create(RowCount)
        else if Field.FieldType.Name = MatrixTypeName then
          PMatrixRec(Integer(Obj) + Field.Offset + Offset)^ :=
            TMatrixRec.Create(RowCount, ColCount)
        else
          Init(Obj, Field.FieldType.Handle, Field.Offset)
      end;
    end;
  end;
end;

// -----------------------------
// TDim
// -----------------------------

constructor TDim.Create(ARowCount: Integer; AColCount: Integer = 0);
begin
  FRowCount := ARowCount;
  FColCount := AColCount;
end;


// -----------------------------
// TPoint2DRec
// -----------------------------

function TPoint2DRec.Create(X, Y: Extended): TPoint2DRec;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure TPoint2DRec.SetPosition(const X, Y: Extended);
begin
  Self.X := X;
  Self.Y := Y;
end;

function TPoint2DRec.Length: Extended;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y);
end;

function TPoint2DRec.Add(const APoint2D: TPoint2DRec): TPoint2DRec;
begin
  Result.SetPosition(Self.X + APoint2D.X, Self.Y + APoint2D.Y);
end;

function TPoint2DRec.Distance(const APoint2D: TPoint2DRec): Extended;
begin
  Result := Sqrt(Sqr(Self.X - APoint2D.X) + Sqr(Self.Y - APoint2D.Y));
end;

procedure TPoint2DRec.Offset(const ADeltaX, ADeltaY: Extended);
begin
  Self.X := Self.X + ADeltaX;
  Self.Y := Self.Y + ADeltaY;
end;

class function TPoint2DRec.PointInCircle(const Point, Center: TPoint2DRec;
  const Radius: Integer): Boolean;
begin
  Result := Point.Distance(Center) <= Radius;
end;

// -----------------------------
// TPoint3DRec
// -----------------------------

function TPoint3DRec.Create(X, Y, Z: Extended): TPoint3DRec;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function TPoint3DRec.Add(const AGLPoint3D: TPoint3DRec): TPoint3DRec;
begin
  Result.X := Self.X + AGLPoint3D.X;
  Result.Y := Self.Y + AGLPoint3D.Y;
  Result.Z := Self.Z + AGLPoint3D.Z;
end;

function TPoint3DRec.Distance(const APoint3D: TPoint3DRec): Extended;
begin
  Result := Self.Length - APoint3D.Length;
end;

function TPoint3DRec.Length: Single;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y + Self.Z * Self.Z);
end;

procedure TPoint3DRec.Offset(const ADeltaX, ADeltaY, ADeltaZ: Extended);
begin
  Self.X := Self.X + ADeltaX;
  Self.Y := Self.Y + ADeltaY;
  Self.Z := Self.Z + ADeltaZ;
end;

procedure TPoint3DRec.SetPosition(const X, Y, Z: Extended);
begin
  Self.X := X;
  Self.Y := Y;
  Self.Z := Z;
end;

// -----------------------------
// TVector2DRec
// -----------------------------

function TVector2DRec.Create(const AX, AY, AW: Single): TVector2DRec;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.W := AW;
end;

function TVector2DRec.CrossProduct(const AVector: TVector2DRec): TVector2DRec;
begin
  Result.X := (Self.Y * AVector.W) - (Self.W * AVector.Y);
  Result.Y := (Self.W * AVector.X) - (Self.X * AVector.W);
  Result.W := (Self.X * AVector.Y) - (Self.Y * AVector.X);
end;

function TVector2DRec.DotProduct(const AVector: TVector2DRec): Extended;
begin
  Result := (Self.X * AVector.X) + (Self.Y * AVector.Y) + (Self.W * AVector.W);
end;

function TVector2DRec.Add(const AVector2D: TVector2DRec): TVector2DRec;
begin
  Result.X := Self.X + AVector2D.X;
  Result.Y := Self.Y + AVector2D.Y;
  Result.W := 1.0;
end;

function TVector2DRec.Length: Extended;
begin
  Result := Sqrt((Self.X * Self.X) + (Self.Y * Self.Y));
end;

function TVector2DRec.Norm: Extended;
begin
  Result := Sqr(Self.X) + Sqr(Self.Y);
end;

function TVector2DRec.Normalize: TVector2DRec;
var
  invLen: Single;
  vn: Single;
const
  Tolerance: Single = 1E-12;
begin
  vn := Self.Norm;
  if vn > Tolerance then
  begin
    invLen := 1 / Sqrt(vn);
    Result.X := Self.X * invLen;
    Result.Y := Self.Y * invLen;
  end
  else
    Result := Self;
end;

// ---------------------------------
// TVector3DRec
// ---------------------------------
function TVector3DRec.Create(const AX, AY, AZ, AW: Single): TVector3DRec;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
  Result.W := AW;
end;

function TVector3DRec.Add(const AVector3D: TVector3DRec): TVector3DRec;
begin
  Result.X := Self.X + AVector3D.X;
  Result.Y := Self.Y + AVector3D.Y;
  Result.Z := Self.Z + AVector3D.Z;
  Result.W := 1.0;
end;

function TVector3DRec.Norm: Single;
begin
  Result := Self.X * Self.X + Self.Y * Self.Y + Self.Z * Self.Z;
end;

function TVector3DRec.Normalize: TVector3DRec;
var
  invLen: Single;
  vn: Single;
const
  Tolerance: Single = 1E-12;
begin
  vn := Self.Norm;
  if vn > 0 then
  begin
    invLen := 1 / Sqrt(vn);
    Result.X := Self.X * invLen;
    Result.Y := Self.Y * invLen;
    Result.Z := Self.Z * invLen;
    Result.W := 0;
  end
  else
    Result := Self;
end;

function TVector3DRec.DotProduct(const AVector3D: TVector3D): Single;
begin
  Result := (Self.X * AVector3D.X) + (Self.Y * AVector3D.Y) +
    (Self.Z * AVector3D.Z);
end;

function TVector3DRec.CrossProduct(const AVector3D: TVector3D): TVector3D;
begin
  Result.X := (Self.Y * AVector3D.Z) - (Self.Z * AVector3D.Y);
  Result.Y := (Self.Z * AVector3D.X) - (Self.X * AVector3D.Z);
  Result.Z := (Self.X * AVector3D.Y) - (Self.Y * AVector3D.X);
end;

function TVector3DRec.Length: Single;
begin
  Result := Sqrt((Self.X * Self.X) + (Self.Y * Self.Y) + (Self.Z * Self.Z));
end;

// ---------------------------------
// TQuaternionRec
// ---------------------------------

function TQuaternionRec.GetElement(Index: Byte): Extended;
begin
  if (Index > 3) then
    raise EMathError.Create(sWRONG_ELEMENT);
  Result := FData[Index];
end;

class operator TQuaternionRec.Implicit(V: TVectorExt): TQuaternionRec;
begin
  if (Length(V) <> 4) then
    raise EMathError.Create(sWRONG_SIZE);
  Move(V[0], Result.FData, SizeOf(Result.FData));
end;

function TQuaternionRec.Inv: TQuaternionRec;
begin
  Result := [FData[0], -FData[1], -FData[2], -FData[3]];
end;

class operator TQuaternionRec.Multiply(Scalar: Extended; Q: TQuaternionRec)
  : TQuaternionRec;
begin
  Result := Q * Scalar;
end;

class operator TQuaternionRec.Multiply(Q: TQuaternionRec; Sc: Extended)
  : TQuaternionRec;
begin
  Result := [Q.FData[0] * Sc, Q.FData[1] * Sc, Q.FData[2] * Sc,
    Q.FData[3] * Sc];
end;

class operator TQuaternionRec.Multiply(Q1, Q2: TQuaternionRec): TQuaternionRec;
var
  Mat: TMatrixRec;
begin
  Mat := [[Q1.FData[0], -Q1.FData[1], -Q1.FData[2], -Q1.FData[3]],
    [Q1.FData[1], Q1.FData[0], -Q1.FData[3], Q1.FData[2]],
    [Q1.FData[2], Q1.FData[3], Q1.FData[0], -Q1.FData[1]],
    [Q1.FData[3], -Q1.FData[2], Q1.FData[1], Q1.FData[0]]];
  Result := Mat * Q2;
end;

constructor TQuaternionRec.Create(Q: TVectorExt);
begin
  if Length(Q) <> 4 then
    raise EMathError.Create(sWRONG_SIZE);
  Move(Q[0], FData[0], SizeOf(FData));
end;

procedure TQuaternionRec.SetElement(Index: Byte; Value: Extended);
begin
  if (Index > 3) then
    raise EMathError.Create(sWRONG_ELEMENT);
  FData[Index] := Value;
end;

function TQuaternionRec.TruncateSTI: TQuaternionRec;
const
  Int32Max: Double = Integer.MaxValue;
  Int32Min: Double = Integer.MinValue;

  function xTrunc(Value: Extended): Double;
  begin
    if (Value >= Int32Min) and (Value <= Int32Max) then
      Result := Trunc(Value)
    else if (Value < Int32Min) then
      Result := Int32Min
    else
      Result := Int32Max;
  end;

begin
  Result[0] := xTrunc(FData[0]);
  Result[1] := xTrunc(FData[1]);
  Result[2] := xTrunc(FData[2]);
  Result[3] := xTrunc(FData[3]);
end;

end.
