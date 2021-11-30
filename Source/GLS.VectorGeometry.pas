//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.VectorGeometry;

(*
  Base classes and structures.

  Most common functions/procedures come in various flavours (using overloads),
  the naming convention is :
  TypeOperation: functions returning a result, or accepting a "var" as last
  parameter to place result (VectorAdd, VectorCrossProduct...)
  OperationType : procedures taking as first parameter a "var" that will be
  used as operand and result (AddVector, CombineVector...)

  As a general rule, procedures implementations (asm or not) are the fastest
  (up to 800% faster than function equivalents), due to reduced return value
  duplication overhead (the exception being the matrix operations).

  For better performance, it is recommended  not  to use the "Math" unit
  that comes with Delphi, and only use functions/procedures from this unit
  (the single-based functions have been optimized and are up to 100% faster,
  than extended-based ones from "Math").
*)

interface

{$I GLScene.inc}
uses
  System.SysUtils,
  System.Types,
  System.Math,

  GLS.VectorTypes;

const
  cMaxArray = (MaxInt shr 4);
  cColinearBias = 1E-8;

type
  (* Data types needed for 3D graphics calculation, included are 'C like'
     aliases for each type (to be conformal with OpenGL types) *)
  PFloat = PSingle;

  PTexPoint = ^TTexPoint;
  TTexPoint = packed record
    S, T: Single;
  end;

  (* Types to specify continous streams of a specific type
     switch off range checking to access values beyond the limits *)
  PByteVector = ^TByteVector;
  PByteArray = PByteVector;
  TByteVector = array [0 .. cMaxArray] of Byte;

  PWordVector = ^TWordVector;
  TWordVector = array [0 .. cMaxArray] of Word;

  PIntegerVector = ^TIntegerVector;
  PIntegerArray = PIntegerVector;
  TIntegerVector = array [0 .. cMaxArray] of Integer;

  PFloatVector = ^TFloatVector;
  PFloatArray = PFloatVector;
  PSingleArray = PFloatArray;
  TFloatVector = array [0 .. cMaxArray] of Single;
  TSingleArray = array of Single;

  PDoubleVector = ^TDoubleVector;
  PDoubleArray = PDoubleVector;
  TDoubleVector = array [0 .. cMaxArray] of Double;

  PExtendedVector = ^TExtendedVector;
  PExtendedArray = PExtendedVector;
  {$IFDEF CROSSVCL}
  TExtendedVector = array [0 .. cMaxArray div 2] of Extended;
  {$ELSE}
  TExtendedVector = array [0 .. cMaxArray] of Extended;
  {$ENDIF}

  PPointerVector = ^TPointerVector;
  PPointerArray = PPointerVector;
  TPointerVector = array [0 .. cMaxArray] of Pointer;

  PCardinalVector = ^TCardinalVector;
  PCardinalArray = PCardinalVector;
  TCardinalVector = array [0 .. cMaxArray] of Cardinal;

  PLongWordVector = ^TLongWordVector;
  PLongWordArray = PLongWordVector;
  TLongWordVector = array [0 .. cMaxArray] of LongWord;

  (*
     Common vector and matrix types with predefined limits
     indices correspond like: x -> 0
     y -> 1
     z -> 2
     w -> 3
  *)
  PHomogeneousByteVector = ^THomogeneousByteVector;
  THomogeneousByteVector = TVector4b;
  PHomogeneousWordVector = ^THomogeneousWordVector;
  THomogeneousWordVector = TVector4w;

  PHomogeneousIntVector = ^THomogeneousIntVector;
  THomogeneousIntVector = TVector4i;

  PHomogeneousFltVector = ^THomogeneousFltVector;
  THomogeneousFltVector = TVector4f;

  PHomogeneousDblVector = ^THomogeneousDblVector;
  THomogeneousDblVector = TVector4d;

  PHomogeneousExtVector = ^THomogeneousExtVector;
  THomogeneousExtVector = TVector4e;

  PHomogeneousPtrVector = ^THomogeneousPtrVector;
  THomogeneousPtrVector = TVector4p;

  PAffineByteVector = ^TAffineByteVector;
  TAffineByteVector = TVector3b;

  PAffineWordVector = ^TAffineWordVector;
  TAffineWordVector = TVector3w;

  PAffineIntVector = ^TAffineIntVector;
  TAffineIntVector = TVector3i;

  PAffineFltVector = ^TAffineFltVector;
  TAffineFltVector = TVector3f;

  PAffineDblVector = ^TAffineDblVector;
  TAffineDblVector = TVector3d;

  PAffineExtVector = ^TAffineExtVector;
  TAffineExtVector = TVector3e;

  PAffinePtrVector = ^TAffinePtrVector;
  TAffinePtrVector = TVector3p;

  PVector2f = ^TVector2f;

  // some simplified names

  PHomogeneousVector = ^THomogeneousVector;
  THomogeneousVector = THomogeneousFltVector;

  PAffineVector = ^TAffineVector;
  TAffineVector = TVector3f;

  PVertex = ^TVertex;
  TVertex = TAffineVector;

  // Arrays of vectors
  PAffineVectorArray = ^TAffineVectorArray;
  TAffineVectorArray = array [0 .. MaxInt shr 4] of TAffineVector;

  PVectorArray = ^TVectorArray;
  TVectorArray = array [0 .. MaxInt shr 5] of TGLVector;

  PTexPointArray = ^TTexPointArray;
  TTexPointArray = array [0 .. MaxInt shr 4] of TTexPoint;

  // Matrices
  THomogeneousByteMatrix = TMatrix4b;
  THomogeneousWordMatrix = array [0 .. 3] of THomogeneousWordVector;
  THomogeneousIntMatrix = TMatrix4i;
  THomogeneousFltMatrix = TMatrix4f;
  THomogeneousDblMatrix = TMatrix4d;
  THomogeneousExtMatrix = array [0 .. 3] of THomogeneousExtVector;
  TAffineByteMatrix = TMatrix3b;
  TAffineWordMatrix = array [0 .. 2] of TAffineWordVector;
  TAffineIntMatrix = TMatrix3i;
  TAffineFltMatrix = TMatrix3f;
  TAffineDblMatrix = TMatrix3d;
  TAffineExtMatrix = array [0 .. 2] of TAffineExtVector;

  // Some simplified names

  TMatrixArray = array [0 .. MaxInt shr 7] of TGLMatrix;
  PMatrixArray = ^TMatrixArray;

  PHomogeneousMatrix = ^THomogeneousMatrix;
  THomogeneousMatrix = THomogeneousFltMatrix;

  PAffineMatrix = ^TAffineMatrix;
  TAffineMatrix = TAffineFltMatrix;

  (* A plane equation.
    Defined by its equation A.x+B.y+C.z+D , a plane can be mapped to the
    homogeneous space coordinates, and this is what we are doing here.
    The typename is just here for easing up data manipulation *)
  THmgPlane = TGLVector;
  TDoubleHmgPlane = THomogeneousDblVector;

  // q = ([x, y, z], w)
  PQuaternion = ^TQuaternion;
  TQuaternion = record
    case Integer of
      0: (ImagPart: TAffineVector;
            RealPart: Single);
      1: (X, Y, Z, W: Single);
  end;


  PQuaternionArray = ^TQuaternionArray;
  TQuaternionArray = array [0 .. MaxInt shr 5] of TQuaternion;

  TRectangle = record
    Left, Top, Width, Height: Integer;
  end;

  PFrustum = ^TFrustum;
  TFrustum = record
    pLeft, pTop, pRight, pBottom, pNear, pFar: THmgPlane;
  end;

   TTransType = (ttScaleX, ttScaleY, ttScaleZ,
                 ttShearXY, ttShearXZ, ttShearYZ,
                 ttRotateX, ttRotateY, ttRotateZ,
                 ttTranslateX, ttTranslateY, ttTranslateZ,
                 ttPerspectiveX, ttPerspectiveY, ttPerspectiveZ, ttPerspectiveW);

  (* Used to describe a sequence of transformations in following order:
     [Sx][Sy][Sz][ShearXY][ShearXZ][ShearZY][Rx][Ry][Rz][Tx][Ty][Tz][P(x,y,z,w)]
     constants are declared for easier access (see MatrixDecompose below) *)
  TTransformations = array [TTransType] of Single;

  TPackedRotationMatrix = array [0 .. 2] of SmallInt;

const
  // TexPoints (2D space)
  XTexPoint: TTexPoint = (S: 1; T: 0);
  YTexPoint: TTexPoint = (S: 0; T: 1);
  XYTexPoint: TTexPoint = (S: 1; T: 1);
  NullTexPoint: TTexPoint = (S: 0; T: 0);
  MidTexPoint: TTexPoint = (S: 0.5; T: 0.5);

  // standard vectors
  XVector: TAffineVector = (X: 1; Y: 0; Z: 0);
  YVector: TAffineVector = (X: 0; Y: 1; Z: 0);
  ZVector: TAffineVector = (X: 0; Y: 0; Z: 1);
  XYVector: TAffineVector = (X: 1; Y: 1; Z: 0);
  XZVector: TAffineVector = (X: 1; Y: 0; Z: 1);
  YZVector: TAffineVector = (X: 0; Y: 1; Z: 1);
  XYZVector: TAffineVector = (X: 1; Y: 1; Z: 1);
  NullVector: TAffineVector = (X: 0; Y: 0; Z: 0);
  MinusXVector: TAffineVector = (X: - 1; Y: 0; Z: 0);
  MinusYVector: TAffineVector = (X: 0; Y: - 1; Z: 0);
  MinusZVector: TAffineVector = (X: 0; Y: 0; Z: - 1);

  // Standard homogeneous vectors
  XHmgVector: THomogeneousVector = (X: 1; Y: 0; Z: 0; W: 0);
  YHmgVector: THomogeneousVector = (X: 0; Y: 1; Z: 0; W: 0);
  ZHmgVector: THomogeneousVector = (X: 0; Y: 0; Z: 1; W: 0);
  WHmgVector: THomogeneousVector = (X: 0; Y: 0; Z: 0; W: 1);
  XYHmgVector: THomogeneousVector = (X: 1; Y: 1; Z: 0; W: 0);
  YZHmgVector: THomogeneousVector = (X: 0; Y: 1; Z: 1; W: 0);
  XZHmgVector: THomogeneousVector = (X: 1; Y: 0; Z: 1; W: 0);
  XYZHmgVector: THomogeneousVector = (X: 1; Y: 1; Z: 1; W: 0);
  XYZWHmgVector: THomogeneousVector = (X: 1; Y: 1; Z: 1; W: 1);
  NullHmgVector: THomogeneousVector = (X: 0; Y: 0; Z: 0; W: 0);

  // Standard homogeneous points
  XHmgPoint: THomogeneousVector = (X: 1; Y: 0; Z: 0; W: 1);
  YHmgPoint: THomogeneousVector = (X: 0; Y: 1; Z: 0; W: 1);
  ZHmgPoint: THomogeneousVector = (X: 0; Y: 0; Z: 1; W: 1);
  WHmgPoint: THomogeneousVector = (X: 0; Y: 0; Z: 0; W: 1);
  NullHmgPoint: THomogeneousVector = (X: 0; Y: 0; Z: 0; W: 1);

  IdentityMatrix: TAffineMatrix = (V: ((X: 1; Y: 0; Z: 0), (X: 0; Y: 1;
    Z: 0), (X: 0; Y: 0; Z: 1)));
  IdentityHmgMatrix: TGLMatrix = (V: ((X: 1; Y: 0; Z: 0; W: 0), (X: 0; Y: 1; Z: 0;
    W: 0), (X: 0; Y: 0; Z: 1; W: 0), (X: 0; Y: 0; Z: 0; W: 1)));
  IdentityHmgDblMatrix: THomogeneousDblMatrix = (V: ((X: 1; Y: 0; Z: 0;
    W: 0), (X: 0; Y: 1; Z: 0; W: 0), (X: 0; Y: 0; Z: 1; W: 0), (X: 0; Y: 0;
    Z: 0; W: 1)));
  EmptyMatrix: TAffineMatrix = (V: ((X: 0; Y: 0; Z: 0), (X: 0; Y: 0;
    Z: 0), (X: 0; Y: 0; Z: 0)));
  EmptyHmgMatrix: TGLMatrix = (V: ((X: 0; Y: 0; Z: 0; W: 0), (X: 0; Y: 0; Z: 0;
    W: 0), (X: 0; Y: 0; Z: 0; W: 0), (X: 0; Y: 0; Z: 0; W: 0)));


  // Quaternions
  IdentityQuaternion: TQuaternion = (ImagPart: (X: 0; Y: 0; Z: 0); RealPart: 1);

  // Some very small numbers
  EPSILON: Single = 1E-40;
  EPSILON2: Single = 1E-30;

(* --------------------------------------------------------------------------
  Vector functions
   --------------------------------------------------------------------------*)
function TexPointMake(const S, T: Single): TTexPoint; inline;
function AffineVectorMake(const X, Y, Z: Single): TAffineVector; overload; inline;
function AffineVectorMake(const V: TGLVector): TAffineVector; overload; inline;
procedure SetAffineVector(out V: TAffineVector; const X, Y, Z: Single); overload; inline;
procedure SetVector(out V: TAffineVector; const X, Y, Z: Single); overload;inline;
procedure SetVector(out V: TAffineVector; const vSrc: TGLVector); overload; inline;
procedure SetVector(out V: TAffineVector; const vSrc: TAffineVector); overload; inline;
procedure SetVector(out V: TAffineDblVector; const vSrc: TAffineVector);  overload; inline;
procedure SetVector(out V: TAffineDblVector; const vSrc: TGLVector); overload; inline;
function VectorMake(const V: TAffineVector; W: Single = 0): TGLVector; overload; inline;
function VectorMake(const X, Y, Z: Single; W: Single = 0): TGLVector; overload; inline;
function VectorMake(const Q: TQuaternion): TGLVector; overload; inline;

function PointMake(const X, Y, Z: Single): TGLVector; overload; inline;
function PointMake(const V: TAffineVector): TGLVector; overload; inline;
function PointMake(const V: TGLVector): TGLVector; overload;inline;
procedure SetVector(out V: TGLVector; const X, Y, Z: Single; W: Single = 0); overload; inline;
procedure SetVector(out V: TGLVector; const av: TAffineVector; W: Single = 0); overload; inline;
procedure SetVector(out V: TGLVector; const vSrc: TGLVector); overload; inline;
procedure MakePoint(out V: TGLVector; const X, Y, Z: Single); overload; inline;
procedure MakePoint(out V: TGLVector; const av: TAffineVector); overload;inline;
procedure MakePoint(out V: TGLVector; const av: TGLVector); overload; inline;
procedure MakeVector(out V: TAffineVector; const X, Y, Z: Single); overload; inline;
procedure MakeVector(out V: TGLVector; const X, Y, Z: Single); overload; inline;
procedure MakeVector(out V: TGLVector; const av: TAffineVector); overload; inline;
procedure MakeVector(out V: TGLVector; const av: TGLVector); overload; inline;
procedure RstVector(var V: TAffineVector); overload; inline;
procedure RstVector(var V: TGLVector); overload; inline;

function VectorEquals(const Vector1, Vector2: TVector2f): Boolean; overload; inline;
function VectorEquals(const Vector1, Vector2: TVector2i): Boolean; overload; inline;
function VectorEquals(const V1, V2: TVector2d): Boolean; overload;inline;
function VectorEquals(const V1, V2: TVector2s): Boolean; overload;inline;
function VectorEquals(const V1, V2: TVector2b): Boolean; overload;inline;

// function VectorEquals(const V1, V2: TVector3f): Boolean; overload; //declared further
function VectorEquals(const V1, V2: TVector3i): Boolean; overload;inline;
function VectorEquals(const V1, V2: TVector3d): Boolean; overload;inline;
function VectorEquals(const V1, V2: TVector3s): Boolean; overload;inline;
function VectorEquals(const V1, V2: TVector3b): Boolean; overload;inline;

// function VectorEquals(const V1, V2: TVector4f): Boolean; overload; //declared further
function VectorEquals(const V1, V2: TVector4i): Boolean; overload;inline;
function VectorEquals(const V1, V2: TVector4d): Boolean; overload;inline;
function VectorEquals(const V1, V2: TVector4s): Boolean; overload;inline;
function VectorEquals(const V1, V2: TVector4b): Boolean; overload;inline;
// 3x3
function MatrixEquals(const Matrix1, Matrix2: TMatrix3f): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix3i): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix3d): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix3s): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix3b): Boolean; overload;
// 4x4
function MatrixEquals(const Matrix1, Matrix2: TMatrix4f): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix4i): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix4d): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix4s): Boolean; overload;
function MatrixEquals(const Matrix1, Matrix2: TMatrix4b): Boolean; overload;
// 2x
function Vector2fMake(const X, Y: Single): TVector2f; overload; inline;
function Vector2iMake(const X, Y: Longint): TVector2i; overload; inline;
function Vector2sMake(const X, Y: SmallInt): TVector2s; overload; inline;
function Vector2dMake(const X, Y: Double): TVector2d; overload; inline;
function Vector2bMake(const X, Y: Byte): TVector2b; overload; inline;
function Vector2fMake(const Vector: TVector3f): TVector2f; overload; inline;
function Vector2iMake(const Vector: TVector3i): TVector2i; overload; inline;
function Vector2sMake(const Vector: TVector3s): TVector2s; overload; inline;
function Vector2dMake(const Vector: TVector3d): TVector2d; overload; inline;
function Vector2bMake(const Vector: TVector3b): TVector2b; overload; inline;
function Vector2fMake(const Vector: TVector4f): TVector2f; overload; inline;
function Vector2iMake(const Vector: TVector4i): TVector2i; overload; inline;
function Vector2sMake(const Vector: TVector4s): TVector2s; overload; inline;
function Vector2dMake(const Vector: TVector4d): TVector2d; overload; inline;
function Vector2bMake(const Vector: TVector4b): TVector2b; overload; inline;
// 3x
function Vector3fMake(const X: Single; const Y: Single = 0; const Z: Single = 0) : TVector3f; overload; inline;
function Vector3iMake(const X: Longint; const Y: Longint = 0; const Z: Longint = 0): TVector3i; overload;inline;
function Vector3sMake(const X: SmallInt; const Y: SmallInt = 0; const Z: SmallInt = 0): TVector3s; overload;inline;
function Vector3dMake(const X: Double; const Y: Double = 0; const Z: Double = 0): TVector3d; overload; inline;
function Vector3bMake(const X: Byte; const Y: Byte = 0; const Z: Byte = 0): TVector3b; overload; inline;
function Vector3fMake(const Vector: TVector2f; const Z: Single = 0): TVector3f; overload; inline;
function Vector3iMake(const Vector: TVector2i; const Z: Longint = 0): TVector3i; overload; inline;
function Vector3sMake(const Vector: TVector2s; const Z: SmallInt = 0): TVector3s; overload; inline;
function Vector3dMake(const Vector: TVector2d; const Z: Double = 0): TVector3d; overload; inline;
function Vector3bMake(const Vector: TVector2b; const Z: Byte = 0): TVector3b; overload; inline;
function Vector3fMake(const Vector: TVector4f): TVector3f; overload; inline;
function Vector3iMake(const Vector: TVector4i): TVector3i; overload; inline;
function Vector3sMake(const Vector: TVector4s): TVector3s; overload; inline;
function Vector3dMake(const Vector: TVector4d): TVector3d; overload; inline;
function Vector3bMake(const Vector: TVector4b): TVector3b; overload; inline;
// 4x
function Vector4fMake(const X: Single; const Y: Single = 0; const Z: Single = 0; const W: Single = 0): TVector4f; overload; inline;
function Vector4iMake(const X: Longint; const Y: Longint = 0; const Z: Longint = 0; const W: Longint = 0): TVector4i; overload; inline;
function Vector4sMake(const X: SmallInt; const Y: SmallInt = 0; const Z: SmallInt = 0; const W: SmallInt = 0): TVector4s; overload; inline;
function Vector4dMake(const X: Double; const Y: Double = 0; const Z: Double = 0; const W: Double = 0): TVector4d; overload; inline;
function Vector4bMake(const X: Byte; const Y: Byte = 0; const Z: Byte = 0; const W: Byte = 0): TVector4b; overload; inline;
function Vector4fMake(const Vector: TVector3f; const W: Single = 0): TVector4f; overload; inline;
function Vector4iMake(const Vector: TVector3i; const W: Longint = 0): TVector4i; overload; inline;
function Vector4sMake(const Vector: TVector3s; const W: SmallInt = 0) : TVector4s; overload; inline;
function Vector4dMake(const Vector: TVector3d; const W: Double = 0): TVector4d; overload; inline;
function Vector4bMake(const Vector: TVector3b; const W: Byte = 0): TVector4b; overload; inline;
function Vector4fMake(const Vector: TVector2f; const Z: Single = 0; const W: Single = 0): TVector4f; overload; inline;
function Vector4iMake(const Vector: TVector2i; const Z: Longint = 0; const W: Longint = 0): TVector4i; overload;inline;
function Vector4sMake(const Vector: TVector2s; const Z: SmallInt = 0; const W: SmallInt = 0): TVector4s; overload;inline;
function Vector4dMake(const Vector: TVector2d; const Z: Double = 0; const W: Double = 0): TVector4d; overload; inline;
function Vector4bMake(const Vector: TVector2b; const Z: Byte = 0; const W: Byte = 0): TVector4b; overload; inline;

// Vector comparison functions:
// 3f
function VectorMoreThen(const SourceVector, ComparedVector: TVector3f): Boolean; overload;
function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector3f): Boolean; overload;
function VectorLessThen(const SourceVector, ComparedVector: TVector3f): Boolean; overload;
function VectorLessEqualThen(const SourceVector, ComparedVector: TVector3f): Boolean; overload;
// 4f
function VectorMoreThen(const SourceVector, ComparedVector: TVector4f): Boolean; overload;
function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector4f): Boolean; overload;
function VectorLessThen(const SourceVector, ComparedVector: TVector4f): Boolean; overload;
function VectorLessEqualThen(const SourceVector, ComparedVector: TVector4f): Boolean; overload;
// 3i
function VectorMoreThen(const SourceVector, ComparedVector: TVector3i): Boolean; overload;
function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector3i): Boolean; overload;
function VectorLessThen(const SourceVector, ComparedVector: TVector3i): Boolean; overload;
function VectorLessEqualThen(const SourceVector, ComparedVector: TVector3i): Boolean; overload;
// 4i
function VectorMoreThen(const SourceVector, ComparedVector: TVector4i): Boolean; overload;
function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector4i): Boolean; overload;
function VectorLessThen(const SourceVector, ComparedVector: TVector4i): Boolean; overload;
function VectorLessEqualThen(const SourceVector, ComparedVector: TVector4i): Boolean; overload;
// 3s
function VectorMoreThen(const SourceVector, ComparedVector: TVector3s): Boolean; overload;
function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector3s): Boolean; overload;
function VectorLessThen(const SourceVector, ComparedVector: TVector3s): Boolean; overload;
function VectorLessEqualThen(const SourceVector, ComparedVector: TVector3s): Boolean; overload;
// 4s
function VectorMoreThen(const SourceVector, ComparedVector: TVector4s): Boolean; overload;
function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector4s): Boolean; overload;
function VectorLessThen(const SourceVector, ComparedVector: TVector4s): Boolean; overload;
function VectorLessEqualThen(const SourceVector, ComparedVector: TVector4s): Boolean; overload;
// ComparedNumber
// 3f
function VectorMoreThen(const SourceVector: TVector3f; const ComparedNumber: Single): Boolean; overload;
function VectorMoreEqualThen(const SourceVector: TVector3f; const ComparedNumber: Single): Boolean; overload;
function VectorLessThen(const SourceVector: TVector3f; const ComparedNumber: Single): Boolean; overload;
function VectorLessEqualThen(const SourceVector: TVector3f; const ComparedNumber: Single): Boolean; overload;
// 4f
function VectorMoreThen(const SourceVector: TVector4f; const ComparedNumber: Single): Boolean; overload;
function VectorMoreEqualThen(const SourceVector: TVector4f; const ComparedNumber: Single): Boolean; overload;
function VectorLessThen(const SourceVector: TVector4f; const ComparedNumber: Single): Boolean; overload;
function VectorLessEqualThen(const SourceVector: TVector4f; const ComparedNumber: Single): Boolean; overload;
// 3i
function VectorMoreThen(const SourceVector: TVector3i; const ComparedNumber: Single): Boolean; overload;
function VectorMoreEqualThen(const SourceVector: TVector3i; const ComparedNumber: Single): Boolean; overload;
function VectorLessThen(const SourceVector: TVector3i; const ComparedNumber: Single): Boolean; overload;
function VectorLessEqualThen(const SourceVector: TVector3i; const ComparedNumber: Single): Boolean; overload;
// 4i
function VectorMoreThen(const SourceVector: TVector4i; const ComparedNumber: Single): Boolean; overload;
function VectorMoreEqualThen(const SourceVector: TVector4i; const ComparedNumber: Single): Boolean; overload;
function VectorLessThen(const SourceVector: TVector4i; const ComparedNumber: Single): Boolean; overload;
function VectorLessEqualThen(const SourceVector: TVector4i; const ComparedNumber: Single): Boolean; overload;
// 3s
function VectorMoreThen(const SourceVector: TVector3s; const ComparedNumber: Single): Boolean; overload;
function VectorMoreEqualThen(const SourceVector: TVector3s; const ComparedNumber: Single): Boolean; overload;
function VectorLessThen(const SourceVector: TVector3s; const ComparedNumber: Single): Boolean; overload;
function VectorLessEqualThen(const SourceVector: TVector3s; const ComparedNumber: Single): Boolean; overload;
// 4s
function VectorMoreThen(const SourceVector: TVector4s; const ComparedNumber: Single): Boolean; overload;
function VectorMoreEqualThen(const SourceVector: TVector4s; const ComparedNumber: Single): Boolean; overload;
function VectorLessThen(const SourceVector: TVector4s; const ComparedNumber: Single): Boolean; overload;
function VectorLessEqualThen(const SourceVector: TVector4s; const ComparedNumber: Single): Boolean; overload;

function VectorAdd(const V1, V2: TVector2f): TVector2f; overload;
// Returns the sum of two affine vectors
function VectorAdd(const V1, V2: TAffineVector): TAffineVector; overload;
// Adds two vectors and places result in vr
procedure VectorAdd(const V1, V2: TAffineVector; var vr: TAffineVector); overload;
procedure VectorAdd(const V1, V2: TAffineVector; vr: PAffineVector); overload;
// Returns the sum of two homogeneous vectors
function VectorAdd(const V1, V2: TGLVector): TGLVector; overload;
procedure VectorAdd(const V1, V2: TGLVector; var vr: TGLVector); overload;
// Sums up f to each component of the vector
function VectorAdd(const V: TAffineVector; const f: Single): TAffineVector; overload; inline;
// Sums up f to each component of the vector
function VectorAdd(const V: TGLVector; const f: Single): TGLVector; overload; inline;
// Adds V2 to V1, result is placed in V1
procedure AddVector(var V1: TAffineVector; const V2: TAffineVector); overload;
// Adds V2 to V1, result is placed in V1
procedure AddVector(var V1: TAffineVector; const V2: TGLVector); overload;
// Adds V2 to V1, result is placed in V1
procedure AddVector(var V1: TGLVector; const V2: TGLVector); overload;
// Sums up f to each component of the vector
procedure AddVector(var V: TAffineVector; const f: Single); overload; inline;
// Sums up f to each component of the vector
procedure AddVector(var V: TGLVector; const f: Single); overload; inline;
// Adds V2 to V1, result is placed in V1. W coordinate is always 1.
procedure AddPoint(var V1: TGLVector; const V2: TGLVector); overload; inline;
// Returns the sum of two homogeneous vectors. W coordinate is always 1.
function PointAdd(var V1: TGLVector; const V2: TGLVector): TGLVector; overload; inline;
// Adds delta to nb texpoints in src and places result in dest
procedure TexPointArrayAdd(const src: PTexPointArray; const delta: TTexPoint; const nb: Integer; dest: PTexPointArray); overload;
procedure TexPointArrayScaleAndAdd(const src: PTexPointArray; const delta: TTexPoint;
  const nb: Integer; const scale: TTexPoint; dest: PTexPointArray); overload;
// Adds delta to nb vectors in src and places result in dest
procedure VectorArrayAdd(const src: PAffineVectorArray;
  const delta: TAffineVector; const nb: Integer; dest: PAffineVectorArray); overload;

// Returns V1-V2
function VectorSubtract(const V1, V2: TVector2f): TVector2f; overload;
// Subtracts V2 from V1, result is placed in V1
procedure SubtractVector(var V1: TVector2f; const V2: TVector2f); overload; 

// Returns V1-V2
function VectorSubtract(const V1, V2: TAffineVector): TAffineVector; overload; 
// Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const V1, V2: TAffineVector; var result: TAffineVector); overload; 
// Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const V1, V2: TAffineVector; var result: TGLVector); overload; 
// Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const V1: TGLVector; const V2: TAffineVector; var result: TGLVector); overload; 
// Returns V1-V2
function VectorSubtract(const V1, V2: TGLVector): TGLVector; overload; 
// Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const V1, V2: TGLVector; var result: TGLVector); overload;
// Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const V1, V2: TGLVector; var result: TAffineVector); overload;
function VectorSubtract(const V1: TAffineVector; delta: Single): TAffineVector; overload; inline;
function VectorSubtract(const V1: TGLVector; delta: Single): TGLVector; overload;inline;
// Subtracts V2 from V1, result is placed in V1
procedure SubtractVector(var V1: TAffineVector; const V2: TAffineVector); overload; 
// Subtracts V2 from V1, result is placed in V1
procedure SubtractVector(var V1: TGLVector; const V2: TGLVector); overload; 

// Combine the first vector with the second : vr:=vr+v*f
procedure CombineVector(var vr: TAffineVector; const V: TAffineVector; var f: Single); overload; 
procedure CombineVector(var vr: TAffineVector; const V: TAffineVector; pf: PFloat); overload; 
// Makes a linear combination of two texpoints
function TexPointCombine(const t1, t2: TTexPoint; f1, f2: Single): TTexPoint; inline;
// Makes a linear combination of two vectors and return the result
function VectorCombine(const V1, V2: TAffineVector; const f1, f2: Single): TAffineVector; overload; inline;
// Makes a linear combination of three vectors and return the result
function VectorCombine3(const V1, V2, V3: TAffineVector; const f1, f2, F3: Single): TAffineVector; overload;inline;
procedure VectorCombine3(const V1, V2, V3: TAffineVector;
  const f1, f2, F3: Single; var vr: TAffineVector); overload;inline;
// Combine the first vector with the second : vr:=vr+v*f
procedure CombineVector(var vr: TGLVector; const V: TGLVector; var f: Single); overload;
// Combine the first vector with the second : vr:=vr+v*f
procedure CombineVector(var vr: TGLVector; const V: TAffineVector; var f: Single); overload;
// Makes a linear combination of two vectors and return the result
function VectorCombine(const V1, V2: TGLVector; const F1, F2: Single): TGLVector; overload; inline;
// Makes a linear combination of two vectors and return the result
function VectorCombine(const V1: TGLVector; const V2: TAffineVector;
  const F1, F2: Single): TGLVector; overload; inline;
// Makes a linear combination of two vectors and place result in vr
procedure VectorCombine(const V1: TGLVector; const V2: TAffineVector; const F1, F2: Single; var VR: TGLVector); overload;inline;
// Makes a linear combination of two vectors and place result in vr
procedure VectorCombine(const V1, V2: TGLVector; const F1, F2: Single; var vr: TGLVector); overload;
// Makes a linear combination of two vectors and place result in vr, F1=1.0
procedure VectorCombine(const V1, V2: TGLVector; const F2: Single; var vr: TGLVector); overload;
// Makes a linear combination of three vectors and return the result
function VectorCombine3(const V1, V2, V3: TGLVector; const F1, F2, F3: Single): TGLVector; overload; inline;
// Makes a linear combination of three vectors and return the result
procedure VectorCombine3(const V1, V2, V3: TGLVector; const F1, F2, F3: Single; var vr: TGLVector); overload;

(* Calculates the dot product between V1 and V2.
  Result:=V1[X] * V2[X] + V1[Y] * V2[Y] *)
function VectorDotProduct(const V1, V2: TVector2f): Single; overload;
(* Calculates the dot product between V1 and V2.
  Result:=V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z] *)
function VectorDotProduct(const V1, V2: TAffineVector): Single; overload;
(* Calculates the dot product between V1 and V2.
  Result:=V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z] *)
function VectorDotProduct(const V1, V2: TGLVector): Single; overload;
(* Calculates the dot product between V1 and V2.
  Result:=V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z] *)
function VectorDotProduct(const V1: TGLVector; const V2: TAffineVector): Single; overload;

(* Projects p on the line defined by o and direction.
   Performs VectorDotProduct(VectorSubtract(p, origin), direction), which,
   if direction is normalized, computes the distance between origin and the
   projection of p on the (origin, direction) line *)
function PointProject(const p, origin, direction: TAffineVector): Single; overload;
function PointProject(const p, origin, direction: TGLVector): Single; overload;

// Calculates the cross product between vector 1 and 2
function VectorCrossProduct(const V1, V2: TAffineVector): TAffineVector; overload;
// Calculates the cross product between vector 1 and 2
function VectorCrossProduct(const V1, V2: TGLVector): TGLVector; overload;
// Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const V1, V2: TGLVector; var vr: TGLVector); overload;
// Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const V1, V2: TAffineVector; var vr: TGLVector); overload;
// Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const V1, V2: TGLVector; var vr: TAffineVector); overload;
// Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const V1, V2: TAffineVector; var vr: TAffineVector); overload;

// Calculates linear interpolation between start and stop at point t
function Lerp(const start, stop, T: Single): Single; inline;
// Calculates angular interpolation between start and stop at point t
function AngleLerp(start, stop, T: Single): Single; inline;
(* This is used for interpolating between 2 matrices. The result
  is used to reposition the model parts each frame. *)
function MatrixLerp(const m1, m2: TGLMatrix; const delta: Single): TGLMatrix;

(* Calculates the angular distance between two angles in radians.
  Result is in the [0; PI] range. *)
function DistanceBetweenAngles(angle1, angle2: Single): Single;

// Calculates linear interpolation between texpoint1 and texpoint2 at point t
function TexPointLerp(const t1, t2: TTexPoint; T: Single): TTexPoint; overload; inline;
// Calculates linear interpolation between vector1 and vector2 at point t
function VectorLerp(const V1, V2: TAffineVector; T: Single): TAffineVector; overload; inline;
// Calculates linear interpolation between vector1 and vector2 at point t, places result in vr
procedure VectorLerp(const V1, V2: TAffineVector; T: Single; var vr: TAffineVector); overload;
// Calculates linear interpolation between vector1 and vector2 at point t
function VectorLerp(const V1, V2: TGLVector; T: Single): TGLVector; overload; inline;
// Calculates linear interpolation between vector1 and vector2 at point t, places result in vr
procedure VectorLerp(const V1, V2: TGLVector; T: Single; var vr: TGLVector); overload; inline;
function VectorAngleLerp(const V1, V2: TAffineVector; T: Single): TAffineVector; overload;
function VectorAngleCombine(const V1, V2: TAffineVector; f: Single): TAffineVector; overload;

// Calculates linear interpolation between vector arrays
procedure VectorArrayLerp(const src1, src2: PVectorArray; T: Single; n: Integer; dest: PVectorArray); overload;
procedure VectorArrayLerp(const src1, src2: PAffineVectorArray; T: Single; n: Integer; dest: PAffineVectorArray); overload;
procedure VectorArrayLerp(const src1, src2: PTexPointArray; T: Single; n: Integer; dest: PTexPointArray); overload;

type
  TGLInterpolationType = (itLinear, itPower, itSin, itSinAlt, itTan, itLn, itExp);

// There functions that do the same as "Lerp", but add some distortions
function InterpolatePower(const start, stop, delta: Single; const DistortionDegree: Single): Single;
function InterpolateLn(const start, stop, delta: Single; const DistortionDegree: Single): Single;
function InterpolateExp(const start, stop, delta: Single; const DistortionDegree: Single): Single;

// Only valid where Delta belongs to [0..1]
function InterpolateSin(const start, stop, delta: Single): Single;
function InterpolateTan(const start, stop, delta: Single): Single;

// "Alt" functions are valid everywhere
function InterpolateSinAlt(const start, stop, delta: Single): Single;
function InterpolateCombinedFastPower(const OriginalStart, OriginalStop,
  OriginalCurrent: Single; const TargetStart, TargetStop: Single;
  const DistortionDegree: Single): Single; inline;
function InterpolateCombinedSafe(const OriginalStart, OriginalStop,
  OriginalCurrent: Single; const TargetStart, TargetStop: Single;
  const DistortionDegree: Single;
  const InterpolationType: TGLInterpolationType): Single; inline;
function InterpolateCombinedFast(const OriginalStart, OriginalStop,
  OriginalCurrent: Single; const TargetStart, TargetStop: Single;
  const DistortionDegree: Single;
  const InterpolationType: TGLInterpolationType): Single; inline;
function InterpolateCombined(const start, stop, delta: Single;
  const DistortionDegree: Single;
  const InterpolationType: TGLInterpolationType): Single; inline;

{ Calculates the length of a vector following the equation sqrt(x*x+y*y). }
function VectorLength(const X, Y: Single): Single; overload; 
{ Calculates the length of a vector following the equation sqrt(x*x+y*y+z*z). }
function VectorLength(const X, Y, Z: Single): Single; overload;
// Calculates the length of a vector following the equation sqrt(x*x+y*y).
function VectorLength(const V: TVector2f): Single; overload;
// Calculates the length of a vector following the equation sqrt(x*x+y*y+z*z).
function VectorLength(const V: TAffineVector): Single; overload;
// Calculates the length of a vector following the equation sqrt(x*x+y*y+z*z+w*w).
function VectorLength(const V: TGLVector): Single; overload;
(* Calculates the length of a vector following the equation: sqrt(x*x+y*y+...).
  Note: The parameter of this function is declared as open array. Thus
  there's no restriction about the number of the components of the vector. *)
function VectorLength(const V: array of Single): Single; overload;

(* Calculates norm of a vector which is defined as norm = x * x + y * y
  Also known as "Norm 2" in the math world, this is sqr(VectorLength). *)
function VectorNorm(const X, Y: Single): Single; overload;
(* Calculates norm of a vector which is defined as norm = x*x + y*y + z*z
  Also known as "Norm 2" in the math world, this is sqr(VectorLength). *)
function VectorNorm(const V: TAffineVector): Single; overload;
(* Calculates norm of a vector which is defined as norm = x*x + y*y + z*z
  Also known as "Norm 2" in the math world, this is sqr(VectorLength). *)
function VectorNorm(const V: TGLVector): Single; overload;
(* Calculates norm of a vector which is defined as norm = v.X*v.X + ...
  Also known as "Norm 2" in the math world, this is sqr(VectorLength). *)
function VectorNorm(var V: array of Single): Single; overload;

// Transforms a vector to unit length
procedure NormalizeVector(var V: TVector2f); overload;
(* Returns the vector transformed to unit length
  Transforms a vector to unit length *)
procedure NormalizeVector(var V: TAffineVector); overload;
// Transforms a vector to unit length
procedure NormalizeVector(var V: TGLVector); overload;
// Returns the vector transformed to unit length
function VectorNormalize(const V: TVector2f): TVector2f; overload;
// Returns the vector transformed to unit length
function VectorNormalize(const V: TAffineVector): TAffineVector; overload;
// Returns the vector transformed to unit length (w component dropped)
function VectorNormalize(const V: TGLVector): TGLVector; overload;

// Transforms vectors to unit length
procedure NormalizeVectorArray(list: PAffineVectorArray; n: Integer); overload; inline;

(* Calculates the cosine of the angle between Vector1 and Vector2.
  Result = DotProduct(V1, V2) / (Length(V1) * Length(V2)) *)
function VectorAngleCosine(const V1, V2: TAffineVector): Single; overload;

(* Calculates the cosine of the angle between Vector1 and Vector2.
  Result = DotProduct(V1, V2) / (Length(V1) * Length(V2)) *)
function VectorAngleCosine(const V1, V2: TGLVector): Single; overload;

// Negates the vector
function VectorNegate(const Vector: TAffineVector): TAffineVector; overload;
function VectorNegate(const Vector: TGLVector): TGLVector; overload;

// Negates the vector
procedure NegateVector(var V: TAffineVector); overload;
// Negates the vector
procedure NegateVector(var V: TGLVector); overload;
// Negates the vector
procedure NegateVector(var V: array of Single); overload;

// Scales given vector by a factor
procedure ScaleVector(var V: TVector2f; factor: Single); overload;
// Scales given vector by a factor
procedure ScaleVector(var V: TAffineVector; factor: Single); overload;
(* Scales given vector by another vector.
  v[x]:=v[x]*factor[x], v[y]:=v[y]*factor[y] etc. *)
procedure ScaleVector(var V: TAffineVector; const factor: TAffineVector); overload;
// Scales given vector by a factor
procedure ScaleVector(var V: TGLVector; factor: Single); overload;
(* Scales given vector by another vector.
  v[x]:=v[x]*factor[x], v[y]:=v[y]*factor[y] etc. *)
procedure ScaleVector(var V: TGLVector; const factor: TGLVector); overload;

// Returns a vector scaled by a factor
function VectorScale(const V: TVector2f; factor: Single): TVector2f; overload;
// Returns a vector scaled by a factor
function VectorScale(const V: TAffineVector; factor: Single): TAffineVector; overload;
// Scales a vector by a factor and places result in vr
procedure VectorScale(const V: TAffineVector; factor: Single; var vr: TAffineVector); overload;
// Returns a vector scaled by a factor
function VectorScale(const V: TGLVector; factor: Single): TGLVector; overload;
// Scales a vector by a factor and places result in vr
procedure VectorScale(const V: TGLVector; factor: Single; var vr: TGLVector); overload;
// Scales a vector by a factor and places result in vr
procedure VectorScale(const V: TGLVector; factor: Single; var vr: TAffineVector); overload;
// Scales given vector by another vector
function VectorScale(const V: TAffineVector; const factor: TAffineVector): TAffineVector; overload;
// RScales given vector by another vector
function VectorScale(const V: TGLVector; const factor: TGLVector): TGLVector; overload;

(* Divides given vector by another vector.
  v[x]:=v[x]/divider[x], v[y]:=v[y]/divider[y] etc. *)
procedure DivideVector(var V: TGLVector; const divider: TGLVector); overload; inline;
procedure DivideVector(var V: TAffineVector; const divider: TAffineVector); overload; inline;
function VectorDivide(const V: TGLVector; const divider: TGLVector): TGLVector; overload; inline;
function VectorDivide(const V: TAffineVector; const divider: TAffineVector): TAffineVector; overload; inline;
// True if all components are equal.
function TexpointEquals(const p1, p2: TTexPoint): Boolean; inline;
// True if all components are equal.
function RectEquals(const Rect1, Rect2: TRect): Boolean; inline;
// True if all components are equal.
function VectorEquals(const V1, V2: TGLVector): Boolean; overload; inline;
// True if all components are equal.
function VectorEquals(const V1, V2: TAffineVector): Boolean; overload; inline;
// True if X, Y and Z components are equal.
function AffineVectorEquals(const V1, V2: TGLVector): Boolean; overload; inline;
// True if x=y=z=0, w ignored
function VectorIsNull(const V: TGLVector): Boolean; overload; inline;
// True if x=y=z=0, w ignored
function VectorIsNull(const V: TAffineVector): Boolean; overload; inline;
// Calculates Abs(v1[x]-v2[x])+Abs(v1[y]-v2[y]), also know as "Norm1".
function VectorSpacing(const V1, V2: TTexPoint): Single; overload;
// Calculates Abs(v1[x]-v2[x])+Abs(v1[y]-v2[y])+..., also know as "Norm1".
function VectorSpacing(const V1, V2: TAffineVector): Single; overload;
// Calculates Abs(v1[x]-v2[x])+Abs(v1[y]-v2[y])+..., also know as "Norm1".
function VectorSpacing(const V1, V2: TGLVector): Single; overload;

// Calculates distance between two vectors. ie. sqrt(sqr(v1[x]-v2[x])+...)
function VectorDistance(const V1, V2: TAffineVector): Single; overload;
(* Calculates distance between two vectors.
  ie. sqrt(sqr(v1[x]-v2[x])+...) (w component ignored) *)
function VectorDistance(const V1, V2: TGLVector): Single; overload;

// Calculates the "Norm 2" between two vectors. ie. sqr(v1[x]-v2[x])+...
function VectorDistance2(const V1, V2: TAffineVector): Single; overload;
// Calculates the "Norm 2" between two vectors. ie. sqr(v1[x]-v2[x])+...(w component ignored)
function VectorDistance2(const V1, V2: TGLVector): Single; overload;

// Calculates a vector perpendicular to N. N is assumed to be of unit length, subtract out any component parallel to N
function VectorPerpendicular(const V, n: TAffineVector): TAffineVector;
// Reflects vector V against N (assumes N is normalized)
function VectorReflect(const V, n: TAffineVector): TAffineVector;
// Rotates Vector about Axis with Angle radians
procedure RotateVector(var Vector: TGLVector; const axis: TAffineVector; angle: Single); overload;
// Rotates Vector about Axis with Angle radians
procedure RotateVector(var Vector: TGLVector; const axis: TGLVector; angle: Single); overload;

// Rotate given vector around the Y axis (alpha is in rad)
procedure RotateVectorAroundY(var V: TAffineVector; alpha: Single);
// Returns given vector rotated around the X axis (alpha is in rad)
function VectorRotateAroundX(const V: TAffineVector; alpha: Single): TAffineVector; overload;
// Returns given vector rotated around the Y axis (alpha is in rad)
function VectorRotateAroundY(const V: TAffineVector; alpha: Single): TAffineVector; overload;
// Returns given vector rotated around the Y axis in vr (alpha is in rad)
procedure VectorRotateAroundY(const V: TAffineVector; alpha: Single; var vr: TAffineVector); overload;
// Returns given vector rotated around the Z axis (alpha is in rad)
function VectorRotateAroundZ(const V: TAffineVector; alpha: Single): TAffineVector; overload;

// Vector components are replaced by their Abs() value. }
procedure AbsVector(var V: TGLVector); overload; inline;
// Vector components are replaced by their Abs() value. }
procedure AbsVector(var V: TAffineVector); overload;inline;
// Returns a vector with components replaced by their Abs value. }
function VectorAbs(const V: TGLVector): TGLVector; overload; inline;
// Returns a vector with components replaced by their Abs value. }
function VectorAbs(const V: TAffineVector): TAffineVector; overload;inline;
// Returns true if both vector are colinear
function IsColinear(const V1, V2: TVector2f): Boolean; overload;
// Returns true if both vector are colinear
function IsColinear(const V1, V2: TAffineVector): Boolean; overload;
// Returns true if both vector are colinear
function IsColinear(const V1, V2: TGLVector): Boolean; overload;

(* ----------------------------------------------------------------------------
  Matrix functions
 ---------------------------------------------------------------------------- *)
procedure SetMatrix(var dest: THomogeneousDblMatrix; const src: TGLMatrix); overload;
procedure SetMatrix(var dest: TAffineMatrix; const src: TGLMatrix); overload;
procedure SetMatrix(var dest: TGLMatrix; const src: TAffineMatrix); overload;
procedure SetMatrixRow(var dest: TGLMatrix; rowNb: Integer; const aRow: TGLVector); overload;

// Creates scale matrix
function CreateScaleMatrix(const V: TAffineVector): TGLMatrix; overload;
// Creates scale matrix
function CreateScaleMatrix(const V: TGLVector): TGLMatrix; overload;
// Creates translation matrix
function CreateTranslationMatrix(const V: TAffineVector): TGLMatrix; overload;
// Creates translation matrix
function CreateTranslationMatrix(const V: TGLVector): TGLMatrix; overload;
{ Creates a scale+translation matrix.
  Scale is applied BEFORE applying offset }
function CreateScaleAndTranslationMatrix(const scale, offset: TGLVector): TGLMatrix; overload;
// Creates matrix for rotation about x-axis (angle in rad)
function CreateRotationMatrixX(const sine, cosine: Single): TGLMatrix; overload;
function CreateRotationMatrixX(const angle: Single): TGLMatrix; overload;
// Creates matrix for rotation about y-axis (angle in rad)
function CreateRotationMatrixY(const sine, cosine: Single): TGLMatrix; overload;
function CreateRotationMatrixY(const angle: Single): TGLMatrix; overload;
// Creates matrix for rotation about z-axis (angle in rad)
function CreateRotationMatrixZ(const sine, cosine: Single): TGLMatrix; overload;
function CreateRotationMatrixZ(const angle: Single): TGLMatrix; overload;
// Creates a rotation matrix along the given Axis by the given Angle in radians.
function CreateRotationMatrix(const anAxis: TAffineVector; angle: Single): TGLMatrix; overload;
function CreateRotationMatrix(const anAxis: TGLVector; angle: Single): TGLMatrix; overload; 
// Creates a rotation matrix along the given Axis by the given Angle in radians.
function CreateAffineRotationMatrix(const anAxis: TAffineVector; angle: Single): TAffineMatrix;

// Multiplies two 3x3 matrices
function MatrixMultiply(const m1, m2: TAffineMatrix): TAffineMatrix; overload; 
// Multiplies two 4x4 matrices
function MatrixMultiply(const m1, m2: TGLMatrix): TGLMatrix; overload;
// Multiplies M1 by M2 and places result in MResult
procedure MatrixMultiply(const m1, m2: TGLMatrix; var MResult: TGLMatrix); overload;

// Transforms a homogeneous vector by multiplying it with a matrix
function VectorTransform(const V: TGLVector; const M: TGLMatrix): TGLVector; overload; 
// Transforms a homogeneous vector by multiplying it with a matrix
function VectorTransform(const V: TGLVector; const M: TAffineMatrix): TGLVector; overload; 
// Transforms an affine vector by multiplying it with a matrix
function VectorTransform(const V: TAffineVector; const M: TGLMatrix): TAffineVector; overload; 
// Transforms an affine vector by multiplying it with a matrix
function VectorTransform(const V: TAffineVector; const M: TAffineMatrix): TAffineVector; overload; 

// Determinant of a 3x3 matrix
function MatrixDeterminant(const M: TAffineMatrix): Single; overload; 
// Determinant of a 4x4 matrix
function MatrixDeterminant(const M: TGLMatrix): Single; overload;

// Adjoint of a 4x4 matrix, used in the computation of the inverse of a 4x4 matrix 
procedure AdjointMatrix(var M: TGLMatrix); overload;
// Adjoint of a 3x3 matrix, used in the computation of the inverse of a 3x3 matrix 
procedure AdjointMatrix(var M: TAffineMatrix); overload;

// Multiplies all elements of a 3x3 matrix with a factor
procedure ScaleMatrix(var M: TAffineMatrix; const factor: Single); overload;
// Multiplies all elements of a 4x4 matrix with a factor
procedure ScaleMatrix(var M: TGLMatrix; const factor: Single); overload;

// Adds the translation vector into the matrix
procedure TranslateMatrix(var M: TGLMatrix; const V: TAffineVector); overload;
procedure TranslateMatrix(var M: TGLMatrix; const V: TGLVector); overload;

(* Normalize the matrix and remove the translation component.
  The resulting matrix is an orthonormal matrix (Y direction preserved, then Z) *)
procedure NormalizeMatrix(var M: TGLMatrix);

// Computes transpose of 3x3 matrix
procedure TransposeMatrix(var M: TAffineMatrix); overload;
// Computes transpose of 4x4 matrix
procedure TransposeMatrix(var M: TGLMatrix); overload;

// Finds the inverse of a 4x4 matrix
procedure InvertMatrix(var M: TGLMatrix); overload;
function MatrixInvert(const M: TGLMatrix): TGLMatrix; overload;

// Finds the inverse of a 3x3 matrix;
procedure InvertMatrix(var M: TAffineMatrix); overload;
function  MatrixInvert(const M: TAffineMatrix): TAffineMatrix; overload;

(* Finds the inverse of an angle preserving matrix.
  Angle preserving matrices can combine translation, rotation and isotropic
  scaling, other matrices won't be properly inverted by this function. *)
function AnglePreservingMatrixInvert(const mat: TGLMatrix): TGLMatrix;

(* Decompose a non-degenerated 4x4 transformation matrix into the sequence of transformations that produced it.
  Modified by ml then eg, original Author: Spencer W. Thomas, University of Michigan
  The coefficient of each transformation is returned in the corresponding
  element of the vector Tran. Returns true upon success, false if the matrix is singular. *)
function MatrixDecompose(const M: TGLMatrix; var Tran: TTransformations): Boolean;
function CreateLookAtMatrix(const eye, center, normUp: TGLVector): TGLMatrix;
function CreateMatrixFromFrustum(Left, Right, Bottom, Top, ZNear, ZFar: Single): TGLMatrix;
function CreatePerspectiveMatrix(FOV, Aspect, ZNear, ZFar: Single): TGLMatrix;
function CreateOrthoMatrix(Left, Right, Bottom, Top, ZNear, ZFar: Single): TGLMatrix;
function CreatePickMatrix(X, Y, deltax, deltay: Single; const viewport: TVector4i): TGLMatrix;
function Project(objectVector: TGLVector; const ViewProjMatrix: TGLMatrix; const viewport: TVector4i; out WindowVector: TGLVector): Boolean;
function UnProject(WindowVector: TGLVector; ViewProjMatrix: TGLMatrix; const viewport: TVector4i; out objectVector: TGLVector): Boolean;

(* ----------------------------------------------------------------------------
 Plane functions
 -----------------------------------------------------------------------------*)

// Computes the parameters of a plane defined by three points.
function PlaneMake(const p1, p2, p3: TAffineVector): THmgPlane; overload;
function PlaneMake(const p1, p2, p3: TGLVector): THmgPlane; overload;
// Computes the parameters of a plane defined by a point and a normal.
function PlaneMake(const point, normal: TAffineVector): THmgPlane; overload;
function PlaneMake(const point, normal: TGLVector): THmgPlane; overload;
// Converts from single to double representation
procedure SetPlane(var dest: TDoubleHmgPlane; const src: THmgPlane);
// Normalize a plane so that point evaluation = plane distance. }
procedure NormalizePlane(var plane: THmgPlane);

(* Calculates the cross-product between the plane normal and plane to point vector.
   This functions gives an hint as to were the point is, if the point is in the
   half-space pointed by the vector, result is positive.
   This function performs an homogeneous space dot-product. *)
function PlaneEvaluatePoint(const plane: THmgPlane; const point: TAffineVector): Single; overload;
function PlaneEvaluatePoint(const plane: THmgPlane; const point: TGLVector): Single; overload;

// Calculate the normal of a plane defined by three points.
function CalcPlaneNormal(const p1, p2, p3: TAffineVector): TAffineVector; overload;
procedure CalcPlaneNormal(const p1, p2, p3: TAffineVector; var vr: TAffineVector); overload;
procedure CalcPlaneNormal(const p1, p2, p3: TGLVector; var vr: TAffineVector); overload;

(* Returns true if point is in the half-space defined by a plane with normal.
   The plane itself is not considered to be in the tested halfspace. *)
function PointIsInHalfSpace(const point, planePoint, planeNormal: TGLVector): Boolean; overload;
function PointIsInHalfSpace(const point, planePoint, planeNormal: TAffineVector): Boolean; overload;
function PointIsInHalfSpace(const point: TAffineVector; const plane: THmgPlane): Boolean; overload;

(* Computes algebraic distance between point and plane.
  Value will be positive if the point is in the halfspace pointed by the normal,
  negative on the other side. *)
function PointPlaneDistance(const point, planePoint, planeNormal: TGLVector): Single; overload;
function PointPlaneDistance(const point, planePoint, planeNormal: TAffineVector): Single; overload;
function PointPlaneDistance(const point: TAffineVector; const plane: THmgPlane): Single; overload;

// Computes point to plane projection. Plane and direction have to be normalized
function PointPlaneOrthoProjection(const point: TAffineVector; const plane: THmgPlane; var inter: TAffineVector; bothface: Boolean = True): Boolean;
function PointPlaneProjection(const point, direction: TAffineVector; const plane: THmgPlane; var inter: TAffineVector; bothface: Boolean = True): Boolean;

// Computes segment / plane intersection return false if there isn't an intersection
function SegmentPlaneIntersection(const ptA, ptB: TAffineVector; const plane: THmgPlane; var inter: TAffineVector): Boolean;

// Computes point to triangle projection. Direction has to be normalized
function PointTriangleOrthoProjection(const point, ptA, ptB, ptC: TAffineVector; var inter: TAffineVector; bothface: Boolean = True): Boolean;
function PointTriangleProjection(const point, direction, ptA, ptB, ptC: TAffineVector; var inter: TAffineVector; bothface: Boolean = True): Boolean;

// Returns true if line intersect ABC triangle
function IsLineIntersectTriangle(const point, direction, ptA, ptB, ptC: TAffineVector): Boolean;
// Computes point to Quad projection. Direction has to be normalized. Quad have to be flat and convex
function PointQuadOrthoProjection(const point, ptA, ptB, ptC, ptD: TAffineVector; var inter: TAffineVector; bothface: Boolean = True): Boolean;
function PointQuadProjection(const point, direction, ptA, ptB, ptC, ptD: TAffineVector; var inter: TAffineVector; bothface: Boolean = True): Boolean;

// Returns true if line intersect ABCD quad. Quad have to be flat and convex
function IsLineIntersectQuad(const point, direction, ptA, ptB, ptC, ptD: TAffineVector): Boolean;

// Computes point to disk projection. Direction has to be normalized
function PointDiskOrthoProjection(const point, center, up: TAffineVector; const radius: Single; var inter: TAffineVector;  bothface: Boolean = True): Boolean;
function PointDiskProjection(const point, direction, center, up: TAffineVector; const radius: Single; var inter: TAffineVector;  bothface: Boolean = True): Boolean;

// Computes closest point on a segment (a segment is a limited line)
function PointSegmentClosestPoint(const point, segmentStart, segmentStop: TAffineVector): TAffineVector; overload;
function PointSegmentClosestPoint(const point, segmentStart, segmentStop: TGLVector): TGLVector; overload;

// Computes algebraic distance between segment and line (a segment is a limited line)
function PointSegmentDistance(const point, segmentStart, segmentStop: TAffineVector): Single;

// Computes closest point on a line
function PointLineClosestPoint(const point, linePoint, lineDirection: TAffineVector): TAffineVector;

// Computes algebraic distance between point and line
function PointLineDistance(const point, linePoint, lineDirection: TAffineVector): Single;

// Computes the closest points (2) given two segments
procedure SegmentSegmentClosestPoint(const S0Start, S0Stop, S1Start,
  S1Stop: TAffineVector; var Segment0Closest, Segment1Closest: TAffineVector);

// Computes the closest distance between two segments
function SegmentSegmentDistance(const S0Start, S0Stop, S1Start, S1Stop: TAffineVector): Single;

// Computes the closest distance between two lines
function LineLineDistance(const linePt0, lineDir0, linePt1, lineDir1: TAffineVector): Single;

(* ----------------------------------------------------------------------------
  Quaternion functions
  ----------------------------------------------------------------------------*)
type
  TEulerOrder = (eulXYZ, eulXZY, eulYXZ, eulYZX, eulZXY, eulZYX);

// Creates a quaternion from the given values
function QuaternionMake(const Imag: array of Single; Real: Single): TQuaternion; overload;
function QuaternionMake(const X,Y,Z,W: Single): TQuaternion; overload;
function QuaternionMake(const V: TGLVector): TQuaternion; overload;

// Returns the conjugate of a quaternion
function QuaternionConjugate(const Q: TQuaternion): TQuaternion;
// Returns the magnitude of the quaternion
function QuaternionMagnitude(const Q: TQuaternion): Single;
// Normalizes the given quaternion
procedure NormalizeQuaternion(var Q: TQuaternion);

// Constructs a unit quaternion from two points on unit sphere
function QuaternionFromPoints(const V1, V2: TAffineVector): TQuaternion;
// Converts a unit quaternion into two points on a unit sphere
procedure QuaternionToPoints(const Q: TQuaternion; var ArcFrom, ArcTo: TAffineVector);
// Constructs a unit quaternion from a rotation matrix
function QuaternionFromMatrix(const mat: TGLMatrix): TQuaternion;
(* Constructs a rotation matrix from (possibly non-unit) quaternion.
  Assumes matrix is used to multiply column vector on the left: vnew = mat vold.
  Works correctly for right-handed coordinate system and right-handed rotations *)
function QuaternionToMatrix(quat: TQuaternion): TGLMatrix;
// Constructs an affine rotation matrix from (possibly non-unit) quaternion
function QuaternionToAffineMatrix(quat: TQuaternion): TAffineMatrix;
// Constructs quaternion from angle (in deg) and axis
function QuaternionFromAngleAxis(const angle: Single; const axis: TAffineVector): TQuaternion;
// Constructs quaternion from Euler angles
function QuaternionFromRollPitchYaw(const r, p, Y: Single): TQuaternion;
// Constructs quaternion from Euler angles in arbitrary order (angles in degrees)
function QuaternionFromEuler(const X, Y, Z: Single; eulerOrder: TEulerOrder): TQuaternion;
(* Returns quaternion product qL * qR. Note: order is important!
  To combine rotations, use the product QuaternionMuliply(qSecond, qFirst),
  which gives the effect of rotating by qFirst then qSecond *)
function QuaternionMultiply(const qL, qR: TQuaternion): TQuaternion;
(* Spherical linear interpolation of unit quaternions with spins.
  QStart, QEnd - start and end unit quaternions
  t            - interpolation parameter (0 to 1)
  Spin         - number of extra spin rotations to involve *)
function QuaternionSlerp(const QStart, QEnd: TQuaternion; Spin: Integer; T: Single): TQuaternion; overload;
function QuaternionSlerp(const source, dest: TQuaternion; const T: Single): TQuaternion; overload;

(* ----------------------------------------------------------------------------
  Exponential functions
 -----------------------------------------------------------------------------*)
function Logarithm2(const X: Single): Single; inline;
// Raise base to any power. For fractional exponents, or |exponents| > MaxInt, base must be > 0
function PowerSingle(const Base, Exponent: Single): Single; overload;
// Raise base to an integer
function PowerInteger(Base: Single; Exponent: Integer): Single; overload;
function PowerInt64(Base: Single; Exponent: Int64): Single; overload;

(* ----------------------------------------------------------------------------
   Trigonometric functions
  ----------------------------------------------------------------------------*)
function DegToRadian(const Degrees: Extended): Extended; overload;
function DegToRadian(const Degrees: Single): Single; overload;
function RadianToDeg(const Radians: Extended): Extended; overload;
function RadianToDeg(const Radians: Single): Single; overload; 

// Normalize to an angle in the [-PI; +PI] range
function NormalizeAngle(angle: Single): Single; 
// Normalize to an angle in the [-180; 180] range
function NormalizeDegAngle(angle: Single): Single; 

// Calculates sine and cosine from the given angle Theta
procedure SinCosine(const Theta: Double; out Sin, Cos: Double); overload;
// Calculates sine and cosine from the given angle Theta
procedure SinCosine(const Theta: Single; out Sin, Cos: Single); overload;
(* Calculates sine and cosine from the given angle Theta and Radius.
  sin and cos values calculated from theta are multiplicated by radius *)
procedure SinCosine(const Theta, radius: Double; out Sin, Cos: Double); overload;
(* Calculates sine and cosine from the given angle Theta and Radius.
  sin and cos values calculated from theta are multiplicated by radius *)
procedure SinCosine(const Theta, radius: Single; out Sin, Cos: Single); overload;

(* Fills up the two given dynamic arrays with sin cos values.
  start and stop angles must be given in degrees, the number of steps is
  determined by the length of the given arrays. *)
procedure PrepareSinCosCache(var S, c: array of Single; startAngle, stopAngle: Single);
function ArcCosine(const X: Extended): Extended; overload;
// Fast ArcTangent2 approximation, about 0.07 rads accuracy
function FastArcTangent2(Y, X: Single): Single;

// ------------------------------------------------------------------------------
// Miscellanious math functions
// ------------------------------------------------------------------------------

// Computes 1/Sqrt(v)
function RSqrt(V: Single): Single;
// Computes 1/Sqrt(Sqr(x)+Sqr(y)).
function RLength(X, Y: Single): Single;
// Computes an integer sqrt approximation
function ISqrt(i: Integer): Integer;
// Computes an integer length Result:=Sqrt(x*x+y*y)
function ILength(X, Y: Integer): Integer; overload;
function ILength(X, Y, Z: Integer): Integer; overload;

// Generates a random point on the unit sphere.
// Point repartition is correctly isotropic with no privilegied direction
procedure RandomPointOnSphere(var p: TAffineVector);

// Rounds the floating point value to the closest integer.
// Behaves like Round but returns a floating point value like Int.
function RoundInt(V: Single): Single; overload;
function RoundInt(V: Extended): Extended; overload;

// Multiples i by s and returns the rounded result.
function ScaleAndRound(i: Integer; var S: Single): Integer;

// Returns the sign of the x value using the (-1, 0, +1) convention
function SignStrict(X: Single): Integer;

// Returns True if x is in [a; b]
function IsInRange(const X, a, b: Single): Boolean; overload;
function IsInRange(const X, a, b: Double): Boolean; overload;

// Returns True if p is in the cube defined by d.
function IsInCube(const p, d: TAffineVector): Boolean; overload;
function IsInCube(const p, d: TGLVector): Boolean; overload;

// Returns the minimum value of the array.
function MinFloat(values: PSingleArray; nbItems: Integer): Single; overload;
function MinFloat(values: PDoubleArray; nbItems: Integer): Double; overload;
function MinFloat(values: PExtendedArray; nbItems: Integer): Extended; overload;
// Returns the minimum of given values.
function MinFloat(const V1, V2: Single): Single; overload; 
function MinFloat(const V: array of Single): Single; overload;
function MinFloat(const V1, V2: Double): Double; overload;
{$IFDEF USE_PLATFORM_HAS_EXTENDED}
function MinFloat(const V1, V2: Extended): Extended; overload;
{$ENDIF}
function MinFloat(const V1, V2, V3: Single): Single; overload;
function MinFloat(const V1, V2, V3: Double): Double; overload;
{$IFDEF USE_PLATFORM_HAS_EXTENDED}
function MinFloat(const V1, V2, V3: Extended): Extended; overload;
{$ENDIF}
// Returns the maximum value of the array.
function MaxFloat(values: PSingleArray; nbItems: Integer): Single; overload;
function MaxFloat(values: PDoubleArray; nbItems: Integer): Double; overload;
function MaxFloat(values: PExtendedArray; nbItems: Integer): Extended; overload;
function MaxFloat(const V: array of Single): Single; overload;
// Returns the maximum of given values.
function MaxFloat(const V1, V2: Single): Single; overload;
function MaxFloat(const V1, V2: Double): Double; overload;
{$IFDEF USE_PLATFORM_HAS_EXTENDED}
function MaxFloat(const V1, V2: Extended): Extended; overload;
{$ENDIF USE_PLATFORM_HAS_EXTENDED}
function MaxFloat(const V1, V2, V3: Single): Single; overload;
function MaxFloat(const V1, V2, V3: Double): Double; overload;
{$IFDEF USE_PLATFORM_HAS_EXTENDED}
function MaxFloat(const V1, V2, V3: Extended): Extended; overload;
{$ENDIF USE_PLATFORM_HAS_EXTENDED}
function MinInteger(const V1, V2: Integer): Integer; overload;
function MinInteger(const V1, V2: Cardinal): Cardinal; overload;
function MinInteger(const V1, V2, V3: Integer): Integer; overload;
function MinInteger(const V1, V2, V3: Cardinal): Cardinal; overload;
function MaxInteger(const V1, V2: Integer): Integer; overload;
function MaxInteger(const V1, V2: Cardinal): Cardinal; overload;
function MaxInteger(const V1, V2, V3: Integer): Integer; overload;
function MaxInteger(const V1, V2, V3: Cardinal): Cardinal; overload;

function ClampInteger(const value, min, max: Integer): Integer; overload; inline;
function ClampInteger(const value, min, max: Cardinal): Cardinal; overload; inline;
// Computes the triangle's area
function TriangleArea(const p1, p2, p3: TAffineVector): Single; overload;
// Computes the polygons's area. Points must be coplanar. Polygon needs not be convex
function PolygonArea(const p: PAffineVectorArray; nSides: Integer): Single; overload;
// Computes a 2D triangle's signed area. Only X and Y coordinates are used, Z is ignored
function TriangleSignedArea(const p1, p2, p3: TAffineVector): Single; overload;
// Computes a 2D polygon's signed area. Only X and Y coordinates are used, Z is ignored. Polygon needs not be convex
function PolygonSignedArea(const p: PAffineVectorArray; nSides: Integer): Single; overload;

(* Multiplies values in the array by factor.
  This function is especially efficient for large arrays, it is not recommended
  for arrays that have less than 10 items.
  Expected performance is 4 to 5 times that of a Deliph-compiled loop on AMD
  CPUs, and 2 to 3 when 3DNow! isn't available *)
procedure ScaleFloatArray(values: PSingleArray; nb: Integer; var factor: Single); overload;
procedure ScaleFloatArray(var values: TSingleArray; factor: Single); overload;

// Adds delta to values in the array. Array size must be a multiple of four
procedure OffsetFloatArray(values: PSingleArray; nb: Integer; var delta: Single); overload;
procedure OffsetFloatArray(var values: array of Single; delta: Single); overload;
procedure OffsetFloatArray(valuesDest, valuesDelta: PSingleArray; nb: Integer); overload;

// Returns the max of the X, Y and Z components of a vector (W is ignored)
function MaxXYZComponent(const V: TGLVector): Single; overload;
function MaxXYZComponent(const V: TAffineVector): Single; overload;
// Returns the min of the X, Y and Z components of a vector (W is ignored)
function MinXYZComponent(const V: TGLVector): Single; overload;
function MinXYZComponent(const V: TAffineVector): Single; overload;
// Returns the max of the Abs(X), Abs(Y) and Abs(Z) components of a vector (W is ignored)
function MaxAbsXYZComponent(V: TGLVector): Single;
// Returns the min of the Abs(X), Abs(Y) and Abs(Z) components of a vector (W is ignored)
function MinAbsXYZComponent(V: TGLVector): Single;
// Replace components of v with the max of v or v1 component. Maximum is computed per component
procedure MaxVector(var V: TGLVector; const V1: TGLVector); overload;
procedure MaxVector(var V: TAffineVector; const V1: TAffineVector); overload;
// Replace components of v with the min of v or v1 component. Minimum is computed per component
procedure MinVector(var V: TGLVector; const V1: TGLVector); overload;
procedure MinVector(var V: TAffineVector; const V1: TAffineVector); overload;

// Sorts given array in ascending order. NOTE : current implementation is a slow bubble sort...
procedure SortArrayAscending(var a: array of Extended);

// Clamps aValue in the aMin-aMax interval
function ClampValue(const aValue, aMin, aMax: Single): Single; overload;
// Clamps aValue in the aMin-INF interval
function ClampValue(const aValue, aMin: Single): Single; overload;

// Returns the detected optimization mode. Returned values is either 'FPU', '3DNow!' or 'SSE'
function GeometryOptimizationMode: String;

(* Begins a FPU-only section.
  You can use a FPU-only section to force use of FPU versions of the math
  functions, though typically slower than their SIMD counterparts, they have
  a higher precision (80 bits internally) that may be required in some cases.
  Each BeginFPUOnlySection call must be balanced by a EndFPUOnlySection (calls
  can be nested). *)
procedure BeginFPUOnlySection;
// Ends a FPU-only section. See BeginFPUOnlySection
procedure EndFPUOnlySection;

// --------------------- Unstandardized functions after these lines

// Mixed functions
// Turn a triplet of rotations about x, y, and z (in that order) into an equivalent rotation around a single axis (all in radians)
function ConvertRotation(const Angles: TAffineVector): TGLVector;

// Miscellaneous functions
function MakeAffineDblVector(var V: array of Double): TAffineDblVector;
function MakeDblVector(var V: array of Double): THomogeneousDblVector;
// Converts a vector containing double sized values into a vector with single sized values
function VectorAffineDblToFlt(const V: TAffineDblVector): TAffineVector;
// Converts a vector containing double sized values into a vector with single sized values
function VectorDblToFlt(const V: THomogeneousDblVector): THomogeneousVector;
// Converts a vector containing single sized values into a vector with double sized values
function VectorAffineFltToDbl(const V: TAffineVector): TAffineDblVector;
// Converts a vector containing single sized values into a vector with double sized values
function VectorFltToDbl(const V: TGLVector): THomogeneousDblVector;
(* The code below is from Wm. Randolph Franklin <wrf@ecse.rpi.edu>
  with some minor modifications for speed. It returns 1 for strictly
  interior points, 0 for strictly exterior, and 0 or 1 for points on the boundary *)
function PointInPolygon(const xp, yp: array of Single; X, Y: Single): Boolean;
// PtInRegion
function IsPointInPolygon(const Polygon: array of TPoint; const p: TPoint): Boolean;
procedure DivMod(Dividend: Integer; Divisor: Word; var result, Remainder: Word);

// Coordinate system manipulation functions

// Rotates the given coordinate system (represented by the matrix) around its Y-axis
function Turn(const Matrix: TGLMatrix; Angle: Single): TGLMatrix; overload;
// Rotates the given coordinate system (represented by the matrix) around MasterUp
function Turn(const Matrix: TGLMatrix; const MasterUp: TAffineVector; Angle: Single): TGLMatrix; overload;
// Rotates the given coordinate system (represented by the matrix) around its X-axis
function Pitch(const Matrix: TGLMatrix; Angle: Single): TGLMatrix; overload;
// Rotates the given coordinate system (represented by the matrix) around MasterRight
function Pitch(const Matrix: TGLMatrix; const MasterRight: TAffineVector; Angle: Single): TGLMatrix; overload;
// Rotates the given coordinate system (represented by the matrix) around its Z-axis
function Roll(const Matrix: TGLMatrix; Angle: Single): TGLMatrix; overload;
// Rotates the given coordinate system (represented by the matrix) around MasterDirection
function Roll(const Matrix: TGLMatrix; const MasterDirection: TAffineVector; Angle: Single): TGLMatrix; overload;

// Intersection functions

(* Compute the intersection point "res" of a line with a plane.
  Return value:
  0 : no intersection, line parallel to plane
  1 : res is valid
  -1 : line is inside plane

  Adapted from:
  E.Hartmann, Computeruntersttzte Darstellende Geometrie, B.G. Teubner Stuttgart 1988 *)
function IntersectLinePlane(const point, direction: TGLVector;
  const plane: THmgPlane; intersectPoint: PGLVector = nil): Integer; overload;

(* Compute intersection between a triangle and a box.
  Returns True if an intersection was found *)
function IntersectTriangleBox(const p1, p2, p3, aMinExtent, aMaxExtent: TAffineVector): Boolean;

(* Compute intersection between a Sphere and a box.
   Up, Direction and Right must be normalized!
   Use CubDepth, CubeHeight and CubeWidth to scale TGLCube *)
function IntersectSphereBox(const SpherePos: TGLVector;
  const SphereRadius: Single; const BoxMatrix: TGLMatrix;
  const BoxScale: TAffineVector; intersectPoint: PAffineVector = nil;
  normal: PAffineVector = nil; depth: PSingle = nil): Boolean;

(* Compute intersection between a ray and a plane.
  Returns True if an intersection was found, the intersection point is placed
  in intersectPoint is the reference is not nil *)
function RayCastPlaneIntersect(const rayStart, rayVector: TGLVector;
  const planePoint, planeNormal: TGLVector; intersectPoint: PGLVector = nil): Boolean; overload;
function RayCastPlaneXZIntersect(const rayStart, rayVector: TGLVector;
  const planeY: Single; intersectPoint: PGLVector = nil): Boolean; overload;

// Compute intersection between a ray and a triangle
function RayCastTriangleIntersect(const rayStart, rayVector: TGLVector;
  const p1, p2, p3: TAffineVector; intersectPoint: PGLVector = nil;
  intersectNormal: PGLVector = nil): Boolean; overload;
// Compute the min distance a ray will pass to a point
function RayCastMinDistToPoint(const rayStart, rayVector: TGLVector; const point: TGLVector): Single;
// Determines if a ray will intersect with a given sphere
function RayCastIntersectsSphere(const rayStart, rayVector: TGLVector;
  const sphereCenter: TGLVector; const SphereRadius: Single): Boolean; overload;
(* Calculates the intersections between a sphere and a ray.
   Returns 0 if no intersection is found (i1 and i2 untouched), 1 if one
   intersection was found (i1 defined, i2 untouched), and 2 is two intersections
   were found (i1 and i2 defined) *)
function RayCastSphereIntersect(const rayStart, rayVector: TGLVector;
  const sphereCenter: TGLVector; const SphereRadius: Single; var i1, i2: TGLVector): Integer; overload;
(* Compute intersection between a ray and a box.
   Returns True if an intersection was found, the intersection point is
   placed in intersectPoint if the reference is not nil *)
function RayCastBoxIntersect(const rayStart, rayVector, aMinExtent,
  aMaxExtent: TAffineVector; intersectPoint: PAffineVector = nil): Boolean;

(* Some 2d intersection functions *)
// Determine if 2 rectanges intersect
function RectanglesIntersect(const ACenterOfRect1, ACenterOfRect2, ASizeOfRect1,
  ASizeOfRect2: TVector2f): Boolean;
// Determine if BigRect completely contains SmallRect
function RectangleContains(const ACenterOfBigRect1, ACenterOfSmallRect2,
  ASizeOfBigRect1, ASizeOfSmallRect2: TVector2f;
  const AEps: Single = 0.0): Boolean;
(* Computes the visible radius of a sphere in a perspective projection.
  This radius can be used for occlusion culling (cone extrusion) or 2D
  intersection testing. *)
function SphereVisibleRadius(distance, radius: Single): Single;
// Extracts a TFrustum for combined modelview and projection matrices
function ExtractFrustumFromModelViewProjection(const modelViewProj: TGLMatrix): TFrustum;
// Determines if volume is clipped or not
function IsVolumeClipped(const objPos: TAffineVector; const objRadius: Single;
  const Frustum: TFrustum): Boolean; overload;
function IsVolumeClipped(const objPos: TGLVector; const objRadius: Single;
  const Frustum: TFrustum): Boolean; overload; inline;
function IsVolumeClipped(const min, max: TAffineVector; const Frustum: TFrustum): Boolean; overload; inline;

(* Misc funcs *)

(* Creates a parallel projection matrix.
  Transformed points will projected on the plane along the specified direction *)
function MakeParallelProjectionMatrix(const plane: THmgPlane; const dir: TGLVector): TGLMatrix;
(* Creates a shadow projection matrix.
  Shadows will be projected onto the plane defined by planePoint and planeNormal,
  from lightPos *)
function MakeShadowMatrix(const planePoint, planeNormal, lightPos: TGLVector): TGLMatrix;
(* Builds a reflection matrix for the given plane.
  Reflection matrix allow implementing planar reflectors (mirrors) *)
function MakeReflectionMatrix(const planePoint, planeNormal: TAffineVector): TGLMatrix;
(* Packs an homogeneous rotation matrix to 6 bytes.
  The 6:64 (or 6:36) compression ratio is achieved by computing the quaternion
  associated to the matrix and storing its Imaginary components at 16 bits
  precision each. Deviation is typically below 0.01% and around 0.1% in worst case situations.
  Note: quaternion conversion is faster and more robust than an angle decomposition *)
function PackRotationMatrix(const mat: TGLMatrix): TPackedRotationMatrix;
// Restores a packed rotation matrix. See PackRotationMatrix
function UnPackRotationMatrix(const packedMatrix: TPackedRotationMatrix): TGLMatrix;

(*Calculates angles for the Camera.MoveAroundTarget(pitch, turn) procedure.
  Initially from then GLCameraColtroller unit, requires AOriginalUpVector to contain only -1, 0 or 1.
  Result contains pitch and turn angles *)
function GetSafeTurnAngle(const AOriginalPosition, AOriginalUpVector,
  ATargetPosition, AMoveAroundTargetCenter: TGLVector): TVector2f; overload;
function GetSafeTurnAngle(const AOriginalPosition, AOriginalUpVector,
  ATargetPosition, AMoveAroundTargetCenter: TAffineVector): TVector2f; overload;

// Extracted from Camera.MoveAroundTarget(pitch, turn)
function MoveObjectAround(const AMovingObjectPosition, AMovingObjectUp,
  ATargetPosition: TGLVector; pitchDelta, turnDelta: Single): TGLVector;

// Calcualtes Angle between 2 Vectors: (A-CenterPoint) and (B-CenterPoint). In radians
function AngleBetweenVectors(const a, b, ACenterPoint: TGLVector): Single; overload;
function AngleBetweenVectors(const a, b, ACenterPoint: TAffineVector): Single; overload;

(*AOriginalPosition - Object initial position.
  ACenter - some point, from which is should be distanced.
  ADistance + AFromCenterSpot - distance, which object should keep from ACenter or
  ADistance + not AFromCenterSpot - distance, which object should shift
  from his current position away from center *)
function ShiftObjectFromCenter(const AOriginalPosition: TGLVector;
  const ACenter: TGLVector; const ADistance: Single;
  const AFromCenterSpot: Boolean): TGLVector; overload;

function ShiftObjectFromCenter(const AOriginalPosition: TAffineVector;
  const ACenter: TAffineVector; const ADistance: Single;
  const AFromCenterSpot: Boolean): TAffineVector; overload;

const
  cPI: Single = 3.141592654;
  cPIdiv180: Single = 0.017453292;
  c180divPI: Single = 57.29577951;
  c2PI: Single = 6.283185307;
  cPIdiv2: Single = 1.570796326;
  cPIdiv4: Single = 0.785398163;
  c3PIdiv2: Single = 4.71238898;
  c3PIdiv4: Single = 2.35619449;
  cInv2PI: Single = 1 / 6.283185307;
  cInv360: Single = 1 / 360;
  c180: Single = 180;
  c360: Single = 360;
  cOneHalf: Single = 0.5;
  cLn10: Single = 2.302585093;

  // Ranges of the IEEE floating point types, including denormals
  // with Math.pas compatible name
  MinSingle = 1.5E-45;
  MaxSingle = 3.4E+38;
  MinDouble = 5.0E-324;
  MaxDouble = 1.7E+308;
  MinExtended = 3.4E-4932;
  MaxExtended = MaxDouble; //1.1E+4932 <-Overflowing in c++;
  MinComp = -9.223372036854775807E+18;
  MaxComp = 9.223372036854775807E+18;

var
  (* This var is adjusted during "initialization", current values are
    + 0 : use standard optimized FPU code
    + 1 : use 3DNow! optimized code (requires K6-2/3 CPU)
    + 2 : use Intel SSE code (Pentium III, NOT IMPLEMENTED YET !) *)
  vSIMD: Byte = 0;

// ==============================================================
implementation
// ==============================================================

const
{$IFDEF USE_ASM}
  // FPU status flags (high order byte)
  cwChop: Word = $1F3F;
{$ENDIF}
  // to be used as descriptive indices
  X = 0;
  Y = 1;
  Z = 2;
  W = 3;

  cZero: Single = 0.0;
  cOne: Single = 1.0;
  cOneDotFive: Single = 0.5;

function GeometryOptimizationMode: String;
begin
  case vSIMD of
    0: result := 'FPU';
    1: result := '3DNow!';
    2: result := 'SSE';
  else
    result := '*ERR*';
  end;
end;

var
  vOldSIMD: Byte;
  vFPUOnlySectionCounter: Integer;

procedure BeginFPUOnlySection;
begin
  if vFPUOnlySectionCounter = 0 then
    vOldSIMD := vSIMD;
  Inc(vFPUOnlySectionCounter);
  vSIMD := 0;
end;

procedure EndFPUOnlySection;
begin
  Dec(vFPUOnlySectionCounter);
  Assert(vFPUOnlySectionCounter >= 0);
  if vFPUOnlySectionCounter = 0 then
    vSIMD := vOldSIMD;
end;

// ------------------------------------------------------------------------------
// ----------------- vector functions -------------------------------------------
// ------------------------------------------------------------------------------

function TexPointMake(const S, T: Single): TTexPoint;
begin
  result.S := S;
  result.T := T;
end;

function AffineVectorMake(const X, Y, Z: Single): TAffineVector; overload;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
end;

function AffineVectorMake(const V: TGLVector): TAffineVector;
begin
  result.X := V.X;
  result.Y := V.Y;
  result.Z := V.Z;
end;

procedure SetAffineVector(out V: TAffineVector; const X, Y, Z: Single);
begin
  V.X := X;
  V.Y := Y;
  V.Z := Z;
end;

procedure SetVector(out V: TAffineVector; const X, Y, Z: Single);
begin
  V.X := X;
  V.Y := Y;
  V.Z := Z;
end;

procedure SetVector(out V: TAffineVector; const vSrc: TGLVector);
begin
  V.X := vSrc.X;
  V.Y := vSrc.Y;
  V.Z := vSrc.Z;
end;

procedure SetVector(out V: TAffineVector; const vSrc: TAffineVector);
begin
  V.X := vSrc.X;
  V.Y := vSrc.Y;
  V.Z := vSrc.Z;
end;

procedure SetVector(out V: TAffineDblVector; const vSrc: TAffineVector);
begin
  V.X := vSrc.X;
  V.Y := vSrc.Y;
  V.Z := vSrc.Z;
end;

procedure SetVector(out V: TAffineDblVector; const vSrc: TGLVector);
begin
  V.X := vSrc.X;
  V.Y := vSrc.Y;
  V.Z := vSrc.Z;
end;

function VectorMake(const V: TAffineVector; W: Single = 0): TGLVector;
begin
  result.X := V.X;
  result.Y := V.Y;
  result.Z := V.Z;
  result.W := W;
end;

function VectorMake(const X, Y, Z: Single; W: Single = 0): TGLVector;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
  result.W := W;
end;

function VectorMake(const Q: TQuaternion): TGLVector; overload; inline;
begin
  result.X := Q.X;
  result.Y := Q.Y;
  result.Z := Q.Z;
  result.W := Q.W;
end;

function PointMake(const X, Y, Z: Single): TGLVector; overload;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
  result.W := 1;
end;

function PointMake(const V: TAffineVector): TGLVector; overload;
begin
  result.X := V.X;
  result.Y := V.Y;
  result.Z := V.Z;
  result.W := 1;
end;

function PointMake(const V: TGLVector): TGLVector; overload;
begin
  result.X := V.X;
  result.Y := V.Y;
  result.Z := V.Z;
  result.W := 1;
end;

procedure SetVector(out V: TGLVector; const X, Y, Z: Single; W: Single = 0);
begin
  V.X := X;
  V.Y := Y;
  V.Z := Z;
  V.W := W;
end;

procedure SetVector(out V: TGLVector; const av: TAffineVector; W: Single = 0);
begin
  V.X := av.X;
  V.Y := av.Y;
  V.Z := av.Z;
  V.W := W;
end;

procedure SetVector(out V: TGLVector; const vSrc: TGLVector);
begin
  // faster than memcpy, move or ':=' on the TGLVector...
  V.X := vSrc.X;
  V.Y := vSrc.Y;
  V.Z := vSrc.Z;
  V.W := vSrc.W;
end;

procedure MakePoint(out V: TGLVector; const X, Y, Z: Single);
begin
  V.X := X;
  V.Y := Y;
  V.Z := Z;
  V.W := 1.0;
end;

procedure MakePoint(out V: TGLVector; const av: TAffineVector);
begin
  V.X := av.X;
  V.Y := av.Y;
  V.Z := av.Z;
  V.W := 1.0; // cOne
end;

procedure MakePoint(out V: TGLVector; const av: TGLVector);
begin
  V.X := av.X;
  V.Y := av.Y;
  V.Z := av.Z;
  V.W := 1.0; // cOne
end;

procedure MakeVector(out V: TAffineVector; const X, Y, Z: Single); overload;
begin
  V.X := X;
  V.Y := Y;
  V.Z := Z;
end;

procedure MakeVector(out V: TGLVector; const X, Y, Z: Single);
begin
  V.X := X;
  V.Y := Y;
  V.Z := Z;
  V.W := 0.0 // cZero;
end;

procedure MakeVector(out V: TGLVector; const av: TAffineVector);
begin
  V.X := av.X;
  V.Y := av.Y;
  V.Z := av.Z;
  V.W := 0.0 // cZero;
end;

procedure MakeVector(out V: TGLVector; const av: TGLVector);
begin
  V.X := av.X;
  V.Y := av.Y;
  V.Z := av.Z;
  V.W := 0.0; // cZero;
end;

procedure RstVector(var V: TAffineVector);
begin
  V.X := 0;
  V.Y := 0;
  V.Z := 0;
end;

procedure RstVector(var V: TGLVector);
begin
  V.X := 0;
  V.Y := 0;
  V.Z := 0;
  V.W := 0;
end;

function VectorAdd(const V1, V2: TVector2f): TVector2f;
begin
  result.X := V1.X + V2.X;
  result.Y := V1.Y + V2.Y;
end;

function VectorAdd(const V1, V2: TAffineVector): TAffineVector;
begin
  result.X := V1.X + V2.X;
  result.Y := V1.Y + V2.Y;
  result.Z := V1.Z + V2.Z;
end;

procedure VectorAdd(const V1, V2: TAffineVector; var vr: TAffineVector); overload;
begin
  vr.X := V1.X + V2.X;
  vr.Y := V1.Y + V2.Y;
  vr.Z := V1.Z + V2.Z;
end;

procedure VectorAdd(const V1, V2: TAffineVector; vr: PAffineVector); overload;
begin
  vr^.X := V1.X + V2.X;
  vr^.Y := V1.Y + V2.Y;
  vr^.Z := V1.Z + V2.Z;

end;

function VectorAdd(const V1, V2: TGLVector): TGLVector;
begin
  result.X := V1.X + V2.X;
  result.Y := V1.Y + V2.Y;
  result.Z := V1.Z + V2.Z;
  result.W := V1.W + V2.W;
end;

procedure VectorAdd(const V1, V2: TGLVector; var vr: TGLVector);
begin
  vr.X := V1.X + V2.X;
  vr.Y := V1.Y + V2.Y;
  vr.Z := V1.Z + V2.Z;
  vr.W := V1.W + V2.W;
end;


function VectorAdd(const V: TAffineVector; const f: Single): TAffineVector;
begin
  result.X := V.X + f;
  result.Y := V.Y + f;
  result.Z := V.Z + f;
end;

function VectorAdd(const V: TGLVector; const f: Single): TGLVector;
begin
  result.X := V.X + f;
  result.Y := V.Y + f;
  result.Z := V.Z + f;
  result.W := V.W + f;
end;

function PointAdd(var V1: TGLVector; const V2: TGLVector): TGLVector;
begin
  result.X := V1.X + V2.X;
  result.Y := V1.Y + V2.Y;
  result.Z := V1.Z + V2.Z;
  result.W := 1;
end;

procedure AddVector(var V1: TAffineVector; const V2: TAffineVector);
begin
  V1.X := V1.X + V2.X;
  V1.Y := V1.Y + V2.Y;
  V1.Z := V1.Z + V2.Z;
end;

procedure AddVector(var V1: TAffineVector; const V2: TGLVector);
begin
  V1.X := V1.X + V2.X;
  V1.Y := V1.Y + V2.Y;
  V1.Z := V1.Z + V2.Z;
end;
procedure AddVector(var V1: TGLVector; const V2: TGLVector);
begin
  V1.X := V1.X + V2.X;
  V1.Y := V1.Y + V2.Y;
  V1.Z := V1.Z + V2.Z;
  V1.W := V1.W + V2.W;
end;

procedure AddVector(var V: TAffineVector; const f: Single);
begin
  V.X := V.X + f;
  V.Y := V.Y + f;
  V.Z := V.Z + f;
end;

procedure AddVector(var V: TGLVector; const f: Single);
begin
  V.X := V.X + f;
  V.Y := V.Y + f;
  V.Z := V.Z + f;
  V.W := V.W + f;
end;

procedure AddPoint(var V1: TGLVector; const V2: TGLVector);
begin
  V1.X := V1.X + V2.X;
  V1.Y := V1.Y + V2.Y;
  V1.Z := V1.Z + V2.Z;
  V1.W := 1;
end;

procedure TexPointArrayAdd(const src: PTexPointArray; const delta: TTexPoint;
  const nb: Integer; dest: PTexPointArray); overload;
var
  i: Integer;
begin
  for i := 0 to nb - 1 do
  begin
    dest^[i].S := src^[i].S + delta.S;
    dest^[i].T := src^[i].T + delta.T;
  end;
end;

procedure TexPointArrayScaleAndAdd(const src: PTexPointArray;
  const delta: TTexPoint; const nb: Integer; const scale: TTexPoint;
  dest: PTexPointArray); overload;
var
  i: Integer;
begin
  for i := 0 to nb - 1 do
  begin
    dest^[i].S := src^[i].S * scale.S + delta.S;
    dest^[i].T := src^[i].T * scale.T + delta.T;
  end;
end;

procedure VectorArrayAdd(const src: PAffineVectorArray;
  const delta: TAffineVector; const nb: Integer; dest: PAffineVectorArray);

var
  i: Integer;
begin
  for i := 0 to nb - 1 do
  begin
    dest^[i].X := src^[i].X + delta.X;
    dest^[i].Y := src^[i].Y + delta.Y;
    dest^[i].Z := src^[i].Z + delta.Z;
  end;
end;

function VectorSubtract(const V1, V2: TAffineVector): TAffineVector;
begin
  result.X := V1.X - V2.X;
  result.Y := V1.Y - V2.Y;
  result.Z := V1.Z - V2.Z;
end;

function VectorSubtract(const V1, V2: TVector2f): TVector2f;
begin
  result.X := V1.X - V2.X;
  result.Y := V1.Y - V2.Y;
end;
procedure VectorSubtract(const V1, V2: TAffineVector;
  var result: TAffineVector);
begin
  result.X := V1.X - V2.X;
  result.Y := V1.Y - V2.Y;
  result.Z := V1.Z - V2.Z;
end;

procedure VectorSubtract(const V1, V2: TAffineVector; var result: TGLVector);
begin
  result.X := V1.X - V2.X;
  result.Y := V1.Y - V2.Y;
  result.Z := V1.Z - V2.Z;
  result.W := 0;
end;
procedure VectorSubtract(const V1: TGLVector; const V2: TAffineVector; var result: TGLVector);
begin
  result.X := V1.X - V2.X;
  result.Y := V1.Y - V2.Y;
  result.Z := V1.Z - V2.Z;
  result.W := V1.W;
end;

function VectorSubtract(const V1, V2: TGLVector): TGLVector;
begin
  result.X := V1.X - V2.X;
  result.Y := V1.Y - V2.Y;
  result.Z := V1.Z - V2.Z;
  result.W := V1.W - V2.W;
end;

procedure VectorSubtract(const V1, V2: TGLVector; var result: TGLVector);
begin
  result.X := V1.X - V2.X;
  result.Y := V1.Y - V2.Y;
  result.Z := V1.Z - V2.Z;
  result.W := V1.W - V2.W;
end;

procedure VectorSubtract(const V1, V2: TGLVector;
  var result: TAffineVector); overload;
begin
  result.X := V1.X - V2.X;
  result.Y := V1.Y - V2.Y;
  result.Z := V1.Z - V2.Z;
end;


function VectorSubtract(const V1: TAffineVector; delta: Single): TAffineVector;
begin
  result.X := V1.X - delta;
  result.Y := V1.Y - delta;
  result.Z := V1.Z - delta;
end;

function VectorSubtract(const V1: TGLVector; delta: Single): TGLVector;
begin
  result.X := V1.X - delta;
  result.Y := V1.Y - delta;
  result.Z := V1.Z - delta;
  result.W := V1.W - delta;
end;

procedure SubtractVector(var V1: TAffineVector; const V2: TAffineVector);
begin
  V1.X := V1.X - V2.X;
  V1.Y := V1.Y - V2.Y;
  V1.Z := V1.Z - V2.Z;
end;

procedure SubtractVector(var V1: TVector2f; const V2: TVector2f);
begin
  V1.X := V1.X - V2.X;
  V1.Y := V1.Y - V2.Y;
end;
procedure SubtractVector(var V1: TGLVector; const V2: TGLVector);
begin
  V1.X := V1.X - V2.X;
  V1.Y := V1.Y - V2.Y;
  V1.Z := V1.Z - V2.Z;
  V1.W := V1.W - V2.W;
end;

procedure CombineVector(var vr: TAffineVector; const V: TAffineVector;
  var f: Single);
begin
  vr.X := vr.X + V.X * f;
  vr.Y := vr.Y + V.Y * f;
  vr.Z := vr.Z + V.Z * f;
end;

procedure CombineVector(var vr: TAffineVector; const V: TAffineVector;
  pf: PFloat);
begin
  vr.X := vr.X + V.X * pf^;
  vr.Y := vr.Y + V.Y * pf^;
  vr.Z := vr.Z + V.Z * pf^;
end;


function TexPointCombine(const t1, t2: TTexPoint; f1, f2: Single): TTexPoint;
begin
  result.S := (f1 * t1.S) + (f2 * t2.S);
  result.T := (f1 * t1.T) + (f2 * t2.T);
end;

function VectorCombine(const V1, V2: TAffineVector; const f1, f2: Single)
  : TAffineVector;
begin
  result.V[X] := (f1 * V1.V[X]) + (f2 * V2.V[X]);
  result.V[Y] := (f1 * V1.V[Y]) + (f2 * V2.V[Y]);
  result.V[Z] := (f1 * V1.V[Z]) + (f2 * V2.V[Z]);
end;

function VectorCombine3(const V1, V2, V3: TAffineVector;
  const f1, f2, F3: Single): TAffineVector;
begin
  result.V[X] := (f1 * V1.V[X]) + (f2 * V2.V[X]) + (F3 * V3.V[X]);
  result.V[Y] := (f1 * V1.V[Y]) + (f2 * V2.V[Y]) + (F3 * V3.V[Y]);
  result.V[Z] := (f1 * V1.V[Z]) + (f2 * V2.V[Z]) + (F3 * V3.V[Z]);
end;

procedure VectorCombine3(const V1, V2, V3: TAffineVector;
  const f1, f2, F3: Single; var vr: TAffineVector);
begin
  vr.V[X] := (f1 * V1.V[X]) + (f2 * V2.V[X]) + (F3 * V3.V[X]);
  vr.V[Y] := (f1 * V1.V[Y]) + (f2 * V2.V[Y]) + (F3 * V3.V[Y]);
  vr.V[Z] := (f1 * V1.V[Z]) + (f2 * V2.V[Z]) + (F3 * V3.V[Z]);
end;

procedure CombineVector(var vr: TGLVector; const V: TGLVector;
  var f: Single); overload;
begin
  vr.X := vr.X + V.X * f;
  vr.Y := vr.Y + V.Y * f;
  vr.Z := vr.Z + V.Z * f;
  vr.W := vr.W + V.W * f;
end;

procedure CombineVector(var vr: TGLVector; const V: TAffineVector;
  var f: Single); overload;
begin
  vr.X := vr.X + V.X * f;
  vr.Y := vr.Y + V.Y * f;
  vr.Z := vr.Z + V.Z * f;
end;

function VectorCombine(const V1, V2: TGLVector; const F1, F2: Single): TGLVector;
begin
  result.V[X] := (F1 * V1.V[X]) + (F2 * V2.V[X]);
  result.V[Y] := (F1 * V1.V[Y]) + (F2 * V2.V[Y]);
  result.V[Z] := (F1 * V1.V[Z]) + (F2 * V2.V[Z]);
  result.V[W] := (F1 * V1.V[W]) + (F2 * V2.V[W]);
end;

function VectorCombine(const V1: TGLVector; const V2: TAffineVector;
  const F1, F2: Single): TGLVector; overload;
begin
  result.V[X] := (F1 * V1.V[X]) + (F2 * V2.V[X]);
  result.V[Y] := (F1 * V1.V[Y]) + (F2 * V2.V[Y]);
  result.V[Z] := (F1 * V1.V[Z]) + (F2 * V2.V[Z]);
  result.V[W] := F1 * V1.V[W];
end;

procedure VectorCombine(const V1, V2: TGLVector; const F1, F2: Single;
  var vr: TGLVector); overload;
begin
  vr.X := (F1 * V1.X) + (F2 * V2.X);
  vr.Y := (F1 * V1.Y) + (F2 * V2.Y);
  vr.Z := (F1 * V1.Z) + (F2 * V2.Z);
  vr.W := (F1 * V1.W) + (F2 * V2.W);
end;

procedure VectorCombine(const V1, V2: TGLVector; const f2: Single;
  var vr: TGLVector); overload;
begin // 201283
  vr.X := V1.X + (f2 * V2.X);
  vr.Y := V1.Y + (f2 * V2.Y);
  vr.Z := V1.Z + (f2 * V2.Z);
  vr.W := V1.W + (f2 * V2.W);
end;

procedure VectorCombine(const V1: TGLVector; const V2: TAffineVector;
  const F1, F2: Single; var vr: TGLVector);
begin
  vr.V[X] := (F1 * V1.V[X]) + (F2 * V2.V[X]);
  vr.V[Y] := (F1 * V1.V[Y]) + (F2 * V2.V[Y]);
  vr.V[Z] := (F1 * V1.V[Z]) + (F2 * V2.V[Z]);
  vr.V[W] := F1 * V1.V[W];
end;

function VectorCombine3(const V1, V2, V3: TGLVector;
  const F1, F2, F3: Single): TGLVector;
begin
  result.V[X] := (F1 * V1.V[X]) + (F2 * V2.V[X]) + (F3 * V3.V[X]);
  result.V[Y] := (F1 * V1.V[Y]) + (F2 * V2.V[Y]) + (F3 * V3.V[Y]);
  result.V[Z] := (F1 * V1.V[Z]) + (F2 * V2.V[Z]) + (F3 * V3.V[Z]);
  result.V[W] := (F1 * V1.V[W]) + (F2 * V2.V[W]) + (F3 * V3.V[W]);
end;

procedure VectorCombine3(const V1, V2, V3: TGLVector; const F1, F2, F3: Single;
  var vr: TGLVector);
begin
  vr.V[X] := (F1 * V1.V[X]) + (F2 * V2.V[X]) + (F3 * V3.V[X]);
  vr.V[Y] := (F1 * V1.V[Y]) + (F2 * V2.V[Y]) + (F3 * V3.V[Y]);
  vr.V[Z] := (F1 * V1.V[Z]) + (F2 * V2.V[Z]) + (F3 * V3.V[Z]);
  vr.V[W] := (F1 * V1.V[W]) + (F2 * V2.V[W]) + (F3 * V3.V[W]);
end;

function VectorDotProduct(const V1, V2: TVector2f): Single;
begin
  result := V1.X * V2.X + V1.Y * V2.Y;
end;

function VectorDotProduct(const V1, V2: TAffineVector): Single;
begin
  result := V1.X * V2.X + V1.Y * V2.Y + V1.Z * V2.Z;
end;
function VectorDotProduct(const V1, V2: TGLVector): Single;
begin
  result := V1.X * V2.X + V1.Y * V2.Y + V1.Z * V2.Z + V1.W * V2.W;
end;
function VectorDotProduct(const V1: TGLVector; const V2: TAffineVector): Single;
begin
  result := V1.X * V2.X + V1.Y * V2.Y + V1.Z * V2.Z;
end;

function PointProject(const p, origin, direction: TAffineVector): Single;
begin
  result := direction.X * (p.X - origin.X) + direction.Y *
    (p.Y - origin.Y) + direction.Z * (p.Z - origin.Z);
end;
function PointProject(const p, origin, direction: TGLVector): Single;
begin
  result := direction.X * (p.X - origin.X) + direction.Y *
    (p.Y - origin.Y) + direction.Z * (p.Z - origin.Z);
end;

function VectorCrossProduct(const V1, V2: TAffineVector): TAffineVector;
begin
  result.X := V1.Y * V2.Z - V1.Z * V2.Y;
  result.Y := V1.Z * V2.X - V1.X * V2.Z;
  result.Z := V1.X * V2.Y - V1.Y * V2.X;
end;

function VectorCrossProduct(const V1, V2: TGLVector): TGLVector;
begin
  result.X := V1.Y * V2.Z - V1.Z * V2.Y;
  result.Y := V1.Z * V2.X - V1.X * V2.Z;
  result.Z := V1.X * V2.Y - V1.Y * V2.X;
  result.W := 0;
end;
procedure VectorCrossProduct(const V1, V2: TGLVector; var vr: TGLVector);
begin
  vr.X := V1.Y * V2.Z - V1.Z * V2.Y;
  vr.Y := V1.Z * V2.X - V1.X * V2.Z;
  vr.Z := V1.X * V2.Y - V1.Y * V2.X;
  vr.W := 0;
end;
procedure VectorCrossProduct(const V1, V2: TAffineVector;
  var vr: TGLVector); overload;
begin
  vr.X := V1.Y * V2.Z - V1.Z * V2.Y;
  vr.Y := V1.Z * V2.X - V1.X * V2.Z;
  vr.Z := V1.X * V2.Y - V1.Y * V2.X;
  vr.W := 0;
end;
procedure VectorCrossProduct(const V1, V2: TGLVector;
  var vr: TAffineVector); overload;
begin
  vr.V[X] := V1.V[Y] * V2.V[Z] - V1.V[Z] * V2.V[Y];
  vr.V[Y] := V1.V[Z] * V2.V[X] - V1.V[X] * V2.V[Z];
  vr.V[Z] := V1.V[X] * V2.V[Y] - V1.V[Y] * V2.V[X];
end;
procedure VectorCrossProduct(const V1, V2: TAffineVector;
  var vr: TAffineVector); overload;
begin
  vr.V[X] := V1.V[Y] * V2.V[Z] - V1.V[Z] * V2.V[Y];
  vr.V[Y] := V1.V[Z] * V2.V[X] - V1.V[X] * V2.V[Z];
  vr.V[Z] := V1.V[X] * V2.V[Y] - V1.V[Y] * V2.V[X];
end;


function Lerp(const start, stop, T: Single): Single;
begin
  result := start + (stop - start) * T;
end;

function AngleLerp(start, stop, T: Single): Single;
var
  d: Single;
begin
  start := NormalizeAngle(start);
  stop := NormalizeAngle(stop);
  d := stop - start;
  if d > PI then
  begin
    // positive d, angle on opposite side, becomes negative i.e. changes direction
    d := -d - c2PI;
  end
  else if d < -PI then
  begin
    // negative d, angle on opposite side, becomes positive i.e. changes direction
    d := d + c2PI;
  end;
  result := start + d * T;
end;

function DistanceBetweenAngles(angle1, angle2: Single): Single;
begin
  angle1 := NormalizeAngle(angle1);
  angle2 := NormalizeAngle(angle2);
  result := Abs(angle2 - angle1);
  if result > PI then
    result := c2PI - result;
end;

function TexPointLerp(const t1, t2: TTexPoint; T: Single): TTexPoint; overload;
begin
  result.S := t1.S + (t2.S - t1.S) * T;
  result.T := t1.T + (t2.T - t1.T) * T;
end;

function VectorLerp(const V1, V2: TAffineVector; T: Single): TAffineVector;
begin
  result.X := V1.X + (V2.X - V1.X) * T;
  result.Y := V1.Y + (V2.Y - V1.Y) * T;
  result.Z := V1.Z + (V2.Z - V1.Z) * T;
end;

procedure VectorLerp(const V1, V2: TAffineVector; T: Single;
  var vr: TAffineVector);
begin
  vr.X := V1.X + (V2.X - V1.X) * T;
  vr.Y := V1.Y + (V2.Y - V1.Y) * T;
  vr.Z := V1.Z + (V2.Z - V1.Z) * T;
end;

function VectorLerp(const V1, V2: TGLVector; T: Single): TGLVector;
begin
  result.X := V1.X + (V2.X - V1.X) * T;
  result.Y := V1.Y + (V2.Y - V1.Y) * T;
  result.Z := V1.Z + (V2.Z - V1.Z) * T;
  result.W := V1.W + (V2.W - V1.W) * T;
end;

procedure VectorLerp(const V1, V2: TGLVector; T: Single; var vr: TGLVector);
begin
  vr.X := V1.X + (V2.X - V1.X) * T;
  vr.Y := V1.Y + (V2.Y - V1.Y) * T;
  vr.Z := V1.Z + (V2.Z - V1.Z) * T;
  vr.W := V1.W + (V2.W - V1.W) * T;
end;

function VectorAngleLerp(const V1, V2: TAffineVector; T: Single): TAffineVector;
var
  q1, q2, qR: TQuaternion;
  M: TGLMatrix;
  Tran: TTransformations;
begin
  if VectorEquals(V1, V2) then
  begin
    result := V1;
  end
  else
  begin
    q1 := QuaternionFromEuler(RadToDeg(V1.X), RadToDeg(V1.Y),
      RadToDeg(V1.Z), eulZYX);
    q2 := QuaternionFromEuler(RadToDeg(V2.X), RadToDeg(V2.Y),
      RadToDeg(V2.Z), eulZYX);
    qR := QuaternionSlerp(q1, q2, T);
    M := QuaternionToMatrix(qR);
    MatrixDecompose(M, Tran);
    result.X := Tran[ttRotateX];
    result.Y := Tran[ttRotateY];
    result.Z := Tran[ttRotateZ];
  end;
end;

function VectorAngleCombine(const V1, V2: TAffineVector; f: Single)
  : TAffineVector;
begin
  result := VectorCombine(V1, V2, 1, f);
end;

procedure VectorArrayLerp(const src1, src2: PVectorArray; T: Single; n: Integer;
  dest: PVectorArray);
var
  i: Integer;
begin
  for i := 0 to n - 1 do
  begin
    dest^[i].X := src1^[i].X + (src2^[i].X - src1^[i].X) * T;
    dest^[i].Y := src1^[i].Y + (src2^[i].Y - src1^[i].Y) * T;
    dest^[i].Z := src1^[i].Z + (src2^[i].Z - src1^[i].Z) * T;
    dest^[i].W := src1^[i].W + (src2^[i].W - src1^[i].W) * T;
  end;
 end;

procedure VectorArrayLerp(const src1, src2: PAffineVectorArray; T: Single;
  n: Integer; dest: PAffineVectorArray);
var
  i: Integer;
begin
  for i := 0 to n - 1 do
  begin
    dest^[i].X := src1^[i].X + (src2^[i].X - src1^[i].X) * T;
    dest^[i].Y := src1^[i].Y + (src2^[i].Y - src1^[i].Y) * T;
    dest^[i].Z := src1^[i].Z + (src2^[i].Z - src1^[i].Z) * T;
  end;
end;

procedure VectorArrayLerp(const src1, src2: PTexPointArray; T: Single;
  n: Integer; dest: PTexPointArray);
var
  i: Integer;
begin
  for i := 0 to n - 1 do
  begin
    dest^[i].S := src1^[i].S + (src2^[i].S - src1^[i].S) * T;
    dest^[i].T := src1^[i].T + (src2^[i].T - src1^[i].T) * T;
  end;
end;

function InterpolateCombined(const start, stop, delta: Single;
  const DistortionDegree: Single;
  const InterpolationType: TGLInterpolationType): Single;
begin
  case InterpolationType of
    itLinear:
      result := Lerp(start, stop, delta);
    itPower:
      result := InterpolatePower(start, stop, delta, DistortionDegree);
    itSin:
      result := InterpolateSin(start, stop, delta);
    itSinAlt:
      result := InterpolateSinAlt(start, stop, delta);
    itTan:
      result := InterpolateTan(start, stop, delta);
    itLn:
      result := InterpolateLn(start, stop, delta, DistortionDegree);
    itExp:
      result := InterpolateExp(start, stop, delta, DistortionDegree);
  else
    begin
      result := -1;
      Assert(False);
    end;
  end;
end;

function InterpolateCombinedFastPower(const OriginalStart, OriginalStop,
  OriginalCurrent: Single; const TargetStart, TargetStop: Single;
  const DistortionDegree: Single): Single;
begin
  result := InterpolatePower(TargetStart, TargetStop,
    (OriginalCurrent - OriginalStart) / (OriginalStop - OriginalStart),
    DistortionDegree);
end;

function InterpolateCombinedSafe(const OriginalStart, OriginalStop,
  OriginalCurrent: Single; const TargetStart, TargetStop: Single;
  const DistortionDegree: Single;
  const InterpolationType: TGLInterpolationType): Single;
var
  ChangeDelta: Single;
begin
  if OriginalStop = OriginalStart then
    result := TargetStart
  else
  begin
    ChangeDelta := (OriginalCurrent - OriginalStart) /
      (OriginalStop - OriginalStart);
    result := InterpolateCombined(TargetStart, TargetStop, ChangeDelta,
      DistortionDegree, InterpolationType);
  end;
end;

function InterpolateCombinedFast(const OriginalStart, OriginalStop,
  OriginalCurrent: Single; const TargetStart, TargetStop: Single;
  const DistortionDegree: Single;
  const InterpolationType: TGLInterpolationType): Single;
var
  ChangeDelta: Single;
begin
  ChangeDelta := (OriginalCurrent - OriginalStart) /
    (OriginalStop - OriginalStart);
  result := InterpolateCombined(TargetStart, TargetStop, ChangeDelta,
    DistortionDegree, InterpolationType);
end;

function InterpolateLn(const start, stop, delta: Single;
  const DistortionDegree: Single): Single;
begin
  result := (stop - start) * Ln(1 + delta * DistortionDegree) /
    Ln(1 + DistortionDegree) + start;
end;

function InterpolateExp(const start, stop, delta: Single;
  const DistortionDegree: Single): Single;
begin
  result := (stop - start) * Exp(-DistortionDegree * (1 - delta)) + start;
end;

function InterpolateSinAlt(const start, stop, delta: Single): Single;
begin
  result := (stop - start) * delta * Sin(delta * PI / 2) + start;
end;

function InterpolateSin(const start, stop, delta: Single): Single;
begin
  result := (stop - start) * Sin(delta * PI / 2) + start;
end;

function InterpolateTan(const start, stop, delta: Single): Single;
begin
  result := (stop - start) * Tan(delta * PI / 4) + start;
end;

function InterpolatePower(const start, stop, delta: Single;
  const DistortionDegree: Single): Single;
var
  i: Integer;
begin
  if (Round(DistortionDegree) <> DistortionDegree) and (delta < 0) then
  begin
    i := Round(DistortionDegree);
    result := (stop - start) * PowerInteger(delta, i) + start;
  end
  else
    result := (stop - start) * Power(delta, DistortionDegree) + start;
end;

function MatrixLerp(const m1, m2: TGLMatrix; const delta: Single): TGLMatrix;
var
  i, J: Integer;
begin
  for J := 0 to 3 do
    for i := 0 to 3 do
      result.V[i].V[J] := m1.V[i].V[J] + (m2.V[i].V[J] - m1.V[i].V[J]) * delta;
end;

function RSqrt(V: Single): Single;
begin
  result := 1 / Sqrt(V);
end;

function VectorLength(const V: array of Single): Single;
var
  i: Integer;
begin
  result := 0;
  for i := Low(V) to High(V) do
    result := result + Sqr(V[i]);
  result := Sqrt(result);
end;

function VectorLength(const X, Y: Single): Single;
begin
  result := Sqrt(X * X + Y * Y);
end;

function VectorLength(const X, Y, Z: Single): Single;
begin
  result := Sqrt(X * X + Y * Y + Z * Z);
end;

function VectorLength(const  V: TVector2f): Single;
begin
  result := Sqrt(VectorNorm(V.X, V.Y));
end;

function VectorLength(const V: TAffineVector): Single;
begin
  result := Sqrt(VectorNorm(V));
end;

function VectorLength(const V: TGLVector): Single;
begin
  result := Sqrt(VectorNorm(V));
end;

function VectorNorm(const X, Y: Single): Single;
begin
  result := Sqr(X) + Sqr(Y);
end;

function VectorNorm(const V: TAffineVector): Single;
begin
  result := V.X * V.X + V.Y * V.Y + V.Z * V.Z;
end;

function VectorNorm(const V: TGLVector): Single;
begin
  result := V.X * V.X + V.Y * V.Y + V.Z * V.Z;
end;

function VectorNorm(var V: array of Single): Single;
var
  i: Integer;
begin
  result := 0;
  for i := Low(V) to High(V) do
    result := result + V[i] * V[i];
end;

procedure NormalizeVector(var V: TVector2f);
var
  invLen: Single;
  vn: Single;
begin
  vn := VectorNorm(V.X, V.Y);
  if vn > 0 then
  begin
    invLen := RSqrt(vn);
    V.X := V.X * invLen;
    V.Y := V.Y * invLen;
  end;
end;

procedure NormalizeVector(var V: TAffineVector);
var
  invLen: Single;
  vn: Single;
begin
  vn := VectorNorm(V);
  if vn > 0 then
  begin
    invLen := RSqrt(vn);
    V.X := V.X * invLen;
    V.Y := V.Y * invLen;
    V.Z := V.Z * invLen;
  end;
end;

function VectorNormalize(const V: TVector2f): TVector2f;
var
  invLen: Single;
  vn: Single;
begin
  vn := VectorNorm(V.X, V.Y);
  if vn = 0 then
    result := V
  else
  begin
    invLen := RSqrt(vn);
    result.X := V.X * invLen;
    result.Y := V.Y * invLen;
  end;
end;

function VectorNormalize(const V: TAffineVector): TAffineVector;
var
  invLen: Single;
  vn: Single;
begin
  vn := VectorNorm(V);
  if vn = 0 then
    SetVector(result, V)
  else
  begin
    invLen := RSqrt(vn);
    result.X := V.X * invLen;
    result.Y := V.Y * invLen;
    result.Z := V.Z * invLen;
  end;
end;

procedure NormalizeVectorArray(list: PAffineVectorArray; n: Integer);
var
  i: Integer;
begin
  for i := 0 to n - 1 do
    NormalizeVector(list^[i]);
end;

procedure NormalizeVector(var V: TGLVector);
var
  invLen: Single;
  vn: Single;
begin
  vn := VectorNorm(V);
  if vn > 0 then
  begin
    invLen := RSqrt(vn);
    V.X := V.X * invLen;
    V.Y := V.Y * invLen;
    V.Z := V.Z * invLen;
  end;
  V.W := 0;
end;

function VectorNormalize(const V: TGLVector): TGLVector;
var
  invLen: Single;
  vn: Single;
begin
  vn := VectorNorm(V);
  if vn = 0 then
    SetVector(result, V)
  else
  begin
    invLen := RSqrt(vn);
    result.X := V.X * invLen;
    result.Y := V.Y * invLen;
    result.Z := V.Z * invLen;
  end;
  result.W := 0;
end;

function VectorAngleCosine(const V1, V2: TAffineVector): Single;
begin
  result := VectorDotProduct(V1, V2) / (VectorLength(V1) * VectorLength(V2));
end;

function VectorAngleCosine(const V1, V2: TGLVector): Single;
begin
  result := VectorDotProduct(V1, V2) / (VectorLength(V1) * VectorLength(V2));
end;

function VectorNegate(const Vector: TAffineVector): TAffineVector;
begin
  result.X := -Vector.X;
  result.Y := -Vector.Y;
  result.Z := -Vector.Z;
end;

function VectorNegate(const Vector: TGLVector): TGLVector;
begin
  result.X := -Vector.X;
  result.Y := -Vector.Y;
  result.Z := -Vector.Z;
  result.W := -Vector.W;
end;

procedure NegateVector(var V: TAffineVector);
begin
  V.X := -V.X;
  V.Y := -V.Y;
  V.Z := -V.Z;
end;

procedure NegateVector(var V: TGLVector);
begin
  V.X := -V.X;
  V.Y := -V.Y;
  V.Z := -V.Z;
  V.W := -V.W;
end;

procedure NegateVector(var V: array of Single);
var
  i: Integer;
begin
  for i := Low(V) to High(V) do
    V[i] := -V[i];
end;

procedure ScaleVector(var V: TVector2f; factor: Single);
begin
  V.X := V.X * factor;
  V.Y := V.Y * factor;
end;

procedure ScaleVector(var V: TAffineVector; factor: Single);
begin
  V.X := V.X * factor;
  V.Y := V.Y * factor;
  V.Z := V.Z * factor;
end;

procedure ScaleVector(var V: TGLVector; factor: Single);
begin
  V.X := V.X * factor;
  V.Y := V.Y * factor;
  V.Z := V.Z * factor;
  V.W := V.W * factor;
end;

procedure ScaleVector(var V: TAffineVector; const factor: TAffineVector);
begin
  V.X := V.X * factor.X;
  V.Y := V.Y * factor.Y;
  V.Z := V.Z * factor.Z;
end;

procedure ScaleVector(var V: TGLVector; const factor: TGLVector);
begin
  V.X := V.X * factor.X;
  V.Y := V.Y * factor.Y;
  V.Z := V.Z * factor.Z;
  V.W := V.W * factor.W;
end;

function VectorScale(const V: TVector2f; factor: Single): TVector2f;
begin
  result.X := V.X * factor;
  result.Y := V.Y * factor;
end;

function VectorScale(const V: TAffineVector; factor: Single): TAffineVector;
begin
  result.X := V.X * factor;
  result.Y := V.Y * factor;
  result.Z := V.Z * factor;
end;

procedure VectorScale(const V: TAffineVector; factor: Single;
  var vr: TAffineVector);
begin
  vr.X := V.X * factor;
  vr.Y := V.Y * factor;
  vr.Z := V.Z * factor;
end;

function VectorScale(const V: TGLVector; factor: Single): TGLVector;
begin
  result.X := V.X * factor;
  result.Y := V.Y * factor;
  result.Z := V.Z * factor;
  result.W := V.W * factor;
end;

procedure VectorScale(const V: TGLVector; factor: Single; var vr: TGLVector);
begin
  vr.X := V.X * factor;
  vr.Y := V.Y * factor;
  vr.Z := V.Z * factor;
  vr.W := V.W * factor;
end;
procedure VectorScale(const V: TGLVector; factor: Single; var vr: TAffineVector);
begin
  vr.X := V.X * factor;
  vr.Y := V.Y * factor;
  vr.Z := V.Z * factor;
end;

function VectorScale(const V: TAffineVector; const factor: TAffineVector)
  : TAffineVector;
begin
  result.X := V.X * factor.X;
  result.Y := V.Y * factor.Y;
  result.Z := V.Z * factor.Z;
end;

function VectorScale(const V: TGLVector; const factor: TGLVector): TGLVector;
begin
  result.X := V.X * factor.X;
  result.Y := V.Y * factor.Y;
  result.Z := V.Z * factor.Z;
  result.W := V.W * factor.W;
end;

procedure DivideVector(var V: TGLVector; const divider: TGLVector);
begin
  V.X := V.X / divider.X;
  V.Y := V.Y / divider.Y;
  V.Z := V.Z / divider.Z;
  V.W := V.W / divider.W;
end;

procedure DivideVector(var V: TAffineVector;
  const divider: TAffineVector); overload;
begin
  V.X := V.X / divider.X;
  V.Y := V.Y / divider.Y;
  V.Z := V.Z / divider.Z;
end;

function VectorDivide(const V: TGLVector; const divider: TGLVector)
  : TGLVector; overload;
begin
  result.X := V.X / divider.X;
  result.Y := V.Y / divider.Y;
  result.Z := V.Z / divider.Z;
  result.W := V.W / divider.W;
end;

function VectorDivide(const V: TAffineVector; const divider: TAffineVector)
  : TAffineVector; overload;
begin
  result.X := V.X / divider.X;
  result.Y := V.Y / divider.Y;
  result.Z := V.Z / divider.Z;
end;

function TexpointEquals(const p1, p2: TTexPoint): Boolean;
begin
  result := (p1.S = p2.S) and (p1.T = p2.T);
end;

function RectEquals(const Rect1, Rect2: TRect): Boolean;
begin
  result := (Rect1.Left = Rect2.Left) and (Rect1.Right = Rect2.Right) and
    (Rect1.Top = Rect2.Top) and (Rect1.Bottom = Rect2.Bottom);
end;

function VectorEquals(const V1, V2: TGLVector): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y) and (V1.Z = V2.Z)
    and (V1.W = V2.W);
end;

function VectorEquals(const V1, V2: TAffineVector): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y) and (V1.Z = V2.Z);
end;

function AffineVectorEquals(const V1, V2: TGLVector): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y) and (V1.Z = V2.Z);
end;

function VectorIsNull(const V: TGLVector): Boolean;
begin
  result := ((V.X = 0) and (V.Y = 0) and (V.Z = 0));
end;

function VectorIsNull(const V: TAffineVector): Boolean; overload;
begin
  result := ((V.X = 0) and (V.Y = 0) and (V.Z = 0));
end;

function VectorSpacing(const V1, V2: TTexPoint): Single; overload;
begin
  result := Abs(V2.S - V1.S) + Abs(V2.T - V1.T);
end;

function VectorSpacing(const V1, V2: TAffineVector): Single;
begin
  result := Abs(V2.X - V1.X) + Abs(V2.Y - V1.Y) +
    Abs(V2.Z - V1.Z);
end;

function VectorSpacing(const V1, V2: TGLVector): Single;
begin
  result := Abs(V2.X - V1.X) + Abs(V2.Y - V1.Y) +
    Abs(V2.Z - V1.Z) + Abs(V2.W - V1.W);
end;

function VectorDistance(const V1, V2: TAffineVector): Single;
begin
  result := Sqrt(Sqr(V2.X - V1.X) + Sqr(V2.Y - V1.Y) + Sqr(V2.Z - V1.Z));
end;

function VectorDistance(const V1, V2: TGLVector): Single;
begin
  result := Sqrt(Sqr(V2.X - V1.X) + Sqr(V2.Y - V1.Y) + Sqr(V2.Z - V1.Z));
end;

function VectorDistance2(const V1, V2: TAffineVector): Single;
begin
  result := Sqr(V2.X - V1.X) + Sqr(V2.Y - V1.Y) + Sqr(V2.Z - V1.Z);
end;

function VectorDistance2(const V1, V2: TGLVector): Single;
begin
  result := Sqr(V2.X - V1.X) + Sqr(V2.Y - V1.Y) + Sqr(V2.Z - V1.Z);
end;

function VectorPerpendicular(const V, n: TAffineVector): TAffineVector;
var
  dot: Single;
begin
  dot := VectorDotProduct(V, n);
  result.X := V.X - dot * n.X;
  result.Y := V.Y - dot * n.Y;
  result.Z := V.Z - dot * n.Z;
end;

function VectorReflect(const V, n: TAffineVector): TAffineVector;
begin
  result := VectorCombine(V, n, 1, -2 * VectorDotProduct(V, n));
end;

procedure RotateVector(var Vector: TGLVector; const axis: TAffineVector;
  angle: Single);
var
  rotMatrix: TMatrix4f;
begin
  rotMatrix := CreateRotationMatrix(axis, angle);
  Vector := VectorTransform(Vector, rotMatrix);
end;

procedure RotateVector(var Vector: TGLVector; const axis: TGLVector;
  angle: Single); overload;
var
  rotMatrix: TMatrix4f;
begin
  rotMatrix := CreateRotationMatrix(PAffineVector(@axis)^, angle);
  Vector := VectorTransform(Vector, rotMatrix);
end;

procedure RotateVectorAroundY(var V: TAffineVector; alpha: Single);
var
  c, S, v0: Single;
begin
  SinCosine(alpha, S, c);
  v0 := V.X;
  V.X := c * v0 + S * V.Z;
  V.Z := c * V.Z - S * v0;
end;

function VectorRotateAroundX(const V: TAffineVector; alpha: Single)
  : TAffineVector;
var
  c, S: Single;
begin
  SinCosine(alpha, S, c);
  result.X := V.X;
  result.Y := c * V.Y + S * V.Z;
  result.Z := c * V.Z - S * V.Y;
end;

function VectorRotateAroundY(const V: TAffineVector; alpha: Single)
  : TAffineVector;
var
  c, S: Single;
begin
  SinCosine(alpha, S, c);
  result.Y := V.Y;
  result.X := c * V.X + S * V.Z;
  result.Z := c * V.Z - S * V.X;
end;

procedure VectorRotateAroundY(const V: TAffineVector; alpha: Single;
  var vr: TAffineVector);
var
  c, S: Single;
begin
  SinCosine(alpha, S, c);
  vr.Y := V.Y;
  vr.X := c * V.X + S * V.Z;
  vr.Z := c * V.Z - S * V.X;
end;

function VectorRotateAroundZ(const V: TAffineVector; alpha: Single)
  : TAffineVector;
var
  c, S: Single;
begin
  SinCosine(alpha, S, c);
  result.X := c * V.X + S * V.Y;
  result.Y := c * V.Y - S * V.X;
  result.Z := V.Z;
end;

procedure AbsVector(var V: TGLVector);
begin
  V.X := Abs(V.X);
  V.Y := Abs(V.Y);
  V.Z := Abs(V.Z);
  V.W := Abs(V.W);
end;

procedure AbsVector(var V: TAffineVector);
begin
  V.X := Abs(V.X);
  V.Y := Abs(V.Y);
  V.Z := Abs(V.Z);
end;

function VectorAbs(const V: TGLVector): TGLVector;
begin
  result.X := Abs(V.X);
  result.Y := Abs(V.Y);
  result.Z := Abs(V.Z);
  result.W := Abs(V.W);
end;

function VectorAbs(const V: TAffineVector): TAffineVector;
begin
  result.X := Abs(V.X);
  result.Y := Abs(V.Y);
  result.Z := Abs(V.Z);
end;

function IsColinear(const V1, V2: TVector2f): Boolean; overload;
var
  a, b, c: Single;
begin
  a := VectorDotProduct(V1, V1);
  b := VectorDotProduct(V1, V2);
  c := VectorDotProduct(V2, V2);
  result := (a * c - b * b) < cColinearBias;
end;

function IsColinear(const V1, V2: TAffineVector): Boolean; overload;
var
  a, b, c: Single;
begin
  a := VectorDotProduct(V1, V1);
  b := VectorDotProduct(V1, V2);
  c := VectorDotProduct(V2, V2);
  result := (a * c - b * b) < cColinearBias;
end;

function IsColinear(const V1, V2: TGLVector): Boolean; overload;
var
  a, b, c: Single;
begin
  a := VectorDotProduct(V1, V1);
  b := VectorDotProduct(V1, V2);
  c := VectorDotProduct(V2, V2);
  result := (a * c - b * b) < cColinearBias;
end;

procedure SetMatrix(var dest: THomogeneousDblMatrix; const src: TGLMatrix);
var
  i: Integer;
begin
  for i := X to W do
  begin
    dest.V[i].X := src.V[i].X;
    dest.V[i].Y := src.V[i].Y;
    dest.V[i].Z := src.V[i].Z;
    dest.V[i].W := src.V[i].W;
  end;
end;

procedure SetMatrix(var dest: TAffineMatrix; const src: TGLMatrix);
begin
  dest.X.X := src.X.X;
  dest.X.Y := src.X.Y;
  dest.X.Z := src.X.Z;
  dest.Y.X := src.Y.X;
  dest.Y.Y := src.Y.Y;
  dest.Y.Z := src.Y.Z;
  dest.Z.X := src.Z.X;
  dest.Z.Y := src.Z.Y;
  dest.Z.Z := src.Z.Z;
end;

procedure SetMatrix(var dest: TGLMatrix; const src: TAffineMatrix);
begin
  dest.X.X := src.X.X;
  dest.X.Y := src.X.Y;
  dest.X.Z := src.X.Z;
  dest.X.W := 0;
  dest.Y.X := src.Y.X;
  dest.Y.Y := src.Y.Y;
  dest.Y.Z := src.Y.Z;
  dest.Y.W := 0;
  dest.Z.X := src.Z.X;
  dest.Z.Y := src.Z.Y;
  dest.Z.Z := src.Z.Z;
  dest.Z.W := 0;
  dest.W.X := 0;
  dest.W.Y := 0;
  dest.W.Z := 0;
  dest.W.W := 1;
end;

procedure SetMatrixRow(var dest: TGLMatrix; rowNb: Integer; const aRow: TGLVector);
begin
  dest.X.V[rowNb] := aRow.X;
  dest.Y.V[rowNb] := aRow.Y;
  dest.Z.V[rowNb] := aRow.Z;
  dest.W.V[rowNb] := aRow.W;
end;

function CreateScaleMatrix(const V: TAffineVector): TGLMatrix;
begin
  result := IdentityHmgMatrix;
  result.X.X := V.V[X];
  result.Y.Y := V.V[Y];
  result.Z.Z := V.V[Z];
end;

function CreateScaleMatrix(const V: TGLVector): TGLMatrix;
begin
  result := IdentityHmgMatrix;
  result.X.X := V.V[X];
  result.Y.Y := V.V[Y];
  result.Z.Z := V.V[Z];
end;

function CreateTranslationMatrix(const V: TAffineVector): TGLMatrix;
begin
  result := IdentityHmgMatrix;
  result.W.X := V.V[X];
  result.W.Y := V.V[Y];
  result.W.Z := V.V[Z];
end;

function CreateTranslationMatrix(const V: TGLVector): TGLMatrix;
begin
  result := IdentityHmgMatrix;
  result.W.X := V.V[X];
  result.W.Y := V.V[Y];
  result.W.Z := V.V[Z];
end;

function CreateScaleAndTranslationMatrix(const scale, offset: TGLVector): TGLMatrix;
begin
  result := IdentityHmgMatrix;
  result.X.X := scale.V[X];
  result.W.X := offset.V[X];
  result.Y.Y := scale.V[Y];
  result.W.Y := offset.V[Y];
  result.Z.Z := scale.V[Z];
  result.W.Z := offset.V[Z];
end;

function CreateRotationMatrixX(const sine, cosine: Single): TGLMatrix;
begin
  result := EmptyHmgMatrix;
  result.X.X := 1;
  result.Y.Y := cosine;
  result.Y.Z := sine;
  result.Z.Y := -sine;
  result.Z.Z := cosine;
  result.W.W := 1;
end;

function CreateRotationMatrixX(const angle: Single): TGLMatrix;
var
  S, c: Single;
begin
  SinCosine(angle, S, c);
  result := CreateRotationMatrixX(S, c);
end;

function CreateRotationMatrixY(const sine, cosine: Single): TGLMatrix;
begin
  result := EmptyHmgMatrix;
  result.X.X := cosine;
  result.X.Z := -sine;
  result.Y.Y := 1;
  result.Z.X := sine;
  result.Z.Z := cosine;
  result.W.W := 1;
end;

function CreateRotationMatrixY(const angle: Single): TGLMatrix;
var
  S, c: Single;
begin
  SinCosine(angle, S, c);
  result := CreateRotationMatrixY(S, c);
end;

function CreateRotationMatrixZ(const sine, cosine: Single): TGLMatrix;
begin
  result := EmptyHmgMatrix;
  result.X.X := cosine;
  result.X.Y := sine;
  result.Y.X := -sine;
  result.Y.Y := cosine;
  result.Z.Z := 1;
  result.W.W := 1;
end;

function CreateRotationMatrixZ(const angle: Single): TGLMatrix;
var
  S, c: Single;
begin
  SinCosine(angle, S, c);
  result := CreateRotationMatrixZ(S, c);
end;

function CreateRotationMatrix(const anAxis: TAffineVector;
  angle: Single): TGLMatrix;
var
  axis: TAffineVector;
  cosine, sine, one_minus_cosine: Single;
begin
  SinCosine(angle, sine, cosine);
  one_minus_cosine := 1 - cosine;
  axis := VectorNormalize(anAxis);

  result.X.X := (one_minus_cosine * axis.X * axis.X) + cosine;
  result.X.Y := (one_minus_cosine * axis.X * axis.Y) - (axis.Z * sine);
  result.X.Z := (one_minus_cosine * axis.Z * axis.X) + (axis.Y * sine);
  result.X.W := 0;

  result.Y.X := (one_minus_cosine * axis.X * axis.Y) + (axis.Z * sine);
  result.Y.Y := (one_minus_cosine * axis.Y * axis.Y) + cosine;
  result.Y.Z := (one_minus_cosine * axis.Y * axis.Z) - (axis.X * sine);
  result.Y.W := 0;

  result.Z.X := (one_minus_cosine * axis.Z * axis.X) - (axis.Y * sine);
  result.Z.Y := (one_minus_cosine * axis.Y * axis.Z) + (axis.X * sine);
  result.Z.Z := (one_minus_cosine * axis.Z * axis.Z) + cosine;
  result.Z.W := 0;

  result.W.X := 0;
  result.W.Y := 0;
  result.W.Z := 0;
  result.W.W := 1;
end;

function CreateRotationMatrix(const anAxis: TGLVector; angle: Single): TGLMatrix;
begin
  result := CreateRotationMatrix(PAffineVector(@anAxis)^, angle);
end;

function CreateAffineRotationMatrix(const anAxis: TAffineVector; angle: Single)
  : TAffineMatrix;
var
  axis: TAffineVector;
  cosine, sine, one_minus_cosine: Single;
begin
  SinCosine(angle, sine, cosine);
  one_minus_cosine := 1 - cosine;
  axis := VectorNormalize(anAxis);

  result.X.X := (one_minus_cosine * Sqr(axis.X)) + cosine;
  result.X.Y := (one_minus_cosine * axis.X * axis.Y) - (axis.Z * sine);
  result.X.Z := (one_minus_cosine * axis.Z * axis.X) + (axis.Y * sine);

  result.Y.X := (one_minus_cosine * axis.X * axis.Y) + (axis.Z * sine);
  result.Y.Y := (one_minus_cosine * Sqr(axis.Y)) + cosine;
  result.Y.Z := (one_minus_cosine * axis.Y * axis.Z) - (axis.X * sine);

  result.Z.X := (one_minus_cosine * axis.Z * axis.X) - (axis.Y * sine);
  result.Z.Y := (one_minus_cosine * axis.Y * axis.Z) + (axis.X * sine);
  result.Z.Z := (one_minus_cosine * Sqr(axis.Z)) + cosine;
end;

function MatrixMultiply(const m1, m2: TAffineMatrix): TAffineMatrix;
begin
  result.X.X := m1.X.X * m2.X.X + m1.X.Y * m2.Y.X + m1.X.Z * m2.Z.X;
  result.X.Y := m1.X.X * m2.X.Y + m1.X.Y * m2.Y.Y + m1.X.Z * m2.Z.Y;
  result.X.Z := m1.X.X * m2.X.Z + m1.X.Y * m2.Y.Z + m1.X.Z * m2.Z.Z;
  result.Y.X := m1.Y.X * m2.X.X + m1.Y.Y * m2.Y.X + m1.Y.Z * m2.Z.X;
  result.Y.Y := m1.Y.X * m2.X.Y + m1.Y.Y * m2.Y.Y + m1.Y.Z * m2.Z.Y;
  result.Y.Z := m1.Y.X * m2.X.Z + m1.Y.Y * m2.Y.Z + m1.Y.Z * m2.Z.Z;
  result.Z.X := m1.Z.X * m2.X.X + m1.Z.Y * m2.Y.X + m1.Z.Z * m2.Z.X;
  result.Z.Y := m1.Z.X * m2.X.Y + m1.Z.Y * m2.Y.Y + m1.Z.Z * m2.Z.Y;
  result.Z.Z := m1.Z.X * m2.X.Z + m1.Z.Y * m2.Y.Z + m1.Z.Z * m2.Z.Z;
end;

function MatrixMultiply(const m1, m2: TGLMatrix): TGLMatrix;
begin
  result.X.X := m1.X.X * m2.X.X + m1.X.Y * m2.Y.X + m1.X.Z * m2.Z.X +
    m1.X.W * m2.W.X;
  result.X.Y := m1.X.X * m2.X.Y + m1.X.Y * m2.Y.Y + m1.X.Z * m2.Z.Y +
    m1.X.W * m2.W.Y;
  result.X.Z := m1.X.X * m2.X.Z + m1.X.Y * m2.Y.Z + m1.X.Z * m2.Z.Z +
    m1.X.W * m2.W.Z;
  result.X.W := m1.X.X * m2.X.W + m1.X.Y * m2.Y.W + m1.X.Z * m2.Z.W +
    m1.X.W * m2.W.W;
  result.Y.X := m1.Y.X * m2.X.X + m1.Y.Y * m2.Y.X + m1.Y.Z * m2.Z.X +
    m1.Y.W * m2.W.X;
  result.Y.Y := m1.Y.X * m2.X.Y + m1.Y.Y * m2.Y.Y + m1.Y.Z * m2.Z.Y +
    m1.Y.W * m2.W.Y;
  result.Y.Z := m1.Y.X * m2.X.Z + m1.Y.Y * m2.Y.Z + m1.Y.Z * m2.Z.Z +
    m1.Y.W * m2.W.Z;
  result.Y.W := m1.Y.X * m2.X.W + m1.Y.Y * m2.Y.W + m1.Y.Z * m2.Z.W +
    m1.Y.W * m2.W.W;
  result.Z.X := m1.Z.X * m2.X.X + m1.Z.Y * m2.Y.X + m1.Z.Z * m2.Z.X +
    m1.Z.W * m2.W.X;
  result.Z.Y := m1.Z.X * m2.X.Y + m1.Z.Y * m2.Y.Y + m1.Z.Z * m2.Z.Y +
    m1.Z.W * m2.W.Y;
  result.Z.Z := m1.Z.X * m2.X.Z + m1.Z.Y * m2.Y.Z + m1.Z.Z * m2.Z.Z +
    m1.Z.W * m2.W.Z;
  result.Z.W := m1.Z.X * m2.X.W + m1.Z.Y * m2.Y.W + m1.Z.Z * m2.Z.W +
    m1.Z.W * m2.W.W;
  result.W.X := m1.W.X * m2.X.X + m1.W.Y * m2.Y.X + m1.W.Z * m2.Z.X +
    m1.W.W * m2.W.X;
  result.W.Y := m1.W.X * m2.X.Y + m1.W.Y * m2.Y.Y + m1.W.Z * m2.Z.Y +
    m1.W.W * m2.W.Y;
  result.W.Z := m1.W.X * m2.X.Z + m1.W.Y * m2.Y.Z + m1.W.Z * m2.Z.Z +
    m1.W.W * m2.W.Z;
  result.W.W := m1.W.X * m2.X.W + m1.W.Y * m2.Y.W + m1.W.Z * m2.Z.W +
    m1.W.W * m2.W.W;
end;

procedure MatrixMultiply(const m1, m2: TGLMatrix; var MResult: TGLMatrix);
begin
  MResult.X.X := m1.X.X * m2.X.X + m1.X.Y * m2.Y.X + m1.X.Z * m2.Z.X +  m1.X.W * m2.W.X;
  MResult.X.Y := m1.X.X * m2.X.Y + m1.X.Y * m2.Y.Y + m1.X.Z * m2.Z.Y +  m1.X.W * m2.W.Y;
  MResult.X.Z := m1.X.X * m2.X.Z + m1.X.Y * m2.Y.Z + m1.X.Z * m2.Z.Z +  m1.X.W * m2.W.Z;
  MResult.X.W := m1.X.X * m2.X.W + m1.X.Y * m2.Y.W + m1.X.Z * m2.Z.W +  m1.X.W * m2.W.W;
  MResult.Y.X := m1.Y.X * m2.X.X + m1.Y.Y * m2.Y.X + m1.Y.Z * m2.Z.X +  m1.Y.W * m2.W.X;
  MResult.Y.Y := m1.Y.X * m2.X.Y + m1.Y.Y * m2.Y.Y + m1.Y.Z * m2.Z.Y +  m1.Y.W * m2.W.Y;
  MResult.Y.Z := m1.Y.X * m2.X.Z + m1.Y.Y * m2.Y.Z + m1.Y.Z * m2.Z.Z +  m1.Y.W * m2.W.Z;
  MResult.Y.W := m1.Y.X * m2.X.W + m1.Y.Y * m2.Y.W + m1.Y.Z * m2.Z.W +  m1.Y.W * m2.W.W;
  MResult.Z.X := m1.Z.X * m2.X.X + m1.Z.Y * m2.Y.X + m1.Z.Z * m2.Z.X +  m1.Z.W * m2.W.X;
  MResult.Z.Y := m1.Z.X * m2.X.Y + m1.Z.Y * m2.Y.Y + m1.Z.Z * m2.Z.Y +  m1.Z.W * m2.W.Y;
  MResult.Z.Z := m1.Z.X * m2.X.Z + m1.Z.Y * m2.Y.Z + m1.Z.Z * m2.Z.Z +  m1.Z.W * m2.W.Z;
  MResult.Z.W := m1.Z.X * m2.X.W + m1.Z.Y * m2.Y.W + m1.Z.Z * m2.Z.W +  m1.Z.W * m2.W.W;
  MResult.W.X := m1.W.X * m2.X.X + m1.W.Y * m2.Y.X + m1.W.Z * m2.Z.X +  m1.W.W * m2.W.X;
  MResult.W.Y := m1.W.X * m2.X.Y + m1.W.Y * m2.Y.Y + m1.W.Z * m2.Z.Y +  m1.W.W * m2.W.Y;
  MResult.W.Z := m1.W.X * m2.X.Z + m1.W.Y * m2.Y.Z + m1.W.Z * m2.Z.Z +  m1.W.W * m2.W.Z;
  MResult.W.W := m1.W.X * m2.X.W + m1.W.Y * m2.Y.W + m1.W.Z * m2.Z.W +  m1.W.W * m2.W.W;

end;

function VectorTransform(const V: TGLVector; const M: TGLMatrix): TGLVector;
begin
    result.V[X] := V.V[X] * M.X.X + V.V[Y] * M.Y.X + V.V[Z] * M.Z.X + V.V[W] * M.W.X;
    result.V[Y] := V.V[X] * M.X.Y + V.V[Y] * M.Y.Y + V.V[Z] * M.Z.Y + V.V[W] * M.W.Y;
    result.V[Z] := V.V[X] * M.X.Z + V.V[Y] * M.Y.Z + V.V[Z] * M.Z.Z + V.V[W] * M.W.Z;
    result.V[W] := V.V[X] * M.X.W + V.V[Y] * M.Y.W + V.V[Z] * M.Z.W + V.V[W] * M.W.W;
end;

function VectorTransform(const V: TGLVector; const M: TAffineMatrix): TGLVector;
begin
  result.X := V.X * M.V[X].X + V.Y * M.V[Y].X + V.Z * M.V[Z].X;
  result.Y := V.X * M.V[X].Y + V.Y * M.V[Y].Y + V.Z * M.V[Z].Y;
  result.Z := V.X * M.V[X].Z + V.Y * M.V[Y].Z + V.Z * M.V[Z].Z;
  result.W := V.W;
end;

function VectorTransform(const V: TAffineVector; const M: TGLMatrix)
  : TAffineVector;
begin
  result.X := V.X * M.V[X].X + V.Y * M.V[Y].X + V.Z * M.V[Z].X + M.V[W].X;
  result.Y := V.X * M.V[X].Y + V.Y * M.V[Y].Y + V.Z * M.V[Z].Y + M.V[W].Y;
  result.Z := V.X * M.V[X].Z + V.Y * M.V[Y].Z + V.Z * M.V[Z].Z + M.V[W].Z;
end;

function VectorTransform(const V: TAffineVector; const M: TAffineMatrix)
  : TAffineVector;
begin
  result.V[X] := V.V[X] * M.X.X + V.V[Y] * M.Y.X + V.V[Z] * M.Z.X;
  result.V[Y] := V.V[X] * M.X.Y + V.V[Y] * M.Y.Y + V.V[Z] * M.Z.Y;
  result.V[Z] := V.V[X] * M.X.Z + V.V[Y] * M.Y.Z + V.V[Z] * M.Z.Z;
end;

function MatrixDeterminant(const M: TAffineMatrix): Single;
begin
  result := M.X.X * (M.Y.Y * M.Z.Z - M.Z.Y * M.Y.Z) - M.X.Y *
    (M.Y.X * M.Z.Z - M.Z.X * M.Y.Z) + M.X.Z * (M.Y.X * M.Z.Y - M.Z.X * M.Y.Y);
end;

function MatrixDetInternal(const a1, a2, a3, b1, b2, b3, c1, c2,
  c3: Single): Single;
// internal version for the determinant of a 3x3 matrix
begin
  result := a1 * (b2 * c3 - b3 * c2) - b1 * (a2 * c3 - a3 * c2) + c1 *
    (a2 * b3 - a3 * b2);
end;

function MatrixDeterminant(const M: TGLMatrix): Single;
begin
  result := M.X.X * MatrixDetInternal(M.Y.Y, M.Z.Y, M.W.Y, M.Y.Z, M.Z.Z, M.W.Z,
    M.Y.W, M.Z.W, M.W.W) - M.X.Y * MatrixDetInternal(M.Y.X, M.Z.X, M.W.X, M.Y.Z,
    M.Z.Z, M.W.Z, M.Y.W, M.Z.W, M.W.W) + M.X.Z * MatrixDetInternal(M.Y.X, M.Z.X,
    M.W.X, M.Y.Y, M.Z.Y, M.W.Y, M.Y.W, M.Z.W, M.W.W) - M.X.W *
    MatrixDetInternal(M.Y.X, M.Z.X, M.W.X, M.Y.Y, M.Z.Y, M.W.Y, M.Y.Z,
    M.Z.Z, M.W.Z);
end;

procedure AdjointMatrix(var M: TGLMatrix);
var
  a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4: Single;
begin
  a1 := M.X.X;
  b1 := M.X.Y;
  c1 := M.X.Z;
  d1 := M.X.W;
  a2 := M.Y.X;
  b2 := M.Y.Y;
  c2 := M.Y.Z;
  d2 := M.Y.W;
  a3 := M.Z.X;
  b3 := M.Z.Y;
  c3 := M.Z.Z;
  d3 := M.Z.W;
  a4 := M.W.X;
  b4 := M.W.Y;
  c4 := M.W.Z;
  d4 := M.W.W;

  // row column labeling reversed since we transpose rows & columns
  M.X.X := MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
  M.Y.X := -MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
  M.Z.X := MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
  M.W.X := -MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);

  M.X.Y := -MatrixDetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
  M.Y.Y := MatrixDetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
  M.Z.Y := -MatrixDetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
  M.W.Y := MatrixDetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);

  M.X.Z := MatrixDetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
  M.Y.Z := -MatrixDetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
  M.Z.Z := MatrixDetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
  M.W.Z := -MatrixDetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);

  M.X.W := -MatrixDetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
  M.Y.W := MatrixDetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
  M.Z.W := -MatrixDetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
  M.W.W := MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

procedure AdjointMatrix(var M: TAffineMatrix);
var
  a1, a2, a3, b1, b2, b3, c1, c2, c3: Single;
begin
  a1 := M.X.X;
  a2 := M.X.Y;
  a3 := M.X.Z;
  b1 := M.Y.X;
  b2 := M.Y.Y;
  b3 := M.Y.Z;
  c1 := M.Z.X;
  c2 := M.Z.Y;
  c3 := M.Z.Z;
  M.X.X := (b2 * c3 - c2 * b3);
  M.Y.X := -(b1 * c3 - c1 * b3);
  M.Z.X := (b1 * c2 - c1 * b2);

  M.X.Y := -(a2 * c3 - c2 * a3);
  M.Y.Y := (a1 * c3 - c1 * a3);
  M.Z.Y := -(a1 * c2 - c1 * a2);

  M.X.Z := (a2 * b3 - b2 * a3);
  M.Y.Z := -(a1 * b3 - b1 * a3);
  M.Z.Z := (a1 * b2 - b1 * a2);
end;

procedure ScaleMatrix(var M: TAffineMatrix; const factor: Single);
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    M.V[i].X := M.V[i].X * factor;
    M.V[i].Y := M.V[i].Y * factor;
    M.V[i].Z := M.V[i].Z * factor;
  end;
end;

procedure ScaleMatrix(var M: TGLMatrix; const factor: Single);
var
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    M.V[i].X := M.V[i].X * factor;
    M.V[i].Y := M.V[i].Y * factor;
    M.V[i].Z := M.V[i].Z * factor;
    M.V[i].W := M.V[i].W * factor;
  end;
end;

procedure TranslateMatrix(var M: TGLMatrix; const V: TAffineVector);
begin
  M.W.X := M.W.X + V.X;
  M.W.Y := M.W.Y + V.Y;
  M.W.Z := M.W.Z + V.Z;
end;

procedure TranslateMatrix(var M: TGLMatrix; const V: TGLVector);
begin
  M.W.X := M.W.X + V.X;
  M.W.Y := M.W.Y + V.Y;
  M.W.Z := M.W.Z + V.Z;
end;

procedure NormalizeMatrix(var M: TGLMatrix);
begin
  M.X.W := 0;
  NormalizeVector(M.X);
  M.Y.W := 0;
  NormalizeVector(M.Y);
  M.Z := VectorCrossProduct(M.X, M.Y);
  M.X := VectorCrossProduct(M.Y, M.Z);
  M.W := WHmgVector;
end;

procedure TransposeMatrix(var M: TAffineMatrix);
var
  f: Single;
begin
  f := M.X.Y;
  M.X.Y := M.Y.X;
  M.Y.X := f;
  f := M.X.Z;
  M.X.Z := M.Z.X;
  M.Z.X := f;
  f := M.Y.Z;
  M.Y.Z := M.Z.Y;
  M.Z.Y := f;
end;

procedure TransposeMatrix(var M: TGLMatrix);
var
  f: Single;
begin
  f := M.X.Y;
  M.X.Y := M.Y.X;
  M.Y.X := f;
  f := M.X.Z;
  M.X.Z := M.Z.X;
  M.Z.X := f;
  f := M.X.W;
  M.X.W := M.W.X;
  M.W.X := f;
  f := M.Y.Z;
  M.Y.Z := M.Z.Y;
  M.Z.Y := f;
  f := M.Y.W;
  M.Y.W := M.W.Y;
  M.W.Y := f;
  f := M.Z.W;
  M.Z.W := M.W.Z;
  M.W.Z := f;
end;

procedure InvertMatrix(var M: TGLMatrix);
var
  det: Single;
begin
  det := MatrixDeterminant(M);
  if Abs(det) < EPSILON then
    M := IdentityHmgMatrix
  else
  begin
    AdjointMatrix(M);
    ScaleMatrix(M, 1 / det);
  end;
end;

function MatrixInvert(const M: TGLMatrix): TGLMatrix;
begin
  result := M;
  InvertMatrix(result);
end;

procedure InvertMatrix(var M: TAffineMatrix);
var
  det: Single;
begin
  det := MatrixDeterminant(M);
  if Abs(det) < EPSILON then
    M := IdentityMatrix
  else
  begin
    AdjointMatrix(M);
    ScaleMatrix(M, 1 / det);
  end;
end;

function MatrixInvert(const M: TAffineMatrix): TAffineMatrix;
begin
  result := M;
  InvertMatrix(result);
end;

procedure Transpose_Scale_M33(const src: TGLMatrix; var dest: TGLMatrix;
  var scale: Single);
begin
  dest.X.X := scale * src.X.X;
  dest.Y.X := scale * src.X.Y;
  dest.Z.X := scale * src.X.Z;
  dest.X.Y := scale * src.Y.X;
  dest.Y.Y := scale * src.Y.Y;
  dest.Z.Y := scale * src.Y.Z;
  dest.X.Z := scale * src.Z.X;
  dest.Y.Z := scale * src.Z.Y;
  dest.Z.Z := scale * src.Z.Z;
end;


function AnglePreservingMatrixInvert(const mat: TGLMatrix): TGLMatrix;
var
  scale: Single;
begin
  scale := VectorNorm(mat.X);

  // Is the submatrix A singular?
  if Abs(scale) < EPSILON then
  begin
    // Matrix M has no inverse
    result := IdentityHmgMatrix;
    Exit;
  end
  else
  begin
    // Calculate the inverse of the square of the isotropic scale factor
    scale := 1.0 / scale;
  end;

  // Fill in last row while CPU is busy with the division
  result.X.W := 0.0;
  result.Y.W := 0.0;
  result.Z.W := 0.0;
  result.W.W := 1.0;

  // Transpose and scale the 3 by 3 upper-left submatrix
  Transpose_Scale_M33(mat, result, scale);

  // Calculate -(transpose(A) / s*s) C
  result.W.X := -(result.X.X * mat.W.X + result.Y.X *
    mat.W.Y + result.Z.X * mat.W.Z);
  result.W.Y := -(result.X.Y * mat.W.X + result.Y.Y *
    mat.W.Y + result.Z.Y * mat.W.Z);
  result.W.Z := -(result.X.Z * mat.W.X + result.Y.Z *
    mat.W.Y + result.Z.Z * mat.W.Z);
end;

function MatrixDecompose(const M: TGLMatrix; var Tran: TTransformations): Boolean;
var
  I, J: Integer;
  LocMat, pmat, invpmat: TGLMatrix;
  prhs, psol: TGLVector;
  row0, row1, row2: TAffineVector;
  f: Single;
begin
  Result := False;
  LocMat := M;
  // normalize the matrix
  if LocMat.W.W = 0 then
    Exit;
  for I := 0 to 3 do
    for J := 0 to 3 do
      LocMat.V[I].V[J] := LocMat.V[I].V[J] / LocMat.W.W;

  // pmat is used to solve for perspective, but it also provides
  // an easy way to test for singularity of the upper 3x3 component.

  pmat := LocMat;
  for I := 0 to 2 do
    pmat.V[I].V[W] := 0;
  pmat.W.W := 1;

  if MatrixDeterminant(pmat) = 0 then
    Exit;

  // First, isolate perspective.  This is the messiest.
  if (LocMat.X.W <> 0) or (LocMat.Y.W <> 0) or (LocMat.Z.W <> 0) then
  begin
    // prhs is the right hand side of the equation.
    prhs.X := LocMat.X.W;
    prhs.Y := LocMat.Y.W;
    prhs.Z := LocMat.Z.W;
    prhs.W := LocMat.W.W;

    // Solve the equation by inverting pmat and multiplying
    // prhs by the inverse.  (This is the easiest way, not
    // necessarily the best.)

    invpmat := pmat;
    InvertMatrix(invpmat);
    TransposeMatrix(invpmat);
    psol := VectorTransform(prhs, invpmat);

    // stuff the answer away
    Tran[ttPerspectiveX] := psol.X;
    Tran[ttPerspectiveY] := psol.Y;
    Tran[ttPerspectiveZ] := psol.Z;
    Tran[ttPerspectiveW] := psol.W;

    // clear the perspective partition
    LocMat.X.W := 0;
    LocMat.Y.W := 0;
    LocMat.Z.W := 0;
    LocMat.W.W := 1;
  end
  else
  begin
    // no perspective
    Tran[ttPerspectiveX] := 0;
    Tran[ttPerspectiveY] := 0;
    Tran[ttPerspectiveZ] := 0;
    Tran[ttPerspectiveW] := 0;
  end;

  // next take care of translation (easy)
  for I := 0 to 2 do
  begin
    Tran[TTransType(Ord(ttTranslateX) + I)] := LocMat.V[W].V[I];
    LocMat.V[W].V[I] := 0;
  end;

  // now get scale and shear
  SetVector(row0, LocMat.X);
  SetVector(row1, LocMat.Y);
  SetVector(row2, LocMat.Z);

  // compute X scale factor and normalize first row
  Tran[ttScaleX] := VectorNorm(row0);
  VectorScale(row0, RSqrt(Tran[ttScaleX]));

  // compute XY shear factor and make 2nd row orthogonal to 1st
  Tran[ttShearXY] := VectorDotProduct(row0, row1);
  f := -Tran[ttShearXY];
  CombineVector(row1, row0, f);

  // now, compute Y scale and normalize 2nd row
  Tran[ttScaleY] := VectorNorm(row1);
  VectorScale(row1, RSqrt(Tran[ttScaleY]));
  Tran[ttShearXY] := Tran[ttShearXY] / Tran[ttScaleY];

  // compute XZ and YZ shears, orthogonalize 3rd row
  Tran[ttShearXZ] := VectorDotProduct(row0, row2);
  f := -Tran[ttShearXZ];
  CombineVector(row2, row0, f);
  Tran[ttShearYZ] := VectorDotProduct(row1, row2);
  f := -Tran[ttShearYZ];
  CombineVector(row2, row1, f);

  // next, get Z scale and normalize 3rd row
  Tran[ttScaleZ] := VectorNorm(row2);
  VectorScale(row2, RSqrt(Tran[ttScaleZ]));
  Tran[ttShearXZ] := Tran[ttShearXZ] / Tran[ttScaleZ];
  Tran[ttShearYZ] := Tran[ttShearYZ] / Tran[ttScaleZ];

  // At this point, the matrix (in rows[]) is orthonormal.
  // Check for a coordinate system flip.  If the determinant
  // is -1, then negate the matrix and the scaling factors.
  if VectorDotProduct(row0, VectorCrossProduct(row1, row2)) < 0 then
  begin
    for I := 0 to 2 do
      Tran[TTransType(Ord(ttScaleX) + I)] :=
        -Tran[TTransType(Ord(ttScaleX) + I)];
    NegateVector(row0);
    NegateVector(row1);
    NegateVector(row2);
  end;

  // now, get the rotations out, as described in the gem
  Tran[ttRotateY] := ArcSin(-row0.Z);
  if Cos(Tran[ttRotateY]) <> 0 then
  begin
    Tran[ttRotateX] := ArcTan2(row1.V[Z], row2.V[Z]);
    Tran[ttRotateZ] := ArcTan2(row0.V[Y], row0.V[X]);
  end
  else
  begin
    Tran[ttRotateX] := ArcTan2(row1.V[X], row1.V[Y]);
    Tran[ttRotateZ] := 0;
  end;
  // All done!
  result := True;
end;

function CreateLookAtMatrix(const eye, center, normUp: TGLVector): TGLMatrix;
var
  XAxis, YAxis, ZAxis, negEye: TGLVector;
begin
  ZAxis := VectorSubtract(center, eye);
  NormalizeVector(ZAxis);
  XAxis := VectorCrossProduct(ZAxis, normUp);
  NormalizeVector(XAxis);
  YAxis := VectorCrossProduct(XAxis, ZAxis);
  result.X := XAxis;
  result.Y := YAxis;
  result.Z := ZAxis;
  NegateVector(result.Z);
  result.W := NullHmgPoint;
  TransposeMatrix(result);
  negEye := eye;
  NegateVector(negEye);
  negEye.W := 1;
  negEye := VectorTransform(negEye, result);
  result.W := negEye;
end;

function CreateMatrixFromFrustum(Left, Right, Bottom, Top, ZNear,
  ZFar: Single): TGLMatrix;
begin
  result.X.X := 2 * ZNear / (Right - Left);
  result.X.Y := 0;
  result.X.Z := 0;
  result.X.W := 0;

  result.Y.X := 0;
  result.Y.Y := 2 * ZNear / (Top - Bottom);
  result.Y.Z := 0;
  result.Y.W := 0;

  result.Z.X := (Right + Left) / (Right - Left);
  result.Z.Y := (Top + Bottom) / (Top - Bottom);
  result.Z.Z := -(ZFar + ZNear) / (ZFar - ZNear);
  result.Z.W := -1;

  result.W.X := 0;
  result.W.Y := 0;
  result.W.Z := -2 * ZFar * ZNear / (ZFar - ZNear);
  result.W.W := 0;
end;

function CreatePerspectiveMatrix(FOV, Aspect, ZNear, ZFar: Single): TGLMatrix;
var
  X, Y: Single;
begin
  FOV := MinFloat(179.9, MaxFloat(0, FOV));
  Y := ZNear * Tangent(DegToRadian(FOV) * 0.5);
  X := Y * Aspect;
  result := CreateMatrixFromFrustum(-X, X, -Y, Y, ZNear, ZFar);
end;

function CreateOrthoMatrix(Left, Right, Bottom, Top, ZNear,
  ZFar: Single): TGLMatrix;
begin
  result.X.X := 2 / (Right - Left);
  result.X.Y := 0;
  result.X.Z := 0;
  result.X.W := 0;

  result.Y.X := 0;
  result.Y.Y := 2 / (Top - Bottom);
  result.Y.Z := 0;
  result.Y.W := 0;

  result.Z.X := 0;
  result.Z.Y := 0;
  result.Z.Z := -2 / (ZFar - ZNear);
  result.Z.W := 0;

  result.W.X := (Left + Right) / (Left - Right);
  result.W.Y := (Bottom + Top) / (Bottom - Top);
  result.W.Z := (ZNear + ZFar) / (ZNear - ZFar);
  result.W.W := 1;
end;

function CreatePickMatrix(X, Y, deltax, deltay: Single;
  const viewport: TVector4i): TGLMatrix;
begin
  if (deltax <= 0) or (deltay <= 0) then
  begin
    result := IdentityHmgMatrix;
    Exit;
  end;
  // Translate and scale the picked region to the entire window
  result := CreateTranslationMatrix
    (AffineVectorMake((viewport.Z - 2 * (X - viewport.X)) / deltax,
    (viewport.W - 2 * (Y - viewport.Y)) / deltay, 0.0));
  result.X.X := viewport.Z / deltax;
  result.Y.Y := viewport.W / deltay;
end;

function Project(objectVector: TGLVector; const ViewProjMatrix: TGLMatrix;
  const viewport: TVector4i; out WindowVector: TGLVector): Boolean;
begin
  result := False;
  objectVector.W := 1.0;
  WindowVector := VectorTransform(objectVector, ViewProjMatrix);
  if WindowVector.W = 0.0 then
    Exit;
  WindowVector.X := WindowVector.X / WindowVector.W;
  WindowVector.Y := WindowVector.Y / WindowVector.W;
  WindowVector.Z := WindowVector.Z / WindowVector.W;
  // Map x, y and z to range 0-1
  WindowVector.X := WindowVector.X * 0.5 + 0.5;
  WindowVector.Y := WindowVector.Y * 0.5 + 0.5;
  WindowVector.Z := WindowVector.Z * 0.5 + 0.5;

  // Map x,y to viewport
  WindowVector.X := WindowVector.X * viewport.Z + viewport.X;
  WindowVector.Y := WindowVector.Y * viewport.W + viewport.Y;
  result := True;
end;

function UnProject(WindowVector: TGLVector; ViewProjMatrix: TGLMatrix;
  const viewport: TVector4i; out objectVector: TGLVector): Boolean;
begin
  result := False;
  InvertMatrix(ViewProjMatrix);
  WindowVector.W := 1.0;
  // Map x and y from window coordinates
  WindowVector.X := (WindowVector.X - viewport.X) / viewport.Z;
  WindowVector.Y := (WindowVector.Y - viewport.Y) / viewport.W;
  // Map to range -1 to 1
  WindowVector.X := WindowVector.X * 2 - 1;
  WindowVector.Y := WindowVector.Y * 2 - 1;
  WindowVector.Z := WindowVector.Z * 2 - 1;
  objectVector := VectorTransform(WindowVector, ViewProjMatrix);
  if objectVector.W = 0.0 then
    Exit;
  objectVector.X := objectVector.X / objectVector.W;
  objectVector.Y := objectVector.Y / objectVector.W;
  objectVector.Z := objectVector.Z / objectVector.W;
  result := True;
end;

function CalcPlaneNormal(const p1, p2, p3: TAffineVector): TAffineVector;
var
  V1, V2: TAffineVector;
begin
  VectorSubtract(p2, p1, V1);
  VectorSubtract(p3, p1, V2);
  VectorCrossProduct(V1, V2, result);
  NormalizeVector(result);
end;

procedure CalcPlaneNormal(const p1, p2, p3: TAffineVector; var vr: TAffineVector);
var
  V1, V2: TAffineVector;
begin
  VectorSubtract(p2, p1, V1);
  VectorSubtract(p3, p1, V2);
  VectorCrossProduct(V1, V2, vr);
  NormalizeVector(vr);
end;

procedure CalcPlaneNormal(const p1, p2, p3: TGLVector; var vr: TAffineVector); overload;
var
  V1, V2: TGLVector;
begin
  VectorSubtract(p2, p1, V1);
  VectorSubtract(p3, p1, V2);
  VectorCrossProduct(V1, V2, vr);
  NormalizeVector(vr);
end;

function PlaneMake(const point, normal: TAffineVector): THmgPlane;
begin
  PAffineVector(@result)^ := normal;
  result.W := -VectorDotProduct(point, normal);
end;

function PlaneMake(const point, normal: TGLVector): THmgPlane;
begin
  PAffineVector(@result)^ := PAffineVector(@normal)^;
  Result.W := -VectorDotProduct(PAffineVector(@point)^, PAffineVector(@normal)^);
end;

function PlaneMake(const p1, p2, p3: TAffineVector): THmgPlane;
begin
  CalcPlaneNormal(p1, p2, p3, PAffineVector(@result)^);
  result.W := -VectorDotProduct(p1, PAffineVector(@result)^);
end;

function PlaneMake(const p1, p2, p3: TGLVector): THmgPlane;
begin
  CalcPlaneNormal(p1, p2, p3, PAffineVector(@result)^);
  result.W := -VectorDotProduct(p1, PAffineVector(@result)^);
end;

procedure SetPlane(var dest: TDoubleHmgPlane; const src: THmgPlane);
begin
  dest.X := src.X;
  dest.Y := src.Y;
  dest.Z := src.Z;
  dest.W := src.W;
end;

procedure NormalizePlane(var plane: THmgPlane);
var
  n: Single;
begin
  n := RSqrt(plane.X * plane.X + plane.Y * plane.Y + plane.Z *
    plane.Z);
  ScaleVector(plane, n);
end;

function PlaneEvaluatePoint(const plane: THmgPlane; const point: TAffineVector): Single;
begin
  result := plane.X * point.X + plane.Y * point.Y + plane.Z *
    point.Z + plane.W;
end;

function PlaneEvaluatePoint(const plane: THmgPlane;
  const point: TGLVector): Single;
begin
  result := plane.X * point.X + plane.Y * point.Y + plane.Z * point.Z + plane.W;
end;

function PointIsInHalfSpace(const point, planePoint, planeNormal: TGLVector): Boolean;
begin
  result := (PointPlaneDistance(point, planePoint, planeNormal) > 0); // 44
end;

function PointIsInHalfSpace(const point, planePoint,
  planeNormal: TAffineVector): Boolean;
begin
  result := (PointPlaneDistance(point, planePoint, planeNormal) > 0);
end;

function PointIsInHalfSpace(const point: TAffineVector;
  const plane: THmgPlane): Boolean;
begin
  result := (PointPlaneDistance(point, plane) > 0);
end;

function PointPlaneDistance(const point, planePoint,
  planeNormal: TGLVector): Single;
begin
  result := (point.X - planePoint.X) * planeNormal.X +
    (point.Y - planePoint.Y) * planeNormal.Y +
    (point.Z - planePoint.Z) * planeNormal.Z;
end;

function PointPlaneDistance(const point, planePoint,
  planeNormal: TAffineVector): Single;
begin
  result := (point.X - planePoint.X) * planeNormal.X +
    (point.Y - planePoint.Y) * planeNormal.Y +
    (point.Z - planePoint.Z) * planeNormal.Z;
end;

function PointPlaneDistance(const point: TAffineVector;
  const plane: THmgPlane): Single;
begin
  result := PlaneEvaluatePoint(plane, point);
end;

function PointPlaneOrthoProjection(const point: TAffineVector;
  const plane: THmgPlane; var inter: TAffineVector;
  bothface: Boolean = True): Boolean;
var
  h: Single;
  normal: TAffineVector;
begin
  result := False;

  h := PointPlaneDistance(point, plane);

  if (not bothface) and (h < 0) then
    Exit;

  normal := Vector3fMake(plane);
  inter := VectorAdd(point, VectorScale(normal, -h));
  result := True;
end;

function PointPlaneProjection(const point, direction: TAffineVector;
  const plane: THmgPlane; var inter: TAffineVector;
  bothface: Boolean = True): Boolean;
var
  h, dot: Single;
  normal: TAffineVector;
begin
  result := False;

  normal := Vector3fMake(plane);
  dot := VectorDotProduct(VectorNormalize(direction), normal);

  if (not bothface) and (dot > 0) then
    Exit;

  if Abs(dot) >= 0.000000001 then
  begin
    h := PointPlaneDistance(point, plane);
    inter := VectorAdd(point, VectorScale(direction, -h / dot));
    result := True;
  end;
end;

function SegmentPlaneIntersection(const ptA, ptB: TAffineVector;
  const plane: THmgPlane; var inter: TAffineVector): Boolean;
var
  hA, hB, dot: Single;
  normal, direction: TVector3f;
begin
  result := False;
  hA := PointPlaneDistance(ptA, plane);
  hB := PointPlaneDistance(ptB, plane);
  if hA * hB <= 0 then
  begin
    normal := Vector3fMake(plane);
    direction := VectorNormalize(VectorSubtract(ptB, ptA));
    dot := VectorDotProduct(direction, normal);
    if Abs(dot) >= 0.000000001 then
    begin
      inter := VectorAdd(ptA, VectorScale(direction, -hA / dot));
      result := True;
    end;
  end;
end;

function PointTriangleOrthoProjection(const point, ptA, ptB, ptC: TAffineVector;
  var inter: TAffineVector; bothface: Boolean = True): Boolean;
var
  plane: THmgPlane;
begin
  result := False;

  plane := PlaneMake(ptA, ptB, ptC);
  if not IsLineIntersectTriangle(point, Vector3fMake(plane), ptA, ptB, ptC) then
    Exit;

  result := PointPlaneOrthoProjection(point, plane, inter, bothface);
end;

function PointTriangleProjection(const point, direction, ptA, ptB,
  ptC: TAffineVector; var inter: TAffineVector;
  bothface: Boolean = True): Boolean;
var
  plane: THmgPlane;
begin
  result := False;

  if not IsLineIntersectTriangle(point, direction, ptA, ptB, ptC) then
    Exit;

  plane := PlaneMake(ptA, ptB, ptC);
  result := PointPlaneProjection(point, direction, plane, inter, bothface);
end;

function IsLineIntersectTriangle(const point, direction, ptA, ptB,
  ptC: TAffineVector): Boolean;
var
  PA, PB, PC: TAffineVector;
  crossAB, crossBC, crossCA: TAffineVector;
begin
  result := False;
  PA := VectorSubtract(ptA, point);
  PB := VectorSubtract(ptB, point);
  PC := VectorSubtract(ptC, point);
  crossAB := VectorCrossProduct(PA, PB);
  crossBC := VectorCrossProduct(PB, PC);
  if VectorDotProduct(crossAB, direction) > 0 then
  begin
    if VectorDotProduct(crossBC, direction) > 0 then
    begin
      crossCA := VectorCrossProduct(PC, PA);
      if VectorDotProduct(crossCA, direction) > 0 then
        result := True;
    end;
  end
  else if VectorDotProduct(crossBC, direction) < 0 then
  begin
    crossCA := VectorCrossProduct(PC, PA);
    if VectorDotProduct(crossCA, direction) < 0 then
      result := True;
  end
end;

function PointQuadOrthoProjection(const point, ptA, ptB, ptC,
  ptD: TAffineVector; var inter: TAffineVector;
  bothface: Boolean = True): Boolean;
var
  plane: THmgPlane;
begin
  result := False;
  plane := PlaneMake(ptA, ptB, ptC);
  if not IsLineIntersectQuad(point, Vector3fMake(plane), ptA, ptB, ptC, ptD)
  then
    Exit;
  result := PointPlaneOrthoProjection(point, plane, inter, bothface);
end;

function PointQuadProjection(const point, direction, ptA, ptB, ptC,
  ptD: TAffineVector; var inter: TAffineVector;
  bothface: Boolean = True): Boolean;
var
  plane: THmgPlane;
begin
  result := False;
  if not IsLineIntersectQuad(point, direction, ptA, ptB, ptC, ptD) then
    Exit;
  plane := PlaneMake(ptA, ptB, ptC);
  result := PointPlaneProjection(point, direction, plane, inter, bothface);
end;

function IsLineIntersectQuad(const point, direction, ptA, ptB, ptC,
  ptD: TAffineVector): Boolean;
var
  PA, PB, PC, PD: TAffineVector;
  crossAB, crossBC, crossCD, crossDA: TAffineVector;
begin
  result := False;
  PA := VectorSubtract(ptA, point);
  PB := VectorSubtract(ptB, point);
  PC := VectorSubtract(ptC, point);
  PD := VectorSubtract(ptD, point);
  crossAB := VectorCrossProduct(PA, PB);
  crossBC := VectorCrossProduct(PB, PC);
  if VectorDotProduct(crossAB, direction) > 0 then
  begin
    if VectorDotProduct(crossBC, direction) > 0 then
    begin
      crossCD := VectorCrossProduct(PC, PD);
      if VectorDotProduct(crossCD, direction) > 0 then
      begin
        crossDA := VectorCrossProduct(PD, PA);
        if VectorDotProduct(crossDA, direction) > 0 then
          result := True;
      end;
    end;
  end
  else if VectorDotProduct(crossBC, direction) < 0 then
  begin
    crossCD := VectorCrossProduct(PC, PD);
    if VectorDotProduct(crossCD, direction) < 0 then
    begin
      crossDA := VectorCrossProduct(PD, PA);
      if VectorDotProduct(crossDA, direction) < 0 then
        result := True;
    end;
  end
end;

function PointDiskOrthoProjection(const point, center, up: TAffineVector;
  const radius: Single; var inter: TAffineVector;
  bothface: Boolean = True): Boolean;
begin
  if PointPlaneOrthoProjection(point, PlaneMake(center, up), inter, bothface)
  then
    result := (VectorDistance2(inter, center) <= radius * radius)
  else
    result := False;
end;

function PointDiskProjection(const point, direction, center, up: TAffineVector;
  const radius: Single; var inter: TAffineVector;
  bothface: Boolean = True): Boolean;
begin
  if PointPlaneProjection(point, direction, PlaneMake(center, up), inter,
    bothface) then
    result := VectorDistance2(inter, center) <= radius * radius
  else
    result := False;
end;

function PointLineClosestPoint(const point, linePoint, lineDirection
  : TAffineVector): TAffineVector;
var
  W: TAffineVector;
  c1, c2, b: Single;
begin
  W := VectorSubtract(point, linePoint);

  c1 := VectorDotProduct(W, lineDirection);
  c2 := VectorDotProduct(lineDirection, lineDirection);
  b := c1 / c2;

  VectorAdd(linePoint, VectorScale(lineDirection, b), result);
end;

function PointLineDistance(const point, linePoint, lineDirection: TAffineVector): Single;
var
  PB: TAffineVector;
begin
  PB := PointLineClosestPoint(point, linePoint, lineDirection);
  result := VectorDistance(point, PB);
end;

function PointSegmentClosestPoint(const point, segmentStart,
  segmentStop: TGLVector): TGLVector;
var
  W, lineDirection: TGLVector;
  c1, c2, b: Single;
begin
  lineDirection := VectorSubtract(segmentStop, segmentStart);
  W := VectorSubtract(point, segmentStart);
  c1 := VectorDotProduct(W, lineDirection);
  c2 := VectorDotProduct(lineDirection, lineDirection);
  b := ClampValue(c1 / c2, 0, 1);
  VectorAdd(segmentStart, VectorScale(lineDirection, b), result);
end;

function PointSegmentClosestPoint(const point, segmentStart,
  segmentStop: TAffineVector): TAffineVector;
var
  W, lineDirection: TAffineVector;
  c1, c2, b: Single;
begin
  lineDirection := VectorSubtract(segmentStop, segmentStart);
  W := VectorSubtract(point, segmentStart);

  c1 := VectorDotProduct(W, lineDirection);
  c2 := VectorDotProduct(lineDirection, lineDirection);
  b := ClampValue(c1 / c2, 0, 1);

  VectorAdd(segmentStart, VectorScale(lineDirection, b), result);
end;

function PointSegmentDistance(const point, segmentStart,
  segmentStop: TAffineVector): Single;
var
  PB: TAffineVector;
begin
  PB := PointSegmentClosestPoint(point, segmentStart, segmentStop);
  result := VectorDistance(point, PB);
end;

// http://geometryalgorithms.com/Archive/algorithm_0104/algorithm_0104B.htm
procedure SegmentSegmentClosestPoint(const S0Start, S0Stop, S1Start,
  S1Stop: TAffineVector; var Segment0Closest, Segment1Closest: TAffineVector);
const
  cSMALL_NUM = 0.000000001;
var
  u, V, W: TAffineVector;
  a, b, c, smalld, e, largeD, sc, sn, sD, tc, tN, tD: Single;
begin
  VectorSubtract(S0Stop, S0Start, u);
  VectorSubtract(S1Stop, S1Start, V);
  VectorSubtract(S0Start, S1Start, W);

  a := VectorDotProduct(u, u);
  b := VectorDotProduct(u, V);
  c := VectorDotProduct(V, V);
  smalld := VectorDotProduct(u, W);
  e := VectorDotProduct(V, W);
  largeD := a * c - b * b;

  sD := largeD;
  tD := largeD;

  if largeD < cSMALL_NUM then
  begin
    sn := 0.0;
    sD := 1.0;
    tN := e;
    tD := c;
  end
  else
  begin
    sn := (b * e - c * smalld);
    tN := (a * e - b * smalld);
    if (sn < 0.0) then
    begin
      sn := 0.0;
      tN := e;
      tD := c;
    end
    else if (sn > sD) then
    begin
      sn := sD;
      tN := e + b;
      tD := c;
    end;
  end;

  if (tN < 0.0) then
  begin
    tN := 0.0;
    // recompute sc for this edge
    if (-smalld < 0.0) then
      sn := 0.0
    else if (-smalld > a) then
      sn := sD
    else
    begin
      sn := -smalld;
      sD := a;
    end;
  end
  else if (tN > tD) then
  begin
    tN := tD;
    // recompute sc for this edge
    if ((-smalld + b) < 0.0) then
      sn := 0
    else if ((-smalld + b) > a) then
      sn := sD
    else
    begin
      sn := (-smalld + b);
      sD := a;
    end;
  end;

  // finally do the division to get sc and tc
  // sc := (abs(sN) < SMALL_NUM ? 0.0 : sN / sD);
  if Abs(sn) < cSMALL_NUM then
    sc := 0
  else
    sc := sn / sD;

  // tc := (abs(tN) < SMALL_NUM ? 0.0 : tN / tD);
  if Abs(tN) < cSMALL_NUM then
    tc := 0
  else
    tc := tN / tD;

  // get the difference of the two closest points
  // Vector   dP = w + (sc * u) - (tc * v);  // = S0(sc) - S1(tc)

  Segment0Closest := VectorAdd(S0Start, VectorScale(u, sc));
  Segment1Closest := VectorAdd(S1Start, VectorScale(V, tc));
end;

function SegmentSegmentDistance(const S0Start, S0Stop, S1Start,
  S1Stop: TAffineVector): Single;
var
  Pb0, PB1: TAffineVector;
begin
  SegmentSegmentClosestPoint(S0Start, S0Stop, S1Start, S1Stop, Pb0, PB1);
  result := VectorDistance(Pb0, PB1);
end;

function LineLineDistance(const linePt0, lineDir0, linePt1,
  lineDir1: TAffineVector): Single;
const
  cBIAS = 0.000000001;
var
  det: Single;
begin
  det := Abs((linePt1.X - linePt0.X) * (lineDir0.Y * lineDir1.Z -
    lineDir1.Y * lineDir0.Z) - (linePt1.Y - linePt0.Y) *
    (lineDir0.X * lineDir1.Z - lineDir1.X * lineDir0.Z) +
    (linePt1.Z - linePt0.Z) * (lineDir0.X * lineDir1.Y -
    lineDir1.X * lineDir0.Y));
  if det < cBIAS then
    result := PointLineDistance(linePt0, linePt1, lineDir1)
  else
    result := det / VectorLength(VectorCrossProduct(lineDir0, lineDir1));
end;

function QuaternionMake(const Imag: array of Single; Real: Single): TQuaternion;
var
  n: Integer;
begin
  n := Length(Imag);
  if n >= 1 then
    result.ImagPart.X := Imag[0];
  if n >= 2 then
    result.ImagPart.Y := Imag[1];
  if n >= 3 then
    result.ImagPart.Z := Imag[2];
  result.RealPart := Real;
end;

function QuaternionMake(const X,Y,Z,W: Single): TQuaternion; overload;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

function QuaternionMake(const V: TGLVector): TQuaternion; overload;
begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := V.Z;
  Result.W := V.W;
end;

function QuaternionConjugate(const Q: TQuaternion): TQuaternion;
begin
  result.ImagPart.X := -Q.ImagPart.X;
  result.ImagPart.Y := -Q.ImagPart.Y;
  result.ImagPart.Z := -Q.ImagPart.Z;
  result.RealPart := Q.RealPart;
end;

function QuaternionMagnitude(const Q: TQuaternion): Single;
begin
  result := Sqrt(VectorNorm(Q.ImagPart) + Sqr(Q.RealPart));
end;

procedure NormalizeQuaternion(var Q: TQuaternion);
var
  M, f: Single;
begin
  M := QuaternionMagnitude(Q);
  if M > EPSILON2 then
  begin
    f := 1 / M;
    ScaleVector(Q.ImagPart, f);
    Q.RealPart := Q.RealPart * f;
  end
  else
    Q := IdentityQuaternion;
end;

function QuaternionFromPoints(const V1, V2: TAffineVector): TQuaternion;
begin
  result.ImagPart := VectorCrossProduct(V1, V2);
  result.RealPart := Sqrt((VectorDotProduct(V1, V2) + 1) / 2);
end;

function QuaternionFromMatrix(const mat: TGLMatrix): TQuaternion;
// the matrix must be a rotation matrix!
var
  traceMat, S, invS: Double;
begin
  traceMat := 1 + mat.X.X + mat.Y.Y + mat.Z.Z;
  if traceMat > EPSILON2 then
  begin
    S := Sqrt(traceMat) * 2;
    invS := 1 / S;
    result.ImagPart.X := (mat.Y.Z - mat.Z.Y) * invS;
    result.ImagPart.Y := (mat.Z.X - mat.X.Z) * invS;
    result.ImagPart.Z := (mat.X.Y - mat.Y.X) * invS;
    result.RealPart := 0.25 * S;
  end
  else if (mat.X.X > mat.Y.Y) and (mat.X.X > mat.Z.Z)
  then
  begin // Row 0:
    S := Sqrt(MaxFloat(EPSILON2, cOne + mat.X.X - mat.Y.Y -
      mat.Z.Z)) * 2;
    invS := 1 / S;
    result.ImagPart.X := 0.25 * S;
    result.ImagPart.Y := (mat.X.Y + mat.Y.X) * invS;
    result.ImagPart.Z := (mat.Z.X + mat.X.Z) * invS;
    result.RealPart := (mat.Y.Z - mat.Z.Y) * invS;
  end
  else if (mat.Y.Y > mat.Z.Z) then
  begin // Row 1:
    S := Sqrt(MaxFloat(EPSILON2, cOne + mat.Y.Y - mat.X.X -
      mat.Z.Z)) * 2;
    invS := 1 / S;
    result.ImagPart.X := (mat.X.Y + mat.Y.X) * invS;
    result.ImagPart.Y := 0.25 * S;
    result.ImagPart.Z := (mat.Y.Z + mat.Z.Y) * invS;
    result.RealPart := (mat.Z.X - mat.X.Z) * invS;
  end
  else
  begin // Row 2:
    S := Sqrt(MaxFloat(EPSILON2, cOne + mat.Z.Z - mat.X.X -
      mat.Y.Y)) * 2;
    invS := 1 / S;
    result.ImagPart.X := (mat.Z.X + mat.X.Z) * invS;
    result.ImagPart.Y := (mat.Y.Z + mat.Z.Y) * invS;
    result.ImagPart.Z := 0.25 * S;
    result.RealPart := (mat.X.Y - mat.Y.X) * invS;
  end;
  NormalizeQuaternion(result);
end;

function QuaternionMultiply(const qL, qR: TQuaternion): TQuaternion;
var
  Temp: TQuaternion;
begin
  Temp.RealPart := qL.RealPart * qR.RealPart - qL.ImagPart.V[X] * qR.ImagPart.V
    [X] - qL.ImagPart.V[Y] * qR.ImagPart.V[Y] - qL.ImagPart.V[Z] *
    qR.ImagPart.V[Z];
  Temp.ImagPart.V[X] := qL.RealPart * qR.ImagPart.V[X] + qL.ImagPart.V[X] *
    qR.RealPart + qL.ImagPart.V[Y] * qR.ImagPart.V[Z] - qL.ImagPart.V[Z] *
    qR.ImagPart.V[Y];
  Temp.ImagPart.V[Y] := qL.RealPart * qR.ImagPart.V[Y] + qL.ImagPart.V[Y] *
    qR.RealPart + qL.ImagPart.V[Z] * qR.ImagPart.V[X] - qL.ImagPart.V[X] *
    qR.ImagPart.V[Z];
  Temp.ImagPart.V[Z] := qL.RealPart * qR.ImagPart.V[Z] + qL.ImagPart.V[Z] *
    qR.RealPart + qL.ImagPart.V[X] * qR.ImagPart.V[Y] - qL.ImagPart.V[Y] *
    qR.ImagPart.V[X];
  result := Temp;
end;

function QuaternionToMatrix(quat: TQuaternion): TGLMatrix;
var
  W, X, Y, Z, xx, xy, xz, xw, yy, yz, yw, zz, zw: Single;
begin
  NormalizeQuaternion(quat);
  W := quat.RealPart;
  X := quat.ImagPart.X;
  Y := quat.ImagPart.Y;
  Z := quat.ImagPart.Z;
  xx := X * X;
  xy := X * Y;
  xz := X * Z;
  xw := X * W;
  yy := Y * Y;
  yz := Y * Z;
  yw := Y * W;
  zz := Z * Z;
  zw := Z * W;
  result.X.X := 1 - 2 * (yy + zz);
  result.Y.X := 2 * (xy - zw);
  result.Z.X := 2 * (xz + yw);
  result.W.X := 0;
  result.X.Y := 2 * (xy + zw);
  result.Y.Y := 1 - 2 * (xx + zz);
  result.Z.Y := 2 * (yz - xw);
  result.W.Y := 0;
  result.X.Z := 2 * (xz - yw);
  result.Y.Z := 2 * (yz + xw);
  result.Z.Z := 1 - 2 * (xx + yy);
  result.W.Z := 0;
  result.X.W := 0;
  result.Y.W := 0;
  result.Z.W := 0;
  result.W.W := 1;
end;

function QuaternionToAffineMatrix(quat: TQuaternion): TAffineMatrix;
var
  W, X, Y, Z, xx, xy, xz, xw, yy, yz, yw, zz, zw: Single;
begin
  NormalizeQuaternion(quat);
  W := quat.RealPart;
  X := quat.ImagPart.X;
  Y := quat.ImagPart.Y;
  Z := quat.ImagPart.Z;
  xx := X * X;
  xy := X * Y;
  xz := X * Z;
  xw := X * W;
  yy := Y * Y;
  yz := Y * Z;
  yw := Y * W;
  zz := Z * Z;
  zw := Z * W;
  result.X.X := 1 - 2 * (yy + zz);
  result.Y.X := 2 * (xy - zw);
  result.Z.X := 2 * (xz + yw);
  result.X.Y := 2 * (xy + zw);
  result.Y.Y := 1 - 2 * (xx + zz);
  result.Z.Y := 2 * (yz - xw);
  result.X.Z := 2 * (xz - yw);
  result.Y.Z := 2 * (yz + xw);
  result.Z.Z := 1 - 2 * (xx + yy);
end;

function QuaternionFromAngleAxis(const angle: Single; const axis: TAffineVector)
  : TQuaternion;
var
  f, S, c: Single;
begin
  SinCosine(DegToRadian(angle * cOneDotFive), S, c);
  result.RealPart := c;
  f := S / VectorLength(axis);
  result.ImagPart.X := axis.X * f;
  result.ImagPart.Y := axis.Y * f;
  result.ImagPart.Z := axis.Z * f;
end;

function QuaternionFromRollPitchYaw(const r, p, Y: Single): TQuaternion;
var
  qp, qy: TQuaternion;
begin
  result := QuaternionFromAngleAxis(r, ZVector);
  qp := QuaternionFromAngleAxis(p, XVector);
  qy := QuaternionFromAngleAxis(Y, YVector);

  result := QuaternionMultiply(qp, result);
  result := QuaternionMultiply(qy, result);
end;

function QuaternionFromEuler(const X, Y, Z: Single; eulerOrder: TEulerOrder): TQuaternion;
// input angles in degrees
var
  gimbalLock: Boolean;
  quat1, quat2: TQuaternion;

  function EulerToQuat(const X, Y, Z: Single; eulerOrder: TEulerOrder)
    : TQuaternion;
  const
    cOrder: array [Low(TEulerOrder) .. High(TEulerOrder)] of array [1 .. 3]
      of Byte = ((1, 2, 3), (1, 3, 2), (2, 1, 3), // eulXYZ, eulXZY, eulYXZ,
      (3, 1, 2), (2, 3, 1), (3, 2, 1)); // eulYZX, eulZXY, eulZYX
  var
    Q: array [1 .. 3] of TQuaternion;
  begin
    Q[cOrder[eulerOrder][1]] := QuaternionFromAngleAxis(X, XVector);
    Q[cOrder[eulerOrder][2]] := QuaternionFromAngleAxis(Y, YVector);
    Q[cOrder[eulerOrder][3]] := QuaternionFromAngleAxis(Z, ZVector);
    result := QuaternionMultiply(Q[2], Q[3]);
    result := QuaternionMultiply(Q[1], result);
  end;

const
  SMALL_ANGLE = 0.001;
begin
  NormalizeDegAngle(X);
  NormalizeDegAngle(Y);
  NormalizeDegAngle(Z);
  case eulerOrder of
    eulXYZ, eulZYX:
      gimbalLock := Abs(Abs(Y) - 90.0) <= EPSILON2; // cos(Y) = 0;
    eulYXZ, eulZXY:
      gimbalLock := Abs(Abs(X) - 90.0) <= EPSILON2; // cos(X) = 0;
    eulXZY, eulYZX:
      gimbalLock := Abs(Abs(Z) - 90.0) <= EPSILON2; // cos(Z) = 0;
  else
    Assert(False);
    gimbalLock := False;
  end;
  if gimbalLock then
  begin
    case eulerOrder of
      eulXYZ, eulZYX:
        quat1 := EulerToQuat(X, Y - SMALL_ANGLE, Z, eulerOrder);
      eulYXZ, eulZXY:
        quat1 := EulerToQuat(X - SMALL_ANGLE, Y, Z, eulerOrder);
      eulXZY, eulYZX:
        quat1 := EulerToQuat(X, Y, Z - SMALL_ANGLE, eulerOrder);
    end;
    case eulerOrder of
      eulXYZ, eulZYX:
        quat2 := EulerToQuat(X, Y + SMALL_ANGLE, Z, eulerOrder);
      eulYXZ, eulZXY:
        quat2 := EulerToQuat(X + SMALL_ANGLE, Y, Z, eulerOrder);
      eulXZY, eulYZX:
        quat2 := EulerToQuat(X, Y, Z + SMALL_ANGLE, eulerOrder);
    end;
    result := QuaternionSlerp(quat1, quat2, 0.5);
  end
  else
  begin
    result := EulerToQuat(X, Y, Z, eulerOrder);
  end;
end;

procedure QuaternionToPoints(const Q: TQuaternion;
  var ArcFrom, ArcTo: TAffineVector);
var
  S, invS: Single;
begin
  S := Q.ImagPart.V[X] * Q.ImagPart.V[X] + Q.ImagPart.V[Y] * Q.ImagPart.V[Y];
  if S = 0 then
    SetAffineVector(ArcFrom, 0, 1, 0)
  else
  begin
    invS := RSqrt(S);
    SetAffineVector(ArcFrom, -Q.ImagPart.V[Y] * invS,
      Q.ImagPart.V[X] * invS, 0);
  end;
  ArcTo.V[X] := Q.RealPart * ArcFrom.V[X] - Q.ImagPart.V[Z] * ArcFrom.V[Y];
  ArcTo.V[Y] := Q.RealPart * ArcFrom.V[Y] + Q.ImagPart.V[Z] * ArcFrom.V[X];
  ArcTo.V[Z] := Q.ImagPart.V[X] * ArcFrom.V[Y] - Q.ImagPart.V[Y] * ArcFrom.V[X];
  if Q.RealPart < 0 then
    SetAffineVector(ArcFrom, -ArcFrom.V[X], -ArcFrom.V[Y], 0);
end;

function Logarithm2(const X: Single): Single;
begin
  result := Log2(X);
end;

function PowerSingle(const Base, Exponent: Single): Single;
begin
{$HINTS OFF}
  if Exponent = cZero then
    result := cOne
  else if (Base = cZero) and (Exponent > cZero) then
    result := cZero
  else if RoundInt(Exponent) = Exponent then
    result := Power(Base, Integer(Round(Exponent)))
  else
    result := Exp(Exponent * Ln(Base));
{$HINTS ON}
end;

function PowerInteger(Base: Single; Exponent: Integer): Single;
begin
{$HINTS OFF}
  result := Power(Base, Exponent);
{$HINTS ON}
end;

function PowerInt64(Base: Single; Exponent: Int64): Single;
begin
{$HINTS OFF}
  result := System.Math.Power(Base, Exponent);
{$HINTS ON}
end;

function DegToRadian(const Degrees: Extended): Extended;
begin
  result := Degrees * (PI / 180);
end;

function DegToRadian(const Degrees: Single): Single;
begin
  result := Degrees * cPIdiv180;
end;

function RadianToDeg(const Radians: Extended): Extended;
begin
  result := Radians * (180 / PI);
end;

function RadianToDeg(const Radians: Single): Single;
begin
  result := Radians * c180divPI;
end;

function NormalizeAngle(angle: Single): Single;
begin
  result := angle - Int(angle * cInv2PI) * c2PI;
  if result > PI then
    result := result - 2 * PI
  else if result < -PI then
    result := result + 2 * PI;
end;

function NormalizeDegAngle(angle: Single): Single;
begin
  result := angle - Int(angle * cInv360) * c360;
  if result > c180 then
    result := result - c360
  else if result < -c180 then
    result := result + c360;
end;

{$IFDEF USE_PLATFORM_HAS_EXTENDED}
procedure SinCosine(const Theta: Extended; out Sin, Cos: Extended);
begin
  Math.SinCos(Theta, Sin, Cos);
end;
{$ENDIF GLS_PLATFORM_HAS_EXTENDED}

procedure SinCosine(const Theta: Double; out Sin, Cos: Double);
var
  S, c: Extended;
begin
  SinCos(Theta, S, c);
{$HINTS OFF}
  Sin := S;
  Cos := c;
{$HINTS ON}
end;

procedure SinCosine(const Theta: Single; out Sin, Cos: Single);
var
  S, c: Extended;
begin
  SinCos(Theta, S, c);
{$HINTS OFF}
  Sin := S;
  Cos := c;
{$HINTS ON}
end;

{$IFDEF USE_PLATFORM_HAS_EXTENDED}

procedure SinCosine(const Theta, radius: Double; out Sin, Cos: Extended);
var
  S, c: Extended;
begin
  SinCos(Theta, S, c);
  Sin := S * radius;
  Cos := c * radius;
end;

{$ENDIF GLS_PLATFORM_HAS_EXTENDED}

procedure SinCosine(const Theta, radius: Double; out Sin, Cos: Double);
var
  S, c: Extended;
begin
  SinCos(Theta, S, c);
  Sin := S * radius;
  Cos := c * radius;
end;

procedure SinCosine(const Theta, radius: Single; out Sin, Cos: Single);
var
  S, c: Extended;
begin
  SinCos(Theta, S, c);
  Sin := S * radius;
  Cos := c * radius;
end;

procedure PrepareSinCosCache(var S, c: array of Single;
  startAngle, stopAngle: Single);
var
  i: Integer;
  d, alpha, beta: Single;
begin
  Assert((High(S) = High(c)) and (Low(S) = Low(c)));
  stopAngle := stopAngle + 1E-5;
  if High(S) > Low(S) then
    d := cPIdiv180 * (stopAngle - startAngle) / (High(S) - Low(S))
  else
    d := 0;

  if High(S) - Low(S) < 1000 then
  begin
    // Fast computation (approx 5.5x)
    alpha := 2 * Sqr(Sin(d * 0.5));
    beta := Sin(d);
    SinCos(startAngle * cPIdiv180, S[Low(S)], c[Low(S)]);
    for i := Low(S) to High(S) - 1 do
    begin
      // Make use of the incremental formulae:
      // cos (theta+delta) = cos(theta) - [alpha*cos(theta) + beta*sin(theta)]
      // sin (theta+delta) = sin(theta) - [alpha*sin(theta) - beta*cos(theta)]
      c[i + 1] := c[i] - alpha * c[i] - beta * S[i];
      S[i + 1] := S[i] - alpha * S[i] + beta * c[i];
    end;
  end
  else
  begin
    // Slower, but maintains precision when steps are small
    startAngle := startAngle * cPIdiv180;
    for i := Low(S) to High(S) do
      SinCos((i - Low(S)) * d + startAngle, S[i], c[i]);
  end;
end;

function ArcCosine(const X: Extended): Extended; overload;
begin
{$HINTS OFF}
  result := ArcCos(X);
{$HINTS ON}
end;

function ArcSinus(const X: Extended): Extended; overload;
begin
{$HINTS OFF}
  result := ArcSin(X);
{$HINTS ON}
end;

function FastArcTangent2(Y, X: Single): Single;
// accuracy of about 0.07 rads
const
  cEpsilon: Single = 1E-10;
var
  abs_y: Single;
begin
  abs_y := Abs(Y) + cEpsilon; // prevent 0/0 condition
  if Y < 0 then
  begin
    if X >= 0 then
      result := cPIdiv4 * (X - abs_y) / (X + abs_y) - cPIdiv4
    else
      result := cPIdiv4 * (X + abs_y) / (abs_y - X) - c3PIdiv4;
  end
  else
  begin
    if X >= 0 then
      result := cPIdiv4 - cPIdiv4 * (X - abs_y) / (X + abs_y)
    else
      result := c3PIdiv4 - cPIdiv4 * (X + abs_y) / (abs_y - X);
  end;
end;

function ISqrt(i: Integer): Integer;
begin
{$HINTS OFF}
  result := Round(Sqrt(i));
{$HINTS ON}
end;

function ILength(X, Y: Integer): Integer;
begin
{$HINTS OFF}
  result := Round(Sqrt(X * X + Y * Y));
{$HINTS ON}
end;

function ILength(X, Y, Z: Integer): Integer;
begin
{$HINTS OFF}
  result := Round(Sqrt(X * X + Y * Y + Z * Z));
{$HINTS ON}
end;

function RLength(X, Y: Single): Single;
begin
  result := 1 / Sqrt(X * X + Y * Y);
end;

procedure RandomPointOnSphere(var p: TAffineVector);
var
  T, W: Single;
begin
  p.Z := 2 * Random - 1;
  T := 2 * PI * Random;
  W := Sqrt(1 - p.Z * p.Z);
  SinCosine(T, W, p.Y, p.X);
end;

function RoundInt(V: Single): Single;
begin
{$HINTS OFF}
  result := Int(V + 0.5);
{$HINTS ON}
end;

function RoundInt(V: Extended): Extended;
begin
  result := Int(V + 0.5);
end;

function SignStrict(X: Single): Integer;
begin
  if X < 0 then
    result := -1
  else
    result := 1
end;

function ScaleAndRound(i: Integer; var S: Single): Integer;
begin
{$HINTS OFF}
  result := Round(i * S);
{$HINTS ON}
end;

function IsInRange(const X, a, b: Single): Boolean;
begin
  if a < b then
    result := (a <= X) and (X <= b)
  else
    result := (b <= X) and (X <= a);
end;

function IsInRange(const X, a, b: Double): Boolean;
begin
  if a < b then
    result := (a <= X) and (X <= b)
  else
    result := (b <= X) and (X <= a);
end;

function IsInCube(const p, d: TAffineVector): Boolean; overload;
begin
  result := ((p.X >= -d.X) and (p.X <= d.X)) and
    ((p.Y >= -d.Y) and (p.Y <= d.Y)) and
    ((p.Z >= -d.Z) and (p.Z <= d.Z));
end;

function IsInCube(const p, d: TGLVector): Boolean; overload;
begin
  result := ((p.X >= -d.X) and (p.X <= d.X)) and
    ((p.Y >= -d.Y) and (p.Y <= d.Y)) and
    ((p.Z >= -d.Z) and (p.Z <= d.Z));
end;

function MinFloat(values: PSingleArray; nbItems: Integer): Single;
var
  i, k: Integer;
begin
  if nbItems > 0 then
  begin
    k := 0;
    for i := 1 to nbItems - 1 do
      if values^[i] < values^[k] then
        k := i;
    result := values^[k];
  end
  else
    result := 0;
end;

function MinFloat(values: PDoubleArray; nbItems: Integer): Double;
var
  i, k: Integer;
begin
  if nbItems > 0 then
  begin
    k := 0;
    for i := 1 to nbItems - 1 do
      if values^[i] < values^[k] then
        k := i;
    result := values^[k];
  end
  else
    result := 0;
end;

function MinFloat(values: PExtendedArray; nbItems: Integer): Extended;
var
  i, k: Integer;
begin
  if nbItems > 0 then
  begin
    k := 0;
    for i := 1 to nbItems - 1 do
      if values^[i] < values^[k] then
        k := i;
    result := values^[k];
  end
  else
    result := 0;
end;

function MinFloat(const V: array of Single): Single;
var
  i: Integer;
begin
  if Length(V) > 0 then
  begin
    result := V[0];
    for i := 1 to High(V) do
      if V[i] < result then
        result := V[i];
  end
  else
    result := 0;
end;

function MinFloat(const V1, V2: Single): Single;
begin
  if V1 < V2 then
    result := V1
  else
    result := V2;
end;

function MinFloat(const V1, V2: Double): Double;
begin
  if V1 < V2 then
    result := V1
  else
    result := V2;
end;

function MinFloat(const V1, V2: Extended): Extended; overload;
begin
  if V1 < V2 then
    result := V1
  else
    result := V2;
end;

function MinFloat(const V1, V2, V3: Single): Single;
begin
  if V1 <= V2 then
    if V1 <= V3 then
      result := V1
    else if V3 <= V2 then
      result := V3
    else
      result := V2
  else if V2 <= V3 then
    result := V2
  else if V3 <= V1 then
    result := V3
  else
    result := V1;
end;

function MinFloat(const V1, V2, V3: Double): Double;
begin
  if V1 <= V2 then
    if V1 <= V3 then
      result := V1
    else if V3 <= V2 then
      result := V3
    else
      result := V2
  else if V2 <= V3 then
    result := V2
  else if V3 <= V1 then
    result := V3
  else
    result := V1;
end;


function MinFloat(const V1, V2, V3: Extended): Extended; overload;
begin
  if V1 <= V2 then
    if V1 <= V3 then
      result := V1
    else if V3 <= V2 then
      result := V3
    else
      result := V2
  else if V2 <= V3 then
    result := V2
  else if V3 <= V1 then
    result := V3
  else
    result := V1;
end;

function MaxFloat(values: PSingleArray; nbItems: Integer): Single; overload;
var
  i, k: Integer;
begin
  if nbItems > 0 then
  begin
    k := 0;
    for i := 1 to nbItems - 1 do
      if values^[i] > values^[k] then
        k := i;
    result := values^[k];
  end
  else
    result := 0;
end;

function MaxFloat(values: PDoubleArray; nbItems: Integer): Double; overload;
var
  i, k: Integer;
begin
  if nbItems > 0 then
  begin
    k := 0;
    for i := 1 to nbItems - 1 do
      if values^[i] > values^[k] then
        k := i;
    result := values^[k];
  end
  else
    result := 0;
end;

function MaxFloat(values: PExtendedArray; nbItems: Integer): Extended; overload;
var
  i, k: Integer;
begin
  if nbItems > 0 then
  begin
    k := 0;
    for i := 1 to nbItems - 1 do
      if values^[i] > values^[k] then
        k := i;
    result := values^[k];
  end
  else
    result := 0;
end;

function MaxFloat(const V: array of Single): Single;
var
  i: Integer;
begin
  if Length(V) > 0 then
  begin
    result := V[0];
    for i := 1 to High(V) do
      if V[i] > result then
        result := V[i];
  end
  else
    result := 0;
end;

function MaxFloat(const V1, V2: Single): Single;
begin
  if V1 > V2 then
    result := V1
  else
    result := V2;
end;

function MaxFloat(const V1, V2: Double): Double;
begin
  if V1 > V2 then
    result := V1
  else
    result := V2;
end;

function MaxFloat(const V1, V2: Extended): Extended; overload;
begin
  if V1 > V2 then
    result := V1
  else
    result := V2;
end;

function MaxFloat(const V1, V2, V3: Single): Single;
begin
  if V1 >= V2 then
    if V1 >= V3 then
      result := V1
    else if V3 >= V2 then
      result := V3
    else
      result := V2
  else if V2 >= V3 then
    result := V2
  else if V3 >= V1 then
    result := V3
  else
    result := V1;
end;

function MaxFloat(const V1, V2, V3: Double): Double;
begin
  if V1 >= V2 then
    if V1 >= V3 then
      result := V1
    else if V3 >= V2 then
      result := V3
    else
      result := V2
  else if V2 >= V3 then
    result := V2
  else if V3 >= V1 then
    result := V3
  else
    result := V1;
end;


function MaxFloat(const V1, V2, V3: Extended): Extended; overload;
begin
  if V1 >= V2 then
    if V1 >= V3 then
      result := V1
    else if V3 >= V2 then
      result := V3
    else
      result := V2
  else if V2 >= V3 then
    result := V2
  else if V3 >= V1 then
    result := V3
  else
    result := V1;
end;

function MinInteger(const V1, V2: Integer): Integer;
begin
  if V1 < V2 then
    result := V1
  else
    result := V2;
end;

function MinInteger(const V1, V2: Cardinal): Cardinal;
begin
  if V1 < V2 then
    result := V1
  else
    result := V2;
end;

function MinInteger(const V1, V2, V3: Integer): Integer;
begin
  if V1 <= V2 then
    if V1 <= V3 then
      result := V1
    else if V3 <= V2 then
      result := V3
    else
      result := V2
  else if V2 <= V3 then
    result := V2
  else if V3 <= V1 then
    result := V3
  else
    result := V1;
end;

function MinInteger(const V1, V2, V3: Cardinal): Cardinal;
begin
  if V1 <= V2 then
    if V1 <= V3 then
      result := V1
    else if V3 <= V2 then
      result := V3
    else
      result := V2
  else if V2 <= V3 then
    result := V2
  else if V3 <= V1 then
    result := V3
  else
    result := V1;
end;

function MaxInteger(const V1, V2: Integer): Integer;
begin
  if V1 > V2 then
    result := V1
  else
    result := V2;
end;

function MaxInteger(const V1, V2: Cardinal): Cardinal;
begin
  if V1 > V2 then
    result := V1
  else
    result := V2;
end;

function MaxInteger(const V1, V2, V3: Integer): Integer;
begin
  if V1 >= V2 then
    if V1 >= V3 then
      result := V1
    else if V3 >= V2 then
      result := V3
    else
      result := V2
  else if V2 >= V3 then
    result := V2
  else if V3 >= V1 then
    result := V3
  else
    result := V1;
end;

function MaxInteger(const V1, V2, V3: Cardinal): Cardinal;
begin
  if V1 >= V2 then
    if V1 >= V3 then
      result := V1
    else if V3 >= V2 then
      result := V3
    else
      result := V2
  else if V2 >= V3 then
    result := V2
  else if V3 >= V1 then
    result := V3
  else
    result := V1;
end;

function ClampInteger(const value, min, max: Integer): Integer;
begin
  result := MinInteger(MaxInteger(value, min), max);
end;

function ClampInteger(const value, min, max: Cardinal): Cardinal;
begin
  result := MinInteger(MaxInteger(value, min), max);
end;

function TriangleArea(const p1, p2, p3: TAffineVector): Single;
begin
  result := 0.5 * VectorLength(VectorCrossProduct(VectorSubtract(p2, p1),
    VectorSubtract(p3, p1)));
end;

function PolygonArea(const p: PAffineVectorArray; nSides: Integer): Single;
var
  r: TAffineVector;
  i: Integer;
  p1, p2, p3: PAffineVector;
begin
  result := 0;
  if nSides > 2 then
  begin
    RstVector(r);
    p1 := @p[0];
    p2 := @p[1];
    for i := 2 to nSides - 1 do
    begin
      p3 := @p[i];
      AddVector(r, VectorCrossProduct(VectorSubtract(p2^, p1^),
        VectorSubtract(p3^, p1^)));
      p2 := p3;
    end;
    result := VectorLength(r) * 0.5;
  end;
end;

function TriangleSignedArea(const p1, p2, p3: TAffineVector): Single;
begin
  result := 0.5 * ((p2.X - p1.X) * (p3.Y - p1.Y) -
    (p3.X - p1.X) * (p2.Y - p1.Y));
end;

function PolygonSignedArea(const p: PAffineVectorArray;
  nSides: Integer): Single;
var
  i: Integer;
  p1, p2, p3: PAffineVector;
begin
  result := 0;
  if nSides > 2 then
  begin
    p1 := @(p^[0]);
    p2 := @(p^[1]);
    for i := 2 to nSides - 1 do
    begin
      p3 := @(p^[i]);
      result := result + (p2^.X - p1^.X) * (p3^.Y - p1^.Y) -
        (p3^.X - p1^.X) * (p2^.Y - p1^.Y);
      p2 := p3;
    end;
    result := result * 0.5;
  end;
end;

procedure ScaleFloatArray(values: PSingleArray; nb: Integer;
  var factor: Single);
var
  i: Integer;
begin
  for i := 0 to nb - 1 do
    values^[i] := values^[i] * factor;
end;

procedure ScaleFloatArray(var values: TSingleArray; factor: Single);
begin
  if Length(values) > 0 then
    ScaleFloatArray(@values[0], Length(values), factor);
end;

procedure OffsetFloatArray(values: PSingleArray; nb: Integer;
  var delta: Single);
var
  i: Integer;
begin
  for i := 0 to nb - 1 do
    values^[i] := values^[i] + delta;
end;

procedure OffsetFloatArray(var values: array of Single; delta: Single);
begin
  if Length(values) > 0 then
    ScaleFloatArray(@values[0], Length(values), delta);
end;

procedure OffsetFloatArray(valuesDest, valuesDelta: PSingleArray; nb: Integer);
var
  i: Integer;
begin
  for i := 0 to nb - 1 do
    valuesDest^[i] := valuesDest^[i] + valuesDelta^[i];
end;

function MaxXYZComponent(const V: TGLVector): Single; overload;
begin
  result := MaxFloat(V.X, V.Y, V.Z);
end;

function MaxXYZComponent(const V: TAffineVector): Single; overload;
begin
  result := MaxFloat(V.X, V.Y, V.Z);
end;

function MinXYZComponent(const V: TGLVector): Single; overload;
begin
  if V.X <= V.Y then
    if V.X <= V.Z then
      result := V.X
    else if V.Z <= V.Y then
      result := V.Z
    else
      result := V.Y
  else if V.Y <= V.Z then
    result := V.Y
  else if V.Z <= V.X then
    result := V.Z
  else
    result := V.X;
end;

function MinXYZComponent(const V: TAffineVector): Single; overload;
begin
  result := MinFloat(V.X, V.Y, V.Z);
end;

function MaxAbsXYZComponent(V: TGLVector): Single;
begin
  AbsVector(V);
  result := MaxXYZComponent(V);
end;

function MinAbsXYZComponent(V: TGLVector): Single;
begin
  AbsVector(V);
  result := MinXYZComponent(V);
end;

procedure MaxVector(var V: TGLVector; const V1: TGLVector);
begin
  if V1.X > V.X then
    V.X := V1.X;
  if V1.Y > V.Y then
    V.Y := V1.Y;
  if V1.Z > V.Z then
    V.Z := V1.Z;
  if V1.W > V.W then
    V.W := V1.W;
end;

procedure MaxVector(var V: TAffineVector; const V1: TAffineVector); overload;
begin
  if V1.X > V.X then
    V.X := V1.X;
  if V1.Y > V.Y then
    V.Y := V1.Y;
  if V1.Z > V.Z then
    V.Z := V1.Z;
end;

procedure MinVector(var V: TGLVector; const V1: TGLVector);
begin
  if V1.X < V.X then
    V.X := V1.X;
  if V1.Y < V.Y then
    V.Y := V1.Y;
  if V1.Z < V.Z then
    V.Z := V1.Z;
  if V1.W < V.W then
    V.W := V1.W;
end;

procedure MinVector(var V: TAffineVector; const V1: TAffineVector);
begin
  if V1.X < V.X then
    V.X := V1.X;
  if V1.Y < V.Y then
    V.Y := V1.Y;
  if V1.Z < V.Z then
    V.Z := V1.Z;
end;

procedure SortArrayAscending(var a: array of Extended);
var
  i, J, M: Integer;
  buf: Extended;
begin
  for i := Low(a) to High(a) - 1 do
  begin
    M := i;
    for J := i + 1 to High(a) do
      if a[J] < a[M] then
        M := J;
    if M <> i then
    begin
      buf := a[M];
      a[M] := a[i];
      a[i] := buf;
    end;
  end;
end;

function ClampValue(const aValue, aMin, aMax: Single): Single;
begin
  if aValue < aMin then
    result := aMin
  else if aValue > aMax then
    result := aMax
  else
    result := aValue;
end;

function ClampValue(const aValue, aMin: Single): Single;
begin
  if aValue < aMin then
    result := aMin
  else
    result := aValue;
end;

function MakeAffineDblVector(var V: array of Double): TAffineDblVector;
begin
  result.X := V[0];
  result.Y := V[1];
  result.Z := V[2];
end;

function MakeDblVector(var V: array of Double): THomogeneousDblVector;
begin
  result.X := V[0];
  result.Y := V[1];
  result.Z := V[2];
  result.W := V[3];
end;

function PointInPolygon(const xp, yp: array of Single; X, Y: Single): Boolean;
var
  i, J: Integer;
begin
  result := False;
  if High(xp) = High(yp) then
  begin
    J := High(xp);
    for i := 0 to High(xp) do
    begin
      if ((((yp[i] <= Y) and (Y < yp[J])) or ((yp[J] <= Y) and (Y < yp[i]))) and
        (X < (xp[J] - xp[i]) * (Y - yp[i]) / (yp[J] - yp[i]) + xp[i])) then
        result := not result;
      J := i;
    end;
  end;
end;

function IsPointInPolygon(const Polygon: array of TPoint; const p: TPoint): Boolean;
var
  a: array of TPoint;
  n, i: Integer;
  inside: Boolean;
begin
  n := High(Polygon) + 1;
  SetLength(a, n + 2);
  a[0] := p;
  for i := 1 to n do
    a[i] := Polygon[i - 1];
  a[n + 1] := a[0];
  inside := True;

  for i := 1 to n do
  begin
    if (a[0].Y > a[i].Y) xor (a[0].Y <= a[i + 1].Y) then
      Continue;
    if (a[0].X - a[i].X) < ((a[0].Y - a[i].Y) * (a[i + 1].X - a[i].X) /
      (a[i + 1].Y - a[i].Y)) then
      inside := not inside;
  end;
  inside := not inside;
  result := inside;
end;

procedure DivMod(Dividend: Integer; Divisor: Word; var result, Remainder: Word);
begin
  result := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
end;

function ConvertRotation(const Angles: TAffineVector): TGLVector;

(*
  Rotation of the Angle t about the axis (X, Y, Z) is given by:
  | X^2 + (1-X^2) Cos(t),    XY(1-Cos(t))  +  Z Sin(t), XZ(1-Cos(t))-Y Sin(t) |
  M = | XY(1-Cos(t))-Z Sin(t), Y^2 + (1-Y^2) Cos(t),      YZ(1-Cos(t)) + X Sin(t) |
  | XZ(1-Cos(t)) + Y Sin(t), YZ(1-Cos(t))-X Sin(t),   Z^2 + (1-Z^2) Cos(t)    |

  Rotation about the three axes (Angles a1, a2, a3) can be represented as
  the product of the individual rotation matrices:
  | 1  0       0       | | Cos(a2) 0 -Sin(a2) | |  Cos(a3) Sin(a3) 0 |
  | 0  Cos(a1) Sin(a1) | * | 0       1  0       | * | -Sin(a3) Cos(a3) 0 |
  | 0 -Sin(a1) Cos(a1) | | Sin(a2) 0  Cos(a2) | |  0       0       1 |
  Mx                       My                     Mz

  We now want to solve for X, Y, Z, and t given 9 equations in 4 unknowns.
  Using the diagonal elements of the two matrices, we get:

  X^2 + (1-X^2) Cos(t) = M[0][0]
  Y^2 + (1-Y^2) Cos(t) = M[1][1]
  Z^2 + (1-Z^2) Cos(t) = M[2][2]

  Adding the three equations, we get:
  X^2  +  Y^2  +  Z^2 - (M[0][0]  +  M[1][1]  +  M[2][2]) =
  - (3 - X^2 - Y^2 - Z^2) Cos(t)

  Since (X^2  +  Y^2  +  Z^2) = 1, we can rewrite as:
  Cos(t) = (1 - (M[0][0]  +  M[1][1]  +  M[2][2])) / 2
  Solving for t, we get:

  t = Acos(((M[0][0]  +  M[1][1]  +  M[2][2]) - 1) / 2)

  We can substitute t into the equations for X^2, Y^2, and Z^2 above
  to get the values for X, Y, and Z.  To find the proper signs we note
  that:
  2 X Sin(t) = M[1][2] - M[2][1]
  2 Y Sin(t) = M[2][0] - M[0][2]
  2 Z Sin(t) = M[0][1] - M[1][0]
*)
var
  Axis1, Axis2: TVector3f;
  M, m1, m2: TGLMatrix;
  cost, cost1, sint, s1, s2, s3: Single;
  i: Integer;
begin
  // see if we are only rotating about a single Axis
  if Abs(Angles.X) < EPSILON then
  begin
    if Abs(Angles.Y) < EPSILON then
    begin
      SetVector(result, 0, 0, 1, Angles.Z);
      Exit;
    end
    else if Abs(Angles.Z) < EPSILON then
    begin
      SetVector(result, 0, 1, 0, Angles.Y);
      Exit;
    end
  end
  else if (Abs(Angles.Y) < EPSILON) and (Abs(Angles.Z) < EPSILON) then
  begin
    SetVector(result, 1, 0, 0, Angles.X);
    Exit;
  end;

  // make the rotation matrix
  Axis1 := XVector;
  M := CreateRotationMatrix(Axis1, Angles.X);

  Axis2 := YVector;
  m2 := CreateRotationMatrix(Axis2, Angles.Y);
  m1 := MatrixMultiply(M, m2);

  Axis2 := ZVector;
  m2 := CreateRotationMatrix(Axis2, Angles.Z);
  M := MatrixMultiply(m1, m2);

  cost := ((M.X.X + M.Y.Y + M.Z.Z) - 1) / 2;
  if cost < -1 then
    cost := -1
  else if cost > 1 - EPSILON then
  begin
    // Bad Angle - this would cause a crash
    SetVector(result, XHmgVector);
    Exit;
  end;

  cost1 := 1 - cost;
  SetVector(result, Sqrt((M.X.X - cost) / cost1), Sqrt((M.Y.Y - cost) / cost1),
    Sqrt((M.Z.Z - cost) / cost1), ArcCosine(cost));

  sint := 2 * Sqrt(1 - cost * cost); // This is actually 2 Sin(t)

  // Determine the proper signs
  for i := 0 to 7 do
  begin
    if (i and 1) > 1 then
      s1 := -1
    else
      s1 := 1;
    if (i and 2) > 1 then
      s2 := -1
    else
      s2 := 1;
    if (i and 4) > 1 then
      s3 := -1
    else
      s3 := 1;
    if (Abs(s1 * result.V[X] * sint - M.Y.Z + M.Z.Y) < EPSILON2) and
      (Abs(s2 * result.V[Y] * sint - M.Z.X + M.X.Z) < EPSILON2) and
      (Abs(s3 * result.V[Z] * sint - M.X.Y + M.Y.X) < EPSILON2) then
    begin
      // We found the right combination of signs
      result.V[X] := result.V[X] * s1;
      result.V[Y] := result.V[Y] * s2;
      result.V[Z] := result.V[Z] * s3;
      Exit;
    end;
  end;
end;

function QuaternionSlerp(const QStart, QEnd: TQuaternion; Spin: Integer;
  T: Single): TQuaternion;
var
  beta, // complementary interp parameter
  Theta, // Angle between A and B
  sint, cost, // sine, cosine of theta
  phi: Single; // theta plus spins
  bflip: Boolean; // use negativ t?
begin
  // cosine theta
  cost := VectorAngleCosine(QStart.ImagPart, QEnd.ImagPart);

  // if QEnd is on opposite hemisphere from QStart, use -QEnd instead
  if cost < 0 then
  begin
    cost := -cost;
    bflip := True;
  end
  else
    bflip := False;

  // if QEnd is (within precision limits) the same as QStart,
  // just linear interpolate between QStart and QEnd.
  // Can't do spins, since we don't know what direction to spin.

  if (1 - cost) < EPSILON then
    beta := 1 - T
  else
  begin
    // normal case
    Theta := ArcCosine(cost);
    phi := Theta + Spin * PI;
    sint := Sin(Theta);
    beta := Sin(Theta - T * phi) / sint;
    T := Sin(T * phi) / sint;
  end;

  if bflip then
    T := -T;

  // interpolate
  result.ImagPart.V[X] := beta * QStart.ImagPart.V[X] + T * QEnd.ImagPart.V[X];
  result.ImagPart.V[Y] := beta * QStart.ImagPart.V[Y] + T * QEnd.ImagPart.V[Y];
  result.ImagPart.V[Z] := beta * QStart.ImagPart.V[Z] + T * QEnd.ImagPart.V[Z];
  result.RealPart := beta * QStart.RealPart + T * QEnd.RealPart;
end;

function QuaternionSlerp(const source, dest: TQuaternion; const T: Single)
  : TQuaternion;
var
  to1: array [0 .. 4] of Single;
  omega, cosom, sinom, scale0, scale1: Extended;
  // t goes from 0 to 1
  // absolute rotations
begin
  // calc cosine
  cosom := source.ImagPart.X * dest.ImagPart.X + source.ImagPart.Y *
    dest.ImagPart.Y + source.ImagPart.Z * dest.ImagPart.Z +
    source.RealPart * dest.RealPart;
  // adjust signs (if necessary)
  if cosom < 0 then
  begin
    cosom := -cosom;
    to1[0] := -dest.ImagPart.X;
    to1[1] := -dest.ImagPart.Y;
    to1[2] := -dest.ImagPart.Z;
    to1[3] := -dest.RealPart;
  end
  else
  begin
    to1[0] := dest.ImagPart.X;
    to1[1] := dest.ImagPart.Y;
    to1[2] := dest.ImagPart.Z;
    to1[3] := dest.RealPart;
  end;
  // calculate coefficients
  if ((1.0 - cosom) > EPSILON2) then
  begin // standard case (slerp)
    omega := ArcCosine(cosom);
    sinom := 1 / Sin(omega);
    scale0 := Sin((1.0 - T) * omega) * sinom;
    scale1 := Sin(T * omega) * sinom;
  end
  else
  begin // "from" and "to" quaternions are very close
    // ... so we can do a linear interpolation
    scale0 := 1.0 - T;
    scale1 := T;
  end;
  // calculate final values
  result.ImagPart.X := scale0 * source.ImagPart.X + scale1 * to1[0];
  result.ImagPart.Y := scale0 * source.ImagPart.Y + scale1 * to1[1];
  result.ImagPart.Z := scale0 * source.ImagPart.Z + scale1 * to1[2];
  result.RealPart := scale0 * source.RealPart + scale1 * to1[3];
  NormalizeQuaternion(result);
end;

function VectorDblToFlt(const V: THomogeneousDblVector): THomogeneousVector;
begin
{$HINTS OFF}
  result.X := V.X;
  result.Y := V.Y;
  result.Z := V.Z;
  result.W := V.W;
{$HINTS ON}
end;

function VectorAffineDblToFlt(const V: TAffineDblVector): TAffineVector;
begin
{$HINTS OFF}
  result.X := V.X;
  result.Y := V.Y;
  result.Z := V.Z;
{$HINTS ON}
end;

function VectorAffineFltToDbl(const V: TAffineVector): TAffineDblVector;
begin
  result.X := V.X;
  result.Y := V.Y;
  result.Z := V.Z;
end;

function VectorFltToDbl(const V: TGLVector): THomogeneousDblVector;
begin
  result.X := V.X;
  result.Y := V.Y;
  result.Z := V.Z;
  result.W := V.W;
end;

// ----------------- coordinate system manipulation functions -----------------------------------------------------------

function Turn(const Matrix: TGLMatrix; angle: Single): TGLMatrix;
begin
  result := MatrixMultiply(Matrix,
    CreateRotationMatrix(AffineVectorMake(Matrix.Y.X, Matrix.Y.Y,
    Matrix.Y.Z), angle));
end;

function Turn(const Matrix: TGLMatrix; const MasterUp: TAffineVector;
  angle: Single): TGLMatrix;
begin
  result := MatrixMultiply(Matrix, CreateRotationMatrix(MasterUp, angle));
end;

function Pitch(const Matrix: TGLMatrix; angle: Single): TGLMatrix;
begin
  result := MatrixMultiply(Matrix,
    CreateRotationMatrix(AffineVectorMake(Matrix.X.X, Matrix.X.Y,
    Matrix.X.Z), angle));
end;

function Pitch(const Matrix: TGLMatrix; const MasterRight: TAffineVector;
  angle: Single): TGLMatrix; overload;
begin
  result := MatrixMultiply(Matrix, CreateRotationMatrix(MasterRight, angle));
end;

function Roll(const Matrix: TGLMatrix; angle: Single): TGLMatrix;
begin
  result := MatrixMultiply(Matrix,
    CreateRotationMatrix(AffineVectorMake(Matrix.Z.X, Matrix.Z.Y,
    Matrix.Z.Z), angle));
end;

function Roll(const Matrix: TGLMatrix; const MasterDirection: TAffineVector;
  angle: Single): TGLMatrix; overload;
begin
  result := MatrixMultiply(Matrix,
    CreateRotationMatrix(MasterDirection, angle));
end;

function RayCastPlaneIntersect(const rayStart, rayVector: TGLVector;
  const planePoint, planeNormal: TGLVector;
  intersectPoint: PGLVector = nil): Boolean;
var
  sp: TGLVector;
  T, d: Single;
begin
  d := VectorDotProduct(rayVector, planeNormal);
  result := ((d > EPSILON2) or (d < -EPSILON2));
  if result and Assigned(intersectPoint) then
  begin
    VectorSubtract(planePoint, rayStart, sp);
    d := 1 / d; // will keep one FPU unit busy during dot product calculation
    T := VectorDotProduct(sp, planeNormal) * d;
    if T > 0 then
      VectorCombine(rayStart, rayVector, T, intersectPoint^)
    else
      result := False;
  end;
end;

function RayCastPlaneXZIntersect(const rayStart, rayVector: TGLVector;
  const planeY: Single; intersectPoint: PGLVector = nil): Boolean;
var
  T: Single;
begin
  if rayVector.Y = 0 then
    result := False
  else
  begin
    T := (rayStart.Y - planeY) / rayVector.Y;
    if T < 0 then
    begin
      if Assigned(intersectPoint) then
        VectorCombine(rayStart, rayVector, T, intersectPoint^);
      result := True;
    end
    else
      result := False;
  end;
end;

function RayCastTriangleIntersect(const rayStart, rayVector: TGLVector;
  const p1, p2, p3: TAffineVector; intersectPoint: PGLVector = nil;
  intersectNormal: PGLVector = nil): Boolean;
var
  pvec: TAffineVector;
  V1, V2, qvec, tvec: TGLVector;
  T, u, V, det, invDet: Single;
begin
  VectorSubtract(p2, p1, V1);
  VectorSubtract(p3, p1, V2);
  VectorCrossProduct(rayVector, V2, pvec);
  det := VectorDotProduct(V1, pvec);
  if ((det < EPSILON2) and (det > -EPSILON2)) then
  begin // vector is parallel to triangle's plane
    result := False;
    Exit;
  end;
  invDet := cOne / det;
  VectorSubtract(rayStart, p1, tvec);
  u := VectorDotProduct(tvec, pvec) * invDet;
  if (u < 0) or (u > 1) then
    result := False
  else
  begin
    qvec := VectorCrossProduct(tvec, V1);
    V := VectorDotProduct(rayVector, qvec) * invDet;
    result := (V >= 0) and (u + V <= 1);
    if result then
    begin
      T := VectorDotProduct(V2, qvec) * invDet;
      if T > 0 then
      begin
        if intersectPoint <> nil then
          VectorCombine(rayStart, rayVector, T, intersectPoint^);
        if intersectNormal <> nil then
          VectorCrossProduct(V1, V2, intersectNormal^);
      end
      else
        result := False;
    end;
  end;
end;

function RayCastMinDistToPoint(const rayStart, rayVector: TGLVector;
  const point: TGLVector): Single;
var
  proj: Single;
begin
  proj := PointProject(point, rayStart, rayVector);
  if proj <= 0 then
    proj := 0; // rays don't go backward!
  result := VectorDistance(point, VectorCombine(rayStart, rayVector, 1, proj));
end;

function RayCastIntersectsSphere(const rayStart, rayVector: TGLVector;
  const sphereCenter: TGLVector; const SphereRadius: Single): Boolean;
var
  proj: Single;
begin
  proj := PointProject(sphereCenter, rayStart, rayVector);
  if proj <= 0 then
    proj := 0; // rays don't go backward!
  result := (VectorDistance2(sphereCenter, VectorCombine(rayStart, rayVector, 1,
    proj)) <= Sqr(SphereRadius));
end;

function RayCastSphereIntersect(const rayStart, rayVector: TGLVector;
  const sphereCenter: TGLVector; const SphereRadius: Single;
  var i1, i2: TGLVector): Integer;
var
  proj, d2: Single;
  id2: Integer;
  projPoint: TGLVector;
begin
  proj := PointProject(sphereCenter, rayStart, rayVector);
  VectorCombine(rayStart, rayVector, proj, projPoint);
  d2 := SphereRadius * SphereRadius - VectorDistance2(sphereCenter, projPoint);
  id2 := PInteger(@d2)^;
  if id2 >= 0 then
  begin
    if id2 = 0 then
    begin
      if PInteger(@proj)^ > 0 then
      begin
        VectorCombine(rayStart, rayVector, proj, i1);
        result := 1;
        Exit;
      end;
    end
    else if id2 > 0 then
    begin
      d2 := Sqrt(d2);
      if proj >= d2 then
      begin
        VectorCombine(rayStart, rayVector, proj - d2, i1);
        VectorCombine(rayStart, rayVector, proj + d2, i2);
        result := 2;
        Exit;
      end
      else if proj + d2 >= 0 then
      begin
        VectorCombine(rayStart, rayVector, proj + d2, i1);
        result := 1;
        Exit;
      end;
    end;
  end;
  result := 0;
end;

function RayCastBoxIntersect(const rayStart, rayVector, aMinExtent,
  aMaxExtent: TAffineVector; intersectPoint: PAffineVector = nil): Boolean;
var
  i, planeInd: Integer;
  ResAFV, MaxDist, plane: TAffineVector;
  isMiddle: array [0 .. 2] of Boolean;
begin
  // Find plane.
  result := True;
  for i := 0 to 2 do
    if rayStart.V[i] < aMinExtent.V[i] then
    begin
      plane.V[i] := aMinExtent.V[i];
      isMiddle[i] := False;
      result := False;
    end
    else if rayStart.V[i] > aMaxExtent.V[i] then
    begin
      plane.V[i] := aMaxExtent.V[i];
      isMiddle[i] := False;
      result := False;
    end
    else
    begin
      isMiddle[i] := True;
    end;
  if result then
  begin
    // rayStart inside box.
    if intersectPoint <> nil then
      intersectPoint^ := rayStart;
  end
  else
  begin
    // Distance to plane.
    planeInd := 0;
    for i := 0 to 2 do
      if isMiddle[i] or (rayVector.V[i] = 0) then
        MaxDist.V[i] := -1
      else
      begin
        MaxDist.V[i] := (plane.V[i] - rayStart.V[i]) / rayVector.V[i];
        if MaxDist.V[i] > 0 then
        begin
          if MaxDist.V[planeInd] < MaxDist.V[i] then
            planeInd := i;
          result := True;
        end;
      end;
    // Inside box ?
    if result then
    begin
      for i := 0 to 2 do
        if planeInd = i then
          ResAFV.V[i] := plane.V[i]
        else
        begin
          ResAFV.V[i] := rayStart.V[i] + MaxDist.V[planeInd] * rayVector.V[i];
          result := (ResAFV.V[i] >= aMinExtent.V[i]) and
            (ResAFV.V[i] <= aMaxExtent.V[i]);
          if not result then
            Exit;
        end;
      if intersectPoint <> nil then
        intersectPoint^ := ResAFV;
    end;
  end;
end;

function SphereVisibleRadius(distance, radius: Single): Single;
var
  d2, r2, ir, tr: Single;
begin
  d2 := distance * distance;
  r2 := radius * radius;
  ir := Sqrt(d2 - r2);
  tr := (d2 + r2 - Sqr(ir)) / (2 * ir);

  result := Sqrt(r2 + Sqr(tr));
end;

function IntersectLinePlane(const point, direction: TGLVector;
  const plane: THmgPlane; intersectPoint: PGLVector = nil): Integer;
var
  a, b: Extended;
  T: Single;
begin
  a := VectorDotProduct(plane, direction);
  // direction projected to plane normal
  b := PlaneEvaluatePoint(plane, point); // distance to plane
  if a = 0 then
  begin // direction is parallel to plane
    if b = 0 then
      result := -1 // line is inside plane
    else
      result := 0; // line is outside plane
  end
  else
  begin
    if Assigned(intersectPoint) then
    begin
      T := -b / a; // parameter of intersection
      intersectPoint^ := point;
      // calculate intersection = p + t*d
      CombineVector(intersectPoint^, direction, T);
    end;
    result := 1;
  end;
end;

function IntersectTriangleBox(const p1, p2, p3, aMinExtent,
  aMaxExtent: TAffineVector): Boolean;
var
  RayDir, iPoint: TAffineVector;
  BoxDiagPt, BoxDiagPt2, BoxDiagDir, iPnt: TGLVector;
begin
  // Triangle edge (p2, p1) - Box intersection
  VectorSubtract(p2, p1, RayDir);
  result := RayCastBoxIntersect(p1, RayDir, aMinExtent, aMaxExtent, @iPoint);
  if result then
    result := VectorNorm(VectorSubtract(p1, iPoint)) <
      VectorNorm(VectorSubtract(p1, p2));
  if result then
    Exit;

  // Triangle edge (p3, p1) - Box intersection
  VectorSubtract(p3, p1, RayDir);
  result := RayCastBoxIntersect(p1, RayDir, aMinExtent, aMaxExtent, @iPoint);
  if result then
    result := VectorNorm(VectorSubtract(p1, iPoint)) <
      VectorNorm(VectorSubtract(p1, p3));
  if result then
    Exit;

  // Triangle edge (p2, p3) - Box intersection
  VectorSubtract(p2, p3, RayDir);
  result := RayCastBoxIntersect(p3, RayDir, aMinExtent, aMaxExtent, @iPoint);
  if result then
    result := VectorNorm(VectorSubtract(p3, iPoint)) <
      VectorNorm(VectorSubtract(p3, p2));
  if result then
    Exit;

  // Triangle - Box diagonal 1 intersection
  BoxDiagPt := VectorMake(aMinExtent);
  VectorSubtract(aMaxExtent, aMinExtent, BoxDiagDir);
  result := RayCastTriangleIntersect(BoxDiagPt, BoxDiagDir, p1, p2, p3, @iPnt);
  if result then
    result := VectorNorm(VectorSubtract(BoxDiagPt, iPnt)) <
      VectorNorm(VectorSubtract(aMaxExtent, aMinExtent));
  if result then
    Exit;

  // Triangle - Box diagonal 2 intersection
  BoxDiagPt := VectorMake(aMinExtent.X, aMinExtent.Y, aMaxExtent.Z);
  BoxDiagPt2 := VectorMake(aMaxExtent.X, aMaxExtent.Y, aMinExtent.Z);
  VectorSubtract(BoxDiagPt2, BoxDiagPt, BoxDiagDir);
  result := RayCastTriangleIntersect(BoxDiagPt, BoxDiagDir, p1, p2, p3, @iPnt);
  if result then
    result := VectorNorm(VectorSubtract(BoxDiagPt, iPnt)) <
      VectorNorm(VectorSubtract(BoxDiagPt, BoxDiagPt2));
  if result then
    Exit;

  // Triangle - Box diagonal 3 intersection
  BoxDiagPt := VectorMake(aMinExtent.X, aMaxExtent.Y, aMinExtent.Z);
  BoxDiagPt2 := VectorMake(aMaxExtent.X, aMinExtent.Y, aMaxExtent.Z);
  VectorSubtract(BoxDiagPt, BoxDiagPt, BoxDiagDir);
  result := RayCastTriangleIntersect(BoxDiagPt, BoxDiagDir, p1, p2, p3, @iPnt);
  if result then
    result := VectorLength(VectorSubtract(BoxDiagPt, iPnt)) <
      VectorLength(VectorSubtract(BoxDiagPt, BoxDiagPt));
  if result then
    Exit;

  // Triangle - Box diagonal 4 intersection
  BoxDiagPt := VectorMake(aMaxExtent.X, aMinExtent.Y, aMinExtent.Z);
  BoxDiagPt2 := VectorMake(aMinExtent.X, aMaxExtent.Y, aMaxExtent.Z);
  VectorSubtract(BoxDiagPt, BoxDiagPt, BoxDiagDir);
  result := RayCastTriangleIntersect(BoxDiagPt, BoxDiagDir, p1, p2, p3, @iPnt);
  if result then
    result := VectorLength(VectorSubtract(BoxDiagPt, iPnt)) <
      VectorLength(VectorSubtract(BoxDiagPt, BoxDiagPt));
end;

function IntersectSphereBox(const SpherePos: TGLVector;
  const SphereRadius: Single; const BoxMatrix: TGLMatrix;
  // Up Direction and Right must be normalized!
  // Use CubDepht, CubeHeight and CubeWidth
  // for scale TGLCube.
  const BoxScale: TAffineVector; intersectPoint: PAffineVector = nil;
  normal: PAffineVector = nil; depth: PSingle = nil): Boolean;

  function dDOTByColumn(const V: TAffineVector; const M: TGLMatrix;
    const aColumn: Integer): Single;
  begin
    result := V.X * M.X.V[aColumn] + V.Y * M.Y.V[aColumn] + V.Z *
      M.Z.V[aColumn];
  end;

  function dDotByRow(const V: TAffineVector; const M: TGLMatrix;
    const aRow: Integer): Single;
  begin
    // Equal with: Result := VectorDotProduct(v, AffineVectorMake(m[aRow]));
    result := V.X * M.V[aRow].X + V.Y * M.V[aRow].Y + V.Z *
      M.V[aRow].Z;
  end;

  function dDotMatrByColumn(const V: TAffineVector; const M: TGLMatrix)
    : TAffineVector;
  begin
    result.X := dDOTByColumn(V, M, 0);
    result.Y := dDOTByColumn(V, M, 1);
    result.Z := dDOTByColumn(V, M, 2);
  end;

  function dDotMatrByRow(const V: TAffineVector; const M: TGLMatrix)
    : TAffineVector;
  begin
    result.X := dDotByRow(V, M, 0);
    result.Y := dDotByRow(V, M, 1);
    result.Z := dDotByRow(V, M, 2);
  end;

var
  tmp, l, T, p, Q, r: TAffineVector;
  FaceDistance, MinDistance, Depth1: Single;
  mini, i: Integer;
  isSphereCenterInsideBox: Boolean;
begin
  // this is easy. get the sphere center `p' relative to the box, and then clip
  // that to the boundary of the box (call that point `q'). if q is on the
  // boundary of the box and |p-q| is <= sphere radius, they touch.
  // if q is inside the box, the sphere is inside the box, so set a contact
  // normal to push the sphere to the closest box face.

  p.X := SpherePos.X - BoxMatrix.W.X;
  p.Y := SpherePos.Y - BoxMatrix.W.Y;
  p.Z := SpherePos.Z - BoxMatrix.W.Z;

  isSphereCenterInsideBox := True;
  for i := 0 to 2 do
  begin
    l.V[i] := 0.5 * BoxScale.V[i];
    T.V[i] := dDotByRow(p, BoxMatrix, i);
    if T.V[i] < -l.V[i] then
    begin
      T.V[i] := -l.V[i];
      isSphereCenterInsideBox := False;
    end
    else if T.V[i] > l.V[i] then
    begin
      T.V[i] := l.V[i];
      isSphereCenterInsideBox := False;
    end;
  end;

  if isSphereCenterInsideBox then
  begin

    MinDistance := l.X - Abs(T.X);
    mini := 0;
    for i := 1 to 2 do
    begin
      FaceDistance := l.V[i] - Abs(T.V[i]);
      if FaceDistance < MinDistance then
      begin
        MinDistance := FaceDistance;
        mini := i;
      end;
    end;

    if intersectPoint <> nil then
      intersectPoint^ := AffineVectorMake(SpherePos);

    if normal <> nil then
    begin
      tmp := NullVector;
      if T.V[mini] > 0 then
        tmp.V[mini] := 1
      else
        tmp.V[mini] := -1;
      normal^ := dDotMatrByRow(tmp, BoxMatrix);
    end;

    if depth <> nil then
      depth^ := MinDistance + SphereRadius;

    result := True;
  end
  else
  begin
    Q := dDotMatrByColumn(T, BoxMatrix);
    r := VectorSubtract(p, Q);
    Depth1 := SphereRadius - VectorLength(r);
    if Depth1 < 0 then
    begin
      result := False;
    end
    else
    begin
      if intersectPoint <> nil then
        intersectPoint^ := VectorAdd(Q, AffineVectorMake(BoxMatrix.W));
      if normal <> nil then
      begin
        normal^ := VectorNormalize(r);
      end;
      if depth <> nil then
        depth^ := Depth1;
      result := True;
    end;
  end;
end;

function ExtractFrustumFromModelViewProjection(const modelViewProj: TGLMatrix)
  : TFrustum;
begin
  with result do
  begin
    // extract left plane
    pLeft.X := modelViewProj.X.W + modelViewProj.X.X;
    pLeft.Y := modelViewProj.Y.W + modelViewProj.Y.X;
    pLeft.Z := modelViewProj.Z.W + modelViewProj.Z.X;
    pLeft.W := modelViewProj.W.W + modelViewProj.W.X;
    NormalizePlane(pLeft);
    // extract top plane
    pTop.X := modelViewProj.X.W - modelViewProj.X.Y;
    pTop.Y := modelViewProj.Y.W - modelViewProj.Y.Y;
    pTop.Z := modelViewProj.Z.W - modelViewProj.Z.Y;
    pTop.W := modelViewProj.W.W - modelViewProj.W.Y;
    NormalizePlane(pTop);
    // extract right plane
    pRight.X := modelViewProj.X.W - modelViewProj.X.X;
    pRight.Y := modelViewProj.Y.W - modelViewProj.Y.X;
    pRight.Z := modelViewProj.Z.W - modelViewProj.Z.X;
    pRight.W := modelViewProj.W.W - modelViewProj.W.X;
    NormalizePlane(pRight);
    // extract bottom plane
    pBottom.X := modelViewProj.X.W + modelViewProj.X.Y;
    pBottom.Y := modelViewProj.Y.W + modelViewProj.Y.Y;
    pBottom.Z := modelViewProj.Z.W + modelViewProj.Z.Y;
    pBottom.W := modelViewProj.W.W + modelViewProj.W.Y;
    NormalizePlane(pBottom);
    // extract far plane
    pFar.X := modelViewProj.X.W - modelViewProj.X.Z;
    pFar.Y := modelViewProj.Y.W - modelViewProj.Y.Z;
    pFar.Z := modelViewProj.Z.W - modelViewProj.Z.Z;
    pFar.W := modelViewProj.W.W - modelViewProj.W.Z;
    NormalizePlane(pFar);
    // extract near plane
    pNear.X := modelViewProj.X.W + modelViewProj.X.Z;
    pNear.Y := modelViewProj.Y.W + modelViewProj.Y.Z;
    pNear.Z := modelViewProj.Z.W + modelViewProj.Z.Z;
    pNear.W := modelViewProj.W.W + modelViewProj.W.Z;
    NormalizePlane(pNear);
  end;
end;

function IsVolumeClipped(const objPos: TAffineVector; const objRadius: Single;
  const Frustum: TFrustum): Boolean;
var
  negRadius: Single;
begin
  negRadius := -objRadius;
  result := (PlaneEvaluatePoint(Frustum.pLeft, objPos) < negRadius) or
    (PlaneEvaluatePoint(Frustum.pTop, objPos) < negRadius) or
    (PlaneEvaluatePoint(Frustum.pRight, objPos) < negRadius) or
    (PlaneEvaluatePoint(Frustum.pBottom, objPos) < negRadius) or
    (PlaneEvaluatePoint(Frustum.pNear, objPos) < negRadius) or
    (PlaneEvaluatePoint(Frustum.pFar, objPos) < negRadius);
end;

function IsVolumeClipped(const objPos: TGLVector; const objRadius: Single;
  const Frustum: TFrustum): Boolean;
begin
  result := IsVolumeClipped(PAffineVector(@objPos)^, objRadius, Frustum);
end;

function IsVolumeClipped(const min, max: TAffineVector;
  const Frustum: TFrustum): Boolean;
begin
  // change box to sphere
  result := IsVolumeClipped(VectorScale(VectorAdd(min, max), 0.5),
    VectorDistance(min, max) * 0.5, Frustum);
end;

function MakeParallelProjectionMatrix(const plane: THmgPlane;
  const dir: TGLVector): TGLMatrix;
// Based on material from a course by William D. Shoaff (www.cs.fit.edu)
var
  dot, invDot: Single;
begin
  dot := plane.X * dir.X + plane.Y * dir.Y + plane.Z * dir.Z;
  if Abs(dot) < 1E-5 then
  begin
    result := IdentityHmgMatrix;
    Exit;
  end;
  invDot := 1 / dot;

  result.X.X := (plane.Y * dir.Y + plane.Z * dir.Z) * invDot;
  result.Y.X := (-plane.Y * dir.X) * invDot;
  result.Z.X := (-plane.Z * dir.X) * invDot;
  result.W.X := (-plane.W * dir.X) * invDot;

  result.X.Y := (-plane.X * dir.Y) * invDot;
  result.Y.Y := (plane.X * dir.X + plane.Z * dir.Z) * invDot;
  result.Z.Y := (-plane.Z * dir.Y) * invDot;
  result.W.Y := (-plane.W * dir.Y) * invDot;

  result.X.Z := (-plane.X * dir.Z) * invDot;
  result.Y.Z := (-plane.Y * dir.Z) * invDot;
  result.Z.Z := (plane.X * dir.X + plane.Y * dir.Y) * invDot;
  result.W.Z := (-plane.W * dir.Z) * invDot;

  result.X.W := 0;
  result.Y.W := 0;
  result.Z.W := 0;
  result.W.W := 1;
end;

function MakeShadowMatrix(const planePoint, planeNormal,
  lightPos: TGLVector): TGLMatrix;
var
  planeNormal3, dot: Single;
begin
  // Find the last coefficient by back substitutions
  planeNormal3 := -(planeNormal.X * planePoint.X + planeNormal.Y *
    planePoint.Y + planeNormal.Z * planePoint.Z);
  // Dot product of plane and light position
  dot := planeNormal.X * lightPos.X + planeNormal.Y * lightPos.Y +
    planeNormal.Z * lightPos.Z + planeNormal3 * lightPos.W;
  // Now do the projection
  // First column
  result.X.X := dot - lightPos.X * planeNormal.X;
  result.Y.X := -lightPos.X * planeNormal.Y;
  result.Z.X := -lightPos.X * planeNormal.Z;
  result.W.X := -lightPos.X * planeNormal3;
  // Second column
  result.X.Y := -lightPos.Y * planeNormal.X;
  result.Y.Y := dot - lightPos.Y * planeNormal.Y;
  result.Z.Y := -lightPos.Y * planeNormal.Z;
  result.W.Y := -lightPos.Y * planeNormal3;
  // Third Column
  result.X.Z := -lightPos.Z * planeNormal.X;
  result.Y.Z := -lightPos.Z * planeNormal.Y;
  result.Z.Z := dot - lightPos.Z * planeNormal.Z;
  result.W.Z := -lightPos.Z * planeNormal3;
  // Fourth Column
  result.X.W := -lightPos.W * planeNormal.X;
  result.Y.W := -lightPos.W * planeNormal.Y;
  result.Z.W := -lightPos.W * planeNormal.Z;
  result.W.W := dot - lightPos.W * planeNormal3;
end;

function MakeReflectionMatrix(const planePoint, planeNormal
  : TAffineVector): TGLMatrix;
var
  pv2: Single;
begin
  // Precalcs
  pv2 := 2 * VectorDotProduct(planePoint, planeNormal);
  // 1st column
  result.X.X := 1 - 2 * Sqr(planeNormal.X);
  result.X.Y := -2 * planeNormal.X * planeNormal.Y;
  result.X.Z := -2 * planeNormal.X * planeNormal.Z;
  result.X.W := 0;
  // 2nd column
  result.Y.X := -2 * planeNormal.Y * planeNormal.X;
  result.Y.Y := 1 - 2 * Sqr(planeNormal.Y);
  result.Y.Z := -2 * planeNormal.Y * planeNormal.Z;
  result.Y.W := 0;
  // 3rd column
  result.Z.X := -2 * planeNormal.Z * planeNormal.X;
  result.Z.Y := -2 * planeNormal.Z * planeNormal.Y;
  result.Z.Z := 1 - 2 * Sqr(planeNormal.Z);
  result.Z.W := 0;
  // 4th column
  result.W.X := pv2 * planeNormal.X;
  result.W.Y := pv2 * planeNormal.Y;
  result.W.Z := pv2 * planeNormal.Z;
  result.W.W := 1;
end;

function PackRotationMatrix(const mat: TGLMatrix): TPackedRotationMatrix;
var
  Q: TQuaternion;
const
  cFact: Single = 32767;
begin
  Q := QuaternionFromMatrix(mat);
  NormalizeQuaternion(Q);
{$HINTS OFF}
  if Q.RealPart < 0 then
  begin
    result[0] := Round(-Q.ImagPart.X * cFact);
    result[1] := Round(-Q.ImagPart.Y * cFact);
    result[2] := Round(-Q.ImagPart.Z * cFact);
  end
  else
  begin
    result[0] := Round(Q.ImagPart.X * cFact);
    result[1] := Round(Q.ImagPart.Y * cFact);
    result[2] := Round(Q.ImagPart.Z * cFact);
  end;
{$HINTS ON}
end;

function UnPackRotationMatrix(const packedMatrix
  : TPackedRotationMatrix): TGLMatrix;
var
  Q: TQuaternion;
const
  cFact: Single = 1 / 32767;
begin
  Q.ImagPart.X := packedMatrix[0] * cFact;
  Q.ImagPart.Y := packedMatrix[1] * cFact;
  Q.ImagPart.Z := packedMatrix[2] * cFact;
  Q.RealPart := 1 - VectorNorm(Q.ImagPart);
  if Q.RealPart < 0 then
    Q.RealPart := 0
  else
    Q.RealPart := Sqrt(Q.RealPart);
  result := QuaternionToMatrix(Q);
end;

//**********************************************************************

function Vector2fMake(const X, Y: Single): TVector2f;
begin
  result.X := X;
  result.Y := Y;
end;

function Vector2iMake(const X, Y: Longint): TVector2i;
begin
  result.X := X;
  result.Y := Y;
end;

function Vector2sMake(const X, Y: SmallInt): TVector2s;
begin
  result.X := X;
  result.Y := Y;
end;

function Vector2dMake(const X, Y: Double): TVector2d;
begin
  result.X := X;
  result.Y := Y;
end;

function Vector2bMake(const X, Y: Byte): TVector2b;
begin
  result.X := X;
  result.Y := Y;
end;

//**********************************************************

function Vector2fMake(const Vector: TVector3f): TVector2f;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
end;

function Vector2iMake(const Vector: TVector3i): TVector2i;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
end;

function Vector2sMake(const Vector: TVector3s): TVector2s;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
end;

function Vector2dMake(const Vector: TVector3d): TVector2d;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
end;

function Vector2bMake(const Vector: TVector3b): TVector2b;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
end;

//*******************************************************

function Vector2fMake(const Vector: TVector4f): TVector2f;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
end;

function Vector2iMake(const Vector: TVector4i): TVector2i;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
end;

function Vector2sMake(const Vector: TVector4s): TVector2s;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
end;

function Vector2dMake(const Vector: TVector4d): TVector2d;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
end;

function Vector2bMake(const Vector: TVector4b): TVector2b;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
end;

//***********************************************************************

function Vector3fMake(const X, Y, Z: Single): TVector3f;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
end;

function Vector3iMake(const X, Y, Z: Longint): TVector3i;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
end;

function Vector3sMake(const X, Y, Z: SmallInt): TVector3s;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
end;

function Vector3dMake(const X, Y, Z: Double): TVector3d;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
end;

function Vector3bMake(const X, Y, Z: Byte): TVector3b;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
end;

function Vector3fMake(const Vector: TVector2f; const Z: Single): TVector3f;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Z;
end;

function Vector3iMake(const Vector: TVector2i; const Z: Longint): TVector3i;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Z;
end;

function Vector3sMake(const Vector: TVector2s; const Z: SmallInt): TVector3s;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Z;
end;

function Vector3dMake(const Vector: TVector2d; const Z: Double): TVector3d;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Z;
end;

function Vector3bMake(const Vector: TVector2b; const Z: Byte): TVector3b;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Z;
end;

function Vector3fMake(const Vector: TVector4f): TVector3f;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Vector.Z;
end;

function Vector3iMake(const Vector: TVector4i): TVector3i;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Vector.Z;
end;

function Vector3sMake(const Vector: TVector4s): TVector3s;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Vector.Z;
end;

function Vector3dMake(const Vector: TVector4d): TVector3d;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Vector.Z;
end;

function Vector3bMake(const Vector: TVector4b): TVector3b;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Vector.Z;
end;

//***********************************************************************

function Vector4fMake(const X, Y, Z, W: Single): TVector4f;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
  result.W := W;
end;

function Vector4iMake(const X, Y, Z, W: Longint): TVector4i;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
  result.W := W;
end;

function Vector4sMake(const X, Y, Z, W: SmallInt): TVector4s;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
  result.W := W;
end;

function Vector4dMake(const X, Y, Z, W: Double): TVector4d;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
  result.W := W;
end;

function Vector4bMake(const X, Y, Z, W: Byte): TVector4b;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
  result.W := W;
end;

function Vector4fMake(const Vector: TVector3f; const W: Single): TVector4f;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Vector.Z;
  result.W := W;
end;

function Vector4iMake(const Vector: TVector3i; const W: Longint): TVector4i;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Vector.Z;
  result.W := W;
end;

function Vector4sMake(const Vector: TVector3s; const W: SmallInt): TVector4s;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Vector.Z;
  result.W := W;
end;

function Vector4dMake(const Vector: TVector3d; const W: Double): TVector4d;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Vector.Z;
  result.W := W;
end;

function Vector4bMake(const Vector: TVector3b; const W: Byte): TVector4b;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Vector.Z;
  result.W := W;
end;

function Vector4fMake(const Vector: TVector2f; const Z: Single; const W: Single)
  : TVector4f;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Z;
  result.W := W;
end;

function Vector4iMake(const Vector: TVector2i; const Z: Longint;
  const W: Longint): TVector4i;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Z;
  result.W := W;
end;

function Vector4sMake(const Vector: TVector2s; const Z: SmallInt;
  const W: SmallInt): TVector4s;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Z;
  result.W := W;
end;

function Vector4dMake(const Vector: TVector2d; const Z: Double; const W: Double)
  : TVector4d;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Z;
  result.W := W;
end;

function Vector4bMake(const Vector: TVector2b; const Z: Byte; const W: Byte)
  : TVector4b;
begin
  result.X := Vector.X;
  result.Y := Vector.Y;
  result.Z := Z;
  result.W := W;
end;

//***********************************************************************

function VectorEquals(const Vector1, Vector2: TVector2f): Boolean;
begin
  result := (Vector1.X = Vector2.X) and (Vector1.Y = Vector2.Y);
end;

function VectorEquals(const Vector1, Vector2: TVector2i): Boolean;
begin
  result := (Vector1.X = Vector2.X) and (Vector1.Y = Vector2.Y);
end;

function VectorEquals(const V1, V2: TVector2d): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y);
end;

function VectorEquals(const V1, V2: TVector2s): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y);
end;

function VectorEquals(const V1, V2: TVector2b): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y);
end;

// ********************************************************************

function VectorEquals(const V1, V2: TVector3i): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y) and (V1.Z = V2.Z);
end;

function VectorEquals(const V1, V2: TVector3d): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y) and (V1.Z = V2.Z);
end;

function VectorEquals(const V1, V2: TVector3s): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y) and (V1.Z = V2.Z);
end;

function VectorEquals(const V1, V2: TVector3b): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y) and (V1.Z = V2.Z);
end;

{ ***************************************************************************** }

function VectorEquals(const V1, V2: TVector4i): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y) and (V1.Z = V2.Z)
    and (V1.W = V2.W);
end;

function VectorEquals(const V1, V2: TVector4d): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y) and (V1.Z = V2.Z)
    and (V1.W = V2.W);
end;

function VectorEquals(const V1, V2: TVector4s): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y) and (V1.Z = V2.Z)
    and (V1.W = V2.W);
end;

function VectorEquals(const V1, V2: TVector4b): Boolean;
begin
  result := (V1.X = V2.X) and (V1.Y = V2.Y) and (V1.Z = V2.Z)
    and (V1.W = V2.W);
end;

{ ***************************************************************************** }

function MatrixEquals(const Matrix1, Matrix2: TMatrix3f): Boolean;
begin
  result := VectorEquals(Matrix1.X, Matrix2.X) and
    VectorEquals(Matrix1.Y, Matrix2.Y) and
    VectorEquals(Matrix1.Z, Matrix2.Z);
end;

// 3x3i
function MatrixEquals(const Matrix1, Matrix2: TMatrix3i): Boolean;
begin
  result := VectorEquals(Matrix1.X, Matrix2.X) and
    VectorEquals(Matrix1.Y, Matrix2.Y) and
    VectorEquals(Matrix1.Z, Matrix2.Z);
end;

function MatrixEquals(const Matrix1, Matrix2: TMatrix3d): Boolean;
begin
  result := VectorEquals(Matrix1.X, Matrix2.X) and
    VectorEquals(Matrix1.Y, Matrix2.Y) and
    VectorEquals(Matrix1.Z, Matrix2.Z);
end;

function MatrixEquals(const Matrix1, Matrix2: TMatrix3s): Boolean;
begin
  result := VectorEquals(Matrix1.X, Matrix2.X) and
    VectorEquals(Matrix1.Y, Matrix2.Y) and
    VectorEquals(Matrix1.Z, Matrix2.Z);
end;

function MatrixEquals(const Matrix1, Matrix2: TMatrix3b): Boolean;
begin
  result := VectorEquals(Matrix1.X, Matrix2.X) and
    VectorEquals(Matrix1.Y, Matrix2.Y) and
    VectorEquals(Matrix1.Z, Matrix2.Z);
end;

{ ***************************************************************************** }

// 4x4f
function MatrixEquals(const Matrix1, Matrix2: TMatrix4f): Boolean;
begin
  result := VectorEquals(Matrix1.X, Matrix2.X) and
    VectorEquals(Matrix1.Y, Matrix2.Y) and
    VectorEquals(Matrix1.Z, Matrix2.Z) and
    VectorEquals(Matrix1.W, Matrix2.W);
end;

// 4x4i
function MatrixEquals(const Matrix1, Matrix2: TMatrix4i): Boolean;
begin
  result := VectorEquals(Matrix1.X, Matrix2.X) and
    VectorEquals(Matrix1.Y, Matrix2.Y) and
    VectorEquals(Matrix1.Z, Matrix2.Z) and
    VectorEquals(Matrix1.W, Matrix2.W);
end;

// 4x4d
function MatrixEquals(const Matrix1, Matrix2: TMatrix4d): Boolean;
begin
  result := VectorEquals(Matrix1.X, Matrix2.X) and
    VectorEquals(Matrix1.Y, Matrix2.Y) and
    VectorEquals(Matrix1.Z, Matrix2.Z) and
    VectorEquals(Matrix1.W, Matrix2.W);
end;

function MatrixEquals(const Matrix1, Matrix2: TMatrix4s): Boolean;
begin
  result := VectorEquals(Matrix1.X, Matrix2.X) and
    VectorEquals(Matrix1.Y, Matrix2.Y) and
    VectorEquals(Matrix1.Z, Matrix2.Z) and
    VectorEquals(Matrix1.W, Matrix2.W);
end;

function MatrixEquals(const Matrix1, Matrix2: TMatrix4b): Boolean;
begin
  result := VectorEquals(Matrix1.X, Matrix2.X) and
    VectorEquals(Matrix1.Y, Matrix2.Y) and
    VectorEquals(Matrix1.Z, Matrix2.Z) and
    VectorEquals(Matrix1.W, Matrix2.W);
end;

{ ***************************************************************************** }

function VectorMoreThen(const SourceVector, ComparedVector: TVector3f)
  : Boolean; overload;
begin
  result := (SourceVector.X > ComparedVector.X) and
    (SourceVector.Y > ComparedVector.Y) and
    (SourceVector.Z > ComparedVector.Z);
end;

function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector3f)
  : Boolean; overload;
begin
  result := (SourceVector.X >= ComparedVector.X) and
    (SourceVector.Y >= ComparedVector.Y) and
    (SourceVector.Z >= ComparedVector.Z);
end;

function VectorLessThen(const SourceVector, ComparedVector: TVector3f)
  : Boolean; overload;
begin
  result := (SourceVector.X < ComparedVector.X) and
    (SourceVector.Y < ComparedVector.Y) and
    (SourceVector.Z < ComparedVector.Z);
end;

function VectorLessEqualThen(const SourceVector, ComparedVector: TVector3f)
  : Boolean; overload;
begin
  result := (SourceVector.X <= ComparedVector.X) and
    (SourceVector.Y <= ComparedVector.Y) and
    (SourceVector.Z <= ComparedVector.Z);
end;

function VectorMoreThen(const SourceVector, ComparedVector: TVector4f)
  : Boolean; overload;
begin
  result := (SourceVector.X > ComparedVector.X) and
    (SourceVector.Y > ComparedVector.Y) and
    (SourceVector.Z > ComparedVector.Z) and
    (SourceVector.W > ComparedVector.W);
end;

function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector4f)
  : Boolean; overload;
begin
  result := (SourceVector.X >= ComparedVector.X) and
    (SourceVector.Y >= ComparedVector.Y) and
    (SourceVector.Z >= ComparedVector.Z) and
    (SourceVector.W >= ComparedVector.W);
end;

function VectorLessThen(const SourceVector, ComparedVector: TVector4f)
  : Boolean; overload;
begin
  result := (SourceVector.X < ComparedVector.X) and
    (SourceVector.Y < ComparedVector.Y) and
    (SourceVector.Z < ComparedVector.Z) and
    (SourceVector.W < ComparedVector.W);
end;

function VectorLessEqualThen(const SourceVector, ComparedVector: TVector4f)
  : Boolean; overload;
begin
  result := (SourceVector.X <= ComparedVector.X) and
    (SourceVector.Y <= ComparedVector.Y) and
    (SourceVector.Z <= ComparedVector.Z) and
    (SourceVector.W <= ComparedVector.W);
end;

function VectorMoreThen(const SourceVector, ComparedVector: TVector3i)
  : Boolean; overload;
begin
  result := (SourceVector.X > ComparedVector.X) and
    (SourceVector.Y > ComparedVector.Y) and
    (SourceVector.Z > ComparedVector.Z);
end;

function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector3i)
  : Boolean; overload;
begin
  result := (SourceVector.X >= ComparedVector.X) and
    (SourceVector.Y >= ComparedVector.Y) and
    (SourceVector.Z >= ComparedVector.Z);
end;

function VectorLessThen(const SourceVector, ComparedVector: TVector3i)
  : Boolean; overload;
begin
  result := (SourceVector.X < ComparedVector.X) and
    (SourceVector.Y < ComparedVector.Y) and
    (SourceVector.Z < ComparedVector.Z);
end;

function VectorLessEqualThen(const SourceVector, ComparedVector: TVector3i)
  : Boolean; overload;
begin
  result := (SourceVector.X <= ComparedVector.X) and
    (SourceVector.Y <= ComparedVector.Y) and
    (SourceVector.Z <= ComparedVector.Z);
end;

function VectorMoreThen(const SourceVector, ComparedVector: TVector4i)
  : Boolean; overload;
begin
  result := (SourceVector.X > ComparedVector.X) and
    (SourceVector.Y > ComparedVector.Y) and
    (SourceVector.Z > ComparedVector.Z) and
    (SourceVector.W > ComparedVector.W);
end;

function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector4i)
  : Boolean; overload;
begin
  result := (SourceVector.X >= ComparedVector.X) and
    (SourceVector.Y >= ComparedVector.Y) and
    (SourceVector.Z >= ComparedVector.Z) and
    (SourceVector.W >= ComparedVector.W);
end;

function VectorLessThen(const SourceVector, ComparedVector: TVector4i)
  : Boolean; overload;
begin
  result := (SourceVector.X < ComparedVector.X) and
    (SourceVector.Y < ComparedVector.Y) and
    (SourceVector.Z < ComparedVector.Z) and
    (SourceVector.W < ComparedVector.W);
end;

function VectorLessEqualThen(const SourceVector, ComparedVector: TVector4i)
  : Boolean; overload;
begin
  result := (SourceVector.X <= ComparedVector.X) and
    (SourceVector.Y <= ComparedVector.Y) and
    (SourceVector.Z <= ComparedVector.Z) and
    (SourceVector.W <= ComparedVector.W);
end;

function VectorMoreThen(const SourceVector, ComparedVector: TVector3s)
  : Boolean; overload;
begin
  result := (SourceVector.X > ComparedVector.X) and
    (SourceVector.Y > ComparedVector.Y) and
    (SourceVector.Z > ComparedVector.Z);
end;

function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector3s)
  : Boolean; overload;
begin
  result := (SourceVector.X >= ComparedVector.X) and
    (SourceVector.Y >= ComparedVector.Y) and
    (SourceVector.Z >= ComparedVector.Z);
end;

function VectorLessThen(const SourceVector, ComparedVector: TVector3s)
  : Boolean; overload;
begin
  result := (SourceVector.X < ComparedVector.X) and
    (SourceVector.Y < ComparedVector.Y) and
    (SourceVector.Z < ComparedVector.Z);
end;

function VectorLessEqualThen(const SourceVector, ComparedVector: TVector3s)
  : Boolean; overload;
begin
  result := (SourceVector.X <= ComparedVector.X) and
    (SourceVector.Y <= ComparedVector.Y) and
    (SourceVector.Z <= ComparedVector.Z);
end;

// 4s
function VectorMoreThen(const SourceVector, ComparedVector: TVector4s)
  : Boolean; overload;
begin
  result := (SourceVector.X > ComparedVector.X) and
    (SourceVector.Y > ComparedVector.Y) and
    (SourceVector.Z > ComparedVector.Z) and
    (SourceVector.W > ComparedVector.W);
end;

function VectorMoreEqualThen(const SourceVector, ComparedVector: TVector4s)
  : Boolean; overload;
begin
  result := (SourceVector.X >= ComparedVector.X) and
    (SourceVector.Y >= ComparedVector.Y) and
    (SourceVector.Z >= ComparedVector.Z) and
    (SourceVector.W >= ComparedVector.W);
end;

function VectorLessThen(const SourceVector, ComparedVector: TVector4s)
  : Boolean; overload;
begin
  result := (SourceVector.X < ComparedVector.X) and
    (SourceVector.Y < ComparedVector.Y) and
    (SourceVector.Z < ComparedVector.Z) and
    (SourceVector.W < ComparedVector.W);
end;

function VectorLessEqualThen(const SourceVector, ComparedVector: TVector4s)
  : Boolean; overload;
begin
  result := (SourceVector.X <= ComparedVector.X) and
    (SourceVector.Y <= ComparedVector.Y) and
    (SourceVector.Z <= ComparedVector.Z) and
    (SourceVector.W <= ComparedVector.W);
end;

function VectorMoreThen(const SourceVector: TVector3f;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X > ComparedNumber) and
    (SourceVector.Y > ComparedNumber) and
    (SourceVector.Z > ComparedNumber);
end;

function VectorMoreEqualThen(const SourceVector: TVector3f;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X >= ComparedNumber) and
    (SourceVector.Y >= ComparedNumber) and
    (SourceVector.Z >= ComparedNumber);
end;

function VectorLessThen(const SourceVector: TVector3f;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X < ComparedNumber) and
    (SourceVector.Y < ComparedNumber) and
    (SourceVector.Z < ComparedNumber);
end;

function VectorLessEqualThen(const SourceVector: TVector3f;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X <= ComparedNumber) and
    (SourceVector.Y <= ComparedNumber) and
    (SourceVector.Z <= ComparedNumber);
end;

function VectorMoreThen(const SourceVector: TVector4f;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X > ComparedNumber) and
    (SourceVector.Y > ComparedNumber) and
    (SourceVector.Z > ComparedNumber) and
    (SourceVector.W > ComparedNumber);
end;

function VectorMoreEqualThen(const SourceVector: TVector4f;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X >= ComparedNumber) and
    (SourceVector.Y >= ComparedNumber) and
    (SourceVector.Z >= ComparedNumber) and
    (SourceVector.W >= ComparedNumber);
end;

function VectorLessThen(const SourceVector: TVector4f;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X < ComparedNumber) and
    (SourceVector.Y < ComparedNumber) and
    (SourceVector.Z < ComparedNumber) and
    (SourceVector.W < ComparedNumber);
end;

function VectorLessEqualThen(const SourceVector: TVector4f;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X <= ComparedNumber) and
    (SourceVector.Y <= ComparedNumber) and
    (SourceVector.Z <= ComparedNumber) and
    (SourceVector.W <= ComparedNumber);
end;

function VectorMoreThen(const SourceVector: TVector3i;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X > ComparedNumber) and
    (SourceVector.Y > ComparedNumber) and
    (SourceVector.Z > ComparedNumber);
end;

function VectorMoreEqualThen(const SourceVector: TVector3i;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X >= ComparedNumber) and
    (SourceVector.Y >= ComparedNumber) and
    (SourceVector.Z >= ComparedNumber);
end;

function VectorLessThen(const SourceVector: TVector3i;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X < ComparedNumber) and
    (SourceVector.Y < ComparedNumber) and
    (SourceVector.Z < ComparedNumber);
end;

function VectorLessEqualThen(const SourceVector: TVector3i;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X <= ComparedNumber) and
    (SourceVector.Y <= ComparedNumber) and
    (SourceVector.Z <= ComparedNumber);
end;

function VectorMoreThen(const SourceVector: TVector4i;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X > ComparedNumber) and
    (SourceVector.Y > ComparedNumber) and
    (SourceVector.Z > ComparedNumber) and
    (SourceVector.W > ComparedNumber);
end;

function VectorMoreEqualThen(const SourceVector: TVector4i;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X >= ComparedNumber) and
    (SourceVector.Y >= ComparedNumber) and
    (SourceVector.Z >= ComparedNumber) and
    (SourceVector.W >= ComparedNumber);
end;

function VectorLessThen(const SourceVector: TVector4i;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X < ComparedNumber) and
    (SourceVector.Y < ComparedNumber) and
    (SourceVector.Z < ComparedNumber) and
    (SourceVector.W < ComparedNumber);
end;

function VectorLessEqualThen(const SourceVector: TVector4i;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X <= ComparedNumber) and
    (SourceVector.Y <= ComparedNumber) and
    (SourceVector.Z <= ComparedNumber) and
    (SourceVector.W <= ComparedNumber);
end;

function VectorMoreThen(const SourceVector: TVector3s;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X > ComparedNumber) and
    (SourceVector.Y > ComparedNumber) and
    (SourceVector.Z > ComparedNumber);
end;

function VectorMoreEqualThen(const SourceVector: TVector3s;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X >= ComparedNumber) and
    (SourceVector.Y >= ComparedNumber) and
    (SourceVector.Z >= ComparedNumber);
end;

function VectorLessThen(const SourceVector: TVector3s;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X < ComparedNumber) and
    (SourceVector.Y < ComparedNumber) and
    (SourceVector.Z < ComparedNumber);
end;

function VectorLessEqualThen(const SourceVector: TVector3s;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X <= ComparedNumber) and
    (SourceVector.Y <= ComparedNumber) and
    (SourceVector.Z <= ComparedNumber);
end;

function VectorMoreThen(const SourceVector: TVector4s;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X > ComparedNumber) and
    (SourceVector.Y > ComparedNumber) and
    (SourceVector.Z > ComparedNumber) and
    (SourceVector.W > ComparedNumber);
end;

function VectorMoreEqualThen(const SourceVector: TVector4s;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X >= ComparedNumber) and
    (SourceVector.Y >= ComparedNumber) and
    (SourceVector.Z >= ComparedNumber) and
    (SourceVector.W >= ComparedNumber);
end;

function VectorLessThen(const SourceVector: TVector4s;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X < ComparedNumber) and
    (SourceVector.Y < ComparedNumber) and
    (SourceVector.Z < ComparedNumber) and
    (SourceVector.W < ComparedNumber);
end;

function VectorLessEqualThen(const SourceVector: TVector4s;
  const ComparedNumber: Single): Boolean; overload;
begin
  result := (SourceVector.X <= ComparedNumber) and
    (SourceVector.Y <= ComparedNumber) and
    (SourceVector.Z <= ComparedNumber) and
    (SourceVector.W <= ComparedNumber);
end;

function RectanglesIntersect(const ACenterOfRect1, ACenterOfRect2, ASizeOfRect1,
  ASizeOfRect2: TVector2f): Boolean;
begin
  result := (Abs(ACenterOfRect1.X - ACenterOfRect2.X) <
    (ASizeOfRect1.X + ASizeOfRect2.X) / 2) and
    (Abs(ACenterOfRect1.Y - ACenterOfRect2.Y) <
    (ASizeOfRect1.Y + ASizeOfRect2.Y) / 2);
end;

function RectangleContains(const ACenterOfBigRect1, ACenterOfSmallRect2,
  ASizeOfBigRect1, ASizeOfSmallRect2: TVector2f;
  const AEps: Single = 0.0): Boolean;
begin
  result := (Abs(ACenterOfBigRect1.X - ACenterOfSmallRect2.X) +
    ASizeOfSmallRect2.X / 2 - ASizeOfBigRect1.X / 2 < AEps) and
    (Abs(ACenterOfBigRect1.Y - ACenterOfSmallRect2.Y) +
    ASizeOfSmallRect2.Y / 2 - ASizeOfBigRect1.Y / 2 < AEps);
end;

function GetSafeTurnAngle(const AOriginalPosition, AOriginalUpVector,
  ATargetPosition, AMoveAroundTargetCenter: TGLVector): TVector2f;
var
  pitchangle0, pitchangle1, turnangle0, turnangle1, pitchangledif, turnangledif,
    dx0, dy0, dz0, dx1, dy1, dz1: Double;
  Sign: shortint;
begin
  // determine relative positions to determine the lines which form the angles
  // distances from initial camera pos to target object
  dx0 := AOriginalPosition.X - AMoveAroundTargetCenter.X;
  dy0 := AOriginalPosition.Y - AMoveAroundTargetCenter.Y;
  dz0 := AOriginalPosition.Z - AMoveAroundTargetCenter.Z;

  // distances from final camera pos to target object
  dx1 := ATargetPosition.X - AMoveAroundTargetCenter.X;
  dy1 := ATargetPosition.Y - AMoveAroundTargetCenter.Y;
  dz1 := ATargetPosition.Z - AMoveAroundTargetCenter.Z;

  // just to make sure we don't get division by 0 exceptions
  if dx0 = 0 then
    dx0 := 0.001;
  if dy0 = 0 then
    dy0 := 0.001;
  if dz0 = 0 then
    dz0 := 0.001;
  if dx1 = 0 then
    dx1 := 0.001;
  if dy1 = 0 then
    dy1 := 0.001;
  if dz1 = 0 then
    dz1 := 0.001;

  // determine "pitch" and "turn" angles for the initial and  final camera position
  // the formulas differ depending on the camera.Up vector
  // I tested all quadrants for all possible integer FJoblist.Camera.Up directions
  if Abs(AOriginalUpVector.Z) = 1 then // Z=1/-1
  begin
    Sign := Round(AOriginalUpVector.Z / Abs(AOriginalUpVector.Z));
    pitchangle0 := arctan(dz0 / Sqrt(Sqr(dx0) + Sqr(dy0)));
    pitchangle1 := arctan(dz1 / Sqrt(Sqr(dx1) + Sqr(dy1)));
    turnangle0 := arctan(dy0 / dx0);
    if (dx0 < 0) and (dy0 < 0) then
      turnangle0 := -(PI - turnangle0)
    else if (dx0 < 0) and (dy0 > 0) then
      turnangle0 := -(PI - turnangle0);
    turnangle1 := arctan(dy1 / dx1);
    if (dx1 < 0) and (dy1 < 0) then
      turnangle1 := -(PI - turnangle1)
    else if (dx1 < 0) and (dy1 > 0) then
      turnangle1 := -(PI - turnangle1);
  end
  else if Abs(AOriginalUpVector.Y) = 1 then // Y=1/-1
  begin
    Sign := Round(AOriginalUpVector.Y / Abs(AOriginalUpVector.Y));
    pitchangle0 := arctan(dy0 / Sqrt(Sqr(dx0) + Sqr(dz0)));
    pitchangle1 := arctan(dy1 / Sqrt(Sqr(dx1) + Sqr(dz1)));
    turnangle0 := -arctan(dz0 / dx0);
    if (dx0 < 0) and (dz0 < 0) then
      turnangle0 := -(PI - turnangle0)
    else if (dx0 < 0) and (dz0 > 0) then
      turnangle0 := -(PI - turnangle0);
    turnangle1 := -arctan(dz1 / dx1);
    if (dx1 < 0) and (dz1 < 0) then
      turnangle1 := -(PI - turnangle1)
    else if (dx1 < 0) and (dz1 > 0) then
      turnangle1 := -(PI - turnangle1);
  end
  else if Abs(AOriginalUpVector.X) = 1 then // X=1/-1
  begin
    Sign := Round(AOriginalUpVector.X / Abs(AOriginalUpVector.X));
    pitchangle0 := arctan(dx0 / Sqrt(Sqr(dz0) + Sqr(dy0)));
    pitchangle1 := arctan(dx1 / Sqrt(Sqr(dz1) + Sqr(dy1)));
    turnangle0 := arctan(dz0 / dy0);
    if (dz0 > 0) and (dy0 > 0) then
      turnangle0 := -(PI - turnangle0)
    else if (dz0 < 0) and (dy0 > 0) then
      turnangle0 := -(PI - turnangle0);
    turnangle1 := arctan(dz1 / dy1);
    if (dz1 > 0) and (dy1 > 0) then
      turnangle1 := -(PI - turnangle1)
    else if (dz1 < 0) and (dy1 > 0) then
      turnangle1 := -(PI - turnangle1);
  end
  else
  begin
    Raise Exception.Create('The Camera.Up vector may contain only -1, 0 or 1');
  end;

  // determine pitch and turn angle differences
  pitchangledif := Sign * (pitchangle1 - pitchangle0);
  turnangledif := Sign * (turnangle1 - turnangle0);

  if Abs(turnangledif) > PI then
    turnangledif := -Abs(turnangledif) / turnangledif *
      (2 * PI - Abs(turnangledif));

  // Determine rotation speeds
  result.X := RadianToDeg(-pitchangledif);
  result.Y := RadianToDeg(turnangledif);
end;

function GetSafeTurnAngle(const AOriginalPosition, AOriginalUpVector,
  ATargetPosition, AMoveAroundTargetCenter: TAffineVector): TVector2f;
var
  pitchangle0, pitchangle1, turnangle0, turnangle1, pitchangledif, turnangledif,
    dx0, dy0, dz0, dx1, dy1, dz1: Double;
  Sign: shortint;
begin
  // determine relative positions to determine the lines which form the angles
  // distances from initial camera pos to target object
  dx0 := AOriginalPosition.X - AMoveAroundTargetCenter.X;
  dy0 := AOriginalPosition.Y - AMoveAroundTargetCenter.Y;
  dz0 := AOriginalPosition.Z - AMoveAroundTargetCenter.Z;

  // distances from final camera pos to target object
  dx1 := ATargetPosition.X - AMoveAroundTargetCenter.X;
  dy1 := ATargetPosition.Y - AMoveAroundTargetCenter.Y;
  dz1 := ATargetPosition.Z - AMoveAroundTargetCenter.Z;

  // just to make sure we don't get division by 0 exceptions
  if dx0 = 0 then
    dx0 := 0.001;
  if dy0 = 0 then
    dy0 := 0.001;
  if dz0 = 0 then
    dz0 := 0.001;
  if dx1 = 0 then
    dx1 := 0.001;
  if dy1 = 0 then
    dy1 := 0.001;
  if dz1 = 0 then
    dz1 := 0.001;

  // determine "pitch" and "turn" angles for the initial and  final camera position
  // the formulas differ depending on the camera.Up vector
  // I tested all quadrants for all possible integer FJoblist.Camera.Up directions
  if Abs(AOriginalUpVector.Z) = 1 then // Z=1/-1
  begin
    Sign := Round(AOriginalUpVector.Z / Abs(AOriginalUpVector.Z));
    pitchangle0 := arctan(dz0 / Sqrt(Sqr(dx0) + Sqr(dy0)));
    pitchangle1 := arctan(dz1 / Sqrt(Sqr(dx1) + Sqr(dy1)));
    turnangle0 := arctan(dy0 / dx0);
    if (dx0 < 0) and (dy0 < 0) then
      turnangle0 := -(PI - turnangle0)
    else if (dx0 < 0) and (dy0 > 0) then
      turnangle0 := -(PI - turnangle0);
    turnangle1 := arctan(dy1 / dx1);
    if (dx1 < 0) and (dy1 < 0) then
      turnangle1 := -(PI - turnangle1)
    else if (dx1 < 0) and (dy1 > 0) then
      turnangle1 := -(PI - turnangle1);
  end
  else if Abs(AOriginalUpVector.Y) = 1 then // Y=1/-1
  begin
    Sign := Round(AOriginalUpVector.Y / Abs(AOriginalUpVector.Y));
    pitchangle0 := arctan(dy0 / Sqrt(Sqr(dx0) + Sqr(dz0)));
    pitchangle1 := arctan(dy1 / Sqrt(Sqr(dx1) + Sqr(dz1)));
    turnangle0 := -arctan(dz0 / dx0);
    if (dx0 < 0) and (dz0 < 0) then
      turnangle0 := -(PI - turnangle0)
    else if (dx0 < 0) and (dz0 > 0) then
      turnangle0 := -(PI - turnangle0);
    turnangle1 := -arctan(dz1 / dx1);
    if (dx1 < 0) and (dz1 < 0) then
      turnangle1 := -(PI - turnangle1)
    else if (dx1 < 0) and (dz1 > 0) then
      turnangle1 := -(PI - turnangle1);
  end
  else if Abs(AOriginalUpVector.X) = 1 then // X=1/-1
  begin
    Sign := Round(AOriginalUpVector.X / Abs(AOriginalUpVector.X));
    pitchangle0 := arctan(dx0 / Sqrt(Sqr(dz0) + Sqr(dy0)));
    pitchangle1 := arctan(dx1 / Sqrt(Sqr(dz1) + Sqr(dy1)));
    turnangle0 := arctan(dz0 / dy0);
    if (dz0 > 0) and (dy0 > 0) then
      turnangle0 := -(PI - turnangle0)
    else if (dz0 < 0) and (dy0 > 0) then
      turnangle0 := -(PI - turnangle0);
    turnangle1 := arctan(dz1 / dy1);
    if (dz1 > 0) and (dy1 > 0) then
      turnangle1 := -(PI - turnangle1)
    else if (dz1 < 0) and (dy1 > 0) then
      turnangle1 := -(PI - turnangle1);
  end
  else
  begin
    Raise Exception.Create('The Camera.Up vector may contain only -1, 0 or 1');
  end;

  // determine pitch and turn angle differences
  pitchangledif := Sign * (pitchangle1 - pitchangle0);
  turnangledif := Sign * (turnangle1 - turnangle0);

  if Abs(turnangledif) > PI then
    turnangledif := -Abs(turnangledif) / turnangledif *
      (2 * PI - Abs(turnangledif));

  // Determine rotation speeds
  result.X := RadianToDeg(-pitchangledif);
  result.Y := RadianToDeg(turnangledif);
end;

function MoveObjectAround(const AMovingObjectPosition, AMovingObjectUp,
  ATargetPosition: TGLVector; pitchDelta, turnDelta: Single): TGLVector;
var
  originalT2C, normalT2C, normalCameraRight: TGLVector;
  pitchNow, dist: Single;
begin
  // normalT2C points away from the direction the camera is looking
  originalT2C := VectorSubtract(AMovingObjectPosition, ATargetPosition);
  SetVector(normalT2C, originalT2C);
  dist := VectorLength(normalT2C);
  NormalizeVector(normalT2C);
  // normalRight points to the camera's right the camera is pitching around this axis.
  normalCameraRight := VectorCrossProduct(AMovingObjectUp, normalT2C);
  if VectorLength(normalCameraRight) < 0.001 then
    SetVector(normalCameraRight, XVector) // arbitrary vector
  else
    NormalizeVector(normalCameraRight);
  // calculate the current pitch. 0 is looking down and PI is looking up
  pitchNow := ArcCosine(VectorDotProduct(AMovingObjectUp, normalT2C));
  pitchNow := ClampValue(pitchNow + DegToRadian(pitchDelta), 0 + 0.025,
    PI - 0.025);
  // creates a new vector pointing up and then rotate it down into the new position
  SetVector(normalT2C, AMovingObjectUp);
  RotateVector(normalT2C, normalCameraRight, -pitchNow);
  RotateVector(normalT2C, AMovingObjectUp, -DegToRadian(turnDelta));
  ScaleVector(normalT2C, dist);
  result := VectorAdd(AMovingObjectPosition, VectorSubtract(normalT2C,
    originalT2C));
end;

function AngleBetweenVectors(const a, b, ACenterPoint: TGLVector): Single;
begin
  result := ArcCosine(VectorAngleCosine(VectorNormalize(VectorSubtract(a,
    ACenterPoint)), VectorNormalize(VectorSubtract(b, ACenterPoint))));
end;

function AngleBetweenVectors(const a, b, ACenterPoint: TAffineVector): Single;
begin
  result := ArcCosine(VectorAngleCosine(VectorNormalize(VectorSubtract(a,
    ACenterPoint)), VectorNormalize(VectorSubtract(b, ACenterPoint))));
end;

function ShiftObjectFromCenter(const AOriginalPosition: TGLVector;
  const ACenter: TGLVector; const ADistance: Single;
  const AFromCenterSpot: Boolean): TGLVector;
var
  lDirection: TGLVector;
begin
  lDirection := VectorNormalize(VectorSubtract(AOriginalPosition, ACenter));
  if AFromCenterSpot then
    result := VectorAdd(ACenter, VectorScale(lDirection, ADistance))
  else
    result := VectorAdd(AOriginalPosition, VectorScale(lDirection, ADistance))
end;

function ShiftObjectFromCenter(const AOriginalPosition: TAffineVector;
  const ACenter: TAffineVector; const ADistance: Single;
  const AFromCenterSpot: Boolean): TAffineVector;
var
  lDirection: TAffineVector;
begin
  lDirection := VectorNormalize(VectorSubtract(AOriginalPosition, ACenter));
  if AFromCenterSpot then
    result := VectorAdd(ACenter, VectorScale(lDirection, ADistance))
  else
    result := VectorAdd(AOriginalPosition, VectorScale(lDirection, ADistance))
end;

// --------------------------------------------------------------
initialization
// --------------------------------------------------------------

vSIMD := 0;

end.

