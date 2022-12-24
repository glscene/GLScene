//
// The graphics rendering engine GLScene http://glscene.org
//
unit CUDA.DataAccess;

(* CUDA data access implementation *)

interface

uses
  System.SysUtils,

  GLS.Logger,
  GLS.Strings,
  GLS.Utils;

type

  GCUDAHostElementAccess<TScalar> = class
  public const
    ElementSize = SizeOf(TScalar);

  type
    TVector2 = array [0 .. 1] of TScalar;
    TVector3 = array [0 .. 2] of TScalar;
    TVector4 = array [0 .. 3] of TScalar;
  private
    class procedure CheckElementSize(ACNum: Cardinal); inline;
    class function GetScalar: TScalar;
    class function GetVector2: TVector2;
    class function GetVector3: TVector3;
    class function GetVector4: TVector4;
    class procedure SetScalar(const AValue: TScalar);
    class procedure SetVector2(const AValue: TVector2);
    class procedure SetVector3(const AValue: TVector3);
    class procedure SetVector4(const AValue: TVector4);
  public
    property Scalar: TScalar read GetScalar write SetScalar;
    property Vector2: TVector2 read GetVector2 write SetVector2;
    property Vector3: TVector3 read GetVector3 write SetVector3;
    property Vector4: TVector4 read GetVector4 write SetVector4;
  end;

  UByteElement = GCUDAHostElementAccess<Byte>;
  ByteElement = GCUDAHostElementAccess<ShortInt>;
  UShortElement = GCUDAHostElementAccess<Word>;
  ShortElement = GCUDAHostElementAccess<SmallInt>;
  UIntElement = GCUDAHostElementAccess<LongWord>;
  IntElement = GCUDAHostElementAccess<LongInt>;
  HalfElement = GCUDAHostElementAccess<THalfFloat>;
  FloatElement = GCUDAHostElementAccess<Single>;
  DoubleElement = GCUDAHostElementAccess<Double>;

procedure SetElementAccessAddress(AValue: PByte; ASize: Cardinal);
function GetElementAccessAddress: PByte;
function GetElementAccessSize: Cardinal;

// -----------------------------------------------
implementation
// -----------------------------------------------

threadvar
  vElementAccessAddress: PByte;
  vElementAccessElementSize: Cardinal;

function GetElementAccessAddress: PByte;
begin
  Result := vElementAccessAddress;
end;

function GetElementAccessSize: Cardinal;
begin
  Result := vElementAccessElementSize;
end;

procedure SetElementAccessAddress(AValue: PByte; ASize: Cardinal);
begin
  vElementAccessAddress := AValue;
  vElementAccessElementSize := ASize;
end;

class procedure GCUDAHostElementAccess<TScalar>.CheckElementSize
  (ACNum: Cardinal);
begin
  if GetElementAccessSize <> ACNum * SizeOf(TScalar) then
  begin
    GLSLogger.LogError(strSizeMismatch);
    Abort;
  end;
end;

class function GCUDAHostElementAccess<TScalar>.GetScalar: TScalar;
begin
  CheckElementSize(1);
  Move(GetElementAccessAddress^, Result, SizeOf(TScalar));
end;

class function GCUDAHostElementAccess<TScalar>.GetVector2: TVector2;
begin
  CheckElementSize(2);
  Move(GetElementAccessAddress^, Result, 2 * SizeOf(TScalar));
end;

class function GCUDAHostElementAccess<TScalar>.GetVector3: TVector3;
begin
  CheckElementSize(3);
  Move(GetElementAccessAddress^, Result, 3 * SizeOf(TScalar));
end;

class function GCUDAHostElementAccess<TScalar>.GetVector4: TVector4;
begin
  CheckElementSize(4);
  Move(GetElementAccessAddress^, Result, 4 * SizeOf(TScalar));
end;

class procedure GCUDAHostElementAccess<TScalar>.SetScalar
  (const AValue: TScalar);
begin
  CheckElementSize(1);
  Move(AValue, GetElementAccessAddress^, SizeOf(TScalar));
end;

class procedure GCUDAHostElementAccess<TScalar>.SetVector2
  (const AValue: TVector2);
begin
  CheckElementSize(2);
  Move(AValue, GetElementAccessAddress^, 2 * SizeOf(TScalar));
end;

class procedure GCUDAHostElementAccess<TScalar>.SetVector3
  (const AValue: TVector3);
begin
  CheckElementSize(3);
  Move(AValue, GetElementAccessAddress^, 3 * SizeOf(TScalar));
end;

class procedure GCUDAHostElementAccess<TScalar>.SetVector4
  (const AValue: TVector4);
begin
  CheckElementSize(4);
  Move(AValue, GetElementAccessAddress^, 4 * SizeOf(TScalar));
end;

end.
