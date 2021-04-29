unit Velthuis.RandomNumbers;

interface

type
  IRandom = interface
    function Next: Integer; overload;
    function Next(MaxValue: Integer): Integer; overload;
    function Next(MinValue, MaxValue: Integer): Integer; overload;
    function NextDouble: Double;
    procedure NextBytes(var Bytes: array of Byte);
    procedure SetSeed(Seed: Int64);
    function GetSeed: Int64;
    property Seed: Int64 read GetSeed write SetSeed;
  end;

  TRandom = class(TInterfacedObject, IRandom)
  private
    FSeed: Int64;       // Only 48 bits are used.
  public
    constructor Create(Seed: Int64 = 0);
    function Next: Integer; overload; virtual;
    function Next(MaxValue: Integer): Integer; overload;
    function Next(MinValue, MaxValue: Integer): Integer; overload;
    procedure NextBytes(var Bytes: array of Byte);
    function NextDouble: Double;
    procedure SetSeed(ASeed: Int64);
    function GetSeed: Int64;
  end;

  TDelphiRandom = class(TInterfacedObject, IRandom)
  public
    constructor Create; overload;
    constructor Create(Seed: Int64); overload;
    function Next: Integer; overload;
    function Next(MaxValue: Integer): Integer; overload;
    function Next(MinValue, MaxValue: Integer): Integer; overload;
    procedure NextBytes(var Bytes: array of Byte);
    function NextDouble: Double;
    procedure SetSeed(ASeed: Int64);
    function GetSeed: Int64;
  end;

implementation

uses
  SysUtils;

{ TRandom }

const
  CSeedMask      = Int64(1) shl 48 - 1;
  CMultiplicator = Int64($00000005DEECE66D);
  CConstant      = Int64($000000000000000B);
  CSeedSize      = 48 div 8;

constructor TRandom.Create(Seed: Int64);
begin
  FSeed := Seed and CSeedMask;
end;

function TRandom.Next: Integer;
var
  Temp: Int64;
begin
{$IFOPT Q+}
{$DEFINE HasRangeChecks}
{$ENDIF}

  Result := FSeed and MaxInt;
  {$RANGECHECKS OFF}
  Temp := (FSeed * CMultiplicator + CConstant) and CSeedMask;
  FSeed := Temp;

{$IFDEF HasRangeChecks}
{$RANGECHECKS ON}
{$ENDIF}
end;

function TRandom.Next(MaxValue: Integer): Integer;
begin
  Result := UInt64(Cardinal(FSeed)) * UInt64(Cardinal(MaxValue)) shr 32;
end;

function TRandom.Next(MinValue, MaxValue: Integer): Integer;
begin
  Result := MinValue + Next(MaxValue - MinValue);
end;

procedure TRandom.NextBytes(var Bytes: array of Byte);
var
  I, Tail: Integer;
  Len: Integer;
begin
  Len := Length(Bytes) and MaxInt;
  Tail := Len mod CSeedSize;
  Len := Len div CSeedSize;
  I := 0;
  while I < Len do
  begin
    Move(FSeed, Bytes[I * CSeedSize], CSeedSize);
    Inc(I);
    Next;
  end;
  if Tail > 0 then
  begin
    Move(FSeed, Bytes[I * CSeedSize], Tail);
    Next;
  end;
end;

function TRandom.NextDouble: Double;
const
  Divisor: Double = (1.0 / $1000000) / $1000000;        // 2^-48;
begin
  Result := FSeed * Divisor;
  Next;
end;

function TRandom.GetSeed: Int64;
begin
  Result := FSeed;
end;

procedure TRandom.SetSeed(ASeed: Int64);
begin
  FSeed := ASeed and CSeedMask;
end;

{ TDelphiRandom }

constructor TDelphiRandom.Create(Seed: Int64);
begin
  System.RandSeed := Integer(Seed);
end;

constructor TDelphiRandom.Create;
begin
  Randomize;
end;

function TDelphiRandom.GetSeed: Int64;
begin
  Result := System.RandSeed;
end;

function TDelphiRandom.Next(MinValue, MaxValue: Integer): Integer;
begin
  Result := MinValue + Next(MaxValue - MinValue);
end;

function TDelphiRandom.Next(MaxValue: Integer): Integer;
begin
  Result := System.Random(MaxValue);
end;

function TDelphiRandom.Next: Integer;
begin
  Result := System.Random(MaxInt);
end;

procedure TDelphiRandom.NextBytes(var Bytes: array of Byte);
var
  I, Tail: Integer;
  Len: Integer;
const
  CSeedSize = SizeOf(System.RandSeed);
begin
  Len := Length(Bytes) and MaxInt;
  Tail := Len mod CSeedSize;
  Len := Len div CSeedSize;

  // Can't use a for-loop, because I is still needed afterward.
  I := 0;
  while I < Len do
  begin
    Move(System.RandSeed, Bytes[I * CSeedSize], CSeedSize);
    Next;
    Inc(I);
  end;

  if Tail > 0 then
  begin
    Move(System.RandSeed, Bytes[I * CSeedSize], Tail);
    Next;
  end;
end;

function TDelphiRandom.NextDouble: Double;
begin
  Result := System.Random;
end;

procedure TDelphiRandom.SetSeed(ASeed: Int64);
begin
  System.RandSeed := Integer(ASeed);
end;

end.



