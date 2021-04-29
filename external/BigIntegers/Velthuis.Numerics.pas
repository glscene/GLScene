unit Velthuis.Numerics;

interface

(*
  Compare
  CompareTo
  HashCode
  HighestOneBit
  LowestOneBit
  Reverse
  ReverseBytes
  RotateLeft
  RotateRight
  Sign
  ToBinaryString
  ToHexString
  ToOctalString
  ToString
  ToString(base)
*)

// For Delphi XE3 and up:
{$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
{$IFEND}

// For Delphi XE and up:
{$IF CompilerVersion >= 22.0}
  {$CODEALIGN 16}
  {$ALIGN 16}
{$IFEND}

uses
  Math, Types;

function BitCount(U: UInt8): Integer; overload;
function BitCount(U: UInt16): Integer; overload;
function BitCount(S: Int32): Integer; overload;
function BitCount(U: UInt32): Integer; overload;

function BitLength(S: Int32): Integer; overload;
function BitLength(U: UInt32): Integer; overload;

function HighestOneBit(S: Int32): Int32; overload;
function HighestOneBit(U: UInt32): UInt32; overload;

function IsPowerOfTwo(S: Int32): Boolean; overload;
function IsPowerOfTwo(U: UInt32): Boolean; overload;

function LowestOneBit(S: Int32): Int32; overload;
function LowestOneBit(U: UInt32): UInt32; overload;

function NumberOfLeadingZeros(U: UInt16): Integer; overload;
function NumberOfLeadingZeros(S: Int32): Integer; overload;
function NumberOfLeadingZeros(U: UInt32): Integer; overload;
function NumberOfTrailingZeros(U: UInt32): Integer; overload;

function Reverse(U: UInt8): UInt8; overload;
function Reverse(U: UInt16): UInt16; overload;
function Reverse(S: Int32): Int32; overload;
function Reverse(U: UInt32): UInt32; overload;

function ReverseBytes(S: Int32): Int32; overload;
function ReverseBytes(U: UInt32): UInt32; overload;

function RotateLeft(S: Int32; Distance: Integer): Int32; overload;
function RotateLeft(U: UInt32; Distance: Integer): UInt32; overload;

function RotateRight(S: Int32; Distance: Integer): Int32; overload;
function RotateRight(U: UInt32; Distance: Integer): UInt32; overload;

function Sign(S: Int32): TValueSign;

function ToBinaryString(S: Int32): string; overload;
function ToBinaryString(U: UInt32): string; overload;

function ToHexString(S: Int32): string; overload;
function ToHexString(U: UInt32): string; overload;

function ToOctalString(S: Int32): string; overload;
function ToOctalString(U: UInt32): string; overload;

function ToString(S: Int32; Base: Byte): string; overload;
function ToString(U: UInt32; Base: Byte): string; overload;

implementation

// https://en.wikipedia.org/wiki/Find_first_set

uses
  SysUtils;

const
  // Currently not used.
  NLZDeBruijn32Mult = $07C4ACDD;
  NLZDeBruijn32: array[0..31] of Byte =
  (
    31, 22, 30, 21, 18, 10, 29,  2, 20, 17, 15, 13,  9,  6, 28,  1,
    23, 19, 11,  3, 16, 14,  7, 24, 12,  4,  8, 25,  5, 26, 27,  0
  );

  NTZDeBruijn32Mult = $077CB531;
  NTZDeBruijn32: array[0..31] of Byte =
  (
     0,  1, 28,  2, 29, 14, 24,  3, 30, 22, 20, 15, 25, 17,  4,  8,
    31, 27, 13, 23, 21, 19, 16,  7, 26, 12, 18,  6, 11,  5, 10,  9
  );

  BitCounts: array[0..15] of Byte = (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4);

function BitCount(U: UInt8): Integer;
begin
  Result := BitCounts[U and $0F] + BitCounts[U shr 4];
end;

function BitCount(U: UInt16): Integer;
{$IF DEFINED(WIN32)}
asm
        MOV     DX,AX
        SHR     DX,1
        AND     DX,$5555
        SUB     AX,DX
        MOV     DX,AX
        AND     AX,$3333
        SHR     DX,2
        AND     DX,$3333
        ADD     AX,DX
        MOV     DX,AX
        SHR     DX,4
        ADD     AX,DX
        AND     AX,$0F0F
        MOV     DX,AX
        SHR     AX,8
        ADD     AX,DX
        AND     EAX,$7F
end;
{$ELSEIF DEFINED(WIN64)}
asm
        .NOFRAME

        MOV     AX,CX
        SHR     CX,1
        AND     CX,$5555
        SUB     AX,CX
        MOV     CX,AX
        AND     AX,$3333
        SHR     CX,2
        AND     CX,$3333
        ADD     AX,CX
        MOV     CX,AX
        SHR     CX,4
        ADD     AX,CX
        AND     AX,$0F0F
        MOV     CX,AX
        SHR     AX,8
        ADD     AX,CX
        AND     EAX,$7F
end;
{$ELSE PUREPASCAL}
begin
  U := U - ((U shr 1) and $5555);
  U := (U and $3333) + ((U shr 2) and $3333);
  U := (U + (U shr 4)) and $0F0F;
  U := U + (U shr 8);
  Result := U and $7F;
end;
{$IFEND PUREPASCAL}

function BitCount(S: Int32): Integer;
begin
  Result := BitCount(UInt32(S));
end;

// Faster than 16 bit table lookups
function BitCount(U: UInt32): Integer;
{$IF DEFINED(WIN32)}
asm
        MOV     EDX,EAX
        SHR     EDX,1
        AND     EDX,$55555555
        SUB     EAX,EDX
        MOV     EDX,EAX
        AND     EAX,$33333333
        SHR     EDX,2
        AND     EDX,$33333333
        ADD     EAX,EDX
        MOV     EDX,EAX
        SHR     EDX,4
        ADD     EAX,EDX
        AND     EAX,$0F0F0F0F
        MOV     EDX,EAX
        SHR     EAX,8
        ADD     EAX,EDX
        MOV     EDX,EAX
        SHR     EDX,16
        ADD     EAX,EDX
        AND     EAX,$7F
end;
{$ELSEIF DEFINED(WIN64)}
asm
        .NOFRAME

        MOV     EAX,ECX
        SHR     ECX,1
        AND     ECX,$55555555
        SUB     EAX,ECX
        MOV     ECX,EAX
        AND     EAX,$33333333
        SHR     ECX,2
        AND     ECX,$33333333
        ADD     EAX,ECX
        MOV     ECX,EAX
        SHR     ECX,4
        ADD     EAX,ECX
        AND     EAX,$0F0F0F0F
        MOV     ECX,EAX
        SHR     EAX,8
        ADD     EAX,ECX
        MOV     ECX,EAX
        SHR     ECX,16
        ADD     EAX,ECX
        AND     EAX,$7F
end;
{$ELSE PUREPASCAL}
begin
  U := U - ((U shr 1) and $55555555);
  U := (U and $33333333) + ((U shr 2) and $33333333);
  U := (U + (U shr 4)) and $0F0F0F0F;
  U := U + (U shr 8);
  U := U + (U shr 16);
  Result := U and $7F;
end;
{$IFEND PUREPASCAL}

function BitLength(S: Int32): Integer;
begin
  Result := BitLength(UInt32(S));
end;

function BitLength(U: UInt32): Integer;
begin
  Result := 32 - NumberOfLeadingZeros(U);
end;

function IsPowerOfTwo(S: Int32): Boolean;
begin
  Result := IsPowerofTwo(UInt32(Abs(S)));
end;

function IsPowerOfTwo(U: UInt32): Boolean;
begin
  Result := (U and (U - 1)) = 0;
end;

function HighestOneBit(S: Int32): Int32;
begin
  Result := Int32(HighestOneBit(UInt32(S)));
end;

function HighestOneBit(U: UInt32): UInt32;
begin
  if U = 0 then
    Result := 0
  else
    Result := UInt32(1) shl (31 - NumberOfLeadingZeros(U));
end;

function LowestOneBit(S: Int32): Int32;
begin
  Result := Int32(LowestOneBit(UInt32(S)));
end;

function LowestOneBit(U: UInt32): UInt32;
begin
  Result := U and -Int32(U);
end;

function NumberOfLeadingZeros(U: UInt16): Integer;
{$IF DEFINED(WIN32)}
asm
        MOVZX   EAX,AX
        BSR     EDX,EAX
        JNZ     @Invert
        MOV     EAX,16
        RET

@Invert:

        MOV     EAX,15
        SUB     EAX,EDX
end;
{$ELSEIF DEFINED(WIN64)}
asm
        .NOFRAME

        MOVZX   EAX,CX
        BSR     ECX,EAX
        JNZ     @Invert
        MOV     EAX,16
        RET

@Invert:

        MOV     EAX,15
        SUB     EAX,ECX
end;
{$ELSE PUREPASCAL}
begin
  if U = 0 then
    Result := 16
  else
  begin
    Result := 0;
    if U <= High(Word) shr 8 then
    begin
      Result := Result + 8;
      U := U shl 8;
    end;
    if U <= High(Word) shr 4 then
    begin
      Result := Result + 4;
      U := U shl 4;
    end;
    if U <= High(Word) shr 2 then
    begin
      Result := Result + 2;
      U := U shl 2;
    end;
    if U <= High(Word) shr 1 then
      Result := Result + 1;
  end;
end;
{$IFEND PUREPASCAL}

function NumberOfLeadingZeros(S: Int32): Integer;
begin
  Result := NumberOfLeadingZeros(UInt32(S));
end;

function NumberOfLeadingZeros(U: UInt32): Integer;
{$IF DEFINED(WIN32)}
asm
        BSR     EDX,EAX
        JNZ     @Invert
        MOV     EAX,32
        RET

@Invert:

        MOV     EAX,31
        SUB     EAX,EDX

@Exit:
end;
{$ELSEIF DEFINED(WIN64)}
asm
         .NOFRAME

         BSR    EDX,ECX
         JNZ    @Invert
         MOV    EAX,32
         RET

@Invert:

         MOV    EAX,31
         SUB    EAX,EDX

@Exit:
end;
{$ELSE PUREPASCAL}

// Faster than X := X or X shr 1..16; Result := NLZDeBruijn32[...];

begin
  if U = 0 then
    Result := 32
  else
  begin
    Result := 0;
    if U <= High(Cardinal) shr 16 then
    begin
      Result := Result + 16;
      U := U shl 16;
    end;
    if U <= High(Cardinal) shr 8 then
    begin
      Result := Result + 8;
      U := U shl 8;
    end;
    if U <= High(Cardinal) shr 4 then
    begin
      Result := Result + 4;
      U := U shl 4;
    end;
    if U <= High(Cardinal) shr 2 then
    begin
      Result := Result + 2;
      U := U shl 2;
    end;
    if U <= High(Cardinal) shr 1 then
      Result := Result + 1;
  end;
end;
{$IFEND PUREPASCAL}

// Faster than NumberOfTrailingZeros2().
function NumberOfTrailingZeros(U: UInt32): Integer;
{$IF DEFINED(WIN32)}
asm
        BSF     EAX,EAX
        JNZ     @Exit
        MOV     EAX,32

@Exit:
end;
{$ELSEIF DEFINED(WIN64)}
asm
        .NOFRAME

        BSF     EAX,ECX
        JNZ     @Exit
        MOV     EAX,32

@Exit:
end;
{$ELSE PUREPASCAL}
begin
  if U = 0 then
    Result := 32
  else
    Result := NTZDeBruijn32[((U and (-Integer(U))) * NTZDeBruijn32Mult) shr 27];
end;
{$IFEND PUREPASCAL}

function Reverse(U: UInt8): UInt8;
begin
  U := ((U shr 1) and $55) or ((U and $55) shl 1);
  U := ((U shr 2) and $33) or ((U and $33) shl 2);
  U := (U shr 4) or (U shl 4);
  Result := U;
end;

function Reverse(U: UInt16): UInt16;
begin
  U := ((U shr 1) and $5555) or ((U and $5555) shl 1);
  U := ((U shr 2) and $3333) or ((U and $3333) shl 2);
  U := ((U shr 4) and $0F0F) or ((U and $0F0F) shl 4);
  U := Swap(U);
  Result := U;
end;

function Reverse(S: Int32): Int32;
begin
  Result := Int32(Reverse(UInt32(S)));
end;

// See http://stackoverflow.com/questions/746171/best-algorithm-for-bit-reversal-from-msb-lsb-to-lsb-msb-in-c too.
// http://stackoverflow.com/a/9144870/95954
function Reverse(U: UInt32): UInt32;
begin
  U := ((U shr 1) and $55555555) or ((U and $55555555) shl 1);  // Swap adjacent bits.
  U := ((U shr 2) and $33333333) or ((U and $33333333) shl 2);  // Swap adjacent bit pairs.
  U := ((U shr 4) and $0F0F0F0F) or ((U and $0F0F0F0F) shl 4);  // Swap nibbles.
  U := ((U shr 8) and $00FF00FF) or ((U and $00FF00FF) shl 8);  // Swap bytes.
  U := (U shr 16) or (U shl 16);                                // Swap words.
  Result := U;
end;

function ReverseBytes(S: Int32): Int32;
begin
  Result := Int32(ReverseBytes(UInt32(S)));
end;

// Byte and word swaps of Reverse(U).
function ReverseBytes(U: UInt32): UInt32;
begin
  U := ((U shr 8) and $00FF00FF) or ((U and $00FF00FF) shl 8);  // Swap bytes.
  U := (U shr 16) or (U shl 16);                                // Swap words.
  Result := U;
end;

function RotateLeft(S: Int32; Distance: Integer): Int32;
begin
  Result := Int32(RotateLeft(UInt32(S), Distance));
end;

function RotateLeft(U: UInt32; Distance: Integer): UInt32;
begin
  Distance := Distance and 31;
  Result := (U shl Distance) or (U shr (32 - Distance));
end;

function RotateRight(S: Int32; Distance: Integer): Int32;
begin
  Result := Int32(RotateRight(UInt32(S), Distance));
end;

function RotateRight(U: UInt32; Distance: Integer): UInt32;
begin
  Distance := Distance and 31;
  Result := (U shr Distance) or (U shl (32- Distance));
end;

function Sign(S: Int32): TValueSign;
begin
  Result := Math.Sign(S);
end;

function ToBinaryString(S: Int32): string;
begin
  Result := ToString(S, 2);
end;

function ToBinaryString(U: UInt32): string;
begin
  Result := ToString(U, 2);
end;

function ToHexString(S: Int32): string;
begin
  Result := ToString(S, 16);
end;

function ToHexString(U: UInt32): string;
begin
  Result := ToString(U, 16);
end;

function ToOctalString(S: Int32): string;
begin
  Result := ToString(S, 8);
end;

function ToOctalString(U: UInt32): string;
begin
  Result := ToString(U, 8);
end;

const
  Digits: array[0..35] of Char = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

function ToString(S: Int32; Base: Byte): string;
begin
  if S < 0 then
    Result := '-' + ToString(UInt32(Abs(S)), Base)
  else
    Result := ToString(UInt32(S), Base);
end;

function ToString(U: UInt32; Base: Byte): string;
begin
  if not (Base in [2..36]) then
    raise Exception.Create('Error Message');  // convert error? argument error?

  if U = 0 then
    Result := '0'
  else
  begin
    Result := '';
    while U > 0 do
    begin
      Result := Digits[U mod Base] + Result;
      U := U div Base;
    end;
  end;
end;

end.
