{                                                                           }
{ File:       Velthuis.ExactFloatStrings.pas                                }
{ Function:   Routines to generate strings that contain the exact values    }
{             of Singles, Doubles or Extendeds.                             }
{ Language:   Delphi version XE3 or later                                   }
{ Author:     Rudy Velthuis                                                 }
{ Copyright:  (c) 2015, Rudy Velthuis                                       }
{ Notes:      Requires the Velthuis.BigIntegers unit                        }
{             Requires record helpers for intrinsic types                   }
{                                                                           }
{ License:    Redistribution and use in source and binary forms, with or    }
{             without modification, are permitted provided that the         }
{             following conditions are met:                                 }
{                                                                           }
{             * Redistributions of source code must retain the above        }
{               copyright notice, this list of conditions and the following }
{               disclaimer.                                                 }
{             * Redistributions in binary form must reproduce the above     }
{               copyright notice, this list of conditions and the following }
{               disclaimer in the documentation and/or other materials      }
{               provided with the distribution.                             }
{                                                                           }
{ Disclaimer: THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER "AS IS"     }
{             AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     }
{             LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND     }
{             FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO        }
{             EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE     }
{             FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,     }
{             OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,      }
{             PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,     }
{             DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED    }
{             AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT   }
{             LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)        }
{             ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF   }
{             ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                    }
{                                                                           }

unit Velthuis.ExactFloatStrings;

interface

uses
  System.SysUtils;

function ExactString(const F: Extended): string; overload;
function ExactString(const F: Double): string; overload;
function ExactString(const F: Single): string; overload;

implementation

uses Velthuis.BigIntegers;

// BigIntegers are required to either multiply the mantissa by powers of 5 or by powers of 2 and to
// generate a string from the resulting BigInteger.
// Record helpers for intrinsics are used to get info out of the floating point types, e.g. IsNan, Mantissa, etc.

function ExactString(const F: Extended): string;
var
  Mantissa: UInt64;
  Exponent: Integer;
  Sign: Boolean;
  BigInt: BigInteger;
  DecimalPoint: Integer;
  Len: Integer;
begin
  if F.IsNaN then
    Exit('NaN')
  else if F.IsNegativeInfinity then
    Exit('NegInfinity')
  else if F.IsPositiveInfinity then
    Exit('Infinity');

  Mantissa := F.Mantissa;
  if Mantissa = 0 then
    Exit('0.0');

  Exponent := F.Exponent - 63;
  Sign := F.Sign;

  while not Odd(Mantissa) do
  begin
    Mantissa := Mantissa shr 1;
    Inc(Exponent);
  end;

  BigInt := Mantissa;

  DecimalPoint := 0;
  if Exponent < 0 then
  begin
    // BigInt must repeatedly be divided by 2.
    // This isn't done directly: On each iteration, BigInt is multiplied by 5 and then the decimal point is moved one
    // position to the left, which is equivalent to dividing by 10. This is done in one fell swoop, using Pow().
    BigInt := BigInt * BigInteger.Pow(5, -Exponent);
    DecimalPoint := -Exponent;
  end
  else
    // BigInt must repeatedly be multipied by 2. This is done in one go, by shifting the BigInteger left by Exponent.
    BigInt := BigInt shl Exponent;

  Result := BigInt.ToString;
  Len := Length(Result);

  // Now we insert zeroes and the decimal point into the plain big integer value to get a nice output.

  if DecimalPoint = 0 then
    Result := Result + '.0'                                             // e.g. 123.0
  else if DecimalPoint >= Len then
    Result := '0.' + StringOfChar('0', DecimalPoint - Len) + Result       // e.g. 0.00123
  else
    Result := Copy(Result, 1, Len - DecimalPoint) + '.' + Copy(Result, Len - DecimalPoint + 1, Len); // e.g. 12.3

  if Sign then
    Result := '-' + Result;
end;

function ExactString(const F: Double): string;
var
  Mantissa: UInt64;
  Exponent: Integer;
  Sign: Boolean;
  BigInt: BigInteger;
  DecimalPoint: Integer;
  Len: Integer;
begin
  if F.IsNaN then
    Exit('NaN')
  else if F.IsNegativeInfinity then
    Exit('NegInfinity')
  else if F.IsPositiveInfinity then
    Exit('Infinity');

  Mantissa := F.Mantissa;
  if Mantissa = 0 then
    Exit('0.0');

  Exponent := F.Exponent - 52;
  Sign := F.Sign;
  if F.SpecialType in [fsDenormal, fsNDenormal] then
    Mantissa := Mantissa and (UInt64(-1) shr 12);

  while not Odd(Mantissa) do
  begin
    Mantissa := Mantissa shr 1;
    Inc(Exponent);
  end;

  BigInt := Mantissa;

  DecimalPoint := 0;
  if Exponent < 0 then
  begin
    // BigInt must be repeatedly divided by 2.
    // This isn't done directly: On each iteration, BigInt is multiplied by 5 and then the decimal point is moved one
    // position to the left, which is equivalent to dividing by 10. This is done in one fell swoop, using Pow().
    BigInt := BigInt * BigInteger.Pow(5, -Exponent);
    DecimalPoint := -Exponent;
  end
  else
    // BigInt must repeatedly be multipied by 2. This is done in one go, by shifting the BigInteger left.
    BigInt := BigInt shl Exponent;

  Result := BigInt.ToString;
  Len := Length(Result);

  // Now we insert zeroes and the decimal point into the plain big integer value to get a nice output.

  if DecimalPoint = 0 then
    Result := Result + '.0'                                             // e.g. 123.0
  else if DecimalPoint >= Len then
    Result := '0.' + StringOfChar('0', DecimalPoint - Len) + Result       // e.g. 0.00123
  else
    Result := Copy(Result, 1, Len - DecimalPoint) + '.' + Copy(Result, Len - DecimalPoint + 1, Len); // e.g. 12.3

  if Sign then
    Result := '-' + Result;
end;

function ExactString(const F: Single): string;
var
  Mantissa: UInt32;
  Exponent: Integer;
  Sign: Boolean;
  BigInt: BigInteger;
  DecimalPoint: Integer;
  Len: Integer;
begin
  if F.IsNaN then
    Exit('NaN')
  else if F.IsNegativeInfinity then
    Exit('NegInfinity')
  else if F.IsPositiveInfinity then
    Exit('Infinity');

  Mantissa := F.Mantissa;
  if Mantissa = 0 then
    Exit('0.0');

  Exponent := F.Exponent - 23;
  Sign := F.Sign;
  if F.SpecialType in [fsDenormal, fsNDenormal] then
    Mantissa := Mantissa and $7FFFFF;

  while not Odd(Mantissa) do
  begin
    Mantissa := Mantissa shr 1;
    Inc(Exponent);
  end;

  BigInt := Mantissa;

  DecimalPoint := 0;
  if Exponent < 0 then
  begin
    // BigInt must be repeatedly divided by 2.
    // This isn't done directly: On each iteration, BigInt is multiplied by 5 and then the decimal point is moved one
    // position to the left, which is equivalent to dividing by 10. This is done in one fell swoop, using Pow().
    BigInt := BigInt * BigInteger.Pow(5, -Exponent);
    DecimalPoint := -Exponent;
  end
  else
    // BigInt must repeatedly be multipied by 2. This is done in one go, by shifting the BigInteger left.
    BigInt := BigInt shl Exponent;

  Result := BigInt.ToString;
  Len := Length(Result);

  // Now we insert zeroes and the decimal point into the plain big integer value to get a nice output.

  if DecimalPoint = 0 then
    Result := Result + '.0'                                             // e.g. 123.0
  else if DecimalPoint >= Len then
    Result := '0.' + StringOfChar('0', DecimalPoint - Len) + Result       // e.g. 0.00123
  else
    Result := Copy(Result, 1, Len - DecimalPoint) + '.' + Copy(Result, Len - DecimalPoint + 1, Len); // e.g. 12.3

  if Sign then
    Result := '-' + Result;

end;

end.
