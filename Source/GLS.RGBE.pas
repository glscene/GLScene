//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.RGBE;

(* GLScene RGBE utils *)

interface

uses
  System.Classes,
  System.SysUtils,
  System.Math,

  GLS.VectorTypes,
  GLS.VectorGeometry;

procedure Float2rgbe(var RGBE: TVector4b; const Red, Green, Blue: Single);
procedure Rgbe2float(var Red, Green, Blue: Single; const RGBE: TVector4b);
procedure LoadRLEpixels(Stream: TStream; Dst: PSingle;
  Scanline_width, Num_scanlines: Integer);
procedure LoadRGBEpixels(Stream: TStream; Dst: PSingle; Numpixels: Integer);

//====================================================================
implementation
//====================================================================

type
  ERGBEexception = class(Exception);

// Extract exponent and mantissa from X
procedure Frexp(X: Extended; var Mantissa: Extended; var Exponent: Integer);
begin
  Exponent := 0;
  if (X <> 0) then
    if (Abs(X) < 0.5) then
      repeat
        X := X * 2;
        Dec(Exponent);
      until (Abs(X) >= 0.5)
    else
      while (Abs(X) >= 1) do
      begin
        X := X / 2;
        Inc(Exponent);
      end;
  Mantissa := X;
end;


function Ldexp(X: Extended; const P: Integer): Extended;
begin
  Ldexp := X * PowerSingle(2.0, P); // Result := X * (2^P)
end;

// standard conversion from float pixels to rgbe pixels
procedure Float2rgbe(var RGBE: TVector4b; const Red, Green, Blue: Single);
var
  V, M: Extended;
  E: Integer;
begin
  V := Red;
  if (Green > V) then
    V := Green;
  if (Blue > V) then
    V := Blue;
  if (V < 1E-32) then
  begin
    RGBE.X := 0;
    RGBE.Y := 0;
    RGBE.Z := 0;
    RGBE.W := 0;
  end
  else
  begin
    FrExp(V, M, E);
    M := M * 256 / V;
    RGBE.X := Floor(Red * V);
    RGBE.Y := Floor(Green * V);
    RGBE.Z := Floor(Blue * V);
    RGBE.W := Floor(E + 128);
  end;
end;

// standard conversion from rgbe to float pixels
// note: Ward uses ldexp(col+0.5,exp-(128+8)).  However we wanted pixels
// in the range [0,1] to map back into the range [0,1].
procedure Rgbe2float(var Red, Green, Blue: Single; const RGBE: TVector4b);
var
  F: Single;
begin
  if RGBE.W <> 0 then // nonzero pixel
  begin
    F := Ldexp(1.0, RGBE.W - (128 + 8));
    Red := RGBE.X * F;
    Green := RGBE.Y * F;
    Blue := RGBE.Z * F;
  end
  else
  begin
    Red := 0;
    Green := 0;
    Blue := 0;
  end;
end;

procedure LoadRLEpixels(Stream: TStream; Dst: PSingle;
  Scanline_width, Num_scanlines: Integer);
var
  RgbeTemp: TVector4b;
  Buf: TVector2b;
  Rf, Gf, Bf: Single;
  Scanline_buffer: PByteArray;
  Ptr, Ptr_end: PByte;
  I: Integer;
  Count: Cardinal;
begin
  if (Scanline_width < 8) or (Scanline_width > $7FFF) then
  begin
    // run length encoding is not allowed so read flat
    LoadRGBEPixels(Stream, Dst, Scanline_width * Num_scanlines);
    Exit;
  end;

  Scanline_buffer := nil;
  while Num_scanlines > 0 do
  begin
    Stream.Read(RgbeTemp, SizeOf(TVector4b));
    if (RgbeTemp.X <> 2) or (RgbeTemp.Y <> 2) or
      (RgbeTemp.Z and $80 <> 0) then
    begin
      // this file is not run length encoded
      Rgbe2float(Rf, Gf, Bf, RgbeTemp);
      Dst^ := Rf;
      Inc(Dst);
      Dst^ := Gf;
      Inc(Dst);
      Dst^ := Bf;
      Inc(Dst);
      if Assigned(Scanline_buffer) then
        FreeMem(Scanline_buffer);
      LoadRGBEpixels(Stream, Dst, Scanline_width * Num_scanlines - 1);
      Exit;
    end;
    if ((Integer(RgbeTemp.Z) shl 8) or RgbeTemp.W) <> Scanline_width
    then
    begin
      if Assigned(Scanline_buffer) then
        FreeMem(Scanline_buffer);
      raise ERGBEexception.Create('Wrong scanline width.');
    end;

    if not Assigned(Scanline_buffer) then
      ReallocMem(Scanline_buffer, 4 * Scanline_width);

    Ptr := PByte(Scanline_buffer);
    // read each of the four channels for the scanline into the buffer
    for I := 0 to 3 do
    begin
      Ptr_end := @Scanline_buffer[(I + 1) * Scanline_width];
      while Cardinal(Ptr) < Cardinal(Ptr_end) do
      begin
        Stream.Read(Buf, SizeOf(TVector2b));
        if Buf.X > 128 then
        begin // a run of the same value
          Count := Buf.X - 128;
          if (Count = 0) or (Count > Cardinal(Ptr_end) - Cardinal(Ptr)) then
          begin
            FreeMem(Scanline_buffer);
            raise ERGBEexception.Create('Bad HDR scanline data.');
          end;
          while Count > 0 do
          begin
            Ptr^ := Buf.Y;
            Dec(Count);
            Inc(Ptr);
          end;
        end
        else
        begin // a non-run
          Count := Buf.X;
          if (Count = 0) or (Count > Cardinal(Ptr_end) - Cardinal(Ptr)) then
          begin
            FreeMem(Scanline_buffer);
            raise ERGBEexception.Create('Bad HDR scanline data.');
          end;
          Ptr^ := Buf.Y;
          Dec(Count);
          Inc(Ptr);
          if Count > 0 then
            Stream.Read(Ptr^, Count);
          Inc(Ptr, Count);
        end;
      end;
    end;

    // now convert data from buffer into floats
    for I := 0 to Scanline_width - 1 do
    begin
      RgbeTemp.X := Scanline_buffer[I];
      RgbeTemp.Y := Scanline_buffer[I + Scanline_width];
      RgbeTemp.Z := Scanline_buffer[I + 2 * Scanline_width];
      RgbeTemp.W := Scanline_buffer[I + 3 * Scanline_width];
      Rgbe2float(Rf, Gf, Bf, RgbeTemp);
      Dst^ := Rf;
      Inc(Dst);
      Dst^ := Gf;
      Inc(Dst);
      Dst^ := Bf;
      Inc(Dst);
    end;
    Dec(Num_scanlines);
  end;
  if Assigned(Scanline_buffer) then
    FreeMem(Scanline_buffer);
end;

procedure LoadRGBEpixels(Stream: TStream; Dst: PSingle; Numpixels: Integer);
var
  RgbeTemp: TVector4b;
  Rf, Gf, Bf: Single;
begin
  while Numpixels > 0 do
  begin
    Stream.Read(RgbeTemp, SizeOf(TVector4b));
    Rgbe2float(Rf, Gf, Bf, RgbeTemp);
    Dst^ := Rf;
    Inc(Dst);
    Dst^ := Gf;
    Inc(Dst);
    Dst^ := Bf;
    Inc(Dst);
    Dec(Numpixels);
  end;
end;

end.
