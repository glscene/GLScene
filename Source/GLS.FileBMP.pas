//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.FileBMP;

(* Graphic engine friendly loading of BMP image *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,

  GLS.OpenGLTokens,
  GLS.Context,
  GLS.Graphics,
  GLS.ApplicationFileIO,
  GLS.TextureFormat;

type

  TGLBMPImage = class(TGLBaseImage)
  private
    FTopDown: Boolean;
    RedMask, GreenMask, BlueMask: LongWord;
    RedShift, GreenShift, BlueShift: ShortInt;
    FLineBuffer: PByteArray;
    FReadSize: Integer;
    FDeltaX: Integer;
    FDeltaY: Integer;
    function CountBits(Value: byte): shortint;
    function ShiftCount(Mask: longword): shortint;
    function ExpandColor(Value: longword): TGLPixel32;
    procedure ExpandRLE4ScanLine(Row: Integer; Stream: TStream);
    procedure ExpandRLE8ScanLine(Row: Integer; Stream: TStream);
    function Monochrome(N: Integer): Integer;
    function Quadrochrome(N: Integer): Integer;
    function Octochrome(N: Integer): Integer;
  public
    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure AssignFromTexture(textureContext: TGLContext;
      const textureHandle: Cardinal;
      textureTarget: TGLTextureTarget;
      const CurrentFormat: boolean;
      const intFormat: TGLInternalFormat); reintroduce;
  end;

//========================================================
implementation
//========================================================

const

  BMmagic = 19778; // BMP magic word is always 19778 : 'BM'
  // Values for Compression field
  BI_RGB = 0;
  BI_RLE8 = 1;
  BI_RLE4 = 2;
  BI_BITFIELDS = 3;

type

  TBitMapFileHeader = packed record
    {00+02 :File type}
    bfType: word;
    {02+04 :File size in bytes}
    bfSize: longint;
    {06+04 : Reserved}
    bfReserved: longint;
    {10+04 : Offset of image data : size if the file hieder + the info header + palette}
    bfOffset: longint;
  end;
  PBitMapFileHeader = ^TBitMapFileHeader;

  TBitMapInfoHeader = packed record
    {14+04 : Size of the bitmap info header : sould be 40=$28}
    Size: longint;
    {18+04 : Image width in pixels}
    Width: longint;
    {22+04 : Image height in pixels}
    Height: longint;
    {26+02 : Number of image planes : should be 1 always}
    Planes: word;
    {28+02 : Color resolution : Number of bits per pixel (1,4,8,16,24,32)}
    BitCount: word;
    {30+04 : Compression Type}
    Compression: longint;
    {34+04 : Size of image data (not headers nor palette): can be 0 if no compression}
    SizeImage: longint;
    {38+04 : Horizontal resolution in pixel/meter}
    XPelsPerMeter: Longint;
    {42+04 : Vertical resolution in pixel/meter}
    YPelsPerMeter: Longint;
    {46+04 : Number of colors used}
    ClrUsed: longint;
    {50+04 : Number of imprtant colors used : usefull for displaying on VGA256}
    ClrImportant: longint;
  end;
  PBitMapInfoHeader = ^TBitMapInfoHeader;

 
procedure TGLBMPImage.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  if FileStreamExists(fileName) then
  begin
    fs := TFileStream.Create(fileName, fmOpenRead);
    try
      LoadFromStream(fs);
    finally
      fs.Free;
      ResourceName := filename;
    end;
  end
  else
    raise EInvalidRasterFile.CreateFmt('File %s not found', [filename]);
end;

procedure TGLBMPImage.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  fs := TFileStream.Create(fileName, fmOpenWrite or fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
  ResourceName := filename;
end;

function TGLBMPImage.CountBits(Value: byte): shortint;
var
  i, bits: shortint;
begin
  bits := 0;
  for i := 0 to 7 do
  begin
    if (value mod 2) <> 0 then
      inc(bits);
    value := value shr 1;
  end;
  Result := bits;
end;

function TGLBMPImage.ShiftCount(Mask: longword): shortint;
var
  tmp: shortint;
begin
  tmp := 0;
  if Mask = 0 then
  begin
    Result := 0;
    exit;
  end;

  while (Mask mod 2) = 0 do // rightmost bit is 0
  begin
    inc(tmp);
    Mask := Mask shr 1;
  end;
  tmp := tmp - (8 - CountBits(Mask and $FF));
  Result := tmp;
end;

function TGLBMPImage.ExpandColor(Value: longword): TGLPixel32;
var
  tmpr, tmpg, tmpb: longword;
begin
  tmpr := value and RedMask;
  tmpg := value and GreenMask;
  tmpb := value and BlueMask;
  if RedShift < 0 then
    Result.R := byte(tmpr shl (-RedShift))
  else
    Result.R := byte(tmpr shr RedShift);
  if GreenShift < 0 then
    Result.G := byte(tmpg shl (-GreenShift))
  else
    Result.G := byte(tmpg shr GreenShift);
  if BlueShift < 0 then
    Result.B := byte(tmpb shl (-BlueShift))
  else
    Result.B := byte(tmpb shr BlueShift);
end;

function TGLBMPImage.Monochrome(N: Integer): Integer;
begin
  Result := (FLineBuffer[N div 8] shr (7 - (N and 7))) and 1;
end;

function TGLBMPImage.Quadrochrome(N: Integer): Integer;
begin
  Result := (FLineBuffer[N div 2] shr (((N + 1) and 1) * 4)) and $0F;
end;

function TGLBMPImage.Octochrome(N: Integer): Integer;
begin
  Result := FLineBuffer[N];
end;

procedure TGLBMPImage.LoadFromStream(stream: TStream);
type
  TBitShiftFunc = function(N: Integer): Integer of object;
var
  LHeader: TBitMapFileHeader;
  LInfo: TBitMapInfoHeader;
  BadCompression: Boolean;
  Ptr: PByte;
  BitCount, LineSize: Integer;
  Row: Integer;
  nPalette: Integer;
  LPalette: array of TGLPixel32;
  BitShiftFunc: TBitShiftFunc;

  procedure ReadScanLine;
  var
    I: Integer;
  begin
    if nPalette > 0 then
    begin
      Stream.Read(FLineBuffer[0], FReadSize);
      for I := LInfo.Width - 1 downto 0 do
        PGLPixel32Array(Ptr)[I] := LPalette[BitShiftFunc(I)];
    end
    else if LInfo.Compression = BI_RLE8 then
    begin
      ExpandRLE8ScanLine(Row, Stream);
      Move(FLineBuffer[0], Ptr^, LineSize);
    end
    else if LInfo.Compression = BI_RLE4 then
    begin
      ExpandRLE4ScanLine(Row, Stream);
      Move(FLineBuffer[0], Ptr^, LineSize);
    end
    else if LInfo.BitCount = 16 then
    begin
      Stream.Read(FLineBuffer[0], FReadSize);
      for I := LInfo.Width - 1 downto 0 do
        PGLPixel32Array(Ptr)[I] := ExpandColor(PWordArray(FLineBuffer)[I]);
    end
    else
      Stream.Read(Ptr^, FReadSize);

    Inc(Ptr, LineSize);
  end;

begin
  stream.Read(LHeader, SizeOf(TBitMapFileHeader));
  if LHeader.bfType <> BMmagic then
    raise EInvalidRasterFile.Create('Invalid BMP header');

  stream.Read(LInfo, SizeOf(TBitMapInfoHeader));
  stream.Position := stream.Position - SizeOf(TBitMapInfoHeader) + LInfo.Size;

  BadCompression := false;
  if ((LInfo.Compression = BI_RLE4) and (LInfo.BitCount <> 4)) then
    BadCompression := true;
  if ((LInfo.Compression = BI_RLE8) and (LInfo.BitCount <> 8)) then
    BadCompression := true;
  if ((LInfo.Compression = BI_BITFIELDS) and (not (LInfo.BitCount in [16, 32]))) then
    BadCompression := true;
  if not (LInfo.Compression in [BI_RGB..BI_BITFIELDS]) then
    BadCompression := true;
  if BadCompression then
    raise EInvalidRasterFile.Create('Bad BMP compression mode');
  FTopDown := (LInfo.Height < 0);
  LInfo.Height := abs(LInfo.Height);
  if (FTopDown and (not (LInfo.Compression in [BI_RGB, BI_BITFIELDS]))) then
    raise EInvalidRasterFile.Create('Top-down bitmaps cannot be compressed');

  nPalette := 0;
  if ((LInfo.Compression = BI_RGB)
    and (LInfo.BitCount = 16)) then // 5 bits per channel, fixed mask
  begin
    RedMask := $7C00;
    RedShift := 7;
    GreenMask := $03E0;
    GreenShift := 2;
    BlueMask := $001F;
    BlueShift := -3;
  end
  else if ((LInfo.Compression = BI_BITFIELDS)
    and (LInfo.BitCount in [16, 32])) then // arbitrary mask
  begin
    Stream.Read(RedMask, 4);
    Stream.Read(GreenMask, 4);
    Stream.Read(BlueMask, 4);
    RedShift := ShiftCount(RedMask);
    GreenShift := ShiftCount(GreenMask);
    BlueShift := ShiftCount(BlueMask);
  end
  else if LInfo.BitCount in [1, 4, 8] then
  begin
    nPalette := 1 shl LInfo.BitCount;
    SetLength(LPalette, nPalette);
    if LInfo.ClrUsed > 0 then
      Stream.Read(LPalette[0], LInfo.ClrUsed * SizeOf(TGLPixel32))
    else // Seems to me that this is dangerous.
      Stream.Read(LPalette[0], nPalette * SizeOf(TGLPixel32));
  end
  else if LInfo.ClrUsed > 0 then { Skip palette }
    Stream.Position := Stream.Position + LInfo.ClrUsed * 3;

  UnMipmap;
  FLOD[0].Width := LInfo.Width;
  FLOD[0].Height := LInfo.Height;
  FLOD[0].Depth := 0;
  BitCount := 0;
  FColorFormat := GL_BGRA;
  FInternalFormat := tfRGBA8;
  FElementSize := 4;

  case LInfo.BitCount of
    1:
      begin
        BitCount := 1;
        BitShiftFunc := Monochrome;
      end;
    4:
      begin
        BitCount := 4;
        BitShiftFunc := Quadrochrome;
      end;
    8:
      begin
        BitCount := 8;
        BitShiftFunc := Octochrome;
      end;
    16:
      BitCount := 16;
    24:
      begin
        BitCount := 24;
        FColorFormat := GL_BGR;
        FInternalFormat := tfRGB8;
        FElementSize := 3;
      end;
    32:
      BitCount := 32;
  end;

  FDataType := GL_UNSIGNED_BYTE;
  FCubeMap := False;
  FTextureArray := False;
  ReallocMem(FData, DataSize);

  FDeltaX := -1;
  FDeltaY := -1;
  Ptr := PByte(FData);
  LineSize := GetWidth * FElementSize;

  FReadSize := ((LInfo.Width * BitCount + 31) div 32) shl 2;
  GetMem(FLineBuffer, FReadSize);

  try
    if FTopDown then
      for Row := 0 to GetHeight - 1 do // A rare case of top-down bitmap!
        ReadScanLine
    else
      for Row := GetHeight - 1 downto 0 do
        ReadScanLine;
  finally
    FreeMem(FLineBuffer);
  end;

end;

procedure TGLBMPImage.ExpandRLE4ScanLine(Row: Integer; Stream: TStream);
var
  i, j, tmpsize: integer;
  b0, b1: byte;
  nibline: PByteArray;
  even: boolean;
begin
  tmpsize := FReadSize * 2; { ReadSize is in bytes, while nibline is made of nibbles, so it's 2*readsize long }
  getmem(nibline, tmpsize);
  try
    i := 0;
    while true do
    begin
      { let's see if we must skip pixels because of delta... }
      if FDeltaY <> -1 then
      begin
        if Row = FDeltaY then
          j := FDeltaX { If we are on the same line, skip till DeltaX }
        else
          j := tmpsize; { else skip up to the end of this line }
        while (i < j) do
        begin
          NibLine[i] := 0;
          inc(i);
        end;

        if Row = FDeltaY then { we don't need delta anymore }
          FDeltaY := -1
        else
          break; { skipping must continue on the next line, we are finished here }
      end;

      Stream.Read(b0, 1);
      Stream.Read(b1, 1);
      if b0 <> 0 then { number of repetitions }
      begin
        if b0 + i > tmpsize then
          raise EInvalidRasterFile.Create('Bad BMP RLE chunk at row ' + inttostr(row) + ', col ' + inttostr(i) + ', file offset $' + inttohex(Stream.Position, 16));
        even := true;
        j := i + b0;
        while (i < j) do
        begin
          if even then
            NibLine[i] := (b1 and $F0) shr 4
          else
            NibLine[i] := b1 and $0F;
          inc(i);
          even := not even;
        end;
      end
      else
        case b1 of
          0: break; { end of line }
          1: break; { end of file }
          2:
            begin { Next pixel position. Skipped pixels should be left untouched, but we set them to zero }
              Stream.Read(b0, 1);
              Stream.Read(b1, 1);
              FDeltaX := i + b0;
              FDeltaY := Row + b1;
            end
        else
          begin { absolute mode }
            if b1 + i > tmpsize then
              raise EInvalidRasterFile.Create('Bad BMP RLE chunk at row ' + inttostr(row) + ', col ' + inttostr(i) + ', file offset $' + inttohex(Stream.Position, 16));
            j := i + b1;
            even := true;
            while (i < j) do
            begin
              if even then
              begin
                Stream.Read(b0, 1);
                NibLine[i] := (b0 and $F0) shr 4;
              end
              else
                NibLine[i] := b0 and $0F;
              inc(i);
              even := not even;
            end;
            { aligned on 2 bytes boundary: see rle8 for details  }
            b1 := b1 + (b1 mod 2);
            if (b1 mod 4) <> 0 then
              Stream.Seek(1, soFromCurrent);
          end;
        end;
    end;
    { pack the nibline into the linebuf }
    for i := 0 to FReadSize - 1 do
      FLineBuffer[i] := (NibLine[i * 2] shl 4) or NibLine[i * 2 + 1];
  finally
    FreeMem(nibline)
  end;
end;

procedure TGLBMPImage.ExpandRLE8ScanLine(Row: Integer; Stream: TStream);
var
  i, j: integer;
  b0, b1: byte;
begin
  i := 0;
  while true do
  begin
    { let's see if we must skip pixels because of delta... }
    if FDeltaY <> -1 then
    begin
      if Row = FDeltaY then
        j := FDeltaX { If we are on the same line, skip till DeltaX }
      else
        j := FReadSize; { else skip up to the end of this line }
      while (i < j) do
      begin
        FLineBuffer[i] := 0;
        inc(i);
      end;

      if Row = FDeltaY then { we don't need delta anymore }
        FDeltaY := -1
      else
        break; { skipping must continue on the next line, we are finished here }
    end;

    Stream.Read(b0, 1);
    Stream.Read(b1, 1);
    if b0 <> 0 then { number of repetitions }
    begin
      if b0 + i > FReadSize then
        raise EInvalidRasterFile.Create('Bad BMP RLE chunk at row ' + inttostr(row) + ', col ' + inttostr(i) + ', file offset $' + inttohex(Stream.Position, 16));
      j := i + b0;
      while (i < j) do
      begin
        FLineBuffer[i] := b1;
        inc(i);
      end;
    end
    else
      case b1 of
        0: break; { end of line }
        1: break; { end of file }
        2:
          begin { Next pixel position. Skipped pixels should be left untouched, but we set them to zero }
            Stream.Read(b0, 1);
            Stream.Read(b1, 1);
            FDeltaX := i + b0;
            FDeltaY := Row + b1;
          end
      else
        begin { absolute mode }
          if b1 + i > FReadSize then
            raise EInvalidRasterFile.Create('Bad BMP RLE chunk at row ' + inttostr(row) + ', col ' + inttostr(i) + ', file offset $' + inttohex(Stream.Position, 16));
          Stream.Read(FLineBuffer[i], b1);
          inc(i, b1);
          { aligned on 2 bytes boundary: every group starts on a 2 bytes boundary, but absolute group
            could end on odd address if there is a odd number of elements, so we pad it  }
          if (b1 mod 2) <> 0 then
            Stream.Seek(1, soFromCurrent);
        end;
      end;
  end;
end;

procedure TGLBMPImage.SaveToStream(stream: TStream);
begin
  {$Message Hint 'TGLBMPImage.SaveToStream not yet implemented' }
end;

procedure TGLBMPImage.AssignFromTexture(textureContext: TGLContext;
  const textureHandle: Cardinal; textureTarget: TGLTextureTarget;
  const CurrentFormat: boolean; const intFormat: TGLInternalFormat);
begin
  {$Message Hint 'TGLBMPImage.AssignFromTexture not yet implemented' }
end;

class function TGLBMPImage.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead {, dfcWrite}];
end;

//=============================================================
initialization
//=============================================================

  { Register this Fileformat-Handler with GLScene }
  RegisterRasterFormat('bmp', 'Bitmap Image File', TGLBMPImage);

end.

