//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit Formats.TGA;

(* Graphic engine friendly loading of TGA image. *)

interface

{.$I GLScene.inc}

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

  TGLTGAImage = class(TGLBaseImage)
  public
    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure AssignFromTexture(textureContext: TGLContext;
      const textureHandle: Cardinal; textureTarget: TGLTextureTarget;
      const CurrentFormat: boolean; const intFormat: TGLInternalFormat);
      reintroduce;
  end;

//===============================================================
implementation
//===============================================================

type

  TTGAFileHeader = packed record
    IDLength: Byte;
    ColorMapType: Byte;
    ImageType: Byte;
    ColorMapOrigin: Word;
    ColorMapLength: Word;
    ColorMapEntrySize: Byte;
    XOrigin: Word;
    YOrigin: Word;
    Width: Word;
    Height: Word;
    PixelSize: Byte;
    ImageDescriptor: Byte;
  end;

procedure ReadAndUnPackRLETGA24(stream: TStream; destBuf: PAnsiChar;
  totalSize: Integer);
type
  TRGB24 = packed record
    r, g, b: Byte;
  end;
  PRGB24 = ^TRGB24;
var
  n: Integer;
  color: TRGB24;
  bufEnd: PAnsiChar;
  b: Byte;
begin
  bufEnd := @destBuf[totalSize];
  while destBuf < bufEnd do
  begin
    stream.Read(b, 1);
    if b >= 128 then
    begin
      // repetition packet
      stream.Read(color, 3);
      b := (b and 127) + 1;
      while b > 0 do
      begin
        PRGB24(destBuf)^ := color;
        Inc(destBuf, 3);
        Dec(b);
      end;
    end
    else
    begin
      n := ((b and 127) + 1) * 3;
      stream.Read(destBuf^, n);
      Inc(destBuf, n);
    end;
  end;
end;

procedure ReadAndUnPackRLETGA32(stream: TStream; destBuf: PAnsiChar;
  totalSize: Integer);
type
  TRGB32 = packed record
    r, g, b, a: Byte;
  end;
  PRGB32 = ^TRGB32;
var
  n: Integer;
  color: TRGB32;
  bufEnd: PAnsiChar;
  b: Byte;
begin
  bufEnd := @destBuf[totalSize];
  while destBuf < bufEnd do
  begin
    stream.Read(b, 1);
    if b >= 128 then
    begin
      // repetition packet
      stream.Read(color, 4);
      b := (b and 127) + 1;
      while b > 0 do
      begin
        PRGB32(destBuf)^ := color;
        Inc(destBuf, 4);
        Dec(b);
      end;
    end
    else
    begin
      n := ((b and 127) + 1) * 4;
      stream.Read(destBuf^, n);
      Inc(destBuf, n);
    end;
  end;
end;

procedure TGLTGAImage.LoadFromFile(const filename: string);
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

procedure TGLTGAImage.SaveToFile(const filename: string);
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

procedure TGLTGAImage.LoadFromStream(stream: TStream);
var
  LHeader: TTGAFileHeader;
  y, rowSize, bufSize: Integer;
  verticalFlip: Boolean;
  unpackBuf: PAnsiChar;
  Ptr: PByte;
begin
  stream.Read(LHeader, Sizeof(TTGAFileHeader));

  if LHeader.ColorMapType <> 0 then
    raise EInvalidRasterFile.Create('ColorMapped TGA unsupported');

  UnMipmap;
  FLOD[0].Width := LHeader.Width;
  FLOD[0].Height := LHeader.Height;
  FLOD[0].Depth := 0;

  case LHeader.PixelSize of
    24:
      begin
        FColorFormat := GL_BGR;
        FInternalFormat := tfRGB8;
        FElementSize := 3;
      end;
    32:
      begin
        FColorFormat := GL_RGBA;
        FInternalFormat := tfRGBA8;
        FElementSize := 4;
      end;
  else
    raise EInvalidRasterFile.Create('Unsupported TGA ImageType');
  end;

  FDataType := GL_UNSIGNED_BYTE;
  FCubeMap := False;
  FTextureArray := False;
  ReallocMem(FData, DataSize);

  rowSize := GetWidth * FElementSize;
  verticalFlip := ((LHeader.ImageDescriptor and $20) <> 1);

  if LHeader.IDLength > 0 then
    stream.Seek(LHeader.IDLength, soFromCurrent);

  case LHeader.ImageType of
    2:
      begin // uncompressed RGB/RGBA
        if verticalFlip then
        begin
          Ptr := PByte(FData);
          Inc(Ptr, rowSize * (GetHeight - 1));
          for y := 0 to GetHeight - 1 do
          begin
            stream.Read(Ptr^, rowSize);
            Dec(Ptr, rowSize);
          end;
        end
        else
          stream.Read(FData^, rowSize * GetHeight);
      end;
    10:
      begin // RLE encoded RGB/RGBA
        bufSize := GetHeight * rowSize;
        GetMem(unpackBuf, bufSize);
        try
          // read & unpack everything
          if LHeader.PixelSize = 24 then
            ReadAndUnPackRLETGA24(stream, unpackBuf, bufSize)
          else
            ReadAndUnPackRLETGA32(stream, unpackBuf, bufSize);
          // fillup bitmap
          if verticalFlip then
          begin
            Ptr := PByte(FData);
            Inc(Ptr, rowSize * (GetHeight - 1));
            for y := 0 to GetHeight - 1 do
            begin
              Move(unPackBuf[y * rowSize], Ptr^, rowSize);
              Dec(Ptr, rowSize);
            end;
          end
          else
            Move(unPackBuf[rowSize * GetHeight], FData^, rowSize * GetHeight);
        finally
          FreeMem(unpackBuf);
        end;
      end;
  else
    raise EInvalidRasterFile.CreateFmt('Unsupported TGA ImageType %d',
      [LHeader.ImageType]);
  end;
end;

procedure TGLTGAImage.SaveToStream(stream: TStream);
begin
{$MESSAGE Hint 'TGLTGAImage.SaveToStream not yet implemented' }
end;

procedure TGLTGAImage.AssignFromTexture(textureContext: TGLContext;
  const textureHandle: Cardinal; textureTarget: TGLTextureTarget;
  const CurrentFormat: boolean; const intFormat: TGLInternalFormat);
begin
{$MESSAGE Hint 'TGLTGAImage.AssignFromTexture not yet implemented' }
end;

class function TGLTGAImage.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead {, dfcWrite}];
end;

//-------------------------------------------
initialization
//-------------------------------------------

  RegisterRasterFormat('tga', 'TARGA Image File', TGLTGAImage);

end.
