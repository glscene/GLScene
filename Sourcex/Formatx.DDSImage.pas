//
// The graphics engine GXScene https://github.com/glscene
//
unit Formatx.DDSImage;
(*
    Alternative for DDS unit with more supported formats of flat image:
    Alpha8, Luminance8, R3G3B2, RGB5A1, RGBA4, Alpha8Luminance8, Luminance16, R5G6B5,
    RGB8, R10G10B10A2, RGBA8, RGBA16, R16F, RGBA16F, R32F, RGBA32F, GR16, GR16F, GR32F,
    Compressed RGB S3TC DXT1, Compressed RGBA S3TC DXT1, Compressed RGBA S3TC DXT3,
    Compressed RGBA S3TC DXT5
    But it down color to RGBA8 because becomes to TBitmap
    Good for preview picture in OpenDialog,
    so you may include both DDSImage (preview) and GLFileDDS (loading)
*)
interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  FMX.Graphics,
  FMX.Types,
  
  GXS.VectorGeometry,
  GXS.Graphics,
  GXS.Context,
  GXS.TextureFormat,
  GXS.FileDDS,
  Formatx.DXTC;


type

  TDDSImage = class(TBitmap)
  public
     procedure LoadFromStream(stream: TStream); //override; -> E2170 Cannot override a non-virtual method override;
    procedure SaveToStream(stream: TStream); //override; -> E2170 Cannot override a non-virtual method override;
  end;

  EDDSException = class(Exception);

//-----------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------

// ------------------
// ------------------ TDDSImage ------------------
// ------------------

procedure TDDSImage.LoadFromStream(stream: TStream);
var
  FullDDS: TgxDDSImage;
  DDSImage: TDDSImage;  // added to replace Scanline in FMX
  BitmapData: TBitmapData;

  bCubeMap: Boolean;
  src, dst: PGLubyte;
  y: integer;
begin
  FullDDS := TgxDDSImage.Create;
  DDSImage := TDDSImage.Create;
  try
    FullDDS.LoadFromStream(stream);
    DDSImage.LoadFromStream(stream);
  except
    FullDDS.Free;
    DDSImage.Free;
    raise;
  end;

  bCubeMap := FullDDS.CubeMap;
  FullDDS.Narrow;
  // TODO : E2129 Cannot assign to a read-only property 
  (*PixelFormat := glpf32bit;*)
  // TODO : E2064 Left side cannot be assigned to 
  (*Transparent := True; *)
  Width := FullDDS.LevelWidth[0];
  Height := FullDDS.LevelHeight[0];

  src := PGLubyte(FullDDS.Data);
  if bCubeMap then
    for y := 0 to Height - 1 do
    begin
      DDSImage.Map(TMapAccess.ReadWrite, BitmapData);
      dst := BitmapData.GetScanline(y);
      BGRA32ToRGBA32(src, dst, Width);
      Inc(src, Width * 4);
    end
  else
    for y := 0 to Height - 1 do
    begin
      DDSImage.Map(TMapAccess.ReadWrite, BitmapData);
      dst := BitmapData.GetScanline(Height - 1 - y);
      BGRA32ToRGBA32(src, dst, Width);
      Inc(src, Width * 4);
    end;
  FullDDS.Free;
  DDSImage.Free;
end;

procedure TDDSImage.SaveToStream(stream: TStream);
const
  Magic: array[0..3] of AnsiChar = 'DDS ';
var
  header: TDDSHeader;
  rowSize: integer;
  i: Integer;

begin
  FillChar(header, SizeOf(TDDSHeader), 0);
  header.magic := cardinal(Magic);
  with header.SurfaceFormat do
  begin
    dwSize := sizeof(TDDSURFACEDESC2);
    dwFlags := DDSD_CAPS + DDSD_PIXELFORMAT + DDSD_WIDTH + DDSD_HEIGHT + DDSD_PITCH;
    dwWidth := Width;
    dwHeight := Height;
    ddpf.dwSize := sizeof(TDDPIXELFORMAT);
    case PixelFormat of
      TPixelFormat.RGBA:
        begin
          ddpf.dwFlags := DDPF_RGB;
          ddpf.dwRGBBitCount := 32;
          ddpf.dwRBitMask := $00FF0000;
          ddpf.dwGBitMask := $0000FF00;
          ddpf.dwBBitMask := $000000FF;
          if Transparent=1 then
          begin
            ddpf.dwFlags := ddpf.dwFlags + DDPF_ALPHAPIXELS;
            ddpf.dwRGBAlphaBitMask := $FF000000;
          end;
        end;
    else
      raise EDDSException.Create('Unsupported pixel format');
    end;
    rowSize := (ddpf.dwRGBBitCount div 8) * dwWidth;
    dwPitchOrLinearSize := dwHeight * cardinal(rowSize);
    dwCaps := DDSCAPS_TEXTURE;
    stream.Write(header, SizeOf(TDDSHeader));
    for i := 0 to Height - 1 do
      // TODO : E2003 Undeclared identifier: 'ScanLine' 
      (* stream.Write(ScanLine[i]^, rowSize);*)
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

 // TODO : E2003 Undeclared identifier: 'RegisterFileFormat' 
 (*
  TPicture.RegisterFileFormat(
    'dds', 'Microsoft DirectDraw Surface', TDDSImage);
  *)

// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------

  // TODO : E2003 Undeclared identifier: 'UnregisterGraphicClass' 
  (*
  TPicture.UnregisterGraphicClass(TDDSImage);
  *)
end.

