//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.ImageUtils;

(* Main purpose is as a fallback in cases where there is no other way to process images *)

// TODO: Complite InfToXXX
// TODO: BPTC decompression
// TODO: S3TC compression
// TODO: LATC compression
// TODO: RGTC compression
// TODO: BPTC compression
// TODO: Build3DMipmap

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils,
  System.Classes,
  System.Math,

  GLS.OpenGLTokens,
  GLS.Strings,
  GLS.TextureFormat,
  GLS.VectorGeometry,
  GLS.Utils;

var
  vImageScaleFilterWidth: Integer = 5; // Relative sample radius for filtering

type

  TIntermediateFormat = record
    R, G, B, A: Single;
  end;

  TPointerArray = array of Pointer;

  PRGBA32F = ^TIntermediateFormat;
  TIntermediateFormatArray = array [0 .. MaxInt div (2 * SizeOf(TIntermediateFormat))] of TIntermediateFormat;
  PIntermediateFormatArray = ^TIntermediateFormatArray;

  TU48BitBlock = array [0 .. 3, 0 .. 3] of Byte;
  T48BitBlock = array [0 .. 3, 0 .. 3] of SmallInt;

  EGLImageUtils = class(Exception);

  TImageFilterFunction = function(Value: Single): Single;
  TImageAlphaProc = procedure(var AColor: TIntermediateFormat);

function ImageBoxFilter(Value: Single): Single;
function ImageTriangleFilter(Value: Single): Single;
function ImageHermiteFilter(Value: Single): Single;
function ImageBellFilter(Value: Single): Single;
function ImageSplineFilter(Value: Single): Single;
function ImageLanczos3Filter(Value: Single): Single;
function ImageMitchellFilter(Value: Single): Single;

procedure ImageAlphaFromIntensity(var AColor: TIntermediateFormat);
procedure ImageAlphaSuperBlackTransparent(var AColor: TIntermediateFormat);
procedure ImageAlphaLuminance(var AColor: TIntermediateFormat);
procedure ImageAlphaLuminanceSqrt(var AColor: TIntermediateFormat);
procedure ImageAlphaOpaque(var AColor: TIntermediateFormat);
procedure ImageAlphaTopLeftPointColorTransparent(var AColor: TIntermediateFormat);
procedure ImageAlphaInverseLuminance(var AColor: TIntermediateFormat);
procedure ImageAlphaInverseLuminanceSqrt(var AColor: TIntermediateFormat);
procedure ImageAlphaBottomRightPointColorTransparent(var AColor: TIntermediateFormat);

procedure ConvertImage(const ASrc: Pointer; const ADst: Pointer; ASrcColorFormat, ADstColorFormat: Cardinal; ASrcDataType, ADstDataType: Cardinal; AWidth, AHeight: Integer);
procedure RescaleImage(const ASrc: Pointer; const ADst: Pointer; AColorFormat: Cardinal; ADataType: Cardinal; AFilter: TImageFilterFunction; ASrcWidth, ASrcHeight, ADstWidth, ADstHeight: Integer);
procedure Build2DMipmap(const ASrc: Pointer; const ADst: TPointerArray; AColorFormat: Cardinal; ADataType: Cardinal; AFilter: TImageFilterFunction; ASrcWidth, ASrcHeight: Integer);
procedure AlphaGammaBrightCorrection(const ASrc: Pointer; AColorFormat: Cardinal; ADataType: Cardinal; ASrcWidth, ASrcHeight: Integer; anAlphaProc: TImageAlphaProc; ABrightness: Single; AGamma: Single);

//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------

const
  cSuperBlack: TIntermediateFormat = (R: 0.0; G: 0.0; B: 0.0; A: 0.0);

type
  TConvertToImfProc = procedure(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  TConvertFromInfProc = procedure(ASource: PIntermediateFormatArray; ADest: Pointer; AColorFormat: Cardinal; AWidth, AHeight: Integer);

procedure Swap(var A, B: Integer); inline;
var
  C: Integer;
begin
  C := A;
  A := B;
  B := C;
end;

// ------------------------------ OpenGL format image to RGBA Float 

procedure UnsupportedToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  begin
    raise EGLImageUtils.Create('Unimplemented type of conversion');
  end;

procedure UbyteToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PByte;
    n: Integer;
    c0: Single;

    function GetChannel: Single;
      begin
        Result := pSource^;
        Inc(pSource);
      end;

  begin
    pSource := PByte(ASource);

    case AColorFormat of
//{$I ImgUtilCaseGL2Imf.inc}
    GL_RGB, GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_BGR, GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_ALPHA, GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := GetChannel;
      end;
    GL_LUMINANCE, GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := 255.0;
      end;
    GL_LUMINANCE_ALPHA, GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := GetChannel;
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := c0;
      end;
    GL_RED, GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_GREEN, GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_BLUE, GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := GetChannel;
        ADest[n].A := 255;
      end;
    GL_RG, GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure Ubyte332ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PByte;
    c0, c1, c2, c3: Byte;
    n: Integer;

    procedure GetChannel;
      begin
        c0 := pSource^;
        c1 := $E0 and c0;
        c2 := $E0 and (c0 shl 3);
        c3 := $C0 and (c0 shl 6);
        Inc(pSource);
      end;

  begin
    pSource := PByte(ASource);

    case AColorFormat of

      GL_RGB:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].R := c1;
          ADest[n].G := c2;
          ADest[n].B := c3;
        end;

      GL_BGR:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].B := c1;
          ADest[n].G := c2;
          ADest[n].R := c3;
        end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure Ubyte233RToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PByte;
    c0, c1, c2, c3: Byte;
    n: Integer;

    procedure GetChannel;
      begin
        c0 := pSource^;
        c3 := $E0 and c0;
        c2 := $E0 and (c0 shl 3);
        c1 := $C0 and (c0 shl 6);
        Inc(pSource);
      end;

  begin
    pSource := PByte(ASource);

    case AColorFormat of

      GL_RGB:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].R := c1;
          ADest[n].G := c2;
          ADest[n].B := c3;
        end;

      GL_BGR:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].B := c1;
          ADest[n].G := c2;
          ADest[n].R := c3;
        end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure ByteToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PShortInt;
    n: Integer;
    c0: Single;

    function GetChannel: Single;
      begin
        Result := pSource^;
        Inc(pSource);
      end;

  begin
    pSource := PShortInt(ASource);
    case AColorFormat of
//{$I ImgUtilCaseGL2Imf.inc}
    GL_RGB, GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_BGR, GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_ALPHA, GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := GetChannel;
      end;
    GL_LUMINANCE, GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := 255.0;
      end;
    GL_LUMINANCE_ALPHA, GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := GetChannel;
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := c0;
      end;
    GL_RED, GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_GREEN, GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_BLUE, GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := GetChannel;
        ADest[n].A := 255;
      end;
    GL_RG, GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure UShortToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PWord;
    n: Integer;
    c0: Single;

    function GetChannel: Single;
      begin
        Result := pSource^ / $100;
        Inc(pSource);
      end;

  begin
    pSource := PWord(ASource);

    case AColorFormat of
    GL_RGB, GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_BGR, GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_ALPHA, GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := GetChannel;
      end;
    GL_LUMINANCE, GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := 255.0;
      end;
    GL_LUMINANCE_ALPHA, GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := GetChannel;
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := c0;
      end;
    GL_RED, GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_GREEN, GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_BLUE, GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := GetChannel;
        ADest[n].A := 255;
      end;
    GL_RG, GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure ShortToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PSmallInt;
    n: Integer;
    c0: Single;

    function GetChannel: Single;
      begin
        Result := pSource^ / $100;
        Inc(pSource);
      end;

  begin
    pSource := PSmallInt(ASource);

    case AColorFormat of
    GL_RGB, GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_BGR, GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_ALPHA, GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := GetChannel;
      end;
    GL_LUMINANCE, GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := 255.0;
      end;
    GL_LUMINANCE_ALPHA, GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := GetChannel;
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := c0;
      end;
    GL_RED, GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_GREEN, GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_BLUE, GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := GetChannel;
        ADest[n].A := 255;
      end;
    GL_RG, GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure UIntToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PLongWord;
    n: Integer;
    c0: Single;

    function GetChannel: Single;
      begin
        Result := pSource^ / $1000000;
        Inc(pSource);
      end;

  begin
    pSource := PLongWord(ASource);

    case AColorFormat of
    GL_RGB, GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_BGR, GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_ALPHA, GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := GetChannel;
      end;
    GL_LUMINANCE, GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := 255.0;
      end;
    GL_LUMINANCE_ALPHA, GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := GetChannel;
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := c0;
      end;
    GL_RED, GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_GREEN, GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_BLUE, GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := GetChannel;
        ADest[n].A := 255;
      end;
    GL_RG, GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure IntToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PLongInt;
    n: Integer;
    c0: Single;

    function GetChannel: Single;
      begin
        Result := pSource^ / $1000000;
        Inc(pSource);
      end;

  begin
    pSource := PLongInt(ASource);

    case AColorFormat of
    GL_RGB, GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_BGR, GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_ALPHA, GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := GetChannel;
      end;
    GL_LUMINANCE, GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := 255.0;
      end;
    GL_LUMINANCE_ALPHA, GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := GetChannel;
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := c0;
      end;
    GL_RED, GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_GREEN, GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_BLUE, GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := GetChannel;
        ADest[n].A := 255;
      end;
    GL_RG, GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure FloatToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PSingle;
    n: Integer;
    c0: Single;

    function GetChannel: Single;
      begin
        Result := pSource^ * 255.0;
        Inc(pSource);
      end;

  begin
    pSource := PSingle(ASource);

    case AColorFormat of
    GL_RGB, GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_BGR, GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_ALPHA, GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := GetChannel;
      end;
    GL_LUMINANCE, GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := 255.0;
      end;
    GL_LUMINANCE_ALPHA, GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := GetChannel;
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := c0;
      end;
    GL_RED, GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_GREEN, GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_BLUE, GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := GetChannel;
        ADest[n].A := 255;
      end;
    GL_RG, GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure HalfFloatToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PHalfFloat;
    n: Integer;
    c0: Single;

    function GetChannel: Single;
      begin
        Result := HalfToFloat(pSource^) * 255.0;
        Inc(pSource);
      end;

  begin
    pSource := PHalfFloat(ASource);

    case AColorFormat of
    GL_RGB, GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_BGR, GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := 255.0;
      end;
    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].B := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].R := GetChannel;
        ADest[n].A := GetChannel;
      end;
    GL_ALPHA, GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := GetChannel;
      end;
    GL_LUMINANCE, GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := 255.0;
      end;
    GL_LUMINANCE_ALPHA, GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := GetChannel;
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        c0 := GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c0;
        ADest[n].B := c0;
        ADest[n].A := c0;
      end;
    GL_RED, GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := 0;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_GREEN, GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    GL_BLUE, GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := 0;
        ADest[n].G := 0;
        ADest[n].B := GetChannel;
        ADest[n].A := 255;
      end;
    GL_RG, GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        ADest[n].R := GetChannel;
        ADest[n].G := GetChannel;
        ADest[n].B := 0;
        ADest[n].A := 255;
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure UInt8888ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PByte;
    n: Integer;
    c0, c1, c2, c3: Byte;

    procedure GetChannel;
      begin
        c0 := pSource^;
        Inc(pSource);
        c1 := pSource^;
        Inc(pSource);
        c2 := pSource^;
        Inc(pSource);
        c3 := pSource^;
        Inc(pSource);
      end;

  begin
    pSource := PByte(ASource);

    case AColorFormat of

      GL_RGBA, GL_RGBA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].R := c0;
          ADest[n].G := c1;
          ADest[n].B := c2;
          ADest[n].A := c3;
        end;

      GL_BGRA, GL_BGRA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].B := c0;
          ADest[n].G := c1;
          ADest[n].R := c2;
          ADest[n].A := c3;
        end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure UInt8888RevToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PByte;
    n: Integer;
    c0, c1, c2, c3: Byte;

    procedure GetChannel;
      begin
        c3 := pSource^;
        Inc(pSource);
        c2 := pSource^;
        Inc(pSource);
        c1 := pSource^;
        Inc(pSource);
        c0 := pSource^;
        Inc(pSource);
      end;

  begin
    pSource := PByte(ASource);

    case AColorFormat of

      GL_RGBA, GL_RGBA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].R := c0;
          ADest[n].G := c1;
          ADest[n].B := c2;
          ADest[n].A := c3;
        end;

      GL_BGRA, GL_BGRA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].B := c0;
          ADest[n].G := c1;
          ADest[n].R := c2;
          ADest[n].A := c3;
        end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure UShort4444ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PByte;
    n: Integer;
    c0, c1, c2, c3, c4: Byte;

    procedure GetChannel;
      begin
        c0 := pSource^;
        c3 := $F0 and (c0 shl 4);
        c4 := $F0 and c0;
        Inc(pSource);
        c0 := pSource^;
        c1 := $F0 and (c0 shl 4);
        c2 := $F0 and c0;
        Inc(pSource);
      end;

  begin
    pSource := PByte(ASource);

    case AColorFormat of

      GL_RGBA, GL_RGBA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].R := c1;
          ADest[n].G := c2;
          ADest[n].B := c3;
          ADest[n].A := c4;
        end;

      GL_BGRA, GL_BGRA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].R := c1;
          ADest[n].G := c2;
          ADest[n].B := c3;
          ADest[n].A := c4;
        end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure UShort4444RevToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PByte;
    n: Integer;
    c0, c1, c2, c3, c4: Byte;

    procedure GetChannel;
      begin
        c0 := pSource^;
        c1 := $F0 and (c0 shl 4);
        c2 := $F0 and c0;
        Inc(pSource);
        c0 := pSource^;
        c3 := $F0 and (c0 shl 4);
        c4 := $F0 and c0;
        Inc(pSource);
      end;

  begin
    pSource := PByte(ASource);

    case AColorFormat of

      GL_RGBA, GL_RGBA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].R := c1;
          ADest[n].G := c2;
          ADest[n].B := c3;
          ADest[n].A := c4;
        end;

      GL_BGRA, GL_BGRA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].B := c1;
          ADest[n].G := c2;
          ADest[n].R := c3;
          ADest[n].A := c4;
        end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure UShort565ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PWord;
    n: Integer;
    c0: Word;
    c1, c2, c3: Byte;

    procedure GetChannel;
      begin
        c0 := pSource^;
        c3 := (c0 and $001F) shl 3;
        c2 := (c0 and $07E0) shr 3;
        c1 := (c0 and $F800) shr 8;
        Inc(pSource);
      end;

  begin
    pSource := PWord(ASource);

    case AColorFormat of

      GL_RGB, GL_RGB_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].R := c1;
          ADest[n].G := c2;
          ADest[n].B := c3;
        end;

      GL_BGR, GL_BGR_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].B := c1;
          ADest[n].G := c2;
          ADest[n].R := c3;
        end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure UShort565RevToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PWord;
    n: Integer;
    c0: Word;
    c1, c2, c3: Byte;

    procedure GetChannel;
      begin
        c0 := pSource^;
        c1 := (c0 and $001F) shl 3;
        c2 := (c0 and $07E0) shr 3;
        c3 := (c0 and $F800) shr 8;
        Inc(pSource);
      end;

  begin
    pSource := PWord(ASource);

    case AColorFormat of

      GL_RGB, GL_RGB_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].R := c1;
          ADest[n].G := c2;
          ADest[n].B := c3;
        end;

      GL_BGR, GL_BGR_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].B := c1;
          ADest[n].G := c2;
          ADest[n].R := c3;
        end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure UShort5551ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PWord;
    n: Integer;
    c0: Word;
    c1, c2, c3, c4: Byte;

    procedure GetChannel;
      begin
        c0 := pSource^;
        c4 := (c0 and $001F) shl 3;
        c3 := (c0 and $03E0) shr 2;
        c2 := (c0 and $7C00) shr 7;
        c1 := (c0 and $8000) shr 8;
        Inc(pSource);
      end;

  begin
    pSource := PWord(ASource);

    case AColorFormat of

      GL_RGBA, GL_RGBA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].R := c1;
          ADest[n].G := c2;
          ADest[n].B := c3;
          ADest[n].A := c4;
        end;

      GL_BGRA, GL_BGRA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].B := c1;
          ADest[n].G := c2;
          ADest[n].R := c3;
          ADest[n].A := c4;
        end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure UShort5551RevToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PWord;
    n: Integer;
    c0: Word;
    c1, c2, c3, c4: Byte;

    procedure GetChannel;
      begin
        c0 := pSource^;
        c1 := (c0 and $001F) shl 3;
        c2 := (c0 and $03E0) shr 2;
        c3 := (c0 and $7C00) shr 7;
        c4 := (c0 and $8000) shr 8;
        Inc(pSource);
      end;

  begin
    pSource := PWord(ASource);

    case AColorFormat of

      GL_RGBA, GL_RGBA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].R := c1;
          ADest[n].G := c2;
          ADest[n].B := c3;
          ADest[n].A := c4;
        end;

      GL_BGRA, GL_BGRA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].B := c1;
          ADest[n].G := c2;
          ADest[n].R := c3;
          ADest[n].A := c4;
        end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure UInt_10_10_10_2_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PLongWord;
    n: Integer;
    c0: LongWord;
    c1, c2, c3, c4: Word;

    procedure GetChannel;
      begin
        c0 := pSource^;
        c1 := (c0 and $000003FF) shl 6;
        c2 := (c0 and $000FFC00) shr 4;
        c3 := (c0 and $3FF00000) shr 14;
        c4 := (c0 and $C0000000) shr 16;
        Inc(pSource);
      end;

  begin
    pSource := PLongWord(ASource);

    case AColorFormat of

      GL_RGBA, GL_RGBA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].R := c1 / $100;
          ADest[n].G := c2 / $100;
          ADest[n].B := c3 / $100;
          ADest[n].A := c4;
        end;

      GL_BGRA, GL_BGRA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].B := c1 / $100;
          ADest[n].G := c2 / $100;
          ADest[n].R := c3 / $100;
          ADest[n].A := c4;
        end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure UInt_10_10_10_2_Rev_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pSource: PLongWord;
    n: Integer;
    c0: LongWord;
    c1, c2, c3, c4: Word;

    procedure GetChannel;
      begin
        c0 := pSource^;
        c1 := (c0 and $000003FF) shl 6;
        c2 := (c0 and $000FFC00) shr 4;
        c3 := (c0 and $3FF00000) shr 14;
        c4 := (c0 and $C0000000) shr 16;
        Inc(pSource);
      end;

  begin
    pSource := PLongWord(ASource);

    case AColorFormat of

      GL_RGBA, GL_RGBA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].R := c1 / $100;
          ADest[n].G := c2 / $100;
          ADest[n].B := c3 / $100;
          ADest[n].A := c4;
        end;

      GL_BGRA, GL_BGRA_INTEGER:
        for n := 0 to AWidth * AHeight - 1 do
        begin
          GetChannel;
          ADest[n].B := c1 / $100;
          ADest[n].G := c2 / $100;
          ADest[n].R := c3 / $100;
          ADest[n].A := c4;
        end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

 
// ------------------------------ Decompression 

procedure DecodeColor565(col: Word; out R, G, B: Byte);
  begin
    R := col and $1F;
    G := (col shr 5) and $3F;
    B := (col shr 11) and $1F;
  end;

procedure DXT1_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    x, y, i, j, k, select, offset: Integer;
    col0, col1: Word;
    colors: TU48BitBlock;
    bitmask: Cardinal;
    temp: PGLubyte;
    r0, g0, b0, r1, g1, b1: Byte;
  begin

    temp := PGLubyte(ASource);
    for y := 0 to (AHeight div 4) - 1 do
    begin
      for x := 0 to (AWidth div 4) - 1 do
      begin
        col0 := PWord(temp)^;
        Inc(temp, 2);
        col1 := PWord(temp)^;
        Inc(temp, 2);
        bitmask := PCardinal(temp)^;
        Inc(temp, 4);

        DecodeColor565(col0, r0, g0, b0);
        DecodeColor565(col1, r1, g1, b1);

        colors[0][0] := r0 shl 3;
        colors[0][1] := g0 shl 2;
        colors[0][2] := b0 shl 3;
        colors[0][3] := $FF;
        colors[1][0] := r1 shl 3;
        colors[1][1] := g1 shl 2;
        colors[1][2] := b1 shl 3;
        colors[1][3] := $FF;

        if col0 > col1 then
        begin
          colors[2][0] := (2 * colors[0][0] + colors[1][0] + 1) div 3;
          colors[2][1] := (2 * colors[0][1] + colors[1][1] + 1) div 3;
          colors[2][2] := (2 * colors[0][2] + colors[1][2] + 1) div 3;
          colors[2][3] := $FF;
          colors[3][0] := (colors[0][0] + 2 * colors[1][0] + 1) div 3;
          colors[3][1] := (colors[0][1] + 2 * colors[1][1] + 1) div 3;
          colors[3][2] := (colors[0][2] + 2 * colors[1][2] + 1) div 3;
          colors[3][3] := $FF;
        end
        else
        begin
          colors[2][0] := (colors[0][0] + colors[1][0]) div 2;
          colors[2][1] := (colors[0][1] + colors[1][1]) div 2;
          colors[2][2] := (colors[0][2] + colors[1][2]) div 2;
          colors[2][3] := $FF;
          colors[3][0] := (colors[0][0] + 2 * colors[1][0] + 1) div 3;
          colors[3][1] := (colors[0][1] + 2 * colors[1][1] + 1) div 3;
          colors[3][2] := (colors[0][2] + 2 * colors[1][2] + 1) div 3;
          colors[3][3] := 0;
        end;

        k := 0;
        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            select := (bitmask and (3 shl (k * 2))) shr (k * 2);
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              ADest[offset].B := colors[select][0];
              ADest[offset].G := colors[select][1];
              ADest[offset].R := colors[select][2];
              ADest[offset].A := colors[select][3];
            end;
            Inc(k);
          end;
        end;

      end;
    end;
  end;

procedure DXT3_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    x, y, i, j, k, select: Integer;
    col0, col1, wrd: Word;
    colors: TU48BitBlock;
    bitmask, offset: Cardinal;
    temp: PGLubyte;
    r0, g0, b0, r1, g1, b1: Byte;
    alpha: array [0 .. 3] of Word;
  begin
    temp := PGLubyte(ASource);
    for y := 0 to (AHeight div 4) - 1 do
    begin
      for x := 0 to (AWidth div 4) - 1 do
      begin
        alpha[0] := PWord(temp)^;
        Inc(temp, 2);
        alpha[1] := PWord(temp)^;
        Inc(temp, 2);
        alpha[2] := PWord(temp)^;
        Inc(temp, 2);
        alpha[3] := PWord(temp)^;
        Inc(temp, 2);
        col0 := PWord(temp)^;
        Inc(temp, 2);
        col1 := PWord(temp)^;
        Inc(temp, 2);
        bitmask := PCardinal(temp)^;
        Inc(temp, 4);

        DecodeColor565(col0, r0, g0, b0);
        DecodeColor565(col1, r1, g1, b1);

        colors[0][0] := r0 shl 3;
        colors[0][1] := g0 shl 2;
        colors[0][2] := b0 shl 3;
        colors[0][3] := $FF;
        colors[1][0] := r1 shl 3;
        colors[1][1] := g1 shl 2;
        colors[1][2] := b1 shl 3;
        colors[1][3] := $FF;
        colors[2][0] := (2 * colors[0][0] + colors[1][0] + 1) div 3;
        colors[2][1] := (2 * colors[0][1] + colors[1][1] + 1) div 3;
        colors[2][2] := (2 * colors[0][2] + colors[1][2] + 1) div 3;
        colors[2][3] := $FF;
        colors[3][0] := (colors[0][0] + 2 * colors[1][0] + 1) div 3;
        colors[3][1] := (colors[0][1] + 2 * colors[1][1] + 1) div 3;
        colors[3][2] := (colors[0][2] + 2 * colors[1][2] + 1) div 3;
        colors[3][3] := $FF;

        k := 0;
        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            select := (bitmask and (3 shl (k * 2))) shr (k * 2);
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              ADest[offset].B := colors[select][0];
              ADest[offset].G := colors[select][1];
              ADest[offset].R := colors[select][2];
              ADest[offset].A := colors[select][3];
            end;
            Inc(k);
          end;
        end;

        for j := 0 to 3 do
        begin
          wrd := alpha[j];
          for i := 0 to 3 do
          begin
            if (((4 * x + i) < AWidth) and ((4 * y + j) < AHeight)) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              r0 := wrd and $0F;
              ADest[offset].A := r0 or (r0 shl 4);
            end;
            wrd := wrd shr 4;
          end;
        end;

      end;
    end;
  end;

procedure DXT5_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    x, y, i, j, k, select, offset: Integer;
    col0, col1: Word;
    colors: TU48BitBlock;
    bits, bitmask: Cardinal;
    temp, alphamask: PGLubyte;
    r0, g0, b0, r1, g1, b1: Byte;
    alphas: array [0 .. 7] of Byte;
  begin
    temp := PGLubyte(ASource);
    for y := 0 to (AHeight div 4) - 1 do
    begin
      for x := 0 to (AWidth div 4) - 1 do
      begin
        alphas[0] := temp^;
        Inc(temp);
        alphas[1] := temp^;
        Inc(temp);
        alphamask := temp;
        Inc(temp, 6);
        col0 := PWord(temp)^;
        Inc(temp, 2);
        col1 := PWord(temp)^;
        Inc(temp, 2);
        bitmask := PCardinal(temp)^;
        Inc(temp, 4);

        DecodeColor565(col0, r0, g0, b0);
        DecodeColor565(col1, r1, g1, b1);

        colors[0][0] := r0 shl 3;
        colors[0][1] := g0 shl 2;
        colors[0][2] := b0 shl 3;
        colors[0][3] := $FF;
        colors[1][0] := r1 shl 3;
        colors[1][1] := g1 shl 2;
        colors[1][2] := b1 shl 3;
        colors[1][3] := $FF;
        colors[2][0] := (2 * colors[0][0] + colors[1][0] + 1) div 3;
        colors[2][1] := (2 * colors[0][1] + colors[1][1] + 1) div 3;
        colors[2][2] := (2 * colors[0][2] + colors[1][2] + 1) div 3;
        colors[2][3] := $FF;
        colors[3][0] := (colors[0][0] + 2 * colors[1][0] + 1) div 3;
        colors[3][1] := (colors[0][1] + 2 * colors[1][1] + 1) div 3;
        colors[3][2] := (colors[0][2] + 2 * colors[1][2] + 1) div 3;
        colors[3][3] := $FF;

        k := 0;
        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            select := (bitmask and (3 shl (k * 2))) shr (k * 2);
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              ADest[offset].B := colors[select][0];
              ADest[offset].G := colors[select][1];
              ADest[offset].R := colors[select][2];
            end;
            Inc(k);
          end;
        end;

        if (alphas[0] > alphas[1]) then
        begin
          alphas[2] := (6 * alphas[0] + 1 * alphas[1] + 3) div 7;
          alphas[3] := (5 * alphas[0] + 2 * alphas[1] + 3) div 7;
          alphas[4] := (4 * alphas[0] + 3 * alphas[1] + 3) div 7;
          alphas[5] := (3 * alphas[0] + 4 * alphas[1] + 3) div 7;
          alphas[6] := (2 * alphas[0] + 5 * alphas[1] + 3) div 7;
          alphas[7] := (1 * alphas[0] + 6 * alphas[1] + 3) div 7;
        end
        else
        begin
          alphas[2] := (4 * alphas[0] + 1 * alphas[1] + 2) div 5;
          alphas[3] := (3 * alphas[0] + 2 * alphas[1] + 2) div 5;
          alphas[4] := (2 * alphas[0] + 3 * alphas[1] + 2) div 5;
          alphas[5] := (1 * alphas[0] + 4 * alphas[1] + 2) div 5;
          alphas[6] := 0;
          alphas[7] := $FF;
        end;

        bits := PCardinal(alphamask)^;
        for j := 0 to 1 do
        begin
          for i := 0 to 3 do
          begin
            if (((4 * x + i) < AWidth) and ((4 * y + j) < AHeight)) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              ADest[offset].A := alphas[bits and 7];
            end;
            bits := bits shr 3;
          end;
        end;

        Inc(alphamask, 3);
        bits := PCardinal(alphamask)^;
        for j := 2 to 3 do
        begin
          for i := 0 to 3 do
          begin
            if (((4 * x + i) < AWidth) and ((4 * y + j) < AHeight)) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              ADest[offset].A := alphas[bits and 7];
            end;
            bits := bits shr 3;
          end;
        end;

      end;
    end;
  end;

procedure Decode48BitBlock(ACode: Int64; out ABlock: TU48BitBlock); overload;
  var
    x, y: Byte;
  begin
    for y := 0 to 3 do
      for x := 0 to 3 do
      begin
        ABlock[x][y] := Byte(ACode and $03);
        ACode := ACode shr 2;
      end;
  end;

procedure Decode48BitBlock(ACode: Int64; out ABlock: T48BitBlock); overload;
  var
    x, y: Byte;
  begin
    for y := 0 to 3 do
      for x := 0 to 3 do
      begin
        ABlock[x][y] := SmallInt(ACode and $03);
        ACode := ACode shr 2;
      end;
  end;

procedure LATC1_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    x, y, i, j, offset: Integer;
    LUM0, LUM1: Byte;
    lum: Single;
    colors: TU48BitBlock;
    bitmask: Int64;
    temp: PGLubyte;
  begin

    temp := PGLubyte(ASource);
    for y := 0 to (AHeight div 4) - 1 do
    begin
      for x := 0 to (AWidth div 4) - 1 do
      begin
        LUM0 := temp^;
        Inc(temp);
        LUM1 := temp^;
        Inc(temp);
        bitmask := PInt64(temp)^;
        Inc(temp, 6);
        Decode48BitBlock(bitmask, colors);

        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            if LUM0 > LUM1 then
              case colors[j, i] of
                0:
                  colors[j, i] := LUM0;
                1:
                  colors[j, i] := LUM1;
                2:
                  colors[j, i] := (6 * LUM0 + LUM1) div 7;
                3:
                  colors[j, i] := (5 * LUM0 + 2 * LUM1) div 7;
                4:
                  colors[j, i] := (4 * LUM0 + 3 * LUM1) div 7;
                5:
                  colors[j, i] := (3 * LUM0 + 4 * LUM1) div 7;
                6:
                  colors[j, i] := (2 * LUM0 + 5 * LUM1) div 7;
                7:
                  colors[j, i] := (LUM0 + 6 * LUM1) div 7;
              end
            else
              case colors[j, i] of
                0:
                  colors[j, i] := LUM0;
                1:
                  colors[j, i] := LUM1;
                2:
                  colors[j, i] := (4 * LUM0 + LUM1) div 5;
                3:
                  colors[j, i] := (3 * LUM0 + 2 * LUM1) div 5;
                4:
                  colors[j, i] := (2 * LUM0 + 3 * LUM1) div 5;
                5:
                  colors[j, i] := (LUM0 + 4 * LUM1) div 5;
                6:
                  colors[j, i] := 0;
                7:
                  colors[j, i] := 255;
              end;
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              lum := colors[j, i];
              ADest[offset].R := lum;
              ADest[offset].G := lum;
              ADest[offset].B := lum;
              ADest[offset].A := 255.0;
            end;
          end;

        end;
      end;
    end;
  end;

procedure SLATC1_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    x, y, i, j, offset: Integer;
    LUM0, LUM1: SmallInt;
    lum: Single;
    colors: T48BitBlock;
    bitmask: Int64;
    temp: PGLubyte;
  begin

    temp := PGLubyte(ASource);
    for y := 0 to (AHeight div 4) - 1 do
    begin
      for x := 0 to (AWidth div 4) - 1 do
      begin
        LUM0 := PSmallInt(temp)^;
        Inc(temp);
        LUM1 := PSmallInt(temp)^;
        Inc(temp);
        bitmask := PInt64(temp)^;
        Inc(temp, 6);
        Decode48BitBlock(bitmask, colors);

        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            if LUM0 > LUM1 then
              case colors[j, i] of
                0:
                  colors[j, i] := LUM0;
                1:
                  colors[j, i] := LUM1;
                2:
                  colors[j, i] := (6 * LUM0 + LUM1) div 7;
                3:
                  colors[j, i] := (5 * LUM0 + 2 * LUM1) div 7;
                4:
                  colors[j, i] := (4 * LUM0 + 3 * LUM1) div 7;
                5:
                  colors[j, i] := (3 * LUM0 + 4 * LUM1) div 7;
                6:
                  colors[j, i] := (2 * LUM0 + 5 * LUM1) div 7;
                7:
                  colors[j, i] := (LUM0 + 6 * LUM1) div 7;
              end
            else
              case colors[j, i] of
                0:
                  colors[j, i] := LUM0;
                1:
                  colors[j, i] := LUM1;
                2:
                  colors[j, i] := (4 * LUM0 + LUM1) div 5;
                3:
                  colors[j, i] := (3 * LUM0 + 2 * LUM1) div 5;
                4:
                  colors[j, i] := (2 * LUM0 + 3 * LUM1) div 5;
                5:
                  colors[j, i] := (LUM0 + 4 * LUM1) div 5;
                6:
                  colors[j, i] := -127;
                7:
                  colors[j, i] := 127;
              end;
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              lum := 2 * colors[j, i];
              ADest[offset].R := lum;
              ADest[offset].G := lum;
              ADest[offset].B := lum;
              ADest[offset].A := 127.0;
            end;
          end;

        end;
      end;
    end;
  end;

procedure LATC2_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    x, y, i, j, offset: Integer;
    LUM0, LUM1: Byte;
    lum: Single;
    colors: TU48BitBlock;
    bitmask: Int64;
    temp: PGLubyte;
  begin

    temp := PGLubyte(ASource);
    for y := 0 to (AHeight div 4) - 1 do
    begin
      for x := 0 to (AWidth div 4) - 1 do
      begin
        LUM0 := temp^;
        Inc(temp);
        LUM1 := temp^;
        Inc(temp);
        bitmask := PInt64(temp)^;
        Inc(temp, 6);
        Decode48BitBlock(bitmask, colors);

        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            if LUM0 > LUM1 then
              case colors[j, i] of
                0:
                  colors[j, i] := LUM0;
                1:
                  colors[j, i] := LUM1;
                2:
                  colors[j, i] := (6 * LUM0 + LUM1) div 7;
                3:
                  colors[j, i] := (5 * LUM0 + 2 * LUM1) div 7;
                4:
                  colors[j, i] := (4 * LUM0 + 3 * LUM1) div 7;
                5:
                  colors[j, i] := (3 * LUM0 + 4 * LUM1) div 7;
                6:
                  colors[j, i] := (2 * LUM0 + 5 * LUM1) div 7;
                7:
                  colors[j, i] := (LUM0 + 6 * LUM1) div 7;
              end
            else
              case colors[j, i] of
                0:
                  colors[j, i] := LUM0;
                1:
                  colors[j, i] := LUM1;
                2:
                  colors[j, i] := (4 * LUM0 + LUM1) div 5;
                3:
                  colors[j, i] := (3 * LUM0 + 2 * LUM1) div 5;
                4:
                  colors[j, i] := (2 * LUM0 + 3 * LUM1) div 5;
                5:
                  colors[j, i] := (LUM0 + 4 * LUM1) div 5;
                6:
                  colors[j, i] := 0;
                7:
                  colors[j, i] := 255;
              end;
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              lum := colors[j][i];
              ADest[offset].R := lum;
              ADest[offset].G := lum;
              ADest[offset].B := lum;
            end;
          end; // for i
        end; // for j

        LUM0 := temp^;
        Inc(temp);
        LUM1 := temp^;
        Inc(temp);
        bitmask := PInt64(temp)^;
        Inc(temp, 6);
        Decode48BitBlock(bitmask, colors);

        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            if LUM0 > LUM1 then
              case colors[j, i] of
                0:
                  colors[j, i] := LUM0;
                1:
                  colors[j, i] := LUM1;
                2:
                  colors[j, i] := (6 * LUM0 + LUM1) div 7;
                3:
                  colors[j, i] := (5 * LUM0 + 2 * LUM1) div 7;
                4:
                  colors[j, i] := (4 * LUM0 + 3 * LUM1) div 7;
                5:
                  colors[j, i] := (3 * LUM0 + 4 * LUM1) div 7;
                6:
                  colors[j, i] := (2 * LUM0 + 5 * LUM1) div 7;
                7:
                  colors[j, i] := (LUM0 + 6 * LUM1) div 7;
              end
            else
              case colors[j, i] of
                0:
                  colors[j, i] := LUM0;
                1:
                  colors[j, i] := LUM1;
                2:
                  colors[j, i] := (4 * LUM0 + LUM1) div 5;
                3:
                  colors[j, i] := (3 * LUM0 + 2 * LUM1) div 5;
                4:
                  colors[j, i] := (2 * LUM0 + 3 * LUM1) div 5;
                5:
                  colors[j, i] := (LUM0 + 4 * LUM1) div 5;
                6:
                  colors[j, i] := 0;
                7:
                  colors[j, i] := 255;
              end;
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
              ADest[((4 * y + j) * AWidth + (4 * x + i))].A := colors[j][i];
          end;
        end;

      end;
    end;
  end;

procedure SLATC2_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    x, y, i, j, offset: Integer;
    LUM0, LUM1: SmallInt;
    lum: Single;
    colors: T48BitBlock;
    bitmask: Int64;
    temp: PGLubyte;
  begin

    temp := PGLubyte(ASource);
    for y := 0 to (AHeight div 4) - 1 do
    begin
      for x := 0 to (AWidth div 4) - 1 do
      begin
        LUM0 := PSmallInt(temp)^;
        Inc(temp);
        LUM1 := PSmallInt(temp)^;
        Inc(temp);
        bitmask := PInt64(temp)^;
        Inc(temp, 6);
        Decode48BitBlock(bitmask, colors);

        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            if LUM0 > LUM1 then
              case colors[j, i] of
                0:
                  colors[j, i] := LUM0;
                1:
                  colors[j, i] := LUM1;
                2:
                  colors[j, i] := (6 * LUM0 + LUM1) div 7;
                3:
                  colors[j, i] := (5 * LUM0 + 2 * LUM1) div 7;
                4:
                  colors[j, i] := (4 * LUM0 + 3 * LUM1) div 7;
                5:
                  colors[j, i] := (3 * LUM0 + 4 * LUM1) div 7;
                6:
                  colors[j, i] := (2 * LUM0 + 5 * LUM1) div 7;
                7:
                  colors[j, i] := (LUM0 + 6 * LUM1) div 7;
              end
            else
              case colors[j, i] of
                0:
                  colors[j, i] := LUM0;
                1:
                  colors[j, i] := LUM1;
                2:
                  colors[j, i] := (4 * LUM0 + LUM1) div 5;
                3:
                  colors[j, i] := (3 * LUM0 + 2 * LUM1) div 5;
                4:
                  colors[j, i] := (2 * LUM0 + 3 * LUM1) div 5;
                5:
                  colors[j, i] := (LUM0 + 4 * LUM1) div 5;
                6:
                  colors[j, i] := -127;
                7:
                  colors[j, i] := 127;
              end;
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              lum := 2 * colors[j][i];
              ADest[offset].R := lum;
              ADest[offset].G := lum;
              ADest[offset].B := lum;
            end;
          end;
        end;

        LUM0 := PSmallInt(temp)^;
        Inc(temp);
        LUM1 := PSmallInt(temp)^;
        Inc(temp);
        bitmask := PInt64(temp)^;
        Inc(temp, 6);
        Decode48BitBlock(bitmask, colors);

        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            if LUM0 > LUM1 then
              case colors[j, i] of
                0:
                  colors[j, i] := LUM0;
                1:
                  colors[j, i] := LUM1;
                2:
                  colors[j, i] := (6 * LUM0 + LUM1) div 7;
                3:
                  colors[j, i] := (5 * LUM0 + 2 * LUM1) div 7;
                4:
                  colors[j, i] := (4 * LUM0 + 3 * LUM1) div 7;
                5:
                  colors[j, i] := (3 * LUM0 + 4 * LUM1) div 7;
                6:
                  colors[j, i] := (2 * LUM0 + 5 * LUM1) div 7;
                7:
                  colors[j, i] := (LUM0 + 6 * LUM1) div 7;
              end
            else
              case colors[j, i] of
                0:
                  colors[j, i] := LUM0;
                1:
                  colors[j, i] := LUM1;
                2:
                  colors[j, i] := (4 * LUM0 + LUM1) div 5;
                3:
                  colors[j, i] := (3 * LUM0 + 2 * LUM1) div 5;
                4:
                  colors[j, i] := (2 * LUM0 + 3 * LUM1) div 5;
                5:
                  colors[j, i] := (LUM0 + 4 * LUM1) div 5;
                6:
                  colors[j, i] := -127;
                7:
                  colors[j, i] := 127;
              end;
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              ADest[((4 * y + j) * AWidth + (4 * x + i))].A := 2 * colors[j][i];
            end;
          end;
        end;
      end;
    end;
  end;

procedure RGTC1_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    x, y, i, j, offset: Integer;
    RED0, RED1: Byte;
    lum: Single;
    colors: TU48BitBlock;
    bitmask: Int64;
    temp: PGLubyte;
  begin

    temp := PGLubyte(ASource);
    for y := 0 to (AHeight div 4) - 1 do
    begin
      for x := 0 to (AWidth div 4) - 1 do
      begin
        RED0 := temp^;
        Inc(temp);
        RED1 := temp^;
        Inc(temp);
        bitmask := PInt64(temp)^;
        Inc(temp, 6);
        Decode48BitBlock(bitmask, colors);

        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            if RED0 > RED1 then
              case colors[j, i] of
                0:
                  colors[j, i] := RED0;
                1:
                  colors[j, i] := RED1;
                2:
                  colors[j, i] := (6 * RED0 + RED1) div 7;
                3:
                  colors[j, i] := (5 * RED0 + 2 * RED1) div 7;
                4:
                  colors[j, i] := (4 * RED0 + 3 * RED1) div 7;
                5:
                  colors[j, i] := (3 * RED0 + 4 * RED1) div 7;
                6:
                  colors[j, i] := (2 * RED0 + 5 * RED1) div 7;
                7:
                  colors[j, i] := (RED0 + 6 * RED1) div 7;
              end
            else
              case colors[j, i] of
                0:
                  colors[j, i] := RED0;
                1:
                  colors[j, i] := RED1;
                2:
                  colors[j, i] := (4 * RED0 + RED1) div 5;
                3:
                  colors[j, i] := (3 * RED0 + 2 * RED1) div 5;
                4:
                  colors[j, i] := (2 * RED0 + 3 * RED1) div 5;
                5:
                  colors[j, i] := (RED0 + 4 * RED1) div 5;
                6:
                  colors[j, i] := 0;
                7:
                  colors[j, i] := 255;
              end;
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i)) * 4;
              lum := colors[j][i];
              ADest[offset].R := lum;
              ADest[offset].G := 0.0;
              ADest[offset].B := 0.0;
              ADest[offset].A := 255.0;
            end;
          end;

        end;
      end;
    end;
  end;

procedure SRGTC1_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    x, y, i, j, offset: Integer;
    RED0, RED1: SmallInt;
    lum: Single;
    colors: T48BitBlock;
    bitmask: Int64;
    temp: PGLubyte;
  begin

    temp := PGLubyte(ASource);
    for y := 0 to (AHeight div 4) - 1 do
    begin
      for x := 0 to (AWidth div 4) - 1 do
      begin
        RED0 := PSmallInt(temp)^;
        Inc(temp);
        RED1 := PSmallInt(temp)^;
        Inc(temp);
        bitmask := PInt64(temp)^;
        Inc(temp, 6);
        Decode48BitBlock(bitmask, colors);

        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            if RED0 > RED1 then
              case colors[j, i] of
                0:
                  colors[j, i] := RED0;
                1:
                  colors[j, i] := RED1;
                2:
                  colors[j, i] := (6 * RED0 + RED1) div 7;
                3:
                  colors[j, i] := (5 * RED0 + 2 * RED1) div 7;
                4:
                  colors[j, i] := (4 * RED0 + 3 * RED1) div 7;
                5:
                  colors[j, i] := (3 * RED0 + 4 * RED1) div 7;
                6:
                  colors[j, i] := (2 * RED0 + 5 * RED1) div 7;
                7:
                  colors[j, i] := (RED0 + 6 * RED1) div 7;
              end
            else
              case colors[j, i] of
                0:
                  colors[j, i] := RED0;
                1:
                  colors[j, i] := RED1;
                2:
                  colors[j, i] := (4 * RED0 + RED1) div 5;
                3:
                  colors[j, i] := (3 * RED0 + 2 * RED1) div 5;
                4:
                  colors[j, i] := (2 * RED0 + 3 * RED1) div 5;
                5:
                  colors[j, i] := (RED0 + 4 * RED1) div 5;
                6:
                  colors[j, i] := -127;
                7:
                  colors[j, i] := 127;
              end;
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              lum := 2 * colors[j][i];
              ADest[offset].R := lum;
              ADest[offset].G := 0.0;
              ADest[offset].B := 0.0;
              ADest[offset].A := 127.0;
            end;
          end;

        end;
      end;
    end;
  end;

procedure RGTC2_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    x, y, i, j, offset: Integer;
    RED0, RED1: Byte;
    colors: TU48BitBlock;
    bitmask: Int64;
    temp: PGLubyte;
  begin

    temp := PGLubyte(ASource);
    for y := 0 to (AHeight div 4) - 1 do
    begin
      for x := 0 to (AWidth div 4) - 1 do
      begin
        RED0 := temp^;
        Inc(temp);
        RED1 := temp^;
        Inc(temp);
        bitmask := PInt64(temp)^;
        Inc(temp, 6);
        Decode48BitBlock(bitmask, colors);

        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            if RED0 > RED1 then
              case colors[j, i] of
                0:
                  colors[j, i] := RED0;
                1:
                  colors[j, i] := RED1;
                2:
                  colors[j, i] := (6 * RED0 + RED1) div 7;
                3:
                  colors[j, i] := (5 * RED0 + 2 * RED1) div 7;
                4:
                  colors[j, i] := (4 * RED0 + 3 * RED1) div 7;
                5:
                  colors[j, i] := (3 * RED0 + 4 * RED1) div 7;
                6:
                  colors[j, i] := (2 * RED0 + 5 * RED1) div 7;
                7:
                  colors[j, i] := (RED0 + 6 * RED1) div 7;
              end
            else
              case colors[j, i] of
                0:
                  colors[j, i] := RED0;
                1:
                  colors[j, i] := RED1;
                2:
                  colors[j, i] := (4 * RED0 + RED1) div 5;
                3:
                  colors[j, i] := (3 * RED0 + 2 * RED1) div 5;
                4:
                  colors[j, i] := (2 * RED0 + 3 * RED1) div 5;
                5:
                  colors[j, i] := (RED0 + 4 * RED1) div 5;
                6:
                  colors[j, i] := 0;
                7:
                  colors[j, i] := 255;
              end;
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              ADest[offset].R := colors[j][i];
              ADest[offset].B := 0.0;
            end;
          end;
        end;

        RED0 := temp^;
        Inc(temp);
        RED1 := temp^;
        Inc(temp);
        bitmask := PInt64(temp)^;
        Inc(temp, 6);
        Decode48BitBlock(bitmask, colors);

        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            if RED0 > RED1 then
              case colors[j, i] of
                0:
                  colors[j, i] := RED0;
                1:
                  colors[j, i] := RED1;
                2:
                  colors[j, i] := (6 * RED0 + RED1) div 7;
                3:
                  colors[j, i] := (5 * RED0 + 2 * RED1) div 7;
                4:
                  colors[j, i] := (4 * RED0 + 3 * RED1) div 7;
                5:
                  colors[j, i] := (3 * RED0 + 4 * RED1) div 7;
                6:
                  colors[j, i] := (2 * RED0 + 5 * RED1) div 7;
                7:
                  colors[j, i] := (RED0 + 6 * RED1) div 7;
              end
            else
              case colors[j, i] of
                0:
                  colors[j, i] := RED0;
                1:
                  colors[j, i] := RED1;
                2:
                  colors[j, i] := (4 * RED0 + RED1) div 5;
                3:
                  colors[j, i] := (3 * RED0 + 2 * RED1) div 5;
                4:
                  colors[j, i] := (2 * RED0 + 3 * RED1) div 5;
                5:
                  colors[j, i] := (RED0 + 4 * RED1) div 5;
                6:
                  colors[j, i] := 0;
                7:
                  colors[j, i] := 255;
              end;
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              ADest[offset].G := colors[j][i];
              ADest[offset].A := 255.0;
            end;
          end;
        end;
      end;
    end;
  end;

procedure SRGTC2_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    x, y, i, j, offset: Integer;
    RED0, RED1: SmallInt;
    lum: Single;
    colors: T48BitBlock;
    bitmask: Int64;
    temp: PGLubyte;
  begin

    temp := PGLubyte(ASource);
    for y := 0 to (AHeight div 4) - 1 do
    begin
      for x := 0 to (AWidth div 4) - 1 do
      begin
        RED0 := PSmallInt(temp)^;
        Inc(temp);
        RED1 := PSmallInt(temp)^;
        Inc(temp);
        bitmask := PInt64(temp)^;
        Inc(temp, 6);
        Decode48BitBlock(bitmask, colors);

        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            if RED0 > RED1 then
              case colors[j, i] of
                0:
                  colors[j, i] := RED0;
                1:
                  colors[j, i] := RED1;
                2:
                  colors[j, i] := (6 * RED0 + RED1) div 7;
                3:
                  colors[j, i] := (5 * RED0 + 2 * RED1) div 7;
                4:
                  colors[j, i] := (4 * RED0 + 3 * RED1) div 7;
                5:
                  colors[j, i] := (3 * RED0 + 4 * RED1) div 7;
                6:
                  colors[j, i] := (2 * RED0 + 5 * RED1) div 7;
                7:
                  colors[j, i] := (RED0 + 6 * RED1) div 7;
              end
            else
              case colors[j, i] of
                0:
                  colors[j, i] := RED0;
                1:
                  colors[j, i] := RED1;
                2:
                  colors[j, i] := (4 * RED0 + RED1) div 5;
                3:
                  colors[j, i] := (3 * RED0 + 2 * RED1) div 5;
                4:
                  colors[j, i] := (2 * RED0 + 3 * RED1) div 5;
                5:
                  colors[j, i] := (RED0 + 4 * RED1) div 5;
                6:
                  colors[j, i] := -127;
                7:
                  colors[j, i] := 127;
              end;
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              lum := 2 * colors[j][i];
              ADest[offset].R := lum;
              ADest[offset].B := 0.0;
            end;
          end;
        end;

        RED0 := PSmallInt(temp)^;
        Inc(temp);
        RED1 := PSmallInt(temp)^;
        Inc(temp);
        bitmask := PInt64(temp)^;
        Inc(temp, 6);
        Decode48BitBlock(bitmask, colors);

        for j := 0 to 3 do
        begin
          for i := 0 to 3 do
          begin
            if RED0 > RED1 then
              case colors[j, i] of
                0:
                  colors[j, i] := RED0;
                1:
                  colors[j, i] := RED1;
                2:
                  colors[j, i] := (6 * RED0 + RED1) div 7;
                3:
                  colors[j, i] := (5 * RED0 + 2 * RED1) div 7;
                4:
                  colors[j, i] := (4 * RED0 + 3 * RED1) div 7;
                5:
                  colors[j, i] := (3 * RED0 + 4 * RED1) div 7;
                6:
                  colors[j, i] := (2 * RED0 + 5 * RED1) div 7;
                7:
                  colors[j, i] := (RED0 + 6 * RED1) div 7;
              end
            else
              case colors[j, i] of
                0:
                  colors[j, i] := RED0;
                1:
                  colors[j, i] := RED1;
                2:
                  colors[j, i] := (4 * RED0 + RED1) div 5;
                3:
                  colors[j, i] := (3 * RED0 + 2 * RED1) div 5;
                4:
                  colors[j, i] := (2 * RED0 + 3 * RED1) div 5;
                5:
                  colors[j, i] := (RED0 + 4 * RED1) div 5;
                6:
                  colors[j, i] := -127;
                7:
                  colors[j, i] := 127;
              end;
            if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            begin
              offset := ((4 * y + j) * AWidth + (4 * x + i));
              lum := 2 * colors[j][i];
              ADest[offset].G := lum;
              ADest[offset].A := 127.0;
            end;
          end;
        end;
      end;
    end;
  end;

// ------------------------------ RGBA Float to OpenGL format image 

procedure UnsupportedFromImf(ASource: PIntermediateFormatArray; ADest: Pointer; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  begin
    raise EGLImageUtils.Create('Unimplemented type of conversion');
  end;

procedure ImfToUbyte(ASource: PIntermediateFormatArray; ADest: Pointer; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pDest: PByte;
    n: Integer;

    procedure SetChannel(AValue: Single);
      begin
        pDest^ := Trunc(ClampValue(AValue, 0.0, 255.0));
        Inc(pDest);
      end;

    procedure SetChannelI(AValue: Single);
      begin
        pDest^ := Trunc(AValue);
        Inc(pDest);
      end;

  begin
    pDest := PByte(ADest);

    case AColorFormat of
    GL_RGB:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
      end;
    GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
      end;
    GL_BGR:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
      end;
    GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
      end;
    GL_RGBA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].A);
      end;
    GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].A);
      end;
    GL_BGRA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].A);
      end;
    GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].A);
      end;
    GL_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].A);
      end;
    GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].A);
      end;
    GL_LUMINANCE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannel(ASource[n].A);
      end;
    GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannelI(ASource[n].A);
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_RED:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
      end;
    GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
      end;
    GL_GREEN:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].G);
      end;
    GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].G);
      end;
    GL_BLUE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
      end;
    GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
      end;
    GL_RG:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
      end;
    GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure ImfToByte(ASource: PIntermediateFormatArray; ADest: Pointer; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pDest: PShortInt;
    n: Integer;

    procedure SetChannel(AValue: Single);
      begin
        pDest^ := Trunc(ClampValue(AValue, -127.0, 127.0));
        Inc(pDest);
      end;

    procedure SetChannelI(AValue: Single);
      begin
        pDest^ := Trunc(AValue);
        Inc(pDest);
      end;

  begin
    pDest := PShortInt(ADest);

    case AColorFormat of
    GL_RGB:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
      end;
    GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
      end;
    GL_BGR:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
      end;
    GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
      end;
    GL_RGBA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].A);
      end;
    GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].A);
      end;
    GL_BGRA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].A);
      end;
    GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].A);
      end;
    GL_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].A);
      end;
    GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].A);
      end;
    GL_LUMINANCE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannel(ASource[n].A);
      end;
    GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannelI(ASource[n].A);
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_RED:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
      end;
    GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
      end;
    GL_GREEN:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].G);
      end;
    GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].G);
      end;
    GL_BLUE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
      end;
    GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
      end;
    GL_RG:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
      end;
    GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure ImfToUShort(ASource: PIntermediateFormatArray; ADest: Pointer; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pDest: PWord;
    n: Integer;

    procedure SetChannel(AValue: Single);
      begin
        pDest^ := Trunc(ClampValue(AValue, 0.0, 65535.0));
        Inc(pDest);
      end;

    procedure SetChannelI(AValue: Single);
      begin
        pDest^ := Trunc(AValue);
        Inc(pDest);
      end;

  begin
    pDest := PWord(ADest);

    case AColorFormat of
    GL_RGB:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
      end;
    GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
      end;
    GL_BGR:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
      end;
    GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
      end;
    GL_RGBA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].A);
      end;
    GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].A);
      end;
    GL_BGRA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].A);
      end;
    GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].A);
      end;
    GL_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].A);
      end;
    GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].A);
      end;
    GL_LUMINANCE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannel(ASource[n].A);
      end;
    GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannelI(ASource[n].A);
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_RED:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
      end;
    GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
      end;
    GL_GREEN:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].G);
      end;
    GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].G);
      end;
    GL_BLUE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
      end;
    GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
      end;
    GL_RG:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
      end;
    GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure ImfToShort(ASource: PIntermediateFormatArray; ADest: Pointer; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pDest: PSmallInt;
    n: Integer;

    procedure SetChannel(AValue: Single);
      begin
        pDest^ := Trunc(ClampValue(AValue, -32767.0, 32767.0));
        Inc(pDest);
      end;

    procedure SetChannelI(AValue: Single);
      begin
        pDest^ := Trunc(AValue);
        Inc(pDest);
      end;

  begin
    pDest := PSmallInt(ADest);

    case AColorFormat of
    GL_RGB:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
      end;
    GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
      end;
    GL_BGR:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
      end;
    GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
      end;
    GL_RGBA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].A);
      end;
    GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].A);
      end;
    GL_BGRA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].A);
      end;
    GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].A);
      end;
    GL_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].A);
      end;
    GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].A);
      end;
    GL_LUMINANCE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannel(ASource[n].A);
      end;
    GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannelI(ASource[n].A);
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_RED:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
      end;
    GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
      end;
    GL_GREEN:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].G);
      end;
    GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].G);
      end;
    GL_BLUE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
      end;
    GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
      end;
    GL_RG:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
      end;
    GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure ImfToUInt(ASource: PIntermediateFormatArray; ADest: Pointer; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pDest: PLongWord;
    n: Integer;

    procedure SetChannel(AValue: Single);
      begin
        pDest^ := Trunc(ClampValue(AValue, 0.0, $FFFFFFFF));
        Inc(pDest);
      end;

    procedure SetChannelI(AValue: Single);
      begin
        pDest^ := Trunc(AValue);
        Inc(pDest);
      end;

  begin
    pDest := PLongWord(ADest);

    case AColorFormat of
    GL_RGB:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
      end;
    GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
      end;
    GL_BGR:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
      end;
    GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
      end;
    GL_RGBA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].A);
      end;
    GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].A);
      end;
    GL_BGRA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].A);
      end;
    GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].A);
      end;
    GL_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].A);
      end;
    GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].A);
      end;
    GL_LUMINANCE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannel(ASource[n].A);
      end;
    GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannelI(ASource[n].A);
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_RED:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
      end;
    GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
      end;
    GL_GREEN:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].G);
      end;
    GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].G);
      end;
    GL_BLUE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
      end;
    GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
      end;
    GL_RG:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
      end;
    GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure ImfToInt(ASource: PIntermediateFormatArray; ADest: Pointer; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  var
    pDest: PLongInt;
    n: Integer;

    procedure SetChannel(AValue: Single);
      begin
        pDest^ := Trunc(ClampValue(AValue, -$7FFFFFFF, $7FFFFFFF));
        Inc(pDest);
      end;

    procedure SetChannelI(AValue: Single);
      begin
        pDest^ := Trunc(AValue);
        Inc(pDest);
      end;

  begin
    pDest := PLongInt(ADest);

    case AColorFormat of
    GL_RGB:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
      end;
    GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
      end;
    GL_BGR:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
      end;
    GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
      end;
    GL_RGBA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].A);
      end;
    GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].A);
      end;
    GL_BGRA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].A);
      end;
    GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].A);
      end;
    GL_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].A);
      end;
    GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].A);
      end;
    GL_LUMINANCE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannel(ASource[n].A);
      end;
    GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannelI(ASource[n].A);
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_RED:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
      end;
    GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
      end;
    GL_GREEN:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].G);
      end;
    GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].G);
      end;
    GL_BLUE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
      end;
    GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
      end;
    GL_RG:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
      end;
    GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure ImfToFloat(ASource: PIntermediateFormatArray; ADest: Pointer; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  const
    cInv255 = 1.0 / 255.0;

  var
    pDest: PSingle;
    n: Integer;

    procedure SetChannel(AValue: Single);
      begin
        pDest^ := AValue * cInv255;
        Inc(pDest);
      end;

    procedure SetChannelI(AValue: Single);
      begin
        pDest^ := AValue * cInv255;
        Inc(pDest);
      end;

  begin
    pDest := PSingle(ADest);

    case AColorFormat of
    GL_RGB:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
      end;
    GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
      end;
    GL_BGR:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
      end;
    GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
      end;
    GL_RGBA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].A);
      end;
    GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].A);
      end;
    GL_BGRA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].A);
      end;
    GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].A);
      end;
    GL_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].A);
      end;
    GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].A);
      end;
    GL_LUMINANCE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannel(ASource[n].A);
      end;
    GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannelI(ASource[n].A);
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_RED:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
      end;
    GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
      end;
    GL_GREEN:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].G);
      end;
    GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].G);
      end;
    GL_BLUE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
      end;
    GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
      end;
    GL_RG:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
      end;
    GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

procedure ImfToHalf(ASource: PIntermediateFormatArray; ADest: Pointer; AColorFormat: Cardinal; AWidth, AHeight: Integer);
  const
    cInv255 = 1.0 / 255.0;

  var
    pDest: PHalfFloat;
    n: Integer;

    procedure SetChannel(AValue: Single);
      begin
        pDest^ := FloatToHalf(AValue * cInv255);
        Inc(pDest);
      end;

    procedure SetChannelI(AValue: Single);
      begin
        pDest^ := FloatToHalf(AValue * cInv255);
        Inc(pDest);
      end;

  begin
    pDest := PHalfFloat(ADest);

    case AColorFormat of
    GL_RGB:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
      end;
    GL_RGB_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
      end;
    GL_BGR:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
      end;
    GL_BGR_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
      end;
    GL_RGBA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].A);
      end;
    GL_RGBA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].A);
      end;
    GL_BGRA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
        SetChannel(ASource[n].G);
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].A);
      end;
    GL_BGRA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
        SetChannelI(ASource[n].G);
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].A);
      end;
    GL_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].A);
      end;
    GL_ALPHA_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].A);
      end;
    GL_LUMINANCE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_LUMINANCE_ALPHA:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannel(ASource[n].A);
      end;
    GL_LUMINANCE_ALPHA_INTEGER_EXT:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
        SetChannelI(ASource[n].A);
      end;
    GL_INTENSITY:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R + ASource[n].G + ASource[n].B / 3.0);
      end;
    GL_RED:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
      end;
    GL_RED_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
      end;
    GL_GREEN:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].G);
      end;
    GL_GREEN_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].G);
      end;
    GL_BLUE:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].B);
      end;
    GL_BLUE_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].B);
      end;
    GL_RG:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannel(ASource[n].R);
        SetChannel(ASource[n].G);
      end;
    GL_RG_INTEGER:
      for n := 0 to AWidth*AHeight-1 do
      begin
        SetChannelI(ASource[n].R);
        SetChannelI(ASource[n].G);
      end;
    else
      raise EGLImageUtils.Create(strInvalidType);
    end;
  end;

// ------------------------------ Compression 
{ function FloatTo565(const AColor: TIntermediateFormat): Integer;
  var
  r, g, b: Integer;
  begin
  // get the components in the correct range
  r := Round( 31.0*AColor.R, 31 );
  g := Round( 63.0*AColor.G, 63 );
  b := Round( 31.0*AColor.B, 31 );
  // pack into a single value
  Result :=  ( r shl 11 ) or ( g shl 5 ) or b;
  end;

  procedure WriteColourBlock(a, b: Integer; const indices: PByteArray; out block: TU48BitBlock);
  var
  I, J: Byte;
  begin
  // write the endpoints
  block[0][0] := a and $ff;
  block[0][1] := a shr 8;
  block[0][2] := b and $ff;
  block[0][3] := b shr 8;
  // write the indices
  for i := 0 to 3 do
  begin
  J := 4*i;
  block[1][i] = indices[J+0] or ( indices[J+1] shl 2 ) or ( indices[J+2] shl 4 ) or ( indices[J+3] shl 6 );
  end;
  end;

  procedure WriteColourBlock3(start, end_: TIntermediateFormat; const indices: PByteArray; out block: TU48BitBlock);
  var
  i, a, b: Integer;
  remapped: array[0..15] of Byte;
  begin
  // get the packed values
  a := FloatTo565( start );
  b := FloatTo565( end_ );

  // remap the indices
  if a <= b then
  begin
  // use the indices directly
  for i := 0 to 15 do
  remapped[i] := indices[i];
  end
  else
  begin
  // swap a and b
  Swap( a, b );
  for i := 0 to 15 do
  begin
  if indices[i] = 0  then
  remapped[i] := 1
  else if indices[i] = 1 then
  remapped[i] := 0
  else
  remapped[i] := indices[i];
  end;
  end;

  // write the block
  WriteColourBlock( a, b, remapped, block );
  end;

  procedure WriteColourBlock4(start, end_: TIntermediateFormat; const indices: PByteArray; out block: TU48BitBlock);
  var
  i, a, b: Integer;
  remapped: array[0..15] of Byte;
  begin
  // get the packed values
  a := FloatTo565( start );
  b := FloatTo565( end_ );

  // remap the indices
  if a < b then
  begin
  // swap a and b
  Swap( a, b );
  for i := 0 to 15 do
  remapped[i] := ( indices[i] xor $01 ) and $03;
  end
  else if a = b then
  begin
  // use index 0
  for i := 0 to 15 do
  remapped[i] := 0;
  end
  else
  begin
  // use the indices directly
  for i := 0 to 15 do
  remapped[i] := indices[i];
  end;

  // write the block
  WriteColourBlock( a, b, remapped, block );
  end; }

// ------------------------------ Image filters 
function ImageBoxFilter(Value: Single): Single;
  begin
    if (Value > -0.5) and (Value <= 0.5) then
      Result := 1.0
    else
      Result := 0.0;
  end;

function ImageTriangleFilter(Value: Single): Single;
  begin
    if Value < 0.0 then
      Value := -Value;
    if Value < 1.0 then
      Result := 1.0 - Value
    else
      Result := 0.0;
  end;

function ImageHermiteFilter(Value: Single): Single;
  begin
    if Value < 0.0 then
      Value := -Value;
    if Value < 1 then
      Result := (2 * Value - 3) * Sqr(Value) + 1
    else
      Result := 0;
  end;

function ImageBellFilter(Value: Single): Single;
  begin
    if Value < 0.0 then
      Value := -Value;
    if Value < 0.5 then
      Result := 0.75 - Sqr(Value)
    else if Value < 1.5 then
    begin
      Value := Value - 1.5;
      Result := 0.5 * Sqr(Value);
    end
    else
      Result := 0.0;
  end;

function ImageSplineFilter(Value: Single): Single;
  var
    temp: Single;
  begin
    if Value < 0.0 then
      Value := -Value;
    if Value < 1.0 then
    begin
      temp := Sqr(Value);
      Result := 0.5 * temp * Value - temp + 2.0 / 3.0;
    end
    else if Value < 2.0 then
    begin
      Value := 2.0 - Value;
      Result := Sqr(Value) * Value / 6.0;
    end
    else
      Result := 0.0;
  end;

function ImageLanczos3Filter(Value: Single): Single;
  const
    Radius = 3.0;
  begin
    Result := 1;
    if Value = 0 then
      Exit;
    if Value < 0.0 then
      Value := -Value;
    if Value < Radius then
    begin
      Value := Value * pi;
      Result := Radius * Sin(Value) * Sin(Value / Radius) / (Value * Value);
    end
    else
      Result := 0.0;
  end;

function ImageMitchellFilter(Value: Single): Single;
  const
    B = 1.0 / 3.0;
    C = 1.0 / 3.0;
  var
    temp: Single;
  begin
    if Value < 0.0 then
      Value := -Value;
    temp := Sqr(Value);
    if Value < 1.0 then
    begin
      Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * temp)) + ((-18.0 + 12.0 * B + 6.0 * C) * temp) + (6.0 - 2.0 * B));
      Result := Value / 6.0;
    end
    else if Value < 2.0 then
    begin
      Value := (((-B - 6.0 * C) * (Value * temp)) + ((6.0 * B + 30.0 * C) * temp) + ((-12.0 * B - 48.0 * C) * Value) + (8.0 * B + 24.0 * C));
      Result := Value / 6.0;
    end
    else
      Result := 0.0;
  end;

const cInvThree = 1.0/3.0;

procedure ImageAlphaFromIntensity(var AColor: TIntermediateFormat);
begin
  AColor.A := (AColor.R + AColor.B + AColor.G) * cInvThree;
end;

procedure ImageAlphaSuperBlackTransparent(var AColor: TIntermediateFormat);
begin
  if (AColor.R = 0.0) and (AColor.B = 0.0) and (AColor.G = 0.0) then
    AColor.A := 0.0
  else
    AColor.A := 255.0;
end;

procedure ImageAlphaLuminance(var AColor: TIntermediateFormat);
begin
  AColor.A := (AColor.R + AColor.B + AColor.G) * cInvThree;
  AColor.R := AColor.A;
  AColor.G := AColor.A;
  AColor.B := AColor.A;
end;

procedure ImageAlphaLuminanceSqrt(var AColor: TIntermediateFormat);
begin
  AColor.A := Sqrt((AColor.R + AColor.B + AColor.G) * cInvThree);
end;

procedure ImageAlphaOpaque(var AColor: TIntermediateFormat);
begin
  AColor.A := 255.0;
end;

var
  vTopLeftColor: TIntermediateFormat;

procedure ImageAlphaTopLeftPointColorTransparent(var AColor: TIntermediateFormat);
begin
  if CompareMem(@AColor, @vTopLeftColor, 3*SizeOf(Single)) then
    AColor.A := 0.0;
end;

procedure ImageAlphaInverseLuminance(var AColor: TIntermediateFormat);
begin
  AColor.A := 255.0 - (AColor.R + AColor.B + AColor.G) * cInvThree;
  AColor.R := AColor.A;
  AColor.G := AColor.A;
  AColor.B := AColor.A;
end;

procedure ImageAlphaInverseLuminanceSqrt(var AColor: TIntermediateFormat);
begin
  AColor.A := 255.0 - Sqrt((AColor.R + AColor.B + AColor.G) * cInvThree);
end;

var
  vBottomRightColor: TIntermediateFormat;

procedure ImageAlphaBottomRightPointColorTransparent(var AColor: TIntermediateFormat);
begin
  if CompareMem(@AColor, @vBottomRightColor, 3*SizeOf(Single)) then
    AColor.A := 0.0;
end;


type
  // Contributor for a pixel
  TContributor = record
    pixel: Integer; // Source pixel
    weight: Single; // Pixel weight
  end;

  TContributorList = array [0 .. MaxInt div (2 * SizeOf(TContributor))] of TContributor;
  PContributorList = ^TContributorList;

  // List of source pixels contributing to a destination pixel
  TCList = record
    n: Integer;
    p: PContributorList;
  end;

  TCListList = array [0 .. MaxInt div (2 * SizeOf(TCList))] of TCList;
  PCListList = ^TCListList;

// ------------------------------ Data type conversion table 

type
  TConvertTableRec = record
    type_: Cardinal;
    proc1: TConvertToImfProc;
    proc2: TConvertFromInfProc;
  end;

const
  cConvertTable: array [0 .. 36] of TConvertTableRec = (
    (type_: GL_UNSIGNED_BYTE; proc1: UbyteToImf; proc2: ImfToUbyte),

    (type_: GL_UNSIGNED_BYTE_3_3_2; proc1: Ubyte332ToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_BYTE_2_3_3_REV; proc1: Ubyte233RToImf; proc2: UnsupportedFromImf),

    (type_: GL_BYTE; proc1: ByteToImf; proc2: ImfToByte),

    (type_: GL_UNSIGNED_SHORT; proc1: UShortToImf; proc2: ImfToUShort),

    (type_: GL_SHORT; proc1: ShortToImf; proc2: ImfToShort),

    (type_: GL_UNSIGNED_INT; proc1: UIntToImf; proc2: ImfToUInt),

    (type_: GL_INT; proc1: IntToImf; proc2: ImfToInt),

    (type_: GL_FLOAT; proc1: FloatToImf; proc2: ImfToFloat),

    (type_: GL_HALF_FLOAT; proc1: HalfFloatToImf; proc2: ImfToHalf),

    (type_: GL_UNSIGNED_INT_8_8_8_8; proc1: UInt8888ToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_INT_8_8_8_8_REV; proc1: UInt8888RevToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_SHORT_4_4_4_4; proc1: UShort4444ToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_SHORT_4_4_4_4_REV; proc1: UShort4444RevToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_SHORT_5_6_5; proc1: UShort565ToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_SHORT_5_6_5_REV; proc1: UShort565RevToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_SHORT_5_5_5_1; proc1: UShort5551ToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_SHORT_1_5_5_5_REV; proc1: UShort5551RevToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_INT_10_10_10_2; proc1: UInt_10_10_10_2_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_INT_2_10_10_10_REV; proc1: UInt_10_10_10_2_Rev_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_RGB_S3TC_DXT1_EXT; proc1: DXT1_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_RGBA_S3TC_DXT1_EXT; proc1: DXT1_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_RGBA_S3TC_DXT3_EXT; proc1: DXT3_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_RGBA_S3TC_DXT5_EXT; proc1: DXT5_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SRGB_S3TC_DXT1_EXT; proc1: UnsupportedToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT; proc1: UnsupportedToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT; proc1: UnsupportedToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT; proc1: UnsupportedToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_LUMINANCE_LATC1_EXT; proc1: LATC1_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT; proc1: SLATC1_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT; proc1: LATC2_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT; proc1: SLATC2_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI; proc1: UnsupportedToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_RED_RGTC1; proc1: RGTC1_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SIGNED_RED_RGTC1; proc1: SRGTC1_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_RG_RGTC2; proc1: RGTC2_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SIGNED_RG_RGTC2; proc1: SRGTC2_ToImf; proc2: UnsupportedFromImf));

procedure ConvertImage(const ASrc: Pointer; const ADst: Pointer; ASrcColorFormat, ADstColorFormat: Cardinal; ASrcDataType, ADstDataType: Cardinal; AWidth, AHeight: Integer);
  var
    ConvertToIntermediateFormat: TConvertToImfProc;
    ConvertFromIntermediateFormat: TConvertFromInfProc;
    i, size: Integer;
    tempBuf: PIntermediateFormatArray;
  begin
    if AWidth < 1 then
      Exit;
    AHeight := MaxInteger(1, AHeight);
    // Allocate memory
    size := AWidth * AHeight * SizeOf(TIntermediateFormat);
    GetMem(tempBuf, size);
    FillChar(tempBuf^, size, $00);

    // Find function to convert external format to intermediate format
    ConvertToIntermediateFormat := UnsupportedToImf;
    for i := 0 to high(cConvertTable) do
    begin
      if ASrcDataType = cConvertTable[i].type_ then
      begin
        ConvertToIntermediateFormat := cConvertTable[i].proc1;
        break;
      end;
    end;

    try
      ConvertToIntermediateFormat(ASrc, tempBuf, ASrcColorFormat, AWidth, AHeight);
    except
      FreeMem(tempBuf);
      raise;
    end;

    // Find function to convert intermediate format to external format
    ConvertFromIntermediateFormat := UnsupportedFromImf;
    for i := 0 to high(cConvertTable) do
    begin
      if ADstDataType = cConvertTable[i].type_ then
      begin
        ConvertFromIntermediateFormat := cConvertTable[i].proc2;
        break;
      end;
    end;

    try
      ConvertFromIntermediateFormat(tempBuf, ADst, ADstColorFormat, AWidth, AHeight);
    except
      FreeMem(tempBuf);
      raise;
    end;

    FreeMem(tempBuf);
  end;

procedure RescaleImage(
  const ASrc: Pointer;
  const ADst: Pointer;
  AColorFormat: Cardinal;
  ADataType: Cardinal;
  AFilter: TImageFilterFunction;
  ASrcWidth, ASrcHeight, ADstWidth, ADstHeight: Integer);

var
  ConvertToIntermediateFormat: TConvertToImfProc;
  ConvertFromIntermediateFormat: TConvertFromInfProc;
  i, j, k, n, size: Integer;
  tempBuf1, tempBuf2, SourceLine, DestLine: PIntermediateFormatArray;
  contrib: PCListList;
  xscale, yscale: Single; // Zoom scale factors
  width, fscale, weight: Single; // Filter calculation variables
  center: Single; // Filter calculation variables
  left, right: Integer; // Filter calculation variables
  color1, color2: TIntermediateFormat;
begin
  if (ASrcWidth < 1) or (ADstWidth < 1) then
    Exit;
  ASrcHeight := MaxInteger(1, ASrcHeight);
  ADstHeight := MaxInteger(1, ADstHeight);

  // Allocate memory
  size := ASrcWidth * ASrcHeight * SizeOf(TIntermediateFormat);
  GetMem(tempBuf1, size);
  FillChar(tempBuf1^, size, $00);

  // Find function to convert external format to intermediate format
  ConvertToIntermediateFormat := UnsupportedToImf;
  for i := 0 to high(cConvertTable) do
  begin
    if ADataType = cConvertTable[i].type_ then
    begin
      ConvertToIntermediateFormat := cConvertTable[i].proc1;
      ConvertFromIntermediateFormat := cConvertTable[i].proc2;
      break;
    end;
  end;

  try
    ConvertToIntermediateFormat(ASrc, tempBuf1, AColorFormat, ASrcWidth, ASrcHeight);
  except
    FreeMem(tempBuf1);
    raise;
  end;

  // Rescale

  if ASrcWidth = 1 then
    xscale := ADstWidth / ASrcWidth
  else
    xscale := (ADstWidth - 1) / (ASrcWidth - 1);
  if ASrcHeight = 1 then
    yscale := ADstHeight / ASrcHeight
  else
    yscale := (ADstHeight - 1) / (ASrcHeight - 1);
  // Pre-calculate filter contributions for a row
  GetMem(contrib, ADstWidth * SizeOf(TCList));
  // Horizontal sub-sampling
  // Scales from bigger to smaller width
  if xscale < 1.0 then
  begin
    width := vImageScaleFilterWidth / xscale;
    fscale := 1.0 / xscale;
    for i := 0 to ADstWidth - 1 do
    begin
      contrib^[i].n := 0;
      GetMem(contrib^[i].p, Trunc(width * 2.0 + 1) * SizeOf(TContributor));
      center := i / xscale;
      left := floor(center - width);
      right := ceil(center + width);
      for j := left to right do
      begin
        weight := AFilter((center - j) / fscale) / fscale;
        if weight = 0.0 then
          continue;
        if (j < 0) then
          n := -j
        else if (j >= ASrcWidth) then
          n := ASrcWidth - j + ASrcWidth - 1
        else
          n := j;
        k := contrib^[i].n;
        contrib^[i].n := contrib^[i].n + 1;
        contrib^[i].p^[k].pixel := n;
        contrib^[i].p^[k].weight := weight;
      end;
    end;
  end
  else
  // Horizontal super-sampling
  // Scales from smaller to bigger width
  begin
    for i := 0 to ADstWidth - 1 do
    begin
      contrib^[i].n := 0;
      GetMem(contrib^[i].p, Trunc(vImageScaleFilterWidth * 2.0 + 1) * SizeOf(TContributor));
      center := i / xscale;
      left := floor(center - vImageScaleFilterWidth);
      right := ceil(center + vImageScaleFilterWidth);
      for j := left to right do
      begin
        weight := AFilter(center - j);
        if weight = 0.0 then
          continue;
        if (j < 0) then
          n := -j
        else if (j >= ASrcWidth) then
          n := ASrcWidth - j + ASrcWidth - 1
        else
          n := j;
        k := contrib^[i].n;
        contrib^[i].n := contrib^[i].n + 1;
        contrib^[i].p^[k].pixel := n;
        contrib^[i].p^[k].weight := weight;
      end;
    end;
  end;

  size := ADstWidth * ASrcHeight * SizeOf(TIntermediateFormat);
  GetMem(tempBuf2, size);

  // Apply filter to sample horizontally from Src to Work
  for k := 0 to ASrcHeight - 1 do
  begin
    SourceLine := @tempBuf1[k * ASrcWidth];
    DestLine := @tempBuf2[k * ADstWidth];
    for i := 0 to ADstWidth - 1 do
    begin
      color1 := cSuperBlack;
      for j := 0 to contrib^[i].n - 1 do
      begin
        weight := contrib^[i].p^[j].weight;
        if weight = 0.0 then
          continue;
        color2 := SourceLine[contrib^[i].p^[j].pixel];
        color1.R := color1.R + color2.R * weight;
        color1.G := color1.G + color2.G * weight;
        color1.B := color1.B + color2.B * weight;
        color1.A := color1.A + color2.A * weight;
      end;
      // Set new pixel value
      DestLine[i] := color1;
    end;
  end;

  // Free the memory allocated for horizontal filter weights
  for i := 0 to ADstWidth - 1 do
    FreeMem(contrib^[i].p);
  FreeMem(contrib);

  // Pre-calculate filter contributions for a column
  GetMem(contrib, ADstHeight * SizeOf(TCList));
  // Vertical sub-sampling
  // Scales from bigger to smaller height
  if yscale < 1.0 then
  begin
    width := vImageScaleFilterWidth / yscale;
    fscale := 1.0 / yscale;
    for i := 0 to ADstHeight - 1 do
    begin
      contrib^[i].n := 0;
      GetMem(contrib^[i].p, Trunc(width * 2.0 + 1) * SizeOf(TContributor));
      center := i / yscale;
      left := floor(center - width);
      right := ceil(center + width);
      for j := left to right do
      begin
        weight := AFilter((center - j) / fscale) / fscale;
        if weight = 0.0 then
          continue;
        if (j < 0) then
          n := -j
        else if (j >= ASrcHeight) then
          n := MaxInteger(ASrcHeight - j + ASrcHeight - 1, 0)
        else
          n := j;
        k := contrib^[i].n;
        contrib^[i].n := contrib^[i].n + 1;
        contrib^[i].p^[k].pixel := n;
        contrib^[i].p^[k].weight := weight;
      end;
    end
  end
  else
  // Vertical super-sampling
  // Scales from smaller to bigger height
  begin
    for i := 0 to ADstHeight - 1 do
    begin
      contrib^[i].n := 0;
      GetMem(contrib^[i].p, Trunc(vImageScaleFilterWidth * 2.0 + 1) * SizeOf(TContributor));
      center := i / yscale;
      left := floor(center - vImageScaleFilterWidth);
      right := ceil(center + vImageScaleFilterWidth);
      for j := left to right do
      begin
        weight := AFilter(center - j);
        if weight = 0.0 then
          continue;
        if j < 0 then
          n := -j
        else if (j >= ASrcHeight) then
          n := MaxInteger(ASrcHeight - j + ASrcHeight - 1, 0)
        else
          n := j;
        k := contrib^[i].n;
        contrib^[i].n := contrib^[i].n + 1;
        contrib^[i].p^[k].pixel := n;
        contrib^[i].p^[k].weight := weight;
      end;
    end;
  end;

  size := ADstWidth * ADstHeight * SizeOf(TIntermediateFormat);
  ReallocMem(tempBuf1, size);

  // Apply filter to sample vertically from Work to Dst
  for k := 0 to ADstWidth - 1 do
  begin
    for i := 0 to ADstHeight - 1 do
    begin
      color1 := cSuperBlack;
      for j := 0 to contrib^[i].n - 1 do
      begin
        weight := contrib^[i].p^[j].weight;
        if weight = 0.0 then
          continue;
        color2 := tempBuf2[k + contrib^[i].p^[j].pixel * ADstWidth];
        color1.R := color1.R + color2.R * weight;
        color1.G := color1.G + color2.G * weight;
        color1.B := color1.B + color2.B * weight;
        color1.A := color1.A + color2.A * weight;
      end;
      tempBuf1[k + i * ADstWidth] := color1;
    end;
  end;

  // Free the memory allocated for vertical filter weights
  for i := 0 to ADstHeight - 1 do
    FreeMem(contrib^[i].p);

  FreeMem(contrib);

  FreeMem(tempBuf2);
  // Back to native image format
  try
    ConvertFromIntermediateFormat(tempBuf1, ADst, AColorFormat, ADstWidth, ADstHeight);
  except
    FreeMem(tempBuf1);
    raise;
  end;
  FreeMem(tempBuf1);
end;

procedure Div2(var Value: Integer); inline;
begin
  Value := Value div 2;
  if Value = 0 then
    Value := 1;
end;

procedure Build2DMipmap(
  const ASrc: Pointer;
  const ADst: TPointerArray;
  AColorFormat: Cardinal;
  ADataType: Cardinal;
  AFilter: TImageFilterFunction;
  ASrcWidth, ASrcHeight: Integer);

var
  ConvertToIntermediateFormat: TConvertToImfProc;
  ConvertFromIntermediateFormat: TConvertFromInfProc;
  ADstWidth, ADstHeight: Integer;
  i, j, k, n, size, level: Integer;
  tempBuf1, tempBuf2, storePtr, SourceLine, DestLine: PIntermediateFormatArray;
  contrib: PCListList;
  xscale, yscale: Single;
  width, fscale, weight: Single;
  center: Single;
  left, right: Integer;
  color1, color2: TIntermediateFormat;
  tempW, tempH: Integer;

begin
  if ASrcWidth < 1 then
    Exit;
  ASrcHeight := MaxInteger(1, ASrcHeight);

  // Allocate memory
  tempW := ASrcWidth;
  tempH := ASrcHeight;
  size := 0;
  for level := 0 to High(ADst) + 1 do
  begin
    Inc(size, tempW * tempH * SizeOf(TIntermediateFormat));
    Div2(tempW);
    Div2(tempH);
  end;
  GetMem(tempBuf1, size);
  storePtr := tempBuf1;
  FillChar(tempBuf1^, size, $00);
  GetMem(tempBuf2, ASrcWidth * ASrcHeight * SizeOf(TIntermediateFormat));

  // Find function to convert external format to intermediate format
  ConvertToIntermediateFormat := UnsupportedToImf;
  ConvertFromIntermediateFormat := UnsupportedFromImf;
  for i := 0 to high(cConvertTable) do
  begin
    if ADataType = cConvertTable[i].type_ then
    begin
      ConvertToIntermediateFormat := cConvertTable[i].proc1;
      ConvertFromIntermediateFormat := cConvertTable[i].proc2;
      break;
    end;
  end;

  try
    ConvertToIntermediateFormat(ASrc, tempBuf1, AColorFormat, ASrcWidth, ASrcHeight);
  except
    FreeMem(tempBuf1);
    raise;
  end;

  contrib := nil;
  tempW := ASrcWidth;
  tempH := ADstHeight;

  try
    // Downsampling
    for level := 0 to High(ADst) do
    begin
      ADstWidth := ASrcWidth;
      ADstHeight := ASrcHeight;
      Div2(ADstWidth);
      Div2(ADstHeight);

      xscale := MaxFloat((ADstWidth - 1) / (ASrcWidth - 1), 0.25);
      yscale := MaxFloat((ADstHeight - 1) / (ASrcHeight - 1), 0.25);

      // Pre-calculate filter contributions for a row
      ReallocMem(contrib, ADstWidth * SizeOf(TCList));
      // Horizontal sub-sampling
      // Scales from bigger to smaller width
      width := vImageScaleFilterWidth / xscale;
      fscale := 1.0 / xscale;
      for i := 0 to ADstWidth - 1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, Trunc(width * 2.0 + 1.0) * SizeOf(TContributor));
        center := i / xscale;
        left := floor(center - width);
        right := ceil(center + width);
        for j := left to right do
        begin
          weight := AFilter((center - j) / fscale) / fscale;
          if weight = 0.0 then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= ASrcWidth) then
            n := MaxInteger(ASrcWidth - j + ASrcWidth - 1, 0)
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end;

      // Apply filter to sample horizontally from Src to Work
      for k := 0 to ASrcHeight - 1 do
      begin
        SourceLine := @tempBuf1[k * ASrcWidth];
        DestLine := @tempBuf2[k * ADstWidth];
        for i := 0 to ADstWidth - 1 do
        begin
          color1 := cSuperBlack;
          for j := 0 to contrib^[i].n - 1 do
          begin
            weight := contrib^[i].p^[j].weight;
            if weight = 0.0 then
              continue;
            color2 := SourceLine[contrib^[i].p^[j].pixel];
            color1.R := color1.R + color2.R * weight;
            color1.G := color1.G + color2.G * weight;
            color1.B := color1.B + color2.B * weight;
            color1.A := color1.A + color2.A * weight;
          end;
          // Set new pixel value
          DestLine[i] := color1;
        end;
      end;

      // Free the memory allocated for horizontal filter weights
      for i := 0 to ADstWidth - 1 do
        FreeMem(contrib^[i].p);

      // Pre-calculate filter contributions for a column
      ReallocMem(contrib, ADstHeight * SizeOf(TCList));
      // Vertical sub-sampling
      // Scales from bigger to smaller height
      width := vImageScaleFilterWidth / yscale;
      fscale := 1.0 / yscale;
      for i := 0 to ADstHeight - 1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, Trunc(width * 2.0 + 1) * SizeOf(TContributor));
        center := i / yscale;
        left := floor(center - width);
        right := ceil(center + width);
        for j := left to right do
        begin
          weight := AFilter((center - j) / fscale) / fscale;
          if weight = 0.0 then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= ASrcHeight) then
            n := MaxInteger(ASrcHeight - j + ASrcHeight - 1, 0)
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end;

      size := ASrcWidth * ASrcHeight * SizeOf(TIntermediateFormat);
      Inc(PByte(tempBuf1), size);

      // Apply filter to sample vertically from Work to Dst
      for k := 0 to ADstWidth - 1 do
      begin
        for i := 0 to ADstHeight - 1 do
        begin
          color1 := cSuperBlack;
          for j := 0 to contrib^[i].n - 1 do
          begin
            weight := contrib^[i].p^[j].weight;
            if weight = 0.0 then
              continue;
            n := k + contrib^[i].p^[j].pixel * ADstWidth;
            color2 := tempBuf2[n];
            color1.R := color1.R + color2.R * weight;
            color1.G := color1.G + color2.G * weight;
            color1.B := color1.B + color2.B * weight;
            color1.A := color1.A + color2.A * weight;
          end;
          tempBuf1[k + i * ADstWidth] := color1;
        end;
      end;

      // Free the memory allocated for vertical filter weights
      for i := 0 to ADstHeight - 1 do
        FreeMem(contrib^[i].p);

      ASrcWidth := ADstWidth;
      ASrcHeight := ADstHeight;

      // Back to native image format
      ConvertFromIntermediateFormat(
        tempBuf1, ADst[level], AColorFormat, ASrcWidth, ASrcHeight);
    end;
  finally
    if Assigned(contrib) then
      FreeMem(contrib);
    FreeMem(tempBuf2);
    FreeMem(storePtr);
  end;
end;

procedure AlphaGammaBrightCorrection(
  const ASrc: Pointer;
  AColorFormat: Cardinal;
  ADataType: Cardinal;
  ASrcWidth, ASrcHeight: Integer;
  anAlphaProc: TImageAlphaProc;
  ABrightness: Single;
  AGamma: Single);

var
  ConvertToIntermediateFormat: TConvertToImfProc;
  ConvertFromIntermediateFormat: TConvertFromInfProc;
  tempBuf1: PIntermediateFormatArray;
  Size, I: Integer;
begin
  if ASrcWidth < 1 then
    Exit;
  ASrcHeight := MaxInteger(1, ASrcHeight);
  Size := ASrcWidth * ASrcHeight;
  GetMem(tempBuf1, Size * SizeOf(TIntermediateFormat));

  // Find function to convert external format to intermediate format
  ConvertToIntermediateFormat := UnsupportedToImf;
  ConvertFromIntermediateFormat := UnsupportedFromImf;
  for i := 0 to high(cConvertTable) do
  begin
    if ADataType = cConvertTable[i].type_ then
    begin
      ConvertToIntermediateFormat := cConvertTable[i].proc1;
      ConvertFromIntermediateFormat := cConvertTable[i].proc2;
      break;
    end;
  end;

  try
    ConvertToIntermediateFormat(
      ASrc, tempBuf1, AColorFormat, ASrcWidth, ASrcHeight);

    vTopLeftColor := tempBuf1[0];
    vBottomRightColor := tempBuf1[Size-1];

    if Assigned(anAlphaProc) then
      for I := Size - 1 downto 0 do
          anAlphaProc(tempBuf1[I]);

    if ABrightness <> 1.0 then
      for I := Size - 1 downto 0 do
        with tempBuf1[I] do
        begin
          R := R * ABrightness;
          G := G * ABrightness;
          B := B * ABrightness;
        end;

    if AGamma <> 1.0 then
      for I := Size - 1 downto 0 do
        with tempBuf1[I] do
        begin
          R := Power(R, AGamma);
          G := Power(G, AGamma);
          B := Power(B, AGamma);
        end;

    // Back to native image format
    ConvertFromIntermediateFormat(
      tempBuf1, ASrc, AColorFormat, ASrcWidth, ASrcHeight);

  except
    FreeMem(tempBuf1);
    raise;
  end;
  FreeMem(tempBuf1);
end;

end.
