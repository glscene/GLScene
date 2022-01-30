//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.Graphics;

(*
   Utility class and functions to manipulate a bitmap in OpenGL's default
   byte order (GL_RGBA vs TBitmap's GL_BGRA)

   Note: TGLBitmap32 = TGLImage has support for Alex Denissov's Graphics32 library
   (http://www.g32.org), just make sure the USE_GRAPHICS32 conditionnal
   is active in GLScene.inc and recompile.
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Math,
  VCL.Graphics,
  VCL.Imaging.Pngimage,
  VCL.Consts,

  {$IFDEF USE_GRAPHICS32} GR32, {$ENDIF}

  GLS.VectorTypes,
  GLS.State,
  GLS.ApplicationFileIO,
  GLS.PersistentClasses,
  GLS.Context,
  GLS.ImageUtils,
  GLS.Color,
  GLS.TextureFormat,
  GLS.VectorGeometry,
  GLS.Utils,
  GLS.Strings,
  GLS.Logger;

{$DEFINE PRF_HACK_PASSES}

type
  TGLPixel24 = packed record
    r, g, b: Byte;
  end;
  PGLPixel24 = ^TGLPixel24;

  TGLPixel32 = packed record
    r, g, b, a: Byte;
  end;
  PGLPixel32 = ^TGLPixel32;

  TGLPixel32Array = array[0..MaxInt shr 3] of TGLPixel32;
  PGLPixel32Array = ^TGLPixel32Array;

  TGLLODStreamingState = (ssKeeping, ssLoading, ssLoaded, ssTransfered);

  TGLImageLevelDesc = record
    Width: Integer;
    Height: Integer;
    Depth: Integer;
    PBO: TGLUnpackPBOHandle;
    MapAddress: Pointer;
    Offset: LongWord;
    StreamOffset: LongWord;
    Size: LongWord;
    State: TGLLODStreamingState;
  end;

  TGLImageLODRange = 0..15;

  TGLImagePiramid = array[TGLImageLODRange] of TGLImageLevelDesc;

  TGLBaseImage = class(TGLDataFile)
  private
    FSourceStream: TStream;
    FStreamLevel: TGLImageLODRange;
    FFinishEvent: TFinishTaskEvent;
{$IFDEF USE_SERVICE_CONTEXT}
    procedure ImageStreamingTask; stdcall;
{$ENDIF}
  protected
    fData: PGLPixel32Array;
    FLOD: TGLImagePiramid;
    fLevelCount: TGLImageLODRange;
    fColorFormat: Cardinal;
    fInternalFormat: TGLInternalFormat;
    fDataType: Cardinal;
    fElementSize: Integer;
    fCubeMap: Boolean;
    fTextureArray: Boolean;
    function GetData: PGLPixel32Array; virtual;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetDepth: Integer;
    function GetLevelAddress(ALevel: Byte): Pointer; overload;
    function GetLevelAddress(ALevel, AFace: Byte): Pointer; overload;
    function GetLevelWidth(ALOD: TGLImageLODRange): Integer;
    function GetLevelHeight(ALOD: TGLImageLODRange): Integer;
    function GetLevelDepth(ALOD: TGLImageLODRange): Integer;
    function GetLevelPBO(ALOD: TGLImageLODRange): TGLUnpackPBOHandle;
    function GetLevelOffset(ALOD: TGLImageLODRange): Integer;
    function GetLevelSizeInByte(ALOD: TGLImageLODRange): Integer;
    function GetLevelStreamingState(ALOD: TGLImageLODRange): TGLLODStreamingState;
    procedure SetLevelStreamingState(ALOD: TGLImageLODRange; AState: TGLLODStreamingState);
    procedure SaveHeader;
    procedure LoadHeader;
    procedure StartStreaming;
    procedure DoStreaming;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetTextureTarget: TGLTextureTarget;
    // Registers the bitmap's content as an OpenGL texture map.
    procedure RegisterAsOpenGLTexture(
      AHandle: TGLTextureHandle;
      aMipmapGen: Boolean;
      aTexFormat: Cardinal;
      out texWidth: integer;
      out texHeight: integer;
      out texDepth: integer); virtual;
    // Assigns from any Texture.
    function AssignFromTexture(
      AHandle: TGLTextureHandle;
      const CastToFormat: Boolean;
      const intFormat: TGLInternalFormat = tfRGBA8;
      const colorFormat: Cardinal = 0;
      const dataType: Cardinal = 0): Boolean; virtual;
    (* Convert vertical cross format of non compressed, non mipmaped image
       to six face of cube map *)
    function ConvertCrossToCubeMap: Boolean;
    // Convert flat image to volume by dividing it into slice.
    function ConvertToVolume(const col, row: Integer; const MakeArray: Boolean): Boolean;
    // Return size in byte of all image
    function DataSize: Cardinal;
    //True if the bitmap is empty (ie. width or height is zero).
    function IsEmpty: Boolean;
    function IsCompressed: Boolean;
    function IsVolume: Boolean;
    //Narrow image data to simple RGBA8 ubyte
    procedure Narrow;
    //Generate LOD pyramid
    procedure GenerateMipmap(AFilter: TImageFilterFunction); virtual;
    // Leave top level and remove other
    procedure UnMipmap; virtual;
    // Direct Access to image data
    property Data: PGLPixel32Array read GetData;
    // Set image of error.
    procedure SetErrorImage;
    // Recalculate levels information based on first level.
    procedure UpdateLevelsInfo;
    property LevelWidth[ALOD: TGLImageLODRange]: Integer read GetLevelWidth;
    property LevelHeight[ALOD: TGLImageLODRange]: Integer read GetLevelHeight;
    property LevelDepth[ALOD: TGLImageLODRange]: Integer read GetLevelDepth;
    property LevelPixelBuffer[ALOD: TGLImageLODRange]: TGLUnpackPBOHandle
      read GetLevelPBO;
    //  LOD offset in byte
    property LevelOffset[ALOD: TGLImageLODRange]: Integer read GetLevelOffset;
    //  LOD size in byte
    property LevelSizeInByte[ALOD: TGLImageLODRange]: Integer
      read GetLevelSizeInByte;
    property LevelStreamingState[ALOD: TGLImageLODRange]: TGLLODStreamingState
      read GetLevelStreamingState write SetLevelStreamingState;
    // Number of levels.
    property LevelCount: TGLImageLODRange read fLevelCount;
    property InternalFormat: TGLInternalFormat read FInternalFormat;
    property ColorFormat: Cardinal read fColorFormat;
    property DataType: Cardinal read fDataType;
    property ElementSize: Integer read fElementSize;
    property CubeMap: Boolean read fCubeMap;
    property TextureArray: Boolean read fTextureArray;
  end;

  TGLBaseImageClass = class of TGLBaseImage;

  (* Contains and manipulates a 32 bits (24+8) bitmap.
     This is the base class for preparing and manipulating textures in GLS.Scene,
     this function does not rely on a windows handle and should be used for
     in-memory manipulations only.
     16 bits textures are automatically converted to 24 bits and an opaque (255)
     alpha channel is assumed for all planes, the byte order is as specified
     in GL_RGBA. If 32 bits is used in this class, it can however output 16 bits texture
     data for use in OpenGL.
     The class has support for registering its content as a texture, as well
     as for directly drawing/reading from the current OpenGL buffer. *)
  TGLImage = class(TGLBaseImage)
  private
    FVerticalReverseOnAssignFromBitmap: Boolean;
    FBlank: Boolean;
    fOldColorFormat: Cardinal;
    fOldDataType: Cardinal;
    procedure DataConvertTask;
  protected
    procedure SetWidth(val: Integer);
    procedure SetHeight(const val: Integer);
    procedure SetDepth(const val: Integer);
    procedure SetBlank(const Value: Boolean);
    procedure SetCubeMap(const val: Boolean);
    procedure SetArray(const val: Boolean);
    function GetScanLine(index: Integer): PGLPixel32Array;
    procedure AssignFrom24BitsBitmap(aBitmap: TBitmap);
    procedure AssignFrom32BitsBitmap(aBitmap: TBitmap);
{$IFDEF USE_GRAPHICS32}
    procedure AssignFromBitmap32(aBitmap32: TBitmap32);
{$ENDIF}
    procedure AssignFromPngImage(aPngImage: TPngImage);
  public
    constructor Create; override;
    destructor Destroy; override;
    // Accepts TGLImage and TGraphic subclasses.
    procedure Assign(Source: TPersistent); override;
    (* Assigns from a 24 bits bitmap without swapping RGB.
      This is faster than a regular assignment, but R and B channels
      will be reversed (from what you would view in a TImage). Suitable
      if you do your own drawing and reverse RGB on the drawing side.
      If you're after speed, don't forget to set the bitmap's dimensions
      to a power of two! *)
    procedure AssignFromBitmap24WithoutRGBSwap(aBitmap: TBitmap);
    (* Assigns from a 2D Texture.
      The context which holds the texture must be active and the texture
      handle valid. *)
    procedure AssignFromTexture2D(textureHandle: Cardinal); overload;
    (*Assigns from a Texture handle.
      If the handle is invalid, the bitmap32 will be empty. *)
    procedure AssignFromTexture2D(textureHandle: TGLTextureHandle); overload;
    // Create a 32 bits TBitmap from self content.
    function Create32BitsBitmap: TBitmap;
    // Width of the bitmap.
    property Width: Integer read GetWidth write SetWidth;
    // Height of the bitmap.
    property Height: Integer read GetHeight write SetHeight;
    // Depth of the bitmap.
    property Depth: Integer read GetDepth write SetDepth;
    // OpenGL color format
    property ColorFormat: Cardinal read fColorFormat;
    // Recommended texture internal format
    property InternalFormat: TGLInternalFormat read FInternalFormat write
      FInternalFormat;
    // OpenGL data type
    property DataType: Cardinal read fDataType;
    // Size in bytes of pixel or block
    property ElementSize: Integer read fElementSize;
    property CubeMap: Boolean read fCubeMap write SetCubeMap;
    property TextureArray: Boolean read fTextureArray write SetArray;
    (* Access to a specific Bitmap ScanLine. index should be in the [0; Height[ range.
      Warning : this function is NOT protected against invalid indexes,
      and invoking it is invalid if the bitmap is Empty. *)
    property ScanLine[index: Integer]: PGLPixel32Array read GetScanLine;
    property VerticalReverseOnAssignFromBitmap: Boolean read
      FVerticalReverseOnAssignFromBitmap write
      FVerticalReverseOnAssignFromBitmap;
    (* Set Blank to true if you actually don't need to allocate data in main menory.
      Useful for textures that are generated by the GPU on the fly. *)
    property Blank: boolean read FBlank write SetBlank;
    // Recast image OpenGL data type and color format.
    procedure SetColorFormatDataType(const AColorFormat, ADataType: Cardinal);
    (* Set Alpha channel values to the pixel intensity.
      The intensity is calculated as the mean of RGB components. *)
    procedure SetAlphaFromIntensity;
    (* Set Alpha channel to 0 for pixels of given color, 255 for others).
      This makes pixels of given color totally transparent while the others
      are completely opaque. *)
    procedure SetAlphaTransparentForColor(const aColor: TColor); overload;
    procedure SetAlphaTransparentForColor(const aColor: TGLPixel32); overload;
    procedure SetAlphaTransparentForColor(const aColor: TGLPixel24); overload;
    // Set Alpha channel values to given byte value.
    procedure SetAlphaToValue(const aValue: Byte);
    // Set Alpha channel values to given float [0..1] value.
    procedure SetAlphaToFloatValue(const aValue: Single);
    (* Inverts the AlphaChannel component.
      What was transparent becomes opaque and vice-versa. *)
    procedure InvertAlpha;
    // AlphaChannel components are replaced by their sqrt.
    procedure SqrtAlpha;
    // Apply a brightness (scaled saturating) correction to the RGB components.
    procedure BrightnessCorrection(const factor: Single);
    // Apply a gamma correction to the RGB components.
    procedure GammaCorrection(const gamma: Single);
    (* Downsample the bitmap by a factor of 2 in both dimensions.
      If one of the dimensions is 1 or less, does nothing. *)
    procedure DownSampleByFactor2;
    (* Reads the given area from the current active OpenGL rendering context.
      The best spot for reading pixels is within a SceneViewer's PostRender
      event : the scene has been fully rendered and the OpenGL context
      is still active. *)
    procedure ReadPixels(const area: TRect);
    (* Draws the whole bitmap at given position in the current OpenGL context.
      This function must be called with a rendering context active.
      Blending and Alpha channel functions are not altered by this function
      and must be adjusted separately. *)
    procedure DrawPixels(const x, y: Single);
    (* Converts a grayscale 'elevation' bitmap to normal map.
      Actually, only the Green component in the original bitmap is used. *)
    procedure GrayScaleToNormalMap(const scale: Single;
      wrapX: Boolean = True; wrapY: Boolean = True);
    // Assumes the bitmap content is a normal map and normalizes all pixels.
    procedure NormalizeNormalMap;
    // Converts a TImage back into a TBitmap
    procedure AssignToBitmap(aBitmap: TBitmap);
    // Generate level of detail.
    procedure GenerateMipmap(AFilter: TImageFilterFunction); override;
    // Clear all levels except first.
    procedure UnMipmap; override;
  end;

  TGLBitmap32 = TGLImage;

  TGLRasterFileFormat = class
  public
    BaseImageClass: TGLBaseImageClass;
    Extension: string;
    Description: string;
    DescResID: Integer;
  end;

  // Stores registered raster file formats.
  TGLRasterFileFormatsList = class(TGLPersistentObjectList)
  public
    destructor Destroy; override;
    procedure Add(const Ext, Desc: string; DescID: Integer; AClass:
      TGLBaseImageClass);
    function FindExt(ext: string): TGLBaseImageClass;
    function FindFromFileName(const fileName: string): TGLBaseImageClass;
    function FindFromStream(const AStream: TStream): TGLBaseImageClass;
    procedure Remove(AClass: TGLBaseImageClass);
    procedure BuildFilterStrings(imageFileClass: TGLBaseImageClass;
      var descriptions, filters: string;
      formatsThatCanBeOpened: Boolean = True;
      formatsThatCanBeSaved: Boolean = False);
    function FindExtByIndex(index: Integer;
      formatsThatCanBeOpened: Boolean = True;
      formatsThatCanBeSaved: Boolean = False): string;
  end;

  EInvalidRasterFile = class(Exception);

procedure Div2(var Value: Integer);
procedure BGR24ToRGB24(src, dest: Pointer; pixelCount: Integer);
procedure BGR24ToRGBA32(src, dest: Pointer; pixelCount: Integer);
procedure RGB24ToRGBA32(src, dest: Pointer; pixelCount: Integer);
procedure BGRA32ToRGBA32(src, dest: Pointer; pixelCount: Integer);
procedure GammaCorrectRGBArray(base: Pointer; pixelCount: Integer; gamma: Single);
procedure BrightenRGBArray(base: Pointer; pixelCount: Integer; factor: Single);
// Read access to the list of registered vector file formats
function GetRasterFileFormats: TGLRasterFileFormatsList;
(* Returns an extension by its index
   in the internal image files dialogs filter.
   Use InternalImageFileFormatsFilter to obtain the filter. *)
function RasterFileFormatExtensionByIndex(index: Integer): string;
procedure RegisterRasterFormat(const AExtension, ADescription: string;
  AClass: TGLBaseImageClass);
procedure UnregisterRasterFormat(AClass: TGLBaseImageClass);
// Return an optimal number of texture pyramid
function GetImageLodNumber(w, h, d: Integer; IsVolume: Boolean): Integer;

(* Returns the TGraphicClass associated to the extension, if any.
   Accepts anExtension with or without the '.' *)
function GraphicClassForExtension(const anExtension: string): TGraphicClass;

(* Adds to the passed TStrings the list of registered formats.
   Convention is "extension=description" for the string, the Objects hold
   the corresponding TGraphicClass (extensions do not include the '.'). *)
procedure HackTPictureRegisteredFormats(destList: TStrings);

var
  vVerticalFlipDDS: Boolean = True;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

var
  vRasterFileFormats: TGLRasterFileFormatsList;

// ------------------------------ Raster File Registries

function GetRasterFileFormats: TGLRasterFileFormatsList;
begin
  if not Assigned(vRasterFileFormats) then
    vRasterFileFormats := TGLRasterFileFormatsList.Create;
  Result := vRasterFileFormats;
end;

procedure RegisterRasterFormat(const AExtension, ADescription: string;
  AClass: TGLBaseImageClass);
begin
  RegisterClass(AClass);
  GetRasterFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

procedure UnregisterRasterFormat(AClass: TGLBaseImageClass);
begin
  if Assigned(vRasterFileFormats) then
    vRasterFileFormats.Remove(AClass);
end;

function RasterFileFormatExtensionByIndex(index: Integer): string;
begin
  Result := GetRasterFileFormats.FindExtByIndex(index);
end;

destructor TGLRasterFileFormatsList.Destroy;
begin
  Clean;
  inherited;
end;

procedure TGLRasterFileFormatsList.Add(const Ext, Desc: string; DescID: Integer;
  AClass: TGLBaseImageClass);
var
  newRec: TGLRasterFileFormat;
begin
  newRec := TGLRasterFileFormat.Create;
  with newRec do
  begin
    Extension := AnsiLowerCase(Ext);
    BaseImageClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(newRec);
end;

function TGLRasterFileFormatsList.FindExt(Ext: string): TGLBaseImageClass;
var
  i: Integer;
begin
  Ext := AnsiLowerCase(Ext);
  for i := Count - 1 downto 0 do
    with TGLRasterFileFormat(Items[i]) do
    begin
      if Extension = Ext then
      begin
        Result := BaseImageClass;
        Exit;
      end;
    end;
  Result := nil;
end;

function TGLRasterFileFormatsList.FindFromFileName(const fileName: string):
  TGLBaseImageClass;
var
  Ext: string;
begin
  Ext := ExtractFileExt(FileName);
  System.Delete(Ext, 1, 1);
  Result := FindExt(Ext);
  if not Assigned(Result) then
    raise EInvalidRasterFile.CreateFmt(strUnknownExtension,
      [Ext, 'GLFile' + UpperCase(Ext)]);
end;

function TGLRasterFileFormatsList.FindFromStream(const AStream: TStream): TGLBaseImageClass;
var
  Ext: string;
  magic: array [0 .. 1] of LongWord;
begin
  magic[0] := 0;
  magic[1] := 1;
  AStream.ReadBuffer(magic, 2 * SizeOf(LongWord));
  AStream.Seek(-2 * SizeOf(LongWord), 1);

  if magic[0] = $20534444 then
    Ext := 'DDS'
  else if magic[1] = $4354334F then
    Ext := 'O3TC'
  else if (magic[0] and $0000FFFF) = $00003F23 then
    Ext := 'HDR'
  else if (magic[0] = $474E5089) and (magic[1] = $0A1A0A0D) then
    Ext := 'PNG'
  else if (magic[0] = $E0FFD8FF) and (magic[1] = $464A1000) then
    Ext := 'JPG';

  Result := FindExt(Ext);
  if not Assigned(Result) then
    raise EInvalidRasterFile.CreateFmt(strUnknownExtension,
      [Ext, 'GLFile' + UpperCase(Ext)]);
end;

procedure TGLRasterFileFormatsList.Remove(AClass: TGLBaseImageClass);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if TGLRasterFileFormat(Items[i]).BaseImageClass.InheritsFrom(AClass) then
      DeleteAndFree(i);
  end;
end;

procedure TGLRasterFileFormatsList.BuildFilterStrings(
  imageFileClass: TGLBaseImageClass;
  var descriptions, filters: string;
  formatsThatCanBeOpened: Boolean = True;
  formatsThatCanBeSaved: Boolean = False);
var
  k, i: Integer;
  p: TGLRasterFileFormat;
begin
  descriptions := '';
  filters := '';
  k := 0;
  for i := 0 to Count - 1 do
  begin
    p := TGLRasterFileFormat(Items[i]);
    if p.BaseImageClass.InheritsFrom(imageFileClass) and (p.Extension <> '')
      and ((formatsThatCanBeOpened and (dfcRead in
      p.BaseImageClass.Capabilities))
      or (formatsThatCanBeSaved and (dfcWrite in p.BaseImageClass.Capabilities))) then
    begin
      with p do
      begin
        if k <> 0 then
        begin
          descriptions := descriptions + '|';
          filters := filters + ';';
        end;
        if (Description = '') and (DescResID <> 0) then
          Description := LoadStr(DescResID);
        FmtStr(descriptions, '%s%s (*.%s)|*.%2:s', [descriptions, Description,
          Extension]);
        filters := filters + '*.' + Extension;
        Inc(k);
      end;
    end;
  end;
  if (k > 1) and (not formatsThatCanBeSaved) then
    FmtStr(descriptions, '%s (%s)|%1:s|%s',
      [sAllFilter, filters, descriptions]);
end;

function TGLRasterFileFormatsList.FindExtByIndex(index: Integer;
  formatsThatCanBeOpened: Boolean = True;
  formatsThatCanBeSaved: Boolean = False): string;
var
  i: Integer;
  p: TGLRasterFileFormat;
begin
  Result := '';
  if index > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      p := TGLRasterFileFormat(Items[i]);
      if (formatsThatCanBeOpened and (dfcRead in p.BaseImageClass.Capabilities))
        or (formatsThatCanBeSaved and (dfcWrite in
        p.BaseImageClass.Capabilities)) then
      begin
        if index = 1 then
        begin
          Result := p.Extension;
          Break;
        end
        else
          Dec(index);
      end;
    end;
  end;
end;

procedure Div2(var Value: Integer);
begin
  Value := Value div 2;
  if Value = 0 then
    Inc(Value);
end;

function GetImageLodNumber(w, h, d: integer; IsVolume: Boolean): Integer;
var
  L: Integer;
begin
  L := 1;
  d := MaxInteger(d, 1);
  while ((w > 1) or (h > 1) or (d > 1)) do
  begin
    Div2(w);
    Div2(h);
    if IsVolume then
      Div2(d);
    Inc(L);
  end;
  Result := L;
end;

procedure CalcImagePiramid(var APiramid: TGLImagePiramid);
begin
  //
end;

// -------------------- RGBA Utils

procedure GammaCorrectRGBArray(base: Pointer; pixelCount: Integer;
  gamma: Single);
var
  vGammaLUT: array[0..255] of Byte;
  invGamma: Single;
  i: Integer;
  ptr: PByte;
begin
  if pixelCount < 1 then
    Exit;
  Assert(gamma > 0);
  // build LUT
  if gamma < 0.1 then
    invGamma := 10
  else
    invGamma := 1 / gamma;
  for i := 0 to 255 do
    vGammaLUT[i] := Round(255 * Power(i * (1 / 255), InvGamma));
  // perform correction
  ptr := base;
  for i := 0 to pixelCount * 3 - 1 do
  begin
    ptr^ := vGammaLUT[ptr^];
    Inc(ptr);
  end;
end;

procedure GammaCorrectRGBAArray(base: Pointer; pixelCount: Integer;
  gamma: Single);
var
  vGammaLUT: array [0 .. 255] of Byte;
  pLUT: PByteArray;
  invGamma: Single;
  i: Integer;
  ptr: PByte;
begin
  if pixelCount < 1 then
    Exit;
  Assert(gamma > 0);
  // build LUT
  if gamma < 0.1 then
    invGamma := 10
  else
    invGamma := 1 / gamma;
  for i := 0 to 255 do
    vGammaLUT[i] := Round(255 * Power(i * (1 / 255), invGamma));
  // perform correction
  ptr := base;
  pLUT := @vGammaLUT[0];
  for i := 0 to pixelCount - 1 do
  begin
    ptr^ := pLUT^[ptr^];
    Inc(ptr);
    ptr^ := pLUT^[ptr^];
    Inc(ptr);
    ptr^ := pLUT^[ptr^];
    Inc(ptr, 2);
  end;
end;

procedure BrightenRGBArray(base: Pointer; pixelCount: Integer; factor: Single);
var
  vBrightnessLUT: array[0..255] of Byte;
  i, k: Integer;
  ptr: PByte;
begin
  if pixelCount < 1 then
    Exit;
  Assert(factor >= 0);
  // build LUT
  for i := 0 to 255 do
  begin
    k := Round(factor * i);
    if k > 255 then
      k := 255;
    vBrightnessLUT[i] := Byte(k);
  end;
  // perform correction
  ptr := base;
  for i := 0 to pixelCount * 3 - 1 do
  begin
    ptr^ := vBrightnessLUT[ptr^];
    Inc(ptr);
  end;
end;

procedure BrightenRGBAArray(base: Pointer; pixelCount: Integer; factor: Single);
var
  vBrightnessLUT: array[0..255] of Byte;
  pLUT: PByteArray;
  i: Integer;
  ptr: PByte;
  k: Integer;
begin
  if pixelCount < 1 then
    Exit;
  Assert(factor >= 0);
  // build LUT
  for i := 0 to 255 do
  begin
    k := Round(factor * i);
    if k > 255 then
      k := 255;
    vBrightnessLUT[i] := k;
  end;
  // perform correction
  ptr := base;
  pLUT := @vBrightnessLUT[0];
  for i := 0 to pixelCount - 1 do
  begin
    ptr^ := pLUT^[ptr^];
    Inc(ptr);
    ptr^ := pLUT^[ptr^];
    Inc(ptr);
    ptr^ := pLUT^[ptr^];
    Inc(ptr, 2);
  end;
end;

procedure BGR24ToRGB24(src, dest: Pointer; pixelCount: Integer); register;
begin
  while pixelCount > 0 do
  begin
    PAnsiChar(dest)[0] := PAnsiChar(src)[2];
    PAnsiChar(dest)[1] := PAnsiChar(src)[1];
    PAnsiChar(dest)[2] := PAnsiChar(src)[0];
    Inc(PAnsiChar(dest), 3);
    Inc(PAnsiChar(src), 3);
    Dec(pixelCount);
  end;
end;

procedure BGR24ToRGBA32(src, dest: Pointer; pixelCount: Integer);
begin
  while pixelCount > 0 do
  begin
    PAnsiChar(dest)[0] := PAnsiChar(src)[2];
    PAnsiChar(dest)[1] := PAnsiChar(src)[1];
    PAnsiChar(dest)[2] := PAnsiChar(src)[0];
    PAnsiChar(dest)[3] := #255;
    Inc(PAnsiChar(dest), 4);
    Inc(PAnsiChar(src), 3);
    Dec(pixelCount);
  end;
end;

procedure RGB24ToRGBA32(src, dest: Pointer; pixelCount: Integer);
begin
  while pixelCount > 0 do
  begin
    PAnsiChar(dest)[0] := PAnsiChar(src)[0];
    PAnsiChar(dest)[1] := PAnsiChar(src)[1];
    PAnsiChar(dest)[2] := PAnsiChar(src)[2];
    PAnsiChar(dest)[3] := #255;
    Inc(PAnsiChar(dest), 4);
    Inc(PAnsiChar(src), 3);
    Dec(pixelCount);
  end;
end;

procedure BGRA32ToRGBA32(src, dest: Pointer; pixelCount: Integer);
begin
  while pixelCount > 0 do
  begin
    PAnsiChar(dest)[0] := PAnsiChar(src)[2];
    PAnsiChar(dest)[1] := PAnsiChar(src)[1];
    PAnsiChar(dest)[2] := PAnsiChar(src)[0];
    PAnsiChar(dest)[3] := PAnsiChar(src)[3];
    Inc(PAnsiChar(dest), 4);
    Inc(PAnsiChar(src), 4);
    Dec(pixelCount);
  end;
end;


// ------------------
// ------------------ TGLBaseImage ------------------
// ------------------

constructor TGLBaseImage.Create;
begin
  inherited Create(Self);
  FillChar(FLOD, SizeOf(TGLImagePiramid), $00);
  fLevelCount := 1; // first level always is present
  fColorFormat := GL_RGBA;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fCubeMap := False;
  fTextureArray := False;
end;

destructor TGLBaseImage.Destroy;
var
  level: TGLImageLODRange;
begin
  if Assigned(fData) then
  begin
    FreeMem(fData);
    fData := nil;
  end;
  FreeAndNil(FFinishEvent);
  for level := 0 to High(TGLImageLODRange) do
  begin
    FLOD[level].PBO.Free;
  end;
  FSourceStream.Free;
  inherited Destroy;
end;

procedure TGLBaseImage.Assign(Source: TPersistent);
var
  img: TGLBaseImage;
  Size: Integer;
begin
  if Source is TGLBaseImage then
  begin
    img := Source as TGLBaseImage;
    FLOD := img.FLOD;
    fLevelCount := img.fLevelCount;
    fColorFormat := img.fColorFormat;
    fInternalFormat := img.fInternalFormat;
    fDataType := img.fDataType;
    fElementSize := img.fElementSize;
    fCubeMap := img.fCubeMap;
    fTextureArray := img.fTextureArray;
    Size := img.DataSize;
    ReallocMem(fData, Size);
    Move(img.fData^, fData^, Size);
  end
  else if Source <> nil then
    inherited; // raise AssingError
end;

function TGLBaseImage.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTexture2D;
  // Choose a texture target
  if GetHeight = 1 then
    Result := ttTexture1D;
  if fCubeMap then
    Result := ttTextureCube;
  if IsVolume then
    Result := ttTexture3D;
  if fTextureArray then
  begin
    if (GetDepth < 2) then
      Result := ttTexture1Darray
    else
      Result := ttTexture2DArray;
    if fCubeMap then
      Result := ttTextureCubeArray;
  end;
  if ((FInternalFormat >= tfFLOAT_R16)
    and (FInternalFormat <= tfFLOAT_RGBA32)) then
    Result := ttTextureRect;
end;

function TGLBaseImage.DataSize: Cardinal;
var
  L: TGLImageLODRange;
  s: Cardinal;
begin
  s := 0;
  if not IsEmpty then
  begin
    UpdateLevelsInfo;
    for l := 0 to FLevelCount - 1 do
      s := s + FLOD[l].Size;
  end;
  Result := s;
end;

function TGLBaseImage.GetWidth: Integer;
begin
  Result := FLOD[0].Width;
end;

function TGLBaseImage.GetDepth: Integer;
begin
  Result := FLOD[0].Depth;
end;

function TGLBaseImage.GetHeight: Integer;
begin
  Result := FLOD[0].Height;
end;

function TGLBaseImage.GetLevelAddress(ALevel: Byte): Pointer;
begin
  Result := fData;
  Inc(PByte(Result), FLOD[ALevel].Offset);
end;

function TGLBaseImage.GetLevelAddress(ALevel, AFace: Byte): Pointer;
begin
  Result := fData;
  Inc(PByte(Result), FLOD[ALevel].Offset);
  Inc(PByte(Result), AFace * (FLOD[ALevel].Size div 6));
end;

function TGLBaseImage.GetLevelDepth(ALOD: TGLImageLODRange): Integer;
begin
  Result := FLOD[ALOD].Depth;
end;

function TGLBaseImage.GetLevelHeight(ALOD: TGLImageLODRange): Integer;
begin
  Result := FLOD[ALOD].Height;
end;

function TGLBaseImage.GetLevelOffset(ALOD: TGLImageLODRange): Integer;
begin
  Result := FLOD[ALOD].Offset;
end;

function TGLBaseImage.GetLevelPBO(ALOD: TGLImageLODRange): TGLUnpackPBOHandle;
begin
  Result := FLOD[ALOD].PBO;
end;

function TGLBaseImage.GetLevelSizeInByte(ALOD: TGLImageLODRange): Integer;
begin
  Result := FLOD[ALOD].Size;
end;

function TGLBaseImage.GetLevelStreamingState(ALOD: TGLImageLODRange): TGLLODStreamingState;
begin
  Result := FLOD[ALOD].State;
end;

function TGLBaseImage.GetLevelWidth(ALOD: TGLImageLODRange): Integer;
begin
  Result := FLOD[ALOD].Width;
end;

function TGLBaseImage.IsEmpty: Boolean;
begin
  Result := (GetWidth = 0) or (GetHeight = 0);
end;

function TGLBaseImage.IsCompressed: Boolean;
begin
  Result := IsCompressedFormat(fInternalFormat);
end;

function TGLBaseImage.IsVolume: Boolean;
begin
  Result := (GetDepth > 0) and not fTextureArray and not fCubeMap;
end;

function TGLBaseImage.ConvertCrossToCubeMap: Boolean;
var
  fW, fH, cubeSize, realCubeSize, e: Integer;
  lData: PByteArray;
  ptr: PGLubyte;
  i, j: Integer;
  bGenMipmap: Boolean;
begin
  Result := False;
  // Can't already be a cubemap
  if fCubeMap or fTextureArray then
    Exit;
  //this function only supports vertical cross format for now (3 wide by 4 high)
  if (GetWidth div 3 <> GetHeight div 4)
    or (GetWidth mod 3 <> 0)
    or (GetHeight mod 4 <> 0)
    or (GetDepth > 0) then
    Exit;

  bGenMipmap := FLevelCount > 1;
  UnMipmap;

  // Get the source data
  lData := PByteArray(fData);
  if IsCompressed then
  begin
    fW := (GetWidth + 3) div 4;
    fH := (GetHeight + 3) div 4;
    realCubeSize := (fH div 4) * 4;
  end
  else
  begin
    fW := GetWidth;
    fH := GetHeight;
    realCubeSize := fH div 4;
  end;
  cubeSize := fH;
  GetMem(fData, fW * fH * fElementSize);
  FLOD[0].Width := realCubeSize;
  FLOD[0].Height := realCubeSize;
  FLOD[0].Depth := 6;

  // Extract the faces
  ptr := PGLubyte(fData);
  // positive X
  for j := 0 to cubeSize - 1 do
  begin
    e := ((fH - (cubeSize + j + 1)) * fW + 2 * cubeSize) * fElementSize;
    Move(lData[e], ptr^, cubeSize * fElementSize);
    Inc(ptr, cubeSize * fElementSize);
  end;
  // negative X
  for j := 0 to cubeSize - 1 do
  begin
    Move(lData[(fH - (cubeSize + j + 1)) * fW * fElementSize],
      ptr^, cubeSize * fElementSize);
    Inc(ptr, cubeSize * fElementSize);
  end;
  // positive Y
  for j := 0 to cubeSize - 1 do
  begin
    e := ((4 * cubeSize - j - 1) * fW + cubeSize) * fElementSize;
    Move(lData[e], ptr^, cubeSize * fElementSize);
    Inc(ptr, cubeSize * fElementSize);
  end;
  // negative Y
  for j := 0 to cubeSize - 1 do
  begin
    e := ((2 * cubeSize - j - 1) * fW + cubeSize) * fElementSize;
    Move(lData[e], ptr^, cubeSize * fElementSize);
    Inc(ptr, cubeSize * fElementSize);
  end;
  // positive Z
  for j := 0 to cubeSize - 1 do
  begin
    e := ((fH - (cubeSize + j + 1)) * fW + cubeSize) * fElementSize;
    Move(lData[e], ptr^, cubeSize * fElementSize);
    Inc(ptr, cubeSize * fElementSize);
  end;
  // negative Z
  for j := 0 to cubeSize - 1 do
    for i := 0 to cubeSize - 1 do
    begin
      e := (j * fW + 2 * cubeSize - (i + 1)) * fElementSize;
      Move(lData[e], ptr^, fElementSize);
      Inc(ptr, fElementSize);
    end;
  // Set the new # of faces, width and height
  fCubeMap := true;
  FreeMem(lData);

  if bGenMipmap then
    GenerateMipmap(ImageTriangleFilter);

  Result := true;
end;

function TGLBaseImage.ConvertToVolume(const col, row: Integer; const MakeArray:
  Boolean): Boolean;
var
  fW, fH, sW, sH, sD: Integer;
  lData: PByteArray;
  ptr: PGLubyte;
  i, j, k: Integer;
begin
  Result := false;
  if fCubeMap then
    Exit;

  if (GetDepth > 0) and not fTextureArray and MakeArray then
  begin
    // Let volume be array
    fTextureArray := True;
    Result := True;
    Exit;
  end;
  if fTextureArray and not MakeArray then
  begin
    // Let array be volume
    fTextureArray := False;
    Result := True;
    Exit;
  end;

  Result := MakeArray;

  // Check sizes
  sD := col * row;
  if sD < 1 then
    Exit;
  if IsCompressed then
  begin
    fW := (GetWidth + 3) div 4;
    fH := (GetHeight + 3) div 4;
  end
  else
  begin
    fW := GetWidth;
    fH := GetHeight;
  end;
  sW := fW div col;
  sH := fH div row;
  if (sW = 0) or (sH = 0) then
  begin
    Result := False;
    Exit;
  end;

  // Mipmaps are not supported
  UnMipmap;
  // Get the source data
  lData := PByteArray(fData);
  GetMem(fData, sW * sH * sD * fElementSize);
  ptr := PGLubyte(fData);
  for i := 0 to row - 1 do
    for j := 0 to col - 1 do
      for k := 0 to sH - 1 do
      begin
        Move(lData[(i * fW * sH + j * sW + k * fW) * fElementSize],
          ptr^, sW * fElementSize);
        Inc(ptr, sW * fElementSize);
      end;

  if IsCompressed then
  begin
    FLOD[0].Width := sW * 4;
    FLOD[0].Height := sH * 4;
  end
  else
  begin
    FLOD[0].Width := sW;
    FLOD[0].Height := sH;
  end;
  FLOD[0].Depth := sD;
  fTextureArray := Result;
  FreeMem(lData);
  Result := True;
end;

procedure TGLBaseImage.SetErrorImage;
const
cTextureError: array[0..12287] of Byte = (
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$00,$00,$FF,$FF,$B6,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$DB,$90,$3A,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$3A,$00,$00,$FF,$DB,$90,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$90,$DB,$FF,$FF,$B6,$90,$90,$DB,$FF,$00,$00,$3A,$00,$00,$00,$00,$00,$00,$90,$3A,$00,$FF,$FF,$DB,$00,$66,$B6,$66,$00,$00,$FF,$FF,$B6,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$3A,$00,$00,$FF,$DB,$90,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$90,$DB,$FF,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$DB,$90,$3A,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$00,$00,$FF,$FF,$B6,$00,$66,$B6,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$90,$DB,$FF,$00,$00,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$DB,$90,$3A,$B6,$FF,$FF,$00,$00,$66,$B6,$66,$00,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$90,$90,$DB,$FF,$FF,$B6,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$FF,$FF,$FF,$B6,$FF,$FF,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$00,$00,$FF,$FF,$B6,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$FF,$FF,$FF,$B6,$FF,$FF,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$90,$DB,$FF,$00,$00,$3A,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$00,$00,$FF,$FF,$B6,$3A,$90,$DB,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$DB,$90,$3A,$90,$DB,$FF,$00,$00,$3A,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$90,$DB,$FF,$00,$00,$3A,$90,$3A,$00,$FF,$FF,$DB,$00,$66,$B6,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$90,$3A,$00,$FF,$FF,$DB,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$DB,$90,$3A,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$90,$3A,$00,$FF,$FF,$DB,$00,$66,$B6,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$90,$DB,$FF,$00,$00,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$00,$00,$FF,$FF,$B6,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$90,$3A,$00,$FF,$FF,$DB,$00,$66,$B6,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$00,$00,$FF,$FF,$B6,$FF,$FF,$FF,$FF,$FF,$FF,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$90,$DB,$FF,$00,$00,$3A,$00,$00,$00,$00,$00,$00,$66,$00,$00,$FF,$FF,$B6,$3A,$90,$DB,$66,$00,$00,$FF,$FF,$B6,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$DB,$90,$3A,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$90,$3A,$00,$FF,$FF,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$DB,$90,$3A,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$DB,$90,$3A,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$66,$00,$00,$FF,$FF,$B6,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$DB,$90,$3A,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$B6,$FF,$FF,$00,$00,$66,$00,$00,$00,$00,$00,$00,$DB,$90,$3A,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$DB,$90,$3A,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$90,$3A,$00,$FF,$FF,$DB,$00,$66,$B6,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$B6,$FF,$FF,$00,$00,$66,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$90,$3A,$00,$DB,$FF,$DB,$00,$3A,$90,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$B6,$66,$00,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$00,$00,$FF,$FF,$B6,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$90,$3A,$00,$DB,$FF,$DB,$00,$3A,$90,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$00,$00,$00,$66,$00,$00,$FF,$FF,$B6,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$00,$00,$FF,$FF,$B6,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$90,$DB,$FF,$00,$00,$3A,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$66,$00,$00,$FF,$FF,$B6,$00,$66,$B6,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$B6,$FF,$FF,$00,$00,$66,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$90,$DB,$FF,$00,$00,$3A,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$90,$DB,$FF,$00,$00,$3A,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$B6,$66,$00,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$90,$3A,$00,$FF,$FF,$DB,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$DB,$FF,$FF,$00,$3A,$90,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$FF,$B6,$66,$66,$B6,$FF,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$90,$3A,$00,$FF,$FF,$DB,$00,$66,$B6,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$FF,$FF,$00,$00,$66,$00,$00,$00,$00,$00,$00,$90,$3A,$00,$FF,$FF,$DB,$00,$66,$B6,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$90,$3A,$00,$FF,$FF,$DB,$00,$66,$B6,$00,$00,$00,$00,$00,$00,$3A,$00,$00,$FF,$DB,$90,$3A,$90,$DB,$00,$00,$00,$66,$00,$00,$FF,$FF,$B6,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3A,$90,$DB,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$90,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$66,$00,$00,$00,$66,$B6,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$DB,$90,$3A,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$66,$B6,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$B6,$90,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$DB,$90,$3A,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$66,$B6,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$DB,$90,$3A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3A,$90,$DB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$B6,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
);

begin
  UnMipmap;
  FLOD[0].Width := 64;
  FLOD[0].Height := 64;
  FLOD[0].Depth := 0;
  fColorFormat := GL_RGBA;
  fInternalFormat := tfRGB8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fCubeMap := false;
  fTextureArray := false;
  FColorFormat := GL_RGB;
  ReallocMem(FData, DataSize);
  Move(cTextureError[0], FData[0], DataSize);
end;

procedure TGLBaseImage.SetLevelStreamingState(ALOD: TGLImageLODRange;
  AState: TGLLODStreamingState);
begin
  FLOD[ALOD].State := AState;
end;

procedure TGLBaseImage.Narrow;
var
  Size: Integer;
  newData: Pointer;
begin
  // Check for already norrow
  if (fColorFormat = GL_RGBA)
    and (GetDepth = 0)
    and (fDataType = GL_UNSIGNED_BYTE)
    and (FLevelCount = 1)
    and not (fTextureArray or fCubeMap) then
    Exit;

  UnMipmap;
  // Use image utils
  Size := GetWidth * GetHeight * 4;
  GetMem(newData, Size);
  try
    ConvertImage(
      fData, newData,
      fColorFormat, GL_RGBA,
      fDataType, GL_UNSIGNED_BYTE,
      GetWidth, GetHeight);
  except
    GLSLogger.LogError(Format(strCantConvertImg, [ClassName]));
    SetErrorImage;
    FreeMem(newData);
    Exit;
  end;
  fInternalFormat := tfRGBA8;
  fColorFormat := GL_RGBA;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fTextureArray := False;
  fCubeMap := False;
  FreeMem(fData);
  fData := newData;
end;

procedure TGLBaseImage.GenerateMipmap(AFilter: TImageFilterFunction);
var
  LAddresses: TPointerArray;
  level, slice, d: Integer;
begin
  UnMipmap;
  if IsVolume then
  begin
    fLevelCount := GetImageLodNumber(GetWidth, GetHeight, GetDepth, True);
    UpdateLevelsInfo;
    ReallocMem(FData, DataSize);
    // Message Hint 'TGLBaseImage.GenerateMipmap not yet implemented for volume images'
  end
  else
  begin
    fLevelCount := GetImageLodNumber(GetWidth, GetHeight, GetDepth, False);
    ReallocMem(FData, DataSize);

    SetLength(LAddresses, fLevelCount-1);
    for level := 1 to fLevelCount-1 do
      LAddresses[level-1] := GetLevelAddress(level);
    d := MaxInteger(GetDepth, 1);
    for slice := 0 to d - 1 do
    begin
      Build2DMipmap(GetLevelAddress(0), LAddresses, fColorFormat, fDataType,
        AFilter, GetWidth, GetHeight);
      for level := 1 to fLevelCount - 1 do
        Inc(PByte(LAddresses[level - 1]), GetLevelSizeInByte(level) div d);
    end;
  end;
end;

procedure TGLBaseImage.UnMipmap;
var
  level: TGLImageLODRange;
begin
  for level := 1 to High(TGLImageLODRange) do
  begin
    FLOD[level].Width := 0;
    FLOD[level].Height := 0;
    FLOD[level].Depth := 0;
  end;
  FLevelCount := 1;
end;

procedure TGLBaseImage.UpdateLevelsInfo;
var
  level: TGLImageLODRange;
  w, h, d: Integer;

  function GetSize(const level: Integer): Integer;
  var
    ld, bw, bh, lsize: Integer;
  begin
    if fTextureArray then
      ld := FLOD[0].Depth
    else
      ld := d;
    if ld = 0 then
      ld := 1;

    if IsCompressed then
    begin
      bw := (w + 3) div 4;
      bh := (h + 3) div 4;
    end
    else
    begin
      bw := w;
      bh := h;
    end;
    if bh = 0 then
      bh := 1;

    lsize := bw * bh * ld * fElementSize;
    if fCubeMap and not fTextureArray then
      lsize := lsize * 6;
    // Align to Double Word
    if (lsize and 3) <> 0 then
      lsize := 4 * (1 + lsize div 4);
    Result := lsize;
  end;

begin
  w := FLOD[0].Width;
  h := FLOD[0].Height;
  d := FLOD[0].Depth;
  FLOD[0].Size := GetSize(0);
  FLOD[0].Offset := 0;

  for level := 1 to High(TGLImageLODRange) do
  begin
    Div2(w);
    Div2(h);
    if not fTextureArray then
      d := d div 2;
    FLOD[level].Width := w;
    FLOD[level].Height := h;
    FLOD[level].Depth := d;
    FLOD[level].Offset := FLOD[level - 1].Offset + FLOD[level - 1].Size;
    FLOD[level].Size := GetSize(level);
  end;
end;

function TGLBaseImage.GetData: PGLPixel32Array;
begin
  Result := fData;
end;

procedure TGLBaseImage.RegisterAsOpenGLTexture(
  AHandle: TGLTextureHandle;
  aMipmapGen: Boolean;
  aTexFormat: Cardinal;
  out texWidth: integer;
  out texHeight: integer;
  out texDepth: integer);
var
  glTarget: Cardinal;
  glHandle: Cardinal;
  Level: integer;
  LLevelCount, face: integer;
  bCompress, bBlank: boolean;
  w, h, d, cw, ch, maxSize: TGLsizei;
  p, buffer: Pointer;
  vtcBuffer, top, bottom: PGLubyte;
  i, j, k: Integer;
  transferMethod: 0..3;

  function blockOffset(x, y, z: Integer): Integer;
  begin

    if z >= (d and -4) then
      Result := fElementSize * (cw * ch * (d and -4) + x +
        cw * (y + ch * (z - 4 * ch)))
    else
      Result := fElementSize * (4 * (x + cw * (y + ch * floor(z / 4))) + (z and
        3));
    if Result < 0 then
      Result := 0;
  end;

begin
  if AHandle.Target = ttNoShape then
    Exit;

    UpdateLevelsInfo;

    if Self is TGLImage then
      bBlank := TGLImage(Self).Blank
    else
      bBlank := False;
    // Check for Non-power-of-two
    if not gl.ARB_texture_non_power_of_two then
    begin
      w := RoundUpToPowerOf2(GetWidth);
      h := RoundUpToPowerOf2(GetHeight);
      d := RoundUpToPowerOf2(GetDepth);
      if GetDepth = 0 then
        d := 0;
    end
    else
    begin
      w := GetWidth;
      h := GetHeight;
      d := GetDepth;
    end;

    // Check maximum dimension
    maxSize := CurrentGLContext.GLStates.MaxTextureSize;
    if w > maxSize then
      w := maxSize;
    if h > maxSize then
      h := maxSize;
    texWidth := w;
    texHeight := h;
    texDepth := d;
    LLevelCount := fLevelCount;
    bCompress := IsCompressed;

    // Rescale if need and can
    buffer := nil;
    if (w <> GetWidth) or (h <> GetHeight) then
    begin
      if not((d > 0) // not volume
        or bCompress // not compressed
        or bBlank) then // not blank
      begin
        GetMem(buffer, w * h * fElementSize);
        try
          RescaleImage(fData, buffer, fColorFormat, fDataType,
            ImageLanczos3Filter, GetWidth, GetHeight, w, h);
          LLevelCount := 1;
        except
          bBlank := True;
        end;
      end
      else
        bBlank := True;
    end;
    if Self is TGLImage then
      TGLImage(Self).FBlank := bBlank;

    glHandle := AHandle.Handle;
    glTarget := DecodeTextureTarget(AHandle.Target);

    // Hardware mipmap autogeneration
    aMipmapGen := aMipmapGen and IsTargetSupportMipmap(glTarget);
    aMipmapGen := aMipmapGen and (LLevelCount = 1);
    if aMipmapGen then
    begin
      if gl.SGIS_generate_mipmap then
      begin
        if gl.EXT_direct_state_access then
          gl.TexParameterf(
            glTarget,
            GL_GENERATE_MIPMAP_SGIS,
            GL_TRUE)
        else
          gl.TexParameteri(glTarget, GL_GENERATE_MIPMAP_SGIS, GL_TRUE);
      end
      else
      begin
        // Software LODs generation
        Self.GenerateMipmap(ImageTriangleFilter);
        LLevelCount := LevelCount;
      end;
    end;

    // Setup top limitation of LODs
    if gl.SGIS_texture_lod and (LLevelCount > 1) then
      if gl.EXT_direct_state_access then
        gl.TexParameterf(
          glTarget,
          GL_TEXTURE_MAX_LEVEL_SGIS,
          LLevelCount - 1)
      else
        gl.TexParameteri(glTarget, GL_TEXTURE_MAX_LEVEL_SGIS, LLevelCount - 1);

    // Select transfer method
    if bCompress then
      transferMethod := 1
    else
      transferMethod := 0;
    if gl.EXT_direct_state_access then
      transferMethod := transferMethod + 2;

    // if image is blank then doing only allocatation texture in videomemory
    vtcBuffer := nil;
    case AHandle.Target of

      ttTexture1D:
        for level := 0 to LLevelCount - 1 do
        begin
          if Assigned(buffer) then
            p := buffer
          else if not bBlank then
            p := GetLevelAddress(level)
          else
            p := nil;

          case transferMethod of
            0: gl.TexImage1D(glTarget, level, aTexFormat, w, 0, fColorFormat,
                fDataType, p);
            1: gl.CompressedTexImage1D(glTarget, level, aTexFormat, w, 0,
                GetLevelSizeInByte(level), p);
            2: gl.TextureImage1D(glHandle, glTarget, level, aTexFormat, w, 0,
                fColorFormat, fDataType, p);
            3: gl.CompressedTextureImage1D(glHandle, glTarget, level, aTexFormat, w,
                0, GetLevelSizeInByte(level), p)
          end;

          Div2(w);
        end;

      ttTexture2D:
        for Level := 0 to LLevelCount - 1 do
        begin
          if Assigned(buffer) then
            p := buffer
          else if not bBlank then
            p := GetLevelAddress(Level)
          else
            p := nil;

          case transferMethod of
            0: gl.TexImage2D(glTarget, level, aTexFormat, w, h, 0, fColorFormat,
                fDataType, p);
            1: gl.CompressedTexImage2D(glTarget, level, aTexFormat, w, h, 0,
                GetLevelSizeInByte(level), p);
            2: gl.TextureImage2D(glHandle, glTarget, level, aTexFormat, w, h, 0,
                fColorFormat, fDataType, p);
            3: gl.CompressedTextureImage2D(glHandle, glTarget, level, aTexFormat, w,
                h, 0, GetLevelSizeInByte(level), p);
          end;

          Div2(w);
          Div2(h);
        end;

      ttTextureRect:
        begin
          if Assigned(buffer) then
            p := buffer
          else if not bBlank then
            p := GetLevelAddress(0)
          else
            p := nil;

          case transferMethod of
            0: gl.TexImage2D(glTarget, 0, aTexFormat, w, h, 0, fColorFormat,
                fDataType, p);
            1: gl.CompressedTexImage2D(glTarget, 0, aTexFormat, w, h, 0,
                GetLevelSizeInByte(0), p);
            2: gl.TextureImage2D(glHandle, glTarget, 0, aTexFormat, w, h, 0,
                fColorFormat, fDataType, p);
            3: gl.CompressedTextureImage2D(glHandle, glTarget, 0, aTexFormat, w, h,
                0, GetLevelSizeInByte(0), p);
          end;
        end;

      ttTexture3D:
        for Level := 0 to LLevelCount - 1 do
        begin
          if Assigned(buffer) then
            p := buffer
          else if not bBlank then
            p := GetLevelAddress(Level)
          else
            p := nil;

          if gl.NV_texture_compression_vtc and bCompress then
          begin
            // Shufle blocks for Volume Texture Compression
            if Assigned(p) then
            begin
              cw := (w + 3) div 4;
              ch := (h + 3) div 4;
              if Level = 0 then
                GetMem(vtcBuffer, GetLevelSizeInByte(0));
              top := p;
              for k := 0 to d - 1 do
                for i := 0 to ch - 1 do
                  for j := 0 to cw - 1 do
                  begin
                    bottom := vtcBuffer;
                    Inc(bottom, blockOffset(j, i, k));
                    Move(top^, bottom^, fElementSize);
                    Inc(top, fElementSize);
                  end;
            end;
            if gl.EXT_direct_state_access then
              gl.CompressedTextureImage3D(glHandle, glTarget, level, aTexFormat, w,
                h, d, 0, GetLevelSizeInByte(level), vtcBuffer)
            else
              gl.CompressedTexImage3D(glTarget, level, aTexFormat, w, h, d, 0,
                GetLevelSizeInByte(level), vtcBuffer);
          end
          else
          begin
            // Normal compression
            case transferMethod of
              0: gl.TexImage3D(glTarget, level, aTexFormat, w, h, d, 0,
                  fColorFormat, fDataType, p);
              1: gl.CompressedTexImage3D(glTarget, level, aTexFormat, w, h, d, 0,
                  GetLevelSizeInByte(level), p);
              2: gl.TextureImage3D(glHandle, glTarget, level, aTexFormat, w, h, d,
                  0, fColorFormat, fDataType, p);
              3: gl.CompressedTextureImage3D(glHandle, glTarget, level, aTexFormat,
                  w, h, d, 0, GetLevelSizeInByte(level), p);
            end;
          end;
          Div2(w);
          Div2(h);
          Div2(d);
        end;

      ttTextureCube:
        for Level := 0 to LLevelCount - 1 do
        begin
          for face := GL_TEXTURE_CUBE_MAP_POSITIVE_X to
            GL_TEXTURE_CUBE_MAP_NEGATIVE_Z do
          begin
            if Assigned(buffer) then
              p := buffer
            else if not bBlank then
              p := GetLevelAddress(level, face - GL_TEXTURE_CUBE_MAP_POSITIVE_X)
            else
              p := nil;

            case transferMethod of
              0: gl.TexImage2D(face, level, aTexFormat, w, h, 0, fColorFormat,
                  fDataType, p);
              1: gl.CompressedTexImage2D(face, level, aTexFormat, w, h, 0,
                  GetLevelSizeInByte(level) div 6, p);
              2: gl.TextureImage2D(glHandle, face, level, aTexFormat, w, h, 0,
                  fColorFormat, fDataType, p);
              3: gl.CompressedTextureImage2D(glHandle, face, level, aTexFormat, w,
                  h, 0, GetLevelSizeInByte(level) div 6, p);
            end;

          end;
          Div2(w);
          Div2(h);
        end;

      ttTexture1DArray:
        for Level := 0 to LLevelCount - 1 do
        begin
          if Assigned(buffer) then
            p := buffer
          else if not bBlank then
            p := GetLevelAddress(Level)
          else
            p := nil;

          case transferMethod of
            0: gl.TexImage2D(glTarget, level, aTexFormat, w, h, 0, fColorFormat,
                fDataType, p);
            1: gl.CompressedTexImage2D(glTarget, level, aTexFormat, w, h, 0,
                GetLevelSizeInByte(level), p);
            2: gl.TextureImage2D(glHandle, glTarget, level, aTexFormat, w, h, 0,
                fColorFormat, fDataType, p);
            3:  gl.CompressedTextureImage2D(glHandle, glTarget, level, aTexFormat, w,
                h, 0, GetLevelSizeInByte(level), p);
          end;

          Div2(w);
        end;

      ttTexture2DArray, ttTextureCubeArray:
        for Level := 0 to LLevelCount - 1 do
        begin
          if Assigned(buffer) then
            p := buffer
          else if not bBlank then
            p := GetLevelAddress(Level)
          else
            p := nil;

          case transferMethod of
            0: gl.TexImage3D(glTarget, level, aTexFormat, w, h, d, 0, fColorFormat,
                fDataType, p);
            1: gl.CompressedTexImage3D(glTarget, level, aTexFormat, w, h, d, 0,
                GetLevelSizeInByte(level), p);
            2:  gl.TextureImage3D(glHandle, glTarget, level, aTexFormat, w, h, d, 0,
                fColorFormat, fDataType, p);
            3:  gl.CompressedTextureImage3D(glHandle, glTarget, level, aTexFormat, w,
                h, d, 0, GetLevelSizeInByte(level), p);
          end;

          Div2(w);
          Div2(h);
        end;
    end; // of case

    if Assigned(buffer) then
      FreeMem(buffer);
    if Assigned(vtcBuffer) then
      FreeMem(vtcBuffer);
end;

function TGLBaseImage.AssignFromTexture(AHandle: TGLTextureHandle;
  const CastToFormat: Boolean; const intFormat: TGLInternalFormat = tfRGBA8;
  const colorFormat: Cardinal = 0; const dataType: Cardinal = 0): Boolean;
var
  LContext: TGLContext;
  texFormat, texLod, optLod: Cardinal;
  glTarget: Cardinal;
  level, maxFace, face: Integer;
  lData: PGLubyte;
  residentFormat: TGLInternalFormat;
  bCompressed: Boolean;
  vtcBuffer, top, bottom: PGLubyte;
  i, j, k: Integer;
  w, d, h, cw, ch: Integer;

  function blockOffset(x, y, z: Integer): Integer;
  begin

    if z >= (d and -4) then
      Result := fElementSize * (cw * ch * (d and -4) + x + cw *
        (y + ch * (z - 4 * ch)))
    else
      Result := fElementSize * (4 * (x + cw * (y + ch * floor(z / 4))) +
        (z and 3));
  end;

begin
  Result := False;
  LContext := CurrentGLContext;
  if LContext = nil then
  begin
    LContext := AHandle.RenderingContext;
    if LContext = nil then
      Exit;
  end;

  LContext.Activate;
  if AHandle.IsDataNeedUpdate then
  begin
    LContext.Deactivate;
    Exit;
  end;

  glTarget := DecodeTextureTarget(AHandle.Target);

  try
    LContext.GLStates.TextureBinding[0, AHandle.Target] := AHandle.Handle;

    FLevelCount := 0;
    gl.GetTexParameteriv(glTarget, GL_TEXTURE_MAX_LEVEL, @texLod);
    if glTarget = GL_TEXTURE_CUBE_MAP then
    begin
      fCubeMap := True;
      maxFace := 5;
      glTarget := GL_TEXTURE_CUBE_MAP_POSITIVE_X;
    end
    else
    begin
      fCubeMap := False;
      maxFace := 0;
    end;
    fTextureArray := (glTarget = GL_TEXTURE_1D_ARRAY) or
      (glTarget = GL_TEXTURE_2D_ARRAY) or
      (glTarget = GL_TEXTURE_CUBE_MAP_ARRAY);

    repeat
      // Check level existence
      gl.GetTexLevelParameteriv(glTarget, FLevelCount,
        GL_TEXTURE_INTERNAL_FORMAT,
        @texFormat);
      if texFormat = 1 then
        Break;
      Inc(FLevelCount);
      if FLevelCount = 1 then
      begin
        gl.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_WIDTH, @FLOD[0].Width);
        gl.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_HEIGHT,@FLOD[0].Height);
        FLOD[0].Depth := 0;
        if (glTarget = GL_TEXTURE_3D)
          or (glTarget = GL_TEXTURE_2D_ARRAY)
          or (glTarget = GL_TEXTURE_CUBE_MAP_ARRAY) then
          gl.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_DEPTH, @FLOD[0].Depth);
        residentFormat := OpenGLFormatToInternalFormat(texFormat);
        if CastToFormat then
          fInternalFormat := residentFormat
        else
          fInternalFormat := intFormat;
        FindCompatibleDataFormat(fInternalFormat, fColorFormat, fDataType);
        // Substitute properties if need
        if colorFormat > 0 then
          fColorFormat := colorFormat;
        if dataType > 0 then
          fDataType := dataType;
        // Get optimal number or MipMap levels
        optLod := GetImageLodNumber(GetWidth, GetHeight, GetDepth, glTarget = GL_TEXTURE_3D);
        if texLod > optLod then
          texLod := optLod;
        // Check for MipMap posibility
        if ((fInternalFormat >= tfFLOAT_R16)
          and (fInternalFormat <= tfFLOAT_RGBA32)) then
          texLod := 1;
      end;
    until FLevelCount = Integer(texLod);

    if FLevelCount > 0 then
    begin
      fElementSize := GetTextureElementSize(fColorFormat, fDataType);
      UpdateLevelsInfo;

      ReallocMem(FData, DataSize);
      lData := PGLubyte(fData);
      bCompressed := IsCompressed;
      vtcBuffer := nil;
      w := GetWidth;
      h := GetHeight;
      d := GetDepth;

      for face := 0 to maxFace do
      begin
        if fCubeMap then
          glTarget := face + GL_TEXTURE_CUBE_MAP_POSITIVE_X;
        for level := 0 to FLevelCount - 1 do
        begin
          if bCompressed then
          begin

            if GL.NV_texture_compression_vtc and (d > 1) and not fTextureArray then
            begin
              if level = 0 then
                GetMem(vtcBuffer, GetLevelSizeInByte(0));
              gl.GetCompressedTexImage(glTarget, level, vtcBuffer);
              // Shufle blocks from VTC to S3TC
              cw := (w + 3) div 4;
              ch := (h + 3) div 4;
              top := lData;
              for k := 0 to d - 1 do
                for i := 0 to ch - 1 do
                  for j := 0 to cw - 1 do
                  begin
                    bottom := vtcBuffer;
                    Inc(bottom, blockOffset(j, i, k));
                    Move(bottom^, top^, fElementSize);
                    Inc(top, fElementSize);
                  end;
              Div2(w);
              Div2(h);
              Div2(d);
            end
            else
              gl.GetCompressedTexImage(glTarget, level, lData);
          end
          else
            gl.GetTexImage(glTarget, level, fColorFormat, fDataType, lData);

          Inc(lData, GetLevelSizeInByte(level));
        end; // for level
      end; // for face
      if Assigned(vtcBuffer) then
        FreeMem(vtcBuffer);
      // Check memory corruption
      ReallocMem(FData, DataSize);
    end;

    if Self is TGLImage then
    begin
      TGLImage(Self).FBlank := FLevelCount = 0;
      if FLevelCount = 0 then
      begin
        UnMipmap;
        FreeMem(fData);
        fData := nil;
      end;
    end;
    gl.CheckError;
    Result := True;
  finally
    LContext.Deactivate;
  end;
end;

procedure TGLBaseImage.SaveHeader;
var
  Temp: Integer;
  LStream: TStream;
begin
  Temp := 0;
  LStream := nil;
  try
    LStream := TFileStream.Create(ResourceName, fmOpenWrite or fmCreate);
    with LStream do
    begin
      Write(Temp, SizeOf(Integer)); // Version
      Write(FLOD[0].Width, SizeOf(Integer));
      Write(FLOD[0].Height, SizeOf(Integer));
      Write(FLOD[0].Depth, SizeOf(Integer));
      Write(fColorFormat, SizeOf(Cardinal));
      Temp := Integer(fInternalFormat);
      Write(Temp, SizeOf(Integer));
      Write(fDataType, SizeOf(Cardinal));
      Write(fElementSize, SizeOf(Integer));
      Write(fLevelCount, SizeOf(TGLImageLODRange));
      Temp := Integer(fCubeMap);
      Write(Temp, SizeOf(Integer));
      Temp := Integer(fTextureArray);
      Write(Temp, SizeOf(Integer));
    end;
  finally
    LStream.Free;
  end;
end;

procedure TGLBaseImage.LoadHeader;
var
  Temp: Integer;
  LStream: TStream;
begin
  LStream := nil;
  try
    LStream := TFileStream.Create(ResourceName, fmOpenRead);
    with LStream do
    begin
      Read(Temp, SizeOf(Integer)); // Version
      if Temp > 0 then
      begin
        GLSLogger.LogError(Format(strUnknownArchive, [Self.ClassType, Temp]));
        Abort;
      end;
      Read(FLOD[0].Width, SizeOf(Integer));
      Read(FLOD[0].Height, SizeOf(Integer));
      Read(FLOD[0].Depth, SizeOf(Integer));
      Read(fColorFormat, SizeOf(Cardinal));
      Read(Temp, SizeOf(Integer));
      fInternalFormat := TGLInternalFormat(Temp);
      Read(fDataType, SizeOf(Cardinal));
      Read(fElementSize, SizeOf(Integer));
      Read(fLevelCount, SizeOf(TGLImageLODRange));
      Read(Temp, SizeOf(Integer));
      fCubeMap := Boolean(Temp);
      Read(Temp, SizeOf(Integer));
      fTextureArray := Boolean(Temp);
      UpdateLevelsInfo;
    end;
  finally
    LStream.Free;
  end;
end;

var
  vGlobalStreamingTaskCounter: Integer = 0;

procedure TGLBaseImage.StartStreaming;
var
  level: TGLImageLODRange;
begin
  FStreamLevel := fLevelCount - 1;
  for level := 0 to High(TGLImageLODRange) do
    FLOD[level].State := ssKeeping;
end;

procedure TGLBaseImage.DoStreaming;
begin
{$IFDEF USE_SERVICE_CONTEXT}
  if Assigned(FFinishEvent) then
  begin
    if FFinishEvent.WaitFor(0) <> wrSignaled then
      Exit;
  end
  else
    FFinishEvent := TFinishTaskEvent.Create;

  Inc(vGlobalStreamingTaskCounter);
  AddTaskForServiceContext(ImageStreamingTask, FFinishEvent);
{$ENDIF}
end;

{$IFDEF USE_SERVICE_CONTEXT}
procedure TGLBaseImage.ImageStreamingTask;
var
  readSize: Integer;
  ptr: PByte;
begin
  with FLOD[FStreamLevel] do
  begin
    if PBO = nil then
      PBO := TGLUnpackPBOHandle.Create;

    PBO.AllocateHandle;
    if PBO.IsDataNeedUpdate then
    begin
      (* This may work with multiple unshared context, but never tested
        because unlikely. *)
      PBO.BindBufferData(nil, MaxInteger(Size, 1024), GL_STREAM_DRAW);
      if Assigned(MapAddress) then
        if not PBO.UnmapBuffer then
          Exit;
      MapAddress := PBO.MapBuffer(GL_WRITE_ONLY);
      StreamOffset := 0;
      PBO.UnBind;
      PBO.NotifyDataUpdated;
    end;

    if FSourceStream = nil then
    begin
      FSourceStream := TFileStream.Create(ResourceName + 
	  IntToHex(FStreamLevel,2), fmOpenRead + fmShareDenyNone);
    end;

    // Move to position of next piece and read it
    readSize := MinInteger(Cardinal(8192 div vGlobalStreamingTaskCounter),
      Cardinal(Size - StreamOffset));
    if readSize > 0 then
    begin
      ptr := PByte(MapAddress);
      Inc(ptr, StreamOffset);
      FSourceStream.Read(ptr^, readSize);
      Inc(StreamOffset, readSize);
    end;

    Dec(vGlobalStreamingTaskCounter);

    if StreamOffset >= Size then
    begin
      PBO.Bind;
      if PBO.UnmapBuffer then
        State := ssLoaded;
      PBO.UnBind;
      if State <> ssLoaded then
        Exit; // Can't unmap
      MapAddress := nil;
      StreamOffset := 0;
      if FStreamLevel > 0 then
        Dec(FStreamLevel);
      FSourceStream.Destroy;
      FSourceStream := nil;
    end;
  end;
end;
{$ENDIF}

// ------------------
// ------------------ TGLImage ------------------
// ------------------

constructor TGLImage.Create;
begin
  inherited Create;
  SetBlank(False);
end;

destructor TGLImage.Destroy;
begin
  inherited Destroy;
end;

procedure TGLImage.Assign(Source: TPersistent);
var
  bmp: TBitmap;
  graphic: TGraphic;
begin
  if (Source is TGLImage) or (Source is TGLBaseImage) then
  begin
    if Source is TGLImage then
      FBlank := TGLImage(Source).FBlank
    else
      FBlank := False;

    if not FBlank then
      inherited
    else
    begin
      FLOD := TGLImage(Source).FLOD;
      FLevelCount := TGLImage(Source).FLevelCount;
      fCubeMap := TGLImage(Source).fCubeMap;
      fColorFormat := TGLImage(Source).fColorFormat;
      fInternalFormat := TGLImage(Source).fInternalFormat;
      fDataType := TGLImage(Source).fDataType;
      fElementSize := TGLImage(Source).fElementSize;
      fTextureArray := TGLImage(Source).fTextureArray;
    end;
  end
  else if Source is TGraphic then
  begin
    if (Source is TBitmap)
    and (TBitmap(Source).PixelFormat in [pf24bit, pf32bit])
    and (((TBitmap(Source).Width and 3) = 0) or gl.EXT_bgra) then
    begin
      if TBitmap(Source).PixelFormat = pf24bit then
        AssignFrom24BitsBitmap(TBitmap(Source))
      else
        AssignFrom32BitsBitmap(TBitmap(Source))
    end
    else if Source is TPngImage then
      AssignFromPngImage(TPngImage(Source))
    else
    begin
      graphic := TGraphic(Source);
      bmp := TBitmap.Create;
      try
        // crossbuilder: useless to set pixelformat before setting the size ?
        // or maybe just useless at all on gtk .. as soon as
        // bmp.canvas is touched, it's the pixelformat of the device
        // no matter what was adjusted before ??
        // bmp.PixelFormat:=glpf24bit;
        // bmp.Height:=graphic.Height;
        // crossbuilder: using setsize because setting width or height while
        // the other one is zero results in not setting with/hight
        bmp.PixelFormat := pf24bit;
        bmp.Height := graphic.Height;
        if (graphic.Width and 3) = 0 then
        begin
          bmp.Width := graphic.Width;
          bmp.Canvas.Draw(0, 0, graphic);
        end
        else
        begin
          bmp.Width := (graphic.Width and $FFFC) + 4;
          bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), graphic);
        end;
        AssignFrom24BitsBitmap(bmp);
      finally
        bmp.Free;
      end;
    end;
{$IFDEF USE_GRAPHICS32}
  end
  else if Source is TBitmap32 then
  begin
    Narrow;
    AssignFromBitmap32(TBitmap32(Source));
{$ENDIF}
  end
  else
    inherited;
end;

procedure TGLImage.AssignFrom24BitsBitmap(aBitmap: TBitmap);
var
  y, lineSize: Integer;
  rowOffset: Int64;
  pSrc, pDest: PAnsiChar;
begin
  Assert(aBitmap.PixelFormat = pf24bit);
  UnMipmap;
  FLOD[0].Width := aBitmap.Width;
  FLOD[0].Height := aBitmap.Height;
  FLOD[0].Depth := 0;
  if gl.EXT_bgra then
  begin
    fColorFormat := GL_BGR;
    fElementSize := 3;
  end
  else
  begin
    Assert((aBitmap.Width and 3) = 0);
    fColorFormat := GL_RGBA;
    fElementSize := 4;
  end;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fCubeMap := False;
  fTextureArray := False;
  ReallocMem(fData, DataSize);
  FBlank := False;
  lineSize := GetWidth * fElementSize;
  if Height > 0 then
  begin
    pDest := @PAnsiChar(FData)[GetWidth * fElementSize * (GetHeight - 1)];
    if Height = 1 then
    begin
      if gl.EXT_bgra then
      begin
        pSrc := aBitmap.ScanLine[0]; //BitmapScanLine(aBitmap, 0);
        Move(pSrc^, pDest^, lineSize);
      end
      else
        BGR24ToRGBA32(aBitmap.ScanLine[0], pDest, GetWidth);
    end
    else
    begin
      if VerticalReverseOnAssignFromBitmap then
      begin
        pSrc := aBitmap.ScanLine[GetHeight - 1];
        rowOffset := Integer(aBitmap.ScanLine[GetHeight - 2]) - Integer(pSrc);
      end
      else
      begin
        pSrc := aBitmap.ScanLine[0];
        rowOffset := Int64(aBitmap.ScanLine[1]) - Int64(pSrc);
      end;
      if gl.EXT_bgra then
      begin
        for y := 0 to Height - 1 do
        begin
          Move(pSrc^, pDest^, lineSize);
          Dec(pDest, lineSize);
          Inc(pSrc, rowOffset);
        end;
      end
      else
      begin
        for y := 0 to Height - 1 do
        begin
          BGR24ToRGBA32(pSrc, pDest, Width);
          Dec(pDest, lineSize);
          Inc(pSrc, rowOffset);
        end;
      end;
    end;
  end;
end;

procedure TGLImage.AssignFromBitmap24WithoutRGBSwap(aBitmap: TBitmap);
var
  y: Integer;
  rowOffset: Int64;
  pSrc, pDest: PAnsiChar;
begin
  Assert(aBitmap.PixelFormat = pf24bit);
  Assert((aBitmap.Width and 3) = 0);
  UnMipmap;
  FLOD[0].Width := aBitmap.Width;
  FLOD[0].Height := aBitmap.Height;
  FLOD[0].Depth := 0;
  fColorFormat := GL_RGBA;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fCubeMap := False;
  fTextureArray := False;
  ReallocMem(fData, DataSize);
  FBlank := False;
  if Height > 0 then
  begin
    pDest := @PAnsiChar(fData)[Width * 4 * (Height - 1)];
    if Height = 1 then
    begin
      RGB24ToRGBA32(aBitmap.ScanLine[0], pDest, GetWidth);
    end
    else
    begin
      if VerticalReverseOnAssignFromBitmap then
      begin
        pSrc := aBitmap.ScanLine[GetHeight - 1];
        rowOffset := Cardinal(aBitmap.ScanLine[GetHeight - 2]);
        Dec(rowOffset, Cardinal(pSrc));
      end
      else
      begin
        pSrc := aBitmap.ScanLine[0];
        rowOffset := Cardinal(aBitmap.ScanLine[1]);
        Dec(rowOffset, Cardinal(pSrc));
      end;
      for y := 0 to Height - 1 do
      begin
        RGB24ToRGBA32(pSrc, pDest, GetWidth);
        Dec(pDest, GetWidth * 4);
        Inc(pSrc, rowOffset);
      end;
    end;
  end;
end;

procedure TGLImage.AssignFrom32BitsBitmap(aBitmap: TBitmap);
var
  y: Integer;
  rowOffset: Int64;
  pSrc, pDest: PAnsiChar;
begin
  Assert(aBitmap.PixelFormat = pf32bit);
  UnMipmap;
  FLOD[0].Width := aBitmap.Width;
  FLOD[0].Height := aBitmap.Height;
  FLOD[0].Depth := 0;
  if gl.EXT_bgra then
    fColorFormat := GL_BGRA
  else
  begin
    Assert((aBitmap.Width and 3) = 0);
    fColorFormat := GL_RGBA;
  end;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fCubeMap := False;
  fTextureArray := False;
  ReallocMem(fData, DataSize);
  FBlank := False;
  if Height > 0 then
  begin
    pDest := @PAnsiChar(FData)[Width * 4 * (Height - 1)];
    if VerticalReverseOnAssignFromBitmap then
    begin
      pSrc := aBitmap.ScanLine[Height - 1];
      if Height > 1 then
      begin
        rowOffset := Cardinal(aBitmap.ScanLine[Height - 2]);
        Dec(rowOffset, Cardinal(pSrc));
      end
      else
        rowOffset := 0;
    end
    else
    begin
      pSrc := aBitmap.ScanLine[0];
      if Height > 1 then
      begin
        rowOffset := Cardinal(aBitmap.ScanLine[1]);
        Dec(rowOffset, Cardinal(pSrc));
      end
      else
        rowOffset := 0;
    end;
    if gl.EXT_bgra then
    begin
      for y := 0 to Height - 1 do
      begin
        Move(pSrc^, pDest^, Width * 4);
        Dec(pDest, Width * 4);
        Inc(pSrc, rowOffset);
      end;
    end
    else
    begin
      for y := 0 to Height - 1 do
      begin
        BGRA32ToRGBA32(pSrc, pDest, Width);
        Dec(pDest, Width * 4);
        Inc(pSrc, rowOffset);
      end;
    end;
  end;
end;

{$IFDEF USE_GRAPHICS32}
procedure TGLImage.AssignFromBitmap32(aBitmap32: TBitmap32);
var
  y: Integer;
  pSrc, pDest: PAnsiChar;
begin
  UnMipmap;
  FLOD[0].Width := aBitmap32.Width;
  FLOD[0].Height := aBitmap32.Height;
  FLOD[0].Depth := 0;
  fColorFormat := GL_RGBA;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fCubeMap := False;
  fTextureArray := False;
  ReallocMem(fData, DataSize);
  FBlank := False;
  if Height > 0 then
  begin
    pDest := @PAnsiChar(FData)[Width * 4 * (Height - 1)];
    for y := 0 to Height - 1 do
    begin
      if VerticalReverseOnAssignFromBitmap then
        pSrc := PAnsiChar(aBitmap32.ScanLine[Height - 1 - y])
      else
        pSrc := PAnsiChar(aBitmap32.ScanLine[y]);
      BGRA32ToRGBA32(pSrc, pDest, Width);
      Dec(pDest, Width * 4);
    end;
  end;
end;
{$ENDIF}

procedure TGLImage.AssignFromPngImage(aPngImage: TPngImage);
var
  i, j: Integer;
  SourceScan: PRGBLine;
  DestScan: PGLPixel32Array;
  AlphaScan: VCL.Imaging.Pngimage.PByteArray;
  Pixel: Integer;
begin
  if (aPngImage.Width and 3) > 0 then
    aPngImage.Resize((aPngImage.Width and $FFFC) + 4, aPngImage.Height);
  UnMipmap;
  FLOD[0].Width := aPngImage.Width;
  FLOD[0].Height := aPngImage.Height;
  FLOD[0].Depth := 0;
  fColorFormat := GL_RGBA;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fCubeMap := False;
  fTextureArray := False;
  ReallocMem(fData, DataSize);
  FBlank := False;
  case aPngImage.Header.ColorType of
    { Direct ScanLine (24 Bits) }
    COLOR_RGB, COLOR_RGBALPHA: for j := 1 to aPngImage.Height do
      begin
        SourceScan := aPngImage.Scanline[aPngImage.Height - j];
        AlphaScan := aPngImage.AlphaScanline[aPngImage.Height - j];
        DestScan := ScanLine[Pred(j)];
        for i := 0 to Pred(aPngImage.Width) do
        begin
          DestScan^[i].r := SourceScan^[i].rgbtRed;
          DestScan^[i].g := SourceScan^[i].rgbtGreen;
          DestScan^[i].b := SourceScan^[i].rgbtBlue;
          if Assigned(AlphaScan) then
            DestScan^[i].a := AlphaScan^[i]
          else
            DestScan^[i].a := $FF;
        end;
      end;
  else
    // Internal Decode TColor - Palette
    for j := 1 to aPngImage.Height do
    begin
      AlphaScan := aPngImage.AlphaScanline[aPngImage.Height - j];
      DestScan := ScanLine[Pred(j)];
      for i := 0 to Pred(aPngImage.Width) do
      begin
        Pixel := aPngImage.Pixels[i, aPngImage.Height - j];
        DestScan^[i].r := Pixel and $FF;
        DestScan^[i].g := (Pixel shr 8) and $FF;
        DestScan^[i].b := (Pixel shr 16) and $FF;
        if Assigned(AlphaScan) then
          DestScan^[i].a := AlphaScan^[i]
        else
          DestScan^[i].a := $FF;
      end;
    end;
  end;
end;

procedure TGLImage.AssignFromTexture2D(textureHandle: Cardinal);
var
  oldTex: Cardinal;
begin
  UnMipmap;

  with CurrentGLContext.GLStates do
  begin
    oldTex := TextureBinding[ActiveTexture, ttTexture2D];
    TextureBinding[ActiveTexture, ttTexture2D] := textureHandle;

    gl.GetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @FLOD[0].Width);
    gl.GetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @FLOD[0].Height);
    FLOD[0].Depth := 0;
    fColorFormat := GL_RGBA;
    fInternalFormat := tfRGBA8;
    fDataType := GL_UNSIGNED_BYTE;
    fElementSize := 4;
    fCubeMap := False;
    fTextureArray := False;
    ReallocMem(fData, DataSize);
    gl.GetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, fData);
    FBlank := False;

    TextureBinding[ActiveTexture, ttTexture2D] := oldTex;
  end;
end;

procedure TGLImage.AssignFromTexture2D(textureHandle: TGLTextureHandle);
var
  oldContext: TGLContext;
  contextActivate: Boolean;
begin
  if Assigned(textureHandle) and (textureHandle.Handle <> 0) then
  begin
    oldContext := CurrentGLContext;
    contextActivate := (oldContext <> textureHandle.RenderingContext);
    if contextActivate then
    begin
      if Assigned(oldContext) then
        oldContext.Deactivate;
      textureHandle.RenderingContext.Activate;
    end;

    try
      AssignFromTexture2D(textureHandle.Handle);
    finally
      if contextActivate then
      begin
        textureHandle.RenderingContext.Deactivate;
        if Assigned(oldContext) then
          oldContext.Activate;
      end;
    end;
  end
  else
  begin
    // Make image empty
    UnMipmap;
    FLOD[0].Width := 0;
    FLOD[0].Height := 0;
    FLOD[0].Depth := 0;
    fColorFormat := GL_RGBA;
    fInternalFormat := tfRGBA8;
    fDataType := GL_UNSIGNED_BYTE;
    fElementSize := 4;
    fCubeMap := False;
    fTextureArray := False;
    ReallocMem(fData, DataSize);
  end;
end;

function TGLImage.Create32BitsBitmap: TBitmap;
var
  y, x, x4: Integer;
  pSrc, pDest: PAnsiChar;
begin
  if FBlank then
  begin
    Result := nil;
    Exit;
  end;
  Narrow;

  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.Width := Width;
  Result.Height := Height;

  if Height > 0 then
  begin
    pSrc := @PAnsiChar(fData)[Width * 4 * (Height - 1)];
    for y := 0 to Height - 1 do
    begin
      pDest := Result.ScanLine[y];
      for x := 0 to Width - 1 do
      begin
        x4 := x * 4;
        pDest[x4 + 0] := pSrc[x4 + 2];
        pDest[x4 + 1] := pSrc[x4 + 1];
        pDest[x4 + 2] := pSrc[x4 + 0];
        pDest[x4 + 3] := pSrc[x4 + 3];
      end;
      Dec(pSrc, Width * 4);
    end;
  end;
end;

procedure TGLImage.SetWidth(val: Integer);
begin
  if val <> FLOD[0].Width then
  begin
    Assert(val >= 0);
    FLOD[0].Width := val;
    FBlank := True;
  end;
end;

procedure TGLImage.SetHeight(const val: Integer);
begin
  if val <> FLOD[0].Height then
  begin
    Assert(val >= 0);
    FLOD[0].Height := val;
    FBlank := True;
  end;
end;

procedure TGLImage.SetDepth(const val: Integer);
begin
  if val <> FLOD[0].Depth then
  begin
    Assert(val >= 0);
    FLOD[0].Depth := val;
    FBlank := True;
  end;
end;

procedure TGLImage.SetCubeMap(const val: Boolean);
begin
  if val <> fCubeMap then
  begin
    fCubeMap := val;
    FBlank := True;
  end;
end;

procedure TGLImage.SetArray(const val: Boolean);
begin
  if val <> fTextureArray then
  begin
    fTextureArray := val;
    FBlank := True;
  end;
end;

procedure TGLImage.SetColorFormatDataType(const AColorFormat, ADataType: Cardinal);
begin
  if FBlank then
  begin
    fDataType := ADataType;
    fColorFormat := AColorFormat;
    Exit;
  end;
  fOldDataType := fDataType;
  fOldColorFormat := fColorFormat;
  fDataType := ADataType;
  fColorFormat := AColorFormat;
  fElementSize := GetTextureElementSize(fColorFormat, fDataType);
  DataConvertTask;
end;

function TGLImage.GetScanLine(index: Integer): PGLPixel32Array;
begin
  Narrow;
  Result := PGLPixel32Array(@fData[index * GetWidth]);
end;

procedure TGLImage.SetAlphaFromIntensity;
var
  i: Integer;
begin
  Narrow;
  for i := 0 to (DataSize div 4) - 1 do
    with FData^[i] do
      a := (Integer(r) + Integer(g) + Integer(b)) div 3;
end;

procedure TGLImage.SetAlphaTransparentForColor(const aColor: TColor);
var
  color: TGLPixel24;
begin
  color.r := GetRValue(aColor);
  color.g := GetGValue(aColor);
  color.b := GetBValue(aColor);
  SetAlphaTransparentForColor(color);
end;

procedure TGLImage.SetAlphaTransparentForColor(const aColor: TGLPixel32);
var
  color: TGLPixel24;
begin
  color.r := aColor.r;
  color.g := aColor.g;
  color.b := aColor.b;
  SetAlphaTransparentForColor(color);
end;

procedure TGLImage.SetAlphaTransparentForColor(const aColor: TGLPixel24);
var
  i: Integer;
  intCol: Integer;
begin
  Narrow;
  intCol := (PInteger(@aColor)^) and $FFFFFF;
  for i := 0 to (DataSize div 4) - 1 do
    if PInteger(@FData[i])^ and $FFFFFF = intCol then
      FData^[i].a := 0
    else
      FData^[i].a := 255;
end;

procedure TGLImage.SetAlphaToValue(const aValue: Byte);
var
  i: Integer;
begin
  Narrow;
  for i := 0 to (DataSize div 4) - 1 do
    FData^[i].a := aValue
end;

procedure TGLImage.SetAlphaToFloatValue(const aValue: Single);
begin
  SetAlphaToValue(Byte(Trunc(aValue * 255) and 255));
end;

procedure TGLImage.InvertAlpha;
var
  i: Integer;
begin
  Narrow;
  for i := (DataSize div 4) - 1 downto 0 do
    FData^[i].a := 255 - FData^[i].a;
end;

procedure TGLImage.SqrtAlpha;
var
  i: Integer;
  sqrt255Array: PSqrt255Array;
begin
  Narrow;
  sqrt255Array := GetSqrt255Array;
  for i := 0 to (DataSize div 4) - 1 do
    with FData^[i] do
      a := sqrt255Array^[(Integer(r) + Integer(g) + Integer(b)) div 3];
end;

procedure TGLImage.BrightnessCorrection(const factor: Single);
begin
  if Assigned(FData) then
  begin
    Narrow;
    BrightenRGBAArray(Data, DataSize div 4, factor);
  end;
end;

procedure TGLImage.GammaCorrection(const gamma: Single);
begin
  if Assigned(FData) then
  begin
    Narrow;
    GammaCorrectRGBAArray(Data, DataSize div 4, gamma);
  end;
end;

procedure TGLImage.DownSampleByFactor2;
type
  T2Pixel32 = packed array[0..1] of TGLPixel32;
  P2Pixel32 = ^T2Pixel32;

  procedure ProcessRowPascal(pDest: PGLPixel32; pLineA, pLineB: P2Pixel32; n:
    Integer);
  var
    i: Integer;
  begin
    for i := 0 to n - 1 do
    begin
      pDest^.r := (pLineA^[0].r + pLineA^[1].r + pLineB^[0].r +
        pLineB^[1].r) shr 2;
      pDest^.g := (pLineA^[0].g + pLineA^[1].g + pLineB^[0].g +
        pLineB^[1].g) shr 2;
      pDest^.b := (pLineA^[0].b + pLineA^[1].b + pLineB^[0].b +
        pLineB^[1].b) shr 2;
      pDest^.a := (pLineA^[0].a + pLineA^[1].a + pLineB^[0].a +
        pLineB^[1].a) shr 2;
      Inc(pLineA);
      Inc(pLineB);
      Inc(pDest);
    end;
  end; // }

var
  y, w2, h2: Integer;
  pDest: PGLPixel32;
  pLineA, pLineB: P2Pixel32;
begin
  if (GetWidth <= 1) or (GetHeight <= 1) then
    Exit;
  Narrow;
  w2 := GetWidth shr 1;
  h2 := GetHeight shr 1;
  pDest := @fData[0];
  pLineA := @fData[0];
  pLineB := @fData[Width];
  for y := 0 to h2 - 1 do
  begin
    ProcessRowPascal(pDest, pLineA, pLineB, w2);
    Inc(pDest, w2);
    Inc(pLineA, Width);
    Inc(pLineB, Width);
  end;
  FLOD[0].Width := w2;
  FLOD[0].Height := h2;
  ReallocMem(fData, DataSize);
end;

procedure TGLImage.ReadPixels(const area: TRect);
begin
  UnMipmap;
  FLOD[0].Width := (area.Right - area.Left) and $FFFC;
  FLOD[0].Height := (area.Bottom - area.Top);
  FLOD[0].Depth := 0;
  fColorFormat := GL_RGBA;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fCubeMap := false;
  fTextureArray := false;
  fBlank := false;
  ReallocMem(FData, DataSize);
  gl.ReadPixels(0, 0, GetWidth, GetHeight, GL_RGBA, GL_UNSIGNED_BYTE, FData);
end;

procedure TGLImage.DrawPixels(const x, y: Single);
begin
  if fBlank or IsEmpty then
    Exit;
  // Assert(not CurrentGLContext.GLStates.ForwardContext);
  gl.RasterPos2f(x, y);
  gl.DrawPixels(Width, Height, fColorFormat, fDataType, FData);
end;

procedure TGLImage.GrayScaleToNormalMap(const scale: Single;
  wrapX: Boolean = True; wrapY: Boolean = True);
var
  x, y: Integer;
  dcx, dcy: Single;
  invLen: Single;
  maskX, maskY: Integer;
  curRow, nextRow, prevRow: PGLPixel32Array;
  normalMapBuffer: PGLPixel32Array;
  p: PGLPixel32;
begin
  if Assigned(FData) then
  begin
    Narrow;
    GetMem(normalMapBuffer, DataSize);
    try
      maskX := Width - 1;
      maskY := Height - 1;
      p := @normalMapBuffer[0];
      for y := 0 to Height - 1 do
      begin
        curRow := GetScanLine(y);
        if wrapY then
        begin
          prevRow := GetScanLine((y - 1) and maskY);
          nextRow := GetScanLine((y + 1) and maskY);
        end
        else
        begin
          if y > 0 then
            prevRow := GetScanLine(y - 1)
          else
            prevRow := curRow;
          if y < Height - 1 then
            nextRow := GetScanLine(y + 1)
          else
            nextRow := curRow;
        end;
        for x := 0 to Width - 1 do
        begin
          if wrapX then
            dcx := scale * (curRow^[(x - 1) and maskX].g - curRow^[(x + 1) and
              maskX].g)
          else
          begin
            if x = 0 then
              dcx := scale * (curRow^[x].g - curRow^[x + 1].g)
            else if x < Width - 1 then
              dcx := scale * (curRow^[x - 1].g - curRow^[x].g)
            else
              dcx := scale * (curRow^[x - 1].g - curRow^[x + 1].g);
          end;
          dcy := scale * (prevRow^[x].g - nextRow^[x].g);
          invLen := 127 * RSqrt(dcx * dcx + dcy * dcy + 1);
          with p^ do
          begin
            r := Integer(Round(128 + ClampValue(dcx * invLen, -128, 127)));
            g := Integer(Round(128 + ClampValue(dcy * invLen, -128, 127)));
            b := Integer(Round(128 + ClampValue(invLen, -128, 127)));
            a := 255;
          end;
          Inc(p);
        end;
      end;
      Move(normalMapBuffer^, FData^, DataSize);
    finally
      FreeMem(normalMapBuffer);
    end;
  end;
end;

procedure TGLImage.NormalizeNormalMap;
var
  x, y: Integer;
  sr, sg, sb: Single;
  invLen: Single;
  curRow: PGLPixel32Array;
  p: PGLPixel32;
const
  cInv128: Single = 1 / 128;
begin
  if not IsEmpty and not Blank then
  begin
    Narrow;
    for y := 0 to Height - 1 do
    begin
      curRow := @FData[y * GetWidth];
      for x := 0 to GetWidth - 1 do
      begin
        p := @curRow[x];
        sr := (p^.r - 128) * cInv128;
        sg := (p^.g - 128) * cInv128;
        sb := (p^.b - 128) * cInv128;
        invLen := RSqrt(sr * sr + sg * sg + sb * sb);
        p^.r := Round(128 + 127 * ClampValue(sr * invLen, -1, 1));
        p^.g := Round(128 + 127 * ClampValue(sg * invLen, -1, 1));
        p^.b := Round(128 + 127 * ClampValue(sb * invLen, -1, 1));
      end;
    end;
  end;
end;

procedure TGLImage.SetBlank(const Value: boolean);
begin
  if not Value and not IsEmpty then
    ReallocMem(FData, DataSize);
  FBlank := Value;
end;

procedure TGLImage.AssignToBitmap(aBitmap: TBitmap);
var
  y: integer;
  pSrc, pDest: PAnsiChar;
begin
  Narrow;
  aBitmap.Width := GetWidth;
  aBitmap.Height := GetHeight;
  aBitmap.PixelFormat := pf32bit;
  if FVerticalReverseOnAssignFromBitmap then
  begin
    for y := 0 to GetHeight - 1 do
    begin
      pSrc := @PAnsiChar(FData)[y * (GetWidth * 4)];
      pDest := aBitmap.ScanLine[y];
      BGRA32ToRGBA32(pSrc, pDest, GetWidth);
    end;
  end
  else
  begin
    for y := 0 to GetHeight - 1 do
    begin
      pSrc := @PAnsiChar(FData)[y * (GetWidth * 4)];
      pDest := aBitmap.ScanLine[GetHeight - 1 - y];
      BGRA32ToRGBA32(pSrc, pDest, GetWidth);
    end;
  end;
end;

procedure TGLImage.GenerateMipmap(AFilter: TImageFilterFunction);
begin
  if not FBlank then
    inherited GenerateMipmap(AFilter);
end;

procedure TGLImage.UnMipmap;
begin
  inherited UnMipmap;
  if not (fBlank or IsEmpty) then
    ReallocMem(FData, DataSize);
end;

procedure TGLImage.DataConvertTask;
var
  oldLOD: TGLImagePiramid;
  newData: Pointer;
  ptr: PByte;
  L: TGLImageLODRange;
  d: Integer;
begin
  oldLOD := FLOD;
  if IsVolume then
  begin
    // Message Hint 'TGLImage.DataConvertTask not yet implemented for volume images'
  end
  else
  begin
    GetMem(newData, DataSize);
    d := MaxInteger(GetDepth, 1);

    try
      for L := FLevelCount - 1 downto 0 do
      begin
        ptr := newData;
        Inc(ptr, oldLOD[L].Offset);
        ConvertImage(GetLevelAddress(L), ptr, fOldColorFormat, fColorFormat,
          fOldDataType, fDataType, oldLOD[L].Width, oldLOD[L].Height * d);
      end;
      FreeMem(fData);
      fData := newData;
    except
      FreeMem(newData);
      GLSLogger.LogError(Format(strCantConvertImg, [ClassName]));
      SetErrorImage;
    end;
  end;
end;

function GraphicClassForExtension(const anExtension: string): TGraphicClass;
var
  i: integer;
  sl: TStringList;
  buf: string;
begin
  Result := nil;
  if anExtension = '' then
    Exit;
  if anExtension[1] = '.' then
    buf := Copy(anExtension, 2, MaxInt)
  else
    buf := anExtension;
  sl := TStringList.Create;
  try
    HackTPictureRegisteredFormats(sl);
    i := sl.IndexOfName(buf);
    if i >= 0 then
      Result := TGraphicClass(sl.Objects[i]);
  finally
    sl.Free;
  end;
end;

type
  PFileFormat = ^TFileFormat;

  TFileFormat = record
    GraphicClass: TGraphicClass;
    Extension: string;
    Description: string;
    DescResID: integer;
  end;

// HackTPictureRegisteredFormats
{$IFOPT R+}
  {$DEFINE HackTPictureRegisteredFormats_Disable_RangeCheck}
  {$R-}
{$ENDIF}
procedure HackTPictureRegisteredFormats(destList: TStrings);
var
  pRegisterFileFormat, pCallGetFileFormat, pGetFileFormats, pFileFormats: PAnsiChar;
  iCall: cardinal;
  i: integer;
  list: TList;
  fileFormat: PFileFormat;
begin
  {$MESSAGE WARN 'HackTPictureRegisteredFormats will crash when Graphics.pas is compiled with the 'Use Debug DCUs' option'}

  pRegisterFileFormat := PAnsiChar(@TPicture.RegisterFileFormat);
  if pRegisterFileFormat[0] = #$FF then // in case of BPL redirector
    pRegisterFileFormat := PAnsiChar(PCardinal(PCardinal(@pRegisterFileFormat[2])^)^);
  pCallGetFileFormat := @pRegisterFileFormat[16];
  iCall := PCardinal(pCallGetFileFormat)^;
  pGetFileFormats := @pCallGetFileFormat[iCall + 4];
  pFileFormats := PAnsiChar(PCardinal(@pGetFileFormats[2])^);
  list := TList(PCardinal(pFileFormats)^);

  if list <> nil then
  begin
    for i := 0 to list.Count - 1 do
    begin
      fileFormat := PFileFormat(list[i]);
      destList.AddObject(fileFormat.Extension + '=' + fileFormat.Description,
        TObject(fileFormat.GraphicClass));
    end;
  end;
end;

{$IFDEF HackTPictureRegisteredFormats_Disable_RangeCheck}
  {$R+}
  {$UNDEF HackTPictureRegisteredFormats_Disable_RangeCheck}
{$ENDIF}

//---------------------------------------------------
initialization
//---------------------------------------------------

finalization

  FreeAndNil(vRasterFileFormats);

end.
