//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.TextureFormat;

(* Texture formats and functions *)

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  GLS.VectorTypes,
  GLS.OpenGLTokens,
  GLS.Strings;

type
  // Texture addressing rules
  TGLSeparateTextureWrap = (twRepeat, twClampToEdge, twClampToBorder,
    twMirrorRepeat, twMirrorClampToEdge, twMirrorClampToBorder);

  (* Specifies the texture comparison mode for currently bound depth textures.
    That is, a texture whose internal format is tfDEPTH_COMPONENT* *)
  TGLTextureCompareMode = (tcmNone, tcmCompareRtoTexture);

  // Filtering quality
  TGLTextureFilteringQuality = (tfIsotropic, tfAnisotropic);

  TGLTextureTarget =
  (
    ttNoShape, ttTexture1D, ttTexture2D, ttTexture3D, ttTexture1DArray,
    ttTexture2DArray, ttTextureRect, ttTextureBuffer, ttTextureCube,
    ttTexture2DMultisample, ttTexture2DMultisampleArray, ttTextureCubeArray
  );

  TGLTextureSwizzle = (tswRed, tswGreen, tswBlue, tswAlpha, tswZero, tswOne);
  TSwizzleVector = array[0..3] of TGLTextureSwizzle;

  TGLInternalFormat = (
    tfALPHA4,
    tfALPHA8,
    tfALPHA12,
    tfALPHA16,
    tfDEPTH_COMPONENT16,
    tfDEPTH_COMPONENT24,
    tfDEPTH_COMPONENT32,
    tfLUMINANCE4,
    tfLUMINANCE8,
    tfLUMINANCE12,
    tfLUMINANCE16,
    tfLUMINANCE4_ALPHA4,
    tfLUMINANCE6_ALPHA2,
    tfLUMINANCE8_ALPHA8,
    tfLUMINANCE12_ALPHA4,
    tfLUMINANCE12_ALPHA12,
    tfLUMINANCE16_ALPHA16,
    tfINTENSITY4,
    tfINTENSITY8,
    tfINTENSITY12,
    tfINTENSITY16,
    tfR3_G3_B2,
    tfRGB4,
    tfRGB5,
    tfRGB8,
    tfRGB10,
    tfRGB12,
    tfR16G16B16,
    tfRGBA2,
    tfRGBA4,
    tfRGB5_A1,
    tfRGBA8,
    tfRGB10_A2,
    tfRGBA12,
    tfR16G16B16A16,
    tfCOMPRESSED_RGB_S3TC_DXT1,
    tfCOMPRESSED_RGBA_S3TC_DXT1,
    tfCOMPRESSED_RGBA_S3TC_DXT3,
    tfCOMPRESSED_RGBA_S3TC_DXT5,
    tfSIGNED_LUMINANCE8,
    tfSIGNED_LUMINANCE8_ALPHA8,
    tfSIGNED_RGB8,
    tfSIGNED_RGBA8,
    tfSIGNED_RGB8_UNSIGNED_ALPHA8,
    tfSIGNED_ALPHA8,
    tfSIGNED_INTENSITY8,
    tfHILO16,
    tfSIGNED_HILO16,
    tfDSDT8,
    tfDSDT8_MAG8,
    tfDSDT8_MAG8_INTENSITY8,
    tfHILO8,
    tfSIGNED_HILO8,
    tfFLOAT_R16,
    tfFLOAT_R32,
    tfFLOAT_RG16,
    tfFLOAT_RGB16,
    tfFLOAT_RGBA16,
    tfFLOAT_RG32,
    tfFLOAT_RGB32,
    tfFLOAT_RGBA32,
    tfRGBA_FLOAT32,
    tfRGB_FLOAT32,
    tfALPHA_FLOAT32,
    tfINTENSITY_FLOAT32,
    tfLUMINANCE_FLOAT32,
    tfLUMINANCE_ALPHA_FLOAT32,
    tfRGBA_FLOAT16,
    tfRGB_FLOAT16,
    tfALPHA_FLOAT16,
    tfINTENSITY_FLOAT16,
    tfLUMINANCE_FLOAT16,
    tfLUMINANCE_ALPHA_FLOAT16,
    tfDEPTH24_STENCIL8,
    tfDEPTH_COMPONENT32F,
    tfDEPTH32F_STENCIL8,
    tfSRGB8,
    tfSRGB8_ALPHA8,
    tfSLUMINANCE8,
    tfSLUMINANCE8_ALPHA8,
    tfCOMPRESSED_SRGB_S3TC_DXT1,
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT1,
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT3,
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5,
    tfRGB9_E5,
    tfR11F_G11F_B10F,
    tfCOMPRESSED_LUMINANCE_LATC1,
    tfCOMPRESSED_SIGNED_LUMINANCE_LATC1,
    tfCOMPRESSED_LUMINANCE_ALPHA_LATC2,
    tfCOMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2,
    tfCOMPRESSED_LUMINANCE_ALPHA_3DC,
    tfRGBA32UI,
    tfRGB32UI,
    tfALPHA32UI,
    tfINTENSITY32UI,
    tfLUMINANCE32UI,
    tfLUMINANCE_ALPHA32UI,
    tfRGBA16UI,
    tfRGB16UI,
    tfALPHA16UI,
    tfINTENSITY16UI,
    tfLUMINANCE16UI,
    tfLUMINANCE_ALPHA16UI,
    tfRGBA8UI,
    tfRGB8UI,
    tfALPHA8UI,
    tfINTENSITY8UI,
    tfLUMINANCE8UI,
    tfLUMINANCE_ALPHA8UI,
    tfRGBA32I,
    tfRGB32I,
    tfALPHA32I,
    tfINTENSITY32I,
    tfLUMINANCE32I,
    tfLUMINANCE_ALPHA32I,
    tfRGBA16I,
    tfRGB16I,
    tfALPHA16I,
    tfINTENSITY16I,
    tfLUMINANCE16I,
    tfLUMINANCE_ALPHA16I,
    tfRGBA8I,
    tfRGB8I,
    tfALPHA8I,
    tfINTENSITY8I,
    tfLUMINANCE8I,
    tfLUMINANCE_ALPHA8I,
    tfRG32UI,
    tfR32UI,
    tfRG16UI,
    tfR16UI,
    tfRG8UI,
    tfR8UI,
    tfRG32I,
    tfR32I,
    tfRG16I,
    tfR16I,
    tfRG8I,
    tfR8I,
    tfRG8,
    tfR8,
    tfRG16,
    tfR16,
    tfRG16F,
    tfR16F,
    tfRG32F,
    tfR32F,
    tfCOMPRESSED_RED_RGTC1,
    tfCOMPRESSED_SIGNED_RED_RGTC1,
    tfCOMPRESSED_RG_RGTC2,
    tfCOMPRESSED_SIGNED_RG_RGTC2,
    tfR8_SNORM,
    tfRG8_SNORM,
    tfRGB8_SNORM,
    tfRGBA8_SNORM,
    tfR16_SNORM,
    tfRG16_SNORM,
    tfRGB16_SNORM,
    tfRGBA16_SNORM
    );

  (* Texture compression option.
     If OpenGL supports it, this will activate a compressed texture format:
      tcDefault : uses global default compression option
      tcNone : do not use compression
      tcStandard : use standard compression, average quality, average rate
      tcHighQuality : choose a high-quality, low-speed compression
      tcHighSpeed : choose a high-speed, low-quality compression *)
  TGLInternalCompression = (tcDefault, tcNone, tcStandard, tcHighQuality,
    tcHighSpeed);

var
  vDefaultTextureFormat: TGLInternalFormat = tfRGBA8;
  vDefaultTextureCompression: TGLInternalCompression = tcNone;

const
  cDefaultSwizzleVector: TSwizzleVector = (tswRed, tswGreen, tswBlue, tswAlpha);

// Give a openGL texture format from GLScene texture format
function InternalFormatToOpenGLFormat(intFormat: TGLInternalFormat): Cardinal;
// Give a GLScene texture format from openGL texture format
function OpenGLFormatToInternalFormat(glFormat: Cardinal): TGLInternalFormat;
// Give a pixel size in bytes from texture format or data format
function GetTextureElementSize(intFormat: TGLInternalFormat): Integer; overload;
function GetTextureElementSize(colorFormat: Cardinal; dataType: Cardinal):
  Integer; overload;
// Give compatible openGL image format and data type
procedure FindCompatibleDataFormat(intFormat: TGLInternalFormat; out dFormat:
  TGLuint; out dType: TGLUint);
(* Give a compressed openGL texture format from GLScene texture format
  if format is have not compression than return same openGL format *)
function CompressedInternalFormatToOpenGL(intFormat: TGLInternalFormat):
  Integer;
// True if texture target supported
function IsTargetSupported(glTarget: Cardinal): Boolean; overload;
function IsTargetSupported(target: TGLTextureTarget): Boolean; overload;
// True if texture format is supported by hardware or software
function IsFormatSupported(intFormat: TGLInternalFormat): Boolean;
// True if texture format is float
function IsFloatFormat(intFormat: TGLInternalFormat): Boolean; overload;
function IsFloatFormat(glFormat: Cardinal): Boolean; overload;
// True if depth texture
function IsDepthFormat(intFormat: TGLInternalFormat): boolean; overload;
function IsDepthFormat(glFormat: Cardinal): Boolean; overload;
// True if texture compressed
function IsCompressedFormat(intFormat: TGLInternalFormat): Boolean; overload;
function IsCompressedFormat(glFormat: Cardinal): Boolean; overload;
// Give generic compressed OpenGL texture format
function GetGenericCompressedFormat(const intFormat: TGLInternalFormat;
  const colorFormat: Cardinal; out internalFormat: Cardinal): Boolean;
// Give uncompressed texture format and OpenGL color format
function GetUncompressedFormat(const intFormat: TGLInternalFormat;
  out internalFormat: TGLInternalFormat; out colorFormat: Cardinal): Boolean;
function DecodeTextureTarget(const TextureTarget: TGLTextureTarget): Cardinal;
function EncodeGLTextureTarget(const glTarget: Cardinal): TGLTextureTarget;
function IsTargetSupportMipmap(const TextureTarget: TGLTextureTarget): Boolean; overload;
function IsTargetSupportMipmap(const glTarget: Cardinal): Boolean; overload;

//---------------------------------------------------------------------------
implementation
//---------------------------------------------------------------------------

uses
  GLS.Context;


type

  TFormatDesc = record
    IntFmt: Cardinal;
    ClrFmt: Cardinal;
    DataFmt: Cardinal;
    RBit: Byte;
    GBit: Byte;
    BBit: Byte;
    ABit: Byte;
    LBit: Byte;
    DBit: Byte;
    Sign: Boolean;
    Flt: Boolean;
    Fix: Boolean;
    Comp: Boolean;
  end;

const
  // InternalFormat, ColorFormat, DataType
  cTextureFormatToOpenGL: array[low(TGLInternalFormat)..high(TGLInternalFormat)] of TFormatDesc =
  (
    (IntFmt: GL_ALPHA4; ClrFmt: GL_ALPHA; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 4; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_ALPHA8; ClrFmt: GL_ALPHA; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_ALPHA12; ClrFmt: GL_ALPHA; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 12; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_ALPHA16; ClrFmt: GL_ALPHA; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_DEPTH_COMPONENT16; ClrFmt: GL_DEPTH_COMPONENT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 16; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_DEPTH_COMPONENT24; ClrFmt: GL_DEPTH_COMPONENT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 24; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_DEPTH_COMPONENT32; ClrFmt: GL_DEPTH_COMPONENT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 32; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE4; ClrFmt: GL_LUMINANCE; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 4; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE8; ClrFmt: GL_LUMINANCE; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE12; ClrFmt: GL_LUMINANCE; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 12; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE16; ClrFmt: GL_LUMINANCE; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE4_ALPHA4; ClrFmt: GL_LUMINANCE_ALPHA; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 4; LBit: 4; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE6_ALPHA2; ClrFmt: GL_LUMINANCE_ALPHA; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 6; LBit: 2; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE8_ALPHA8; ClrFmt: GL_LUMINANCE_ALPHA; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE12_ALPHA4; ClrFmt: GL_LUMINANCE_ALPHA; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 4; LBit: 12; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE12_ALPHA12; ClrFmt: GL_LUMINANCE_ALPHA; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 12; LBit: 12; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE16_ALPHA16; ClrFmt: GL_LUMINANCE_ALPHA; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 16; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_INTENSITY4; ClrFmt: GL_LUMINANCE; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 4; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_INTENSITY8; ClrFmt: GL_LUMINANCE; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_INTENSITY12; ClrFmt: GL_LUMINANCE; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 12; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_INTENSITY16; ClrFmt: GL_LUMINANCE; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_R3_G3_B2; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_BYTE_3_3_2; RBit: 3; GBit: 3; BBit: 2; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGB4; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_BYTE; RBit: 4; GBit: 4; BBit: 4; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGB5; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_SHORT_5_6_5; RBit: 5; GBit: 6; BBit: 5; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGB8; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGB10; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_INT_10_10_10_2; RBit: 10; GBit: 10; BBit: 10; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGB12; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_BYTE; RBit: 12; GBit: 12; BBit: 12; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGB16; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGBA2; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_BYTE; RBit: 2; GBit: 2; BBit: 2; ABit: 2; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGBA4; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_SHORT_4_4_4_4; RBit: 4; GBit: 4; BBit: 4; ABit: 4; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGB5_A1; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_SHORT_5_5_5_1; RBit: 5; GBit: 5; BBit: 5; ABit: 1; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGBA8; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGB10_A2; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_INT_10_10_10_2; RBit: 10; GBit: 10; BBit: 10; ABit: 2; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGBA12; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_BYTE; RBit: 12; GBit: 12; BBit: 12; ABit: 12; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGBA16; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_COMPRESSED_RGB_S3TC_DXT1_EXT; ClrFmt: GL_COMPRESSED_RGB_S3TC_DXT1_EXT; DataFmt: GL_COMPRESSED_RGB_S3TC_DXT1_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_COMPRESSED_RGBA_S3TC_DXT1_EXT; ClrFmt: GL_COMPRESSED_RGBA_S3TC_DXT1_EXT; DataFmt: GL_COMPRESSED_RGBA_S3TC_DXT1_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 1; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_COMPRESSED_RGBA_S3TC_DXT3_EXT; ClrFmt: GL_COMPRESSED_RGBA_S3TC_DXT3_EXT; DataFmt: GL_COMPRESSED_RGBA_S3TC_DXT3_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_COMPRESSED_RGBA_S3TC_DXT5_EXT; ClrFmt: GL_COMPRESSED_RGBA_S3TC_DXT5_EXT; DataFmt: GL_COMPRESSED_RGBA_S3TC_DXT5_EXT; RBit: 8; GBit: 8; BBit: 0; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_SIGNED_LUMINANCE8_NV; ClrFmt: GL_LUMINANCE; DataFmt: GL_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_SIGNED_LUMINANCE8_ALPHA8_NV; ClrFmt: GL_LUMINANCE_ALPHA; DataFmt: GL_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_SIGNED_RGB8_NV; ClrFmt: GL_RGB; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_SIGNED_RGBA8_NV; ClrFmt: GL_RGBA; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV; ClrFmt: GL_RGBA; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_SIGNED_ALPHA8_NV; ClrFmt: GL_ALPHA; DataFmt: GL_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_SIGNED_INTENSITY8_NV; ClrFmt: GL_INTENSITY; DataFmt: GL_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_HILO16_NV; ClrFmt: GL_RG; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_SIGNED_HILO16_NV; ClrFmt: GL_RG; DataFmt: GL_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_DSDT8_NV; ClrFmt: GL_RED; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_DSDT8_MAG8_NV; ClrFmt: GL_RG; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_DSDT8_MAG8_INTENSITY8_NV; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_HILO8_NV; ClrFmt: GL_RG; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_SIGNED_HILO8_NV; ClrFmt: GL_RG; DataFmt: GL_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_FLOAT_R16_NV; ClrFmt: GL_RED; DataFmt: GL_HALF_FLOAT; RBit: 16; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_FLOAT_R32_NV; ClrFmt: GL_RED; DataFmt: GL_FLOAT; RBit: 32; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_FLOAT_RG16_NV; ClrFmt: GL_RG; DataFmt: GL_HALF_FLOAT; RBit: 16; GBit: 16; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_FLOAT_RGB16_NV; ClrFmt: GL_RGB; DataFmt: GL_HALF_FLOAT; RBit: 16; GBit: 16; BBit: 16; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_FLOAT_RGBA16_NV; ClrFmt: GL_RGBA; DataFmt: GL_HALF_FLOAT; RBit: 16; GBit: 16; BBit: 16; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_FLOAT_RG32_NV; ClrFmt: GL_RG; DataFmt: GL_FLOAT; RBit: 32; GBit: 32; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_FLOAT_RGB32_NV; ClrFmt: GL_RGB; DataFmt: GL_FLOAT; RBit: 32; GBit: 32; BBit: 32; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_FLOAT_RGBA32_NV; ClrFmt: GL_RGBA; DataFmt: GL_FLOAT; RBit: 32; GBit: 32; BBit: 32; ABit: 32; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_RGBA32F_ARB; ClrFmt: GL_RGBA; DataFmt: GL_FLOAT; RBit: 32; GBit: 32; BBit: 32; ABit: 32; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_RGB32F_ARB; ClrFmt: GL_RGB; DataFmt: GL_FLOAT; RBit: 32; GBit: 32; BBit: 32; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_ALPHA32F_ARB; ClrFmt: GL_ALPHA; DataFmt: GL_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 32; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_INTENSITY32F_ARB; ClrFmt: GL_LUMINANCE; DataFmt: GL_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 32; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE32F_ARB; ClrFmt: GL_LUMINANCE; DataFmt: GL_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 32; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE_ALPHA32F_ARB; ClrFmt: GL_LUMINANCE_ALPHA; DataFmt: GL_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 32; LBit: 32; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_RGBA16F_ARB; ClrFmt: GL_RGBA; DataFmt: GL_HALF_FLOAT; RBit: 16; GBit: 16; BBit: 16; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_RGB16F_ARB; ClrFmt: GL_RGB; DataFmt: GL_HALF_FLOAT; RBit: 16; GBit: 16; BBit: 16; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_ALPHA16F_ARB; ClrFmt: GL_ALPHA; DataFmt: GL_HALF_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_INTENSITY16F_ARB; ClrFmt: GL_LUMINANCE; DataFmt: GL_HALF_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE16F_ARB; ClrFmt: GL_LUMINANCE; DataFmt: GL_HALF_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_LUMINANCE_ALPHA16F_ARB; ClrFmt: GL_LUMINANCE_ALPHA; DataFmt: GL_HALF_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 16; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_DEPTH24_STENCIL8; ClrFmt: GL_DEPTH_STENCIL; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 24; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_DEPTH_COMPONENT32F; ClrFmt: GL_DEPTH_COMPONENT; DataFmt: GL_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 32; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_DEPTH32F_STENCIL8; ClrFmt: GL_DEPTH_STENCIL; DataFmt: GL_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 32; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_SRGB8; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_SRGB8_ALPHA8; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_SLUMINANCE8; ClrFmt: GL_LUMINANCE; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_SLUMINANCE8_ALPHA8; ClrFmt: GL_LUMINANCE_ALPHA; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_COMPRESSED_SRGB_S3TC_DXT1_EXT; ClrFmt: GL_COMPRESSED_SRGB_S3TC_DXT1_EXT; DataFmt: GL_COMPRESSED_SRGB_S3TC_DXT1_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT; ClrFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT; DataFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 1; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT; ClrFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT; DataFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT; ClrFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT; DataFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_RGB9_E5; ClrFmt: GL_RGBA; DataFmt: GL_FLOAT; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_R11F_G11F_B10F; ClrFmt: GL_RGB; DataFmt: GL_FLOAT; RBit: 11; GBit: 11; BBit: 10; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_COMPRESSED_LUMINANCE_LATC1_EXT; ClrFmt: GL_COMPRESSED_LUMINANCE_LATC1_EXT; DataFmt: GL_COMPRESSED_LUMINANCE_LATC1_EXT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT; ClrFmt: GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT; DataFmt: GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT; ClrFmt: GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT; DataFmt: GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT; ClrFmt: GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT; DataFmt: GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI; ClrFmt: GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI; DataFmt: GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_RGBA32UI; ClrFmt: GL_RGBA_INTEGER; DataFmt: GL_UNSIGNED_INT; RBit: 32; GBit: 32; BBit: 32; ABit: 32; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RGB32UI; ClrFmt: GL_RGB_INTEGER; DataFmt: GL_UNSIGNED_INT; RBit: 32; GBit: 32; BBit: 32; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_ALPHA32UI_EXT; ClrFmt: GL_ALPHA_INTEGER; DataFmt: GL_UNSIGNED_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 32; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_INTENSITY32UI_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 32; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_LUMINANCE32UI_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 32; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_LUMINANCE_ALPHA32UI_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 32; LBit: 32; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_RGBA16UI; ClrFmt: GL_RGBA_INTEGER; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RGB16UI; ClrFmt: GL_RGB_INTEGER; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_ALPHA16UI_EXT; ClrFmt: GL_ALPHA_INTEGER; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_INTENSITY16UI_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_LUMINANCE16UI_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_LUMINANCE_ALPHA16UI_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 16; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RGBA8UI; ClrFmt: GL_RGBA_INTEGER; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RGB8UI; ClrFmt: GL_RGB_INTEGER; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_ALPHA8UI_EXT; ClrFmt: GL_ALPHA_INTEGER; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_INTENSITY8UI_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_LUMINANCE8UI_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_LUMINANCE_ALPHA8UI_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_RGBA32I; ClrFmt: GL_RGBA_INTEGER; DataFmt: GL_INT; RBit: 32; GBit: 32; BBit: 32; ABit: 32; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RGB32I; ClrFmt: GL_RGB_INTEGER; DataFmt: GL_INT; RBit: 32; GBit: 32; BBit: 32; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_ALPHA32I_EXT; ClrFmt: GL_ALPHA_INTEGER; DataFmt: GL_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 32; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_INTENSITY32I_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 32; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_LUMINANCE32I_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 32; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_LUMINANCE_ALPHA32I_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 32; LBit: 32; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RGBA16I; ClrFmt: GL_RGBA_INTEGER; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 16; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RGB16I; ClrFmt: GL_RGB_INTEGER; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_ALPHA16I_EXT; ClrFmt: GL_ALPHA_INTEGER; DataFmt: GL_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_INTENSITY16I_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_LUMINANCE16I_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_LUMINANCE_ALPHA16I_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 16; DBit: 0; Sign: True; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_RGBA8I; ClrFmt: GL_RGBA_INTEGER; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RGB8I; ClrFmt: GL_RGB_INTEGER; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_ALPHA8I_EXT; ClrFmt: GL_ALPHA_INTEGER; DataFmt: GL_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_INTENSITY8I_EXT; ClrFmt: GL_INTENSITY; DataFmt: GL_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_LUMINANCE8I_EXT; ClrFmt: GL_LUMINANCE; DataFmt: GL_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_LUMINANCE_ALPHA8I_EXT; ClrFmt: GL_LUMINANCE_ALPHA; DataFmt: GL_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RG32UI; ClrFmt: GL_RG; DataFmt: GL_UNSIGNED_INT; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_R32UI; ClrFmt: GL_RED_INTEGER; DataFmt: GL_UNSIGNED_INT; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RG16UI; ClrFmt: GL_RG; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 16; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_R16UI; ClrFmt: GL_RED_INTEGER; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RG8UI; ClrFmt: GL_RG; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_R8UI; ClrFmt: GL_RED_INTEGER; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_RG32I; ClrFmt: GL_RG; DataFmt: GL_INT; RBit: 32; GBit: 32; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_R32I; ClrFmt: GL_RED_INTEGER; DataFmt: GL_INT; RBit: 16; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RG16I; ClrFmt: GL_RG; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_R16I; ClrFmt: GL_RED_INTEGER; DataFmt: GL_SHORT; RBit: 16; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RG8I; ClrFmt: GL_RG; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_R8I; ClrFmt: GL_RED_INTEGER; DataFmt: GL_BYTE; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
    (IntFmt: GL_RG8; ClrFmt: GL_RG; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_R8; ClrFmt: GL_RED; DataFmt: GL_BYTE; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RG16; ClrFmt: GL_RG; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_R16; ClrFmt: GL_RED; DataFmt: GL_SHORT; RBit: 16; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RG16F; ClrFmt: GL_RG; DataFmt: GL_HALF_FLOAT; RBit: 16; GBit: 16; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_R16F; ClrFmt: GL_RED; DataFmt: GL_HALF_FLOAT; RBit: 16; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_RG32F; ClrFmt: GL_RG; DataFmt: GL_FLOAT; RBit: 32; GBit: 32; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_R32F; ClrFmt: GL_LUMINANCE; DataFmt: GL_FLOAT; RBit: 32; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
    (IntFmt: GL_COMPRESSED_RED_RGTC1; ClrFmt: GL_COMPRESSED_RED_RGTC1; DataFmt: GL_COMPRESSED_RED_RGTC1; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_COMPRESSED_SIGNED_RED_RGTC1; ClrFmt: GL_COMPRESSED_SIGNED_RED_RGTC1; DataFmt: GL_COMPRESSED_SIGNED_RED_RGTC1; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_COMPRESSED_RG_RGTC2; ClrFmt: GL_COMPRESSED_RG_RGTC2; DataFmt: GL_COMPRESSED_RG_RGTC2; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_COMPRESSED_SIGNED_RG_RGTC2; ClrFmt: GL_COMPRESSED_SIGNED_RG_RGTC2; DataFmt: GL_COMPRESSED_SIGNED_RG_RGTC2; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: True),
    (IntFmt: GL_R8_SNORM; ClrFmt: GL_R; DataFmt: GL_BYTE; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RG8_SNORM; ClrFmt: GL_RG; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGB8_SNORM; ClrFmt: GL_RGB; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGBA8_SNORM; ClrFmt: GL_RGBA; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_R16_SNORM; ClrFmt: GL_R; DataFmt: GL_SHORT; RBit: 16; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RG16_SNORM; ClrFmt: GL_RG; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGB16_SNORM; ClrFmt: GL_RGB; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
    (IntFmt: GL_RGBA16_SNORM; ClrFmt: GL_RGBA; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False)
    );

function InternalFormatToOpenGLFormat(intFormat: TGLInternalFormat): Cardinal;
begin
  Result := cTextureFormatToOpenGL[intFormat].IntFmt;
end;

function OpenGLFormatToInternalFormat(glFormat: Cardinal): TGLInternalFormat;
var
  i: TGLInternalFormat;
begin
  Result := tfRGBA8;
  for i := Low(cTextureFormatToOpenGL) to High(cTextureFormatToOpenGL) do
    if glFormat = cTextureFormatToOpenGL[i].IntFmt then
    begin
      Result := i;
      Exit;
    end;
  Assert(false);
end;

function GetTextureElementSize(intFormat: TGLInternalFormat): Integer;
begin
  Result := GetTextureElementSize(
    cTextureFormatToOpenGL[intFormat].ClrFmt,
    cTextureFormatToOpenGL[intFormat].DataFmt);
end;

function GetTextureElementSize(colorFormat: Cardinal; dataType: Cardinal):
  Integer;
var
  components: Byte;
begin
  case colorFormat of
    GL_RGB, GL_BGR: components := 3;
    GL_RGBA, GL_BGRA: components := 4;
    GL_ALPHA: components := 1;
    GL_LUMINANCE: components := 1;
    GL_LUMINANCE_ALPHA: components := 2;
    GL_INTENSITY: components := 1;
    GL_RED: components := 1;
    GL_GREEN: components := 1;
    GL_BLUE: components := 1;
    GL_RG: components := 2;

    GL_RGB_INTEGER: components := 3;
    GL_RGBA_INTEGER: components := 4;
    GL_ALPHA_INTEGER: components := 1;
    GL_LUMINANCE_INTEGER_EXT: components := 1;
    GL_LUMINANCE_ALPHA_INTEGER_EXT: components := 2;
    GL_RED_INTEGER: components := 1;
    GL_RG_INTEGER: components := 2;
  else
    components := 1;
  end;

  case dataType of
    GL_BITMAP,
      GL_UNSIGNED_BYTE,
      GL_BYTE: Result := components;
    GL_UNSIGNED_BYTE_3_3_2,
      GL_UNSIGNED_BYTE_2_3_3_REV: Result := 1;
    GL_UNSIGNED_SHORT,
      GL_SHORT: Result := components * 2;
    GL_UNSIGNED_SHORT_4_4_4_4,
      GL_UNSIGNED_SHORT_4_4_4_4_REV,
      GL_UNSIGNED_SHORT_5_6_5,
      GL_UNSIGNED_SHORT_5_6_5_REV,
      GL_UNSIGNED_SHORT_5_5_5_1,
      GL_UNSIGNED_SHORT_1_5_5_5_REV: Result := 2;

    GL_UNSIGNED_INT,
      GL_INT: Result := components * 4;
    GL_UNSIGNED_INT_8_8_8_8,
      GL_UNSIGNED_INT_8_8_8_8_REV,
      GL_UNSIGNED_INT_10_10_10_2,
      GL_UNSIGNED_INT_2_10_10_10_REV: Result := 4;

    GL_FLOAT: Result := components * 4;
    GL_HALF_FLOAT: Result := components * 2;

    GL_COMPRESSED_RGB_S3TC_DXT1_EXT: Result := 8;
    GL_COMPRESSED_RGBA_S3TC_DXT1_EXT: Result := 8;
    GL_COMPRESSED_RGBA_S3TC_DXT3_EXT: Result := 16;
    GL_COMPRESSED_RGBA_S3TC_DXT5_EXT: Result := 16;
    GL_COMPRESSED_SRGB_S3TC_DXT1_EXT: Result := 8;
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT: Result := 8;
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT: Result := 16;
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT: Result := 16;
    GL_COMPRESSED_LUMINANCE_LATC1_EXT: Result := 8;
    GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT: Result := 8;
    GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT: Result := 16;
    GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT: Result := 16;
    GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI: Result := 16;
    GL_COMPRESSED_RED_RGTC1: Result := 8;
    GL_COMPRESSED_SIGNED_RED_RGTC1: Result := 8;
    GL_COMPRESSED_RG_RGTC2: Result := 16;
    GL_COMPRESSED_SIGNED_RG_RGTC2: Result := 16;
  else
    Result := 1;
  end;
end;

function CompressedInternalFormatToOpenGL(intFormat: TGLInternalFormat):
  Integer;
begin
  Result := GL_COMPRESSED_RGBA;
  case intFormat of
    tfRGB8: Result := GL_COMPRESSED_RGB;
    tfRGBA8: Result := GL_COMPRESSED_RGBA;
    tfRGB5: Result := GL_COMPRESSED_RGB;
    tfRGBA4: Result := GL_COMPRESSED_RGBA;
    tfALPHA8: Result := GL_COMPRESSED_ALPHA;
    tfLUMINANCE8: Result := GL_COMPRESSED_LUMINANCE;
    tfLUMINANCE8_ALPHA8: Result := GL_COMPRESSED_LUMINANCE_ALPHA;
    tfINTENSITY8: Result := GL_COMPRESSED_INTENSITY;
  else
    Assert(false);
  end;
end;

procedure FindCompatibleDataFormat(intFormat: TGLInternalFormat; out dFormat:
  Cardinal; out dType: Cardinal);
begin
  dFormat := cTextureFormatToOpenGL[intFormat].ClrFmt;
  dType := cTextureFormatToOpenGL[intFormat].DataFmt;
end;

function IsTargetSupported(target: TGLTextureTarget): Boolean;
begin
  Result := IsTargetSupported(DecodeTextureTarget(target));
end;

function IsTargetSupported(glTarget: Cardinal): Boolean;
begin
  case glTarget of
    GL_TEXTURE_1D: Result := GL.VERSION_1_1 or GL.EXT_texture_object;
    GL_TEXTURE_2D: Result := GL.VERSION_1_1 or GL.EXT_texture_object;
    GL_TEXTURE_3D: Result := GL.EXT_texture3D;
    GL_TEXTURE_RECTANGLE: Result := GL.ARB_texture_rectangle;
    GL_TEXTURE_CUBE_MAP,
      GL_TEXTURE_CUBE_MAP_POSITIVE_X,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
      GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
      GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Z: Result := GL.ARB_texture_cube_map;
    GL_TEXTURE_1D_ARRAY: Result := GL.EXT_texture_array;
    GL_TEXTURE_2D_ARRAY: Result := GL.EXT_texture_array;
    GL_TEXTURE_CUBE_MAP_ARRAY: Result := GL.ARB_texture_cube_map_array;
    GL_TEXTURE_BUFFER: Result := GL.ARB_texture_buffer_object;
    GL_TEXTURE_2D_MULTISAMPLE,
      GL_TEXTURE_2D_MULTISAMPLE_ARRAY: Result := GL.ARB_texture_multisample;
  else
    begin
      Result := false;
      // Assert(False, strErrorEx + strUnknownType);
    end;
  end;
end;

function IsFormatSupported(intFormat: TGLInternalFormat): Boolean;
begin
  Result := false;

  if ((intFormat >= tfALPHA4) and (intFormat <= tfALPHA16)) or
    ((intFormat >= tfLUMINANCE4) and (intFormat <= tfR16G16B16A16)) then
  begin
    Result := GL.VERSION_1_1;
    EXIT;
  end;

  if ((intFormat >= tfDEPTH_COMPONENT16) and (intFormat <= tfDEPTH_COMPONENT32)) then
  begin
    Result := GL.ARB_depth_texture;
    EXIT;
  end;

  if ((intFormat >= tfCOMPRESSED_RGB_S3TC_DXT1) and (intFormat <=
    tfCOMPRESSED_RGBA_S3TC_DXT5)) then
  begin
    Result := GL.EXT_texture_compression_s3tc;
    EXIT;
  end;

  if ((intFormat >= tfSIGNED_LUMINANCE8) and (intFormat <=
    tfDSDT8_MAG8_INTENSITY8)) then
  begin
    Result := GL.NV_texture_shader;
    EXIT;
  end;

  if ((intFormat = tfHILO8) or (intFormat = tfSIGNED_HILO8)) then
  begin
    Result := GL.NV_texture_shader3;
    EXIT;
  end;

  if ((intFormat >= tfFLOAT_R16) and (intFormat <= tfFLOAT_RGBA32)) then
  begin
    Result := GL.NV_float_buffer;
    EXIT;
  end;

  if ((intFormat >= tfRGBA_FLOAT32)
    and (intFormat <= tfLUMINANCE_ALPHA_FLOAT16)) then
  begin
    Result := GL.ARB_texture_float or GL.ATI_texture_float;
    EXIT;
  end;

  if intFormat = tfDEPTH24_STENCIL8 then
  begin
    Result := GL.EXT_packed_depth_stencil;
    EXIT;
  end;

  if ((intFormat = tfDEPTH_COMPONENT32F) or (intFormat = tfDEPTH32F_STENCIL8)) then
  begin
    Result := GL.NV_depth_buffer_float;
    EXIT;
  end;

  if ((intFormat >= tfSRGB8) and (intFormat <=
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5)) then
  begin
    Result := GL.EXT_texture_sRGB;
    EXIT;
  end;

  if intFormat = tfRGB9_E5 then
  begin
    Result := GL.EXT_texture_shared_exponent;
    EXIT;
  end;

  if intFormat = tfR11F_G11F_B10F then
  begin
    Result := GL.EXT_packed_float;
    EXIT;
  end;

  if ((intFormat >= tfCOMPRESSED_LUMINANCE_LATC1) and (intFormat <=
    tfCOMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2)) then
  begin
    Result := GL.EXT_texture_compression_latc;
    EXIT;
  end;

  if intFormat = tfCOMPRESSED_LUMINANCE_ALPHA_3DC then
  begin
    Result := GL.ATI_texture_compression_3dc;
    EXIT;
  end;

  if ((intFormat >= tfRGBA32UI) and (intFormat <= tfLUMINANCE_ALPHA8I)) then
  begin
    Result := GL.EXT_texture_integer;
    EXIT;
  end;

  if ((intFormat >= tfRG32UI) and (intFormat <= tfR32F)) then
    Result := GL.ARB_texture_rg;

  if ((intFormat >= tfCOMPRESSED_RED_RGTC1) and (intFormat <=
    tfCOMPRESSED_SIGNED_RG_RGTC2)) then
  begin
    Result := GL.ARB_texture_compression_rgtc;
    EXIT;
  end;

  if ((intFormat >= tfR8_SNORM) and (intFormat <= tfRGBA16_SNORM)) then
  begin
    Result := GL.VERSION_3_1;
    EXIT;
  end
end;

function IsFloatFormat(intFormat: TGLInternalFormat): boolean;
begin
  Result := cTextureFormatToOpenGL[intFormat].Flt;
end;

function IsFloatFormat(glFormat: Cardinal): boolean;
begin
  Result := IsFloatFormat(OpenGLFormatToInternalFormat(glFormat));
end;

function IsDepthFormat(intFormat: TGLInternalFormat): boolean;
begin
  Result := cTextureFormatToOpenGL[intFormat].DBit > 0;
end;

function IsDepthFormat(glFormat: Cardinal): boolean;
begin
  Result := cTextureFormatToOpenGL[OpenGLFormatToInternalFormat(glFormat)].DBit > 0;
end;

function IsCompressedFormat(intFormat: TGLInternalFormat): boolean;
begin
  Result := cTextureFormatToOpenGL[intFormat].Comp;
end;

function IsCompressedFormat(glFormat: Cardinal): boolean;
begin
  Result := cTextureFormatToOpenGL[OpenGLFormatToInternalFormat(glFormat)].Comp;
end;

function GetGenericCompressedFormat(const intFormat: TGLInternalFormat;
  const colorFormat: Cardinal; out internalFormat: Cardinal): Boolean;

begin
  Result := false;
  if IsCompressedFormat(intFormat) then
    Exit;
  if not IsFormatSupported(intFormat) then
    Exit;
  internalFormat := 0;

  if ((intFormat >= tfSRGB8) and (intFormat <=
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5)) then
    case colorFormat of
      GL_RGB: internalFormat := GL_COMPRESSED_SRGB;
      GL_RGBA: internalFormat := GL_COMPRESSED_SRGB_ALPHA;
      GL_LUMINANCE: internalFormat := GL_COMPRESSED_SLUMINANCE;
      GL_LUMINANCE_ALPHA: internalFormat := GL_COMPRESSED_SLUMINANCE_ALPHA;
    end
  else
    case colorFormat of
      GL_RGB, GL_BGR: internalFormat := GL_COMPRESSED_RGB;
      GL_RGBA, GL_BGRA: internalFormat := GL_COMPRESSED_RGBA;
      GL_ALPHA: internalFormat := GL_COMPRESSED_ALPHA;
      GL_LUMINANCE: internalFormat := GL_COMPRESSED_LUMINANCE;
      GL_LUMINANCE_ALPHA: internalFormat := GL_COMPRESSED_LUMINANCE_ALPHA;
      GL_INTENSITY: internalFormat := GL_COMPRESSED_INTENSITY;
      GL_RED: internalFormat := GL_COMPRESSED_RED;
      GL_RG: internalFormat := GL_COMPRESSED_RG;
    end;

  if internalFormat = 0 then
    Exit;
  Result := true;
end;

function GetUncompressedFormat(const intFormat: TGLInternalFormat;
  out internalFormat: TGLInternalFormat; out colorFormat: Cardinal): Boolean;
begin
  Result := false;
  if not IsCompressedFormat(intFormat) then
    Exit;
  if not IsFormatSupported(intFormat) then
    Exit;
  colorFormat := 0;
  case intFormat of
    tfCOMPRESSED_RGB_S3TC_DXT1:
      begin
        colorFormat := GL_RGB;
        internalFormat := tfRGB8;
      end;
    tfCOMPRESSED_RGBA_S3TC_DXT1:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfRGBA8;
      end;
    tfCOMPRESSED_RGBA_S3TC_DXT3:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfRGBA8;
      end;
    tfCOMPRESSED_RGBA_S3TC_DXT5:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfRGBA8;
      end;
    tfCOMPRESSED_SRGB_S3TC_DXT1:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfSRGB8;
      end;
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT1:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfSRGB8_ALPHA8;
      end;
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT3:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfSRGB8_ALPHA8;
      end;
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfSRGB8_ALPHA8;
      end;
    tfCOMPRESSED_LUMINANCE_LATC1:
      begin
        colorFormat := GL_LUMINANCE;
        internalFormat := tfLUMINANCE8;
      end;
    tfCOMPRESSED_SIGNED_LUMINANCE_LATC1:
      begin
        colorFormat := GL_LUMINANCE;
        internalFormat := tfSIGNED_LUMINANCE8;
      end;
    tfCOMPRESSED_LUMINANCE_ALPHA_LATC2:
      begin
        colorFormat := GL_LUMINANCE_ALPHA;
        internalFormat := tfLUMINANCE8_ALPHA8;
      end;
    tfCOMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2:
      begin
        colorFormat := GL_LUMINANCE_ALPHA;
        internalFormat := tfSIGNED_LUMINANCE8_ALPHA8;
      end;
    tfCOMPRESSED_LUMINANCE_ALPHA_3DC:
      begin
        colorFormat := GL_LUMINANCE_ALPHA;
        internalFormat := tfLUMINANCE8_ALPHA8;
      end;
    tfCOMPRESSED_RED_RGTC1:
      begin
        colorFormat := GL_RED;
        internalFormat := tfR8;
      end;
    tfCOMPRESSED_SIGNED_RED_RGTC1:
      begin
        colorFormat := GL_RED;
        internalFormat := tfR8;
      end;
    tfCOMPRESSED_RG_RGTC2:
      begin
        colorFormat := GL_RG;
        internalFormat := tfRG8;
      end;
    tfCOMPRESSED_SIGNED_RG_RGTC2:
      begin
        colorFormat := GL_RG;
        internalFormat := tfRG8;
      end;
  end;
  Result := colorFormat <> 0;
end;

function DecodeTextureTarget(const TextureTarget: TGLTextureTarget): Cardinal;
const
  cTargetToEnum: array[TGLTextureTarget] of Cardinal =
  (
    0,
    GL_TEXTURE_1D,
    GL_TEXTURE_2D,
    GL_TEXTURE_3D,
    GL_TEXTURE_1D_ARRAY,
    GL_TEXTURE_2D_ARRAY,
    GL_TEXTURE_RECTANGLE,
    GL_TEXTURE_BUFFER,
    GL_TEXTURE_CUBE_MAP,
    GL_TEXTURE_2D_MULTISAMPLE,
    GL_TEXTURE_2D_MULTISAMPLE_ARRAY,
    GL_TEXTURE_CUBE_MAP_ARRAY
  );
begin
 // Assert(TextureTarget <> ttNoShape);
  Result := cTargetToEnum[TextureTarget];
end;

function EncodeGLTextureTarget(const glTarget: Cardinal): TGLTextureTarget;
begin
  case glTarget of
    GL_TEXTURE_1D: Result := ttTexture1d;
    GL_TEXTURE_2D: Result := ttTexture2d;
    GL_TEXTURE_3D: Result := ttTexture3d;
    GL_TEXTURE_RECTANGLE: Result := ttTextureRect;
    GL_TEXTURE_CUBE_MAP: Result := ttTextureCube;
    GL_TEXTURE_1D_ARRAY: Result := ttTexture1dArray;
    GL_TEXTURE_2D_ARRAY: Result := ttTexture2dArray;
    GL_TEXTURE_CUBE_MAP_ARRAY: Result := ttTextureCubeArray;
    GL_TEXTURE_2D_MULTISAMPLE: Result := ttTexture2DMultisample;
    GL_TEXTURE_2D_MULTISAMPLE_ARRAY: Result := ttTexture2DMultisampleArray;
  else
    begin
      Result := ttTexture2d;
      Assert(False, strErrorEx + strUnknownType);
    end;
  end;
end;

function IsTargetSupportMipmap(const TextureTarget: TGLTextureTarget): Boolean;
begin
  Result := (TextureTarget <> ttTextureRect)
    and (TextureTarget <> ttTexture2DMultisample)
    and (TextureTarget <> ttTexture2DMultisampleArray);
end;

function IsTargetSupportMipmap(const glTarget: Cardinal): Boolean;
begin
  Result := (glTarget <> GL_TEXTURE_RECTANGLE)
    and (glTarget <> GL_TEXTURE_2D_MULTISAMPLE)
    and (glTarget <> GL_TEXTURE_2D_MULTISAMPLE_ARRAY);
end;

end.

