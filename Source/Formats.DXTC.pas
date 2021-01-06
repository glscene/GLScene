//
// The graphics rendering engine GLScene http://glscene.org
//

unit Formats.DXTC;

(*
   DXTC (also S3TC) decoding.
   Adapted from DevIL image library (http://openil.sourceforge.net)
*)

interface

  {$I GLScene.inc}
  {$Z4}  // Minimum enum size = dword

uses
   Winapi.OpenGL,
   Winapi.OpenGLext,
   System.SysUtils,
   
   GLS.TextureFormat;

const
   DDSD_CAPS        = $00000001;
   DDSD_HEIGHT      = $00000002;
   DDSD_WIDTH       = $00000004;
   DDSD_PITCH       = $00000008;
   DDSD_PIXELFORMAT = $00001000;
   DDSD_MIPMAPCOUNT = $00020000;
   DDSD_LINEARSIZE  = $00080000;
   DDSD_DEPTH       = $00800000;

   DDPF_ALPHAPIXELS = $00000001;
   DDPF_A           = $00000002;
   DDPF_FOURCC      = $00000004;
   DDPF_RGB         = $00000040;
   DDPF_RGBA        = $00000041;
   DDPF_L           = $00020000;
   DDPF_LA          = $00020001;

   DDSCAPS_COMPLEX  = $00000008;
   DDSCAPS_TEXTURE  = $00001000;
   DDSCAPS_MIPMAP   = $00400000;

   DDSCAPS2_CUBEMAP           = $00000200;
   DDSCAPS2_CUBEMAP_POSITIVEX = $00000400;
   DDSCAPS2_CUBEMAP_NEGATIVEX = $00000800;
   DDSCAPS2_CUBEMAP_POSITIVEY = $00001000;
   DDSCAPS2_CUBEMAP_NEGATIVEY = $00002000;
   DDSCAPS2_CUBEMAP_POSITIVEZ = $00004000;
   DDSCAPS2_CUBEMAP_NEGATIVEZ = $00008000;
   DDSCAPS2_VOLUME            = $00200000;


type
   TDDPIXELFORMAT = record
      dwSize,
      dwFlags,
      dwFourCC,
      dwRGBBitCount,
      dwRBitMask,
      dwGBitMask,
      dwBBitMask,
      dwRGBAlphaBitMask : Cardinal;
   end;

   TDDSURFACEDESC2 = record
      dwSize,
      dwFlags,
      dwHeight,
      dwWidth,
      dwPitchOrLinearSize, (*The number of bytes per scan line in an
                            uncompressed texture; the total number of bytes
                            in the top level texture for a compressed texture.*)
      dwDepth,
      dwMipMapCount : Cardinal;
      dwReserved1 : array[0..10] of Cardinal;
      ddpf : TDDPIXELFORMAT;
      dwCaps,
      dwCaps2,
      dwCaps3,
      dwCaps4  : Cardinal;
      dwReserved2 : Cardinal;
   end;

   TDDSHeader = record
      Magic : Cardinal;
      SurfaceFormat : TDDSURFACEDESC2;
   end;

  DXTColBlock = record
    col0: Word;
    col1: Word;
    row: array[0..3] of Byte;
  end;
  PDXTColBlock = ^DXTColBlock;

  DXT3AlphaBlock = record
    row: array[0..3] of Word;
  end;
  PDXT3AlphaBlock = ^DXT3AlphaBlock;

  DXT5AlphaBlock = record
    alpha0 : Byte;
    alpha1 : Byte;
    row : array[0..5] of Byte;
  end;
  PDXT5AlphaBlock = ^DXT5AlphaBlock;

  const
//  TDXGI_FORMAT =
//  (
    DXGI_FORMAT_FORCE_UINT                  = -1;
    DXGI_FORMAT_UNKNOWN	                    = 0;
    DXGI_FORMAT_R32G32B32A32_TYPELESS       = 1;
    DXGI_FORMAT_R32G32B32A32_FLOAT          = 2;
    DXGI_FORMAT_R32G32B32A32_UINT           = 3;
    DXGI_FORMAT_R32G32B32A32_SINT           = 4;
    DXGI_FORMAT_R32G32B32_TYPELESS          = 5;
    DXGI_FORMAT_R32G32B32_FLOAT             = 6;
    DXGI_FORMAT_R32G32B32_UINT              = 7;
    DXGI_FORMAT_R32G32B32_SINT              = 8;
    DXGI_FORMAT_R16G16B16A16_TYPELESS       = 9;
    DXGI_FORMAT_R16G16B16A16_FLOAT          = 10;
    DXGI_FORMAT_R16G16B16A16_UNORM          = 11;
    DXGI_FORMAT_R16G16B16A16_UINT           = 12;
    DXGI_FORMAT_R16G16B16A16_SNORM          = 13;
    DXGI_FORMAT_R16G16B16A16_SINT           = 14;
    DXGI_FORMAT_R32G32_TYPELESS             = 15;
    DXGI_FORMAT_R32G32_FLOAT                = 16;
    DXGI_FORMAT_R32G32_UINT                 = 17;
    DXGI_FORMAT_R32G32_SINT                 = 18;
    DXGI_FORMAT_R32G8X24_TYPELESS           = 19;
    DXGI_FORMAT_D32_FLOAT_S8X24_UINT        = 20;
    DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS    = 21;
    DXGI_FORMAT_X32_TYPELESS_G8X24_UINT     = 22;
    DXGI_FORMAT_R10G10B10A2_TYPELESS        = 23;
    DXGI_FORMAT_R10G10B10A2_UNORM           = 24;
    DXGI_FORMAT_R10G10B10A2_UINT            = 25;
    DXGI_FORMAT_R11G11B10_FLOAT             = 26;
    DXGI_FORMAT_R8G8B8A8_TYPELESS           = 27;
    DXGI_FORMAT_R8G8B8A8_UNORM              = 28;
    DXGI_FORMAT_R8G8B8A8_UNORM_SRGB         = 29;
    DXGI_FORMAT_R8G8B8A8_UINT               = 30;
    DXGI_FORMAT_R8G8B8A8_SNORM              = 31;
    DXGI_FORMAT_R8G8B8A8_SINT               = 32;
    DXGI_FORMAT_R16G16_TYPELESS             = 33;
    DXGI_FORMAT_R16G16_FLOAT                = 34;
    DXGI_FORMAT_R16G16_UNORM                = 35;
    DXGI_FORMAT_R16G16_UINT                 = 36;
    DXGI_FORMAT_R16G16_SNORM                = 37;
    DXGI_FORMAT_R16G16_SINT                 = 38;
    DXGI_FORMAT_R32_TYPELESS                = 39;
    DXGI_FORMAT_D32_FLOAT                   = 40;
    DXGI_FORMAT_R32_FLOAT                   = 41;
    DXGI_FORMAT_R32_UINT                    = 42;
    DXGI_FORMAT_R32_SINT                    = 43;
    DXGI_FORMAT_R24G8_TYPELESS              = 44;
    DXGI_FORMAT_D24_UNORM_S8_UINT           = 45;
    DXGI_FORMAT_R24_UNORM_X8_TYPELESS       = 46;
    DXGI_FORMAT_X24_TYPELESS_G8_UINT        = 47;
    DXGI_FORMAT_R8G8_TYPELESS               = 48;
    DXGI_FORMAT_R8G8_UNORM                  = 49;
    DXGI_FORMAT_R8G8_UINT                   = 50;
    DXGI_FORMAT_R8G8_SNORM                  = 51;
    DXGI_FORMAT_R8G8_SINT                   = 52;
    DXGI_FORMAT_R16_TYPELESS                = 53;
    DXGI_FORMAT_R16_FLOAT                   = 54;
    DXGI_FORMAT_D16_UNORM                   = 55;
    DXGI_FORMAT_R16_UNORM                   = 56;
    DXGI_FORMAT_R16_UINT                    = 57;
    DXGI_FORMAT_R16_SNORM                   = 58;
    DXGI_FORMAT_R16_SINT                    = 59;
    DXGI_FORMAT_R8_TYPELESS                 = 60;
    DXGI_FORMAT_R8_UNORM                    = 61;
    DXGI_FORMAT_R8_UINT                     = 62;
    DXGI_FORMAT_R8_SNORM                    = 63;
    DXGI_FORMAT_R8_SINT                     = 64;
    DXGI_FORMAT_A8_UNORM                    = 65;
    DXGI_FORMAT_R1_UNORM                    = 66;
    DXGI_FORMAT_R9G9B9E5_SHAREDEXP          = 67;
    DXGI_FORMAT_R8G8_B8G8_UNORM             = 68;
    DXGI_FORMAT_G8R8_G8B8_UNORM             = 69;
    DXGI_FORMAT_BC1_TYPELESS                = 70;
    DXGI_FORMAT_BC1_UNORM                   = 71;
    DXGI_FORMAT_BC1_UNORM_SRGB              = 72;
    DXGI_FORMAT_BC2_TYPELESS                = 73;
    DXGI_FORMAT_BC2_UNORM                   = 74;
    DXGI_FORMAT_BC2_UNORM_SRGB              = 75;
    DXGI_FORMAT_BC3_TYPELESS                = 76;
    DXGI_FORMAT_BC3_UNORM                   = 77;
    DXGI_FORMAT_BC3_UNORM_SRGB              = 78;
    DXGI_FORMAT_BC4_TYPELESS                = 79;
    DXGI_FORMAT_BC4_UNORM                   = 80;
    DXGI_FORMAT_BC4_SNORM                   = 81;
    DXGI_FORMAT_BC5_TYPELESS                = 82;
    DXGI_FORMAT_BC5_UNORM                   = 83;
    DXGI_FORMAT_BC5_SNORM                   = 84;
    DXGI_FORMAT_B5G6R5_UNORM                = 85;
    DXGI_FORMAT_B5G5R5A1_UNORM              = 86;
    DXGI_FORMAT_B8G8R8A8_UNORM              = 87;
    DXGI_FORMAT_B8G8R8X8_UNORM              = 88;
    DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM  = 89;
    DXGI_FORMAT_B8G8R8A8_TYPELESS           = 90;
    DXGI_FORMAT_B8G8R8A8_UNORM_SRGB         = 91;
    DXGI_FORMAT_B8G8R8X8_TYPELESS           = 92;
    DXGI_FORMAT_B8G8R8X8_UNORM_SRGB         = 93;
    DXGI_FORMAT_BC6H_TYPELESS               = 94;
    DXGI_FORMAT_BC6H_UF16                   = 95;
    DXGI_FORMAT_BC6H_SF16                   = 96;
    DXGI_FORMAT_BC7_TYPELESS                = 97;
    DXGI_FORMAT_BC7_UNORM                   = 98;
    DXGI_FORMAT_BC7_UNORM_SRGB              = 99;
//  );

//  TD3D11_RESOURCE_DIMENSION =
//  (
    D3D11_RESOURCE_DIMENSION_UNKNOWN	  = 0;
	  D3D11_RESOURCE_DIMENSION_BUFFER	    = 1;
	  D3D11_RESOURCE_DIMENSION_TEXTURE1D	= 2;
	  D3D11_RESOURCE_DIMENSION_TEXTURE2D	= 3;
	  D3D11_RESOURCE_DIMENSION_TEXTURE3D	= 4;
//  );

type
  TDDS_HEADER_DXT10 = record
    dxgiFormat : Integer; //TDXGI_FORMAT;
    resourceDimension : Integer; //TD3D11_RESOURCE_DIMENSION;
    miscFlag : Cardinal;
    arraySize : Cardinal;
    reserved : Cardinal;
  end;

  TFOURCC = array[0..3] of AnsiChar;

const
    FOURCC_UNKNOWN       = 0;
    FOURCC_R8G8B8        = 20;
    FOURCC_A8R8G8B8      = 21;
    FOURCC_X8R8G8B8      = 22;
    FOURCC_R5G6B5        = 23;
    FOURCC_X1R5G5B5      = 24;
    FOURCC_A1R5G5B5      = 25;
    FOURCC_A4R4G4B4      = 26;
    FOURCC_R3G3B2        = 27;
    FOURCC_A8            = 28;
    FOURCC_A8R3G3B2      = 29;
    FOURCC_X4R4G4B4      = 30;
    FOURCC_A2B10G10R10   = 31;
    FOURCC_A8B8G8R8      = 32;
    FOURCC_X8B8G8R8      = 33;
    FOURCC_G16R16        = 34;
    FOURCC_A2R10G10B10   = 35;
    FOURCC_A16B16G16R16  = 36;

    FOURCC_L8            = 50;
    FOURCC_A8L8          = 51;
    FOURCC_A4L4          = 52;
    FOURCC_DXT1          = $31545844;
    FOURCC_DXT2          = $32545844;
    FOURCC_DXT3          = $33545844;
    FOURCC_DXT4          = $34545844;
    FOURCC_DXT5          = $35545844;
    FOURCC_ATI1          = $31495441;
    FOURCC_ATI2          = $32495441;

    FOURCC_D16_LOCKABLE  = 70;
    FOURCC_D32           = 71;
    FOURCC_D24X8         = 77;
    FOURCC_D16           = 80;

    FOURCC_D32F_LOCKABLE = 82;

    FOURCC_L16           = 81;

// Floating point surface formats

// s10e5 formats (16-bits per channel)
    FOURCC_R16F          = 111;
    FOURCC_G16R16F       = 112;
    FOURCC_A16B16G16R16F = 113;

// IEEE s23e8 formats (32-bits per channel)
    FOURCC_R32F          = 114;
    FOURCC_G32R32F       = 115;
    FOURCC_A32B32G32R32F = 116;

// DX10 header indicator
    FOURCC_DX10          = $47495844;

type
  TGLImageDataFormat = record
    ColorFlag: Cardinal;
    RBits, GBits, BBits, ABits: Cardinal;
    colorFormat: Cardinal;
    TexFormat: TGLinternalFormat;
    dType: Cardinal;
  end;

const
  cImageDataFormat8bits: array[0..3] of TGLImageDataFormat = (
    (ColorFlag: DDPF_RGB;
     RBits: $E0;
     GBits: $1C;
     BBits: $03;
     ABits: $00;
     colorFormat: GL_RGB;
     TexFormat: tfR3_G3_B2;
     dType: GL_UNSIGNED_BYTE_3_3_2),

    (ColorFlag: DDPF_LA;
     RBits: $0F;
     GBits: $00;
     BBits: $00;
     ABits: $F0;
     colorFormat: GL_LUMINANCE_ALPHA;
     TexFormat: tfLUMINANCE4_ALPHA4;
     dType: GL_UNSIGNED_BYTE),

    (ColorFlag: DDPF_A;
     RBits: $00;
     GBits: $00;
     BBits: $00;
     ABits: $FF;
     colorFormat: GL_ALPHA;
     TexFormat: tfALPHA8;
     dType: GL_UNSIGNED_BYTE),

    (ColorFlag: DDPF_L;
     RBits: $FF;
     GBits: $00;
     BBits: $00;
     ABits: $00;
     colorFormat: GL_LUMINANCE;
     TexFormat: tfLUMINANCE8;
     dType: GL_UNSIGNED_BYTE)
  );

  cImageDataFormat16bits: array[0..4] of TGLImageDataFormat = (
    (ColorFlag: DDPF_RGBA;
     RBits: $0F00;
     GBits: $F0;
     BBits: $0F;
     ABits: $F000;
     colorFormat: GL_BGRA;
     TexFormat: tfRGBA4;
     dType: GL_UNSIGNED_SHORT_4_4_4_4_REV),

    (ColorFlag: DDPF_RGB;
     RBits: $F800;
     GBits: $07E0;
     BBits: $1F;
     ABits: $00;
     colorFormat: GL_RGB;
     TexFormat: tfRGB5;
     dType: GL_UNSIGNED_SHORT_5_6_5),

    (ColorFlag: DDPF_L;
     RBits: $FFFF;
     GBits: $00;
     BBits: $00;
     ABits: $00;
     colorFormat: GL_LUMINANCE;
     TexFormat: tfLUMINANCE16;
     dType: GL_UNSIGNED_SHORT),

    (ColorFlag: DDPF_LA;
     RBits: $FF;
     GBits: $00;
     BBits: $00;
     ABits: $FF00;
     colorFormat: GL_LUMINANCE_ALPHA;
     TexFormat: tfLUMINANCE8_ALPHA8;
     dType: GL_UNSIGNED_BYTE),

    (ColorFlag: DDPF_RGBA;
     RBits: $7C00;
     GBits: $03E0;
     BBits: $1F;
     ABits: $8000;
     colorFormat: GL_BGRA;
     TexFormat: tfRGB5_A1;
     dType: GL_UNSIGNED_SHORT_1_5_5_5_REV)
  );

  cImageDataFormat24bits: array[0..0] of TGLImageDataFormat = (
    (ColorFlag: DDPF_RGB;
     RBits: $FF0000;
     GBits: $FF00;
     BBits: $FF;
     ABits: $00;
     colorFormat: GL_BGR;
     TexFormat: tfRGB8;
     dType: GL_UNSIGNED_BYTE)
  );

  cImageDataFormat32bits: array[0..6] of TGLImageDataFormat = (
    (ColorFlag: DDPF_RGBA;
     RBits: $FF;
     GBits: $FF00;
     BBits: $FF0000;
     ABits: $FF000000;
     colorFormat: GL_RGBA;
     TexFormat: tfRGBA8;
     dType: GL_UNSIGNED_BYTE),

    (ColorFlag: DDPF_RGBA;
     RBits: $FF0000;
     GBits: $FF00;
     BBits: $FF;
     ABits: $FF000000;
     colorFormat: GL_BGRA;
     TexFormat: tfRGBA8;
     dType: GL_UNSIGNED_BYTE),

    (ColorFlag: DDPF_RGBA;
     RBits: $3FF00000;
     GBits: $0FFC00;
     BBits: $03FF;
     ABits: $0C0000000;
     colorFormat: GL_RGBA;
     TexFormat: tfRGB10_A2;
     dType: GL_UNSIGNED_INT_2_10_10_10_REV),

    (ColorFlag: DDPF_RGBA;
     RBits: $03FF;
     GBits: $FFC00;
     BBits: $3FF00000;
     ABits: $C0000000;
     colorFormat: GL_BGRA;
     TexFormat: tfRGB10_A2;
     dType: GL_UNSIGNED_INT_2_10_10_10_REV),

    (ColorFlag: DDPF_RGBA;
     RBits: $FF0000;
     GBits: $FF00;
     BBits: $FF;
     ABits: $FF000000;
     colorFormat: GL_BGRA;
     TexFormat: tfRGB8;
     dType: GL_UNSIGNED_INT_8_8_8_8),

    (ColorFlag: DDPF_RGBA;
     RBits: $FF;
     GBits: $FF00;
     BBits: $FF0000;
     ABits: $FF000000;
     colorFormat: GL_RGBA;
     TexFormat: tfRGB8;
     dType: GL_UNSIGNED_INT_8_8_8_8),

    (ColorFlag: DDPF_RGB;
     RBits: $FFFF;
     GBits: $FFFF0000;
     BBits: $00;
     ABits: $00;
     colorFormat: GL_RG;
     TexFormat: tfRG16;
     dType: GL_UNSIGNED_SHORT)
  );

procedure DecodeDXT1toBitmap32(
   encData, decData : PByteArray;
   w,h : Integer; var trans : Boolean);
procedure DecodeDXT3toBitmap32(encData, decData : PByteArray; w,h : Integer);
procedure DecodeDXT5toBitmap32(encData, decData : PByteArray; w,h : Integer);
procedure flip_blocks_dxtc1( data : PGLubyte; numBlocks: integer);
procedure flip_blocks_dxtc3( data : PGLubyte; numBlocks: integer);
procedure flip_blocks_dxtc5( data : PGLubyte; numBlocks: integer);
procedure flip_dxt5_alpha  ( block : PDXT5AlphaBlock);

function DDSHeaderToGLEnum(const DX9header: TDDSHeader;
                           const DX11header: TDDS_HEADER_DXT10;
                           const useDX11: Boolean;
                           out iFormat: TGLInternalFormat;
                           out colorFormat: Cardinal;
                           out dataType: Cardinal;
                           out bpe: Integer): Boolean;

function GLEnumToDDSHeader(var DX9header: TDDSHeader;
                           var DX11header: TDDS_HEADER_DXT10;
                           const useDX11: Boolean;
                           const iFormat: TGLInternalFormat;
                           const colorFormat: Cardinal;
                           const dataType: Cardinal;
                           const bpe: Integer): Boolean;

function FindDDSCompatibleDataFormat(const iFormat: TGLInternalFormat;
                                     out colorFormat: Cardinal;
                                     out dataType: Cardinal): Boolean;

implementation

procedure DecodeColor565(col : Word; out r,g,b : Byte);
begin
   r:=col and $1F;
   g:=(col shr 5) and $3F;
   b:=(col shr 11) and $1F;
end;

procedure DecodeDXT1toBitmap32(
   encData, decData : PByteArray;
   w,h : Integer; var trans : Boolean);
var
   x,y,i,j,k,select : Integer;
   col0, col1 : Word;
   colors : array[0..3] of array[0..3] of Byte;
   bitmask : Cardinal;
   temp : PGLubyte;
   r0,g0,b0,r1,g1,b1 : Byte;
begin
   trans:=False;

   if not (Assigned(encData) and Assigned(decData)) then exit;

   temp:=PGLubyte(encData);
   for y:=0 to (h div 4)-1 do begin
      for x:=0 to (w div 4)-1 do begin
         col0:=PWord(temp)^;        Inc(temp, 2);
         col1:=PWord(temp)^;        Inc(temp, 2);
         bitmask:=PCardinal(temp)^; Inc(temp, 4);

         DecodeColor565(col0,r0,g0,b0);
         DecodeColor565(col1,r1,g1,b1);

         colors[0][0]:=r0 shl 3;
         colors[0][1]:=g0 shl 2;
         colors[0][2]:=b0 shl 3;
         colors[0][3]:=$FF;
         colors[1][0]:=r1 shl 3;
         colors[1][1]:=g1 shl 2;
         colors[1][2]:=b1 shl 3;
         colors[1][3]:=$FF;

         if col0>col1 then begin
            colors[2][0]:=(2*colors[0][0]+colors[1][0]+1) div 3;
            colors[2][1]:=(2*colors[0][1]+colors[1][1]+1) div 3;
            colors[2][2]:=(2*colors[0][2]+colors[1][2]+1) div 3;
            colors[2][3]:=$FF;
            colors[3][0]:=(colors[0][0]+2*colors[1][0]+1) div 3;
            colors[3][1]:=(colors[0][1]+2*colors[1][1]+1) div 3;
            colors[3][2]:=(colors[0][2]+2*colors[1][2]+1) div 3;
            colors[3][3]:=$FF;
         end else begin
            trans:=True;
            colors[2][0]:=(colors[0][0]+colors[1][0]) div 2;
            colors[2][1]:=(colors[0][1]+colors[1][1]) div 2;
            colors[2][2]:=(colors[0][2]+colors[1][2]) div 2;
            colors[2][3]:=$FF;
            colors[3][0]:=(colors[0][0]+2*colors[1][0]+1) div 3;
            colors[3][1]:=(colors[0][1]+2*colors[1][1]+1) div 3;
            colors[3][2]:=(colors[0][2]+2*colors[1][2]+1) div 3;
            colors[3][3]:=0;
         end;

         k:=0;
         for j:=0 to 3 do begin
            for i:=0 to 3 do begin
               select:=(bitmask and (3 shl (k*2))) shr (k*2);
               if ((4*x+i)<w) and ((4*y+j)<h) then
                  PCardinal(@decData[((4*y+j)*w+(4*x+i))*4])^:=Cardinal(colors[select]);
               Inc(k);
            end;
         end;

      end;
   end;
end;

procedure DecodeDXT3toBitmap32(encData, decData : PByteArray; w,h : Integer);
var
   x,y,i,j,k,select : Integer;
   col0, col1, wrd : Word;
   colors : array[0..3] of array[0..3] of Byte;
   bitmask, offset : Cardinal;
   temp : PGLubyte;
   r0,g0,b0,r1,g1,b1 : Byte;
   alpha : array[0..3] of Word;
begin
   if not (Assigned(encData) and Assigned(decData)) then exit;

   temp:=PGLubyte(encData);
   for y:=0 to (h div 4)-1 do begin
      for x:=0 to (w div 4)-1 do begin
         alpha[0]:=PWord(temp)^;    Inc(temp, 2);
         alpha[1]:=PWord(temp)^;    Inc(temp, 2);
         alpha[2]:=PWord(temp)^;    Inc(temp, 2);
         alpha[3]:=PWord(temp)^;    Inc(temp, 2);
         col0:=PWord(temp)^;        Inc(temp, 2);
         col1:=PWord(temp)^;        Inc(temp, 2);
         bitmask:=PCardinal(temp)^; Inc(temp, 4);

         DecodeColor565(col0,r0,g0,b0);
         DecodeColor565(col1,r1,g1,b1);

         colors[0][0]:=r0 shl 3;
         colors[0][1]:=g0 shl 2;
         colors[0][2]:=b0 shl 3;
         colors[0][3]:=$FF;
         colors[1][0]:=r1 shl 3;
         colors[1][1]:=g1 shl 2;
         colors[1][2]:=b1 shl 3;
         colors[1][3]:=$FF;
         colors[2][0]:=(2*colors[0][0]+colors[1][0]+1) div 3;
         colors[2][1]:=(2*colors[0][1]+colors[1][1]+1) div 3;
         colors[2][2]:=(2*colors[0][2]+colors[1][2]+1) div 3;
         colors[2][3]:=$FF;
         colors[3][0]:=(colors[0][0]+2*colors[1][0]+1) div 3;
         colors[3][1]:=(colors[0][1]+2*colors[1][1]+1) div 3;
         colors[3][2]:=(colors[0][2]+2*colors[1][2]+1) div 3;
         colors[3][3]:=$FF;

         k:=0;
         for j:=0 to 3 do begin
            for i:=0 to 3 do begin
               select:=(bitmask and (3 shl (k*2))) shr (k*2);
               if ((4*x+i)<w) and ((4*y+j)<h) then
                  PCardinal(@decData[((4*y+j)*w+(4*x+i))*4])^:=Cardinal(colors[select]);
               Inc(k);
            end;
         end;

         for j:=0 to 3 do begin
            wrd:=alpha[j];
            for i:=0 to 3 do begin
               if (((4*x+i)<w) and ((4*y+j)<h)) then begin
                  offset:=((4*y+j)*w+(4*x+i))*4+3;
                  decData[offset]:=wrd and $0F;
                  decData[offset]:=decData[offset] or (decData[offset] shl 4);
               end;
               wrd:=wrd shr 4;
            end;
         end;

      end;
   end;
end;

procedure DecodeDXT5toBitmap32(encData, decData : PByteArray; w,h : Integer);
var
   x,y,i,j,k,select : Integer;
   col0, col1 : Word;
   colors : array[0..3] of array[0..3] of Byte;
   bits, bitmask, offset : Cardinal;
   temp, alphamask : PGLubyte;
   r0,g0,b0,r1,g1,b1 : Byte;
   alphas : array[0..7] of Byte;
begin
   if not (Assigned(encData) and Assigned(decData)) then exit;

   temp:=PGLubyte(encData);
   for y:=0 to (h div 4)-1 do begin
      for x:=0 to (w div 4)-1 do begin
         alphas[0]:=temp^; Inc(temp);
         alphas[1]:=temp^; Inc(temp);
         alphamask:=temp; Inc(temp, 6);
         col0:=PWord(temp)^;        Inc(temp, 2);
         col1:=PWord(temp)^;        Inc(temp, 2);
         bitmask:=PCardinal(temp)^; Inc(temp, 4);

         DecodeColor565(col0,r0,g0,b0);
         DecodeColor565(col1,r1,g1,b1);

         colors[0][0]:=r0 shl 3;
         colors[0][1]:=g0 shl 2;
         colors[0][2]:=b0 shl 3;
         colors[0][3]:=$FF;
         colors[1][0]:=r1 shl 3;
         colors[1][1]:=g1 shl 2;
         colors[1][2]:=b1 shl 3;
         colors[1][3]:=$FF;
         colors[2][0]:=(2*colors[0][0]+colors[1][0]+1) div 3;
         colors[2][1]:=(2*colors[0][1]+colors[1][1]+1) div 3;
         colors[2][2]:=(2*colors[0][2]+colors[1][2]+1) div 3;
         colors[2][3]:=$FF;
         colors[3][0]:=(colors[0][0]+2*colors[1][0]+1) div 3;
         colors[3][1]:=(colors[0][1]+2*colors[1][1]+1) div 3;
         colors[3][2]:=(colors[0][2]+2*colors[1][2]+1) div 3;
         colors[3][3]:=$FF;

         k:=0;
         for j:=0 to 3 do begin
            for i:=0 to 3 do begin
               select:=(bitmask and (3 shl (k*2))) shr (k*2);
               if ((4*x+i)<w) and ((4*y+j)<h) then
                  PCardinal(@decData[((4*y+j)*w+(4*x+i))*4])^:=Cardinal(colors[select]);
               Inc(k);
            end;
         end;

         if (alphas[0] > alphas[1]) then begin
            alphas[2]:=(6*alphas[0]+1*alphas[1]+3) div 7;
            alphas[3]:=(5*alphas[0]+2*alphas[1]+3) div 7;
            alphas[4]:=(4*alphas[0]+3*alphas[1]+3) div 7;
            alphas[5]:=(3*alphas[0]+4*alphas[1]+3) div 7;
            alphas[6]:=(2*alphas[0]+5*alphas[1]+3) div 7;
            alphas[7]:=(1*alphas[0]+6*alphas[1]+3) div 7;
         end else begin
            alphas[2]:=(4*alphas[0]+1*alphas[1]+2) div 5;
            alphas[3]:=(3*alphas[0]+2*alphas[1]+2) div 5;
            alphas[4]:=(2*alphas[0]+3*alphas[1]+2) div 5;
            alphas[5]:=(1*alphas[0]+4*alphas[1]+2) div 5;
            alphas[6]:=0;
            alphas[7]:=$FF;
         end;

         bits:=PCardinal(alphamask)^;
         for j:=0 to 1 do begin
            for i:=0 to 3 do begin
               if (((4*x+i)<w) and ((4*y+j)<h)) then begin
                  offset:=((4*y+j)*w+(4*x+i))*4+3;
                  decData[Offset]:=alphas[bits and 7];
               end;
               bits:=bits shr 3;
            end;
         end;

         Inc(alphamask, 3);
         bits:=PCardinal(alphamask)^;
         for j:=2 to 3 do begin
            for i:=0 to 3 do begin
               if (((4*x+i)<w) and ((4*y+j)<h)) then begin
                  offset:=((4*y+j)*w+(4*x+i))*4+3;
                  decData[offset]:=alphas[bits and 7];
               end;
               bits:=bits shr 3;
            end;
         end;

      end;
   end;
end;

////////////////////////////////////////////////////////////
procedure flip_blocks_dxtc1( data : PGLubyte; numBlocks: integer);
var
  curblock : PDXTColBlock;
  temp : Byte;
  i : integer;
begin
  curblock := PDXTColBlock( data );
  for i := 0 to  numBlocks-1 do begin
    temp := curblock.row[0];
    curblock.row[0] := curblock.row[3];
    curblock.row[3] := temp;
    temp := curblock.row[1];
    curblock.row[1] := curblock.row[2];
    curblock.row[2] := temp;

    Inc( curblock );
  end;
end;

// flip a DXT3 color block
////////////////////////////////////////////////////////////
procedure flip_blocks_dxtc3( data: PGLubyte; numBlocks: integer );
var
  curblock : PDXTColBlock;
  alphablock : PDXT3AlphaBlock;
  tempS : Word;
  tempB : Byte;
  i : integer;
begin
  curblock := PDXTColBlock( data );
  for i := 0 to numBlocks-1 do
  begin
    alphablock := PDXT3AlphaBlock( curblock );

    tempS := alphablock.row[0];
    alphablock.row[0] := alphablock.row[3];
    alphablock.row[3] := tempS;
    tempS := alphablock.row[1];
    alphablock.row[1] := alphablock.row[2];
    alphablock.row[2] := tempS;

    Inc( curblock );

    tempB := curblock.row[0];
    curblock.row[0] := curblock.row[3];
    curblock.row[3] := tempB;
    tempB := curblock.row[1];
    curblock.row[1] := curblock.row[2];
    curblock.row[2] := tempB;

    Inc( curblock );
  end;
end;

////////////////////////////////////////////////////////////
procedure flip_dxt5_alpha( block : PDXT5AlphaBlock);
const
  mask = $00000007;          // bits = 00 00 01 11
var
  gBits : array[0..3, 0..3] of Byte;
  bits  : Integer;
begin
  bits := 0;
  Move(block.row[0], bits, sizeof(Byte) * 3);

  gBits[0][0] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[0][1] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[0][2] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[0][3] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[1][0] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[1][1] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[1][2] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[1][3] := Byte(bits and mask);

  bits := 0;
  Move(block.row[3], bits, sizeof(Byte) * 3);

  gBits[2][0] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[2][1] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[2][2] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[2][3] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[3][0] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[3][1] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[3][2] := Byte(bits and mask);
  bits := bits shr 3;
  gBits[3][3] := Byte(bits and mask);

  // clear existing alpha bits
  FillChar( block.row, sizeof(Byte) * 6, 0);

  bits := block.row[0]+block.row[1]*$100+block.row[2]*$10000;

  bits := bits or (gBits[3][0] shl 0);
  bits := bits or (gBits[3][1] shl 3);
  bits := bits or (gBits[3][2] shl 6);
  bits := bits or (gBits[3][3] shl 9);

  bits := bits or (gBits[2][0] shl 12);
  bits := bits or (gBits[2][1] shl 15);
  bits := bits or (gBits[2][2] shl 18);
  bits := bits or (gBits[2][3] shl 21);

  block.row[0] := bits and $FF;
  block.row[1] := (bits shr 8) and $FF;
  block.row[2] := (bits shr 16) and $FF;

  bits := block.row[3]+block.row[4]*$100+block.row[5]*$10000;

  bits := bits or (gBits[1][0] shl 0);
  bits := bits or (gBits[1][1] shl 3);
  bits := bits or (gBits[1][2] shl 6);
  bits := bits or (gBits[1][3] shl 9);

  bits := bits or (gBits[0][0] shl 12);
  bits := bits or (gBits[0][1] shl 15);
  bits := bits or (gBits[0][2] shl 18);
  bits := bits or (gBits[0][3] shl 21);

  block.row[3] := bits and $FF;
  block.row[4] := (bits shr 8) and $FF;
  block.row[5] := (bits shr 16) and $FF;
end;

////////////////////////////////////////////////////////////
procedure flip_blocks_dxtc5( data: PGLubyte; numBlocks: integer );
var
  curblock : PDXTColBlock;
  temp : Byte;
  i : integer;
begin
  curblock := PDXTColBlock( data );
  for i := 0 to numBlocks-1 do
  begin
    flip_dxt5_alpha( PDXT5AlphaBlock( curblock ) );
    Inc( curblock );
    temp := curblock.row[0];
    curblock.row[0] := curblock.row[3];
    curblock.row[3] := temp;
    temp := curblock.row[1];
    curblock.row[1] := curblock.row[2];
    curblock.row[2] := temp;
    Inc( curblock );
  end;
end;

function DDSHeaderToGLEnum(const DX9header: TDDSHeader;
                           const DX11header: TDDS_HEADER_DXT10;
                           const useDX11: Boolean;
                           out iFormat: TGLInternalFormat;
                           out colorFormat: Cardinal;
                           out dataType: Cardinal;
                           out bpe: Integer): Boolean;
var
  i: Integer;
begin
  Result := true;
  if useDX11 then
  begin
    Assert(false, 'DXGI images not supported.');
  end
  // Use DX9 formats
  else begin
    // figure out what the image format is
    if (DX9header.SurfaceFormat.ddpf.dwFlags and DDPF_FOURCC)<>0 then
    begin
      case DX9header.SurfaceFormat.ddpf.dwFourCC of
        FOURCC_DXT1:
          begin
            colorFormat := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
            iFormat := tfCOMPRESSED_RGBA_S3TC_DXT1;
            dataType := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
            bpe := 8;
          end;
        FOURCC_DXT2, FOURCC_DXT3:
          begin
            colorFormat := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
            iFormat := tfCOMPRESSED_RGBA_S3TC_DXT3;
            dataType := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
            bpe := 16;
          end;
        FOURCC_DXT4, FOURCC_DXT5:
          begin
            colorFormat := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
            iFormat := tfCOMPRESSED_RGBA_S3TC_DXT5;
            dataType := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
            bpe := 16;
          end;
        FOURCC_ATI1:
          begin
            colorFormat := GL_COMPRESSED_RED_RGTC1;
            iFormat := tfCOMPRESSED_RED_RGTC1;
            dataType := GL_COMPRESSED_RED_RGTC1;
            bpe := 8;
          end;
        FOURCC_ATI2:
          begin
            colorFormat := GL_COMPRESSED_RG_RGTC2;
            iFormat := tfCOMPRESSED_RG_RGTC2;
            dataType := GL_COMPRESSED_RG_RGTC2;
            bpe := 16;
          end;
        FOURCC_R8G8B8:
          begin
            colorFormat := GL_BGR;
            iFormat := tfRGB8;
            dataType := GL_UNSIGNED_BYTE;
            bpe := 3;
          end;
        FOURCC_A8R8G8B8:
          begin
            colorFormat := GL_BGRA;
            iFormat := tfRGBA8;
            dataType := GL_UNSIGNED_BYTE;
            bpe := 4;
          end;
        FOURCC_X8R8G8B8:
          begin
            colorFormat := GL_BGRA;
            iFormat := tfRGB8;
            dataType := GL_UNSIGNED_INT_8_8_8_8;
            bpe := 4;
          end;
        FOURCC_R5G6B5:
          begin
            colorFormat := GL_RGB;
            iFormat := tfRGB5;
            dataType := GL_UNSIGNED_SHORT_5_6_5;
            bpe := 2;
          end;
        FOURCC_A8:
          begin
            colorFormat := GL_ALPHA;
            iFormat := tfALPHA8;
            dataType := GL_UNSIGNED_BYTE;
            bpe := 1;
          end;
        FOURCC_A2B10G10R10:
          begin
            colorFormat := GL_RGBA;
            iFormat := tfRGB10_A2;
            dataType := GL_UNSIGNED_INT_10_10_10_2;
            bpe := 4;
          end;
        FOURCC_A8B8G8R8:
          begin
            colorFormat := GL_RGBA;
            iFormat := tfRGBA8;
            dataType := GL_UNSIGNED_BYTE;
            bpe := 4;
          end;
        FOURCC_X8B8G8R8:
          begin
            colorFormat := GL_RGBA;
            iFormat := tfRGB8;
            dataType := GL_UNSIGNED_INT_8_8_8_8;
            bpe := 4;
          end;
        FOURCC_A2R10G10B10:
          begin
            colorFormat := GL_BGRA;
            iFormat := tfRGB10_A2;
            dataType := GL_UNSIGNED_INT_10_10_10_2;
            bpe := 4;
          end;
        FOURCC_A16B16G16R16:
          begin
            colorFormat := GL_RGBA;
            iFormat := tfR16G16B16A16;
            dataType := GL_UNSIGNED_SHORT;
            bpe := 8;
          end;
        FOURCC_L8:
          begin
            colorFormat := GL_LUMINANCE;
            iFormat := tfLUMINANCE8;
            dataType := GL_UNSIGNED_BYTE;
            bpe := 1;
          end;
        FOURCC_A8L8:
          begin
            colorFormat := GL_LUMINANCE_ALPHA;
            iFormat := tfLUMINANCE8_ALPHA8;
            dataType := GL_UNSIGNED_BYTE;
            bpe := 2;
          end;
        FOURCC_L16:
          begin
            colorFormat := GL_LUMINANCE;
            iFormat := tfLUMINANCE16;
            dataType := GL_UNSIGNED_SHORT;
            bpe := 2;
          end;
        FOURCC_R16F:
          begin
            colorFormat := GL_RED;
            iFormat := tfLUMINANCE_FLOAT16;
            dataType := GL_HALF_FLOAT_ARB;
            bpe := 2;
          end;
        FOURCC_A16B16G16R16F:
          begin
            colorFormat := GL_RGBA;
            iFormat := tfRGBA_FLOAT16;
            dataType := GL_HALF_FLOAT_ARB;
            bpe := 8;
          end;
        FOURCC_R32F:
          begin
            colorFormat := GL_RED;
            iFormat := tfLUMINANCE_FLOAT32;
            dataType := GL_FLOAT;
            bpe := 4;
          end;
        FOURCC_G16R16:
          begin
            colorFormat := GL_RG;
            iFormat := tfRG16;
            dataType := GL_UNSIGNED_SHORT;
            bpe := 4;
          end;
        FOURCC_G16R16F:
          begin
            colorFormat := GL_RG;
            iFormat := tfRG16F;
            dataType := GL_HALF_FLOAT;
            bpe := 4;
          end;
        FOURCC_G32R32F:
          begin
            colorFormat := GL_RG;
            iFormat := tfRG32F;
            dataType := GL_FLOAT;
            bpe := 8;
          end;
        FOURCC_UNKNOWN,
        FOURCC_X1R5G5B5,
        FOURCC_A1R5G5B5,
        FOURCC_A4R4G4B4,
        FOURCC_R3G3B2,
        FOURCC_A8R3G3B2,
        FOURCC_X4R4G4B4,
        FOURCC_A4L4,
        FOURCC_D16_LOCKABLE,
        FOURCC_D32,
        FOURCC_D24X8,
        FOURCC_D16,
        FOURCC_D32F_LOCKABLE: Result := false; //these are unsupported for now
      end; // of case
    end // not FOURCC

    else
      with DX9header.SurfaceFormat.ddpf do
        case dwRGBBitCount of
        8: begin
          for i := 0 to High(cImageDataFormat8bits) do
            if  (cImageDataFormat8bits[i].ColorFlag = dwFlags)
            and (cImageDataFormat8bits[i].RBits = dwRBitMask)
            and (cImageDataFormat8bits[i].GBits = dwGBitMask)
            and (cImageDataFormat8bits[i].BBits = dwBBitMask)
            and (cImageDataFormat8bits[i].ABits = dwRGBAlphaBitMask) then
            begin
              colorFormat := cImageDataFormat8bits[i].colorFormat;
              iFormat := cImageDataFormat8bits[i].TexFormat;
              dataType := cImageDataFormat8bits[i].dType;
              Result := true;
              Break;
            end;
          bpe := 1;
        end;
        16: begin
          for i := 0 to High(cImageDataFormat16bits) do
            if  (cImageDataFormat16bits[i].ColorFlag = dwFlags)
            and (cImageDataFormat16bits[i].RBits = dwRBitMask)
            and (cImageDataFormat16bits[i].GBits = dwGBitMask)
            and (cImageDataFormat16bits[i].BBits = dwBBitMask)
            and (cImageDataFormat16bits[i].ABits = dwRGBAlphaBitMask) then
            begin
              colorFormat := cImageDataFormat16bits[i].colorFormat;
              iFormat := cImageDataFormat16bits[i].TexFormat;
              dataType := cImageDataFormat16bits[i].dType;
              Result := true;
              Break;
            end;
          bpe := 2;
        end;
        24: begin
          for i := 0 to High(cImageDataFormat24bits) do
            if  (cImageDataFormat24bits[i].ColorFlag = dwFlags)
            and (cImageDataFormat24bits[i].RBits = dwRBitMask)
            and (cImageDataFormat24bits[i].GBits = dwGBitMask)
            and (cImageDataFormat24bits[i].BBits = dwBBitMask)
            and (cImageDataFormat24bits[i].ABits = dwRGBAlphaBitMask) then
            begin
              colorFormat := cImageDataFormat24bits[i].colorFormat;
              iFormat := cImageDataFormat24bits[i].TexFormat;
              dataType := cImageDataFormat24bits[i].dType;
              Result := true;
              Break;
            end;
          bpe := 3;
        end;
        32: begin
          for i := 0 to High(cImageDataFormat32bits) do
            if  (cImageDataFormat32bits[i].ColorFlag = dwFlags)
            and (cImageDataFormat32bits[i].RBits = dwRBitMask)
            and (cImageDataFormat32bits[i].GBits = dwGBitMask)
            and (cImageDataFormat32bits[i].BBits = dwBBitMask)
            and (cImageDataFormat32bits[i].ABits = dwRGBAlphaBitMask) then
            begin
              colorFormat := cImageDataFormat32bits[i].colorFormat;
              iFormat := cImageDataFormat32bits[i].TexFormat;
              dataType := cImageDataFormat32bits[i].dType;
              Result := true;
              Break;
            end;
          bpe := 4;
        end;
        else Result := false;
      end; // of case
  end;
end;

function GLEnumToDDSHeader(var DX9header: TDDSHeader;
                           var DX11header: TDDS_HEADER_DXT10;
                           const useDX11: Boolean;
                           const iFormat: TGLInternalFormat;
                           const colorFormat: Cardinal;
                           const dataType: Cardinal;
                           const bpe: Integer): Boolean;
var
  i: Integer;
begin
  Result := true;
  if useDX11 then
  begin
    Assert(false, 'DXGI images not supported.');
  end;

  if IsCompressedFormat(iFormat) then
  begin
    with DX9header.SurfaceFormat.ddpf do
    begin
      dwFlags := DDPF_FOURCC;
      case iFormat of
        tfCOMPRESSED_RGB_S3TC_DXT1:         dwFourCC := FOURCC_DXT1;
        tfCOMPRESSED_RGBA_S3TC_DXT1:        dwFourCC := FOURCC_DXT1;
        tfCOMPRESSED_RGBA_S3TC_DXT3:        dwFourCC := FOURCC_DXT3;
        tfCOMPRESSED_RGBA_S3TC_DXT5:        dwFourCC := FOURCC_DXT5;
        tfCOMPRESSED_LUMINANCE_LATC1:       dwFourCC := FOURCC_ATI1;
        tfCOMPRESSED_LUMINANCE_ALPHA_LATC2: dwFourCC := FOURCC_ATI2;
        else Result := false;
      end;
    end;
  end
  else if IsFloatFormat(iFormat) then
  begin
    with DX9header.SurfaceFormat.ddpf do
    begin
      dwFlags := DDPF_FOURCC;
      case iFormat of
        tfINTENSITY_FLOAT16,
        tfLUMINANCE_FLOAT16,
        tfR16F:                             dwFourCC := FOURCC_R16F;
        tfRGBA_FLOAT16:                     dwFourCC := FOURCC_A16B16G16R16F;
        tfINTENSITY_FLOAT32,
        tfLUMINANCE_FLOAT32,
        tfR32F:                             dwFourCC := FOURCC_R32F;
        tfLUMINANCE_ALPHA_FLOAT16,
        tfRG16F:                            dwFourCC := FOURCC_G16R16F;
        tfLUMINANCE_ALPHA_FLOAT32,
        tfRG32F:                            dwFourCC := FOURCC_G32R32F;
        tfRGBA_FLOAT32:                     dwFourCC := FOURCC_A32B32G32R32F
        else Result := false;
      end;
    end;
  end
  else with DX9header.SurfaceFormat.ddpf do
  begin
    dwFourCC := 0;
    dwRGBBitCount := bpe * 8;
    case bpe of
      1: begin
        for i := 0 to High(cImageDataFormat8bits) do
          if (cImageDataFormat8bits[i].colorFormat = colorFormat)
          and (cImageDataFormat8bits[i].TexFormat = iFormat)
          and (cImageDataFormat8bits[i].dType = dataType) then
          begin
            dwFlags := cImageDataFormat8bits[i].ColorFlag;
            dwRBitMask := cImageDataFormat8bits[i].RBits;
            dwGBitMask := cImageDataFormat8bits[i].GBits;
            dwBBitMask := cImageDataFormat8bits[i].BBits;
            dwRGBAlphaBitMask := cImageDataFormat8bits[i].ABits;
            Break;
          end;
      end;

      2: begin
        for i := 0 to High(cImageDataFormat16bits) do
          if (cImageDataFormat16bits[i].colorFormat = colorFormat)
          and (cImageDataFormat16bits[i].TexFormat = iFormat)
          and (cImageDataFormat16bits[i].dType = dataType) then
          begin
            dwFlags := cImageDataFormat16bits[i].ColorFlag;
            dwRBitMask := cImageDataFormat16bits[i].RBits;
            dwGBitMask := cImageDataFormat16bits[i].GBits;
            dwBBitMask := cImageDataFormat16bits[i].BBits;
            dwRGBAlphaBitMask := cImageDataFormat16bits[i].ABits;
            Break;
          end;
      end;

      3: begin
        for i := 0 to High(cImageDataFormat24bits) do
          if (cImageDataFormat24bits[i].colorFormat = colorFormat)
          and (cImageDataFormat24bits[i].TexFormat = iFormat)
          and (cImageDataFormat24bits[i].dType = dataType) then
          begin
            dwFlags := cImageDataFormat24bits[i].ColorFlag;
            dwRBitMask := cImageDataFormat24bits[i].RBits;
            dwGBitMask := cImageDataFormat24bits[i].GBits;
            dwBBitMask := cImageDataFormat24bits[i].BBits;
            dwRGBAlphaBitMask := cImageDataFormat24bits[i].ABits;
            Break;
          end;
      end;

      4: begin
        for i := 0 to High(cImageDataFormat32bits) do
          if (cImageDataFormat32bits[i].colorFormat = colorFormat)
          and (cImageDataFormat32bits[i].TexFormat = iFormat)
          and (cImageDataFormat32bits[i].dType = dataType) then
          begin
            dwFlags := cImageDataFormat32bits[i].ColorFlag;
            dwRBitMask := cImageDataFormat32bits[i].RBits;
            dwGBitMask := cImageDataFormat32bits[i].GBits;
            dwBBitMask := cImageDataFormat32bits[i].BBits;
            dwRGBAlphaBitMask := cImageDataFormat32bits[i].ABits;
            Break;
          end;
      end;

      else Result := false;
    end; // of case
  end;

end;

function FindDDSCompatibleDataFormat(const iFormat: TGLInternalFormat;
                                     out colorFormat: Cardinal;
                                     out dataType: Cardinal): Boolean;
var
  i: Integer;
begin
  Result := false;
  // 32 bits data format
  for i := 0 to High(cImageDataFormat32bits) do
    if cImageDataFormat32bits[i].TexFormat = iFormat then
    begin
      colorFormat := cImageDataFormat32bits[i].colorFormat;
      dataType := cImageDataFormat32bits[i].dType;
      Result := true;
      Exit;
    end;
  // 24 bits data format
  for i := 0 to High(cImageDataFormat24bits) do
    if cImageDataFormat24bits[i].TexFormat = iFormat then
    begin
      colorFormat := cImageDataFormat24bits[i].colorFormat;
      dataType := cImageDataFormat24bits[i].dType;
      Result := true;
      Exit;
    end;
  // 16 bits data format
  for i := 0 to High(cImageDataFormat16bits) do
    if cImageDataFormat16bits[i].TexFormat = iFormat then
    begin
      colorFormat := cImageDataFormat16bits[i].colorFormat;
      dataType := cImageDataFormat16bits[i].dType;
      Result := true;
      Exit;
    end;
  // 8 bits data format
  for i := 0 to High(cImageDataFormat8bits) do
    if cImageDataFormat8bits[i].TexFormat = iFormat then
    begin
      colorFormat := cImageDataFormat8bits[i].colorFormat;
      dataType := cImageDataFormat8bits[i].dType;
      Result := true;
      Exit;
    end;
end;

end.