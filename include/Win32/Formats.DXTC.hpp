// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Formats.DXTC.pas' rev: 35.00 (Windows)

#ifndef Formats_DxtcHPP
#define Formats_DxtcHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.OpenGLext.hpp>
#include <System.SysUtils.hpp>
#include <GLS.TextureFormat.hpp>

//-- user supplied -----------------------------------------------------------

namespace Formats
{
namespace Dxtc
{
//-- forward type declarations -----------------------------------------------
struct TDDPIXELFORMAT;
struct TDDSURFACEDESC2;
struct TDDSHeader;
struct DXTColBlock;
struct DXT3AlphaBlock;
struct DXT5AlphaBlock;
struct TDDS_HEADER_DXT10;
struct TGLImageDataFormat;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TDDPIXELFORMAT
{
public:
	unsigned dwSize;
	unsigned dwFlags;
	unsigned dwFourCC;
	unsigned dwRGBBitCount;
	unsigned dwRBitMask;
	unsigned dwGBitMask;
	unsigned dwBBitMask;
	unsigned dwRGBAlphaBitMask;
};


struct DECLSPEC_DRECORD TDDSURFACEDESC2
{
public:
	unsigned dwSize;
	unsigned dwFlags;
	unsigned dwHeight;
	unsigned dwWidth;
	unsigned dwPitchOrLinearSize;
	unsigned dwDepth;
	unsigned dwMipMapCount;
	System::StaticArray<unsigned, 11> dwReserved1;
	TDDPIXELFORMAT ddpf;
	unsigned dwCaps;
	unsigned dwCaps2;
	unsigned dwCaps3;
	unsigned dwCaps4;
	unsigned dwReserved2;
};


struct DECLSPEC_DRECORD TDDSHeader
{
public:
	unsigned Magic;
	TDDSURFACEDESC2 SurfaceFormat;
};


struct DECLSPEC_DRECORD DXTColBlock
{
public:
	System::Word col0;
	System::Word col1;
	System::StaticArray<System::Byte, 4> row;
};


typedef DXTColBlock *PDXTColBlock;

struct DECLSPEC_DRECORD DXT3AlphaBlock
{
public:
	System::StaticArray<System::Word, 4> row;
};


typedef DXT3AlphaBlock *PDXT3AlphaBlock;

struct DECLSPEC_DRECORD DXT5AlphaBlock
{
public:
	System::Byte alpha0;
	System::Byte alpha1;
	System::StaticArray<System::Byte, 6> row;
};


typedef DXT5AlphaBlock *PDXT5AlphaBlock;

struct DECLSPEC_DRECORD TDDS_HEADER_DXT10
{
public:
	int dxgiFormat;
	int resourceDimension;
	unsigned miscFlag;
	unsigned arraySize;
	unsigned reserved;
};


typedef System::StaticArray<char, 4> TFOURCC;

struct DECLSPEC_DRECORD TGLImageDataFormat
{
public:
	unsigned ColorFlag;
	unsigned RBits;
	unsigned GBits;
	unsigned BBits;
	unsigned ABits;
	unsigned colorFormat;
	Gls::Textureformat::TGLInternalFormat TexFormat;
	unsigned dType;
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 DDSD_CAPS = System::Int8(0x1);
static const System::Int8 DDSD_HEIGHT = System::Int8(0x2);
static const System::Int8 DDSD_WIDTH = System::Int8(0x4);
static const System::Int8 DDSD_PITCH = System::Int8(0x8);
static const System::Word DDSD_PIXELFORMAT = System::Word(0x1000);
static const int DDSD_MIPMAPCOUNT = int(0x20000);
static const int DDSD_LINEARSIZE = int(0x80000);
static const int DDSD_DEPTH = int(0x800000);
static const System::Int8 DDPF_ALPHAPIXELS = System::Int8(0x1);
static const System::Int8 DDPF_A = System::Int8(0x2);
static const System::Int8 DDPF_FOURCC = System::Int8(0x4);
static const System::Int8 DDPF_RGB = System::Int8(0x40);
static const System::Int8 DDPF_RGBA = System::Int8(0x41);
static const int DDPF_L = int(0x20000);
static const int DDPF_LA = int(0x20001);
static const System::Int8 DDSCAPS_COMPLEX = System::Int8(0x8);
static const System::Word DDSCAPS_TEXTURE = System::Word(0x1000);
static const int DDSCAPS_MIPMAP = int(0x400000);
static const System::Word DDSCAPS2_CUBEMAP = System::Word(0x200);
static const System::Word DDSCAPS2_CUBEMAP_POSITIVEX = System::Word(0x400);
static const System::Word DDSCAPS2_CUBEMAP_NEGATIVEX = System::Word(0x800);
static const System::Word DDSCAPS2_CUBEMAP_POSITIVEY = System::Word(0x1000);
static const System::Word DDSCAPS2_CUBEMAP_NEGATIVEY = System::Word(0x2000);
static const System::Word DDSCAPS2_CUBEMAP_POSITIVEZ = System::Word(0x4000);
static const System::Word DDSCAPS2_CUBEMAP_NEGATIVEZ = System::Word(0x8000);
static const int DDSCAPS2_VOLUME = int(0x200000);
static const System::Int8 DXGI_FORMAT_FORCE_UINT = System::Int8(-1);
static const System::Int8 DXGI_FORMAT_UNKNOWN = System::Int8(0x0);
static const System::Int8 DXGI_FORMAT_R32G32B32A32_TYPELESS = System::Int8(0x1);
static const System::Int8 DXGI_FORMAT_R32G32B32A32_FLOAT = System::Int8(0x2);
static const System::Int8 DXGI_FORMAT_R32G32B32A32_UINT = System::Int8(0x3);
static const System::Int8 DXGI_FORMAT_R32G32B32A32_SINT = System::Int8(0x4);
static const System::Int8 DXGI_FORMAT_R32G32B32_TYPELESS = System::Int8(0x5);
static const System::Int8 DXGI_FORMAT_R32G32B32_FLOAT = System::Int8(0x6);
static const System::Int8 DXGI_FORMAT_R32G32B32_UINT = System::Int8(0x7);
static const System::Int8 DXGI_FORMAT_R32G32B32_SINT = System::Int8(0x8);
static const System::Int8 DXGI_FORMAT_R16G16B16A16_TYPELESS = System::Int8(0x9);
static const System::Int8 DXGI_FORMAT_R16G16B16A16_FLOAT = System::Int8(0xa);
static const System::Int8 DXGI_FORMAT_R16G16B16A16_UNORM = System::Int8(0xb);
static const System::Int8 DXGI_FORMAT_R16G16B16A16_UINT = System::Int8(0xc);
static const System::Int8 DXGI_FORMAT_R16G16B16A16_SNORM = System::Int8(0xd);
static const System::Int8 DXGI_FORMAT_R16G16B16A16_SINT = System::Int8(0xe);
static const System::Int8 DXGI_FORMAT_R32G32_TYPELESS = System::Int8(0xf);
static const System::Int8 DXGI_FORMAT_R32G32_FLOAT = System::Int8(0x10);
static const System::Int8 DXGI_FORMAT_R32G32_UINT = System::Int8(0x11);
static const System::Int8 DXGI_FORMAT_R32G32_SINT = System::Int8(0x12);
static const System::Int8 DXGI_FORMAT_R32G8X24_TYPELESS = System::Int8(0x13);
static const System::Int8 DXGI_FORMAT_D32_FLOAT_S8X24_UINT = System::Int8(0x14);
static const System::Int8 DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS = System::Int8(0x15);
static const System::Int8 DXGI_FORMAT_X32_TYPELESS_G8X24_UINT = System::Int8(0x16);
static const System::Int8 DXGI_FORMAT_R10G10B10A2_TYPELESS = System::Int8(0x17);
static const System::Int8 DXGI_FORMAT_R10G10B10A2_UNORM = System::Int8(0x18);
static const System::Int8 DXGI_FORMAT_R10G10B10A2_UINT = System::Int8(0x19);
static const System::Int8 DXGI_FORMAT_R11G11B10_FLOAT = System::Int8(0x1a);
static const System::Int8 DXGI_FORMAT_R8G8B8A8_TYPELESS = System::Int8(0x1b);
static const System::Int8 DXGI_FORMAT_R8G8B8A8_UNORM = System::Int8(0x1c);
static const System::Int8 DXGI_FORMAT_R8G8B8A8_UNORM_SRGB = System::Int8(0x1d);
static const System::Int8 DXGI_FORMAT_R8G8B8A8_UINT = System::Int8(0x1e);
static const System::Int8 DXGI_FORMAT_R8G8B8A8_SNORM = System::Int8(0x1f);
static const System::Int8 DXGI_FORMAT_R8G8B8A8_SINT = System::Int8(0x20);
static const System::Int8 DXGI_FORMAT_R16G16_TYPELESS = System::Int8(0x21);
static const System::Int8 DXGI_FORMAT_R16G16_FLOAT = System::Int8(0x22);
static const System::Int8 DXGI_FORMAT_R16G16_UNORM = System::Int8(0x23);
static const System::Int8 DXGI_FORMAT_R16G16_UINT = System::Int8(0x24);
static const System::Int8 DXGI_FORMAT_R16G16_SNORM = System::Int8(0x25);
static const System::Int8 DXGI_FORMAT_R16G16_SINT = System::Int8(0x26);
static const System::Int8 DXGI_FORMAT_R32_TYPELESS = System::Int8(0x27);
static const System::Int8 DXGI_FORMAT_D32_FLOAT = System::Int8(0x28);
static const System::Int8 DXGI_FORMAT_R32_FLOAT = System::Int8(0x29);
static const System::Int8 DXGI_FORMAT_R32_UINT = System::Int8(0x2a);
static const System::Int8 DXGI_FORMAT_R32_SINT = System::Int8(0x2b);
static const System::Int8 DXGI_FORMAT_R24G8_TYPELESS = System::Int8(0x2c);
static const System::Int8 DXGI_FORMAT_D24_UNORM_S8_UINT = System::Int8(0x2d);
static const System::Int8 DXGI_FORMAT_R24_UNORM_X8_TYPELESS = System::Int8(0x2e);
static const System::Int8 DXGI_FORMAT_X24_TYPELESS_G8_UINT = System::Int8(0x2f);
static const System::Int8 DXGI_FORMAT_R8G8_TYPELESS = System::Int8(0x30);
static const System::Int8 DXGI_FORMAT_R8G8_UNORM = System::Int8(0x31);
static const System::Int8 DXGI_FORMAT_R8G8_UINT = System::Int8(0x32);
static const System::Int8 DXGI_FORMAT_R8G8_SNORM = System::Int8(0x33);
static const System::Int8 DXGI_FORMAT_R8G8_SINT = System::Int8(0x34);
static const System::Int8 DXGI_FORMAT_R16_TYPELESS = System::Int8(0x35);
static const System::Int8 DXGI_FORMAT_R16_FLOAT = System::Int8(0x36);
static const System::Int8 DXGI_FORMAT_D16_UNORM = System::Int8(0x37);
static const System::Int8 DXGI_FORMAT_R16_UNORM = System::Int8(0x38);
static const System::Int8 DXGI_FORMAT_R16_UINT = System::Int8(0x39);
static const System::Int8 DXGI_FORMAT_R16_SNORM = System::Int8(0x3a);
static const System::Int8 DXGI_FORMAT_R16_SINT = System::Int8(0x3b);
static const System::Int8 DXGI_FORMAT_R8_TYPELESS = System::Int8(0x3c);
static const System::Int8 DXGI_FORMAT_R8_UNORM = System::Int8(0x3d);
static const System::Int8 DXGI_FORMAT_R8_UINT = System::Int8(0x3e);
static const System::Int8 DXGI_FORMAT_R8_SNORM = System::Int8(0x3f);
static const System::Int8 DXGI_FORMAT_R8_SINT = System::Int8(0x40);
static const System::Int8 DXGI_FORMAT_A8_UNORM = System::Int8(0x41);
static const System::Int8 DXGI_FORMAT_R1_UNORM = System::Int8(0x42);
static const System::Int8 DXGI_FORMAT_R9G9B9E5_SHAREDEXP = System::Int8(0x43);
static const System::Int8 DXGI_FORMAT_R8G8_B8G8_UNORM = System::Int8(0x44);
static const System::Int8 DXGI_FORMAT_G8R8_G8B8_UNORM = System::Int8(0x45);
static const System::Int8 DXGI_FORMAT_BC1_TYPELESS = System::Int8(0x46);
static const System::Int8 DXGI_FORMAT_BC1_UNORM = System::Int8(0x47);
static const System::Int8 DXGI_FORMAT_BC1_UNORM_SRGB = System::Int8(0x48);
static const System::Int8 DXGI_FORMAT_BC2_TYPELESS = System::Int8(0x49);
static const System::Int8 DXGI_FORMAT_BC2_UNORM = System::Int8(0x4a);
static const System::Int8 DXGI_FORMAT_BC2_UNORM_SRGB = System::Int8(0x4b);
static const System::Int8 DXGI_FORMAT_BC3_TYPELESS = System::Int8(0x4c);
static const System::Int8 DXGI_FORMAT_BC3_UNORM = System::Int8(0x4d);
static const System::Int8 DXGI_FORMAT_BC3_UNORM_SRGB = System::Int8(0x4e);
static const System::Int8 DXGI_FORMAT_BC4_TYPELESS = System::Int8(0x4f);
static const System::Int8 DXGI_FORMAT_BC4_UNORM = System::Int8(0x50);
static const System::Int8 DXGI_FORMAT_BC4_SNORM = System::Int8(0x51);
static const System::Int8 DXGI_FORMAT_BC5_TYPELESS = System::Int8(0x52);
static const System::Int8 DXGI_FORMAT_BC5_UNORM = System::Int8(0x53);
static const System::Int8 DXGI_FORMAT_BC5_SNORM = System::Int8(0x54);
static const System::Int8 DXGI_FORMAT_B5G6R5_UNORM = System::Int8(0x55);
static const System::Int8 DXGI_FORMAT_B5G5R5A1_UNORM = System::Int8(0x56);
static const System::Int8 DXGI_FORMAT_B8G8R8A8_UNORM = System::Int8(0x57);
static const System::Int8 DXGI_FORMAT_B8G8R8X8_UNORM = System::Int8(0x58);
static const System::Int8 DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM = System::Int8(0x59);
static const System::Int8 DXGI_FORMAT_B8G8R8A8_TYPELESS = System::Int8(0x5a);
static const System::Int8 DXGI_FORMAT_B8G8R8A8_UNORM_SRGB = System::Int8(0x5b);
static const System::Int8 DXGI_FORMAT_B8G8R8X8_TYPELESS = System::Int8(0x5c);
static const System::Int8 DXGI_FORMAT_B8G8R8X8_UNORM_SRGB = System::Int8(0x5d);
static const System::Int8 DXGI_FORMAT_BC6H_TYPELESS = System::Int8(0x5e);
static const System::Int8 DXGI_FORMAT_BC6H_UF16 = System::Int8(0x5f);
static const System::Int8 DXGI_FORMAT_BC6H_SF16 = System::Int8(0x60);
static const System::Int8 DXGI_FORMAT_BC7_TYPELESS = System::Int8(0x61);
static const System::Int8 DXGI_FORMAT_BC7_UNORM = System::Int8(0x62);
static const System::Int8 DXGI_FORMAT_BC7_UNORM_SRGB = System::Int8(0x63);
static const System::Int8 D3D11_RESOURCE_DIMENSION_UNKNOWN = System::Int8(0x0);
static const System::Int8 D3D11_RESOURCE_DIMENSION_BUFFER = System::Int8(0x1);
static const System::Int8 D3D11_RESOURCE_DIMENSION_TEXTURE1D = System::Int8(0x2);
static const System::Int8 D3D11_RESOURCE_DIMENSION_TEXTURE2D = System::Int8(0x3);
static const System::Int8 D3D11_RESOURCE_DIMENSION_TEXTURE3D = System::Int8(0x4);
static const System::Int8 FOURCC_UNKNOWN = System::Int8(0x0);
static const System::Int8 FOURCC_R8G8B8 = System::Int8(0x14);
static const System::Int8 FOURCC_A8R8G8B8 = System::Int8(0x15);
static const System::Int8 FOURCC_X8R8G8B8 = System::Int8(0x16);
static const System::Int8 FOURCC_R5G6B5 = System::Int8(0x17);
static const System::Int8 FOURCC_X1R5G5B5 = System::Int8(0x18);
static const System::Int8 FOURCC_A1R5G5B5 = System::Int8(0x19);
static const System::Int8 FOURCC_A4R4G4B4 = System::Int8(0x1a);
static const System::Int8 FOURCC_R3G3B2 = System::Int8(0x1b);
static const System::Int8 FOURCC_A8 = System::Int8(0x1c);
static const System::Int8 FOURCC_A8R3G3B2 = System::Int8(0x1d);
static const System::Int8 FOURCC_X4R4G4B4 = System::Int8(0x1e);
static const System::Int8 FOURCC_A2B10G10R10 = System::Int8(0x1f);
static const System::Int8 FOURCC_A8B8G8R8 = System::Int8(0x20);
static const System::Int8 FOURCC_X8B8G8R8 = System::Int8(0x21);
static const System::Int8 FOURCC_G16R16 = System::Int8(0x22);
static const System::Int8 FOURCC_A2R10G10B10 = System::Int8(0x23);
static const System::Int8 FOURCC_A16B16G16R16 = System::Int8(0x24);
static const System::Int8 FOURCC_L8 = System::Int8(0x32);
static const System::Int8 FOURCC_A8L8 = System::Int8(0x33);
static const System::Int8 FOURCC_A4L4 = System::Int8(0x34);
static const int FOURCC_DXT1 = int(0x31545844);
static const int FOURCC_DXT2 = int(0x32545844);
static const int FOURCC_DXT3 = int(0x33545844);
static const int FOURCC_DXT4 = int(0x34545844);
static const int FOURCC_DXT5 = int(0x35545844);
static const int FOURCC_ATI1 = int(0x31495441);
static const int FOURCC_ATI2 = int(0x32495441);
static const System::Int8 FOURCC_D16_LOCKABLE = System::Int8(0x46);
static const System::Int8 FOURCC_D32 = System::Int8(0x47);
static const System::Int8 FOURCC_D24X8 = System::Int8(0x4d);
static const System::Int8 FOURCC_D16 = System::Int8(0x50);
static const System::Int8 FOURCC_D32F_LOCKABLE = System::Int8(0x52);
static const System::Int8 FOURCC_L16 = System::Int8(0x51);
static const System::Int8 FOURCC_R16F = System::Int8(0x6f);
static const System::Int8 FOURCC_G16R16F = System::Int8(0x70);
static const System::Int8 FOURCC_A16B16G16R16F = System::Int8(0x71);
static const System::Int8 FOURCC_R32F = System::Int8(0x72);
static const System::Int8 FOURCC_G32R32F = System::Int8(0x73);
static const System::Int8 FOURCC_A32B32G32R32F = System::Int8(0x74);
static const int FOURCC_DX10 = int(0x47495844);
extern DELPHI_PACKAGE System::StaticArray<TGLImageDataFormat, 4> cImageDataFormat8bits;
extern DELPHI_PACKAGE System::StaticArray<TGLImageDataFormat, 5> cImageDataFormat16bits;
extern DELPHI_PACKAGE System::StaticArray<TGLImageDataFormat, 1> cImageDataFormat24bits;
extern DELPHI_PACKAGE System::StaticArray<TGLImageDataFormat, 7> cImageDataFormat32bits;
extern DELPHI_PACKAGE void __fastcall DecodeDXT1toBitmap32(System::Sysutils::PByteArray encData, System::Sysutils::PByteArray decData, int w, int h, bool &trans);
extern DELPHI_PACKAGE void __fastcall DecodeDXT3toBitmap32(System::Sysutils::PByteArray encData, System::Sysutils::PByteArray decData, int w, int h);
extern DELPHI_PACKAGE void __fastcall DecodeDXT5toBitmap32(System::Sysutils::PByteArray encData, System::Sysutils::PByteArray decData, int w, int h);
extern DELPHI_PACKAGE void __fastcall flip_blocks_dxtc1(Winapi::Opengl::PGLubyte data, int numBlocks);
extern DELPHI_PACKAGE void __fastcall flip_blocks_dxtc3(Winapi::Opengl::PGLubyte data, int numBlocks);
extern DELPHI_PACKAGE void __fastcall flip_dxt5_alpha(PDXT5AlphaBlock block);
extern DELPHI_PACKAGE void __fastcall flip_blocks_dxtc5(Winapi::Opengl::PGLubyte data, int numBlocks);
extern DELPHI_PACKAGE bool __fastcall DDSHeaderToGLEnum(const TDDSHeader &DX9header, const TDDS_HEADER_DXT10 &DX11header, const bool useDX11, /* out */ Gls::Textureformat::TGLInternalFormat &iFormat, /* out */ unsigned &colorFormat, /* out */ unsigned &dataType, /* out */ int &bpe);
extern DELPHI_PACKAGE bool __fastcall GLEnumToDDSHeader(TDDSHeader &DX9header, TDDS_HEADER_DXT10 &DX11header, const bool useDX11, const Gls::Textureformat::TGLInternalFormat iFormat, const unsigned colorFormat, const unsigned dataType, const int bpe);
extern DELPHI_PACKAGE bool __fastcall FindDDSCompatibleDataFormat(const Gls::Textureformat::TGLInternalFormat iFormat, /* out */ unsigned &colorFormat, /* out */ unsigned &dataType);
}	/* namespace Dxtc */
}	/* namespace Formats */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS_DXTC)
using namespace Formats::Dxtc;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS)
using namespace Formats;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Formats_DxtcHPP
