// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.TextureFormat.pas' rev: 35.00 (Windows)

#ifndef Gls_TextureformatHPP
#define Gls_TextureformatHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.OpenGLext.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Strings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Textureformat
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLSeparateTextureWrap : unsigned char { twRepeat, twClampToEdge, twClampToBorder, twMirrorRepeat, twMirrorClampToEdge, twMirrorClampToBorder };

enum DECLSPEC_DENUM TGLTextureCompareMode : unsigned char { tcmNone, tcmCompareRtoTexture };

enum DECLSPEC_DENUM TGLTextureFilteringQuality : unsigned char { tfIsotropic, tfAnisotropic };

enum DECLSPEC_DENUM TGLTextureTarget : unsigned char { ttNoShape, ttTexture1D, ttTexture2D, ttTexture3D, ttTexture1DArray, ttTexture2DArray, ttTextureRect, ttTextureBuffer, ttTextureCube, ttTexture2DMultisample, ttTexture2DMultisampleArray, ttTextureCubeArray };

enum DECLSPEC_DENUM TGLTextureSwizzle : unsigned char { tswRed, tswGreen, tswBlue, tswAlpha, tswZero, tswOne };

typedef System::StaticArray<TGLTextureSwizzle, 4> TSwizzleVector;

enum DECLSPEC_DENUM TGLInternalFormat : unsigned char { tfALPHA4, tfALPHA8, tfALPHA12, tfALPHA16, tfDEPTH_COMPONENT16, tfDEPTH_COMPONENT24, tfDEPTH_COMPONENT32, tfLUMINANCE4, tfLUMINANCE8, tfLUMINANCE12, tfLUMINANCE16, tfLUMINANCE4_ALPHA4, tfLUMINANCE6_ALPHA2, tfLUMINANCE8_ALPHA8, tfLUMINANCE12_ALPHA4, tfLUMINANCE12_ALPHA12, tfLUMINANCE16_ALPHA16, tfINTENSITY4, tfINTENSITY8, tfINTENSITY12, tfINTENSITY16, tfR3_G3_B2, tfRGB4, tfRGB5, tfRGB8, tfRGB10, tfRGB12, tfR16G16B16, tfRGBA2, tfRGBA4, tfRGB5_A1, tfRGBA8, tfRGB10_A2, tfRGBA12, tfR16G16B16A16, tfCOMPRESSED_RGB_S3TC_DXT1, tfCOMPRESSED_RGBA_S3TC_DXT1, tfCOMPRESSED_RGBA_S3TC_DXT3, tfCOMPRESSED_RGBA_S3TC_DXT5, tfSIGNED_LUMINANCE8, tfSIGNED_LUMINANCE8_ALPHA8, tfSIGNED_RGB8, tfSIGNED_RGBA8, 
	tfSIGNED_RGB8_UNSIGNED_ALPHA8, tfSIGNED_ALPHA8, tfSIGNED_INTENSITY8, tfHILO16, tfSIGNED_HILO16, tfDSDT8, tfDSDT8_MAG8, tfDSDT8_MAG8_INTENSITY8, tfHILO8, tfSIGNED_HILO8, tfFLOAT_R16, tfFLOAT_R32, tfFLOAT_RG16, tfFLOAT_RGB16, tfFLOAT_RGBA16, tfFLOAT_RG32, tfFLOAT_RGB32, tfFLOAT_RGBA32, tfRGBA_FLOAT32, tfRGB_FLOAT32, tfALPHA_FLOAT32, tfINTENSITY_FLOAT32, tfLUMINANCE_FLOAT32, tfLUMINANCE_ALPHA_FLOAT32, tfRGBA_FLOAT16, tfRGB_FLOAT16, tfALPHA_FLOAT16, tfINTENSITY_FLOAT16, tfLUMINANCE_FLOAT16, tfLUMINANCE_ALPHA_FLOAT16, tfDEPTH24_STENCIL8, tfDEPTH_COMPONENT32F, tfDEPTH32F_STENCIL8, tfSRGB8, tfSRGB8_ALPHA8, tfSLUMINANCE8, tfSLUMINANCE8_ALPHA8, tfCOMPRESSED_SRGB_S3TC_DXT1, tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT1, tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT3, 
	tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5, tfRGB9_E5, tfR11F_G11F_B10F, tfCOMPRESSED_LUMINANCE_LATC1, tfCOMPRESSED_SIGNED_LUMINANCE_LATC1, tfCOMPRESSED_LUMINANCE_ALPHA_LATC2, tfCOMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2, tfCOMPRESSED_LUMINANCE_ALPHA_3DC, tfRGBA32UI, tfRGB32UI, tfALPHA32UI, tfINTENSITY32UI, tfLUMINANCE32UI, tfLUMINANCE_ALPHA32UI, tfRGBA16UI, tfRGB16UI, tfALPHA16UI, tfINTENSITY16UI, tfLUMINANCE16UI, tfLUMINANCE_ALPHA16UI, tfRGBA8UI, tfRGB8UI, tfALPHA8UI, tfINTENSITY8UI, tfLUMINANCE8UI, tfLUMINANCE_ALPHA8UI, tfRGBA32I, tfRGB32I, tfALPHA32I, tfINTENSITY32I, tfLUMINANCE32I, tfLUMINANCE_ALPHA32I, tfRGBA16I, tfRGB16I, tfALPHA16I, tfINTENSITY16I, tfLUMINANCE16I, tfLUMINANCE_ALPHA16I, tfRGBA8I, tfRGB8I, tfALPHA8I, tfINTENSITY8I, tfLUMINANCE8I, 
	tfLUMINANCE_ALPHA8I, tfRG32UI, tfR32UI, tfRG16UI, tfR16UI, tfRG8UI, tfR8UI, tfRG32I, tfR32I, tfRG16I, tfR16I, tfRG8I, tfR8I, tfRG8, tfR8, tfRG16, tfR16, tfRG16F, tfR16F, tfRG32F, tfR32F, tfCOMPRESSED_RED_RGTC1, tfCOMPRESSED_SIGNED_RED_RGTC1, tfCOMPRESSED_RG_RGTC2, tfCOMPRESSED_SIGNED_RG_RGTC2, tfR8_SNORM, tfRG8_SNORM, tfRGB8_SNORM, tfRGBA8_SNORM, tfR16_SNORM, tfRG16_SNORM, tfRGB16_SNORM, tfRGBA16_SNORM };

enum DECLSPEC_DENUM TGLInternalCompression : unsigned char { tcDefault, tcNone, tcStandard, tcHighQuality, tcHighSpeed };

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLInternalFormat vDefaultTextureFormat;
extern DELPHI_PACKAGE TGLInternalCompression vDefaultTextureCompression;
extern DELPHI_PACKAGE TSwizzleVector cDefaultSwizzleVector;
extern DELPHI_PACKAGE unsigned __fastcall InternalFormatToOpenGLFormat(TGLInternalFormat intFormat);
extern DELPHI_PACKAGE TGLInternalFormat __fastcall OpenGLFormatToInternalFormat(unsigned glFormat);
extern DELPHI_PACKAGE int __fastcall GetTextureElementSize(TGLInternalFormat intFormat)/* overload */;
extern DELPHI_PACKAGE int __fastcall GetTextureElementSize(unsigned colorFormat, unsigned dataType)/* overload */;
extern DELPHI_PACKAGE int __fastcall CompressedInternalFormatToOpenGL(TGLInternalFormat intFormat);
extern DELPHI_PACKAGE void __fastcall FindCompatibleDataFormat(TGLInternalFormat intFormat, /* out */ unsigned &dFormat, /* out */ unsigned &dType);
extern DELPHI_PACKAGE bool __fastcall IsTargetSupported(TGLTextureTarget target)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsTargetSupported(unsigned glTarget)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsFormatSupported(TGLInternalFormat intFormat);
extern DELPHI_PACKAGE bool __fastcall IsFloatFormat(TGLInternalFormat intFormat)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsFloatFormat(unsigned glFormat)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsDepthFormat(TGLInternalFormat intFormat)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsDepthFormat(unsigned glFormat)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsCompressedFormat(TGLInternalFormat intFormat)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsCompressedFormat(unsigned glFormat)/* overload */;
extern DELPHI_PACKAGE bool __fastcall GetGenericCompressedFormat(const TGLInternalFormat intFormat, const unsigned colorFormat, /* out */ unsigned &internalFormat);
extern DELPHI_PACKAGE bool __fastcall GetUncompressedFormat(const TGLInternalFormat intFormat, /* out */ TGLInternalFormat &internalFormat, /* out */ unsigned &colorFormat);
extern DELPHI_PACKAGE unsigned __fastcall DecodeTextureTarget(const TGLTextureTarget TextureTarget);
extern DELPHI_PACKAGE TGLTextureTarget __fastcall EncodeGLTextureTarget(const unsigned glTarget);
extern DELPHI_PACKAGE bool __fastcall IsTargetSupportMipmap(const TGLTextureTarget TextureTarget)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsTargetSupportMipmap(const unsigned glTarget)/* overload */;
}	/* namespace Textureformat */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_TEXTUREFORMAT)
using namespace Gls::Textureformat;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_TextureformatHPP
