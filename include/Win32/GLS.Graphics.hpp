// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Graphics.pas' rev: 35.00 (Windows)

#ifndef Gls_GraphicsHPP
#define Gls_GraphicsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.OpenGLext.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <System.Math.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Imaging.pngimage.hpp>
#include <Vcl.Consts.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.State.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.Context.hpp>
#include <GLS.ImageUtils.hpp>
#include <GLS.Color.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Utils.hpp>
#include <GLS.Strings.hpp>
#include <GLS.Logger.hpp>
#include <GLS.BaseClasses.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Graphics
{
//-- forward type declarations -----------------------------------------------
struct TGLPixel24;
struct TGLPixel32;
struct TGLImageLevelDesc;
class DELPHICLASS TGLBaseImage;
class DELPHICLASS TGLImage;
class DELPHICLASS TGLRasterFileFormat;
class DELPHICLASS TGLRasterFileFormatsList;
class DELPHICLASS EInvalidRasterFile;
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLPixel24
{
public:
	System::Byte r;
	System::Byte g;
	System::Byte b;
};
#pragma pack(pop)


typedef TGLPixel24 *PGLPixel24;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLPixel32
{
public:
	System::Byte r;
	System::Byte g;
	System::Byte b;
	System::Byte a;
};
#pragma pack(pop)


typedef TGLPixel32 *PGLPixel32;

typedef System::StaticArray<TGLPixel32, 268435456> TGLPixel32Array;

typedef TGLPixel32Array *PGLPixel32Array;

enum DECLSPEC_DENUM TGLLODStreamingState : unsigned char { ssKeeping, ssLoading, ssLoaded, ssTransfered };

struct DECLSPEC_DRECORD TGLImageLevelDesc
{
public:
	int Width;
	int Height;
	int Depth;
	Gls::Context::TGLUnpackPBOHandle* PBO;
	void *MapAddress;
	unsigned Offset;
	unsigned StreamOffset;
	unsigned Size;
	TGLLODStreamingState State;
};


typedef System::Int8 TGLImageLODRange;

typedef System::StaticArray<TGLImageLevelDesc, 16> TGLImagePiramid;

class PASCALIMPLEMENTATION TGLBaseImage : public Gls::Applicationfileio::TGLDataFile
{
	typedef Gls::Applicationfileio::TGLDataFile inherited;
	
private:
	System::Classes::TStream* FSourceStream;
	TGLImageLODRange FStreamLevel;
	Gls::Context::TFinishTaskEvent* FFinishEvent;
	
protected:
	TGLPixel32Array *fData;
	TGLImagePiramid FLOD;
	TGLImageLODRange fLevelCount;
	unsigned fColorFormat;
	Gls::Textureformat::TGLInternalFormat fInternalFormat;
	unsigned fDataType;
	int fElementSize;
	bool fCubeMap;
	bool fTextureArray;
	virtual PGLPixel32Array __fastcall GetData();
	int __fastcall GetWidth();
	int __fastcall GetHeight();
	int __fastcall GetDepth();
	void * __fastcall GetLevelAddress(System::Byte ALevel)/* overload */;
	void * __fastcall GetLevelAddress(System::Byte ALevel, System::Byte AFace)/* overload */;
	int __fastcall GetLevelWidth(TGLImageLODRange ALOD);
	int __fastcall GetLevelHeight(TGLImageLODRange ALOD);
	int __fastcall GetLevelDepth(TGLImageLODRange ALOD);
	Gls::Context::TGLUnpackPBOHandle* __fastcall GetLevelPBO(TGLImageLODRange ALOD);
	int __fastcall GetLevelOffset(TGLImageLODRange ALOD);
	int __fastcall GetLevelSizeInByte(TGLImageLODRange ALOD);
	TGLLODStreamingState __fastcall GetLevelStreamingState(TGLImageLODRange ALOD);
	void __fastcall SetLevelStreamingState(TGLImageLODRange ALOD, TGLLODStreamingState AState);
	void __fastcall SaveHeader();
	void __fastcall LoadHeader();
	void __fastcall StartStreaming();
	void __fastcall DoStreaming();
	
public:
	__fastcall virtual TGLBaseImage();
	__fastcall virtual ~TGLBaseImage();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	virtual void __fastcall RegisterAsOpenGLTexture(Gls::Context::TGLTextureHandle* AHandle, bool aMipmapGen, unsigned aTexFormat, /* out */ int &texWidth, /* out */ int &texHeight, /* out */ int &texDepth);
	virtual bool __fastcall AssignFromTexture(Gls::Context::TGLTextureHandle* AHandle, const bool CastToFormat, const Gls::Textureformat::TGLInternalFormat intFormat = (Gls::Textureformat::TGLInternalFormat)(0x1f), const unsigned colorFormat = (unsigned)(0x0), const unsigned dataType = (unsigned)(0x0));
	bool __fastcall ConvertCrossToCubeMap();
	bool __fastcall ConvertToVolume(const int col, const int row, const bool MakeArray);
	unsigned __fastcall DataSize();
	bool __fastcall IsEmpty();
	bool __fastcall IsCompressed();
	bool __fastcall IsVolume();
	void __fastcall Narrow();
	virtual void __fastcall GenerateMipmap(Gls::Imageutils::TImageFilterFunction AFilter);
	virtual void __fastcall UnMipmap();
	__property PGLPixel32Array Data = {read=GetData};
	void __fastcall SetErrorImage();
	void __fastcall UpdateLevelsInfo();
	__property int LevelWidth[TGLImageLODRange ALOD] = {read=GetLevelWidth};
	__property int LevelHeight[TGLImageLODRange ALOD] = {read=GetLevelHeight};
	__property int LevelDepth[TGLImageLODRange ALOD] = {read=GetLevelDepth};
	__property Gls::Context::TGLUnpackPBOHandle* LevelPixelBuffer[TGLImageLODRange ALOD] = {read=GetLevelPBO};
	__property int LevelOffset[TGLImageLODRange ALOD] = {read=GetLevelOffset};
	__property int LevelSizeInByte[TGLImageLODRange ALOD] = {read=GetLevelSizeInByte};
	__property TGLLODStreamingState LevelStreamingState[TGLImageLODRange ALOD] = {read=GetLevelStreamingState, write=SetLevelStreamingState};
	__property TGLImageLODRange LevelCount = {read=fLevelCount, nodefault};
	__property Gls::Textureformat::TGLInternalFormat InternalFormat = {read=fInternalFormat, nodefault};
	__property unsigned ColorFormat = {read=fColorFormat, nodefault};
	__property unsigned DataType = {read=fDataType, nodefault};
	__property int ElementSize = {read=fElementSize, nodefault};
	__property bool CubeMap = {read=fCubeMap, nodefault};
	__property bool TextureArray = {read=fTextureArray, nodefault};
};


typedef System::TMetaClass* TGLBaseImageClass;

class PASCALIMPLEMENTATION TGLImage : public TGLBaseImage
{
	typedef TGLBaseImage inherited;
	
private:
	bool FVerticalReverseOnAssignFromBitmap;
	bool FBlank;
	unsigned fOldColorFormat;
	unsigned fOldDataType;
	void __fastcall DataConvertTask();
	
protected:
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(const int val);
	void __fastcall SetDepth(const int val);
	void __fastcall SetBlank(const bool Value);
	void __fastcall SetCubeMap(const bool val);
	void __fastcall SetArray(const bool val);
	PGLPixel32Array __fastcall GetScanLine(int index);
	void __fastcall AssignFrom24BitsBitmap(Vcl::Graphics::TBitmap* aBitmap);
	void __fastcall AssignFrom32BitsBitmap(Vcl::Graphics::TBitmap* aBitmap);
	void __fastcall AssignFromPngImage(Vcl::Imaging::Pngimage::TPngImage* aPngImage);
	
public:
	__fastcall virtual TGLImage();
	__fastcall virtual ~TGLImage();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall AssignFromBitmap24WithoutRGBSwap(Vcl::Graphics::TBitmap* aBitmap);
	void __fastcall AssignFromTexture2D(unsigned textureHandle)/* overload */;
	void __fastcall AssignFromTexture2D(Gls::Context::TGLTextureHandle* textureHandle)/* overload */;
	Vcl::Graphics::TBitmap* __fastcall Create32BitsBitmap();
	__property int Width = {read=GetWidth, write=SetWidth, nodefault};
	__property int Height = {read=GetHeight, write=SetHeight, nodefault};
	__property int Depth = {read=GetDepth, write=SetDepth, nodefault};
	__property unsigned ColorFormat = {read=fColorFormat, nodefault};
	__property Gls::Textureformat::TGLInternalFormat InternalFormat = {read=fInternalFormat, write=fInternalFormat, nodefault};
	__property unsigned DataType = {read=fDataType, nodefault};
	__property int ElementSize = {read=fElementSize, nodefault};
	__property bool CubeMap = {read=fCubeMap, write=SetCubeMap, nodefault};
	__property bool TextureArray = {read=fTextureArray, write=SetArray, nodefault};
	__property PGLPixel32Array ScanLine[int index] = {read=GetScanLine};
	__property bool VerticalReverseOnAssignFromBitmap = {read=FVerticalReverseOnAssignFromBitmap, write=FVerticalReverseOnAssignFromBitmap, nodefault};
	__property bool Blank = {read=FBlank, write=SetBlank, nodefault};
	void __fastcall SetColorFormatDataType(const unsigned AColorFormat, const unsigned ADataType);
	void __fastcall SetAlphaFromIntensity();
	void __fastcall SetAlphaTransparentForColor(const System::Uitypes::TColor aColor)/* overload */;
	void __fastcall SetAlphaTransparentForColor(const TGLPixel32 aColor)/* overload */;
	void __fastcall SetAlphaTransparentForColor(const TGLPixel24 aColor)/* overload */;
	void __fastcall SetAlphaToValue(const System::Byte aValue);
	void __fastcall SetAlphaToFloatValue(const float aValue);
	void __fastcall InvertAlpha();
	void __fastcall SqrtAlpha();
	void __fastcall BrightnessCorrection(const float factor);
	void __fastcall GammaCorrection(const float gamma);
	void __fastcall DownSampleByFactor2();
	void __fastcall ReadPixels(const System::Types::TRect &area);
	void __fastcall DrawPixels(const float x, const float y);
	void __fastcall GrayScaleToNormalMap(const float scale, bool wrapX = true, bool wrapY = true);
	void __fastcall NormalizeNormalMap();
	void __fastcall AssignToBitmap(Vcl::Graphics::TBitmap* aBitmap);
	virtual void __fastcall GenerateMipmap(Gls::Imageutils::TImageFilterFunction AFilter);
	virtual void __fastcall UnMipmap();
};


typedef TGLImage TGLBitmap32;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLRasterFileFormat : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TGLBaseImageClass BaseImageClass;
	System::UnicodeString Extension;
	System::UnicodeString Description;
	int DescResID;
public:
	/* TObject.Create */ inline __fastcall TGLRasterFileFormat() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLRasterFileFormat() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLRasterFileFormatsList : public Gls::Persistentclasses::TGLPersistentObjectList
{
	typedef Gls::Persistentclasses::TGLPersistentObjectList inherited;
	
public:
	__fastcall virtual ~TGLRasterFileFormatsList();
	HIDESBASE void __fastcall Add(const System::UnicodeString Ext, const System::UnicodeString Desc, int DescID, TGLBaseImageClass AClass);
	TGLBaseImageClass __fastcall FindExt(System::UnicodeString ext);
	TGLBaseImageClass __fastcall FindFromFileName(const System::UnicodeString fileName);
	TGLBaseImageClass __fastcall FindFromStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall Remove(TGLBaseImageClass AClass);
	void __fastcall BuildFilterStrings(TGLBaseImageClass imageFileClass, System::UnicodeString &descriptions, System::UnicodeString &filters, bool formatsThatCanBeOpened = true, bool formatsThatCanBeSaved = false);
	System::UnicodeString __fastcall FindExtByIndex(int index, bool formatsThatCanBeOpened = true, bool formatsThatCanBeSaved = false);
public:
	/* TGLPersistentObjectList.Create */ inline __fastcall virtual TGLRasterFileFormatsList() : Gls::Persistentclasses::TGLPersistentObjectList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLRasterFileFormatsList(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Persistentclasses::TGLPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EInvalidRasterFile : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EInvalidRasterFile(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EInvalidRasterFile(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidRasterFile(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidRasterFile(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidRasterFile(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidRasterFile(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EInvalidRasterFile(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidRasterFile(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidRasterFile(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidRasterFile(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidRasterFile(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidRasterFile(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EInvalidRasterFile() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool vVerticalFlipDDS;
extern DELPHI_PACKAGE TGLRasterFileFormatsList* __fastcall GetRasterFileFormats(void);
extern DELPHI_PACKAGE void __fastcall RegisterRasterFormat(const System::UnicodeString AExtension, const System::UnicodeString ADescription, TGLBaseImageClass AClass);
extern DELPHI_PACKAGE void __fastcall UnregisterRasterFormat(TGLBaseImageClass AClass);
extern DELPHI_PACKAGE System::UnicodeString __fastcall RasterFileFormatExtensionByIndex(int index);
extern DELPHI_PACKAGE void __fastcall Div2(int &Value);
extern DELPHI_PACKAGE int __fastcall GetImageLodNumber(int w, int h, int d, bool IsVolume);
extern DELPHI_PACKAGE void __fastcall GammaCorrectRGBArray(void * base, int pixelCount, float gamma);
extern DELPHI_PACKAGE void __fastcall BrightenRGBArray(void * base, int pixelCount, float factor);
extern DELPHI_PACKAGE void __fastcall BGR24ToRGB24(void * src, void * dest, int pixelCount);
extern DELPHI_PACKAGE void __fastcall BGR24ToRGBA32(void * src, void * dest, int pixelCount);
extern DELPHI_PACKAGE void __fastcall RGB24ToRGBA32(void * src, void * dest, int pixelCount);
extern DELPHI_PACKAGE void __fastcall BGRA32ToRGBA32(void * src, void * dest, int pixelCount);
extern DELPHI_PACKAGE Vcl::Graphics::TGraphicClass __fastcall GraphicClassForExtension(const System::UnicodeString anExtension);
extern DELPHI_PACKAGE void __fastcall HackTPictureRegisteredFormats(System::Classes::TStrings* destList);
}	/* namespace Graphics */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_GRAPHICS)
using namespace Gls::Graphics;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_GraphicsHPP
