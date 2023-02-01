// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Texture.pas' rev: 35.00 (Windows)

#ifndef Gls_TextureHPP
#define Gls_TextureHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.OpenGLext.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include <Vcl.Imaging.pngimage.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.Context.hpp>
#include <GLS.State.hpp>
#include <GLS.Color.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.Utils.hpp>
#include <GLS.Strings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Texture
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE IGLTextureNotifyAble;
typedef System::DelphiInterface<IGLTextureNotifyAble> _di_IGLTextureNotifyAble;
class DELPHICLASS TGLTextureImage;
class DELPHICLASS TGLBlankImage;
class DELPHICLASS TGLPictureImage;
class DELPHICLASS TGLPersistentImage;
class DELPHICLASS TGLPicFileImage;
class DELPHICLASS TGLCubeMapImage;
class DELPHICLASS TGLTexture;
class DELPHICLASS TGLTextureExItem;
class DELPHICLASS TGLTextureEx;
class DELPHICLASS ETexture;
class DELPHICLASS EGLShaderException;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLTextureMode : unsigned char { tmDecal, tmModulate, tmBlend, tmReplace, tmAdd };

enum DECLSPEC_DENUM TGLTextureWrap : unsigned char { twBoth, twNone, twVertical, twHorizontal, twSeparate };

enum DECLSPEC_DENUM TGLMinFilter : unsigned char { miNearest, miLinear, miNearestMipmapNearest, miLinearMipmapNearest, miNearestMipmapLinear, miLinearMipmapLinear };

enum DECLSPEC_DENUM TGLMagFilter : unsigned char { maNearest, maLinear };

enum DECLSPEC_DENUM TGLDepthTextureMode : unsigned char { dtmLuminance, dtmIntensity, dtmAlpha };

typedef Gls::State::TGLComparisonFunction TGLDepthCompareFunc;

enum DECLSPEC_DENUM TGLTextureFormat : unsigned char { tfDefault, tfRGB, tfRGBA, tfRGB16, tfRGBA16, tfAlpha, tfLuminance, tfLuminanceAlpha, tfIntensity, tfNormalMap, tfRGBAFloat16, tfRGBAFloat32, tfExtended };

typedef Gls::Textureformat::TGLInternalCompression TGLTextureCompression;

__interface  INTERFACE_UUID("{0D9DC0B0-ECE4-4513-A8A1-5AE7022C9426}") IGLTextureNotifyAble  : public Gls::Baseclasses::IGLNotifyAble 
{
	virtual void __fastcall NotifyTexMapChange(System::TObject* Sender) = 0 ;
};

typedef void __fastcall (__closure *TGLTextureNeededEvent)(System::TObject* Sender, System::UnicodeString &textureFileName);

enum DECLSPEC_DENUM TGLTextureChange : unsigned char { tcImage, tcParams };

typedef System::Set<TGLTextureChange, TGLTextureChange::tcImage, TGLTextureChange::tcParams> TGLTextureChanges;

enum DECLSPEC_DENUM TGLTextureImageAlpha : unsigned char { tiaDefault, tiaAlphaFromIntensity, tiaSuperBlackTransparent, tiaLuminance, tiaLuminanceSqrt, tiaOpaque, tiaTopLeftPointColorTransparent, tiaInverseLuminance, tiaInverseLuminanceSqrt, tiaBottomRightPointColorTransparent };

class PASCALIMPLEMENTATION TGLTextureImage : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	System::UnicodeString __fastcall GetResourceName();
	
protected:
	TGLTexture* FOwnerTexture;
	TGLTextureNeededEvent FOnTextureNeeded;
	System::UnicodeString FResourceFile;
	__classmethod virtual bool __fastcall IsSelfLoading();
	virtual void __fastcall LoadTexture(Gls::Textureformat::TGLInternalFormat AInternalFormat);
	virtual Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	virtual int __fastcall GetHeight();
	virtual int __fastcall GetWidth();
	virtual int __fastcall GetDepth();
	__property TGLTextureNeededEvent OnTextureNeeded = {read=FOnTextureNeeded, write=FOnTextureNeeded};
	
public:
	__fastcall virtual TGLTextureImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLTextureImage();
	__property TGLTexture* OwnerTexture = {read=FOwnerTexture, write=FOwnerTexture};
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall SaveToFile(const System::UnicodeString fileName);
	virtual void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall Invalidate();
	virtual Gls::Graphics::TGLImage* __fastcall GetBitmap32();
	virtual void __fastcall ReleaseBitmap32();
	Vcl::Graphics::TBitmap* __fastcall AsBitmap();
	void __fastcall AssignToBitmap(Vcl::Graphics::TBitmap* aBitmap);
	__property int Width = {read=GetWidth, nodefault};
	__property int Height = {read=GetHeight, nodefault};
	__property int Depth = {read=GetDepth, nodefault};
	__property Gls::Textureformat::TGLTextureTarget NativeTextureTarget = {read=GetTextureTarget, nodefault};
	__property System::UnicodeString ResourceName = {read=GetResourceName};
};


typedef System::TMetaClass* TGLTextureImageClass;

class PASCALIMPLEMENTATION TGLBlankImage : public TGLTextureImage
{
	typedef TGLTextureImage inherited;
	
private:
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(int val);
	void __fastcall SetDepth(int val);
	void __fastcall SetCubeMap(const bool val);
	void __fastcall SetArray(const bool val);
	
protected:
	Gls::Graphics::TGLImage* fBitmap;
	int fWidth;
	int fHeight;
	int fDepth;
	unsigned fColorFormat;
	bool fCubeMap;
	bool fArray;
	virtual int __fastcall GetWidth();
	virtual int __fastcall GetHeight();
	virtual int __fastcall GetDepth();
	virtual Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	
public:
	__fastcall virtual TGLBlankImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLBlankImage();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Gls::Graphics::TGLImage* __fastcall GetBitmap32();
	virtual void __fastcall ReleaseBitmap32();
	virtual void __fastcall SaveToFile(const System::UnicodeString fileName);
	virtual void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	
__published:
	__property int Width = {read=GetWidth, write=SetWidth, default=256};
	__property int Height = {read=GetHeight, write=SetHeight, default=256};
	__property int Depth = {read=GetDepth, write=SetDepth, default=0};
	__property bool CubeMap = {read=fCubeMap, write=SetCubeMap, default=0};
	__property bool TextureArray = {read=fArray, write=SetArray, default=0};
	__property unsigned ColorFormat = {read=fColorFormat, write=fColorFormat, nodefault};
};


class PASCALIMPLEMENTATION TGLPictureImage : public TGLTextureImage
{
	typedef TGLTextureImage inherited;
	
private:
	Gls::Graphics::TGLImage* FBitmap;
	Vcl::Graphics::TPicture* FGLPicture;
	int FUpdateCounter;
	
protected:
	virtual int __fastcall GetHeight();
	virtual int __fastcall GetWidth();
	virtual int __fastcall GetDepth();
	virtual Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	Vcl::Graphics::TPicture* __fastcall GetPicture();
	void __fastcall SetPicture(Vcl::Graphics::TPicture* const aPicture);
	void __fastcall PictureChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TGLPictureImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLPictureImage();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	HIDESBASE void __fastcall BeginUpdate();
	HIDESBASE void __fastcall EndUpdate();
	virtual Gls::Graphics::TGLImage* __fastcall GetBitmap32();
	virtual void __fastcall ReleaseBitmap32();
	__property Vcl::Graphics::TPicture* Picture = {read=GetPicture, write=SetPicture};
};


class PASCALIMPLEMENTATION TGLPersistentImage : public TGLPictureImage
{
	typedef TGLPictureImage inherited;
	
public:
	__fastcall virtual TGLPersistentImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLPersistentImage();
	virtual void __fastcall SaveToFile(const System::UnicodeString fileName);
	virtual void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property NativeTextureTarget;
	
__published:
	__property Picture;
};


class PASCALIMPLEMENTATION TGLPicFileImage : public TGLPictureImage
{
	typedef TGLPictureImage inherited;
	
private:
	System::UnicodeString FPictureFileName;
	bool FAlreadyWarnedAboutMissingFile;
	int FWidth;
	int FHeight;
	
protected:
	void __fastcall SetPictureFileName(const System::UnicodeString val);
	virtual int __fastcall GetWidth();
	virtual int __fastcall GetHeight();
	virtual int __fastcall GetDepth();
	
public:
	__fastcall virtual TGLPicFileImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLPicFileImage();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall SaveToFile(const System::UnicodeString fileName);
	virtual void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property NativeTextureTarget;
	virtual Gls::Graphics::TGLImage* __fastcall GetBitmap32();
	virtual void __fastcall Invalidate();
	
__published:
	__property System::UnicodeString PictureFileName = {read=FPictureFileName, write=SetPictureFileName};
};


typedef int TGLCubeMapTarget;

class PASCALIMPLEMENTATION TGLCubeMapImage : public TGLTextureImage
{
	typedef TGLTextureImage inherited;
	
private:
	Gls::Graphics::TGLImage* FImage;
	int FUpdateCounter;
	System::StaticArray<Vcl::Graphics::TPicture*, 6> FPicture;
	
protected:
	virtual int __fastcall GetWidth();
	virtual int __fastcall GetHeight();
	virtual int __fastcall GetDepth();
	void __fastcall SetPicture(int index, Vcl::Graphics::TPicture* const val);
	Vcl::Graphics::TPicture* __fastcall GetPicture(int index);
	virtual Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	void __fastcall PictureChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TGLCubeMapImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLCubeMapImage();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Gls::Graphics::TGLImage* __fastcall GetBitmap32();
	virtual void __fastcall ReleaseBitmap32();
	HIDESBASE void __fastcall BeginUpdate();
	HIDESBASE void __fastcall EndUpdate();
	virtual void __fastcall SaveToFile(const System::UnicodeString fileName);
	virtual void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property NativeTextureTarget;
	__property Vcl::Graphics::TPicture* Picture[int index] = {read=GetPicture, write=SetPicture};
	
__published:
	__property Vcl::Graphics::TPicture* PicturePX = {read=GetPicture, write=SetPicture, index=0};
	__property Vcl::Graphics::TPicture* PictureNX = {read=GetPicture, write=SetPicture, index=1};
	__property Vcl::Graphics::TPicture* PicturePY = {read=GetPicture, write=SetPicture, index=2};
	__property Vcl::Graphics::TPicture* PictureNY = {read=GetPicture, write=SetPicture, index=3};
	__property Vcl::Graphics::TPicture* PicturePZ = {read=GetPicture, write=SetPicture, index=4};
	__property Vcl::Graphics::TPicture* PictureNZ = {read=GetPicture, write=SetPicture, index=5};
};


enum DECLSPEC_DENUM TGLTextureMappingMode : unsigned char { tmmUser, tmmObjectLinear, tmmEyeLinear, tmmSphere, tmmCubeMapReflection, tmmCubeMapNormal, tmmCubeMapLight0, tmmCubeMapCamera };

class PASCALIMPLEMENTATION TGLTexture : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Gls::Context::TGLTextureHandle* FTextureHandle;
	Gls::Context::TGLVirtualHandle* FSamplerHandle;
	Gls::Textureformat::TGLInternalFormat FTextureFormat;
	TGLTextureMode FTextureMode;
	TGLTextureWrap FTextureWrap;
	TGLMinFilter FMinFilter;
	TGLMagFilter FMagFilter;
	bool FDisabled;
	TGLTextureImage* FImage;
	TGLTextureImageAlpha FImageAlpha;
	float FImageBrightness;
	float FImageGamma;
	TGLTextureMappingMode FMappingMode;
	Gls::Coordinates::TGLCoordinates4* FMapSCoordinates;
	Gls::Coordinates::TGLCoordinates4* FMapTCoordinates;
	Gls::Coordinates::TGLCoordinates4* FMapRCoordinates;
	Gls::Coordinates::TGLCoordinates4* FMapQCoordinates;
	TGLTextureNeededEvent FOnTextureNeeded;
	Gls::Textureformat::TGLInternalCompression FCompression;
	int FRequiredMemorySize;
	Gls::Textureformat::TGLTextureFilteringQuality FFilteringQuality;
	int FTexWidth;
	int FTexHeight;
	int FTexDepth;
	Gls::Color::TGLColor* FEnvColor;
	Gls::Color::TGLColor* FBorderColor;
	float FNormalMapScale;
	Gls::Textureformat::TGLSeparateTextureWrap FTextureWrapS;
	Gls::Textureformat::TGLSeparateTextureWrap FTextureWrapT;
	Gls::Textureformat::TGLSeparateTextureWrap FTextureWrapR;
	Gls::Textureformat::TGLTextureCompareMode fTextureCompareMode;
	Gls::State::TGLComparisonFunction fTextureCompareFunc;
	TGLDepthTextureMode fDepthTextureMode;
	bool FKeepImageAfterTransfer;
	
protected:
	void __fastcall SetImage(TGLTextureImage* AValue);
	void __fastcall SetImageAlpha(const TGLTextureImageAlpha val);
	void __fastcall SetImageBrightness(const float val);
	bool __fastcall StoreBrightness();
	void __fastcall SetImageGamma(const float val);
	bool __fastcall StoreGamma();
	void __fastcall SetMagFilter(TGLMagFilter AValue);
	void __fastcall SetMinFilter(TGLMinFilter AValue);
	void __fastcall SetTextureMode(TGLTextureMode AValue);
	void __fastcall SetTextureWrap(TGLTextureWrap AValue);
	void __fastcall SetTextureWrapS(Gls::Textureformat::TGLSeparateTextureWrap AValue);
	void __fastcall SetTextureWrapT(Gls::Textureformat::TGLSeparateTextureWrap AValue);
	void __fastcall SetTextureWrapR(Gls::Textureformat::TGLSeparateTextureWrap AValue);
	TGLTextureFormat __fastcall GetTextureFormat();
	void __fastcall SetTextureFormat(const TGLTextureFormat val);
	void __fastcall SetTextureFormatEx(const Gls::Textureformat::TGLInternalFormat val);
	bool __fastcall StoreTextureFormatEx();
	void __fastcall SetCompression(const Gls::Textureformat::TGLInternalCompression val);
	void __fastcall SetFilteringQuality(const Gls::Textureformat::TGLTextureFilteringQuality val);
	void __fastcall SetMappingMode(const TGLTextureMappingMode val);
	Gls::Coordinates::TGLCoordinates4* __fastcall GetMappingSCoordinates();
	void __fastcall SetMappingSCoordinates(Gls::Coordinates::TGLCoordinates4* const val);
	bool __fastcall StoreMappingSCoordinates();
	Gls::Coordinates::TGLCoordinates4* __fastcall GetMappingTCoordinates();
	void __fastcall SetMappingTCoordinates(Gls::Coordinates::TGLCoordinates4* const val);
	bool __fastcall StoreMappingTCoordinates();
	Gls::Coordinates::TGLCoordinates4* __fastcall GetMappingRCoordinates();
	void __fastcall SetMappingRCoordinates(Gls::Coordinates::TGLCoordinates4* const val);
	bool __fastcall StoreMappingRCoordinates();
	Gls::Coordinates::TGLCoordinates4* __fastcall GetMappingQCoordinates();
	void __fastcall SetMappingQCoordinates(Gls::Coordinates::TGLCoordinates4* const val);
	bool __fastcall StoreMappingQCoordinates();
	void __fastcall SetDisabled(bool AValue);
	void __fastcall SetEnabled(const bool val);
	bool __fastcall GetEnabled();
	void __fastcall SetEnvColor(Gls::Color::TGLColor* const val);
	void __fastcall SetBorderColor(Gls::Color::TGLColor* const val);
	void __fastcall SetNormalMapScale(const float val);
	void __fastcall SetTextureCompareMode(const Gls::Textureformat::TGLTextureCompareMode val);
	void __fastcall SetTextureCompareFunc(const Gls::State::TGLComparisonFunction val);
	void __fastcall SetDepthTextureMode(const TGLDepthTextureMode val);
	bool __fastcall StoreNormalMapScale();
	bool __fastcall StoreImageClassName();
	unsigned __fastcall GetHandle();
	virtual void __fastcall PrepareImage(unsigned target);
	virtual void __fastcall PrepareParams(unsigned target);
	void __fastcall DoOnTextureNeeded(System::TObject* Sender, System::UnicodeString &textureFileName);
	void __fastcall OnSamplerAllocate(Gls::Context::TGLVirtualHandle* Sender, unsigned &Handle);
	void __fastcall OnSamplerDestroy(Gls::Context::TGLVirtualHandle* Sender, unsigned &Handle);
	void __fastcall SetTextureErrorImage();
	
public:
	__fastcall virtual TGLTexture(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLTexture();
	__property TGLTextureNeededEvent OnTextureNeeded = {read=FOnTextureNeeded, write=FOnTextureNeeded};
	void __fastcall PrepareBuildList();
	void __fastcall ApplyMappingMode();
	void __fastcall UnApplyMappingMode();
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall ApplyAsTexture2(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, Gls::Vectortypes::PGLMatrix textureMatrix = (Gls::Vectortypes::PGLMatrix)(0x0));
	void __fastcall UnApplyAsTexture2(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool reloadIdentityTextureMatrix);
	void __fastcall ApplyAsTextureN(int n, Gls::Rendercontextinfo::TGLRenderContextInfo &rci, Gls::Vectortypes::PGLMatrix textureMatrix = (Gls::Vectortypes::PGLMatrix)(0x0));
	void __fastcall UnApplyAsTextureN(int n, Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool reloadIdentityTextureMatrix);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall NotifyImageChange();
	void __fastcall NotifyParamsChange();
	void __fastcall DestroyHandles();
	void __fastcall SetImageClassName(const System::UnicodeString val);
	System::UnicodeString __fastcall GetImageClassName();
	int __fastcall TextureImageRequiredMemory();
	unsigned __fastcall AllocateHandle();
	bool __fastcall IsHandleAllocated();
	int __fastcall OpenGLTextureFormat();
	bool __fastcall IsFloatType();
	__property bool Enabled = {read=GetEnabled, write=SetEnabled, nodefault};
	__property unsigned Handle = {read=GetHandle, nodefault};
	__property Gls::Context::TGLTextureHandle* TextureHandle = {read=FTextureHandle};
	__property int TexWidth = {read=FTexWidth, nodefault};
	__property int TexHeight = {read=FTexHeight, nodefault};
	__property int TexDepth = {read=FTexDepth, nodefault};
	
__published:
	__property System::UnicodeString ImageClassName = {read=GetImageClassName, write=SetImageClassName, stored=StoreImageClassName};
	__property TGLTextureImage* Image = {read=FImage, write=SetImage};
	__property TGLTextureImageAlpha ImageAlpha = {read=FImageAlpha, write=SetImageAlpha, default=0};
	__property float ImageBrightness = {read=FImageBrightness, write=SetImageBrightness, stored=StoreBrightness};
	__property float ImageGamma = {read=FImageGamma, write=SetImageGamma, stored=StoreGamma};
	__property TGLMagFilter MagFilter = {read=FMagFilter, write=SetMagFilter, default=1};
	__property TGLMinFilter MinFilter = {read=FMinFilter, write=SetMinFilter, default=5};
	__property TGLTextureMode TextureMode = {read=FTextureMode, write=SetTextureMode, default=0};
	__property TGLTextureWrap TextureWrap = {read=FTextureWrap, write=SetTextureWrap, default=0};
	__property Gls::Textureformat::TGLSeparateTextureWrap TextureWrapS = {read=FTextureWrapS, write=SetTextureWrapS, default=0};
	__property Gls::Textureformat::TGLSeparateTextureWrap TextureWrapT = {read=FTextureWrapT, write=SetTextureWrapT, default=0};
	__property Gls::Textureformat::TGLSeparateTextureWrap TextureWrapR = {read=FTextureWrapR, write=SetTextureWrapR, default=0};
	__property TGLTextureFormat TextureFormat = {read=GetTextureFormat, write=SetTextureFormat, default=0};
	__property Gls::Textureformat::TGLInternalFormat TextureFormatEx = {read=FTextureFormat, write=SetTextureFormatEx, stored=StoreTextureFormatEx, nodefault};
	__property Gls::Textureformat::TGLInternalCompression Compression = {read=FCompression, write=SetCompression, default=0};
	__property Gls::Textureformat::TGLTextureFilteringQuality FilteringQuality = {read=FFilteringQuality, write=SetFilteringQuality, default=0};
	__property TGLTextureMappingMode MappingMode = {read=FMappingMode, write=SetMappingMode, default=0};
	__property Gls::Coordinates::TGLCoordinates4* MappingSCoordinates = {read=GetMappingSCoordinates, write=SetMappingSCoordinates, stored=StoreMappingSCoordinates};
	__property Gls::Coordinates::TGLCoordinates4* MappingTCoordinates = {read=GetMappingTCoordinates, write=SetMappingTCoordinates, stored=StoreMappingTCoordinates};
	__property Gls::Coordinates::TGLCoordinates4* MappingRCoordinates = {read=GetMappingRCoordinates, write=SetMappingRCoordinates, stored=StoreMappingRCoordinates};
	__property Gls::Coordinates::TGLCoordinates4* MappingQCoordinates = {read=GetMappingQCoordinates, write=SetMappingQCoordinates, stored=StoreMappingQCoordinates};
	__property Gls::Color::TGLColor* EnvColor = {read=FEnvColor, write=SetEnvColor};
	__property Gls::Color::TGLColor* BorderColor = {read=FBorderColor, write=SetBorderColor};
	__property bool Disabled = {read=FDisabled, write=SetDisabled, default=1};
	__property float NormalMapScale = {read=FNormalMapScale, write=SetNormalMapScale, stored=StoreNormalMapScale};
	__property Gls::Textureformat::TGLTextureCompareMode TextureCompareMode = {read=fTextureCompareMode, write=SetTextureCompareMode, default=0};
	__property Gls::State::TGLComparisonFunction TextureCompareFunc = {read=fTextureCompareFunc, write=SetTextureCompareFunc, default=3};
	__property TGLDepthTextureMode DepthTextureMode = {read=fDepthTextureMode, write=SetDepthTextureMode, default=0};
	__property bool KeepImageAfterTransfer = {read=FKeepImageAfterTransfer, write=FKeepImageAfterTransfer, default=0};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureExItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLTexture* FTexture;
	int FTextureIndex;
	Gls::Coordinates::TGLCoordinates3* FTextureOffset;
	Gls::Coordinates::TGLCoordinates3* FTextureScale;
	bool FTextureMatrixIsIdentity;
	Gls::Vectortypes::TMatrix4f FTextureMatrix;
	bool FApplied;
	HRESULT __stdcall QueryInterface(const GUID &IID, /* out */ void *Obj);
	int __stdcall _AddRef();
	int __stdcall _Release();
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	void __fastcall SetTexture(TGLTexture* const Value);
	void __fastcall SetTextureIndex(const int Value);
	void __fastcall SetTextureOffset(Gls::Coordinates::TGLCoordinates3* const Value);
	void __fastcall SetTextureScale(Gls::Coordinates::TGLCoordinates3* const Value);
	void __fastcall NotifyTexMapChange(System::TObject* Sender);
	void __fastcall CalculateTextureMatrix();
	void __fastcall OnNotifyChange(System::TObject* Sender);
	
public:
	__fastcall virtual TGLTextureExItem(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLTextureExItem();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property TGLTexture* Texture = {read=FTexture, write=SetTexture};
	__property int TextureIndex = {read=FTextureIndex, write=SetTextureIndex, nodefault};
	__property Gls::Coordinates::TGLCoordinates3* TextureOffset = {read=FTextureOffset, write=SetTextureOffset};
	__property Gls::Coordinates::TGLCoordinates3* TextureScale = {read=FTextureScale, write=SetTextureScale};
private:
	void *__IGLTextureNotifyAble;	// IGLTextureNotifyAble 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0D9DC0B0-ECE4-4513-A8A1-5AE7022C9426}
	operator _di_IGLTextureNotifyAble()
	{
		_di_IGLTextureNotifyAble intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IGLTextureNotifyAble*(void) { return (IGLTextureNotifyAble*)&__IGLTextureNotifyAble; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureEx : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLTextureExItem* operator[](int index) { return this->Items[index]; }
	
private:
	Gls::Baseclasses::TGLUpdateAbleObject* FOwner;
	
protected:
	void __fastcall SetItems(int index, TGLTextureExItem* const Value);
	TGLTextureExItem* __fastcall GetItems(int index);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	__fastcall TGLTextureEx(Gls::Baseclasses::TGLUpdateAbleObject* AOwner);
	void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	bool __fastcall IsTextureEnabled(int Index);
	HIDESBASE TGLTextureExItem* __fastcall Add();
	__property TGLTextureExItem* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall Loaded();
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLTextureEx() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION ETexture : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ETexture(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ETexture(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ETexture(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ETexture(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ETexture(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ETexture(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ETexture(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ETexture(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ETexture(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ETexture(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ETexture(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ETexture(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ETexture() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLShaderException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLShaderException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLShaderException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLShaderException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLShaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLShaderException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLShaderException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLShaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLShaderException() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define cDefaultNormalMapScale  (1.250000E-01)
static const System::Int8 CmtPX = System::Int8(0x0);
static const System::Int8 CmtNX = System::Int8(0x1);
static const System::Int8 CmtPY = System::Int8(0x2);
static const System::Int8 CmtNY = System::Int8(0x3);
static const System::Int8 CmtPZ = System::Int8(0x4);
static const System::Int8 CmtNZ = System::Int8(0x5);
extern DELPHI_PACKAGE void __fastcall RegisterTGraphicClassFileExtension(const System::UnicodeString extension, const Vcl::Graphics::TGraphicClass aClass);
extern DELPHI_PACKAGE Vcl::Graphics::TGraphic* __fastcall CreateGraphicFromFile(const System::UnicodeString fileName);
extern DELPHI_PACKAGE void __fastcall RegisterGLTextureImageClass(TGLTextureImageClass textureImageClass);
extern DELPHI_PACKAGE TGLTextureImageClass __fastcall FindGLTextureImageClass(const System::UnicodeString className);
extern DELPHI_PACKAGE TGLTextureImageClass __fastcall FindGLTextureImageClassByFriendlyName(const System::UnicodeString friendlyName);
extern DELPHI_PACKAGE void __fastcall SetGLTextureImageClassesToStrings(System::Classes::TStrings* aStrings);
extern DELPHI_PACKAGE System::Classes::TStrings* __fastcall GetGLTextureImageClassesAsStrings(void);
}	/* namespace Texture */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_TEXTURE)
using namespace Gls::Texture;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_TextureHPP
