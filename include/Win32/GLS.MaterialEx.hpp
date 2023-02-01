// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.MaterialEx.pas' rev: 35.00 (Windows)

#ifndef Gls_MaterialexHPP
#define Gls_MaterialexHPP

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
#include <System.Math.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Context.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Material.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Color.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.State.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.TextureCombiners.hpp>
#include <GLSL.ShaderParameter.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.Strings.hpp>
#include <GLS.ImageUtils.hpp>
#include <GLS.Utils.hpp>
#include <GLS.XOpenGL.hpp>
#include <GLS.Logger.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Materialex
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBaseMaterialCollectionItem;
class DELPHICLASS TGLLibMaterialProperty;
class DELPHICLASS TGLTextureSampler;
class DELPHICLASS TGLAbstractTexture;
class DELPHICLASS TGLTextureImageEx;
class DELPHICLASS TGLFrameBufferAttachment;
class DELPHICLASS TGLTextureSwizzling;
class DELPHICLASS TGLTextureProperties;
class DELPHICLASS TGLFixedFunctionProperties;
class DELPHICLASS TGLTextureCombiner;
class DELPHICLASS TGLASMVertexProgram;
class DELPHICLASS TGLMultitexturingProperties;
class DELPHICLASS TGLShaderEx;
class DELPHICLASS TGLAbstractShaderUniform;
class DELPHICLASS TGLShaderUniform;
class DELPHICLASS TGLShaderUniformDSA;
class DELPHICLASS TGLShaderUniformTexture;
class DELPHICLASS TGLBaseShaderModel;
class DELPHICLASS TGLShaderModel3;
class DELPHICLASS TGLShaderModel4;
class DELPHICLASS TGLShaderModel5;
class DELPHICLASS TGLLibMaterialEx;
class DELPHICLASS TGLLibMaterialsEx;
class DELPHICLASS TGLMatLibComponents;
class DELPHICLASS TGLMaterialLibraryEx;
//-- type declarations -------------------------------------------------------
typedef System::UnicodeString TGLMaterialComponentName;

typedef void __fastcall (__closure *TOnAsmProgSetting)(TGLASMVertexProgram* Sender, Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);

typedef void __fastcall (__closure *TOnUniformInitialize)(TGLBaseShaderModel* Sender);

typedef void __fastcall (__closure *TOnUniformSetting)(TGLBaseShaderModel* Sender, Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseMaterialCollectionItem : public Gls::Xcollection::TXCollectionItem
{
	typedef Gls::Xcollection::TXCollectionItem inherited;
	
private:
	int FNameHashKey;
	Gls::Persistentclasses::TGLPersistentObjectList* FUserList;
	bool FDefferedInit;
	bool FNotifying;
	bool FIsValid;
	Gls::Persistentclasses::TGLPersistentObjectList* __fastcall GetUserList();
	TGLMaterialLibraryEx* __fastcall GetMaterialLibraryEx();
	
protected:
	virtual void __fastcall SetName(const System::UnicodeString AValue);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property Gls::Persistentclasses::TGLPersistentObjectList* UserList = {read=GetUserList};
	virtual void __fastcall DoOnPrepare(Gls::Context::TGLContext* Sender) = 0 ;
	
public:
	__fastcall virtual ~TGLBaseMaterialCollectionItem();
	void __fastcall RegisterUser(Gls::Baseclasses::TGLUpdateAbleObject* AUser);
	void __fastcall UnregisterUser(Gls::Baseclasses::TGLUpdateAbleObject* AUser);
	int __fastcall GetUserCount();
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	__property TGLMaterialLibraryEx* MaterialLibrary = {read=GetMaterialLibraryEx};
	__property bool IsValid = {read=FIsValid, nodefault};
	
__published:
	__property System::UnicodeString Name = {read=GetName, write=SetName};
	__property bool DefferedInit = {read=FDefferedInit, write=FDefferedInit, default=0};
public:
	/* TXCollectionItem.Create */ inline __fastcall virtual TGLBaseMaterialCollectionItem(Gls::Xcollection::TXCollection* aOwner) : Gls::Xcollection::TXCollectionItem(aOwner) { }
	
private:
	void *__IGLMaterialLibrarySupported;	// Gls::Material::IGLMaterialLibrarySupported 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Gls::Material::_di_IGLMaterialLibrarySupported()
	{
		Gls::Material::_di_IGLMaterialLibrarySupported intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gls::Material::IGLMaterialLibrarySupported*(void) { return (Gls::Material::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};

#pragma pack(pop)

typedef System::TMetaClass* CGLBaseMaterialCollectionItem;

class PASCALIMPLEMENTATION TGLLibMaterialProperty : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
protected:
	bool FEnabled;
	System::UnicodeString FNextPassName;
	TGLLibMaterialEx* __fastcall GetMaterial();
	TGLMaterialLibraryEx* __fastcall GetMaterialLibraryEx();
	virtual void __fastcall SetEnabled(bool AValue);
	void __fastcall SetNextPass(const System::UnicodeString AValue);
	virtual void __fastcall Loaded();
	__property System::UnicodeString NextPass = {read=FNextPassName, write=SetNextPass};
	
public:
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	__property TGLMaterialLibraryEx* MaterialLibrary = {read=GetMaterialLibraryEx};
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLLibMaterialProperty(System::Classes::TPersistent* AOwner) : Gls::Baseclasses::TGLUpdateAbleObject(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLLibMaterialProperty() { }
	
private:
	void *__IGLMaterialLibrarySupported;	// Gls::Material::IGLMaterialLibrarySupported 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Gls::Material::_di_IGLMaterialLibrarySupported()
	{
		Gls::Material::_di_IGLMaterialLibrarySupported intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gls::Material::IGLMaterialLibrarySupported*(void) { return (Gls::Material::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureSampler : public TGLBaseMaterialCollectionItem
{
	typedef TGLBaseMaterialCollectionItem inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
private:
	Gls::Context::TGLSamplerHandle* FHandle;
	Gls::Texture::TGLMinFilter FMinFilter;
	Gls::Texture::TGLMagFilter FMagFilter;
	Gls::Textureformat::TGLTextureFilteringQuality FFilteringQuality;
	int FLODBias;
	float FLODBiasFract;
	System::StaticArray<Gls::Textureformat::TGLSeparateTextureWrap, 3> FWrap;
	Gls::Color::TGLColor* FBorderColor;
	Gls::Textureformat::TGLTextureCompareMode FCompareMode;
	Gls::State::TGLComparisonFunction FCompareFunc;
	bool FDecodeSRGB;
	void __fastcall SetMagFilter(Gls::Texture::TGLMagFilter AValue);
	void __fastcall SetMinFilter(Gls::Texture::TGLMinFilter AValue);
	void __fastcall SetLODBias(int AValue);
	void __fastcall SetFilteringQuality(Gls::Textureformat::TGLTextureFilteringQuality AValue);
	Gls::Textureformat::TGLSeparateTextureWrap __fastcall GetWrap(int Index);
	void __fastcall SetWrap(int Index, Gls::Textureformat::TGLSeparateTextureWrap AValue);
	void __fastcall SetBorderColor(Gls::Color::TGLColor* const AValue);
	void __fastcall SetCompareMode(Gls::Textureformat::TGLTextureCompareMode AValue);
	void __fastcall SetCompareFunc(Gls::State::TGLComparisonFunction AValue);
	void __fastcall SetDecodeSRGB(bool AValue);
	
public:
	__fastcall virtual TGLTextureSampler(Gls::Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLTextureSampler();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall DoOnPrepare(Gls::Context::TGLContext* Sender);
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__property Gls::Context::TGLSamplerHandle* Handle = {read=FHandle};
	
__published:
	__property Gls::Texture::TGLMagFilter MagFilter = {read=FMagFilter, write=SetMagFilter, default=1};
	__property Gls::Texture::TGLMinFilter MinFilter = {read=FMinFilter, write=SetMinFilter, default=5};
	__property Gls::Textureformat::TGLTextureFilteringQuality FilteringQuality = {read=FFilteringQuality, write=SetFilteringQuality, default=1};
	__property int LodBias = {read=FLODBias, write=SetLODBias, default=0};
	__property Gls::Textureformat::TGLSeparateTextureWrap WrapX = {read=GetWrap, write=SetWrap, index=0, default=0};
	__property Gls::Textureformat::TGLSeparateTextureWrap WrapY = {read=GetWrap, write=SetWrap, index=1, default=0};
	__property Gls::Textureformat::TGLSeparateTextureWrap WrapZ = {read=GetWrap, write=SetWrap, index=2, default=0};
	__property Gls::Color::TGLColor* BorderColor = {read=FBorderColor, write=SetBorderColor};
	__property Gls::Textureformat::TGLTextureCompareMode CompareMode = {read=FCompareMode, write=SetCompareMode, default=0};
	__property Gls::State::TGLComparisonFunction CompareFunc = {read=FCompareFunc, write=SetCompareFunc, default=3};
	__property bool sRGB_Encode = {read=FDecodeSRGB, write=SetDecodeSRGB, default=1};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLAbstractTexture : public TGLBaseMaterialCollectionItem
{
	typedef TGLBaseMaterialCollectionItem inherited;
	
protected:
	Gls::Context::TGLTextureHandle* FHandle;
	Gls::Textureformat::TGLInternalFormat FInternalFormat;
	int FWidth;
	int FHeight;
	int FDepth;
	Gls::Textureformat::TSwizzleVector FSwizzles;
	TGLTextureSampler* FApplicableSampler;
	TGLTextureSampler* FLastSampler;
	Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	virtual void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci) = 0 ;
	virtual void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci) = 0 ;
	
public:
	__property Gls::Context::TGLTextureHandle* Handle = {read=FHandle};
	
__published:
	__property Gls::Textureformat::TGLTextureTarget Shape = {read=GetTextureTarget, nodefault};
public:
	/* TGLBaseMaterialCollectionItem.Destroy */ inline __fastcall virtual ~TGLAbstractTexture() { }
	
public:
	/* TXCollectionItem.Create */ inline __fastcall virtual TGLAbstractTexture(Gls::Xcollection::TXCollection* aOwner) : TGLBaseMaterialCollectionItem(aOwner) { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TMipmapGenerationMode : unsigned char { mgmNoMip, mgmLeaveExisting, mgmOnFly, mgmBoxFilter, mgmTriangleFilter, mgmHermiteFilter, mgmBellFilter, mgmSplineFilter, mgmLanczos3Filter, mgmMitchellFilter };

class PASCALIMPLEMENTATION TGLTextureImageEx : public TGLAbstractTexture
{
	typedef TGLAbstractTexture inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
private:
	Gls::Textureformat::TGLInternalCompression FCompression;
	Gls::Graphics::TGLBaseImage* FImage;
	Gls::Texture::TGLTextureImageAlpha FImageAlpha;
	float FImageBrightness;
	float FImageGamma;
	float FHeightToNormalScale;
	System::UnicodeString FSourceFile;
	int FApplyCounter;
	bool FInternallyStored;
	TMipmapGenerationMode FMipGenMode;
	bool FUseStreaming;
	int FBaseLevel;
	int FMaxLevel;
	double FLastTime;
	void __fastcall SetInternalFormat(const Gls::Textureformat::TGLInternalFormat AValue);
	void __fastcall SetImageAlpha(const Gls::Texture::TGLTextureImageAlpha AValue);
	void __fastcall SetImageBrightness(const float AValue);
	bool __fastcall StoreBrightness();
	void __fastcall SetImageGamma(const float AValue);
	bool __fastcall StoreGamma();
	void __fastcall SetNormalMapScale(const float AValue);
	bool __fastcall StoreNormalMapScale();
	void __fastcall SetCompression(const Gls::Textureformat::TGLInternalCompression AValue);
	void __fastcall SetSourceFile(System::UnicodeString AValue);
	void __fastcall SetInternallyStored(const bool AValue);
	void __fastcall SetMipGenMode(const TMipmapGenerationMode AValue);
	void __fastcall SetUseStreaming(const bool AValue);
	void __fastcall PrepareImage();
	void __fastcall FullTransfer();
	void __fastcall StreamTransfer();
	void __fastcall CalcLODRange(/* out */ int &AFirstLOD, /* out */ int &ALastLOD);
	
public:
	__fastcall virtual TGLTextureImageEx(Gls::Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLTextureImageEx();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall DoOnPrepare(Gls::Context::TGLContext* Sender);
	virtual void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	virtual void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	
__published:
	__property int InternalWidth = {read=FWidth, nodefault};
	__property int InternalHeight = {read=FHeight, nodefault};
	__property int InternalDepth = {read=FDepth, nodefault};
	__property Gls::Textureformat::TGLInternalFormat InternalFormat = {read=FInternalFormat, write=SetInternalFormat, default=31};
	__property Gls::Texture::TGLTextureImageAlpha ImageAlpha = {read=FImageAlpha, write=SetImageAlpha, default=0};
	__property float ImageBrightness = {read=FImageBrightness, write=SetImageBrightness, stored=StoreBrightness};
	__property float ImageGamma = {read=FImageGamma, write=SetImageGamma, stored=StoreGamma};
	__property Gls::Textureformat::TGLInternalCompression Compression = {read=FCompression, write=SetCompression, default=0};
	__property float HeightToNormalScale = {read=FHeightToNormalScale, write=SetNormalMapScale, stored=StoreNormalMapScale};
	__property System::UnicodeString SourceFile = {read=FSourceFile, write=SetSourceFile};
	__property bool InternallyStored = {read=FInternallyStored, write=SetInternallyStored, default=0};
	__property TMipmapGenerationMode MipGenMode = {read=FMipGenMode, write=SetMipGenMode, default=2};
	__property bool UseStreaming = {read=FUseStreaming, write=SetUseStreaming, default=0};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFrameBufferAttachment : public TGLAbstractTexture
{
	typedef TGLAbstractTexture inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
private:
	Gls::Context::TGLRenderbufferHandle* FRenderBufferHandle;
	bool FLayered;
	bool FCubeMap;
	int FSamples;
	bool FOnlyWrite;
	bool FFixedSamplesLocation;
	void __fastcall SetWidth(int AValue);
	void __fastcall SetHeight(int AValue);
	void __fastcall SetDepth(int AValue);
	void __fastcall SetInternalFormat(const Gls::Textureformat::TGLInternalFormat AValue);
	void __fastcall SetOnlyWrite(bool AValue);
	void __fastcall SetLayered(bool AValue);
	void __fastcall SetCubeMap(bool AValue);
	void __fastcall SetSamples(int AValue);
	void __fastcall SetFixedSamplesLocation(bool AValue);
	
public:
	__fastcall virtual TGLFrameBufferAttachment(Gls::Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLFrameBufferAttachment();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall DoOnPrepare(Gls::Context::TGLContext* Sender);
	virtual void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	virtual void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	
__published:
	__property int InternalWidth = {read=FWidth, write=SetWidth, default=256};
	__property int InternalHeight = {read=FHeight, write=SetHeight, default=256};
	__property int InternalDepth = {read=FDepth, write=SetDepth, default=0};
	__property Gls::Textureformat::TGLInternalFormat InternalFormat = {read=FInternalFormat, write=SetInternalFormat, default=31};
	__property bool OnlyWrite = {read=FOnlyWrite, write=SetOnlyWrite, default=0};
	__property bool Layered = {read=FLayered, write=SetLayered, default=0};
	__property bool CubeMap = {read=FCubeMap, write=SetCubeMap, default=0};
	__property int Samples = {read=FSamples, write=SetSamples, default=-1};
	__property bool FixedSamplesLocation = {read=FFixedSamplesLocation, write=SetFixedSamplesLocation, default=0};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLTextureSwizzling : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Gls::Textureformat::TSwizzleVector FSwizzles;
	Gls::Textureformat::TGLTextureSwizzle __fastcall GetSwizzle(int AIndex);
	void __fastcall SetSwizzle(int AIndex, Gls::Textureformat::TGLTextureSwizzle AValue);
	bool __fastcall StoreSwizzle(int AIndex);
	
public:
	__fastcall virtual TGLTextureSwizzling(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
__published:
	__property Gls::Textureformat::TGLTextureSwizzle RedFrom = {read=GetSwizzle, write=SetSwizzle, stored=StoreSwizzle, index=0, nodefault};
	__property Gls::Textureformat::TGLTextureSwizzle GreenFrom = {read=GetSwizzle, write=SetSwizzle, stored=StoreSwizzle, index=1, nodefault};
	__property Gls::Textureformat::TGLTextureSwizzle BlueFrom = {read=GetSwizzle, write=SetSwizzle, stored=StoreSwizzle, index=2, nodefault};
	__property Gls::Textureformat::TGLTextureSwizzle AlphaFrom = {read=GetSwizzle, write=SetSwizzle, stored=StoreSwizzle, index=3, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLTextureSwizzling() { }
	
};


class PASCALIMPLEMENTATION TGLTextureProperties : public TGLLibMaterialProperty
{
	typedef TGLLibMaterialProperty inherited;
	
private:
	System::UnicodeString FLibTextureName;
	System::UnicodeString FLibSamplerName;
	TGLAbstractTexture* FLibTexture;
	TGLTextureSampler* FLibSampler;
	Gls::Coordinates::TGLCoordinates3* FTextureOffset;
	Gls::Coordinates::TGLCoordinates3* FTextureScale;
	float FTextureRotate;
	bool FTextureMatrixIsIdentity;
	bool FTextureOverride;
	Gls::Vectortypes::TMatrix4f FTextureMatrix;
	Gls::Texture::TGLTextureMappingMode FMappingMode;
	Gls::Color::TGLColor* FEnvColor;
	Gls::Coordinates::TGLCoordinates4* FMapSCoordinates;
	Gls::Coordinates::TGLCoordinates4* FMapTCoordinates;
	Gls::Coordinates::TGLCoordinates4* FMapRCoordinates;
	Gls::Coordinates::TGLCoordinates4* FMapQCoordinates;
	TGLTextureSwizzling* FSwizzling;
	System::UnicodeString __fastcall GetLibTextureName();
	System::UnicodeString __fastcall GetLibSamplerName();
	void __fastcall SetLibTextureName(const System::UnicodeString AValue);
	void __fastcall SetLibSamplerName(const System::UnicodeString AValue);
	Gls::Coordinates::TGLCoordinates3* __fastcall GetTextureOffset();
	void __fastcall SetTextureOffset(Gls::Coordinates::TGLCoordinates3* const AValue);
	bool __fastcall StoreTextureOffset();
	Gls::Coordinates::TGLCoordinates3* __fastcall GetTextureScale();
	void __fastcall SetTextureScale(Gls::Coordinates::TGLCoordinates3* const AValue);
	bool __fastcall StoreTextureScale();
	void __fastcall SetTextureMatrix(const Gls::Vectortypes::TMatrix4f &AValue);
	void __fastcall SetTextureRotate(float AValue);
	bool __fastcall StoreTextureRotate();
	void __fastcall SetMappingMode(const Gls::Texture::TGLTextureMappingMode AValue);
	Gls::Coordinates::TGLCoordinates4* __fastcall GetMappingSCoordinates();
	void __fastcall SetMappingSCoordinates(Gls::Coordinates::TGLCoordinates4* const AValue);
	bool __fastcall StoreMappingSCoordinates();
	Gls::Coordinates::TGLCoordinates4* __fastcall GetMappingTCoordinates();
	void __fastcall SetMappingTCoordinates(Gls::Coordinates::TGLCoordinates4* const AValue);
	bool __fastcall StoreMappingTCoordinates();
	Gls::Coordinates::TGLCoordinates4* __fastcall GetMappingRCoordinates();
	void __fastcall SetMappingRCoordinates(Gls::Coordinates::TGLCoordinates4* const AValue);
	bool __fastcall StoreMappingRCoordinates();
	Gls::Coordinates::TGLCoordinates4* __fastcall GetMappingQCoordinates();
	void __fastcall SetMappingQCoordinates(Gls::Coordinates::TGLCoordinates4* const AValue);
	bool __fastcall StoreMappingQCoordinates();
	void __fastcall SetSwizzling(TGLTextureSwizzling* const AValue);
	bool __fastcall StoreSwizzling();
	void __fastcall SetEnvColor(Gls::Color::TGLColor* const AValue);
	void __fastcall CalculateTextureMatrix();
	void __fastcall ApplyMappingMode();
	void __fastcall UnApplyMappingMode();
	
protected:
	virtual void __fastcall Loaded();
	
public:
	__fastcall virtual TGLTextureProperties(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLTextureProperties();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall Notification(System::TObject* Sender, System::Classes::TOperation Operation);
	bool __fastcall IsValid();
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	__property Gls::Vectortypes::TMatrix4f TextureMatrix = {read=FTextureMatrix, write=SetTextureMatrix};
	
__published:
	__property System::UnicodeString LibTextureName = {read=GetLibTextureName, write=SetLibTextureName};
	__property System::UnicodeString LibSamplerName = {read=GetLibSamplerName, write=SetLibSamplerName};
	__property Gls::Coordinates::TGLCoordinates3* TextureOffset = {read=GetTextureOffset, write=SetTextureOffset, stored=StoreTextureOffset};
	__property Gls::Coordinates::TGLCoordinates3* TextureScale = {read=GetTextureScale, write=SetTextureScale, stored=StoreTextureScale};
	__property float TextureRotate = {read=FTextureRotate, write=SetTextureRotate, stored=StoreTextureRotate};
	__property Gls::Color::TGLColor* EnvColor = {read=FEnvColor, write=SetEnvColor};
	__property Gls::Texture::TGLTextureMappingMode MappingMode = {read=FMappingMode, write=SetMappingMode, default=0};
	__property Gls::Coordinates::TGLCoordinates4* MappingSCoordinates = {read=GetMappingSCoordinates, write=SetMappingSCoordinates, stored=StoreMappingSCoordinates};
	__property Gls::Coordinates::TGLCoordinates4* MappingTCoordinates = {read=GetMappingTCoordinates, write=SetMappingTCoordinates, stored=StoreMappingTCoordinates};
	__property Gls::Coordinates::TGLCoordinates4* MappingRCoordinates = {read=GetMappingRCoordinates, write=SetMappingRCoordinates, stored=StoreMappingRCoordinates};
	__property Gls::Coordinates::TGLCoordinates4* MappingQCoordinates = {read=GetMappingQCoordinates, write=SetMappingQCoordinates, stored=StoreMappingQCoordinates};
	__property TGLTextureSwizzling* Swizzling = {read=FSwizzling, write=SetSwizzling, stored=StoreSwizzling};
};


class PASCALIMPLEMENTATION TGLFixedFunctionProperties : public TGLLibMaterialProperty
{
	typedef TGLLibMaterialProperty inherited;
	
private:
	Gls::Material::TGLFaceProperties* FFrontProperties;
	Gls::Material::TGLFaceProperties* FBackProperties;
	Gls::Material::TGLDepthProperties* FDepthProperties;
	Gls::Material::TGLBlendingMode FBlendingMode;
	Gls::Material::TGLBlendingParameters* FBlendingParams;
	TGLTextureProperties* FTexProp;
	Gls::Material::TGLMaterialOptions FMaterialOptions;
	Gls::Material::TGLFaceCulling FFaceCulling;
	Gls::State::TGLPolygonMode FPolygonMode;
	Gls::Texture::TGLTextureMode FTextureMode;
	Gls::Material::TGLFaceProperties* __fastcall GetBackProperties();
	void __fastcall SetBackProperties(Gls::Material::TGLFaceProperties* AValues);
	void __fastcall SetFrontProperties(Gls::Material::TGLFaceProperties* AValues);
	void __fastcall SetDepthProperties(Gls::Material::TGLDepthProperties* AValues);
	void __fastcall SetBlendingMode(const Gls::Material::TGLBlendingMode AValue);
	void __fastcall SetMaterialOptions(const Gls::Material::TGLMaterialOptions AValue);
	void __fastcall SetFaceCulling(const Gls::Material::TGLFaceCulling AValue);
	void __fastcall SetPolygonMode(Gls::State::TGLPolygonMode AValue);
	void __fastcall SetBlendingParams(Gls::Material::TGLBlendingParameters* const AValue);
	void __fastcall SetTexProp(TGLTextureProperties* AValue);
	void __fastcall SetTextureMode(Gls::Texture::TGLTextureMode AValue);
	
public:
	__fastcall virtual TGLFixedFunctionProperties(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLFixedFunctionProperties();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	bool __fastcall Blended();
	
__published:
	__property Gls::Material::TGLMaterialOptions MaterialOptions = {read=FMaterialOptions, write=SetMaterialOptions, default=0};
	__property Gls::Material::TGLFaceProperties* BackProperties = {read=GetBackProperties, write=SetBackProperties};
	__property Gls::Material::TGLFaceProperties* FrontProperties = {read=FFrontProperties, write=SetFrontProperties};
	__property Gls::Material::TGLDepthProperties* DepthProperties = {read=FDepthProperties, write=SetDepthProperties};
	__property Gls::Material::TGLBlendingMode BlendingMode = {read=FBlendingMode, write=SetBlendingMode, default=0};
	__property Gls::Material::TGLBlendingParameters* BlendingParams = {read=FBlendingParams, write=SetBlendingParams};
	__property Gls::Material::TGLFaceCulling FaceCulling = {read=FFaceCulling, write=SetFaceCulling, default=0};
	__property Gls::State::TGLPolygonMode PolygonMode = {read=FPolygonMode, write=SetPolygonMode, default=0};
	__property TGLTextureProperties* Texture = {read=FTexProp, write=SetTexProp};
	__property Gls::Texture::TGLTextureMode TextureMode = {read=FTextureMode, write=SetTextureMode, default=0};
	__property NextPass = {default=0};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureCombiner : public TGLBaseMaterialCollectionItem
{
	typedef TGLBaseMaterialCollectionItem inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
private:
	Gls::Context::TGLVirtualHandle* FHandle;
	System::Classes::TStringList* FScript;
	Gls::Texturecombiners::TCombinerCache FCommandCache;
	void __fastcall SetScript(System::Classes::TStringList* AValue);
	void __fastcall DoAllocate(Gls::Context::TGLVirtualHandle* Sender, unsigned &handle);
	void __fastcall DoDeallocate(Gls::Context::TGLVirtualHandle* Sender, unsigned &handle);
	
public:
	__fastcall virtual TGLTextureCombiner(Gls::Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLTextureCombiner();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall DoOnPrepare(Gls::Context::TGLContext* Sender);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	
__published:
	__property System::Classes::TStringList* Script = {read=FScript, write=SetScript};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLASMVertexProgram : public TGLBaseMaterialCollectionItem
{
	typedef TGLBaseMaterialCollectionItem inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
private:
	Gls::Context::TGLARBVertexProgramHandle* FHandle;
	System::Classes::TStringList* FSource;
	System::UnicodeString FSourceFile;
	System::UnicodeString FInfoLog;
	void __fastcall SetSource(System::Classes::TStringList* AValue);
	void __fastcall SetSourceFile(System::UnicodeString AValue);
	Gls::Context::TGLARBVertexProgramHandle* __fastcall GetHandle();
	
public:
	__fastcall virtual TGLASMVertexProgram(Gls::Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLASMVertexProgram();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoOnPrepare(Gls::Context::TGLContext* Sender);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property Gls::Context::TGLARBVertexProgramHandle* Handle = {read=GetHandle};
	
__published:
	__property System::Classes::TStringList* Source = {read=FSource, write=SetSource};
	__property System::UnicodeString SourceFile = {read=FSourceFile, write=SetSourceFile};
	__property System::UnicodeString InfoLog = {read=FInfoLog};
};

#pragma pack(pop)

enum DECLSPEC_DENUM TLightDir2TexEnvColor : unsigned char { l2eNone, l2eEnvColor0, l2eEnvColor1, l2eEnvColor2, l2eEnvColor3 };

class PASCALIMPLEMENTATION TGLMultitexturingProperties : public TGLLibMaterialProperty
{
	typedef TGLLibMaterialProperty inherited;
	
private:
	TGLTextureCombiner* FLibCombiner;
	TGLASMVertexProgram* FLibAsmProg;
	System::UnicodeString FLibCombinerName;
	System::UnicodeString FLibAsmProgName;
	System::StaticArray<TGLTextureProperties*, 4> FTexProps;
	Gls::Texture::TGLTextureMode FTextureMode;
	TLightDir2TexEnvColor FLightDir;
	int FLightSourceIndex;
	System::UnicodeString __fastcall GetLibCombinerName();
	System::UnicodeString __fastcall GetLibAsmProgName();
	void __fastcall SetLibCombinerName(const System::UnicodeString AValue);
	void __fastcall SetLibAsmProgName(const System::UnicodeString AValue);
	TGLTextureProperties* __fastcall GetTexProps(int AIndex);
	void __fastcall SetTexProps(int AIndex, TGLTextureProperties* AValue);
	void __fastcall SetTextureMode(Gls::Texture::TGLTextureMode AValue);
	void __fastcall SetLightSourceIndex(int AValue);
	
protected:
	virtual void __fastcall Loaded();
	
public:
	__fastcall virtual TGLMultitexturingProperties(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLMultitexturingProperties();
	virtual void __fastcall Notification(System::TObject* Sender, System::Classes::TOperation Operation);
	bool __fastcall IsValid();
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	
__published:
	__property System::UnicodeString LibCombinerName = {read=GetLibCombinerName, write=SetLibCombinerName};
	__property System::UnicodeString LibAsmProgName = {read=GetLibAsmProgName, write=SetLibAsmProgName};
	__property TGLTextureProperties* Texture0 = {read=GetTexProps, write=SetTexProps, index=0};
	__property TGLTextureProperties* Texture1 = {read=GetTexProps, write=SetTexProps, index=1};
	__property TGLTextureProperties* Texture2 = {read=GetTexProps, write=SetTexProps, index=2};
	__property TGLTextureProperties* Texture3 = {read=GetTexProps, write=SetTexProps, index=3};
	__property Gls::Texture::TGLTextureMode TextureMode = {read=FTextureMode, write=SetTextureMode, default=0};
	__property TLightDir2TexEnvColor LightDirTo = {read=FLightDir, write=FLightDir, default=0};
	__property int LightSourceIndex = {read=FLightSourceIndex, write=SetLightSourceIndex, default=0};
	__property NextPass = {default=0};
};


enum DECLSPEC_DENUM TGLShaderType : unsigned char { shtVertex, shtControl, shtEvaluation, shtGeometry, shtFragment };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLShaderEx : public TGLBaseMaterialCollectionItem
{
	typedef TGLBaseMaterialCollectionItem inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
private:
	System::StaticArray<Gls::Context::TGLShaderHandle*, 5> FHandle;
	System::Classes::TStringList* FSource;
	System::UnicodeString FSourceFile;
	TGLShaderType FShaderType;
	System::UnicodeString FInfoLog;
	Glsl::Shaderparameter::TGLgsInTypes FGeometryInput;
	Glsl::Shaderparameter::TGLgsOutTypes FGeometryOutput;
	int FGeometryVerticesOut;
	void __fastcall SetSource(System::Classes::TStringList* AValue);
	void __fastcall SetSourceFile(System::UnicodeString AValue);
	void __fastcall SetShaderType(TGLShaderType AValue);
	void __fastcall SetGeometryInput(Glsl::Shaderparameter::TGLgsInTypes AValue);
	void __fastcall SetGeometryOutput(Glsl::Shaderparameter::TGLgsOutTypes AValue);
	void __fastcall SetGeometryVerticesOut(int AValue);
	Gls::Context::TGLShaderHandle* __fastcall GetHandle();
	
public:
	__fastcall virtual TGLShaderEx(Gls::Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLShaderEx();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoOnPrepare(Gls::Context::TGLContext* Sender);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property Gls::Context::TGLShaderHandle* Handle = {read=GetHandle};
	
__published:
	__property System::Classes::TStringList* Source = {read=FSource, write=SetSource};
	__property System::UnicodeString SourceFile = {read=FSourceFile, write=SetSourceFile};
	__property TGLShaderType ShaderType = {read=FShaderType, write=SetShaderType, default=0};
	__property System::UnicodeString InfoLog = {read=FInfoLog};
	__property Glsl::Shaderparameter::TGLgsInTypes GeometryInput = {read=FGeometryInput, write=SetGeometryInput, default=0};
	__property Glsl::Shaderparameter::TGLgsOutTypes GeometryOutput = {read=FGeometryOutput, write=SetGeometryOutput, default=0};
	__property int GeometryVerticesOut = {read=FGeometryVerticesOut, write=SetGeometryVerticesOut, default=1};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLAbstractShaderUniform : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
protected:
	System::UnicodeString FName;
	int FNameHashCode;
	Glsl::Shaderparameter::TGLSLDataType FType;
	Glsl::Shaderparameter::TGLSLSamplerType FSamplerType;
	System::UnicodeString __fastcall GetName();
	Glsl::Shaderparameter::TGLSLDataType __fastcall GetGLSLType();
	Glsl::Shaderparameter::TGLSLSamplerType __fastcall GetGLSLSamplerType();
	virtual System::UnicodeString __fastcall GetAutoSetMethod();
	virtual System::UnicodeString __fastcall GetTextureName();
	virtual System::UnicodeString __fastcall GetSamplerName();
	virtual Gls::Textureformat::TSwizzleVector __fastcall GetTextureSwizzle();
	virtual void __fastcall SetTextureName(const System::UnicodeString AValue);
	virtual void __fastcall SetSamplerName(const System::UnicodeString AValue);
	virtual void __fastcall SetAutoSetMethod(const System::UnicodeString AValue);
	virtual void __fastcall SetTextureSwizzle(const Gls::Textureformat::TSwizzleVector AValue);
	virtual float __fastcall GetFloat();
	virtual Gls::Vectortypes::TVector2f __fastcall GetVec2();
	virtual Gls::Vectortypes::TVector3f __fastcall GetVec3();
	virtual Gls::Vectortypes::TVector4f __fastcall GetVec4();
	virtual int __fastcall GetInt();
	virtual Gls::Vectortypes::TVector2i __fastcall GetIVec2();
	virtual Gls::Vectortypes::TVector3i __fastcall GetIVec3();
	virtual Gls::Vectortypes::TVector4i __fastcall GetIVec4();
	virtual unsigned __fastcall GetUInt();
	virtual Gls::Vectortypes::TVector2ui __fastcall GetUVec2();
	virtual Gls::Vectortypes::TVector3ui __fastcall GetUVec3();
	virtual Gls::Vectortypes::TVector4ui __fastcall GetUVec4();
	virtual void __fastcall SetFloat(const float Value);
	virtual void __fastcall SetVec2(const Gls::Vectortypes::TVector2f &Value);
	virtual void __fastcall SetVec3(const Gls::Vectortypes::TVector3f &Value);
	virtual void __fastcall SetVec4(const Gls::Vectortypes::TVector4f &Value);
	virtual void __fastcall SetInt(const int Value);
	virtual void __fastcall SetIVec2(const Gls::Vectortypes::TVector2i &Value);
	virtual void __fastcall SetIVec3(const Gls::Vectortypes::TVector3i &Value);
	virtual void __fastcall SetIVec4(const Gls::Vectortypes::TVector4i &Value);
	virtual void __fastcall SetUInt(const unsigned Value);
	virtual void __fastcall SetUVec2(const Gls::Vectortypes::TVector2ui &Value);
	virtual void __fastcall SetUVec3(const Gls::Vectortypes::TVector3ui &Value);
	virtual void __fastcall SetUVec4(const Gls::Vectortypes::TVector4ui &Value);
	virtual Gls::Vectortypes::TMatrix2f __fastcall GetMat2();
	virtual Gls::Vectortypes::TMatrix3f __fastcall GetMat3();
	virtual Gls::Vectortypes::TMatrix4f __fastcall GetMat4();
	virtual void __fastcall SetMat2(const Gls::Vectortypes::TMatrix2f &Value);
	virtual void __fastcall SetMat3(const Gls::Vectortypes::TMatrix3f &Value);
	virtual void __fastcall SetMat4(const Gls::Vectortypes::TMatrix4f &Value);
	virtual void __fastcall SetFloatArray(const System::PSingle Values, int Count);
	virtual void __fastcall SetIntArray(const System::PInteger Values, int Count);
	virtual void __fastcall SetUIntArray(const System::PCardinal Values, int Count);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	virtual void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLAbstractShaderUniform(System::Classes::TPersistent* AOwner) : Gls::Baseclasses::TGLUpdateAbleObject(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLAbstractShaderUniform() { }
	
private:
	void *__IShaderParameter;	// Glsl::Shaderparameter::IShaderParameter 
	
public:
	operator Glsl::Shaderparameter::IShaderParameter*(void) { return (Glsl::Shaderparameter::IShaderParameter*)&__IShaderParameter; }
	
};


typedef System::TMetaClass* CGLAbstractShaderUniform;

class PASCALIMPLEMENTATION TGLShaderUniform : public TGLAbstractShaderUniform
{
	typedef TGLAbstractShaderUniform inherited;
	
protected:
	int FLocation;
	unsigned FStoreProgram;
	Glsl::Shaderparameter::TUniformAutoSetMethod FAutoSet;
	unsigned __fastcall GetProgram();
	void __fastcall PushProgram();
	void __fastcall PopProgram();
	virtual float __fastcall GetFloat();
	virtual Gls::Vectortypes::TVector2f __fastcall GetVec2();
	virtual Gls::Vectortypes::TVector3f __fastcall GetVec3();
	virtual Gls::Vectortypes::TVector4f __fastcall GetVec4();
	virtual int __fastcall GetInt();
	virtual Gls::Vectortypes::TVector2i __fastcall GetIVec2();
	virtual Gls::Vectortypes::TVector3i __fastcall GetIVec3();
	virtual Gls::Vectortypes::TVector4i __fastcall GetIVec4();
	virtual unsigned __fastcall GetUInt();
	virtual Gls::Vectortypes::TVector2ui __fastcall GetUVec2();
	virtual Gls::Vectortypes::TVector3ui __fastcall GetUVec3();
	virtual Gls::Vectortypes::TVector4ui __fastcall GetUVec4();
	virtual void __fastcall SetFloat(const float Value);
	virtual void __fastcall SetVec2(const Gls::Vectortypes::TVector2f &Value);
	virtual void __fastcall SetVec3(const Gls::Vectortypes::TVector3f &Value);
	virtual void __fastcall SetVec4(const Gls::Vectortypes::TVector4f &Value);
	virtual void __fastcall SetInt(const int Value);
	virtual void __fastcall SetIVec2(const Gls::Vectortypes::TVector2i &Value);
	virtual void __fastcall SetIVec3(const Gls::Vectortypes::TVector3i &Value);
	virtual void __fastcall SetIVec4(const Gls::Vectortypes::TVector4i &Value);
	virtual void __fastcall SetUInt(const unsigned Value);
	virtual void __fastcall SetUVec2(const Gls::Vectortypes::TVector2ui &Value);
	virtual void __fastcall SetUVec3(const Gls::Vectortypes::TVector3ui &Value);
	virtual void __fastcall SetUVec4(const Gls::Vectortypes::TVector4ui &Value);
	virtual Gls::Vectortypes::TMatrix2f __fastcall GetMat2();
	virtual Gls::Vectortypes::TMatrix3f __fastcall GetMat3();
	virtual Gls::Vectortypes::TMatrix4f __fastcall GetMat4();
	virtual void __fastcall SetMat2(const Gls::Vectortypes::TMatrix2f &Value);
	virtual void __fastcall SetMat3(const Gls::Vectortypes::TMatrix3f &Value);
	virtual void __fastcall SetMat4(const Gls::Vectortypes::TMatrix4f &Value);
	virtual System::UnicodeString __fastcall GetAutoSetMethod();
	virtual void __fastcall SetAutoSetMethod(const System::UnicodeString AValue);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
public:
	virtual void __fastcall SetFloatArray(const System::PSingle Values, int Count);
	virtual void __fastcall SetIntArray(const System::PInteger Values, int Count);
	virtual void __fastcall SetUIntArray(const System::PCardinal Values, int Count);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	__property System::UnicodeString Name = {read=GetName};
	__property int Location = {read=FLocation, nodefault};
	__property Glsl::Shaderparameter::TGLSLDataType GLSLType = {read=GetGLSLType, nodefault};
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLShaderUniform(System::Classes::TPersistent* AOwner) : TGLAbstractShaderUniform(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLShaderUniform() { }
	
private:
	void *__IShaderParameter;	// Glsl::Shaderparameter::IShaderParameter 
	
public:
	operator Glsl::Shaderparameter::IShaderParameter*(void) { return (Glsl::Shaderparameter::IShaderParameter*)&__IShaderParameter; }
	
};


class PASCALIMPLEMENTATION TGLShaderUniformDSA : public TGLShaderUniform
{
	typedef TGLShaderUniform inherited;
	
protected:
	virtual void __fastcall SetFloat(const float Value);
	virtual void __fastcall SetVec2(const Gls::Vectortypes::TVector2f &Value);
	virtual void __fastcall SetVec3(const Gls::Vectortypes::TVector3f &Value);
	virtual void __fastcall SetVec4(const Gls::Vectortypes::TVector4f &Value);
	virtual void __fastcall SetInt(const int Value);
	virtual void __fastcall SetIVec2(const Gls::Vectortypes::TVector2i &Value);
	virtual void __fastcall SetIVec3(const Gls::Vectortypes::TVector3i &Value);
	virtual void __fastcall SetIVec4(const Gls::Vectortypes::TVector4i &Value);
	virtual void __fastcall SetUInt(const unsigned Value);
	virtual void __fastcall SetUVec2(const Gls::Vectortypes::TVector2ui &Value);
	virtual void __fastcall SetUVec3(const Gls::Vectortypes::TVector3ui &Value);
	virtual void __fastcall SetUVec4(const Gls::Vectortypes::TVector4ui &Value);
	virtual void __fastcall SetMat2(const Gls::Vectortypes::TMatrix2f &Value);
	virtual void __fastcall SetMat3(const Gls::Vectortypes::TMatrix3f &Value);
	virtual void __fastcall SetMat4(const Gls::Vectortypes::TMatrix4f &Value);
	
public:
	virtual void __fastcall SetFloatArray(const System::PSingle Values, int Count);
	virtual void __fastcall SetIntArray(const System::PInteger Values, int Count);
	virtual void __fastcall SetUIntArray(const System::PCardinal Values, int Count);
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLShaderUniformDSA(System::Classes::TPersistent* AOwner) : TGLShaderUniform(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLShaderUniformDSA() { }
	
};


class PASCALIMPLEMENTATION TGLShaderUniformTexture : public TGLShaderUniform
{
	typedef TGLShaderUniform inherited;
	
private:
	TGLAbstractTexture* FLibTexture;
	TGLTextureSampler* FLibSampler;
	Gls::Textureformat::TGLTextureTarget FTarget;
	Gls::Textureformat::TSwizzleVector FSwizzling;
	
protected:
	System::UnicodeString FLibTexureName;
	System::UnicodeString FLibSamplerName;
	virtual System::UnicodeString __fastcall GetTextureName();
	virtual System::UnicodeString __fastcall GetSamplerName();
	virtual Gls::Textureformat::TSwizzleVector __fastcall GetTextureSwizzle();
	virtual void __fastcall SetTextureName(const System::UnicodeString AValue);
	virtual void __fastcall SetSamplerName(const System::UnicodeString AValue);
	virtual void __fastcall SetTextureSwizzle(const Gls::Textureformat::TSwizzleVector AValue);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	void __fastcall Loaded();
	
public:
	__fastcall virtual TGLShaderUniformTexture(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLShaderUniformTexture();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Notification(System::TObject* Sender, System::Classes::TOperation Operation);
	virtual void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	__property System::UnicodeString LibTextureName = {read=GetTextureName, write=SetTextureName};
	__property System::UnicodeString LibSamplerName = {read=GetSamplerName, write=SetSamplerName};
	__property Glsl::Shaderparameter::TGLSLSamplerType GLSLSampler = {read=GetGLSLSamplerType, nodefault};
	__property Gls::Textureformat::TSwizzleVector Swizzling = {read=GetTextureSwizzle, write=SetTextureSwizzle};
};


class PASCALIMPLEMENTATION TGLBaseShaderModel : public TGLLibMaterialProperty
{
	typedef TGLLibMaterialProperty inherited;
	
	
private:
	typedef System::StaticArray<System::UnicodeString, 5> _TGLBaseShaderModel__1;
	
	
protected:
	Gls::Context::TGLProgramHandle* FHandle;
	_TGLBaseShaderModel__1 FLibShaderName;
	System::StaticArray<TGLShaderEx*, 5> FShaders;
	bool FIsValid;
	System::UnicodeString FInfoLog;
	Gls::Persistentclasses::TGLPersistentObjectList* FUniforms;
	bool FAutoFill;
	System::UnicodeString __fastcall GetLibShaderName(TGLShaderType AType);
	void __fastcall SetLibShaderName(TGLShaderType AType, const System::UnicodeString AValue);
	Glsl::Shaderparameter::_di_IShaderParameter __fastcall GetUniform(const System::UnicodeString AName);
	__classmethod void __fastcall ReleaseUniforms(Gls::Persistentclasses::TGLPersistentObjectList* AList);
	__property System::UnicodeString LibVertexShaderName = {read=GetLibShaderName, write=SetLibShaderName, index=0};
	__property System::UnicodeString LibFragmentShaderName = {read=GetLibShaderName, write=SetLibShaderName, index=4};
	__property System::UnicodeString LibGeometryShaderName = {read=GetLibShaderName, write=SetLibShaderName, index=3};
	__property System::UnicodeString LibTessEvalShaderName = {read=GetLibShaderName, write=SetLibShaderName, index=2};
	__property System::UnicodeString LibTessControlShaderName = {read=GetLibShaderName, write=SetLibShaderName, index=1};
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadUniforms(System::Classes::TStream* AStream);
	void __fastcall WriteUniforms(System::Classes::TStream* AStream);
	virtual void __fastcall Loaded();
	virtual __classmethod bool __fastcall IsSupported() = 0 ;
	
public:
	__fastcall virtual TGLBaseShaderModel(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLBaseShaderModel();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall Notification(System::TObject* Sender, System::Classes::TOperation Operation);
	void __fastcall DoOnPrepare(Gls::Context::TGLContext* Sender);
	virtual void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	virtual void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall GetUniformNames(System::Classes::TGetStrProc Proc);
	__property Gls::Context::TGLProgramHandle* Handle = {read=FHandle};
	__property bool IsValid = {read=FIsValid, nodefault};
	__property Glsl::Shaderparameter::_di_IShaderParameter Uniforms[const System::UnicodeString AName] = {read=GetUniform};
	
__published:
	__property System::UnicodeString InfoLog = {read=FInfoLog};
	__property bool AutoFillOfUniforms = {read=FAutoFill, write=FAutoFill, stored=false, nodefault};
	__property NextPass = {default=0};
};


class PASCALIMPLEMENTATION TGLShaderModel3 : public TGLBaseShaderModel
{
	typedef TGLBaseShaderModel inherited;
	
public:
	__classmethod virtual bool __fastcall IsSupported();
	
__published:
	__property LibVertexShaderName = {index=0, default=0};
	__property LibFragmentShaderName = {index=4, default=0};
public:
	/* TGLBaseShaderModel.Create */ inline __fastcall virtual TGLShaderModel3(System::Classes::TPersistent* AOwner) : TGLBaseShaderModel(AOwner) { }
	/* TGLBaseShaderModel.Destroy */ inline __fastcall virtual ~TGLShaderModel3() { }
	
};


class PASCALIMPLEMENTATION TGLShaderModel4 : public TGLBaseShaderModel
{
	typedef TGLBaseShaderModel inherited;
	
public:
	__classmethod virtual bool __fastcall IsSupported();
	
__published:
	__property LibVertexShaderName = {index=0, default=0};
	__property LibGeometryShaderName = {index=3, default=0};
	__property LibFragmentShaderName = {index=4, default=0};
public:
	/* TGLBaseShaderModel.Create */ inline __fastcall virtual TGLShaderModel4(System::Classes::TPersistent* AOwner) : TGLBaseShaderModel(AOwner) { }
	/* TGLBaseShaderModel.Destroy */ inline __fastcall virtual ~TGLShaderModel4() { }
	
};


class PASCALIMPLEMENTATION TGLShaderModel5 : public TGLBaseShaderModel
{
	typedef TGLBaseShaderModel inherited;
	
public:
	virtual void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	virtual void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	__classmethod virtual bool __fastcall IsSupported();
	
__published:
	__property LibTessControlShaderName = {index=1, default=0};
	__property LibTessEvalShaderName = {index=2, default=0};
	__property LibVertexShaderName = {index=0, default=0};
	__property LibGeometryShaderName = {index=3, default=0};
	__property LibFragmentShaderName = {index=4, default=0};
public:
	/* TGLBaseShaderModel.Create */ inline __fastcall virtual TGLShaderModel5(System::Classes::TPersistent* AOwner) : TGLBaseShaderModel(AOwner) { }
	/* TGLBaseShaderModel.Destroy */ inline __fastcall virtual ~TGLShaderModel5() { }
	
};


class PASCALIMPLEMENTATION TGLLibMaterialEx : public Gls::Material::TGLAbstractLibMaterial
{
	typedef Gls::Material::TGLAbstractLibMaterial inherited;
	
private:
	Gls::Context::TGLVirtualHandle* FHandle;
	Gls::State::TGLMaterialLevel FApplicableLevel;
	Gls::State::TGLMaterialLevel FSelectedLevel;
	TGLFixedFunctionProperties* FFixedFunc;
	TGLMultitexturingProperties* FMultitexturing;
	TGLShaderModel3* FSM3;
	TGLShaderModel4* FSM4;
	TGLShaderModel5* FSM5;
	TOnAsmProgSetting FOnAsmProgSetting;
	TOnUniformInitialize FOnSM3UniformInit;
	TOnUniformSetting FOnSM3UniformSetting;
	TOnUniformInitialize FOnSM4UniformInit;
	TOnUniformSetting FOnSM4UniformSetting;
	TOnUniformInitialize FOnSM5UniformInit;
	TOnUniformSetting FOnSM5UniformSetting;
	TGLLibMaterialEx* FNextPass;
	bool FStoreAmalgamating;
	void __fastcall SetLevel(Gls::State::TGLMaterialLevel AValue);
	void __fastcall SetFixedFunc(TGLFixedFunctionProperties* AValue);
	void __fastcall SetMultitexturing(TGLMultitexturingProperties* AValue);
	void __fastcall SetSM3(TGLShaderModel3* AValue);
	void __fastcall SetSM4(TGLShaderModel4* AValue);
	void __fastcall SetSM5(TGLShaderModel5* AValue);
	void __fastcall DoAllocate(Gls::Context::TGLVirtualHandle* Sender, unsigned &handle);
	void __fastcall DoDeallocate(Gls::Context::TGLVirtualHandle* Sender, unsigned &handle);
	
protected:
	virtual void __fastcall Loaded();
	void __fastcall RemoveDefferedInit();
	void __fastcall DoOnPrepare(Gls::Context::TGLContext* Sender);
	
public:
	__fastcall virtual TGLLibMaterialEx(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLLibMaterialEx();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	virtual bool __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	virtual bool __fastcall Blended();
	
__published:
	__property Gls::State::TGLMaterialLevel ApplicableLevel = {read=FApplicableLevel, write=SetLevel, default=0};
	__property Gls::State::TGLMaterialLevel SelectedLevel = {read=FSelectedLevel, nodefault};
	__property TGLFixedFunctionProperties* FixedFunction = {read=FFixedFunc, write=SetFixedFunc};
	__property TGLMultitexturingProperties* Multitexturing = {read=FMultitexturing, write=SetMultitexturing};
	__property TGLShaderModel3* ShaderModel3 = {read=FSM3, write=SetSM3};
	__property TGLShaderModel4* ShaderModel4 = {read=FSM4, write=SetSM4};
	__property TGLShaderModel5* ShaderModel5 = {read=FSM5, write=SetSM5};
	__property TOnAsmProgSetting OnAsmProgSetting = {read=FOnAsmProgSetting, write=FOnAsmProgSetting};
	__property TOnUniformInitialize OnSM3UniformInitialize = {read=FOnSM3UniformInit, write=FOnSM3UniformInit};
	__property TOnUniformSetting OnSM3UniformSetting = {read=FOnSM3UniformSetting, write=FOnSM3UniformSetting};
	__property TOnUniformInitialize OnSM4UniformInitialize = {read=FOnSM4UniformInit, write=FOnSM4UniformInit};
	__property TOnUniformSetting OnSM4UniformSetting = {read=FOnSM4UniformSetting, write=FOnSM4UniformSetting};
	__property TOnUniformInitialize OnSM5UniformInitialize = {read=FOnSM5UniformInit, write=FOnSM5UniformInit};
	__property TOnUniformSetting OnSM5UniformSetting = {read=FOnSM5UniformSetting, write=FOnSM5UniformSetting};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLibMaterialsEx : public Gls::Material::TGLAbstractLibMaterials
{
	typedef Gls::Material::TGLAbstractLibMaterials inherited;
	
public:
	TGLLibMaterialEx* operator[](int index) { return this->Items[index]; }
	
protected:
	void __fastcall SetItems(int AIndex, TGLLibMaterialEx* const AValue);
	TGLLibMaterialEx* __fastcall GetItems(int AIndex);
	
public:
	__fastcall TGLLibMaterialsEx(System::Classes::TComponent* AOwner);
	TGLMaterialLibraryEx* __fastcall MaterialLibrary();
	int __fastcall IndexOf(TGLLibMaterialEx* const Item);
	HIDESBASE TGLLibMaterialEx* __fastcall Add();
	HIDESBASE TGLLibMaterialEx* __fastcall FindItemID(int ID);
	__property TGLLibMaterialEx* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	TGLLibMaterialEx* __fastcall GetLibMaterialByName(const System::UnicodeString AName);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLLibMaterialsEx() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMatLibComponents : public Gls::Xcollection::TXCollection
{
	typedef Gls::Xcollection::TXCollection inherited;
	
public:
	TGLBaseMaterialCollectionItem* operator[](int index) { return this->Items[index]; }
	
protected:
	HIDESBASE TGLBaseMaterialCollectionItem* __fastcall GetItems(int index);
	
public:
	DYNAMIC System::UnicodeString __fastcall GetNamePath();
	__classmethod virtual Gls::Xcollection::TXCollectionItemClass __fastcall ItemsClass();
	__property TGLBaseMaterialCollectionItem* Items[int index] = {read=GetItems/*, default*/};
	TGLBaseMaterialCollectionItem* __fastcall GetItemByName(const System::UnicodeString AName);
	TGLAbstractTexture* __fastcall GetTextureByName(const System::UnicodeString AName);
	TGLFrameBufferAttachment* __fastcall GetAttachmentByName(const System::UnicodeString AName);
	TGLTextureSampler* __fastcall GetSamplerByName(const System::UnicodeString AName);
	TGLTextureCombiner* __fastcall GetCombinerByName(const System::UnicodeString AName);
	TGLShaderEx* __fastcall GetShaderByName(const System::UnicodeString AName);
	TGLASMVertexProgram* __fastcall GetAsmProgByName(const System::UnicodeString AName);
	System::UnicodeString __fastcall MakeUniqueName(const System::UnicodeString AName);
public:
	/* TXCollection.Create */ inline __fastcall virtual TGLMatLibComponents(System::Classes::TPersistent* aOwner) : Gls::Xcollection::TXCollection(aOwner) { }
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLMatLibComponents() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLMaterialLibraryEx : public Gls::Material::TGLAbstractMaterialLibrary
{
	typedef Gls::Material::TGLAbstractMaterialLibrary inherited;
	
private:
	TGLMatLibComponents* FComponents;
	
protected:
	virtual void __fastcall Loaded();
	TGLLibMaterialsEx* __fastcall GetMaterials();
	void __fastcall SetMaterials(TGLLibMaterialsEx* AValue);
	bool __fastcall StoreMaterials();
	void __fastcall SetComponents(TGLMatLibComponents* AValue);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall WriteComponents(System::Classes::TStream* AStream);
	void __fastcall ReadComponents(System::Classes::TStream* AStream);
	
public:
	__fastcall virtual TGLMaterialLibraryEx(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMaterialLibraryEx();
	void __fastcall GetNames(System::Classes::TGetStrProc Proc, CGLBaseMaterialCollectionItem AClass)/* overload */;
	TGLTextureImageEx* __fastcall AddTexture(const System::UnicodeString AName);
	TGLFrameBufferAttachment* __fastcall AddAttachment(const System::UnicodeString AName);
	TGLTextureSampler* __fastcall AddSampler(const System::UnicodeString AName);
	TGLTextureCombiner* __fastcall AddCombiner(const System::UnicodeString AName);
	TGLShaderEx* __fastcall AddShader(const System::UnicodeString AName);
	TGLASMVertexProgram* __fastcall AddAsmProg(const System::UnicodeString AName);
	void __fastcall SetLevelForAll(const Gls::State::TGLMaterialLevel ALevel);
	
__published:
	__property TGLLibMaterialsEx* Materials = {read=GetMaterials, write=SetMaterials, stored=StoreMaterials};
	__property TGLMatLibComponents* Components = {read=FComponents, write=SetComponents};
	__property TexturePaths = {default=0};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall RegisterGLMaterialExNameChangeEvent(System::Classes::TNotifyEvent AEvent);
extern DELPHI_PACKAGE void __fastcall DeRegisterGLMaterialExNameChangeEvent(System::Classes::TNotifyEvent AEvent);
}	/* namespace Materialex */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_MATERIALEX)
using namespace Gls::Materialex;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_MaterialexHPP
