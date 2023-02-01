// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Material.pas' rev: 35.00 (Windows)

#ifndef Gls_MaterialHPP
#define Gls_MaterialHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Context.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Color.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.State.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.XOpenGL.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.Utils.hpp>
#include <GLS.Strings.hpp>
#include <GLS.Logger.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Material
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE IGLMaterialLibrarySupported;
typedef System::DelphiInterface<IGLMaterialLibrarySupported> _di_IGLMaterialLibrarySupported;
class DELPHICLASS TGLShader;
class DELPHICLASS TGLFaceProperties;
class DELPHICLASS TGLDepthProperties;
class DELPHICLASS TGLBlendingParameters;
class DELPHICLASS TGLMaterial;
class DELPHICLASS TGLAbstractLibMaterial;
class DELPHICLASS TGLLibMaterial;
class DELPHICLASS TGLAbstractLibMaterials;
class DELPHICLASS TGLLibMaterials;
class DELPHICLASS TGLAbstractMaterialLibrary;
class DELPHICLASS TGLMaterialLibrary;
//-- type declarations -------------------------------------------------------
__interface  INTERFACE_UUID("{8E442AF9-D212-4A5E-8A88-92F798BABFD1}") IGLMaterialLibrarySupported  : public System::IInterface 
{
	virtual TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary() = 0 ;
};

enum DECLSPEC_DENUM TGLShaderStyle : unsigned char { ssHighLevel, ssLowLevel, ssReplace };

enum DECLSPEC_DENUM TGLShaderFailedInitAction : unsigned char { fiaSilentDisable, fiaRaiseStandardException, fiaRaiseHandledException, fiaReRaiseException };

class PASCALIMPLEMENTATION TGLShader : public Gls::Baseclasses::TGLUpdateAbleComponent
{
	typedef Gls::Baseclasses::TGLUpdateAbleComponent inherited;
	
private:
	bool FEnabled;
	System::Classes::TList* FLibMatUsers;
	Gls::Context::TGLVirtualHandle* FVirtualHandle;
	TGLShaderStyle FShaderStyle;
	int FUpdateCount;
	bool FShaderActive;
	TGLShaderFailedInitAction FFailedInitAction;
	
protected:
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoFinalize();
	bool __fastcall GetShaderInitialized();
	void __fastcall InitializeShader(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	void __fastcall FinalizeShader();
	void __fastcall OnVirtualHandleAllocate(Gls::Context::TGLVirtualHandle* sender, unsigned &handle);
	void __fastcall OnVirtualHandleDestroy(Gls::Context::TGLVirtualHandle* sender, unsigned &handle);
	void __fastcall SetEnabled(bool val);
	__property bool ShaderInitialized = {read=GetShaderInitialized, nodefault};
	__property bool ShaderActive = {read=FShaderActive, nodefault};
	void __fastcall RegisterUser(TGLLibMaterial* libMat);
	void __fastcall UnRegisterUser(TGLLibMaterial* libMat);
	virtual void __fastcall HandleFailedInitialization(const System::UnicodeString LastErrorMessage = System::UnicodeString());
	virtual System::UnicodeString __fastcall GetStardardNotSupportedMessage();
	
public:
	__fastcall virtual TGLShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLShader();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall BeginUpdate();
	void __fastcall EndUpdate();
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	bool __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property TGLShaderStyle ShaderStyle = {read=FShaderStyle, write=FShaderStyle, default=1};
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual bool __fastcall ShaderSupported();
	__property TGLShaderFailedInitAction FailedInitAction = {read=FFailedInitAction, write=FFailedInitAction, default=1};
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
};


_DECLARE_METACLASS(System::TMetaClass, TGLShaderClass);

typedef System::Byte TGLShininess;

class PASCALIMPLEMENTATION TGLFaceProperties : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Gls::Color::TGLColor* FAmbient;
	Gls::Color::TGLColor* FDiffuse;
	Gls::Color::TGLColor* FSpecular;
	Gls::Color::TGLColor* FEmission;
	TGLShininess FShininess;
	
protected:
	void __fastcall SetAmbient(Gls::Color::TGLColor* AValue);
	void __fastcall SetDiffuse(Gls::Color::TGLColor* AValue);
	void __fastcall SetEmission(Gls::Color::TGLColor* AValue);
	void __fastcall SetSpecular(Gls::Color::TGLColor* AValue);
	void __fastcall SetShininess(TGLShininess AValue);
	
public:
	__fastcall virtual TGLFaceProperties(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLFaceProperties();
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, Gls::State::TGLCullFaceMode aFace);
	void __fastcall ApplyNoLighting(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, Gls::State::TGLCullFaceMode aFace);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Gls::Color::TGLColor* Ambient = {read=FAmbient, write=SetAmbient};
	__property Gls::Color::TGLColor* Diffuse = {read=FDiffuse, write=SetDiffuse};
	__property Gls::Color::TGLColor* Emission = {read=FEmission, write=SetEmission};
	__property TGLShininess Shininess = {read=FShininess, write=SetShininess, default=0};
	__property Gls::Color::TGLColor* Specular = {read=FSpecular, write=SetSpecular};
};


class PASCALIMPLEMENTATION TGLDepthProperties : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	bool FDepthTest;
	bool FDepthWrite;
	float FZNear;
	float FZFar;
	Gls::State::TGLComparisonFunction FCompareFunc;
	bool FDepthClamp;
	
protected:
	void __fastcall SetZNear(float Value);
	void __fastcall SetZFar(float Value);
	void __fastcall SetCompareFunc(Gls::State::TGLComparisonFunction Value);
	void __fastcall SetDepthTest(bool Value);
	void __fastcall SetDepthWrite(bool Value);
	void __fastcall SetDepthClamp(bool Value);
	bool __fastcall StoreZNear();
	bool __fastcall StoreZFar();
	
public:
	__fastcall virtual TGLDepthProperties(System::Classes::TPersistent* AOwner);
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float ZNear = {read=FZNear, write=SetZNear, stored=StoreZNear};
	__property float ZFar = {read=FZFar, write=SetZFar, stored=StoreZFar};
	__property Gls::State::TGLComparisonFunction DepthCompareFunction = {read=FCompareFunc, write=SetCompareFunc, default=3};
	__property bool DepthTest = {read=FDepthTest, write=SetDepthTest, default=1};
	__property bool DepthWrite = {read=FDepthWrite, write=SetDepthWrite, default=1};
	__property bool DepthClamp = {read=FDepthClamp, write=SetDepthClamp, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLDepthProperties() { }
	
};


typedef System::UnicodeString TGLLibMaterialName;

typedef Gls::State::TGLComparisonFunction TGlAlphaFunc;

class PASCALIMPLEMENTATION TGLBlendingParameters : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	bool FUseAlphaFunc;
	bool FUseBlendFunc;
	bool FSeparateBlendFunc;
	Gls::State::TGLComparisonFunction FAlphaFuncType;
	float FAlphaFuncRef;
	Gls::State::TGLBlendFunction FBlendFuncSFactor;
	Gls::State::TGLBlendFunction FBlendFuncDFactor;
	Gls::State::TGLBlendFunction FAlphaBlendFuncSFactor;
	Gls::State::TGLBlendFunction FAlphaBlendFuncDFactor;
	void __fastcall SetUseAlphaFunc(const bool Value);
	void __fastcall SetUseBlendFunc(const bool Value);
	void __fastcall SetSeparateBlendFunc(const bool Value);
	void __fastcall SetAlphaFuncRef(const float Value);
	void __fastcall SetAlphaFuncType(const Gls::State::TGLComparisonFunction Value);
	void __fastcall SetBlendFuncDFactor(const Gls::State::TGLBlendFunction Value);
	void __fastcall SetBlendFuncSFactor(const Gls::State::TGLBlendFunction Value);
	void __fastcall SetAlphaBlendFuncDFactor(const Gls::State::TGLBlendFunction Value);
	void __fastcall SetAlphaBlendFuncSFactor(const Gls::State::TGLBlendFunction Value);
	bool __fastcall StoreAlphaFuncRef();
	
public:
	__fastcall virtual TGLBlendingParameters(System::Classes::TPersistent* AOwner);
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property bool UseAlphaFunc = {read=FUseAlphaFunc, write=SetUseAlphaFunc, default=0};
	__property Gls::State::TGLComparisonFunction AlphaFunctType = {read=FAlphaFuncType, write=SetAlphaFuncType, default=5};
	__property float AlphaFuncRef = {read=FAlphaFuncRef, write=SetAlphaFuncRef, stored=StoreAlphaFuncRef};
	__property bool UseBlendFunc = {read=FUseBlendFunc, write=SetUseBlendFunc, default=1};
	__property bool SeparateBlendFunc = {read=FSeparateBlendFunc, write=SetSeparateBlendFunc, default=0};
	__property Gls::State::TGLBlendFunction BlendFuncSFactor = {read=FBlendFuncSFactor, write=SetBlendFuncSFactor, default=6};
	__property Gls::State::TGLBlendFunction BlendFuncDFactor = {read=FBlendFuncDFactor, write=SetBlendFuncDFactor, default=7};
	__property Gls::State::TGLBlendFunction AlphaBlendFuncSFactor = {read=FAlphaBlendFuncSFactor, write=SetAlphaBlendFuncSFactor, default=6};
	__property Gls::State::TGLBlendFunction AlphaBlendFuncDFactor = {read=FAlphaBlendFuncDFactor, write=SetAlphaBlendFuncDFactor, default=7};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLBlendingParameters() { }
	
};


enum DECLSPEC_DENUM TGLBlendingMode : unsigned char { bmOpaque, bmTransparency, bmAdditive, bmAlphaTest50, bmAlphaTest100, bmModulate, bmCustom };

enum DECLSPEC_DENUM TGLFaceCulling : unsigned char { fcBufferDefault, fcCull, fcNoCull };

enum DECLSPEC_DENUM TGLMaterialOption : unsigned char { moIgnoreFog, moNoLighting };

typedef System::Set<TGLMaterialOption, TGLMaterialOption::moIgnoreFog, TGLMaterialOption::moNoLighting> TGLMaterialOptions;

class PASCALIMPLEMENTATION TGLMaterial : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	TGLFaceProperties* FFrontProperties;
	TGLFaceProperties* FBackProperties;
	TGLDepthProperties* FDepthProperties;
	TGLBlendingMode FBlendingMode;
	TGLBlendingParameters* FBlendingParams;
	Gls::Texture::TGLTexture* FTexture;
	Gls::Texture::TGLTextureEx* FTextureEx;
	TGLAbstractMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FLibMaterialName;
	TGLMaterialOptions FMaterialOptions;
	TGLFaceCulling FFaceCulling;
	Gls::State::TGLPolygonMode FPolygonMode;
	TGLAbstractLibMaterial* currentLibMaterial;
	TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
protected:
	TGLFaceProperties* __fastcall GetBackProperties();
	void __fastcall SetBackProperties(TGLFaceProperties* Values);
	void __fastcall SetFrontProperties(TGLFaceProperties* Values);
	void __fastcall SetDepthProperties(TGLDepthProperties* Values);
	void __fastcall SetBlendingMode(const TGLBlendingMode val);
	void __fastcall SetMaterialOptions(const TGLMaterialOptions val);
	Gls::Texture::TGLTexture* __fastcall GetTexture();
	void __fastcall SetTexture(Gls::Texture::TGLTexture* ATexture);
	void __fastcall SetMaterialLibrary(TGLAbstractMaterialLibrary* const val);
	void __fastcall SetLibMaterialName(const System::UnicodeString val);
	void __fastcall SetFaceCulling(const TGLFaceCulling val);
	void __fastcall SetPolygonMode(Gls::State::TGLPolygonMode AValue);
	Gls::Texture::TGLTextureEx* __fastcall GetTextureEx();
	void __fastcall SetTextureEx(Gls::Texture::TGLTextureEx* const value);
	bool __fastcall StoreTextureEx();
	void __fastcall SetBlendingParams(TGLBlendingParameters* const Value);
	void __fastcall NotifyLibMaterialDestruction();
	bool __fastcall StoreMaterialProps();
	
public:
	__fastcall virtual TGLMaterial(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLMaterial();
	void __fastcall PrepareBuildList();
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	bool __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall NotifyTexMapChange(System::TObject* Sender);
	void __fastcall DestroyHandles();
	void __fastcall Loaded();
	bool __fastcall Blended();
	bool __fastcall HasSecondaryTexture();
	bool __fastcall MaterialIsLinkedToLib();
	Gls::Texture::TGLTexture* __fastcall GetActualPrimaryTexture();
	TGLMaterial* __fastcall GetActualPrimaryMaterial();
	TGLLibMaterial* __fastcall GetLibMaterial();
	void __fastcall QuickAssignMaterial(TGLMaterialLibrary* const MaterialLibrary, TGLLibMaterial* const Material);
	
__published:
	__property TGLFaceProperties* BackProperties = {read=GetBackProperties, write=SetBackProperties, stored=StoreMaterialProps};
	__property TGLFaceProperties* FrontProperties = {read=FFrontProperties, write=SetFrontProperties, stored=StoreMaterialProps};
	__property TGLDepthProperties* DepthProperties = {read=FDepthProperties, write=SetDepthProperties, stored=StoreMaterialProps};
	__property TGLBlendingMode BlendingMode = {read=FBlendingMode, write=SetBlendingMode, stored=StoreMaterialProps, default=0};
	__property TGLBlendingParameters* BlendingParams = {read=FBlendingParams, write=SetBlendingParams};
	__property TGLMaterialOptions MaterialOptions = {read=FMaterialOptions, write=SetMaterialOptions, default=0};
	__property Gls::Texture::TGLTexture* Texture = {read=GetTexture, write=SetTexture, stored=StoreMaterialProps};
	__property TGLFaceCulling FaceCulling = {read=FFaceCulling, write=SetFaceCulling, default=0};
	__property TGLAbstractMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString LibMaterialName = {read=FLibMaterialName, write=SetLibMaterialName};
	__property Gls::Texture::TGLTextureEx* TextureEx = {read=GetTextureEx, write=SetTextureEx, stored=StoreTextureEx};
	__property Gls::State::TGLPolygonMode PolygonMode = {read=FPolygonMode, write=SetPolygonMode, default=0};
private:
	void *__IGLTextureNotifyAble;	// Gls::Texture::IGLTextureNotifyAble 
	void *__IGLMaterialLibrarySupported;	// IGLMaterialLibrarySupported 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0D9DC0B0-ECE4-4513-A8A1-5AE7022C9426}
	operator Gls::Texture::_di_IGLTextureNotifyAble()
	{
		Gls::Texture::_di_IGLTextureNotifyAble intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gls::Texture::IGLTextureNotifyAble*(void) { return (Gls::Texture::IGLTextureNotifyAble*)&__IGLTextureNotifyAble; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator _di_IGLMaterialLibrarySupported()
	{
		_di_IGLMaterialLibrarySupported intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IGLMaterialLibrarySupported*(void) { return (IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLAbstractLibMaterial : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
protected:
	System::Classes::TList* FUserList;
	System::UnicodeString FName;
	int FNameHashKey;
	int FTag;
	bool FNotifying;
	TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	HRESULT __stdcall QueryInterface(const GUID &IID, /* out */ void *Obj);
	int __stdcall _AddRef();
	int __stdcall _Release();
	virtual System::UnicodeString __fastcall GetDisplayName();
	__classmethod int __fastcall ComputeNameHashKey(const System::UnicodeString name);
	void __fastcall SetName(const System::UnicodeString val);
	virtual void __fastcall Loaded();
	
public:
	__fastcall virtual TGLAbstractLibMaterial(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLAbstractLibMaterial();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	virtual bool __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall RegisterUser(Gls::Baseclasses::TGLUpdateAbleObject* obj)/* overload */;
	void __fastcall UnregisterUser(Gls::Baseclasses::TGLUpdateAbleObject* obj)/* overload */;
	void __fastcall RegisterUser(Gls::Baseclasses::TGLUpdateAbleComponent* comp)/* overload */;
	void __fastcall UnregisterUser(Gls::Baseclasses::TGLUpdateAbleComponent* comp)/* overload */;
	void __fastcall RegisterUser(TGLLibMaterial* libMaterial)/* overload */;
	void __fastcall UnregisterUser(TGLLibMaterial* libMaterial)/* overload */;
	void __fastcall NotifyUsers();
	bool __fastcall IsUsed();
	__property int NameHashKey = {read=FNameHashKey, nodefault};
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual bool __fastcall Blended();
	__property TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary};
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property int Tag = {read=FTag, write=FTag, nodefault};
private:
	void *__IGLNotifyAble;	// Gls::Baseclasses::IGLNotifyAble 
	void *__IGLMaterialLibrarySupported;	// IGLMaterialLibrarySupported 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00079A6C-D46E-4126-86EE-F9E2951B4593}
	operator Gls::Baseclasses::_di_IGLNotifyAble()
	{
		Gls::Baseclasses::_di_IGLNotifyAble intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gls::Baseclasses::IGLNotifyAble*(void) { return (Gls::Baseclasses::IGLNotifyAble*)&__IGLNotifyAble; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator _di_IGLMaterialLibrarySupported()
	{
		_di_IGLMaterialLibrarySupported intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IGLMaterialLibrarySupported*(void) { return (IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLLibMaterial : public TGLAbstractLibMaterial
{
	typedef TGLAbstractLibMaterial inherited;
	
private:
	TGLMaterial* FMaterial;
	Gls::Coordinates::TGLCoordinates3* FTextureOffset;
	Gls::Coordinates::TGLCoordinates3* FTextureScale;
	float FTextureRotate;
	bool FTextureMatrixIsIdentity;
	bool FTextureOverride;
	Gls::Vectortypes::TMatrix4f FTextureMatrix;
	System::UnicodeString FTexture2Name;
	TGLShader* FShader;
	TGLLibMaterial* libMatTexture2;
	
protected:
	virtual void __fastcall Loaded();
	void __fastcall SetMaterial(TGLMaterial* const val);
	void __fastcall SetTextureOffset(Gls::Coordinates::TGLCoordinates3* const val);
	void __fastcall SetTextureScale(Gls::Coordinates::TGLCoordinates3* const val);
	void __fastcall SetTextureMatrix(const Gls::Vectortypes::TMatrix4f &Value);
	void __fastcall SetTexture2Name(const System::UnicodeString val);
	void __fastcall SetShader(TGLShader* const val);
	void __fastcall SetTextureRotate(float Value);
	bool __fastcall StoreTextureRotate();
	void __fastcall CalculateTextureMatrix();
	void __fastcall DestroyHandles();
	void __fastcall DoOnTextureNeeded(System::TObject* Sender, System::UnicodeString &textureFileName);
	void __fastcall OnNotifyChange(System::TObject* Sender);
	
public:
	__fastcall virtual TGLLibMaterial(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLLibMaterial();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall PrepareBuildList();
	virtual void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	virtual bool __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall NotifyUsersOfTexMapChange();
	__property Gls::Vectortypes::TMatrix4f TextureMatrix = {read=FTextureMatrix, write=SetTextureMatrix};
	__property bool TextureMatrixIsIdentity = {read=FTextureMatrixIsIdentity, nodefault};
	void __fastcall NotifyTexMapChange(System::TObject* Sender);
	virtual bool __fastcall Blended();
	
__published:
	__property TGLMaterial* Material = {read=FMaterial, write=SetMaterial};
	__property Gls::Coordinates::TGLCoordinates3* TextureOffset = {read=FTextureOffset, write=SetTextureOffset};
	__property Gls::Coordinates::TGLCoordinates3* TextureScale = {read=FTextureScale, write=SetTextureScale};
	__property float TextureRotate = {read=FTextureRotate, write=SetTextureRotate, stored=StoreTextureRotate};
	__property System::UnicodeString Texture2Name = {read=FTexture2Name, write=SetTexture2Name};
	__property TGLShader* Shader = {read=FShader, write=SetShader};
private:
	void *__IGLTextureNotifyAble;	// Gls::Texture::IGLTextureNotifyAble 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0D9DC0B0-ECE4-4513-A8A1-5AE7022C9426}
	operator Gls::Texture::_di_IGLTextureNotifyAble()
	{
		Gls::Texture::_di_IGLTextureNotifyAble intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gls::Texture::IGLTextureNotifyAble*(void) { return (Gls::Texture::IGLTextureNotifyAble*)&__IGLTextureNotifyAble; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLAbstractLibMaterials : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
protected:
	void __fastcall Loaded();
	TGLAbstractLibMaterial* __fastcall GetMaterial(const System::UnicodeString AName);
	
public:
	System::UnicodeString __fastcall MakeUniqueName(const System::UnicodeString nameRoot);
public:
	/* TOwnedCollection.Create */ inline __fastcall TGLAbstractLibMaterials(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLAbstractLibMaterials() { }
	
};


class PASCALIMPLEMENTATION TGLLibMaterials : public TGLAbstractLibMaterials
{
	typedef TGLAbstractLibMaterials inherited;
	
public:
	TGLLibMaterial* operator[](int index) { return this->Items[index]; }
	
protected:
	void __fastcall SetItems(int index, TGLLibMaterial* const val);
	TGLLibMaterial* __fastcall GetItems(int index);
	void __fastcall DestroyHandles();
	
public:
	__fastcall TGLLibMaterials(System::Classes::TComponent* AOwner);
	HIDESBASE System::Classes::TPersistent* __fastcall Owner();
	int __fastcall IndexOf(TGLLibMaterial* const Item);
	HIDESBASE TGLLibMaterial* __fastcall Add();
	HIDESBASE TGLLibMaterial* __fastcall FindItemID(int ID);
	__property TGLLibMaterial* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	TGLLibMaterial* __fastcall GetLibMaterialByName(const System::UnicodeString AName);
	int __fastcall GetTextureIndex(Gls::Texture::TGLTexture* const Texture);
	int __fastcall GetMaterialIndex(TGLMaterial* const Material);
	System::UnicodeString __fastcall GetNameOfTexture(Gls::Texture::TGLTexture* const Texture);
	System::UnicodeString __fastcall GetNameOfLibMaterial(TGLLibMaterial* const Material);
	void __fastcall PrepareBuildList();
	void __fastcall DeleteUnusedMaterials();
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLLibMaterials() { }
	
};


class PASCALIMPLEMENTATION TGLAbstractMaterialLibrary : public Gls::Baseclasses::TGLCadenceAbleComponent
{
	typedef Gls::Baseclasses::TGLCadenceAbleComponent inherited;
	
protected:
	TGLAbstractLibMaterials* FMaterials;
	TGLAbstractLibMaterial* FLastAppliedMaterial;
	System::UnicodeString FTexturePaths;
	System::Classes::TStringList* FTexturePathList;
	void __fastcall SetTexturePaths(const System::UnicodeString val);
	__property System::UnicodeString TexturePaths = {read=FTexturePaths, write=SetTexturePaths};
	virtual void __fastcall Loaded();
	
public:
	void __fastcall SetNamesToTStrings(System::Classes::TStrings* AStrings);
	bool __fastcall ApplyMaterial(const System::UnicodeString AName, Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	bool __fastcall UnApplyMaterial(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
public:
	/* TComponent.Create */ inline __fastcall virtual TGLAbstractMaterialLibrary(System::Classes::TComponent* AOwner) : Gls::Baseclasses::TGLCadenceAbleComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TGLAbstractMaterialLibrary() { }
	
};


class PASCALIMPLEMENTATION TGLMaterialLibrary : public TGLAbstractMaterialLibrary
{
	typedef TGLAbstractMaterialLibrary inherited;
	
private:
	bool FDoNotClearMaterialsOnLoad;
	Gls::Texture::TGLTextureNeededEvent FOnTextureNeeded;
	
protected:
	TGLLibMaterials* __fastcall GetMaterials();
	void __fastcall SetMaterials(TGLLibMaterials* const val);
	bool __fastcall StoreMaterials();
	
public:
	__fastcall virtual TGLMaterialLibrary(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMaterialLibrary();
	void __fastcall DestroyHandles();
	void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* writer);
	void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* reader);
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	void __fastcall AddMaterialsFromStream(System::Classes::TStream* aStream);
	void __fastcall SaveToFile(const System::UnicodeString fileName);
	void __fastcall LoadFromFile(const System::UnicodeString fileName);
	void __fastcall AddMaterialsFromFile(const System::UnicodeString fileName);
	TGLLibMaterial* __fastcall AddTextureMaterial(const System::UnicodeString materialName, const System::UnicodeString fileName, bool persistent = true)/* overload */;
	TGLLibMaterial* __fastcall AddTextureMaterial(const System::UnicodeString materialName, Vcl::Graphics::TGraphic* graphic)/* overload */;
	TGLLibMaterial* __fastcall LibMaterialByName(const System::UnicodeString AName);
	Gls::Texture::TGLTexture* __fastcall TextureByName(const System::UnicodeString LibMatName);
	System::UnicodeString __fastcall GetNameOfTexture(Gls::Texture::TGLTexture* const Texture);
	System::UnicodeString __fastcall GetNameOfLibMaterial(TGLLibMaterial* const LibMat);
	
__published:
	__property TGLLibMaterials* Materials = {read=GetMaterials, write=SetMaterials, stored=StoreMaterials};
	__property Gls::Texture::TGLTextureNeededEvent OnTextureNeeded = {read=FOnTextureNeeded, write=FOnTextureNeeded};
	__property TexturePaths = {default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Material */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_MATERIAL)
using namespace Gls::Material;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_MaterialHPP
