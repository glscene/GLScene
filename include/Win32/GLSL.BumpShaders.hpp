// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.BumpShaders.pas' rev: 35.00 (Windows)

#ifndef Glsl_BumpshadersHPP
#define Glsl_BumpshadersHPP

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
#include <GLS.OpenGLTokens.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Material.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.Color.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Context.hpp>
#include <GLS.Cadencer.hpp>
#include <GLS.Strings.hpp>
#include <GLSL.Shader.hpp>
#include <GLSL.CustomShader.hpp>
#include <GLS.Utils.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Bumpshaders
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLSLBumpShaderException;
class DELPHICLASS TGLBaseCustomGLSLBumpShader;
class DELPHICLASS TGLBaseCustomGLSLBumpShaderMT;
class DELPHICLASS TGLCustomGLSLBumpShaderAM;
class DELPHICLASS TGLCustomGLSLBumpShaderMT;
class DELPHICLASS TGLCustomGLSLBumpShader;
class DELPHICLASS TGLCustomGLSLMLBumpShader;
class DELPHICLASS TGLCustomGLSLMLBumpShaderMT;
class DELPHICLASS TGLSLBumpShaderMT;
class DELPHICLASS TGLSLBumpShader;
class DELPHICLASS TGLSLBumpShaderAM;
class DELPHICLASS TGLSLMLBumpShader;
class DELPHICLASS TGLSLMLBumpShaderMT;
class DELPHICLASS TGLBumpShader;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TBumpMethod : unsigned char { bmDot3TexCombiner, bmBasicARBFP };

enum DECLSPEC_DENUM TBumpSpace : unsigned char { bsObject, bsTangentExternal, bsTangentQuaternion };

enum DECLSPEC_DENUM TBumpOption : unsigned char { boDiffuseTexture2, boSpecularTexture3, boUseSecondaryTexCoords, boLightAttenuation, boParallaxMapping };

typedef System::Set<TBumpOption, TBumpOption::boDiffuseTexture2, TBumpOption::boParallaxMapping> TBumpOptions;

enum DECLSPEC_DENUM TSpecularMode : unsigned char { smOff, smBlinn, smPhong };

#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLSLBumpShaderException : public Glsl::Shader::EGLSLShaderException
{
	typedef Glsl::Shader::EGLSLShaderException inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLSLBumpShaderException(const System::UnicodeString Msg) : Glsl::Shader::EGLSLShaderException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLSLBumpShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : Glsl::Shader::EGLSLShaderException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLBumpShaderException(NativeUInt Ident)/* overload */ : Glsl::Shader::EGLSLShaderException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLBumpShaderException(System::PResStringRec ResStringRec)/* overload */ : Glsl::Shader::EGLSLShaderException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLBumpShaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Glsl::Shader::EGLSLShaderException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLBumpShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Glsl::Shader::EGLSLShaderException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLSLBumpShaderException(const System::UnicodeString Msg, int AHelpContext) : Glsl::Shader::EGLSLShaderException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLSLBumpShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Glsl::Shader::EGLSLShaderException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLBumpShaderException(NativeUInt Ident, int AHelpContext)/* overload */ : Glsl::Shader::EGLSLShaderException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLBumpShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Glsl::Shader::EGLSLShaderException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLBumpShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Glsl::Shader::EGLSLShaderException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLBumpShaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Glsl::Shader::EGLSLShaderException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLSLBumpShaderException() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLBaseCustomGLSLBumpShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	float FBumpHeight;
	int FBumpSmoothness;
	float FSpecularPower;
	float FSpecularSpread;
	float FLightPower;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	Gls::Texture::TGLTexture* FNormalTexture;
	Gls::Texture::TGLTexture* FSpecularTexture;
	System::UnicodeString FNormalTextureName;
	System::UnicodeString FSpecularTextureName;
	System::UnicodeString __fastcall GetNormalTextureName();
	System::UnicodeString __fastcall GetSpecularTextureName();
	void __fastcall SetNormalTextureName(const System::UnicodeString Value);
	void __fastcall SetSpecularTextureName(const System::UnicodeString Value);
	void __fastcall SetSpecularTexture(Gls::Texture::TGLTexture* const Value);
	void __fastcall SetNormalTexture(Gls::Texture::TGLTexture* const Value);
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLBaseCustomGLSLBumpShader(System::Classes::TComponent* AOwner);
	__property float BumpHeight = {read=FBumpHeight, write=FBumpHeight};
	__property int BumpSmoothness = {read=FBumpSmoothness, write=FBumpSmoothness, nodefault};
	__property float SpecularPower = {read=FSpecularPower, write=FSpecularPower};
	__property float SpecularSpread = {read=FSpecularSpread, write=FSpecularSpread};
	__property float LightPower = {read=FLightPower, write=FLightPower};
	__property Gls::Texture::TGLTexture* NormalTexture = {read=FNormalTexture, write=SetNormalTexture};
	__property Gls::Texture::TGLTexture* SpecularTexture = {read=FSpecularTexture, write=SetSpecularTexture};
	__property System::UnicodeString NormalTextureName = {read=GetNormalTextureName, write=SetNormalTextureName};
	__property System::UnicodeString SpecularTextureName = {read=GetSpecularTextureName, write=SetSpecularTextureName};
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLBaseCustomGLSLBumpShader() { }
	
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


class PASCALIMPLEMENTATION TGLBaseCustomGLSLBumpShaderMT : public TGLBaseCustomGLSLBumpShader
{
	typedef TGLBaseCustomGLSLBumpShader inherited;
	
private:
	Gls::Texture::TGLTexture* FMainTexture;
	System::UnicodeString FMainTextureName;
	System::UnicodeString __fastcall GetMainTextureName();
	void __fastcall SetMainTextureName(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__property Gls::Texture::TGLTexture* MainTexture = {read=FMainTexture, write=FMainTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTextureName, write=SetMainTextureName};
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLBaseCustomGLSLBumpShaderMT(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLBumpShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLBaseCustomGLSLBumpShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLBumpShaderAM : public TGLBaseCustomGLSLBumpShaderMT
{
	typedef TGLBaseCustomGLSLBumpShaderMT inherited;
	
private:
	Gls::Color::TGLColor* FAmbientColor;
	Gls::Color::TGLColor* FDiffuseColor;
	Gls::Color::TGLColor* FSpecularColor;
	float __fastcall GetAlpha();
	void __fastcall SetAlpha(const float Value);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLBumpShaderAM(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLBumpShaderAM();
	__property Gls::Color::TGLColor* AmbientColor = {read=FAmbientColor};
	__property Gls::Color::TGLColor* DiffuseColor = {read=FDiffuseColor};
	__property Gls::Color::TGLColor* SpecularColor = {read=FSpecularColor};
	__property float Alpha = {read=GetAlpha, write=SetAlpha};
};


class PASCALIMPLEMENTATION TGLCustomGLSLBumpShaderMT : public TGLBaseCustomGLSLBumpShaderMT
{
	typedef TGLBaseCustomGLSLBumpShaderMT inherited;
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLCustomGLSLBumpShaderMT(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLBumpShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLBumpShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLBumpShader : public TGLBaseCustomGLSLBumpShader
{
	typedef TGLBaseCustomGLSLBumpShader inherited;
	
private:
	void __fastcall SetShaderTextures(Gls::Texture::TGLTexture* const *Textures, const int Textures_High);
	void __fastcall GetShaderTextures(Gls::Texture::TGLTexture* *Textures, const int Textures_High);
	void __fastcall SetShaderColorParams(const Gls::Vectortypes::TVector4f &AAmbientColor, const Gls::Vectortypes::TVector4f &ADiffuseColor, const Gls::Vectortypes::TVector4f &ASpecularcolor);
	void __fastcall GetShaderColorParams(Gls::Vectortypes::TVector4f &AAmbientColor, Gls::Vectortypes::TVector4f &ADiffuseColor, Gls::Vectortypes::TVector4f &ASpecularcolor);
	void __fastcall SetShaderMiscParameters(Gls::Cadencer::TGLCadencer* const ACadencer, Gls::Material::TGLMaterialLibrary* const AMatLib, const Glsl::Customshader::TGLLightSourceSet ALightSources);
	void __fastcall GetShaderMiscParameters(Gls::Cadencer::TGLCadencer* &ACadencer, Gls::Material::TGLMaterialLibrary* &AMatLib, Glsl::Customshader::TGLLightSourceSet &ALightSources);
	float __fastcall GetShaderAlpha();
	void __fastcall SetShaderAlpha(const float Value);
	System::UnicodeString __fastcall GetShaderDescription();
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLCustomGLSLBumpShader(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLBumpShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLBumpShader() { }
	
private:
	void *__IGLShaderDescription;	// Glsl::Customshader::IGLShaderDescription 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {04089C64-60C2-43F5-AC9C-38ED46264812}
	operator Glsl::Customshader::_di_IGLShaderDescription()
	{
		Glsl::Customshader::_di_IGLShaderDescription intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glsl::Customshader::IGLShaderDescription*(void) { return (Glsl::Customshader::IGLShaderDescription*)&__IGLShaderDescription; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLMLBumpShader : public TGLBaseCustomGLSLBumpShader
{
	typedef TGLBaseCustomGLSLBumpShader inherited;
	
private:
	Glsl::Customshader::TGLLightSourceSet FLightSources;
	float FLightCompensation;
	void __fastcall SetLightSources(const Glsl::Customshader::TGLLightSourceSet Value);
	void __fastcall SetLightCompensation(const float Value);
	void __fastcall SetShaderTextures(Gls::Texture::TGLTexture* const *Textures, const int Textures_High);
	void __fastcall GetShaderTextures(Gls::Texture::TGLTexture* *Textures, const int Textures_High);
	void __fastcall SetShaderColorParams(const Gls::Vectortypes::TVector4f &AAmbientColor, const Gls::Vectortypes::TVector4f &ADiffuseColor, const Gls::Vectortypes::TVector4f &ASpecularcolor);
	void __fastcall GetShaderColorParams(Gls::Vectortypes::TVector4f &AAmbientColor, Gls::Vectortypes::TVector4f &ADiffuseColor, Gls::Vectortypes::TVector4f &ASpecularcolor);
	void __fastcall SetShaderMiscParameters(Gls::Cadencer::TGLCadencer* const ACadencer, Gls::Material::TGLMaterialLibrary* const AMatLib, const Glsl::Customshader::TGLLightSourceSet ALightSources);
	void __fastcall GetShaderMiscParameters(Gls::Cadencer::TGLCadencer* &ACadencer, Gls::Material::TGLMaterialLibrary* &AMatLib, Glsl::Customshader::TGLLightSourceSet &ALightSources);
	float __fastcall GetShaderAlpha();
	void __fastcall SetShaderAlpha(const float Value);
	System::UnicodeString __fastcall GetShaderDescription();
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLMLBumpShader(System::Classes::TComponent* AOwner);
	__property Glsl::Customshader::TGLLightSourceSet LightSources = {read=FLightSources, write=SetLightSources, default=2};
	__property float LightCompensation = {read=FLightCompensation, write=SetLightCompensation};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLMLBumpShader() { }
	
private:
	void *__IGLShaderDescription;	// Glsl::Customshader::IGLShaderDescription 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {04089C64-60C2-43F5-AC9C-38ED46264812}
	operator Glsl::Customshader::_di_IGLShaderDescription()
	{
		Glsl::Customshader::_di_IGLShaderDescription intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glsl::Customshader::IGLShaderDescription*(void) { return (Glsl::Customshader::IGLShaderDescription*)&__IGLShaderDescription; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLMLBumpShaderMT : public TGLBaseCustomGLSLBumpShaderMT
{
	typedef TGLBaseCustomGLSLBumpShaderMT inherited;
	
private:
	Glsl::Customshader::TGLLightSourceSet FLightSources;
	float FLightCompensation;
	void __fastcall SetLightSources(const Glsl::Customshader::TGLLightSourceSet Value);
	void __fastcall SetLightCompensation(const float Value);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLMLBumpShaderMT(System::Classes::TComponent* AOwner);
	__property Glsl::Customshader::TGLLightSourceSet LightSources = {read=FLightSources, write=SetLightSources, default=2};
	__property float LightCompensation = {read=FLightCompensation, write=SetLightCompensation};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLMLBumpShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLSLBumpShaderMT : public TGLCustomGLSLBumpShaderMT
{
	typedef TGLCustomGLSLBumpShaderMT inherited;
	
__published:
	__property MainTextureName = {default=0};
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLSLBumpShaderMT(System::Classes::TComponent* AOwner) : TGLCustomGLSLBumpShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLBumpShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLSLBumpShader : public TGLCustomGLSLBumpShader
{
	typedef TGLCustomGLSLBumpShader inherited;
	
__published:
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLSLBumpShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLBumpShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLBumpShader() { }
	
};


class PASCALIMPLEMENTATION TGLSLBumpShaderAM : public TGLCustomGLSLBumpShaderAM
{
	typedef TGLCustomGLSLBumpShaderAM inherited;
	
__published:
	__property AmbientColor;
	__property DiffuseColor;
	__property SpecularColor;
	__property Alpha = {stored=false, default=0};
	__property MainTextureName = {default=0};
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
public:
	/* TGLCustomGLSLBumpShaderAM.Create */ inline __fastcall virtual TGLSLBumpShaderAM(System::Classes::TComponent* AOwner) : TGLCustomGLSLBumpShaderAM(AOwner) { }
	/* TGLCustomGLSLBumpShaderAM.Destroy */ inline __fastcall virtual ~TGLSLBumpShaderAM() { }
	
};


class PASCALIMPLEMENTATION TGLSLMLBumpShader : public TGLCustomGLSLMLBumpShader
{
	typedef TGLCustomGLSLMLBumpShader inherited;
	
__published:
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
	__property LightSources = {default=2};
	__property LightCompensation = {default=0};
public:
	/* TGLCustomGLSLMLBumpShader.Create */ inline __fastcall virtual TGLSLMLBumpShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLMLBumpShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLMLBumpShader() { }
	
};


class PASCALIMPLEMENTATION TGLSLMLBumpShaderMT : public TGLCustomGLSLMLBumpShaderMT
{
	typedef TGLCustomGLSLMLBumpShaderMT inherited;
	
__published:
	__property MainTextureName = {default=0};
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
	__property LightSources = {default=2};
	__property LightCompensation = {default=0};
public:
	/* TGLCustomGLSLMLBumpShaderMT.Create */ inline __fastcall virtual TGLSLMLBumpShaderMT(System::Classes::TComponent* AOwner) : TGLCustomGLSLMLBumpShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLMLBumpShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLBumpShader : public Gls::Material::TGLShader
{
	typedef Gls::Material::TGLShader inherited;
	
private:
	Gls::Context::TGLARBVertexProgramHandle* FVertexProgramHandle;
	Gls::Context::TGLARBFragmentProgramHandle* FFragmentProgramHandle;
	Gls::Vectorlists::TGLIntegerList* FLightIDs;
	int FLightsEnabled;
	TBumpMethod FBumpMethod;
	TBumpSpace FBumpSpace;
	TBumpOptions FBumpOptions;
	TSpecularMode FSpecularMode;
	bool FDesignTimeEnabled;
	bool FAmbientPass;
	bool FDiffusePass;
	System::Classes::TStringList* FVertexProgram;
	System::Classes::TStringList* FFragmentProgram;
	float FParallaxOffset;
	System::UnicodeString __fastcall GenerateVertexProgram();
	System::UnicodeString __fastcall GenerateFragmentProgram();
	void __fastcall DoLightPass(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, unsigned lightID);
	
protected:
	void __fastcall SetBumpMethod(const TBumpMethod Value);
	void __fastcall SetBumpSpace(const TBumpSpace Value);
	void __fastcall SetBumpOptions(const TBumpOptions Value);
	void __fastcall SetSpecularMode(const TSpecularMode Value);
	void __fastcall SetDesignTimeEnabled(const bool Value);
	void __fastcall SetParallaxOffset(const float Value);
	virtual void __fastcall Loaded();
	void __fastcall DeleteVertexPrograms();
	void __fastcall DeleteFragmentPrograms();
	
public:
	__fastcall virtual TGLBumpShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBumpShader();
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property TBumpMethod BumpMethod = {read=FBumpMethod, write=SetBumpMethod, nodefault};
	__property TBumpSpace BumpSpace = {read=FBumpSpace, write=SetBumpSpace, nodefault};
	__property TBumpOptions BumpOptions = {read=FBumpOptions, write=SetBumpOptions, nodefault};
	__property TSpecularMode SpecularMode = {read=FSpecularMode, write=SetSpecularMode, nodefault};
	__property bool DesignTimeEnabled = {read=FDesignTimeEnabled, write=SetDesignTimeEnabled, nodefault};
	__property float ParallaxOffset = {read=FParallaxOffset, write=SetParallaxOffset};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Bumpshaders */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_BUMPSHADERS)
using namespace Glsl::Bumpshaders;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_BumpshadersHPP
