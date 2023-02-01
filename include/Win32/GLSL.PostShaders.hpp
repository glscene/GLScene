// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.PostShaders.pas' rev: 35.00 (Windows)

#ifndef Glsl_PostshadersHPP
#define Glsl_PostshadersHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Scene.hpp>
#include <GLS.State.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.Context.hpp>
#include <GLS.Material.hpp>
#include <GLSL.Shader.hpp>
#include <GLSL.CustomShader.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Postshaders
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomGLSLPostBlurShader;
class DELPHICLASS TGLSLPostBlurShader;
class DELPHICLASS TGLCustomGLSLPostThermalVisionShader;
class DELPHICLASS TGLSLPostThermalVisionShader;
class DELPHICLASS TGLCustomGLSLPostDreamVisionShader;
class DELPHICLASS TGLSLPostDreamVisionShader;
class DELPHICLASS TGLCustomGLSLPostNightVisionShader;
class DELPHICLASS TGLSLPostNightVisionShader;
class DELPHICLASS TGLCustomGLSLPostPixelateShader;
class DELPHICLASS TGLSLPostPixelateShader;
class DELPHICLASS TGLCustomGLSLPostPosterizeShader;
class DELPHICLASS TGLSLPostPosterizeShader;
class DELPHICLASS TGLCustomGLSLPostFrostShader;
class DELPHICLASS TGLSLPostFrostShader;
class DELPHICLASS TGLCustomGLSLPostTroubleShader;
class DELPHICLASS TGLSLPostTroubleShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCustomGLSLPostBlurShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	float FThreshold;
	void __fastcall DoUseTempTexture(Gls::Context::TGLTextureHandle* const TempTexture, Gls::Textureformat::TGLTextureTarget TextureTarget);
	Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	bool __fastcall StoreThreshold();
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLPostBlurShader(System::Classes::TComponent* AOwner);
	__property float Threshold = {read=FThreshold, write=FThreshold, stored=StoreThreshold};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLPostBlurShader() { }
	
private:
	void *__IGLPostShader;	// Glsl::Customshader::IGLPostShader 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}
	operator Glsl::Customshader::_di_IGLPostShader()
	{
		Glsl::Customshader::_di_IGLPostShader intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glsl::Customshader::IGLPostShader*(void) { return (Glsl::Customshader::IGLPostShader*)&__IGLPostShader; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLSLPostBlurShader : public TGLCustomGLSLPostBlurShader
{
	typedef TGLCustomGLSLPostBlurShader inherited;
	
__published:
	__property Threshold = {default=0};
public:
	/* TGLCustomGLSLPostBlurShader.Create */ inline __fastcall virtual TGLSLPostBlurShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLPostBlurShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLPostBlurShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLPostThermalVisionShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	float FThreshold;
	float Fintensity;
	void __fastcall DoUseTempTexture(Gls::Context::TGLTextureHandle* const TempTexture, Gls::Textureformat::TGLTextureTarget TextureTarget);
	Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	bool __fastcall StoreThreshold();
	bool __fastcall StoreIntensity();
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLPostThermalVisionShader(System::Classes::TComponent* AOwner);
	__property float Threshold = {read=FThreshold, write=FThreshold, stored=StoreThreshold};
	__property float Intensity = {read=Fintensity, write=Fintensity, stored=StoreIntensity};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLPostThermalVisionShader() { }
	
private:
	void *__IGLPostShader;	// Glsl::Customshader::IGLPostShader 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}
	operator Glsl::Customshader::_di_IGLPostShader()
	{
		Glsl::Customshader::_di_IGLPostShader intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glsl::Customshader::IGLPostShader*(void) { return (Glsl::Customshader::IGLPostShader*)&__IGLPostShader; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLSLPostThermalVisionShader : public TGLCustomGLSLPostThermalVisionShader
{
	typedef TGLCustomGLSLPostThermalVisionShader inherited;
	
__published:
	__property Threshold = {default=0};
	__property Intensity = {default=0};
public:
	/* TGLCustomGLSLPostThermalVisionShader.Create */ inline __fastcall virtual TGLSLPostThermalVisionShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLPostThermalVisionShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLPostThermalVisionShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLPostDreamVisionShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	float FThreshold;
	void __fastcall DoUseTempTexture(Gls::Context::TGLTextureHandle* const TempTexture, Gls::Textureformat::TGLTextureTarget TextureTarget);
	Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	bool __fastcall StoreThreshold();
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLPostDreamVisionShader(System::Classes::TComponent* AOwner);
	__property float Threshold = {read=FThreshold, write=FThreshold, stored=StoreThreshold};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLPostDreamVisionShader() { }
	
private:
	void *__IGLPostShader;	// Glsl::Customshader::IGLPostShader 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}
	operator Glsl::Customshader::_di_IGLPostShader()
	{
		Glsl::Customshader::_di_IGLPostShader intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glsl::Customshader::IGLPostShader*(void) { return (Glsl::Customshader::IGLPostShader*)&__IGLPostShader; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLSLPostDreamVisionShader : public TGLCustomGLSLPostDreamVisionShader
{
	typedef TGLCustomGLSLPostDreamVisionShader inherited;
	
__published:
	__property Threshold = {default=0};
public:
	/* TGLCustomGLSLPostDreamVisionShader.Create */ inline __fastcall virtual TGLSLPostDreamVisionShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLPostDreamVisionShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLPostDreamVisionShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLPostNightVisionShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	Gls::Material::TGLAbstractMaterialLibrary* FMaterialLibrary;
	float FLuminanceThreshold;
	float FColorAmplification;
	float FElapsedTime;
	int FUseMask;
	Gls::Texture::TGLTexture* FNoiseTex;
	Gls::Texture::TGLTexture* FMaskTex;
	System::UnicodeString FNoiseTexName;
	System::UnicodeString FMaskTexName;
	void __fastcall DoUseTempTexture(Gls::Context::TGLTextureHandle* const TempTexture, Gls::Textureformat::TGLTextureTarget TextureTarget);
	Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	bool __fastcall StoreLuminanceThreshold();
	bool __fastcall StoreColorAmplification();
	void __fastcall SetMaskTexTexture(Gls::Texture::TGLTexture* const Value);
	void __fastcall SetNoiseTexTexture(Gls::Texture::TGLTexture* const Value);
	System::UnicodeString __fastcall GetNoiseTexName();
	void __fastcall SetNoiseTexName(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetMaskTexName();
	void __fastcall SetMaskTexName(const System::UnicodeString Value);
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Gls::Material::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomGLSLPostNightVisionShader(System::Classes::TComponent* AOwner);
	__property float LuminanceThreshold = {read=FLuminanceThreshold, write=FLuminanceThreshold, stored=StoreLuminanceThreshold};
	__property float ColorAmplification = {read=FColorAmplification, write=FColorAmplification, stored=StoreColorAmplification};
	__property float ElapsedTime = {read=FElapsedTime, write=FElapsedTime, stored=false};
	__property Gls::Material::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gls::Texture::TGLTexture* NoiseTex = {read=FNoiseTex, write=SetNoiseTexTexture};
	__property System::UnicodeString NoiseTexName = {read=GetNoiseTexName, write=SetNoiseTexName};
	__property Gls::Texture::TGLTexture* MaskTex = {read=FMaskTex, write=SetMaskTexTexture};
	__property System::UnicodeString MaskTexName = {read=GetMaskTexName, write=SetMaskTexName};
	__property int UseMask = {read=FUseMask, write=FUseMask, nodefault};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLPostNightVisionShader() { }
	
private:
	void *__IGLPostShader;	// Glsl::Customshader::IGLPostShader 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}
	operator Glsl::Customshader::_di_IGLPostShader()
	{
		Glsl::Customshader::_di_IGLPostShader intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glsl::Customshader::IGLPostShader*(void) { return (Glsl::Customshader::IGLPostShader*)&__IGLPostShader; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLSLPostNightVisionShader : public TGLCustomGLSLPostNightVisionShader
{
	typedef TGLCustomGLSLPostNightVisionShader inherited;
	
__published:
	__property LuminanceThreshold = {default=0};
	__property ColorAmplification = {default=0};
	__property ElapsedTime = {default=0};
	__property MaterialLibrary;
	__property NoiseTexName = {default=0};
	__property MaskTexName = {default=0};
	__property UseMask;
public:
	/* TGLCustomGLSLPostNightVisionShader.Create */ inline __fastcall virtual TGLSLPostNightVisionShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLPostNightVisionShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLPostNightVisionShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLPostPixelateShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	float FPixelWidth;
	float FPixelHeight;
	void __fastcall DoUseTempTexture(Gls::Context::TGLTextureHandle* const TempTexture, Gls::Textureformat::TGLTextureTarget TextureTarget);
	Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	bool __fastcall StorePixelWidth();
	bool __fastcall StorePixelHeight();
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLPostPixelateShader(System::Classes::TComponent* AOwner);
	__property float PixelWidth = {read=FPixelWidth, write=FPixelWidth, stored=StorePixelWidth};
	__property float PixelHeight = {read=FPixelHeight, write=FPixelHeight, stored=StorePixelHeight};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLPostPixelateShader() { }
	
private:
	void *__IGLPostShader;	// Glsl::Customshader::IGLPostShader 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}
	operator Glsl::Customshader::_di_IGLPostShader()
	{
		Glsl::Customshader::_di_IGLPostShader intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glsl::Customshader::IGLPostShader*(void) { return (Glsl::Customshader::IGLPostShader*)&__IGLPostShader; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLSLPostPixelateShader : public TGLCustomGLSLPostPixelateShader
{
	typedef TGLCustomGLSLPostPixelateShader inherited;
	
__published:
	__property PixelWidth = {default=0};
	__property PixelHeight = {default=0};
public:
	/* TGLCustomGLSLPostPixelateShader.Create */ inline __fastcall virtual TGLSLPostPixelateShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLPostPixelateShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLPostPixelateShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLPostPosterizeShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	float FGamma;
	float FNumColors;
	void __fastcall DoUseTempTexture(Gls::Context::TGLTextureHandle* const TempTexture, Gls::Textureformat::TGLTextureTarget TextureTarget);
	Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	bool __fastcall StoreGamma();
	bool __fastcall StoreNumColors();
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLPostPosterizeShader(System::Classes::TComponent* AOwner);
	__property float Gamma = {read=FGamma, write=FGamma, stored=StoreGamma};
	__property float NumColors = {read=FNumColors, write=FNumColors, stored=StoreNumColors};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLPostPosterizeShader() { }
	
private:
	void *__IGLPostShader;	// Glsl::Customshader::IGLPostShader 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}
	operator Glsl::Customshader::_di_IGLPostShader()
	{
		Glsl::Customshader::_di_IGLPostShader intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glsl::Customshader::IGLPostShader*(void) { return (Glsl::Customshader::IGLPostShader*)&__IGLPostShader; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLSLPostPosterizeShader : public TGLCustomGLSLPostPosterizeShader
{
	typedef TGLCustomGLSLPostPosterizeShader inherited;
	
__published:
	__property Gamma = {default=0};
	__property NumColors = {default=0};
public:
	/* TGLCustomGLSLPostPosterizeShader.Create */ inline __fastcall virtual TGLSLPostPosterizeShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLPostPosterizeShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLPostPosterizeShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLPostFrostShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	float FRandScale;
	float FRandFactor;
	void __fastcall DoUseTempTexture(Gls::Context::TGLTextureHandle* const TempTexture, Gls::Textureformat::TGLTextureTarget TextureTarget);
	Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	bool __fastcall StoreRandScale();
	bool __fastcall StoreRandFactor();
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLPostFrostShader(System::Classes::TComponent* AOwner);
	__property float RandScale = {read=FRandScale, write=FRandScale, stored=StoreRandScale};
	__property float RandFactor = {read=FRandFactor, write=FRandFactor, stored=StoreRandFactor};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLPostFrostShader() { }
	
private:
	void *__IGLPostShader;	// Glsl::Customshader::IGLPostShader 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}
	operator Glsl::Customshader::_di_IGLPostShader()
	{
		Glsl::Customshader::_di_IGLPostShader intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glsl::Customshader::IGLPostShader*(void) { return (Glsl::Customshader::IGLPostShader*)&__IGLPostShader; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLSLPostFrostShader : public TGLCustomGLSLPostFrostShader
{
	typedef TGLCustomGLSLPostFrostShader inherited;
	
__published:
	__property RandScale = {default=0};
	__property RandFactor = {default=0};
public:
	/* TGLCustomGLSLPostFrostShader.Create */ inline __fastcall virtual TGLSLPostFrostShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLPostFrostShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLPostFrostShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLPostTroubleShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	float FPixelX;
	float FPixelY;
	float FFreq;
	Gls::Material::TGLAbstractMaterialLibrary* FMaterialLibrary;
	Gls::Texture::TGLTexture* FNoiseTex;
	System::UnicodeString FNoiseTexName;
	void __fastcall DoUseTempTexture(Gls::Context::TGLTextureHandle* const TempTexture, Gls::Textureformat::TGLTextureTarget TextureTarget);
	Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	void __fastcall SetNoiseTexTexture(Gls::Texture::TGLTexture* const Value);
	System::UnicodeString __fastcall GetNoiseTexName();
	void __fastcall SetNoiseTexName(const System::UnicodeString Value);
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	bool __fastcall StorePixelX();
	bool __fastcall StorePixelY();
	bool __fastcall StoreFreq();
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Gls::Material::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomGLSLPostTroubleShader(System::Classes::TComponent* AOwner);
	__property float PixelX = {read=FPixelX, write=FPixelX, stored=StorePixelX};
	__property float PixelY = {read=FPixelY, write=FPixelY, stored=StorePixelY};
	__property float Freq = {read=FFreq, write=FFreq, stored=StoreFreq};
	__property Gls::Material::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gls::Texture::TGLTexture* NoiseTex = {read=FNoiseTex, write=SetNoiseTexTexture};
	__property System::UnicodeString NoiseTexName = {read=GetNoiseTexName, write=SetNoiseTexName};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLPostTroubleShader() { }
	
private:
	void *__IGLPostShader;	// Glsl::Customshader::IGLPostShader 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}
	operator Glsl::Customshader::_di_IGLPostShader()
	{
		Glsl::Customshader::_di_IGLPostShader intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glsl::Customshader::IGLPostShader*(void) { return (Glsl::Customshader::IGLPostShader*)&__IGLPostShader; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLSLPostTroubleShader : public TGLCustomGLSLPostTroubleShader
{
	typedef TGLCustomGLSLPostTroubleShader inherited;
	
__published:
	__property PixelX = {default=0};
	__property PixelY = {default=0};
	__property Freq = {default=0};
	__property MaterialLibrary;
	__property NoiseTexName = {default=0};
public:
	/* TGLCustomGLSLPostTroubleShader.Create */ inline __fastcall virtual TGLSLPostTroubleShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLPostTroubleShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLPostTroubleShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Postshaders */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_POSTSHADERS)
using namespace Glsl::Postshaders;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_PostshadersHPP
