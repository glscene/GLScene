// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.ShapeShaders.pas' rev: 35.00 (Windows)

#ifndef Glsl_ShapeshadersHPP
#define Glsl_ShapeshadersHPP

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
#include <GLS.Scene.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.State.hpp>
#include <GLS.Context.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.Color.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Material.hpp>
#include <GLSL.Shader.hpp>
#include <GLSL.CustomShader.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Shapeshaders
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCelShader;
class DELPHICLASS TGLCustomGLSLSimpleErosionShader;
class DELPHICLASS TGLSLSimpleErosionShader;
class DELPHICLASS TGLCustomGLSLGlassShader;
class DELPHICLASS TGLSLGlassShader;
class DELPHICLASS TGLCustomGLSLSimpleGoochShader;
class DELPHICLASS TGLSLSimpleGoochShader;
class DELPHICLASS TGLCustomGLSLFurShader;
class DELPHICLASS TGLSLFurShader;
class DELPHICLASS TGLCustomGLSLIvoryShader;
class DELPHICLASS TGLSLIvoryShader;
class DELPHICLASS TGLCustomGLSLSimpleLatticeShader;
class DELPHICLASS TGLCustomGLSLLatticeShader;
class DELPHICLASS TGLSLSimpleLatticeShader;
class DELPHICLASS TGLSLLatticeShader;
class DELPHICLASS TGLCustomGLSLSemShader;
class DELPHICLASS TGLSLSemShader;
class DELPHICLASS TGLCustomGLSLToonShader;
class DELPHICLASS TGLSLToonShader;
class DELPHICLASS TGLCustomGLSLVertexDisplacementShader;
class DELPHICLASS TGLSLVertexDisplacementShader;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLCelShaderOption : unsigned char { csoOutlines, csoTextured, csoNoBuildShadeTexture };

typedef System::Set<TGLCelShaderOption, TGLCelShaderOption::csoOutlines, TGLCelShaderOption::csoNoBuildShadeTexture> TGLCelShaderOptions;

typedef void __fastcall (__closure *TGLCelShaderGetIntensity)(System::TObject* Sender, System::Byte &intensity);

class PASCALIMPLEMENTATION TGLCelShader : public Gls::Material::TGLShader
{
	typedef Gls::Material::TGLShader inherited;
	
private:
	float FOutlineWidth;
	TGLCelShaderOptions FCelShaderOptions;
	Gls::Context::TGLARBVertexProgramHandle* FVPHandle;
	Gls::Texture::TGLTexture* FShadeTexture;
	TGLCelShaderGetIntensity FOnGetIntensity;
	bool FOutlinePass;
	bool FUnApplyShadeTexture;
	Gls::Color::TGLColor* FOutlineColor;
	
protected:
	void __fastcall SetCelShaderOptions(const TGLCelShaderOptions val);
	void __fastcall SetOutlineWidth(const float val);
	void __fastcall SetOutlineColor(Gls::Color::TGLColor* const val);
	void __fastcall BuildShadeTexture();
	virtual void __fastcall Loaded();
	System::UnicodeString __fastcall GenerateVertexProgram();
	
public:
	__fastcall virtual TGLCelShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCelShader();
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property Gls::Texture::TGLTexture* ShadeTexture = {read=FShadeTexture};
	
__published:
	__property TGLCelShaderOptions CelShaderOptions = {read=FCelShaderOptions, write=SetCelShaderOptions, nodefault};
	__property Gls::Color::TGLColor* OutlineColor = {read=FOutlineColor, write=SetOutlineColor};
	__property float OutlineWidth = {read=FOutlineWidth, write=SetOutlineWidth};
	__property TGLCelShaderGetIntensity OnGetIntensity = {read=FOnGetIntensity, write=FOnGetIntensity};
};


class PASCALIMPLEMENTATION TGLCustomGLSLSimpleErosionShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	Gls::Material::TGLAbstractMaterialLibrary* FMaterialLibrary;
	Gls::Texture::TGLTexture* FMainTex;
	Gls::Texture::TGLTexture* FNoiseTex;
	Gls::Texture::TGLTexture* FErosionTex;
	System::UnicodeString FMainTexName;
	System::UnicodeString FNoiseTexName;
	System::UnicodeString FErosionTexName;
	float FErosionScale;
	float FErosionFactor;
	float FIntensityFactor1;
	float FIntensityFactor2;
	Gls::Color::TGLColor* FSpecularColor;
	Gls::Color::TGLColor* FAmbientColor;
	float FAmbientFactor;
	float FDiffuseFactor;
	float FSpecularFactor;
	float FSpecularRoughness;
	float FAnisotropicRoughness;
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMainTexTexture(Gls::Texture::TGLTexture* const Value);
	void __fastcall SetNoiseTexTexture(Gls::Texture::TGLTexture* const Value);
	void __fastcall SetErosionTexTexture(Gls::Texture::TGLTexture* const Value);
	System::UnicodeString __fastcall GetMainTexName();
	void __fastcall SetMainTexName(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetNoiseTexName();
	void __fastcall SetNoiseTexName(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetErosionTexName();
	void __fastcall SetErosionTexName(const System::UnicodeString Value);
	void __fastcall SetAmbientColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetSpecularColor(Gls::Color::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Gls::Material::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomGLSLSimpleErosionShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLSimpleErosionShader();
	__property Gls::Material::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gls::Texture::TGLTexture* MainTexture = {read=FMainTex, write=SetMainTexTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTexName, write=SetMainTexName};
	__property Gls::Texture::TGLTexture* NoiseTexture = {read=FNoiseTex, write=SetNoiseTexTexture};
	__property System::UnicodeString NoiseTextureName = {read=GetNoiseTexName, write=SetNoiseTexName};
	__property Gls::Texture::TGLTexture* ErosionTexture = {read=FErosionTex, write=SetErosionTexTexture};
	__property System::UnicodeString ErosionTextureName = {read=GetErosionTexName, write=SetErosionTexName};
	__property float ErosionFactor = {read=FErosionFactor, write=FErosionFactor};
	__property float ErosionScale = {read=FErosionFactor, write=FErosionFactor};
	__property float IntensityFactor1 = {read=FIntensityFactor1, write=FIntensityFactor1};
	__property float IntensityFactor2 = {read=FIntensityFactor2, write=FIntensityFactor2};
	__property Gls::Color::TGLColor* SpecularColor = {read=FSpecularColor, write=SetSpecularColor};
	__property Gls::Color::TGLColor* AmbientColor = {read=FAmbientColor, write=SetAmbientColor};
	__property float AmbientFactor = {read=FAmbientFactor, write=FAmbientFactor};
	__property float DiffuseFactor = {read=FDiffuseFactor, write=FDiffuseFactor};
	__property float SpecularFactor = {read=FSpecularFactor, write=FSpecularFactor};
	__property float SpecularRoughness = {read=FSpecularRoughness, write=FSpecularRoughness};
	__property float AnisotropicRoughness = {read=FAnisotropicRoughness, write=FAnisotropicRoughness};
};


class PASCALIMPLEMENTATION TGLSLSimpleErosionShader : public TGLCustomGLSLSimpleErosionShader
{
	typedef TGLCustomGLSLSimpleErosionShader inherited;
	
__published:
	__property MaterialLibrary;
	__property MainTexture;
	__property MainTextureName = {default=0};
	__property NoiseTexture;
	__property NoiseTextureName = {default=0};
	__property ErosionTexture;
	__property ErosionTextureName = {default=0};
	__property ErosionScale = {default=0};
	__property ErosionFactor = {default=0};
	__property IntensityFactor1 = {default=0};
	__property IntensityFactor2 = {default=0};
	__property SpecularColor;
	__property AmbientColor;
	__property AmbientFactor = {default=0};
	__property DiffuseFactor = {default=0};
	__property SpecularFactor = {default=0};
	__property SpecularRoughness = {default=0};
	__property AnisotropicRoughness = {default=0};
public:
	/* TGLCustomGLSLSimpleErosionShader.Create */ inline __fastcall virtual TGLSLSimpleErosionShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLSimpleErosionShader(AOwner) { }
	/* TGLCustomGLSLSimpleErosionShader.Destroy */ inline __fastcall virtual ~TGLSLSimpleErosionShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLGlassShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	Gls::Color::TGLColor* FDiffuseColor;
	float FDepth;
	float FMix;
	float FAlpha;
	Gls::Material::TGLAbstractMaterialLibrary* FMaterialLibrary;
	Gls::Texture::TGLTexture* FMainTexture;
	System::UnicodeString FMainTexName;
	Gls::Texture::TGLTexture* FRefractionTexture;
	System::UnicodeString FRefractionTexName;
	Gls::Scene::TGLBaseSceneObject* FOwnerObject;
	Gls::State::TGLBlendFunction FBlendSrc;
	Gls::State::TGLBlendFunction FBlendDst;
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMainTexTexture(Gls::Texture::TGLTexture* const Value);
	System::UnicodeString __fastcall GetMainTexName();
	void __fastcall SetMainTexName(const System::UnicodeString Value);
	void __fastcall SetRefractionTexTexture(Gls::Texture::TGLTexture* const Value);
	System::UnicodeString __fastcall GetRefractionTexName();
	void __fastcall SetRefractionTexName(const System::UnicodeString Value);
	void __fastcall SetDiffuseColor(Gls::Color::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Gls::Material::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomGLSLGlassShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLGlassShader();
	__property Gls::Color::TGLColor* DiffuseColor = {read=FDiffuseColor, write=SetDiffuseColor};
	__property float Depth = {read=FDepth, write=FDepth};
	__property float Mix = {read=FMix, write=FMix};
	__property float Alpha = {read=FAlpha, write=FAlpha};
	__property Gls::Material::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gls::Texture::TGLTexture* MainTexture = {read=FMainTexture, write=SetMainTexTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTexName, write=SetMainTexName};
	__property Gls::Texture::TGLTexture* RefractionTexture = {read=FRefractionTexture, write=SetRefractionTexTexture};
	__property System::UnicodeString RefractionTextureName = {read=GetRefractionTexName, write=SetRefractionTexName};
	__property Gls::Scene::TGLBaseSceneObject* OwnerObject = {read=FOwnerObject, write=FOwnerObject};
	__property Gls::State::TGLBlendFunction BlendSrc = {read=FBlendSrc, write=FBlendSrc, default=6};
	__property Gls::State::TGLBlendFunction BlendDst = {read=FBlendDst, write=FBlendDst, default=8};
};


class PASCALIMPLEMENTATION TGLSLGlassShader : public TGLCustomGLSLGlassShader
{
	typedef TGLCustomGLSLGlassShader inherited;
	
__published:
	__property DiffuseColor;
	__property Depth = {default=0};
	__property Mix = {default=0};
	__property Alpha = {default=0};
	__property MaterialLibrary;
	__property MainTexture;
	__property MainTextureName = {default=0};
	__property RefractionTexture;
	__property RefractionTextureName = {default=0};
	__property OwnerObject;
	__property BlendSrc = {default=6};
	__property BlendDst = {default=8};
public:
	/* TGLCustomGLSLGlassShader.Create */ inline __fastcall virtual TGLSLGlassShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLGlassShader(AOwner) { }
	/* TGLCustomGLSLGlassShader.Destroy */ inline __fastcall virtual ~TGLSLGlassShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLSimpleGoochShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	Gls::Color::TGLColor* FDiffuseColor;
	Gls::Color::TGLColor* FWarmColor;
	Gls::Color::TGLColor* FCoolColor;
	Gls::Color::TGLColor* FSpecularColor;
	Gls::Color::TGLColor* FAmbientColor;
	float FDiffuseWarm;
	float FDiffuseCool;
	float FAmbientFactor;
	float FDiffuseFactor;
	float FSpecularFactor;
	Glsl::Customshader::TGLBlendingModeEx FBlendingMode;
	void __fastcall SetDiffuseColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetAmbientColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetSpecularColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetWarmColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetCoolColor(Gls::Color::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLSimpleGoochShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLSimpleGoochShader();
	__property Gls::Color::TGLColor* DiffuseColor = {read=FDiffuseColor, write=SetDiffuseColor};
	__property Gls::Color::TGLColor* WarmColor = {read=FWarmColor, write=SetWarmColor};
	__property Gls::Color::TGLColor* CoolColor = {read=FCoolColor, write=SetCoolColor};
	__property Gls::Color::TGLColor* SpecularColor = {read=FSpecularColor, write=SetSpecularColor};
	__property Gls::Color::TGLColor* AmbientColor = {read=FAmbientColor, write=SetAmbientColor};
	__property float WarmFactor = {read=FDiffuseWarm, write=FDiffuseWarm};
	__property float CoolFactor = {read=FDiffuseCool, write=FDiffuseCool};
	__property float AmbientFactor = {read=FAmbientFactor, write=FAmbientFactor};
	__property float DiffuseFactor = {read=FDiffuseFactor, write=FDiffuseFactor};
	__property float SpecularFactor = {read=FSpecularFactor, write=FSpecularFactor};
	__property Glsl::Customshader::TGLBlendingModeEx BlendingMode = {read=FBlendingMode, write=FBlendingMode, default=0};
};


class PASCALIMPLEMENTATION TGLSLSimpleGoochShader : public TGLCustomGLSLSimpleGoochShader
{
	typedef TGLCustomGLSLSimpleGoochShader inherited;
	
__published:
	__property DiffuseColor;
	__property WarmColor;
	__property CoolColor;
	__property SpecularColor;
	__property AmbientColor;
	__property WarmFactor = {default=0};
	__property CoolFactor = {default=0};
	__property AmbientFactor = {default=0};
	__property DiffuseFactor = {default=0};
	__property SpecularFactor = {default=0};
public:
	/* TGLCustomGLSLSimpleGoochShader.Create */ inline __fastcall virtual TGLSLSimpleGoochShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLSimpleGoochShader(AOwner) { }
	/* TGLCustomGLSLSimpleGoochShader.Destroy */ inline __fastcall virtual ~TGLSLSimpleGoochShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLFurShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	Gls::Material::TGLAbstractMaterialLibrary* FMaterialLibrary;
	int FCurrentPass;
	float FPassCount;
	float FFurLength;
	float FMaxFurLength;
	float FFurScale;
	bool FRandomFurLength;
	Gls::Color::TGLColor* FColorScale;
	Gls::Color::TGLColor* FAmbient;
	Gls::Coordinates::TGLCoordinates3* FGravity;
	float FLightIntensity;
	Gls::Texture::TGLTexture* FMainTex;
	Gls::Texture::TGLTexture* FNoiseTex;
	System::UnicodeString FNoiseTexName;
	System::UnicodeString FMainTexName;
	Gls::State::TGLBlendFunction FBlendSrc;
	Gls::State::TGLBlendFunction FBlendDst;
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMainTexTexture(Gls::Texture::TGLTexture* const Value);
	void __fastcall SetNoiseTexTexture(Gls::Texture::TGLTexture* const Value);
	System::UnicodeString __fastcall GetNoiseTexName();
	void __fastcall SetNoiseTexName(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetMainTexName();
	void __fastcall SetMainTexName(const System::UnicodeString Value);
	void __fastcall SetGravity(Gls::Coordinates::TGLCoordinates3* APosition);
	void __fastcall SetAmbient(Gls::Color::TGLColor* AValue);
	void __fastcall SetColorScale(Gls::Color::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Gls::Material::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomGLSLFurShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLFurShader();
	__property float PassCount = {read=FPassCount, write=FPassCount};
	__property float FurLength = {read=FFurLength, write=FFurLength};
	__property float MaxFurLength = {read=FMaxFurLength, write=FMaxFurLength};
	__property float FurDensity = {read=FFurScale, write=FFurScale};
	__property bool RandomFurLength = {read=FRandomFurLength, write=FRandomFurLength, nodefault};
	__property Gls::Color::TGLColor* ColorScale = {read=FColorScale, write=SetColorScale};
	__property Gls::Color::TGLColor* Ambient = {read=FAmbient, write=SetAmbient};
	__property Gls::Material::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gls::Texture::TGLTexture* MainTexture = {read=FMainTex, write=SetMainTexTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTexName, write=SetMainTexName};
	__property Gls::Texture::TGLTexture* NoiseTexture = {read=FNoiseTex, write=SetNoiseTexTexture};
	__property System::UnicodeString NoiseTextureName = {read=GetNoiseTexName, write=SetNoiseTexName};
	__property Gls::State::TGLBlendFunction BlendSrc = {read=FBlendSrc, write=FBlendSrc, default=2};
	__property Gls::State::TGLBlendFunction BlendDst = {read=FBlendDst, write=FBlendDst, default=5};
	__property Gls::Coordinates::TGLCoordinates3* Gravity = {read=FGravity, write=SetGravity};
	__property float LightIntensity = {read=FLightIntensity, write=FLightIntensity};
};


class PASCALIMPLEMENTATION TGLSLFurShader : public TGLCustomGLSLFurShader
{
	typedef TGLCustomGLSLFurShader inherited;
	
__published:
	__property PassCount = {default=0};
	__property FurLength = {default=0};
	__property MaxFurLength = {default=0};
	__property FurDensity = {default=0};
	__property RandomFurLength;
	__property ColorScale;
	__property Ambient;
	__property LightIntensity = {default=0};
	__property Gravity;
	__property BlendSrc = {default=2};
	__property BlendDst = {default=5};
	__property MainTexture;
	__property MainTextureName = {default=0};
	__property NoiseTexture;
	__property NoiseTextureName = {default=0};
public:
	/* TGLCustomGLSLFurShader.Create */ inline __fastcall virtual TGLSLFurShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLFurShader(AOwner) { }
	/* TGLCustomGLSLFurShader.Destroy */ inline __fastcall virtual ~TGLSLFurShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLIvoryShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLIvoryShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLIvoryShader();
};


class PASCALIMPLEMENTATION TGLSLIvoryShader : public TGLCustomGLSLIvoryShader
{
	typedef TGLCustomGLSLIvoryShader inherited;
	
public:
	/* TGLCustomGLSLIvoryShader.Create */ inline __fastcall virtual TGLSLIvoryShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLIvoryShader(AOwner) { }
	/* TGLCustomGLSLIvoryShader.Destroy */ inline __fastcall virtual ~TGLSLIvoryShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLSimpleLatticeShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	Gls::Coordinates::TGLCoordinates2* FLatticeScale;
	Gls::Coordinates::TGLCoordinates2* FLatticeThreshold;
	void __fastcall SetLatticeScale(Gls::Coordinates::TGLCoordinates2* const Value);
	void __fastcall SetLatticeThreshold(Gls::Coordinates::TGLCoordinates2* const Value);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLSimpleLatticeShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLSimpleLatticeShader();
	__property Gls::Coordinates::TGLCoordinates2* LatticeScale = {read=FLatticeScale, write=SetLatticeScale};
	__property Gls::Coordinates::TGLCoordinates2* LatticeThreshold = {read=FLatticeThreshold, write=SetLatticeThreshold};
};


class PASCALIMPLEMENTATION TGLCustomGLSLLatticeShader : public TGLCustomGLSLSimpleLatticeShader
{
	typedef TGLCustomGLSLSimpleLatticeShader inherited;
	
private:
	Gls::Color::TGLColor* FAmbientColor;
	Gls::Color::TGLColor* FDiffuseColor;
	Gls::Color::TGLColor* FSpecularColor;
	Gls::Material::TGLAbstractMaterialLibrary* FMaterialLibrary;
	Gls::Texture::TGLTexture* FMainTexture;
	System::UnicodeString FMainTexName;
	float FSpecularPower;
	float FLightPower;
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMainTexTexture(Gls::Texture::TGLTexture* const Value);
	System::UnicodeString __fastcall GetMainTexName();
	void __fastcall SetMainTexName(const System::UnicodeString Value);
	void __fastcall SetDiffuseColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetAmbientColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetSpecularColor(Gls::Color::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall SetMaterialLibrary(Gls::Material::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomGLSLLatticeShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLLatticeShader();
	__property Gls::Color::TGLColor* DiffuseColor = {read=FDiffuseColor, write=SetDiffuseColor};
	__property Gls::Color::TGLColor* SpecularColor = {read=FSpecularColor, write=SetSpecularColor};
	__property Gls::Color::TGLColor* AmbientColor = {read=FAmbientColor, write=SetAmbientColor};
	__property Gls::Material::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gls::Texture::TGLTexture* MainTexture = {read=FMainTexture, write=SetMainTexTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTexName, write=SetMainTexName};
	__property float SpecularPower = {read=FSpecularPower, write=FSpecularPower};
	__property float LightPower = {read=FLightPower, write=FLightPower};
};


class PASCALIMPLEMENTATION TGLSLSimpleLatticeShader : public TGLCustomGLSLSimpleLatticeShader
{
	typedef TGLCustomGLSLSimpleLatticeShader inherited;
	
__published:
	__property LatticeScale;
	__property LatticeThreshold;
public:
	/* TGLCustomGLSLSimpleLatticeShader.Create */ inline __fastcall virtual TGLSLSimpleLatticeShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLSimpleLatticeShader(AOwner) { }
	/* TGLCustomGLSLSimpleLatticeShader.Destroy */ inline __fastcall virtual ~TGLSLSimpleLatticeShader() { }
	
};


class PASCALIMPLEMENTATION TGLSLLatticeShader : public TGLCustomGLSLLatticeShader
{
	typedef TGLCustomGLSLLatticeShader inherited;
	
__published:
	__property LatticeScale;
	__property LatticeThreshold;
	__property AmbientColor;
	__property DiffuseColor;
	__property SpecularColor;
	__property MainTexture;
	__property SpecularPower = {default=0};
	__property LightPower = {default=0};
public:
	/* TGLCustomGLSLLatticeShader.Create */ inline __fastcall virtual TGLSLLatticeShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLLatticeShader(AOwner) { }
	/* TGLCustomGLSLLatticeShader.Destroy */ inline __fastcall virtual ~TGLSLLatticeShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLSemShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	Gls::Color::TGLColor* FAmbientColor;
	Gls::Color::TGLColor* FSpecularColor;
	float FAmbientFactor;
	float FDiffuseFactor;
	float FSpecularFactor;
	Gls::Material::TGLAbstractMaterialLibrary* FMaterialLibrary;
	Gls::Texture::TGLTexture* FMainTexture;
	System::UnicodeString FMainTexName;
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMainTexTexture(Gls::Texture::TGLTexture* const Value);
	System::UnicodeString __fastcall GetMainTexName();
	void __fastcall SetMainTexName(const System::UnicodeString Value);
	void __fastcall SetAmbientColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetSpecularColor(Gls::Color::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Gls::Material::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomGLSLSemShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLSemShader();
	__property Gls::Color::TGLColor* SpecularColor = {read=FSpecularColor, write=SetSpecularColor};
	__property Gls::Color::TGLColor* AmbientColor = {read=FAmbientColor, write=SetAmbientColor};
	__property float AmbientFactor = {read=FAmbientFactor, write=FAmbientFactor};
	__property float DiffuseFactor = {read=FDiffuseFactor, write=FDiffuseFactor};
	__property float SpecularFactor = {read=FSpecularFactor, write=FSpecularFactor};
	__property Gls::Material::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gls::Texture::TGLTexture* MainTexture = {read=FMainTexture, write=SetMainTexTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTexName, write=SetMainTexName};
};


class PASCALIMPLEMENTATION TGLSLSemShader : public TGLCustomGLSLSemShader
{
	typedef TGLCustomGLSLSemShader inherited;
	
__published:
	__property AmbientColor;
	__property SpecularColor;
	__property AmbientFactor = {default=0};
	__property DiffuseFactor = {default=0};
	__property SpecularFactor = {default=0};
	__property MaterialLibrary;
	__property MainTexture;
	__property MainTextureName = {default=0};
public:
	/* TGLCustomGLSLSemShader.Create */ inline __fastcall virtual TGLSLSemShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLSemShader(AOwner) { }
	/* TGLCustomGLSLSemShader.Destroy */ inline __fastcall virtual ~TGLSLSemShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLToonShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	Gls::Color::TGLColor* FHighlightColor;
	Gls::Color::TGLColor* FMidColor;
	Gls::Color::TGLColor* FLightenShadowColor;
	Gls::Color::TGLColor* FDarkenShadowColor;
	Gls::Color::TGLColor* FOutlineColor;
	float FHighlightSize;
	float FMidSize;
	float FShadowSize;
	float FOutlineWidth;
	void __fastcall SetHighLightColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetMidColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetLightenShadowColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetDarkenShadowColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetOutlineColor(Gls::Color::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLToonShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLToonShader();
	__property Gls::Color::TGLColor* HighlightColor = {read=FHighlightColor, write=SetHighLightColor};
	__property Gls::Color::TGLColor* MidColor = {read=FMidColor, write=SetMidColor};
	__property Gls::Color::TGLColor* LightenShadowColor = {read=FLightenShadowColor, write=SetLightenShadowColor};
	__property Gls::Color::TGLColor* DarkenShadowrColor = {read=FDarkenShadowColor, write=SetDarkenShadowColor};
	__property Gls::Color::TGLColor* OutlinetColor = {read=FOutlineColor, write=SetOutlineColor};
	__property float HighlightSize = {read=FHighlightSize, write=FHighlightSize};
	__property float MidSize = {read=FMidSize, write=FMidSize};
	__property float ShadowSize = {read=FShadowSize, write=FShadowSize};
	__property float OutlineWidth = {read=FOutlineWidth, write=FOutlineWidth};
};


class PASCALIMPLEMENTATION TGLSLToonShader : public TGLCustomGLSLToonShader
{
	typedef TGLCustomGLSLToonShader inherited;
	
__published:
	__property HighlightColor;
	__property MidColor;
	__property LightenShadowColor;
	__property DarkenShadowrColor;
	__property OutlinetColor;
	__property HighlightSize = {default=0};
	__property MidSize = {default=0};
	__property ShadowSize = {default=0};
	__property OutlineWidth = {default=0};
public:
	/* TGLCustomGLSLToonShader.Create */ inline __fastcall virtual TGLSLToonShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLToonShader(AOwner) { }
	/* TGLCustomGLSLToonShader.Destroy */ inline __fastcall virtual ~TGLSLToonShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLVertexDisplacementShader : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	Gls::Color::TGLColor* FAmbientColor;
	Gls::Color::TGLColor* FSpecularColor;
	float FAmbientFactor;
	float FDiffuseFactor;
	float FSpecularFactor;
	Gls::Material::TGLAbstractMaterialLibrary* FMaterialLibrary;
	Gls::Texture::TGLTexture* FMainTexture;
	System::UnicodeString FMainTexName;
	float FElapsedTime;
	float FNoise;
	float FDisplacementScale;
	float FNoiseScale;
	float FTurbulenceFactor;
	float FNoisePeriod;
	float FTimeFactor;
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMainTexTexture(Gls::Texture::TGLTexture* const Value);
	System::UnicodeString __fastcall GetMainTexName();
	void __fastcall SetMainTexName(const System::UnicodeString Value);
	void __fastcall SetAmbientColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetSpecularColor(Gls::Color::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Gls::Material::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomGLSLVertexDisplacementShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLVertexDisplacementShader();
	__property Gls::Color::TGLColor* SpecularColor = {read=FSpecularColor, write=SetSpecularColor};
	__property Gls::Color::TGLColor* AmbientColor = {read=FAmbientColor, write=SetAmbientColor};
	__property float AmbientFactor = {read=FAmbientFactor, write=FAmbientFactor};
	__property float DiffuseFactor = {read=FDiffuseFactor, write=FDiffuseFactor};
	__property float SpecularFactor = {read=FSpecularFactor, write=FSpecularFactor};
	__property Gls::Material::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gls::Texture::TGLTexture* MainTexture = {read=FMainTexture, write=SetMainTexTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTexName, write=SetMainTexName};
	__property float ElapsedTime = {read=FElapsedTime, write=FElapsedTime};
	__property float NoiseFactor = {read=FNoise, write=FNoise};
	__property float NoiseScale = {read=FNoiseScale, write=FNoiseScale};
	__property float TurbulenceFactor = {read=FTurbulenceFactor, write=FTurbulenceFactor};
	__property float NoisePeriod = {read=FNoisePeriod, write=FNoisePeriod};
	__property float DisplacementScale = {read=FDisplacementScale, write=FDisplacementScale};
	__property float TimeFactor = {read=FTimeFactor, write=FTimeFactor};
};


class PASCALIMPLEMENTATION TGLSLVertexDisplacementShader : public TGLCustomGLSLVertexDisplacementShader
{
	typedef TGLCustomGLSLVertexDisplacementShader inherited;
	
__published:
	__property AmbientColor;
	__property SpecularColor;
	__property AmbientFactor = {default=0};
	__property DiffuseFactor = {default=0};
	__property SpecularFactor = {default=0};
	__property MaterialLibrary;
	__property MainTexture;
	__property MainTextureName = {default=0};
	__property ElapsedTime = {default=0};
	__property NoiseFactor = {default=0};
	__property NoiseScale = {default=0};
	__property TurbulenceFactor = {default=0};
	__property NoisePeriod = {default=0};
	__property DisplacementScale = {default=0};
	__property TimeFactor = {default=0};
public:
	/* TGLCustomGLSLVertexDisplacementShader.Create */ inline __fastcall virtual TGLSLVertexDisplacementShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLVertexDisplacementShader(AOwner) { }
	/* TGLCustomGLSLVertexDisplacementShader.Destroy */ inline __fastcall virtual ~TGLSLVertexDisplacementShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Shapeshaders */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_SHAPESHADERS)
using namespace Glsl::Shapeshaders;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_ShapeshadersHPP
