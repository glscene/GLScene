// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.DiffuseSpecularShader.pas' rev: 35.00 (Windows)

#ifndef Glsl_DiffusespecularshaderHPP
#define Glsl_DiffusespecularshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Scene.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Strings.hpp>
#include <GLSL.Shader.hpp>
#include <GLSL.CustomShader.hpp>
#include <GLS.Color.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Material.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Diffusespecularshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLSLDiffuseSpecularShaderException;
class DELPHICLASS TGLBaseCustomGLSLDiffuseSpecular;
class DELPHICLASS TGLBaseGLSLDiffuseSpecularShaderMT;
class DELPHICLASS TGLCustomGLSLDiffuseSpecularShader;
class DELPHICLASS TGLCustomGLSLDiffuseSpecularShaderMT;
struct TLightRecord;
class DELPHICLASS TGLCustomGLSLMLDiffuseSpecularShader;
class DELPHICLASS TGLCustomGLSLMLDiffuseSpecularShaderMT;
class DELPHICLASS TGLSLDiffuseSpecularShaderMT;
class DELPHICLASS TGLSLDiffuseSpecularShader;
class DELPHICLASS TGLSLMLDiffuseSpecularShaderMT;
class DELPHICLASS TGLSLMLDiffuseSpecularShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION EGLSLDiffuseSpecularShaderException : public Glsl::Shader::EGLSLShaderException
{
	typedef Glsl::Shader::EGLSLShaderException inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLSLDiffuseSpecularShaderException(const System::UnicodeString Msg) : Glsl::Shader::EGLSLShaderException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLSLDiffuseSpecularShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : Glsl::Shader::EGLSLShaderException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLDiffuseSpecularShaderException(NativeUInt Ident)/* overload */ : Glsl::Shader::EGLSLShaderException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::PResStringRec ResStringRec)/* overload */ : Glsl::Shader::EGLSLShaderException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLDiffuseSpecularShaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Glsl::Shader::EGLSLShaderException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Glsl::Shader::EGLSLShaderException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(const System::UnicodeString Msg, int AHelpContext) : Glsl::Shader::EGLSLShaderException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Glsl::Shader::EGLSLShaderException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(NativeUInt Ident, int AHelpContext)/* overload */ : Glsl::Shader::EGLSLShaderException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Glsl::Shader::EGLSLShaderException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Glsl::Shader::EGLSLShaderException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Glsl::Shader::EGLSLShaderException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLSLDiffuseSpecularShaderException() { }
	
};


class PASCALIMPLEMENTATION TGLBaseCustomGLSLDiffuseSpecular : public Glsl::Shader::TGLCustomGLSLShader
{
	typedef Glsl::Shader::TGLCustomGLSLShader inherited;
	
private:
	float FLightPower;
	bool FRealisticSpecular;
	Glsl::Customshader::TGLShaderFogSupport FFogSupport;
	void __fastcall SetRealisticSpecular(const bool Value);
	void __fastcall SetFogSupport(const Glsl::Customshader::TGLShaderFogSupport Value);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLBaseCustomGLSLDiffuseSpecular(System::Classes::TComponent* AOwner);
	__property float LightPower = {read=FLightPower, write=FLightPower};
	__property bool RealisticSpecular = {read=FRealisticSpecular, write=SetRealisticSpecular, nodefault};
	__property Glsl::Customshader::TGLShaderFogSupport FogSupport = {read=FFogSupport, write=SetFogSupport, default=2};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLBaseCustomGLSLDiffuseSpecular() { }
	
};


class PASCALIMPLEMENTATION TGLBaseGLSLDiffuseSpecularShaderMT : public TGLBaseCustomGLSLDiffuseSpecular
{
	typedef TGLBaseCustomGLSLDiffuseSpecular inherited;
	
private:
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	Gls::Texture::TGLTexture* FMainTexture;
	System::UnicodeString FMainTextureName;
	System::UnicodeString __fastcall GetMainTextureName();
	void __fastcall SetMainTextureName(const System::UnicodeString Value);
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
protected:
	virtual void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__property Gls::Texture::TGLTexture* MainTexture = {read=FMainTexture, write=FMainTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTextureName, write=SetMainTextureName};
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLBaseGLSLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLDiffuseSpecular(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLBaseGLSLDiffuseSpecularShaderMT() { }
	
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


class PASCALIMPLEMENTATION TGLCustomGLSLDiffuseSpecularShader : public TGLBaseCustomGLSLDiffuseSpecular
{
	typedef TGLBaseCustomGLSLDiffuseSpecular inherited;
	
protected:
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLCustomGLSLDiffuseSpecularShader(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLDiffuseSpecular(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLDiffuseSpecularShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLDiffuseSpecularShaderMT : public TGLBaseGLSLDiffuseSpecularShaderMT
{
	typedef TGLBaseGLSLDiffuseSpecularShaderMT inherited;
	
protected:
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLCustomGLSLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner) : TGLBaseGLSLDiffuseSpecularShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLDiffuseSpecularShaderMT() { }
	
};


struct DECLSPEC_DRECORD TLightRecord
{
public:
	bool Enabled;
	Gls::Scene::TGLLightStyle Style;
};


class PASCALIMPLEMENTATION TGLCustomGLSLMLDiffuseSpecularShader : public TGLBaseCustomGLSLDiffuseSpecular
{
	typedef TGLBaseCustomGLSLDiffuseSpecular inherited;
	
private:
	System::StaticArray<TLightRecord, 8> FLightTrace;
	
protected:
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLMLDiffuseSpecularShader(System::Classes::TComponent* AOwner);
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLMLDiffuseSpecularShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLMLDiffuseSpecularShaderMT : public TGLBaseGLSLDiffuseSpecularShaderMT
{
	typedef TGLBaseGLSLDiffuseSpecularShaderMT inherited;
	
private:
	System::StaticArray<TLightRecord, 8> FLightTrace;
	
protected:
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLMLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner);
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLMLDiffuseSpecularShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLSLDiffuseSpecularShaderMT : public TGLCustomGLSLDiffuseSpecularShaderMT
{
	typedef TGLCustomGLSLDiffuseSpecularShaderMT inherited;
	
__published:
	__property MainTextureName = {default=0};
	__property LightPower = {default=0};
	__property FogSupport = {default=2};
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLSLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner) : TGLCustomGLSLDiffuseSpecularShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLDiffuseSpecularShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLSLDiffuseSpecularShader : public TGLCustomGLSLDiffuseSpecularShader
{
	typedef TGLCustomGLSLDiffuseSpecularShader inherited;
	
__published:
	__property LightPower = {default=0};
	__property FogSupport = {default=2};
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLSLDiffuseSpecularShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLDiffuseSpecularShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLDiffuseSpecularShader() { }
	
};


class PASCALIMPLEMENTATION TGLSLMLDiffuseSpecularShaderMT : public TGLCustomGLSLMLDiffuseSpecularShaderMT
{
	typedef TGLCustomGLSLMLDiffuseSpecularShaderMT inherited;
	
__published:
	__property MainTextureName = {default=0};
	__property LightPower = {default=0};
	__property FogSupport = {default=2};
public:
	/* TGLCustomGLSLMLDiffuseSpecularShaderMT.Create */ inline __fastcall virtual TGLSLMLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner) : TGLCustomGLSLMLDiffuseSpecularShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLMLDiffuseSpecularShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLSLMLDiffuseSpecularShader : public TGLCustomGLSLMLDiffuseSpecularShader
{
	typedef TGLCustomGLSLMLDiffuseSpecularShader inherited;
	
__published:
	__property LightPower = {default=0};
	__property FogSupport = {default=2};
public:
	/* TGLCustomGLSLMLDiffuseSpecularShader.Create */ inline __fastcall virtual TGLSLMLDiffuseSpecularShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLMLDiffuseSpecularShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLMLDiffuseSpecularShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Diffusespecularshader */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_DIFFUSESPECULARSHADER)
using namespace Glsl::Diffusespecularshader;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_DiffusespecularshaderHPP
