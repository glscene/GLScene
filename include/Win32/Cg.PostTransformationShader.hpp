// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Cg.PostTransformationShader.pas' rev: 35.00 (Windows)

#ifndef Cg_PosttransformationshaderHPP
#define Cg_PosttransformationshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Cadencer.hpp>
#include <GLS.Context.hpp>
#include <GLS.Scene.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.TextureFormat.hpp>
#include <Cg.Import.hpp>
#include <Cg.GL.hpp>
#include <Cg.Shader.hpp>
#include <GLSL.CustomShader.hpp>
#include <GLS.Material.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cg
{
namespace Posttransformationshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomCGPostTransformationShader;
class DELPHICLASS TGLCGPostTransformationShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCustomCGPostTransformationShader : public Cg::Shader::TCustomCgShader
{
	typedef Cg::Shader::TCustomCgShader inherited;
	
private:
	float FTransformationPower;
	Gls::Texture::TGLTexture* FTransformationTexture;
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	void __fastcall DoUseTempTexture(Gls::Context::TGLTextureHandle* const TempTexture, Gls::Textureformat::TGLTextureTarget TextureTarget);
	Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	
public:
	__fastcall virtual TGLCustomCGPostTransformationShader(System::Classes::TComponent* AOwner);
	__property float TransformationPower = {read=FTransformationPower, write=FTransformationPower};
	__property Gls::Texture::TGLTexture* TransformationTexture = {read=FTransformationTexture, write=FTransformationTexture};
public:
	/* TCustomCgShader.Destroy */ inline __fastcall virtual ~TGLCustomCGPostTransformationShader() { }
	
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


class PASCALIMPLEMENTATION TGLCGPostTransformationShader : public TGLCustomCGPostTransformationShader
{
	typedef TGLCustomCGPostTransformationShader inherited;
	
__published:
	__property TransformationPower = {default=0};
	__property TransformationTexture;
public:
	/* TGLCustomCGPostTransformationShader.Create */ inline __fastcall virtual TGLCGPostTransformationShader(System::Classes::TComponent* AOwner) : TGLCustomCGPostTransformationShader(AOwner) { }
	
public:
	/* TCustomCgShader.Destroy */ inline __fastcall virtual ~TGLCGPostTransformationShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Posttransformationshader */
}	/* namespace Cg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CG_POSTTRANSFORMATIONSHADER)
using namespace Cg::Posttransformationshader;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CG)
using namespace Cg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cg_PosttransformationshaderHPP
