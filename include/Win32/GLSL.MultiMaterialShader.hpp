// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.MultiMaterialShader.pas' rev: 35.00 (Windows)

#ifndef Glsl_MultimaterialshaderHPP
#define Glsl_MultimaterialshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <GLS.Material.hpp>
#include <GLS.Context.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Multimaterialshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMultiMaterialShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLMultiMaterialShader : public Gls::Material::TGLShader
{
	typedef Gls::Material::TGLShader inherited;
	
private:
	int FPass;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	bool FVisibleAtDesignTime;
	bool FShaderActiveAtDesignTime;
	Gls::Material::TGLShaderStyle FShaderStyle;
	void __fastcall SetVisibleAtDesignTime(const bool Value);
	void __fastcall SetShaderStyle(const Gls::Material::TGLShaderStyle Value);
	
protected:
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const val);
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLMultiMaterialShader(System::Classes::TComponent* aOwner);
	
__published:
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property bool VisibleAtDesignTime = {read=FVisibleAtDesignTime, write=SetVisibleAtDesignTime, nodefault};
	__property Gls::Material::TGLShaderStyle ShaderStyle = {read=FShaderStyle, write=SetShaderStyle, nodefault};
public:
	/* TGLShader.Destroy */ inline __fastcall virtual ~TGLMultiMaterialShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Multimaterialshader */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_MULTIMATERIALSHADER)
using namespace Glsl::Multimaterialshader;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_MultimaterialshaderHPP
