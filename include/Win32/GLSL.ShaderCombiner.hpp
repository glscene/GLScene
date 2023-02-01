// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.ShaderCombiner.pas' rev: 35.00 (Windows)

#ifndef Glsl_ShadercombinerHPP
#define Glsl_ShadercombinerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <GLS.Material.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Strings.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Shadercombiner
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomShaderCombiner;
class DELPHICLASS TGLShaderCombiner;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLShaderCombinerType : unsigned char { sctOneSPTwoAP, sctTwoSPOneAP, sctOneMPTwoSP, sctTwoMPOneSP };

class PASCALIMPLEMENTATION TGLCustomShaderCombiner : public Gls::Material::TGLShader
{
	typedef Gls::Material::TGLShader inherited;
	
private:
	int FCurrentPass;
	TGLShaderCombinerType FCombinerType;
	Gls::Material::TGLShader* FShaderOne;
	Gls::Material::TGLShader* FShaderTwo;
	void __fastcall SetShaderOne(Gls::Material::TGLShader* const Value);
	void __fastcall SetShaderTwo(Gls::Material::TGLShader* const Value);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	__property TGLShaderCombinerType CombinerType = {read=FCombinerType, write=FCombinerType, default=0};
	__property Gls::Material::TGLShader* ShaderOne = {read=FShaderOne, write=SetShaderOne};
	__property Gls::Material::TGLShader* ShaderTwo = {read=FShaderTwo, write=SetShaderTwo};
	__property int CurrentPass = {read=FCurrentPass, stored=false, nodefault};
	
public:
	__fastcall virtual TGLCustomShaderCombiner(System::Classes::TComponent* AOwner);
	virtual bool __fastcall ShaderSupported();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
public:
	/* TGLShader.Destroy */ inline __fastcall virtual ~TGLCustomShaderCombiner() { }
	
};


class PASCALIMPLEMENTATION TGLShaderCombiner : public TGLCustomShaderCombiner
{
	typedef TGLCustomShaderCombiner inherited;
	
__published:
	__property CombinerType = {default=0};
	__property ShaderOne;
	__property ShaderTwo;
	__property ShaderStyle = {default=1};
public:
	/* TGLCustomShaderCombiner.Create */ inline __fastcall virtual TGLShaderCombiner(System::Classes::TComponent* AOwner) : TGLCustomShaderCombiner(AOwner) { }
	
public:
	/* TGLShader.Destroy */ inline __fastcall virtual ~TGLShaderCombiner() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Shadercombiner */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_SHADERCOMBINER)
using namespace Glsl::Shadercombiner;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_ShadercombinerHPP
