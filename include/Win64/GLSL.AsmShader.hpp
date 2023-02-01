// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.AsmShader.pas' rev: 35.00 (Windows)

#ifndef Glsl_AsmshaderHPP
#define Glsl_AsmshaderHPP

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
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Context.hpp>
#include <GLSL.CustomShader.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Material.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Asmshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLAsmShaderParameter;
class DELPHICLASS TGLCustomAsmShader;
class DELPHICLASS TGLAsmShader;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TGLAsmShaderEvent)(TGLCustomAsmShader* Shader);

typedef void __fastcall (__closure *TGLAsmShaderUnUplyEvent)(TGLCustomAsmShader* Shader, bool &ThereAreMorePasses);

class PASCALIMPLEMENTATION TGLAsmShaderParameter : public Glsl::Customshader::TGLCustomShaderParameter
{
	typedef Glsl::Customshader::TGLCustomShaderParameter inherited;
	
public:
	/* TObject.Create */ inline __fastcall TGLAsmShaderParameter() : Glsl::Customshader::TGLCustomShaderParameter() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLAsmShaderParameter() { }
	
};


class PASCALIMPLEMENTATION TGLCustomAsmShader : public Glsl::Customshader::TGLCustomShader
{
	typedef Glsl::Customshader::TGLCustomShader inherited;
	
private:
	Gls::Context::TGLARBVertexProgramHandle* FVPHandle;
	Gls::Context::TGLARBFragmentProgramHandle* FFPHandle;
	Gls::Context::TGLARBGeometryProgramHandle* FGPHandle;
	TGLAsmShaderEvent FOnInitialize;
	TGLAsmShaderEvent FOnApply;
	TGLAsmShaderUnUplyEvent FOnUnApply;
	
protected:
	void __fastcall ApplyShaderPrograms();
	void __fastcall UnApplyShaderPrograms();
	virtual void __fastcall DestroyARBPrograms();
	__property TGLAsmShaderEvent OnApply = {read=FOnApply, write=FOnApply};
	__property TGLAsmShaderUnUplyEvent OnUnApply = {read=FOnUnApply, write=FOnUnApply};
	__property TGLAsmShaderEvent OnInitialize = {read=FOnInitialize, write=FOnInitialize};
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoFinalize();
	
public:
	__fastcall virtual TGLCustomAsmShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomAsmShader();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual bool __fastcall ShaderSupported();
};


class PASCALIMPLEMENTATION TGLAsmShader : public TGLCustomAsmShader
{
	typedef TGLCustomAsmShader inherited;
	
__published:
	__property FragmentProgram;
	__property VertexProgram;
	__property GeometryProgram;
	__property OnApply;
	__property OnUnApply;
	__property OnInitialize;
	__property ShaderStyle = {default=1};
	__property FailedInitAction = {default=1};
public:
	/* TGLCustomAsmShader.Create */ inline __fastcall virtual TGLAsmShader(System::Classes::TComponent* AOwner) : TGLCustomAsmShader(AOwner) { }
	/* TGLCustomAsmShader.Destroy */ inline __fastcall virtual ~TGLAsmShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Asmshader */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_ASMSHADER)
using namespace Glsl::Asmshader;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_AsmshaderHPP
