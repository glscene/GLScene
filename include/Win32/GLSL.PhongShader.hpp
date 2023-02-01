// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.PhongShader.pas' rev: 35.00 (Windows)

#ifndef Glsl_PhongshaderHPP
#define Glsl_PhongshaderHPP

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
#include <GLS.Texture.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Context.hpp>
#include <GLSL.AsmShader.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLSL.CustomShader.hpp>
#include <GLS.State.hpp>
#include <GLS.Material.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Phongshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLPhongShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLPhongShader : public Glsl::Asmshader::TGLCustomAsmShader
{
	typedef Glsl::Asmshader::TGLCustomAsmShader inherited;
	
private:
	Gls::Vectorlists::TGLIntegerList* FLightIDs;
	bool FDesignTimeEnabled;
	bool FAmbientPass;
	void __fastcall SetDesignTimeEnabled(const bool Value);
	
protected:
	virtual void __fastcall DoLightPass(unsigned lightID);
	virtual void __fastcall DoAmbientPass(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall UnApplyLights(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLPhongShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPhongShader();
	virtual bool __fastcall ShaderSupported();
	
__published:
	__property bool DesignTimeEnabled = {read=FDesignTimeEnabled, write=SetDesignTimeEnabled, default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Phongshader */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_PHONGSHADER)
using namespace Glsl::Phongshader;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_PhongshaderHPP
