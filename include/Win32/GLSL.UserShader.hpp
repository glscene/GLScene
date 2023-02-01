// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.UserShader.pas' rev: 35.00 (Windows)

#ifndef Glsl_UsershaderHPP
#define Glsl_UsershaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <GLS.Material.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Usershader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLUserShader;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TOnDoApplyEvent)(System::TObject* Sender, Gls::Rendercontextinfo::TGLRenderContextInfo &rci);

typedef void __fastcall (__closure *TOnDoUnApplyEvent)(System::TObject* Sender, int Pass, Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool &Continue);

class PASCALIMPLEMENTATION TGLUserShader : public Gls::Material::TGLShader
{
	typedef Gls::Material::TGLShader inherited;
	
private:
	int FPass;
	TOnDoApplyEvent FOnDoApply;
	TOnDoUnApplyEvent FOnDoUnApply;
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property TOnDoApplyEvent OnDoApply = {read=FOnDoApply, write=FOnDoApply};
	__property TOnDoUnApplyEvent OnDoUnApply = {read=FOnDoUnApply, write=FOnDoUnApply};
	__property ShaderStyle = {default=1};
public:
	/* TGLShader.Create */ inline __fastcall virtual TGLUserShader(System::Classes::TComponent* AOwner) : Gls::Material::TGLShader(AOwner) { }
	/* TGLShader.Destroy */ inline __fastcall virtual ~TGLUserShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Usershader */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_USERSHADER)
using namespace Glsl::Usershader;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_UsershaderHPP
