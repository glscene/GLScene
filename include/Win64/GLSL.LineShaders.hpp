// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.LineShaders.pas' rev: 35.00 (Windows)

#ifndef Glsl_LineshadersHPP
#define Glsl_LineshadersHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Color.hpp>
#include <GLS.Material.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>
#include <GLS.Context.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Lineshaders
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLLineSettings;
class DELPHICLASS TGLHiddenLineShader;
class DELPHICLASS TGLOutlineShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLLineSettings : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Gls::Color::TGLColor* FColor;
	float FWidth;
	System::Word FPattern;
	bool FForceMaterial;
	void __fastcall SetPattern(const System::Word value);
	void __fastcall SetColor(Gls::Color::TGLColor* const v);
	void __fastcall SetWidth(const float Value);
	void __fastcall SetForceMaterial(bool v);
	
public:
	__fastcall virtual TGLLineSettings(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLLineSettings();
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property float Width = {read=FWidth, write=SetWidth};
	__property Gls::Color::TGLColor* Color = {read=FColor, write=SetColor};
	__property System::Word Pattern = {read=FPattern, write=SetPattern, default=65535};
	__property bool ForceMaterial = {read=FForceMaterial, write=SetForceMaterial, default=0};
};


class PASCALIMPLEMENTATION TGLHiddenLineShader : public Gls::Material::TGLShader
{
	typedef Gls::Material::TGLShader inherited;
	
private:
	int FPassCount;
	bool FLineSmooth;
	bool FSolid;
	Gls::Color::TGLColor* FBackGroundColor;
	TGLLineSettings* FFrontLine;
	TGLLineSettings* FBackLine;
	bool FLighting;
	Gls::Scene::TGLShadeModel FShadeModel;
	void __fastcall SetlineSmooth(bool v);
	void __fastcall SetSolid(bool v);
	void __fastcall SetBackgroundColor(Gls::Color::TGLColor* AColor);
	void __fastcall SetLighting(bool v);
	void __fastcall SetShadeModel(const Gls::Scene::TGLShadeModel val);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLHiddenLineShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLHiddenLineShader();
	
__published:
	__property TGLLineSettings* FrontLine = {read=FFrontLine, write=FFrontLine};
	__property TGLLineSettings* BackLine = {read=FBackLine, write=FBackLine};
	__property bool LineSmooth = {read=FLineSmooth, write=SetlineSmooth, default=0};
	__property bool Solid = {read=FSolid, write=SetSolid, default=0};
	__property Gls::Color::TGLColor* BackgroundColor = {read=FBackGroundColor, write=SetBackgroundColor};
	__property bool SurfaceLit = {read=FLighting, write=SetLighting, default=1};
	__property Gls::Scene::TGLShadeModel ShadeModel = {read=FShadeModel, write=SetShadeModel, default=0};
};


class PASCALIMPLEMENTATION TGLOutlineShader : public Gls::Material::TGLShader
{
	typedef Gls::Material::TGLShader inherited;
	
private:
	int FPassCount;
	Gls::Color::TGLColor* FLineColor;
	bool FOutlineSmooth;
	float FOutlineWidth;
	void __fastcall SetOutlineWidth(float v);
	void __fastcall SetOutlineSmooth(bool v);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLOutlineShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLOutlineShader();
	
__published:
	__property Gls::Color::TGLColor* LineColor = {read=FLineColor, write=FLineColor};
	__property bool LineSmooth = {read=FOutlineSmooth, write=SetOutlineSmooth, default=0};
	__property float LineWidth = {read=FOutlineWidth, write=SetOutlineWidth};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Lineshaders */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_LINESHADERS)
using namespace Glsl::Lineshaders;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_LineshadersHPP
