// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.HUDObjects.pas' rev: 35.00 (Windows)

#ifndef Gls_HudobjectsHPP
#define Gls_HudobjectsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <Vcl.StdCtrls.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Objects.hpp>
#include <GLS.BitmapFont.hpp>
#include <GLS.Color.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Material.hpp>
#include <GLS.Texture.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Hudobjects
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLHUDSprite;
class DELPHICLASS TGLHUDText;
class DELPHICLASS TGLAbsoluteHUDText;
class DELPHICLASS TGLResolutionIndependantHUDText;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLHUDSprite : public Gls::Objects::TGLSprite
{
	typedef Gls::Objects::TGLSprite inherited;
	
private:
	int FXTiles;
	int FYTiles;
	bool __fastcall StoreWidth();
	bool __fastcall StoreHeight();
	
protected:
	void __fastcall SetXTiles(const int val);
	void __fastcall SetYTiles(const int val);
	
public:
	__fastcall virtual TGLHUDSprite(System::Classes::TComponent* AOwner);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property int XTiles = {read=FXTiles, write=SetXTiles, default=1};
	__property int YTiles = {read=FYTiles, write=SetYTiles, default=1};
	__property Width = {stored=StoreWidth, default=0};
	__property Height = {stored=StoreHeight, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLHUDSprite() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLHUDSprite(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Objects::TGLSprite(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLHUDText : public Gls::Scene::TGLImmaterialSceneObject
{
	typedef Gls::Scene::TGLImmaterialSceneObject inherited;
	
private:
	Gls::Bitmapfont::TGLCustomBitmapFont* FBitmapFont;
	System::UnicodeString FText;
	float FRotation;
	System::Classes::TAlignment FAlignment;
	Vcl::Stdctrls::TTextLayout FLayout;
	Gls::Color::TGLColor* FModulateColor;
	
protected:
	void __fastcall SetBitmapFont(Gls::Bitmapfont::TGLCustomBitmapFont* const val);
	void __fastcall SetText(const System::UnicodeString val);
	HIDESBASE void __fastcall SetRotation(const float val);
	void __fastcall SetAlignment(const System::Classes::TAlignment val);
	void __fastcall SetLayout(const Vcl::Stdctrls::TTextLayout val);
	void __fastcall SetModulateColor(Gls::Color::TGLColor* const val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall RenderTextAtPosition(const float X, const float Y, const float Z, Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLHUDText(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLHUDText();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property Gls::Bitmapfont::TGLCustomBitmapFont* BitmapFont = {read=FBitmapFont, write=SetBitmapFont};
	__property System::UnicodeString Text = {read=FText, write=SetText};
	__property float Rotation = {read=FRotation, write=SetRotation};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property Vcl::Stdctrls::TTextLayout Layout = {read=FLayout, write=SetLayout, default=0};
	__property Gls::Color::TGLColor* ModulateColor = {read=FModulateColor, write=SetModulateColor};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLHUDText(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLAbsoluteHUDText : public TGLHUDText
{
	typedef TGLHUDText inherited;
	
public:
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
public:
	/* TGLHUDText.Create */ inline __fastcall virtual TGLAbsoluteHUDText(System::Classes::TComponent* AOwner) : TGLHUDText(AOwner) { }
	/* TGLHUDText.Destroy */ inline __fastcall virtual ~TGLAbsoluteHUDText() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLAbsoluteHUDText(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLHUDText(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLResolutionIndependantHUDText : public TGLHUDText
{
	typedef TGLHUDText inherited;
	
public:
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	__fastcall virtual TGLResolutionIndependantHUDText(System::Classes::TComponent* AOwner);
public:
	/* TGLHUDText.Destroy */ inline __fastcall virtual ~TGLResolutionIndependantHUDText() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLResolutionIndependantHUDText(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLHUDText(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Hudobjects */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_HUDOBJECTS)
using namespace Gls::Hudobjects;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_HudobjectsHPP
