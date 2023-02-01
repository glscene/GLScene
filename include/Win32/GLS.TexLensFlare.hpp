// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.TexLensFlare.pas' rev: 35.00 (Windows)

#ifndef Gls_TexlensflareHPP
#define Gls_TexlensflareHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <GLS.Scene.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Texture.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Context.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.State.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Coordinates.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Texlensflare
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTextureLensFlare;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLTextureLensFlare : public Gls::Scene::TGLBaseSceneObject
{
	typedef Gls::Scene::TGLBaseSceneObject inherited;
	
private:
	int FSize;
	float FCurrSize;
	int FNumSecs;
	bool FAutoZTest;
	double FDeltaTime;
	Gls::Texture::TGLTexture* FImgSecondaries;
	Gls::Texture::TGLTexture* FImgRays;
	Gls::Texture::TGLTexture* FImgRing;
	Gls::Texture::TGLTexture* FImgGlow;
	int FSeed;
	void __fastcall SetImgGlow(Gls::Texture::TGLTexture* const Value);
	void __fastcall SetImgRays(Gls::Texture::TGLTexture* const Value);
	void __fastcall SetImgRing(Gls::Texture::TGLTexture* const Value);
	void __fastcall SetImgSecondaries(Gls::Texture::TGLTexture* const Value);
	void __fastcall SetSeed(const int Value);
	
protected:
	void __fastcall SetSize(int aValue);
	void __fastcall SetNumSecs(int aValue);
	void __fastcall SetAutoZTest(bool aValue);
	
public:
	__fastcall virtual TGLTextureLensFlare(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTextureLensFlare();
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	
__published:
	__property int Size = {read=FSize, write=SetSize, default=50};
	__property int Seed = {read=FSeed, write=SetSeed, nodefault};
	__property int NumSecs = {read=FNumSecs, write=SetNumSecs, default=8};
	__property bool AutoZTest = {read=FAutoZTest, write=SetAutoZTest, default=1};
	__property Gls::Texture::TGLTexture* ImgGlow = {read=FImgGlow, write=SetImgGlow};
	__property Gls::Texture::TGLTexture* ImgRays = {read=FImgRays, write=SetImgRays};
	__property Gls::Texture::TGLTexture* ImgRing = {read=FImgRing, write=SetImgRing};
	__property Gls::Texture::TGLTexture* ImgSecondaries = {read=FImgSecondaries, write=SetImgSecondaries};
	__property ObjectsSorting = {default=0};
	__property Position;
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
	__property Effects;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTextureLensFlare(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Texlensflare */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_TEXLENSFLARE)
using namespace Gls::Texlensflare;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_TexlensflareHPP
