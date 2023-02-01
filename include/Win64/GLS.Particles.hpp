// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Particles.pas' rev: 35.00 (Windows)

#ifndef Gls_ParticlesHPP
#define Gls_ParticlesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Scene.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Context.hpp>
#include <GLS.Color.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Particles
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLParticles;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TGLParticleEvent)(System::TObject* Sender, Gls::Scene::TGLBaseSceneObject* particle);

class PASCALIMPLEMENTATION TGLParticles : public Gls::Scene::TGLImmaterialSceneObject
{
	typedef Gls::Scene::TGLImmaterialSceneObject inherited;
	
private:
	float FCubeSize;
	Gls::Color::TGLColor* FEdgeColor;
	bool FVisibleAtRunTime;
	System::Classes::TList* particlePool;
	int FParticlePoolSize;
	TGLParticleEvent FOnCreateParticle;
	TGLParticleEvent FOnActivateParticle;
	TGLParticleEvent FOnKillParticle;
	TGLParticleEvent FOnDestroyParticle;
	Gls::Scene::TGLDirectRenderEvent FOnBeforeRenderParticles;
	Gls::Scene::TGLDirectRenderEvent FOnAfterRenderParticles;
	
protected:
	void __fastcall SetCubeSize(const float val);
	void __fastcall SetEdgeColor(Gls::Color::TGLColor* const val);
	void __fastcall SetVisibleAtRunTime(const bool val);
	void __fastcall SetParticlePoolSize(int val);
	void __fastcall ClearParticlePool();
	
public:
	__fastcall virtual TGLParticles(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLParticles();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	Gls::Scene::TGLBaseSceneObject* __fastcall CreateParticle();
	void __fastcall KillParticle(Gls::Scene::TGLBaseSceneObject* aParticle);
	void __fastcall KillParticles();
	
__published:
	__property float CubeSize = {read=FCubeSize, write=SetCubeSize};
	__property Gls::Color::TGLColor* EdgeColor = {read=FEdgeColor, write=SetEdgeColor};
	__property bool VisibleAtRunTime = {read=FVisibleAtRunTime, write=SetVisibleAtRunTime, default=0};
	__property int ParticlePoolSize = {read=FParticlePoolSize, write=SetParticlePoolSize, default=0};
	__property TGLParticleEvent OnCreateParticle = {read=FOnCreateParticle, write=FOnCreateParticle};
	__property TGLParticleEvent OnActivateParticle = {read=FOnActivateParticle, write=FOnActivateParticle};
	__property TGLParticleEvent OnKillParticle = {read=FOnKillParticle, write=FOnKillParticle};
	__property TGLParticleEvent OnDestroyParticle = {read=FOnDestroyParticle, write=FOnDestroyParticle};
	__property Gls::Scene::TGLDirectRenderEvent OnBeforeRenderParticles = {read=FOnBeforeRenderParticles, write=FOnBeforeRenderParticles};
	__property Gls::Scene::TGLDirectRenderEvent OnAfterRenderParticles = {read=FOnAfterRenderParticles, write=FOnAfterRenderParticles};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLParticles(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Particles */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_PARTICLES)
using namespace Gls::Particles;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ParticlesHPP
