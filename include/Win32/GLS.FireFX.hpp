// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FireFX.pas' rev: 35.00 (Windows)

#ifndef Gls_FirefxHPP
#define Gls_FirefxHPP

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
#include <System.Types.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Context.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Cadencer.hpp>
#include <GLS.Color.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.Manager.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>
#include <GLS.TextureFormat.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Firefx
{
//-- forward type declarations -----------------------------------------------
struct TGLFireParticle;
class DELPHICLASS TGLFireFXManager;
class DELPHICLASS TGLBFireFX;
//-- type declarations -------------------------------------------------------
typedef TGLFireParticle *PGLFireParticle;

struct DECLSPEC_DRECORD TGLFireParticle
{
public:
	Gls::Vectortypes::TVector4f Position;
	Gls::Vectortypes::TVector4f Speed;
	float Alpha;
	float TimeToLive;
	float LifeLength;
};


typedef System::StaticArray<TGLFireParticle, 33554432> TGLFireParticleArray;

typedef TGLFireParticleArray *PGLFireParticleArray;

class PASCALIMPLEMENTATION TGLFireFXManager : public Gls::Baseclasses::TGLCadenceAbleComponent
{
	typedef Gls::Baseclasses::TGLCadenceAbleComponent inherited;
	
private:
	System::Classes::TList* FClients;
	TGLFireParticleArray *FFireParticles;
	Gls::Coordinates::TGLCoordinates3* FFireDir;
	Gls::Coordinates::TGLCoordinates3* FInitialDir;
	Gls::Cadencer::TGLCadencer* FCadencer;
	int FMaxParticles;
	int FParticleLife;
	float FParticleSize;
	float FFireDensity;
	float FFireEvaporation;
	float FFireCrown;
	float FParticleInterval;
	float IntervalDelta;
	int NP;
	Gls::Color::TGLColor* FInnerColor;
	Gls::Color::TGLColor* FOuterColor;
	float FFireBurst;
	float FFireRadius;
	bool FDisabled;
	bool FPaused;
	bool FUseInterval;
	Gls::Scene::TGLBaseSceneObject* FReference;
	bool FNoZWrite;
	
protected:
	void __fastcall RegisterClient(TGLBFireFX* aClient);
	void __fastcall DeRegisterClient(TGLBFireFX* aClient);
	void __fastcall DeRegisterAllClients();
	void __fastcall SetFireDir(Gls::Coordinates::TGLCoordinates3* const val);
	void __fastcall SetInitialDir(Gls::Coordinates::TGLCoordinates3* const val);
	void __fastcall SetCadencer(Gls::Cadencer::TGLCadencer* const val);
	bool __fastcall StoreParticleSize();
	void __fastcall SetInnerColor(Gls::Color::TGLColor* const val);
	void __fastcall SetOuterColor(Gls::Color::TGLColor* const val);
	HIDESBASE void __fastcall SetReference(Gls::Scene::TGLBaseSceneObject* const val);
	void __fastcall SetMaxParticles(const int val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall CalcFire(double deltaTime, float ParticleInterval, float ParticleLife, float FireAlpha);
	void __fastcall AffParticle3d(const Gls::Vectortypes::TVector4f &Color2, const Gls::Vectortypes::TMatrix4f &mat);
	
public:
	__fastcall virtual TGLFireFXManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLFireFXManager();
	void __fastcall FireInit();
	void __fastcall IsotropicExplosion(float minInitialSpeed, float maxInitialSpeed, float lifeBoostFactor, int nbParticles = 0xffffffff);
	void __fastcall RingExplosion(float minInitialSpeed, float maxInitialSpeed, float lifeBoostFactor, const Gls::Vectortypes::TVector3f &ringVectorX, const Gls::Vectortypes::TVector3f &ringVectorY, int nbParticles = 0xffffffff);
	__property int ParticleCount = {read=NP, nodefault};
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	
__published:
	__property Gls::Coordinates::TGLCoordinates3* FireDir = {read=FFireDir, write=SetFireDir};
	__property Gls::Coordinates::TGLCoordinates3* InitialDir = {read=FInitialDir, write=SetInitialDir};
	__property Gls::Cadencer::TGLCadencer* Cadencer = {read=FCadencer, write=SetCadencer};
	__property int MaxParticles = {read=FMaxParticles, write=SetMaxParticles, default=256};
	__property float ParticleSize = {read=FParticleSize, write=FParticleSize, stored=StoreParticleSize};
	__property Gls::Color::TGLColor* InnerColor = {read=FInnerColor, write=SetInnerColor};
	__property Gls::Color::TGLColor* OuterColor = {read=FOuterColor, write=SetOuterColor};
	__property float FireDensity = {read=FFireDensity, write=FFireDensity};
	__property float FireEvaporation = {read=FFireEvaporation, write=FFireEvaporation};
	__property float FireCrown = {read=FFireCrown, write=FFireCrown};
	__property int ParticleLife = {read=FParticleLife, write=FParticleLife, default=3};
	__property float FireBurst = {read=FFireBurst, write=FFireBurst};
	__property float FireRadius = {read=FFireRadius, write=FFireRadius};
	__property bool Disabled = {read=FDisabled, write=FDisabled, nodefault};
	__property bool Paused = {read=FPaused, write=FPaused, nodefault};
	__property float ParticleInterval = {read=FParticleInterval, write=FParticleInterval};
	__property bool UseInterval = {read=FUseInterval, write=FUseInterval, nodefault};
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, default=1};
	__property Gls::Scene::TGLBaseSceneObject* Reference = {read=FReference, write=SetReference};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBFireFX : public Gls::Scene::TGLObjectPostEffect
{
	typedef Gls::Scene::TGLObjectPostEffect inherited;
	
private:
	TGLFireFXManager* FManager;
	System::UnicodeString FManagerName;
	
protected:
	void __fastcall SetManager(TGLFireFXManager* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	
public:
	__fastcall virtual TGLBFireFX(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBFireFX();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall Render(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property TGLFireFXManager* Manager = {read=FManager, write=SetManager};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLBFireFX* __fastcall GetOrCreateFireFX(Gls::Scene::TGLEffects* effects)/* overload */;
extern DELPHI_PACKAGE TGLBFireFX* __fastcall GetOrCreateFireFX(Gls::Scene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Firefx */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FIREFX)
using namespace Gls::Firefx;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FirefxHPP
