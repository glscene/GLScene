// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.LensFlare.pas' rev: 35.00 (Windows)

#ifndef Gls_LensflareHPP
#define Gls_LensflareHPP

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
#include <System.Math.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Context.hpp>
#include <GLS.Color.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Utils.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.Coordinates.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Lensflare
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLFlareGradient;
class DELPHICLASS TGLLensFlare;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLFlareElement : unsigned char { feGlow, feRing, feStreaks, feRays, feSecondaries };

typedef System::Set<TGLFlareElement, TGLFlareElement::feGlow, TGLFlareElement::feSecondaries> TGLFlareElements;

class PASCALIMPLEMENTATION TGLFlareGradient : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Gls::Color::TGLColor* FFromColor;
	Gls::Color::TGLColor* FToColor;
	
protected:
	void __fastcall SetFromColor(Gls::Color::TGLColor* const val);
	void __fastcall SetToColor(Gls::Color::TGLColor* const val);
	
public:
	__fastcall virtual TGLFlareGradient(System::Classes::TPersistent* AOwner);
	__fastcall TGLFlareGradient(System::Classes::TPersistent* AOwner, const Gls::Vectortypes::TVector4f &fromColor, const Gls::Vectortypes::TVector4f &toColor);
	__fastcall virtual ~TGLFlareGradient();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Gls::Color::TGLColor* FromColor = {read=FFromColor, write=SetFromColor};
	__property Gls::Color::TGLColor* ToColor = {read=FToColor, write=SetToColor};
};


class PASCALIMPLEMENTATION TGLLensFlare : public Gls::Scene::TGLBaseSceneObject
{
	typedef Gls::Scene::TGLBaseSceneObject inherited;
	
	
private:
	typedef System::DynamicArray<float> _TGLLensFlare__1;
	
	typedef System::DynamicArray<float> _TGLLensFlare__2;
	
	
private:
	int FSize;
	float FDeltaTime;
	float FCurrSize;
	int FSeed;
	float FSqueeze;
	int FNumStreaks;
	float FStreakWidth;
	float FStreakAngle;
	int FNumSecs;
	int FResolution;
	bool FAutoZTest;
	TGLFlareElements FElements;
	_TGLLensFlare__1 FSin20Res;
	_TGLLensFlare__1 FCos20Res;
	_TGLLensFlare__2 FSinRes;
	_TGLLensFlare__2 FCosRes;
	Gls::Context::TGLTextureHandle* FTexRays;
	bool FFlareIsNotOccluded;
	Gls::Context::TGLOcclusionQueryHandle* FOcclusionQuery;
	TGLFlareGradient* FGlowGradient;
	TGLFlareGradient* FRingGradient;
	TGLFlareGradient* FStreaksGradient;
	TGLFlareGradient* FRaysGradient;
	TGLFlareGradient* FSecondariesGradient;
	bool FDynamic;
	Gls::Scene::TGLRenderPoint* FPreRenderPoint;
	
protected:
	void __fastcall SetGlowGradient(TGLFlareGradient* const val);
	void __fastcall SetRingGradient(TGLFlareGradient* const val);
	void __fastcall SetStreaksGradient(TGLFlareGradient* const val);
	void __fastcall SetRaysGradient(TGLFlareGradient* const val);
	void __fastcall SetSecondariesGradient(TGLFlareGradient* const val);
	void __fastcall SetSize(int aValue);
	void __fastcall SetSeed(int aValue);
	void __fastcall SetSqueeze(float aValue);
	bool __fastcall StoreSqueeze();
	void __fastcall SetNumStreaks(int aValue);
	void __fastcall SetStreakWidth(float aValue);
	bool __fastcall StoreStreakWidth();
	void __fastcall SetStreakAngle(float aValue);
	void __fastcall SetNumSecs(int aValue);
	void __fastcall SetResolution(int aValue);
	void __fastcall SetAutoZTest(bool aValue);
	void __fastcall SetElements(TGLFlareElements aValue);
	void __fastcall SetDynamic(bool aValue);
	void __fastcall SetPreRenderPoint(Gls::Scene::TGLRenderPoint* const val);
	void __fastcall PreRenderEvent(System::TObject* Sender, Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall PreRenderPointFreed(System::TObject* Sender);
	void __fastcall SetupRenderingOptions(Gls::State::TGLStateCache* StateCache);
	void __fastcall RenderRays(Gls::State::TGLStateCache* StateCache, const float size);
	void __fastcall RenderStreaks(Gls::State::TGLStateCache* StateCache);
	void __fastcall RenderRing();
	void __fastcall RenderSecondaries(const Gls::Vectortypes::TVector3f &posVector);
	
public:
	__fastcall virtual TGLLensFlare(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLLensFlare();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	void __fastcall PreRender(Gls::Scene::TGLSceneBuffer* activeBuffer);
	__property float FlareInstantaneousSize = {read=FCurrSize, write=FCurrSize};
	
__published:
	__property TGLFlareGradient* GlowGradient = {read=FGlowGradient, write=SetGlowGradient};
	__property TGLFlareGradient* RingGradient = {read=FRingGradient};
	__property TGLFlareGradient* StreaksGradient = {read=FStreaksGradient};
	__property TGLFlareGradient* RaysGradient = {read=FRaysGradient};
	__property TGLFlareGradient* SecondariesGradient = {read=FSecondariesGradient};
	__property int Size = {read=FSize, write=SetSize, default=50};
	__property int Seed = {read=FSeed, write=SetSeed, nodefault};
	__property float Squeeze = {read=FSqueeze, write=SetSqueeze, stored=StoreSqueeze};
	__property int NumStreaks = {read=FNumStreaks, write=SetNumStreaks, default=4};
	__property float StreakWidth = {read=FStreakWidth, write=SetStreakWidth, stored=StoreStreakWidth};
	__property float StreakAngle = {read=FStreakAngle, write=SetStreakAngle};
	__property int NumSecs = {read=FNumSecs, write=SetNumSecs, default=8};
	__property int Resolution = {read=FResolution, write=SetResolution, default=64};
	__property bool AutoZTest = {read=FAutoZTest, write=SetAutoZTest, default=1};
	__property bool FlareIsNotOccluded = {read=FFlareIsNotOccluded, write=FFlareIsNotOccluded, nodefault};
	__property TGLFlareElements Elements = {read=FElements, write=SetElements, default=31};
	__property bool Dynamic = {read=FDynamic, write=FDynamic, default=1};
	__property Gls::Scene::TGLRenderPoint* PreRenderPoint = {read=FPreRenderPoint, write=SetPreRenderPoint};
	__property ObjectsSorting = {default=0};
	__property Position;
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
	__property Effects;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLLensFlare(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define cDefaultFlareElements (System::Set<TGLFlareElement, TGLFlareElement::feGlow, TGLFlareElement::feSecondaries>() << TGLFlareElement::feGlow << TGLFlareElement::feRing << TGLFlareElement::feStreaks << TGLFlareElement::feRays << TGLFlareElement::feSecondaries )
}	/* namespace Lensflare */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_LENSFLARE)
using namespace Gls::Lensflare;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_LensflareHPP
