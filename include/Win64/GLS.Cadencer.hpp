// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Cadencer.pas' rev: 35.00 (Windows)

#ifndef Gls_CadencerHPP
#define Gls_CadencerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <Vcl.Forms.hpp>
#include <GLS.Scene.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Cadencer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCadencer;
class DELPHICLASS TGLCustomCadencedComponent;
class DELPHICLASS TGLCadencedComponent;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLCadencerMode : unsigned char { cmManual, cmASAP, cmApplicationIdle };

enum DECLSPEC_DENUM TGLCadencerTimeReference : unsigned char { cmRTC, cmPerformanceCounter, cmExternal };

class PASCALIMPLEMENTATION TGLCadencer : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TList* FSubscribedCadenceableComponents;
	Gls::Scene::TGLScene* FScene;
	double FTimeMultiplier;
	double lastTime;
	double downTime;
	double lastMultiplier;
	bool FEnabled;
	int FSleepLength;
	TGLCadencerMode FMode;
	TGLCadencerTimeReference FTimeReference;
	double FCurrentTime;
	double FOriginTime;
	double FMaxDeltaTime;
	double FMinDeltaTime;
	double FFixedDeltaTime;
	Gls::Baseclasses::TGLProgressEvent FOnProgress;
	Gls::Baseclasses::TGLProgressEvent FOnTotalProgress;
	int FProgressing;
	void __fastcall SetCurrentTime(const double Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	bool __fastcall StoreTimeMultiplier();
	void __fastcall SetEnabled(const bool val);
	void __fastcall SetScene(Gls::Scene::TGLScene* const val);
	void __fastcall SetMode(const TGLCadencerMode val);
	void __fastcall SetTimeReference(const TGLCadencerTimeReference val);
	void __fastcall SetTimeMultiplier(const double val);
	double __fastcall GetRawReferenceTime();
	void __fastcall RestartASAP();
	virtual void __fastcall Loaded();
	void __fastcall OnIdleEvent(System::TObject* Sender, bool &Done);
	
public:
	__fastcall virtual TGLCadencer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCadencer();
	void __fastcall Subscribe(Gls::Baseclasses::TGLCadenceAbleComponent* aComponent);
	void __fastcall UnSubscribe(Gls::Baseclasses::TGLCadenceAbleComponent* aComponent);
	void __fastcall Progress();
	double __fastcall GetCurrenttime();
	bool __fastcall IsBusy();
	void __fastcall Reset();
	__property double OriginTime = {read=FOriginTime, write=FOriginTime};
	__property double CurrentTime = {read=FCurrentTime, write=SetCurrentTime};
	
__published:
	__property Gls::Scene::TGLScene* Scene = {read=FScene, write=SetScene};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
	__property TGLCadencerTimeReference TimeReference = {read=FTimeReference, write=SetTimeReference, default=1};
	__property double TimeMultiplier = {read=FTimeMultiplier, write=SetTimeMultiplier, stored=StoreTimeMultiplier};
	__property double MaxDeltaTime = {read=FMaxDeltaTime, write=FMaxDeltaTime};
	__property double MinDeltaTime = {read=FMinDeltaTime, write=FMinDeltaTime};
	__property double FixedDeltaTime = {read=FFixedDeltaTime, write=FFixedDeltaTime};
	__property TGLCadencerMode Mode = {read=FMode, write=SetMode, default=1};
	__property int SleepLength = {read=FSleepLength, write=FSleepLength, default=-1};
	__property Gls::Baseclasses::TGLProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property Gls::Baseclasses::TGLProgressEvent OnTotalProgress = {read=FOnTotalProgress, write=FOnTotalProgress};
};


class PASCALIMPLEMENTATION TGLCustomCadencedComponent : public Gls::Baseclasses::TGLUpdateAbleComponent
{
	typedef Gls::Baseclasses::TGLUpdateAbleComponent inherited;
	
private:
	TGLCadencer* FCadencer;
	
protected:
	void __fastcall SetCadencer(TGLCadencer* const val);
	__property TGLCadencer* Cadencer = {read=FCadencer, write=SetCadencer};
	
public:
	__fastcall virtual ~TGLCustomCadencedComponent();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
public:
	/* TComponent.Create */ inline __fastcall virtual TGLCustomCadencedComponent(System::Classes::TComponent* AOwner) : Gls::Baseclasses::TGLUpdateAbleComponent(AOwner) { }
	
};


class PASCALIMPLEMENTATION TGLCadencedComponent : public TGLCustomCadencedComponent
{
	typedef TGLCustomCadencedComponent inherited;
	
__published:
	__property Cadencer;
public:
	/* TGLCustomCadencedComponent.Destroy */ inline __fastcall virtual ~TGLCadencedComponent() { }
	
public:
	/* TComponent.Create */ inline __fastcall virtual TGLCadencedComponent(System::Classes::TComponent* AOwner) : TGLCustomCadencedComponent(AOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Cadencer */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_CADENCER)
using namespace Gls::Cadencer;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_CadencerHPP
