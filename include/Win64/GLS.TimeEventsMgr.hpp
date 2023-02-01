// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.TimeEventsMgr.pas' rev: 35.00 (Windows)

#ifndef Gls_TimeeventsmgrHPP
#define Gls_TimeeventsmgrHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.Cadencer.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Timeeventsmgr
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTimeEventsMGR;
class DELPHICLASS TTimeEvents;
class DELPHICLASS TTimeEvent;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLTimeEventsMGR : public Gls::Baseclasses::TGLUpdateAbleComponent
{
	typedef Gls::Baseclasses::TGLUpdateAbleComponent inherited;
	
private:
	Gls::Cadencer::TGLCadencer* FCadencer;
	bool FEnabled;
	bool FFreeEventOnEnd;
	TTimeEvents* FEvents;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetCadencer(Gls::Cadencer::TGLCadencer* const val);
	void __fastcall SetEvents(TTimeEvents* const val);
	
public:
	__fastcall virtual TGLTimeEventsMGR(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLTimeEventsMGR();
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	void __fastcall Reset();
	
__published:
	__property Gls::Cadencer::TGLCadencer* Cadencer = {read=FCadencer, write=SetCadencer};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=1};
	__property bool FreeEventOnEnd = {read=FFreeEventOnEnd, write=FFreeEventOnEnd, default=0};
	__property TTimeEvents* Events = {read=FEvents, write=SetEvents};
};


class PASCALIMPLEMENTATION TTimeEvents : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TTimeEvent* operator[](int index) { return this->Items[index]; }
	
protected:
	System::Classes::TComponent* Owner;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	void __fastcall SetItems(int index, TTimeEvent* const val);
	TTimeEvent* __fastcall GetItems(int index);
	
public:
	__fastcall TTimeEvents(System::Classes::TComponent* aOwner);
	HIDESBASE TTimeEvent* __fastcall Add();
	HIDESBASE TTimeEvent* __fastcall FindItemID(int ID);
	TTimeEvent* __fastcall EventByName(const System::UnicodeString name);
	__property TTimeEvent* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTimeEvents() { }
	
};


enum DECLSPEC_DENUM TTimeEventType : unsigned char { etOneShot, etContinuous, etPeriodic };

typedef void __fastcall (__closure *TTimeEventProc)(TTimeEvent* event);

class PASCALIMPLEMENTATION TTimeEvent : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	double FStartTime;
	double FEndTime;
	double FElapsedTime;
	double FPeriod;
	TTimeEventType FEventType;
	TTimeEventProc FOnEvent;
	bool FEnabled;
	unsigned FTickCount;
	void __fastcall SetEnabled(const bool Value);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetName(const System::UnicodeString val);
	void __fastcall DoEvent(const double CurTime);
	
public:
	__fastcall virtual TTimeEvent(System::Classes::TCollection* Collection);
	__fastcall virtual ~TTimeEvent();
	__property unsigned TickCount = {read=FTickCount, nodefault};
	__property double ElapsedTime = {read=FElapsedTime};
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property double StartTime = {read=FStartTime, write=FStartTime};
	__property double EndTime = {read=FEndTime, write=FEndTime};
	__property double Period = {read=FPeriod, write=FPeriod};
	__property TTimeEventType EventType = {read=FEventType, write=FEventType, default=0};
	__property TTimeEventProc OnEvent = {read=FOnEvent, write=FOnEvent};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Timeeventsmgr */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_TIMEEVENTSMGR)
using namespace Gls::Timeeventsmgr;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_TimeeventsmgrHPP
