// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.AsyncTimer.pas' rev: 35.00 (Windows)

#ifndef Gls_AsynctimerHPP
#define Gls_AsynctimerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <GLS.Utils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Asynctimer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLAsyncTimer;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLAsyncTimer : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FEnabled;
	System::Classes::TNotifyEvent FOnTimer;
	System::Classes::TThread* FTimerThread;
	System::Syncobjs::TCriticalSection* FMutex;
	
protected:
	void __fastcall SetEnabled(bool Value);
	System::Word __fastcall GetInterval();
	void __fastcall SetInterval(System::Word Value);
	System::Classes::TThreadPriority __fastcall GetThreadPriority();
	void __fastcall SetThreadPriority(System::Classes::TThreadPriority Value);
	void __fastcall DoTimer();
	
public:
	__fastcall virtual TGLAsyncTimer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLAsyncTimer();
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=0};
	__property System::Word Interval = {read=GetInterval, write=SetInterval, default=1000};
	__property System::Classes::TNotifyEvent OnTimer = {read=FOnTimer, write=FOnTimer};
	__property System::Classes::TThreadPriority ThreadPriority = {read=GetThreadPriority, write=SetThreadPriority, default=6};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word cDEFAULT_TIMER_INTERVAL = System::Word(0x3e8);
}	/* namespace Asynctimer */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_ASYNCTIMER)
using namespace Gls::Asynctimer;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_AsynctimerHPP
