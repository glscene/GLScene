// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.AsyncHDS.pas' rev: 35.00 (Windows)

#ifndef Gls_AsynchdsHPP
#define Gls_AsynchdsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.HeightData.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Asynchds
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLAsyncHDS;
class DELPHICLASS TGLAsyncHDThread;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TIdleEvent)(TGLAsyncHDS* Sender, bool TilesUpdated);

typedef void __fastcall (__closure *TNewTilePreparedEvent)(TGLAsyncHDS* Sender, Gls::Heightdata::TGLHeightData* HeightData);

enum DECLSPEC_DENUM TUseDirtyTiles : unsigned char { dtNever, dtUntilReplaced, dtUntilAllReplaced };

class PASCALIMPLEMENTATION TGLAsyncHDS : public Gls::Heightdata::TGLHeightDataSourceFilter
{
	typedef Gls::Heightdata::TGLHeightDataSourceFilter inherited;
	
private:
	TIdleEvent FOnIdleEvent;
	TNewTilePreparedEvent FOnNewTilePrepared;
	TUseDirtyTiles FUseDirtyTiles;
	bool FTilesUpdated;
	
public:
	__fastcall virtual TGLAsyncHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLAsyncHDS();
	virtual void __fastcall BeforePreparingData(Gls::Heightdata::TGLHeightData* HeightData);
	virtual void __fastcall StartPreparingData(Gls::Heightdata::TGLHeightData* HeightData);
	virtual void __fastcall ThreadIsIdle();
	void __fastcall NewTilePrepared(Gls::Heightdata::TGLHeightData* HeightData);
	int __fastcall ThreadCount();
	void __fastcall WaitFor(int TimeOut = 0x7d0);
	bool __fastcall TilesUpdated();
	void __fastcall TilesUpdatedFlagReset();
	
__published:
	__property TIdleEvent OnIdle = {read=FOnIdleEvent, write=FOnIdleEvent};
	__property TNewTilePreparedEvent OnNewTilePrepared = {read=FOnNewTilePrepared, write=FOnNewTilePrepared};
	__property TUseDirtyTiles UseDirtyTiles = {read=FUseDirtyTiles, write=FUseDirtyTiles, nodefault};
	__property MaxThreads;
	__property Active;
};


class PASCALIMPLEMENTATION TGLAsyncHDThread : public Gls::Heightdata::TGLHeightDataThread
{
	typedef Gls::Heightdata::TGLHeightDataThread inherited;
	
public:
	TGLAsyncHDS* Owner;
	Gls::Heightdata::TGLHeightDataSource* HDS;
	virtual void __fastcall Execute();
	void __fastcall Sync();
public:
	/* TGLHeightDataThread.Destroy */ inline __fastcall virtual ~TGLAsyncHDThread() { }
	
public:
	/* TThread.Create */ inline __fastcall TGLAsyncHDThread()/* overload */ : Gls::Heightdata::TGLHeightDataThread() { }
	/* TThread.Create */ inline __fastcall TGLAsyncHDThread(bool CreateSuspended)/* overload */ : Gls::Heightdata::TGLHeightDataThread(CreateSuspended) { }
	/* TThread.Create */ inline __fastcall TGLAsyncHDThread(bool CreateSuspended, NativeUInt ReservedStackSize)/* overload */ : Gls::Heightdata::TGLHeightDataThread(CreateSuspended, ReservedStackSize) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Asynchds */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_ASYNCHDS)
using namespace Gls::Asynchds;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_AsynchdsHPP
