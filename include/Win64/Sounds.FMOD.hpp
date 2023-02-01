// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Sounds.FMOD.pas' rev: 35.00 (Windows)

#ifndef Sounds_FmodHPP
#define Sounds_FmodHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.SoundManager.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <Sounds.FMODImport.hpp>
#include <Sounds.FMODtypes.hpp>
#include <Sounds.FMODpresets.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sounds
{
namespace Fmod
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSMFMOD;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLSMFMOD : public Gls::Soundmanager::TGLSoundManager
{
	typedef Gls::Soundmanager::TGLSoundManager inherited;
	
private:
	bool FActivated;
	bool FEAXCapable;
	
protected:
	virtual bool __fastcall DoActivate();
	virtual void __fastcall DoDeActivate();
	virtual void __fastcall NotifyMasterVolumeChange();
	virtual void __fastcall Notify3DFactorsChanged();
	virtual void __fastcall NotifyEnvironmentChanged();
	virtual void __fastcall KillSource(Gls::Soundmanager::TGLBaseSoundSource* aSource);
	virtual void __fastcall UpdateSource(Gls::Soundmanager::TGLBaseSoundSource* aSource);
	virtual void __fastcall MuteSource(Gls::Soundmanager::TGLBaseSoundSource* aSource, bool muted);
	virtual void __fastcall PauseSource(Gls::Soundmanager::TGLBaseSoundSource* aSource, bool paused);
	int __fastcall GetDefaultFrequency(Gls::Soundmanager::TGLBaseSoundSource* aSource);
	
public:
	__fastcall virtual TGLSMFMOD(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSMFMOD();
	virtual void __fastcall UpdateSources();
	virtual float __fastcall CPUUsagePercent();
	virtual bool __fastcall EAXSupported();
	
__published:
	__property MaxChannels = {default=32};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Fmod */
}	/* namespace Sounds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SOUNDS_FMOD)
using namespace Sounds::Fmod;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SOUNDS)
using namespace Sounds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sounds_FmodHPP
