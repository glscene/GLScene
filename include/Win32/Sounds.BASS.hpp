// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Sounds.BASS.pas' rev: 35.00 (Windows)

#ifndef Sounds_BassHPP
#define Sounds_BassHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Forms.hpp>
#include <GLS.Scene.hpp>
#include <GLS.SoundManager.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <Sounds.BASSImport.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sounds
{
namespace Bass
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSMBASS;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TBASS3DAlgorithm : unsigned char { algDefault, algOff, algFull, algLight };

class PASCALIMPLEMENTATION TGLSMBASS : public Gls::Soundmanager::TGLSoundManager
{
	typedef Gls::Soundmanager::TGLSoundManager inherited;
	
private:
	bool FActivated;
	TBASS3DAlgorithm FAlgorithm3D;
	
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
	__fastcall virtual TGLSMBASS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSMBASS();
	virtual void __fastcall UpdateSources();
	virtual float __fastcall CPUUsagePercent();
	virtual bool __fastcall EAXSupported();
	
__published:
	__property TBASS3DAlgorithm Algorithm3D = {read=FAlgorithm3D, write=FAlgorithm3D, default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Bass */
}	/* namespace Sounds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SOUNDS_BASS)
using namespace Sounds::Bass;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SOUNDS)
using namespace Sounds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sounds_BassHPP
