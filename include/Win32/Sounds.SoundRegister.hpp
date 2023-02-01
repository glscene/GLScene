// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Sounds.SoundRegister.pas' rev: 35.00 (Windows)

#ifndef Sounds_SoundregisterHPP
#define Sounds_SoundregisterHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Sounds.BASS.hpp>
#include <Sounds.FMOD.hpp>
#include <Sounds.OpenAL.hpp>
#include <Sounds.WaveOut.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sounds
{
namespace Soundregister
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Soundregister */
}	/* namespace Sounds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SOUNDS_SOUNDREGISTER)
using namespace Sounds::Soundregister;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SOUNDS)
using namespace Sounds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sounds_SoundregisterHPP
