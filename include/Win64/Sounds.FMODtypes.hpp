// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Sounds.FMODtypes.pas' rev: 35.00 (Windows)

#ifndef Sounds_FmodtypesHPP
#define Sounds_FmodtypesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sounds
{
namespace Fmodtypes
{
//-- forward type declarations -----------------------------------------------
struct TFSoundVector;
struct TFSoundReverbProperties;
struct TFSoundReverbChannelProperties;
struct TFSoundTOCTag;
//-- type declarations -------------------------------------------------------
typedef void * PFSoundSample;

typedef void * PFSoundStream;

typedef void * PFSoundDSPUnit;

typedef void * PFMusicModule;

typedef void * PFSyncPoint;

typedef TFSoundVector *PFSoundVector;

struct DECLSPEC_DRECORD TFSoundVector
{
public:
	float x;
	float y;
	float z;
};


typedef System::ByteBool __stdcall (*TFSoundStreamCallback)(void * Stream, void * Buff, int Length, int Param);

typedef void * __stdcall (*TFSoundDSPCallback)(void * OriginalBuffer, void * NewBuffer, int Length, int Param);

typedef void __stdcall (*TFMusicCallback)(void * Module, System::Byte Param);

typedef unsigned __stdcall (*TFSoundOpenCallback)(System::WideChar * Name);

typedef void __stdcall (*TFSoundCloseCallback)(unsigned Handle);

typedef unsigned __stdcall (*TFSoundReadCallback)(void * Buffer, unsigned Size, unsigned Handle);

typedef void __stdcall (*TFSoundSeekCallback)(unsigned Handle, unsigned Pos, System::Byte Mode);

typedef unsigned __stdcall (*TFSoundTellCallback)(unsigned Handle);

typedef void * __stdcall (*TFSoundAllocCallback)(unsigned Size);

typedef void * __stdcall (*TFSoundReallocCallback)(void * Ptr, unsigned Size);

typedef void __stdcall (*TFSoundFreeCallback)(void * Ptr);

typedef System::ByteBool __stdcall (*TFMetaDataCallback)(System::WideChar * Name, System::WideChar * Value, int userdata);

enum DECLSPEC_DENUM TFModErrors : unsigned int { FMOD_ERR_NONE, FMOD_ERR_BUSY, FMOD_ERR_UNINITIALIZED, FMOD_ERR_INIT, FMOD_ERR_ALLOCATED, FMOD_ERR_PLAY, FMOD_ERR_OUTPUT_FORMAT, FMOD_ERR_COOPERATIVELEVEL, FMOD_ERR_CREATEBUFFER, FMOD_ERR_FILE_NOTFOUND, FMOD_ERR_FILE_FORMAT, FMOD_ERR_FILE_BAD, FMOD_ERR_MEMORY, FMOD_ERR_VERSION, FMOD_ERR_INVALID_PARAM, FMOD_ERR_NO_EAX, FMOD_ERR_CHANNEL_ALLOC, FMOD_ERR_RECORD, FMOD_ERR_MEDIAPLAYER, FMOD_ERR_CDDEVICE };

enum DECLSPEC_DENUM TFSoundOutputTypes : unsigned int { FSOUND_OUTPUT_NOSOUND, FSOUND_OUTPUT_WINMM, FSOUND_OUTPUT_DSOUND, FSOUND_OUTPUT_A3D, FSOUND_OUTPUT_OSS, FSOUND_OUTPUT_ESD, FSOUND_OUTPUT_ALSA, FSOUND_OUTPUT_ASIO, FSOUND_OUTPUT_XBOX, FSOUND_OUTPUT_PS2, FSOUND_OUTPUT_MAC, FSOUND_OUTPUT_GC, FSOUND_OUTPUT_PSP, FSOUND_OUTPUT_NOSOUND_NONREALTIME };

enum DECLSPEC_DENUM TFSoundMixerTypes : unsigned int { FSOUND_MIXER_AUTODETECT, FSOUND_MIXER_BLENDMODE, FSOUND_MIXER_MMXP5, FSOUND_MIXER_MMXP6, FSOUND_MIXER_QUALITY_AUTODETECT, FSOUND_MIXER_QUALITY_FPU, FSOUND_MIXER_QUALITY_MMXP5, FSOUND_MIXER_QUALITY_MMXP6, FSOUND_MIXER_MONO, FSOUND_MIXER_QUALITY_MONO, FSOUND_MIXER_MAX };

enum DECLSPEC_DENUM TFMusicTypes : unsigned int { FMUSIC_TYPE_NONE, FMUSIC_TYPE_MOD, FMUSIC_TYPE_S3M, FMUSIC_TYPE_XM, FMUSIC_TYPE_IT, FMUSIC_TYPE_MIDI, FMUSIC_TYPE_FSB };

struct DECLSPEC_DRECORD TFSoundReverbProperties
{
public:
	unsigned Environment;
	float EnvSize;
	float EnvDiffusion;
	int Room;
	int RoomHF;
	int RoomLF;
	float DecayTime;
	float DecayHFRatio;
	float DecayLFRatio;
	int Reflections;
	float ReflectionsDelay;
	System::StaticArray<float, 3> ReflectionsPan;
	int Reverb;
	float ReverbDelay;
	System::StaticArray<float, 3> ReverbPan;
	float EchoTime;
	float EchoDepth;
	float ModulationTime;
	float ModulationDepth;
	float AirAbsorptionHF;
	float HFReference;
	float LFReference;
	float RoomRolloffFactor;
	float Diffusion;
	float Density;
	unsigned Flags;
};


struct DECLSPEC_DRECORD TFSoundReverbChannelProperties
{
public:
	int Direct;
	int DirectHF;
	int Room;
	int RoomHF;
	int Obstruction;
	float ObstructionLFRatio;
	int Occlusion;
	float OcclusionLFRatio;
	float OcclusionRoomRatio;
	float OcclusionDirectRatio;
	int Exclusion;
	float ExclusionLFRatio;
	int OutsideVolumeHF;
	float DopplerFactor;
	float RolloffFactor;
	float RoomRolloffFactor;
	float AirAbsorptionFactor;
	int Flags;
};


enum DECLSPEC_DENUM TFSoundFXModes : unsigned int { FSOUND_FX_CHORUS, FSOUND_FX_COMPRESSOR, FSOUND_FX_DISTORTION, FSOUND_FX_ECHO, FSOUND_FX_FLANGER, FSOUND_FX_GARGLE, FSOUND_FX_I3DL2REVERB, FSOUND_FX_PARAMEQ, FSOUND_FX_WAVES_REVERB, FSOUND_FX_MAX };

enum DECLSPEC_DENUM TFSoundSpeakerModes : unsigned int { FSOUND_SPEAKERMODE_DOLBYDIGITAL, FSOUND_SPEAKERMODE_HEADPHONES, FSOUND_SPEAKERMODE_MONO, FSOUND_SPEAKERMODE_QUAD, FSOUND_SPEAKERMODE_STEREO, FSOUND_SPEAKERMODE_SURROUND, FSOUND_SPEAKERMODE_DTS };

typedef TFSoundSpeakerModes FSOUND_SPEAKERMODES;

enum DECLSPEC_DENUM TFSoundStreamNetStatus : unsigned int { FSOUND_STREAM_NET_NOTCONNECTED, FSOUND_STREAM_NET_CONNECTING, FSOUND_STREAM_NET_BUFFERING, FSOUND_STREAM_NET_READY, FSOUND_STREAM_NET_ERROR };

enum DECLSPEC_DENUM TFSoundTagFieldType : unsigned int { FSOUND_TAGFIELD_VORBISCOMMENT, FSOUND_TAGFIELD_ID3V1, FSOUND_TAGFIELD_ID3V2, FSOUND_TAGFIELD_SHOUTCAST, FSOUND_TAGFIELD_ICECAST, FSOUND_TAGFIELD_ASF };

struct DECLSPEC_DRECORD TFSoundTOCTag
{
public:
	System::StaticArray<char, 4> Name;
	int NumTracks;
	System::StaticArray<int, 100> Min;
	System::StaticArray<int, 100> Sec;
	System::StaticArray<int, 100> Frame;
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE float FMOD_VERSION;
static const System::Int8 FSOUND_DSP_DEFAULTPRIORITY_CLEARUNIT = System::Int8(0x0);
static const System::Int8 FSOUND_DSP_DEFAULTPRIORITY_SFXUNIT = System::Int8(0x64);
static const System::Byte FSOUND_DSP_DEFAULTPRIORITY_MUSICUNIT = System::Byte(0xc8);
static const System::Word FSOUND_DSP_DEFAULTPRIORITY_USER = System::Word(0x12c);
static const System::Word FSOUND_DSP_DEFAULTPRIORITY_FFTUNIT = System::Word(0x384);
static const System::Word FSOUND_DSP_DEFAULTPRIORITY_CLIPANDCOPYUNIT = System::Word(0x3e8);
static const System::Int8 FSOUND_CAPS_HARDWARE = System::Int8(0x1);
static const System::Int8 FSOUND_CAPS_EAX2 = System::Int8(0x2);
static const System::Int8 FSOUND_CAPS_EAX3 = System::Int8(0x10);
static const System::Int8 FSOUND_LOOP_OFF = System::Int8(0x1);
static const System::Int8 FSOUND_LOOP_NORMAL = System::Int8(0x2);
static const System::Int8 FSOUND_LOOP_BIDI = System::Int8(0x4);
static const System::Int8 FSOUND_8BITS = System::Int8(0x8);
static const System::Int8 FSOUND_16BITS = System::Int8(0x10);
static const System::Int8 FSOUND_MONO = System::Int8(0x20);
static const System::Int8 FSOUND_STEREO = System::Int8(0x40);
static const System::Byte FSOUND_UNSIGNED = System::Byte(0x80);
static const System::Word FSOUND_SIGNED = System::Word(0x100);
static const System::Word FSOUND_DELTA = System::Word(0x200);
static const System::Word FSOUND_IT214 = System::Word(0x400);
static const System::Word FSOUND_IT215 = System::Word(0x800);
static const System::Word FSOUND_HW3D = System::Word(0x1000);
static const System::Word FSOUND_2D = System::Word(0x2000);
static const System::Word FSOUND_STREAMABLE = System::Word(0x4000);
static const System::Word FSOUND_LOADMEMORY = System::Word(0x8000);
static const int FSOUND_LOADRAW = int(0x10000);
static const int FSOUND_MPEGACCURATE = int(0x20000);
static const int FSOUND_FORCEMONO = int(0x40000);
static const int FSOUND_HW2D = int(0x80000);
static const int FSOUND_ENABLEFX = int(0x100000);
static const int FSOUND_MPEGHALFRATE = int(0x200000);
static const int FSOUND_XADPCM = int(0x400000);
static const int FSOUND_VAG = int(0x800000);
static const int FSOUND_NONBLOCKING = int(0x1000000);
static const int FSOUND_GCADPCM = int(0x2000000);
static const int FSOUND_MULTICHANNEL = int(0x4000000);
static const int FSOUND_USECORE0 = int(0x8000000);
static const int FSOUND_USECORE1 = int(0x10000000);
static const int FSOUND_LOADMEMORYIOP = int(0x20000000);
static const System::Word FSOUND_NORMAL = System::Word(0x130);
static const System::Int8 FSOUND_CD_PLAYCONTINUOUS = System::Int8(0x0);
static const System::Int8 FSOUND_CD_PLAYONCE = System::Int8(0x1);
static const System::Int8 FSOUND_CD_PLAYLOOPED = System::Int8(0x2);
static const System::Int8 FSOUND_CD_PLAYRANDOM = System::Int8(0x3);
static const System::Int8 FSOUND_FREE = System::Int8(-1);
static const System::Int8 FSOUND_UNMANAGED = System::Int8(-2);
static const System::Int8 FSOUND_ALL = System::Int8(-3);
static const System::Int8 FSOUND_STEREOPAN = System::Int8(-1);
static const short FSOUND_SYSTEMCHANNEL = short(-1000);
static const short FSOUND_SYSTEMSAMPLE = short(-1000);
static const System::Int8 FSOUND_REVERBFLAGS_DECAYTIMESCALE = System::Int8(0x1);
static const System::Int8 FSOUND_REVERBFLAGS_REFLECTIONSSCALE = System::Int8(0x2);
static const System::Int8 FSOUND_REVERBFLAGS_REFLECTIONSDELAYSCALE = System::Int8(0x4);
static const System::Int8 FSOUND_REVERBFLAGS_REVERBSCALE = System::Int8(0x8);
static const System::Int8 FSOUND_REVERBFLAGS_REVERBDELAYSCALE = System::Int8(0x10);
static const System::Int8 FSOUND_REVERBFLAGS_DECAYHFLIMIT = System::Int8(0x20);
static const System::Int8 FSOUND_REVERBFLAGS_ECHOTIMESCALE = System::Int8(0x40);
static const System::Byte FSOUND_REVERBFLAGS_MODULATIONTIMESCALE = System::Byte(0x80);
static const System::Word FSOUND_REVERB_FLAGS_CORE0 = System::Word(0x100);
static const System::Word FSOUND_REVERB_FLAGS_CORE1 = System::Word(0x200);
static const System::Word FSOUND_REVERBFLAGS_DEFAULT = System::Word(0x33f);
static const System::Int8 FSOUND_REVERB_CHANNELFLAGS_DIRECTHFAUTO = System::Int8(0x1);
static const System::Int8 FSOUND_REVERB_CHANNELFLAGS_ROOMAUTO = System::Int8(0x2);
static const System::Int8 FSOUND_REVERB_CHANNELFLAGS_ROOMHFAUTO = System::Int8(0x4);
static const System::Int8 FSOUND_REVERB_CHANNELFLAGS_DEFAULT = System::Int8(0x7);
static const System::Int8 FSOUND_INIT_USEDEFAULTMIDISYNTH = System::Int8(0x1);
static const System::Int8 FSOUND_INIT_GLOBALFOCUS = System::Int8(0x2);
static const System::Int8 FSOUND_INIT_ENABLESYSTEMCHANNELFX = System::Int8(0x4);
static const System::Int8 FSOUND_INIT_ACCURATEVULEVELS = System::Int8(0x8);
static const System::Int8 FSOUND_INIT_PS2_DISABLECORE0REVERB = System::Int8(0x10);
static const System::Int8 FSOUND_INIT_PS2_DISABLECORE1REVERB = System::Int8(0x20);
static const System::Int8 FSOUND_INIT_PS2_SWAPDMACORES = System::Int8(0x40);
static const System::Byte FSOUND_INIT_DONTLATENCYADJUST = System::Byte(0x80);
static const System::Word FSOUND_INIT_GC_INITLIBS = System::Word(0x100);
static const System::Word FSOUND_INIT_STREAM_FROM_MAIN_THREAD = System::Word(0x200);
static const System::Word FSOUND_INIT_PS2_USEVOLUMERAMPING = System::Word(0x400);
static const System::Word FSOUND_INIT_DSOUND_DEFERRED = System::Word(0x800);
static const System::Word FSOUND_INIT_DSOUND_HRTF_LIGHT = System::Word(0x1000);
static const System::Word FSOUND_INIT_DSOUND_HRTF_FULL = System::Word(0x2000);
static const System::Word FSOUND_INIT_XBOX_REMOVEHEADROOM = System::Word(0x4000);
static const System::Word FSOUND_INIT_PSP_SILENCEONUNDERRUN = System::Word(0x8000);
static const System::Int8 FSOUND_PROTOCOL_SHOUTCAST = System::Int8(0x1);
static const System::Int8 FSOUND_PROTOCOL_ICECAST = System::Int8(0x2);
static const System::Int8 FSOUND_PROTOCOL_HTTP = System::Int8(0x4);
static const int FSOUND_FORMAT_MPEG = int(0x10000);
static const int FSOUND_FORMAT_OGGVORBIS = int(0x20000);
}	/* namespace Fmodtypes */
}	/* namespace Sounds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SOUNDS_FMODTYPES)
using namespace Sounds::Fmodtypes;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SOUNDS)
using namespace Sounds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sounds_FmodtypesHPP
