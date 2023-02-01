// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Sounds.BASSImport.pas' rev: 35.00 (Windows)

#ifndef Sounds_BassimportHPP
#define Sounds_BassimportHPP

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
namespace Bassimport
{
//-- forward type declarations -----------------------------------------------
struct BASS_DEVICEINFO;
struct BASS_INFO;
struct BASS_RECORDINFO;
struct BASS_SAMPLE;
struct BASS_CHANNELINFO;
struct BASS_PLUGINFORM;
struct BASS_PLUGININFO;
struct BASS_3DVECTOR;
struct BASS_FILEPROCS;
struct TAG_ID3;
struct TAG_APE_BINARY;
struct TAG_BEXT;
struct BASS_DX8_CHORUS;
struct BASS_DX8_COMPRESSOR;
struct BASS_DX8_DISTORTION;
struct BASS_DX8_ECHO;
struct BASS_DX8_FLANGER;
struct BASS_DX8_GARGLE;
struct BASS_DX8_I3DL2REVERB;
struct BASS_DX8_PARAMEQ;
struct BASS_DX8_REVERB;
//-- type declarations -------------------------------------------------------
typedef unsigned DWORD;

typedef System::LongBool BOOL;

typedef __int64 QWORD;

typedef unsigned HMUSIC;

typedef unsigned HSAMPLE;

typedef unsigned HCHANNEL;

typedef unsigned HSTREAM;

typedef unsigned HRECORD;

typedef unsigned HSYNC;

typedef unsigned HDSP;

typedef unsigned HFX;

typedef unsigned HPLUGIN;

struct DECLSPEC_DRECORD BASS_DEVICEINFO
{
public:
	char *name;
	char *driver;
	unsigned flags;
};


struct DECLSPEC_DRECORD BASS_INFO
{
public:
	unsigned flags;
	unsigned hwsize;
	unsigned hwfree;
	unsigned freesam;
	unsigned free3d;
	unsigned minrate;
	unsigned maxrate;
	System::LongBool eax;
	unsigned minbuf;
	unsigned dsver;
	unsigned latency;
	unsigned initflags;
	unsigned speakers;
	unsigned freq;
};


struct DECLSPEC_DRECORD BASS_RECORDINFO
{
public:
	unsigned flags;
	unsigned formats;
	unsigned inputs;
	System::LongBool singlein;
	unsigned freq;
};


struct DECLSPEC_DRECORD BASS_SAMPLE
{
public:
	unsigned freq;
	float volume;
	float pan;
	unsigned flags;
	unsigned length;
	unsigned max;
	unsigned origres;
	unsigned chans;
	unsigned mingap;
	unsigned mode3d;
	float mindist;
	float maxdist;
	unsigned iangle;
	unsigned oangle;
	float outvol;
	unsigned vam;
	unsigned priority;
};


struct DECLSPEC_DRECORD BASS_CHANNELINFO
{
public:
	unsigned freq;
	unsigned chans;
	unsigned flags;
	unsigned ctype;
	unsigned origres;
	unsigned plugin;
	unsigned sample;
	System::WideChar *filename;
};


struct DECLSPEC_DRECORD BASS_PLUGINFORM
{
public:
	unsigned ctype;
	char *name;
	char *exts;
};


typedef System::StaticArray<BASS_PLUGINFORM, 178956970> TBASS_PLUGINFORMS;

typedef TBASS_PLUGINFORMS *PBASS_PLUGINFORMS;

typedef BASS_PLUGININFO *PBASS_PLUGININFO;

struct DECLSPEC_DRECORD BASS_PLUGININFO
{
public:
	unsigned version;
	unsigned formatc;
	TBASS_PLUGINFORMS *formats;
};


struct DECLSPEC_DRECORD BASS_3DVECTOR
{
public:
	float x;
	float y;
	float z;
};


typedef void __stdcall (*FILECLOSEPROC)(void * user);

typedef __int64 __stdcall (*FILELENPROC)(void * user);

typedef unsigned __stdcall (*FILEREADPROC)(void * buffer, unsigned length, void * user);

typedef System::LongBool __stdcall (*FILESEEKPROC)(__int64 offset, void * user);

struct DECLSPEC_DRECORD BASS_FILEPROCS
{
public:
	FILECLOSEPROC close;
	FILELENPROC length;
	FILEREADPROC read;
	FILESEEKPROC seek;
};


typedef TAG_ID3 *PTAG_ID3;

struct DECLSPEC_DRECORD TAG_ID3
{
public:
	System::StaticArray<char, 3> id;
	System::StaticArray<char, 30> title;
	System::StaticArray<char, 30> artist;
	System::StaticArray<char, 30> album;
	System::StaticArray<char, 4> year;
	System::StaticArray<char, 30> comment;
	System::Byte genre;
};


typedef TAG_APE_BINARY *PTAG_APE_BINARY;

struct DECLSPEC_DRECORD TAG_APE_BINARY
{
public:
	char *key;
	char *data;
	unsigned length;
};


typedef TAG_BEXT *PTAG_BEXT;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TAG_BEXT
{
public:
	System::StaticArray<char, 256> Description;
	System::StaticArray<char, 32> Originator;
	System::StaticArray<char, 32> OriginatorReference;
	System::StaticArray<char, 10> OriginationDate;
	System::StaticArray<char, 8> OriginationTime;
	__int64 TimeReference;
	System::Word Version;
	System::StaticArray<System::Byte, 64> UMID;
	System::StaticArray<System::Byte, 190> Reserved;
	System::StaticArray<char, 1073741823> CodingHistory;
};
#pragma pack(pop)


struct DECLSPEC_DRECORD BASS_DX8_CHORUS
{
public:
	float fWetDryMix;
	float fDepth;
	float fFeedback;
	float fFrequency;
	unsigned lWaveform;
	float fDelay;
	unsigned lPhase;
};


struct DECLSPEC_DRECORD BASS_DX8_COMPRESSOR
{
public:
	float fGain;
	float fAttack;
	float fRelease;
	float fThreshold;
	float fRatio;
	float fPredelay;
};


struct DECLSPEC_DRECORD BASS_DX8_DISTORTION
{
public:
	float fGain;
	float fEdge;
	float fPostEQCenterFrequency;
	float fPostEQBandwidth;
	float fPreLowpassCutoff;
};


struct DECLSPEC_DRECORD BASS_DX8_ECHO
{
public:
	float fWetDryMix;
	float fFeedback;
	float fLeftDelay;
	float fRightDelay;
	System::LongBool lPanDelay;
};


struct DECLSPEC_DRECORD BASS_DX8_FLANGER
{
public:
	float fWetDryMix;
	float fDepth;
	float fFeedback;
	float fFrequency;
	unsigned lWaveform;
	float fDelay;
	unsigned lPhase;
};


struct DECLSPEC_DRECORD BASS_DX8_GARGLE
{
public:
	unsigned dwRateHz;
	unsigned dwWaveShape;
};


struct DECLSPEC_DRECORD BASS_DX8_I3DL2REVERB
{
public:
	int lRoom;
	int lRoomHF;
	float flRoomRolloffFactor;
	float flDecayTime;
	float flDecayHFRatio;
	int lReflections;
	float flReflectionsDelay;
	int lReverb;
	float flReverbDelay;
	float flDiffusion;
	float flDensity;
	float flHFReference;
};


struct DECLSPEC_DRECORD BASS_DX8_PARAMEQ
{
public:
	float fCenter;
	float fBandwidth;
	float fGain;
};


struct DECLSPEC_DRECORD BASS_DX8_REVERB
{
public:
	float fInGain;
	float fReverbMix;
	float fReverbTime;
	float fHighFreqRTRatio;
};


typedef unsigned __stdcall (*STREAMPROC)(unsigned handle, void * buffer, unsigned length, void * user);

typedef void __stdcall (*DOWNLOADPROC)(void * buffer, unsigned length, void * user);

typedef void __stdcall (*SYNCPROC)(unsigned handle, unsigned channel, unsigned data, void * user);

typedef void __stdcall (*DSPPROC)(unsigned handle, unsigned channel, void * buffer, unsigned length, void * user);

typedef System::LongBool __stdcall (*RECORDPROC)(unsigned handle, void * buffer, unsigned length, void * user);

typedef NativeUInt TBASSModuleHandle;

//-- var, const, procedure ---------------------------------------------------
#define bassdll L"bass32.dll"
static const System::Word BASSVERSION = System::Word(0x204);
#define BASSVERSIONTEXT L"2.4"
static const unsigned DW_ERROR = unsigned(0xffffffff);
static const __int64 QW_ERROR = -1LL;
static const System::Int8 BASS_OK = System::Int8(0x0);
static const System::Int8 BASS_ERROR_MEM = System::Int8(0x1);
static const System::Int8 BASS_ERROR_FILEOPEN = System::Int8(0x2);
static const System::Int8 BASS_ERROR_DRIVER = System::Int8(0x3);
static const System::Int8 BASS_ERROR_BUFLOST = System::Int8(0x4);
static const System::Int8 BASS_ERROR_HANDLE = System::Int8(0x5);
static const System::Int8 BASS_ERROR_FORMAT = System::Int8(0x6);
static const System::Int8 BASS_ERROR_POSITION = System::Int8(0x7);
static const System::Int8 BASS_ERROR_INIT = System::Int8(0x8);
static const System::Int8 BASS_ERROR_START = System::Int8(0x9);
static const System::Int8 BASS_ERROR_ALREADY = System::Int8(0xe);
static const System::Int8 BASS_ERROR_NOCHAN = System::Int8(0x12);
static const System::Int8 BASS_ERROR_ILLTYPE = System::Int8(0x13);
static const System::Int8 BASS_ERROR_ILLPARAM = System::Int8(0x14);
static const System::Int8 BASS_ERROR_NO3D = System::Int8(0x15);
static const System::Int8 BASS_ERROR_NOEAX = System::Int8(0x16);
static const System::Int8 BASS_ERROR_DEVICE = System::Int8(0x17);
static const System::Int8 BASS_ERROR_NOPLAY = System::Int8(0x18);
static const System::Int8 BASS_ERROR_FREQ = System::Int8(0x19);
static const System::Int8 BASS_ERROR_NOTFILE = System::Int8(0x1b);
static const System::Int8 BASS_ERROR_NOHW = System::Int8(0x1d);
static const System::Int8 BASS_ERROR_EMPTY = System::Int8(0x1f);
static const System::Int8 BASS_ERROR_NONET = System::Int8(0x20);
static const System::Int8 BASS_ERROR_CREATE = System::Int8(0x21);
static const System::Int8 BASS_ERROR_NOFX = System::Int8(0x22);
static const System::Int8 BASS_ERROR_NOTAVAIL = System::Int8(0x25);
static const System::Int8 BASS_ERROR_DECODE = System::Int8(0x26);
static const System::Int8 BASS_ERROR_DX = System::Int8(0x27);
static const System::Int8 BASS_ERROR_TIMEOUT = System::Int8(0x28);
static const System::Int8 BASS_ERROR_FILEFORM = System::Int8(0x29);
static const System::Int8 BASS_ERROR_SPEAKER = System::Int8(0x2a);
static const System::Int8 BASS_ERROR_VERSION = System::Int8(0x2b);
static const System::Int8 BASS_ERROR_CODEC = System::Int8(0x2c);
static const System::Int8 BASS_ERROR_ENDED = System::Int8(0x2d);
static const System::Int8 BASS_ERROR_BUSY = System::Int8(0x2e);
static const System::Int8 BASS_ERROR_UNKNOWN = System::Int8(-1);
static const System::Int8 BASS_CONFIG_BUFFER = System::Int8(0x0);
static const System::Int8 BASS_CONFIG_UPDATEPERIOD = System::Int8(0x1);
static const System::Int8 BASS_CONFIG_GVOL_SAMPLE = System::Int8(0x4);
static const System::Int8 BASS_CONFIG_GVOL_STREAM = System::Int8(0x5);
static const System::Int8 BASS_CONFIG_GVOL_MUSIC = System::Int8(0x6);
static const System::Int8 BASS_CONFIG_CURVE_VOL = System::Int8(0x7);
static const System::Int8 BASS_CONFIG_CURVE_PAN = System::Int8(0x8);
static const System::Int8 BASS_CONFIG_FLOATDSP = System::Int8(0x9);
static const System::Int8 BASS_CONFIG_3DALGORITHM = System::Int8(0xa);
static const System::Int8 BASS_CONFIG_NET_TIMEOUT = System::Int8(0xb);
static const System::Int8 BASS_CONFIG_NET_BUFFER = System::Int8(0xc);
static const System::Int8 BASS_CONFIG_PAUSE_NOPLAY = System::Int8(0xd);
static const System::Int8 BASS_CONFIG_NET_PREBUF = System::Int8(0xf);
static const System::Int8 BASS_CONFIG_NET_PASSIVE = System::Int8(0x12);
static const System::Int8 BASS_CONFIG_REC_BUFFER = System::Int8(0x13);
static const System::Int8 BASS_CONFIG_NET_PLAYLIST = System::Int8(0x15);
static const System::Int8 BASS_CONFIG_MUSIC_VIRTUAL = System::Int8(0x16);
static const System::Int8 BASS_CONFIG_VERIFY = System::Int8(0x17);
static const System::Int8 BASS_CONFIG_UPDATETHREADS = System::Int8(0x18);
static const System::Int8 BASS_CONFIG_DEV_BUFFER = System::Int8(0x1b);
static const System::Int8 BASS_CONFIG_VISTA_TRUEPOS = System::Int8(0x1e);
static const System::Int8 BASS_CONFIG_IOS_MIXAUDIO = System::Int8(0x22);
static const System::Int8 BASS_CONFIG_DEV_DEFAULT = System::Int8(0x24);
static const System::Int8 BASS_CONFIG_NET_READTIMEOUT = System::Int8(0x25);
static const System::Int8 BASS_CONFIG_VISTA_SPEAKERS = System::Int8(0x26);
static const System::Int8 BASS_CONFIG_IOS_SPEAKER = System::Int8(0x27);
static const System::Int8 BASS_CONFIG_MF_DISABLE = System::Int8(0x28);
static const System::Int8 BASS_CONFIG_HANDLES = System::Int8(0x29);
static const System::Int8 BASS_CONFIG_UNICODE = System::Int8(0x2a);
static const System::Int8 BASS_CONFIG_SRC = System::Int8(0x2b);
static const System::Int8 BASS_CONFIG_SRC_SAMPLE = System::Int8(0x2c);
static const System::Int8 BASS_CONFIG_ASYNCFILE_BUFFER = System::Int8(0x2d);
static const System::Int8 BASS_CONFIG_OGG_PRESCAN = System::Int8(0x2f);
static const System::Int8 BASS_CONFIG_MF_VIDEO = System::Int8(0x30);
static const System::Int8 BASS_CONFIG_AIRPLAY = System::Int8(0x31);
static const System::Int8 BASS_CONFIG_DEV_NONSTOP = System::Int8(0x32);
static const System::Int8 BASS_CONFIG_IOS_NOCATEGORY = System::Int8(0x33);
static const System::Int8 BASS_CONFIG_VERIFY_NET = System::Int8(0x34);
static const System::Int8 BASS_CONFIG_DEV_PERIOD = System::Int8(0x35);
static const System::Int8 BASS_CONFIG_FLOAT = System::Int8(0x36);
static const System::Int8 BASS_CONFIG_NET_AGENT = System::Int8(0x10);
static const System::Int8 BASS_CONFIG_NET_PROXY = System::Int8(0x11);
static const System::Int8 BASS_DEVICE_8BITS = System::Int8(0x1);
static const System::Int8 BASS_DEVICE_MONO = System::Int8(0x2);
static const System::Int8 BASS_DEVICE_3D = System::Int8(0x4);
static const System::Int8 BASS_DEVICE_16BITS = System::Int8(0x8);
static const System::Word BASS_DEVICE_LATENCY = System::Word(0x100);
static const System::Word BASS_DEVICE_CPSPEAKERS = System::Word(0x400);
static const System::Word BASS_DEVICE_SPEAKERS = System::Word(0x800);
static const System::Word BASS_DEVICE_NOSPEAKER = System::Word(0x1000);
static const System::Word BASS_DEVICE_DMIX = System::Word(0x2000);
static const System::Word BASS_DEVICE_FREQ = System::Word(0x4000);
static const System::Word BASS_DEVICE_STEREO = System::Word(0x8000);
static const System::Int8 BASS_OBJECT_DS = System::Int8(0x1);
static const System::Int8 BASS_OBJECT_DS3DL = System::Int8(0x2);
static const System::Int8 BASS_DEVICE_ENABLED = System::Int8(0x1);
static const System::Int8 BASS_DEVICE_DEFAULT = System::Int8(0x2);
static const System::Int8 BASS_DEVICE_INIT = System::Int8(0x4);
static const unsigned BASS_DEVICE_TYPE_MASK = unsigned(0xff000000);
static const int BASS_DEVICE_TYPE_NETWORK = int(0x1000000);
static const int BASS_DEVICE_TYPE_SPEAKERS = int(0x2000000);
static const int BASS_DEVICE_TYPE_LINE = int(0x3000000);
static const int BASS_DEVICE_TYPE_HEADPHONES = int(0x4000000);
static const int BASS_DEVICE_TYPE_MICROPHONE = int(0x5000000);
static const int BASS_DEVICE_TYPE_HEADSET = int(0x6000000);
static const int BASS_DEVICE_TYPE_HANDSET = int(0x7000000);
static const int BASS_DEVICE_TYPE_DIGITAL = int(0x8000000);
static const int BASS_DEVICE_TYPE_SPDIF = int(0x9000000);
static const int BASS_DEVICE_TYPE_HDMI = int(0xa000000);
static const int BASS_DEVICE_TYPE_DISPLAYPORT = int(0x40000000);
static const int BASS_DEVICES_AIRPLAY = int(0x1000000);
static const System::Int8 DSCAPS_CONTINUOUSRATE = System::Int8(0x10);
static const System::Int8 DSCAPS_EMULDRIVER = System::Int8(0x20);
static const System::Int8 DSCAPS_CERTIFIED = System::Int8(0x40);
static const System::Word DSCAPS_SECONDARYMONO = System::Word(0x100);
static const System::Word DSCAPS_SECONDARYSTEREO = System::Word(0x200);
static const System::Word DSCAPS_SECONDARY8BIT = System::Word(0x400);
static const System::Word DSCAPS_SECONDARY16BIT = System::Word(0x800);
static const System::Int8 DSCCAPS_EMULDRIVER = System::Int8(0x20);
static const System::Int8 DSCCAPS_CERTIFIED = System::Int8(0x40);
static const System::Int8 BASS_SAMPLE_8BITS = System::Int8(0x1);
static const System::Word BASS_SAMPLE_FLOAT = System::Word(0x100);
static const System::Int8 BASS_SAMPLE_MONO = System::Int8(0x2);
static const System::Int8 BASS_SAMPLE_LOOP = System::Int8(0x4);
static const System::Int8 BASS_SAMPLE_3D = System::Int8(0x8);
static const System::Int8 BASS_SAMPLE_SOFTWARE = System::Int8(0x10);
static const System::Int8 BASS_SAMPLE_MUTEMAX = System::Int8(0x20);
static const System::Int8 BASS_SAMPLE_VAM = System::Int8(0x40);
static const System::Byte BASS_SAMPLE_FX = System::Byte(0x80);
static const int BASS_SAMPLE_OVER_VOL = int(0x10000);
static const int BASS_SAMPLE_OVER_POS = int(0x20000);
static const int BASS_SAMPLE_OVER_DIST = int(0x30000);
static const int BASS_STREAM_PRESCAN = int(0x20000);
static const int BASS_MP3_SETPOS = int(0x20000);
static const int BASS_STREAM_AUTOFREE = int(0x40000);
static const int BASS_STREAM_RESTRATE = int(0x80000);
static const int BASS_STREAM_BLOCK = int(0x100000);
static const int BASS_STREAM_DECODE = int(0x200000);
static const int BASS_STREAM_STATUS = int(0x800000);
static const System::Word BASS_MUSIC_FLOAT = System::Word(0x100);
static const System::Int8 BASS_MUSIC_MONO = System::Int8(0x2);
static const System::Int8 BASS_MUSIC_LOOP = System::Int8(0x4);
static const System::Int8 BASS_MUSIC_3D = System::Int8(0x8);
static const System::Byte BASS_MUSIC_FX = System::Byte(0x80);
static const int BASS_MUSIC_AUTOFREE = int(0x40000);
static const int BASS_MUSIC_DECODE = int(0x200000);
static const int BASS_MUSIC_PRESCAN = int(0x20000);
static const int BASS_MUSIC_CALCLEN = int(0x20000);
static const System::Word BASS_MUSIC_RAMP = System::Word(0x200);
static const System::Word BASS_MUSIC_RAMPS = System::Word(0x400);
static const System::Word BASS_MUSIC_SURROUND = System::Word(0x800);
static const System::Word BASS_MUSIC_SURROUND2 = System::Word(0x1000);
static const System::Word BASS_MUSIC_FT2PAN = System::Word(0x2000);
static const System::Word BASS_MUSIC_FT2MOD = System::Word(0x2000);
static const System::Word BASS_MUSIC_PT1MOD = System::Word(0x4000);
static const int BASS_MUSIC_NONINTER = int(0x10000);
static const int BASS_MUSIC_SINCINTER = int(0x800000);
static const System::Word BASS_MUSIC_POSRESET = System::Word(0x8000);
static const int BASS_MUSIC_POSRESETEX = int(0x400000);
static const int BASS_MUSIC_STOPBACK = int(0x80000);
static const int BASS_MUSIC_NOSAMPLE = int(0x100000);
static const int BASS_SPEAKER_FRONT = int(0x1000000);
static const int BASS_SPEAKER_REAR = int(0x2000000);
static const int BASS_SPEAKER_CENLFE = int(0x3000000);
static const int BASS_SPEAKER_REAR2 = int(0x4000000);
static const int BASS_SPEAKER_LEFT = int(0x10000000);
static const int BASS_SPEAKER_RIGHT = int(0x20000000);
static const int BASS_SPEAKER_FRONTLEFT = int(0x11000000);
static const int BASS_SPEAKER_FRONTRIGHT = int(0x21000000);
static const int BASS_SPEAKER_REARLEFT = int(0x12000000);
static const int BASS_SPEAKER_REARRIGHT = int(0x22000000);
static const int BASS_SPEAKER_CENTER = int(0x13000000);
static const int BASS_SPEAKER_LFE = int(0x23000000);
static const int BASS_SPEAKER_REAR2LEFT = int(0x14000000);
static const int BASS_SPEAKER_REAR2RIGHT = int(0x24000000);
static const int BASS_ASYNCFILE = int(0x40000000);
static const unsigned BASS_UNICODE = unsigned(0x80000000);
static const System::Word BASS_RECORD_PAUSE = System::Word(0x8000);
static const System::Int8 BASS_VAM_HARDWARE = System::Int8(0x1);
static const System::Int8 BASS_VAM_SOFTWARE = System::Int8(0x2);
static const System::Int8 BASS_VAM_TERM_TIME = System::Int8(0x4);
static const System::Int8 BASS_VAM_TERM_DIST = System::Int8(0x8);
static const System::Int8 BASS_VAM_TERM_PRIO = System::Int8(0x10);
static const System::Int8 BASS_CTYPE_SAMPLE = System::Int8(0x1);
static const System::Int8 BASS_CTYPE_RECORD = System::Int8(0x2);
static const int BASS_CTYPE_STREAM = int(0x10000);
static const int BASS_CTYPE_STREAM_OGG = int(0x10002);
static const int BASS_CTYPE_STREAM_MP1 = int(0x10003);
static const int BASS_CTYPE_STREAM_MP2 = int(0x10004);
static const int BASS_CTYPE_STREAM_MP3 = int(0x10005);
static const int BASS_CTYPE_STREAM_AIFF = int(0x10006);
static const int BASS_CTYPE_STREAM_WAV = int(0x40000);
static const int BASS_CTYPE_STREAM_WAV_PCM = int(0x50001);
static const int BASS_CTYPE_STREAM_WAV_FLOAT = int(0x50003);
static const int BASS_CTYPE_MUSIC_MOD = int(0x20000);
static const int BASS_CTYPE_MUSIC_MTM = int(0x20001);
static const int BASS_CTYPE_MUSIC_S3M = int(0x20002);
static const int BASS_CTYPE_MUSIC_XM = int(0x20003);
static const int BASS_CTYPE_MUSIC_IT = int(0x20004);
static const System::Word BASS_CTYPE_MUSIC_MO3 = System::Word(0x100);
static const System::Int8 BASS_3DMODE_NORMAL = System::Int8(0x0);
static const System::Int8 BASS_3DMODE_RELATIVE = System::Int8(0x1);
static const System::Int8 BASS_3DMODE_OFF = System::Int8(0x2);
static const System::Int8 BASS_3DALG_DEFAULT = System::Int8(0x0);
static const System::Int8 BASS_3DALG_OFF = System::Int8(0x1);
static const System::Int8 BASS_3DALG_FULL = System::Int8(0x2);
static const System::Int8 BASS_3DALG_LIGHT = System::Int8(0x3);
static const System::Int8 EAX_ENVIRONMENT_GENERIC = System::Int8(0x0);
static const System::Int8 EAX_ENVIRONMENT_PADDEDCELL = System::Int8(0x1);
static const System::Int8 EAX_ENVIRONMENT_ROOM = System::Int8(0x2);
static const System::Int8 EAX_ENVIRONMENT_BATHROOM = System::Int8(0x3);
static const System::Int8 EAX_ENVIRONMENT_LIVINGROOM = System::Int8(0x4);
static const System::Int8 EAX_ENVIRONMENT_STONEROOM = System::Int8(0x5);
static const System::Int8 EAX_ENVIRONMENT_AUDITORIUM = System::Int8(0x6);
static const System::Int8 EAX_ENVIRONMENT_CONCERTHALL = System::Int8(0x7);
static const System::Int8 EAX_ENVIRONMENT_CAVE = System::Int8(0x8);
static const System::Int8 EAX_ENVIRONMENT_ARENA = System::Int8(0x9);
static const System::Int8 EAX_ENVIRONMENT_HANGAR = System::Int8(0xa);
static const System::Int8 EAX_ENVIRONMENT_CARPETEDHALLWAY = System::Int8(0xb);
static const System::Int8 EAX_ENVIRONMENT_HALLWAY = System::Int8(0xc);
static const System::Int8 EAX_ENVIRONMENT_STONECORRIDOR = System::Int8(0xd);
static const System::Int8 EAX_ENVIRONMENT_ALLEY = System::Int8(0xe);
static const System::Int8 EAX_ENVIRONMENT_FOREST = System::Int8(0xf);
static const System::Int8 EAX_ENVIRONMENT_CITY = System::Int8(0x10);
static const System::Int8 EAX_ENVIRONMENT_MOUNTAINS = System::Int8(0x11);
static const System::Int8 EAX_ENVIRONMENT_QUARRY = System::Int8(0x12);
static const System::Int8 EAX_ENVIRONMENT_PLAIN = System::Int8(0x13);
static const System::Int8 EAX_ENVIRONMENT_PARKINGLOT = System::Int8(0x14);
static const System::Int8 EAX_ENVIRONMENT_SEWERPIPE = System::Int8(0x15);
static const System::Int8 EAX_ENVIRONMENT_UNDERWATER = System::Int8(0x16);
static const System::Int8 EAX_ENVIRONMENT_DRUGGED = System::Int8(0x17);
static const System::Int8 EAX_ENVIRONMENT_DIZZY = System::Int8(0x18);
static const System::Int8 EAX_ENVIRONMENT_PSYCHOTIC = System::Int8(0x19);
static const System::Int8 EAX_ENVIRONMENT_COUNT = System::Int8(0x1a);
static const unsigned BASS_STREAMPROC_END = unsigned(0x80000000);
static const System::Int8 STREAMFILE_NOBUFFER = System::Int8(0x0);
static const System::Int8 STREAMFILE_BUFFER = System::Int8(0x1);
static const System::Int8 STREAMFILE_BUFFERPUSH = System::Int8(0x2);
static const System::Int8 BASS_FILEDATA_END = System::Int8(0x0);
static const System::Int8 BASS_FILEPOS_CURRENT = System::Int8(0x0);
static const System::Int8 BASS_FILEPOS_DECODE = System::Int8(0x0);
static const System::Int8 BASS_FILEPOS_DOWNLOAD = System::Int8(0x1);
static const System::Int8 BASS_FILEPOS_END = System::Int8(0x2);
static const System::Int8 BASS_FILEPOS_START = System::Int8(0x3);
static const System::Int8 BASS_FILEPOS_CONNECTED = System::Int8(0x4);
static const System::Int8 BASS_FILEPOS_BUFFER = System::Int8(0x5);
static const System::Int8 BASS_FILEPOS_SOCKET = System::Int8(0x6);
static const System::Int8 BASS_FILEPOS_ASYNCBUF = System::Int8(0x7);
static const System::Int8 BASS_FILEPOS_SIZE = System::Int8(0x8);
static const System::Int8 BASS_SYNC_POS = System::Int8(0x0);
static const System::Int8 BASS_SYNC_END = System::Int8(0x2);
static const System::Int8 BASS_SYNC_META = System::Int8(0x4);
static const System::Int8 BASS_SYNC_SLIDE = System::Int8(0x5);
static const System::Int8 BASS_SYNC_STALL = System::Int8(0x6);
static const System::Int8 BASS_SYNC_DOWNLOAD = System::Int8(0x7);
static const System::Int8 BASS_SYNC_FREE = System::Int8(0x8);
static const System::Int8 BASS_SYNC_SETPOS = System::Int8(0xb);
static const System::Int8 BASS_SYNC_MUSICPOS = System::Int8(0xa);
static const System::Int8 BASS_SYNC_MUSICINST = System::Int8(0x1);
static const System::Int8 BASS_SYNC_MUSICFX = System::Int8(0x3);
static const System::Int8 BASS_SYNC_OGG_CHANGE = System::Int8(0xc);
static const int BASS_SYNC_MIXTIME = int(0x40000000);
static const unsigned BASS_SYNC_ONETIME = unsigned(0x80000000);
static const System::Int8 BASS_ACTIVE_STOPPED = System::Int8(0x0);
static const System::Int8 BASS_ACTIVE_PLAYING = System::Int8(0x1);
static const System::Int8 BASS_ACTIVE_STALLED = System::Int8(0x2);
static const System::Int8 BASS_ACTIVE_PAUSED = System::Int8(0x3);
static const System::Int8 BASS_ATTRIB_FREQ = System::Int8(0x1);
static const System::Int8 BASS_ATTRIB_VOL = System::Int8(0x2);
static const System::Int8 BASS_ATTRIB_PAN = System::Int8(0x3);
static const System::Int8 BASS_ATTRIB_EAXMIX = System::Int8(0x4);
static const System::Int8 BASS_ATTRIB_NOBUFFER = System::Int8(0x5);
static const System::Int8 BASS_ATTRIB_VBR = System::Int8(0x6);
static const System::Int8 BASS_ATTRIB_CPU = System::Int8(0x7);
static const System::Int8 BASS_ATTRIB_SRC = System::Int8(0x8);
static const System::Int8 BASS_ATTRIB_NET_RESUME = System::Int8(0x9);
static const System::Int8 BASS_ATTRIB_SCANINFO = System::Int8(0xa);
static const System::Int8 BASS_ATTRIB_NORAMP = System::Int8(0xb);
static const System::Int8 BASS_ATTRIB_BITRATE = System::Int8(0xc);
static const System::Word BASS_ATTRIB_MUSIC_AMPLIFY = System::Word(0x100);
static const System::Word BASS_ATTRIB_MUSIC_PANSEP = System::Word(0x101);
static const System::Word BASS_ATTRIB_MUSIC_PSCALER = System::Word(0x102);
static const System::Word BASS_ATTRIB_MUSIC_BPM = System::Word(0x103);
static const System::Word BASS_ATTRIB_MUSIC_SPEED = System::Word(0x104);
static const System::Word BASS_ATTRIB_MUSIC_VOL_GLOBAL = System::Word(0x105);
static const System::Word BASS_ATTRIB_MUSIC_ACTIVE = System::Word(0x106);
static const System::Word BASS_ATTRIB_MUSIC_VOL_CHAN = System::Word(0x200);
static const System::Word BASS_ATTRIB_MUSIC_VOL_INST = System::Word(0x300);
static const System::Int8 BASS_DATA_AVAILABLE = System::Int8(0x0);
static const int BASS_DATA_FIXED = int(0x20000000);
static const int BASS_DATA_FLOAT = int(0x40000000);
static const unsigned BASS_DATA_FFT256 = unsigned(0x80000000);
static const unsigned BASS_DATA_FFT512 = unsigned(0x80000001);
static const unsigned BASS_DATA_FFT1024 = unsigned(0x80000002);
static const unsigned BASS_DATA_FFT2048 = unsigned(0x80000003);
static const unsigned BASS_DATA_FFT4096 = unsigned(0x80000004);
static const unsigned BASS_DATA_FFT8192 = unsigned(0x80000005);
static const unsigned BASS_DATA_FFT16384 = unsigned(0x80000006);
static const unsigned BASS_DATA_FFT32768 = unsigned(0x80000007);
static const System::Int8 BASS_DATA_FFT_INDIVIDUAL = System::Int8(0x10);
static const System::Int8 BASS_DATA_FFT_NOWINDOW = System::Int8(0x20);
static const System::Int8 BASS_DATA_FFT_REMOVEDC = System::Int8(0x40);
static const System::Byte BASS_DATA_FFT_COMPLEX = System::Byte(0x80);
static const System::Int8 BASS_LEVEL_MONO = System::Int8(0x1);
static const System::Int8 BASS_LEVEL_STEREO = System::Int8(0x2);
static const System::Int8 BASS_LEVEL_RMS = System::Int8(0x4);
static const System::Int8 BASS_TAG_ID3 = System::Int8(0x0);
static const System::Int8 BASS_TAG_ID3V2 = System::Int8(0x1);
static const System::Int8 BASS_TAG_OGG = System::Int8(0x2);
static const System::Int8 BASS_TAG_HTTP = System::Int8(0x3);
static const System::Int8 BASS_TAG_ICY = System::Int8(0x4);
static const System::Int8 BASS_TAG_META = System::Int8(0x5);
static const System::Int8 BASS_TAG_APE = System::Int8(0x6);
static const System::Int8 BASS_TAG_MP4 = System::Int8(0x7);
static const System::Int8 BASS_TAG_WMA = System::Int8(0x8);
static const System::Int8 BASS_TAG_VENDOR = System::Int8(0x9);
static const System::Int8 BASS_TAG_LYRICS3 = System::Int8(0xa);
static const System::Int8 BASS_TAG_CA_CODEC = System::Int8(0xb);
static const System::Int8 BASS_TAG_MF = System::Int8(0xd);
static const System::Int8 BASS_TAG_WAVEFORMAT = System::Int8(0xe);
static const System::Word BASS_TAG_RIFF_INFO = System::Word(0x100);
static const System::Word BASS_TAG_RIFF_BEXT = System::Word(0x101);
static const System::Word BASS_TAG_RIFF_CART = System::Word(0x102);
static const System::Word BASS_TAG_RIFF_DISP = System::Word(0x103);
static const System::Word BASS_TAG_APE_BINARY = System::Word(0x1000);
static const int BASS_TAG_MUSIC_NAME = int(0x10000);
static const int BASS_TAG_MUSIC_MESSAGE = int(0x10001);
static const int BASS_TAG_MUSIC_ORDERS = int(0x10002);
static const int BASS_TAG_MUSIC_AUTH = int(0x10003);
static const int BASS_TAG_MUSIC_INST = int(0x10100);
static const int BASS_TAG_MUSIC_SAMPLE = int(0x10300);
static const System::Int8 BASS_POS_BYTE = System::Int8(0x0);
static const System::Int8 BASS_POS_MUSIC_ORDER = System::Int8(0x1);
static const System::Int8 BASS_POS_OGG = System::Int8(0x3);
static const int BASS_POS_INEXACT = int(0x8000000);
static const int BASS_POS_DECODE = int(0x10000000);
static const int BASS_POS_DECODETO = int(0x20000000);
static const int BASS_POS_SCAN = int(0x40000000);
static const int BASS_INPUT_OFF = int(0x10000);
static const int BASS_INPUT_ON = int(0x20000);
static const unsigned BASS_INPUT_TYPE_MASK = unsigned(0xff000000);
static const System::Int8 BASS_INPUT_TYPE_UNDEF = System::Int8(0x0);
static const int BASS_INPUT_TYPE_DIGITAL = int(0x1000000);
static const int BASS_INPUT_TYPE_LINE = int(0x2000000);
static const int BASS_INPUT_TYPE_MIC = int(0x3000000);
static const int BASS_INPUT_TYPE_SYNTH = int(0x4000000);
static const int BASS_INPUT_TYPE_CD = int(0x5000000);
static const int BASS_INPUT_TYPE_PHONE = int(0x6000000);
static const int BASS_INPUT_TYPE_SPEAKER = int(0x7000000);
static const int BASS_INPUT_TYPE_WAVE = int(0x8000000);
static const int BASS_INPUT_TYPE_AUX = int(0x9000000);
static const int BASS_INPUT_TYPE_ANALOG = int(0xa000000);
static const System::Int8 BASS_FX_DX8_CHORUS = System::Int8(0x0);
static const System::Int8 BASS_FX_DX8_COMPRESSOR = System::Int8(0x1);
static const System::Int8 BASS_FX_DX8_DISTORTION = System::Int8(0x2);
static const System::Int8 BASS_FX_DX8_ECHO = System::Int8(0x3);
static const System::Int8 BASS_FX_DX8_FLANGER = System::Int8(0x4);
static const System::Int8 BASS_FX_DX8_GARGLE = System::Int8(0x5);
static const System::Int8 BASS_FX_DX8_I3DL2REVERB = System::Int8(0x6);
static const System::Int8 BASS_FX_DX8_PARAMEQ = System::Int8(0x7);
static const System::Int8 BASS_FX_DX8_REVERB = System::Int8(0x8);
static const System::Int8 BASS_DX8_PHASE_NEG_180 = System::Int8(0x0);
static const System::Int8 BASS_DX8_PHASE_NEG_90 = System::Int8(0x1);
static const System::Int8 BASS_DX8_PHASE_ZERO = System::Int8(0x2);
static const System::Int8 BASS_DX8_PHASE_90 = System::Int8(0x3);
static const System::Int8 BASS_DX8_PHASE_180 = System::Int8(0x4);
#define STREAMPROC_DUMMY (void *)(0)
#define STREAMPROC_PUSH (void *)(0xffffffff)
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_SetConfig)(unsigned option, unsigned value);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_GetConfig)(unsigned option);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_SetConfigPtr)(unsigned option, void * value);
extern DELPHI_PACKAGE void * __stdcall (*BASS_GetConfigPtr)(unsigned option);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_GetVersion)(void);
extern DELPHI_PACKAGE int __stdcall (*BASS_ErrorGetCode)(void);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_GetDeviceInfo)(unsigned device, BASS_DEVICEINFO &info);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_Init)(int device, unsigned freq, unsigned flags, HWND win, System::PGUID clsid);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_SetDevice)(unsigned device);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_GetDevice)(void);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_Free)(void);
extern DELPHI_PACKAGE void * __stdcall (*BASS_GetDSoundObject)(unsigned obj);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_GetInfo)(BASS_INFO &info);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_Update)(unsigned length);
extern DELPHI_PACKAGE float __stdcall (*BASS_GetCPU)(void);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_Start)(void);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_Stop)(void);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_Pause)(void);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_SetVolume)(float volume);
extern DELPHI_PACKAGE float __stdcall (*BASS_GetVolume)(void);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_PluginLoad)(System::WideChar * filename, unsigned flags);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_PluginFree)(unsigned handle);
extern DELPHI_PACKAGE PBASS_PLUGININFO __stdcall (*BASS_PluginGetInfo)(unsigned handle);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_Set3DFactors)(float distf, float rollf, float doppf);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_Get3DFactors)(float &distf, float &rollf, float &doppf);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_Set3DPosition)(BASS_3DVECTOR &pos, BASS_3DVECTOR &vel, BASS_3DVECTOR &front, BASS_3DVECTOR &top);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_Get3DPosition)(BASS_3DVECTOR &pos, BASS_3DVECTOR &vel, BASS_3DVECTOR &front, BASS_3DVECTOR &top);
extern DELPHI_PACKAGE void __stdcall (*BASS_Apply3D)(void);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_SetEAXParameters)(int env, float vol, float decay, float damp);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_GetEAXParameters)(unsigned &env, float &vol, float &decay, float &damp);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_MusicLoad)(System::LongBool mem, void * f, __int64 offset, unsigned length, unsigned flags, unsigned freq);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_MusicFree)(unsigned handle);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_SampleLoad)(System::LongBool mem, void * f, __int64 offset, unsigned length, unsigned max, unsigned flags);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_SampleCreate)(unsigned length, unsigned freq, unsigned chans, unsigned max, unsigned flags);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_SampleFree)(unsigned handle);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_SampleSetData)(unsigned handle, void * buffer);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_SampleGetData)(unsigned handle, void * buffer);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_SampleGetInfo)(unsigned handle, BASS_SAMPLE &info);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_SampleSetInfo)(unsigned handle, BASS_SAMPLE &info);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_SampleGetChannel)(unsigned handle, System::LongBool onlynew);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_SampleGetChannels)(unsigned handle, void * channels);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_SampleStop)(unsigned handle);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_StreamCreate)(unsigned freq, unsigned chans, unsigned flags, STREAMPROC proc, void * user);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_StreamCreateFile)(System::LongBool mem, void * f, __int64 offset, __int64 length, unsigned flags);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_StreamCreateURL)(char * url, unsigned offset, unsigned flags, DOWNLOADPROC proc, void * user);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_StreamCreateFileUser)(unsigned system, unsigned flags, BASS_FILEPROCS &procs, void * user);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_StreamFree)(unsigned handle);
extern DELPHI_PACKAGE __int64 __stdcall (*BASS_StreamGetFilePosition)(unsigned handle, unsigned mode);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_StreamPutData)(unsigned handle, void * buffer, unsigned length);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_StreamPutFileData)(unsigned handle, void * buffer, unsigned length);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_RecordGetDeviceInfo)(unsigned device, BASS_DEVICEINFO &info);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_RecordInit)(int device);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_RecordSetDevice)(unsigned device);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_RecordGetDevice)(void);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_RecordFree)(void);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_RecordGetInfo)(BASS_RECORDINFO &info);
extern DELPHI_PACKAGE char * __stdcall (*BASS_RecordGetInputName)(int input);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_RecordSetInput)(int input, unsigned flags, float volume);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_RecordGetInput)(int input, float &volume);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_RecordStart)(unsigned freq, unsigned chans, unsigned flags, RECORDPROC proc, void * user);
extern DELPHI_PACKAGE double __stdcall (*BASS_ChannelBytes2Seconds)(unsigned handle, __int64 pos);
extern DELPHI_PACKAGE __int64 __stdcall (*BASS_ChannelSeconds2Bytes)(unsigned handle, double pos);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_ChannelGetDevice)(unsigned handle);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelSetDevice)(unsigned handle, unsigned device);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_ChannelIsActive)(unsigned handle);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelGetInfo)(unsigned handle, BASS_CHANNELINFO &info);
extern DELPHI_PACKAGE char * __stdcall (*BASS_ChannelGetTags)(unsigned handle, unsigned tags);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_ChannelFlags)(unsigned handle, unsigned flags, unsigned mask);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelUpdate)(unsigned handle, unsigned length);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelLock)(unsigned handle, System::LongBool lock);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelPlay)(unsigned handle, System::LongBool restart);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelStop)(unsigned handle);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelPause)(unsigned handle);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelSetAttribute)(unsigned handle, unsigned attrib, float value);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelGetAttribute)(unsigned handle, unsigned attrib, float &value);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelSlideAttribute)(unsigned handle, unsigned attrib, float value, unsigned time);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelIsSliding)(unsigned handle, unsigned attrib);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelSetAttributeEx)(unsigned handle, unsigned attrib, void * value, unsigned size);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_ChannelGetAttributeEx)(unsigned handle, unsigned attrib, void * value, unsigned size);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelSet3DAttributes)(unsigned handle, int mode, float min, float max, int iangle, int oangle, int outvol);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelGet3DAttributes)(unsigned handle, unsigned &mode, float &min, float &max, unsigned &iangle, unsigned &oangle, unsigned &outvol);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelSet3DPosition)(unsigned handle, BASS_3DVECTOR &pos, BASS_3DVECTOR &orient, BASS_3DVECTOR &vel);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelGet3DPosition)(unsigned handle, BASS_3DVECTOR &pos, BASS_3DVECTOR &orient, BASS_3DVECTOR &vel);
extern DELPHI_PACKAGE __int64 __stdcall (*BASS_ChannelGetLength)(unsigned handle, unsigned mode);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelSetPosition)(unsigned handle, __int64 pos, unsigned mode);
extern DELPHI_PACKAGE __int64 __stdcall (*BASS_ChannelGetPosition)(unsigned handle, unsigned mode);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_ChannelGetLevel)(unsigned handle);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelGetLevelEx)(unsigned handle, Winapi::Windows::PSingle levels, float length, unsigned flags);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_ChannelGetData)(unsigned handle, void * buffer, unsigned length);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_ChannelSetSync)(unsigned handle, unsigned type_, __int64 param, SYNCPROC proc, void * user);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelRemoveSync)(unsigned handle, unsigned sync);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_ChannelSetDSP)(unsigned handle, DSPPROC proc, void * user, int priority);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelRemoveDSP)(unsigned handle, unsigned dsp);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelSetLink)(unsigned handle, unsigned chan);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelRemoveLink)(unsigned handle, unsigned chan);
extern DELPHI_PACKAGE unsigned __stdcall (*BASS_ChannelSetFX)(unsigned handle, unsigned type_, int priority);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_ChannelRemoveFX)(unsigned handle, unsigned fx);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_FXSetParameters)(unsigned handle, void * par);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_FXGetParameters)(unsigned handle, void * par);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_FXReset)(unsigned handle);
extern DELPHI_PACKAGE System::LongBool __stdcall (*BASS_FXSetPriority)(unsigned handle, int priority);
extern DELPHI_PACKAGE unsigned __fastcall BASS_SPEAKER_N(unsigned n);
extern DELPHI_PACKAGE System::LongBool __fastcall BASS_SetEAXPreset(int env);
extern DELPHI_PACKAGE bool __fastcall BASS_Load(System::WideChar * LibName);
extern DELPHI_PACKAGE void __fastcall BASS_UnLoad(void);
extern DELPHI_PACKAGE bool __fastcall BASS_IsLoaded(void);
}	/* namespace Bassimport */
}	/* namespace Sounds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SOUNDS_BASSIMPORT)
using namespace Sounds::Bassimport;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SOUNDS)
using namespace Sounds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sounds_BassimportHPP
