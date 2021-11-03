(*
  BASS 2.4 Delphi unit
  Copyright (c) 1999-2021 Un4seen Developments Ltd.
  http://www.un4seen.com/music/, free for freeware

  See the BASS.CHM file for more detailed documentation

  How to install
  --------------
  Copy BASS.PAS to the \LIB subdirectory of your Delphi path or your project dir

  NOTE: Delphi users should use the BASS_UNICODE flag where possible
*)
unit Sounds.BASSImport;

interface

uses
  Winapi.Windows;

// Functions
const
{$IFDEF MSWINDOWS}
  {$IFDEF WIN32}
     bassdll = 'bass32.dll';
  {$ELSE}
     bassdll = 'bass64.dll';
  {$ENDIF}
{$ENDIF}
{$IFDEF LINUX}
  bassdll = 'libbass.so';
{$ENDIF}
{$IFDEF MACOS}
  bassdll = 'libbass.dylib';
{$ENDIF}


const
  BASSVERSION = $204;             // API version
  BASSVERSIONTEXT = '2.4';

  // Use these to test for error from functions that return a DWORD or QWORD
  DW_ERROR = LongWord(-1); // -1 (DWORD)
  QW_ERROR = Int64(-1);    // -1 (QWORD)

  // Error codes returned by BASS_ErrorGetCode()
  BASS_OK                 = 0;    // all is OK
  BASS_ERROR_MEM          = 1;    // memory error
  BASS_ERROR_FILEOPEN     = 2;    // can't open the file
  BASS_ERROR_DRIVER       = 3;    // can't find a free sound driver
  BASS_ERROR_BUFLOST      = 4;    // the sample buffer was lost
  BASS_ERROR_HANDLE       = 5;    // invalid handle
  BASS_ERROR_FORMAT       = 6;    // unsupported sample format
  BASS_ERROR_POSITION     = 7;    // invalid position
  BASS_ERROR_INIT         = 8;    // BASS_Init has not been successfully called
  BASS_ERROR_START        = 9;    // BASS_Start has not been successfully called
  BASS_ERROR_ALREADY      = 14;   // already initialized/paused/whatever
  BASS_ERROR_NOCHAN       = 18;   // can't get a free channel
  BASS_ERROR_ILLTYPE      = 19;   // an illegal type was specified
  BASS_ERROR_ILLPARAM     = 20;   // an illegal parameter was specified
  BASS_ERROR_NO3D         = 21;   // no 3D support
  BASS_ERROR_NOEAX        = 22;   // no EAX support
  BASS_ERROR_DEVICE       = 23;   // illegal device number
  BASS_ERROR_NOPLAY       = 24;   // not playing
  BASS_ERROR_FREQ         = 25;   // illegal sample rate
  BASS_ERROR_NOTFILE      = 27;   // the stream is not a file stream
  BASS_ERROR_NOHW         = 29;   // no hardware voices available
  BASS_ERROR_EMPTY        = 31;   // the MOD music has no sequence data
  BASS_ERROR_NONET        = 32;   // no internet connection could be opened
  BASS_ERROR_CREATE       = 33;   // couldn't create the file
  BASS_ERROR_NOFX         = 34;   // effects are not available
  BASS_ERROR_NOTAVAIL     = 37;   // requested data is not available
  BASS_ERROR_DECODE       = 38;   // the channel is/isn't a "decoding channel"
  BASS_ERROR_DX           = 39;   // a sufficient DirectX version is not installed
  BASS_ERROR_TIMEOUT      = 40;   // connection timedout
  BASS_ERROR_FILEFORM     = 41;   // unsupported file format
  BASS_ERROR_SPEAKER      = 42;   // unavailable speaker
  BASS_ERROR_VERSION      = 43;   // invalid BASS version (used by add-ons)
  BASS_ERROR_CODEC        = 44;   // codec is not available/supported
  BASS_ERROR_ENDED        = 45;   // the channel/file has ended
  BASS_ERROR_BUSY         = 46;   // the device is busy
  BASS_ERROR_UNKNOWN      = -1;   // some other mystery problem

  // BASS_SetConfig options
  BASS_CONFIG_BUFFER        = 0;
  BASS_CONFIG_UPDATEPERIOD  = 1;
  BASS_CONFIG_GVOL_SAMPLE   = 4;
  BASS_CONFIG_GVOL_STREAM   = 5;
  BASS_CONFIG_GVOL_MUSIC    = 6;
  BASS_CONFIG_CURVE_VOL     = 7;
  BASS_CONFIG_CURVE_PAN     = 8;
  BASS_CONFIG_FLOATDSP      = 9;
  BASS_CONFIG_3DALGORITHM   = 10;
  BASS_CONFIG_NET_TIMEOUT   = 11;
  BASS_CONFIG_NET_BUFFER    = 12;
  BASS_CONFIG_PAUSE_NOPLAY  = 13;
  BASS_CONFIG_NET_PREBUF    = 15;
  BASS_CONFIG_NET_PASSIVE   = 18;
  BASS_CONFIG_REC_BUFFER    = 19;
  BASS_CONFIG_NET_PLAYLIST  = 21;
  BASS_CONFIG_MUSIC_VIRTUAL = 22;
  BASS_CONFIG_VERIFY        = 23;
  BASS_CONFIG_UPDATETHREADS = 24;
  BASS_CONFIG_DEV_BUFFER    = 27;
  BASS_CONFIG_VISTA_TRUEPOS = 30;
  BASS_CONFIG_IOS_MIXAUDIO  = 34;
  BASS_CONFIG_DEV_DEFAULT   = 36;
  BASS_CONFIG_NET_READTIMEOUT = 37;
  BASS_CONFIG_VISTA_SPEAKERS = 38;
  BASS_CONFIG_IOS_SPEAKER   = 39;
  BASS_CONFIG_MF_DISABLE    = 40;
  BASS_CONFIG_HANDLES       = 41;
  BASS_CONFIG_UNICODE       = 42;
  BASS_CONFIG_SRC           = 43;
  BASS_CONFIG_SRC_SAMPLE    = 44;
  BASS_CONFIG_ASYNCFILE_BUFFER = 45;
  BASS_CONFIG_OGG_PRESCAN   = 47;
  BASS_CONFIG_MF_VIDEO      = 48;
  BASS_CONFIG_AIRPLAY       = 49;
  BASS_CONFIG_DEV_NONSTOP   = 50;
  BASS_CONFIG_IOS_NOCATEGORY = 51;
  BASS_CONFIG_VERIFY_NET    = 52;
  BASS_CONFIG_DEV_PERIOD    = 53;
  BASS_CONFIG_FLOAT         = 54;

  // BASS_SetConfigPtr options
  BASS_CONFIG_NET_AGENT     = 16;
  BASS_CONFIG_NET_PROXY     = 17;

  // BASS_Init flags
  BASS_DEVICE_8BITS       = 1;    // 8 bit resolution, else 16 bit
  BASS_DEVICE_MONO        = 2;    // mono, else stereo
  BASS_DEVICE_3D          = 4;    // enable 3D functionality
  BASS_DEVICE_16BITS      = 8;    // limit output to 16 bit
  {
    If the BASS_DEVICE_3D flag is not specified when
    initilizing BASS, then the 3D flags (BASS_SAMPLE_3D
    and BASS_MUSIC_3D) are ignored when loading/creating
    a sample/stream/music.
  }
  BASS_DEVICE_LATENCY     = $100;  // calculate device latency (BASS_INFO struct)
  BASS_DEVICE_CPSPEAKERS  = $400; // detect speakers via Windows control panel
  BASS_DEVICE_SPEAKERS    = $800; // force enabling of speaker assignment
  BASS_DEVICE_NOSPEAKER   = $1000; // ignore speaker arrangement
  BASS_DEVICE_DMIX        = $2000; // use ALSA "dmix" plugin
  BASS_DEVICE_FREQ        = $4000; // set device sample rate
  BASS_DEVICE_STEREO      = $8000; // limit output to stereo

  // DirectSound interfaces (for use with BASS_GetDSoundObject)
  BASS_OBJECT_DS          = 1;   // IDirectSound
  BASS_OBJECT_DS3DL       = 2;   // IDirectSound3DListener

  // BASS_DEVICEINFO flags
  BASS_DEVICE_ENABLED     = 1;
  BASS_DEVICE_DEFAULT     = 2;
  BASS_DEVICE_INIT        = 4;

  BASS_DEVICE_TYPE_MASK        = $ff000000;
  BASS_DEVICE_TYPE_NETWORK     = $01000000;
  BASS_DEVICE_TYPE_SPEAKERS    = $02000000;
  BASS_DEVICE_TYPE_LINE        = $03000000;
  BASS_DEVICE_TYPE_HEADPHONES  = $04000000;
  BASS_DEVICE_TYPE_MICROPHONE  = $05000000;
  BASS_DEVICE_TYPE_HEADSET     = $06000000;
  BASS_DEVICE_TYPE_HANDSET     = $07000000;
  BASS_DEVICE_TYPE_DIGITAL     = $08000000;
  BASS_DEVICE_TYPE_SPDIF       = $09000000;
  BASS_DEVICE_TYPE_HDMI        = $0a000000;
  BASS_DEVICE_TYPE_DISPLAYPORT = $40000000;

  // BASS_GetDeviceInfo flags
  BASS_DEVICES_AIRPLAY         = $1000000;

  // BASS_INFO flags (from DSOUND.H)
  DSCAPS_CONTINUOUSRATE   = $00000010;     // supports all sample rates between min/maxrate
  DSCAPS_EMULDRIVER       = $00000020;     // device does NOT have hardware DirectSound support
  DSCAPS_CERTIFIED        = $00000040;     // device driver has been certified by Microsoft
  {
    The following flags tell what type of samples are
    supported by HARDWARE mixing, all these formats are
    supported by SOFTWARE mixing
  }
  DSCAPS_SECONDARYMONO    = $00000100;     // mono
  DSCAPS_SECONDARYSTEREO  = $00000200;     // stereo
  DSCAPS_SECONDARY8BIT    = $00000400;     // 8 bit
  DSCAPS_SECONDARY16BIT   = $00000800;     // 16 bit

  // BASS_RECORDINFO flags (from DSOUND.H)
  DSCCAPS_EMULDRIVER = DSCAPS_EMULDRIVER;  // device does NOT have hardware DirectSound recording support
  DSCCAPS_CERTIFIED = DSCAPS_CERTIFIED;    // device driver has been certified by Microsoft

  // defines for formats field of BASS_RECORDINFO (from MMSYSTEM.H)
(*
  WAVE_FORMAT_1M08       = $00000001;      // 11.025 kHz, Mono,   8-bit
  WAVE_FORMAT_1S08       = $00000002;      // 11.025 kHz, Stereo, 8-bit
  WAVE_FORMAT_1M16       = $00000004;      // 11.025 kHz, Mono,   16-bit
  WAVE_FORMAT_1S16       = $00000008;      // 11.025 kHz, Stereo, 16-bit
  WAVE_FORMAT_2M08       = $00000010;      // 22.05  kHz, Mono,   8-bit
  WAVE_FORMAT_2S08       = $00000020;      // 22.05  kHz, Stereo, 8-bit
  WAVE_FORMAT_2M16       = $00000040;      // 22.05  kHz, Mono,   16-bit
  WAVE_FORMAT_2S16       = $00000080;      // 22.05  kHz, Stereo, 16-bit
  WAVE_FORMAT_4M08       = $00000100;      // 44.1   kHz, Mono,   8-bit
  WAVE_FORMAT_4S08       = $00000200;      // 44.1   kHz, Stereo, 8-bit
  WAVE_FORMAT_4M16       = $00000400;      // 44.1   kHz, Mono,   16-bit
  WAVE_FORMAT_4S16       = $00000800;      // 44.1   kHz, Stereo, 16-bit
*)
  BASS_SAMPLE_8BITS       = 1;   // 8 bit
  BASS_SAMPLE_FLOAT       = 256; // 32 bit floating-point
  BASS_SAMPLE_MONO        = 2;   // mono
  BASS_SAMPLE_LOOP        = 4;   // looped
  BASS_SAMPLE_3D          = 8;   // 3D functionality
  BASS_SAMPLE_SOFTWARE    = 16;  // not using hardware mixing
  BASS_SAMPLE_MUTEMAX     = 32;  // mute at max distance (3D only)
  BASS_SAMPLE_VAM         = 64;  // DX7 voice allocation & management
  BASS_SAMPLE_FX          = 128; // old implementation of DX8 effects
  BASS_SAMPLE_OVER_VOL    = $10000; // override lowest volume
  BASS_SAMPLE_OVER_POS    = $20000; // override longest playing
  BASS_SAMPLE_OVER_DIST   = $30000; // override furthest from listener (3D only)

  BASS_STREAM_PRESCAN     = $20000; // enable pin-point seeking/length (MP3/MP2/MP1)
  BASS_MP3_SETPOS         = BASS_STREAM_PRESCAN;
  BASS_STREAM_AUTOFREE	  = $40000; // automatically free the stream when it stop/ends
  BASS_STREAM_RESTRATE	  = $80000; // restrict the download rate of internet file streams
  BASS_STREAM_BLOCK       = $100000;// download/play internet file stream in small blocks
  BASS_STREAM_DECODE      = $200000;// don't play the stream, only decode (BASS_ChannelGetData)
  BASS_STREAM_STATUS      = $800000;// give server status info (HTTP/ICY tags) in DOWNLOADPROC

  BASS_MUSIC_FLOAT        = BASS_SAMPLE_FLOAT; // 32-bit floating-point
  BASS_MUSIC_MONO         = BASS_SAMPLE_MONO; // force mono mixing (less CPU usage)
  BASS_MUSIC_LOOP         = BASS_SAMPLE_LOOP; // loop music
  BASS_MUSIC_3D           = BASS_SAMPLE_3D; // enable 3D functionality
  BASS_MUSIC_FX           = BASS_SAMPLE_FX; // enable old implementation of DX8 effects
  BASS_MUSIC_AUTOFREE     = BASS_STREAM_AUTOFREE; // automatically free the music when it stop/ends
  BASS_MUSIC_DECODE       = BASS_STREAM_DECODE; // don't play the music, only decode (BASS_ChannelGetData)
  BASS_MUSIC_PRESCAN      = BASS_STREAM_PRESCAN; // calculate playback length
  BASS_MUSIC_CALCLEN      = BASS_MUSIC_PRESCAN;
  BASS_MUSIC_RAMP         = $200;  // normal ramping
  BASS_MUSIC_RAMPS        = $400;  // sensitive ramping
  BASS_MUSIC_SURROUND     = $800;  // surround sound
  BASS_MUSIC_SURROUND2    = $1000; // surround sound (mode 2)
  BASS_MUSIC_FT2PAN       = $2000; // apply FastTracker 2 panning to XM files
  BASS_MUSIC_FT2MOD       = $2000; // play .MOD as FastTracker 2 does
  BASS_MUSIC_PT1MOD       = $4000; // play .MOD as ProTracker 1 does
  BASS_MUSIC_NONINTER     = $10000; // non-interpolated sample mixing
  BASS_MUSIC_SINCINTER    = $800000; // sinc interpolated sample mixing
  BASS_MUSIC_POSRESET     = $8000; // stop all notes when moving position
  BASS_MUSIC_POSRESETEX   = $400000; // stop all notes and reset bmp/etc when moving position
  BASS_MUSIC_STOPBACK     = $80000; // stop the music on a backwards jump effect
  BASS_MUSIC_NOSAMPLE     = $100000; // don't load the samples

  // Speaker assignment flags
  BASS_SPEAKER_FRONT      = $1000000;  // front speakers
  BASS_SPEAKER_REAR       = $2000000;  // rear/side speakers
  BASS_SPEAKER_CENLFE     = $3000000;  // center & LFE speakers (5.1)
  BASS_SPEAKER_REAR2      = $4000000;  // rear center speakers (7.1)
  BASS_SPEAKER_LEFT       = $10000000; // modifier: left
  BASS_SPEAKER_RIGHT      = $20000000; // modifier: right
  BASS_SPEAKER_FRONTLEFT  = BASS_SPEAKER_FRONT or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_FRONTRIGHT = BASS_SPEAKER_FRONT or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_REARLEFT   = BASS_SPEAKER_REAR or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_REARRIGHT  = BASS_SPEAKER_REAR or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_CENTER     = BASS_SPEAKER_CENLFE or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_LFE        = BASS_SPEAKER_CENLFE or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_REAR2LEFT  = BASS_SPEAKER_REAR2 or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_REAR2RIGHT = BASS_SPEAKER_REAR2 or BASS_SPEAKER_RIGHT;

  BASS_ASYNCFILE          = $40000000;
  BASS_UNICODE            = $80000000;

  BASS_RECORD_PAUSE       = $8000; // start recording paused

  // DX7 voice allocation & management flags
  BASS_VAM_HARDWARE       = 1;
  {
    Play the sample in hardware. If no hardware voices are available then
    the "play" call will fail
  }
  BASS_VAM_SOFTWARE       = 2;
  {
    Play the sample in software (ie. non-accelerated). No other VAM flags
    may be used together with this flag.
  }

  // DX7 voice management flags
  {
    These flags enable hardware resource stealing... if the hardware has no
    available voices, a currently playing buffer will be stopped to make room
    for the new buffer. NOTE: only samples loaded/created with the
    BASS_SAMPLE_VAM flag are considered for termination by the DX7 voice
    management.
  }
  BASS_VAM_TERM_TIME      = 4;
  {
    If there are no free hardware voices, the buffer to be terminated will be
    the one with the least time left to play.
  }
  BASS_VAM_TERM_DIST      = 8;
  {
    If there are no free hardware voices, the buffer to be terminated will be
    one that was loaded/created with the BASS_SAMPLE_MUTEMAX flag and is
    beyond
    it's max distance. If there are no buffers that match this criteria, then
    the "play" call will fail.
  }
  BASS_VAM_TERM_PRIO      = 16;
  {
    If there are no free hardware voices, the buffer to be terminated will be
    the one with the lowest priority.
  }

  // BASS_CHANNELINFO types
  BASS_CTYPE_SAMPLE       = 1;
  BASS_CTYPE_RECORD       = 2;
  BASS_CTYPE_STREAM       = $10000;
  BASS_CTYPE_STREAM_OGG   = $10002;
  BASS_CTYPE_STREAM_MP1   = $10003;
  BASS_CTYPE_STREAM_MP2   = $10004;
  BASS_CTYPE_STREAM_MP3   = $10005;
  BASS_CTYPE_STREAM_AIFF  = $10006;
  BASS_CTYPE_STREAM_WAV   = $40000; // WAVE flag, LOWORD=codec
  BASS_CTYPE_STREAM_WAV_PCM = $50001;
  BASS_CTYPE_STREAM_WAV_FLOAT = $50003;
  BASS_CTYPE_MUSIC_MOD    = $20000;
  BASS_CTYPE_MUSIC_MTM    = $20001;
  BASS_CTYPE_MUSIC_S3M    = $20002;
  BASS_CTYPE_MUSIC_XM     = $20003;
  BASS_CTYPE_MUSIC_IT     = $20004;
  BASS_CTYPE_MUSIC_MO3    = $00100; // MO3 flag

  // 3D channel modes
  BASS_3DMODE_NORMAL      = 0; // normal 3D processing
  BASS_3DMODE_RELATIVE    = 1; // position is relative to the listener
  {
    The channel's 3D position (position/velocity/
    orientation) are relative to the listener. When the
    listener's position/velocity/orientation is changed
    with BASS_Set3DPosition, the channel's position
    relative to the listener does not change.
  }
  BASS_3DMODE_OFF         = 2; // no 3D processing
  {
    Turn off 3D processing on the channel, the sound will
    be played in the center.
  }

  // software 3D mixing algorithms (used with BASS_CONFIG_3DALGORITHM)
  BASS_3DALG_DEFAULT      = 0;
  {
    default algorithm (currently translates to BASS_3DALG_OFF)
  }
  BASS_3DALG_OFF          = 1;
  {
    Uses normal left and right panning. The vertical axis is ignored except
    for scaling of volume due to distance. Doppler shift and volume scaling
    are still applied, but the 3D filtering is not performed. This is the
    most CPU efficient software implementation, but provides no virtual 3D
    audio effect. Head Related Transfer Function processing will not be done.
    Since only normal stereo panning is used, a channel using this algorithm
    may be accelerated by a 2D hardware voice if no free 3D hardware voices
    are available.
  }
  BASS_3DALG_FULL         = 2;
  {
    This algorithm gives the highest quality 3D audio effect, but uses more
    CPU. Requires Windows 98 2nd Edition or Windows 2000 that uses WDM
    drivers, if this mode is not available then BASS_3DALG_OFF will be used
    instead.
  }
  BASS_3DALG_LIGHT        = 3;
  {
    This algorithm gives a good 3D audio effect, and uses less CPU than the
    FULL mode. Requires Windows 98 2nd Edition or Windows 2000 that uses WDM
    drivers, if this mode is not available then BASS_3DALG_OFF will be used
    instead.
  }

  // EAX environments, use with BASS_SetEAXParameters
  EAX_ENVIRONMENT_GENERIC           = 0;
  EAX_ENVIRONMENT_PADDEDCELL        = 1;
  EAX_ENVIRONMENT_ROOM              = 2;
  EAX_ENVIRONMENT_BATHROOM          = 3;
  EAX_ENVIRONMENT_LIVINGROOM        = 4;
  EAX_ENVIRONMENT_STONEROOM         = 5;
  EAX_ENVIRONMENT_AUDITORIUM        = 6;
  EAX_ENVIRONMENT_CONCERTHALL       = 7;
  EAX_ENVIRONMENT_CAVE              = 8;
  EAX_ENVIRONMENT_ARENA             = 9;
  EAX_ENVIRONMENT_HANGAR            = 10;
  EAX_ENVIRONMENT_CARPETEDHALLWAY   = 11;
  EAX_ENVIRONMENT_HALLWAY           = 12;
  EAX_ENVIRONMENT_STONECORRIDOR     = 13;
  EAX_ENVIRONMENT_ALLEY             = 14;
  EAX_ENVIRONMENT_FOREST            = 15;
  EAX_ENVIRONMENT_CITY              = 16;
  EAX_ENVIRONMENT_MOUNTAINS         = 17;
  EAX_ENVIRONMENT_QUARRY            = 18;
  EAX_ENVIRONMENT_PLAIN             = 19;
  EAX_ENVIRONMENT_PARKINGLOT        = 20;
  EAX_ENVIRONMENT_SEWERPIPE         = 21;
  EAX_ENVIRONMENT_UNDERWATER        = 22;
  EAX_ENVIRONMENT_DRUGGED           = 23;
  EAX_ENVIRONMENT_DIZZY             = 24;
  EAX_ENVIRONMENT_PSYCHOTIC         = 25;
  // total number of environments
  EAX_ENVIRONMENT_COUNT             = 26;

  BASS_STREAMPROC_END = $80000000; // end of user stream flag

  // BASS_StreamCreateFileUser file systems
  STREAMFILE_NOBUFFER     = 0;
  STREAMFILE_BUFFER       = 1;
  STREAMFILE_BUFFERPUSH   = 2;

  // BASS_StreamPutFileData options
  BASS_FILEDATA_END       = 0; // end & close the file

  // BASS_StreamGetFilePosition modes
  BASS_FILEPOS_CURRENT    = 0;
  BASS_FILEPOS_DECODE     = BASS_FILEPOS_CURRENT;
  BASS_FILEPOS_DOWNLOAD   = 1;
  BASS_FILEPOS_END        = 2;
  BASS_FILEPOS_START      = 3;
  BASS_FILEPOS_CONNECTED  = 4;
  BASS_FILEPOS_BUFFER     = 5;
  BASS_FILEPOS_SOCKET     = 6;
  BASS_FILEPOS_ASYNCBUF   = 7;
  BASS_FILEPOS_SIZE       = 8;

  // BASS_ChannelSetSync types
  {
    Sync types (with BASS_ChannelSetSync() "param" and
    SYNCPROC "data" definitions) & flags.
  }
  BASS_SYNC_POS           = 0;
  {
    Sync when a channel reaches a position.
    param: position in bytes
    data : not used
  }
  BASS_SYNC_END           = 2;
  {
    Sync when a channel reaches the end.
    param: not used
    data : not used
  }
  BASS_SYNC_META          = 4;
  {
    Sync when metadata is received in a stream.
    param: not used
    data : pointer to the metadata
  }
  BASS_SYNC_SLIDE         = 5;
  {
    Sync when an attribute slide is completed.
    param: not used
    data : the type of slide completed (one of the BASS_SLIDE_xxx values)
  }
  BASS_SYNC_STALL         = 6;
  {
    Sync when playback has stalled.
    param: not used
    data : 0=stalled, 1=resumed
  }
  BASS_SYNC_DOWNLOAD      = 7;
  {
    Sync when downloading of an internet (or "buffered" user file) stream has ended.
    param: not used
    data : not used
  }
  BASS_SYNC_FREE          = 8;
  {
    Sync when a channel is freed.
    param: not used
    data : not used
  }
  BASS_SYNC_SETPOS        = 11;
  {
    Sync when a channel's position is set.
    param: not used
    data : 0 = playback buffer not flushed, 1 = playback buffer flushed
  }
  BASS_SYNC_MUSICPOS      = 10;
  {
    Sync when a MOD music reaches an order:row position.
    param: LOWORD=order (0=first, -1=all) HIWORD=row (0=first, -1=all)
    data : LOWORD=order HIWORD=row
  }
  BASS_SYNC_MUSICINST     = 1;
  {
    Sync when an instrument (sample for the non-instrument based formats)
    is played in a MOD music (not including retrigs).
    param: LOWORD=instrument (1=first) HIWORD=note (0=c0...119=b9, -1=all)
    data : LOWORD=note HIWORD=volume (0-64)
  }
  BASS_SYNC_MUSICFX       = 3;
  {
    Sync when the "sync" effect (XM/MTM/MOD: E8x/Wxx, IT/S3M: S2x) is used.
    param: 0:data=pos, 1:data="x" value
    data : param=0: LOWORD=order HIWORD=row, param=1: "x" value
  }
  BASS_SYNC_OGG_CHANGE    = 12;
  { FLAG: post a Windows message (instead of callback)
    When using a window message "callback", the message to post is given in the "proc"
    parameter of BASS_ChannelSetSync, and is posted to the window specified in the BASS_Init
    call. The message parameters are: WPARAM = data, LPARAM = user.
  }
  BASS_SYNC_MIXTIME       = $40000000; // FLAG: sync at mixtime, else at playtime
  BASS_SYNC_ONETIME       = $80000000; // FLAG: sync only once, else continuously

  // BASS_ChannelIsActive return values
  BASS_ACTIVE_STOPPED = 0;
  BASS_ACTIVE_PLAYING = 1;
  BASS_ACTIVE_STALLED = 2;
  BASS_ACTIVE_PAUSED  = 3;

  // Channel attributes
  BASS_ATTRIB_FREQ                  = 1;
  BASS_ATTRIB_VOL                   = 2;
  BASS_ATTRIB_PAN                   = 3;
  BASS_ATTRIB_EAXMIX                = 4;
  BASS_ATTRIB_NOBUFFER              = 5;
  BASS_ATTRIB_VBR                   = 6;
  BASS_ATTRIB_CPU                   = 7;
  BASS_ATTRIB_SRC                   = 8;
  BASS_ATTRIB_NET_RESUME            = 9;
  BASS_ATTRIB_SCANINFO              = 10;
  BASS_ATTRIB_NORAMP                = 11;
  BASS_ATTRIB_BITRATE               = 12;
  BASS_ATTRIB_MUSIC_AMPLIFY         = $100;
  BASS_ATTRIB_MUSIC_PANSEP          = $101;
  BASS_ATTRIB_MUSIC_PSCALER         = $102;
  BASS_ATTRIB_MUSIC_BPM             = $103;
  BASS_ATTRIB_MUSIC_SPEED           = $104;
  BASS_ATTRIB_MUSIC_VOL_GLOBAL      = $105;
  BASS_ATTRIB_MUSIC_ACTIVE          = $106;
  BASS_ATTRIB_MUSIC_VOL_CHAN        = $200; // + channel #
  BASS_ATTRIB_MUSIC_VOL_INST        = $300; // + instrument #

  // BASS_ChannelGetData flags
  BASS_DATA_AVAILABLE = 0;        // query how much data is buffered
  BASS_DATA_FIXED     = $20000000; // flag: return 8.24 fixed-point data
  BASS_DATA_FLOAT     = $40000000; // flag: return floating-point sample data
  BASS_DATA_FFT256    = $80000000; // 256 sample FFT
  BASS_DATA_FFT512    = $80000001; // 512 FFT
  BASS_DATA_FFT1024   = $80000002; // 1024 FFT
  BASS_DATA_FFT2048   = $80000003; // 2048 FFT
  BASS_DATA_FFT4096   = $80000004; // 4096 FFT
  BASS_DATA_FFT8192   = $80000005; // 8192 FFT
  BASS_DATA_FFT16384  = $80000006; // 16384 FFT
  BASS_DATA_FFT32768  = $80000007; // 32768 FFT
  BASS_DATA_FFT_INDIVIDUAL = $10; // FFT flag: FFT for each channel, else all combined
  BASS_DATA_FFT_NOWINDOW = $20;   // FFT flag: no Hanning window
  BASS_DATA_FFT_REMOVEDC = $40;   // FFT flag: pre-remove DC bias
  BASS_DATA_FFT_COMPLEX = $80;    // FFT flag: return complex data

  // BASS_ChannelGetLevelEx flags
  BASS_LEVEL_MONO     = 1;
  BASS_LEVEL_STEREO   = 2;
  BASS_LEVEL_RMS      = 4;

  // BASS_ChannelGetTags types : what's returned
  BASS_TAG_ID3        = 0; // ID3v1 tags : TAG_ID3 structure
  BASS_TAG_ID3V2      = 1; // ID3v2 tags : variable length block
  BASS_TAG_OGG        = 2; // OGG comments : series of null-terminated UTF-8 strings
  BASS_TAG_HTTP       = 3; // HTTP headers : series of null-terminated ANSI strings
  BASS_TAG_ICY        = 4; // ICY headers : series of null-terminated ANSI strings
  BASS_TAG_META       = 5; // ICY metadata : ANSI string
  BASS_TAG_APE        = 6; // APEv2 tags : series of null-terminated UTF-8 strings
  BASS_TAG_MP4        = 7; // MP4/iTunes metadata : series of null-terminated UTF-8 strings
  BASS_TAG_WMA        = 8; // WMA tags : series of null-terminated UTF-8 strings
  BASS_TAG_VENDOR     = 9; // OGG encoder : UTF-8 string
  BASS_TAG_LYRICS3    = 10; // Lyric3v2 tag : ASCII string
  BASS_TAG_CA_CODEC   = 11;	// CoreAudio codec info : TAG_CA_CODEC structure
  BASS_TAG_MF         = 13;	// Media Foundation tags : series of null-terminated UTF-8 strings
  BASS_TAG_WAVEFORMAT = 14;	// WAVE format : WAVEFORMATEEX structure
  BASS_TAG_RIFF_INFO  = $100; // RIFF "INFO" tags : series of null-terminated ANSI strings
  BASS_TAG_RIFF_BEXT  = $101; // RIFF/BWF "bext" tags : TAG_BEXT structure
  BASS_TAG_RIFF_CART  = $102; // RIFF/BWF "cart" tags : TAG_CART structure
  BASS_TAG_RIFF_DISP  = $103; // RIFF "DISP" text tag : ANSI string
  BASS_TAG_APE_BINARY = $1000; // + index #, binary APEv2 tag : TAG_APE_BINARY structure
  BASS_TAG_MUSIC_NAME = $10000;	// MOD music name : ANSI string
  BASS_TAG_MUSIC_MESSAGE = $10001; // MOD message : ANSI string
  BASS_TAG_MUSIC_ORDERS = $10002; // MOD order list : BYTE array of pattern numbers
  BASS_TAG_MUSIC_AUTH = $10003; // MOD author : UTF-8 string
  BASS_TAG_MUSIC_INST = $10100;	// + instrument #, MOD instrument name : ANSI string
  BASS_TAG_MUSIC_SAMPLE = $10300; // + sample #, MOD sample name : ANSI string

  // BASS_ChannelGetLength/GetPosition/SetPosition modes
  BASS_POS_BYTE           = 0; // byte position
  BASS_POS_MUSIC_ORDER    = 1; // order.row position, MAKELONG(order,row)
  BASS_POS_OGG            = 3; // OGG bitstream number
  BASS_POS_INEXACT        = $8000000; // flag: allow seeking to inexact position
  BASS_POS_DECODE         = $10000000; // flag: get the decoding (not playing) position
  BASS_POS_DECODETO       = $20000000; // flag: decode to the position instead of seeking
  BASS_POS_SCAN           = $40000000; // flag: scan to the position

  // BASS_RecordSetInput flags
  BASS_INPUT_OFF    = $10000;
  BASS_INPUT_ON     = $20000;

  BASS_INPUT_TYPE_MASK    = $FF000000;
  BASS_INPUT_TYPE_UNDEF   = $00000000;
  BASS_INPUT_TYPE_DIGITAL = $01000000;
  BASS_INPUT_TYPE_LINE    = $02000000;
  BASS_INPUT_TYPE_MIC     = $03000000;
  BASS_INPUT_TYPE_SYNTH   = $04000000;
  BASS_INPUT_TYPE_CD      = $05000000;
  BASS_INPUT_TYPE_PHONE   = $06000000;
  BASS_INPUT_TYPE_SPEAKER = $07000000;
  BASS_INPUT_TYPE_WAVE    = $08000000;
  BASS_INPUT_TYPE_AUX     = $09000000;
  BASS_INPUT_TYPE_ANALOG  = $0A000000;

  BASS_FX_DX8_CHORUS	  = 0;
  BASS_FX_DX8_COMPRESSOR  = 1;
  BASS_FX_DX8_DISTORTION  = 2;
  BASS_FX_DX8_ECHO        = 3;
  BASS_FX_DX8_FLANGER     = 4;
  BASS_FX_DX8_GARGLE      = 5;
  BASS_FX_DX8_I3DL2REVERB = 6;
  BASS_FX_DX8_PARAMEQ     = 7;
  BASS_FX_DX8_REVERB      = 8;

  BASS_DX8_PHASE_NEG_180 = 0;
  BASS_DX8_PHASE_NEG_90  = 1;
  BASS_DX8_PHASE_ZERO    = 2;
  BASS_DX8_PHASE_90      = 3;
  BASS_DX8_PHASE_180     = 4;

type
  DWORD = LongWord;
  BOOL = LongBool;
  QWORD = Int64;

  HMUSIC = DWORD;       // MOD music handle
  HSAMPLE = DWORD;      // sample handle
  HCHANNEL = DWORD;     // playing sample's channel handle
  HSTREAM = DWORD;      // sample stream handle
  HRECORD = DWORD;      // recording handle
  HSYNC = DWORD;        // synchronizer handle
  HDSP = DWORD;         // DSP handle
  HFX = DWORD;          // DX8 effect handle
  HPLUGIN = DWORD;      // Plugin handle

  // Device info structure
  BASS_DEVICEINFO = record
    name: PAnsiChar;    // description
    driver: PAnsiChar;  // driver
    flags: DWORD;
  end;

  BASS_INFO = record
    flags: DWORD;       // device capabilities (DSCAPS_xxx flags)
    hwsize: DWORD;      // size of total device hardware memory
    hwfree: DWORD;      // size of free device hardware memory
    freesam: DWORD;     // number of free sample slots in the hardware
    free3d: DWORD;      // number of free 3D sample slots in the hardware
    minrate: DWORD;     // min sample rate supported by the hardware
    maxrate: DWORD;     // max sample rate supported by the hardware
    eax: BOOL;          // device supports EAX? (always FALSE if BASS_DEVICE_3D was not used)
    minbuf: DWORD;      // recommended minimum buffer length in ms (requires BASS_DEVICE_LATENCY)
    dsver: DWORD;       // DirectSound version
    latency: DWORD;     // delay (in ms) before start of playback (requires BASS_DEVICE_LATENCY)
    initflags: DWORD;   // BASS_Init "flags" parameter
    speakers: DWORD;    // number of speakers available
    freq: DWORD;        // current output rate
  end;

  // Recording device info structure
  BASS_RECORDINFO = record
    flags: DWORD;       // device capabilities (DSCCAPS_xxx flags)
    formats: DWORD;     // supported standard formats (WAVE_FORMAT_xxx flags)
    inputs: DWORD;      // number of inputs
    singlein: BOOL;     // only 1 input can be set at a time
    freq: DWORD;        // current input rate
  end;

  // Sample info structure
  BASS_SAMPLE = record
    freq: DWORD;        // default playback rate
    volume: Single;     // default volume (0-100)
    pan: Single;        // default pan (-100=left, 0=middle, 100=right)
    flags: DWORD;       // BASS_SAMPLE_xxx flags
    length: DWORD;      // length (in samples, not bytes)
    max: DWORD;         // maximum simultaneous playbacks
    origres: DWORD;     // original resolution
    chans: DWORD;       // number of channels
    mingap: DWORD;      // minimum gap (ms) between creating channels
    {
      The following are the sample's default 3D attributes
      (if the sample is 3D, BASS_SAMPLE_3D is in flags)
      see BASS_ChannelSet3DAttributes
    }
    mode3d: DWORD;      // BASS_3DMODE_xxx mode
    mindist: Single;    // minimum distance
    maxdist: Single;    // maximum distance
    iangle: DWORD;      // angle of inside projection cone
    oangle: DWORD;      // angle of outside projection cone
    outvol: Single;     // delta-volume outside the projection cone
    {
      The following are the defaults used if the sample uses the DirectX 7
      voice allocation/management features.
    }
    vam: DWORD;         // voice allocation/management flags (BASS_VAM_xxx)
    priority: DWORD;    // priority (0=lowest, $ffffffff=highest)
  end;

  // Channel info structure
  BASS_CHANNELINFO = record
    freq: DWORD;        // default playback rate
    chans: DWORD;       // channels
    flags: DWORD;       // BASS_SAMPLE/STREAM/MUSIC/SPEAKER flags
    ctype: DWORD;       // type of channel
    origres: DWORD;     // original resolution
    plugin: HPLUGIN;    // plugin
    sample: HSAMPLE;    // sample
    {$IFDEF CPUX64}
    padding: DWORD;
    {$ENDIF}
    filename: PChar;    // filename
  end;

  BASS_PLUGINFORM = record
    ctype: DWORD;       // channel type
    {$IFDEF CPUX64}
    padding: DWORD;
    {$ENDIF}
    name: PAnsiChar;    // format description
    exts: PAnsiChar;    // file extension filter (*.ext1;*.ext2;etc...)
  end;
  PBASS_PLUGINFORMS = ^TBASS_PLUGINFORMS;
  TBASS_PLUGINFORMS = array[0..maxInt div sizeOf(BASS_PLUGINFORM) - 1] of BASS_PLUGINFORM;

  PBASS_PLUGININFO = ^BASS_PLUGININFO;
  BASS_PLUGININFO = record
    version: DWORD;             // version (same form as BASS_GetVersion)
    formatc: DWORD;             // number of formats
    formats: PBASS_PLUGINFORMS; // the array of formats
  end;

  // 3D vector (for 3D positions/velocities/orientations)
  BASS_3DVECTOR = record
    x: Single;          // +=right, -=left
    y: Single;          // +=up, -=down
    z: Single;          // +=front, -=behind
  end;

  // User file stream callback functions
  FILECLOSEPROC = procedure(user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  FILELENPROC = function(user: Pointer): QWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  FILEREADPROC = function(buffer: Pointer; length: DWORD; user: Pointer): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  FILESEEKPROC = function(offset: QWORD; user: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  BASS_FILEPROCS = record
    close: FILECLOSEPROC;
    length: FILELENPROC;
    read: FILEREADPROC;
    seek: FILESEEKPROC;
  end;

  // ID3v1 tag structure
  PTAG_ID3 = ^TAG_ID3;
  TAG_ID3 = record
    id: Array[0..2] of AnsiChar;
    title: Array[0..29] of AnsiChar;
    artist: Array[0..29] of AnsiChar;
    album: Array[0..29] of AnsiChar;
    year: Array[0..3] of AnsiChar;
    comment: Array[0..29] of AnsiChar;
    genre: Byte;
  end;

  // Binary APEv2 tag structure
  PTAG_APE_BINARY = ^TAG_APE_BINARY;
  TAG_APE_BINARY = record
    key: PAnsiChar;
    data: PAnsiChar;
    length: DWORD;
  end;

  // BWF "bext" tag structure
  PTAG_BEXT = ^TAG_BEXT;
  TAG_BEXT = packed record
    Description: Array[0..255] of AnsiChar;     // description
    Originator: Array[0..31] of AnsiChar;       // name of the originator
    OriginatorReference: Array[0..31] of AnsiChar; // reference of the originator
    OriginationDate: Array[0..9] of AnsiChar;   // date of creation (yyyy-mm-dd)
    OriginationTime: Array[0..7] of AnsiChar;   // time of creation (hh-mm-ss)
    TimeReference: QWORD;                       // first sample count since midnight (little-endian)
    Version: Word;                              // BWF version (little-endian)
    UMID: Array[0..63] of Byte;                 // SMPTE UMID
    Reserved: Array[0..189] of Byte;
    CodingHistory: Array[0..maxInt div 2 - 1] of AnsiChar;           // history
  end;

  BASS_DX8_CHORUS = record
    fWetDryMix: Single;
    fDepth: Single;
    fFeedback: Single;
    fFrequency: Single;
    lWaveform: DWORD;   // 0=triangle, 1=sine
    fDelay: Single;
    lPhase: DWORD;      // BASS_DX8_PHASE_xxx
  end;

  BASS_DX8_COMPRESSOR = record
    fGain: Single;
    fAttack: Single;
    fRelease: Single;
    fThreshold: Single;
    fRatio: Single;
    fPredelay: Single;
  end;

  BASS_DX8_DISTORTION = record
    fGain: Single;
    fEdge: Single;
    fPostEQCenterFrequency: Single;
    fPostEQBandwidth: Single;
    fPreLowpassCutoff: Single;
  end;

  BASS_DX8_ECHO = record
    fWetDryMix: Single;
    fFeedback: Single;
    fLeftDelay: Single;
    fRightDelay: Single;
    lPanDelay: BOOL;
  end;

  BASS_DX8_FLANGER = record
    fWetDryMix: Single;
    fDepth: Single;
    fFeedback: Single;
    fFrequency: Single;
    lWaveform: DWORD;   // 0=triangle, 1=sine
    fDelay: Single;
    lPhase: DWORD;      // BASS_DX8_PHASE_xxx
  end;

  BASS_DX8_GARGLE = record
    dwRateHz: DWORD;               // Rate of modulation in hz
    dwWaveShape: DWORD;            // 0=triangle, 1=square
  end;

  BASS_DX8_I3DL2REVERB = record
    lRoom: LongInt;                // [-10000, 0]      default: -1000 mB
    lRoomHF: LongInt;              // [-10000, 0]      default: 0 mB
    flRoomRolloffFactor: Single;   // [0.0, 10.0]      default: 0.0
    flDecayTime: Single;           // [0.1, 20.0]      default: 1.49s
    flDecayHFRatio: Single;        // [0.1, 2.0]       default: 0.83
    lReflections: LongInt;         // [-10000, 1000]   default: -2602 mB
    flReflectionsDelay: Single;    // [0.0, 0.3]       default: 0.007 s
    lReverb: LongInt;              // [-10000, 2000]   default: 200 mB
    flReverbDelay: Single;         // [0.0, 0.1]       default: 0.011 s
    flDiffusion: Single;           // [0.0, 100.0]     default: 100.0 %
    flDensity: Single;             // [0.0, 100.0]     default: 100.0 %
    flHFReference: Single;         // [20.0, 20000.0]  default: 5000.0 Hz
  end;

  BASS_DX8_PARAMEQ = record
    fCenter: Single;
    fBandwidth: Single;
    fGain: Single;
  end;

  BASS_DX8_REVERB = record
    fInGain: Single;               // [-96.0,0.0]            default: 0.0 dB
    fReverbMix: Single;            // [-96.0,0.0]            default: 0.0 db
    fReverbTime: Single;           // [0.001,3000.0]         default: 1000.0 ms
    fHighFreqRTRatio: Single;      // [0.001,0.999]          default: 0.001
  end;

  // callback function types
  STREAMPROC = function(handle: HSTREAM; buffer: Pointer; length: DWORD; user: Pointer): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    User stream callback function. NOTE: A stream function should obviously be as
    quick as possible, other streams (and MOD musics) can't be mixed until
    it's finished.
    handle : The stream that needs writing
    buffer : Buffer to write the samples in
    length : Number of bytes to write
    user   : The 'user' parameter value given when calling BASS_StreamCreate
    RETURN : Number of bytes written. Set the BASS_STREAMPROC_END flag to end
             the stream.
  }

const
  // special STREAMPROCs
  STREAMPROC_DUMMY = Pointer(0);   // "dummy" stream
  STREAMPROC_PUSH = Pointer(-1);   // push stream

type

  {  
    User file stream callback function.
    action : The action to perform, one of BASS_FILE_xxx values.
    param1 : Depends on "action"
    param2 : Depends on "action"
    user   : The 'user' parameter value given when calling BASS_StreamCreate
    RETURN : Depends on "action"
  }

  DOWNLOADPROC = procedure(buffer: Pointer; length: DWORD; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Internet stream download callback function.
    buffer : Buffer containing the downloaded data... NULL=end of download
    length : Number of bytes in the buffer
    user   : The 'user' parameter value given when calling BASS_StreamCreateURL
  }

  SYNCPROC = procedure(handle: HSYNC; channel, data: DWORD; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Sync callback function. NOTE: a sync callback function should be very
    quick as other syncs cannot be processed until it has finished. If the
    sync is a "mixtime" sync, then other streams and MOD musics can not be
    mixed until it's finished either.
    handle : The sync that has occured
    channel: Channel that the sync occured in
    data   : Additional data associated with the sync's occurance
    user   : The 'user' parameter given when calling BASS_ChannelSetSync
  }

  DSPPROC = procedure(handle: HDSP; channel: DWORD; buffer: Pointer; length: DWORD; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    DSP callback function. NOTE: A DSP function should obviously be as quick
    as possible... other DSP functions, streams and MOD musics can not be
    processed until it's finished.
    handle : The DSP handle
    channel: Channel that the DSP is being applied to
    buffer : Buffer to apply the DSP to
    length : Number of bytes in the buffer
    user   : The 'user' parameter given when calling BASS_ChannelSetDSP
  }

  RECORDPROC = function(handle: HRECORD; buffer: Pointer; length: DWORD; user: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Recording callback function.
    handle : The recording handle
    buffer : Buffer containing the recorded sample data
    length : Number of bytes
    user   : The 'user' parameter value given when calling BASS_RecordStart
    RETURN : TRUE = continue recording, FALSE = stop
  }

var
BASS_SetConfig: function(option, value: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_GetConfig: function(option: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_SetConfigPtr: function(option: DWORD; value: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_GetConfigPtr: function(option: DWORD): Pointer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_GetVersion: function(): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ErrorGetCode: function(): LongInt; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_GetDeviceInfo: function(device: DWORD; var info: BASS_DEVICEINFO): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
{$IFDEF MSWINDOWS}
BASS_Init: function(device: LongInt; freq, flags: DWORD; win: HWND; clsid: PGUID): BOOL; stdcall;// external bassdll;
{$ELSE}
BASS_Init: function(device: LongInt; freq, flags: DWORD; win: Pointer; clsid: Pointer): BOOL; cdecl; //external bassdll;
{$ENDIF}
BASS_SetDevice: function(device: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_GetDevice: function():DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_Free: function():BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
{$IFDEF MSWINDOWS}
BASS_GetDSoundObject: function(obj: DWORD): Pointer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
{$ENDIF}
BASS_GetInfo: function(var info: BASS_INFO): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_Update: function(length: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_GetCPU: function():Single; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_Start: function():BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_Stop: function():BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_Pause: function():BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_SetVolume: function(volume: Single): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_GetVolume: function():Single; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;

BASS_PluginLoad: function(filename: PChar; flags: DWORD): HPLUGIN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_PluginFree: function(handle: HPLUGIN): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_PluginGetInfo: function(handle: HPLUGIN): PBASS_PLUGININFO; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;

BASS_Set3DFactors: function(distf, rollf, doppf: Single): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_Get3DFactors: function(var distf, rollf, doppf: Single): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_Set3DPosition: function(var pos, vel, front, top: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_Get3DPosition: function(var pos, vel, front, top: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_Apply3D: procedure(); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
{$IFDEF MSWINDOWS}
BASS_SetEAXParameters: function(env: LongInt; vol, decay, damp: Single): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_GetEAXParameters: function(var env: DWORD; var vol, decay, damp: Single): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
{$ENDIF}

BASS_MusicLoad: function(mem: BOOL; f: Pointer; offset: QWORD; length, flags, freq: DWORD): HMUSIC; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_MusicFree: function(handle: HMUSIC): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;

BASS_SampleLoad: function(mem: BOOL; f: Pointer; offset: QWORD; length, max, flags: DWORD): HSAMPLE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_SampleCreate: function(length, freq, chans, max, flags: DWORD): HSAMPLE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_SampleFree: function(handle: HSAMPLE): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_SampleSetData: function(handle: HSAMPLE; buffer: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_SampleGetData: function(handle: HSAMPLE; buffer: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_SampleGetInfo: function(handle: HSAMPLE; var info: BASS_SAMPLE): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_SampleSetInfo: function(handle: HSAMPLE; var info: BASS_SAMPLE): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_SampleGetChannel: function(handle: HSAMPLE; onlynew: BOOL): HCHANNEL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_SampleGetChannels: function(handle: HSAMPLE; channels: Pointer): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_SampleStop: function(handle: HSAMPLE): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;

BASS_StreamCreate: function(freq, chans, flags: DWORD; proc: STREAMPROC; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_StreamCreateFile: function(mem: BOOL; f: Pointer; offset, length: QWORD; flags: DWORD): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_StreamCreateURL: function(url: PAnsiChar; offset: DWORD; flags: DWORD; proc: DOWNLOADPROC; user: Pointer):HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_StreamCreateFileUser: function(system, flags: DWORD; var procs: BASS_FILEPROCS; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_StreamFree: function(handle: HSTREAM): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_StreamGetFilePosition: function(handle: HSTREAM; mode: DWORD): QWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_StreamPutData: function(handle: HSTREAM; buffer: Pointer; length: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_StreamPutFileData: function(handle: HSTREAM; buffer: Pointer; length: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;

BASS_RecordGetDeviceInfo: function(device: DWORD; var info: BASS_DEVICEINFO): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_RecordInit: function(device: LongInt):BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_RecordSetDevice: function(device: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_RecordGetDevice: function():DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_RecordFree: function():BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_RecordGetInfo: function(var info: BASS_RECORDINFO): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_RecordGetInputName: function(input: LongInt): PAnsiChar; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_RecordSetInput: function(input: LongInt; flags: DWORD; volume: Single): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_RecordGetInput: function(input: LongInt; var volume: Single): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_RecordStart: function(freq, chans, flags: DWORD; proc: RECORDPROC; user: Pointer): HRECORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;

BASS_ChannelBytes2Seconds: function(handle: DWORD; pos: QWORD): Double; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};//external bassdll;
BASS_ChannelSeconds2Bytes: function(handle: DWORD; pos: Double): QWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};//external bassdll;
BASS_ChannelGetDevice: function(handle: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelSetDevice: function(handle, device: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelIsActive: function(handle: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};//external bassdll;
BASS_ChannelGetInfo: function(handle: DWORD; var info: BASS_CHANNELINFO):BOOL;{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};//external bassdll;
BASS_ChannelGetTags: function(handle: HSTREAM; tags: DWORD): PAnsiChar; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelFlags: function(handle, flags, mask: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelUpdate: function(handle, length: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelLock: function(handle: DWORD; lock: BOOL): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelPlay: function(handle: DWORD; restart: BOOL): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelStop: function(handle: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelPause: function(handle: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelSetAttribute: function(handle, attrib: DWORD; value: Single): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelGetAttribute: function(handle, attrib: DWORD; var value: Single): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelSlideAttribute: function(handle, attrib: DWORD; value: Single; time: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelIsSliding: function(handle, attrib: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelSetAttributeEx: function(handle, attrib: DWORD; value: Pointer; size: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelGetAttributeEx: function(handle, attrib: DWORD; value: Pointer; size: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelSet3DAttributes: function(handle: DWORD; mode: LongInt; min, max: Single; iangle, oangle, outvol: LongInt): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelGet3DAttributes: function(handle: DWORD; var mode: DWORD; var min, max: Single; var iangle, oangle, outvol: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelSet3DPosition: function(handle: DWORD; var pos, orient, vel: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelGet3DPosition: function(handle: DWORD; var pos, orient, vel: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelGetLength: function(handle, mode: DWORD): QWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelSetPosition: function(handle: DWORD; pos: QWORD; mode: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelGetPosition: function(handle, mode: DWORD): QWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelGetLevel: function(handle: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelGetLevelEx: function(handle: DWORD; levels: PSingle; length: Single; flags: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelGetData: function(handle: DWORD; buffer: Pointer; length: DWORD): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelSetSync: function(handle: DWORD; type_: DWORD; param: QWORD; proc: SYNCPROC; user: Pointer): HSYNC; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelRemoveSync: function(handle: DWORD; sync: HSYNC): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelSetDSP: function(handle: DWORD; proc: DSPPROC; user: Pointer; priority: LongInt): HDSP; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelRemoveDSP: function(handle: DWORD; dsp: HDSP): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelSetLink: function(handle, chan: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelRemoveLink: function(handle, chan: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelSetFX: function(handle, type_: DWORD; priority: LongInt): HFX; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_ChannelRemoveFX: function(handle: DWORD; fx: HFX): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;

BASS_FXSetParameters: function(handle: HFX; par: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_FXGetParameters: function(handle: HFX; par: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_FXReset: function(handle: HFX): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;
BASS_FXSetPriority: function(handle: HFX; priority: LongInt): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; //external bassdll;


function BASS_SPEAKER_N(n: DWORD): DWORD;
{$IFDEF MSWINDOWS}
function BASS_SetEAXPreset(env: LongInt): BOOL;
{
  This function is defined in the implementation part of this unit.
  It is not part of BASS.DLL but an extra function which makes it easier
  to set the predefined EAX environments.
  env    : a EAX_ENVIRONMENT_xxx constant
}
{$ENDIF}

type
  TBASSModuleHandle = HINST;

function BASS_Load(LibName: PChar ): Boolean;
procedure BASS_UnLoad;
function BASS_IsLoaded:boolean;

implementation
const
  INVALID_MODULEHANDLE_VALUE = TBASSModuleHandle(0);

var
  BASSHandle: TBASSModuleHandle;

function BASS_SPEAKER_N(n: DWORD): DWORD;
begin
  Result := n shl 24;
end;

{$IFDEF MSWINDOWS}
function BASS_SetEAXPreset(env: LongInt): BOOL;
begin
  case (env) of
    EAX_ENVIRONMENT_GENERIC:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_GENERIC, 0.5, 1.493, 0.5);
    EAX_ENVIRONMENT_PADDEDCELL:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PADDEDCELL, 0.25, 0.1, 0);
    EAX_ENVIRONMENT_ROOM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_ROOM, 0.417, 0.4, 0.666);
    EAX_ENVIRONMENT_BATHROOM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_BATHROOM, 0.653, 1.499, 0.166);
    EAX_ENVIRONMENT_LIVINGROOM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_LIVINGROOM, 0.208, 0.478, 0);
    EAX_ENVIRONMENT_STONEROOM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_STONEROOM, 0.5, 2.309, 0.888);
    EAX_ENVIRONMENT_AUDITORIUM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_AUDITORIUM, 0.403, 4.279, 0.5);
    EAX_ENVIRONMENT_CONCERTHALL:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CONCERTHALL, 0.5, 3.961, 0.5);
    EAX_ENVIRONMENT_CAVE:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CAVE, 0.5, 2.886, 1.304);
    EAX_ENVIRONMENT_ARENA:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_ARENA, 0.361, 7.284, 0.332);
    EAX_ENVIRONMENT_HANGAR:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_HANGAR, 0.5, 10.0, 0.3);
    EAX_ENVIRONMENT_CARPETEDHALLWAY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CARPETEDHALLWAY, 0.153, 0.259, 2.0);
    EAX_ENVIRONMENT_HALLWAY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_HALLWAY, 0.361, 1.493, 0);
    EAX_ENVIRONMENT_STONECORRIDOR:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_STONECORRIDOR, 0.444, 2.697, 0.638);
    EAX_ENVIRONMENT_ALLEY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_ALLEY, 0.25, 1.752, 0.776);
    EAX_ENVIRONMENT_FOREST:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_FOREST, 0.111, 3.145, 0.472);
    EAX_ENVIRONMENT_CITY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CITY, 0.111, 2.767, 0.224);
    EAX_ENVIRONMENT_MOUNTAINS:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_MOUNTAINS, 0.194, 7.841, 0.472);
    EAX_ENVIRONMENT_QUARRY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_QUARRY, 1, 1.499, 0.5);
    EAX_ENVIRONMENT_PLAIN:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PLAIN, 0.097, 2.767, 0.224);
    EAX_ENVIRONMENT_PARKINGLOT:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PARKINGLOT, 0.208, 1.652, 1.5);
    EAX_ENVIRONMENT_SEWERPIPE:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_SEWERPIPE, 0.652, 2.886, 0.25);
    EAX_ENVIRONMENT_UNDERWATER:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_UNDERWATER, 1, 1.499, 0);
    EAX_ENVIRONMENT_DRUGGED:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_DRUGGED, 0.875, 8.392, 1.388);
    EAX_ENVIRONMENT_DIZZY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_DIZZY, 0.139, 17.234, 0.666);
    EAX_ENVIRONMENT_PSYCHOTIC:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PSYCHOTIC, 0.486, 7.563, 0.806);
    else
      Result := FALSE;
  end;
end;
{$ENDIF}

function BASS_Load(LibName: PChar ): Boolean;
begin
  Result := False;

  // Make sure the previous library is unloaded
  BASS_Unload;

  // If no library name given, use the default library names
  if LibName = nil then
    LibName := bassdll;

  // Load the library 
  BASSHandle := LoadLibrary(LibName);
  if BASSHandle = INVALID_MODULEHANDLE_VALUE then
    Exit;

  // Get all the function addresses from the library 
  BASS_SetConfig := getprocaddress(basshandle,'BASS_SetConfig');
  BASS_GetConfig := getprocaddress(basshandle,'BASS_GetConfig');
  BASS_SetConfigPtr := getprocaddress(basshandle,'BASS_SetConfigPtr');
  BASS_GetConfigPtr := getprocaddress(basshandle,'BASS_GetConfigPtr');
  BASS_GetVersion := getprocaddress(basshandle,'BASS_GetVersion');
  BASS_ErrorGetCode := getprocaddress(basshandle,'BASS_ErrorGetCode');
  BASS_GetDeviceInfo := getprocaddress(basshandle,'BASS_GetDeviceInfo');
  {$IFDEF MSWINDOWS}
  BASS_Init := getprocaddress(basshandle,'BASS_Init');
  {$ELSE}
  BASS_Init := getprocaddress(basshandle,'BASS_Init_');
 {$ENDIF}
  BASS_SetDevice := getprocaddress(basshandle,'BASS_SetDevice');
  BASS_GetDevice := getprocaddress(basshandle,'BASS_GetDevice');
  BASS_Free := getprocaddress(basshandle,'BASS_Free');
  {$IFDEF MSWINDOWS}
  BASS_GetDSoundObject := getprocaddress(basshandle,'BASS_GetDSoundObject');
  {$ENDIF}
  BASS_GetInfo := getprocaddress(basshandle,'BASS_GetInfo');
  BASS_Update := getprocaddress(basshandle,'BASS_Update');
  BASS_GetCPU := getprocaddress(basshandle,'BASS_GetCPU');
  BASS_Start := getprocaddress(basshandle,'BASS_Start');
  BASS_Stop := getprocaddress(basshandle,'BASS_Stop');
  BASS_Pause := getprocaddress(basshandle,'BASS_Pause');
  BASS_SetVolume := getprocaddress(basshandle,'BASS_SetVolume');
  BASS_GetVolume := getprocaddress(basshandle,'BASS_GetVolume');

  BASS_PluginLoad := getprocaddress(basshandle, 'BASS_PluginLoad');
  BASS_PluginFree := getprocaddress(basshandle, 'BASS_PluginFree');
  BASS_PluginGetInfo := getprocaddress(basshandle, 'BASS_PluginGetInfo');

  BASS_Set3DFactors := getprocaddress(basshandle,'BASS_Set3DFactors');
  BASS_Get3DFactors := getprocaddress(basshandle,'BASS_Get3DFactors');
  BASS_Set3DPosition := getprocaddress(basshandle,'BASS_Set3DPosition');
  BASS_Get3DPosition := getprocaddress(basshandle,'BASS_Get3DPosition');
  BASS_Apply3D := getprocaddress(basshandle,'BASS_Apply3D');
  {$IFDEF MSWINDOWS}
  BASS_SetEAXParameters := getprocaddress(basshandle,'BASS_SetEAXParameters');
  BASS_GetEAXParameters := getprocaddress(basshandle,'BASS_GetEAXParameters');
  {$ENDIF}

  BASS_MusicLoad := getprocaddress(basshandle,'BASS_MusicLoad');
  BASS_MusicFree := getprocaddress(basshandle,'BASS_MusicFree');

  BASS_SampleLoad := getprocaddress(basshandle,'BASS_SampleLoad');
  BASS_SampleCreate := getprocaddress(basshandle,'BASS_SampleCreate');
  BASS_SampleFree := getprocaddress(basshandle,'BASS_SampleFree');
  BASS_SampleSetData := getprocaddress(basshandle,'BASS_SampleSetData');
  BASS_SampleGetData := getprocaddress(basshandle,'BASS_SampleGetData');
  BASS_SampleGetInfo := getprocaddress(basshandle,'BASS_SampleGetInfo');
  BASS_SampleSetInfo := getprocaddress(basshandle,'BASS_SampleSetInfo');
  BASS_SampleGetChannel := getprocaddress(basshandle,'BASS_SampleGetChannel');
  BASS_SampleGetChannels := getprocaddress(basshandle,'BASS_SampleGetChannels');
  BASS_SampleStop := getprocaddress(basshandle,'BASS_SampleStop');

  BASS_StreamCreate := getprocaddress(basshandle,'BASS_StreamCreate');
  BASS_StreamCreateFile := getprocaddress(basshandle,'BASS_StreamCreateFile');
  BASS_StreamCreateURL := getprocaddress(basshandle,'BASS_StreamCreateURL');
  BASS_StreamCreateFileUser := getprocaddress(basshandle,'BASS_StreamCreateFileUser');
  BASS_StreamFree := getprocaddress(basshandle,'BASS_StreamFree');
  BASS_StreamGetFilePosition := getprocaddress(basshandle,'BASS_StreamGetFilePosition');
  BASS_StreamPutData := getprocaddress(basshandle,'BASS_StreamPutData');
  BASS_StreamPutFileData := getprocaddress(basshandle,'BASS_StreamPutFileData');

  BASS_RecordGetDeviceInfo := getprocaddress(basshandle,'BASS_RecordGetDeviceInfo');
  BASS_RecordInit := getprocaddress(basshandle,'BASS_RecordInit');
  BASS_RecordSetDevice := getprocaddress(basshandle,'BASS_RecordSetDevice');
  BASS_RecordGetDevice := getprocaddress(basshandle,'BASS_RecordGetDevice');
  BASS_RecordFree := getprocaddress(basshandle,'BASS_RecordFree');
  BASS_RecordGetInfo := getprocaddress(basshandle,'BASS_RecordGetInfo');
  BASS_RecordGetInputName := getprocaddress(basshandle,'BASS_RecordGetInputName');
  BASS_RecordSetInput := getprocaddress(basshandle,'BASS_RecordSetInput');
  BASS_RecordGetInput := getprocaddress(basshandle,'BASS_RecordGetInput');
  BASS_RecordStart := getprocaddress(basshandle,'BASS_RecordStart');

  BASS_ChannelBytes2Seconds := getprocaddress(basshandle,'BASS_ChannelBytes2Seconds');
  BASS_ChannelSeconds2Bytes := getprocaddress(basshandle,'BASS_ChannelSeconds2Bytes');
  BASS_ChannelGetDevice := getprocaddress(basshandle,'BASS_ChannelGetDevice');
  BASS_ChannelSetDevice := getprocaddress(basshandle,'BASS_ChannelSetDevice');
  BASS_ChannelIsActive := getprocaddress(basshandle,'BASS_ChannelIsActive');
  BASS_ChannelGetInfo := getprocaddress(basshandle,'BASS_ChannelGetInfo');
  BASS_ChannelGetTags := getprocaddress(basshandle,'BASS_ChannelGetTags');
  BASS_ChannelFlags := getprocaddress(basshandle,'BASS_ChannelFlags');
  BASS_ChannelUpdate := getprocaddress(basshandle,'BASS_ChannelUpdate');
  BASS_ChannelLock := getprocaddress(basshandle,'BASS_ChannelLock');
  BASS_ChannelPlay := getprocaddress(basshandle,'BASS_ChannelPlay');
  BASS_ChannelStop := getprocaddress(basshandle,'BASS_ChannelStop');
  BASS_ChannelPause := getprocaddress(basshandle,'BASS_ChannelPause');
  BASS_ChannelSetAttribute := getprocaddress(basshandle,'BASS_ChannelSetAttribute');
  BASS_ChannelGetAttribute := getprocaddress(basshandle,'BASS_ChannelGetAttribute');
  BASS_ChannelSlideAttribute := getprocaddress(basshandle,'BASS_ChannelSlideAttribute');
  BASS_ChannelIsSliding := getprocaddress(basshandle,'BASS_ChannelIsSliding');
  BASS_ChannelSetAttributeEx := getprocaddress(basshandle,'BASS_ChannelSetAttributeEx');
  BASS_ChannelGetAttributeEx := getprocaddress(basshandle,'BASS_ChannelGetAttributeEx');
  BASS_ChannelSet3DAttributes := getprocaddress(basshandle,'BASS_ChannelSet3DAttributes');
  BASS_ChannelGet3DAttributes := getprocaddress(basshandle,'BASS_ChannelGet3DAttributes');
  BASS_ChannelSet3DPosition := getprocaddress(basshandle,'BASS_ChannelSet3DPosition');
  BASS_ChannelGet3DPosition := getprocaddress(basshandle,'BASS_ChannelGet3DPosition');
  BASS_ChannelGetLength :=getprocaddress(basshandle,'BASS_ChannelGetLength');
  BASS_ChannelSetPosition := getprocaddress(basshandle,'BASS_ChannelSetPosition');
  BASS_ChannelGetPosition := getprocaddress(basshandle,'BASS_ChannelGetPosition');
  BASS_ChannelGetLevel := getprocaddress(basshandle,'BASS_ChannelGetLevel');
  BASS_ChannelGetLevelEx := getprocaddress(basshandle,'BASS_ChannelGetLevelEx');
  BASS_ChannelGetData := getprocaddress(basshandle,'BASS_ChannelGetData');
  BASS_ChannelSetSync := getprocaddress(basshandle,'BASS_ChannelSetSync');
  BASS_ChannelRemoveSync := getprocaddress(basshandle,'BASS_ChannelRemoveSync');
  BASS_ChannelSetDSP := getprocaddress(basshandle,'BASS_ChannelSetDSP');
  BASS_ChannelRemoveDSP := getprocaddress(basshandle,'BASS_ChannelRemoveDSP');
  BASS_ChannelSetLink := getprocaddress(basshandle,'BASS_ChannelSetLink');
  BASS_ChannelRemoveLink := getprocaddress(basshandle,'BASS_ChannelRemoveLink');
  BASS_ChannelSetFX := getprocaddress(basshandle,'BASS_ChannelSetFX');
  BASS_ChannelRemoveFX := getprocaddress(basshandle,'BASS_ChannelRemoveFX');

  BASS_FXSetParameters := getprocaddress(basshandle,'BASS_FXSetParameters');
  BASS_FXGetParameters := getprocaddress(basshandle,'BASS_FXGetParameters');
  BASS_FXReset := getprocaddress(basshandle,'BASS_FXReset');
  BASS_FXSetPriority := getprocaddress(basshandle,'BASS_FXSetPriority');

  Result := True;
end;

procedure BASS_Unload;
begin
  // Only free the library if it was already loaded 
  if BASSHandle <> INVALID_MODULEHANDLE_VALUE then
    FreeLibrary(BASSHandle);
  BASSHandle := INVALID_MODULEHANDLE_VALUE;
end;

Function BASS_IsLoaded:boolean;
begin
  result := BASSHandle <> INVALID_MODULEHANDLE_VALUE;
end;


initialization
  BASSHandle := INVALID_MODULEHANDLE_VALUE;

finalization
  // Make sure the library is unloaded 
  BASS_Unload;


end.

