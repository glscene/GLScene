//
// The graphics engine GLXEngine. The unit of GXScene for Delphi
//
(*==========================================================================================
 FMOD Main header file. Copyright (c), FireLight Technologies Pty, Ltd. 1999-2003.
 ===========================================================================================

  NOTE: For the demos to run you must have either fmod.dll (in Windows)
  or libfmod-3.75.so (in Linux) installed.

  In Winapi.Windows, copy the fmod.dll file found in the api directory to either of
  the following locations (in order of preference)
  - your application directory
  - Windows\System (95/98) or WinNT\System32 (NT/2000/XP)

  In Linux, make sure you are signed in as root and copy the libfmod-3.75.so
  file from the api directory to your /usr/lib/ directory.
  Then via a command line, navigate to the /usr/lib/ directory and create
  a symbolic link between libfmod-3.75.so and libfmod.so. This is done with
  the following command (assuming you are in /usr/lib/)...
  ln -s libfmod-3.75.so libfmod.so.

============================================================================================*)

unit GXS.Sounds.FMOD;

interface

uses
  System.SysUtils,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  FMX.Dialogs,
  FMOD.Types;

// ===============================================================================================
// FUNCTION PROTOTYPES
// ===============================================================================================

{ ================================== }
{ Library load/unload functions. }
{ ================================== }

{
  If no library name is passed to FMOD_Load, then the default library name
  used.
}

function FMOD_Load(LibName: PChar = nil): boolean;
procedure FMOD_Unload;

{ ================================== }
{ Initialization / Global functions. }
{ ================================== }

{
  Pre FSOUND_Init functions. These can't be called after FSOUND_Init is
  called (they will fail). They set up FMOD system functionality.
}

var
  FSOUND_SetOutput: function(OutputType: TFSoundOutputTypes): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetDriver: function(Driver: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMixer: function(Mixer: TFSoundMixerTypes): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetBufferSize: function(LenMs: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetHWND: function(Hwnd: THandle): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMinHardwareChannels: function(Min: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMaxHardwareChannels: function(Max: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMemorySystem: function(Pool: Pointer; PoolLen: integer;
    UserAlloc: TFSoundAllocCallback; UserRealloc: TFSoundReallocCallback;
    UserFree: TFSoundFreeCallback): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  {
    Main initialization / closedown functions
    Note : Use FSOUND_INIT_USEDEFAULTMIDISYNTH with FSOUND_Init for software override with MIDI playback.
    : Use FSOUND_INIT_GLOBALFOCUS with FSOUND_Init to make sound audible
    no matter which window is in focus. (FSOUND_OUTPUT_DSOUND only)
  }

var
  FSOUND_Init: function(MixRate: integer; MaxSoftwareChannels: integer;
    Flags: cardinal): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Close: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  {
    Runtime system level functions
  }

var
  FSOUND_Update: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  // This is called to update 3d sound / non-realtime output
  FSOUND_SetSpeakerMode: procedure(SpeakerMode: cardinal);
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetSFXMasterVolume: procedure(Volume: integer);
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPanSeperation: procedure(PanSep: single);
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_File_SetCallbacks: procedure(OpenCallback: TFSoundOpenCallback;
    CloseCallback: TFSoundCloseCallback; ReadCallback: TFSoundReadCallback;
    SeekCallback: TFSoundSeekCallback; TellCallback: TFSoundTellCallback);
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  {
    System information functions
  }

var
  FSOUND_GetError: function: TFModErrors; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FSOUND_GetVersion: function: single; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FSOUND_GetOutput: function: TFSoundOutputTypes;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetOutputHandle: function: Pointer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriver: function: integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FSOUND_GetMixer: function: TFSoundMixerTypes;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetNumDrivers: function: integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FSOUND_GetDriverName: function(Id: integer): PChar;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriverCaps: function(Id: integer; var Caps: cardinal): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FSOUND_GetOutputRate: function: integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FSOUND_GetMaxChannels: function: integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FSOUND_GetMaxSamples: function: integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FSOUND_GetSpeakerMode: function: integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FSOUND_GetSFXMasterVolume: function: integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetNumHWChannels: function(var Num2D: integer; var Num3D: integer;
    var Total: integer): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetChannelsPlaying: function: integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCPUUsage: function: single; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FSOUND_GetMemoryStats: procedure(var CurrentAlloced: cardinal;
    var MaxAlloced: cardinal); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  { =================================== }
  { Sample management / load functions. }
  { =================================== }

  {
    Sample creation and management functions
    Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Sample_Load to load from memory.
    Use FSOUND_LOADRAW      flag with FSOUND_Sample_Load to treat as as raw pcm data.
  }

var
  FSOUND_Sample_Load: function(Index: integer; const NameOrData: PChar;
    Mode: cardinal; Offset: integer; Length: integer): PFSoundSample;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Alloc: function(Index: integer; Length: integer; Mode: cardinal;
    DefFreq: integer; DefVol: integer; DefPan: integer; DefPri: integer)
    : PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Free: procedure(Sptr: PFSoundSample);
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Upload: function(Sptr: PFSoundSample; SrcData: Pointer;
    Mode: cardinal): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Lock: function(Sptr: PFSoundSample; Offset: integer;
    Length: integer; var Ptr1: Pointer; var Ptr2: Pointer; var Len1: cardinal;
    var Len2: cardinal): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Unlock: function(Sptr: PFSoundSample; Ptr1: Pointer;
    Ptr2: Pointer; Len1: cardinal; Len2: cardinal): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  {
    Sample control functions
  }

var
  FSOUND_Sample_SetMode: function(Sptr: PFSoundSample; Mode: cardinal)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetLoopPoints: function(Sptr: PFSoundSample;
    LoopStart, LoopEnd: integer): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FSOUND_Sample_SetDefaults: function(Sptr: PFSoundSample;
    DefFreq, DefVol, DefPan, DefPri: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetDefaultsEx: function(Sptr: PFSoundSample;
    DefFreq, DefVol, DefPan, DefPri, VarFreq, VarVol, VarPan: integer)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetMinMaxDistance: function(Sptr: PFSoundSample;
    Min, Max: single): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetMaxPlaybacks: function(Sptr: PFSoundSample; Max: integer)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  {
    Sample information functions
  }

var
  FSOUND_Sample_Get: function(SampNo: integer): PFSoundSample;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetName: function(Sptr: PFSoundSample): PChar;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetLength: function(Sptr: PFSoundSample): cardinal;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetLoopPoints: function(Sptr: PFSoundSample;
    var LoopStart: integer; var LoopEnd: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetDefaults: function(Sptr: PFSoundSample; var DefFreq: integer;
    var DefVol: integer; var DefPan: integer; var DefPri: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetDefaultsEx: function(Sptr: PFSoundSample;
    var DefFreq: integer; var DefVol: integer; var DefPan: integer;
    var DefPri: integer; var VarFreq: integer; var VarVol: integer; var VarPan)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetMode: function(Sptr: PFSoundSample): cardinal;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetMinMaxDistance: function(Sptr: PFSoundSample;
    var Min: single; var Max: single): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  { ============================ }
  { Channel control functions. }
  { ============================ }

  {
    Playing and stopping sounds.
    Note : Use FSOUND_FREE as the 'channel' variable, to let FMOD pick a free channel for you.
    Use FSOUND_ALL as the 'channel' variable to control ALL channels with one function call!
  }

var
  FSOUND_PlaySound: function(Channel: integer; Sptr: PFSoundSample): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_PlaySoundEx: function(Channel: integer; Sptr: PFSoundSample;
    Dsp: PFSoundDSPUnit; StartPaused: bytebool): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_StopSound: function(Channel: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  {
    Functions to control playback of a channel.
  }

var
  FSOUND_SetFrequency: function(Channel: integer; Freq: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetVolume: function(Channel: integer; Vol: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetVolumeAbsolute: function(Channel: integer; Vol: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPan: function(Channel: integer; Pan: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetSurround: function(Channel: integer; Surround: bytebool): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMute: function(Channel: integer; Mute: bytebool): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPriority: function(Channel: integer; Priority: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetReserved: function(Channel: integer; Reserved: bytebool): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPaused: function(Channel: integer; Paused: bytebool): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetLoopMode: function(Channel: integer; LoopMode: cardinal): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetCurrentPosition: function(Channel: integer; Offset: cardinal)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetAttributes: function(Channel: integer; Pos: PFSoundVector;
    Vel: PFSoundVector): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetMinMaxDistance: function(Channel: integer; Min: single;
    Max: single): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  {
    Channel information functions
  }

var
  FSOUND_IsPlaying: function(Channel: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetFrequency: function(Channel: integer): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetVolume: function(Channel: integer): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetAmplitude: function(Channel: integer): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPan: function(Channel: integer): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSurround: function(Channel: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMute: function(Channel: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPriority: function(Channel: integer): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetReserved: function(Channel: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPaused: function(Channel: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetLoopMode: function(Channel: integer): cardinal;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentPosition: function(Channel: integer): cardinal;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentSample: function(Channel: integer): PFSoundSample;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentLevels: function(Channel: integer; L, R: PSingle): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetNumSubChannels: function(Channel: integer): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSubChannel: function(Channel: integer; SubChannel: integer)
    : integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_GetAttributes: function(Channel: integer; Pos: PFSoundVector;
    Vel: PFSoundVector): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_GetMinMaxDistance: function(Channel: integer; var Min: single;
    var Max: single): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  { =================== }
  { 3D sound functions. }
  { =================== }

  {
    See also 3d sample and channel based functions above.
    Call FSOUND_Update once a frame to process 3d information.
  }

var
  FSOUND_3D_Listener_SetCurrent: procedure(current: integer);
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_Listener_SetAttributes: procedure(Pos: PFSoundVector;
    Vel: PFSoundVector; fx: single; fy: single; fz: single; tx: single;
    ty: single; tz: single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_Listener_GetAttributes: procedure(Pos: PFSoundVector;
    Vel: PFSoundVector; fx: PSingle; fy: PSingle; fz: PSingle; tx: PSingle;
    ty: PSingle; tz: PSingle); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetDopplerFactor: procedure(Scale: single);
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetDistanceFactor: procedure(Scale: single);
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetRolloffFactor: procedure(Scale: single);
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  { =================== }
  { FX functions. }
  { =================== }

  {
    Functions to control DX8 only effects processing.

    - FX enabled samples can only be played once at a time, not multiple times at once.
    - Sounds have to be created with FSOUND_HW2D or FSOUND_HW3D for this to work.
    - FSOUND_INIT_ENABLESYSTEMCHANNELFX can be used to apply hardware effect processing to the
    global mixed output of FMOD's software channels.
    - FSOUND_FX_Enable returns an FX handle that you can use to alter fx parameters.
    - FSOUND_FX_Enable can be called multiple times in a row, even on the same FX type,
    it will return a unique handle for each FX.
    - FSOUND_FX_Enable cannot be called if the sound is playing or locked.
    - Stopping or starting a sound resets all FX and they must be re-enabled each time
    if this happens.
  }

var
  FSOUND_FX_Enable: function(Channel: integer; fx: TFSoundFXModes): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF}; { Set bits to enable following fx }
  FSOUND_FX_Disable: function(Channel: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_FX_SetChorus: function(FXId: integer; WetDryMix, Depth, Feedback,
    Frequency: single; Waveform: integer; Delay: single; Phase: integer)
    : bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetCompressor: function(FXId: integer;
    Gain, Attack, Release, Threshold, Ratio, Predelay: single): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetDistortion: function(FXId: integer;
    Gain, Edge, PostEQCenterFrequency, PostEQBandwidth, PreLowpassCutoff
    : single): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetEcho: function(FXId: integer; WetDryMix, Feedback, LeftDelay,
    RightDelay: single; PanDelay: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetFlanger: function(FXId: integer;
    WetDryMix, Depth, Feedback, Frequency: single; Waveform: integer;
    Delay: single; Phase: integer): bytebool; {$IFDEF UNIX} cdecl
{$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetGargle: function(FXId, RateHz, WaveShape: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetI3DL2Reverb: function(FXId, Room, RoomHF: integer;
    RoomRolloffFactor, DecayTime, DecayHFRatio: single; Reflections: integer;
    ReflectionsDelay: single; Reverb: integer; ReverbDelay, Diffusion, Density,
    HFReference: single): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetParamEQ: function(FXId: integer; Center, Bandwidth, Gain: single)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetWavesReverb: function(FXId: integer;
    InGain, ReverbMix, ReverbTime, HighFreqRTRatio: single): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  { ========================= }
  { File Streaming functions. }
  { ========================= }

  {
    Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Stream_Open to stream from memory.
    Use FSOUND_LOADRAW      flag with FSOUND_Stream_Open to treat stream as raw pcm data.
    Use FSOUND_MPEGACCURATE flag with FSOUND_Stream_Open to open mpegs in 'accurate mode' for settime/gettime/getlengthms.
    Use FSOUND_FREE as the 'channel' variable, to let FMOD pick a free channel for you.
  }

var
  // call this before opening streams, not after
  FSOUND_Stream_SetBufferSize: function(Ms: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_Open: function(const name_or_data: PChar; Mode: cardinal;
    Offset: integer; Length: integer): PFSoundStream;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Create: function(Callback: TFSoundStreamCallback;
    Length: integer; Mode: cardinal; SampleRate: integer; UserData: integer)
    : PFSoundStream;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Close: function(Stream: PFSoundStream): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_Play: function(Channel: integer; Stream: PFSoundStream)
    : integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_PlayEx: function(Channel: integer; Stream: PFSoundStream;
    Dsp: PFSoundDSPUnit; StartPaused: bytebool): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Stop: function(Stream: PFSoundStream): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetPosition: function(Stream: PFSoundStream; Position: cardinal)
    : bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetPosition: function(Stream: PFSoundStream): cardinal;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetTime: function(Stream: PFSoundStream; Ms: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetTime: function(Stream: PFSoundStream): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetLength: function(Stream: PFSoundStream): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetLengthMs: function(Stream: PFSoundStream): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetMode: function(Stream: PFSoundStream; Mode: integer)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetMode: function(Stream: PFSoundStream): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetLoopPoints: function(Stream: PFSoundStream;
    LoopStartPCM, LoopEndPCM: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetLoopCount: function(Stream: PFSoundStream; Count: integer)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetOpenState: function(Stream: PFSoundStream): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetSample: function(Stream: PFSoundStream): PFSoundSample;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  { Every stream contains a sample to play back on }
  FSOUND_Stream_CreateDSP: function(Stream: PFSoundStream;
    Callback: TFSoundDSPCallback; Priority: integer; Param: integer)
    : PFSoundDSPUnit;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetEndCallback: function(Stream: PFSoundStream;
    Callback: TFSoundStreamCallback; UserData: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetSyncCallback: function(Stream: PFSoundStream;
    Callback: TFSoundStreamCallback; UserData: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_AddSyncPoint: function(Stream: PFSoundStream;
    PCMOffset: cardinal; Name: PChar): PFSyncPoint;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_DeleteSyncPoint: function(Point: PFSyncPoint): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetNumSyncPoints: function(Stream: PFSoundStream): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetSyncPoint: function(Stream: PFSoundStream; Index: integer)
    : PFSyncPoint; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetSyncPointInfo: function(Point: PFSyncPoint;
    var PCMOffset: cardinal): integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};

  FSOUND_Stream_SetSubStream: function(Stream: PFSoundStream; Index: integer)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetNumSubStreams: function(Stream: PFSoundStream): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetSubStreamSentence: function(Stream: PFSoundStream;
    var sentencelist: cardinal; numitems: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_GetNumTagFields: function(Stream: PFSoundStream;
    var Num: integer): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetTagField: function(Stream: PFSoundStream; Num: integer;
    var _Type: TFSoundTagFieldType; var Name: PChar; var Value: Pointer;
    var Length: integer): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FSOUND_Stream_FindTagField: function(Stream: PFSoundStream;
    _Type: TFSoundTagFieldType; Name: PChar; var Value: Pointer;
    var Length: integer): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};

  FSOUND_Stream_Net_SetProxy: function(Proxy: PChar): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_GetLastServerStatus: function: PChar;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_SetBufferProperties: function(BufferSize: integer;
    PreBuffer_Percent: integer; ReBuffer_Percent: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_GetBufferProperties: function(var BufferSize: integer;
    var PreBuffer_Percent: integer; var ReBuffer_Percent: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_SetMetadataCallback: function(Stream: PFSoundStream;
    Callback: TFMetaDataCallback; UserData: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_GetStatus: function(Stream: PFSoundStream;
    var Status: TFSoundStreamNetStatus; var BufferPercentUsed: integer;
    var BitRate: integer; var Flags: cardinal): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  { =================== }
  { CD audio functions. }
  { =================== }

  {
    Note : 0 = default cdrom.  Otherwise specify the drive letter, for example. 'D'.
  }

var
  FSOUND_CD_Play: function(Drive: byte; Track: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetPlayMode: procedure(Drive: byte; Mode: integer);
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_Stop: function(Drive: byte): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetPaused: function(Drive: byte; Paused: bytebool): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetVolume: function(Drive: byte; Volume: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetTrackTime: function(Drive: byte; Ms: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_OpenTray: function(Drive: byte; Open: byte): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FSOUND_CD_GetPaused: function(Drive: byte): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrack: function(Drive: byte): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetNumTracks: function(Drive: byte): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetVolume: function(Drive: byte): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrackLength: function(Drive: byte; Track: integer): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrackTime: function(Drive: byte): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  { ============== }
  { DSP functions. }
  { ============== }

  {
    DSP Unit control and information functions.
  }

var
  FSOUND_DSP_Create: function(Callback: TFSoundDSPCallback; Priority: integer;
    Param: integer): PFSoundDSPUnit;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_Free: procedure(DSPUnit: PFSoundDSPUnit);
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_SetPriority: procedure(DSPUnit: PFSoundDSPUnit; Priority: integer);
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetPriority: function(DSPUnit: PFSoundDSPUnit): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_SetActive: procedure(DSPUnit: PFSoundDSPUnit; Active: bytebool);
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetActive: function(DSPUnit: PFSoundDSPUnit): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  {
    Functions to get hold of FSOUND 'system DSP unit' handles.
  }

var
  FSOUND_DSP_GetClearUnit: function: PFSoundDSPUnit;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetSFXUnit: function: PFSoundDSPUnit;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetMusicUnit: function: PFSoundDSPUnit;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetClipAndCopyUnit: function: PFSoundDSPUnit;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetFFTUnit: function: PFSoundDSPUnit;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  {
    Miscellaneous DSP functions
    Note for the spectrum analysis function to work, you have to enable the FFT DSP unit with
    the following code FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE);
    It is off by default to save cpu usage.
  }

var
  FSOUND_DSP_MixBuffers: function(DestBuffer: Pointer; SrcBuffer: Pointer;
    Len: integer; Freq: integer; Vol: integer; Pan: integer; Mode: cardinal)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_ClearMixBuffer: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FSOUND_DSP_GetBufferLength: function: integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF}; { Length of each DSP update }
  FSOUND_DSP_GetBufferLengthTotal: function: integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  { Total buffer length due to FSOUND_SetBufferSize }
  FSOUND_DSP_GetSpectrum: function: PSingle;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  { Array of 512 floats - call FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE)) for this to work. }

  { ========================================================================== }
  { Reverb functions. (eax2/eax3 reverb)  (NOT SUPPORTED IN LINUX/CE) }
  { ========================================================================== }

  {
    See structures above for definitions and information on the reverb parameters.
  }

var
  FSOUND_Reverb_SetProperties: function(const Prop: TFSoundReverbProperties)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_GetProperties: function(var Prop: TFSoundReverbProperties)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_SetChannelProperties: function(Channel: integer;
    var Prop: TFSoundReverbChannelProperties): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_GetChannelProperties: function(Channel: integer;
    var Prop: TFSoundReverbChannelProperties): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  { ================================================ }
  { Recording functions  (NOT SUPPORTED IN LINUX/MAC) }
  { ================================================ }

  {
    Recording initialization functions
  }

var
  FSOUND_Record_SetDriver: function(OutputType: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetNumDrivers: function: integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetDriverName: function(Id: integer): PChar;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetDriver: function: integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  {
    Recording functionality. Only one recording session will work at a time.
  }

var
  FSOUND_Record_StartSample: function(Sptr: PFSoundSample; Loop: bytebool)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_Stop: function: bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FSOUND_Record_GetPosition: function: integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  { ============================================================================================= }
  { FMUSIC API (MOD,S3M,XM,IT,MIDI PLAYBACK) }
  { ============================================================================================= }

  {
    Song management / playback functions.
  }

var
  FMUSIC_LoadSong: function(const Name: PChar): PFMusicModule;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_LoadSongEx: function(name_or_data: Pointer; Offset: integer;
    Length: integer; Mode: cardinal; var SampleList: integer;
    SampleListNum: integer): PFMusicModule; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FMUSIC_GetOpenState: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_FreeSong: function(Module: PFMusicModule): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_PlaySong: function(Module: PFMusicModule): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_StopSong: function(Module: PFMusicModule): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_StopAllSongs: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FMUSIC_SetZxxCallback: function(Module: PFMusicModule;
    Callback: TFMusicCallback): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FMUSIC_SetRowCallback: function(Module: PFMusicModule;
    Callback: TFMusicCallback; RowStep: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetOrderCallback: function(Module: PFMusicModule;
    Callback: TFMusicCallback; OrderStep: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetInstCallback: function(Module: PFMusicModule;
    Callback: TFMusicCallback; Instrument: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FMUSIC_SetSample: function(Module: PFMusicModule; SampNo: integer;
    Sptr: PFSoundSample): bytebool; {$IFDEF UNIX} cdecl {$ELSE} stdcall
{$ENDIF};
  FMUSIC_SetUserData: function(Module: PFMusicModule; UserData: integer)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_OptimizeChannels: function(Module: PFMusicModule; MaxChannels: integer;
    MinVolume: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  {
    Runtime song functions.
  }

var
  FMUSIC_SetReverb: function(Reverb: bytebool): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetLooping: function(Module: PFMusicModule; Looping: bytebool)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetOrder: function(Module: PFMusicModule; Order: integer): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetPaused: function(Module: PFMusicModule; Pause: bytebool): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetMasterVolume: function(Module: PFMusicModule; Volume: integer)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetMasterSpeed: function(Module: PFMusicModule; Speed: single)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetPanSeperation: function(Module: PFMusicModule; PanSep: single)
    : bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  {
    Static song information functions.
  }

var
  FMUSIC_GetName: function(Module: PFMusicModule): PChar;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetType: function(Module: PFMusicModule): TFMusicTypes;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumOrders: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumPatterns: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumInstruments: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumSamples: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumChannels: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetSample: function(Module: PFMusicModule; SampNo: integer)
    : PFSoundSample;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPatternLength: function(Module: PFMusicModule;
    OrderNo: integer): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  {
    Runtime song information.
  }

var
  FMUSIC_IsFinished: function(Module: PFMusicModule): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_IsPlaying: function(Module: PFMusicModule): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetMasterVolume: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetGlobalVolume: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetOrder: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPattern: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetSpeed: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetBPM: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetRow: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPaused: function(Module: PFMusicModule): bytebool;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetTime: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetRealChannel: function(Module: PFMusicModule;
    ModChannel: integer): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetUserData: function(Module: PFMusicModule): integer;
{$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------
implementation
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------

{$IFDEF UNIX}
{$IFDEF LINUX}
uses
  Libc;
{$ENDIF}
uses
  dynlibs;
{$ENDIF}

const
{$IFDEF LINUX}
  FMOD_DLL = 'libfmod.so';
{$ELSE}
{$IFDEF MSWINDOWS}
  FMOD_DLL = 'fmod.dll';
{$ENDIF}
{$IFDEF DARWIN}
  FMOD_DLL = 'fmod.dylib';
{$ENDIF}
{$ENDIF}

type
{$IFDEF UNIX}
  TFMODModuleHandle = TLibHandle;
{$ELSE}
  TFMODModuleHandle = HINST;
{$ENDIF}

const
  INVALID_MODULEHANDLE_VALUE = TFMODModuleHandle(0);

var
  FMODHandle: TFMODModuleHandle;

function GetAddress(Handle: TFMODModuleHandle; FuncName: PChar): Pointer;
begin
  Result := GetProcAddress(Handle, FuncName);
  if not Assigned(Result) then
    ShowMessage(Format('Failed to find "%s" in "%s"', [FuncName, FMOD_DLL]));
end;

function FMOD_Load(LibName: PChar): boolean;
begin
  Result := False;

  { Make sure the previous library is unloaded }
  FMOD_Unload;

  { If no library name given, use the default library names }
  if LibName = nil then
    LibName := FMOD_DLL;

  { Load the library }
  FMODHandle := INVALID_MODULEHANDLE_VALUE;
  FMODHandle := LoadLibrary(LibName);

  if FMODHandle = INVALID_MODULEHANDLE_VALUE then
    Exit;

  { Get all the function addresses from the library }
{$IFDEF MSWINDOWS}
  FMUSIC_FreeSong := GetAddress(FMODHandle, '_FMUSIC_FreeSong@4');
  FMUSIC_GetBPM := GetAddress(FMODHandle, '_FMUSIC_GetBPM@4');
  FMUSIC_GetGlobalVolume := GetAddress(FMODHandle, '_FMUSIC_GetGlobalVolume@4');
  FMUSIC_GetMasterVolume := GetAddress(FMODHandle, '_FMUSIC_GetMasterVolume@4');
  FMUSIC_GetName := GetAddress(FMODHandle, '_FMUSIC_GetName@4');
  FMUSIC_GetNumChannels := GetAddress(FMODHandle, '_FMUSIC_GetNumChannels@4');
  FMUSIC_GetNumInstruments := GetAddress(FMODHandle,
    '_FMUSIC_GetNumInstruments@4');
  FMUSIC_GetNumOrders := GetAddress(FMODHandle, '_FMUSIC_GetNumOrders@4');
  FMUSIC_GetNumPatterns := GetAddress(FMODHandle, '_FMUSIC_GetNumPatterns@4');
  FMUSIC_GetNumSamples := GetAddress(FMODHandle, '_FMUSIC_GetNumSamples@4');
  FMUSIC_GetOpenState := GetAddress(FMODHandle, '_FMUSIC_GetOpenState@4');
  FMUSIC_GetOrder := GetAddress(FMODHandle, '_FMUSIC_GetOrder@4');
  FMUSIC_GetPattern := GetAddress(FMODHandle, '_FMUSIC_GetPattern@4');
  FMUSIC_GetPatternLength := GetAddress(FMODHandle,
    '_FMUSIC_GetPatternLength@8');
  FMUSIC_GetPaused := GetAddress(FMODHandle, '_FMUSIC_GetPaused@4');
  FMUSIC_GetRealChannel := GetAddress(FMODHandle, '_FMUSIC_GetRealChannel@8');
  FMUSIC_GetRow := GetAddress(FMODHandle, '_FMUSIC_GetRow@4');
  FMUSIC_GetSample := GetAddress(FMODHandle, '_FMUSIC_GetSample@8');
  FMUSIC_GetSpeed := GetAddress(FMODHandle, '_FMUSIC_GetSpeed@4');
  FMUSIC_GetTime := GetAddress(FMODHandle, '_FMUSIC_GetTime@4');
  FMUSIC_GetType := GetAddress(FMODHandle, '_FMUSIC_GetType@4');
  FMUSIC_GetUserData := GetAddress(FMODHandle, '_FMUSIC_GetUserData@4');
  FMUSIC_IsFinished := GetAddress(FMODHandle, '_FMUSIC_IsFinished@4');
  FMUSIC_IsPlaying := GetAddress(FMODHandle, '_FMUSIC_IsPlaying@4');
  FMUSIC_LoadSong := GetAddress(FMODHandle, '_FMUSIC_LoadSong@4');
  FMUSIC_LoadSongEx := GetAddress(FMODHandle, '_FMUSIC_LoadSongEx@24');
  FMUSIC_OptimizeChannels := GetAddress(FMODHandle,
    '_FMUSIC_OptimizeChannels@12');
  FMUSIC_PlaySong := GetAddress(FMODHandle, '_FMUSIC_PlaySong@4');
  FMUSIC_SetInstCallback := GetAddress(FMODHandle,
    '_FMUSIC_SetInstCallback@12');
  FMUSIC_SetLooping := GetAddress(FMODHandle, '_FMUSIC_SetLooping@8');
  FMUSIC_SetMasterSpeed := GetAddress(FMODHandle, '_FMUSIC_SetMasterSpeed@8');
  FMUSIC_SetMasterVolume := GetAddress(FMODHandle, '_FMUSIC_SetMasterVolume@8');
  FMUSIC_SetOrder := GetAddress(FMODHandle, '_FMUSIC_SetOrder@8');
  FMUSIC_SetOrderCallback := GetAddress(FMODHandle,
    '_FMUSIC_SetOrderCallback@12');
  FMUSIC_SetPanSeperation := GetAddress(FMODHandle,
    '_FMUSIC_SetPanSeperation@8');
  FMUSIC_SetPaused := GetAddress(FMODHandle, '_FMUSIC_SetPaused@8');
  FMUSIC_SetReverb := GetAddress(FMODHandle, '_FMUSIC_SetReverb@4');
  FMUSIC_SetRowCallback := GetAddress(FMODHandle, '_FMUSIC_SetRowCallback@12');
  FMUSIC_SetSample := GetAddress(FMODHandle, '_FMUSIC_SetSample@12');
  FMUSIC_SetUserData := GetAddress(FMODHandle, '_FMUSIC_SetUserData@8');
  FMUSIC_SetZxxCallback := GetAddress(FMODHandle, '_FMUSIC_SetZxxCallback@8');
  FMUSIC_StopAllSongs := GetAddress(FMODHandle, '_FMUSIC_StopAllSongs@0');
  FMUSIC_StopSong := GetAddress(FMODHandle, '_FMUSIC_StopSong@4');
  FSOUND_3D_GetAttributes := GetAddress(FMODHandle,
    '_FSOUND_3D_GetAttributes@12');
  FSOUND_3D_GetMinMaxDistance := GetAddress(FMODHandle,
    '_FSOUND_3D_GetMinMaxDistance@12');
  FSOUND_3D_Listener_GetAttributes := GetAddress(FMODHandle,
    '_FSOUND_3D_Listener_GetAttributes@32');
  FSOUND_3D_Listener_SetAttributes := GetAddress(FMODHandle,
    '_FSOUND_3D_Listener_SetAttributes@32');
  FSOUND_3D_Listener_SetCurrent := GetAddress(FMODHandle,
    '_FSOUND_3D_Listener_SetCurrent@8');
  FSOUND_3D_SetAttributes := GetAddress(FMODHandle,
    '_FSOUND_3D_SetAttributes@12');
  FSOUND_3D_SetDistanceFactor := GetAddress(FMODHandle,
    '_FSOUND_3D_SetDistanceFactor@4');
  FSOUND_3D_SetDopplerFactor := GetAddress(FMODHandle,
    '_FSOUND_3D_SetDopplerFactor@4');
  FSOUND_3D_SetMinMaxDistance := GetAddress(FMODHandle,
    '_FSOUND_3D_SetMinMaxDistance@12');
  FSOUND_3D_SetRolloffFactor := GetAddress(FMODHandle,
    '_FSOUND_3D_SetRolloffFactor@4');
  // FSOUND_CD_Eject                  := GetAddress(FMODHandle,'_FSOUND_CD_Eject@4');
  FSOUND_CD_GetNumTracks := GetAddress(FMODHandle, '_FSOUND_CD_GetNumTracks@4');
  FSOUND_CD_GetPaused := GetAddress(FMODHandle, '_FSOUND_CD_GetPaused@4');
  FSOUND_CD_GetTrack := GetAddress(FMODHandle, '_FSOUND_CD_GetTrack@4');
  FSOUND_CD_GetTrackLength := GetAddress(FMODHandle,
    '_FSOUND_CD_GetTrackLength@8');
  FSOUND_CD_GetTrackTime := GetAddress(FMODHandle, '_FSOUND_CD_GetTrackTime@4');
  FSOUND_CD_GetVolume := GetAddress(FMODHandle, '_FSOUND_CD_GetVolume@4');
  FSOUND_CD_OpenTray := GetAddress(FMODHandle, '_FSOUND_CD_OpenTray@8');
  FSOUND_CD_Play := GetAddress(FMODHandle, '_FSOUND_CD_Play@8');
  FSOUND_CD_SetPaused := GetAddress(FMODHandle, '_FSOUND_CD_SetPaused@8');
  FSOUND_CD_SetPlayMode := GetAddress(FMODHandle, '_FSOUND_CD_SetPlayMode@8');
  FSOUND_CD_SetTrackTime := GetAddress(FMODHandle, '_FSOUND_CD_SetTrackTime@8');
  FSOUND_CD_SetVolume := GetAddress(FMODHandle, '_FSOUND_CD_SetVolume@8');
  FSOUND_CD_Stop := GetAddress(FMODHandle, '_FSOUND_CD_Stop@4');
  FSOUND_Close := GetAddress(FMODHandle, '_FSOUND_Close@0');
  FSOUND_DSP_ClearMixBuffer := GetAddress(FMODHandle,
    '_FSOUND_DSP_ClearMixBuffer@0');
  FSOUND_DSP_Create := GetAddress(FMODHandle, '_FSOUND_DSP_Create@12');
  FSOUND_DSP_Free := GetAddress(FMODHandle, '_FSOUND_DSP_Free@4');
  FSOUND_DSP_GetActive := GetAddress(FMODHandle, '_FSOUND_DSP_GetActive@4');
  FSOUND_DSP_GetBufferLength := GetAddress(FMODHandle,
    '_FSOUND_DSP_GetBufferLength@0');
  FSOUND_DSP_GetBufferLengthTotal := GetAddress(FMODHandle,
    '_FSOUND_DSP_GetBufferLengthTotal@0');
  FSOUND_DSP_GetClearUnit := GetAddress(FMODHandle,
    '_FSOUND_DSP_GetClearUnit@0');
  FSOUND_DSP_GetClipAndCopyUnit := GetAddress(FMODHandle,
    '_FSOUND_DSP_GetClipAndCopyUnit@0');
  FSOUND_DSP_GetFFTUnit := GetAddress(FMODHandle, '_FSOUND_DSP_GetFFTUnit@0');
  FSOUND_DSP_GetMusicUnit := GetAddress(FMODHandle,
    '_FSOUND_DSP_GetMusicUnit@0');
  FSOUND_DSP_GetPriority := GetAddress(FMODHandle, '_FSOUND_DSP_GetPriority@4');
  FSOUND_DSP_GetSFXUnit := GetAddress(FMODHandle, '_FSOUND_DSP_GetSFXUnit@0');
  FSOUND_DSP_GetSpectrum := GetAddress(FMODHandle, '_FSOUND_DSP_GetSpectrum@0');
  FSOUND_DSP_MixBuffers := GetAddress(FMODHandle, '_FSOUND_DSP_MixBuffers@28');
  FSOUND_DSP_SetActive := GetAddress(FMODHandle, '_FSOUND_DSP_SetActive@8');
  FSOUND_DSP_SetPriority := GetAddress(FMODHandle, '_FSOUND_DSP_SetPriority@8');
  FSOUND_File_SetCallbacks := GetAddress(FMODHandle,
    '_FSOUND_File_SetCallbacks@20');
  FSOUND_FX_Disable := GetAddress(FMODHandle, '_FSOUND_FX_Disable@4');
  FSOUND_FX_Enable := GetAddress(FMODHandle, '_FSOUND_FX_Enable@8');
  FSOUND_FX_SetChorus := GetAddress(FMODHandle, '_FSOUND_FX_SetChorus@32');
  FSOUND_FX_SetCompressor := GetAddress(FMODHandle,
    '_FSOUND_FX_SetCompressor@28');
  FSOUND_FX_SetDistortion := GetAddress(FMODHandle,
    '_FSOUND_FX_SetDistortion@24');
  FSOUND_FX_SetEcho := GetAddress(FMODHandle, '_FSOUND_FX_SetEcho@24');
  FSOUND_FX_SetFlanger := GetAddress(FMODHandle, '_FSOUND_FX_SetFlanger@32');
  FSOUND_FX_SetGargle := GetAddress(FMODHandle, '_FSOUND_FX_SetGargle@12');
  FSOUND_FX_SetI3DL2Reverb := GetAddress(FMODHandle,
    '_FSOUND_FX_SetI3DL2Reverb@52');
  FSOUND_FX_SetParamEQ := GetAddress(FMODHandle, '_FSOUND_FX_SetParamEQ@16');
  FSOUND_FX_SetWavesReverb := GetAddress(FMODHandle,
    '_FSOUND_FX_SetWavesReverb@20');
  FSOUND_GetAmplitude := GetAddress(FMODHandle, '_FSOUND_GetAmplitude@4');
  FSOUND_GetChannelsPlaying := GetAddress(FMODHandle,
    '_FSOUND_GetChannelsPlaying@0');
  FSOUND_GetCPUUsage := GetAddress(FMODHandle, '_FSOUND_GetCPUUsage@0');
  FSOUND_GetCurrentLevels := GetAddress(FMODHandle,
    '_FSOUND_GetCurrentLevels@12');
  FSOUND_GetCurrentPosition := GetAddress(FMODHandle,
    '_FSOUND_GetCurrentPosition@4');
  FSOUND_GetCurrentSample := GetAddress(FMODHandle,
    '_FSOUND_GetCurrentSample@4');
  FSOUND_GetDriver := GetAddress(FMODHandle, '_FSOUND_GetDriver@0');
  FSOUND_GetDriverCaps := GetAddress(FMODHandle, '_FSOUND_GetDriverCaps@8');
  FSOUND_GetDriverName := GetAddress(FMODHandle, '_FSOUND_GetDriverName@4');
  FSOUND_GetError := GetAddress(FMODHandle, '_FSOUND_GetError@0');
  FSOUND_GetFrequency := GetAddress(FMODHandle, '_FSOUND_GetFrequency@4');
  FSOUND_GetLoopMode := GetAddress(FMODHandle, '_FSOUND_GetLoopMode@4');
  FSOUND_GetMaxChannels := GetAddress(FMODHandle, '_FSOUND_GetMaxChannels@0');
  FSOUND_GetMaxSamples := GetAddress(FMODHandle, '_FSOUND_GetMaxSamples@0');
  FSOUND_GetMemoryStats := GetAddress(FMODHandle, '_FSOUND_GetMemoryStats@8');
  FSOUND_GetMixer := GetAddress(FMODHandle, '_FSOUND_GetMixer@0');
  FSOUND_GetMute := GetAddress(FMODHandle, '_FSOUND_GetMute@4');
  FSOUND_GetNumDrivers := GetAddress(FMODHandle, '_FSOUND_GetNumDrivers@0');
  // FSOUND_GetNumHardwareChannels                  := GetAddress(FMODHandle,'_FSOUND_GetNumHardwareChannels@0');
  FSOUND_GetNumHWChannels := GetAddress(FMODHandle,
    '_FSOUND_GetNumHWChannels@12');
  FSOUND_GetNumSubChannels := GetAddress(FMODHandle,
    '_FSOUND_GetNumSubChannels@4');
  FSOUND_GetOutput := GetAddress(FMODHandle, '_FSOUND_GetOutput@0');
  FSOUND_GetOutputHandle := GetAddress(FMODHandle, '_FSOUND_GetOutputHandle@0');
  FSOUND_GetOutputRate := GetAddress(FMODHandle, '_FSOUND_GetOutputRate@0');
  FSOUND_GetPan := GetAddress(FMODHandle, '_FSOUND_GetPan@4');
  FSOUND_GetPaused := GetAddress(FMODHandle, '_FSOUND_GetPaused@4');
  FSOUND_GetPriority := GetAddress(FMODHandle, '_FSOUND_GetPriority@4');
  FSOUND_GetReserved := GetAddress(FMODHandle, '_FSOUND_GetReserved@4');
  FSOUND_GetSFXMasterVolume := GetAddress(FMODHandle,
    '_FSOUND_GetSFXMasterVolume@0');
  FSOUND_GetSpeakerMode := GetAddress(FMODHandle, '_FSOUND_GetSpeakerMode@0');
  FSOUND_GetSubChannel := GetAddress(FMODHandle, '_FSOUND_GetSubChannel@8');
  FSOUND_GetSurround := GetAddress(FMODHandle, '_FSOUND_GetSurround@4');
  FSOUND_GetVersion := GetAddress(FMODHandle, '_FSOUND_GetVersion@0');
  FSOUND_GetVolume := GetAddress(FMODHandle, '_FSOUND_GetVolume@4');
  FSOUND_Init := GetAddress(FMODHandle, '_FSOUND_Init@12');
  FSOUND_IsPlaying := GetAddress(FMODHandle, '_FSOUND_IsPlaying@4');
  FSOUND_PlaySound := GetAddress(FMODHandle, '_FSOUND_PlaySound@8');
  FSOUND_PlaySoundEx := GetAddress(FMODHandle, '_FSOUND_PlaySoundEx@16');
  FSOUND_Record_GetDriver := GetAddress(FMODHandle,
    '_FSOUND_Record_GetDriver@0');
  FSOUND_Record_GetDriverName := GetAddress(FMODHandle,
    '_FSOUND_Record_GetDriverName@4');
  FSOUND_Record_GetNumDrivers := GetAddress(FMODHandle,
    '_FSOUND_Record_GetNumDrivers@0');
  FSOUND_Record_GetPosition := GetAddress(FMODHandle,
    '_FSOUND_Record_GetPosition@0');
  FSOUND_Record_SetDriver := GetAddress(FMODHandle,
    '_FSOUND_Record_SetDriver@4');
  FSOUND_Record_StartSample := GetAddress(FMODHandle,
    '_FSOUND_Record_StartSample@8');
  FSOUND_Record_Stop := GetAddress(FMODHandle, '_FSOUND_Record_Stop@0');
  FSOUND_Reverb_GetChannelProperties :=
    GetAddress(FMODHandle, '_FSOUND_Reverb_GetChannelProperties@8');
  FSOUND_Reverb_GetProperties := GetAddress(FMODHandle,
    '_FSOUND_Reverb_GetProperties@4');
  FSOUND_Reverb_SetChannelProperties :=
    GetAddress(FMODHandle, '_FSOUND_Reverb_SetChannelProperties@8');
  FSOUND_Reverb_SetProperties := GetAddress(FMODHandle,
    '_FSOUND_Reverb_SetProperties@4');
  FSOUND_Sample_Alloc := GetAddress(FMODHandle, '_FSOUND_Sample_Alloc@28');
  FSOUND_Sample_Free := GetAddress(FMODHandle, '_FSOUND_Sample_Free@4');
  FSOUND_Sample_Get := GetAddress(FMODHandle, '_FSOUND_Sample_Get@4');
  FSOUND_Sample_GetDefaults := GetAddress(FMODHandle,
    '_FSOUND_Sample_GetDefaults@20');
  FSOUND_Sample_GetDefaultsEx := GetAddress(FMODHandle,
    '_FSOUND_Sample_GetDefaultsEx@32');
  FSOUND_Sample_GetLength := GetAddress(FMODHandle,
    '_FSOUND_Sample_GetLength@4');
  FSOUND_Sample_GetLoopPoints := GetAddress(FMODHandle,
    '_FSOUND_Sample_GetLoopPoints@12');
  FSOUND_Sample_GetMinMaxDistance := GetAddress(FMODHandle,
    '_FSOUND_Sample_GetMinMaxDistance@12');
  FSOUND_Sample_GetMode := GetAddress(FMODHandle, '_FSOUND_Sample_GetMode@4');
  FSOUND_Sample_GetName := GetAddress(FMODHandle, '_FSOUND_Sample_GetName@4');
  FSOUND_Sample_Load := GetAddress(FMODHandle, '_FSOUND_Sample_Load@20');
  FSOUND_Sample_Lock := GetAddress(FMODHandle, '_FSOUND_Sample_Lock@28');
  FSOUND_Sample_SetDefaults := GetAddress(FMODHandle,
    '_FSOUND_Sample_SetDefaults@20');
  FSOUND_Sample_SetDefaultsEx := GetAddress(FMODHandle,
    '_FSOUND_Sample_SetDefaultsEx@32');
  FSOUND_Sample_SetLoopPoints := GetAddress(FMODHandle,
    '_FSOUND_Sample_SetLoopPoints@12');
  FSOUND_Sample_SetMaxPlaybacks := GetAddress(FMODHandle,
    '_FSOUND_Sample_SetMaxPlaybacks@8');
  FSOUND_Sample_SetMinMaxDistance := GetAddress(FMODHandle,
    '_FSOUND_Sample_SetMinMaxDistance@12');
  FSOUND_Sample_SetMode := GetAddress(FMODHandle, '_FSOUND_Sample_SetMode@8');
  FSOUND_Sample_Unlock := GetAddress(FMODHandle, '_FSOUND_Sample_Unlock@20');
  FSOUND_Sample_Upload := GetAddress(FMODHandle, '_FSOUND_Sample_Upload@12');
  FSOUND_SetBufferSize := GetAddress(FMODHandle, '_FSOUND_SetBufferSize@4');
  FSOUND_SetCurrentPosition := GetAddress(FMODHandle,
    '_FSOUND_SetCurrentPosition@8');
  FSOUND_SetDriver := GetAddress(FMODHandle, '_FSOUND_SetDriver@4');
  FSOUND_SetFrequency := GetAddress(FMODHandle, '_FSOUND_SetFrequency@8');
  // FSOUND_SetFrequencyEx                  := GetAddress(FMODHandle,'_FSOUND_SetFrequencyEx@8');
  FSOUND_SetHWND := GetAddress(FMODHandle, '_FSOUND_SetHWND@4');
  FSOUND_SetLoopMode := GetAddress(FMODHandle, '_FSOUND_SetLoopMode@8');
  FSOUND_SetMaxHardwareChannels := GetAddress(FMODHandle,
    '_FSOUND_SetMaxHardwareChannels@4');
  FSOUND_SetMemorySystem := GetAddress(FMODHandle,
    '_FSOUND_SetMemorySystem@20');
  FSOUND_SetMinHardwareChannels := GetAddress(FMODHandle,
    '_FSOUND_SetMinHardwareChannels@4');
  FSOUND_SetMixer := GetAddress(FMODHandle, '_FSOUND_SetMixer@4');
  FSOUND_SetMute := GetAddress(FMODHandle, '_FSOUND_SetMute@8');
  FSOUND_SetOutput := GetAddress(FMODHandle, '_FSOUND_SetOutput@4');
  FSOUND_SetPan := GetAddress(FMODHandle, '_FSOUND_SetPan@8');
  FSOUND_SetPanSeperation := GetAddress(FMODHandle,
    '_FSOUND_SetPanSeperation@4');
  FSOUND_SetPaused := GetAddress(FMODHandle, '_FSOUND_SetPaused@8');
  FSOUND_SetPriority := GetAddress(FMODHandle, '_FSOUND_SetPriority@8');
  FSOUND_SetReserved := GetAddress(FMODHandle, '_FSOUND_SetReserved@8');
  FSOUND_SetSFXMasterVolume := GetAddress(FMODHandle,
    '_FSOUND_SetSFXMasterVolume@4');
  FSOUND_SetSpeakerMode := GetAddress(FMODHandle, '_FSOUND_SetSpeakerMode@4');
  FSOUND_SetSurround := GetAddress(FMODHandle, '_FSOUND_SetSurround@8');
  FSOUND_SetVolume := GetAddress(FMODHandle, '_FSOUND_SetVolume@8');
  FSOUND_SetVolumeAbsolute := GetAddress(FMODHandle,
    '_FSOUND_SetVolumeAbsolute@8');
  FSOUND_StopSound := GetAddress(FMODHandle, '_FSOUND_StopSound@4');
  FSOUND_Stream_AddSyncPoint := GetAddress(FMODHandle,
    '_FSOUND_Stream_AddSyncPoint@12');
  FSOUND_Stream_Close := GetAddress(FMODHandle, '_FSOUND_Stream_Close@4');
  FSOUND_Stream_Create := GetAddress(FMODHandle, '_FSOUND_Stream_Create@20');
  FSOUND_Stream_CreateDSP := GetAddress(FMODHandle,
    '_FSOUND_Stream_CreateDSP@16');
  FSOUND_Stream_DeleteSyncPoint := GetAddress(FMODHandle,
    '_FSOUND_Stream_DeleteSyncPoint@4');
  FSOUND_Stream_FindTagField := GetAddress(FMODHandle,
    '_FSOUND_Stream_FindTagField@20');
  FSOUND_Stream_GetLength := GetAddress(FMODHandle,
    '_FSOUND_Stream_GetLength@4');
  FSOUND_Stream_GetLengthMs := GetAddress(FMODHandle,
    '_FSOUND_Stream_GetLengthMs@4');
  FSOUND_Stream_GetMode := GetAddress(FMODHandle, '_FSOUND_Stream_GetMode@4');
  FSOUND_Stream_GetNumSubStreams := GetAddress(FMODHandle,
    '_FSOUND_Stream_GetNumSubStreams@4');
  FSOUND_Stream_GetNumSyncPoints := GetAddress(FMODHandle,
    '_FSOUND_Stream_GetNumSyncPoints@4');
  FSOUND_Stream_GetNumTagFields := GetAddress(FMODHandle,
    '_FSOUND_Stream_GetNumTagFields@8');
  FSOUND_Stream_GetOpenState := GetAddress(FMODHandle,
    '_FSOUND_Stream_GetOpenState@4');
  FSOUND_Stream_GetPosition := GetAddress(FMODHandle,
    '_FSOUND_Stream_GetPosition@4');
  FSOUND_Stream_GetSample := GetAddress(FMODHandle,
    '_FSOUND_Stream_GetSample@4');
  FSOUND_Stream_GetSyncPoint := GetAddress(FMODHandle,
    '_FSOUND_Stream_GetSyncPoint@8');
  FSOUND_Stream_GetSyncPointInfo := GetAddress(FMODHandle,
    '_FSOUND_Stream_GetSyncPointInfo@8');
  FSOUND_Stream_GetTagField := GetAddress(FMODHandle,
    '_FSOUND_Stream_GetTagField@24');
  FSOUND_Stream_GetTime := GetAddress(FMODHandle, '_FSOUND_Stream_GetTime@4');
  FSOUND_Stream_Net_GetBufferProperties :=
    GetAddress(FMODHandle, '_FSOUND_Stream_Net_GetBufferProperties@12');
  FSOUND_Stream_Net_GetLastServerStatus :=
    GetAddress(FMODHandle, '_FSOUND_Stream_Net_GetLastServerStatus@0');
  FSOUND_Stream_Net_GetStatus := GetAddress(FMODHandle,
    '_FSOUND_Stream_Net_GetStatus@20');
  FSOUND_Stream_Net_SetBufferProperties :=
    GetAddress(FMODHandle, '_FSOUND_Stream_Net_SetBufferProperties@12');
  FSOUND_Stream_Net_SetMetadataCallback :=
    GetAddress(FMODHandle, '_FSOUND_Stream_Net_SetMetadataCallback@12');
  // FSOUND_Stream_Net_SetNetDataCallback                  := GetAddress(FMODHandle,'_FSOUND_Stream_Net_SetNetDataCallback@8');
  FSOUND_Stream_Net_SetProxy := GetAddress(FMODHandle,
    '_FSOUND_Stream_Net_SetProxy@4');
  // FSOUND_Stream_Net_SetTimeout                  := GetAddress(FMODHandle,'_FSOUND_Stream_Net_SetTimeout@4');
  FSOUND_Stream_Open := GetAddress(FMODHandle, '_FSOUND_Stream_Open@16');
  FSOUND_Stream_Play := GetAddress(FMODHandle, '_FSOUND_Stream_Play@8');
  FSOUND_Stream_PlayEx := GetAddress(FMODHandle, '_FSOUND_Stream_PlayEx@16');
  FSOUND_Stream_SetBufferSize := GetAddress(FMODHandle,
    '_FSOUND_Stream_SetBufferSize@4');
  FSOUND_Stream_SetEndCallback := GetAddress(FMODHandle,
    '_FSOUND_Stream_SetEndCallback@12');
  FSOUND_Stream_SetLoopCount := GetAddress(FMODHandle,
    '_FSOUND_Stream_SetLoopCount@8');
  FSOUND_Stream_SetLoopPoints := GetAddress(FMODHandle,
    '_FSOUND_Stream_SetLoopPoints@12');
  FSOUND_Stream_SetMode := GetAddress(FMODHandle, '_FSOUND_Stream_SetMode@8');
  // FSOUND_Stream_SetPCM                  := GetAddress(FMODHandle,'_FSOUND_Stream_SetPCM@8');
  FSOUND_Stream_SetPosition := GetAddress(FMODHandle,
    '_FSOUND_Stream_SetPosition@8');
  FSOUND_Stream_SetSubStream := GetAddress(FMODHandle,
    '_FSOUND_Stream_SetSubStream@8');
  FSOUND_Stream_SetSubStreamSentence :=
    GetAddress(FMODHandle, '_FSOUND_Stream_SetSubStreamSentence@12');
  FSOUND_Stream_SetSyncCallback := GetAddress(FMODHandle,
    '_FSOUND_Stream_SetSyncCallback@12');
  FSOUND_Stream_SetTime := GetAddress(FMODHandle, '_FSOUND_Stream_SetTime@8');
  FSOUND_Stream_Stop := GetAddress(FMODHandle, '_FSOUND_Stream_Stop@4');
  FSOUND_Update := GetAddress(FMODHandle, '_FSOUND_Update@0');
{$ELSE}
  FSOUND_SetOutput := GetAddress(FMODHandle, 'FSOUND_SetOutput');
  FSOUND_SetDriver := GetAddress(FMODHandle, 'FSOUND_SetDriver');
  FSOUND_SetMixer := GetAddress(FMODHandle, 'FSOUND_SetMixer');
  FSOUND_SetBufferSize := GetAddress(FMODHandle, 'FSOUND_SetBufferSize');
  FSOUND_SetHWND := GetAddress(FMODHandle, 'FSOUND_SetHWND');
  FSOUND_SetMinHardwareChannels := GetAddress(FMODHandle,
    'FSOUND_SetMinHardwareChannels');
  FSOUND_SetMaxHardwareChannels := GetAddress(FMODHandle,
    'FSOUND_SetMaxHardwareChannels');
  FSOUND_SetMemorySystem := GetAddress(FMODHandle, 'FSOUND_SetMemorySystem');
  FSOUND_Init := GetAddress(FMODHandle, 'FSOUND_Init');
  FSOUND_Close := GetAddress(FMODHandle, 'FSOUND_Close');
  FSOUND_Update := GetAddress(FMODHandle, 'FSOUND_Update');
  FSOUND_SetSpeakerMode := GetAddress(FMODHandle, 'FSOUND_SetSpeakerMode');
  FSOUND_SetSFXMasterVolume := GetAddress(FMODHandle,
    'FSOUND_SetSFXMasterVolume');
  FSOUND_SetPanSeperation := GetAddress(FMODHandle, 'FSOUND_SetPanSeperation');
  FSOUND_GetError := GetAddress(FMODHandle, 'FSOUND_GetError');
  FSOUND_GetVersion := GetAddress(FMODHandle, 'FSOUND_GetVersion');
  FSOUND_GetOutput := GetAddress(FMODHandle, 'FSOUND_GetOutput');
  FSOUND_GetOutputHandle := GetAddress(FMODHandle, 'FSOUND_GetOutputHandle');
  FSOUND_GetDriver := GetAddress(FMODHandle, 'FSOUND_GetDriver');
  FSOUND_GetMixer := GetAddress(FMODHandle, 'FSOUND_GetMixer');
  FSOUND_GetNumDrivers := GetAddress(FMODHandle, 'FSOUND_GetNumDrivers');
  FSOUND_GetDriverName := GetAddress(FMODHandle, 'FSOUND_GetDriverName');
  FSOUND_GetDriverCaps := GetAddress(FMODHandle, 'FSOUND_GetDriverCaps');
  FSOUND_GetOutputRate := GetAddress(FMODHandle, 'FSOUND_GetOutputRate');
  FSOUND_GetMaxChannels := GetAddress(FMODHandle, 'FSOUND_GetMaxChannels');
  FSOUND_GetMaxSamples := GetAddress(FMODHandle, 'FSOUND_GetMaxSamples');
  FSOUND_GetSpeakerMode := GetAddress(FMODHandle, 'FSOUND_GetSpeakerMode');
  FSOUND_GetSFXMasterVolume := GetAddress(FMODHandle,
    'FSOUND_GetSFXMasterVolume');
  FSOUND_GetNumHWChannels := GetAddress(FMODHandle, 'FSOUND_GetNumHWChannels');
  FSOUND_GetChannelsPlaying := GetAddress(FMODHandle,
    'FSOUND_GetChannelsPlaying');
  FSOUND_GetCPUUsage := GetAddress(FMODHandle, 'FSOUND_GetCPUUsage');
  FSOUND_GetMemoryStats := GetAddress(FMODHandle, 'FSOUND_GetMemoryStats');
  FSOUND_Sample_Load := GetAddress(FMODHandle, 'FSOUND_Sample_Load');
  FSOUND_Sample_Alloc := GetAddress(FMODHandle, 'FSOUND_Sample_Alloc');
  FSOUND_Sample_Free := GetAddress(FMODHandle, 'FSOUND_Sample_Free');
  FSOUND_Sample_Upload := GetAddress(FMODHandle, 'FSOUND_Sample_Upload');
  FSOUND_Sample_Lock := GetAddress(FMODHandle, 'FSOUND_Sample_Lock');
  FSOUND_Sample_Unlock := GetAddress(FMODHandle, 'FSOUND_Sample_Unlock');
  FSOUND_Sample_SetMode := GetAddress(FMODHandle, 'FSOUND_Sample_SetMode');
  FSOUND_Sample_SetLoopPoints := GetAddress(FMODHandle,
    'FSOUND_Sample_SetLoopPoints');
  FSOUND_Sample_SetDefaults := GetAddress(FMODHandle,
    'FSOUND_Sample_SetDefaults');
  FSOUND_Sample_SetDefaultsEx := GetAddress(FMODHandle,
    'FSOUND_Sample_SetDefaultsEx');
  FSOUND_Sample_SetMinMaxDistance := GetAddress(FMODHandle,
    'FSOUND_Sample_SetMinMaxDistance');
  FSOUND_Sample_SetMaxPlaybacks := GetAddress(FMODHandle,
    'FSOUND_Sample_SetMaxPlaybacks');
  FSOUND_Sample_Get := GetAddress(FMODHandle, 'FSOUND_Sample_Get');
  FSOUND_Sample_GetName := GetAddress(FMODHandle, 'FSOUND_Sample_GetName');
  FSOUND_Sample_GetLength := GetAddress(FMODHandle, 'FSOUND_Sample_GetLength');
  FSOUND_Sample_GetLoopPoints := GetAddress(FMODHandle,
    'FSOUND_Sample_GetLoopPoints');
  FSOUND_Sample_GetDefaults := GetAddress(FMODHandle,
    'FSOUND_Sample_GetDefaults');
  FSOUND_Sample_GetDefaultsEx := GetAddress(FMODHandle,
    'FSOUND_Sample_GetDefaultsEx');
  FSOUND_Sample_GetMode := GetAddress(FMODHandle, 'FSOUND_Sample_GetMode');
  FSOUND_Sample_GetMinMaxDistance := GetAddress(FMODHandle,
    'FSOUND_Sample_GetMinMaxDistance');
  FSOUND_PlaySound := GetAddress(FMODHandle, 'FSOUND_PlaySound');
  FSOUND_PlaySoundEx := GetAddress(FMODHandle, 'FSOUND_PlaySoundEx');
  FSOUND_StopSound := GetAddress(FMODHandle, 'FSOUND_StopSound');
  FSOUND_SetFrequency := GetAddress(FMODHandle, 'FSOUND_SetFrequency');
  FSOUND_SetVolume := GetAddress(FMODHandle, 'FSOUND_SetVolume');
  FSOUND_SetVolumeAbsolute := GetAddress(FMODHandle,
    'FSOUND_SetVolumeAbsolute');
  FSOUND_SetPan := GetAddress(FMODHandle, 'FSOUND_SetPan');
  FSOUND_SetSurround := GetAddress(FMODHandle, 'FSOUND_SetSurround');
  FSOUND_SetMute := GetAddress(FMODHandle, 'FSOUND_SetMute');
  FSOUND_SetPriority := GetAddress(FMODHandle, 'FSOUND_SetPriority');
  FSOUND_SetReserved := GetAddress(FMODHandle, 'FSOUND_SetReserved');
  FSOUND_SetPaused := GetAddress(FMODHandle, 'FSOUND_SetPaused');
  FSOUND_SetLoopMode := GetAddress(FMODHandle, 'FSOUND_SetLoopMode');
  FSOUND_SetCurrentPosition := GetAddress(FMODHandle,
    'FSOUND_SetCurrentPosition');
  FSOUND_3D_SetAttributes := GetAddress(FMODHandle, 'FSOUND_3D_SetAttributes');
  FSOUND_3D_SetMinMaxDistance := GetAddress(FMODHandle,
    'FSOUND_3D_SetMinMaxDistance');
  FSOUND_IsPlaying := GetAddress(FMODHandle, 'FSOUND_IsPlaying');
  FSOUND_GetFrequency := GetAddress(FMODHandle, 'FSOUND_GetFrequency');
  FSOUND_GetVolume := GetAddress(FMODHandle, 'FSOUND_GetVolume');
  FSOUND_GetAmplitude := GetAddress(FMODHandle, 'FSOUND_GetAmplitude');
  FSOUND_GetPan := GetAddress(FMODHandle, 'FSOUND_GetPan');
  FSOUND_GetSurround := GetAddress(FMODHandle, 'FSOUND_GetSurround');
  FSOUND_GetMute := GetAddress(FMODHandle, 'FSOUND_GetMute');
  FSOUND_GetPriority := GetAddress(FMODHandle, 'FSOUND_GetPriority');
  FSOUND_GetReserved := GetAddress(FMODHandle, 'FSOUND_GetReserved');
  FSOUND_GetPaused := GetAddress(FMODHandle, 'FSOUND_GetPaused');
  FSOUND_GetLoopMode := GetAddress(FMODHandle, 'FSOUND_GetLoopMode');
  FSOUND_GetCurrentPosition := GetAddress(FMODHandle,
    'FSOUND_GetCurrentPosition');
  FSOUND_GetCurrentSample := GetAddress(FMODHandle, 'FSOUND_GetCurrentSample');
  FSOUND_GetCurrentLevels := GetAddress(FMODHandle, 'FSOUND_GetCurrentLevels');
  FSOUND_GetNumSubChannels := GetAddress(FMODHandle,
    'FSOUND_GetNumSubChannels');
  FSOUND_GetSubChannel := GetAddress(FMODHandle, 'FSOUND_GetSubChannel');
  FSOUND_3D_GetAttributes := GetAddress(FMODHandle, 'FSOUND_3D_GetAttributes');
  FSOUND_3D_GetMinMaxDistance := GetAddress(FMODHandle,
    'FSOUND_3D_GetMinMaxDistance');
  FSOUND_3D_Listener_SetCurrent := GetAddress(FMODHandle,
    'FSOUND_3D_Listener_SetCurrent');
  FSOUND_3D_Listener_SetAttributes := GetAddress(FMODHandle,
    'FSOUND_3D_Listener_SetAttributes');
  FSOUND_3D_Listener_GetAttributes := GetAddress(FMODHandle,
    'FSOUND_3D_Listener_GetAttributes');
  FSOUND_3D_SetDopplerFactor := GetAddress(FMODHandle,
    'FSOUND_3D_SetDopplerFactor');
  FSOUND_3D_SetDistanceFactor := GetAddress(FMODHandle,
    'FSOUND_3D_SetDistanceFactor');
  FSOUND_3D_SetRolloffFactor := GetAddress(FMODHandle,
    'FSOUND_3D_SetRolloffFactor');
  FSOUND_FX_Enable := GetAddress(FMODHandle, 'FSOUND_FX_Enable');
  FSOUND_FX_SetChorus := GetAddress(FMODHandle, 'FSOUND_FX_SetChorus');
  FSOUND_FX_SetCompressor := GetAddress(FMODHandle, 'FSOUND_FX_SetCompressor');
  FSOUND_FX_SetDistortion := GetAddress(FMODHandle, 'FSOUND_FX_SetDistortion');
  FSOUND_FX_SetEcho := GetAddress(FMODHandle, 'FSOUND_FX_SetEcho');
  FSOUND_FX_SetFlanger := GetAddress(FMODHandle, 'FSOUND_FX_SetFlanger');
  FSOUND_FX_SetGargle := GetAddress(FMODHandle, 'FSOUND_FX_SetGargle');
  FSOUND_FX_SetI3DL2Reverb := GetAddress(FMODHandle,
    'FSOUND_FX_SetI3DL2Reverb');
  FSOUND_FX_SetParamEQ := GetAddress(FMODHandle, 'FSOUND_FX_SetParamEQ');
  FSOUND_FX_SetWavesReverb := GetAddress(FMODHandle,
    'FSOUND_FX_SetWavesReverb');
  FSOUND_Stream_Open := GetAddress(FMODHandle, 'FSOUND_Stream_Open');
  FSOUND_Stream_Create := GetAddress(FMODHandle, 'FSOUND_Stream_Create');
  FSOUND_Stream_Close := GetAddress(FMODHandle, 'FSOUND_Stream_Close');
  FSOUND_Stream_Play := GetAddress(FMODHandle, 'FSOUND_Stream_Play');
  FSOUND_Stream_PlayEx := GetAddress(FMODHandle, 'FSOUND_Stream_PlayEx');
  FSOUND_Stream_Stop := GetAddress(FMODHandle, 'FSOUND_Stream_Stop');
  FSOUND_Stream_SetEndCallback := GetAddress(FMODHandle,
    'FSOUND_Stream_SetEndCallback');
  FSOUND_Stream_SetSyncCallback := GetAddress(FMODHandle,
    'FSOUND_Stream_SetSyncCallback');
  FSOUND_Stream_GetSample := GetAddress(FMODHandle, 'FSOUND_Stream_GetSample');
  FSOUND_Stream_CreateDSP := GetAddress(FMODHandle, 'FSOUND_Stream_CreateDSP');
  FSOUND_Stream_SetBufferSize := GetAddress(FMODHandle,
    'FSOUND_Stream_SetBufferSize');
  FSOUND_Stream_SetPosition := GetAddress(FMODHandle,
    'FSOUND_Stream_SetPosition');
  FSOUND_Stream_GetPosition := GetAddress(FMODHandle,
    'FSOUND_Stream_GetPosition');
  FSOUND_Stream_SetTime := GetAddress(FMODHandle, 'FSOUND_Stream_SetTime');
  FSOUND_Stream_GetTime := GetAddress(FMODHandle, 'FSOUND_Stream_GetTime');
  FSOUND_Stream_GetLength := GetAddress(FMODHandle, 'FSOUND_Stream_GetLength');
  FSOUND_Stream_GetLengthMs := GetAddress(FMODHandle,
    'FSOUND_Stream_GetLengthMs');
  FSOUND_Stream_SetMode := GetAddress(FMODHandle, 'FSOUND_Stream_SetMode');
  FSOUND_Stream_GetMode := GetAddress(FMODHandle, 'FSOUND_Stream_GetMode');
  FSOUND_Stream_SetLoopPoints := GetAddress(FMODHandle,
    'FSOUND_Stream_SetLoopPoints');
  FSOUND_Stream_SetLoopCount := GetAddress(FMODHandle,
    'FSOUND_Stream_SetLoopCount');
  FSOUND_Stream_GetOpenState := GetAddress(FMODHandle,
    'FSOUND_Stream_GetOpenState');
  FSOUND_Stream_AddSyncPoint := GetAddress(FMODHandle,
    'FSOUND_Stream_AddSyncPoint');
  FSOUND_Stream_DeleteSyncPoint := GetAddress(FMODHandle,
    'FSOUND_Stream_DeleteSyncPoint');
  FSOUND_Stream_GetNumSyncPoints := GetAddress(FMODHandle,
    'FSOUND_Stream_GetNumSyncPoints');
  FSOUND_Stream_GetSyncPoint := GetAddress(FMODHandle,
    'FSOUND_Stream_GetSyncPoint');
  FSOUND_Stream_GetSyncPointInfo := GetAddress(FMODHandle,
    'FSOUND_Stream_GetSyncPointInfo');
  FSOUND_Stream_SetSubStream := GetAddress(FMODHandle,
    'FSOUND_Stream_SetSubStream');
  FSOUND_Stream_GetNumSubStreams := GetAddress(FMODHandle,
    'FSOUND_Stream_GetNumSubStreams');
  FSOUND_Stream_SetSubStreamSentence :=
    GetAddress(FMODHandle, 'FSOUND_Stream_SetSubStreamSentence');
  FSOUND_Stream_GetNumTagFields := GetAddress(FMODHandle,
    'FSOUND_Stream_GetNumTagFields');
  FSOUND_Stream_GetTagField := GetAddress(FMODHandle,
    'FSOUND_Stream_GetTagField');
  FSOUND_Stream_FindTagField := GetAddress(FMODHandle,
    'FSOUND_Stream_FindTagField');
  FSOUND_Stream_Net_SetProxy := GetAddress(FMODHandle,
    'FSOUND_Stream_Net_SetProxy');
  FSOUND_Stream_Net_GetLastServerStatus :=
    GetAddress(FMODHandle, 'FSOUND_Stream_Net_GetLastServerStatus');
  FSOUND_Stream_Net_SetBufferProperties :=
    GetAddress(FMODHandle, 'FSOUND_Stream_Net_SetBufferProperties');
  FSOUND_Stream_Net_GetBufferProperties :=
    GetAddress(FMODHandle, 'FSOUND_Stream_Net_GetBufferProperties');
  FSOUND_Stream_Net_SetMetadataCallback :=
    GetAddress(FMODHandle, 'FSOUND_Stream_Net_SetMetadataCallback');
  FSOUND_Stream_Net_GetStatus := GetAddress(FMODHandle,
    'FSOUND_Stream_Net_GetStatus');
  FSOUND_CD_Play := GetAddress(FMODHandle, 'FSOUND_CD_Play');
  FSOUND_CD_SetPlayMode := GetAddress(FMODHandle, 'FSOUND_CD_SetPlayMode');
  FSOUND_CD_Stop := GetAddress(FMODHandle, 'FSOUND_CD_Stop');
  FSOUND_CD_SetPaused := GetAddress(FMODHandle, 'FSOUND_CD_SetPaused');
  FSOUND_CD_SetVolume := GetAddress(FMODHandle, 'FSOUND_CD_SetVolume');
  FSOUND_CD_SetTrackTime := GetAddress(FMODHandle, 'FSOUND_CD_SetTrackTime');
  FSOUND_CD_OpenTray := GetAddress(FMODHandle, 'FSOUND_CD_OpenTray');
  FSOUND_CD_GetPaused := GetAddress(FMODHandle, 'FSOUND_CD_GetPaused');
  FSOUND_CD_GetTrack := GetAddress(FMODHandle, 'FSOUND_CD_GetTrack');
  FSOUND_CD_GetNumTracks := GetAddress(FMODHandle, 'FSOUND_CD_GetNumTracks');
  FSOUND_CD_GetVolume := GetAddress(FMODHandle, 'FSOUND_CD_GetVolume');
  FSOUND_CD_GetTrackLength := GetAddress(FMODHandle,
    'FSOUND_CD_GetTrackLength');
  FSOUND_CD_GetTrackTime := GetAddress(FMODHandle, 'FSOUND_CD_GetTrackTime');
  FSOUND_DSP_Create := GetAddress(FMODHandle, 'FSOUND_DSP_Create');
  FSOUND_DSP_Free := GetAddress(FMODHandle, 'FSOUND_DSP_Free');
  FSOUND_DSP_SetPriority := GetAddress(FMODHandle, 'FSOUND_DSP_SetPriority');
  FSOUND_DSP_GetPriority := GetAddress(FMODHandle, 'FSOUND_DSP_GetPriority');
  FSOUND_DSP_SetActive := GetAddress(FMODHandle, 'FSOUND_DSP_SetActive');
  FSOUND_DSP_GetActive := GetAddress(FMODHandle, 'FSOUND_DSP_GetActive');
  FSOUND_DSP_GetClearUnit := GetAddress(FMODHandle, 'FSOUND_DSP_GetClearUnit');
  FSOUND_DSP_GetSFXUnit := GetAddress(FMODHandle, 'FSOUND_DSP_GetSFXUnit');
  FSOUND_DSP_GetMusicUnit := GetAddress(FMODHandle, 'FSOUND_DSP_GetMusicUnit');
  FSOUND_DSP_GetClipAndCopyUnit := GetAddress(FMODHandle,
    'FSOUND_DSP_GetClipAndCopyUnit');
  FSOUND_DSP_GetFFTUnit := GetAddress(FMODHandle, 'FSOUND_DSP_GetFFTUnit');
  FSOUND_DSP_MixBuffers := GetAddress(FMODHandle, 'FSOUND_DSP_MixBuffers');
  FSOUND_DSP_ClearMixBuffer := GetAddress(FMODHandle,
    'FSOUND_DSP_ClearMixBuffer');
  FSOUND_DSP_GetBufferLength := GetAddress(FMODHandle,
    'FSOUND_DSP_GetBufferLength');
  FSOUND_DSP_GetBufferLengthTotal := GetAddress(FMODHandle,
    'FSOUND_DSP_GetBufferLengthTotal');
  FSOUND_DSP_GetSpectrum := GetAddress(FMODHandle, 'FSOUND_DSP_GetSpectrum');
  FSOUND_Reverb_SetProperties := GetAddress(FMODHandle,
    'FSOUND_Reverb_SetProperties');
  FSOUND_Reverb_GetProperties := GetAddress(FMODHandle,
    'FSOUND_Reverb_GetProperties');
  FSOUND_Reverb_SetChannelProperties :=
    GetAddress(FMODHandle, 'FSOUND_Reverb_SetChannelProperties');
  FSOUND_Reverb_GetChannelProperties :=
    GetAddress(FMODHandle, 'FSOUND_Reverb_GetChannelProperties');
  FSOUND_Record_SetDriver := GetAddress(FMODHandle, 'FSOUND_Record_SetDriver');
  FSOUND_Record_GetNumDrivers := GetAddress(FMODHandle,
    'FSOUND_Record_GetNumDrivers');
  FSOUND_Record_GetDriverName := GetAddress(FMODHandle,
    'FSOUND_Record_GetDriverName');
  FSOUND_Record_GetDriver := GetAddress(FMODHandle, 'FSOUND_Record_GetDriver');
  FSOUND_Record_StartSample := GetAddress(FMODHandle,
    'FSOUND_Record_StartSample');
  FSOUND_Record_Stop := GetAddress(FMODHandle, 'FSOUND_Record_Stop');
  FSOUND_Record_GetPosition := GetAddress(FMODHandle,
    'FSOUND_Record_GetPosition');
  FSOUND_File_SetCallbacks := GetAddress(FMODHandle,
    'FSOUND_File_SetCallbacks');
  FMUSIC_LoadSong := GetAddress(FMODHandle, 'FMUSIC_LoadSong');
  FMUSIC_LoadSongEx := GetAddress(FMODHandle, 'FMUSIC_LoadSongEx');
  FMUSIC_GetOpenState := GetAddress(FMODHandle, 'FMUSIC_GetOpenState');
  FMUSIC_FreeSong := GetAddress(FMODHandle, 'FMUSIC_FreeSong');
  FMUSIC_PlaySong := GetAddress(FMODHandle, 'FMUSIC_PlaySong');
  FMUSIC_StopSong := GetAddress(FMODHandle, 'FMUSIC_StopSong');
  FMUSIC_StopAllSongs := GetAddress(FMODHandle, 'FMUSIC_StopAllSongs');
  FMUSIC_SetZxxCallback := GetAddress(FMODHandle, 'FMUSIC_SetZxxCallback');
  FMUSIC_SetRowCallback := GetAddress(FMODHandle, 'FMUSIC_SetRowCallback');
  FMUSIC_SetOrderCallback := GetAddress(FMODHandle, 'FMUSIC_SetOrderCallback');
  FMUSIC_SetInstCallback := GetAddress(FMODHandle, 'FMUSIC_SetInstCallback');
  FMUSIC_SetSample := GetAddress(FMODHandle, 'FMUSIC_SetSample');
  FMUSIC_SetUserData := GetAddress(FMODHandle, 'FMUSIC_SetUserData');
  FMUSIC_OptimizeChannels := GetAddress(FMODHandle, 'FMUSIC_OptimizeChannels');
  FMUSIC_SetReverb := GetAddress(FMODHandle, 'FMUSIC_SetReverb');
  FMUSIC_SetLooping := GetAddress(FMODHandle, 'FMUSIC_SetLooping');
  FMUSIC_SetOrder := GetAddress(FMODHandle, 'FMUSIC_SetOrder');
  FMUSIC_SetPaused := GetAddress(FMODHandle, 'FMUSIC_SetPaused');
  FMUSIC_SetMasterVolume := GetAddress(FMODHandle, 'FMUSIC_SetMasterVolume');
  FMUSIC_SetMasterSpeed := GetAddress(FMODHandle, 'FMUSIC_SetMasterSpeed');
  FMUSIC_SetPanSeperation := GetAddress(FMODHandle, 'FMUSIC_SetPanSeperation');
  FMUSIC_GetName := GetAddress(FMODHandle, 'FMUSIC_GetName');
  FMUSIC_GetType := GetAddress(FMODHandle, 'FMUSIC_GetType');
  FMUSIC_GetNumOrders := GetAddress(FMODHandle, 'FMUSIC_GetNumOrders');
  FMUSIC_GetNumPatterns := GetAddress(FMODHandle, 'FMUSIC_GetNumPatterns');
  FMUSIC_GetNumInstruments := GetAddress(FMODHandle,
    'FMUSIC_GetNumInstruments');
  FMUSIC_GetNumSamples := GetAddress(FMODHandle, 'FMUSIC_GetNumSamples');
  FMUSIC_GetNumChannels := GetAddress(FMODHandle, 'FMUSIC_GetNumChannels');
  FMUSIC_GetSample := GetAddress(FMODHandle, 'FMUSIC_GetSample');
  FMUSIC_GetPatternLength := GetAddress(FMODHandle, 'FMUSIC_GetPatternLength');
  FMUSIC_IsFinished := GetAddress(FMODHandle, 'FMUSIC_IsFinished');
  FMUSIC_IsPlaying := GetAddress(FMODHandle, 'FMUSIC_IsPlaying');
  FMUSIC_GetMasterVolume := GetAddress(FMODHandle, 'FMUSIC_GetMasterVolume');
  FMUSIC_GetGlobalVolume := GetAddress(FMODHandle, 'FMUSIC_GetGlobalVolume');
  FMUSIC_GetOrder := GetAddress(FMODHandle, 'FMUSIC_GetOrder');
  FMUSIC_GetPattern := GetAddress(FMODHandle, 'FMUSIC_GetPattern');
  FMUSIC_GetSpeed := GetAddress(FMODHandle, 'FMUSIC_GetSpeed');
  FMUSIC_GetBPM := GetAddress(FMODHandle, 'FMUSIC_GetBPM');
  FMUSIC_GetRow := GetAddress(FMODHandle, 'FMUSIC_GetRow');
  FMUSIC_GetPaused := GetAddress(FMODHandle, 'FMUSIC_GetPaused');
  FMUSIC_GetTime := GetAddress(FMODHandle, 'FMUSIC_GetTime');
  FMUSIC_GetRealChannel := GetAddress(FMODHandle, 'FMUSIC_GetRealChannel');
  FMUSIC_GetUserData := GetAddress(FMODHandle, 'FMUSIC_GetUserData');
{$ENDIF}
  Result := True;
end;

procedure FMOD_Unload;
begin
  { Only free the library if it was already loaded }
  if FMODHandle <> INVALID_MODULEHANDLE_VALUE then
    FreeLibrary(FMODHandle);

  FMODHandle := INVALID_MODULEHANDLE_VALUE;
end;

var
  Saved8087CW: word;

initialization

{ Save the current FPU state and then disable FPU exceptions }
Saved8087CW := Default8087CW;
Set8087CW($133F); { Disable all fpu exceptions }

finalization

{ Make sure the library is unloaded }
FMOD_Unload;
{ Reset the FPU to the previous state }
Set8087CW(Saved8087CW);

end.
