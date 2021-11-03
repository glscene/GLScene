//
// The graphics rendering engine GLScene http://glscene.org
//
unit Sounds.FMODImport;

(*============================================================================================== 
  FMOD Main header file. Copyright (c), Firelight Technologies Pty, Ltd. 1999-2004.               
  ============================================================================================== 

  NOTE: For the demos to run you must have either fmod.dll (in Windows)
  or libfmod-3.75.so (in Linux) installed.

  In Windows, copy the fmod.dll file found in the api directory to either of
  the following locations (in order of preference)
  - your application directory
  - Windows\System (95/98) or WinNT\System32 (NT/2000/XP)

  In Linux, make sure you are signed in as root and copy the libfmod-3.75.so
  file from the api directory to your /usr/lib/ directory.
  Then via a command line, navigate to the /usr/lib/ directory and create
  a symbolic link between libfmod-3.75.so and libfmod.so. This is done with
  the following command (assuming you are in /usr/lib/)...
  ln -s libfmod-3.75.so libfmod.so.
 ==============================================================================================*) 

interface

{$I GLScene.inc}

uses

{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  Sounds.FMODtypes;


 const
{$IFDEF WIN32}
  FMOD_DLL = 'fmod32.dll';
{$ELSE}
  FMOD_DLL = 'fmod64.dll';
{$ENDIF}

{$IFDEF LINUX}
  FMOD_DLL = 'libfmod.so';
{$ELSE}
{$IFDEF DARWIN}
  FMOD_DLL = 'fmod.dylib';
{$ENDIF}
{$ENDIF}


//===============================================================================================
// FUNCTION PROTOTYPES
//===============================================================================================

// ================================== 
// Library load/unload functions.     
// ================================== 

(*
  If no library name is passed to FMOD_Load, then the default library name
  used.  These are stub functions.
*)

function FMOD_Load(LibName: PChar): Boolean;
procedure FMOD_Unload;

// ================================== 
// Initialization / Global functions. 
// ================================== 

(*
    PRE - FSOUND_Init functions. These can't be called after FSOUND_Init is
    called (they will fail). They set up FMOD system functionality.
*)

function FSOUND_SetOutput(OutputType: TFSoundOutputTypes): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetDriver(Driver: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMixer(Mixer: TFSoundMixerTypes): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetBufferSize(LenMs: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetHWND(Hwnd: THandle): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMinHardwareChannels(Min: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMaxHardwareChannels(Max: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMemorySystem(Pool: Pointer; PoolLen: Integer;
                                UserAlloc: TFSoundAllocCallback;
                                UserRealloc: TFSoundReallocCallback;
                                UserFree: TFSoundFreeCallback): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    Main initialization / closedown functions.
    Note : Use FSOUND_INIT_USEDEFAULTMIDISYNTH with FSOUND_Init for software override
           with MIDI playback.
         : Use FSOUND_INIT_GLOBALFOCUS with FSOUND_Init to make sound audible no matter
           which window is in focus. (FSOUND_OUTPUT_DSOUND only)
*)

function FSOUND_Init(MixRate: Integer; MaxSoftwareChannels: Integer; Flags: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_Close; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    Runtime system level functions
*)

procedure FSOUND_Update; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};  // This is called to update 3d sound / non-realtime output

procedure FSOUND_SetSpeakerMode(SpeakerMode: Cardinal); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_SetSFXMasterVolume(Volume: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_SetPanSeperation(PanSep: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_File_SetCallbacks(OpenCallback: TFSoundOpenCallback;
                                   CloseCallback: TFSoundCloseCallback;
                                   ReadCallback: TFSoundReadCallback;
                                   SeekCallback: TFSoundSeekCallback;
                                   TellCallback: TFSoundTellCallback); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    System information functions
*)

function FSOUND_GetError: TFModErrors; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetVersion: Single; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetOutput: TFSoundOutputTypes; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetOutputHandle: Pointer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetDriver: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetMixer: TFSoundMixerTypes; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetNumDrivers: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetDriverName(Id: Integer): PAnsiChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetDriverCaps(Id: Integer; var Caps: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetOutputRate: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetMaxChannels: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetMaxSamples: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
//function FSOUND_GetSpeakerMode: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetSFXMasterVolume: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetNumHWChannels(var num2d: Integer; var num3d: Integer; var total: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetChannelsPlaying: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetCPUUsage: Single; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_GetMemoryStats(var CurrentAlloced: Cardinal; var MaxAlloced: Cardinal); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

// =================================== 
// Sample management / load functions. 
// =================================== 

(*
    Sample creation and management functions
    Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Sample_Load to load from memory.
           Use FSOUND_LOADRAW      flag with FSOUND_Sample_Load to treat as as raw pcm data.
*)

function FSOUND_Sample_Load(Index: Integer; const NameOrData: PAnsiChar; Mode: Cardinal; Offset: Integer; Length: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_Alloc(Index: Integer; Length: Integer; Mode: Cardinal; DefFreq: Integer; DefVol: Integer; DefPan: Integer; DefPri: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_Sample_Free(Sptr: PFSoundSample); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_Upload(Sptr: PFSoundSample; SrcData: Pointer; Mode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_Lock(Sptr: PFSoundSample; Offset: Integer; Length: Integer; var Ptr1: Pointer; var Ptr2: Pointer; var Len1: Cardinal; var Len2: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_Unlock(Sptr: PFSoundSample; Ptr1: Pointer; Ptr2: Pointer; Len1: Cardinal; Len2: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    Sample control functions
*)

function FSOUND_Sample_SetMode(Sptr: PFSoundSample; Mode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_SetLoopPoints(Sptr: PFSoundSample; LoopStart, LoopEnd: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_SetDefaults(Sptr: PFSoundSample; DefFreq, DefVol, DefPan, DefPri: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_SetDefaultsEx(Sptr: PFSoundSample; DefFreq, DefVol, DefPan, DefPri, VarFreq, VarVol, VarPan: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_SetMinMaxDistance(Sptr: PFSoundSample; Min, Max: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_SetMaxPlaybacks(Sptr: PFSoundSample; Max: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    Sample information functions
*)

function FSOUND_Sample_Get(SampNo: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetName(Sptr: PFSoundSample): PAnsiChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetLength(Sptr: PFSoundSample): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetLoopPoints(Sptr: PFSoundSample; var LoopStart: Integer; var LoopEnd: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetDefaults(Sptr: PFSoundSample; var DefFreq: Integer; var DefVol: Integer; var DefPan: Integer; var DefPri: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetDefaultsEx(Sptr: PFSoundSample; var DefFreq: Integer; var DefVol: Integer; var DefPan: Integer; var DefPri: Integer;var VarFreq: Integer; var VarVol: Integer; var VarPan: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetMode(Sptr: PFSoundSample): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetMinMaxDistance(Sptr: PFSoundSample; var Min: Single; var Max: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

// ============================ 
// Channel control functions.   
// ============================ 

(*
    Playing and stopping sounds.
    Note : Use FSOUND_FREE as the 'channel' variable, to let FMOD pick a free channel for you.
           Use FSOUND_ALL as the 'channel' variable to control ALL channels with one function call!
*)

function FSOUND_PlaySound(Channel: Integer; Sptr: PFSoundSample): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_PlaySoundEx(Channel: Integer; Sptr: PFSoundSample; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_StopSound(Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    Functions to control playback of a channel.
    Note : FSOUND_ALL can be used on most of these functions as a channel value.
*)

function FSOUND_SetFrequency(Channel: Integer; Freq: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetVolume(Channel: Integer; Vol: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetVolumeAbsolute(Channel: Integer; Vol: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetPan(Channel: Integer; Pan: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetSurround(Channel: Integer; Surround: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMute(Channel: Integer; Mute: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetPriority(Channel: Integer; Priority: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetReserved(Channel: Integer; Reserved: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetPaused(Channel: Integer; Paused: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetLoopMode(Channel: Integer; LoopMode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetCurrentPosition(Channel: Integer; Offset: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_3D_SetAttributes(Channel: Integer; Pos: PFSoundVector; Vel: PFSoundVector): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_3D_SetMinMaxDistance(Channel: Integer; Min: Single; Max: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    Channel information functions
*)

function FSOUND_IsPlaying(Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetFrequency(Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetVolume(Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetAmplitude(Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetPan(Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetSurround(Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetMute(Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetPriority(Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetReserved(Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetPaused(Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetLoopMode(Channel: Integer): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetCurrentPosition(Channel: Integer): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetCurrentSample(Channel: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetCurrentLevels(Channel: Integer; l, r: PSingle): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetNumSubChannels(Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetSubChannel(Channel: Integer; SubChannel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_3D_GetAttributes(Channel: Integer; Pos: PFSoundVector; Vel: PFSoundVector): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_3D_GetMinMaxDistance(Channel: Integer; var Min: Single; var Max: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

// =================== 
// 3D sound functions. 
// =================== 

(*
    See also 3d sample and channel based functions above.
    Call FSOUND_Update once a frame to process 3d information.
*)

procedure FSOUND_3D_Listener_SetCurrent(current: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_Listener_SetAttributes(Pos: PFSoundVector; Vel: PFSoundVector;
                                           fx: Single; fy: Single; fz: Single;
                                           tx: Single; ty: Single; tz: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_Listener_GetAttributes(Pos: PFSoundVector; Vel: PFSoundVector;
                                           fx: PSingle; fy: PSingle; fz: PSingle;  
                                           tx: PSingle; ty: PSingle; tz: PSingle); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_SetDopplerFactor(Scale: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_SetDistanceFactor(Scale: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_SetRolloffFactor(Scale: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

// =================== 
// FX functions.       
// =================== 

(*
    Functions to control DX8 only effects processing.

    - FX enabled samples can only be played once at a time, not multiple times at once.
    - Sounds have to be created with FSOUND_HW2D or FSOUND_HW3D for this to work.
    - FSOUND_INIT_ENABLESYSTEMCHANNELFX can be used to apply hardware effect processing to the
      global mixed output of FMOD's software channels.
    - FSOUND_FX_Enable returns an FX handle that you can use to alter fx parameters.
    - FSOUND_FX_Enable can be called multiple times in a row, even on the same FX type,
      it will return a unique handle for each FX.
    - FSOUND_FX_Enable cannot be called if the sound is playing or locked.
    - FSOUND_FX_Disable must be called to reset/clear the FX from a channel.
*)

function FSOUND_FX_Enable(Channel: Integer; Fx: TFSoundFXModes): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_Disable(Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_FX_SetChorus(FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetCompressor(FXId: Integer; Gain, Attack, Release, Threshold, Ratio, Predelay: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetDistortion(FXId: Integer; Gain, Edge, PostEQCenterFrequency, PostEQBandwidth, PreLowpassCutoff: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetEcho(FXId: Integer; WetDryMix, Feedback, LeftDelay, RightDelay: Single; PanDelay: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetFlanger(FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetGargle(FXId, RateHz, WaveShape: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetI3DL2Reverb(FXId, Room, RoomHF: Integer; RoomRolloffFactor, DecayTime, DecayHFRatio: Single; Reflections: Integer; ReflectionsDelay: Single; Reverb: Integer; ReverbDelay, Diffusion, Density, HFReference: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetParamEQ(FXId: Integer; Center, Bandwidth, Gain: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetWavesReverb(FXId: Integer; InGain, ReverbMix, ReverbTime, HighFreqRTRatio: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

// ========================= 
// File Streaming functions. 
// ========================= 

(*
    Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Stream_Open to stream from memory.
           Use FSOUND_LOADRAW      flag with FSOUND_Stream_Open to treat stream as raw pcm data.
           Use FSOUND_MPEGACCURATE flag with FSOUND_Stream_Open to open mpegs in 'accurate mode' for settime/gettime/getlengthms.
           Use FSOUND_FREE as the 'channel' variable, to let FMOD pick a free channel for you.
*)

function FSOUND_Stream_SetBufferSize(Ms: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_Stream_Open(const name_or_data: PAnsiChar; Mode: Cardinal; Offset: Integer; Length: Integer): PFSoundStream; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Create(Callback: TFSoundStreamCallback; Length: Integer; Mode: Cardinal; SampleRate: Integer; UserData: Integer): PFSoundStream; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Close(Stream: PFSoundStream): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_Stream_Play(Channel: Integer; Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_PlayEx(Channel: Integer; Stream: PFSoundStream; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Stop(Stream: PFSoundStream): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_Stream_SetPosition(Stream: PFSoundStream; Position: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetPosition(Stream: PFSoundStream): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_SetTime(Stream: PFSoundStream; Ms: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetTime(Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetLength(Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetLengthMs(Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_Stream_SetMode(Stream: PFSoundStream; mode: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetMode(Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_SetLoopPoints(Stream: PFSoundStream; LoopStartPCM, LoopEndPCM: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_SetLoopCount(Stream: PFSoundStream; Count: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetOpenState(Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetSample(Stream: PFSoundStream): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_CreateDSP(Stream: PFSoundStream; Callback: TFSoundDSPCallback; Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_Stream_SetEndCallback(Stream: PFSoundStream; Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_SetSyncCallback(Stream: PFSoundStream; Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_Stream_AddSyncPoint(Stream: PFSoundStream; PCMOffset: Cardinal; Name: PAnsiChar): PFSyncPoint; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_DeleteSyncPoint(Point: PFSyncPoint): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetNumSyncPoints(Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetSyncPoint(Stream: PFSoundStream; Index: Integer): PFSyncPoint; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetSyncPointInfo(Point: PFSyncPoint; var PCMOffset: Cardinal): PAnsiChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_Stream_SetSubStream(Stream: PFSoundStream; Index: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetNumSubStreams(Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_SetSubStreamSentence(Stream: PFSoundStream; var SentenceList: Cardinal; NumItems: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_Stream_GetNumTagFields(Stream: PFSoundStream; var Num: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetTagField(Stream: PFSoundStream; Num: Integer; var TagType: TFSoundTagFieldType; var Name: PAnsiChar; var Value: Pointer; var Length: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_FindTagField(Stream: PFSoundStream; TagType: TFSoundTagFieldType; Name: PAnsiChar; var Value: Pointer; var Length: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    Internet streaming functions
*)

function FSOUND_Stream_Net_SetProxy(Proxy: PAnsiChar): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Net_GetLastServerStatus(): PAnsiChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Net_SetBufferProperties(BufferSize: Integer; PreBuffer_Percent: Integer; ReBuffer_Percent:  Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Net_GetBufferProperties(var Buffersize: Integer; var PreBuffer_Percent: Integer;  var ReBuffer_Percent: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Net_SetMetadataCallback(Stream: PFSoundStream; Callback: TFMetaDataCallback; UserData: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Net_GetStatus(Stream: PFSoundStream; var Status: TFSoundStreamNetStatus; var BufferPercentUsed: Integer; var BitRate: Integer; var Flags: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

// =================== 
// CD audio functions. 
// =================== 

(*
    Note : 0 = default cdrom.  Otherwise specify the drive letter, for example. 'D'. 
*)

function FSOUND_CD_Play(Drive: Byte; Track: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_CD_SetPlayMode(Drive: Byte; Mode: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_Stop(Drive: Byte): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_SetPaused(Drive: Byte; Paused: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_SetVolume(Drive: Byte; Volume: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_SetTrackTime(Drive: Byte; ms: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_OpenTray(Drive: Byte; Open: Byte): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_CD_GetPaused(Drive: Byte): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetTrack(Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetNumTracks(Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetVolume(Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetTrackLength(Drive: Byte; Track: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetTrackTime(Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

// ============== 
// DSP functions. 
// ============== 


(*
  DSP Unit control and information functions.
*)

function FSOUND_DSP_Create(Callback: TFSoundDSPCallback; Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_DSP_Free(DSPUnit: PFSoundDSPUnit); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_DSP_SetPriority(DSPUnit: PFSoundDSPUnit; Priority: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetPriority(DSPUnit: PFSoundDSPUnit): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_DSP_SetActive(DSPUnit: PFSoundDSPUnit; Active: ByteBool); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetActive(DSPUnit: PFSoundDSPUnit): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    Functions to get hold of FSOUND 'system DSP unit' handles.
*)

function FSOUND_DSP_GetClearUnit: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetSFXUnit: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetMusicUnit: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetFFTUnit: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetClipAndCopyUnit: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    Miscellaneous DSP functions
    Note for the spectrum analysis function to work, you have to enable the FFT DSP unit with
    the following code FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE);
    It is off by default to save cpu usage.
*)

function FSOUND_DSP_MixBuffers(DestBuffer: Pointer; SrcBuffer: Pointer; Len: Integer; Freq: Integer; Vol: Integer; Pan: Integer; Mode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_DSP_ClearMixBuffer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetBufferLength: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};      { Length of each DSP update }
function FSOUND_DSP_GetBufferLengthTotal: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF}; { Total buffer length due to FSOUND_SetBufferSize }
function FSOUND_DSP_GetSpectrum: PSingle; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};          { Array of 512 floats - call FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE)) for this to work. }

// ========================================================================== 
// Reverb functions. (eax2/3 reverb)  (NOT SUPPORTED IN LINUX/CE)               
// ========================================================================== 

(*
  See structures above for definitions and information on the reverb parameters.
*)

function FSOUND_Reverb_SetProperties(var Prop: TFSoundReverbProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Reverb_GetProperties(var Prop: TFSoundReverbProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Reverb_SetChannelProperties(Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Reverb_GetChannelProperties(Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

// ================================================ 
// Recording functions  (NOT SUPPORTED IN LINUX/MAC) 
// ================================================ 

(*
    Recording initialization functions
*)

function FSOUND_Record_SetDriver(OutputType: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_GetNumDrivers: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_GetDriverName(Id: Integer): PAnsiChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_GetDriver: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    Recording functionality. Only one recording session will work at a time.
*)

function FSOUND_Record_StartSample(Sptr: PFSoundSample; Loop: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_Stop: ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_GetPosition: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

// ============================================================================================= 
// FMUSIC API (MOD,S3M,XM,IT,MIDI PLAYBACK)                                                      
// ============================================================================================= 

(*
    Song management / playback functions.
*)

function FMUSIC_LoadSong(const Name: PAnsiChar): PFMusicModule; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_LoadSongEx(Name_Or_Data: Pointer; Offset: Integer; Length: Integer; Mode: Cardinal; var SampleList: Integer; SampleListNum: Integer): PFMusicModule; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetOpenState(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_FreeSong(Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_PlaySong(Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_StopSong(Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FMUSIC_StopAllSongs; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

function FMUSIC_SetZxxCallback(Module: PFMusicModule; Callback: TFMusicCallback): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetRowCallback(Module: PFMusicModule; Callback: TFMusicCallback; RowStep: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetOrderCallback(Module: PFMusicModule; Callback: TFMusicCallback; OrderStep: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetInstCallback(Module: PFMusicModule; Callback: TFMusicCallback; Instrument: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

function FMUSIC_SetSample(Module: PFMusicModule; SampNo: Integer; Sptr: PFSoundSample): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetUserData(Module: PFMusicModule; userdata: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_OptimizeChannels(Module: PFMusicModule; MaxChannels: Integer; MinVolume: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    Runtime song functions.
*)

function FMUSIC_SetReverb(Reverb: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetLooping(Module: PFMusicModule; Looping: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetOrder(Module: PFMusicModule; Order: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetPaused(Module: PFMusicModule; Pause: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetMasterVolume(Module: PFMusicModule; Volume: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetMasterSpeed(Module: PFMusicModule; speed: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetPanSeperation(Module: PFMusicModule; PanSep: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    Static song information functions.
*)

function FMUSIC_GetName(Module: PFMusicModule): PAnsiChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetType(Module: PFMusicModule): TFMusicTypes; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumOrders(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumPatterns(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumInstruments(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumSamples(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumChannels(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetSample(Module: PFMusicModule; SampNo: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetPatternLength(Module: PFMusicModule; OrderNo: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
    Runtime song information.
*)

function FMUSIC_IsFinished(Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_IsPlaying(Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetMasterVolume(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetGlobalVolume(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetOrder(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetPattern(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetSpeed(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetBPM(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetRow(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetPaused(Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetTime(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetRealChannel(Module: PFMusicModule; modchannel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetUserData(Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

//--------------------------------------------
implementation
//--------------------------------------------

(*
  Stub functions to allow applications to swap between static and dynamic with
  no code changes at all.
*)

function FMOD_Load(LibName: PChar): Boolean;
begin
  Result := True;
end;

procedure FMOD_Unload;
begin
end;

function FSOUND_SetOutput; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetOutput@4' {$ENDIF};
function FSOUND_SetDriver; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetDriver@4' {$ENDIF};
function FSOUND_SetMixer; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetMixer@4' {$ENDIF};
function FSOUND_SetBufferSize; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetBufferSize@4' {$ENDIF};
function FSOUND_SetHWND; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetHWND@4' {$ENDIF};
function FSOUND_SetMinHardwareChannels; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetMinHardwareChannels@4' {$ENDIF};
function FSOUND_SetMaxHardwareChannels; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetMaxHardwareChannels@4' {$ENDIF};
function FSOUND_SetMemorySystem; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetMemorySystem@20' {$ENDIF};
function FSOUND_Init; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Init@12' {$ENDIF};
procedure FSOUND_Close; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Close@0' {$ENDIF};
procedure FSOUND_Update; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Update@0' {$ENDIF};
procedure FSOUND_SetSpeakerMode; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetSpeakerMode@4' {$ENDIF};
procedure FSOUND_SetSFXMasterVolume; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetSFXMasterVolume@4' {$ENDIF};
procedure FSOUND_SetPanSeperation; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetPanSeperation@4' {$ENDIF};
function FSOUND_GetError; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetError@0' {$ENDIF};
function FSOUND_GetVersion; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetVersion@0' {$ENDIF};
function FSOUND_GetOutput; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetOutput@0' {$ENDIF};
function FSOUND_GetOutputHandle; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetOutputHandle@0' {$ENDIF};
function FSOUND_GetDriver; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetDriver@0' {$ENDIF};
function FSOUND_GetMixer; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetMixer@0' {$ENDIF};
function FSOUND_GetNumDrivers; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetNumDrivers@0' {$ENDIF};
function FSOUND_GetDriverName; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetDriverName@4' {$ENDIF};
function FSOUND_GetDriverCaps; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetDriverCaps@8' {$ENDIF};
function FSOUND_GetOutputRate; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetOutputRate@0' {$ENDIF};
function FSOUND_GetMaxChannels; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetMaxChannels@0' {$ENDIF};
function FSOUND_GetMaxSamples; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetMaxSamples@0' {$ENDIF};
//function FSOUND_GetSpeakerMode; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetSpeakerMode@0' {$ENDIF};
function FSOUND_GetSFXMasterVolume; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetSFXMasterVolume@0' {$ENDIF};
function FSOUND_GetNumHWChannels; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetNumHWChannels@12' {$ENDIF};
function FSOUND_GetChannelsPlaying; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetChannelsPlaying@0' {$ENDIF};
function FSOUND_GetCPUUsage; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetCPUUsage@0' {$ENDIF};
procedure FSOUND_GetMemoryStats; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetMemoryStats@8' {$ENDIF};
function FSOUND_Sample_Load; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_Load@20' {$ENDIF};
function FSOUND_Sample_Alloc; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_Alloc@28' {$ENDIF};
procedure FSOUND_Sample_Free; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_Free@4' {$ENDIF};
function FSOUND_Sample_Upload; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_Upload@12' {$ENDIF};
function FSOUND_Sample_Lock; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_Lock@28' {$ENDIF};
function FSOUND_Sample_Unlock; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_Unlock@20' {$ENDIF};
function FSOUND_Sample_SetMode; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_SetMode@8' {$ENDIF};
function FSOUND_Sample_SetLoopPoints; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_SetLoopPoints@12' {$ENDIF};
function FSOUND_Sample_SetDefaults; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_SetDefaults@20' {$ENDIF};
function FSOUND_Sample_SetDefaultsEx; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_SetDefaultsEx@32' {$ENDIF};
function FSOUND_Sample_SetMinMaxDistance; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_SetMinMaxDistance@12' {$ENDIF};
function FSOUND_Sample_SetMaxPlaybacks; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_SetMaxPlaybacks@8' {$ENDIF};
function FSOUND_Sample_Get; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_Get@4' {$ENDIF};
function FSOUND_Sample_GetName; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_GetName@4' {$ENDIF};
function FSOUND_Sample_GetLength; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_GetLength@4' {$ENDIF};
function FSOUND_Sample_GetLoopPoints; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_GetLoopPoints@12' {$ENDIF};
function FSOUND_Sample_GetDefaults; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_GetDefaults@20' {$ENDIF};
function FSOUND_Sample_GetDefaultsEx; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_GetDefaultsEx@32' {$ENDIF};
function FSOUND_Sample_GetMode; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_GetMode@4' {$ENDIF};
function FSOUND_Sample_GetMinMaxDistance; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Sample_GetMinMaxDistance@12' {$ENDIF};
function FSOUND_PlaySound; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_PlaySound@8' {$ENDIF};
function FSOUND_PlaySoundEx; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_PlaySoundEx@16' {$ENDIF};
function FSOUND_StopSound; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_StopSound@4' {$ENDIF};
function FSOUND_SetFrequency; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetFrequency@8' {$ENDIF};
function FSOUND_SetVolume; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetVolume@8' {$ENDIF};
function FSOUND_SetVolumeAbsolute; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetVolumeAbsolute@8' {$ENDIF};
function FSOUND_SetPan; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetPan@8' {$ENDIF};
function FSOUND_SetSurround; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetSurround@8' {$ENDIF};
function FSOUND_SetMute; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetMute@8' {$ENDIF};
function FSOUND_SetPriority; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetPriority@8' {$ENDIF};
function FSOUND_SetReserved; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetReserved@8' {$ENDIF};
function FSOUND_SetPaused; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetPaused@8' {$ENDIF};
function FSOUND_SetLoopMode; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetLoopMode@8' {$ENDIF};
function FSOUND_SetCurrentPosition; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_SetCurrentPosition@8' {$ENDIF};
function FSOUND_3D_SetAttributes; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_3D_SetAttributes@12' {$ENDIF};
function FSOUND_3D_SetMinMaxDistance; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_3D_SetMinMaxDistance@12' {$ENDIF};
function FSOUND_IsPlaying; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_IsPlaying@4' {$ENDIF};
function FSOUND_GetFrequency; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetFrequency@4' {$ENDIF};
function FSOUND_GetVolume; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetVolume@4' {$ENDIF};
function FSOUND_GetAmplitude; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetAmplitude@4' {$ENDIF};
function FSOUND_GetPan; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetPan@4' {$ENDIF};
function FSOUND_GetSurround; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetSurround@4' {$ENDIF};
function FSOUND_GetMute; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetMute@4' {$ENDIF};
function FSOUND_GetPriority; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetPriority@4' {$ENDIF};
function FSOUND_GetReserved; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetReserved@4' {$ENDIF};
function FSOUND_GetPaused; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetPaused@4' {$ENDIF};
function FSOUND_GetLoopMode; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetLoopMode@4' {$ENDIF};
function FSOUND_GetCurrentPosition; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetCurrentPosition@4' {$ENDIF};
function FSOUND_GetCurrentSample; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetCurrentSample@4' {$ENDIF};
function FSOUND_GetCurrentLevels; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetCurrentLevels@12' {$ENDIF};
function FSOUND_GetNumSubChannels; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetNumSubChannels@4' {$ENDIF};
function FSOUND_GetSubChannel; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_GetSubChannel@8' {$ENDIF};
function FSOUND_3D_GetAttributes; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_3D_GetAttributes@12' {$ENDIF};
function FSOUND_3D_GetMinMaxDistance; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_3D_GetMinMaxDistance@12' {$ENDIF};
procedure FSOUND_3D_Listener_SetCurrent; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_3D_Listener_SetCurrent@8' {$ENDIF};
procedure FSOUND_3D_Listener_SetAttributes; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_3D_Listener_SetAttributes@32' {$ENDIF};
procedure FSOUND_3D_Listener_GetAttributes; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_3D_Listener_GetAttributes@32' {$ENDIF};
procedure FSOUND_3D_SetDopplerFactor; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_3D_SetDopplerFactor@4' {$ENDIF};
procedure FSOUND_3D_SetDistanceFactor; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_3D_SetDistanceFactor@4' {$ENDIF};
procedure FSOUND_3D_SetRolloffFactor; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_3D_SetRolloffFactor@4' {$ENDIF};
function FSOUND_FX_Enable; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_FX_Enable@8' {$ENDIF};
function FSOUND_FX_Disable; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_FX_Disable@4' {$ENDIF};
function FSOUND_FX_SetChorus; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_FX_SetChorus@32' {$ENDIF};
function FSOUND_FX_SetCompressor; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_FX_SetCompressor@28' {$ENDIF};
function FSOUND_FX_SetDistortion; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_FX_SetDistortion@24' {$ENDIF};
function FSOUND_FX_SetEcho; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_FX_SetEcho@24' {$ENDIF};
function FSOUND_FX_SetFlanger; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_FX_SetFlanger@32' {$ENDIF};
function FSOUND_FX_SetGargle; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_FX_SetGargle@12' {$ENDIF};
function FSOUND_FX_SetI3DL2Reverb; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_FX_SetI3DL2Reverb@52' {$ENDIF};
function FSOUND_FX_SetParamEQ; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_FX_SetParamEQ@16' {$ENDIF};
function FSOUND_FX_SetWavesReverb; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_FX_SetWavesReverb@20' {$ENDIF};
function FSOUND_Stream_Open; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_Open@16' {$ENDIF};
function FSOUND_Stream_Create; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_Create@20' {$ENDIF};
function FSOUND_Stream_Play; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_Play@8' {$ENDIF};
function FSOUND_Stream_PlayEx; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_PlayEx@16' {$ENDIF};
function FSOUND_Stream_Stop; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_Stop@4' {$ENDIF};
function FSOUND_Stream_Close; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_Close@4' {$ENDIF};
function FSOUND_Stream_SetEndCallback; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_SetEndCallback@12' {$ENDIF};
function FSOUND_Stream_SetSyncCallback; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_SetSyncCallback@12' {$ENDIF};
function FSOUND_Stream_GetSample; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_GetSample@4' {$ENDIF};
function FSOUND_Stream_CreateDSP; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_CreateDSP@16' {$ENDIF};
function FSOUND_Stream_SetBufferSize; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_SetBufferSize@4' {$ENDIF};
function FSOUND_Stream_SetPosition; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_SetPosition@8' {$ENDIF};
function FSOUND_Stream_GetPosition; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_GetPosition@4' {$ENDIF};
function FSOUND_Stream_SetTime; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_SetTime@8' {$ENDIF};
function FSOUND_Stream_GetTime; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_GetTime@4' {$ENDIF};
function FSOUND_Stream_GetLength; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_GetLength@4' {$ENDIF};
function FSOUND_Stream_GetLengthMs; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_GetLengthMs@4' {$ENDIF};
function FSOUND_Stream_SetMode; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_SetMode@8' {$ENDIF};
function FSOUND_Stream_GetMode; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_GetMode@4' {$ENDIF};
function FSOUND_Stream_SetLoopPoints; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_SetLoopPoints@12' {$ENDIF};
function FSOUND_Stream_SetLoopCount; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_SetLoopCount@8' {$ENDIF};
function FSOUND_Stream_AddSyncPoint; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_AddSyncPoint@12' {$ENDIF};
function FSOUND_Stream_DeleteSyncPoint; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_DeleteSyncPoint@4' {$ENDIF};
function FSOUND_Stream_GetNumSyncPoints; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_GetNumSyncPoints@4' {$ENDIF};
function FSOUND_Stream_GetSyncPoint; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_GetSyncPoint@8' {$ENDIF};
function FSOUND_Stream_GetSyncPointInfo; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_GetSyncPointInfo@8' {$ENDIF};
function FSOUND_Stream_GetOpenState; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_GetOpenState@4' {$ENDIF};
function FSOUND_Stream_SetSubStream; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_SetSubStream@8' {$ENDIF};
function FSOUND_Stream_GetNumSubStreams; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_GetNumSubStreams@4' {$ENDIF};
function FSOUND_Stream_SetSubStreamSentence; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_SetSubStreamSentence@12' {$ENDIF};
function FSOUND_Stream_GetNumTagFields; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_GetNumTagFields@8' {$ENDIF};
function FSOUND_Stream_GetTagField; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_GetTagField@24' {$ENDIF};
function FSOUND_Stream_FindTagField; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_FindTagField@20' {$ENDIF};
function FSOUND_Stream_Net_SetProxy; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_Net_SetProxy@4' {$ENDIF};
function FSOUND_Stream_Net_GetLastServerStatus; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_Net_GetLastServerStatus@0' {$ENDIF};
function FSOUND_Stream_Net_SetBufferProperties; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_Net_SetBufferProperties@12' {$ENDIF};
function FSOUND_Stream_Net_GetBufferProperties; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_Net_GetBufferProperties@12' {$ENDIF};
function FSOUND_Stream_Net_SetMetadataCallback; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_Net_SetMetadataCallback@12' {$ENDIF};
function FSOUND_Stream_Net_GetStatus; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Stream_Net_GetStatus@20' {$ENDIF};
function FSOUND_CD_Play; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_CD_Play@8' {$ENDIF};
procedure FSOUND_CD_SetPlayMode; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_CD_SetPlayMode@8' {$ENDIF};
function FSOUND_CD_Stop; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_CD_Stop@4' {$ENDIF};
function FSOUND_CD_SetPaused; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_CD_SetPaused@8' {$ENDIF};
function FSOUND_CD_SetVolume; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_CD_SetVolume@8' {$ENDIF};
function FSOUND_CD_SetTrackTime; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_CD_SetTrackTime@8' {$ENDIF};
function FSOUND_CD_OpenTray; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_CD_OpenTray@8' {$ENDIF};
function FSOUND_CD_GetPaused; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_CD_GetPaused@4' {$ENDIF};
function FSOUND_CD_GetTrack; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_CD_GetTrack@4' {$ENDIF};
function FSOUND_CD_GetNumTracks; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_CD_GetNumTracks@4' {$ENDIF};
function FSOUND_CD_GetVolume; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_CD_GetVolume@4' {$ENDIF};
function FSOUND_CD_GetTrackLength; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_CD_GetTrackLength@8' {$ENDIF};
function FSOUND_CD_GetTrackTime; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_CD_GetTrackTime@4' {$ENDIF};
function FSOUND_DSP_Create; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_Create@12' {$ENDIF};
procedure FSOUND_DSP_Free; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_Free@4' {$ENDIF};
procedure FSOUND_DSP_SetPriority; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_SetPriority@8' {$ENDIF};
function FSOUND_DSP_GetPriority; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_GetPriority@4' {$ENDIF};
procedure FSOUND_DSP_SetActive; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_SetActive@8' {$ENDIF};
function FSOUND_DSP_GetActive; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_GetActive@4' {$ENDIF};
function FSOUND_DSP_GetClearUnit; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_GetClearUnit@0' {$ENDIF};
function FSOUND_DSP_GetSFXUnit; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_GetSFXUnit@0' {$ENDIF};
function FSOUND_DSP_GetMusicUnit; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_GetMusicUnit@0' {$ENDIF};
function FSOUND_DSP_GetClipAndCopyUnit; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_GetClipAndCopyUnit@0' {$ENDIF};
function FSOUND_DSP_GetFFTUnit; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_GetFFTUnit@0' {$ENDIF};
function FSOUND_DSP_MixBuffers; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_MixBuffers@28' {$ENDIF};
procedure FSOUND_DSP_ClearMixBuffer; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_ClearMixBuffer@0' {$ENDIF};
function FSOUND_DSP_GetBufferLength; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_GetBufferLength@0' {$ENDIF};
function FSOUND_DSP_GetBufferLengthTotal; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_GetBufferLengthTotal@0' {$ENDIF};
function FSOUND_DSP_GetSpectrum; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_DSP_GetSpectrum@0' {$ENDIF};
function FSOUND_Reverb_SetProperties; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Reverb_SetProperties@4' {$ENDIF};
function FSOUND_Reverb_GetProperties; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Reverb_GetProperties@4' {$ENDIF};
function FSOUND_Reverb_SetChannelProperties; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Reverb_SetChannelProperties@8' {$ENDIF};
function FSOUND_Reverb_GetChannelProperties; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Reverb_GetChannelProperties@8' {$ENDIF};
function FSOUND_Record_SetDriver; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Record_SetDriver@4' {$ENDIF};
function FSOUND_Record_GetNumDrivers; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Record_GetNumDrivers@0' {$ENDIF};
function FSOUND_Record_GetDriverName; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Record_GetDriverName@4' {$ENDIF};
function FSOUND_Record_GetDriver; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Record_GetDriver@0' {$ENDIF};
function FSOUND_Record_StartSample; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Record_StartSample@8' {$ENDIF};
function FSOUND_Record_Stop; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Record_Stop@0' {$ENDIF};
function FSOUND_Record_GetPosition; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_Record_GetPosition@0' {$ENDIF};
procedure FSOUND_File_SetCallbacks; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FSOUND_File_SetCallbacks@20' {$ENDIF};
function FMUSIC_LoadSong; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_LoadSong@4' {$ENDIF};
function FMUSIC_LoadSongEx; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_LoadSongEx@24' {$ENDIF};
function FMUSIC_GetOpenState; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetOpenState@4' {$ENDIF};
function FMUSIC_FreeSong; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_FreeSong@4' {$ENDIF};
function FMUSIC_PlaySong; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_PlaySong@4' {$ENDIF};
function FMUSIC_StopSong; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_StopSong@4' {$ENDIF};
procedure FMUSIC_StopAllSongs; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_StopAllSongs@0' {$ENDIF};
function FMUSIC_SetZxxCallback; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_SetZxxCallback@8' {$ENDIF};
function FMUSIC_SetRowCallback; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_SetRowCallback@12' {$ENDIF};
function FMUSIC_SetOrderCallback; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_SetOrderCallback@12' {$ENDIF};
function FMUSIC_SetInstCallback; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_SetInstCallback@12' {$ENDIF};
function FMUSIC_SetSample; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_SetSample@12' {$ENDIF};
function FMUSIC_SetUserData; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_SetUserData@8' {$ENDIF};
function FMUSIC_OptimizeChannels; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_OptimizeChannels@12' {$ENDIF};
function FMUSIC_SetReverb; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_SetReverb@4' {$ENDIF};
function FMUSIC_SetLooping; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_SetLooping@8' {$ENDIF};
function FMUSIC_SetOrder; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_SetOrder@8' {$ENDIF};
function FMUSIC_SetPaused; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_SetPaused@8' {$ENDIF};
function FMUSIC_SetMasterVolume; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_SetMasterVolume@8' {$ENDIF};
function FMUSIC_SetMasterSpeed; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_SetMasterSpeed@8' {$ENDIF};
function FMUSIC_SetPanSeperation; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_SetPanSeperation@8' {$ENDIF};
function FMUSIC_GetName; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetName@4' {$ENDIF};
function FMUSIC_GetType; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetType@4' {$ENDIF};
function FMUSIC_GetNumOrders; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetNumOrders@4' {$ENDIF};
function FMUSIC_GetNumPatterns; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetNumPatterns@4' {$ENDIF};
function FMUSIC_GetNumInstruments; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetNumInstruments@4' {$ENDIF};
function FMUSIC_GetNumSamples; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetNumSamples@4' {$ENDIF};
function FMUSIC_GetNumChannels; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetNumChannels@4' {$ENDIF};
function FMUSIC_GetSample; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetSample@8' {$ENDIF};
function FMUSIC_GetPatternLength; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetPatternLength@8' {$ENDIF};
function FMUSIC_IsFinished; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_IsFinished@4' {$ENDIF};
function FMUSIC_IsPlaying; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_IsPlaying@4' {$ENDIF};
function FMUSIC_GetMasterVolume; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetMasterVolume@4' {$ENDIF};
function FMUSIC_GetGlobalVolume; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetGlobalVolume@4' {$ENDIF};
function FMUSIC_GetOrder; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetOrder@4' {$ENDIF};
function FMUSIC_GetPattern; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetPattern@4' {$ENDIF};
function FMUSIC_GetSpeed; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetSpeed@4' {$ENDIF};
function FMUSIC_GetBPM; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetBPM@4' {$ENDIF};
function FMUSIC_GetRow; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetRow@4' {$ENDIF};
function FMUSIC_GetPaused; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetPaused@4' {$ENDIF};
function FMUSIC_GetTime; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetTime@4' {$ENDIF};
function FMUSIC_GetRealChannel; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetRealChannel@8' {$ENDIF};
function FMUSIC_GetUserData; external FMOD_DLL {$IFDEF MSWINDOWS} name '_FMUSIC_GetUserData@4' {$ENDIF};

var
  Saved8087CW: Word;

//---------------------------------------------
initialization
//---------------------------------------------

  // Save the current FPU state and then disable FPU exceptions 
  Saved8087CW := Default8087CW;
  Set8087CW($133f); // Disable all fpu exceptions 

finalization
  // Reset the FPU to the previous state 
  Set8087CW(Saved8087CW);
end.
