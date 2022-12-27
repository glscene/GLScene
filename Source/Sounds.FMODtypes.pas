//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit Sounds.FMODtypes;

(*=============================================================================================== 
  FMOD Types header file. Copyright (c), Firelight Technologies Pty, Ltd. 1999-2004.              
  =============================================================================================== 

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
*)


interface

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

(*=============================================================================================== 
  DEFINITIONS                                                                                     
 ===============================================================================================*) 

(*
  Force four-byte enums
*)

{$Z4}

const
  FMOD_VERSION: Single = 3.75;

(*
  FMOD defined types
*)

type
  PFSoundSample = Pointer;
  PFSoundStream = Pointer;
  PFSoundDSPUnit = Pointer;
  PFMusicModule = Pointer;
  PFSyncPoint = Pointer;

  PFSoundVector = ^TFSoundVector;
  TFSoundVector = record
    x: Single;
    y: Single;
    z: Single;
  end;

 (*
    Callback types
 *)

  TFSoundStreamCallback   = function (Stream: PFSoundStream; Buff: Pointer; Length, Param: Integer): ByteBool;  {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  TFSoundDSPCallback      = function (OriginalBuffer: Pointer; NewBuffer: Pointer; Length, Param: Integer): Pointer;  {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  TFMusicCallback         = procedure (Module: PFMusicModule; Param: Byte);  {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  TFSoundOpenCallback     = function (Name: PChar): Cardinal;  {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  TFSoundCloseCallback    = procedure (Handle: Cardinal);  {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  TFSoundReadCallback     = function (Buffer: Pointer; Size: Cardinal; Handle: Cardinal): Cardinal;  {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  TFSoundSeekCallback     = procedure (Handle: Cardinal; Pos: Cardinal; Mode: Byte);  {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  TFSoundTellCallback     = function (Handle: Cardinal): Cardinal;  {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  TFSoundAllocCallback    = function(Size: Cardinal): Pointer;  {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  TFSoundReallocCallback  = function(Ptr: Pointer; Size: Cardinal): Pointer;  {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  TFSoundFreeCallback     = procedure(Ptr: Pointer);  {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  TFMetaDataCallback      = function(Name: PChar; Value: PChar; userdata: Integer): ByteBool;  {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

(*
[ENUM]
[
  [DESCRIPTION]
  On failure of commands in FMOD, use FSOUND_GetError to attain what happened.

  [SEE_ALSO]
  FSOUND_GetError
]
*)

type
  TFModErrors = (
    FMOD_ERR_NONE,             // No errors
    FMOD_ERR_BUSY,             // Cannot call this command after FSOUND_Init.  Call FSOUND_Close first.
    FMOD_ERR_UNINITIALIZED,    // This command failed because FSOUND_Init was not called
    FMOD_ERR_INIT,             // Error initializing output device.
    FMOD_ERR_ALLOCATED,        // Error initializing output device, but more specifically, the output device is already in use and cannot be reused.
    FMOD_ERR_PLAY,             // Playing the sound failed.
    FMOD_ERR_OUTPUT_FORMAT,    // Soundcard does not support the features needed for this soundsystem (16bit stereo output)
    FMOD_ERR_COOPERATIVELEVEL, // Error setting cooperative level for hardware.
    FMOD_ERR_CREATEBUFFER,     // Error creating hardware sound buffer.
    FMOD_ERR_FILE_NOTFOUND,    // File not found
    FMOD_ERR_FILE_FORMAT,      // Unknown file format
    FMOD_ERR_FILE_BAD,         // Error loading file
    FMOD_ERR_MEMORY,           // Not enough memory or resources
    FMOD_ERR_VERSION,          // The version number of this file format is not supported
    FMOD_ERR_INVALID_PARAM,    // An invalid parameter was passed to this function
    FMOD_ERR_NO_EAX,           // Tried to use an EAX command on a non EAX enabled channel or output.
    FMOD_ERR_CHANNEL_ALLOC,    // Failed to allocate a new channel
    FMOD_ERR_RECORD,           // Recording is not supported on this machine
    FMOD_ERR_MEDIAPLAYER,      // Windows Media Player not installed so cannot play wma or use internet streaming. */
    FMOD_ERR_CDDEVICE          // An error occured trying to open the specified CD device
  );

(*
[ENUM]
[
    [DESCRIPTION]
    These output types are used with FSOUND_SetOutput, to choose which output driver to use.

	FSOUND_OUTPUT_DSOUND will not support hardware 3d acceleration if the sound card driver
	does not support DirectX 6 Voice Manager Extensions.

    FSOUND_OUTPUT_WINMM is recommended for NT and CE.

    [SEE_ALSO]
    FSOUND_SetOutput
    FSOUND_GetOutput
]
*)

type
  TFSoundOutputTypes = (
    FSOUND_OUTPUT_NOSOUND,  // NoSound driver, all calls to this succeed but do nothing.
    FSOUND_OUTPUT_WINMM,    // Windows Multimedia driver.
    FSOUND_OUTPUT_DSOUND,   // DirectSound driver.  You need this to get EAX2 or EAX3 support, or FX api support.
    FSOUND_OUTPUT_A3D,      // A3D driver.

    FSOUND_OUTPUT_OSS,      // Linux/Unix OSS (Open Sound System) driver, i.e. the kernel sound drivers.
    FSOUND_OUTPUT_ESD,      // Linux/Unix ESD (Enlightment Sound Daemon) driver.
    FSOUND_OUTPUT_ALSA,     // Linux Alsa driver.

    FSOUND_OUTPUT_ASIO,     // Low latency ASIO driver.
    FSOUND_OUTPUT_XBOX,     // Xbox driver.
    FSOUND_OUTPUT_PS2,      // PlayStation 2 driver.
    FSOUND_OUTPUT_MAC,      // Mac SoundMager driver.
    FSOUND_OUTPUT_GC,       // Gamecube driver.
    FSOUND_OUTPUT_PSP,      // PlayStation Portable driver.

    FSOUND_OUTPUT_NOSOUND_NONREALTIME   // This is the same as nosound, but the sound generation is driven by FSOUND_Update
  );


(*
[ENUM]
[
  [DESCRIPTION]
  These mixer types are used with FSOUND_SetMixer, to choose which mixer to use, or to act
  upon for other reasons using FSOUND_GetMixer.
  It is not necessary to set the mixer.  FMOD will autodetect the best mixer for you.

  [SEE_ALSO]
  FSOUND_SetMixer
  FSOUND_GetMixer
]
*)

type
  TFSoundMixerTypes = (
    FSOUND_MIXER_AUTODETECT,        // CE/PS2/GC Only - Non interpolating/low quality mixer.
    FSOUND_MIXER_BLENDMODE,         // Removed / obsolete
    FSOUND_MIXER_MMXP5,             // Removed / obsolete
    FSOUND_MIXER_MMXP6,             // Removed / obsolete

    FSOUND_MIXER_QUALITY_AUTODETECT,// All platforms - Autodetect the fastest quality mixer based on your cpu.
    FSOUND_MIXER_QUALITY_FPU,       // Win32/Linux only - Interpolating/volume ramping FPU mixer.
    FSOUND_MIXER_QUALITY_MMXP5,     // Win32/Linux only - Interpolating/volume ramping P5 MMX mixer.
    FSOUND_MIXER_QUALITY_MMXP6,     // Win32/Linux only - Interpolating/volume ramping ppro+ MMX mixer.

    FSOUND_MIXER_MONO,              // CE/PS2/GC only - MONO non interpolating/low quality mixer. For speed
    FSOUND_MIXER_QUALITY_MONO,      // CE/PS2/GC only - MONO Interpolating mixer.  For speed

    FSOUND_MIXER_MAX
  );


(*
[ENUM]
[
  [DESCRIPTION]
  These definitions describe the type of song being played.

  [SEE_ALSO]
  FMUSIC_GetType
]
*)

type
  TFMusicTypes = (
    FMUSIC_TYPE_NONE,
    FMUSIC_TYPE_MOD,  // Protracker / FastTracker
    FMUSIC_TYPE_S3M,  // ScreamTracker 3
    FMUSIC_TYPE_XM,   // FastTracker 2
    FMUSIC_TYPE_IT,   // Impulse Tracker
    FMUSIC_TYPE_MIDI, // MIDI file
    FMUSIC_TYPE_FSB   // FMOD Sample Bank file
  );


(*
[DEFINE_START]
[
  [NAME]
  FSOUND_DSP_PRIORITIES

  [DESCRIPTION]
  These default priorities are used by FMOD internal system DSP units.  They describe the
  position of the DSP chain, and the order of how audio processing is executed.
  You can actually through the use of FSOUND_DSP_GetxxxUnit (where xxx is the name of the DSP
  unit), disable or even change the priority of a DSP unit.

  [SEE_ALSO]
  FSOUND_DSP_Create
  FSOUND_DSP_SetPriority
  FSOUND_DSP_GetSpectrum
]
*)

const
  FSOUND_DSP_DEFAULTPRIORITY_CLEARUNIT        = 0;    // DSP CLEAR unit - done first
  FSOUND_DSP_DEFAULTPRIORITY_SFXUNIT          = 100;  // DSP SFX unit - done second
  FSOUND_DSP_DEFAULTPRIORITY_MUSICUNIT        = 200;  // DSP MUSIC unit - done third
  FSOUND_DSP_DEFAULTPRIORITY_USER             = 300;  // User priority, use this as reference for your own DSP units
  FSOUND_DSP_DEFAULTPRIORITY_FFTUNIT          = 900;  // This reads data for FSOUND_DSP_GetSpectrum, so it comes after user units
  FSOUND_DSP_DEFAULTPRIORITY_CLIPANDCOPYUNIT  = 1000; // DSP CLIP AND COPY unit - last
// [DEFINE_END]


(*
[DEFINE_START]
[
  [NAME]
  FSOUND_CAPS

  [DESCRIPTION]
  Driver description bitfields. Use FSOUND_Driver_GetCaps to determine if a driver enumerated
  has the settings you are after. The enumerated driver depends on the output mode, see
  FSOUND_OUTPUTTYPES

  [SEE_ALSO]
  FSOUND_GetDriverCaps
  FSOUND_OUTPUTTYPES
]
*)

const
  FSOUND_CAPS_HARDWARE              = $1;  // This driver supports hardware accelerated 3d sound.
  FSOUND_CAPS_EAX2                  = $2;  // This driver supports EAX 2 reverb
  FSOUND_CAPS_EAX3                  = $10; // This driver supports EAX 3 reverb
// [DEFINE_END]


(*
[DEFINE_START]
[
    [NAME]
    FSOUND_MODES

    [DESCRIPTION]
    Sample description bitfields, OR them together for loading and describing samples.
    NOTE.  If the file format being loaded already has a defined format, such as WAV or MP3, then
    trying to override the pre-defined format with a new set of format flags will not work.  For
    example, an 8 bit WAV file will not load as 16bit if you specify FSOUND_16BITS.  It will just
    ignore the flag and go ahead loading it as 8bits.  For these type of formats the only flags
    you can specify that will really alter the behaviour of how it is loaded, are the following.

    Looping behaviour - FSOUND_LOOP_OFF, FSOUND_LOOP_NORMAL, FSOUND_LOOP_BIDI 
    Load destination - FSOUND_HW3D, FSOUND_HW2D, FSOUND_2D
    Loading behaviour - FSOUND_NONBLOCKING, FSOUND_LOADMEMORY, FSOUND_LOADRAW, FSOUND_MPEGACCURATE, FSOUND_MPEGHALFRATE, FSOUND_FORCEMONO
    Playback behaviour - FSOUND_STREAMABLE, FSOUND_ENABLEFX
    PlayStation 2 only - FSOUND_USECORE0, FSOUND_USECORE1, FSOUND_LOADMEMORYIOP    

    See flag descriptions for what these do.
]
*)

const
  FSOUND_LOOP_OFF      = $00000001;  // For non looping samples.
  FSOUND_LOOP_NORMAL   = $00000002;  // For forward looping samples.
  FSOUND_LOOP_BIDI     = $00000004;  // For bidirectional looping samples.  (no effect if in hardware).
  FSOUND_8BITS         = $00000008;  // For 8 bit samples.
  FSOUND_16BITS        = $00000010;  // For 16 bit samples.
  FSOUND_MONO          = $00000020;  // For mono samples.
  FSOUND_STEREO        = $00000040;  // For stereo samples.
  FSOUND_UNSIGNED      = $00000080;  // For user created source data containing unsigned samples.
  FSOUND_SIGNED        = $00000100;  // For user created source data containing signed data.
  FSOUND_DELTA         = $00000200;  // For user created source data stored as delta values.
  FSOUND_IT214         = $00000400;  // For user created source data stored using IT214 compression.
  FSOUND_IT215         = $00000800;  // For user created source data stored using IT215 compression.
  FSOUND_HW3D          = $00001000;  // Attempts to make samples use 3d hardware acceleration. (if the card supports it)
  FSOUND_2D            = $00002000;  // Ignores any 3d processing.  Overrides FSOUND_HW3D.  Located in software.
  FSOUND_STREAMABLE    = $00004000;  // For a streamimg sound where you feed the data to it. */
  FSOUND_LOADMEMORY    = $00008000;  // "name" will be interpreted as a pointer to data for streaming and samples.
  FSOUND_LOADRAW       = $00010000;  // Will ignore file format and treat as raw pcm.
  FSOUND_MPEGACCURATE  = $00020000;  // For FSOUND_Stream_OpenFile - for accurate FSOUND_Stream_GetLengthMs/FSOUND_Stream_SetTime.  WARNING, see FSOUND_Stream_OpenFile for inital opening time performance issues.
  FSOUND_FORCEMONO     = $00040000;  // For forcing stereo streams and samples to be mono - needed if using FSOUND_HW3D and stereo data - incurs a small speed hit for streams
  FSOUND_HW2D          = $00080000;  // 2D hardware sounds.  allows hardware specific effects
  FSOUND_ENABLEFX      = $00100000;  // Allows DX8 FX to be played back on a sound.  Requires DirectX 8 - Note these sounds cannot be played more than once, be 8 bit, be less than a certain size, or have a changing frequency
  FSOUND_MPEGHALFRATE  = $00200000;  // For FMODCE only - decodes mpeg streams using a lower quality decode, but faster execution
  FSOUND_XADPCM        = $00400000;  // For XBOX only - Contents are compressed as XADPCM  */
  FSOUND_VAG           = $00800000;  // For PS2 only - Contents are compressed as Sony VAG format */
  FSOUND_NONBLOCKING   = $01000000;  // For FSOUND_Stream_OpenFile - Causes stream to open in the background and not block the foreground app - stream plays only when ready.
  FSOUND_GCADPCM       = $02000000;  // For Gamecube only - Contents are compressed as Gamecube DSP-ADPCM format
  FSOUND_MULTICHANNEL  = $04000000;  // For PS2 only - Contents are interleaved into a multi-channel (more than stereo) format
  FSOUND_USECORE0      = $08000000;  // For PS2 only - Sample/Stream is forced to use hardware voices 00-23
  FSOUND_USECORE1      = $10000000;  // For PS2 only - Sample/Stream is forced to use hardware voices 24-47
  FSOUND_LOADMEMORYIOP = $20000000;  // For PS2 only - "name" will be interpreted as a pointer to data for streaming and samples.  The address provided will be an IOP address

const
  FSOUND_NORMAL = (FSOUND_16BITS or FSOUND_SIGNED or FSOUND_MONO);
// [DEFINE_END]


(*
[DEFINE_START]
[
    [NAME]
    FSOUND_CDPLAYMODES

    [DESCRIPTION]
    Playback method for a CD Audio track, using FSOUND_CD_SetPlayMode

    [SEE_ALSO]
    FSOUND_CD_SetPlayMode  
    FSOUND_CD_Play
]
*)

const
  FSOUND_CD_PLAYCONTINUOUS = 0;   // Starts from the current track and plays to end of CD.
  FSOUND_CD_PLAYONCE = 1;         // Plays the specified track then stops.
  FSOUND_CD_PLAYLOOPED = 2;       // Plays the specified track looped, forever until stopped manually.
  FSOUND_CD_PLAYRANDOM = 3;       // Plays tracks in random order
// [DEFINE_END]


(*
[DEFINE_START]
[
  [NAME]
  FSOUND_CHANNELSAMPLEMODE

  [DESCRIPTION]
  Miscellaneous values for FMOD functions.

  [SEE_ALSO]
  FSOUND_PlaySound
  FSOUND_PlaySoundEx
  FSOUND_Sample_Alloc
  FSOUND_Sample_Load
  FSOUND_SetPan
]
*)

const
  FSOUND_FREE           = -1;     // value to play on any free channel, or to allocate a sample in a free sample slot.
  FSOUND_UNMANAGED      = -2;     // value to allocate a sample that is NOT managed by FSOUND or placed in a sample slot.
  FSOUND_ALL            = -3;     // for a channel index , this flag will affect ALL channels available! Not supported by every function.
  FSOUND_STEREOPAN      = -1;     // value for FSOUND_SetPan so that stereo sounds are not played at half volume. See FSOUND_SetPan for more on this.
  FSOUND_SYSTEMCHANNEL  = -1000;  // special 'channel' ID for all channel based functions that want to alter the global FSOUND software mixing output
  FSOUND_SYSTEMSAMPLE   = -1000;  // special 'sample' ID for all sample based functions that want to alter the global FSOUND software mixing output sample
// [DEFINE_END]


(*
[STRUCT_START]
[
    [NAME]
    FSOUND_REVERB_PROPERTIES

    [DESCRIPTION]
    Structure defining a reverb environment.

    [REMARKS]
    For more indepth descriptions of the reverb properties under win32, please see the EAX2/EAX3
    documentation at http://developer.creative.com/ under the 'downloads' section.
    If they do not have the EAX3 documentation, then most information can be attained from
    the EAX2 documentation, as EAX3 only adds some more parameters and functionality on top of
    EAX2.
    Note the default reverb properties are the same as the FSOUND_PRESET_GENERIC preset.
    Note that integer values that typically range from -10,000 to 1000 are represented in 
    decibels, and are of a logarithmic scale, not linear, wheras float values are typically linear.
    PORTABILITY: Each member has the platform it supports in braces ie (win32/xbox).  
    Some reverb parameters are only supported in win32 and some only on xbox. If all parameters are set then
    the reverb should product a similar effect on either platform.
    Only WIN32 supports the reverb api.

    The numerical values listed below are the maximum, minimum and default values for each variable respectively.

    [SEE_ALSO]
    FSOUND_Reverb_SetProperties
    FSOUND_Reverb_GetProperties
    FSOUND_REVERB_PRESETS
    FSOUND_REVERB_FLAGS
]
*)

type
  TFSoundReverbProperties = record          // MIN     MAX    DEFAULT DESCRIPTION
    Environment: Cardinal;                  // 0       25     0       sets all listener properties (win32 only)
    EnvSize: Single;                        // 1.0     100.0  7.5     environment size in meters (win32 only)
    EnvDiffusion: Single;                   // 0.0     1.0    1.0     environment diffusion (win32/xbox)
    Room: Integer;                          // -10000  0      -1000   room effect level (at mid frequencies) (win32/xbox)
    RoomHF: Integer;                        // -10000  0      -100    relative room effect level at high frequencies (win32/xbox)
    RoomLF: Integer;                        // -10000  0      0       relative room effect level at low frequencies (win32 only)
    DecayTime: Single;                      // 0.1     20.0   1.49    reverberation decay time at mid frequencies (win32/xbox)
    DecayHFRatio: Single;                   // 0.1     2.0    0.83    high-frequency to mid-frequency decay time ratio (win32/xbox)
    DecayLFRatio: Single;                   // 0.1     2.0    1.0     low-frequency to mid-frequency decay time ratio (win32 only)
    Reflections: Integer;                   // -10000  1000   -2602   early reflections level relative to room effect (win32/xbox)
    ReflectionsDelay: Single;               // 0.0     0.3    0.007   initial reflection delay time (win32/xbox)
    ReflectionsPan: array [0..2] of Single; //                0,0,0   early reflections panning vector (win32 only)
    Reverb: Integer;                        // -10000  2000   200     late reverberation level relative to room effect (win32/xbox)
    ReverbDelay: Single;                    // 0.0     0.1    0.011   late reverberation delay time relative to initial reflection (win32/xbox)
    ReverbPan: array [0..2] of Single;      //                0,0,0   late reverberation panning vector (win32 only)
    EchoTime: Single;                       // .075    0.25   0.25    echo time (win32 only)
    EchoDepth: Single;                      // 0.0     1.0    0.0     echo depth (win32 only)
    ModulationTime: Single;                 // 0.04    4.0    0.25    modulation time (win32 only)
    ModulationDepth: Single;                // 0.0     1.0    0.0     modulation depth (win32 only)
    AirAbsorptionHF: Single;                // -100    0.0    -5.0    change in level per meter at high frequencies (win32 only)
    HFReference: Single;                    // 1000.0  20000  5000.0  reference high frequency (hz) (win32/xbox)
    LFReference: Single;                    // 20.0    1000.0 250.0   reference low frequency (hz) (win32 only)
    RoomRolloffFactor: Single;              // 0.0     10.0   0.0     like FSOUND_3D_SetRolloffFactor but for room effect (win32/xbox)
    Diffusion: Single;                      // 0.0     100.0  100.0   Value that controls the echo density in the late reverberation decay. (xbox only)
    Density: Single;                        // 0.0     100.0  100.0   Value that controls the modal density in the late reverberation decay (xbox only)
    Flags: Cardinal;                        // FSOUND_REVERB_PROPERTYFLAGS - modifies the behavior of above properties (win32 only)
  end;
// [STRUCT_END]


(*
[DEFINE_START]
[
    [NAME]
    FSOUND_REVERB_FLAGS

    [DESCRIPTION]
    Values for the Flags member of the FSOUND_REVERB_PROPERTIES structure.

    [SEE_ALSO]
    FSOUND_REVERB_PROPERTIES
]
*)

const
  FSOUND_REVERBFLAGS_DECAYTIMESCALE         = $00000001;  // EnvironmentSize affects reverberation decay time
  FSOUND_REVERBFLAGS_REFLECTIONSSCALE       = $00000002;  // EnvironmentSize affects reflection level
  FSOUND_REVERBFLAGS_REFLECTIONSDELAYSCALE  = $00000004;  // EnvironmentSize affects initial reflection delay time
  FSOUND_REVERBFLAGS_REVERBSCALE            = $00000008;  // EnvironmentSize affects reflections level
  FSOUND_REVERBFLAGS_REVERBDELAYSCALE       = $00000010;  // EnvironmentSize affects late reverberation delay time
  FSOUND_REVERBFLAGS_DECAYHFLIMIT           = $00000020;  // AirAbsorptionHF affects DecayHFRatio
  FSOUND_REVERBFLAGS_ECHOTIMESCALE          = $00000040;  // EnvironmentSize affects echo time
  FSOUND_REVERBFLAGS_MODULATIONTIMESCALE    = $00000080;  // EnvironmentSize affects modulation time
  FSOUND_REVERB_FLAGS_CORE0                 = $00000100;  // PS2 Only - Reverb is applied to CORE0 (hw voices 0-23)
  FSOUND_REVERB_FLAGS_CORE1                 = $00000200;  // PS2 Only - Reverb is applied to CORE1 (hw voices 24-47)
  FSOUND_REVERBFLAGS_DEFAULT                = FSOUND_REVERBFLAGS_DECAYTIMESCALE or FSOUND_REVERBFLAGS_REFLECTIONSSCALE or
                                              FSOUND_REVERBFLAGS_REFLECTIONSDELAYSCALE or FSOUND_REVERBFLAGS_REVERBSCALE or
                                              FSOUND_REVERBFLAGS_REVERBDELAYSCALE or FSOUND_REVERBFLAGS_DECAYHFLIMIT or
                                              FSOUND_REVERB_FLAGS_CORE0 or FSOUND_REVERB_FLAGS_CORE1;
// [DEFINE_END]


(*
[DEFINE_START]
[
    [NAME]
    FSOUND_REVERB_PRESETS

    [DESCRIPTION]
    A set of predefined environment PARAMETERS, created by Creative Labs
    These are used to initialize an FSOUND_REVERB_PROPERTIES structure statically.
    ie 
    FSOUND_REVERB_PROPERTIES prop = FSOUND_PRESET_GENERIC;

    [SEE_ALSO]
    FSOUND_Reverb_SetProperties
]
*)
(*
const
//                                 Env  Size    Diffus  Room   RoomHF  RmLF DecTm   DecHF  DecLF   Refl  RefDel  RefPan           Revb  RevDel  ReverbPan       EchoTm  EchDp  ModTm  ModDp  AirAbs  HFRef    LFRef  RRlOff Diffus  Densty  FLAGS 
  FSOUND_PRESET_OFF              = 0,	7.5f,	1.00f, -10000, -10000, 0,   1.00f,  1.00f, 1.0f,  -2602, 0.007f, 0.0f,0.0f,0.0f,   200, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,   0.0f,   0.0f, 0x3f ;
  FSOUND_PRESET_GENERIC          = 0,	7.5f,	1.00f, -1000,  -100,   0,   1.49f,  0.83f, 1.0f,  -2602, 0.007f, 0.0f,0.0f,0.0f,   200, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_PADDEDCELL       = 1,	1.4f,	1.00f, -1000,  -6000,  0,   0.17f,  0.10f, 1.0f,  -1204, 0.001f, 0.0f,0.0f,0.0f,   207, 0.002f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_ROOM             = 2,	1.9f,	1.00f, -1000,  -454,   0,   0.40f,  0.83f, 1.0f,  -1646, 0.002f, 0.0f,0.0f,0.0f,    53, 0.003f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_BATHROOM         = 3,	1.4f,	1.00f, -1000,  -1200,  0,   1.49f,  0.54f, 1.0f,   -370, 0.007f, 0.0f,0.0f,0.0f,  1030, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f,  60.0f, 0x3f ;
  FSOUND_PRESET_LIVINGROOM       = 4,	2.5f,	1.00f, -1000,  -6000,  0,   0.50f,  0.10f, 1.0f,  -1376, 0.003f, 0.0f,0.0f,0.0f, -1104, 0.004f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_STONEROOM        = 5,	11.6f,	1.00f, -1000,  -300,   0,   2.31f,  0.64f, 1.0f,   -711, 0.012f, 0.0f,0.0f,0.0f,    83, 0.017f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_AUDITORIUM       = 6,	21.6f,	1.00f, -1000,  -476,   0,   4.32f,  0.59f, 1.0f,   -789, 0.020f, 0.0f,0.0f,0.0f,  -289, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_CONCERTHALL      = 7,	19.6f,	1.00f, -1000,  -500,   0,   3.92f,  0.70f, 1.0f,  -1230, 0.020f, 0.0f,0.0f,0.0f,    -2, 0.029f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_CAVE             = 8,	14.6f,	1.00f, -1000,  0,      0,   2.91f,  1.30f, 1.0f,   -602, 0.015f, 0.0f,0.0f,0.0f,  -302, 0.022f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_ARENA            = 9,	36.2f,	1.00f, -1000,  -698,   0,   7.24f,  0.33f, 1.0f,  -1166, 0.020f, 0.0f,0.0f,0.0f,    16, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_HANGAR           = 10,	50.3f,	1.00f, -1000,  -1000,  0,   10.05f, 0.23f, 1.0f,   -602, 0.020f, 0.0f,0.0f,0.0f,   198, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_CARPETTEDHALLWAY = 11,	1.9f,	1.00f, -1000,  -4000,  0,   0.30f,  0.10f, 1.0f,  -1831, 0.002f, 0.0f,0.0f,0.0f, -1630, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_HALLWAY          = 12,	1.8f,	1.00f, -1000,  -300,   0,   1.49f,  0.59f, 1.0f,  -1219, 0.007f, 0.0f,0.0f,0.0f,   441, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_STONECORRIDOR    = 13,	13.5f,	1.00f, -1000,  -237,   0,   2.70f,  0.79f, 1.0f,  -1214, 0.013f, 0.0f,0.0f,0.0f,   395, 0.020f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_ALLEY 	         = 14,	7.5f,	0.30f, -1000,  -270,   0,   1.49f,  0.86f, 1.0f,  -1204, 0.007f, 0.0f,0.0f,0.0f,    -4, 0.011f, 0.0f,0.0f,0.0f, 0.125f, 0.95f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_FOREST 	         = 15,	38.0f,	0.30f, -1000,  -3300,  0,   1.49f,  0.54f, 1.0f,  -2560, 0.162f, 0.0f,0.0f,0.0f,  -229, 0.088f, 0.0f,0.0f,0.0f, 0.125f, 1.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  79.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_CITY             = 16,	7.5f,	0.50f, -1000,  -800,   0,   1.49f,  0.67f, 1.0f,  -2273, 0.007f, 0.0f,0.0f,0.0f, -1691, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  50.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_MOUNTAINS        = 17,	100.0f, 0.27f, -1000,  -2500,  0,   1.49f,  0.21f, 1.0f,  -2780, 0.300f, 0.0f,0.0f,0.0f, -1434, 0.100f, 0.0f,0.0f,0.0f, 0.250f, 1.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  27.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_QUARRY           = 18,	17.5f,	1.00f, -1000,  -1000,  0,   1.49f,  0.83f, 1.0f, -10000, 0.061f, 0.0f,0.0f,0.0f,   500, 0.025f, 0.0f,0.0f,0.0f, 0.125f, 0.70f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_PLAIN            = 19,	42.5f,	0.21f, -1000,  -2000,  0,   1.49f,  0.50f, 1.0f,  -2466, 0.179f, 0.0f,0.0f,0.0f, -1926, 0.100f, 0.0f,0.0f,0.0f, 0.250f, 1.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  21.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_PARKINGLOT       = 20,	8.3f,	1.00f, -1000,  0,      0,   1.65f,  1.50f, 1.0f,  -1363, 0.008f, 0.0f,0.0f,0.0f, -1153, 0.012f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_SEWERPIPE        = 21,	1.7f,	0.80f, -1000,  -1000,  0,   2.81f,  0.14f, 1.0f,    429, 0.014f, 0.0f,0.0f,0.0f,  1023, 0.021f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  80.0f,  60.0f, 0x3f ;
  FSOUND_PRESET_UNDERWATER       = 22,	1.8f,	1.00f, -1000,  -4000,  0,   1.49f,  0.10f, 1.0f,   -449, 0.007f, 0.0f,0.0f,0.0f,  1700, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 1.18f, 0.348f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;

// Non I3DL2 presets 

  FSOUND_PRESET_DRUGGED          = 23,	1.9f,	0.50f, -1000,  0,      0,   8.39f,  1.39f, 1.0f,  -115,  0.002f, 0.0f,0.0f,0.0f,   985, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 1.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_DIZZY            = 24,	1.8f,	0.60f, -1000,  -400,   0,   17.23f, 0.56f, 1.0f,  -1713, 0.020f, 0.0f,0.0f,0.0f,  -613, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 1.00f, 0.81f, 0.310f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_PSYCHOTIC        = 25,	1.0f,	0.50f, -1000,  -151,   0,   7.56f,  0.91f, 1.0f,  -626,  0.020f, 0.0f,0.0f,0.0f,   774, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 4.00f, 1.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
*)
// [DEFINE_END]


(*
[STRUCTURE] 
[
    [DESCRIPTION]
    Structure defining the properties for a reverb source, related to a FSOUND channel.
    For more indepth descriptions of the reverb properties under win32, please see the EAX3
    documentation at http://developer.creative.com/ under the 'downloads' section.
    If they do not have the EAX3 documentation, then most information can be attained from
    the EAX2 documentation, as EAX3 only adds some more parameters and functionality on top of
    EAX2.

    Note the default reverb properties are the same as the FSOUND_PRESET_GENERIC preset.
    Note that integer values that typically range from -10,000 to 1000 are represented in 
    decibels, and are of a logarithmic scale, not linear, wheras float values are typically linear.
    PORTABILITY: Each member has the platform it supports in braces ie (win32/xbox).  
    Some reverb parameters are only supported in win32 and some only on xbox. If all parameters are set then
    the reverb should product a similar effect on either platform.
    Linux and FMODCE do not support the reverb api.

    The numerical values listed below are the maximum, minimum and default values for each variable respectively.

    [SEE_ALSO]
    FSOUND_Reverb_SetChannelProperties
    FSOUND_Reverb_GetChannelProperties
    FSOUND_REVERB_CHANNELFLAGS
]
*)
type
  TFSoundReverbChannelProperties = record   // MIN     MAX    DEFAULT
    Direct: Integer;                        // -10000  1000   0       direct path level (at low and mid frequencies) (win32/xbox)
    DirectHF: Integer;                      // -10000  0      0       relative direct path level at high frequencies (win32/xbox)
    Room: Integer;                          // -10000  1000   0       room effect level (at low and mid frequencies) (win32/xbox)
    RoomHF: Integer;                        // -10000  0      0       relative room effect level at high frequencies (win32/xbox)
    Obstruction: Integer;                   // -10000  0      0       main obstruction control (attenuation at high frequencies)  (win32/xbox)
    ObstructionLFRatio: Single;             // 0.0     1.0    0.0     obstruction low-frequency level re. main control (win32/xbox)
    Occlusion: Integer;                     // -10000  0      0       main occlusion control (attenuation at high frequencies) (win32/xbox)
    OcclusionLFRatio: Single;               // 0.0     1.0    0.25    occlusion low-frequency level re. main control (win32/xbox)
    OcclusionRoomRatio: Single;             // 0.0     10.0   1.5     relative occlusion control for room effect (win32)
    OcclusionDirectRatio: Single;           // 0.0     10.0   1.0     relative occlusion control for direct path (win32)
    Exclusion: Integer;                     // -10000  0      0       main exlusion control (attenuation at high frequencies) (win32)
    ExclusionLFRatio: Single;               // 0.0     1.0    1.0     exclusion low-frequency level re. main control (win32)
    OutsideVolumeHF: Integer;               // -10000  0      0       outside sound cone level at high frequencies (win32)
    DopplerFactor: Single;                  // 0.0     10.0   0.0     like DS3D flDopplerFactor but per source (win32)
    RolloffFactor: Single;                  // 0.0     10.0   0.0     like DS3D flRolloffFactor but per source (win32)
    RoomRolloffFactor: Single;              // 0.0     10.0   0.0     like DS3D flRolloffFactor but for room effect (win32/xbox)
    AirAbsorptionFactor: Single;            // 0.0     10.0   1.0     multiplies AirAbsorptionHF member of FSOUND_REVERB_PROPERTIES (win32)
    Flags: Integer;                         // FSOUND_REVERB_CHANNELFLAGS - modifies the behavior of properties (win32)
  end;
// [STRUCT_END]

(*
[DEFINE_START] 
[
    [NAME] 
    FSOUND_REVERB_CHANNELFLAGS
    
    [DESCRIPTION]
    Values for the Flags member of the FSOUND_REVERB_CHANNELPROPERTIES structure.

    [SEE_ALSO]
    FSOUND_REVERB_CHANNELPROPERTIES
]
*)
const
  FSOUND_REVERB_CHANNELFLAGS_DIRECTHFAUTO  = $01;  // Automatic setting of 'Direct'  due to distance from listener
  FSOUND_REVERB_CHANNELFLAGS_ROOMAUTO      = $02;  // Automatic setting of 'Room'  due to distance from listener
  FSOUND_REVERB_CHANNELFLAGS_ROOMHFAUTO    = $04;  // Automatic setting of 'RoomHF' due to distance from listener
  FSOUND_REVERB_CHANNELFLAGS_DEFAULT       = FSOUND_REVERB_CHANNELFLAGS_DIRECTHFAUTO or 
                                             FSOUND_REVERB_CHANNELFLAGS_ROOMAUTO or
                                             FSOUND_REVERB_CHANNELFLAGS_ROOMHFAUTO;
// [DEFINE_END]


(*
[ENUM] 
[
	[DESCRIPTION]
    These values are used with FSOUND_FX_Enable to enable DirectX 8 FX for a channel.

	[SEE_ALSO]
    FSOUND_FX_Enable
    FSOUND_FX_Disable
    FSOUND_FX_SetChorus
    FSOUND_FX_SetCompressor
    FSOUND_FX_SetDistortion
    FSOUND_FX_SetEcho
    FSOUND_FX_SetFlanger
    FSOUND_FX_SetGargle
    FSOUND_FX_SetI3DL2Reverb
    FSOUND_FX_SetParamEQ
    FSOUND_FX_SetWavesReverb
]
*)

type
  TFSoundFXModes = (
    FSOUND_FX_CHORUS,
    FSOUND_FX_COMPRESSOR,
    FSOUND_FX_DISTORTION,
    FSOUND_FX_ECHO,
    FSOUND_FX_FLANGER,
    FSOUND_FX_GARGLE,
    FSOUND_FX_I3DL2REVERB,
    FSOUND_FX_PARAMEQ,
    FSOUND_FX_WAVES_REVERB,
    FSOUND_FX_MAX
  );
// [DEFINE_END]


(*
[ENUM]
[
	[DESCRIPTION]
    These are speaker types defined for use with the FSOUND_SetSpeakerMode command.
    Note - Only reliably works with FSOUND_OUTPUT_DSOUND or FSOUND_OUTPUT_XBOX output modes.  Other output modes will only 
    interpret FSOUND_SPEAKERMODE_MONO and set everything else to be stereo.

	[SEE_ALSO]
    FSOUND_SetSpeakerMode

    [REMARKS]
    Note - Only reliably works with FSOUND_OUTPUT_DSOUND or FSOUND_OUTPUT_XBOX output modes.  Other output modes will only 
    interpret FSOUND_SPEAKERMODE_MONO and set everything else to be stereo.

    Using either DolbyDigital or DTS will use whatever 5.1 digital mode is available if destination hardware is unsure.
]
*)
type
  TFSoundSpeakerModes =
  (
    FSOUND_SPEAKERMODE_DOLBYDIGITAL,  // The audio is played through a speaker arrangement of surround speakers with a subwoofer.
    FSOUND_SPEAKERMODE_HEADPHONES,    // The speakers are headphones.
    FSOUND_SPEAKERMODE_MONO,          // The speakers are monaural.
    FSOUND_SPEAKERMODE_QUAD,          // The speakers are quadraphonic.
    FSOUND_SPEAKERMODE_STEREO,        // The speakers are stereo (default value).
    FSOUND_SPEAKERMODE_SURROUND,      // The speakers are surround sound.
    FSOUND_SPEAKERMODE_DTS            // The audio is played through a speaker arrangement of surround speakers with a subwoofer.
  );
  FSOUND_SPEAKERMODES = TFSoundSpeakerModes;


(*
[DEFINE_START]
[
    [NAME] 
    FSOUND_INIT_FLAGS
    
    [DESCRIPTION]
    Initialization flags.  Use them with FSOUND_Init in the flags parameter to change various behaviour.

    FSOUND_INIT_ENABLESYSTEMCHANNELFX Is an init mode which enables the FSOUND mixer buffer to be affected by DirectX 8 effects.
    Note that due to limitations of DirectSound, FSOUND_Init may fail if this is enabled because the buffersize is too small.
    This can be fixed with FSOUND_SetBufferSize.  Increase the BufferSize until it works.
    When it is enabled you can use the FSOUND_FX api, and use FSOUND_SYSTEMCHANNEL as the channel id when setting parameters.

    [SEE_ALSO]
    FSOUND_Init
]
*)
const
  FSOUND_INIT_USEDEFAULTMIDISYNTH     = $01;  // Causes MIDI playback to force software decoding.
  FSOUND_INIT_GLOBALFOCUS             = $02;  // For DirectSound output - sound is not muted when window is out of focus.
  FSOUND_INIT_ENABLESYSTEMCHANNELFX   = $04;  // For DirectSound output - Allows FSOUND_FX api to be used on global software mixer output!
  FSOUND_INIT_ACCURATEVULEVELS        = $08;  // This latency adjusts FSOUND_GetCurrentLevels, but incurs a small cpu and memory hit.
  FSOUND_INIT_PS2_DISABLECORE0REVERB  = $10;  // PS2 only - Disable reverb on CORE 0 to regain SRAM.
  FSOUND_INIT_PS2_DISABLECORE1REVERB  = $20;  // PS2 only - Disable reverb on CORE 1 to regain SRAM.
  FSOUND_INIT_PS2_SWAPDMACORES        = $40;  // PS2 only - By default FMOD uses DMA CH0 for mixing, CH1 for uploads, this flag swaps them around.
  FSOUND_INIT_DONTLATENCYADJUST       = $80;  // Callbacks are not latency adjusted, and are called at mix time.  Also information functions are immediate.
  FSOUND_INIT_GC_INITLIBS             = $100; // Gamecube only - Initializes GC audio libraries.
  FSOUND_INIT_STREAM_FROM_MAIN_THREAD = $200; // Turns off fmod streamer thread, and makes streaming update from FSOUND_Update called by the user.
  FSOUND_INIT_PS2_USEVOLUMERAMPING    = $400; // PS2 only   - Turns on volume ramping system to remove hardware clicks.
  FSOUND_INIT_DSOUND_DEFERRED         = $800; // Win32 only - For DirectSound output.  3D commands are batched together and executed at FSOUND_Update.
  FSOUND_INIT_DSOUND_HRTF_LIGHT       = $1000; // Win32 only - For DirectSound output.  FSOUND_HW3D buffers use a slightly higher quality algorithm when 3d hardware acceleration is not present.
  FSOUND_INIT_DSOUND_HRTF_FULL        = $2000; // Win32 only - For DirectSound output.  FSOUND_HW3D buffers use full quality 3d playback when 3d hardware acceleration is not present.
  FSOUND_INIT_XBOX_REMOVEHEADROOM     = $4000; // XBox only - By default directsound attenuates all sound by 6db to avoid clipping/distortion.  CAUTION.  If you use this flag you are responsible for the final mix to make sure clipping / distortion doesn't happen.
  FSOUND_INIT_PSP_SILENCEONUNDERRUN   = $8000; // PSP only - If streams skip / stutter when device is powered on, either increase stream buffersize, or use this flag instead to play silence while the UMD is recovering.

// [DEFINE_END]

(*
[ENUM]
[
    [DESCRIPTION]
    Status values for internet streams. Use FSOUND_Stream_Net_GetStatus to get the current status of an internet stream.

    [SEE_ALSO]
    FSOUND_Stream_Net_GetStatus
]
*)
type
  TFSoundStreamNetStatus =
  (
    FSOUND_STREAM_NET_NOTCONNECTED,         (* Stream hasn't connected yet *)
    FSOUND_STREAM_NET_CONNECTING,           (* Stream is connecting to remote host *)
    FSOUND_STREAM_NET_BUFFERING,            (* Stream is buffering data *)
    FSOUND_STREAM_NET_READY,                (* Stream is ready to play *)
    FSOUND_STREAM_NET_ERROR                 (* Stream has suffered a fatal error *)
  );


(*
[ENUM]
[
    [DESCRIPTION]
    Describes the type of a particular tag field.

    [SEE_ALSO]
    FSOUND_Stream_GetNumTagFields
    FSOUND_Stream_GetTagField
    FSOUND_Stream_FindTagField
]
*)
type
  TFSoundTagFieldType =
  (
    FSOUND_TAGFIELD_VORBISCOMMENT,          (* A vorbis comment *)
    FSOUND_TAGFIELD_ID3V1,                  (* Part of an ID3v1 tag *)
    FSOUND_TAGFIELD_ID3V2,                  (* An ID3v2 frame *)
    FSOUND_TAGFIELD_SHOUTCAST,              (* A SHOUTcast header line *)
    FSOUND_TAGFIELD_ICECAST,                (* An Icecast header line *)
    FSOUND_TAGFIELD_ASF                     (* An Advanced Streaming Format header line *)
  );


(*
[DEFINE_START]
[
    [NAME]
    FSOUND_STATUS_FLAGS

    [DESCRIPTION]
    These values describe the protocol and format of an internet stream. Use FSOUND_Stream_Net_GetStatus to retrieve this information for an open internet stream.

    [SEE_ALSO]
    FSOUND_Stream_Net_GetStatus
]
*)
const
  FSOUND_PROTOCOL_SHOUTCAST = $00000001;
  FSOUND_PROTOCOL_ICECAST   = $00000002;
  FSOUND_PROTOCOL_HTTP      = $00000004;
  FSOUND_FORMAT_MPEG        = $00010000;
  FSOUND_FORMAT_OGGVORBIS   = $00020000;
(* [DEFINE_END] *)

(*
[STRUCTURE] 
[
    [DESCRIPTION]
    Structure defining a CD table of contents. This structure is returned as a tag from FSOUND_Stream_FindTagField when the tag name "CD_TOC" is specified.
    Note: All tracks on the CD - including data tracks- will be represented in this structure so it's use for anything other than generating disc id information is not recommended.
    See the cdda example program for info on retrieving and using this structure.

    [SEE_ALSO]
    FSOUND_Stream_Open
    FSOUND_Stream_FindTagField
]
*)
type
  TFSoundTOCTag = record
    Name: array [0..3] of AnsiChar;             // The string "TOC" (4th character is 0), just in case this structure is accidentally treated as a string.
    NumTracks: Integer;                     // The number of tracks on the CD.
    Min: array [0..99] of Integer;          // The start offset of each track in minutes.
    Sec: array [0..99] of Integer;          // The start offset of each track in seconds.
    Frame: array [0..99] of Integer;        // The start offset of each track in frames.
  end;
// [STRUCT_END]

//----------------------------------------------
implementation
//----------------------------------------------

end.
