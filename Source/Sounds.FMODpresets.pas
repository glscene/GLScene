//
// The graphics rendering engine GLScene http://glscene.org
//
unit Sounds.FMODpresets;

(*===============================================================================================
  FMOD presets header file. Copyright (c), FireLight Technologies Pty, Ltd. 1999-2004.
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
  a symbolic link between libfmod-3.5.so and libfmod.so. This is done with
  the following command (assuming you are in /usr/lib/)...
  ln -s libfmod-3.75.so libfmod.so.
*)

interface

uses
  Sounds.FMODtypes;

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

{$J+}
const
  FSOUND_PRESET_OFF:              TFSoundReverbProperties = (Environment: 0;  EnvSize: 7.5;   EnvDiffusion: 1.00;   Room: -10000; RoomHF: -10000; RoomLF: 0;  DecayTime: 1.00;  DecayHFRatio: 1.00; DecayLFRatio: 1.0;  Reflections: -2602; ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 200;  ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 0.0;   Density: 0.0;   Flags: $33f);
  FSOUND_PRESET_GENERIC:          TFSoundReverbProperties = (Environment: 0;  EnvSize: 7.5;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -100;   RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.83; DecayLFRatio: 1.0;  Reflections: -2602; ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 200;  ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_PADDEDCELL:       TFSoundReverbProperties = (Environment: 1;  EnvSize: 1.4;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -6000;  RoomLF: 0;  DecayTime: 0.17;  DecayHFRatio: 0.10; DecayLFRatio: 1.0;  Reflections: -1204; ReflectionsDelay: 0.001;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 207;  ReverbDelay: 0.002; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_ROOM:             TFSoundReverbProperties = (Environment: 2;  EnvSize: 1.9;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -454;   RoomLF: 0;  DecayTime: 0.40;  DecayHFRatio: 0.83; DecayLFRatio: 1.0;  Reflections: -1646; ReflectionsDelay: 0.002;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 53;   ReverbDelay: 0.003; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_BATHROOM:         TFSoundReverbProperties = (Environment: 3;  EnvSize: 1.4;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -1200;  RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.54; DecayLFRatio: 1.0;  Reflections: -370;  ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 1030; ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 60.0;  Flags: $3f);
  FSOUND_PRESET_LIVINGROOM:       TFSoundReverbProperties = (Environment: 4;  EnvSize: 2.5;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -6000;  RoomLF: 0;  DecayTime: 0.50;  DecayHFRatio: 0.10; DecayLFRatio: 1.0;  Reflections: -1376; ReflectionsDelay: 0.003;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-1104; ReverbDelay: 0.004; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_STONEROOM:        TFSoundReverbProperties = (Environment: 5;  EnvSize: 11.6;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -300;   RoomLF: 0;  DecayTime: 2.31;  DecayHFRatio: 0.64; DecayLFRatio: 1.0;  Reflections: -711;  ReflectionsDelay: 0.012;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 83;   ReverbDelay: 0.017; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_AUDITORIUM:       TFSoundReverbProperties = (Environment: 6;  EnvSize: 21.6;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -476;   RoomLF: 0;  DecayTime: 4.32;  DecayHFRatio: 0.59; DecayLFRatio: 1.0;  Reflections: -789;  ReflectionsDelay: 0.020;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-289;  ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_CONCERTHALL:      TFSoundReverbProperties = (Environment: 7;  EnvSize: 19.6;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -500;   RoomLF: 0;  DecayTime: 3.92;  DecayHFRatio: 0.70; DecayLFRatio: 1.0;  Reflections: -1230; ReflectionsDelay: 0.020;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-2;    ReverbDelay: 0.029; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_CAVE:             TFSoundReverbProperties = (Environment: 8;  EnvSize: 14.6;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: 0;      RoomLF: 0;  DecayTime: 2.91;  DecayHFRatio: 1.30; DecayLFRatio: 1.0;  Reflections: -602;  ReflectionsDelay: 0.015;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-302;  ReverbDelay: 0.022; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $1f);
  FSOUND_PRESET_ARENA:            TFSoundReverbProperties = (Environment: 9;  EnvSize: 36.2;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -698;   RoomLF: 0;  DecayTime: 7.24;  DecayHFRatio: 0.33; DecayLFRatio: 1.0;  Reflections: -1166; ReflectionsDelay: 0.020;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 16;   ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_HANGAR:           TFSoundReverbProperties = (Environment: 10; EnvSize: 50.3;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -1000;  RoomLF: 0;  DecayTime: 10.05; DecayHFRatio: 0.23; DecayLFRatio: 1.0;  Reflections: -602;  ReflectionsDelay: 0.020;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 198;  ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_CARPETTEDHALLWAY: TFSoundReverbProperties = (Environment: 11; EnvSize: 1.9;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -4000;  RoomLF: 0;  DecayTime: 0.30;  DecayHFRatio: 0.10; DecayLFRatio: 1.0;  Reflections: -1831; ReflectionsDelay: 0.002;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-1630; ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_HALLWAY:          TFSoundReverbProperties = (Environment: 12; EnvSize: 1.8;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -300;   RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.59; DecayLFRatio: 1.0;  Reflections: -1219; ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 441;  ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_STONECORRIDOR:    TFSoundReverbProperties = (Environment: 13; EnvSize: 13.5;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -237;   RoomLF: 0;  DecayTime: 2.70;  DecayHFRatio: 0.79; DecayLFRatio: 1.0;  Reflections: -1214; ReflectionsDelay: 0.013;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 395;  ReverbDelay: 0.020; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_ALLEY:            TFSoundReverbProperties = (Environment: 14; EnvSize: 7.5;   EnvDiffusion: 0.30;   Room: -1000;  RoomHF: -270;   RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.86; DecayLFRatio: 1.0;  Reflections: -1204; ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-4;    ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.125;  EchoDepth: 0.95;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_FOREST:           TFSoundReverbProperties = (Environment: 15; EnvSize: 38.0;  EnvDiffusion: 0.30;   Room: -1000;  RoomHF: -3300;  RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.54; DecayLFRatio: 1.0;  Reflections: -2560; ReflectionsDelay: 0.162;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-229;  ReverbDelay: 0.088; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.125;  EchoDepth: 1.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 79.0;  Density: 100.0; Flags: $3f);
  FSOUND_PRESET_CITY:             TFSoundReverbProperties = (Environment: 16; EnvSize: 7.5;   EnvDiffusion: 0.50;   Room: -1000;  RoomHF: -800;   RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.67; DecayLFRatio: 1.0;  Reflections: -2273; ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-1691; ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 50.0;  Density: 100.0; Flags: $3f);
  FSOUND_PRESET_MOUNTAINS:        TFSoundReverbProperties = (Environment: 17; EnvSize: 100.0; EnvDiffusion: 0.27;   Room: -1000;  RoomHF: -2500;  RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.21; DecayLFRatio: 1.0;  Reflections: -2780; ReflectionsDelay: 0.300;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-1434; ReverbDelay: 0.100; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 1.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 27.0;  Density: 100.0; Flags: $1f);
  FSOUND_PRESET_QUARRY:           TFSoundReverbProperties = (Environment: 18; EnvSize: 17.5;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -1000;  RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.83; DecayLFRatio: 1.0;  Reflections: -10000;ReflectionsDelay: 0.061;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 500;  ReverbDelay: 0.025; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.125;  EchoDepth: 0.70;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_PLAIN:            TFSoundReverbProperties = (Environment: 19; EnvSize: 42.5;  EnvDiffusion: 0.21;   Room: -1000;  RoomHF: -2000;  RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.50; DecayLFRatio: 1.0;  Reflections: -2466; ReflectionsDelay: 0.179;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-1926; ReverbDelay: 0.100; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 1.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 21.0;  Density: 100.0; Flags: $3f);
  FSOUND_PRESET_PARKINGLOT:       TFSoundReverbProperties = (Environment: 20; EnvSize: 8.3;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: 0;      RoomLF: 0;  DecayTime: 1.65;  DecayHFRatio: 1.50; DecayLFRatio: 1.0;  Reflections: -1363; ReflectionsDelay: 0.008;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-1153; ReverbDelay: 0.012; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $1f);
  FSOUND_PRESET_SEWERPIPE:        TFSoundReverbProperties = (Environment: 21; EnvSize: 1.7;   EnvDiffusion: 0.80;   Room: -1000;  RoomHF: -1000;  RoomLF: 0;  DecayTime: 2.81;  DecayHFRatio: 0.14; DecayLFRatio: 1.0;  Reflections:  429;  ReflectionsDelay: 0.014;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 1023; ReverbDelay: 0.021; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 80.0;  Density: 60.0;  Flags: $3f);
  FSOUND_PRESET_UNDERWATER:       TFSoundReverbProperties = (Environment: 22; EnvSize: 1.8;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -4000;  RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.10; DecayLFRatio: 1.0;  Reflections: -449;  ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 1700; ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 1.18; ModulationDepth: 0.348; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);

(* Non I3DL2 presets *)

  FSOUND_PRESET_DRUGGED:          TFSoundReverbProperties = (Environment: 23; EnvSize: 1.9;   EnvDiffusion: 0.50;   Room: -1000;  RoomHF: 0;      RoomLF: 0;  DecayTime: 8.39;  DecayHFRatio: 1.39; DecayLFRatio: 1.0;  Reflections: -115;  ReflectionsDelay: 0.002;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 985;  ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 1.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $1f);
  FSOUND_PRESET_DIZZY:            TFSoundReverbProperties = (Environment: 24; EnvSize: 1.8;   EnvDiffusion: 0.60;   Room: -1000;  RoomHF: -400;   RoomLF: 0;  DecayTime: 17.23; DecayHFRatio: 0.56; DecayLFRatio: 1.0;  Reflections: -1713; ReflectionsDelay: 0.020;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-613;  ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 1.00;  ModulationTime: 0.81; ModulationDepth: 0.310; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $1f);
  FSOUND_PRESET_PSYCHOTIC:        TFSoundReverbProperties = (Environment: 25; EnvSize: 1.0;   EnvDiffusion: 0.50;   Room: -1000;  RoomHF: -151;   RoomLF: 0;  DecayTime: 7.56;  DecayHFRatio: 0.91; DecayLFRatio: 1.0;  Reflections: -626;  ReflectionsDelay: 0.020;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 774;  ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 4.00; ModulationDepth: 1.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $1f);

(* PlayStation 2 Only presets *)
(* Delphi/Kylix cannot create PlayStation 2 executables, so there is no need to
   convert the PlayStation 2 presets. *)
{$J-}

(* [DEFINE_END] *)

//--------------------------------------------
implementation
//--------------------------------------------

end.
