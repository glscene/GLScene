// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Sounds.FMODImport.pas' rev: 35.00 (Windows)

#ifndef Sounds_FmodimportHPP
#define Sounds_FmodimportHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Sounds.FMODtypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sounds
{
namespace Fmodimport
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
#define FMOD_DLL L"fmod64.dll"
extern DELPHI_PACKAGE bool __fastcall FMOD_Load(System::WideChar * LibName);
extern DELPHI_PACKAGE void __fastcall FMOD_Unload(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetOutput(Sounds::Fmodtypes::TFSoundOutputTypes OutputType);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetDriver(int Driver);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetMixer(Sounds::Fmodtypes::TFSoundMixerTypes Mixer);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetBufferSize(int LenMs);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetHWND(NativeUInt Hwnd);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetMinHardwareChannels(int Min);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetMaxHardwareChannels(int Max);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetMemorySystem(void * Pool, int PoolLen, Sounds::Fmodtypes::TFSoundAllocCallback UserAlloc, Sounds::Fmodtypes::TFSoundReallocCallback UserRealloc, Sounds::Fmodtypes::TFSoundFreeCallback UserFree);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Init(int MixRate, int MaxSoftwareChannels, unsigned Flags);
extern DELPHI_PACKAGE void __stdcall FSOUND_Close(void);
extern DELPHI_PACKAGE void __stdcall FSOUND_Update(void);
extern DELPHI_PACKAGE void __stdcall FSOUND_SetSpeakerMode(unsigned SpeakerMode);
extern DELPHI_PACKAGE void __stdcall FSOUND_SetSFXMasterVolume(int Volume);
extern DELPHI_PACKAGE void __stdcall FSOUND_SetPanSeperation(float PanSep);
extern DELPHI_PACKAGE Sounds::Fmodtypes::TFModErrors __stdcall FSOUND_GetError(void);
extern DELPHI_PACKAGE float __stdcall FSOUND_GetVersion(void);
extern DELPHI_PACKAGE Sounds::Fmodtypes::TFSoundOutputTypes __stdcall FSOUND_GetOutput(void);
extern DELPHI_PACKAGE void * __stdcall FSOUND_GetOutputHandle(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetDriver(void);
extern DELPHI_PACKAGE Sounds::Fmodtypes::TFSoundMixerTypes __stdcall FSOUND_GetMixer(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetNumDrivers(void);
extern DELPHI_PACKAGE char * __stdcall FSOUND_GetDriverName(int Id);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetDriverCaps(int Id, unsigned &Caps);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetOutputRate(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetMaxChannels(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetMaxSamples(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetSFXMasterVolume(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetNumHWChannels(int &num2d, int &num3d, int &total);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetChannelsPlaying(void);
extern DELPHI_PACKAGE float __stdcall FSOUND_GetCPUUsage(void);
extern DELPHI_PACKAGE void __stdcall FSOUND_GetMemoryStats(unsigned &CurrentAlloced, unsigned &MaxAlloced);
extern DELPHI_PACKAGE void * __stdcall FSOUND_Sample_Load(int Index, const char * NameOrData, unsigned Mode, int Offset, int Length);
extern DELPHI_PACKAGE void * __stdcall FSOUND_Sample_Alloc(int Index, int Length, unsigned Mode, int DefFreq, int DefVol, int DefPan, int DefPri);
extern DELPHI_PACKAGE void __stdcall FSOUND_Sample_Free(void * Sptr);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_Upload(void * Sptr, void * SrcData, unsigned Mode);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_Lock(void * Sptr, int Offset, int Length, void * &Ptr1, void * &Ptr2, unsigned &Len1, unsigned &Len2);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_Unlock(void * Sptr, void * Ptr1, void * Ptr2, unsigned Len1, unsigned Len2);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_SetMode(void * Sptr, unsigned Mode);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_SetLoopPoints(void * Sptr, int LoopStart, int LoopEnd);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_SetDefaults(void * Sptr, int DefFreq, int DefVol, int DefPan, int DefPri);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_SetDefaultsEx(void * Sptr, int DefFreq, int DefVol, int DefPan, int DefPri, int VarFreq, int VarVol, int VarPan);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_SetMinMaxDistance(void * Sptr, float Min, float Max);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_SetMaxPlaybacks(void * Sptr, int Max);
extern DELPHI_PACKAGE void * __stdcall FSOUND_Sample_Get(int SampNo);
extern DELPHI_PACKAGE char * __stdcall FSOUND_Sample_GetName(void * Sptr);
extern DELPHI_PACKAGE unsigned __stdcall FSOUND_Sample_GetLength(void * Sptr);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_GetLoopPoints(void * Sptr, int &LoopStart, int &LoopEnd);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_GetDefaults(void * Sptr, int &DefFreq, int &DefVol, int &DefPan, int &DefPri);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_GetDefaultsEx(void * Sptr, int &DefFreq, int &DefVol, int &DefPan, int &DefPri, int &VarFreq, int &VarVol, int &VarPan);
extern DELPHI_PACKAGE unsigned __stdcall FSOUND_Sample_GetMode(void * Sptr);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_GetMinMaxDistance(void * Sptr, float &Min, float &Max);
extern DELPHI_PACKAGE int __stdcall FSOUND_PlaySound(int Channel, void * Sptr);
extern DELPHI_PACKAGE int __stdcall FSOUND_PlaySoundEx(int Channel, void * Sptr, void * Dsp, System::ByteBool StartPaused);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_StopSound(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetFrequency(int Channel, int Freq);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetVolume(int Channel, int Vol);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetVolumeAbsolute(int Channel, int Vol);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetPan(int Channel, int Pan);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetSurround(int Channel, System::ByteBool Surround);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetMute(int Channel, System::ByteBool Mute);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetPriority(int Channel, int Priority);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetReserved(int Channel, System::ByteBool Reserved);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetPaused(int Channel, System::ByteBool Paused);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetLoopMode(int Channel, unsigned LoopMode);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetCurrentPosition(int Channel, unsigned Offset);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_3D_SetAttributes(int Channel, Sounds::Fmodtypes::PFSoundVector Pos, Sounds::Fmodtypes::PFSoundVector Vel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_3D_SetMinMaxDistance(int Channel, float Min, float Max);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_IsPlaying(int Channel);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetFrequency(int Channel);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetVolume(int Channel);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetAmplitude(int Channel);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetPan(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetSurround(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetMute(int Channel);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetPriority(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetReserved(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetPaused(int Channel);
extern DELPHI_PACKAGE unsigned __stdcall FSOUND_GetLoopMode(int Channel);
extern DELPHI_PACKAGE unsigned __stdcall FSOUND_GetCurrentPosition(int Channel);
extern DELPHI_PACKAGE void * __stdcall FSOUND_GetCurrentSample(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetCurrentLevels(int Channel, Winapi::Windows::PSingle l, Winapi::Windows::PSingle r);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetNumSubChannels(int Channel);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetSubChannel(int Channel, int SubChannel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_3D_GetAttributes(int Channel, Sounds::Fmodtypes::PFSoundVector Pos, Sounds::Fmodtypes::PFSoundVector Vel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_3D_GetMinMaxDistance(int Channel, float &Min, float &Max);
extern DELPHI_PACKAGE void __stdcall FSOUND_3D_Listener_SetCurrent(int current);
extern DELPHI_PACKAGE void __stdcall FSOUND_3D_Listener_SetAttributes(Sounds::Fmodtypes::PFSoundVector Pos, Sounds::Fmodtypes::PFSoundVector Vel, float fx, float fy, float fz, float tx, float ty, float tz);
extern DELPHI_PACKAGE void __stdcall FSOUND_3D_Listener_GetAttributes(Sounds::Fmodtypes::PFSoundVector Pos, Sounds::Fmodtypes::PFSoundVector Vel, Winapi::Windows::PSingle fx, Winapi::Windows::PSingle fy, Winapi::Windows::PSingle fz, Winapi::Windows::PSingle tx, Winapi::Windows::PSingle ty, Winapi::Windows::PSingle tz);
extern DELPHI_PACKAGE void __stdcall FSOUND_3D_SetDopplerFactor(float Scale);
extern DELPHI_PACKAGE void __stdcall FSOUND_3D_SetDistanceFactor(float Scale);
extern DELPHI_PACKAGE void __stdcall FSOUND_3D_SetRolloffFactor(float Scale);
extern DELPHI_PACKAGE int __stdcall FSOUND_FX_Enable(int Channel, Sounds::Fmodtypes::TFSoundFXModes Fx);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_Disable(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetChorus(int FXId, float WetDryMix, float Depth, float Feedback, float Frequency, int Waveform, float Delay, int Phase);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetCompressor(int FXId, float Gain, float Attack, float Release, float Threshold, float Ratio, float Predelay);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetDistortion(int FXId, float Gain, float Edge, float PostEQCenterFrequency, float PostEQBandwidth, float PreLowpassCutoff);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetEcho(int FXId, float WetDryMix, float Feedback, float LeftDelay, float RightDelay, int PanDelay);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetFlanger(int FXId, float WetDryMix, float Depth, float Feedback, float Frequency, int Waveform, float Delay, int Phase);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetGargle(int FXId, int RateHz, int WaveShape);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetI3DL2Reverb(int FXId, int Room, int RoomHF, float RoomRolloffFactor, float DecayTime, float DecayHFRatio, int Reflections, float ReflectionsDelay, int Reverb, float ReverbDelay, float Diffusion, float Density, float HFReference);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetParamEQ(int FXId, float Center, float Bandwidth, float Gain);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetWavesReverb(int FXId, float InGain, float ReverbMix, float ReverbTime, float HighFreqRTRatio);
extern DELPHI_PACKAGE void * __stdcall FSOUND_Stream_Open(const char * name_or_data, unsigned Mode, int Offset, int Length);
extern DELPHI_PACKAGE void * __stdcall FSOUND_Stream_Create(Sounds::Fmodtypes::TFSoundStreamCallback Callback, int Length, unsigned Mode, int SampleRate, int UserData);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_Play(int Channel, void * Stream);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_PlayEx(int Channel, void * Stream, void * Dsp, System::ByteBool StartPaused);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Stop(void * Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Close(void * Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetEndCallback(void * Stream, Sounds::Fmodtypes::TFSoundStreamCallback Callback, int UserData);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetSyncCallback(void * Stream, Sounds::Fmodtypes::TFSoundStreamCallback Callback, int UserData);
extern DELPHI_PACKAGE void * __stdcall FSOUND_Stream_GetSample(void * Stream);
extern DELPHI_PACKAGE void * __stdcall FSOUND_Stream_CreateDSP(void * Stream, Sounds::Fmodtypes::TFSoundDSPCallback Callback, int Priority, int Param);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetBufferSize(int Ms);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetPosition(void * Stream, unsigned Position);
extern DELPHI_PACKAGE unsigned __stdcall FSOUND_Stream_GetPosition(void * Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetTime(void * Stream, int Ms);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetTime(void * Stream);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetLength(void * Stream);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetLengthMs(void * Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetMode(void * Stream, int mode);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetMode(void * Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetLoopPoints(void * Stream, int LoopStartPCM, int LoopEndPCM);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetLoopCount(void * Stream, int Count);
extern DELPHI_PACKAGE void * __stdcall FSOUND_Stream_AddSyncPoint(void * Stream, unsigned PCMOffset, char * Name);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_DeleteSyncPoint(void * Point);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetNumSyncPoints(void * Stream);
extern DELPHI_PACKAGE void * __stdcall FSOUND_Stream_GetSyncPoint(void * Stream, int Index);
extern DELPHI_PACKAGE char * __stdcall FSOUND_Stream_GetSyncPointInfo(void * Point, unsigned &PCMOffset);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetOpenState(void * Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetSubStream(void * Stream, int Index);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetNumSubStreams(void * Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetSubStreamSentence(void * Stream, unsigned &SentenceList, int NumItems);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_GetNumTagFields(void * Stream, int &Num);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_GetTagField(void * Stream, int Num, Sounds::Fmodtypes::TFSoundTagFieldType &TagType, char * &Name, void * &Value, int &Length);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_FindTagField(void * Stream, Sounds::Fmodtypes::TFSoundTagFieldType TagType, char * Name, void * &Value, int &Length);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Net_SetProxy(char * Proxy);
extern DELPHI_PACKAGE char * __stdcall FSOUND_Stream_Net_GetLastServerStatus(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Net_SetBufferProperties(int BufferSize, int PreBuffer_Percent, int ReBuffer_Percent);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Net_GetBufferProperties(int &Buffersize, int &PreBuffer_Percent, int &ReBuffer_Percent);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Net_SetMetadataCallback(void * Stream, Sounds::Fmodtypes::TFMetaDataCallback Callback, int UserData);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Net_GetStatus(void * Stream, Sounds::Fmodtypes::TFSoundStreamNetStatus &Status, int &BufferPercentUsed, int &BitRate, unsigned &Flags);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_Play(System::Byte Drive, int Track);
extern DELPHI_PACKAGE void __stdcall FSOUND_CD_SetPlayMode(System::Byte Drive, int Mode);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_Stop(System::Byte Drive);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_SetPaused(System::Byte Drive, System::ByteBool Paused);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_SetVolume(System::Byte Drive, int Volume);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_SetTrackTime(System::Byte Drive, int ms);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_OpenTray(System::Byte Drive, System::Byte Open);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_GetPaused(System::Byte Drive);
extern DELPHI_PACKAGE int __stdcall FSOUND_CD_GetTrack(System::Byte Drive);
extern DELPHI_PACKAGE int __stdcall FSOUND_CD_GetNumTracks(System::Byte Drive);
extern DELPHI_PACKAGE int __stdcall FSOUND_CD_GetVolume(System::Byte Drive);
extern DELPHI_PACKAGE int __stdcall FSOUND_CD_GetTrackLength(System::Byte Drive, int Track);
extern DELPHI_PACKAGE int __stdcall FSOUND_CD_GetTrackTime(System::Byte Drive);
extern DELPHI_PACKAGE void * __stdcall FSOUND_DSP_Create(Sounds::Fmodtypes::TFSoundDSPCallback Callback, int Priority, int Param);
extern DELPHI_PACKAGE void __stdcall FSOUND_DSP_Free(void * DSPUnit);
extern DELPHI_PACKAGE void __stdcall FSOUND_DSP_SetPriority(void * DSPUnit, int Priority);
extern DELPHI_PACKAGE int __stdcall FSOUND_DSP_GetPriority(void * DSPUnit);
extern DELPHI_PACKAGE void __stdcall FSOUND_DSP_SetActive(void * DSPUnit, System::ByteBool Active);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_DSP_GetActive(void * DSPUnit);
extern DELPHI_PACKAGE void * __stdcall FSOUND_DSP_GetClearUnit(void);
extern DELPHI_PACKAGE void * __stdcall FSOUND_DSP_GetSFXUnit(void);
extern DELPHI_PACKAGE void * __stdcall FSOUND_DSP_GetMusicUnit(void);
extern DELPHI_PACKAGE void * __stdcall FSOUND_DSP_GetClipAndCopyUnit(void);
extern DELPHI_PACKAGE void * __stdcall FSOUND_DSP_GetFFTUnit(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_DSP_MixBuffers(void * DestBuffer, void * SrcBuffer, int Len, int Freq, int Vol, int Pan, unsigned Mode);
extern DELPHI_PACKAGE void __stdcall FSOUND_DSP_ClearMixBuffer(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_DSP_GetBufferLength(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_DSP_GetBufferLengthTotal(void);
extern DELPHI_PACKAGE Winapi::Windows::PSingle __stdcall FSOUND_DSP_GetSpectrum(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Reverb_SetProperties(Sounds::Fmodtypes::TFSoundReverbProperties &Prop);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Reverb_GetProperties(Sounds::Fmodtypes::TFSoundReverbProperties &Prop);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Reverb_SetChannelProperties(int Channel, Sounds::Fmodtypes::TFSoundReverbChannelProperties &Prop);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Reverb_GetChannelProperties(int Channel, Sounds::Fmodtypes::TFSoundReverbChannelProperties &Prop);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Record_SetDriver(int OutputType);
extern DELPHI_PACKAGE int __stdcall FSOUND_Record_GetNumDrivers(void);
extern DELPHI_PACKAGE char * __stdcall FSOUND_Record_GetDriverName(int Id);
extern DELPHI_PACKAGE int __stdcall FSOUND_Record_GetDriver(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Record_StartSample(void * Sptr, System::ByteBool Loop);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Record_Stop(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_Record_GetPosition(void);
extern DELPHI_PACKAGE void __stdcall FSOUND_File_SetCallbacks(Sounds::Fmodtypes::TFSoundOpenCallback OpenCallback, Sounds::Fmodtypes::TFSoundCloseCallback CloseCallback, Sounds::Fmodtypes::TFSoundReadCallback ReadCallback, Sounds::Fmodtypes::TFSoundSeekCallback SeekCallback, Sounds::Fmodtypes::TFSoundTellCallback TellCallback);
extern DELPHI_PACKAGE void * __stdcall FMUSIC_LoadSong(const char * Name);
extern DELPHI_PACKAGE void * __stdcall FMUSIC_LoadSongEx(void * Name_Or_Data, int Offset, int Length, unsigned Mode, int &SampleList, int SampleListNum);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetOpenState(void * Module);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_FreeSong(void * Module);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_PlaySong(void * Module);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_StopSong(void * Module);
extern DELPHI_PACKAGE void __stdcall FMUSIC_StopAllSongs(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetZxxCallback(void * Module, Sounds::Fmodtypes::TFMusicCallback Callback);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetRowCallback(void * Module, Sounds::Fmodtypes::TFMusicCallback Callback, int RowStep);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetOrderCallback(void * Module, Sounds::Fmodtypes::TFMusicCallback Callback, int OrderStep);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetInstCallback(void * Module, Sounds::Fmodtypes::TFMusicCallback Callback, int Instrument);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetSample(void * Module, int SampNo, void * Sptr);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetUserData(void * Module, int userdata);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_OptimizeChannels(void * Module, int MaxChannels, int MinVolume);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetReverb(System::ByteBool Reverb);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetLooping(void * Module, System::ByteBool Looping);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetOrder(void * Module, int Order);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetPaused(void * Module, System::ByteBool Pause);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetMasterVolume(void * Module, int Volume);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetMasterSpeed(void * Module, float speed);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetPanSeperation(void * Module, float PanSep);
extern DELPHI_PACKAGE char * __stdcall FMUSIC_GetName(void * Module);
extern DELPHI_PACKAGE Sounds::Fmodtypes::TFMusicTypes __stdcall FMUSIC_GetType(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetNumOrders(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetNumPatterns(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetNumInstruments(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetNumSamples(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetNumChannels(void * Module);
extern DELPHI_PACKAGE void * __stdcall FMUSIC_GetSample(void * Module, int SampNo);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetPatternLength(void * Module, int OrderNo);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_IsFinished(void * Module);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_IsPlaying(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetMasterVolume(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetGlobalVolume(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetOrder(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetPattern(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetSpeed(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetBPM(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetRow(void * Module);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_GetPaused(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetTime(void * Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetRealChannel(void * Module, int modchannel);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetUserData(void * Module);
}	/* namespace Fmodimport */
}	/* namespace Sounds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SOUNDS_FMODIMPORT)
using namespace Sounds::Fmodimport;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SOUNDS)
using namespace Sounds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sounds_FmodimportHPP
