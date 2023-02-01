// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.SoundManager.pas' rev: 35.00 (Windows)

#ifndef Gls_SoundmanagerHPP
#define Gls_SoundmanagerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.SoundFileObjects.hpp>
#include <GLS.Scene.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Cadencer.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.FileMP3.hpp>
#include <GLS.FileWAV.hpp>
#include <GLS.Utils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Soundmanager
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSoundSample;
class DELPHICLASS TGLSoundSamples;
class DELPHICLASS TGLSoundLibrary;
class DELPHICLASS TGLBaseSoundSource;
class DELPHICLASS TGLSoundSource;
class DELPHICLASS TGLSoundSources;
class DELPHICLASS TGLSoundManager;
class DELPHICLASS TGLBSoundEmitter;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSoundSample : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	Gls::Soundfileobjects::TGLSoundFile* FData;
	int FTag;
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual void __fastcall ReadData(System::Classes::TStream* Stream);
	virtual void __fastcall WriteData(System::Classes::TStream* Stream);
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetData(Gls::Soundfileobjects::TGLSoundFile* const val);
	
public:
	__fastcall virtual TGLSoundSample(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLSoundSample();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall LoadFromFile(const System::UnicodeString fileName);
	void __fastcall PlayOnWaveOut();
	Gls::Soundfileobjects::TGLSoundSampling* __fastcall Sampling();
	int __fastcall LengthInBytes();
	int __fastcall LengthInSamples();
	float __fastcall LengthInSec();
	__property int ManagerTag = {read=FTag, write=FTag, nodefault};
	
__published:
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property Gls::Soundfileobjects::TGLSoundFile* Data = {read=FData, write=SetData, stored=false};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSoundSamples : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLSoundSample* operator[](int index) { return this->Items[index]; }
	
protected:
	System::Classes::TComponent* owner;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	void __fastcall SetItems(int index, TGLSoundSample* const val);
	TGLSoundSample* __fastcall GetItems(int index);
	
public:
	__fastcall TGLSoundSamples(System::Classes::TComponent* AOwner);
	HIDESBASE TGLSoundSample* __fastcall Add();
	HIDESBASE TGLSoundSample* __fastcall FindItemID(int ID);
	__property TGLSoundSample* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	TGLSoundSample* __fastcall GetByName(const System::UnicodeString aName);
	TGLSoundSample* __fastcall AddFile(const System::UnicodeString fileName, const System::UnicodeString sampleName = System::UnicodeString());
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLSoundSamples() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLSoundLibrary : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TGLSoundSamples* FSamples;
	
protected:
	void __fastcall SetSamples(TGLSoundSamples* const val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLSoundLibrary(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSoundLibrary();
	
__published:
	__property TGLSoundSamples* Samples = {read=FSamples, write=SetSamples};
};


enum DECLSPEC_DENUM TGLSoundSourceChange : unsigned char { sscTransformation, sscSample, sscStatus };

typedef System::Set<TGLSoundSourceChange, TGLSoundSourceChange::sscTransformation, TGLSoundSourceChange::sscStatus> TGLSoundSourceChanges;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseSoundSource : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLBSoundEmitter* FBehaviourToNotify;
	int FPriority;
	Gls::Scene::TGLBaseSceneObject* FOrigin;
	float FVolume;
	float FMinDistance;
	float FMaxDistance;
	float FInsideConeAngle;
	float FOutsideConeAngle;
	float FConeOutsideVolume;
	System::UnicodeString FSoundLibraryName;
	TGLSoundLibrary* FSoundLibrary;
	System::UnicodeString FSoundName;
	bool FMute;
	bool FPause;
	TGLSoundSourceChanges FChanges;
	int FNbLoops;
	unsigned FTag;
	int FFrequency;
	
protected:
	void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetPriority(const int val);
	void __fastcall SetOrigin(Gls::Scene::TGLBaseSceneObject* const val);
	void __fastcall SetVolume(const float val);
	void __fastcall SetMinDistance(const float val);
	void __fastcall SetMaxDistance(const float val);
	void __fastcall SetInsideConeAngle(const float val);
	void __fastcall SetOutsideConeAngle(const float val);
	void __fastcall SetConeOutsideVolume(const float val);
	TGLSoundLibrary* __fastcall GetSoundLibrary();
	void __fastcall SetSoundLibrary(TGLSoundLibrary* const val);
	void __fastcall SetSoundName(const System::UnicodeString val);
	void __fastcall SetMute(const bool val);
	void __fastcall SetPause(const bool val);
	void __fastcall SetNbLoops(const int val);
	void __fastcall SetFrequency(const int val);
	
public:
	__fastcall virtual TGLBaseSoundSource(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLBaseSoundSource();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property TGLSoundSourceChanges Changes = {read=FChanges, nodefault};
	TGLSoundSample* __fastcall Sample();
	__property unsigned ManagerTag = {read=FTag, write=FTag, nodefault};
	__property Gls::Scene::TGLBaseSceneObject* Origin = {read=FOrigin, write=SetOrigin};
	
__published:
	__property TGLSoundLibrary* SoundLibrary = {read=GetSoundLibrary, write=SetSoundLibrary};
	__property System::UnicodeString SoundName = {read=FSoundName, write=SetSoundName};
	__property float Volume = {read=FVolume, write=SetVolume};
	__property int NbLoops = {read=FNbLoops, write=SetNbLoops, default=1};
	__property bool Mute = {read=FMute, write=SetMute, default=0};
	__property bool Pause = {read=FPause, write=SetPause, default=0};
	__property int Priority = {read=FPriority, write=SetPriority, default=0};
	__property float MinDistance = {read=FMinDistance, write=SetMinDistance};
	__property float MaxDistance = {read=FMaxDistance, write=SetMaxDistance};
	__property float InsideConeAngle = {read=FInsideConeAngle, write=SetInsideConeAngle};
	__property float OutsideConeAngle = {read=FOutsideConeAngle, write=SetOutsideConeAngle};
	__property float ConeOutsideVolume = {read=FConeOutsideVolume, write=SetConeOutsideVolume};
	__property int Frequency = {read=FFrequency, write=SetFrequency, default=-1};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSoundSource : public TGLBaseSoundSource
{
	typedef TGLBaseSoundSource inherited;
	
public:
	__fastcall virtual ~TGLSoundSource();
	
__published:
	__property Origin;
public:
	/* TGLBaseSoundSource.Create */ inline __fastcall virtual TGLSoundSource(System::Classes::TCollection* Collection) : TGLBaseSoundSource(Collection) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSoundSources : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLSoundSource* operator[](int index) { return this->Items[index]; }
	
protected:
	System::Classes::TComponent* owner;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	void __fastcall SetItems(int index, TGLSoundSource* const val);
	TGLSoundSource* __fastcall GetItems(int index);
	HIDESBASE TGLSoundSource* __fastcall Add();
	HIDESBASE TGLSoundSource* __fastcall FindItemID(int ID);
	
public:
	__fastcall TGLSoundSources(System::Classes::TComponent* AOwner);
	__property TGLSoundSource* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLSoundSources() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TGLSoundEnvironment : unsigned char { seDefault, sePaddedCell, seRoom, seBathroom, seLivingRoom, seStoneroom, seAuditorium, seConcertHall, seCave, seArena, seHangar, seCarpetedHallway, seHallway, seStoneCorridor, seAlley, seForest, seCity, seMountains, seQuarry, sePlain, seParkingLot, seSewerPipe, seUnderWater, seDrugged, seDizzy, sePsychotic };

class PASCALIMPLEMENTATION TGLSoundManager : public Gls::Baseclasses::TGLCadenceAbleComponent
{
	typedef Gls::Baseclasses::TGLCadenceAbleComponent inherited;
	
private:
	bool FActive;
	bool FMute;
	bool FPause;
	float FMasterVolume;
	Gls::Scene::TGLBaseSceneObject* FListener;
	Gls::Vectortypes::TVector4f FLastListenerPosition;
	TGLSoundSources* FSources;
	int FMaxChannels;
	int FOutputFrequency;
	float FUpdateFrequency;
	float FDistanceFactor;
	float FRollOffFactor;
	float FDopplerFactor;
	TGLSoundEnvironment FSoundEnvironment;
	float FLastUpdateTime;
	float FLastDeltaTime;
	Gls::Cadencer::TGLCadencer* FCadencer;
	void __fastcall SetActive(const bool val);
	void __fastcall SetMute(const bool val);
	void __fastcall SetPause(const bool val);
	void __fastcall WriteDoppler(System::Classes::TWriter* writer);
	void __fastcall ReadDoppler(System::Classes::TReader* reader);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetSources(TGLSoundSources* const val);
	void __fastcall SetMasterVolume(const float val);
	void __fastcall SetListener(Gls::Scene::TGLBaseSceneObject* const val);
	void __fastcall SetMaxChannels(const int val);
	void __fastcall SetOutputFrequency(const int val);
	void __fastcall SetUpdateFrequency(const float val);
	bool __fastcall StoreUpdateFrequency();
	void __fastcall SetCadencer(Gls::Cadencer::TGLCadencer* const val);
	void __fastcall SetDistanceFactor(const float val);
	bool __fastcall StoreDistanceFactor();
	void __fastcall SetRollOffFactor(const float val);
	bool __fastcall StoreRollOffFactor();
	void __fastcall SetDopplerFactor(const float val);
	void __fastcall SetSoundEnvironment(const TGLSoundEnvironment val);
	virtual void __fastcall Loaded();
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ListenerCoordinates(Gls::Vectortypes::TVector4f &position, Gls::Vectortypes::TVector4f &velocity, Gls::Vectortypes::TVector4f &direction, Gls::Vectortypes::TVector4f &up);
	virtual bool __fastcall DoActivate();
	virtual void __fastcall DoDeActivate();
	virtual bool __fastcall DoMute();
	virtual void __fastcall DoUnMute();
	virtual bool __fastcall DoPause();
	virtual void __fastcall DoUnPause();
	virtual void __fastcall NotifyMasterVolumeChange();
	virtual void __fastcall Notify3DFactorsChanged();
	virtual void __fastcall NotifyEnvironmentChanged();
	virtual void __fastcall KillSource(TGLBaseSoundSource* aSource);
	virtual void __fastcall UpdateSource(TGLBaseSoundSource* aSource);
	virtual void __fastcall MuteSource(TGLBaseSoundSource* aSource, bool muted);
	virtual void __fastcall PauseSource(TGLBaseSoundSource* aSource, bool paused);
	
public:
	__fastcall virtual TGLSoundManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSoundManager();
	virtual void __fastcall UpdateSources();
	void __fastcall StopAllSources();
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	virtual float __fastcall CPUUsagePercent();
	virtual bool __fastcall EAXSupported();
	
__published:
	__property bool Active = {read=FActive, write=SetActive, default=0};
	__property int MaxChannels = {read=FMaxChannels, write=SetMaxChannels, default=8};
	__property int OutputFrequency = {read=FOutputFrequency, write=SetOutputFrequency, default=44100};
	__property bool Mute = {read=FMute, write=SetMute, default=0};
	__property bool Pause = {read=FPause, write=SetPause, default=0};
	__property float MasterVolume = {read=FMasterVolume, write=SetMasterVolume};
	__property Gls::Scene::TGLBaseSceneObject* Listener = {read=FListener, write=SetListener};
	__property TGLSoundSources* Sources = {read=FSources, write=SetSources};
	__property float UpdateFrequency = {read=FUpdateFrequency, write=SetUpdateFrequency, stored=StoreUpdateFrequency};
	__property Gls::Cadencer::TGLCadencer* Cadencer = {read=FCadencer, write=SetCadencer};
	__property float DistanceFactor = {read=FDistanceFactor, write=SetDistanceFactor, stored=StoreDistanceFactor};
	__property float RollOffFactor = {read=FRollOffFactor, write=SetRollOffFactor, stored=StoreRollOffFactor};
	__property float DopplerFactor = {read=FDopplerFactor, write=SetDopplerFactor, stored=false};
	__property TGLSoundEnvironment Environment = {read=FSoundEnvironment, write=SetSoundEnvironment, default=0};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBSoundEmitter : public Gls::Scene::TGLBehaviour
{
	typedef Gls::Scene::TGLBehaviour inherited;
	
private:
	bool FPlaying;
	TGLBaseSoundSource* FSource;
	TGLSoundSource* FPlayingSource;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	void __fastcall SetSource(TGLBaseSoundSource* const val);
	void __fastcall SetPlaying(const bool val);
	bool __fastcall GetPlaying();
	void __fastcall NotifySourceDestruction(TGLSoundSource* aSource);
	
public:
	__fastcall virtual TGLBSoundEmitter(Gls::Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLBSoundEmitter();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	__property TGLSoundSource* PlayingSource = {read=FPlayingSource};
	
__published:
	__property TGLBaseSoundSource* Source = {read=FSource, write=SetSource};
	__property bool Playing = {read=GetPlaying, write=SetPlaying, default=0};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool vVerboseGLSMErrors;
extern DELPHI_PACKAGE TGLSoundManager* __fastcall ActiveSoundManager(void);
extern DELPHI_PACKAGE TGLSoundLibrary* __fastcall GetSoundLibraryByName(const System::UnicodeString aName);
extern DELPHI_PACKAGE TGLBSoundEmitter* __fastcall GetOrCreateSoundEmitter(Gls::Scene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLBSoundEmitter* __fastcall GetOrCreateSoundEmitter(Gls::Scene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Soundmanager */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SOUNDMANAGER)
using namespace Gls::Soundmanager;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_SoundmanagerHPP
