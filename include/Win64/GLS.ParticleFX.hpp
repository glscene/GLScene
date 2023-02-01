// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.ParticleFX.pas' rev: 35.00 (Windows)

#ifndef Gls_ParticlefxHPP
#define Gls_ParticlefxHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <System.Math.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Utils.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.State.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.Material.hpp>
#include <GLS.Cadencer.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.Context.hpp>
#include <GLS.Color.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Manager.hpp>
#include <GLS.TextureFormat.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Particlefx
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLParticle;
class DELPHICLASS TGLParticleList;
class DELPHICLASS TGLParticleFXManager;
class DELPHICLASS TGLParticleFXEffect;
struct TParticleReference;
struct TPFXRegion;
class DELPHICLASS TGLParticleFXRenderer;
class DELPHICLASS TGLSourcePFXEffect;
class DELPHICLASS TGLDynamicPFXManager;
class DELPHICLASS TPFXLifeColor;
class DELPHICLASS TPFXLifeColors;
class DELPHICLASS TGLLifeColoredPFXManager;
class DELPHICLASS TGLCustomPFXManager;
class DELPHICLASS TGLPolygonPFXManager;
class DELPHICLASS TGLBaseSpritePFXManager;
class DELPHICLASS TGLCustomSpritePFXManager;
class DELPHICLASS TGLPointLightPFXManager;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLParticle : public Gls::Persistentclasses::TGLPersistentObject
{
	typedef Gls::Persistentclasses::TGLPersistentObject inherited;
	
private:
	int FID;
	int FTag;
	TGLParticleFXManager* FManager;
	Gls::Vectortypes::TVector3f FPosition;
	Gls::Vectortypes::TVector3f FVelocity;
	float FRotation;
	double FCreationTime;
	float FEffectScale;
	float __fastcall GetPosition(const int Index);
	void __fastcall WritePosition(const int Index, const float aValue);
	float __fastcall GetVelocity(const int Index);
	void __fastcall WriteVelocity(const int Index, const float aValue);
	
public:
	__fastcall virtual TGLParticle();
	__fastcall virtual ~TGLParticle();
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* reader);
	__property TGLParticleFXManager* Manager = {read=FManager, write=FManager};
	__property int ID = {read=FID, nodefault};
	__property Gls::Vectortypes::TVector3f Position = {read=FPosition, write=FPosition};
	__property Gls::Vectortypes::TVector3f Velocity = {read=FVelocity, write=FVelocity};
	__property double CreationTime = {read=FCreationTime, write=FCreationTime};
	__property float PosX = {read=GetPosition, write=WritePosition, index=0};
	__property float PosY = {read=GetPosition, write=WritePosition, index=1};
	__property float PosZ = {read=GetPosition, write=WritePosition, index=2};
	__property float VelX = {read=GetVelocity, write=WriteVelocity, index=0};
	__property float VelY = {read=GetVelocity, write=WriteVelocity, index=1};
	__property float VelZ = {read=GetVelocity, write=WriteVelocity, index=2};
	__property int Tag = {read=FTag, write=FTag, nodefault};
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLParticle(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Persistentclasses::TGLPersistentObject(reader) { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TGLParticleClass);

typedef System::StaticArray<TGLParticle*, 134217728> TGLParticleArray;

typedef TGLParticleArray *PGLParticleArray;

class PASCALIMPLEMENTATION TGLParticleList : public Gls::Persistentclasses::TGLPersistentObject
{
	typedef Gls::Persistentclasses::TGLPersistentObject inherited;
	
public:
	TGLParticle* operator[](int index) { return this->Items[index]; }
	
private:
	TGLParticleFXManager* FOwner;
	Gls::Persistentclasses::TGLPersistentObjectList* FItemList;
	TGLParticleArray *FDirectList;
	
protected:
	TGLParticle* __fastcall GetItems(int index);
	void __fastcall SetItems(int index, TGLParticle* val);
	void __fastcall AfterItemCreated(System::TObject* Sender);
	
public:
	__fastcall virtual TGLParticleList();
	__fastcall virtual ~TGLParticleList();
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* reader);
	__property TGLParticleFXManager* Owner = {read=FOwner, write=FOwner};
	__property TGLParticle* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	int __fastcall ItemCount();
	int __fastcall AddItem(TGLParticle* aItem);
	void __fastcall RemoveAndFreeItem(TGLParticle* aItem);
	int __fastcall IndexOfItem(TGLParticle* aItem);
	void __fastcall Pack();
	__property PGLParticleArray List = {read=FDirectList};
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLParticleList(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Persistentclasses::TGLPersistentObject(reader) { }
	
};


typedef void __fastcall (__closure *TPFXCreateParticleEvent)(System::TObject* Sender, TGLParticle* aParticle);

class PASCALIMPLEMENTATION TGLParticleFXManager : public Gls::Cadencer::TGLCadencedComponent
{
	typedef Gls::Cadencer::TGLCadencedComponent inherited;
	
private:
	Gls::Material::TGLBlendingMode FBlendingMode;
	TGLParticleFXRenderer* FRenderer;
	TGLParticleList* FParticles;
	int FNextID;
	TPFXCreateParticleEvent FOnCreateParticle;
	bool FAutoFreeWhenEmpty;
	System::Classes::TList* FUsers;
	
protected:
	void __fastcall SetRenderer(TGLParticleFXRenderer* const val);
	void __fastcall SetParticles(TGLParticleList* const aParticles);
	virtual unsigned __fastcall TexturingMode() = 0 ;
	virtual void __fastcall InitializeRendering(Gls::Rendercontextinfo::TGLRenderContextInfo &rci) = 0 ;
	virtual void __fastcall BeginParticles(Gls::Rendercontextinfo::TGLRenderContextInfo &rci) = 0 ;
	virtual void __fastcall RenderParticle(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, TGLParticle* aParticle) = 0 ;
	virtual void __fastcall EndParticles(Gls::Rendercontextinfo::TGLRenderContextInfo &rci) = 0 ;
	virtual void __fastcall FinalizeRendering(Gls::Rendercontextinfo::TGLRenderContextInfo &rci) = 0 ;
	__property int NextID = {read=FNextID, write=FNextID, nodefault};
	__property Gls::Material::TGLBlendingMode BlendingMode = {read=FBlendingMode, write=FBlendingMode, nodefault};
	void __fastcall ApplyBlendingMode(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall UnapplyBlendingMode(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall registerUser(TGLParticleFXEffect* obj);
	void __fastcall unregisterUser(TGLParticleFXEffect* obj);
	
public:
	__fastcall virtual TGLParticleFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLParticleFXManager();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	__classmethod virtual TGLParticleClass __fastcall ParticlesClass();
	virtual TGLParticle* __fastcall CreateParticle();
	void __fastcall CreateParticles(int nbParticles);
	__property TGLParticleList* Particles = {read=FParticles, write=SetParticles};
	virtual int __fastcall ParticleCount();
	__property bool AutoFreeWhenEmpty = {read=FAutoFreeWhenEmpty, write=FAutoFreeWhenEmpty, nodefault};
	
__published:
	__property TGLParticleFXRenderer* Renderer = {read=FRenderer, write=SetRenderer};
	__property TPFXCreateParticleEvent OnCreateParticle = {read=FOnCreateParticle, write=FOnCreateParticle};
	__property Cadencer;
};


class PASCALIMPLEMENTATION TGLParticleFXEffect : public Gls::Scene::TGLObjectPostEffect
{
	typedef Gls::Scene::TGLObjectPostEffect inherited;
	
private:
	TGLParticleFXManager* FManager;
	System::UnicodeString FManagerName;
	float FEffectScale;
	void __fastcall SetEffectScale(const float Value);
	
protected:
	void __fastcall SetManager(TGLParticleFXManager* val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	void __fastcall managerNotification(TGLParticleFXManager* aManager, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLParticleFXEffect(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLParticleFXEffect();
	
__published:
	__property TGLParticleFXManager* Manager = {read=FManager, write=SetManager};
	__property float EffectScale = {read=FEffectScale, write=SetEffectScale};
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TParticleReference
{
public:
	TGLParticle* particle;
	int distance;
};
#pragma pack(pop)


typedef TParticleReference *PParticleReference;

typedef System::StaticArray<TParticleReference, 8388607> TParticleReferenceArray;

typedef TParticleReferenceArray *PParticleReferenceArray;

typedef System::StaticArray<void *, 8388607> TFXPointerList;

typedef TFXPointerList *PFXPointerList;

struct DECLSPEC_DRECORD TPFXRegion
{
public:
	int count;
	int capacity;
	TParticleReferenceArray *particleRef;
	TFXPointerList *particleOrder;
};


typedef TPFXRegion *PPFXRegion;

enum DECLSPEC_DENUM TPFXSortAccuracy : unsigned char { saLow, saOneTenth, saOneThird, saOneHalf, saHigh };

class PASCALIMPLEMENTATION TGLParticleFXRenderer : public Gls::Scene::TGLBaseSceneObject
{
	typedef Gls::Scene::TGLBaseSceneObject inherited;
	
private:
	System::Classes::TList* FManagerList;
	double FLastSortTime;
	int FLastParticleCount;
	bool FZWrite;
	bool FZTest;
	bool FZCull;
	TPFXSortAccuracy FZSortAccuracy;
	float FZMaxDistance;
	Gls::Material::TGLBlendingMode FBlendingMode;
	System::StaticArray<TPFXRegion, 128> FRegions;
	
protected:
	bool __fastcall StoreZMaxDistance();
	void __fastcall RegisterManager(TGLParticleFXManager* aManager);
	void __fastcall UnRegisterManager(TGLParticleFXManager* aManager);
	void __fastcall UnRegisterAll();
	
public:
	__fastcall virtual TGLParticleFXRenderer(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLParticleFXRenderer();
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property double LastSortTime = {read=FLastSortTime};
	__property int LastParticleCount = {read=FLastParticleCount, nodefault};
	
__published:
	__property bool ZWrite = {read=FZWrite, write=FZWrite, default=0};
	__property bool ZTest = {read=FZTest, write=FZTest, default=1};
	__property bool ZCull = {read=FZCull, write=FZCull, default=1};
	__property TPFXSortAccuracy ZSortAccuracy = {read=FZSortAccuracy, write=FZSortAccuracy, default=4};
	__property float ZMaxDistance = {read=FZMaxDistance, write=FZMaxDistance, stored=StoreZMaxDistance};
	__property Gls::Material::TGLBlendingMode BlendingMode = {read=FBlendingMode, write=FBlendingMode, default=2};
	__property Visible = {default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLParticleFXRenderer(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLBaseSceneObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLSourcePFXVelocityMode : unsigned char { svmAbsolute, svmRelative };

enum DECLSPEC_DENUM TGLSourcePFXPositionMode : unsigned char { spmAbsoluteOffset, spmRelative };

enum DECLSPEC_DENUM TGLSourcePFXDispersionMode : unsigned char { sdmFast, sdmIsotropic, sdmGaussian };

class PASCALIMPLEMENTATION TGLSourcePFXEffect : public TGLParticleFXEffect
{
	typedef TGLParticleFXEffect inherited;
	
private:
	Gls::Coordinates::TGLCoordinates3* FInitialVelocity;
	Gls::Coordinates::TGLCoordinates3* FInitialPosition;
	Gls::Coordinates::TGLCoordinates3* FPositionDispersionRange;
	float FVelocityDispersion;
	float FPositionDispersion;
	float FParticleInterval;
	TGLSourcePFXVelocityMode FVelocityMode;
	TGLSourcePFXPositionMode FPositionMode;
	TGLSourcePFXDispersionMode FDispersionMode;
	bool FEnabled;
	bool FDisabledIfOwnerInvisible;
	double FTimeRemainder;
	float FRotationDispersion;
	
protected:
	void __fastcall SetInitialVelocity(Gls::Coordinates::TGLCoordinates3* const val);
	void __fastcall SetInitialPosition(Gls::Coordinates::TGLCoordinates3* const val);
	void __fastcall SetPositionDispersionRange(Gls::Coordinates::TGLCoordinates3* const val);
	void __fastcall SetParticleInterval(const float val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	Gls::Vectortypes::TVector3f __fastcall ParticleAbsoluteInitialPos();
	
public:
	__fastcall virtual TGLSourcePFXEffect(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLSourcePFXEffect();
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	void __fastcall Burst(double time, int nb);
	void __fastcall RingExplosion(double time, float minInitialSpeed, float maxInitialSpeed, int nbParticles);
	
__published:
	__property Gls::Coordinates::TGLCoordinates3* InitialVelocity = {read=FInitialVelocity, write=SetInitialVelocity};
	__property float VelocityDispersion = {read=FVelocityDispersion, write=FVelocityDispersion};
	__property Gls::Coordinates::TGLCoordinates3* InitialPosition = {read=FInitialPosition, write=SetInitialPosition};
	__property float PositionDispersion = {read=FPositionDispersion, write=FPositionDispersion};
	__property Gls::Coordinates::TGLCoordinates3* PositionDispersionRange = {read=FPositionDispersionRange, write=SetPositionDispersionRange};
	__property float ParticleInterval = {read=FParticleInterval, write=SetParticleInterval};
	__property TGLSourcePFXVelocityMode VelocityMode = {read=FVelocityMode, write=FVelocityMode, default=0};
	__property TGLSourcePFXPositionMode PositionMode = {read=FPositionMode, write=FPositionMode, default=0};
	__property TGLSourcePFXDispersionMode DispersionMode = {read=FDispersionMode, write=FDispersionMode, default=0};
	__property float RotationDispersion = {read=FRotationDispersion, write=FRotationDispersion};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property bool DisabledIfOwnerInvisible = {read=FDisabledIfOwnerInvisible, write=FDisabledIfOwnerInvisible, nodefault};
};


class PASCALIMPLEMENTATION TGLDynamicPFXManager : public TGLParticleFXManager
{
	typedef TGLParticleFXManager inherited;
	
private:
	Gls::Coordinates::TGLCoordinates3* FAcceleration;
	float FFriction;
	double FCurrentTime;
	
protected:
	void __fastcall SetAcceleration(Gls::Coordinates::TGLCoordinates3* const val);
	virtual float __fastcall MaxParticleAge() = 0 ;
	__property double CurrentTime = {read=FCurrentTime};
	
public:
	__fastcall virtual TGLDynamicPFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLDynamicPFXManager();
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	
__published:
	__property Gls::Coordinates::TGLCoordinates3* Acceleration = {read=FAcceleration, write=SetAcceleration};
	__property float Friction = {read=FFriction, write=FFriction};
};


class PASCALIMPLEMENTATION TPFXLifeColor : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Gls::Color::TGLColor* FColorInner;
	Gls::Color::TGLColor* FColorOuter;
	float FLifeTime;
	float FInvLifeTime;
	float FIntervalRatio;
	float FSizeScale;
	bool FDoScale;
	bool FDoRotate;
	float FRotateAngle;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetColorInner(Gls::Color::TGLColor* const val);
	void __fastcall SetColorOuter(Gls::Color::TGLColor* const val);
	void __fastcall SetLifeTime(const float val);
	void __fastcall SetSizeScale(const float val);
	void __fastcall SetRotateAngle(const float Value);
	
public:
	__fastcall virtual TPFXLifeColor(System::Classes::TCollection* Collection);
	__fastcall virtual ~TPFXLifeColor();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property float InvLifeTime = {read=FInvLifeTime};
	__property float InvIntervalRatio = {read=FIntervalRatio};
	
__published:
	__property Gls::Color::TGLColor* ColorInner = {read=FColorInner, write=SetColorInner};
	__property Gls::Color::TGLColor* ColorOuter = {read=FColorOuter, write=SetColorOuter};
	__property float LifeTime = {read=FLifeTime, write=SetLifeTime};
	__property float SizeScale = {read=FSizeScale, write=SetSizeScale};
	__property float RotateAngle = {read=FRotateAngle, write=SetRotateAngle};
};


class PASCALIMPLEMENTATION TPFXLifeColors : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TPFXLifeColor* operator[](int index) { return this->Items[index]; }
	
protected:
	void __fastcall SetItems(int index, TPFXLifeColor* const val);
	TPFXLifeColor* __fastcall GetItems(int index);
	
public:
	__fastcall TPFXLifeColors(System::Classes::TPersistent* AOwner);
	HIDESBASE TPFXLifeColor* __fastcall Add();
	HIDESBASE TPFXLifeColor* __fastcall FindItemID(int ID);
	__property TPFXLifeColor* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	double __fastcall MaxLifeTime();
	bool __fastcall RotationsDefined();
	bool __fastcall ScalingDefined();
	void __fastcall PrepareIntervalRatios();
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TPFXLifeColors() { }
	
};


class PASCALIMPLEMENTATION TGLLifeColoredPFXManager : public TGLDynamicPFXManager
{
	typedef TGLDynamicPFXManager inherited;
	
private:
	TPFXLifeColors* FLifeColors;
	System::Classes::TList* FLifeColorsLookup;
	bool FLifeRotations;
	bool FLifeScaling;
	Gls::Color::TGLColor* FColorInner;
	Gls::Color::TGLColor* FColorOuter;
	float FParticleSize;
	
protected:
	void __fastcall SetLifeColors(TPFXLifeColors* const val);
	void __fastcall SetColorInner(Gls::Color::TGLColor* const val);
	void __fastcall SetColorOuter(Gls::Color::TGLColor* const val);
	virtual void __fastcall InitializeRendering(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall FinalizeRendering(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual float __fastcall MaxParticleAge();
	void __fastcall ComputeColors(float &lifeTime, Gls::Vectortypes::TVector4f &inner, Gls::Vectortypes::TVector4f &outer);
	void __fastcall ComputeInnerColor(float &lifeTime, Gls::Vectortypes::TVector4f &inner);
	void __fastcall ComputeOuterColor(float &lifeTime, Gls::Vectortypes::TVector4f &outer);
	bool __fastcall ComputeSizeScale(float &lifeTime, float &sizeScale);
	bool __fastcall ComputeRotateAngle(float &lifeTime, float &rotateAngle);
	void __fastcall RotateVertexBuf(Gls::Vectorlists::TGLAffineVectorList* buf, float lifeTime, const Gls::Vectortypes::TVector3f &axis, float offsetAngle);
	
public:
	__fastcall virtual TGLLifeColoredPFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLLifeColoredPFXManager();
	__property float ParticleSize = {read=FParticleSize, write=FParticleSize};
	__property Gls::Color::TGLColor* ColorInner = {read=FColorInner, write=SetColorInner};
	__property Gls::Color::TGLColor* ColorOuter = {read=FColorOuter, write=SetColorOuter};
	__property TPFXLifeColors* LifeColors = {read=FLifeColors, write=SetLifeColors};
	
__published:
	__property BlendingMode = {default=2};
};


typedef void __fastcall (__closure *TPFXDirectRenderEvent)(System::TObject* Sender, TGLParticle* aParticle, Gls::Rendercontextinfo::TGLRenderContextInfo &rci);

typedef void __fastcall (__closure *TPFXProgressEvent)(System::TObject* Sender, const Gls::Baseclasses::TGLProgressTimes &progressTime, bool &defaultProgress);

typedef void __fastcall (__closure *TPFXParticleProgress)(System::TObject* Sender, const Gls::Baseclasses::TGLProgressTimes &progressTime, TGLParticle* aParticle, bool &killParticle);

typedef int __fastcall (__closure *TPFXGetParticleCountEvent)(System::TObject* Sender);

class PASCALIMPLEMENTATION TGLCustomPFXManager : public TGLLifeColoredPFXManager
{
	typedef TGLLifeColoredPFXManager inherited;
	
private:
	Gls::Scene::TGLDirectRenderEvent FOnInitializeRendering;
	Gls::Scene::TGLDirectRenderEvent FOnBeginParticles;
	TPFXDirectRenderEvent FOnRenderParticle;
	Gls::Scene::TGLDirectRenderEvent FOnEndParticles;
	Gls::Scene::TGLDirectRenderEvent FOnFinalizeRendering;
	TPFXProgressEvent FOnProgress;
	TPFXParticleProgress FOnParticleProgress;
	TPFXGetParticleCountEvent FOnGetParticleCountEvent;
	
protected:
	virtual unsigned __fastcall TexturingMode();
	virtual void __fastcall InitializeRendering(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall BeginParticles(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall RenderParticle(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, TGLParticle* aParticle);
	virtual void __fastcall EndParticles(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall FinalizeRendering(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	virtual int __fastcall ParticleCount();
	
__published:
	__property Gls::Scene::TGLDirectRenderEvent OnInitializeRendering = {read=FOnInitializeRendering, write=FOnInitializeRendering};
	__property Gls::Scene::TGLDirectRenderEvent OnBeginParticles = {read=FOnBeginParticles, write=FOnBeginParticles};
	__property TPFXDirectRenderEvent OnRenderParticle = {read=FOnRenderParticle, write=FOnRenderParticle};
	__property Gls::Scene::TGLDirectRenderEvent OnEndParticles = {read=FOnEndParticles, write=FOnEndParticles};
	__property Gls::Scene::TGLDirectRenderEvent OnFinalizeRendering = {read=FOnFinalizeRendering, write=FOnFinalizeRendering};
	__property TPFXProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property TPFXParticleProgress OnParticleProgress = {read=FOnParticleProgress, write=FOnParticleProgress};
	__property TPFXGetParticleCountEvent OnGetParticleCountEvent = {read=FOnGetParticleCountEvent, write=FOnGetParticleCountEvent};
	__property ParticleSize = {default=0};
	__property ColorInner;
	__property ColorOuter;
	__property LifeColors;
public:
	/* TGLLifeColoredPFXManager.Create */ inline __fastcall virtual TGLCustomPFXManager(System::Classes::TComponent* aOwner) : TGLLifeColoredPFXManager(aOwner) { }
	/* TGLLifeColoredPFXManager.Destroy */ inline __fastcall virtual ~TGLCustomPFXManager() { }
	
};


class PASCALIMPLEMENTATION TGLPolygonPFXManager : public TGLLifeColoredPFXManager
{
	typedef TGLLifeColoredPFXManager inherited;
	
private:
	int FNbSides;
	Gls::Vectortypes::TVector3f Fvx;
	Gls::Vectortypes::TVector3f Fvy;
	Gls::Vectorlists::TGLAffineVectorList* FVertices;
	Gls::Vectorlists::TGLAffineVectorList* FVertBuf;
	
protected:
	void __fastcall SetNbSides(const int val);
	virtual unsigned __fastcall TexturingMode();
	virtual void __fastcall InitializeRendering(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall BeginParticles(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall RenderParticle(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, TGLParticle* aParticle);
	virtual void __fastcall EndParticles(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall FinalizeRendering(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLPolygonPFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLPolygonPFXManager();
	
__published:
	__property int NbSides = {read=FNbSides, write=SetNbSides, default=6};
	__property ParticleSize = {default=0};
	__property ColorInner;
	__property ColorOuter;
	__property LifeColors;
};


enum DECLSPEC_DENUM TSpriteColorMode : unsigned char { scmFade, scmInner, scmOuter, scmNone };

enum DECLSPEC_DENUM TSpritesPerTexture : unsigned char { sptOne, sptFour };

class PASCALIMPLEMENTATION TGLBaseSpritePFXManager : public TGLLifeColoredPFXManager
{
	typedef TGLLifeColoredPFXManager inherited;
	
private:
	Gls::Context::TGLTextureHandle* FTexHandle;
	Gls::Vectortypes::TVector3f Fvx;
	Gls::Vectortypes::TVector3f Fvy;
	Gls::Vectortypes::TVector3f Fvz;
	Gls::Vectorlists::TGLAffineVectorList* FVertices;
	Gls::Vectorlists::TGLAffineVectorList* FVertBuf;
	float FAspectRatio;
	float FRotation;
	TGLBaseSpritePFXManager* FShareSprites;
	TSpritesPerTexture FSpritesPerTexture;
	TSpriteColorMode FColorMode;
	
protected:
	virtual void __fastcall PrepareImage(Gls::Graphics::TGLImage* bmp32, int &texFormat) = 0 ;
	void __fastcall BindTexture(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetSpritesPerTexture(const TSpritesPerTexture val);
	void __fastcall SetColorMode(const TSpriteColorMode val);
	void __fastcall SetAspectRatio(const float val);
	bool __fastcall StoreAspectRatio();
	void __fastcall SetRotation(const float val);
	void __fastcall SetShareSprites(TGLBaseSpritePFXManager* const val);
	virtual unsigned __fastcall TexturingMode();
	virtual void __fastcall InitializeRendering(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall BeginParticles(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall RenderParticle(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, TGLParticle* aParticle);
	virtual void __fastcall EndParticles(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall FinalizeRendering(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property TSpritesPerTexture SpritesPerTexture = {read=FSpritesPerTexture, write=SetSpritesPerTexture, nodefault};
	
public:
	__fastcall virtual TGLBaseSpritePFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLBaseSpritePFXManager();
	__property TSpriteColorMode ColorMode = {read=FColorMode, write=SetColorMode, nodefault};
	
__published:
	__property float AspectRatio = {read=FAspectRatio, write=SetAspectRatio, stored=StoreAspectRatio};
	__property float Rotation = {read=FRotation, write=SetRotation};
	__property TGLBaseSpritePFXManager* ShareSprites = {read=FShareSprites, write=FShareSprites};
};


typedef void __fastcall (__closure *TPFXPrepareTextureImageEvent)(System::TObject* Sender, Gls::Graphics::TGLImage* destBmp32, int &texFormat);

class PASCALIMPLEMENTATION TGLCustomSpritePFXManager : public TGLBaseSpritePFXManager
{
	typedef TGLBaseSpritePFXManager inherited;
	
private:
	TPFXPrepareTextureImageEvent FOnPrepareTextureImage;
	
protected:
	virtual void __fastcall PrepareImage(Gls::Graphics::TGLImage* bmp32, int &texFormat);
	
public:
	__fastcall virtual TGLCustomSpritePFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLCustomSpritePFXManager();
	
__published:
	__property TPFXPrepareTextureImageEvent OnPrepareTextureImage = {read=FOnPrepareTextureImage, write=FOnPrepareTextureImage};
	__property ColorMode = {default=1};
	__property SpritesPerTexture = {default=0};
	__property ParticleSize = {default=0};
	__property ColorInner;
	__property ColorOuter;
	__property LifeColors;
};


class PASCALIMPLEMENTATION TGLPointLightPFXManager : public TGLBaseSpritePFXManager
{
	typedef TGLBaseSpritePFXManager inherited;
	
private:
	int FTexMapSize;
	
protected:
	virtual void __fastcall PrepareImage(Gls::Graphics::TGLImage* bmp32, int &texFormat);
	void __fastcall SetTexMapSize(const int val);
	
public:
	__fastcall virtual TGLPointLightPFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLPointLightPFXManager();
	
__published:
	__property int TexMapSize = {read=FTexMapSize, write=SetTexMapSize, default=5};
	__property ColorMode = {default=1};
	__property ParticleSize = {default=0};
	__property ColorInner;
	__property ColorOuter;
	__property LifeColors;
};


//-- var, const, procedure ---------------------------------------------------
static const System::Byte cPFXNbRegions = System::Byte(0x80);
static const System::Byte cPFXGranularity = System::Byte(0x80);
extern DELPHI_PACKAGE TGLSourcePFXEffect* __fastcall GetOrCreateSourcePFX(Gls::Scene::TGLBaseSceneObject* obj, const System::UnicodeString name = System::UnicodeString());
}	/* namespace Particlefx */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_PARTICLEFX)
using namespace Gls::Particlefx;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ParticlefxHPP
