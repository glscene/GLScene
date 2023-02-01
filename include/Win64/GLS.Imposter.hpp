// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Imposter.pas' rev: 35.00 (Windows)

#ifndef Gls_ImposterHPP
#define Gls_ImposterHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.OpenGLext.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Context.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.Color.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.State.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.Utils.hpp>

//-- user supplied -----------------------------------------------------------
typedef GLS.Graphics::TGLBitmap32* __fastcall (__closure *TLoadingImposterEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* impostoredObject, TImposter* destImposter);

namespace Gls
{
namespace Imposter
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TImposter;
class DELPHICLASS TGLImposterBuilder;
class DELPHICLASS TGLStaticImposterBuilderCorona;
struct TCoronaTangentLookup;
class DELPHICLASS TGLStaticImposterBuilderCoronas;
class DELPHICLASS TStaticImposter;
class DELPHICLASS TGLStaticImposterBuilder;
class DELPHICLASS TGLDynamicImposterBuilder;
class DELPHICLASS TGLImposter;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TImposterOption : unsigned char { impoBlended, impoAlphaTest, impoNearestFiltering, impoPerspectiveCorrection };

typedef System::Set<TImposterOption, TImposterOption::impoBlended, TImposterOption::impoPerspectiveCorrection> TImposterOptions;

class PASCALIMPLEMENTATION TImposter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FRequestCount;
	TGLImposterBuilder* FBuilder;
	Gls::Context::TGLTextureHandle* FTexture;
	Gls::Scene::TGLBaseSceneObject* FImpostoredObject;
	float FAspectRatio;
	bool FModulated;
	
protected:
	Gls::Vectortypes::TVector4f FVx;
	Gls::Vectortypes::TVector4f FVy;
	Gls::Vectortypes::TVector4f FStaticOffset;
	System::StaticArray<Gls::Vectortypes::TVector4f, 4> FQuad;
	float FStaticScale;
	virtual void __fastcall PrepareTexture(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall RenderQuad(const Gls::Vectortypes::TVector4f &texExtents, const Gls::Vectortypes::TVector4f &objPos, float size);
	
public:
	__fastcall virtual TImposter(TGLImposterBuilder* aBuilder);
	__fastcall virtual ~TImposter();
	virtual void __fastcall BeginRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall Render(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, const Gls::Vectortypes::TVector4f &objPos, const Gls::Vectortypes::TVector4f &localCameraPos, float size);
	virtual void __fastcall EndRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall RenderOnce(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, const Gls::Vectortypes::TVector4f &objPos, const Gls::Vectortypes::TVector4f &localCameraPos, float size);
	__property float AspectRatio = {read=FAspectRatio, write=FAspectRatio};
	__property TGLImposterBuilder* Builder = {read=FBuilder};
	__property Gls::Context::TGLTextureHandle* Texture = {read=FTexture};
	__property Gls::Scene::TGLBaseSceneObject* ImpostoredObject = {read=FImpostoredObject, write=FImpostoredObject};
	__property bool Modulated = {read=FModulated, write=FModulated, nodefault};
};


typedef void __fastcall (__closure *TImposterLoadedEvent)(System::TObject* Sender, Gls::Scene::TGLBaseSceneObject* impostoredObject, TImposter* destImposter);

enum DECLSPEC_DENUM TImposterReference : unsigned char { irCenter, irTop, irBottom };

class PASCALIMPLEMENTATION TGLImposterBuilder : public Gls::Baseclasses::TGLUpdateAbleComponent
{
	typedef Gls::Baseclasses::TGLUpdateAbleComponent inherited;
	
private:
	Gls::Color::TGLColor* FBackColor;
	Gls::Coordinates::TGLCoordinates3* FBuildOffset;
	Gls::Persistentclasses::TGLPersistentObjectList* FImposterRegister;
	Gls::Scene::TGLRenderPoint* FRenderPoint;
	TImposterOptions FImposterOptions;
	float FAlphaTreshold;
	TImposterReference FImposterReference;
	TLoadingImposterEvent FOnLoadingImposter;
	TImposterLoadedEvent FOnImposterLoaded;
	
protected:
	void __fastcall SetRenderPoint(Gls::Scene::TGLRenderPoint* AValue);
	void __fastcall RenderPointFreed(System::TObject* Sender);
	void __fastcall SetBackColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetBuildOffset(Gls::Coordinates::TGLCoordinates3* AValue);
	void __fastcall SetImposterReference(TImposterReference AValue);
	void __fastcall InitializeImpostorTexture(const System::Types::TPoint &TextureSize);
	__property Gls::Persistentclasses::TGLPersistentObjectList* ImposterRegister = {read=FImposterRegister};
	void __fastcall UnregisterImposter(TImposter* imposter);
	virtual TImposter* __fastcall CreateNewImposter();
	virtual void __fastcall PrepareImposters(System::TObject* Sender, Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoPrepareImposter(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, Gls::Scene::TGLBaseSceneObject* impostoredObject, TImposter* destImposter) = 0 ;
	virtual void __fastcall DoUserSpecifiedImposter(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, TImposter* destImposter, Gls::Graphics::TGLImage* bmp32);
	
public:
	__fastcall virtual TGLImposterBuilder(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLImposterBuilder();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	TImposter* __fastcall ImposterFor(Gls::Scene::TGLBaseSceneObject* impostoredObject);
	void __fastcall RequestImposterFor(Gls::Scene::TGLBaseSceneObject* impostoredObject);
	void __fastcall UnRequestImposterFor(Gls::Scene::TGLBaseSceneObject* impostoredObject);
	
__published:
	__property Gls::Scene::TGLRenderPoint* RenderPoint = {read=FRenderPoint, write=SetRenderPoint};
	__property Gls::Color::TGLColor* BackColor = {read=FBackColor, write=SetBackColor};
	__property Gls::Coordinates::TGLCoordinates3* BuildOffset = {read=FBuildOffset, write=SetBuildOffset};
	__property TImposterOptions ImposterOptions = {read=FImposterOptions, write=FImposterOptions, default=3};
	__property TImposterReference ImposterReference = {read=FImposterReference, write=SetImposterReference, default=0};
	__property float AlphaTreshold = {read=FAlphaTreshold, write=FAlphaTreshold};
	__property TLoadingImposterEvent OnLoadingImposter = {read=FOnLoadingImposter, write=FOnLoadingImposter};
	__property TImposterLoadedEvent OnImposterLoaded = {read=FOnImposterLoaded, write=FOnImposterLoaded};
};


class PASCALIMPLEMENTATION TGLStaticImposterBuilderCorona : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	int FSamples;
	float FElevation;
	int FSampleBaseIndex;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetSamples(int AValue);
	void __fastcall SetElevation(float AValue);
	
public:
	__fastcall virtual TGLStaticImposterBuilderCorona(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLStaticImposterBuilderCorona();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property int Samples = {read=FSamples, write=SetSamples, default=8};
	__property float Elevation = {read=FElevation, write=SetElevation};
};


struct DECLSPEC_DRECORD TCoronaTangentLookup
{
public:
	float minTan;
	float maxTan;
	TGLStaticImposterBuilderCorona* corona;
};


class PASCALIMPLEMENTATION TGLStaticImposterBuilderCoronas : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
	
private:
	typedef System::DynamicArray<TCoronaTangentLookup> _TGLStaticImposterBuilderCoronas__1;
	
	
public:
	TGLStaticImposterBuilderCorona* operator[](int AIndex) { return this->Items[AIndex]; }
	
private:
	_TGLStaticImposterBuilderCoronas__1 FCoronaTangentLookup;
	
protected:
	void __fastcall SetItems(int AIndex, TGLStaticImposterBuilderCorona* const AValue);
	TGLStaticImposterBuilderCorona* __fastcall GetItems(int AIndex);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	void __fastcall PrepareSampleBaseIndices();
	void __fastcall PrepareCoronaTangentLookup();
	TGLStaticImposterBuilderCorona* __fastcall CoronaForElevationTangent(float aTangent);
	
public:
	__fastcall TGLStaticImposterBuilderCoronas(System::Classes::TPersistent* AOwner);
	HIDESBASE TGLStaticImposterBuilderCorona* __fastcall Add()/* overload */;
	HIDESBASE TGLStaticImposterBuilderCorona* __fastcall Add(const float elevation, int samples)/* overload */;
	__property TGLStaticImposterBuilderCorona* Items[int AIndex] = {read=GetItems, write=SetItems/*, default*/};
	int __fastcall SampleCount();
	virtual void __fastcall NotifyChange();
	virtual void __fastcall EndUpdate();
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLStaticImposterBuilderCoronas() { }
	
};


class PASCALIMPLEMENTATION TStaticImposter : public TImposter
{
	typedef TImposter inherited;
	
public:
	virtual void __fastcall Render(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, const Gls::Vectortypes::TVector4f &objPos, const Gls::Vectortypes::TVector4f &localCameraPos, float size);
public:
	/* TImposter.Create */ inline __fastcall virtual TStaticImposter(TGLImposterBuilder* aBuilder) : TImposter(aBuilder) { }
	/* TImposter.Destroy */ inline __fastcall virtual ~TStaticImposter() { }
	
};


enum DECLSPEC_DENUM TSIBLigthing : unsigned char { siblNoLighting, siblStaticLighting, siblLocalLighting };

class PASCALIMPLEMENTATION TGLStaticImposterBuilder : public TGLImposterBuilder
{
	typedef TGLImposterBuilder inherited;
	
private:
	TGLStaticImposterBuilderCoronas* FCoronas;
	int FSampleSize;
	System::Types::TPoint FTextureSize;
	System::Types::TPoint FSamplesPerAxis;
	Gls::Vectortypes::TVector2f FInvSamplesPerAxis;
	float FSamplingRatioBias;
	float FInvSamplingRatioBias;
	TSIBLigthing FLighting;
	float FSamplesAlphaScale;
	
protected:
	void __fastcall SetCoronas(TGLStaticImposterBuilderCoronas* AValue);
	void __fastcall SetSampleSize(int AValue);
	void __fastcall SetSamplingRatioBias(float AValue);
	bool __fastcall StoreSamplingRatioBias();
	void __fastcall SetLighting(TSIBLigthing AValue);
	void __fastcall SetSamplesAlphaScale(float AValue);
	bool __fastcall StoreSamplesAlphaScale();
	System::UnicodeString __fastcall GetTextureSizeInfo();
	void __fastcall SetTextureSizeInfo(const System::UnicodeString texSize);
	System::Types::TPoint __fastcall ComputeOptimalTextureSize();
	virtual TImposter* __fastcall CreateNewImposter();
	virtual void __fastcall DoPrepareImposter(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, Gls::Scene::TGLBaseSceneObject* impostoredObject, TImposter* destImposter);
	virtual void __fastcall DoUserSpecifiedImposter(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, TImposter* destImposter, Gls::Graphics::TGLImage* bmp32);
	void __fastcall ComputeStaticParams(TImposter* destImposter);
	
public:
	__fastcall virtual TGLStaticImposterBuilder(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLStaticImposterBuilder();
	void __fastcall Render(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, Gls::Scene::TGLBaseSceneObject* impostoredObject, TImposter* destImposter);
	float __fastcall TextureFillRatio();
	__property System::Types::TPoint TextureSize = {read=FTextureSize};
	__property System::Types::TPoint SamplesPerAxis = {read=FSamplesPerAxis};
	
__published:
	__property TGLStaticImposterBuilderCoronas* Coronas = {read=FCoronas, write=SetCoronas};
	__property int SampleSize = {read=FSampleSize, write=SetSampleSize, default=32};
	__property float SamplingRatioBias = {read=FSamplingRatioBias, write=SetSamplingRatioBias, stored=StoreSamplingRatioBias};
	__property float SamplesAlphaScale = {read=FSamplesAlphaScale, write=SetSamplesAlphaScale, stored=StoreSamplesAlphaScale};
	__property TSIBLigthing Lighting = {read=FLighting, write=FLighting, default=1};
	__property System::UnicodeString TextureSizeInfo = {read=GetTextureSizeInfo, write=SetTextureSizeInfo, stored=false};
};


class PASCALIMPLEMENTATION TGLDynamicImposterBuilder : public TGLImposterBuilder
{
	typedef TGLImposterBuilder inherited;
	
private:
	int FMinTexSize;
	int FMaxTexSize;
	float FMinDistance;
	float FTolerance;
	bool FUseMatrixError;
	
protected:
	void __fastcall SetMinDistance(const float AValue);
	
public:
	__fastcall virtual TGLDynamicImposterBuilder(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLDynamicImposterBuilder();
	
__published:
	__property int MinTexSize = {read=FMinTexSize, write=FMinTexSize, nodefault};
	__property int MaxTexSize = {read=FMaxTexSize, write=FMaxTexSize, nodefault};
	__property float MinDistance = {read=FMinDistance, write=SetMinDistance};
	__property float Tolerance = {read=FTolerance, write=FTolerance};
	__property bool UseMatrixError = {read=FUseMatrixError, write=FUseMatrixError, nodefault};
};


class PASCALIMPLEMENTATION TGLImposter : public Gls::Scene::TGLImmaterialSceneObject
{
	typedef Gls::Scene::TGLImmaterialSceneObject inherited;
	
private:
	TGLImposterBuilder* FBuilder;
	Gls::Scene::TGLBaseSceneObject* FImpostoredObject;
	
protected:
	void __fastcall SetBuilder(TGLImposterBuilder* const AValue);
	void __fastcall SetImpostoredObject(Gls::Scene::TGLBaseSceneObject* const AValue);
	
public:
	__fastcall virtual TGLImposter(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLImposter();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	
__published:
	__property TGLImposterBuilder* Builder = {read=FBuilder, write=SetBuilder};
	__property Gls::Scene::TGLBaseSceneObject* ImpostoredObject = {read=FImpostoredObject, write=SetImpostoredObject};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLImposter(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define cDefaultImposterOptions (System::Set<TImposterOption, TImposterOption::impoBlended, TImposterOption::impoPerspectiveCorrection>() << TImposterOption::impoBlended << TImposterOption::impoAlphaTest )
}	/* namespace Imposter */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_IMPOSTER)
using namespace Gls::Imposter;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ImposterHPP
