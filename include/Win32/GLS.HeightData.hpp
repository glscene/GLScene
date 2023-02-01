// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.HeightData.pas' rev: 35.00 (Windows)

#ifndef Gls_HeightdataHPP
#define Gls_HeightdataHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.Utils.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Material.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Heightdata
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLHeightDataSource;
struct TGLHeightDataUser;
class DELPHICLASS TGLHeightData;
class DELPHICLASS TGLHeightDataThread;
class DELPHICLASS TGLBitmapHDS;
class DELPHICLASS TGLCustomHDS;
class DELPHICLASS TGLTerrainBaseHDS;
class DELPHICLASS TGLHeightDataSourceFilter;
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<System::Byte, 1073741824> TByteArray;

typedef System::StaticArray<Gls::Vectorgeometry::PByteVector, 268435456> TByteRaster;

typedef TByteRaster *PByteRaster;

typedef System::StaticArray<short, 536870912> TSmallintArray;

typedef TSmallintArray *PSmallIntArray;

typedef System::StaticArray<PSmallIntArray, 268435456> TSmallIntRaster;

typedef TSmallIntRaster *PSmallIntRaster;

typedef System::StaticArray<Gls::Vectorgeometry::PFloatVector, 268435456> TSingleRaster;

typedef TSingleRaster *PSingleRaster;

typedef System::TMetaClass* TGLHeightDataClass;

enum DECLSPEC_DENUM TGLHeightDataType : unsigned char { hdtByte, hdtSmallInt, hdtSingle, hdtDefault };

class PASCALIMPLEMENTATION TGLHeightDataSource : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TThreadList* FData;
	System::StaticArray<System::Classes::TList*, 256> FDataHash;
	System::Classes::TThread* FThread;
	int FMaxThreads;
	int FMaxPoolSize;
	TGLHeightDataClass FHeightDataClass;
	float FDefaultHeight;
	
protected:
	void __fastcall SetMaxThreads(const int Val);
	int __fastcall HashKey(int XLeft, int YTop);
	__property TGLHeightDataClass HeightDataClass = {read=FHeightDataClass, write=FHeightDataClass};
	TGLHeightData* __fastcall FindMatchInList(int XLeft, int YTop, int size, TGLHeightDataType DataType);
	
public:
	__fastcall virtual TGLHeightDataSource(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLHeightDataSource();
	__property System::Classes::TThreadList* Data = {read=FData};
	void __fastcall Clear();
	void __fastcall CleanUp();
	virtual TGLHeightData* __fastcall GetData(int XLeft, int YTop, int size, TGLHeightDataType DataType);
	virtual TGLHeightData* __fastcall PreLoad(int XLeft, int YTop, int size, TGLHeightDataType DataType);
	void __fastcall PreloadReplacement(TGLHeightData* aHeightData);
	virtual void __fastcall Release(TGLHeightData* aHeightData);
	virtual void __fastcall MarkDirty(const System::Types::TRect &Area)/* overload */;
	void __fastcall MarkDirty(int XLeft, int YTop, int xRight, int yBottom)/* overload */;
	void __fastcall MarkDirty()/* overload */;
	__property int MaxThreads = {read=FMaxThreads, write=SetMaxThreads, nodefault};
	__property int MaxPoolSize = {read=FMaxPoolSize, write=FMaxPoolSize, nodefault};
	__property float DefaultHeight = {read=FDefaultHeight, write=FDefaultHeight};
	virtual float __fastcall InterpolatedHeight(float x, float y, int TileSize);
	virtual int __fastcall Width() = 0 ;
	virtual int __fastcall Height() = 0 ;
	virtual void __fastcall ThreadIsIdle();
	virtual void __fastcall BeforePreparingData(TGLHeightData* HeightData);
	virtual void __fastcall StartPreparingData(TGLHeightData* HeightData);
	virtual void __fastcall AfterPreparingData(TGLHeightData* HeightData);
	void __fastcall TextureCoordinates(TGLHeightData* HeightData, bool Stretch = false);
};


enum DECLSPEC_DENUM THDTextureCoordinatesMode : unsigned char { tcmWorld, tcmLocal };

enum DECLSPEC_DENUM TGLHeightDataState : unsigned char { hdsQueued, hdsPreparing, hdsReady, hdsNone };

typedef void __fastcall (__closure *TOnHeightDataDirtyEvent)(TGLHeightData* sender);

struct DECLSPEC_DRECORD TGLHeightDataUser
{
public:
	System::TObject* user;
	TOnHeightDataDirtyEvent event;
};


class PASCALIMPLEMENTATION TGLHeightData : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
	
private:
	typedef System::DynamicArray<TGLHeightDataUser> _TGLHeightData__1;
	
	
private:
	_TGLHeightData__1 FUsers;
	TGLHeightDataSource* FOwner;
	TGLHeightDataState FDataState;
	int FSize;
	int FXLeft;
	int FYTop;
	int FUseCounter;
	TGLHeightDataType FDataType;
	int FDataSize;
	Gls::Vectorgeometry::TByteVector *FByteData;
	TByteRaster *FByteRaster;
	TSmallintArray *FSmallIntData;
	TSmallIntRaster *FSmallIntRaster;
	Gls::Vectorgeometry::TFloatVector *FSingleData;
	TSingleRaster *FSingleRaster;
	THDTextureCoordinatesMode FTextureCoordinatesMode;
	Gls::Vectorgeometry::TTexPoint FTCOffset;
	Gls::Vectorgeometry::TTexPoint FTCScale;
	System::UnicodeString FMaterialName;
	Gls::Material::TGLLibMaterial* FLibMaterial;
	System::TObject* FObjectTag;
	int FTag;
	int FTag2;
	System::Classes::TNotifyEvent FOnDestroy;
	bool FDirty;
	float FHeightMin;
	float FHeightMax;
	void __fastcall BuildByteRaster();
	void __fastcall BuildSmallIntRaster();
	void __fastcall BuildSingleRaster();
	void __fastcall ConvertByteToSmallInt();
	void __fastcall ConvertByteToSingle();
	void __fastcall ConvertSmallIntToByte();
	void __fastcall ConvertSmallIntToSingle();
	void __fastcall ConvertSingleToByte();
	void __fastcall ConvertSingleToSmallInt();
	
protected:
	TGLHeightDataThread* FThread;
	void __fastcall SetDataType(const TGLHeightDataType Val);
	void __fastcall SetMaterialName(const System::UnicodeString MaterialName);
	void __fastcall SetLibMaterial(Gls::Material::TGLLibMaterial* LibMaterial);
	float __fastcall GetHeightMin();
	float __fastcall GetHeightMax();
	
public:
	TGLHeightData* OldVersion;
	TGLHeightData* NewVersion;
	bool DontUse;
	__fastcall virtual TGLHeightData(TGLHeightDataSource* AOwner, int aXLeft, int aYTop, int aSize, TGLHeightDataType aDataType);
	__fastcall virtual ~TGLHeightData();
	__property TGLHeightDataSource* Owner = {read=FOwner};
	__property System::Classes::TNotifyEvent OnDestroy = {read=FOnDestroy, write=FOnDestroy};
	__property int UseCounter = {read=FUseCounter, nodefault};
	void __fastcall RegisterUse();
	virtual void __fastcall Allocate(const TGLHeightDataType Val);
	void __fastcall Release();
	void __fastcall MarkDirty();
	__property int XLeft = {read=FXLeft, nodefault};
	__property int YTop = {read=FYTop, nodefault};
	__property TGLHeightDataType DataType = {read=FDataType, write=SetDataType, nodefault};
	__property TGLHeightDataState DataState = {read=FDataState, write=FDataState, nodefault};
	__property int Size = {read=FSize, nodefault};
	__property bool Dirty = {read=FDirty, write=FDirty, nodefault};
	__property int DataSize = {read=FDataSize, nodefault};
	__property Gls::Vectorgeometry::PByteVector ByteData = {read=FByteData};
	__property PByteRaster ByteRaster = {read=FByteRaster};
	__property PSmallIntArray SmallIntData = {read=FSmallIntData};
	__property PSmallIntRaster SmallIntRaster = {read=FSmallIntRaster};
	__property Gls::Vectorgeometry::PFloatVector SingleData = {read=FSingleData};
	__property PSingleRaster SingleRaster = {read=FSingleRaster};
	__property System::UnicodeString MaterialName = {read=FMaterialName, write=SetMaterialName};
	__property Gls::Material::TGLLibMaterial* LibMaterial = {read=FLibMaterial, write=SetLibMaterial};
	__property THDTextureCoordinatesMode TextureCoordinatesMode = {read=FTextureCoordinatesMode, write=FTextureCoordinatesMode, nodefault};
	__property Gls::Vectorgeometry::TTexPoint TextureCoordinatesOffset = {read=FTCOffset, write=FTCOffset};
	__property Gls::Vectorgeometry::TTexPoint TextureCoordinatesScale = {read=FTCScale, write=FTCScale};
	System::Byte __fastcall ByteHeight(int x, int y);
	short __fastcall SmallIntHeight(int x, int y);
	float __fastcall SingleHeight(int x, int y);
	float __fastcall InterpolatedHeight(float x, float y);
	__property float HeightMin = {read=GetHeightMin, write=FHeightMin};
	__property float HeightMax = {read=GetHeightMax, write=FHeightMax};
	float __fastcall Height(int x, int y);
	Gls::Vectortypes::TVector3f __fastcall Normal(int x, int y, const Gls::Vectortypes::TVector3f &scale);
	Gls::Vectortypes::TVector3f __fastcall NormalAtNode(int x, int y, const Gls::Vectortypes::TVector3f &scale);
	bool __fastcall OverlapsArea(const System::Types::TRect &Area);
	__property System::TObject* ObjectTag = {read=FObjectTag, write=FObjectTag};
	__property int Tag = {read=FTag, write=FTag, nodefault};
	__property int Tag2 = {read=FTag2, write=FTag2, nodefault};
	__property TGLHeightDataThread* Thread = {read=FThread, write=FThread};
};


class PASCALIMPLEMENTATION TGLHeightDataThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
protected:
	TGLHeightData* FHeightData;
	
public:
	__fastcall virtual ~TGLHeightDataThread();
	__property TGLHeightData* HeightData = {read=FHeightData, write=FHeightData};
public:
	/* TThread.Create */ inline __fastcall TGLHeightDataThread()/* overload */ : System::Classes::TThread() { }
	/* TThread.Create */ inline __fastcall TGLHeightDataThread(bool CreateSuspended)/* overload */ : System::Classes::TThread(CreateSuspended) { }
	/* TThread.Create */ inline __fastcall TGLHeightDataThread(bool CreateSuspended, NativeUInt ReservedStackSize)/* overload */ : System::Classes::TThread(CreateSuspended, ReservedStackSize) { }
	
};


class PASCALIMPLEMENTATION TGLBitmapHDS : public TGLHeightDataSource
{
	typedef TGLHeightDataSource inherited;
	
	
private:
	typedef System::DynamicArray<Gls::Vectorgeometry::PByteVector> _TGLBitmapHDS__1;
	
	
private:
	_TGLBitmapHDS__1 FScanLineCache;
	Vcl::Graphics::TBitmap* FBitmap;
	Vcl::Graphics::TPicture* FPicture;
	bool FInfiniteWrap;
	bool FInverted;
	
protected:
	void __fastcall SetPicture(Vcl::Graphics::TPicture* const Val);
	void __fastcall OnPictureChanged(System::TObject* sender);
	void __fastcall SetInfiniteWrap(bool Val);
	void __fastcall SetInverted(bool Val);
	void __fastcall CreateMonochromeBitmap(int size);
	void __fastcall FreeMonochromeBitmap();
	Gls::Vectorgeometry::PByteVector __fastcall GetScanLine(int y);
	
public:
	__fastcall virtual TGLBitmapHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBitmapHDS();
	virtual void __fastcall StartPreparingData(TGLHeightData* HeightData);
	virtual void __fastcall MarkDirty(const System::Types::TRect &Area)/* overload */;
	virtual int __fastcall Width();
	virtual int __fastcall Height();
	
__published:
	__property Vcl::Graphics::TPicture* Picture = {read=FPicture, write=SetPicture};
	__property bool InfiniteWrap = {read=FInfiniteWrap, write=SetInfiniteWrap, default=1};
	__property bool Inverted = {read=FInverted, write=SetInverted, default=1};
	__property MaxPoolSize;
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  MarkDirty(int XLeft, int YTop, int xRight, int yBottom){ TGLHeightDataSource::MarkDirty(XLeft, YTop, xRight, yBottom); }
	inline void __fastcall  MarkDirty(){ TGLHeightDataSource::MarkDirty(); }
	
};


typedef void __fastcall (__closure *TStartPreparingDataEvent)(TGLHeightData* HeightData);

typedef void __fastcall (__closure *TMarkDirtyEvent)(const System::Types::TRect &Area);

class PASCALIMPLEMENTATION TGLCustomHDS : public TGLHeightDataSource
{
	typedef TGLHeightDataSource inherited;
	
private:
	TStartPreparingDataEvent FOnStartPreparingData;
	TMarkDirtyEvent FOnMarkDirty;
	
public:
	__fastcall virtual TGLCustomHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomHDS();
	virtual void __fastcall StartPreparingData(TGLHeightData* HeightData);
	virtual void __fastcall MarkDirty(const System::Types::TRect &Area)/* overload */;
	
__published:
	__property MaxPoolSize;
	__property TStartPreparingDataEvent OnStartPreparingData = {read=FOnStartPreparingData, write=FOnStartPreparingData};
	__property TMarkDirtyEvent OnMarkDirtyEvent = {read=FOnMarkDirty, write=FOnMarkDirty};
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  MarkDirty(int XLeft, int YTop, int xRight, int yBottom){ TGLHeightDataSource::MarkDirty(XLeft, YTop, xRight, yBottom); }
	inline void __fastcall  MarkDirty(){ TGLHeightDataSource::MarkDirty(); }
	
};


class PASCALIMPLEMENTATION TGLTerrainBaseHDS : public TGLHeightDataSource
{
	typedef TGLHeightDataSource inherited;
	
public:
	__fastcall virtual TGLTerrainBaseHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTerrainBaseHDS();
	virtual void __fastcall StartPreparingData(TGLHeightData* HeightData);
	
__published:
	__property MaxPoolSize;
};


typedef void __fastcall (__closure *TSourceDataFetchedEvent)(TGLHeightDataSourceFilter* sender, TGLHeightData* HeightData);

class PASCALIMPLEMENTATION TGLHeightDataSourceFilter : public TGLHeightDataSource
{
	typedef TGLHeightDataSource inherited;
	
private:
	TGLHeightDataSource* FHDS;
	TSourceDataFetchedEvent FOnSourceDataFetched;
	bool FActive;
	
protected:
	virtual void __fastcall PreparingData(TGLHeightData* HeightData) = 0 ;
	void __fastcall SetHDS(TGLHeightDataSource* Val);
	
public:
	__fastcall virtual TGLHeightDataSourceFilter(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLHeightDataSourceFilter();
	virtual void __fastcall Release(TGLHeightData* aHeightData);
	virtual void __fastcall StartPreparingData(TGLHeightData* HeightData);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual int __fastcall Width();
	virtual int __fastcall Height();
	__property TSourceDataFetchedEvent OnSourceDataFetched = {read=FOnSourceDataFetched, write=FOnSourceDataFetched};
	
__published:
	__property MaxPoolSize;
	__property TGLHeightDataSource* HeightDataSource = {read=FHDS, write=SetHDS};
	__property bool Active = {read=FActive, write=FActive, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Heightdata */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_HEIGHTDATA)
using namespace Gls::Heightdata;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_HeightdataHPP
