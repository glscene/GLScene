// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.RandomHDS.pas' rev: 35.00 (Windows)

#ifndef Gls_RandomhdsHPP
#define Gls_RandomhdsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <System.SysUtils.hpp>
#include <System.UITypes.hpp>
#include <System.Contnrs.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include <Vcl.Forms.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.HeightData.hpp>
#include <GLS.TerrainRenderer.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Color.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Material.hpp>
#include <GLS.Context.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Randomhds
{
//-- forward type declarations -----------------------------------------------
struct TSeaErosion;
struct TRainErosion;
struct TLifeErosion;
struct TFractionErosion;
struct TLandTileInfo;
struct TSteps;
class DELPHICLASS TGLBaseRandomHDS;
class DELPHICLASS TGLCustomRandomHDS;
class DELPHICLASS TGLFractalHDS;
struct TRelativeCoordinate;
class DELPHICLASS TGLTiledRndLandscape;
class DELPHICLASS TGLFractalArchipelago;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TSeaErosion
{
public:
	bool Enabled;
	float BeachHeight;
};


struct DECLSPEC_DRECORD TRainErosion
{
public:
	bool Enabled;
	float ErosionRate;
	float DepositRate;
};


struct DECLSPEC_DRECORD TLifeErosion
{
public:
	bool Enabled;
	float Robustness;
};


struct DECLSPEC_DRECORD TFractionErosion
{
public:
	bool Enabled;
	float Slope;
};


struct DECLSPEC_DRECORD TLandTileInfo
{
public:
	int x;
	int z;
	Gls::Heightdata::TGLHeightDataState State;
};


struct DECLSPEC_DRECORD TSteps
{
public:
	bool Enabled;
	int Count;
};


typedef System::DynamicArray<float> Gls_Randomhds__1;

typedef System::DynamicArray<System::DynamicArray<float> > TMapOfSingle;

typedef System::DynamicArray<Gls::Vectortypes::TVector4f> Gls_Randomhds__2;

typedef System::DynamicArray<System::DynamicArray<Gls::Vectortypes::TVector4f> > TMapOfVector;

typedef Gls::Vectortypes::TVector4f __fastcall (__closure *TOnDrawTexture)(TGLBaseRandomHDS* const Sender, int x, int y, double z, const Gls::Vectortypes::TVector4f &Normal);

typedef void __fastcall (__closure *TSingleClamp)(float &x, float &y);

typedef void __fastcall (__closure *TIntegerClamp)(int &x, int &y);

class PASCALIMPLEMENTATION TGLBaseRandomHDS : public Gls::Heightdata::TGLHeightDataSource
{
	typedef Gls::Heightdata::TGLHeightDataSource inherited;
	
private:
	TSteps FSteps;
	bool FLandCover;
	void __fastcall SetOnDrawTexture(const TOnDrawTexture Value);
	void __fastcall SetSteps(const TSteps &Value);
	void __fastcall SetLandCover(const bool Value);
	
protected:
	int FSeed;
	int FSize;
	System::UnicodeString FMaterialName;
	bool FLighting;
	Gls::Vectortypes::TVector4f FLightDirection;
	Gls::Terrainrenderer::TGLTerrainRenderer* FTerrainRenderer;
	Gls::Vectortypes::TVector4f FLightColor;
	bool FShadows;
	bool FSea;
	float FSeaLevel;
	float FAmbientLight;
	int FTaskProgress;
	int FTextureScale;
	TFractionErosion FErosionByFraction;
	bool FLightSmoothing;
	bool FCyclic;
	float FSeaTransparency;
	bool FPrimerLandscape;
	TLandTileInfo FLandTileInfo;
	TOnDrawTexture FOnDrawTexture;
	Gls::Vectortypes::TVector4f __fastcall OnDrawTextureDefault(TGLBaseRandomHDS* const Sender, int x, int y, double z, const Gls::Vectortypes::TVector4f &Normal);
	void __fastcall SetSeed(const int Value);
	void __fastcall SetMaterialName(const System::UnicodeString Value);
	void __fastcall SetLighting(const bool Value);
	void __fastcall SetLightDirection(const Gls::Vectortypes::TVector4f &Value);
	virtual void __fastcall SetTerrainRenderer(Gls::Terrainrenderer::TGLTerrainRenderer* const Value) = 0 ;
	void __fastcall SetLightColor(const Gls::Vectortypes::TVector4f &Value);
	void __fastcall SetShadows(const bool Value);
	void __fastcall SetSea(const bool Value);
	void __fastcall SetSeaLevel(const float Value);
	void __fastcall SetAmbientLight(const float Value);
	void __fastcall SetErosionByRain(const TRainErosion &Value);
	TRainErosion __fastcall GetErosionByRain();
	void __fastcall SetErosionBySea(const TSeaErosion &Value);
	void __fastcall SetTextureScale(const int Value);
	void __fastcall SetErosionByLife(const TLifeErosion &Value);
	void __fastcall SetErosionByFraction(const TFractionErosion &Value);
	void __fastcall SetLightSmoothing(const bool Value);
	void __fastcall SetSeaTransparency(const float Value);
	void __fastcall SetPrimerLandscape(const bool Value);
	float __fastcall GetSeaLevel();
	float __fastcall GetSeaTransparency();
	void __fastcall SetLandTileInfo(const TLandTileInfo &Value);
	TLandTileInfo __fastcall GetLandTileInfo();
	virtual void __fastcall SetCyclic(const bool Value) = 0 ;
	
public:
	TRainErosion FErosionByRain;
	TSeaErosion FErosionBySea;
	TLifeErosion FErosionByLife;
	__fastcall virtual TGLBaseRandomHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBaseRandomHDS();
	__property Gls::Vectortypes::TVector4f LightColor = {read=FLightColor, write=SetLightColor};
	__property Gls::Vectortypes::TVector4f LightDirection = {read=FLightDirection, write=SetLightDirection};
	__property TOnDrawTexture OnDrawTexture = {read=FOnDrawTexture, write=SetOnDrawTexture};
	
__published:
	__property float AmbientLight = {read=FAmbientLight, write=SetAmbientLight};
	__property bool Cyclic = {read=FCyclic, write=SetCyclic, nodefault};
	
public:
	__property TFractionErosion ErosionByFraction = {read=FErosionByFraction, write=SetErosionByFraction};
	__property TLifeErosion ErosionByLife = {read=FErosionByLife, write=SetErosionByLife};
	__property TRainErosion ErosionByRain = {read=FErosionByRain, write=SetErosionByRain};
	__property TSeaErosion ErosionBySea = {read=FErosionBySea, write=SetErosionBySea};
	
__published:
	__property bool LandCover = {read=FLandCover, write=SetLandCover, nodefault};
	__property bool Lighting = {read=FLighting, write=SetLighting, nodefault};
	__property bool LightSmoothing = {read=FLightSmoothing, write=SetLightSmoothing, nodefault};
	__property System::UnicodeString MaterialName = {read=FMaterialName, write=SetMaterialName};
	__property bool PrimerLandscape = {read=FPrimerLandscape, write=SetPrimerLandscape, nodefault};
	__property bool Sea = {read=FSea, write=SetSea, nodefault};
	__property float SeaLevel = {read=GetSeaLevel, write=SetSeaLevel};
	__property float SeaTransparency = {read=GetSeaTransparency, write=SetSeaTransparency};
	__property int Seed = {read=FSeed, write=SetSeed, nodefault};
	__property bool Shadows = {read=FShadows, write=SetShadows, nodefault};
	
public:
	__property TSteps Steps = {read=FSteps, write=SetSteps};
	
__published:
	__property Gls::Terrainrenderer::TGLTerrainRenderer* TerrainRenderer = {read=FTerrainRenderer, write=SetTerrainRenderer};
	__property int TextureScale = {read=FTextureScale, write=SetTextureScale, nodefault};
};


class PASCALIMPLEMENTATION TGLCustomRandomHDS : public TGLBaseRandomHDS
{
	typedef TGLBaseRandomHDS inherited;
	
private:
	bool FSlave;
	float FMaxHeight;
	float FMinHeight;
	float FRangeHeight;
	System::UnicodeString FTask;
	TSingleClamp FSingleConstrain;
	TIntegerClamp FIntegerConstrain;
	bool FKeepNormals;
	float __fastcall GetHeight(int x, int y);
	void __fastcall SetHeight(int x, int y, const float Value);
	void __fastcall SetKeepNormals(const bool Value);
	
protected:
	virtual void __fastcall SetTerrainRenderer(Gls::Terrainrenderer::TGLTerrainRenderer* const Value);
	virtual void __fastcall SetCyclic(const bool Value);
	void __fastcall BoundaryClamp(float &x, float &y)/* overload */;
	void __fastcall BoundaryClamp(int &x, int &y)/* overload */;
	void __fastcall CyclicClamp(float &x, float &y)/* overload */;
	void __fastcall CyclicClamp(int &x, int &y)/* overload */;
	void __fastcall GetTerrainBounds(float &l, float &t, float &r, float &b);
	void __fastcall SetSize(const int aSize);
	
public:
	TMapOfSingle FHeight;
	TMapOfSingle FLightMap;
	TMapOfVector FNormal;
	int __fastcall BoundaryX();
	int __fastcall BoundaryZ();
	virtual void __fastcall BuildHeightField() = 0 /* overload */;
	void __fastcall BuildLandscape();
	void __fastcall BuildLightMap()/* overload */;
	void __fastcall BuildLightMap(const Gls::Vectortypes::TVector4f &aLightDirection)/* overload */;
	void __fastcall BuildNormals();
	void __fastcall BuildTexture();
	void __fastcall ClearHeightField();
	void __fastcall ClearLightMap();
	void __fastcall ConstrainCoordinates(float &x, float &y)/* overload */;
	void __fastcall ConstrainCoordinates(int &x, int &y)/* overload */;
	__fastcall virtual TGLCustomRandomHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomRandomHDS();
	void __fastcall DoCyclicBoundaries();
	void __fastcall DoErosionByFraction();
	void __fastcall DoErosionByLife();
	void __fastcall DoErosionByRain();
	void __fastcall DoErosionBySea();
	void __fastcall DoSea();
	void __fastcall DoSteps();
	__property float Heights[int x][int y] = {read=GetHeight, write=SetHeight};
	float __fastcall Interpolate(float x, float y);
	__property bool KeepNormals = {read=FKeepNormals, write=SetKeepNormals, nodefault};
	__property TLandTileInfo LandTileInfo = {read=GetLandTileInfo, write=SetLandTileInfo};
	bool __fastcall PointInMap(const float x, const float y)/* overload */;
	bool __fastcall PointInMap(const int x, const int y)/* overload */;
	__property float MaxHeight = {read=FMaxHeight};
	__property float MinHeight = {read=FMinHeight};
	Gls::Vectortypes::TVector4f __fastcall Normal(const Gls::Vectortypes::TVector4f &Position);
	__property float RangeHeight = {read=FRangeHeight};
	Gls::Coordinates::TGLCoordinates3* __fastcall Scale();
	__property int Size = {read=FSize, nodefault};
	float __fastcall StandardisedHeight(const int x, const int y);
	__property System::UnicodeString Task = {read=FTask};
	__property int TaskProgress = {read=FTaskProgress, nodefault};
	float __fastcall XMoveBoundary();
	float __fastcall ZMoveBoundary();
	virtual void __fastcall StartPreparingData(Gls::Heightdata::TGLHeightData* heightData);
	
__published:
	__property bool Cyclic = {read=FCyclic, write=SetCyclic, nodefault};
};


class PASCALIMPLEMENTATION TGLFractalHDS : public TGLCustomRandomHDS
{
	typedef TGLCustomRandomHDS inherited;
	
private:
	int FAmplitude;
	int FDepth;
	float FRoughness;
	void __fastcall SetAmplitude(const int Value);
	void __fastcall SetDepth(const int Value);
	void __fastcall SetRoughness(const float Value);
	
public:
	virtual void __fastcall BuildHeightField()/* overload */;
	HIDESBASE void __fastcall BuildHeightField(const int aDepth, const int aSeed, const int aAmplitude)/* overload */;
	__fastcall virtual TGLFractalHDS(System::Classes::TComponent* AOwner);
	
__published:
	__property int Amplitude = {read=FAmplitude, write=SetAmplitude, nodefault};
	__property int Depth = {read=FDepth, write=SetDepth, nodefault};
	__property float Roughness = {read=FRoughness, write=SetRoughness};
public:
	/* TGLCustomRandomHDS.Destroy */ inline __fastcall virtual ~TGLFractalHDS() { }
	
};


typedef TGLCustomRandomHDS TGLLandTile;

struct DECLSPEC_DRECORD TRelativeCoordinate
{
public:
	int DX;
	int DZ;
};


typedef void __fastcall (__closure *TOnCreateLandTile)(int x, int z, int Seed, TGLCustomRandomHDS* &aLandscape);

typedef bool __fastcall (__closure *TIsDefaultTile)(int X, int Z);

class PASCALIMPLEMENTATION TGLTiledRndLandscape : public TGLBaseRandomHDS
{
	typedef TGLBaseRandomHDS inherited;
	
	
private:
	typedef System::DynamicArray<TRelativeCoordinate> _TGLTiledRndLandscape__1;
	
	
private:
	bool FLandTileComputing;
	int FExtentX;
	int FExtentZ;
	int FExtentXhalf;
	int FExtentZhalf;
	int fLandTileSize;
	TSingleClamp FSingleConstrain;
	TIntegerClamp FIntegerConstrain;
	Gls::Terrainrenderer::TGLTerrainRenderer* FTerrainRenderer;
	Gls::Scene::TGLCamera* FCamera;
	TOnCreateLandTile fOnCreateLandTile;
	Gls::Heightdata::TStartPreparingDataEvent fOnCreateDefaultTile;
	TIsDefaultTile FIsDefaultTile;
	int FSeed;
	int fBaseSeed;
	TGLCustomRandomHDS* fComputedLandTile;
	int FLandTileCapacity;
	int FGenerationRadius;
	float FLandTileDensity;
	void __fastcall fDefaultOnCreateDefaultTile(Gls::Heightdata::TGLHeightData* heightData);
	bool __fastcall fDefaultIsDefaultTile(int x, int z);
	void __fastcall SetExtentX(const int Value);
	void __fastcall SetExtentZ(const int Value);
	void __fastcall SetOnCreateLandTile(const TOnCreateLandTile Value);
	void __fastcall SetCamera(Gls::Scene::TGLCamera* const Value);
	void __fastcall SetIsDefaultTile(const TIsDefaultTile Value);
	HIDESBASE void __fastcall SetSeed(const int Value);
	void __fastcall SetOnCreateDefaultTile(const Gls::Heightdata::TStartPreparingDataEvent Value);
	System::UnicodeString __fastcall GetTask();
	int __fastcall GetTaskProgress();
	void __fastcall SetLandTileCapacity(const int Value);
	void __fastcall SetGenerationRadius(const int Value);
	void __fastcall SetLandTileDensity(const float Value);
	
protected:
	_TGLTiledRndLandscape__1 FGenRadius;
	int FOldCamX;
	int FOldCamZ;
	bool FMapUpdating;
	System::Contnrs::TComponentList* FLandTiles;
	void __fastcall BoundaryClamp(float &x, float &z)/* overload */;
	void __fastcall BoundaryClamp(int &x, int &z)/* overload */;
	virtual void __fastcall ComputeLandTile(const int aX, const int aZ, TGLCustomRandomHDS* &NewLandTile);
	void __fastcall CyclicClamp(float &x, float &z)/* overload */;
	void __fastcall CyclicClamp(int &x, int &z)/* overload */;
	void __fastcall GetTerrainBounds(float &l, float &t, float &r, float &b);
	int __fastcall LandTileSeed(int x, int z);
	__property Gls::Heightdata::TStartPreparingDataEvent OnCreateDefaultTile = {read=fOnCreateDefaultTile, write=SetOnCreateDefaultTile};
	virtual void __fastcall SetCyclic(const bool Value);
	void __fastcall SetSize(const int aSize);
	int __fastcall fSortLandscapes(void * Item1, void * Item2);
	void __fastcall PrepareLandTileData(Gls::Heightdata::TGLHeightData* HeightData, TGLCustomRandomHDS* LandTile);
	virtual void __fastcall SetTerrainRenderer(Gls::Terrainrenderer::TGLTerrainRenderer* const Value);
	
public:
	void __fastcall ApplyLighting(TGLCustomRandomHDS* &aLandTile);
	void __fastcall ApplyTexture(TGLCustomRandomHDS* &aLandTile);
	void __fastcall ApplyTopography(TGLCustomRandomHDS* &aLandTile);
	void __fastcall CameraPosition(int &TileX, int &TileZ);
	HIDESBASE void __fastcall CleanUp();
	void __fastcall ConstrainCoordinates(float &x, float &z)/* overload */;
	void __fastcall ConstrainCoordinates(int &x, int &z)/* overload */;
	__fastcall virtual TGLTiledRndLandscape(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTiledRndLandscape();
	void __fastcall FindLandTile(const float x, const float z, int &TileX, int &TileZ);
	virtual void __fastcall Initialize(const float aX, const float aZ);
	__property TIsDefaultTile IsDefaultTile = {read=FIsDefaultTile, write=SetIsDefaultTile};
	int __fastcall LandtileCount();
	__property int LandTileSize = {read=fLandTileSize, nodefault};
	__property TOnCreateLandTile OnCreateLandTile = {read=fOnCreateLandTile, write=SetOnCreateLandTile};
	__property System::UnicodeString Task = {read=GetTask};
	__property int TaskProgress = {read=GetTaskProgress, nodefault};
	float __fastcall TileDistance(const int x1, const int z1, const int x2, const int z2);
	int __fastcall TileDistanceSquared(const int x1, const int z1, const int x2, const int z2);
	void __fastcall Update();
	__property bool MapUpdating = {read=FMapUpdating, nodefault};
	float __fastcall XMoveBoundary();
	float __fastcall ZMoveBoundary();
	virtual void __fastcall StartPreparingData(Gls::Heightdata::TGLHeightData* heightData);
	
__published:
	__property Gls::Scene::TGLCamera* Camera = {read=FCamera, write=SetCamera};
	__property bool Cyclic = {read=FCyclic, write=SetCyclic, nodefault};
	__property int ExtentX = {read=FExtentX, write=SetExtentX, nodefault};
	__property int ExtentZ = {read=FExtentZ, write=SetExtentZ, nodefault};
	__property int GenerationRadius = {read=FGenerationRadius, write=SetGenerationRadius, nodefault};
	__property int LandTileCapacity = {read=FLandTileCapacity, write=SetLandTileCapacity, nodefault};
	__property float LandTileDensity = {read=FLandTileDensity, write=SetLandTileDensity};
	__property int Seed = {read=FSeed, write=SetSeed, nodefault};
	__property Gls::Terrainrenderer::TGLTerrainRenderer* TerrainRenderer = {read=FTerrainRenderer, write=SetTerrainRenderer};
};


class PASCALIMPLEMENTATION TGLFractalArchipelago : public TGLTiledRndLandscape
{
	typedef TGLTiledRndLandscape inherited;
	
private:
	int FDepth;
	float FRoughnessMax;
	float FRoughnessMin;
	int FAmplitudeMin;
	int FAmplitudeMax;
	bool FSeaDynamic;
	System::UnicodeString FSeaMaterialName;
	float FWaveAmplitude;
	float FWaveSpeed;
	float __fastcall GetIslandDensity();
	void __fastcall FPostRenderSeaStatic(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::Classes::TList* &HeightDatas);
	void __fastcall FPostRenderSeaDynamic(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::Classes::TList* &HeightDatas);
	void __fastcall SetIslandDensity(const float Value);
	void __fastcall SetDepth(const int Value);
	void __fastcall SetRoughnessMax(const float Value);
	void __fastcall SetRoughnessMin(const float Value);
	void __fastcall SetAmplitudeMax(const int Value);
	void __fastcall SetAmplitudeMin(const int Value);
	void __fastcall SetSeaDynamic(const bool Value);
	void __fastcall SetSeaMaterialName(const System::UnicodeString Value);
	void __fastcall SetWaveAmplitude(const float Value);
	void __fastcall SetWaveSpeed(const float Value);
	
protected:
	virtual void __fastcall SetTerrainRenderer(Gls::Terrainrenderer::TGLTerrainRenderer* const Value);
	HIDESBASE void __fastcall fOnCreateLandTile(int aX, int aZ, int aSeed, TGLCustomRandomHDS* &aLandscape);
	HIDESBASE void __fastcall fOnCreateDefaultTile(Gls::Heightdata::TGLHeightData* heightData);
	
public:
	virtual void __fastcall ComputeLandTile(const int aX, const int aZ, TGLCustomRandomHDS* &NewLandTile);
	__fastcall virtual TGLFractalArchipelago(System::Classes::TComponent* AOwner);
	
__published:
	__property int AmplitudeMax = {read=FAmplitudeMax, write=SetAmplitudeMax, nodefault};
	__property int AmplitudeMin = {read=FAmplitudeMin, write=SetAmplitudeMin, nodefault};
	__property int Depth = {read=FDepth, write=SetDepth, nodefault};
	__property float IslandDensity = {read=GetIslandDensity, write=SetIslandDensity};
	__property float RoughnessMax = {read=FRoughnessMax, write=SetRoughnessMax};
	__property float RoughnessMin = {read=FRoughnessMin, write=SetRoughnessMin};
	__property bool SeaDynamic = {read=FSeaDynamic, write=SetSeaDynamic, nodefault};
	__property System::UnicodeString SeaMaterialName = {read=FSeaMaterialName, write=SetSeaMaterialName};
	__property float WaveAmplitude = {read=FWaveAmplitude, write=SetWaveAmplitude};
	__property float WaveSpeed = {read=FWaveSpeed, write=SetWaveSpeed};
public:
	/* TGLTiledRndLandscape.Destroy */ inline __fastcall virtual ~TGLFractalArchipelago() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Byte VerticalScalingFactor = System::Byte(0x80);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall LoadJPGtexture(const System::UnicodeString JpgName);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall NoisyColor(const System::Uitypes::TColor Color, const float Noise = 5.000000E-02f);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall TextureSand(const int x, const int y);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall TextureBrownSoil(const int x, const int y);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall TextureDarkGreen(const int x, const int y);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall TextureDarkGray(const int x, const int y);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall TextureWhite(const int x, const int y);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall TextureBlue(const int x, const int y);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall TextureGreen(const int x, const int y);
extern DELPHI_PACKAGE void __fastcall InitializeRandomGenerator(const int Seed);
extern DELPHI_PACKAGE void __fastcall FractalMiddlePointHDS(const int aDepth, const int aSeed, const int aAmplitude, const float aRoughness, bool aCyclic, TMapOfSingle &z, float &MinZ, float &MaxZ);
extern DELPHI_PACKAGE void __fastcall PrimerNull(TMapOfSingle &z);
extern DELPHI_PACKAGE void __fastcall PrimerIsland(float LowZ, float HighZ, TMapOfSingle &z);
}	/* namespace Randomhds */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_RANDOMHDS)
using namespace Gls::Randomhds;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_RandomhdsHPP
