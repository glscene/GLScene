// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.TerrainRenderer.pas' rev: 35.00 (Windows)

#ifndef Gls_TerrainrendererHPP
#define Gls_TerrainrendererHPP

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
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.HeightData.hpp>
#include <GLS.Material.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Context.hpp>
#include <GLS.ROAMPatch.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.XOpenGL.hpp>
#include <GLS.Utils.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Texture.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Terrainrenderer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTerrainRenderer;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TGetTerrainBoundsEvent)(float &l, float &t, float &r, float &b);

typedef void __fastcall (__closure *TPatchPostRenderEvent)(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::Classes::TList* const patches);

typedef void __fastcall (__closure *TGLHeightDataPostRenderEvent)(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::Classes::TList* &HeightDatas);

typedef void __fastcall (__closure *TMaxCLODTrianglesReachedEvent)(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);

enum DECLSPEC_DENUM TTerrainHighResStyle : unsigned char { hrsFullGeometry, hrsTesselated };

enum DECLSPEC_DENUM TTerrainOcclusionTesselate : unsigned char { totTesselateAlways, totTesselateIfVisible };

enum DECLSPEC_DENUM TTileManagementFlag : unsigned char { tmClearUsedFlags, tmMarkUsedTiles, tmReleaseUnusedTiles, tmAllocateNewTiles, tmWaitForPreparing };

typedef System::Set<TTileManagementFlag, TTileManagementFlag::tmClearUsedFlags, TTileManagementFlag::tmWaitForPreparing> TTileManagementFlags;

class PASCALIMPLEMENTATION TGLTerrainRenderer : public Gls::Scene::TGLSceneObject
{
	typedef Gls::Scene::TGLSceneObject inherited;
	
private:
	Gls::Heightdata::TGLHeightDataSource* FHeightDataSource;
	int FTileSize;
	float FQualityDistance;
	float FinvTileSize;
	int FLastTriangleCount;
	float FTilesPerTexture;
	int FMaxCLODTriangles;
	int FCLODPrecision;
	Gls::Vectorlists::TGLAffineVectorList* FBufferVertices;
	Gls::Vectorlists::TGLTexPointList* FBufferTexPoints;
	Gls::Vectorlists::TGLIntegerList* FBufferVertexIndices;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	TGetTerrainBoundsEvent FOnGetTerrainBounds;
	TPatchPostRenderEvent FOnPatchPostRender;
	TGLHeightDataPostRenderEvent FOnHeightDataPostRender;
	TMaxCLODTrianglesReachedEvent FOnMaxCLODTrianglesReached;
	TTerrainHighResStyle FQualityStyle;
	int FOcclusionFrameSkip;
	TTerrainOcclusionTesselate FOcclusionTesselate;
	int FContourInterval;
	int FContourWidth;
	
protected:
	System::StaticArray<System::Classes::TList*, 256> FTilesHash;
	void __fastcall MarkAllTilesAsUnused();
	void __fastcall ReleaseAllUnusedTiles();
	void __fastcall MarkHashedTileAsUsed(const Gls::Vectortypes::TVector3f &tilePos);
	Gls::Heightdata::TGLHeightData* __fastcall HashedTile(const Gls::Vectortypes::TVector3f &tilePos, bool canAllocate = true)/* overload */;
	Gls::Heightdata::TGLHeightData* __fastcall HashedTile(const int xLeft, const int yTop, bool canAllocate = true)/* overload */;
	void __fastcall SetHeightDataSource(Gls::Heightdata::TGLHeightDataSource* const val);
	void __fastcall SetTileSize(const int val);
	void __fastcall SetTilesPerTexture(const float val);
	void __fastcall SetCLODPrecision(const int val);
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const Val);
	void __fastcall SetQualityStyle(const TTerrainHighResStyle Val);
	void __fastcall SetOcclusionFrameSkip(int Val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DestroyHandle();
	void __fastcall ReleaseAllTiles();
	void __fastcall OnTileDestroyed(System::TObject* Sender);
	Gls::Roampatch::TGLROAMPatch* __fastcall GetPreparedPatch(const Gls::Vectortypes::TVector3f &TilePos, const Gls::Vectortypes::TVector3f &EyePos, float TexFactor, System::Classes::TList* HDList);
	
public:
	TTileManagementFlags TileManagement;
	__fastcall virtual TGLTerrainRenderer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTerrainRenderer();
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	float __fastcall InterpolatedHeight(const Gls::Vectortypes::TVector4f &p)/* overload */;
	float __fastcall InterpolatedHeight(const Gls::Vectortypes::TVector3f &p)/* overload */;
	__property int LastTriangleCount = {read=FLastTriangleCount, nodefault};
	int __fastcall HashedTileCount();
	
__published:
	__property Gls::Heightdata::TGLHeightDataSource* HeightDataSource = {read=FHeightDataSource, write=SetHeightDataSource};
	__property int TileSize = {read=FTileSize, write=SetTileSize, default=16};
	__property float TilesPerTexture = {read=FTilesPerTexture, write=SetTilesPerTexture};
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property float QualityDistance = {read=FQualityDistance, write=FQualityDistance};
	__property TTerrainHighResStyle QualityStyle = {read=FQualityStyle, write=SetQualityStyle, default=0};
	__property int MaxCLODTriangles = {read=FMaxCLODTriangles, write=FMaxCLODTriangles, default=65536};
	__property int CLODPrecision = {read=FCLODPrecision, write=SetCLODPrecision, default=100};
	__property int OcclusionFrameSkip = {read=FOcclusionFrameSkip, write=SetOcclusionFrameSkip, default=0};
	__property TTerrainOcclusionTesselate OcclusionTesselate = {read=FOcclusionTesselate, write=FOcclusionTesselate, default=1};
	__property TGetTerrainBoundsEvent OnGetTerrainBounds = {read=FOnGetTerrainBounds, write=FOnGetTerrainBounds};
	__property TPatchPostRenderEvent OnPatchPostRender = {read=FOnPatchPostRender, write=FOnPatchPostRender};
	__property TGLHeightDataPostRenderEvent OnHeightDataPostRender = {read=FOnHeightDataPostRender, write=FOnHeightDataPostRender};
	__property TMaxCLODTrianglesReachedEvent OnMaxCLODTrianglesReached = {read=FOnMaxCLODTrianglesReached, write=FOnMaxCLODTrianglesReached};
	__property int ContourInterval = {read=FContourInterval, write=FContourInterval, default=0};
	__property int ContourWidth = {read=FContourWidth, write=FContourWidth, default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTerrainRenderer(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Byte cTilesHashSize = System::Byte(0xff);
}	/* namespace Terrainrenderer */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_TERRAINRENDERER)
using namespace Gls::Terrainrenderer;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_TerrainrendererHPP
