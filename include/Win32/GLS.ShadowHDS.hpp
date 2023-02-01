// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.ShadowHDS.pas' rev: 35.00 (Windows)

#ifndef Gls_ShadowhdsHPP
#define Gls_ShadowhdsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.HeightData.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Texture.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.Material.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Shadowhds
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLShadowHDS;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TNewTilePreparedEvent)(TGLShadowHDS* Sender, Gls::Heightdata::TGLHeightData* heightData, Gls::Material::TGLLibMaterial* ShadowMapMaterial);

typedef void __fastcall (__closure *TThreadBmp32)(TGLShadowHDS* Sender, Gls::Heightdata::TGLHeightData* heightData, Gls::Graphics::TGLImage* bmp32);

class PASCALIMPLEMENTATION TGLShadowHDS : public Gls::Heightdata::TGLHeightDataSourceFilter
{
	typedef Gls::Heightdata::TGLHeightDataSourceFilter inherited;
	
private:
	int FTileSize;
	Gls::Material::TGLMaterialLibrary* FShadowmapLibrary;
	Gls::Coordinates::TGLCoordinates3* FLightVector;
	Gls::Coordinates::TGLCoordinates3* FScale;
	Gls::Vectortypes::TVector3f FScaleVec;
	TNewTilePreparedEvent FOnNewTilePrepared;
	TThreadBmp32 FOnThreadBmp32;
	int FMaxTextures;
	Gls::Vectortypes::TVector3f Step;
	int FScanDistance;
	unsigned FSoftRange;
	float FDiffuse;
	float FAmbient;
	Gls::Heightdata::TGLHeightDataSource* OwnerHDS;
	
protected:
	void __fastcall SetShadowmapLibrary(Gls::Material::TGLMaterialLibrary* const val);
	void __fastcall SetScale(Gls::Coordinates::TGLCoordinates3* AValue);
	void __fastcall SetLightVector(Gls::Coordinates::TGLCoordinates3* AValue);
	void __fastcall SetSoftRange(unsigned AValue);
	void __fastcall SetDiffuse(float AValue);
	void __fastcall SetAmbient(float AValue);
	void __fastcall Trim(int MaxTextureCount);
	Gls::Material::TGLLibMaterial* __fastcall FindUnusedMaterial();
	Gls::Vectortypes::TVector3f __fastcall CalcStep();
	Gls::Vectortypes::TVector3f __fastcall CalcScale();
	int __fastcall WrapDist(float Lx, float Ly);
	void __fastcall LocalToWorld(float Lx, float Ly, Gls::Heightdata::TGLHeightData* HD, float &Wx, float &Wy);
	void __fastcall WorldToLocal(float Wx, float Wy, Gls::Heightdata::TGLHeightData* &HD, float &Lx, float &Ly);
	
public:
	bool SkipGenerate;
	__fastcall virtual TGLShadowHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLShadowHDS();
	void __fastcall TrimTextureCache(int MaxTextureCount = 0x0);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall BeforePreparingData(Gls::Heightdata::TGLHeightData* heightData);
	virtual void __fastcall PreparingData(Gls::Heightdata::TGLHeightData* heightData);
	virtual void __fastcall AfterPreparingData(Gls::Heightdata::TGLHeightData* heightData);
	void __fastcall GenerateShadowMap(Gls::Heightdata::TGLHeightData* heightData, Gls::Graphics::TGLImage* ShadowMap, float scale);
	float __fastcall RayCastShadowHeight(Gls::Heightdata::TGLHeightData* HD, float localX, float localY)/* overload */;
	void __fastcall RayCastLine(Gls::Heightdata::TGLHeightData* heightData, float Lx, float Ly, Gls::Graphics::TGLImage* ShadowMap);
	System::Byte __fastcall Shade(Gls::Heightdata::TGLHeightData* heightData, int x, int y, float ShadowHeight, float TerrainHeight);
	
__published:
	__property Gls::Material::TGLMaterialLibrary* ShadowmapLibrary = {read=FShadowmapLibrary, write=SetShadowmapLibrary};
	__property TThreadBmp32 OnThreadBmp32 = {read=FOnThreadBmp32, write=FOnThreadBmp32};
	__property TNewTilePreparedEvent OnNewTilePrepared = {read=FOnNewTilePrepared, write=FOnNewTilePrepared};
	__property Gls::Coordinates::TGLCoordinates3* LightVector = {read=FLightVector, write=SetLightVector};
	__property Gls::Coordinates::TGLCoordinates3* scale = {read=FScale, write=FScale};
	__property int ScanDistance = {read=FScanDistance, write=FScanDistance, nodefault};
	__property unsigned SoftRange = {read=FSoftRange, write=SetSoftRange, nodefault};
	__property float Diffuse = {read=FDiffuse, write=SetDiffuse};
	__property float Ambient = {read=FAmbient, write=SetAmbient};
	__property int MaxTextures = {read=FMaxTextures, write=FMaxTextures, nodefault};
	__property OnSourceDataFetched;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Shadowhds */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SHADOWHDS)
using namespace Gls::Shadowhds;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ShadowhdsHPP
