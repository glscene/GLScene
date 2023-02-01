// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.BumpmapHDS.pas' rev: 35.00 (Windows)

#ifndef Gls_BumpmaphdsHPP
#define Gls_BumpmaphdsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.HeightData.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.Color.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Material.hpp>
#include <GLS.Utils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Bumpmaphds
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBumpmapHDS;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLNormalMapSpace : unsigned char { nmsObject, nmsTangent };

typedef void __fastcall (__closure *TNewTilePreparedEvent)(TGLBumpmapHDS* Sender, Gls::Heightdata::TGLHeightData* heightData, Gls::Material::TGLLibMaterial* normalMapMaterial);

class PASCALIMPLEMENTATION TGLBumpmapHDS : public Gls::Heightdata::TGLHeightDataSourceFilter
{
	typedef Gls::Heightdata::TGLHeightDataSourceFilter inherited;
	
private:
	Gls::Material::TGLMaterialLibrary* FBumpmapLibrary;
	TNewTilePreparedEvent FOnNewTilePrepared;
	float FBumpScale;
	int FSubSampling;
	int FMaxTextures;
	System::Syncobjs::TCriticalSection* Uno;
	
protected:
	void __fastcall SetBumpmapLibrary(Gls::Material::TGLMaterialLibrary* const val);
	void __fastcall SetBumpScale(const float val);
	bool __fastcall StoreBumpScale();
	void __fastcall SetSubSampling(const int val);
	void __fastcall Trim(int MaxTextureCount);
	
public:
	__fastcall virtual TGLBumpmapHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBumpmapHDS();
	virtual void __fastcall Release(Gls::Heightdata::TGLHeightData* aHeightData);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall GenerateNormalMap(Gls::Heightdata::TGLHeightData* heightData, Gls::Graphics::TGLImage* normalMap, float scale);
	void __fastcall TrimTextureCache(int MaxTextureCount);
	virtual void __fastcall PreparingData(Gls::Heightdata::TGLHeightData* heightData);
	
__published:
	__property Gls::Material::TGLMaterialLibrary* BumpmapLibrary = {read=FBumpmapLibrary, write=SetBumpmapLibrary};
	__property TNewTilePreparedEvent OnNewTilePrepared = {read=FOnNewTilePrepared, write=FOnNewTilePrepared};
	__property float BumpScale = {read=FBumpScale, write=SetBumpScale, stored=StoreBumpScale};
	__property int SubSampling = {read=FSubSampling, write=SetSubSampling, default=1};
	__property MaxPoolSize;
	__property int MaxTextures = {read=FMaxTextures, write=FMaxTextures, nodefault};
	__property OnSourceDataFetched;
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall CalcObjectSpaceLightVectors(const Gls::Vectortypes::TVector3f &Light, Gls::Vectorlists::TGLAffineVectorList* Vertices, Gls::Vectorlists::TGLVectorList* Colors);
extern DELPHI_PACKAGE void __fastcall SetupTangentSpace(Gls::Vectorlists::TGLAffineVectorList* Vertices, Gls::Vectorlists::TGLAffineVectorList* Normals, Gls::Vectorlists::TGLAffineVectorList* TexCoords, Gls::Vectorlists::TGLAffineVectorList* Tangents, Gls::Vectorlists::TGLAffineVectorList* BiNormals);
extern DELPHI_PACKAGE void __fastcall CalcTangentSpaceLightVectors(const Gls::Vectortypes::TVector3f &Light, Gls::Vectorlists::TGLAffineVectorList* Vertices, Gls::Vectorlists::TGLAffineVectorList* Normals, Gls::Vectorlists::TGLAffineVectorList* Tangents, Gls::Vectorlists::TGLAffineVectorList* BiNormals, Gls::Vectorlists::TGLVectorList* Colors);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall CreateObjectSpaceNormalMap(int Width, int Height, Gls::Vectorlists::TGLAffineVectorList* HiNormals, Gls::Vectorlists::TGLAffineVectorList* HiTexCoords);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall CreateTangentSpaceNormalMap(int Width, int Height, Gls::Vectorlists::TGLAffineVectorList* HiNormals, Gls::Vectorlists::TGLAffineVectorList* HiTexCoords, Gls::Vectorlists::TGLAffineVectorList* LoNormals, Gls::Vectorlists::TGLAffineVectorList* LoTexCoords, Gls::Vectorlists::TGLAffineVectorList* Tangents, Gls::Vectorlists::TGLAffineVectorList* BiNormals);
}	/* namespace Bumpmaphds */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_BUMPMAPHDS)
using namespace Gls::Bumpmaphds;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_BumpmaphdsHPP
