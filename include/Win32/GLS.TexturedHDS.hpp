// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.TexturedHDS.pas' rev: 35.00 (Windows)

#ifndef Gls_TexturedhdsHPP
#define Gls_TexturedhdsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.HeightData.hpp>
#include <GLS.Material.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Texturedhds
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTexturedHDS;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLTexturedHDS : public Gls::Heightdata::TGLHeightDataSource
{
	typedef Gls::Heightdata::TGLHeightDataSource inherited;
	
private:
	Gls::Heightdata::TStartPreparingDataEvent FOnStartPreparingData;
	Gls::Heightdata::TMarkDirtyEvent FOnMarkDirty;
	Gls::Heightdata::TGLHeightDataSource* FHeightDataSource;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	bool FWholeTilesOnly;
	int FTileSize;
	int FTilesPerTexture;
	
protected:
	void __fastcall SetHeightDataSource(Gls::Heightdata::TGLHeightDataSource* val);
	
public:
	__fastcall virtual TGLTexturedHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTexturedHDS();
	virtual void __fastcall StartPreparingData(Gls::Heightdata::TGLHeightData* heightData);
	virtual void __fastcall MarkDirty(const System::Types::TRect &area)/* overload */;
	
__published:
	__property MaxPoolSize;
	__property Gls::Heightdata::TStartPreparingDataEvent OnStartPreparingData = {read=FOnStartPreparingData, write=FOnStartPreparingData};
	__property Gls::Heightdata::TMarkDirtyEvent OnMarkDirtyEvent = {read=FOnMarkDirty, write=FOnMarkDirty};
	__property Gls::Heightdata::TGLHeightDataSource* HeightDataSource = {read=FHeightDataSource, write=SetHeightDataSource};
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=FMaterialLibrary};
	__property bool WholeTilesOnly = {read=FWholeTilesOnly, write=FWholeTilesOnly, nodefault};
	__property int TileSize = {read=FTileSize, write=FTileSize, nodefault};
	__property int TilesPerTexture = {read=FTilesPerTexture, write=FTilesPerTexture, nodefault};
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  MarkDirty(int XLeft, int YTop, int xRight, int yBottom){ Gls::Heightdata::TGLHeightDataSource::MarkDirty(XLeft, YTop, xRight, yBottom); }
	inline void __fastcall  MarkDirty(){ Gls::Heightdata::TGLHeightDataSource::MarkDirty(); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Texturedhds */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_TEXTUREDHDS)
using namespace Gls::Texturedhds;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_TexturedhdsHPP
