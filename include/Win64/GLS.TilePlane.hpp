// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.TilePlane.pas' rev: 35.00 (Windows)

#ifndef Gls_TileplaneHPP
#define Gls_TileplaneHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.State.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Context.hpp>
#include <GLS.Material.hpp>
#include <GLS.Objects.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.XOpenGL.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Tileplane
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTiledAreaRow;
class DELPHICLASS TGLTiledArea;
class DELPHICLASS TGLTilePlane;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLTiledAreaRow : public Gls::Persistentclasses::TGLPersistentObject
{
	typedef Gls::Persistentclasses::TGLPersistentObject inherited;
	
public:
	int operator[](int col) { return this->Cell[col]; }
	
private:
	int FColMin;
	int FColMax;
	Gls::Vectorlists::TGLIntegerList* FData;
	
protected:
	void __fastcall SetColMin(const int val);
	void __fastcall SetColMax(const int val);
	int __fastcall GetCell(int col);
	void __fastcall SetCell(int col, int val);
	
public:
	__fastcall virtual TGLTiledAreaRow();
	__fastcall virtual ~TGLTiledAreaRow();
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* reader);
	__property int Cell[int col] = {read=GetCell, write=SetCell/*, default*/};
	__property int ColMin = {read=FColMin, write=SetColMin, nodefault};
	__property int ColMax = {read=FColMax, write=SetColMax, nodefault};
	__property Gls::Vectorlists::TGLIntegerList* Data = {read=FData};
	void __fastcall Pack();
	bool __fastcall Empty();
	void __fastcall RemapTiles(Gls::Vectorlists::TGLIntegerList* remapList);
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLTiledAreaRow(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Persistentclasses::TGLPersistentObject(reader) { }
	
};


class PASCALIMPLEMENTATION TGLTiledArea : public Gls::Persistentclasses::TGLPersistentObject
{
	typedef Gls::Persistentclasses::TGLPersistentObject inherited;
	
private:
	int FRowMin;
	int FRowMax;
	Gls::Persistentclasses::TGLPersistentObjectList* FRows;
	
protected:
	void __fastcall SetRowMin(const int val);
	void __fastcall SetRowMax(const int val);
	int __fastcall GetTile(int col, int row);
	void __fastcall SetTile(int col, int row, int val);
	TGLTiledAreaRow* __fastcall GetRow(int index);
	
public:
	__fastcall virtual TGLTiledArea();
	__fastcall virtual ~TGLTiledArea();
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* reader);
	__property int Tile[int col][int row] = {read=GetTile, write=SetTile/*, default*/};
	__property TGLTiledAreaRow* Row[int index] = {read=GetRow};
	__property int RowMin = {read=FRowMin, write=SetRowMin, nodefault};
	__property int RowMax = {read=FRowMax, write=SetRowMax, nodefault};
	void __fastcall Pack();
	void __fastcall Clear();
	bool __fastcall Empty();
	void __fastcall RemapTiles(Gls::Vectorlists::TGLIntegerList* remapList);
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLTiledArea(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Persistentclasses::TGLPersistentObject(reader) { }
	
};


class PASCALIMPLEMENTATION TGLTilePlane : public Gls::Scene::TGLImmaterialSceneObject
{
	typedef Gls::Scene::TGLImmaterialSceneObject inherited;
	
private:
	bool FNoZWrite;
	TGLTiledArea* FTiles;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	bool FSortByMaterials;
	
protected:
	void __fastcall SetNoZWrite(const bool val);
	void __fastcall SetTiles(TGLTiledArea* const val);
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const val);
	void __fastcall SetSortByMaterials(const bool val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLTilePlane(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTilePlane();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property TGLTiledArea* Tiles = {read=FTiles, write=SetTiles};
	__property bool SortByMaterials = {read=FSortByMaterials, write=SetSortByMaterials, nodefault};
	
__published:
	__property bool NoZWrite = {read=FNoZWrite, write=SetNoZWrite, nodefault};
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTilePlane(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tileplane */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_TILEPLANE)
using namespace Gls::Tileplane;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_TileplaneHPP
