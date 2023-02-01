// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.HeightTileFileHDS.pas' rev: 35.00 (Windows)

#ifndef Gls_HeighttilefilehdsHPP
#define Gls_HeighttilefilehdsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.HeightData.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Heighttilefilehds
{
//-- forward type declarations -----------------------------------------------
struct TGLHeightTileInfo;
struct TGLHeightTile;
struct THTFHeader;
class DELPHICLASS TGLHeightTileFile;
class DELPHICLASS TGLHeightTileFileHDS;
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<int, 268435456> TIntegerArray;

typedef TIntegerArray *PIntegerArray;

typedef System::StaticArray<short, 536870912> TSmallIntArray;

typedef TSmallIntArray *PSmallIntArray;

typedef System::StaticArray<System::Int8, 536870912> TShortIntArray;

typedef TShortIntArray *PShortIntArray;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLHeightTileInfo
{
public:
	int left;
	int top;
	int width;
	int height;
	short min;
	short max;
	short average;
	__int64 fileOffset;
};
#pragma pack(pop)


typedef TGLHeightTileInfo *PGLHeightTileInfo;

typedef PGLHeightTileInfo *PPHeightTileInfo;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLHeightTile
{
	
private:
	typedef System::DynamicArray<short> _TGLHeightTile__1;
	
	
public:
	TGLHeightTileInfo info;
	_TGLHeightTile__1 data;
};
#pragma pack(pop)


typedef TGLHeightTile *PGLHeightTile;

#pragma pack(push,1)
struct DECLSPEC_DRECORD THTFHeader
{
public:
	System::StaticArray<char, 6> FileVersion;
	__int64 TileIndexOffset;
	int SizeX;
	int SizeY;
	int TileSize;
	short DefaultZ;
};
#pragma pack(pop)


typedef System::DynamicArray<int> _TGLHeightTileFile__3;

class PASCALIMPLEMENTATION TGLHeightTileFile : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<TGLHeightTileInfo> _TGLHeightTileFile__1;
	
	typedef System::DynamicArray<unsigned> _TGLHeightTileFile__2;
	
	typedef System::StaticArray<_TGLHeightTileFile__3, 1024> _TGLHeightTileFile__4;
	
	typedef System::DynamicArray<int> _TGLHeightTileFile__5;
	
	typedef System::StaticArray<System::StaticArray<_TGLHeightTileFile__5, 32>, 32> _TGLHeightTileFile__6;
	
	typedef System::DynamicArray<System::Int8> _TGLHeightTileFile__7;
	
	
private:
	System::Classes::TStream* FFile;
	THTFHeader FHeader;
	_TGLHeightTileFile__1 FTileIndex;
	_TGLHeightTileFile__2 FTileMark;
	unsigned FLastMark;
	_TGLHeightTileFile__4 FHashTable;
	_TGLHeightTileFile__6 FQuadTable;
	bool FCreating;
	TGLHeightTile FHeightTile;
	_TGLHeightTileFile__7 FInBuf;
	
protected:
	PGLHeightTileInfo __fastcall GetTiles(int index);
	int __fastcall QuadTableX(int x);
	int __fastcall QuadTableY(int y);
	void __fastcall PackTile(int aWidth, int aHeight, PSmallIntArray src);
	void __fastcall UnPackTile(PShortIntArray source);
	__property __int64 TileIndexOffset = {read=FHeader.TileIndexOffset, write=FHeader.TileIndexOffset};
	
public:
	__fastcall TGLHeightTileFile(const System::UnicodeString fileName, int aSizeX, int aSizeY, int aTileSize);
	__fastcall TGLHeightTileFile(const System::UnicodeString fileName);
	__fastcall virtual ~TGLHeightTileFile();
	int __fastcall GetTileIndex(int aLeft, int aTop);
	PGLHeightTile __fastcall GetTile(int aLeft, int aTop, PPHeightTileInfo pTileInfo = (PPHeightTileInfo)(0x0));
	void __fastcall CompressTile(int aLeft, int aTop, int aWidth, int aHeight, PSmallIntArray aData);
	void __fastcall ExtractRow(int x, int y, int len, PSmallIntArray dest);
	PGLHeightTileInfo __fastcall XYTileInfo(int anX, int anY);
	short __fastcall XYHeight(int anX, int anY);
	void __fastcall TilesInRect(int aLeft, int aTop, int aRight, int aBottom, System::Classes::TList* destList);
	int __fastcall TileCount();
	__property PGLHeightTileInfo Tiles[int index] = {read=GetTiles};
	int __fastcall IndexOfTile(PGLHeightTileInfo aTile);
	int __fastcall TileCompressedSize(int tileIndex);
	__property int SizeX = {read=FHeader.SizeX, nodefault};
	__property int SizeY = {read=FHeader.SizeY, nodefault};
	__property int TileSize = {read=FHeader.TileSize, nodefault};
	__property short DefaultZ = {read=FHeader.DefaultZ, write=FHeader.DefaultZ, nodefault};
};


class PASCALIMPLEMENTATION TGLHeightTileFileHDS : public Gls::Heightdata::TGLHeightDataSource
{
	typedef Gls::Heightdata::TGLHeightDataSource inherited;
	
private:
	bool FInfiniteWrap;
	bool FInverted;
	System::UnicodeString FHTFFileName;
	TGLHeightTileFile* FHTF;
	int FMinElevation;
	
protected:
	void __fastcall SetHTFFileName(const System::UnicodeString val);
	void __fastcall SetInfiniteWrap(bool val);
	void __fastcall SetInverted(bool val);
	void __fastcall SetMinElevation(int val);
	
public:
	__fastcall virtual TGLHeightTileFileHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLHeightTileFileHDS();
	virtual void __fastcall StartPreparingData(Gls::Heightdata::TGLHeightData* HeightData);
	virtual int __fastcall Width();
	virtual int __fastcall Height();
	TGLHeightTileFile* __fastcall OpenHTF();
	
__published:
	__property System::UnicodeString HTFFileName = {read=FHTFFileName, write=SetHTFFileName};
	__property bool InfiniteWrap = {read=FInfiniteWrap, write=SetInfiniteWrap, default=1};
	__property bool Inverted = {read=FInverted, write=SetInverted, default=1};
	__property int MinElevation = {read=FMinElevation, write=SetMinElevation, default=-32768};
	__property MaxPoolSize;
	__property DefaultHeight = {default=0};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word cHTFHashTableSize = System::Word(0x3ff);
static const System::Int8 cHTFQuadTableSize = System::Int8(0x1f);
}	/* namespace Heighttilefilehds */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_HEIGHTTILEFILEHDS)
using namespace Gls::Heighttilefilehds;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_HeighttilefilehdsHPP
