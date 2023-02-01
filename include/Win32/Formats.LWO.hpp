// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Formats.LWO.pas' rev: 35.00 (Windows)

#ifndef Formats_LwoHPP
#define Formats_LwoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.IOUtils.hpp>
#include <System.Math.hpp>
#include <GLS.VectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Formats
{
namespace Lwo
{
//-- forward type declarations -----------------------------------------------
struct TLWChunkRec;
struct TLWSubChunkRec;
struct TLWPolsInfo;
struct TLWPntsInfo;
struct TLWPolyTagMap;
struct TLWVertexMap;
class DELPHICLASS TLWChunk;
class DELPHICLASS TLWSubChunk;
class DELPHICLASS TLWChunkList;
class DELPHICLASS TLWParentChunk;
class DELPHICLASS TLWPnts;
class DELPHICLASS TLWPols;
class DELPHICLASS TLWVMap;
class DELPHICLASS TLWTags;
class DELPHICLASS TLWSurf;
class DELPHICLASS TLWLayr;
class DELPHICLASS TLWPTag;
class DELPHICLASS TLWObjectFile;
class DELPHICLASS TLWClip;
class DELPHICLASS TLWContentDir;
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<char, 4> TID4;

typedef TID4 *PID4;

typedef System::DynamicArray<TID4> TID4DynArray;

typedef System::Int8 TI1;

typedef System::Int8 *PI1;

typedef short TI2;

typedef short *PI2;

typedef int TI4;

typedef int *PI4;

typedef System::Byte TU1;

typedef System::Byte *PU1;

typedef System::DynamicArray<System::Byte> TU1DynArray;

typedef System::Word TU2;

typedef System::Word *PU2;

typedef System::StaticArray<System::Word, 65535> TU2Array;

typedef TU2Array *PU2Array;

typedef System::DynamicArray<System::Word> TU2DynArray;

typedef unsigned TU4;

typedef unsigned *PU4;

typedef System::StaticArray<unsigned, 65535> TU4Array;

typedef TU4Array *PU4Array;

typedef System::DynamicArray<unsigned> TU4DynArray;

typedef float TF4;

typedef float *PF4;

typedef System::StaticArray<float, 65535> TF4Array;

typedef TF4Array *PF4Array;

typedef System::DynamicArray<float> TF4DynArray;

typedef float TANG4;

typedef float *PANG4;

typedef System::StaticArray<float, 3> TVec12;

typedef TVec12 *PVec12;

typedef System::StaticArray<System::StaticArray<float, 3>, 65535> TVec12Array;

typedef TVec12Array *PVec12Array;

typedef System::DynamicArray<TVec12> TVec12DynArray;

typedef TVec12 TColr12;

typedef TVec12 *PColr12;

typedef System::DynamicArray<TVec12> TColr12DynArray;

typedef System::StaticArray<System::Byte, 4> TColr4;

typedef TColr4 *PColr4;

typedef TLWChunkRec *PLWChunkRec;

struct DECLSPEC_DRECORD TLWChunkRec
{
public:
	TID4 id;
	unsigned size;
	void *data;
};


typedef TLWSubChunkRec *PLWSubChunkRec;

struct DECLSPEC_DRECORD TLWSubChunkRec
{
public:
	TID4 id;
	System::Word size;
	void *data;
};


struct DECLSPEC_DRECORD TLWPolsInfo
{
public:
	TVec12 norm;
	TVec12DynArray vnorms;
	System::Word surfid;
};


typedef System::DynamicArray<TLWPolsInfo> TLWPolsInfoDynArray;

struct DECLSPEC_DRECORD TLWPntsInfo
{
public:
	System::Word npols;
	TU2DynArray pols;
};


typedef System::DynamicArray<TLWPntsInfo> TLWPntsInfoDynArray;

typedef TU2DynArray TLWPolsDynArray;

typedef TU2DynArray TLWPolyTagMapDynArray;

struct DECLSPEC_DRECORD TLWPolyTagMap
{
public:
	System::Word poly;
	System::Word tag;
};


typedef TLWPolyTagMap *PLWPolyTagMap;

struct DECLSPEC_DRECORD TLWVertexMap
{
public:
	System::Word vert;
	TF4DynArray values;
};


typedef System::DynamicArray<TLWVertexMap> TLWVertexMapDynArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWChunk : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	void *FData;
	TID4 FID;
	unsigned FSize;
	TLWParentChunk* FParentChunk;
	TLWChunkList* FOwner;
	TLWChunkList* __fastcall GetRootChunks();
	int __fastcall GetIndex();
	
protected:
	virtual void __fastcall Clear();
	virtual void __fastcall LoadData(System::Classes::TStream* AStream, unsigned DataStart, unsigned DataSize);
	virtual void __fastcall Loaded();
	
public:
	__fastcall virtual ~TLWChunk();
	__classmethod virtual TID4 __fastcall GetID();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	__property void * data = {read=FData};
	__property TID4 id = {read=FID};
	__property unsigned size = {read=FSize, nodefault};
	__property TLWParentChunk* ParentChunk = {read=FParentChunk};
	__property TLWChunkList* RootChunks = {read=GetRootChunks};
	__property int Index = {read=GetIndex, nodefault};
	__property TLWChunkList* Owner = {read=FOwner};
public:
	/* TObject.Create */ inline __fastcall TLWChunk() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TLWChunkClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWSubChunk : public TLWChunk
{
	typedef TLWChunk inherited;
	
public:
	virtual void __fastcall LoadFromStream(System::Classes::TStream* AStream);
public:
	/* TLWChunk.Destroy */ inline __fastcall virtual ~TLWSubChunk() { }
	
public:
	/* TObject.Create */ inline __fastcall TLWSubChunk() : TLWChunk() { }
	
};

#pragma pack(pop)

typedef void __fastcall (*TLWChunkFind)(TLWChunk* AChunk, void * Criteria, bool &Found);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWChunkList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TLWChunk* operator[](int Index) { return this->Items[Index]; }
	
private:
	bool FOwnsItems;
	System::TObject* FOwner;
	TLWChunk* __fastcall GetItem(int Index);
	
protected:
	virtual void __fastcall Loaded();
	
public:
	__fastcall TLWChunkList(bool AOwnsItems, System::TObject* AOwner);
	__fastcall virtual ~TLWChunkList();
	HIDESBASE int __fastcall Add(TLWChunk* AChunk);
	virtual void __fastcall Clear();
	HIDESBASE void __fastcall Delete(int Index);
	int __fastcall FindChunk(TLWChunkFind ChunkFind, void * Criteria, int StartIndex = 0x0);
	__property TLWChunk* Items[int Index] = {read=GetItem/*, default*/};
	__property bool OwnsItems = {read=FOwnsItems, nodefault};
	__property System::TObject* Owner = {read=FOwner};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWParentChunk : public TLWChunk
{
	typedef TLWChunk inherited;
	
private:
	TLWChunkList* FItems;
	TLWChunkList* __fastcall GetItems();
	float __fastcall GetFloatParam(TID4 Param);
	System::Word __fastcall GetWordParam(TID4 Param);
	TVec12 __fastcall GetVec3Param(TID4 Param);
	unsigned __fastcall GetLongParam(TID4 Param);
	System::Word __fastcall GetVXParam(TID4 Param);
	
protected:
	virtual void * __fastcall GetParamAddr(TID4 Param);
	virtual void __fastcall Clear();
	virtual void __fastcall Loaded();
	
public:
	__property TLWChunkList* Items = {read=GetItems};
	__property void * ParamAddr[TID4 Param] = {read=GetParamAddr};
	__property float FloatParam[TID4 Param] = {read=GetFloatParam};
	__property System::Word WordParam[TID4 Param] = {read=GetWordParam};
	__property unsigned LongParam[TID4 Param] = {read=GetLongParam};
	__property TVec12 Vec3Param[TID4 Param] = {read=GetVec3Param};
	__property System::Word VXParam[TID4 Param] = {read=GetVXParam};
public:
	/* TLWChunk.Destroy */ inline __fastcall virtual ~TLWParentChunk() { }
	
public:
	/* TObject.Create */ inline __fastcall TLWParentChunk() : TLWChunk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWPnts : public TLWParentChunk
{
	typedef TLWParentChunk inherited;
	
private:
	TVec12DynArray FPnts;
	TLWPntsInfoDynArray FPntsInfo;
	unsigned __fastcall GetPntsCount();
	int __fastcall AddPoly(int PntIdx, int PolyIdx);
	
protected:
	virtual void __fastcall Clear();
	virtual void __fastcall LoadData(System::Classes::TStream* AStream, unsigned DataStart, unsigned DataSize);
	
public:
	__classmethod virtual TID4 __fastcall GetID();
	bool __fastcall GetVMap(TID4 VMapID, /* out */ TLWVMap* &VMap);
	__property unsigned PntsCount = {read=GetPntsCount, nodefault};
	__property TVec12DynArray Pnts = {read=FPnts};
	__property TLWPntsInfoDynArray PntsInfo = {read=FPntsInfo};
public:
	/* TLWChunk.Destroy */ inline __fastcall virtual ~TLWPnts() { }
	
public:
	/* TObject.Create */ inline __fastcall TLWPnts() : TLWParentChunk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWPols : public TLWParentChunk
{
	typedef TLWParentChunk inherited;
	
private:
	TID4 FPolsType;
	TU2DynArray FPols;
	TLWPolsInfoDynArray FPolsInfo;
	int FPolsCount;
	int __fastcall GetPolsByIndex(System::Word AIndex);
	unsigned __fastcall GetIndiceCount();
	System::Word __fastcall GetIndice(int AIndex);
	int __fastcall GetPolsCount();
	void __fastcall CalcPolsNormals();
	void __fastcall CalcPntsNormals();
	
protected:
	virtual void __fastcall Clear();
	virtual void __fastcall LoadData(System::Classes::TStream* AStream, unsigned DataStart, unsigned DataSize);
	virtual void __fastcall Loaded();
	
public:
	__classmethod virtual TID4 __fastcall GetID();
	int __fastcall GetPolsByPntIdx(System::Word VertIdx, TU2DynArray &VertPolys);
	__property int PolsByIndex[System::Word AIndex] = {read=GetPolsByIndex};
	__property unsigned IndiceCount = {read=GetIndiceCount, nodefault};
	__property System::Word Indices[int AIndex] = {read=GetIndice};
	__property TID4 PolsType = {read=FPolsType};
	__property int PolsCount = {read=GetPolsCount, nodefault};
	__property TLWPolsInfoDynArray PolsInfo = {read=FPolsInfo};
public:
	/* TLWChunk.Destroy */ inline __fastcall virtual ~TLWPols() { }
	
public:
	/* TObject.Create */ inline __fastcall TLWPols() : TLWParentChunk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWVMap : public TLWChunk
{
	typedef TLWChunk inherited;
	
private:
	System::Word FDimensions;
	System::UnicodeString FName;
	TLWVertexMapDynArray FValues;
	TID4 FVMapType;
	TLWVertexMap __fastcall GetValue(System::Word AIndex);
	int __fastcall GetValueCount();
	
protected:
	virtual void __fastcall Clear();
	virtual void __fastcall LoadData(System::Classes::TStream* AStream, unsigned DataStart, unsigned DataSize);
	
public:
	__classmethod virtual TID4 __fastcall GetID();
	__property System::Word Dimensions = {read=FDimensions, nodefault};
	__property System::UnicodeString Name = {read=FName};
	__property TLWVertexMap Value[System::Word AIndex] = {read=GetValue};
	__property int ValueCount = {read=GetValueCount, nodefault};
	__property TID4 VMapType = {read=FVMapType};
public:
	/* TLWChunk.Destroy */ inline __fastcall virtual ~TLWVMap() { }
	
public:
	/* TObject.Create */ inline __fastcall TLWVMap() : TLWChunk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWTags : public TLWChunk
{
	typedef TLWChunk inherited;
	
private:
	System::Classes::TStrings* FTags;
	System::Classes::TStrings* __fastcall GetTags();
	
protected:
	virtual void __fastcall Clear();
	virtual void __fastcall LoadData(System::Classes::TStream* AStream, unsigned DataStart, unsigned DataSize);
	
public:
	__fastcall virtual ~TLWTags();
	__classmethod virtual TID4 __fastcall GetID();
	System::UnicodeString __fastcall TagToName(System::Word tag);
	__property System::Classes::TStrings* Tags = {read=GetTags};
public:
	/* TObject.Create */ inline __fastcall TLWTags() : TLWChunk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWSurf : public TLWParentChunk
{
	typedef TLWParentChunk inherited;
	
private:
	System::UnicodeString FName;
	System::UnicodeString FSource;
	int __fastcall GetSurfId();
	
protected:
	virtual void * __fastcall GetParamAddr(TID4 Param);
	virtual void __fastcall LoadData(System::Classes::TStream* AStream, unsigned DataStart, unsigned DataSize);
	
public:
	__fastcall virtual ~TLWSurf();
	__classmethod virtual TID4 __fastcall GetID();
	__property int surfid = {read=GetSurfId, nodefault};
	__property System::UnicodeString Name = {read=FName};
	__property System::UnicodeString Source = {read=FSource};
public:
	/* TObject.Create */ inline __fastcall TLWSurf() : TLWParentChunk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWLayr : public TLWParentChunk
{
	typedef TLWParentChunk inherited;
	
private:
	System::Word FFlags;
	System::UnicodeString FName;
	System::Word FNumber;
	System::Word FParent;
	TVec12 FPivot;
	
protected:
	virtual void __fastcall LoadData(System::Classes::TStream* AStream, unsigned DataStart, unsigned DataSize);
	
public:
	__fastcall virtual ~TLWLayr();
	__classmethod virtual TID4 __fastcall GetID();
	__property System::Word Flags = {read=FFlags, nodefault};
	__property System::UnicodeString Name = {read=FName};
	__property System::Word Number = {read=FNumber, nodefault};
	__property System::Word Parent = {read=FParent, nodefault};
	__property TVec12 Pivot = {read=FPivot};
public:
	/* TObject.Create */ inline __fastcall TLWLayr() : TLWParentChunk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWPTag : public TLWChunk
{
	typedef TLWChunk inherited;
	
public:
	TLWPolyTagMap operator[](int AIndex) { return this->TagMaps[AIndex]; }
	
private:
	TID4 FMapType;
	TU2DynArray FTagMaps;
	TU2DynArray FTags;
	int __fastcall AddTag(System::Word Value);
	System::Word __fastcall GetTag(int AIndex);
	int __fastcall GetTagCount();
	int __fastcall GetTagMapCount();
	TLWPolyTagMap __fastcall GetTagMaps(int AIndex);
	void __fastcall ValidateTagInfo();
	
protected:
	virtual void __fastcall Clear();
	virtual void __fastcall LoadData(System::Classes::TStream* AStream, unsigned DataStart, unsigned DataSize);
	
public:
	__fastcall TLWPTag();
	int __fastcall GetPolsByTag(System::Word tag, TU2DynArray &PolyIndices);
	__classmethod virtual TID4 __fastcall GetID();
	__property TID4 MapType = {read=FMapType};
	__property int TagCount = {read=GetTagCount, nodefault};
	__property int TagMapCount = {read=GetTagMapCount, nodefault};
	__property TLWPolyTagMap TagMaps[int AIndex] = {read=GetTagMaps/*, default*/};
	__property System::Word Tags[int AIndex] = {read=GetTag};
public:
	/* TLWChunk.Destroy */ inline __fastcall virtual ~TLWPTag() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWObjectFile : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TLWChunkList* FChunks;
	System::UnicodeString FFileName;
	TLWChunkList* __fastcall GetChunks();
	int __fastcall GetCount();
	TLWSurf* __fastcall GetSurfaceByName(System::UnicodeString Index);
	TLWSurf* __fastcall GetSurfaceByTag(System::Word Index);
	
public:
	__fastcall TLWObjectFile();
	__fastcall virtual ~TLWObjectFile();
	System::UnicodeString __fastcall TagToName(System::Word tag);
	void __fastcall LoadFromFile(const System::UnicodeString AFilename);
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	__property int ChunkCount = {read=GetCount, nodefault};
	__property TLWChunkList* Chunks = {read=GetChunks};
	__property System::UnicodeString FileName = {read=FFileName};
	__property TLWSurf* SurfaceByName[System::UnicodeString Index] = {read=GetSurfaceByName};
	__property TLWSurf* SurfaceByTag[System::Word Index] = {read=GetSurfaceByTag};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWClip : public TLWParentChunk
{
	typedef TLWParentChunk inherited;
	
private:
	unsigned FClipIndex;
	
protected:
	virtual void __fastcall LoadData(System::Classes::TStream* AStream, unsigned DataStart, unsigned DataSize);
	
public:
	__classmethod virtual TID4 __fastcall GetID();
	__property unsigned ClipIndex = {read=FClipIndex, nodefault};
public:
	/* TLWChunk.Destroy */ inline __fastcall virtual ~TLWClip() { }
	
public:
	/* TObject.Create */ inline __fastcall TLWClip() : TLWParentChunk() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TLWContentNotify)(System::TObject* Sender, System::UnicodeString &Content);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLWContentDir : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStrings* FSubDirs;
	System::UnicodeString FRoot;
	System::Classes::TStrings* __fastcall GetSubDirs();
	void __fastcall SetRoot(const System::UnicodeString Value);
	void __fastcall SetSubDirs(System::Classes::TStrings* const Value);
	
public:
	__fastcall virtual ~TLWContentDir();
	System::UnicodeString __fastcall FindContent(System::UnicodeString AFilename);
	__property System::UnicodeString Root = {read=FRoot, write=SetRoot};
	__property System::Classes::TStrings* SubDirs = {read=GetSubDirs, write=SetSubDirs};
public:
	/* TObject.Create */ inline __fastcall TLWContentDir() : System::TObject() { }
	
};

#pragma pack(pop)

typedef void __cdecl (*TLWOReadCallback)(TLWChunkRec Chunk, void * data);

//-- var, const, procedure ---------------------------------------------------
#define ID_NULL L"#0#0#0#0"
extern DELPHI_PACKAGE TID4 ID_LWSC;
extern DELPHI_PACKAGE TID4 ID_FORM;
extern DELPHI_PACKAGE TID4 ID_LWOB;
extern DELPHI_PACKAGE TID4 ID_LWLO;
extern DELPHI_PACKAGE TID4 ID_LAYR;
extern DELPHI_PACKAGE TID4 ID_PNTS;
extern DELPHI_PACKAGE TID4 ID_SRFS;
extern DELPHI_PACKAGE TID4 ID_POLS;
extern DELPHI_PACKAGE TID4 ID_CRVS;
extern DELPHI_PACKAGE TID4 ID_PCHS;
extern DELPHI_PACKAGE TID4 ID_SURF;
extern DELPHI_PACKAGE TID4 ID_COLR;
extern DELPHI_PACKAGE TID4 ID_FLAG;
extern DELPHI_PACKAGE TID4 ID_LUMI;
extern DELPHI_PACKAGE TID4 ID_DIFF;
extern DELPHI_PACKAGE TID4 ID_SPEC;
extern DELPHI_PACKAGE TID4 ID_REFL;
extern DELPHI_PACKAGE TID4 ID_TRAN;
extern DELPHI_PACKAGE TID4 ID_VLUM;
extern DELPHI_PACKAGE TID4 ID_VDIF;
extern DELPHI_PACKAGE TID4 ID_VSPC;
extern DELPHI_PACKAGE TID4 ID_VRFL;
extern DELPHI_PACKAGE TID4 ID_VTRN;
extern DELPHI_PACKAGE TID4 ID_GLOS;
extern DELPHI_PACKAGE TID4 ID_SIDE;
extern DELPHI_PACKAGE TID4 ID_RFLT;
extern DELPHI_PACKAGE TID4 ID_RFOP;
extern DELPHI_PACKAGE TID4 ID_RIMG;
extern DELPHI_PACKAGE TID4 ID_RSAN;
extern DELPHI_PACKAGE TID4 ID_RIND;
extern DELPHI_PACKAGE TID4 ID_EDGE;
extern DELPHI_PACKAGE TID4 ID_SMAN;
extern DELPHI_PACKAGE TID4 ID_ALPH;
extern DELPHI_PACKAGE TID4 ID_CTEX;
extern DELPHI_PACKAGE TID4 ID_DTEX;
extern DELPHI_PACKAGE TID4 ID_STEX;
extern DELPHI_PACKAGE TID4 ID_RTEX;
extern DELPHI_PACKAGE TID4 ID_TTEX;
extern DELPHI_PACKAGE TID4 ID_LTEX;
extern DELPHI_PACKAGE TID4 ID_BTEX;
extern DELPHI_PACKAGE TID4 ID_TFLG;
extern DELPHI_PACKAGE TID4 ID_TSIZ;
extern DELPHI_PACKAGE TID4 ID_TCTR;
extern DELPHI_PACKAGE TID4 ID_TFAL;
extern DELPHI_PACKAGE TID4 ID_TVEL;
extern DELPHI_PACKAGE TID4 ID_TREF;
extern DELPHI_PACKAGE TID4 ID_TCLR;
extern DELPHI_PACKAGE TID4 ID_TVAL;
extern DELPHI_PACKAGE TID4 ID_TAMP;
extern DELPHI_PACKAGE TID4 ID_TFP0;
extern DELPHI_PACKAGE TID4 ID_TFP1;
extern DELPHI_PACKAGE TID4 ID_TFP2;
extern DELPHI_PACKAGE TID4 ID_TIP0;
extern DELPHI_PACKAGE TID4 ID_TIP1;
extern DELPHI_PACKAGE TID4 ID_TIP2;
extern DELPHI_PACKAGE TID4 ID_TSP0;
extern DELPHI_PACKAGE TID4 ID_TSP1;
extern DELPHI_PACKAGE TID4 ID_TSP2;
extern DELPHI_PACKAGE TID4 ID_TFRQ;
extern DELPHI_PACKAGE TID4 ID_TIMG;
extern DELPHI_PACKAGE TID4 ID_TALP;
extern DELPHI_PACKAGE TID4 ID_TWRP;
extern DELPHI_PACKAGE TID4 ID_TAAS;
extern DELPHI_PACKAGE TID4 ID_TOPC;
extern DELPHI_PACKAGE TID4 ID_SHDR;
extern DELPHI_PACKAGE TID4 ID_SDAT;
extern DELPHI_PACKAGE TID4 ID_IMSQ;
extern DELPHI_PACKAGE TID4 ID_FLYR;
extern DELPHI_PACKAGE TID4 ID_IMCC;
static const System::Int8 SURF_FLAG_LUMINOUS = System::Int8(0x1);
static const System::Int8 SURF_FLAG_OUTLINE = System::Int8(0x2);
static const System::Int8 SURF_FLAG_SMOOTHING = System::Int8(0x4);
static const System::Int8 SURF_FLAG_COLORHIGHLIGHTS = System::Int8(0x8);
static const System::Int8 SURF_FLAG_COLORFILTER = System::Int8(0x10);
static const System::Int8 SURF_FLAG_OPAQUEEDGE = System::Int8(0x20);
static const System::Int8 SURF_FLAG_TRANSPARENTEDGE = System::Int8(0x40);
static const System::Byte SURF_FLAG_SHARPTERMINATOR = System::Byte(0x80);
static const System::Word SURF_FLAG_DOUBLESIDED = System::Word(0x100);
static const System::Word SURF_FLAG_ADDITIVE = System::Word(0x200);
static const System::Word SURF_FLAG_SHADOWALPHA = System::Word(0x400);
static const System::Int8 CURV_CONTINUITY_FIRST = System::Int8(0x1);
static const System::Int8 CURV_CONTINUITY_LAST = System::Int8(0x2);
static const System::Int8 IMSQ_FLAG_LOOP = System::Int8(0x1);
static const System::Int8 IMSQ_FLAG_INTERLACE = System::Int8(0x2);
extern DELPHI_PACKAGE TID4 ID_LWO2;
extern DELPHI_PACKAGE TID4 ID_VMAP;
extern DELPHI_PACKAGE TID4 ID_TAGS;
extern DELPHI_PACKAGE TID4 ID_PTAG;
extern DELPHI_PACKAGE TID4 ID_VMAD;
extern DELPHI_PACKAGE TID4 ID_ENVL;
extern DELPHI_PACKAGE TID4 ID_CLIP;
extern DELPHI_PACKAGE TID4 ID_BBOX;
extern DELPHI_PACKAGE TID4 ID_DESC;
extern DELPHI_PACKAGE TID4 ID_TEXT;
extern DELPHI_PACKAGE TID4 ID_ICON;
extern DELPHI_PACKAGE TID4 ENVL_PRE;
extern DELPHI_PACKAGE TID4 ENVL_POST;
extern DELPHI_PACKAGE TID4 ENVL_KEY;
extern DELPHI_PACKAGE TID4 ENVL_SPAN;
extern DELPHI_PACKAGE TID4 ENVL_CHAN;
extern DELPHI_PACKAGE TID4 ENVL_NAME;
extern DELPHI_PACKAGE TID4 ID_STIL;
extern DELPHI_PACKAGE TID4 ID_ISEQ;
extern DELPHI_PACKAGE TID4 ID_ANIM;
extern DELPHI_PACKAGE TID4 ID_STCC;
extern DELPHI_PACKAGE TID4 ID_CONT;
extern DELPHI_PACKAGE TID4 ID_BRIT;
extern DELPHI_PACKAGE TID4 ID_SATR;
extern DELPHI_PACKAGE TID4 ID_HUE;
extern DELPHI_PACKAGE TID4 ID_GAMMA;
extern DELPHI_PACKAGE TID4 ID_NEGA;
extern DELPHI_PACKAGE TID4 ID_IFLT;
extern DELPHI_PACKAGE TID4 ID_PFLT;
extern DELPHI_PACKAGE TID4 POLS_TYPE_FACE;
extern DELPHI_PACKAGE TID4 POLS_TYPE_CURV;
extern DELPHI_PACKAGE TID4 POLS_TYPE_PTCH;
extern DELPHI_PACKAGE TID4 POLS_TYPE_MBAL;
extern DELPHI_PACKAGE TID4 POLS_TYPE_BONE;
extern DELPHI_PACKAGE TID4 VMAP_TYPE_PICK;
extern DELPHI_PACKAGE TID4 VMAP_TYPE_WGHT;
extern DELPHI_PACKAGE TID4 VMAP_TYPE_MNVW;
extern DELPHI_PACKAGE TID4 VMAP_TYPE_TXUV;
extern DELPHI_PACKAGE TID4 VMAP_TYPE_RGB;
extern DELPHI_PACKAGE TID4 VMAP_TYPE_RGBA;
extern DELPHI_PACKAGE TID4 VMAP_TYPE_MORF;
extern DELPHI_PACKAGE TID4 VMAP_TYPE_SPOT;
extern DELPHI_PACKAGE TID4 PTAG_TYPE_SURF;
extern DELPHI_PACKAGE TID4 PTAG_TYPE_PART;
extern DELPHI_PACKAGE TID4 PTAG_TYPE_SMGP;
static const System::Int8 PRE_POST_RESET = System::Int8(0x0);
static const System::Int8 PRE_POST_CONSTANT = System::Int8(0x1);
static const System::Int8 PRE_POST_REPEAT = System::Int8(0x2);
static const System::Int8 PRE_POST_OSCILLATE = System::Int8(0x3);
static const System::Int8 PRE_POST_OFFSET = System::Int8(0x4);
static const System::Int8 PRE_POST_LINEAR = System::Int8(0x5);
static const System::Word POLS_VCOUNT_MASK = System::Word(0x3ff);
static const System::Word POLS_FLAGS_MASK = System::Word(0xfc00);
static const System::Int8 SIDE_FRONT = System::Int8(0x1);
static const System::Int8 SIDE_BACK = System::Int8(0x2);
static const System::Int8 SIDE_FRONT_AND_BACK = System::Int8(0x0);
static const System::Int8 RFOP_BACKDROP = System::Int8(0x0);
static const System::Int8 RFOP_RAYTRACEANDBACKDROP = System::Int8(0x1);
static const System::Int8 RFOP_SPHERICALMAP = System::Int8(0x2);
static const System::Int8 RFOP_RAYTRACEANDSPHERICALMAP = System::Int8(0x3);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ToDosPath(const System::UnicodeString Path);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ToUnixPath(const System::UnicodeString Path);
extern DELPHI_PACKAGE TLWContentDir* __fastcall GetContentDir(void);
extern DELPHI_PACKAGE void __fastcall FindChunkById(TLWChunk* AChunk, void * data, bool &Found);
extern DELPHI_PACKAGE void __fastcall FindClipByClipIndex(TLWChunk* AChunk, void * AIndex, bool &Found);
extern DELPHI_PACKAGE void __fastcall FindSurfaceByName(TLWChunk* AChunk, void * AName, bool &Found);
extern DELPHI_PACKAGE void __fastcall FindSurfaceByTag(TLWChunk* AChunk, void * ATag, bool &Found);
extern DELPHI_PACKAGE void __fastcall FindVMapByName(TLWChunk* AChunk, void * AName, bool &Found);
extern DELPHI_PACKAGE void __fastcall RegisterChunkClass(TLWChunkClass ChunkClass);
extern DELPHI_PACKAGE unsigned __cdecl LoadLW0FromStream(System::Classes::TStream* Stream, TLWOReadCallback ReadCallback, void * UserData);
extern DELPHI_PACKAGE unsigned __fastcall LoadLWOFromFile(const System::UnicodeString AFilename, TLWOReadCallback ReadCallback, void * UserData);
extern DELPHI_PACKAGE void __fastcall ReverseByteOrder(void * ValueIn, int size, int Count = 0x1);
extern DELPHI_PACKAGE void __fastcall ReadMotorolaNumber(System::Classes::TStream* Stream, void * data, int ElementSize, int Count = 0x1);
extern DELPHI_PACKAGE int __fastcall WriteMotorolaNumber(System::Classes::TStream* Stream, void * data, int ElementSize, int Count = 0x1);
extern DELPHI_PACKAGE int __fastcall ReadS0(System::Classes::TStream* Stream, /* out */ System::UnicodeString &Str);
extern DELPHI_PACKAGE int __fastcall ReadVXAsU4(System::Classes::TStream* Stream, void * data, int Count = 0x1);
extern DELPHI_PACKAGE void __fastcall WriteS0(System::Classes::TStream* Stream, System::UnicodeString data);
extern DELPHI_PACKAGE void __fastcall WriteU4AsVX(System::Classes::TStream* Stream, void * data, int Count);
extern DELPHI_PACKAGE int __fastcall ID4ToInt(const TID4 id);
}	/* namespace Lwo */
}	/* namespace Formats */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS_LWO)
using namespace Formats::Lwo;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS)
using namespace Formats;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Formats_LwoHPP
