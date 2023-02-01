// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.VectorLists.pas' rev: 35.00 (Windows)

#ifndef Gls_VectorlistsHPP
#define Gls_VectorlistsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.PersistentClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Vectorlists
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBaseList;
class DELPHICLASS TGLBaseVectorList;
class DELPHICLASS TGLAffineVectorList;
class DELPHICLASS TGLVectorList;
class DELPHICLASS TGLTexPointList;
class DELPHICLASS TGLIntegerList;
class DELPHICLASS TGLSingleList;
class DELPHICLASS TGLDoubleList;
class DELPHICLASS TGLByteList;
class DELPHICLASS TGLQuaternionList;
struct TGL4ByteData;
class DELPHICLASS TGL4ByteList;
class DELPHICLASS TGLLongWordList;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLBaseListOption : unsigned char { bloExternalMemory, bloSetCountResetsMemory };

typedef System::Set<TGLBaseListOption, TGLBaseListOption::bloExternalMemory, TGLBaseListOption::bloSetCountResetsMemory> TGLBaseListOptions;

class PASCALIMPLEMENTATION TGLBaseList : public Gls::Persistentclasses::TGLPersistentObject
{
	typedef Gls::Persistentclasses::TGLPersistentObject inherited;
	
private:
	int FCount;
	int FCapacity;
	int FGrowthDelta;
	Gls::Vectorgeometry::TByteVector *FBufferItem;
	TGLBaseListOptions FOptions;
	unsigned FRevision;
	System::UnicodeString FTagString;
	
protected:
	Gls::Vectorgeometry::TByteVector *FBaseList;
	int FItemSize;
	void __fastcall SetCount(int Val);
	virtual void __fastcall SetCapacity(int NewCapacity);
	Gls::Vectorgeometry::PByteVector __fastcall BufferItem();
	bool __fastcall GetSetCountResetsMemory();
	void __fastcall SetSetCountResetsMemory(const bool Val);
	virtual void __fastcall ReadItemsData(System::Classes::TReader* AReader);
	virtual void __fastcall WriteItemsData(System::Classes::TWriter* AWriter);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* AFiler);
	
public:
	__fastcall virtual TGLBaseList();
	__fastcall virtual ~TGLBaseList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* reader);
	void __fastcall AddNulls(unsigned nbVals);
	void __fastcall InsertNulls(int Index, unsigned nbVals);
	void __fastcall AdjustCapacityToAtLeast(const int size);
	int __fastcall DataSize();
	void __fastcall UseMemory(void * rangeStart, int rangeCapacity);
	void __fastcall Flush();
	void __fastcall Clear();
	void __fastcall Delete(int Index);
	void __fastcall DeleteItems(int Index, unsigned nbVals);
	void __fastcall Exchange(int index1, int index2);
	void __fastcall Move(int curIndex, int newIndex);
	void __fastcall Reverse();
	__property int Count = {read=FCount, write=SetCount, nodefault};
	__property int Capacity = {read=FCapacity, write=SetCapacity, nodefault};
	__property int GrowthDelta = {read=FGrowthDelta, write=FGrowthDelta, nodefault};
	__property bool SetCountResetsMemory = {read=GetSetCountResetsMemory, write=SetSetCountResetsMemory, nodefault};
	__property System::UnicodeString TagString = {read=FTagString, write=FTagString};
	__property unsigned Revision = {read=FRevision, write=FRevision, nodefault};
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLBaseList(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Persistentclasses::TGLPersistentObject(reader) { }
	
};


class PASCALIMPLEMENTATION TGLBaseVectorList : public TGLBaseList
{
	typedef TGLBaseList inherited;
	
protected:
	Gls::Vectorgeometry::PFloatVector __fastcall GetItemAddress(int Index);
	
public:
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* reader);
	virtual void __fastcall GetExtents(/* out */ Gls::Vectortypes::TVector3f &min, /* out */ Gls::Vectortypes::TVector3f &max);
	Gls::Vectortypes::TVector3f __fastcall Sum();
	virtual void __fastcall Normalize();
	float __fastcall MaxSpacing(TGLBaseVectorList* list2);
	virtual void __fastcall Translate(const Gls::Vectortypes::TVector3f &delta)/* overload */;
	virtual void __fastcall Translate(TGLBaseVectorList* const delta)/* overload */;
	virtual void __fastcall TranslateInv(TGLBaseVectorList* const delta)/* overload */;
	virtual void __fastcall Lerp(TGLBaseVectorList* const list1, TGLBaseVectorList* const list2, float lerpFactor) = 0 ;
	void __fastcall AngleLerp(TGLBaseVectorList* const list1, TGLBaseVectorList* const list2, float lerpFactor);
	void __fastcall AngleCombine(TGLBaseVectorList* const list1, float intensity);
	virtual void __fastcall Combine(TGLBaseVectorList* const list2, float factor);
	__property Gls::Vectorgeometry::PFloatVector ItemAddress[int Index] = {read=GetItemAddress};
public:
	/* TGLBaseList.Create */ inline __fastcall virtual TGLBaseVectorList() : TGLBaseList() { }
	/* TGLBaseList.Destroy */ inline __fastcall virtual ~TGLBaseVectorList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLBaseVectorList(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLBaseList(reader) { }
	
};


class PASCALIMPLEMENTATION TGLAffineVectorList : public TGLBaseVectorList
{
	typedef TGLBaseVectorList inherited;
	
public:
	Gls::Vectortypes::TVector3f operator[](int Index) { return this->Items[Index]; }
	
private:
	Gls::Vectorgeometry::TAffineVectorArray *FList;
	
protected:
	Gls::Vectortypes::TVector3f __fastcall Get(int Index);
	void __fastcall Put(int Index, const Gls::Vectortypes::TVector3f &item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TGLAffineVectorList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const Gls::Vectortypes::TVector3f &item)/* overload */;
	int __fastcall Add(const Gls::Vectortypes::TVector4f &item)/* overload */;
	void __fastcall Add(const Gls::Vectortypes::TVector3f &i1, const Gls::Vectortypes::TVector3f &i2)/* overload */;
	void __fastcall Add(const Gls::Vectortypes::TVector3f &i1, const Gls::Vectortypes::TVector3f &i2, const Gls::Vectortypes::TVector3f &i3)/* overload */;
	int __fastcall Add(const Gls::Vectortypes::TVector2f &item)/* overload */;
	int __fastcall Add(const Gls::Vectorgeometry::TTexPoint &item)/* overload */;
	int __fastcall Add(const float X, const float Y)/* overload */;
	int __fastcall Add(const float X, const float Y, const float Z)/* overload */;
	int __fastcall Add(const int X, const int Y, const int Z)/* overload */;
	int __fastcall AddNC(const int X, const int Y, const int Z)/* overload */;
	int __fastcall Add(const Gls::Vectorgeometry::PIntegerVector xy, const int Z)/* overload */;
	int __fastcall AddNC(const Gls::Vectorgeometry::PIntegerVector xy, const int Z)/* overload */;
	void __fastcall Add(TGLAffineVectorList* const list)/* overload */;
	void __fastcall Push(const Gls::Vectortypes::TVector3f &Val);
	Gls::Vectortypes::TVector3f __fastcall Pop();
	void __fastcall Insert(int Index, const Gls::Vectortypes::TVector3f &item);
	int __fastcall IndexOf(const Gls::Vectortypes::TVector3f &item);
	int __fastcall FindOrAdd(const Gls::Vectortypes::TVector3f &item);
	__property Gls::Vectortypes::TVector3f Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Gls::Vectorgeometry::PAffineVectorArray List = {read=FList};
	virtual void __fastcall Translate(const Gls::Vectortypes::TVector3f &delta)/* overload */;
	HIDESBASE void __fastcall Translate(const Gls::Vectortypes::TVector3f &delta, int base, int nb)/* overload */;
	void __fastcall TranslateItem(int Index, const Gls::Vectortypes::TVector3f &delta);
	void __fastcall TranslateItems(int Index, const Gls::Vectortypes::TVector3f &delta, int nb);
	void __fastcall CombineItem(int Index, const Gls::Vectortypes::TVector3f &vector, const float f);
	void __fastcall TransformAsPoints(const Gls::Vectortypes::TMatrix4f &matrix);
	void __fastcall TransformAsVectors(const Gls::Vectortypes::TMatrix4f &matrix)/* overload */;
	void __fastcall TransformAsVectors(const Gls::Vectortypes::TMatrix3f &matrix)/* overload */;
	virtual void __fastcall Normalize();
	virtual void __fastcall Lerp(TGLBaseVectorList* const list1, TGLBaseVectorList* const list2, float lerpFactor);
	void __fastcall Scale(float factor)/* overload */;
	void __fastcall Scale(const Gls::Vectortypes::TVector3f &factors)/* overload */;
public:
	/* TGLBaseList.Destroy */ inline __fastcall virtual ~TGLAffineVectorList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLAffineVectorList(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLBaseVectorList(reader) { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  Translate(TGLBaseVectorList* const delta){ TGLBaseVectorList::Translate(delta); }
	
};


class PASCALIMPLEMENTATION TGLVectorList : public TGLBaseVectorList
{
	typedef TGLBaseVectorList inherited;
	
public:
	Gls::Vectortypes::TVector4f operator[](int Index) { return this->Items[Index]; }
	
private:
	Gls::Vectorgeometry::TVectorArray *FList;
	
protected:
	Gls::Vectortypes::TVector4f __fastcall Get(int Index);
	void __fastcall Put(int Index, const Gls::Vectortypes::TVector4f &item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TGLVectorList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const Gls::Vectortypes::TVector4f &item)/* overload */;
	int __fastcall Add(const Gls::Vectortypes::TVector3f &item, float w)/* overload */;
	int __fastcall Add(const float X, const float Y, const float Z, const float w)/* overload */;
	void __fastcall Add(const Gls::Vectortypes::TVector3f &i1, const Gls::Vectortypes::TVector3f &i2, const Gls::Vectortypes::TVector3f &i3, float w)/* overload */;
	int __fastcall AddVector(const Gls::Vectortypes::TVector3f &item)/* overload */;
	int __fastcall AddPoint(const Gls::Vectortypes::TVector3f &item)/* overload */;
	int __fastcall AddPoint(const float X, const float Y, const float Z = 0.000000E+00f)/* overload */;
	void __fastcall Push(const Gls::Vectortypes::TVector4f &Val);
	Gls::Vectortypes::TVector4f __fastcall Pop();
	int __fastcall IndexOf(const Gls::Vectortypes::TVector4f &item);
	int __fastcall FindOrAdd(const Gls::Vectortypes::TVector4f &item);
	int __fastcall FindOrAddPoint(const Gls::Vectortypes::TVector3f &item);
	void __fastcall Insert(int Index, const Gls::Vectortypes::TVector4f &item);
	__property Gls::Vectortypes::TVector4f Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Gls::Vectorgeometry::PVectorArray List = {read=FList};
	virtual void __fastcall Lerp(TGLBaseVectorList* const list1, TGLBaseVectorList* const list2, float lerpFactor);
public:
	/* TGLBaseList.Destroy */ inline __fastcall virtual ~TGLVectorList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLVectorList(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLBaseVectorList(reader) { }
	
};


class PASCALIMPLEMENTATION TGLTexPointList : public TGLBaseVectorList
{
	typedef TGLBaseVectorList inherited;
	
public:
	Gls::Vectorgeometry::TTexPoint operator[](int Index) { return this->Items[Index]; }
	
private:
	Gls::Vectorgeometry::TTexPointArray *FList;
	
protected:
	Gls::Vectorgeometry::TTexPoint __fastcall Get(int Index);
	void __fastcall Put(int Index, const Gls::Vectorgeometry::TTexPoint &item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TGLTexPointList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall IndexOf(const Gls::Vectorgeometry::TTexPoint &item);
	int __fastcall FindOrAdd(const Gls::Vectorgeometry::TTexPoint &item);
	int __fastcall Add(const Gls::Vectorgeometry::TTexPoint &item)/* overload */;
	int __fastcall Add(const Gls::Vectortypes::TVector2f &item)/* overload */;
	int __fastcall Add(const float texS, const float Text)/* overload */;
	int __fastcall Add(const int texS, const int Text)/* overload */;
	int __fastcall AddNC(const int texS, const int Text)/* overload */;
	int __fastcall Add(const Gls::Vectorgeometry::PIntegerVector texST)/* overload */;
	int __fastcall AddNC(const Gls::Vectorgeometry::PIntegerVector texST)/* overload */;
	void __fastcall Push(const Gls::Vectorgeometry::TTexPoint &Val);
	Gls::Vectorgeometry::TTexPoint __fastcall Pop();
	void __fastcall Insert(int Index, const Gls::Vectorgeometry::TTexPoint &item);
	__property Gls::Vectorgeometry::TTexPoint Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Gls::Vectorgeometry::PTexPointArray List = {read=FList};
	HIDESBASE void __fastcall Translate(const Gls::Vectorgeometry::TTexPoint &delta);
	void __fastcall ScaleAndTranslate(const Gls::Vectorgeometry::TTexPoint &scale, const Gls::Vectorgeometry::TTexPoint &delta)/* overload */;
	void __fastcall ScaleAndTranslate(const Gls::Vectorgeometry::TTexPoint &scale, const Gls::Vectorgeometry::TTexPoint &delta, int base, int nb)/* overload */;
	virtual void __fastcall Lerp(TGLBaseVectorList* const list1, TGLBaseVectorList* const list2, float lerpFactor);
public:
	/* TGLBaseList.Destroy */ inline __fastcall virtual ~TGLTexPointList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLTexPointList(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLBaseVectorList(reader) { }
	
};


class PASCALIMPLEMENTATION TGLIntegerList : public TGLBaseList
{
	typedef TGLBaseList inherited;
	
public:
	int operator[](int Index) { return this->Items[Index]; }
	
private:
	Gls::Vectorgeometry::TIntegerVector *FList;
	
protected:
	int __fastcall Get(int Index);
	void __fastcall Put(int Index, const int item);
	virtual void __fastcall SetCapacity(int newCapacity);
	
public:
	__fastcall virtual TGLIntegerList();
	virtual void __fastcall Assign(System::Classes::TPersistent* src);
	int __fastcall Add(const int item)/* overload */;
	int __fastcall AddNC(const int item)/* overload */;
	void __fastcall Add(const int i1, const int i2)/* overload */;
	void __fastcall Add(const int i1, const int i2, const int i3)/* overload */;
	void __fastcall Add(TGLIntegerList* const AList)/* overload */;
	void __fastcall Push(const int Val);
	int __fastcall Pop();
	void __fastcall Insert(int Index, const int item);
	void __fastcall Remove(const int item);
	int __fastcall IndexOf(int item);
	__property int Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Gls::Vectorgeometry::PIntegerVector List = {read=FList};
	void __fastcall AddSerie(int aBase, int aDelta, int aCount);
	void __fastcall AddIntegers(const System::PInteger First, int n)/* overload */;
	void __fastcall AddIntegers(TGLIntegerList* const aList)/* overload */;
	void __fastcall AddIntegers(const int *anArray, const int anArray_High)/* overload */;
	int __fastcall MinInteger();
	int __fastcall MaxInteger();
	void __fastcall Sort();
	void __fastcall SortAndRemoveDuplicates();
	int __fastcall BinarySearch(const int Value)/* overload */;
	int __fastcall BinarySearch(const int Value, bool returnBestFit, bool &found)/* overload */;
	int __fastcall AddSorted(const int Value, const bool ignoreDuplicates = false);
	void __fastcall RemoveSorted(const int Value);
	void __fastcall Offset(int delta)/* overload */;
	void __fastcall Offset(int delta, const int base, const int nb)/* overload */;
public:
	/* TGLBaseList.Destroy */ inline __fastcall virtual ~TGLIntegerList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLIntegerList(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLBaseList(reader) { }
	
};


typedef System::StaticArray<float, 134217728> TGLSingleArrayList;

typedef TGLSingleArrayList *PGLSingleArrayList;

class PASCALIMPLEMENTATION TGLSingleList : public TGLBaseList
{
	typedef TGLBaseList inherited;
	
public:
	float operator[](int Index) { return this->Items[Index]; }
	
private:
	TGLSingleArrayList *FList;
	
protected:
	float __fastcall Get(int Index);
	void __fastcall Put(int Index, const float item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TGLSingleList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const float item)/* overload */;
	void __fastcall Add(const float i1, const float i2)/* overload */;
	void __fastcall AddSingles(const System::PSingle First, int n)/* overload */;
	void __fastcall AddSingles(const float *anArray, const int anArray_High)/* overload */;
	void __fastcall Push(const float Val);
	float __fastcall Pop();
	void __fastcall Insert(int Index, const float item);
	__property float Items[int Index] = {read=Get, write=Put/*, default*/};
	__property PGLSingleArrayList List = {read=FList};
	void __fastcall AddSerie(float aBase, float aDelta, int aCount);
	void __fastcall Offset(float delta)/* overload */;
	void __fastcall Offset(TGLSingleList* const delta)/* overload */;
	void __fastcall Scale(float factor);
	void __fastcall Sqr();
	void __fastcall Sqrt();
	float __fastcall Sum();
	float __fastcall Min();
	float __fastcall Max();
public:
	/* TGLBaseList.Destroy */ inline __fastcall virtual ~TGLSingleList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLSingleList(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLBaseList(reader) { }
	
};


typedef System::StaticArray<double, 134217728> TGLDoubleArrayList;

typedef TGLDoubleArrayList *PGLDoubleArrayList;

class PASCALIMPLEMENTATION TGLDoubleList : public TGLBaseList
{
	typedef TGLBaseList inherited;
	
public:
	double operator[](int Index) { return this->Items[Index]; }
	
private:
	TGLDoubleArrayList *FList;
	
protected:
	double __fastcall Get(int Index);
	void __fastcall Put(int Index, const double item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TGLDoubleList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const double item);
	void __fastcall Push(const double Val);
	double __fastcall Pop();
	void __fastcall Insert(int Index, const double item);
	__property double Items[int Index] = {read=Get, write=Put/*, default*/};
	__property PGLDoubleArrayList List = {read=FList};
	void __fastcall AddSerie(double aBase, double aDelta, int aCount);
	void __fastcall Offset(double delta)/* overload */;
	void __fastcall Offset(TGLDoubleList* const delta)/* overload */;
	void __fastcall Scale(double factor);
	void __fastcall Sqr();
	void __fastcall Sqrt();
	double __fastcall Sum();
	float __fastcall Min();
	float __fastcall Max();
public:
	/* TGLBaseList.Destroy */ inline __fastcall virtual ~TGLDoubleList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLDoubleList(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLBaseList(reader) { }
	
};


class PASCALIMPLEMENTATION TGLByteList : public TGLBaseList
{
	typedef TGLBaseList inherited;
	
public:
	System::Byte operator[](int Index) { return this->Items[Index]; }
	
private:
	Gls::Vectorgeometry::TByteVector *FList;
	
protected:
	System::Byte __fastcall Get(int Index);
	void __fastcall Put(int Index, const System::Byte item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TGLByteList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const System::Byte item);
	void __fastcall Insert(int Index, const System::Byte item);
	__property System::Byte Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Gls::Vectorgeometry::PByteVector List = {read=FList};
public:
	/* TGLBaseList.Destroy */ inline __fastcall virtual ~TGLByteList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLByteList(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLBaseList(reader) { }
	
};


class PASCALIMPLEMENTATION TGLQuaternionList : public TGLBaseVectorList
{
	typedef TGLBaseVectorList inherited;
	
public:
	Gls::Vectorgeometry::TQuaternion operator[](int Index) { return this->Items[Index]; }
	
private:
	Gls::Vectorgeometry::TQuaternionArray *FList;
	
protected:
	Gls::Vectorgeometry::TQuaternion __fastcall Get(int Index);
	void __fastcall Put(int Index, const Gls::Vectorgeometry::TQuaternion &item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TGLQuaternionList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const Gls::Vectorgeometry::TQuaternion &item)/* overload */;
	int __fastcall Add(const Gls::Vectortypes::TVector3f &item, float w)/* overload */;
	int __fastcall Add(const float X, const float Y, const float Z, const float W)/* overload */;
	void __fastcall Push(const Gls::Vectorgeometry::TQuaternion &Val);
	Gls::Vectorgeometry::TQuaternion __fastcall Pop();
	int __fastcall IndexOf(const Gls::Vectorgeometry::TQuaternion &item);
	int __fastcall FindOrAdd(const Gls::Vectorgeometry::TQuaternion &item);
	void __fastcall Insert(int Index, const Gls::Vectorgeometry::TQuaternion &item);
	__property Gls::Vectorgeometry::TQuaternion Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Gls::Vectorgeometry::PQuaternionArray List = {read=FList};
	virtual void __fastcall Lerp(TGLBaseVectorList* const list1, TGLBaseVectorList* const list2, float lerpFactor);
	virtual void __fastcall Combine(TGLBaseVectorList* const list2, float factor);
public:
	/* TGLBaseList.Destroy */ inline __fastcall virtual ~TGLQuaternionList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLQuaternionList(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLBaseVectorList(reader) { }
	
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TGL4ByteData
{
	
private:
	struct DECLSPEC_DRECORD _TGL4ByteData__1
	{
	public:
		System::StaticArray<System::Byte, 4> Value;
	};
	
	
	struct DECLSPEC_DRECORD _TGL4ByteData__2
	{
	public:
		int Value;
	};
	
	
	struct DECLSPEC_DRECORD _TGL4ByteData__3
	{
	public:
		unsigned Value;
	};
	
	
	struct DECLSPEC_DRECORD _TGL4ByteData__4
	{
	public:
		float Value;
	};
	
	
	struct DECLSPEC_DRECORD _TGL4ByteData__5
	{
	public:
		System::StaticArray<System::Word, 2> Value;
	};
	
	
	
	
public:
	union
	{
		struct 
		{
			_TGL4ByteData__5 Word;
		};
		struct 
		{
			_TGL4ByteData__4 Float;
		};
		struct 
		{
			_TGL4ByteData__3 UInt;
		};
		struct 
		{
			_TGL4ByteData__2 Int;
		};
		struct 
		{
			_TGL4ByteData__1 Bytes;
		};
		
	};
};
#pragma pack(pop)


typedef System::StaticArray<TGL4ByteData, 134217728> T4ByteArrayList;

typedef T4ByteArrayList *P4ByteArrayList;

class PASCALIMPLEMENTATION TGL4ByteList : public TGLBaseList
{
	typedef TGLBaseList inherited;
	
public:
	TGL4ByteData operator[](int Index) { return this->Items[Index]; }
	
private:
	T4ByteArrayList *FList;
	
protected:
	TGL4ByteData __fastcall Get(int Index);
	void __fastcall Put(int Index, const TGL4ByteData item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TGL4ByteList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const TGL4ByteData item)/* overload */;
	void __fastcall Add(const float i1)/* overload */;
	void __fastcall Add(const float i1, const float i2)/* overload */;
	void __fastcall Add(const float i1, const float i2, const float i3)/* overload */;
	void __fastcall Add(const float i1, const float i2, const float i3, const float i4)/* overload */;
	void __fastcall Add(const int i1)/* overload */;
	void __fastcall Add(const int i1, const int i2)/* overload */;
	void __fastcall Add(const int i1, const int i2, const int i3)/* overload */;
	void __fastcall Add(const int i1, const int i2, const int i3, const int i4)/* overload */;
	void __fastcall Add(const unsigned i1)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2, const unsigned i3)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2, const unsigned i3, const unsigned i4)/* overload */;
	void __fastcall Add(TGL4ByteList* const AList)/* overload */;
	void __fastcall Push(const TGL4ByteData Val);
	TGL4ByteData __fastcall Pop();
	void __fastcall Insert(int Index, const TGL4ByteData item);
	__property TGL4ByteData Items[int Index] = {read=Get, write=Put/*, default*/};
	__property P4ByteArrayList List = {read=FList};
public:
	/* TGLBaseList.Destroy */ inline __fastcall virtual ~TGL4ByteList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGL4ByteList(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLBaseList(reader) { }
	
};


class PASCALIMPLEMENTATION TGLLongWordList : public TGLBaseList
{
	typedef TGLBaseList inherited;
	
public:
	unsigned operator[](int Index) { return this->Items[Index]; }
	
private:
	Gls::Vectorgeometry::TLongWordVector *FList;
	
protected:
	unsigned __fastcall Get(int Index);
	void __fastcall Put(int Index, const unsigned item);
	virtual void __fastcall SetCapacity(int newCapacity);
	
public:
	__fastcall virtual TGLLongWordList();
	virtual void __fastcall Assign(System::Classes::TPersistent* src);
	int __fastcall Add(const unsigned item)/* overload */;
	int __fastcall AddNC(const unsigned item)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2, const unsigned i3)/* overload */;
	void __fastcall Add(TGLLongWordList* const AList)/* overload */;
	void __fastcall Push(const unsigned Val);
	unsigned __fastcall Pop();
	void __fastcall Insert(int Index, const unsigned item);
	void __fastcall Remove(const unsigned item);
	unsigned __fastcall IndexOf(int item);
	__property unsigned Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Gls::Vectorgeometry::PLongWordVector List = {read=FList};
	void __fastcall AddLongWords(const System::PLongWord First, int n)/* overload */;
	void __fastcall AddLongWords(TGLLongWordList* const aList)/* overload */;
	void __fastcall AddLongWords(const unsigned *anArray, const int anArray_High)/* overload */;
public:
	/* TGLBaseList.Destroy */ inline __fastcall virtual ~TGLLongWordList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLLongWordList(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLBaseList(reader) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall QuickSortLists(int startIndex, int endIndex, TGLSingleList* refList, System::Classes::TList* objList)/* overload */;
extern DELPHI_PACKAGE void __fastcall QuickSortLists(int startIndex, int endIndex, TGLSingleList* refList, TGLBaseList* objList)/* overload */;
extern DELPHI_PACKAGE void __fastcall FastQuickSortLists(int startIndex, int endIndex, TGLSingleList* const refList, Gls::Persistentclasses::TGLPersistentObjectList* const objList);
}	/* namespace Vectorlists */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_VECTORLISTS)
using namespace Gls::Vectorlists;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_VectorlistsHPP
