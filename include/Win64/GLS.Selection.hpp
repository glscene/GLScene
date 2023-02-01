// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Selection.pas' rev: 35.00 (Windows)

#ifndef Gls_SelectionHPP
#define Gls_SelectionHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Context.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.PersistentClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Selection
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TPickRecord;
class DELPHICLASS TGLPickList;
class DELPHICLASS TGLBaseSelectTechnique;
class DELPHICLASS TGLSelectRenderModeTechnique;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<int> TPickSubObjects;

class PASCALIMPLEMENTATION TPickRecord : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Gls::Baseclasses::TGLUpdateAbleComponent* AObject;
	TPickSubObjects SubObjects;
	float ZMin;
	float ZMax;
public:
	/* TObject.Create */ inline __fastcall TPickRecord() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TPickRecord() { }
	
};


enum DECLSPEC_DENUM TPickSortType : unsigned char { psDefault, psName, psMinDepth, psMaxDepth };

class PASCALIMPLEMENTATION TGLPickList : public Gls::Persistentclasses::TGLPersistentObjectList
{
	typedef Gls::Persistentclasses::TGLPersistentObjectList inherited;
	
public:
	System::TObject* operator[](int Index) { return this->Hit[Index]; }
	
private:
	float __fastcall GetFar(int aValue);
	System::TObject* __fastcall GetHit(int aValue);
	float __fastcall GetNear(int aValue);
	TPickSubObjects __fastcall GetSubObjects(int aValue);
	
public:
	__fastcall TGLPickList(TPickSortType aSortType);
	void __fastcall AddHit(System::TObject* obj, const TPickSubObjects subObj, float zMin, float zMax);
	virtual void __fastcall Clear();
	int __fastcall FindObject(System::TObject* AObject);
	__property float FarDistance[int Index] = {read=GetFar};
	__property System::TObject* Hit[int Index] = {read=GetHit/*, default*/};
	__property float NearDistance[int Index] = {read=GetNear};
	__property TPickSubObjects SubObjects[int Index] = {read=GetSubObjects};
public:
	/* TGLPersistentObjectList.Destroy */ inline __fastcall virtual ~TGLPickList() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLPickList(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Persistentclasses::TGLPersistentObjectList(reader) { }
	
};


class PASCALIMPLEMENTATION TGLBaseSelectTechnique : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<System::TObject*> _TGLBaseSelectTechnique__1;
	
	
protected:
	_TGLBaseSelectTechnique__1 FObjectStack;
	System::StaticArray<unsigned, 256> FNameStack;
	int FCurrentName;
	int FStackPosition;
	int FObjectCountGuess;
	int FHits;
	virtual System::TObject* __fastcall GetObject() = 0 ;
	virtual void __fastcall SetObject(System::TObject* Value) = 0 ;
	virtual int __fastcall GetHits() = 0 ;
	virtual void __fastcall SetHits(int Value) = 0 ;
	virtual void __fastcall SetObjectCountGuess(int Value) = 0 ;
	virtual System::TObject* __fastcall GetItems(int Value) = 0 ;
	
public:
	virtual __classmethod bool __fastcall IsSupported() = 0 ;
	virtual void __fastcall Start() = 0 ;
	virtual bool __fastcall Stop() = 0 ;
	virtual void __fastcall PushObject(System::TObject* AName) = 0 ;
	virtual void __fastcall PopObject() = 0 ;
	virtual void __fastcall LoadObject(System::TObject* AName) = 0 ;
	virtual void __fastcall FillPickingList(TGLPickList* &AList) = 0 ;
	__property System::TObject* CurrentObject = {read=GetObject, write=SetObject};
	__property int ObjectCountGuess = {read=FObjectCountGuess, write=SetObjectCountGuess, nodefault};
	__property int Hits = {read=GetHits, write=SetHits, nodefault};
public:
	/* TObject.Create */ inline __fastcall TGLBaseSelectTechnique() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBaseSelectTechnique() { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TGLBaseSelectTechniqueClass);

class PASCALIMPLEMENTATION TGLSelectRenderModeTechnique : public TGLBaseSelectTechnique
{
	typedef TGLBaseSelectTechnique inherited;
	
	
private:
	typedef System::DynamicArray<unsigned> _TGLSelectRenderModeTechnique__1;
	
	
private:
	_TGLSelectRenderModeTechnique__1 FBuffer;
	
protected:
	virtual System::TObject* __fastcall GetObject();
	virtual void __fastcall SetObject(System::TObject* Value);
	virtual int __fastcall GetHits();
	virtual void __fastcall SetHits(int Value);
	virtual void __fastcall SetObjectCountGuess(int Value);
	
public:
	__classmethod virtual bool __fastcall IsSupported();
	virtual void __fastcall Start();
	virtual bool __fastcall Stop();
	virtual void __fastcall FillPickingList(TGLPickList* &AList);
	__property ObjectCountGuess;
	__property Hits;
	__property CurrentObject;
public:
	/* TObject.Create */ inline __fastcall TGLSelectRenderModeTechnique() : TGLBaseSelectTechnique() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSelectRenderModeTechnique() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word MAX_OBJECT_STACK_DEPTH = System::Word(0x200);
extern DELPHI_PACKAGE TGLBaseSelectTechniqueClass __fastcall GetBestSelectorClass(void);
}	/* namespace Selection */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SELECTION)
using namespace Gls::Selection;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_SelectionHPP
