// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.XCollection.pas' rev: 35.00 (Windows)

#ifndef Gls_XcollectionHPP
#define Gls_XcollectionHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <GLS.Strings.hpp>
#include <GLS.PersistentClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Xcollection
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EFilerException;
class DELPHICLASS TXCollectionItem;
class DELPHICLASS TXCollection;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION EFilerException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EFilerException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EFilerException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EFilerException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EFilerException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EFilerException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EFilerException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EFilerException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EFilerException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EFilerException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EFilerException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EFilerException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EFilerException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EFilerException() { }
	
};


class PASCALIMPLEMENTATION TXCollectionItem : public Gls::Persistentclasses::TGLInterfacedPersistent
{
	typedef Gls::Persistentclasses::TGLInterfacedPersistent inherited;
	
private:
	TXCollection* FOwner;
	System::UnicodeString FName;
	
protected:
	virtual System::UnicodeString __fastcall GetName();
	virtual void __fastcall SetName(const System::UnicodeString val);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	void __fastcall RaiseFilerException(const int archiveVersion);
	
public:
	__fastcall virtual TXCollectionItem(TXCollection* aOwner);
	__fastcall virtual ~TXCollectionItem();
	DYNAMIC System::UnicodeString __fastcall GetNamePath();
	__property TXCollection* Owner = {read=FOwner};
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall MoveUp();
	void __fastcall MoveDown();
	int __fastcall Index();
	virtual __classmethod System::UnicodeString __fastcall FriendlyName() = 0 ;
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual System::UnicodeString __fastcall ItemCategory();
	__classmethod virtual bool __fastcall UniqueItem();
	__classmethod virtual bool __fastcall CanAddTo(TXCollection* collection);
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
};


_DECLARE_METACLASS(System::TMetaClass, TXCollectionItemClass);

class PASCALIMPLEMENTATION TXCollection : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	TXCollectionItem* operator[](int index) { return this->Items[index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	System::Classes::TList* FList;
	int FCount;
	int FArchiveVersion;
	
protected:
	TXCollectionItem* __fastcall GetItems(int Index);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	
public:
	__fastcall virtual TXCollection(System::Classes::TPersistent* aOwner);
	__fastcall virtual ~TXCollection();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Loaded();
	__property System::Classes::TPersistent* Owner = {read=FOwner, write=FOwner};
	DYNAMIC System::UnicodeString __fastcall GetNamePath();
	__classmethod virtual TXCollectionItemClass __fastcall ItemsClass();
	__property TXCollectionItem* Items[int index] = {read=GetItems/*, default*/};
	__property int Count = {read=FCount, nodefault};
	int __fastcall Add(TXCollectionItem* anItem);
	TXCollectionItem* __fastcall GetOrCreate(TXCollectionItemClass anItem);
	void __fastcall Delete(int Index);
	void __fastcall Remove(TXCollectionItem* anItem);
	void __fastcall Clear();
	int __fastcall IndexOf(TXCollectionItem* anItem);
	int __fastcall IndexOfClass(TXCollectionItemClass aClass);
	TXCollectionItem* __fastcall GetByClass(TXCollectionItemClass aClass);
	int __fastcall IndexOfName(const System::UnicodeString aName);
	virtual bool __fastcall CanAdd(TXCollectionItemClass aClass);
	__property int ArchiveVersion = {read=FArchiveVersion, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall RegisterXCollectionDestroyEvent(System::Classes::TNotifyEvent notifyEvent);
extern DELPHI_PACKAGE void __fastcall DeRegisterXCollectionDestroyEvent(System::Classes::TNotifyEvent notifyEvent);
extern DELPHI_PACKAGE void __fastcall RegisterXCollectionItemClass(TXCollectionItemClass aClass);
extern DELPHI_PACKAGE void __fastcall UnregisterXCollectionItemClass(TXCollectionItemClass aClass);
extern DELPHI_PACKAGE TXCollectionItemClass __fastcall FindXCollectionItemClass(const System::UnicodeString ClassName);
extern DELPHI_PACKAGE System::Classes::TList* __fastcall GetXCollectionItemClassesList(TXCollectionItemClass baseClass = 0x0);
extern DELPHI_PACKAGE void __fastcall GetXCollectionClassesList(System::Classes::TList* &ClassesList, TXCollectionItemClass baseClass = 0x0);
}	/* namespace Xcollection */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_XCOLLECTION)
using namespace Gls::Xcollection;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_XcollectionHPP
