// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.PlugInManager.pas' rev: 35.00 (Windows)

#ifndef Gls_PluginmanagerHPP
#define Gls_PluginmanagerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Pluginmanager
{
//-- forward type declarations -----------------------------------------------
struct TGLPlugInEntry;
class DELPHICLASS TGLResourceManager;
class DELPHICLASS TGLPlugInList;
struct TResManagerEntry;
class DELPHICLASS TGLPlugInManager;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TPIServiceType : unsigned char { stRaw, stObject, stBitmap, stTexture, stImport, stExport };

typedef System::Set<TPIServiceType, TPIServiceType::stRaw, TPIServiceType::stExport> TPIServices;

typedef void __stdcall (*TEnumCallBack)(char * Name);

typedef void __stdcall (*TEnumResourceNames)(TPIServiceType Service, TEnumCallBack Callback);

typedef TPIServices __stdcall (*TGetServices)(void);

typedef char * __stdcall (*TGetVendor)(void);

typedef char * __stdcall (*TGetDescription)(void);

typedef char * __stdcall (*TGetVersion)(void);

typedef TGLPlugInEntry *PPlugInEntry;

struct DECLSPEC_DRECORD TGLPlugInEntry
{
public:
	System::Sysutils::TFileName Path;
	NativeUInt Handle;
	int FileSize;
	System::TDateTime FileDate;
	TEnumResourceNames EnumResourcenames;
	TGetServices GetServices;
	TGetVendor GetVendor;
	TGetDescription GetDescription;
	TGetVersion GetVersion;
};


class PASCALIMPLEMENTATION TGLResourceManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
public:
	virtual void __fastcall Notify(TGLPlugInManager* Sender, System::Classes::TOperation Operation, TPIServiceType Service, int PlugIn) = 0 ;
public:
	/* TComponent.Create */ inline __fastcall virtual TGLResourceManager(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TGLResourceManager() { }
	
};


class PASCALIMPLEMENTATION TGLPlugInList : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
public:
	PPlugInEntry operator[](int Index) { return this->Objects[Index]; }
	
private:
	TGLPlugInManager* FOwner;
	PPlugInEntry __fastcall GetPlugInEntry(int Index);
	void __fastcall SetPlugInEntry(int Index, PPlugInEntry AEntry);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadPlugIns(System::Classes::TReader* Reader);
	void __fastcall WritePlugIns(System::Classes::TWriter* Writer);
	
public:
	__fastcall virtual TGLPlugInList(TGLPlugInManager* AOwner);
	void __fastcall ClearList();
	__property PPlugInEntry Objects[int Index] = {read=GetPlugInEntry, write=SetPlugInEntry/*, default*/};
	__property TGLPlugInManager* Owner = {read=FOwner};
public:
	/* TStringList.Destroy */ inline __fastcall virtual ~TGLPlugInList() { }
	
};


typedef TResManagerEntry *PResManagerEntry;

struct DECLSPEC_DRECORD TResManagerEntry
{
public:
	TGLResourceManager* Manager;
	TPIServices Services;
};


class PASCALIMPLEMENTATION TGLPlugInManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TGLPlugInList* FLibraryList;
	System::Classes::TList* FResManagerList;
	
protected:
	void __fastcall DoNotify(System::Classes::TOperation Operation, TPIServiceType Service, int PlugIn);
	PResManagerEntry __fastcall FindResManager(TGLResourceManager* AManager);
	int __fastcall GetIndexFromFilename(System::UnicodeString FileName);
	PPlugInEntry __fastcall GetPlugInFromFilename(System::UnicodeString FileName);
	
public:
	__fastcall virtual TGLPlugInManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPlugInManager();
	int __fastcall AddPlugIn(System::Sysutils::TFileName Path);
	void __fastcall EditPlugInList();
	void __fastcall RegisterResourceManager(TGLResourceManager* AManager, TPIServices Services);
	void __fastcall RemovePlugIn(int Index);
	void __fastcall UnRegisterRessourceManager(TGLResourceManager* AManager, TPIServices Services);
	
__published:
	__property TGLPlugInList* PlugIns = {read=FLibraryList, write=FLibraryList};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Pluginmanager */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_PLUGINMANAGER)
using namespace Gls::Pluginmanager;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_PluginmanagerHPP
