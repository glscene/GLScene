// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.ApplicationFileIO.pas' rev: 35.00 (Windows)

#ifndef Gls_ApplicationfileioHPP
#define Gls_ApplicationfileioHPP

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
#include <GLS.BaseClasses.hpp>
#include <GLS.Strings.hpp>
#include <GLS.Logger.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Applicationfileio
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLApplicationFileIO;
class DELPHICLASS TGLDataFile;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLApplicationResource : unsigned char { aresNone, aresSplash, aresTexture, aresMaterial, aresSampler, aresFont, aresMesh };

typedef System::Classes::TStream* __fastcall (*TAFIOCreateFileStream)(const System::UnicodeString fileName, System::Word mode);

typedef bool __fastcall (*TAFIOFileStreamExists)(const System::UnicodeString fileName);

typedef void __fastcall (__closure *TAFIOFileStreamEvent)(const System::UnicodeString fileName, System::Word mode, System::Classes::TStream* &Stream);

typedef bool __fastcall (__closure *TAFIOFileStreamExistsEvent)(const System::UnicodeString fileName);

class PASCALIMPLEMENTATION TGLApplicationFileIO : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TAFIOFileStreamEvent FOnFileStream;
	TAFIOFileStreamExistsEvent FOnFileStreamExists;
	
public:
	__fastcall virtual TGLApplicationFileIO(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLApplicationFileIO();
	
__published:
	__property TAFIOFileStreamEvent OnFileStream = {read=FOnFileStream, write=FOnFileStream};
	__property TAFIOFileStreamExistsEvent OnFileStreamExists = {read=FOnFileStreamExists, write=FOnFileStreamExists};
};


enum DECLSPEC_DENUM TGLDataFileCapability : unsigned char { dfcRead, dfcWrite };

typedef System::Set<TGLDataFileCapability, TGLDataFileCapability::dfcRead, TGLDataFileCapability::dfcWrite> TGLDataFileCapabilities;

class PASCALIMPLEMENTATION TGLDataFile : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	System::UnicodeString FResourceName;
	void __fastcall SetResourceName(const System::UnicodeString AName);
	
public:
	__classmethod virtual TGLDataFileCapabilities __fastcall Capabilities();
	virtual TGLDataFile* __fastcall CreateCopy(System::Classes::TPersistent* AOwner);
	virtual void __fastcall LoadFromFile(const System::UnicodeString fileName);
	virtual void __fastcall SaveToFile(const System::UnicodeString fileName);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream);
	virtual void __fastcall Initialize();
	__property System::UnicodeString ResourceName = {read=FResourceName, write=SetResourceName};
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLDataFile(System::Classes::TPersistent* AOwner) : Gls::Baseclasses::TGLUpdateAbleObject(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLDataFile() { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TGLDataFileClass);

typedef System::Classes::TResourceStream TGLResourceStream;

//-- var, const, procedure ---------------------------------------------------
#define GLS_RC_DDS_Type (System::WideChar *)(0x000000000000000aULL)
#define GLS_RC_JPG_Type (System::WideChar *)(0x000000000000000aULL)
#define GLS_RC_XML_Type (System::WideChar *)(0x000000000000000aULL)
#define GLS_RC_String_Type (System::WideChar *)(0x000000000000000aULL)
extern DELPHI_PACKAGE TAFIOCreateFileStream vAFIOCreateFileStream;
extern DELPHI_PACKAGE TAFIOFileStreamExists vAFIOFileStreamExists;
extern DELPHI_PACKAGE bool __fastcall ApplicationFileIODefined(void);
extern DELPHI_PACKAGE bool __fastcall FileStreamExists(const System::UnicodeString fileName);
extern DELPHI_PACKAGE System::Classes::TResourceStream* __fastcall CreateResourceStream(const System::UnicodeString ResName, System::WideChar * ResType);
extern DELPHI_PACKAGE TGLApplicationResource __fastcall StrToGLSResType(const System::UnicodeString AStrRes);
}	/* namespace Applicationfileio */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_APPLICATIONFILEIO)
using namespace Gls::Applicationfileio;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ApplicationfileioHPP
