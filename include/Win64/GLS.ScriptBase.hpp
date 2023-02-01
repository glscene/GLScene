// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.ScriptBase.pas' rev: 35.00 (Windows)

#ifndef Gls_ScriptbaseHPP
#define Gls_ScriptbaseHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <GLS.XCollection.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Scriptbase
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLScriptBase;
class DELPHICLASS TGLScripts;
class DELPHICLASS TGLScriptLibrary;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLScriptState : unsigned char { ssUncompiled, ssCompileErrors, ssCompiled, ssRunningErrors, ssRunning };

class PASCALIMPLEMENTATION TGLScriptBase : public Gls::Xcollection::TXCollectionItem
{
	typedef Gls::Xcollection::TXCollectionItem inherited;
	
private:
	System::Classes::TStringList* FText;
	System::UnicodeString FDescription;
	System::Classes::TStringList* FErrors;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	virtual TGLScriptState __fastcall GetState() = 0 ;
	void __fastcall SetText(System::Classes::TStringList* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLScriptBase(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLScriptBase();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Compile() = 0 ;
	virtual void __fastcall Start() = 0 ;
	virtual void __fastcall Stop() = 0 ;
	virtual void __fastcall Execute() = 0 ;
	virtual void __fastcall Invalidate() = 0 ;
	virtual System::Variant __fastcall Call(System::UnicodeString aName, System::Variant *aParams, const int aParams_High) = 0 ;
	__property System::Classes::TStringList* Errors = {read=FErrors};
	__property TGLScriptState State = {read=GetState, nodefault};
	
__published:
	__property System::Classes::TStringList* Text = {read=FText, write=SetText};
	__property System::UnicodeString Description = {read=FDescription, write=FDescription};
};


class PASCALIMPLEMENTATION TGLScripts : public Gls::Xcollection::TXCollection
{
	typedef Gls::Xcollection::TXCollection inherited;
	
public:
	TGLScriptBase* operator[](int index) { return this->Items[index]; }
	
protected:
	HIDESBASE TGLScriptBase* __fastcall GetItems(int index);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual Gls::Xcollection::TXCollectionItemClass __fastcall ItemsClass();
	virtual bool __fastcall CanAdd(Gls::Xcollection::TXCollectionItemClass aClass);
	__property TGLScriptBase* Items[int index] = {read=GetItems/*, default*/};
public:
	/* TXCollection.Create */ inline __fastcall virtual TGLScripts(System::Classes::TPersistent* aOwner) : Gls::Xcollection::TXCollection(aOwner) { }
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLScripts() { }
	
};


class PASCALIMPLEMENTATION TGLScriptLibrary : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TGLScripts* FScripts;
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall WriteScriptsData(System::Classes::TStream* Stream);
	void __fastcall ReadScriptsData(System::Classes::TStream* Stream);
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLScriptLibrary(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLScriptLibrary();
	
__published:
	__property TGLScripts* Scripts = {read=FScripts};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Scriptbase */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SCRIPTBASE)
using namespace Gls::Scriptbase;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ScriptbaseHPP
