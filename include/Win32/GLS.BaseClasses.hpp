// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.BaseClasses.pas' rev: 35.00 (Windows)

#ifndef Gls_BaseclassesHPP
#define Gls_BaseclassesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.Strings.hpp>
#include <GLS.PersistentClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Baseclasses
{
//-- forward type declarations -----------------------------------------------
struct TGLProgressTimes;
__interface DELPHIINTERFACE IGLNotifyAble;
typedef System::DelphiInterface<IGLNotifyAble> _di_IGLNotifyAble;
__interface DELPHIINTERFACE IGLProgessAble;
typedef System::DelphiInterface<IGLProgessAble> _di_IGLProgessAble;
class DELPHICLASS TGLUpdateAbleObject;
class DELPHICLASS TGLCadenceAbleComponent;
class DELPHICLASS TGLUpdateAbleComponent;
class DELPHICLASS TGLNotifyCollection;
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLProgressTimes
{
public:
	double DeltaTime;
	double NewTime;
	float SqrDeltaTime;
	float InvSqrDeltaTime;
};
#pragma pack(pop)


typedef void __fastcall (__closure *TGLProgressEvent)(System::TObject* Sender, const double DeltaTime, const double NewTime);

__interface  INTERFACE_UUID("{00079A6C-D46E-4126-86EE-F9E2951B4593}") IGLNotifyAble  : public System::IInterface 
{
	virtual void __fastcall NotifyChange(System::TObject* Sender) = 0 ;
};

__interface  INTERFACE_UUID("{95E44548-B0FE-4607-98D0-CA51169AF8B5}") IGLProgessAble  : public System::IInterface 
{
	virtual void __fastcall DoProgress(const TGLProgressTimes &progressTime) = 0 ;
};

class PASCALIMPLEMENTATION TGLUpdateAbleObject : public Gls::Persistentclasses::TGLInterfacedPersistent
{
	typedef Gls::Persistentclasses::TGLInterfacedPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	int FUpdating;
	System::Classes::TNotifyEvent FOnNotifyChange;
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner() _FINAL_ATTRIBUTE;
	
public:
	__fastcall virtual TGLUpdateAbleObject(System::Classes::TPersistent* AOwner);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall Notification(System::TObject* Sender, System::Classes::TOperation Operation);
	__property int Updating = {read=FUpdating, nodefault};
	void __fastcall BeginUpdate();
	void __fastcall EndUpdate();
	__property System::Classes::TPersistent* Owner = {read=FOwner};
	__property System::Classes::TNotifyEvent OnNotifyChange = {read=FOnNotifyChange, write=FOnNotifyChange};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLUpdateAbleObject() { }
	
private:
	void *__IGLNotifyAble;	// IGLNotifyAble 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00079A6C-D46E-4126-86EE-F9E2951B4593}
	operator _di_IGLNotifyAble()
	{
		_di_IGLNotifyAble intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IGLNotifyAble*(void) { return (IGLNotifyAble*)&__IGLNotifyAble; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLCadenceAbleComponent : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
public:
	virtual void __fastcall DoProgress(const TGLProgressTimes &progressTime);
public:
	/* TComponent.Create */ inline __fastcall virtual TGLCadenceAbleComponent(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TGLCadenceAbleComponent() { }
	
private:
	void *__IGLProgessAble;	// IGLProgessAble 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {95E44548-B0FE-4607-98D0-CA51169AF8B5}
	operator _di_IGLProgessAble()
	{
		_di_IGLProgessAble intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IGLProgessAble*(void) { return (IGLProgessAble*)&__IGLProgessAble; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLUpdateAbleComponent : public TGLCadenceAbleComponent
{
	typedef TGLCadenceAbleComponent inherited;
	
public:
	virtual void __fastcall NotifyChange(System::TObject* Sender);
public:
	/* TComponent.Create */ inline __fastcall virtual TGLUpdateAbleComponent(System::Classes::TComponent* AOwner) : TGLCadenceAbleComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TGLUpdateAbleComponent() { }
	
private:
	void *__IGLNotifyAble;	// IGLNotifyAble 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00079A6C-D46E-4126-86EE-F9E2951B4593}
	operator _di_IGLNotifyAble()
	{
		_di_IGLNotifyAble intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IGLNotifyAble*(void) { return (IGLNotifyAble*)&__IGLNotifyAble; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLNotifyCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
private:
	System::Classes::TNotifyEvent FOnNotifyChange;
	
protected:
	virtual void __fastcall Update(System::Classes::TCollectionItem* item);
	
public:
	__fastcall TGLNotifyCollection(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass AItemClass);
	__property System::Classes::TNotifyEvent OnNotifyChange = {read=FOnNotifyChange, write=FOnNotifyChange};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLNotifyCollection() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Baseclasses */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_BASECLASSES)
using namespace Gls::Baseclasses;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_BaseclassesHPP
