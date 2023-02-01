// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.ObjectManager.pas' rev: 35.00 (Windows)

#ifndef Gls_ObjectmanagerHPP
#define Gls_ObjectmanagerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Menus.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Utils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Objectmanager
{
//-- forward type declarations -----------------------------------------------
struct TGLSceneObjectEntry;
class DELPHICLASS TGLObjectManager;
//-- type declarations -------------------------------------------------------
typedef TGLSceneObjectEntry *PSceneObjectEntry;

struct DECLSPEC_DRECORD TGLSceneObjectEntry
{
public:
	Gls::Scene::TGLSceneObjectClass ObjectClass;
	System::UnicodeString Name;
	System::UnicodeString Category;
	int Index;
	int ImageIndex;
};


class PASCALIMPLEMENTATION TGLObjectManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TList* FSceneObjectList;
	Vcl::Controls::TImageList* FObjectIcons;
	int FOverlayIndex;
	int FSceneRootIndex;
	int FCameraRootIndex;
	int FLightsourceRootIndex;
	int FObjectRootIndex;
	
protected:
	void __fastcall DestroySceneObjectList();
	PSceneObjectEntry __fastcall FindSceneObjectClass(Gls::Scene::TGLSceneObjectClass AObjectClass, const System::UnicodeString ASceneObject = System::UnicodeString());
	
public:
	__fastcall virtual TGLObjectManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLObjectManager();
	void __fastcall CreateDefaultObjectIcons(unsigned ResourceModule);
	Gls::Scene::TGLSceneObjectClass __fastcall GetClassFromIndex(int Index);
	int __fastcall GetImageIndex(Gls::Scene::TGLSceneObjectClass ASceneObject);
	System::UnicodeString __fastcall GetCategory(Gls::Scene::TGLSceneObjectClass ASceneObject);
	void __fastcall GetRegisteredSceneObjects(System::Classes::TStringList* ObjectList);
	void __fastcall PopulateMenuWithRegisteredSceneObjects(Vcl::Menus::TMenuItem* AMenuItem, System::Classes::TNotifyEvent aClickEvent);
	void __fastcall RegisterSceneObject(Gls::Scene::TGLSceneObjectClass ASceneObject, const System::UnicodeString aName, const System::UnicodeString aCategory)/* overload */;
	void __fastcall RegisterSceneObject(Gls::Scene::TGLSceneObjectClass ASceneObject, const System::UnicodeString aName, const System::UnicodeString aCategory, Vcl::Graphics::TBitmap* aBitmap)/* overload */;
	void __fastcall RegisterSceneObject(Gls::Scene::TGLSceneObjectClass ASceneObject, const System::UnicodeString aName, const System::UnicodeString aCategory, unsigned ResourceModule, System::UnicodeString ResourceName = System::UnicodeString())/* overload */;
	void __fastcall UnRegisterSceneObject(Gls::Scene::TGLSceneObjectClass ASceneObject);
	__property Vcl::Controls::TImageList* ObjectIcons = {read=FObjectIcons};
	__property int SceneRootIndex = {read=FSceneRootIndex, nodefault};
	__property int LightsourceRootIndex = {read=FLightsourceRootIndex, nodefault};
	__property int CameraRootIndex = {read=FCameraRootIndex, nodefault};
	__property int ObjectRootIndex = {read=FObjectRootIndex, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Objectmanager */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_OBJECTMANAGER)
using namespace Gls::Objectmanager;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ObjectmanagerHPP
