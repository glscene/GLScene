// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Gui.pas' rev: 35.00 (Windows)

#ifndef Gls_GuiHPP
#define Gls_GuiHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Scene.hpp>
#include <GLS.BitmapFont.hpp>
#include <GLS.Material.hpp>
#include <GLS.Context.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Gui
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBaseGuiObject;
struct TGUIRect;
class DELPHICLASS TGLGuiElement;
class DELPHICLASS TGLGuiElementList;
class DELPHICLASS TGLGuiComponent;
class DELPHICLASS TGLGuiComponentList;
class DELPHICLASS TGLGuiLayout;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLBaseGuiObject : public Gls::Scene::TGLBaseSceneObject
{
	typedef Gls::Scene::TGLBaseSceneObject inherited;
	
private:
	bool FRecursiveVisible;
	float FWidth;
	float FHeight;
	
protected:
	virtual void __fastcall NotifyHide();
	virtual void __fastcall NotifyShow();
	void __fastcall SetLeft(const float Value);
	float __fastcall GetLeft();
	void __fastcall SetTop(const float Value);
	float __fastcall GetTop();
	void __fastcall SetWidth(const float val);
	void __fastcall SetHeight(const float val);
	virtual void __fastcall SetVisible(bool aValue);
	
public:
	__fastcall virtual TGLBaseGuiObject(System::Classes::TComponent* AOwner);
	virtual void __fastcall AddChild(Gls::Scene::TGLBaseSceneObject* AChild);
	virtual void __fastcall Insert(int aIndex, Gls::Scene::TGLBaseSceneObject* AChild);
	__property float Width = {read=FWidth, write=SetWidth};
	__property float Height = {read=FHeight, write=SetHeight};
	__property float Left = {read=GetLeft, write=SetLeft};
	__property float Top = {read=GetTop, write=SetTop};
	__property bool RecursiveVisible = {read=FRecursiveVisible, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLBaseGuiObject(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLBaseSceneObject(aParentOwner) { }
	/* TGLBaseSceneObject.Destroy */ inline __fastcall virtual ~TGLBaseGuiObject() { }
	
};


enum DECLSPEC_DENUM TGUIAlignments : unsigned char { GLAlTopLeft, GLAlTop, GLAlTopRight, GLAlLeft, GLAlCenter, GLAlRight, GLAlBottomLeft, GLAlBottom, GLAlBottomRight, GLAlBorder };

struct DECLSPEC_DRECORD TGUIRect
{
public:
	float X1;
	float Y1;
	float X2;
	float Y2;
	float XTiles;
	float YTiles;
};


typedef System::StaticArray<TGUIRect, 10> TGUIDrawResult;

typedef System::UnicodeString TGLGuiElementName;

class PASCALIMPLEMENTATION TGLGuiElement : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Gls::Coordinates::TGLCoordinates2* FTopLeft;
	Gls::Coordinates::TGLCoordinates2* FBottomRight;
	Gls::Coordinates::TGLCoordinates2* FScale;
	TGUIAlignments FAlign;
	System::UnicodeString FName;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetName(const System::UnicodeString val);
	
public:
	__fastcall virtual TGLGuiElement(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLGuiElement();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
__published:
	__property Gls::Coordinates::TGLCoordinates2* TopLeft = {read=FTopLeft, write=FTopLeft};
	__property Gls::Coordinates::TGLCoordinates2* BottomRight = {read=FBottomRight, write=FBottomRight};
	__property Gls::Coordinates::TGLCoordinates2* Scale = {read=FScale, write=FScale};
	__property TGUIAlignments Align = {read=FAlign, write=FAlign, nodefault};
	__property System::UnicodeString Name = {read=FName, write=SetName};
};


class PASCALIMPLEMENTATION TGLGuiElementList : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLGuiElement* operator[](int index) { return this->Items[index]; }
	
private:
	TGLGuiComponent* FGuiComponent;
	
protected:
	void __fastcall SetItems(int index, TGLGuiElement* const val);
	TGLGuiElement* __fastcall GetItems(int index);
	
public:
	__fastcall TGLGuiElementList(TGLGuiComponent* AOwner);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	int __fastcall IndexOf(TGLGuiElement* const Item);
	__property TGLGuiElement* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLGuiElementList() { }
	
};


typedef System::UnicodeString TGLGuiComponentName;

class PASCALIMPLEMENTATION TGLGuiComponent : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLGuiElementList* FElements;
	System::UnicodeString FName;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetName(const System::UnicodeString val);
	
public:
	__fastcall virtual TGLGuiComponent(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLGuiComponent();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	void __fastcall RenderToArea(float X1, float Y1, float X2, float Y2, TGUIDrawResult &Res, bool Refresh = true, float Scale = 1.000000E+00f);
	TGLGuiComponentList* __fastcall GetOwnerList();
	__property TGLGuiComponentList* Owner = {read=GetOwnerList};
	
__published:
	__property TGLGuiElementList* Elements = {read=FElements, write=FElements};
	__property System::UnicodeString Name = {read=FName, write=SetName};
};


class PASCALIMPLEMENTATION TGLGuiComponentList : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLGuiComponent* operator[](int index) { return this->Items[index]; }
	
private:
	TGLGuiLayout* FLayout;
	
protected:
	void __fastcall SetItems(int index, TGLGuiComponent* const val);
	TGLGuiComponent* __fastcall GetItems(int index);
	
public:
	__fastcall TGLGuiComponentList(TGLGuiLayout* AOwner);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	TGLGuiComponent* __fastcall FindItem(System::UnicodeString name);
	__property TGLGuiComponent* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLGuiComponentList() { }
	
};


class PASCALIMPLEMENTATION TGLGuiLayout : public Gls::Baseclasses::TGLUpdateAbleComponent
{
	typedef Gls::Baseclasses::TGLUpdateAbleComponent inherited;
	
private:
	Gls::Bitmapfont::TGLCustomBitmapFont* FBitmapFont;
	Gls::Material::TGLMaterial* FMaterial;
	TGLGuiComponentList* FGuiComponents;
	System::UnicodeString FFileName;
	System::Classes::TList* FGuiComponentList;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetFileName(System::UnicodeString newName);
	
public:
	__fastcall virtual TGLGuiLayout(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLGuiLayout();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	void __fastcall LoadFromFile(System::UnicodeString FN);
	void __fastcall Clear();
	void __fastcall SaveToStream(System::Classes::TStream* Stream);
	void __fastcall SaveToFile(System::UnicodeString FN);
	void __fastcall AddGuiComponent(Gls::Baseclasses::TGLUpdateAbleComponent* Component);
	void __fastcall RemoveGuiComponent(Gls::Baseclasses::TGLUpdateAbleComponent* Component);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	
__published:
	__property Gls::Bitmapfont::TGLCustomBitmapFont* BitmapFont = {read=FBitmapFont, write=FBitmapFont};
	__property Gls::Material::TGLMaterial* Material = {read=FMaterial, write=FMaterial};
	__property TGLGuiComponentList* GuiComponents = {read=FGuiComponents, write=FGuiComponents};
	__property System::UnicodeString FileName = {read=FFileName, write=SetFileName};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGUIRect GuiNullRect;
extern DELPHI_PACKAGE bool __fastcall IsInRect(const TGUIRect &R, float X, float Y);
}	/* namespace Gui */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_GUI)
using namespace Gls::Gui;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_GuiHPP
