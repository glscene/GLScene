// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.GameMenu.pas' rev: 35.00 (Windows)

#ifndef Gls_GamemenuHPP
#define Gls_GamemenuHPP

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
#include <GLS.OpenGLTokens.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Material.hpp>
#include <GLS.BitmapFont.hpp>
#include <GLS.Color.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Canvas.hpp>
#include <GLS.Context.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Gamemenu
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLGameMenu;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLGameMenuScale : unsigned char { gmsNormal, gms1024x768 };

class PASCALIMPLEMENTATION TGLGameMenu : public Gls::Scene::TGLSceneObject
{
	typedef Gls::Scene::TGLSceneObject inherited;
	
private:
	System::Classes::TStrings* FItems;
	int FSelected;
	Gls::Bitmapfont::TGLCustomBitmapFont* FFont;
	int FMarginVert;
	int FMarginHorz;
	int FSpacing;
	TGLGameMenuScale FMenuScale;
	Gls::Color::TGLColor* FBackColor;
	Gls::Color::TGLColor* FInactiveColor;
	Gls::Color::TGLColor* FActiveColor;
	Gls::Color::TGLColor* FDisabledColor;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FTitleMaterialName;
	int FTitleWidth;
	int FTitleHeight;
	System::Classes::TNotifyEvent FOnSelectedChanged;
	int FBoxTop;
	int FBoxBottom;
	int FBoxLeft;
	int FBoxRight;
	int FMenuTop;
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
protected:
	void __fastcall SetMenuScale(TGLGameMenuScale AValue);
	void __fastcall SetMarginHorz(int AValue);
	void __fastcall SetMarginVert(int AValue);
	void __fastcall SetSpacing(int AValue);
	void __fastcall SetFont(Gls::Bitmapfont::TGLCustomBitmapFont* AValue);
	void __fastcall SetBackColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetInactiveColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetActiveColor(Gls::Color::TGLColor* AValue);
	void __fastcall SetDisabledColor(Gls::Color::TGLColor* AValue);
	bool __fastcall GetEnabled(int AIndex);
	void __fastcall SetEnabled(int AIndex, bool AValue);
	void __fastcall SetItems(System::Classes::TStrings* AValue);
	void __fastcall SetSelected(int AValue);
	System::UnicodeString __fastcall GetSelectedText();
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* AValue);
	void __fastcall SetTitleMaterialName(const System::UnicodeString AValue);
	void __fastcall SetTitleWidth(int AValue);
	void __fastcall SetTitleHeight(int AValue);
	void __fastcall ItemsChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TGLGameMenu(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLGameMenu();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__property bool Enabled[int AIndex] = {read=GetEnabled, write=SetEnabled};
	__property System::UnicodeString SelectedText = {read=GetSelectedText};
	void __fastcall SelectNext();
	void __fastcall SelectPrev();
	void __fastcall MouseMenuSelect(const int X, const int Y);
	
__published:
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property TGLGameMenuScale MenuScale = {read=FMenuScale, write=SetMenuScale, default=0};
	__property int MarginHorz = {read=FMarginHorz, write=SetMarginHorz, default=16};
	__property int MarginVert = {read=FMarginVert, write=SetMarginVert, default=16};
	__property int Spacing = {read=FSpacing, write=SetSpacing, default=16};
	__property Gls::Bitmapfont::TGLCustomBitmapFont* Font = {read=FFont, write=SetFont};
	__property System::UnicodeString TitleMaterialName = {read=FTitleMaterialName, write=SetTitleMaterialName};
	__property int TitleWidth = {read=FTitleWidth, write=SetTitleWidth, default=0};
	__property int TitleHeight = {read=FTitleHeight, write=SetTitleHeight, default=0};
	__property Gls::Color::TGLColor* BackColor = {read=FBackColor, write=SetBackColor};
	__property Gls::Color::TGLColor* InactiveColor = {read=FInactiveColor, write=SetInactiveColor};
	__property Gls::Color::TGLColor* ActiveColor = {read=FActiveColor, write=SetActiveColor};
	__property Gls::Color::TGLColor* DisabledColor = {read=FDisabledColor, write=SetDisabledColor};
	__property System::Classes::TStrings* Items = {read=FItems, write=SetItems};
	__property int Selected = {read=FSelected, write=SetSelected, default=-1};
	__property System::Classes::TNotifyEvent OnSelectedChanged = {read=FOnSelectedChanged, write=FOnSelectedChanged};
	__property int BoxTop = {read=FBoxTop, nodefault};
	__property int BoxBottom = {read=FBoxBottom, nodefault};
	__property int BoxLeft = {read=FBoxLeft, nodefault};
	__property int BoxRight = {read=FBoxRight, nodefault};
	__property int MenuTop = {read=FMenuTop, nodefault};
	__property ObjectsSorting = {default=0};
	__property VisibilityCulling = {default=0};
	__property Position;
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
	__property Effects;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGameMenu(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLSceneObject(aParentOwner) { }
	
private:
	void *__IGLMaterialLibrarySupported;	// Gls::Material::IGLMaterialLibrarySupported 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Gls::Material::_di_IGLMaterialLibrarySupported()
	{
		Gls::Material::_di_IGLMaterialLibrarySupported intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gls::Material::IGLMaterialLibrarySupported*(void) { return (Gls::Material::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gamemenu */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_GAMEMENU)
using namespace Gls::Gamemenu;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_GamemenuHPP
