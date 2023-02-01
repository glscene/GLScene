// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.SimpleNavigation.pas' rev: 35.00 (Windows)

#ifndef Gls_SimplenavigationHPP
#define Gls_SimplenavigationHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.TypInfo.hpp>
#include <System.Math.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <GLS.SceneForm.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Scene.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.Strings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Simplenavigation
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSimpleNavigationKeyCombination;
class DELPHICLASS TGLSimpleNavigationKeyCombinations;
class DELPHICLASS TGLSimpleNavigation;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLSimpleNavigationOption : unsigned char { snoInvertMoveAroundX, snoInvertMoveAroundY, snoInvertZoom, snoInvertMouseWheel, snoInvertRotateX, snoInvertRotateY, snoMouseWheelHandled, snoShowFPS };

typedef System::Set<TGLSimpleNavigationOption, TGLSimpleNavigationOption::snoInvertMoveAroundX, TGLSimpleNavigationOption::snoShowFPS> TGLSimpleNavigationOptions;

enum DECLSPEC_DENUM TGLSimpleNavigationAction : unsigned char { snaNone, snaMoveAroundTarget, snaZoom, snaRotateTarget, snaCustom };

typedef void __fastcall (__closure *TGLSimpleNavigationCustomActionEvent)(TGLSimpleNavigationKeyCombination* Sender, System::Classes::TShiftState Shift, int X, int Y);

class PASCALIMPLEMENTATION TGLSimpleNavigationKeyCombination : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	bool FExitOnMatch;
	TGLSimpleNavigationAction FAction;
	TGLSimpleNavigationCustomActionEvent FOnCustomAction;
	System::Classes::TShiftState FShiftState;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	virtual void __fastcall DoOnCustomAction(System::Classes::TShiftState Shift, int X, int Y);
	
public:
	__fastcall virtual TGLSimpleNavigationKeyCombination(System::Classes::TCollection* Collection);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property System::Classes::TShiftState ShiftState = {read=FShiftState, write=FShiftState, default=0};
	__property bool ExitOnMatch = {read=FExitOnMatch, write=FExitOnMatch, default=1};
	__property TGLSimpleNavigationAction Action = {read=FAction, write=FAction, default=0};
	__property TGLSimpleNavigationCustomActionEvent OnCustomAction = {read=FOnCustomAction, write=FOnCustomAction};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TGLSimpleNavigationKeyCombination() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSimpleNavigationKeyCombinations : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLSimpleNavigationKeyCombination* operator[](int Index) { return this->Items[Index]; }
	
private:
	TGLSimpleNavigationKeyCombination* __fastcall GetItems(int Index);
	void __fastcall SetItems(int Index, TGLSimpleNavigationKeyCombination* const Value);
	
public:
	HIDESBASE TGLSimpleNavigationKeyCombination* __fastcall Add()/* overload */;
	HIDESBASE TGLSimpleNavigationKeyCombination* __fastcall Add(const System::Classes::TShiftState AShiftState, const TGLSimpleNavigationAction AAction, const bool AExitOnMatch = true)/* overload */;
	__property TGLSimpleNavigationKeyCombination* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TOwnedCollection.Create */ inline __fastcall TGLSimpleNavigationKeyCombinations(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLSimpleNavigationKeyCombinations() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLSimpleNavigation : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Vcl::Extctrls::TTimer* FTimer;
	Vcl::Forms::TCustomForm* FForm;
	Gls::Sceneviewer::TGLSceneViewer* FGLSceneViewer;
	int FOldX;
	int FOldY;
	System::UnicodeString FFormCaption;
	float FMoveAroundTargetSpeed;
	float FZoomSpeed;
	TGLSimpleNavigationOptions FOptions;
	TGLSimpleNavigationKeyCombinations* FKeyCombinations;
	float FRotateTargetSpeed;
	Vcl::Controls::TMouseMoveEvent FOnMouseMove;
	bool FSceneForm;
	void __fastcall ShowFPS(System::TObject* Sender);
	void __fastcall ViewerMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall ViewerMouseWheel(System::TObject* Sender, System::Classes::TShiftState Shift, int WheelDelta, const System::Types::TPoint &MousePos, bool &Handled);
	void __fastcall SetGLSceneViewer(Gls::Sceneviewer::TGLSceneViewer* const Value);
	void __fastcall SetForm(Vcl::Forms::TCustomForm* const Value);
	bool __fastcall StoreFormCaption();
	bool __fastcall StoreMoveAroundTargetSpeed();
	bool __fastcall StoreZoomSpeed();
	void __fastcall SetKeyCombinations(TGLSimpleNavigationKeyCombinations* const Value);
	bool __fastcall StoreRotateTargetSpeed();
	void __fastcall SetOptions(const TGLSimpleNavigationOptions Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLSimpleNavigation(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSimpleNavigation();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Vcl::Forms::TCustomForm* Form = {read=FForm, write=SetForm};
	__property Gls::Sceneviewer::TGLSceneViewer* GLSceneViewer = {read=FGLSceneViewer, write=SetGLSceneViewer};
	__property float ZoomSpeed = {read=FZoomSpeed, write=FZoomSpeed, stored=StoreZoomSpeed};
	__property float MoveAroundTargetSpeed = {read=FMoveAroundTargetSpeed, write=FMoveAroundTargetSpeed, stored=StoreMoveAroundTargetSpeed};
	__property float RotateTargetSpeed = {read=FRotateTargetSpeed, write=FRotateTargetSpeed, stored=StoreRotateTargetSpeed};
	__property System::UnicodeString FormCaption = {read=FFormCaption, write=FFormCaption, stored=StoreFormCaption};
	__property TGLSimpleNavigationOptions Options = {read=FOptions, write=SetOptions, default=192};
	__property TGLSimpleNavigationKeyCombinations* KeyCombinations = {read=FKeyCombinations, write=SetKeyCombinations};
	__property Vcl::Controls::TMouseMoveEvent OnMouseMove = {read=FOnMouseMove, write=FOnMouseMove};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Simplenavigation */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SIMPLENAVIGATION)
using namespace Gls::Simplenavigation;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_SimplenavigationHPP
