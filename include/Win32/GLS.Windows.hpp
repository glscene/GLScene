// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Windows.pas' rev: 35.00 (Windows)

#ifndef Gls_WindowsHPP
#define Gls_WindowsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.Strings.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Objects.hpp>
#include <GLS.State.hpp>
#include <GLS.Utils.hpp>
#include <GLS.Scene.hpp>
#include <GLS.HUDObjects.hpp>
#include <GLS.Material.hpp>
#include <GLS.Context.hpp>
#include <GLS.BitmapFont.hpp>
#include <GLS.WindowsFont.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Gui.hpp>
#include <GLS.Color.hpp>
#include <GLS.Texture.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.BaseClasses.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Windows
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBaseComponent;
class DELPHICLASS TGLBaseControl;
class DELPHICLASS TGLBaseFontControl;
class DELPHICLASS TGLBaseTextControl;
class DELPHICLASS TGLFocusControl;
class DELPHICLASS TGLCustomControl;
class DELPHICLASS TGLPopupMenu;
class DELPHICLASS TGLForm;
class DELPHICLASS TGLPanel;
class DELPHICLASS TGLCheckBox;
class DELPHICLASS TGLButton;
class DELPHICLASS TGLEdit;
class DELPHICLASS TGLLabel;
class DELPHICLASS TGLAdvancedLabel;
class DELPHICLASS TGLScrollbar;
class DELPHICLASS TGLStringGrid;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLBaseComponent : public Gls::Gui::TGLBaseGuiObject
{
	typedef Gls::Gui::TGLBaseGuiObject inherited;
	
private:
	bool FGUIRedraw;
	Gls::Gui::TGLGuiLayout* FGuiLayout;
	System::UnicodeString FGuiLayoutName;
	Gls::Gui::TGLGuiComponent* FGuiComponent;
	bool FReBuildGui;
	bool FRedrawAtOnce;
	float MoveX;
	float MoveY;
	Gls::Gui::TGUIDrawResult FRenderStatus;
	float FAlphaChannel;
	float FRotation;
	bool FNoZWrite;
	bool BlockRendering;
	int RenderingCount;
	int BlockedCount;
	bool GuiDestroying;
	bool FDoChangesOnProgress;
	bool FAutosize;
	void __fastcall SetGUIRedraw(bool value);
	void __fastcall SetDoChangesOnProgress(const bool Value);
	void __fastcall SetAutosize(const bool Value);
	
protected:
	void __fastcall RenderHeader(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	void __fastcall RenderFooter(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall SetGuiLayout(Gls::Gui::TGLGuiLayout* NewGui);
	void __fastcall SetGuiLayoutName(const System::UnicodeString NewName);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	HIDESBASE void __fastcall SetRotation(const float val);
	void __fastcall SetAlphaChannel(const float val);
	bool __fastcall StoreAlphaChannel();
	void __fastcall SetNoZWrite(const bool val);
	
public:
	void __fastcall BlockRender();
	void __fastcall UnBlockRender();
	__fastcall virtual TGLBaseComponent(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBaseComponent();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall DoChanges();
	void __fastcall MoveGUI(float XRel, float YRel);
	void __fastcall PlaceGUI(float XPos, float YPos);
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall InternalRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	__property bool GUIRedraw = {read=FGUIRedraw, write=SetGUIRedraw, nodefault};
	__property bool ReBuildGui = {read=FReBuildGui, write=FReBuildGui, nodefault};
	
__published:
	__property bool Autosize = {read=FAutosize, write=SetAutosize, nodefault};
	__property bool RedrawAtOnce = {read=FRedrawAtOnce, write=FRedrawAtOnce, nodefault};
	__property Gls::Gui::TGLGuiLayout* GuiLayout = {read=FGuiLayout, write=SetGuiLayout};
	__property System::UnicodeString GuiLayoutName = {read=FGuiLayoutName, write=SetGuiLayoutName};
	__property float Rotation = {read=FRotation, write=SetRotation};
	__property float AlphaChannel = {read=FAlphaChannel, write=SetAlphaChannel, stored=StoreAlphaChannel};
	__property bool NoZWrite = {read=FNoZWrite, write=SetNoZWrite, nodefault};
	__property bool DoChangesOnProgress = {read=FDoChangesOnProgress, write=SetDoChangesOnProgress, nodefault};
	__property Visible = {default=1};
	__property Width = {default=0};
	__property Height = {default=0};
	__property Left = {default=0};
	__property Top = {default=0};
	__property Position;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLBaseComponent(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Gui::TGLBaseGuiObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLMouseAction : unsigned char { ma_mouseup, ma_mousedown, ma_mousemove };

typedef void __fastcall (__closure *TGLAcceptMouseQuery)(TGLBaseControl* Sender, System::Classes::TShiftState Shift, TGLMouseAction Action, System::Uitypes::TMouseButton Button, int X, int Y, bool &Accept);

class PASCALIMPLEMENTATION TGLBaseControl : public TGLBaseComponent
{
	typedef TGLBaseComponent inherited;
	
private:
	Vcl::Controls::TMouseEvent FOnMouseDown;
	Vcl::Controls::TMouseMoveEvent FOnMouseMove;
	Vcl::Controls::TMouseEvent FOnMouseUp;
	bool FKeepMouseEvents;
	TGLBaseControl* FActiveControl;
	TGLFocusControl* FFocusedControl;
	TGLAcceptMouseQuery FOnAcceptMouseQuery;
	System::Classes::TNotifyEvent FOnMouseLeave;
	System::Classes::TNotifyEvent FOnMouseEnter;
	TGLBaseControl* FEnteredControl;
	
protected:
	virtual void __fastcall InternalMouseDown(System::Classes::TShiftState Shift, System::Uitypes::TMouseButton Button, int X, int Y);
	virtual void __fastcall InternalMouseUp(System::Classes::TShiftState Shift, System::Uitypes::TMouseButton Button, int X, int Y);
	virtual void __fastcall InternalMouseMove(System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall SetActiveControl(TGLBaseControl* NewControl);
	void __fastcall SetFocusedControl(TGLFocusControl* NewControl);
	TGLBaseControl* __fastcall FindFirstGui();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall DoMouseEnter();
	void __fastcall DoMouseLeave();
	
public:
	virtual bool __fastcall MouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual bool __fastcall MouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual bool __fastcall MouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall KeyPress(System::TObject* Sender, System::WideChar &Key);
	virtual void __fastcall KeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall KeyUp(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	__property TGLBaseControl* ActiveControl = {read=FActiveControl, write=SetActiveControl};
	__property bool KeepMouseEvents = {read=FKeepMouseEvents, write=FKeepMouseEvents, default=0};
	
__published:
	__property TGLFocusControl* FocusedControl = {read=FFocusedControl, write=SetFocusedControl};
	__property Vcl::Controls::TMouseEvent OnMouseDown = {read=FOnMouseDown, write=FOnMouseDown};
	__property Vcl::Controls::TMouseMoveEvent OnMouseMove = {read=FOnMouseMove, write=FOnMouseMove};
	__property Vcl::Controls::TMouseEvent OnMouseUp = {read=FOnMouseUp, write=FOnMouseUp};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	__property TGLAcceptMouseQuery OnAcceptMouseQuery = {read=FOnAcceptMouseQuery, write=FOnAcceptMouseQuery};
public:
	/* TGLBaseComponent.Create */ inline __fastcall virtual TGLBaseControl(System::Classes::TComponent* AOwner) : TGLBaseComponent(AOwner) { }
	/* TGLBaseComponent.Destroy */ inline __fastcall virtual ~TGLBaseControl() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLBaseControl(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLBaseComponent(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLBaseFontControl : public TGLBaseControl
{
	typedef TGLBaseControl inherited;
	
private:
	Gls::Bitmapfont::TGLCustomBitmapFont* FBitmapFont;
	Gls::Vectortypes::TVector4f FDefaultColor;
	
protected:
	System::Uitypes::TColor __fastcall GetDefaultColor();
	void __fastcall SetDefaultColor(System::Uitypes::TColor value);
	void __fastcall SetBitmapFont(Gls::Bitmapfont::TGLCustomBitmapFont* NewFont);
	Gls::Bitmapfont::TGLCustomBitmapFont* __fastcall GetBitmapFont();
	void __fastcall WriteTextAt(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, const float X, const float Y, const System::UnicodeString Data, const Gls::Vectortypes::TVector4f &Color)/* overload */;
	void __fastcall WriteTextAt(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, const float X1, const float Y1, const float X2, const float Y2, const System::UnicodeString Data, const Gls::Vectortypes::TVector4f &Color)/* overload */;
	int __fastcall GetFontHeight();
	
public:
	__fastcall virtual TGLBaseFontControl(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBaseFontControl();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
__published:
	__property Gls::Bitmapfont::TGLCustomBitmapFont* BitmapFont = {read=GetBitmapFont, write=SetBitmapFont};
	__property System::Uitypes::TColor DefaultColor = {read=GetDefaultColor, write=SetDefaultColor, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLBaseFontControl(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLBaseControl(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLBaseTextControl : public TGLBaseFontControl
{
	typedef TGLBaseFontControl inherited;
	
private:
	System::UnicodeString FCaption;
	
protected:
	void __fastcall SetCaption(const System::UnicodeString NewCaption);
	
__published:
	__property System::UnicodeString Caption = {read=FCaption, write=SetCaption};
public:
	/* TGLBaseFontControl.Create */ inline __fastcall virtual TGLBaseTextControl(System::Classes::TComponent* AOwner) : TGLBaseFontControl(AOwner) { }
	/* TGLBaseFontControl.Destroy */ inline __fastcall virtual ~TGLBaseTextControl() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLBaseTextControl(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLBaseFontControl(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLFocusControl : public TGLBaseTextControl
{
	typedef TGLBaseTextControl inherited;
	
private:
	TGLBaseControl* FRootControl;
	bool FFocused;
	Vcl::Controls::TKeyEvent FOnKeyDown;
	Vcl::Controls::TKeyEvent FOnKeyUp;
	Vcl::Controls::TKeyPressEvent FOnKeyPress;
	System::Classes::TShiftState FShiftState;
	Gls::Vectortypes::TVector4f FFocusedColor;
	
protected:
	virtual void __fastcall InternalKeyPress(System::WideChar &Key);
	virtual void __fastcall InternalKeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall InternalKeyUp(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall SetFocused(bool Value);
	TGLBaseControl* __fastcall GetRootControl();
	System::Uitypes::TColor __fastcall GetFocusedColor();
	void __fastcall SetFocusedColor(const System::Uitypes::TColor Val);
	
public:
	__fastcall virtual ~TGLFocusControl();
	virtual void __fastcall NotifyHide();
	virtual void __fastcall MoveTo(Gls::Scene::TGLBaseSceneObject* newParent);
	void __fastcall ReGetRootControl();
	void __fastcall SetFocus();
	void __fastcall PrevControl();
	void __fastcall NextControl();
	virtual void __fastcall KeyPress(System::TObject* Sender, System::WideChar &Key);
	virtual void __fastcall KeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall KeyUp(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	
__published:
	__property TGLBaseControl* RootControl = {read=GetRootControl};
	__property bool Focused = {read=FFocused, write=SetFocused, nodefault};
	__property System::Uitypes::TColor FocusedColor = {read=GetFocusedColor, write=SetFocusedColor, nodefault};
	__property Vcl::Controls::TKeyEvent OnKeyDown = {read=FOnKeyDown, write=FOnKeyDown};
	__property Vcl::Controls::TKeyEvent OnKeyUp = {read=FOnKeyUp, write=FOnKeyUp};
	__property Vcl::Controls::TKeyPressEvent OnKeyPress = {read=FOnKeyPress, write=FOnKeyPress};
public:
	/* TGLBaseFontControl.Create */ inline __fastcall virtual TGLFocusControl(System::Classes::TComponent* AOwner) : TGLBaseTextControl(AOwner) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFocusControl(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLBaseTextControl(aParentOwner) { }
	
};


typedef void __fastcall (__closure *TGLCustomRenderEvent)(TGLCustomControl* Sender, Vcl::Graphics::TBitmap* Bitmap);

class PASCALIMPLEMENTATION TGLCustomControl : public TGLFocusControl
{
	typedef TGLFocusControl inherited;
	
private:
	void *FCustomData;
	System::TObject* FCustomObject;
	TGLCustomRenderEvent FOnRender;
	Gls::Material::TGLMaterial* FMaterial;
	Vcl::Graphics::TBitmap* FBitmap;
	Vcl::Graphics::TBitmap* FInternalBitmap;
	bool FBitmapChanged;
	float FXTexCoord;
	float FYTexCoord;
	int FInvalidRenderCount;
	int FMaxInvalidRenderCount;
	bool FCentered;
	void __fastcall SetCentered(const bool Value);
	
protected:
	void __fastcall OnBitmapChanged(System::TObject* Sender);
	void __fastcall SetBitmap(Vcl::Graphics::TBitmap* ABitmap);
	
public:
	__fastcall virtual TGLCustomControl(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomControl();
	virtual void __fastcall InternalRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	void __fastcall SetMaterial(Gls::Material::TGLMaterial* AMaterial);
	__property void * CustomData = {read=FCustomData, write=FCustomData};
	__property System::TObject* CustomObject = {read=FCustomObject, write=FCustomObject};
	
__published:
	__property TGLCustomRenderEvent OnRender = {read=FOnRender, write=FOnRender};
	__property bool Centered = {read=FCentered, write=SetCentered, nodefault};
	__property Gls::Material::TGLMaterial* Material = {read=FMaterial, write=SetMaterial};
	__property Vcl::Graphics::TBitmap* Bitmap = {read=FBitmap, write=SetBitmap};
	__property int MaxInvalidRenderCount = {read=FMaxInvalidRenderCount, write=FMaxInvalidRenderCount, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCustomControl(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLFocusControl(aParentOwner) { }
	
};


typedef void __fastcall (__closure *TGLPopupMenuClick)(TGLPopupMenu* Sender, int index, const System::UnicodeString MenuItemText);

class PASCALIMPLEMENTATION TGLPopupMenu : public TGLFocusControl
{
	typedef TGLFocusControl inherited;
	
private:
	TGLPopupMenuClick FOnClick;
	System::Classes::TStrings* FMenuItems;
	int FSelIndex;
	float FMarginSize;
	float NewHeight;
	
protected:
	virtual void __fastcall SetFocused(bool Value);
	void __fastcall SetMenuItems(System::Classes::TStrings* Value);
	void __fastcall SetMarginSize(const float val);
	void __fastcall SetSelIndex(const int val);
	virtual void __fastcall InternalMouseDown(System::Classes::TShiftState Shift, System::Uitypes::TMouseButton Button, int X, int Y);
	virtual void __fastcall InternalMouseMove(System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall OnStringListChange(System::TObject* Sender);
	
public:
	__fastcall virtual TGLPopupMenu(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPopupMenu();
	void __fastcall PopUp(int Px, int Py);
	virtual void __fastcall InternalRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual bool __fastcall MouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	
__published:
	__property System::Classes::TStrings* MenuItems = {read=FMenuItems, write=SetMenuItems};
	__property TGLPopupMenuClick OnClick = {read=FOnClick, write=FOnClick};
	__property float MarginSize = {read=FMarginSize, write=SetMarginSize};
	__property int SelIndex = {read=FSelIndex, write=SetSelIndex, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPopupMenu(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLFocusControl(aParentOwner) { }
	
};


typedef void __fastcall (__closure *TGLFormCanRequest)(TGLForm* Sender, bool &Can);

enum DECLSPEC_DENUM TGLFormCloseOptions : unsigned char { co_Hide, co_Ignore, co_Destroy };

typedef void __fastcall (__closure *TGLFormCanClose)(TGLForm* Sender, TGLFormCloseOptions &CanClose);

typedef void __fastcall (__closure *TGLFormNotify)(TGLForm* Sender);

typedef void __fastcall (__closure *TGLFormMove)(TGLForm* Sender, float &Left, float &Top);

class PASCALIMPLEMENTATION TGLForm : public TGLBaseTextControl
{
	typedef TGLBaseTextControl inherited;
	
private:
	TGLFormCanRequest FOnCanMove;
	TGLFormCanRequest FOnCanResize;
	TGLFormCanClose FOnCanClose;
	TGLFormNotify FOnShow;
	TGLFormNotify FOnHide;
	TGLFormMove FOnMoving;
	bool Moving;
	int OldX;
	int OldY;
	Gls::Vectortypes::TVector4f FTitleColor;
	float FTitleOffset;
	
protected:
	virtual void __fastcall InternalMouseDown(System::Classes::TShiftState Shift, System::Uitypes::TMouseButton Button, int X, int Y);
	virtual void __fastcall InternalMouseUp(System::Classes::TShiftState Shift, System::Uitypes::TMouseButton Button, int X, int Y);
	virtual void __fastcall InternalMouseMove(System::Classes::TShiftState Shift, int X, int Y);
	System::Uitypes::TColor __fastcall GetTitleColor();
	void __fastcall SetTitleColor(System::Uitypes::TColor value);
	
public:
	__fastcall virtual TGLForm(System::Classes::TComponent* AOwner);
	void __fastcall Close();
	virtual void __fastcall NotifyShow();
	virtual void __fastcall NotifyHide();
	virtual bool __fastcall MouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual bool __fastcall MouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall InternalRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property System::Uitypes::TColor TitleColor = {read=GetTitleColor, write=SetTitleColor, nodefault};
	__property TGLFormCanRequest OnCanMove = {read=FOnCanMove, write=FOnCanMove};
	__property TGLFormCanRequest OnCanResize = {read=FOnCanResize, write=FOnCanResize};
	__property TGLFormCanClose OnCanClose = {read=FOnCanClose, write=FOnCanClose};
	__property TGLFormNotify OnShow = {read=FOnShow, write=FOnShow};
	__property TGLFormNotify OnHide = {read=FOnHide, write=FOnHide};
	__property TGLFormMove OnMoving = {read=FOnMoving, write=FOnMoving};
	__property float TitleOffset = {read=FTitleOffset, write=FTitleOffset};
public:
	/* TGLBaseFontControl.Destroy */ inline __fastcall virtual ~TGLForm() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLForm(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLBaseTextControl(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLPanel : public TGLBaseControl
{
	typedef TGLBaseControl inherited;
	
public:
	/* TGLBaseComponent.Create */ inline __fastcall virtual TGLPanel(System::Classes::TComponent* AOwner) : TGLBaseControl(AOwner) { }
	/* TGLBaseComponent.Destroy */ inline __fastcall virtual ~TGLPanel() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPanel(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLBaseControl(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLCheckBox : public TGLBaseControl
{
	typedef TGLBaseControl inherited;
	
private:
	bool FChecked;
	System::Classes::TNotifyEvent FOnChange;
	System::UnicodeString FGuiLayoutNameChecked;
	Gls::Gui::TGLGuiComponent* FGuiCheckedComponent;
	int FGroup;
	
protected:
	void __fastcall SetChecked(bool NewChecked);
	virtual void __fastcall InternalMouseDown(System::Classes::TShiftState Shift, System::Uitypes::TMouseButton Button, int X, int Y);
	virtual void __fastcall InternalMouseUp(System::Classes::TShiftState Shift, System::Uitypes::TMouseButton Button, int X, int Y);
	void __fastcall SetGuiLayoutNameChecked(const System::UnicodeString newName);
	virtual void __fastcall SetGuiLayout(Gls::Gui::TGLGuiLayout* NewGui);
	void __fastcall SetGroup(const int val);
	
public:
	__fastcall virtual TGLCheckBox(System::Classes::TComponent* AOwner);
	virtual void __fastcall InternalRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	
__published:
	__property int Group = {read=FGroup, write=SetGroup, nodefault};
	__property bool Checked = {read=FChecked, write=SetChecked, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::UnicodeString GuiLayoutNameChecked = {read=FGuiLayoutNameChecked, write=SetGuiLayoutNameChecked};
public:
	/* TGLBaseComponent.Destroy */ inline __fastcall virtual ~TGLCheckBox() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCheckBox(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLBaseControl(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLButton : public TGLFocusControl
{
	typedef TGLFocusControl inherited;
	
private:
	bool FPressed;
	System::Classes::TNotifyEvent FOnButtonClick;
	System::UnicodeString FGuiLayoutNamePressed;
	Gls::Gui::TGLGuiComponent* FGuiPressedComponent;
	Gls::Material::TGLMaterial* FBitBtn;
	int FGroup;
	float FLogicWidth;
	float FLogicHeight;
	float FXOffSet;
	float FYOffSet;
	bool FAllowUp;
	
protected:
	void __fastcall SetPressed(bool NewPressed);
	virtual void __fastcall InternalMouseDown(System::Classes::TShiftState Shift, System::Uitypes::TMouseButton Button, int X, int Y);
	virtual void __fastcall InternalMouseUp(System::Classes::TShiftState Shift, System::Uitypes::TMouseButton Button, int X, int Y);
	virtual void __fastcall InternalKeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall InternalKeyUp(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall SetFocused(bool Value);
	void __fastcall SetGuiLayoutNamePressed(const System::UnicodeString newName);
	virtual void __fastcall SetGuiLayout(Gls::Gui::TGLGuiLayout* NewGui);
	void __fastcall SetBitBtn(Gls::Material::TGLMaterial* AValue);
	virtual void __fastcall DestroyHandle();
	void __fastcall SetGroup(const int val);
	void __fastcall SetLogicWidth(const float val);
	void __fastcall SetLogicHeight(const float val);
	void __fastcall SetXOffset(const float val);
	void __fastcall SetYOffset(const float val);
	
public:
	__fastcall virtual TGLButton(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLButton();
	virtual void __fastcall InternalRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property int Group = {read=FGroup, write=SetGroup, nodefault};
	__property Gls::Material::TGLMaterial* BitBtn = {read=FBitBtn, write=SetBitBtn};
	__property bool Pressed = {read=FPressed, write=SetPressed, nodefault};
	__property System::Classes::TNotifyEvent OnButtonClick = {read=FOnButtonClick, write=FOnButtonClick};
	__property System::UnicodeString GuiLayoutNamePressed = {read=FGuiLayoutNamePressed, write=SetGuiLayoutNamePressed};
	__property float LogicWidth = {read=FLogicWidth, write=SetLogicWidth};
	__property float LogicHeight = {read=FLogicHeight, write=SetLogicHeight};
	__property float XOffset = {read=FXOffSet, write=SetXOffset};
	__property float YOffset = {read=FYOffSet, write=SetYOffset};
	__property bool AllowUp = {read=FAllowUp, write=FAllowUp, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLButton(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLFocusControl(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLEdit : public TGLFocusControl
{
	typedef TGLFocusControl inherited;
	
private:
	System::Classes::TNotifyEvent FOnChange;
	int FSelStart;
	bool FReadOnly;
	System::UnicodeString FEditChar;
	
protected:
	virtual void __fastcall InternalMouseDown(System::Classes::TShiftState Shift, System::Uitypes::TMouseButton Button, int X, int Y);
	virtual void __fastcall InternalKeyPress(System::WideChar &Key);
	virtual void __fastcall InternalKeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall InternalKeyUp(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall SetFocused(bool Value);
	void __fastcall SetSelStart(const int Value);
	void __fastcall SetEditChar(const System::UnicodeString Value);
	
public:
	__fastcall virtual TGLEdit(System::Classes::TComponent* AOwner);
	virtual void __fastcall InternalRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property System::UnicodeString EditChar = {read=FEditChar, write=SetEditChar};
	__property bool ReadOnly = {read=FReadOnly, write=FReadOnly, default=0};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property int SelStart = {read=FSelStart, write=SetSelStart, nodefault};
public:
	/* TGLFocusControl.Destroy */ inline __fastcall virtual ~TGLEdit() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLEdit(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLFocusControl(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLLabel : public TGLBaseTextControl
{
	typedef TGLBaseTextControl inherited;
	
private:
	System::Classes::TAlignment FAlignment;
	Vcl::Stdctrls::TTextLayout FTextLayout;
	void __fastcall SetAlignment(const System::Classes::TAlignment Value);
	void __fastcall SetTextLayout(const Vcl::Stdctrls::TTextLayout Value);
	
public:
	__fastcall virtual TGLLabel(System::Classes::TComponent* AOwner);
	virtual void __fastcall InternalRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, nodefault};
	__property Vcl::Stdctrls::TTextLayout TextLayout = {read=FTextLayout, write=SetTextLayout, nodefault};
public:
	/* TGLBaseFontControl.Destroy */ inline __fastcall virtual ~TGLLabel() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLLabel(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLBaseTextControl(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLAdvancedLabel : public TGLFocusControl
{
	typedef TGLFocusControl inherited;
	
public:
	virtual void __fastcall InternalRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
public:
	/* TGLFocusControl.Destroy */ inline __fastcall virtual ~TGLAdvancedLabel() { }
	
public:
	/* TGLBaseFontControl.Create */ inline __fastcall virtual TGLAdvancedLabel(System::Classes::TComponent* AOwner) : TGLFocusControl(AOwner) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLAdvancedLabel(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLFocusControl(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLScrollbar : public TGLFocusControl
{
	typedef TGLFocusControl inherited;
	
private:
	float FMin;
	float FMax;
	float FStep;
	float FPos;
	float FPageSize;
	System::Classes::TNotifyEvent FOnChange;
	System::UnicodeString FGuiLayoutKnobName;
	Gls::Gui::TGLGuiComponent* FGuiKnobComponent;
	Gls::Gui::TGUIDrawResult FKnobRenderStatus;
	float FScrollOffs;
	bool FScrolling;
	bool FHorizontal;
	bool FLocked;
	
protected:
	void __fastcall SetMin(const float val);
	void __fastcall SetMax(const float val);
	void __fastcall SetPos(const float val);
	void __fastcall SetPageSize(const float val);
	void __fastcall SetHorizontal(const bool val);
	void __fastcall SetGuiLayoutKnobName(const System::UnicodeString newName);
	virtual void __fastcall SetGuiLayout(Gls::Gui::TGLGuiLayout* NewGui);
	float __fastcall GetScrollPosY(float ScrollPos);
	float __fastcall GetYScrollPos(float Y);
	float __fastcall GetScrollPosX(float ScrollPos);
	float __fastcall GetXScrollPos(float X);
	virtual void __fastcall InternalMouseDown(System::Classes::TShiftState Shift, System::Uitypes::TMouseButton Button, int X, int Y);
	virtual void __fastcall InternalMouseUp(System::Classes::TShiftState Shift, System::Uitypes::TMouseButton Button, int X, int Y);
	virtual void __fastcall InternalMouseMove(System::Classes::TShiftState Shift, int X, int Y);
	
public:
	__fastcall virtual TGLScrollbar(System::Classes::TComponent* AOwner);
	void __fastcall StepUp();
	void __fastcall StepDown();
	void __fastcall PageUp();
	void __fastcall PageDown();
	virtual bool __fastcall MouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual bool __fastcall MouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall InternalRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property bool Horizontal = {read=FHorizontal, write=SetHorizontal, nodefault};
	__property float Pos = {read=FPos, write=SetPos};
	__property float Min = {read=FMin, write=SetMin};
	__property float Max = {read=FMax, write=SetMax};
	__property float Step = {read=FStep, write=FStep};
	__property float PageSize = {read=FPageSize, write=SetPageSize};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::UnicodeString GuiLayoutKnobName = {read=FGuiLayoutKnobName, write=SetGuiLayoutKnobName};
	__property bool Locked = {read=FLocked, write=FLocked, default=0};
public:
	/* TGLFocusControl.Destroy */ inline __fastcall virtual ~TGLScrollbar() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLScrollbar(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLFocusControl(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLStringGrid : public TGLFocusControl
{
	typedef TGLFocusControl inherited;
	
private:
	int FSelCol;
	int FSelRow;
	bool FRowSelect;
	bool FColSelect;
	System::Classes::TStrings* FColumns;
	System::Classes::TList* FRows;
	Gls::Vectortypes::TVector4f FHeaderColor;
	int FMarginSize;
	int FColumnSize;
	int FRowHeight;
	TGLScrollbar* FScrollbar;
	bool FDrawHeader;
	
protected:
	bool __fastcall GetCell(int X, int Y, /* out */ int &oCol, /* out */ int &oRow);
	virtual void __fastcall InternalMouseDown(System::Classes::TShiftState Shift, System::Uitypes::TMouseButton Button, int X, int Y);
	void __fastcall SetColumns(System::Classes::TStrings* const val);
	void __fastcall SetColSelect(const bool val);
	System::Classes::TStringList* __fastcall GetRow(int index);
	void __fastcall SetRow(int index, System::Classes::TStringList* const val);
	int __fastcall GetRowCount();
	void __fastcall SetRowCount(const int val);
	void __fastcall SetSelCol(const int val);
	void __fastcall SetSelRow(const int val);
	void __fastcall SetRowSelect(const bool val);
	void __fastcall SetDrawHeader(const bool val);
	System::Uitypes::TColor __fastcall GetHeaderColor();
	void __fastcall SetHeaderColor(const System::Uitypes::TColor val);
	void __fastcall SetMarginSize(const int val);
	void __fastcall SetColumnSize(const int val);
	void __fastcall SetRowHeight(const int val);
	void __fastcall SetScrollbar(TGLScrollbar* const val);
	virtual void __fastcall SetGuiLayout(Gls::Gui::TGLGuiLayout* NewGui);
	
public:
	__fastcall virtual TGLStringGrid(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLStringGrid();
	void __fastcall Clear();
	int __fastcall Add(const System::UnicodeString *Data, const int Data_High)/* overload */;
	int __fastcall Add(const System::UnicodeString Data)/* overload */;
	void __fastcall SetText(System::UnicodeString Data);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall InternalRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	void __fastcall OnStringListChange(System::TObject* Sender);
	__property System::Classes::TStringList* Row[int index] = {read=GetRow, write=SetRow};
	
__published:
	__property System::Uitypes::TColor HeaderColor = {read=GetHeaderColor, write=SetHeaderColor, nodefault};
	__property System::Classes::TStrings* Columns = {read=FColumns, write=SetColumns};
	__property int MarginSize = {read=FMarginSize, write=SetMarginSize, nodefault};
	__property int ColumnSize = {read=FColumnSize, write=SetColumnSize, nodefault};
	__property int RowHeight = {read=FRowHeight, write=SetRowHeight, nodefault};
	__property int RowCount = {read=GetRowCount, write=SetRowCount, nodefault};
	__property int SelCol = {read=FSelCol, write=SetSelCol, nodefault};
	__property int SelRow = {read=FSelRow, write=SetSelRow, nodefault};
	__property bool RowSelect = {read=FRowSelect, write=SetRowSelect, nodefault};
	__property bool ColSelect = {read=FColSelect, write=SetColSelect, nodefault};
	__property bool DrawHeader = {read=FDrawHeader, write=SetDrawHeader, nodefault};
	__property TGLScrollbar* Scrollbar = {read=FScrollbar, write=SetScrollbar};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLStringGrid(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLFocusControl(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall UnpressGroup(Gls::Scene::TGLBaseSceneObject* CurrentObject, int AGroupID);
}	/* namespace Windows */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_WINDOWS)
using namespace Gls::Windows;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_WindowsHPP
