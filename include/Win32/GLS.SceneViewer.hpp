// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.SceneViewer.pas' rev: 35.00 (Windows)

#ifndef Gls_SceneviewerHPP
#define Gls_SceneviewerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <GLS.Scene.hpp>
#include <GLS.WindowsContext.hpp>
#include <GLS.Context.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Sceneviewer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSceneViewer;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TTouchEvent)(int X, int Y, int TouchWidth, int TouchHeight, unsigned TouchID, int TouchCount, bool FromPen);

class PASCALIMPLEMENTATION TGLSceneViewer : public Vcl::Controls::TWinControl
{
	typedef Vcl::Controls::TWinControl inherited;
	
private:
	Gls::Scene::TGLSceneBuffer* FBuffer;
	Gls::Context::TGLVSyncMode FVSync;
	HDC FOwnDC;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	bool FMouseInControl;
	System::Types::TPoint FLastScreenPos;
	bool FPenAsTouch;
	TTouchEvent FOnTouchMove;
	TTouchEvent FOnTouchUp;
	TTouchEvent FOnTouchDown;
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Message);
	MESSAGE void __fastcall WMGetDglCode(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMDestroy(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMTouch(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &msg);
	float __fastcall GetFieldOfView();
	void __fastcall SetFieldOfView(const float Value);
	bool __fastcall GetIsRenderingContextAvailable();
	
protected:
	void __fastcall SetBeforeRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetBeforeRender();
	void __fastcall SetPostRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetPostRender();
	void __fastcall SetAfterRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetAfterRender();
	void __fastcall SetCamera(Gls::Scene::TGLCamera* const val);
	Gls::Scene::TGLCamera* __fastcall GetCamera();
	void __fastcall SetBuffer(Gls::Scene::TGLSceneBuffer* const val);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd();
	virtual void __fastcall DestroyWnd();
	virtual void __fastcall Loaded();
	virtual void __fastcall DoBeforeRender(System::TObject* Sender);
	void __fastcall DoBufferChange(System::TObject* Sender);
	virtual void __fastcall DoBufferStructuralChange(System::TObject* Sender);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	
public:
	__fastcall virtual TGLSceneViewer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSceneViewer();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	HIDESBASE void __fastcall RecreateWnd();
	__property bool IsRenderingContextAvailable = {read=GetIsRenderingContextAvailable, nodefault};
	float __fastcall LastFrameTime();
	float __fastcall FramesPerSecond();
	System::UnicodeString __fastcall FramesPerSecondText(int decimals = 0x1);
	void __fastcall ResetPerformanceMonitor();
	Vcl::Graphics::TBitmap* __fastcall CreateSnapShotBitmap();
	void __fastcall RegisterTouch();
	void __fastcall UnregisterTouch();
	__property HDC RenderDC = {read=FOwnDC, nodefault};
	__property bool MouseInControl = {read=FMouseInControl, nodefault};
	
__published:
	__property Gls::Scene::TGLCamera* Camera = {read=GetCamera, write=SetCamera};
	__property Gls::Context::TGLVSyncMode VSync = {read=FVSync, write=FVSync, default=1};
	__property System::Classes::TNotifyEvent BeforeRender = {read=GetBeforeRender, write=SetBeforeRender};
	__property System::Classes::TNotifyEvent PostRender = {read=GetPostRender, write=SetPostRender};
	__property System::Classes::TNotifyEvent AfterRender = {read=GetAfterRender, write=SetAfterRender};
	__property Gls::Scene::TGLSceneBuffer* Buffer = {read=FBuffer, write=SetBuffer};
	__property float FieldOfView = {read=GetFieldOfView, write=SetFieldOfView};
	__property bool PenAsTouch = {read=FPenAsTouch, write=FPenAsTouch, nodefault};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property TTouchEvent OnTouchMove = {read=FOnTouchMove, write=FOnTouchMove};
	__property TTouchEvent OnTouchUp = {read=FOnTouchUp, write=FOnTouchUp};
	__property TTouchEvent OnTouchDown = {read=FOnTouchDown, write=FOnTouchDown};
	__property Align = {default=0};
	__property Anchors = {default=3};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property HelpContext = {default=0};
	__property Hint = {default=0};
	__property PopupMenu;
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnStartDrag;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseWheelDown;
	__property OnMouseWheelUp;
	__property OnKeyDown;
	__property OnKeyUp;
	__property OnContextPopup;
	__property TabStop = {default=0};
	__property TabOrder = {default=-1};
	__property OnEnter;
	__property OnExit;
	__property OnGesture;
	__property Touch;
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLSceneViewer(HWND ParentWindow) : Vcl::Controls::TWinControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall SetupVSync(const Gls::Context::TGLVSyncMode AVSyncMode);
}	/* namespace Sceneviewer */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SCENEVIEWER)
using namespace Gls::Sceneviewer;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_SceneviewerHPP
