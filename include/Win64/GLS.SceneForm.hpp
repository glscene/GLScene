// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.SceneForm.pas' rev: 35.00 (Windows)

#ifndef Gls_SceneformHPP
#define Gls_SceneformHPP

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
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Context.hpp>
#include <GLS.Screen.hpp>
#include <GLS.SceneViewer.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Sceneform
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLFullScreenVideoMode;
class DELPHICLASS TGLSceneForm;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLFullScreenResolution : unsigned char { fcUseCurrent, fcNearestResolution, fcManualResolution };

class PASCALIMPLEMENTATION TGLFullScreenVideoMode : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TGLSceneForm* FOwner;
	bool FEnabled;
	bool FAltTabSupportEnable;
	int FWidth;
	int FHeight;
	int FColorDepth;
	int FFrequency;
	TGLFullScreenResolution FResolutionMode;
	void __fastcall SetEnabled(bool aValue);
	void __fastcall SetAltTabSupportEnable(bool aValue);
	
public:
	__fastcall TGLFullScreenVideoMode(TGLSceneForm* AOwner);
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=0};
	__property bool AltTabSupportEnable = {read=FAltTabSupportEnable, write=SetAltTabSupportEnable, default=0};
	__property TGLFullScreenResolution ResolutionMode = {read=FResolutionMode, write=FResolutionMode, default=0};
	__property int Width = {read=FWidth, write=FWidth, nodefault};
	__property int Height = {read=FHeight, write=FHeight, nodefault};
	__property int ColorDepth = {read=FColorDepth, write=FColorDepth, nodefault};
	__property int Frequency = {read=FFrequency, write=FFrequency, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLFullScreenVideoMode() { }
	
};


class PASCALIMPLEMENTATION TGLSceneForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
private:
	Gls::Scene::TGLSceneBuffer* FBuffer;
	Gls::Context::TGLVSyncMode FVSync;
	HDC FOwnDC;
	TGLFullScreenVideoMode* FFullScreenVideoMode;
	void __fastcall SetBeforeRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetBeforeRender();
	void __fastcall SetPostRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetPostRender();
	void __fastcall SetAfterRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetAfterRender();
	void __fastcall SetCamera(Gls::Scene::TGLCamera* const val);
	Gls::Scene::TGLCamera* __fastcall GetCamera();
	void __fastcall SetBuffer(Gls::Scene::TGLSceneBuffer* const val);
	float __fastcall GetFieldOfView();
	void __fastcall SetFieldOfView(const float Value);
	bool __fastcall GetIsRenderingContextAvailable();
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Message);
	HIDESBASE MESSAGE void __fastcall WMDestroy(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall LastFocus(Winapi::Messages::TMessage &Mess);
	void __fastcall SetFullScreenVideoMode(TGLFullScreenVideoMode* AValue);
	void __fastcall StartupFS();
	void __fastcall ShutdownFS();
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall CreateWnd();
	virtual void __fastcall Loaded();
	virtual void __fastcall DoBeforeRender(System::TObject* Sender);
	virtual void __fastcall DoBufferChange(System::TObject* Sender);
	virtual void __fastcall DoBufferStructuralChange(System::TObject* Sender);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	
public:
	__fastcall virtual TGLSceneForm(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSceneForm();
	virtual void __fastcall DestroyWnd();
	__property bool IsRenderingContextAvailable = {read=GetIsRenderingContextAvailable, nodefault};
	__property HDC RenderDC = {read=FOwnDC};
	
__published:
	__property Gls::Scene::TGLCamera* Camera = {read=GetCamera, write=SetCamera};
	__property Gls::Context::TGLVSyncMode VSync = {read=FVSync, write=FVSync, default=1};
	__property System::Classes::TNotifyEvent BeforeRender = {read=GetBeforeRender, write=SetBeforeRender};
	__property System::Classes::TNotifyEvent PostRender = {read=GetPostRender, write=SetPostRender};
	__property System::Classes::TNotifyEvent AfterRender = {read=GetAfterRender, write=SetAfterRender};
	__property Gls::Scene::TGLSceneBuffer* Buffer = {read=FBuffer, write=SetBuffer};
	__property float FieldOfView = {read=GetFieldOfView, write=SetFieldOfView};
	__property TGLFullScreenVideoMode* FullScreenVideoMode = {read=FFullScreenVideoMode, write=SetFullScreenVideoMode};
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLSceneForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLSceneForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 lcl_major = System::Int8(0x0);
static const System::Int8 lcl_minor = System::Int8(0x0);
static const System::Int8 lcl_release = System::Int8(0x0);
}	/* namespace Sceneform */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SCENEFORM)
using namespace Gls::Sceneform;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_SceneformHPP
