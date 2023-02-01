// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.AVIRecorder.pas' rev: 35.00 (Windows)

#ifndef Gls_AvirecorderHPP
#define Gls_AvirecorderHPP

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
#include <System.UITypes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Dialogs.hpp>
#include <Formats.VFW.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.Scene.hpp>
#include <GLS.SceneViewer.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Avirecorder
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLAVIRecorder;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TAVICompressor : unsigned char { acDefault, acShowDialog, acDivX };

typedef Formats::Vfw::_di_IAVIStream *PAVIStream;

enum DECLSPEC_DENUM TAVISizeRestriction : unsigned char { srNoRestriction, srForceBlock2x2, srForceBlock4x4, srForceBlock8x8 };

enum DECLSPEC_DENUM TAVIRecorderState : unsigned char { rsNone, rsRecording };

enum DECLSPEC_DENUM TAVIImageRetrievalMode : unsigned char { irmSnapShot, irmRenderToBitmap, irmBitBlt };

typedef void __fastcall (__closure *TAVIRecorderPostProcessEvent)(System::TObject* Sender, Vcl::Graphics::TBitmap* frame);

class PASCALIMPLEMENTATION TGLAVIRecorder : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Vcl::Graphics::TBitmap* AVIBitmap;
	int AVIFrameIndex;
	int AVI_DPI;
	Formats::Vfw::TAVIStreamInfoW asi;
	Formats::Vfw::_di_IAVIFile pfile;
	Formats::Vfw::_di_IAVIStream Stream;
	Formats::Vfw::_di_IAVIStream Stream_c;
	tagBITMAPINFOHEADER *FBitmapInfo;
	void *FBitmapBits;
	unsigned FBitmapSize;
	System::UnicodeString FTempName;
	System::UnicodeString FAVIFilename;
	System::Byte FFPS;
	int FWidth;
	int FHeight;
	TAVISizeRestriction FSizeRestriction;
	TAVIImageRetrievalMode FImageRetrievalMode;
	TAVIRecorderState RecorderState;
	TAVIRecorderPostProcessEvent FOnPostProcessEvent;
	Gls::Scene::TGLSceneBuffer* FBuffer;
	void __fastcall SetHeight(const int val);
	void __fastcall SetWidth(const int val);
	void __fastcall SetSizeRestriction(const TAVISizeRestriction val);
	void __fastcall SetGLSceneViewer(Gls::Sceneviewer::TGLSceneViewer* const Value);
	void __fastcall SetGLNonVisualViewer(Gls::Scene::TGLNonVisualViewer* const Value);
	
protected:
	Gls::Sceneviewer::TGLSceneViewer* FGLSceneViewer;
	Gls::Scene::TGLNonVisualViewer* FGLNonVisualViewer;
	TAVICompressor FCompressor;
	int __fastcall Restricted(int s);
	void __fastcall InternalAddAVIFrame();
	
public:
	__fastcall virtual TGLAVIRecorder(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLAVIRecorder();
	bool __fastcall CreateAVIFile(int DPI = 0x0);
	void __fastcall AddAVIFrame()/* overload */;
	void __fastcall AddAVIFrame(Vcl::Graphics::TBitmap* bmp)/* overload */;
	void __fastcall CloseAVIFile(bool UserAbort = false);
	bool __fastcall Recording();
	
__published:
	__property System::Byte FPS = {read=FFPS, write=FFPS, default=25};
	__property Gls::Sceneviewer::TGLSceneViewer* GLSceneViewer = {read=FGLSceneViewer, write=SetGLSceneViewer};
	__property Gls::Scene::TGLNonVisualViewer* GLNonVisualViewer = {read=FGLNonVisualViewer, write=SetGLNonVisualViewer};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
	__property System::UnicodeString Filename = {read=FAVIFilename, write=FAVIFilename};
	__property TAVICompressor Compressor = {read=FCompressor, write=FCompressor, default=0};
	__property TAVISizeRestriction SizeRestriction = {read=FSizeRestriction, write=SetSizeRestriction, default=3};
	__property TAVIImageRetrievalMode ImageRetrievalMode = {read=FImageRetrievalMode, write=FImageRetrievalMode, default=2};
	__property TAVIRecorderPostProcessEvent OnPostProcessEvent = {read=FOnPostProcessEvent, write=FOnPostProcessEvent};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Avirecorder */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_AVIRECORDER)
using namespace Gls::Avirecorder;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_AvirecorderHPP
