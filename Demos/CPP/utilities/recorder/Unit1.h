//---------------------------------------------------------------------------
#include "vfw_BCB.h"
#include "GLAVIRecorder.hpp"
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "GLKeyboard.hpp"
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------

// must manually add this #define to correct the Ambiguous error of AVIRecorder1PostProcessEvent
#define TBitmap Graphics::TBitmap

class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLScene * GLScene1;
  TGLSceneViewer *GLSceneViewer1;
  TTrackBar *TrackBar;
  TGLCube *Cube1;
  TGLCube *Cube3;
  TGLCube *Cube2;
  TGLCamera *GLCamera1;
  TGLLightSource *GLLightSource1;
  TStaticText *StaticText1;
  TGLDummyCube *DummyCube1;
  TGLDummyCube *DummyCube2;
  TGLCadencer *GLCadencer1;
  TButton *Button1;
	TGLAVIRecorder *AVIRecorder1;
  void __fastcall TrackBarChange(TObject * Sender);
  void __fastcall FormResize(TObject * Sender);
  void __fastcall Button1Click(TObject * Sender);
  void __fastcall AVIRecorder1PostProcessEvent(TObject * Sender,
                                               TBitmap * frame);
        void __fastcall FormKeyPress(TObject *Sender, char &Key);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

