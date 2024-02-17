//---------------------------------------------------------------------------

#ifndef fCubeMapCH
#define fCubeMapCH
//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "Jpeg.hpp"

#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Color.hpp"
#include "GLS.Context.hpp"
#include "GLS.Texture.hpp"
#include "GLS.FileDDS.hpp"
#include "GLS.VectorFileObjects.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
  __published: // IDE-managed Components
    TGLSceneViewer* GLSceneViewer1;
    TButton* btnApply;
    TGLScene* GLScene1;
	TGLDummyCube* DummyCube1;
    TGLLightSource* GLLightSource1;
    TGLTeapot* Teapot1;
    TGLCamera* GLCamera1;
    TGLPlane* Plane1;
    void __fastcall GLSceneViewer1BeforeRender(TObject* Sender);
    void __fastcall btnApplyClick(TObject* Sender);
    void __fastcall GLSceneViewer1MouseDown(
        TObject* Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
    void __fastcall GLSceneViewer1MouseMove(
        TObject* Sender, TShiftState Shift, int X, int Y);
    void __fastcall FormMouseWheel(TObject* Sender, TShiftState Shift,
        int WheelDelta, TPoint &MousePos, bool &Handled);
    void __fastcall FormCreate(TObject* Sender);
  private: // User declarations
    int mx, my;
    bool CubmapSupported;
  public: // User declarations
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1* Form1;
//---------------------------------------------------------------------------
#endif

