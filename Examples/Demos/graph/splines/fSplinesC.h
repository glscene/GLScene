//---------------------------------------------------------------------------

#ifndef fSplinesCH
#define fSplinesCH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Objects.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLScene * GLScene1;
  TGLSceneViewer *GLSceneViewer1;
  TGLCamera *GLCamera1;
  TGLLines *GLLines1;
	TGLLightSource *GLLightSource1;
  void __fastcall GLSceneViewer1MouseDown(TObject * Sender, TMouseButton Button,
                                          TShiftState Shift, int X, int Y);
  void __fastcall GLSceneViewer1MouseMove(TObject * Sender, TShiftState Shift,
                                          int X, int Y);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);

  void __fastcall MoveCenterNodeTo(int x, int y);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

