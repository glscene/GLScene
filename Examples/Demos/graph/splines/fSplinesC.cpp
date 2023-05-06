/* This is a quick demo for the TGLLines object and spline functionality.

   TGLLines can handle normal lines and cubic splines, each node can have a
   different color, and the line can be color-interpolated.

   Note that the camera in this sample is in orthogonal mode, this makes
   for a quick and easy way to work in 2D with OpenGL (try switching the camera
   to perpective mode if you don't see the point).
*/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fSplinesC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.Objects"
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma resource "*.dfm"
TFormSplines *FormSplines;
//---------------------------------------------------------------------------
__fastcall TFormSplines::TFormSplines(TComponent * Owner):TForm(Owner)
{
}

//---------------------------------------------------------------------------

void __fastcall TFormSplines::MoveCenterNodeTo(int x, int y)
{
  GLLines1->Nodes->Items[1]->AsAffineVector =
    GLSceneViewer1->Buffer->ScreenToWorld(x, y);
}

//---------------------------------------------------------------------------

void __fastcall TFormSplines::GLSceneViewer1MouseDown(TObject * Sender,
                                                TMouseButton Button,
                                                TShiftState Shift, int X, int Y)
{
  MoveCenterNodeTo(X, Y);
}

//---------------------------------------------------------------------------

void __fastcall TFormSplines::GLSceneViewer1MouseMove(TObject * Sender,
                                                TShiftState Shift, int X, int Y)
{
  if(Shift.Contains(ssShift))
    MoveCenterNodeTo(X, Y);
}

//---------------------------------------------------------------------------

