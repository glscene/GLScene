/*: This is a quick demo for the TGLLines object and spline functionality.

   TGLLines can handle normal lines and cubic splines, each node can have a
   different color, and the line can be color-interpolated.

   Note that the camera in this sample is in <i>orthogonal</i> mode, this makes
   for a quick and easy way to work in 2D with OpenGL (try switching the camera
   to perpective mode if you don't see the point).
*/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLObjects"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
}

//---------------------------------------------------------------------------

void __fastcall TForm1::MoveCenterNodeTo(int x, int y)
{
  GLLines1->Nodes->Items[1]->AsAffineVector =
    GLSceneViewer1->Buffer->ScreenToWorld(x, y);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject * Sender,
                                                TMouseButton Button,
                                                TShiftState Shift, int X, int Y)
{
  MoveCenterNodeTo(X, Y);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject * Sender,
                                                TShiftState Shift, int X, int Y)
{
  if(Shift.Contains(ssShift))
    MoveCenterNodeTo(X, Y);
}

//---------------------------------------------------------------------------

