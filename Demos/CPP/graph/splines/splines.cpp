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
USEFORM("Unit1.cpp", Form1);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TForm1), &Form1);
		Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
