/*: Demonstrates how to use texture coordinates to warp an image.

   Load an image (preferably with dimensions a power of two, not too big,
   and less than 256x256 if you have and old hardware, all TNT, GeForce,
   Radeon and better should have no trouble loading big pictures), then click
   somewhere in the image to define the warp point.<br>
   You may use the menu to adjust or choose the effect.

   This sample displays an image with the help of a single TGLHeightField used
   as a convenient way to specify texture coordinates. The camera is in
   orthogonal mode and adjusted along with the viewer to a ratio of 1:1.

   All the warping code is in the TForm1.HeightFieldGetHeight event (the two
   warping codes actually), the rest are just utility methods to load/save,
   adjust settings etc.
*/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("warping.res");
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
