/*: Visibility culling demo/test/sample.

   This sample is used to test and showcase the efficiency (or inefficiency) of
   visibility in various cases. Be aware that the sample may be slow loading
   (the same mesh is loaded multiple times to put some stress).<br>
   In each of the tests, a "square grid" of objects is created and made visible,
   the camera points at the center of the square, making most of the objects
   off-screen. Visibility culling detects that and does not render the off-screen
   or too-far away objects.

   <ul>
   <li>Spheres: this is the default setting, and one in which culling is
      completely inefficient on a T&L board or good OpenGL ICD, mainly because
      the spheres are rendered with build lists that already have some visibility
      culling built-in. If culling is efficient for you in this case, well be
      happy it is, but start looking for a newer graphics board ;)
   <li>Actors: this one is culling friendly, and your framerate can more than
      double by choosing "ObjectBased" mode. This is due to the fact that the
      actor geometry must be resent at each frame, thus limiting T&L capability
      (the AGP stands in the way...). A culled object's geometry is not sent
      at all, and that can reduce the AGP and graphics driver load drastically.
   </ul>
*/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("culling.res");
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
