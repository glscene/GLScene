/*: Demo/test case for the PointTo method of objects.

   The PointTo method allows to easily orient an object to point toward another
   object, whatever their relative positions in the scene hierarchy.<br>
   In this sample, we have a green sphere turning in circle and riding a sin,
   while a blue arrow, turning in a smaller circle, is maintained pointed
   toward the sphere. The other items (lines...) are just here to help visualize
   the 3D nature of the thing.
*/

//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("pointto.res");
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
