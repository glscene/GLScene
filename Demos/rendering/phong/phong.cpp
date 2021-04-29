/*: Per-Pixel phong shading demo.

   The TGLPhongShader implements phong shading through the use of an
   ARB vertex and fragment program. So far only the material and light
   properties are supported, some form of texture support will be
   added in future updates.

*/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("phong.res");
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
