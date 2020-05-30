//---------------------------------------------------------------------------
// Simple GLSL shader demo. The first version provided by k00m
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEFORM("SimpleGLSLUnit.cpp", Form1);
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
