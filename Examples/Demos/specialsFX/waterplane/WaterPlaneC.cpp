//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORM("fWaterPlaneC.cpp", FormWaterPlane);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TFormWaterPlane), &FormWaterPlane);
		Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
