//---------------------------------------------------------------------------

#include <vcl.h>
#include <sysutils.hpp>

#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("fCameraControllerC.cpp", Form1);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		FormatSettings.DecimalSeparator = (wchar_t)".";
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TForm1), &Form1);
		Application->Run();
	}
	catch (Exception &exception)
	{
		Application->ShowException(&exception);
	}
	catch (...)
	{
		try
		{
			throw Exception("");
		}
		catch (Exception &exception)
		{
			Application->ShowException(&exception);
		}
	}
	return 0;
}
//---------------------------------------------------------------------------
