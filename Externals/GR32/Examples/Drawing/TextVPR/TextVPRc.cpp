//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORM("fTextVPRc.cpp", Form1);
i//---------------------------------------------------------------------------
nt WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		ApApplication->CreateForm(__classid(TForm1), &Form1);
		plication->Run();
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