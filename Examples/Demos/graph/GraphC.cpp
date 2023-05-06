//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("points\fPointsC.cpp", FormPoints);
USEFORM("projection\fProjectionC.cpp", FormProjection);
USEFORM("splines\fSplinesC.cpp", FormSplines);
USEFORM("heightfield\fHeightfieldC.cpp", Form1);
USEFORM("fGraphC.cpp", FormGraphC);
USEFORM("fxy\fFxyC.cpp", FormPlot);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TFormGraphC), &FormGraphC);
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
