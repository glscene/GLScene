//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("points\fPointsC.cpp", FormPoints);
USEFORM("projection\fProjectionC.cpp", FormProjection);
USEFORM("splines\fSplinesC.cpp", FormSplines);
USEFORM("fcGraph.cpp", FormGraph);
USEFORM("fxy\fFxyC.cpp", FormPlot);
USEFORM("heightfield\fHeightfieldC.cpp", FormHeightField);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TFormGraph), &FormGraph);
		Application->CreateForm(__classid(TFormHeightField), &FormHeightField);
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
