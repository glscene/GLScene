//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("pointto\fPointtoC.cpp", FormPointto);
USEFORM("pong\fPongC.cpp", FormPong);
USEFORM("smoothnavi\fSmoothNaviC.cpp", Form1);
USEFORM("tweening\fTweeningC.cpp", FormTweening);
USEFORM("objmove\fObjmoveC.cpp", FormObjmove);
USEFORM("column\fColumnC.cpp", FormColumn);
USEFORM("events\fEventsC.cpp", FormEvents);
USEFORM("fcMovements.cpp", FormMovements);
USEFORM("hierarch\fHierarchC.cpp", FormHierarch);
USEFORM("manual\fManualC.cpp", FormManual);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TFormMovements), &FormMovements);
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
