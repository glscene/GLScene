//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("megaglasscube\fMegaglassC.cpp", FormMegaglasscube);
USEFORM("smoking\fSmokingC.cpp", FormSmoking);
USEFORM("volcano\fVolcanoC.cpp", FormVolcano);
USEFORM("whirlwind\fWhirlC.cpp", FormWhirl);
USEFORM("canvas\fCanvasC.cpp", FormCanvas);
USEFORM("fBenchC.cpp", frmBench);
USEFORM("megacube\fMegaCubeC.cpp", FormMegacube);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TfrmBench), &frmBench);
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
