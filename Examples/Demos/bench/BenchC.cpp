//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("smoking\fSmokingC.cpp", FormSmoking);
USEFORM("volcano\fVolcanoC.cpp", FormVolcano);
USEFORM("whirlwind\fWhirlC.cpp", FormWhirl);
USEFORM("canvas\fCanvasC.cpp", FormCanvas);
USEFORM("fcBench.cpp", FirmBench);
USEFORM("megacube\fMegaCubeC.cpp", Form1);
USEFORM("megaglasscube\fMegaglassC.cpp", FormMegaglasscube);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TFirmBench), &FirmBench);
		Application->CreateForm(__classid(TFormCanvas), &FormCanvas);
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
