//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("pawn\fPawnC.cpp", FormPawn);
USEFORM("tentacles\fTentaclesC.cpp", FormTentacles);
USEFORM("nutsnbolts\fNutsnBoltsC.cpp", FormNutsnBolts);
USEFORM("bendingcyl\fBendingC.cpp", FormBending);
USEFORM("cutoutstar\fCutoutstarC.cpp", FormCutoutstar);
USEFORM("fcExtrusion.cpp", FormExtrusion);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TFormExtrusion), &FormExtrusion);
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
