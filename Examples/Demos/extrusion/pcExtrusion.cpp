//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("nutsnbolts\fNutsnBoltsC.cpp", FormNutsnBolts);
USEFORM("pawn\fPawnC.cpp", FormPawn);
USEFORM("tentacles\fTentaclesC.cpp", FormTentacles);
USEFORM("fcExtrusion.cpp", frmExtrusionC);
USEFORM("bendingcyl\fBendingC.cpp", FormBending);
USEFORM("cutoutstar\fCutoutstarC.cpp", FormCutoutstar);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TfrmExtrusionC), &frmExtrusionC);
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
