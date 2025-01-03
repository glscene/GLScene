//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("MainUnitC.cpp", Form4);
USEFORMNS("RGBALoaderUnit.pas", Rgbaloaderunit, RGBALoaderForm);
USEFORMNS("NewImageUnit.pas", Newimageunit, FrmNewImage);
USEFORMNS("MainUnit.pas", Mainunit, MainForm);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TForm4), &Form4);
		Application->CreateForm(__classid(TRGBALoaderForm), &RGBALoaderForm);
		Application->CreateForm(__classid(TFrmNewImage), &FrmNewImage);
		Application->CreateForm(__classid(TMainForm), &MainForm);
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
