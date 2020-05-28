/*
//-----------------------------------------------------
//  SphereSweepAndSlide - Initial work by Dan Bartlett
//  Shows how to use the FPS Movement behaviour
//------------------------------------------------------
//  Controls:
//    W,A,S,D: Movement
//    Mouse: Movement
//    I,J,K,L,O,P: Movement (2nd sphere)
//    F2, F3: First person, Third person
//    F5: Toggle wireframe
//    Space: Move upwards
//    Esc: Quit
//--------------------------------------------------------
*/
#include <vcl.h>
#include <tchar.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("Unit1.cpp", Form1);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
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
