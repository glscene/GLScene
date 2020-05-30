//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBarChange(TObject *Sender)
{
	int t;

	t = TrackBar->Position;
	// the "sun" spins slowly
	Cube1->TurnAngle = t/4;
	// "earth" rotates around the sun and spins
	DummyCube1->TurnAngle = -t;
	Cube2->TurnAngle = t*2;
	// "moon" rotates around earth and spins
	DummyCube2->RollAngle = 3*t;
	Cube3->TurnAngle = 4*t;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
	if (CBPlay->Checked && Visible)
		// simulate a user action on the trackbar...
		TrackBar->Position = ((TrackBar->Position+1) % 360);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormResize(TObject *Sender)
{
	GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
	// We need to stop playing here :
	// 	since the timer is asynchronous, if we don't stop play,
	// 	it may get triggered during the form's destruction
	CBPlay->Checked = false;
}
//---------------------------------------------------------------------------
