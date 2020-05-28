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
   // the "sun" turns slowly around Y axis
   Cube1->TurnAngle = t/4;
   // "earth" rotates around the sun on the Y axis
   Cube2->Position->X =3*cos(DegToRad((float)t));
   Cube2->Position->Z =3*sin(DegToRad((float)t));
   // "moon" rotates around earth on the X axis
   Cube3->Position->X = Cube2->Position->X;
   Cube3->Position->Y = Cube2->Position->Y+1*cos(DegToRad((float)3*t));
   Cube3->Position->Z = Cube2->Position->Z+1*sin(DegToRad((float)3*t));
   // update FPS count
   StaticText1->Caption = IntToStr(Trunc(GLSceneViewer1->FramesPerSecond()))+" FPS";
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
