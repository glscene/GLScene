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
#pragma link "GLExtrusion"
#pragma link "GLFireFX"
#pragma link "GLGeomObjects"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"

TForm1 *Form1;
int mx,my;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormResize(TObject *Sender)
{
   GLCamera1->FocalLength = Height/3;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
  mx = X; my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBar1Change(TObject *Sender)
{
  GLFireFXManager1->FireDir->Z = -TrackBar1->Position*0.1;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
  if (Shift.Contains(ssLeft) || Shift.Contains(ssRight)) // if (Shift <> [])
   {
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
	  GLCadencer1->Progress();
	  mx = X; my = Y;
   }

}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   int n;

   Caption = "GLScene Candles - "+ Format("%.1f FPS", ARRAYOFCONST((GLSceneViewer1->FramesPerSecond())));
   GLSceneViewer1->ResetPerformanceMonitor();
   if (TrackBar1->Position==0)
	  GLFireFXManager1->Disabled = False;
   else {
	  n = abs(TrackBar1->Position)-15;
	  if (n>0)
		 if (Random()/n<0.15) GLFireFXManager1->Disabled = True;
   }

}
//---------------------------------------------------------------------------

