//---------------------------------------------------------------------------

#include <vcl.h>

#pragma hdrstop

#include "fCandlesC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Extrusion"
#pragma link "GLS.FireFX"
#pragma link "GLS.GeomObjects"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"

TFormCandles *FormCandles;
int mx,my;
//---------------------------------------------------------------------------
__fastcall TFormCandles::TFormCandles(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormCandles::FormResize(TObject *Sender)
{
   GLCamera1->FocalLength = Height/3;
}
//---------------------------------------------------------------------------
void __fastcall TFormCandles::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
  mx = X; my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TFormCandles::TrackBar1Change(TObject *Sender)
{
  GLFireFXManager1->FireDir->Z = -TrackBar1->Position*0.1;
}
//---------------------------------------------------------------------------
void __fastcall TFormCandles::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
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
void __fastcall TFormCandles::Timer1Timer(TObject *Sender)
{
   int n;

   Caption = "GLS.Scene Candles - "+ Format("%.1f FPS", ARRAYOFCONST((GLSceneViewer1->FramesPerSecond())));
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

