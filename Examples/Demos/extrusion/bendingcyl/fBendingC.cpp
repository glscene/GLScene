//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#include <math.h>
#include <string.h>

#pragma hdrstop

#include "fBendingC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Extrusion"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"

#pragma resource "*.dfm"
TFormBending *FormBending;

//---------------------------------------------------------------------------
__fastcall TFormBending::TFormBending(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------


void __fastcall TFormBending::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
   Pipe1->Nodes->Items[2]->X = 1*sin(newTime*5); //used 5 instead of  M_PI/180 or cPIdiv180
   if (CBFat->Checked)
   {
	 Pipe1->Nodes->Items[2]->X = 1+cos(newTime*5);
	 Pipe1->Radius = 1;
   }
   else
   {
	 Pipe1->Nodes->RotateAroundZ(0);
	 Pipe1->Nodes->Items[2]->X = 1*sin(newTime*5);
	 Pipe1->Radius = 0.2;
   }
}
//---------------------------------------------------------------------------

void __fastcall TFormBending::CBSplineClick(TObject *Sender)
{
   if (CBSpline->Checked)
	  Pipe1->SplineMode = lsmCubicSpline;
   else
	  Pipe1->SplineMode = lsmLines;
}
//---------------------------------------------------------------------------
void __fastcall TFormBending::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
 my = X; my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TFormBending::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
  if (Shift.Contains(ssLeft))
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
   mx = X;  my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TFormBending::Timer1Timer(TObject *Sender)
{
/*
  PanelFPS->Caption = Format("%d Triangles, %.1f FPS",
	 ARRAYOFCONST((Pipe1->TriangleCount, GLSceneViewer1->FramesPerSecond())));
*/
  String s1 = Pipe1->TriangleCount;
  String s2 = GLSceneViewer1->FramesPerSecond();
  PanelFPS->Caption = s1 + " " + s2;
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------


