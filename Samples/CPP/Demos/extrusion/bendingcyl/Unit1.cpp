//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#include <math.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLExtrusion"
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


void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
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
//	 Pipe1->Nodes->RotateAroundZ(0);
	 Pipe1->Nodes->Items[2]->X = 1*sin(newTime*5);
	 Pipe1->Radius = 0.2;
   }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CBSplineClick(TObject *Sender)
{
   if (CBSpline->Checked)
	  Pipe1->SplineMode = lsmCubicSpline;
   else
	  Pipe1->SplineMode = lsmLines;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
 my = X; my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
  if (Shift.Contains(ssLeft))
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
   mx = X;  my = Y;
}
//---------------------------------------------------------------------------


void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  PanelFPS->Caption = Format("%d Triangles, %.1f FPS",
	 ARRAYOFCONST((Pipe1->TriangleCount, GLSceneViewer1->FramesPerSecond())));
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------


