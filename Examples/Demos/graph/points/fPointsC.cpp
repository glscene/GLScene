//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "fPointsC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormPoints *FormPoints;

const int
   cNbPoints = 200;

//---------------------------------------------------------------------------
__fastcall TFormPoints::TFormPoints(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormPoints::FormCreate(TObject *Sender)
{
   // allocate points in the 1st point set
   GLPoints1->Positions->Count = cNbPoints;
   // specify white color for the 1st point set
   // (if a single color is defined, all points will use it,
   // otherwise, it's a per-point coloring)
   GLPoints1->Colors->Add(clrWhite);
   // specify blue color for the 2nd point set
   GLPoints2->Colors->Add(clrBlue);
}
//---------------------------------------------------------------------------

void __fastcall TFormPoints::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
   int i;
   float f, a, ab, ca, sa;
   TGLAffineVectorList *p;
   TAffineVector *v;

   p = new TGLAffineVectorList;
   v = new TAffineVector;

   if (CBAnimate->Checked)
   {
	  // update the 1st point set with values from a math func
	  f = 1+Cos(newTime);
	  p = GLPoints1->Positions;
	  ab = newTime*0.1;
	  for (i=0; i < cNbPoints-1; i++) {
		 a = DegToRad((float)4*i)+ ab;
		 SinCos(a, sa, ca);
		 v->X = 2*ca;
		 v->Y = 2*Cos(f*a);
		 v->Z = 2*sa;
		 p->Items[i] = *v;
	  }
	  // replicate points in second set
	  GLPoints2->Positions = GLPoints1->Positions;
   }
   GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TFormPoints::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
 mx = X; my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TFormPoints::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
  if (Shift.Contains(ssLeft) || Shift.Contains(ssRight))
	{
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
	  mx =X;
	  my =Y;
  }
}
//---------------------------------------------------------------------------

void __fastcall TFormPoints::CBAnimateClick(TObject *Sender)
{
   GLPoints1->Static = !CBAnimate->Checked;
   GLPoints2->Static = !CBAnimate->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TFormPoints::CBPointParamsClick(TObject *Sender)
{
   GLPoints1->PointParameters->Enabled = CBPointParams->Checked;
   GLPoints2->PointParameters->Enabled = CBPointParams->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TFormPoints::Timer1Timer(TObject *Sender)
{
   LabelFPS->Caption = //Format("%.1f FPS", ARRAYOFCONST ((
	  GLSceneViewer1->FramesPerSecond(); //)))
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------

