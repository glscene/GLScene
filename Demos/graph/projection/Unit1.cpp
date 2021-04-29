//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma link "GLS.GeomObjects"
#pragma link "GLS.Graph"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.BaseClasses"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
   int i;
   // generate a bunch of random points
   for (i=1; i < 1000; i++)
	  GLPoints->Positions->Add((float)(Random()-0.5)*5,
			  (float)(Random()-0.5)*5, (float)(Random()-0.5)*5);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DirectOpenGLRender(TObject *Sender, TGLRenderContextInfo &rci)

{
   int i;
   TGLMatrix mat;
   TGLVector p, pProj;
   TGLVector planePoint, planeNormal;
   THmgPlane plane;

   // Here we recover our plane point and normal...
   planePoint = GLPlane->Position->AsVector;
   planeNormal = GLPlane->Direction->AsVector;
   // ...which we use to create a plane (equation)
   plane = PlaneMake(planePoint, planeNormal);
   // from that plane equation and our pojection direction
   // (which is here the plane normal)
   mat = MakeParallelProjectionMatrix(plane, planeNormal);

   // save state, turn off lighting and specify the lines color
   rci.GLStates->Disable(stLighting);
   glColor3f(1, 1, 0);

   // we'll be drawing a bunch of lines, to specify a line in OpenGL,
   // you only need to specify the line start and end vertices
   glBegin(GL_LINES);
	  for (i=0; i < GLPoints->Positions->Count-1; i++) {
		 // read the point coordinates, directly from the TGLPoints list
		 MakePoint(p, GLPoints->Positions->Items[i]);
		 // project this point on the plane with the matrix
		 pProj = VectorTransform(p, mat);
		 // specify the two vertices
		 glVertex3fv(p.V);
		 glVertex3fv(pProj.V);
	  }
   glEnd();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SceneViewerMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
   mx = X; my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SceneViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y)
{
   if (Shift.Contains(ssLeft))
	  GLCamera->MoveAroundTarget(my-Y, mx-X);
   else if (Shift.Contains(ssRight))
	  GLCamera->RotateObject(GLPlane, my-Y, mx-X);
   mx = X; my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
   GLPlane->Position->Y = GLPlane->Position->Y+WheelDelta*0.001;
}
//---------------------------------------------------------------------------
