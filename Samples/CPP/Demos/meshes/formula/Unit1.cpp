//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMesh"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;

const int
   // half-grid resolution, grid width is actually cResolution*2 of "quads"
   cResolution = 50;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------


TAffineVector __fastcall TForm1::MakeVect(const float aX, const float aY)
{
  TAffineVector Result;
  SetVector(Result, aX*invRes1, sin((aX*aX+aY*aY)*invRes2), aY*invRes1);
  return Result;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::AddTriangle(const TAffineVector p1,
									const TAffineVector p2,
									const TAffineVector p3,
									const TColorVector color)
{
  Mesh1->Vertices->AddVertex(p1, NullVector, color);
  Mesh1->Vertices->AddVertex(p2, NullVector, color);
  Mesh1->Vertices->AddVertex(p3, NullVector, color);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
   int x, y;
   TAffineVector pTopLeft;
   TAffineVector pTopRight;
   TAffineVector pBottomRight;
   TAffineVector pBottomLeft;

   // scaling precalcs for our math func
   invRes1 = (float) 10/cResolution;
   invRes2 = 0.1*sqrt(invRes1);
   //
   // Triangles
   //
   // this one is basic : we calculate the corner points for each grid quad and
   // add the two triangles that make it
   Mesh1->Mode = mmTriangles;
   Mesh1->Vertices->Clear();
   for (y = -cResolution; y < cResolution; y++)
	 for (x = -cResolution; x < cResolution; x++)
	 {
			pTopLeft = MakeVect(x, y+1);
			pTopRight = MakeVect(x+1, y+1);
			pBottomRight = MakeVect(x+1, y);
			pBottomLeft = MakeVect(x, y);
			// top left triangle
			AddTriangle(pBottomLeft, pTopLeft, pTopRight, clrBlue);
			// bottom right triangle
			AddTriangle(pTopRight, pBottomRight, pBottomLeft, clrBlue);
   }
   Mesh1->CalcNormals(fwCounterClockWise);
//      Vertices.Locked:=True;

   //
   // TriangleStrip
   //
   // Same as triangle, however trianglestrips are continuous, and to cover
   // the grid, "null" segments are used at both ends of a strip (to avoid a
   // visible triangle that would stretch for the full width of the grid).
   // Note : this can be avoided by reversing grid traversing direction (one line
   // from left to right, one from right to left, etc.)
   Mesh2->Mode = mmTriangleStrip;
   Mesh2->Vertices->Clear();
   for (y = -cResolution; y < cResolution; y++)
   {
	 pTopLeft = MakeVect(-cResolution, y+1);
	 Mesh2->Vertices->AddVertex(pTopLeft, NullVector, clrBlue);
	 Mesh2->Vertices->AddVertex(pTopLeft, NullVector, clrBlue);
	 for (x = -cResolution; x < cResolution; x++)
	 {
		pTopRight = MakeVect(x+1, y+1);
		pBottomLeft = MakeVect(x, y);
		Mesh2->Vertices->AddVertex(pBottomLeft, NullVector, clrBlue);
		Mesh2->Vertices->AddVertex(pTopRight, NullVector, clrBlue);
	 }
	 pBottomRight = MakeVect(cResolution+1, y);
	 Mesh2->Vertices->AddVertex(pBottomRight, NullVector, clrBlue);
	 Mesh2->Vertices->AddVertex(pBottomRight, NullVector, clrBlue);
	 Mesh2->CalcNormals(fwClockWise);
//      Vertices.Locked:=True;
   }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   // nb of triangles in scene
   Caption = "Formula " + Format("%d Triangles",
	  ARRAYOFCONST ((2*(cResolution*2)*(cResolution*2))));
   // calculate & display triangles framerate
   // we render twice to get a fair FPS rating
   GLSceneViewer1->ResetPerformanceMonitor();
   GLSceneViewer1->Buffer->Render();
   GLSceneViewer1->Buffer->Render();
   Label1->Caption = Format("%.2f FPS (mmTriangles)",
	  ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond())));

   // calculate & display trianglestrip framerate
   // we render twice to get a fair FPS rating
   GLSceneViewer2->ResetPerformanceMonitor();
   GLSceneViewer2->Buffer->Render();
   GLSceneViewer2->Buffer->Render();
   Label2->Caption = Format("%.2f FPS (mmTriangleStrip)",
      ARRAYOFCONST ((GLSceneViewer2->FramesPerSecond())));
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
   mx = X; my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
   if (Shift.Contains(ssLeft))
   {
	 GLSceneViewer1->Camera->MoveAroundTarget(my-Y, mx-X);
	 my = Y; mx = X;
   }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer2MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
   mx = X; my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer2MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
   if (Shift.Contains(ssLeft))
   {
	 GLSceneViewer2->Camera->MoveAroundTarget(my-Y, mx-X);
	 my = Y; mx = X;
   }
}
//---------------------------------------------------------------------------

