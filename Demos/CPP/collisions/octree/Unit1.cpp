//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLGeomObjects"
#pragma link "GLCadencer"
#pragma link "GLWin32Viewer"
#pragma link "GLObjects"
#pragma link "GLVectorFileObjects"
#pragma link "GLScene"
#pragma link "GLFile3DS"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  __int64 t;
  SetGLSceneMediaDir();
  // Load high poly mesh (10,000 triangles).
  FreeForm1->LoadFromFile("HighPolyObject.3ds");

  t = StartPrecisionTimer();

  FreeForm1->BuildOctree(3);

  LABuild->Caption =
	Format("Build time: %.3f ms", ARRAYOFCONST((StopPrecisionTimer(t) * 1000)));

  Label1->Caption = "Octree Nodes: " + IntToStr(FreeForm1->Octree->NodeCount);
  Label2->Caption =
	"Tri Count Octree: " + IntToStr(FreeForm1->Octree->TriCountOctree);
  Label3->Caption =
	"Tri Count Mesh: " + IntToStr(FreeForm1->Octree->TriCountMesh);

  mousex = -1;
  mousey = -1;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer2MouseDown(TObject * Sender,
												TMouseButton Button,
												TShiftState Shift, int X, int Y)
{
  Glvectorgeometry::TVector rayStart, rayVector, iPoint, iNormal;
  __int64 t;

  SetVector(rayStart, GLCamera2->AbsolutePosition);
  SetVector(rayVector,
			GLSceneViewer2->Buffer->
			ScreenToVector(AffineVectorMake(X, GLSceneViewer2->Height - Y, 0)),0);
  NormalizeVector(rayVector);

  t = StartPrecisionTimer();
  if(CBOctree->Checked)
  {
	// Octree method (fast)
	if(FreeForm1->
	   OctreeRayCastIntersect(rayStart, rayVector, &iPoint, &iNormal))
	{
	  Sphere1->Visible = True;
	  Sphere1->Position->AsVector = iPoint;
	  Sphere1->Direction->AsVector = VectorNormalize(iNormal);
	}
	else
	  Sphere1->Visible = False;
    Label4->Hint =
      "# Nodes hit with raycast: " +
      IntToStr(FreeForm1->Octree->ResultArray.Length);
  }
  else
  {
    // Brute-Force method (slow)
    if(FreeForm1->RayCastIntersect(rayStart, rayVector, &iPoint, &iNormal))
    {
      Sphere1->Visible = True;
	  Sphere1->Position->AsVector = iPoint;
      Sphere1->Direction->AsVector = VectorNormalize(iNormal);
    }
    else
      Sphere1->Visible = False;
  }
  Label5->Hint =
    Format("Intersect Time: %.3f ms",
           ARRAYOFCONST((StopPrecisionTimer(t) * 1000)));
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer2MouseMove(TObject * Sender,
                                                TShiftState Shift, int X, int Y)
{
  mousex = X;
  mousey = Y;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject * Sender,
                                            const double deltaTime,
                                            const double newTime)
{
  TShiftState ss;
  ss << ssShift;
  if(CheckBox1->Checked)
    GLSceneViewer2MouseDown(Sender, Controls::mbLeft, ss, mousex, mousey);

  FreeForm1->RollAngle = 5.0 * newTime;   // 45° per second
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject * Sender)
{
// Show FPS Rating
  LabelFPS->Caption =
    Format("%.2f FPS", ARRAYOFCONST((GLSceneViewer2->FramesPerSecond())));
  GLSceneViewer2->ResetPerformanceMonitor();
  // Not doing so causes ugly flickering and a significant decrease in FPS...
  Label4->Caption = Label4->Hint;
  Label5->Caption = Label5->Hint;
}

//---------------------------------------------------------------------------

