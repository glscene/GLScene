//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#include <stdlib.h>
#include <GLKeyboard.hpp>
#pragma hdrstop

#include "Unit1.h"
#include "GLFPSMovement.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLGeomObjects"
#pragma link "GLNavigator"
#pragma link "GLCadencer"
#pragma link "GLWin32Viewer"
#pragma link "GLObjects"
#pragma link "GLVectorFileObjects"
#pragma link "GLScene"
#pragma link "GLFile3DS"
#pragma link "GLVectorGeometry"
#pragma link "GLKeyboard"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma resource "*.dfm"
TForm1 *Form1;

float random()
{
  return (float)(rand() & 0x1FFF) / (float)0x1FFF;
}

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  SetGLSceneMediaDir();
  FreeForm1->LoadFromFile("BoxedIn.3ds");

  FreeForm1->BuildOctree(3);
  Label1->Caption =
	"Octree Nodes    : " + IntToStr(FreeForm1->Octree->NodeCount);
  Label2->Caption =
	"Tri Count Octree: " + IntToStr(FreeForm1->Octree->TriCountOctree);
  Label3->Caption =
	"Tri Count Mesh  : " + IntToStr(FreeForm1->Octree->TriCountMesh);

  Lines1->AddNode(0, 0, 0);
  Lines1->ObjectStyle = Lines1->ObjectStyle << osDirectDraw;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject * Sender,
											const double deltaTime,
											const double newTime)
{
  Glvectorgeometry::TVector rayStart, rayVector;
  float velocity;
  Glvectorgeometry::TVector pPoint;
  Glvectorgeometry::TVector pNormal;
  __int64 t;

  if(IsKeyDown(VK_ESCAPE))
	Close();

  velocity = TrackBar1->Position * deltaTime * 50;

  t = StartPrecisionTimer();

  SetVector(rayStart, Sphere2->AbsolutePosition);
  SetVector(rayVector, Sphere2->AbsoluteDirection);
  NormalizeVector(rayVector);
  //Note: since collision may be performed on multiple meshes, we might need to know which hit
  //      is closest (ie: d=raystart - pPoint)->
  if(FreeForm1->
	 OctreeSphereSweepIntersect(rayStart, rayVector, velocity, Sphere2->Radius,
								&pPoint, &pNormal))
  {
	// Show the polygon intersection point
	NormalizeVector(pNormal);
	Sphere1->Position->AsVector = pPoint;
	Sphere1->Direction->AsVector = pNormal;

	// Make it rebound->->->
	Sphere2->Direction->AsAffineVector =
	  VectorReflect(Sphere2->Direction->AsAffineVector,
                    AffineVectorMake(pNormal));
    // Add some "english"->->->
	Sphere2->Direction->X = Sphere2->Direction->X + random() / 10;
	Sphere2->Direction->Y = Sphere2->Direction->Y + random() / 10;
    Sphere2->Direction->Z = Sphere2->Direction->Z + random() / 10;

    // Add intersect point to trail
    AddToTrail(pPoint);
  }
  else
  {
    Sphere2->Move(velocity);    //No collision, so just move the ball->
  }
  // Last trail point is always the sphere's current position
  Lines1->Nodes->Last()->AsVector = Sphere2->Position->AsVector;

  colTotalTime = colTotalTime + StopPrecisionTimer(t);
  colCount++;
}

//---------------------------------------------------------------------------

void TForm1::AddToTrail(const Glvectorgeometry::TVector & p)
{
  int i, k;
  Lines1->Nodes->Last()->AsVector = p;
  Lines1->AddNode(0, 0, 0);
  if(Lines1->Nodes->Count > 20) // limit trail to 20 points
	Lines1->Nodes->Delete(0);

  for(i = 0; i <= 19; i++)
  {
	k = Lines1->Nodes->Count - i - 1;
	if(k >= 0)
	  ((TGLLinesNode *) Lines1->Nodes->Items[k])->Color->Alpha =
		0.95 - i * 0.05;
  }
}
void __fastcall TForm1::Timer1Timer(TObject * Sender)
{
  float t;
  String s;

  if(colCount > 0)
	t = colTotalTime * 1000 / colCount;
  else
	t = 0;
	LabelFPS->Caption = Format("%.2f FPS for collisions/frame",
	  ARRAYOFCONST((GLSceneViewer2->FramesPerSecond())));
	GLSceneViewer2->ResetPerformanceMonitor();
	colTotalTime = 0;
	colCount = 0;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Button1Click(TObject * Sender)
{
//if(the ball gets stuck in a pattern, hit the reset button->

  Sphere2->Position->X = random();
  Sphere2->Position->Y = random();
  Sphere2->Position->Z = random();

  Sphere2->Direction->X = random();
  if(random() > 0.5)
    Sphere2->Direction->X = -Sphere2->Direction->X;
  Sphere2->Direction->Y = random();
  if(random() > 0.5)
    Sphere2->Direction->Y = -Sphere2->Direction->Y;
  Sphere2->Direction->Z = random();
  if(random() > 0.5)
	Sphere2->Direction->Z = -Sphere2->Direction->Z;

}

//---------------------------------------------------------------------------

