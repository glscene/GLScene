//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCollision"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSpaceText"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFile3DS"

#pragma resource "*.dfm"
TForm1 *Form1;

//  const int cbmFaces = 5;

  const String
  StringNames[] = {"Point","Sphere","Ellipsoid","Cube","Faces"};

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
   int i;
   SetGLSceneMediaDir();
   TeaPot1->LoadFromFile("TeaPot.3ds");
   TeaPot1->BuildOctree();

   TeaPot2->LoadFromFile("TeaPot.3ds");
   TeaPot2->BuildOctree();

//   rgObjectsClick(nil);

   //Fill StringGrid1 with current state of collisions
   for (i=0; i < cbmFaces; i++)
   {
	 StringGrid1->Cells[0][i+1] = StringNames[i];
	 StringGrid1->Cells[i+1][0] = StringNames[i];
   }
   //point
   StringGrid1->Cells[1][1] = "complete";      //Point-Point
   StringGrid1->Cells[1][2] = "complete";      //Sphere-Point
   StringGrid1->Cells[1][3] = "complete";      //Ellipsoid-Point
   StringGrid1->Cells[1][4] = "complete";      //Cube-Point
   StringGrid1->Cells[1][5] = "Cube-Point";    //Faces-Point
   //sphere
   StringGrid1->Cells[2][1] = "complete";      //Point-Sphere
   StringGrid1->Cells[2][2] = "complete";      //Sphere-Sphere
   StringGrid1->Cells[2][3] = "complete";      //Ellipsoid-Sphere
   StringGrid1->Cells[2][4] = "complete";      //Cube-Sphere
   StringGrid1->Cells[2][5] = "Cube-Sphere";   //Faces-Sphere
   //ellipsoid
   StringGrid1->Cells[3][1] ="complete";      //Point-Ellipsoid
   StringGrid1->Cells[3][2] ="complete";      //Sphere-Ellipsoid
   StringGrid1->Cells[3][3] ="incorrect";     //Ellipsoid-Ellipsoid
   StringGrid1->Cells[3][4] ="Cube-Sphere";   //Cube-Ellipsoid
   StringGrid1->Cells[3][5] ="Cube-Ellipsoid";//Faces-Ellipsoid
   //cube
   StringGrid1->Cells[4][1] ="complete";      //Point-Cube
   StringGrid1->Cells[4][2] ="complete";      //Sphere-Cube
   StringGrid1->Cells[4][3] ="Sphere-Cube";   //Ellipsoid-Cube
   StringGrid1->Cells[4][4] ="complete";      //Cube-Cube
   StringGrid1->Cells[4][5] ="experimental";  //Faces-Cube
   //Faces
   StringGrid1->Cells[5][1] ="Point-Cube";    //Point-Faces
   StringGrid1->Cells[5][2] ="Sphere-Cube";   //Sphere-Faces
   StringGrid1->Cells[5][3] ="Ellipsoid-Cube";//Ellipsoid-Faces
   StringGrid1->Cells[5][4] ="experimental";  //Cube-Faces
   StringGrid1->Cells[5][5] ="complete";      //Faces-Faces
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cbCollisionModeClick(TObject *Sender)
{
   ((TGLBCollision *)(TeaPot1->Behaviours->Items[0]))->BoundingMode =
	 ((TCollisionBoundingMode)(cbCollisionMode->ItemIndex));
   ((TGLBCollision *)(TeaPot2->Behaviours->Items[0]))->BoundingMode =
	 ((TCollisionBoundingMode)(cbCollisionMode->ItemIndex));
   ((TGLBCollision *)(Bar->Behaviours->Items[0]))->BoundingMode = cbmCube;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormShow(TObject *Sender)
{
   //initialize
   CurrSO = TeaPot1;
   cbCollisionModeClick(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  const TColor
	cColor[false][true] = {clLime, clRed};

   __int64 t;

   Timer1->Enabled = false;
   CollisionDetected = false;

   t = StartPrecisionTimer();

   Memo1->Lines->Clear();
   Memo1->Lines->BeginUpdate();
   CollisionManager1->CheckCollisions();
   Memo1->Lines->EndUpdate();

   LATime->Caption = Format("%.1f ms", ARRAYOFCONST ((StopPrecisionTimer(t)*1000)));

   Shape1->Brush->Color = *cColor[CollisionDetected];
   Timer1->Enabled = true;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::CollisionManager1Collision(TObject *Sender, TGLBaseSceneObject *object1,
          TGLBaseSceneObject *object2)
{
   if (Sender=CollisionManager1)
   {
   CollisionDetected = true;
	 Memo1->Lines->Add(object1->Name+
	 /*"("+StringNames[((TGLBCollision*)(object1->Behaviours->GetByClass(TGLBCollision))->BoundingMode)]+")"*/ +
		"  -  "+object2->Name
	 /*+"("+StringNames[((TGLBCollision*)(object2->Behaviours->GetByClass(TGLBCollision))->BoundingMode)]+")"*/);
   }
   else
	 Memo1->Lines->Add(object1->Name+
	 /*"("+StringNames[((TGLBCollision*)(object1->Behaviours->GetByClass(TGLBCollision))->BoundingMode)]+")"*/ +
		"  -  "+object2->Name
	 /*+"("+StringNames[((TGLBCollision*)(object2->Behaviours->GetByClass(TGLBCollision))->BoundingMode)]+") ** BB collision **"*/);
   ;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
   TGLCustomSceneObject *pick;

   pick = (TGLCustomSceneObject *) GLSceneViewer1->Buffer->GetPickedObject(X, Y) ;
   if (pick)
	  CurrSO = pick;
   // store mouse coordinates when a button went down
   mdx = X;
   mdy = Y;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
   int dx, dy;
   Glvectorgeometry::TVector VX, VY;
   TGLCamera *Camera;

   Camera = GLSceneViewer1->Camera;
   // calculate delta since last move or last mousedown
   dx = mdx - X;
   dy = mdy - Y;
   mdx = X;
   mdy = Y;
   if (Shift.Contains(ssLeft))
   {
	  if (Shift.Contains(ssShift))
		// left button with shift rotates the object
		// (rotation happens around camera's axis)
		Camera->RotateObject(CurrSO, dy, dx);
	  else
		// left button without shift changes camera angle
		// (we're moving around the parent and target dummycube)
		Camera->MoveAroundTarget(dy, dx);
   }
   else
   if (Shift.Contains(ssRight))
   {
	  //Moving the objects
	  //Description:
	  //1. via VectorPerpendicular we create a vector that is 90° to camera view and points to Y (Up)
	  //   this is Y-direction of moving
	  //2. now using VectorCrossProduct we create the vector that is 90° to camera view and to the other
	  //   vector (VY), this is X-direction of moving
	  VY = VectorMake(VectorPerpendicular(YVector, VectorNormalize(GLCamera2->Position->AsAffineVector)));
	  VX = VectorCrossProduct(VY, VectorNormalize(GLCamera2->Position->AsVector));
	  NormalizeVector(VY);
	  NormalizeVector(VX);
	  CurrSO->Position->Translate(VectorCombine(VX, VY,
		 -dx * 0.132 * Camera->DistanceToTarget() / Camera->FocalLength,
		  dy * 0.132 * Camera->DistanceToTarget() / Camera->FocalLength));
   }
}
//---------------------------------------------------------------------------
