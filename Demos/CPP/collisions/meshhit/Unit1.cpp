//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFile3DS"

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
   // Load mushroom mesh
   SetGLSceneMediaDir();
   FreeForm1->LoadFromFile("mushroom.3ds");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
   Glvectorgeometry::TVector rayStart, rayVector, iPoint, iNormal;
   // retrieve raycasting data:
   //    rayStart is obtained for camera and screen position
   //    rayVector is the camera direction (i.e direction to target since our camera is targeted)
   // (note that (0, 0) is lower left for the Screen function, whereas Delphi
   //  uses top-left as origin, hence the Y inversion)
   SetVector(rayStart, GLSceneViewer1->Buffer->OrthoScreenToWorld(X, GLSceneViewer1->Height-Y));
   SetVector(rayVector, GLCamera1->AbsoluteVectorToTarget());
   NormalizeVector(rayVector);
   // Here we require RayCast intersection
   if (FreeForm1->RayCastIntersect(rayStart, rayVector, &iPoint, &iNormal))
	 {
	  // got one, move the sphere there and orient it appropriately
	  Sphere1->Position->AsVector = iPoint;
	  Sphere1->Direction->AsVector = VectorNormalize(iNormal);
	  // make it visible
	  Sphere1->Visible = True;
	 }
   else
	  // hide it if we did not hit
	  Sphere1->Visible = False;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
   // when mouse moves, recompute intersection
   if (Shift.Contains(ssLeft)||Shift.Contains(ssRight))
	GLSceneViewer1MouseDown(Sender, TMouseButton(Vcl::Controls::mbLeft), Shift, X, Y);

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer2MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
   Glvectorgeometry::TVector rayStart, rayVector, iPoint, iNormal;
   // retrieve raycasting data:
   //    rayStart is the eye (camera) position
   //    rayVector is computed from screen position
   // (note that (0, 0) is lower left for the Screen function, whereas Delphi
   //  uses top-left as origin, hence the Y inversion)
   SetVector(rayStart, GLCamera2->AbsolutePosition);
   SetVector(rayVector, GLSceneViewer2->Buffer->ScreenToVector(AffineVectorMake(X, GLSceneViewer2->Height-Y, 0)));
   NormalizeVector(rayVector);
   // Here we request RayCast intersection
   if (FreeForm1->RayCastIntersect(rayStart, rayVector, &iPoint, &iNormal))
	 {
	  // got one, move the sphere there and orient it appropriately
	  Sphere1->Position->AsVector = iPoint;
	  Sphere1->Direction->AsVector = VectorNormalize(iNormal);
	  // make it visible
	  Sphere1->Visible = True;
	 }
   else
	  // hide it if we did not hit
	  Sphere1->Visible = False;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer2MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
   // when mouse moves, recompute intersection
   if (Shift.Contains(ssLeft)||Shift.Contains(ssRight))
	GLSceneViewer2MouseDown(Sender, TMouseButton(Vcl::Controls::mbLeft), Shift, X, Y);


}
//---------------------------------------------------------------------------
