//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
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
void __fastcall TForm1::BUCastClick(TObject *Sender)
{
   Glvectorgeometry::TVector o, v, vLight, light, iPoint, iNormal;
   Glvectorgeometry::TVector up, right, dir;
   int x, y, dx, dy;
   float f, d;
   TColor color;
   TGLBaseSceneObject *iObj;
   __int64 t;

   Screen->Cursor = crHourGlass;
   t = StartPrecisionTimer();

   // First we extract/prepare the vector we will use during our raycasting
   // the origin is the camera position, and factor was grossly adjusted so
   // that both view look grossly similar
   MakePoint (o,   GLCamera1->AbsolutePosition);
   MakeVector(dir, GLCamera1->AbsoluteDirection);
   MakeVector(up,  GLCamera1->AbsoluteUp);
   MakePoint(light, GLLightSource1->AbsolutePosition);
   right = VectorCrossProduct(dir, up);
   f = (float)1/300;
   dx = (PaintBox1->Width / 2);
   dy = (PaintBox1->Height / 2);

   // Cover a square area
   for (y=0; y < PaintBox1->Height-1; y++)
	  for (x=0; x < PaintBox1->Width-1; x++)
   {
		 // Calculate our ray vector for current pixel
		 v = VectorCombine3(dir, right, up, 1, (x-dx)*f, (dy-y)*f);
		 // ray vectors must be of unit length!
		 NormalizeVector(v);

		 // ray cast
		 iObj = GLScene1->RayCastIntersect(o, v, &iPoint, &iNormal);
		 if (iObj)
		 {
			// if something found, calculate vector to light source
			vLight = VectorSubtract(light, iPoint);
			NormalizeVector(vLight);
			// color is given by the normal/lightsource vectors dot-product
			// and this intensity is composited with the object's diffuse color
			NormalizeVector(iNormal);
			d = VectorDotProduct(iNormal, vLight);
			if (d<0) d=0;

		  color =
		  ConvertColorVector(((TGLCustomSceneObject *) iObj)->Material->FrontProperties->Diffuse->Color, d);
		 }
		 else
		   color = clGray;

		 // plot our point
		 PaintBox1->Canvas->Pixels[x][y] = color;
   }

   Caption = Format("RayCast in %.1f ms",
	 ARRAYOFCONST ((StopPrecisionTimer(t)*1000)));
   Screen->Cursor = crDefault;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
   DummyCube1->TurnAngle = newTime*50;
}
//---------------------------------------------------------------------------
