//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}

const int
  cSize = 5;

//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  int x, y, z;
  TGLCube *cube;
  float factor, cubeSize;

  // bench only creation and 1st render (with lists builds, etc...)
  factor = 70 / (cSize * 2 + 1);
  cubeSize = 0.4 * factor;
  for (x = -cSize; x< cSize; x++)
	for (y = -cSize; y< cSize; y++)
	  for (z = -cSize; z< cSize; z++)
	  {
		cube = (TGLCube *) (DummyCube1->AddNewChild(__classid(TGLCube)));
		cube->Position->AsVector = PointMake(factor * x, factor * y, factor * z);
		cube->CubeWidth = cubeSize;
		cube->CubeHeight = cubeSize;
		cube->CubeDepth = cubeSize;
		cube->Material->FrontProperties->Diffuse->Color =
  		  VectorLerp(clrYellow, clrRed, (float)(x * x + y * y + z * z)/(cSize * cSize * 3));

	  // uncomment following lines to stress OpenGL with more color changes calls

	   //cube->Material->FrontProperties->Ambient->Color=VectorLerp(clrYellow, clrRed, (x*x+y*y+z*z)/(cSize*cSize*3));
	   //cube->Material->FrontProperties->Emission->Color=VectorLerp(clrYellow, clrRed, (x*x+y*y+z*z)/(cSize*cSize*3));
	   //cube->Material->FrontProperties->Specular->Color=VectorLerp(clrYellow, clrRed, (x*x+y*y+z*z)/(cSize*cSize*3));
	  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  DummyCube1->TurnAngle = 90 * newTime; // 90° per second
}
//---------------------------------------------------------------------------
