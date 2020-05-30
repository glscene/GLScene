//---------------------------------------------------------------------------

#include <vcl.h>
#include <math.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLGeomObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLCadencer"
#pragma link "GLVectorGeometry"
#pragma link "GLScene"
#pragma link "GLObjects"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject * Sender,
                                            const double deltaTime,
                                            const double newTime)
{
  // Make the blue sphere turn and ride a sin
  DCSphere->Turn(deltaTime * 30);
  Sphere->Position->Y = sin(DegToRad(newTime * 50)) * 3;

  // Make the arrow turn
  DCArrow->Turn(-deltaTime * 15);

  // Make the arrow point toward the sphere, using Y as up reference
  ArrowLine->PointTo(Sphere, YHmgVector);
}

//---------------------------------------------------------------------------

