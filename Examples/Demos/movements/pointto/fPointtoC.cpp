//---------------------------------------------------------------------------

#include <vcl.h>
#include <math.h>
#pragma hdrstop

#include "fPointtoC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.GeomObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.Cadencer"
#pragma link "Stage.VectorGeometry"
#pragma link "GLS.Scene"
#pragma link "GLS.Objects"
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma resource "*.dfm"
TFormPointto *FormPointto;
//---------------------------------------------------------------------------
__fastcall TFormPointto::TFormPointto(TComponent * Owner):TForm(Owner)
{
}

//---------------------------------------------------------------------------

void __fastcall TFormPointto::GLCadencer1Progress(TObject * Sender,
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

