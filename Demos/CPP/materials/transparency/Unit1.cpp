#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include <math.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLCadencer"
#pragma link "GLGeomObjects"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RBSTCClick(TObject *Sender)
{
   // we have 3 objects, move up twice and we're on the top !
   CentralSphere->MoveUp();
   CentralSphere->MoveUp();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RBTSCClick(TObject *Sender)
{
   // we have 3 objects, move down twice and we're on the top,
   // then once down, we're in the middle !
   CentralSphere->MoveUp();
   CentralSphere->MoveUp();
   CentralSphere->MoveDown();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RBTCSClick(TObject *Sender)
{
   // we have 3 objects, move down twice and we're on the bottom !
   CentralSphere->MoveDown();
   CentralSphere->MoveDown();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBAdditiveClick(TObject *Sender)
{
   // adjust blending mode for both orbiting spheres
   if (CBAdditive->Checked)
	  OrbitingSphere1->Material->BlendingMode = bmAdditive;
   else OrbitingSphere1->Material->BlendingMode = bmTransparency;
   OrbitingSphere2->Material->BlendingMode = OrbitingSphere1->Material->BlendingMode;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBSortingClick(TObject *Sender)
{
   // adjust sorting on the parent object
   if (CBSorting->Checked)
	  BaseDummyCube->ObjectsSorting = osRenderFarthestFirst;
   else BaseDummyCube->ObjectsSorting = osNone;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender,
	  const double deltaTime, const double newTime)
{
   double alpha;

   // move the spheres
   alpha = DegToRad(newTime*60);
   OrbitingSphere1->Position->SetPoint(1.5*cos(alpha), 1.5*sin(alpha), 1.5*sin(alpha));
   alpha = alpha+M_PI/2;
   OrbitingSphere2->Position->SetPoint(1.5*cos(alpha), 1.5*sin(alpha), 1.5*sin(alpha));
}
//---------------------------------------------------------------------------

