//---------------------------------------------------------------------------

#include <vcl.h>
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
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBar1Change(TObject *Sender)
{
   Sphere1->Position->Z = (float)TrackBar1->Position/10;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
   CollisionManager1->CheckCollisions();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CollisionManager1Collision(TObject *Sender, TGLBaseSceneObject *object1,
          TGLBaseSceneObject *object2)
{
   ShowMessage("Collision between "+object1->Name+" and "+object2->Name);
}
//---------------------------------------------------------------------------
