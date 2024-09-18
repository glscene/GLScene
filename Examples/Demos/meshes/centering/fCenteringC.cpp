//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fCenteringC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.File3DS"
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
   const String cFileName = "polyhedron.3ds";
   TFileName Path = GetCurrentAssetPath();
   SetCurrentDir(Path  + "\\model");

   // left one
   FreeForm3->AutoCentering = FreeForm3->AutoCentering << macCenterX, macCenterZ;
   FreeForm3->LoadFromFile(cFileName);
   // central one
   FreeForm2->AutoCentering = FreeForm2->AutoCentering << macCenterY;
   FreeForm2->LoadFromFile(cFileName);
   // right one
   FreeForm1->AutoCentering = FreeForm1->AutoCentering << macCenterX, macCenterY, macCenterZ;
   FreeForm1->LoadFromFile(cFileName);

}
//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBar1Change(TObject *Sender)
{
   DCCamera->PitchAngle = TrackBar1->Position;
}
//---------------------------------------------------------------------------
