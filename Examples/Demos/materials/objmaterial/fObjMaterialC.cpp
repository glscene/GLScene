//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fObjMaterialC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"
#pragma link "GLS.GeomObjects"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.Material"
#pragma link "GLS.Mesh"
#pragma resource "*.dfm"
TFormObjMaterial *FormObjMaterial;

//---------------------------------------------------------------------------
__fastcall TFormObjMaterial::TFormObjMaterial(TComponent* Owner)
	: TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TFormObjMaterial::FormCreate(TObject *Sender)
{
// Load vertices of convexhull
}

//---------------------------------------------------------------------------
void __fastcall TFormObjMaterial::GLCadencer1Progress(TObject *Sender, const double DeltaTime,
		  const double NewTime)
{
  if (chbRotation->Checked) {
	dcPolyhedra->Roll(DeltaTime*50);
	dcPolyhedra->Pitch(DeltaTime*50);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFormObjMaterial::Timer1Timer(TObject *Sender)
{
// dcPolyhedra->Turn(Timer1->Interval*50);
}
//---------------------------------------------------------------------------

void __fastcall TFormObjMaterial::rgPolyhedraClick(TObject *Sender)
{
  dcPolyTet->Visible = false;
  dcPolyOct->Visible = false;
  dcPolyHex->Visible = false;
  dcPolyIco->Visible = false;
  dcPolyDod->Visible = false;
  dcConvexhull->Visible = false;

   switch (rgPolyhedra->ItemIndex) {
	 case 0: {
			   dcPolyTet->Visible = true; break;
			 };
	 case 1: {
			   dcPolyOct->Visible = true; break;
			 };
	 case 2: {
			   dcPolyHex->Visible = true; break;
			 };
	 case 3: {
			   dcPolyIco->Visible = true; break;
			 };
	 case 4: {
			   dcPolyDod->Visible = true; break;
			 };
	 case 5: {
			   dcConvexhull->Visible = true; break;
			 };
   default:;
   }

}
//---------------------------------------------------------------------------

