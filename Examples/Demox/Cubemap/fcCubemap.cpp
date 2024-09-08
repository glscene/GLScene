//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "fcCubemap.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GBE.Cubemap"
#pragma resource "*.fmx"
TFormCubemap *FormCubemap;
//---------------------------------------------------------------------------
__fastcall TFormCubemap::TFormCubemap(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormCubemap::FloatAnimation1Process(TObject *Sender)
{
  Camera1->RotationAngle->X = Camera1->RotationAngle->X + 0.1;
  Camera1->RotationAngle->Y = Camera1->RotationAngle->Y + 0.2;
}
//---------------------------------------------------------------------------

