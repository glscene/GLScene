//---------------------------------------------------------------------------

#include <fmx.h>
#include "stdlib.h"
#pragma hdrstop

#include "fcGrass.h"
#include "GBE.Grass.hpp"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TFormGrass *FormGrass;
//---------------------------------------------------------------------------
__fastcall TFormGrass::TFormGrass(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormGrass::FormCreate(TObject *Sender)
{
  int i;
  TGBEGrass *GBEGrass;
  Randomize();
  for (i = 0; i <= 500; i++)
  {
      GBEGrass = new TGBEGrass (this);
      GBEGrass->Position->X = Random(40)-20;
      GBEGrass->Position->Z = Random(40)-20;
      GBEGrass->RotationAngle->Y = Random(360);
      if (i / 10 == 0)
        GBEGrass->MaterialSource = TextureMaterialSource2;
      else
      {
        if (i / 2 == 0)
          GBEGrass->MaterialSource = TextureMaterialSource;
        else
          GBEGrass->MaterialSource = TextureMaterialSource1;
      }
      GBEGrass->Width = 5;
      GBEGrass->Height = 5;
      GBEGrass->Depth = 0;
      GBEGrass->Parent = Dummy;
      GBEGrass->Temps = 0.1;
  }
}
//---------------------------------------------------------------------------
