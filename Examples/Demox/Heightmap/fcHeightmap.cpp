//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "fcHeightmap.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GBE.Heightmap"
#pragma resource "*.fmx"
TFormHeightmap *FormHeightmap;
//---------------------------------------------------------------------------
__fastcall TFormHeightmap::TFormHeightmap(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormHeightmap::FormCreate(TObject *Sender)
{
  TMemoryStream *Stream;
  Stream = new TMemoryStream();
  Image1->Bitmap->SaveToStream(Stream);
  GBEHeightmap1->LoadHeightmapFromStream(Stream);
  Stream->Free();
}
//---------------------------------------------------------------------------
void __fastcall TFormHeightmap::FloatAnimation2Process(TObject *Sender)
{
  Cylinder1->Position->Y = GBEHeightmap1->GetHeight(Cylinder1->Position->Point);
}
//---------------------------------------------------------------------------
void __fastcall TFormHeightmap::SwitchToLinesSwitch(TObject *Sender)
{
  GBEHeightmap1->ShowLines = SwitchToLines->IsChecked;
}
//---------------------------------------------------------------------------
void __fastcall TFormHeightmap::SwitchToRampSwitch(TObject *Sender)
{
  if (SwitchToRamp->IsChecked)
    GBEHeightmap1->MaterialSource = LightMaterialSource3;
  else
    GBEHeightmap1->MaterialSource = LightMaterialSource1;
  GBEHeightmap1->UseRamp = SwitchToRamp->IsChecked;

}
//---------------------------------------------------------------------------
void __fastcall TFormHeightmap::SpinBoxBlurChange(TObject *Sender)
{
  GBEHeightmap1->Flou = trunc(SpinBoxBlur->Value);
}
//---------------------------------------------------------------------------

