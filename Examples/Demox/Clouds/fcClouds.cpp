//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "fcClouds.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GBE.Clouds"
#pragma resource "*.fmx"
TFormClouds *FormClouds;
//---------------------------------------------------------------------------
__fastcall TFormClouds::TFormClouds(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormClouds::FormCreate(TObject *Sender)
{
  GBEClouds1->AddTextureCloud(TextureMaterialSource1);
  GBEClouds1->AddTextureCloud(TextureMaterialSource2);
  GBEClouds1->AddTextureCloud(TextureMaterialSource3);

  GBEClouds1->NbClouds = 15;
  GBEClouds1->WindSpeed = 0.1;
  GBEClouds1->Limits = 100;

  GBEClouds1->ActiveWind = true;
  FloatAnimation1->Start();

}
//---------------------------------------------------------------------------
void __fastcall TFormClouds::FloatAnimation1Process(TObject *Sender)
{
  GBEClouds1->MoveClouds();
}
//---------------------------------------------------------------------------
void __fastcall TFormClouds::ArcDial1Change(TObject *Sender)
{
  GBEClouds1->RotationAngle->Y = ArcDial1->Value;
}
//---------------------------------------------------------------------------
void __fastcall TFormClouds::SpinBox1Change(TObject *Sender)
{
  if (SpinBox1->Value > 0)
    GBEClouds1->NbClouds = round(SpinBox1->Value);
}
//---------------------------------------------------------------------------
void __fastcall TFormClouds::SpinBox2Change(TObject *Sender)
{
  GBEClouds1->WindSpeed = 0.1 * SpinBox2->Value;
}
//---------------------------------------------------------------------------
