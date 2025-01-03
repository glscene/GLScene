//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMandelC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GR32_ExtImage"
#pragma link "GR32_Image"
#pragma resource "*.dfm"
TFormMandelC *FormMandelC;
//---------------------------------------------------------------------------
__fastcall TFormMandelC::TFormMandelC(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormMandelC::FormCreate(TObject *Sender)
{
  MandelSampler = TMandelbrotSampler->Create(Img);
  AdaptiveSampler = TAdaptiveSuperSampler->Create(MandelSampler);
  SuperSampler = TSuperSampler->Create(MandelSampler);
  JitteredSampler = TPatternSampler->Create(MandelSampler);
  Sampler = MandelSampler();
}
//---------------------------------------------------------------------------
