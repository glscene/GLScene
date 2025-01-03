//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fLineStipplingC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GR32_Image"
#pragma resource "*.dfm"
TFormLineStipplingC *FormLineStipplingC;
//---------------------------------------------------------------------------
__fastcall TFormLineStipplingC::TFormLineStipplingC(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormLineStipplingC::FormCreate(TObject *Sender)
{
  Image->SetupBitmap;
  ScrollBarChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TFormLineStipplingC::ScrollBarChange(TObject *Sender)
{
//
}
//---------------------------------------------------------------------------
void __fastcall TFormLineStipplingC::Spiral(int X, int Y);
{
//
}
//---------------------------------------------------------------------------

