//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLWindowsFont"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
		: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Button1Click(TObject *Sender)
{
  FontDialog1->Font  =  Panel1->Font;
  if (FontDialog1->Execute())
  {
	Panel1->Font  =  FontDialog1->Font;
	FFont->Font  =  FontDialog1->Font;
	Image1->Picture->Assign(FFont->Glyphs);
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  FFont = new TGLWindowsBitmapFont(this);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Button2Click(TObject *Sender)
{
  if (SaveDialog1->Execute())
  {
	FFont->Glyphs->SaveToFile(SaveDialog1->FileName);
  }
}
//---------------------------------------------------------------------------
