//---------------------------------------------------------------------------

#include <vcl.h>
#include <Graphics.hpp>
#pragma hdrstop

#include "FSplitterMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
		: TForm(Owner)
{
  SetCurrentDir(ExtractFilePath(ExtractFileDir(Application->ExeName))+"\\Data");
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button1Click(TObject *Sender)
{
  TPicture *pic;
  Graphics::TBitmap *bmp, *bmp2;
  int s, sd, f;
  int x, y;

  s = StrToInt(EDTileSize->Text);
  pic = new TPicture();

  if (RBHalf->Checked)
    f = 2;
  else if (RBLow->Checked)
    f = 4;
  else f = 1;
  sd = s/f;

  ProgressBar->Position = 0;
  Screen->Cursor = crHourGlass;

  bmp = new Graphics::TBitmap();
  bmp->PixelFormat = pf24bit;
  bmp->Width = sd;
  bmp->Height = sd;

  if (f != 1) {
    bmp2 = new Graphics::TBitmap();
    bmp2->PixelFormat = pf24bit;
    bmp2->Width = s;
    bmp2->Height = s;
  } else bmp2 = NULL;

  LAAction->Caption = "Loading Jpeg texture...";
  LAAction->Visible = true;
  Refresh();
  pic->LoadFromFile(EDFile->Text);
  x = 0; while (x<pic->Width)  {
    y = 0; while (y<pic->Height)  {

       if (sd != s) {
          bmp2->Canvas->Draw(-x, -y, pic->Graphic);
          bmp->Canvas->StretchDraw(Rect(0, 0, sd, sd), bmp2);
       } else bmp->Canvas->Draw(-x, -y, pic->Graphic);
       LAAction->Caption = Format("Generating tile %d-%d...", ARRAYOFCONST((x/s, y/s)));
       Refresh();
       bmp->SaveToFile(Format(EDMask->Text, ARRAYOFCONST((x/s, y/s))));
       ProgressBar->StepBy(1);

       y += s;
    }
    x += s;
  }

  delete bmp2;
  delete bmp;
  delete pic;

  Screen->Cursor = crDefault;
  LAAction->Caption = "Completed";
  ShowMessage("Done!");
  Application->Terminate();
}
//---------------------------------------------------------------------------
 