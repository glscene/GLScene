//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "fFontColorC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.BitmapFont"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.HUDObjects"
#pragma link "GLS.Scene"
#pragma link "GLS.GeomObjects"
#pragma link "GLS.TimeEventsMgr"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TForm1 *Form1;

const int FadeOutMax = 100;
const int FadeInMax  = 100;
const float  OverallTrans = 0.7;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
   TFileName Path = GetCurrentAssetPath();
   BitmapFont->Glyphs->LoadFromFile("toonfont.bmp");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLTimeEventsMGR1Events0Event(TTimeEvent *event)
{
   if (FadeOutCount < 0) exit;
   HUDText1->ModulateColor->Color = VectorMake(1, 1, 1, (FadeOutCount/FadeOutMax)*OverallTrans);
   FadeOutCount--;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLTimeEventsMGR1Events1Event(TTimeEvent *event)
{
   FadeOutCount = FadeOutMax;
   FadeInCount = 0;

   OriginalColor = HUDText2->ModulateColor->Color;

   HUDText1->ModulateColor->Color = VectorMake(1, 1, 1, (FadeOutCount/FadeOutMax)*OverallTrans);
   HUDText2->ModulateColor->Color = VectorMake(1, 1, 1, 0);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLTimeEventsMGR1Events2Event(TTimeEvent *event)
{
   TVector4f NewColor;
   if (FadeInCount >= FadeInMax) exit;

   NewColor = VectorScale(OriginalColor, FadeInCount/FadeInMax);

   HUDText2->ModulateColor->Color = NewColor;
   FadeInCount++;
}
//---------------------------------------------------------------------------
