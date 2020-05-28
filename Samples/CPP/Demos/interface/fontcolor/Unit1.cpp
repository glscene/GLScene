//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBitmapFont"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLHUDObjects"
#pragma link "GLScene"
#pragma link "GLTeapot"
#pragma link "GLTimeEventsMgr"
#pragma link "GLWin32Viewer"
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
   SetGLSceneMediaDir();
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
