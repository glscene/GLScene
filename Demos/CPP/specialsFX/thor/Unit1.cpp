//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGraph"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
#pragma link "GLSkydome"
#pragma link "GLThorFX"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  SetGLSceneMediaDir();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GSbarChange(TObject *Sender)
{
  GLThorFXManager1->GlowSize = (float) GSbar->Position/50;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GAbarChange(TObject *Sender)
{
  GLThorFXManager1->InnerColor->Alpha = (float) GAbar->Position/50;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::WildBarChange(TObject *Sender)
{
 GLThorFXManager1->Wildness = (float) WildBar->Position/5;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::VibBarChange(TObject *Sender)
{
  GLThorFXManager1->Vibrate = (float) VibBar->Position/10;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DistanceBarChange(TObject *Sender)
{
   float Dist = GLCamera1->DistanceToTarget();
   float cx = GLCamera1->Position->X;
   float cy = GLCamera1->Position->Y;
   float cz = GLCamera1->Position->Z;
   float NewDist = DistanceBar->Position;
   GLCamera1->Position->X = (float) cx/Dist*NewDist;
   GLCamera1->Position->Y = (float) cy/Dist*NewDist;
   GLCamera1->Position->Z = (float) cz/Dist*NewDist;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CoreBoxClick(TObject *Sender)
{
  GLThorFXManager1->Core = CoreBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLThorFXManager1CalcPoint(TObject *Sender, int PointNo, float &x,
		  float &y, float &z)
{
//---------------Add user-definable formula to individual points in thor-object-------------
 if (SpinBox->Checked)
 {
	float Place = (float) PointNo/GLThorFXManager1->Maxpoints;
	float Spin = (Place*M_PI)*10+(GLCadencer1->CurrentTime*20);
	float Scale = (float) Sin(Place*M_PI)/2;
	y = y+Sin(Spin)*Scale;
	x = x+Cos(Spin)*Scale;
 }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PauseBoxClick(TObject *Sender)
{
   GLThorFXManager1->Disabled = PauseBox->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::HeightField1GetHeight(const float x, const float y, float &z,
		  TVector4f &Color, TTexPoint &TexPoint)
{
   z = 2;
}
//---------------------------------------------------------------------------

