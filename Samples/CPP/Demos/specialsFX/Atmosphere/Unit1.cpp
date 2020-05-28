//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLScene"
#pragma link "GLObjects"
#pragma link "GLCadencer"
#pragma link "GLWin32Viewer"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLLensFlare"
#pragma link "GLSimpleNavigation"
#pragma link "GLSkydome"
#pragma link "GLAtmosphere"
#pragma link "GLLensFlare"

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
  Atmosphere = (TGLAtmosphere *)(GLDummyCube1->AddNewChild(__classid(TGLAtmosphere)));
  Atmosphere->Sun  = GLLensFlare1;
  Atmosphere->SetOptimalAtmosphere2(GLSphere1->Radius);
  GLSkyDome1->Bands->Clear();
  GLSkyDome1->Stars->AddRandomStars(5000, ConvertColorVector(clrWhite));
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  Atmosphere->Free();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  GLSphere1->Roll(20);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject *Sender)
{
  GLLensFlare1->Slide(0.8);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button3Click(TObject *Sender)
{
  GLLensFlare1->Slide(-0.8);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button4Click(TObject *Sender)
{
  GLDummyCube1->Slide(-0.5);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button5Click(TObject *Sender)
{
  GLDummyCube1->Slide(0.5);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button6Click(TObject *Sender)
{
  Atmosphere->Visible = ! Atmosphere->Visible;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button8Click(TObject *Sender)
{
  Atmosphere->TogleBlendingMode();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button10Click(TObject *Sender)
{
  GLCamera1->AdjustDistanceToTarget(1.1);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button9Click(TObject *Sender)
{
  GLCamera1->AdjustDistanceToTarget((float)1 / 1.1);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
