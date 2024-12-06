//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fAtmosphereC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.Scene"
#pragma link "GLS.Objects"
#pragma link "GLS.Cadencer"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma link "GLS.LensFlare"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLS.SkyDome"
#pragma link "GLS.Atmosphere"


#pragma resource "*.dfm"
TFormAtmosphere *FormAtmosphere;

//---------------------------------------------------------------------------
__fastcall TFormAtmosphere::TFormAtmosphere(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormAtmosphere::FormCreate(TObject *Sender)
{
  Atmosphere = (TGLAtmosphere *)(GLDummyCube1->AddNewChild(__classid(TGLAtmosphere)));
  Atmosphere->Sun  = GLLensFlare1;
  Atmosphere->SetOptimalAtmosphere2(Planet->Radius);
  GLSkyDome1->Bands->Clear();
  GLSkyDome1->Stars->AddRandomStars(5000, ConvertColorVector(clrWhite));
}
//---------------------------------------------------------------------------
void __fastcall TFormAtmosphere::FormClose(TObject *Sender, TCloseAction &Action)
{
  Atmosphere->Free();
}
//---------------------------------------------------------------------------
void __fastcall TFormAtmosphere::ButtonRotateClick(TObject *Sender)
{
  GLCadencer1->Enabled = !GLCadencer1->Enabled;
}
//---------------------------------------------------------------------------

void __fastcall TFormAtmosphere::Button2Click(TObject *Sender)
{
  GLLensFlare1->Slide(0.8);
}
//---------------------------------------------------------------------------
void __fastcall TFormAtmosphere::Button3Click(TObject *Sender)
{
  GLLensFlare1->Slide(-0.8);
}
//---------------------------------------------------------------------------

void __fastcall TFormAtmosphere::Button4Click(TObject *Sender)
{
  GLDummyCube1->Slide(-0.5);
}
//---------------------------------------------------------------------------

void __fastcall TFormAtmosphere::Button5Click(TObject *Sender)
{
  GLDummyCube1->Slide(0.5);
}
//---------------------------------------------------------------------------
void __fastcall TFormAtmosphere::btnShowAtmosphereClick(TObject *Sender)
{
  Atmosphere->Visible = !Atmosphere->Visible;
}
//---------------------------------------------------------------------------

void __fastcall TFormAtmosphere::Button8Click(TObject *Sender)
{
  Atmosphere->TogleBlendingMode();
}
//---------------------------------------------------------------------------
void __fastcall TFormAtmosphere::Button10Click(TObject *Sender)
{
  GLCamera1->AdjustDistanceToTarget(1.1);
}
//---------------------------------------------------------------------------
void __fastcall TFormAtmosphere::Button9Click(TObject *Sender)
{
  GLCamera1->AdjustDistanceToTarget((float)1 / 1.1);
}
//---------------------------------------------------------------------------

void __fastcall TFormAtmosphere::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  Planet->Turn(deltaTime *20);
  GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
