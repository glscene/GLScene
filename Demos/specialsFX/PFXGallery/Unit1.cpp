//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Behaviours"
#pragma link "GLS.BitmapFont"
#pragma link "GLS.Blur"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.HUDObjects"
#pragma link "GLS.Navigator"
#pragma link "GLS.Objects"
#pragma link "GLS.ParticleFX"
#pragma link "GLS.PerlinPFX"
#pragma link "GLS.Scene"
#pragma link "GLS.SpaceText"
#pragma link "GLS.SceneViewer"
#include "GLS.Keyboard.hpp"

#pragma resource "*.dfm"
TForm1 *Form1;

int  cRunBoost = 10;
int	 cWalkStep = 20;
int	 cStrafeStep =20;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  chkFloorClick(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::chkFloorClick(TObject *Sender)
{
   GLPlane1->Visible = chkFloor->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::chkBlurClick(TObject *Sender)
{
   GLBlur1->Visible = chkBlur->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::chkMouseLookClick(TObject *Sender)
{
   GLUserInterface1->MouseLookActive = chkMouseLook->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
	GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
	 HandleKeys(deltaTime);
	 GLUserInterface1->MouseLook();

	 GLSceneViewer1->Invalidate();
	 GLUserInterface1->MouseUpdate();

	 GLSceneViewer1->Invalidate();

}
//---------------------------------------------------------------------------

void __fastcall TForm1::HandleKeys(double deltaTime)
{
   float boost;

   if (IsKeyDown(vkEscape)) {
	  chkMouseLook->Checked = false;
	  chkMouseLookClick(this);
   }

   if (IsKeyDown(vkShift))
	  boost = cRunBoost*deltaTime;
   else
   if (IsKeyDown(vkControl))
	  boost = cRunBoost*0.01*deltaTime;
   else
	  boost = deltaTime;

   if (IsKeyDown('W'))
	  GLCamera1->Move(cWalkStep*boost);
   if (IsKeyDown('S'))
	  GLCamera1->Move(-cWalkStep*boost);

   if (IsKeyDown('A'))
	  GLCamera1->Slide(-cStrafeStep*boost);
   if (IsKeyDown('D'))
	  GLCamera1->Slide(cStrafeStep*boost);
}
//---------------------------------------------------------------------------


