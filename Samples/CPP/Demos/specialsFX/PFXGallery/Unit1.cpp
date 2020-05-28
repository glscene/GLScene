//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBehaviours"
#pragma link "GLBitmapFont"
#pragma link "GLBlur"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLHUDObjects"
#pragma link "GLNavigator"
#pragma link "GLObjects"
#pragma link "GLParticleFX"
#pragma link "GLPerlinPFX"
#pragma link "GLScene"
#pragma link "GLSpaceText"
#pragma link "GLWin32Viewer"
#include "GLKeyboard.hpp"

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
	Caption = "PFXGallery FPS - " + IntToStr(Round(GLSceneViewer1->FramesPerSecond()));
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


