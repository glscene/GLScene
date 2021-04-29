//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMainC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.GeomObjects"
#pragma link "GLS.Objects"
#pragma link "GLS.ParticleFX"
#pragma link "GLS.PerlinPFX"
#pragma link "GLS.Scene"
#pragma link "GLS.ShadowPlane"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.File3DS"
#pragma link "GLS.FileJPEG"
#pragma link "GLS.Utils"

#pragma resource "*.dfm"
TForm1 *Form1;
int mx, my;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormActivate(TObject *Sender)
{
   SetGLSceneMediaDir();
   GLFreeForm1->LoadFromFile("beer.3ds");

   GLFreeForm1->Material->Texture->Image->LoadFromFile("clouds.jpg");
   GLShadowPlane1->Material->Texture->Image->LoadFromFile("ashwood.jpg");
   GetOrCreateSourcePFX(GLDummyCube3)->Burst(0, 150);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1DblClick(TObject *Sender)
{
  GLCadencer1->Enabled = !GLCadencer1->Enabled;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
  if (Shift.Contains(ssShift))
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
   mx = X;
   my = Y;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
 GLCamera1->MoveAroundTarget(0, 10*deltaTime);
}
//---------------------------------------------------------------------------
