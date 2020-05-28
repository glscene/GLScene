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
#pragma link "GLGeomObjects"
#pragma link "GLObjects"
#pragma link "GLParticleFX"
#pragma link "GLPerlinPFX"
#pragma link "GLScene"
#pragma link "GLShadowPlane"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFile3DS"
#pragma link "GLFileJPEG"

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
   String MediaPath = ExtractFilePath(ParamStr(0));
   int I = MediaPath.Pos("Samples");
   if (I != 0) {
	MediaPath.Delete(I+8,MediaPath.Length()-I);
	MediaPath += "Media\\";
	SetCurrentDir(MediaPath);
  }
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
