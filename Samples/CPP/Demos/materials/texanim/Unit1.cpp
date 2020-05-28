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
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
	int i;
	Graphics::TBitmap *bmp;
   // We generate a handful of bitmaps from scratch
   // you could also load them from a set of files, extract from an AVI etc.
	for (i = 0; i <= 9; i++) {
		bmp = new TBitmap;
		bmp->PixelFormat = pf24bit;
		bmp->Width = 60;
		bmp->Height = 60;
		bmp->Canvas->Font->Name = "Arial";
		bmp->Canvas->Font->Height = 56;
		bmp->Canvas->TextOutW(15, 5, i);
		GLMaterialLibrary1->AddTextureMaterial("IMG" + IntToStr(i), bmp);
        bmp->Free();
	}
	// Initialize our loop
	Cube1->Material->MaterialLibrary = GLMaterialLibrary1;
	Cube1->Material->LibMaterialName = "IMG0";
	GLMaterialLibrary1->Tag = 0;
   // GUI update
	CBAnimate->Enabled = true;
    Button1->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
	// cube turns slowly
	Cube1->Turn(deltaTime * 3);
   // cycle textures
	if (CBAnimate->Checked){
	  // coutdown to next frame
		timeToNextFrame = timeToNextFrame-deltaTime;
		// if it's time for the next frame
		if(timeToNextFrame<0){
		 // first, update frame counter (the Tag property in our sample)
		 // (such a loop is a little overkill, yeah)
			while (timeToNextFrame<0){
				timeToNextFrame = timeToNextFrame + 0.2;
				GLMaterialLibrary1->Tag = (GLMaterialLibrary1->Tag+1)%10;
			}
			// then, we update the material reference
			Cube1->Material->LibMaterialName = "IMG" + IntToStr(GLMaterialLibrary1->Tag);
		}
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
   // stop animation
	CBAnimate->Checked = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
	// standard FPS
    LabelFPS->Caption =  Format("%.1f FPS",	ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond())));
}
//---------------------------------------------------------------------------
