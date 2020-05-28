//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLHUDObjects"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLParticles"
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
void __fastcall TForm1::FormCreate(TObject *Sender)
{
   SetGLSceneMediaDir();
   // hide the Windows cursor for the GLSceneViewer
   GLSceneViewer1->Cursor = crNone;
   // and load my ugly cursor (size adjusted in design props)
   GLMaterialLibrary1->Materials->Items[0]->Material->Texture->Image->LoadFromFile("cursor.bmp");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MILoadImageClick(TObject *Sender)
{
   if (OpenPictureDialog1->Execute())
   {
	  // use the hourglass cursor, it may take some time to load the bitmap,
	  // rescale it and generate mipmaps before sending it to OpenGL
	  Screen->Cursor = crHourGlass;
	  HSBitmap->Material->Texture->Image->LoadFromFile(OpenPictureDialog1->FileName);
		 // adjust hud sprite size to match that of the picture
	  HSBitmap->SetSize(Width, Height);
		 // adjust position, hudsprites are centered on their x, y coords
	  HSBitmap->Position->X = Width/2;
	  HSBitmap->Position->Y = Height/2;
	  Screen->Cursor = crDefault;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
   TColor color;
   // Prevents event floods on slow hardware
   if (!handleMouseMoves) exit;
   handleMouseMoves = false;
   // Mouse moved, adjust the position of our cursor
   HSCursor->Position->X = X;
   HSCursor->Position->Y = Y;
   // Update the status bar with some misc. info
   color =GLSceneViewer1->Buffer->GetPixelColor(X, Y);
   StatusBar1->SimpleText = Format("X:%4d Y:%4d, R:%3d G:%3d B:%3d",
		   ARRAYOFCONST	((X, Y, GetRValue(color), GetGValue(color), GetBValue(color))));
   // Add a trail particle
   if (MITrail->Checked)
	  GLParticles1->CreateParticle();
   // Update things now
   GLCadencer1->Progress();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1AfterRender(TObject *Sender)
{
  handleMouseMoves = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
   GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::HSParticleProgress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
	  // decrease life time / alpha
	HSParticle->TagFloat = HSParticle->TagFloat + deltaTime;
	  // update alpha channel, but if no more life is left, then suicide
   if (HSParticle->TagFloat<0)
	   GLParticles1->KillParticle(HSParticle);
   else
	  HSParticle->AlphaChannel = HSParticle->TagFloat*0.2;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLParticles1ActivateParticle(TObject *Sender, TGLBaseSceneObject *particle)
{
//   with (particle as TGLHUDSprite) do begin
	  // we are cadencing real-time, so these are 5 seconds
	  particle->TagFloat = 5;
	  // new particle stands where cursor is
	  particle->Position->AsVector = HSCursor->Position->AsVector;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   // update FPS and sprite count
   miFPS->Caption = Format("%.1f FPS - %d Cursor Sprites",
		  ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond(), GLParticles1->Count)));
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MITrailClick(TObject *Sender)
{
   // turn trails on/off
   MITrail->Checked = !MITrail->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MIExitClick(TObject *Sender)
{
  Close();

}
//---------------------------------------------------------------------------
