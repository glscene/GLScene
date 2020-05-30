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
#pragma link "GLObjects"
#pragma link "GLParticleFX"
#pragma link "GLPerlinPFX"
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
void __fastcall TForm1::GLCadencerProgress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  SmokePFX->Rotation = newTime;
  GLSceneViewer->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::TimerTimer(TObject *Sender)
{
   Panel1->Caption =
	Format("%.1f FPS",
	ARRAYOFCONST((GLSceneViewer->FramesPerSecond(), SmokePFX->ParticleCount()+FlamePFX->ParticleCount(),
					 ParticleFXRenderer->LastSortTime)));
   GLSceneViewer->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
