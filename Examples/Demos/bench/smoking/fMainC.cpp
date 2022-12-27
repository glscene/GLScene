//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "fMainC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Objects"
#pragma link "GLS.ParticleFX"
#pragma link "GLS.PerlinPFX"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
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
