//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBehaviours"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLObjects"
#pragma link "GLParticleFX"
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
   RadioGroup1Click(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RadioGroup1Click(TObject *Sender)
{
   TGLSourcePFXEffect *source;

   source = GetOrCreateSourcePFX(DCVolcano);
   switch (RadioGroup1->ItemIndex) {
	 case 0: source->ParticleInterval =0.1; break;
	 case 1: source->ParticleInterval =0.5; break;
	 case 2: source->ParticleInterval =0.02; break;
	 case 3: source->ParticleInterval =0.01; break;
	 case 4: source->ParticleInterval =0.005; break;
	 case 5: source->ParticleInterval =0.001; break;
   default:
	   ;
   }

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
   GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   Panel1->Caption = Format("%.1f FPS - %3d Particles - Depth Sort: %.2f msec",
   ARRAYOFCONST((GLSceneViewer1->FramesPerSecond(), PFXVolcano->Particles->ItemCount()+PFXBlue->Particles->ItemCount(),
					 PFXRenderer->LastSortTime)));
   GLSceneViewer1->ResetPerformanceMonitor();

}
//---------------------------------------------------------------------------
