//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#include <system.hpp>
#include <math.hpp>

#pragma hdrstop

#include "fWhirlC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Behaviours"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Objects"
#pragma link "GLS.Particles"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormWhirlC *FormWhirlC;
//---------------------------------------------------------------------------
__fastcall TFormWhirlC::TFormWhirlC(TComponent* Owner)
	: TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TFormWhirlC::GLParticles1ActivateParticle(TObject *Sender, TGLBaseSceneObject *particle)

{
	float r, alpha, cr, sr;

	alpha = Random()*2*M_PI;
	r = 2*Random();
	SinCosine(alpha, r*r, sr, cr);
	particle->Children[0]->Position->SetPoint(sr, 3*r-3, cr);
	GetOrCreateInertia(particle)->TurnSpeed = Random(30);
	particle->TagFloat = GLCadencer1->CurrentTime;
}

//---------------------------------------------------------------------------
void __fastcall TFormWhirlC::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
  mx = X; my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TFormWhirlC::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
  if (Shift.Contains(ssLeft))
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
   mx = X;
   my = Y;
}
//---------------------------------------------------------------------------


void __fastcall TFormWhirlC::GLDummyCube1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  if (newTime-(GLParticles1->TagFloat)>3)
	GLParticles1->KillParticle(GLParticles1);

}
//---------------------------------------------------------------------------

void __fastcall TFormWhirlC::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  GLParticles1->CreateParticle();
}
//---------------------------------------------------------------------------

void __fastcall TFormWhirlC::Timer1Timer(TObject *Sender)
{
  Panel1->Caption = Format("%d particles, %.1f FPS",
	  ARRAYOFCONST((GLParticles1->Count, GLSceneViewer1->FramesPerSecond())));
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------

