//---------------------------------------------------------------------------

#include <vcl.h>
#include <stdlib.h>
#pragma hdrstop

#include "fParticlesC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.SceneViewer"
#pragma link "GLS.VectorGeometry"
#pragma link "GLS.Behaviours"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Particles"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma link "GLS.SceneViewer"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.SimpleNavigation"
#pragma resource "*.dfm"
TFormStars *FormStars;

float random(void)
{
  return (float)(rand() & 0xFFF) / (float)0xFFF;
}

//---------------------------------------------------------------------------
__fastcall TFormStars::TFormStars(TComponent * Owner):TForm(Owner)
{
  TFileName Path = GetCurrentAssetPath() + "\\texture\\";
  // work only with (path + "flare1.bmp")  for sprites
  Sprite1->Material->Texture->Image->LoadFromFile(Path + "flare1.bmp");
  Randomize();
}

//---------------------------------------------------------------------------

void __fastcall TFormStars::GLParticles1ActivateParticle(TObject * Sender,
													 TGLBaseSceneObject *
													 particle)
{
  // this event is called when a star is activated,
  // ie. just before it will be rendered
  TGLFaceProperties *fp = ((TGLSprite *) particle)->Material->FrontProperties;
  // we pick a random color
  fp->Emission->Color = PointMake(random(), random(), random());
  // our halo starts transparent
  fp->Diffuse->Alpha = 0;

  // this is our "birth time"
  ((TGLSprite *) particle)->TagFloat = GLCadencer1->CurrentTime;
}

//---------------------------------------------------------------------------
void __fastcall TFormStars::Sprite1Progress(TObject * Sender,
										const double deltaTime,
										const double newTime)
{
  double life;

  // calculate for how long we've been living
  life = (newTime - ((TGLSprite *) Sender)->TagFloat);
  if(life > 10)
	// old particle to kill
	GLParticles1->KillParticle((TGLSprite *) Sender);
  else if(life < 1)
	// baby particles become brighter in their 1st second of life...
	((TGLSprite *) Sender)->Material->FrontProperties->Diffuse->Alpha = life;
  else         // ...and slowly disappear in the darkness
	((TGLSprite *) Sender)->Material->FrontProperties->Diffuse->Alpha =
	  (9.0 - life) / 9.0;
}

//---------------------------------------------------------------------------
void __fastcall TFormStars::Timer1Timer(TObject * Sender)
{
  // every timer, we create a particle at a random position
  TGLCoordinates *c = ((TGLSprite *) GLParticles1->CreateParticle())->Position;
  c->X = 3 * (random() - 0.5);
  c->Y = 3 * (random() - 0.5);
  c->Z = 3 * (random() - 0.5);

  // infos for the user
  Caption = "Stars - "+
	Format("%d stars, %.1f FPS", ARRAYOFCONST((GLParticles1->Count - 1,
			GLSceneViewer1->FramesPerSecond())));
  GLSceneViewer1->ResetPerformanceMonitor();
}

//---------------------------------------------------------------------------
void __fastcall TFormStars::FormResize(TObject * Sender)
{
  // change focal so the view will shrink and not just get clipped
  GLCamera1->FocalLength = 50 * Width / 280;
}

//---------------------------------------------------------------------------

