//---------------------------------------------------------------------------

#include <vcl.h>
#include <stdlib.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLWin32Viewer"
#pragma link "GLVectorGeometry"
#pragma link "GLBehaviours"
#pragma link "GLCadencer"
#pragma link "GLParticles"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLWin32Viewer"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;

float random(void)
{
  return (float)(rand() & 0xFFF) / (float)0xFFF;
}

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  String MediaPath = ExtractFilePath(ParamStr(0));
  int I = MediaPath.Pos("Samples");
  if (I != 0) {
	MediaPath.Delete(I+8,MediaPath.Length()-I);
	MediaPath += "Media\\";
	SetCurrentDir(MediaPath);
  }
  Sprite1->Material->Texture->Image->LoadFromFile(MediaPath+"Flare1.bmp");
  Randomize();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLParticles1ActivateParticle(TObject * Sender,
													 TGLBaseSceneObject *
													 particle)
{
// this event is called when a particle is activated,
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
void __fastcall TForm1::Sprite1Progress(TObject * Sender,
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
void __fastcall TForm1::Timer1Timer(TObject * Sender)
{
  // every timer, we create a particle at a random position
  TGLCoordinates *c = ((TGLSprite *) GLParticles1->CreateParticle())->Position;
  c->X = 3 * (random() - 0.5);
  c->Y = 3 * (random() - 0.5);
  c->Z = 3 * (random() - 0.5);

  // infos for the user
  Caption = "GLScene Particles - "+
	Format("%d particles, %.1f FPS", ARRAYOFCONST((GLParticles1->Count - 1,
			GLSceneViewer1->FramesPerSecond())));
  GLSceneViewer1->ResetPerformanceMonitor();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormResize(TObject * Sender)
{
  // change focal so the view will shrink and not just get clipped
  GLCamera1->FocalLength = 50 * Width / 280;
}

//---------------------------------------------------------------------------

