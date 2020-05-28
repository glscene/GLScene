//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLWin32Viewer"
#pragma link "GLCrossPlatform"
#pragma link "GLVectorGeometry"
#pragma link "GLBehaviours"
#pragma link "GLWin32Viewer"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLCadencer"
#pragma link "GLParticleFX"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLFullScreenViewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TimerTimer(TObject * Sender)
{
// Display FPS and particle count in the form's caption
  Caption = "Spiral - " +
	Format("%d Particles - %.1f FPS",
		   ARRAYOFCONST((PFXRing->Particles->ItemCount() +
					  PFXSpiral->Particles->ItemCount(),GLSceneViewer->FramesPerSecond())));
  GLSceneViewer->ResetPerformanceMonitor();

  // Alternatively trigger a "sphere" or "ring" explosion
  //
  TGLSourcePFXEffect *e;

  Timer->Tag = Timer->Tag + 1;
  if((Timer->Tag & 1) != 0)
  {
	// "Sphere" explosion
	e = GetOrCreateSourcePFX(DCBase,"");
	e->VelocityDispersion = 1.5;
	e->Burst(GLCadencer->CurrentTime, 200);
	e->VelocityDispersion = 0;
  }
  else
  {
	// Ring explosion
	GetOrCreateSourcePFX(DCBase,"")->RingExplosion(GLCadencer->CurrentTime, 1, 1.2,
												150);
  }
}

//---------------------------------------------------------------------------

void __fastcall TForm1::FormResize(TObject * Sender)
{
  // Rescale when window is resized
  GLCamera->SceneScale = GLSceneViewer->Height / 350;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewerDblClick(TObject * Sender)
{
  // Switch to full-screen, but using the same screen resolution
  // if(you uncomment the line below, it will switch to 800x600x32
  GLFullScreenViewer->UseCurrentResolution();
  GLFullScreenViewer->Active = True;
  // Hide the windows viewer so it is no longer updated
  GLSceneViewer->Visible = False;
  // Apply proper scale
  GLCamera->SceneScale = GLFullScreenViewer->Height / 350;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLFullScreenViewerDblClick(TObject * Sender)
{
  // Make the windows viewer visible again
  GLSceneViewer->Visible = True;
  // Deactivate full-screen mode
  GLFullScreenViewer->Active = False;
  // And apply back the adequate scale for the SceneViewer
  FormResize(this);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLFullScreenViewerKeyPress(TObject * Sender, char &Key)
{
  // Hitting 'ESC' has same effect as a double-click
  // (the FullScreenViewer has several Form-like events)
  if(Key == 0x27)
  {
    GLFullScreenViewerDblClick(this);
    Key = '\0';
  }
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewerMouseMove(TObject * Sender,
                                               TShiftState Shift, int X, int Y)
{
  // Mouse moved in the Viewer (windowed or fullscreen mode)
  if(Shift.Contains(ssRight))
    GLCamera->Position->Y =
      ((float)GLScene->CurrentBuffer->Height() / 2.0 -
       Y) * 0.1 / GLCamera->SceneScale;
  // Ensures we don't flood the event system with mouse moves (high priority events)
  // and ) prevent the cadencer from progressing (low priority events)
  GLCadencer->Progress();
}

//---------------------------------------------------------------------------

