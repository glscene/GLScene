//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "fMotionBlurC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.GeomObjects"
#pragma link "GLS.HUDObjects"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.VectorFileObjects"
#pragma resource "*.dfm"
TFormMotionBlur *FormMotionBlur;
//---------------------------------------------------------------------------
__fastcall TFormMotionBlur::TFormMotionBlur(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormMotionBlur::FormCreate(TObject *Sender)
{
   Frames = 5;
   HUD->Material->FrontProperties->Diffuse->Alpha = 1.0 - (float)1.00/Frames;
}
//---------------------------------------------------------------------------
void __fastcall TFormMotionBlur::GLSceneViewerPostRender(TObject *Sender)
{
   // render is done, we transfer it to our hud plane so it can be used
   // in the next frame
   GLSceneViewer->Buffer->CopyToTexture(HUD->Material->Texture);
}
//---------------------------------------------------------------------------
void __fastcall TFormMotionBlur::FormResize(TObject *Sender)
{
   int w, h;

   // Here we resize our texture and plane to follow window dimension changes
   // Note that we have to stick to power of two texture dimensions if we don't
   // want performance to drop dramatically, this implies we can waste 3/4
   // of our texture memory... (f.i. a 513x513 window will require and use
   // a 1024x1024 texture)
   w = RoundUpToPowerOf2(GLSceneViewer->Width);
   h = RoundUpToPowerOf2(GLSceneViewer->Height);
   HUD->Material->Texture->DestroyHandles();

/*
   HUD->Material->Texture->Image->Width = w;
   HUD->Material->Texture->Image->Height = h;
*/

   HUD->Position->X = w*0.5;
   HUD->Position->Y = GLSceneViewer->Height - h*0.5;
   HUD->Width = w;
   HUD->Height = h;

}
//---------------------------------------------------------------------------

void __fastcall TFormMotionBlur::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
   // make things move
   Cube->TurnAngle = newTime*90;
   DummyCube->PitchAngle = newTime*60;
   Dodecahedron->RollAngle = newTime*15;
}
//---------------------------------------------------------------------------
void __fastcall TFormMotionBlur::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)

{
   // turn on/off VSync, this has an obvious impact on framerate,
   // which in turns impacts the motion blur look
   if ((IsKeyDown('s') || IsKeyDown('S'))
   || (IsKeyDown('v') || IsKeyDown('V')))
	  if (GLSceneViewer->VSync == vsmNoSync)
		 GLSceneViewer->VSync = vsmSync;
	  else
		 GLSceneViewer->VSync = vsmNoSync;

   // change the number of motion blur frames, and adjust
   // the transparency of the plane accordingly
   if (Key == VK_UP) Frames++;
   if ((Key == VK_DOWN) && (Frames>0)) Frames--;
   if (Frames == 0)
	  HUD->Visible = false;
   else
   {
	  HUD->Visible = true;
	  HUD->Material->FrontProperties->Diffuse->Alpha = 1 - (float)1/(1+Frames);
   }
}

// standard issue camera movement
//---------------------------------------------------------------------------
void __fastcall TFormMotionBlur::GLSceneViewerMouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
 mx = X; my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TFormMotionBlur::GLSceneViewerMouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
   if (Shift.Contains(ssLeft))
	  Camera->MoveAroundTarget(my-Y, mx-X);
   mx = X; my = Y;

}
//---------------------------------------------------------------------------

void __fastcall TFormMotionBlur::Timer1Timer(TObject *Sender)
{
//  const String  cVSync[vsmSync][vsmNoSync] = {"VSync ON", "VSync OFF"};
  Panel1->Caption = Format("Motion Blur on %d Frames %f FPS",
	ARRAYOFCONST ((Frames, GLSceneViewer->FramesPerSecond())));
   GLSceneViewer->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------

