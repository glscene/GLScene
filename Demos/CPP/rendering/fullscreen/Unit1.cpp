//---------------------------------------------------------------------------
#pragma hdrstop
#include <vcl.h>
#include <tchar.h>
#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "Vcl.Controls.TControl"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLFullScreenViewer"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLTeapot"
#pragma resource "*.dfm"
TDataModule1 *DataModule1;
//---------------------------------------------------------------------------
__fastcall TDataModule1::TDataModule1(TComponent* Owner)
	: TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::DataModuleCreate(TObject *Sender)
{
   // Adjusts Zoom to size (might have been modified in the IDE, by you, user!)
   GLCamera1->SceneScale = GLFullScreenViewer1->Width/160;
   // Start fullscreen mode, no cursor
   GLFullScreenViewer1->Cursor = crNone;
   GLFullScreenViewer1->Active = true;
   while (GLFullScreenViewer1->Active)
   {
	  // Message queue is not operational, but there may still be some messages
	  Application->ProcessMessages();
	  // Relinquish some of that CPU time
	  Sleep(1);
	  // Slowly rotate the teapot and the blue light
	  Teapot1->TurnAngle = 4*Frac(24*(Now()))*3600;
	  DCBlueLight->RollAngle = 32*Frac(24*(Now()))*3600;
   }
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::GLFullScreenViewer1PostRender(TObject *Sender)
{
   TGLCanvas *glc;
   int x, y;
   glc = new TGLCanvas(GLFullScreenViewer1->Width, GLFullScreenViewer1->Height);
//   with glc do begin
   x = Mouse->CursorPos.X;
   y = Mouse->CursorPos.Y;
   glc->PenColor = clYellow;
  // Alpha-transparency antialiasing:
  // we render the ellipse twice, the first pass with a very transparent
  // wide pen, and a second time with a thinner pen.
   glc->PenAlpha = 0.4;
   glc->PenWidth = 3;
   glc->Ellipse(x, y, 16, 16);
   glc->PenAlpha = 0.75;
   glc->PenWidth = 2;
   glc->Ellipse(x, y, 16, 16);
	  // Complete the reticle
   glc->PenAlpha = 0.3;
   glc->PenWidth = 2;
   glc->Line(x-32, y, x+32, y);
   glc->Line(x, y-32, x, y+32);
   glc->Free();
}
//---------------------------------------------------------------------------

void __fastcall TDataModule1::GLFullScreenViewer1KeyPress(TObject *Sender, System::WideChar &Key)

{
   // ESC leaves fullscreen mode
   if (Key = '\27') {
	  GLFullScreenViewer1->Active = false;
	  Key = '\0';
   }
}
//---------------------------------------------------------------------------

