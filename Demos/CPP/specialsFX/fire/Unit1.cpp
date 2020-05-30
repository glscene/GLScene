//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBehaviours"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLFireFX"
#pragma link "GLGeomObjects"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
int mx,my;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
		  TPoint &MousePos, bool &Handled)
{
 GLCamera1->AdjustDistanceToTarget(System::Math::Power(1.1, WheelDelta/120));
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  Caption = "GLScene Fire - "+ Format("%.1f FPS", ARRAYOFCONST((GLSceneViewer1->FramesPerSecond())));
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
  if (Shift.Contains(ssLeft))
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
   mx = X;
   my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
   mx = X; my = Y;
}
//---------------------------------------------------------------------------
