//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLGraph"
#pragma link "GLObjects"
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
  Navigator = new TGLSmoothNavigator (this);
  Navigator->AngleLock = false;
  Navigator->AutoUpdateObject = false;
  Navigator->InvertHorizontalSteeringWhenUpsideDown = true;
  Navigator->MoveUpWhenMovingForward = true;
  Navigator->UseVirtualUp = true;
  Navigator->VirtualUp->AsAffineVector = YVector;
  Navigator->MovingObject = GLCamera1;

  Navigator->InertiaParams->MovementAcceleration = 7;
  Navigator->InertiaParams->MovementInertia = 200;
  Navigator->InertiaParams->MovementSpeed = 200;

  Navigator->InertiaParams->TurnInertia = 150;
  Navigator->InertiaParams->TurnSpeed = 40;
  Navigator->InertiaParams->TurnMaxAngle = 0.5;

  Navigator->MoveAroundParams->TargetObject = GLArrowLine1;

  UI = new TGLSmoothUserInterface(this);
//  UI.AutoUpdateMouse = false;
  UI->SmoothNavigator = Navigator;

}
//---------------------------------------------------------------------------

void __fastcall  TForm1::CheckControls(double DeltaTime, double newTime)
{
  bool NeedToAccelerate;

  NeedToAccelerate = IsKeyDown(VK_SHIFT);

  Navigator->StrafeVertical(IsKeyDown('F'), IsKeyDown('R'), DeltaTime, NeedToAccelerate);
  Navigator->MoveForward(IsKeyDown('W'), IsKeyDown('S'), DeltaTime, NeedToAccelerate);
  Navigator->StrafeHorizontal(IsKeyDown('D'), IsKeyDown('A'), DeltaTime, NeedToAccelerate);

//  GetCursorPos(RealPos);
  UI->MouseLook(/*RealPos, */DeltaTime);
//  if UI.MouseLookActive then
//    SetCursorPos(Round(UI.OriginalMousePos.X), Round(UI.OriginalMousePos.Y));
}


void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  GLSceneViewer1->Invalidate();


  if (UI->MouseLookActive)
	CheckControls(deltaTime, newTime);
  else
  {
	if (ShiftState.Contains(ssRight) && ShiftState.Contains(ssLeft))
	{
	  Navigator->MoveAroundTarget(0, 0, deltaTime);
	  Navigator->AdjustDistanceToTarget(yy - NewYY, deltaTime);
	}
	else
	if (ShiftState.Contains(ssRight) || ShiftState.Contains(ssLeft))
	{
	  Navigator->MoveAroundTarget(yy - NewYY, xx - NewXX, deltaTime);
	  Navigator->AdjustDistanceToTarget(0, deltaTime);
	}
	else
	{
	  Navigator->MoveAroundTarget(0, 0, deltaTime);
	  Navigator->AdjustDistanceToTarget(0, deltaTime);
	}
	xx = NewXX;
	yy = NewYY;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FPSTimerTimer(TObject *Sender)
{
  Caption = "Smooth Navigator  -  " + GLSceneViewer1->FramesPerSecondText();
  Navigator->AutoScaleParameters(GLSceneViewer1->FramesPerSecond());
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::MouseLookCheckBoxClick(TObject *Sender)
{
  if (MouseLookCheckBox->Checked)
  {
	GLCamera1->TargetObject = NULL;
	GLCamera1->PointTo(GLArrowLine1, YHmgVector);

	UI->MouseLookActive = true;
//    GetCursorPos(RealPos);
//    UI->OriginalMousePos->SetPoint2D(RealPos->X, RealPos->Y);
//    ShowCursor(false);
  }
  else
  {
	UI->MouseLookActive = false;
//    ShowCursor(true);
	GLCamera1->Up->SetVector(0, 1, 0);
	GLCamera1->TargetObject = GLArrowLine1;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyPress(TObject *Sender, System::WideChar &Key)
{
  if (Key == Char(VK_SPACE))
	MouseLookCheckBoxClick(Sender);
  if (Key == Char(VK_ESCAPE))
	Close();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  GLSceneViewer1->Enabled = false;
  GLCadencer1->Enabled = false;
  FPSTimer->Enabled = false;

  delete UI;
  delete Navigator;
  GLShowCursor(true);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RadioButton6Click(TObject *Sender)
{
  GLCadencer1->FixedDeltaTime = 0;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RadioButton7Click(TObject *Sender)
{
  GLCadencer1->FixedDeltaTime = 0.01;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RadioButton8Click(TObject *Sender)
{
  GLCadencer1->FixedDeltaTime = 0.1;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
  ShiftState =  Shift;
  NewXX = X;
  NewYY = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
  xx = X;
  yy = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  // (WheelDelta / Abs(WheelDelta) is used to deternime the sign.
  Navigator->AdjustDistanceParams->AddImpulse((WheelDelta / abs(WheelDelta)));
}
//---------------------------------------------------------------------------

