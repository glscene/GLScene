//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCameraController"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLGraph"
#pragma link "GLMaterial"
#pragma link "GLNavigator"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSmoothNavigator"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GetInput(TButton *Sender)
{
  FCameraSmoothAnimator_AbsPos->Enabled = false;
  FCameraSmoothAnimator_RelPos->Enabled = false;

  if ((Sender == btnMoveToPos) ||
	 (Sender == btnOrbitToPos) ||
	 (Sender == btnOrbitToPosAdv) ||
	 (Sender == btnSafeOrbitAndZoomToPos) ||
	 (Sender == btSmoothOrbit) ||
	 (Sender == btSmoothOrbitAndZoom) ||
	 (Sender == btSmoothOrbitToPosAdv))
	 {
		DextX = StrToFloatDef(eDestX->Text);
		DextY = StrToFloatDef(eDestY->Text);
		DextZ = StrToFloatDef(eDestZ->Text);
	 }
  if ((Sender == btnMoveToPos) ||
	 (Sender->Name == "btnZoomToDistance") ||
	 (Sender->Name == "btnOrbitToPosAdv") ||
	 (Sender->Name == "btnOrbitToPos"))
		 Time = StrToFloat(eTime->Text);
  if (Sender->Name=="btnZoomToDistance")
		 ZoomDistance = StrToFloat(eDistance->Text);
  if (Sender->Name=="btnSafeOrbitAndZoomToPos")
  {
	FGLCameraController->soSafeDistance = StrToFloat(eSafeDistance->Text);
	FGLCameraController->soTimeToSafePlacement = StrToFloat(eTimeToSafePlacement->Text);
	FGLCameraController->soTimeToOrbit = StrToFloat(eTimeToOrbit->Text);
	FGLCameraController->soTimeToZoomBackIn = StrToFloat(eTimeToZoomBackIn->Text);
  }
}
//---------------------------------------------------------------------------
Glvectorgeometry::TVector __fastcall TForm1::OnGetCameraPosition(
					   TGLNavigatorSmoothChangeVector* const ASender)
{
  if (ASender == FCameraSmoothAnimator_AbsPos)
	return GLCamera->AbsolutePosition;
  else
	return GLCamera->Position->DirectVector;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::OnSetCameraPosition(TGLNavigatorSmoothChangeVector* const ASender,
					   const Glvectortypes::TVector4f &AValue)
{
  if (ASender == FCameraSmoothAnimator_AbsPos)
	GLCamera->AbsolutePosition = AValue;
  else
	GLCamera->Position->AsVector = AValue;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  FGLCameraController = GLCameraController1;

  FCameraSmoothAnimator_AbsPos =
	new (TGLNavigatorSmoothChangeVector)(GLSmoothNavigator->CustomAnimatedItems);
  FCameraSmoothAnimator_AbsPos->Enabled = false;
  FCameraSmoothAnimator_AbsPos->Inertia = 0.6;
  FCameraSmoothAnimator_AbsPos->Speed = 1;
  FCameraSmoothAnimator_AbsPos->SpeedLimit = 5000;
  FCameraSmoothAnimator_AbsPos->Cutoff = 0.0001;
  FCameraSmoothAnimator_AbsPos->OnGetCurrentValue = OnGetCameraPosition;
  FCameraSmoothAnimator_AbsPos->OnSetCurrentValue = OnSetCameraPosition;

  FCameraSmoothAnimator_RelPos =
	new (TGLNavigatorSmoothChangeVector)(GLSmoothNavigator->CustomAnimatedItems);
  FCameraSmoothAnimator_RelPos->Assign(FCameraSmoothAnimator_AbsPos);

  GLSmoothNavigator->MovingObject = GLCamera;
  GLSmoothNavigator->MoveAroundParams->TargetObject = GLCamera->TargetObject;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::btnMoveToPosClick(TObject *Sender)
{
  GetInput(btnMoveToPos);
  FGLCameraController->MoveToPos(DextX, DextY, DextZ, Time);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnZoomToDistanceClick(TObject *Sender)
{
  GetInput(btnZoomToDistance);
  FGLCameraController->ZoomToDistance(ZoomDistance,Time);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnOrbitToPosClick(TObject *Sender)
{
  Glvectorgeometry::TVector lTargetPosition;

  GetInput(btnOrbitToPos);
  lTargetPosition = dcSphere->LocalToAbsolute(PointMake(DextX, DextY, DextZ));

  FGLCameraController->OrbitToPos(lTargetPosition.X,
     lTargetPosition.Y, lTargetPosition.Z, Time);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnSafeOrbitAndZoomToPosClick(TObject *Sender)
{
  GetInput(btnSafeOrbitAndZoomToPos);
  FGLCameraController->SafeOrbitAndZoomToPos(DextX, DextY, DextZ);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btSmoothOrbitAndZoomClick(TObject *Sender)
{
  btSmoothOrbitClick(btSmoothOrbitAndZoom);
  GLSmoothNavigator->AdjustDistanceParams->AddImpulse(Sign(Random() - 0.5) * 10);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btSmoothOrbitClick(TObject *Sender)
{
  float lAngle; // In radians.
  float lTime;
  bool lNeedToRecalculateZoom;
  Glvectorgeometry::TVector lTargetPosition;

  GetInput(btSmoothOrbit);
  lTargetPosition = dcSphere->LocalToAbsolute(PointMake(DextX, DextY, DextZ));
  lAngle = AngleBetweenVectors(GLCamera->AbsolutePosition, lTargetPosition, GLSphere1->AbsolutePosition);

  // The final look and feel of smooth animation is affected by
  // FCameraSmoothAnimator_AbsPos's propperties and this value.
  lTime = lAngle * 2;
  FCameraSmoothAnimator_RelPos->ResetTargetValue();
  FCameraSmoothAnimator_RelPos->Enabled = True;

  if (Sender = btSmoothOrbit)
	lNeedToRecalculateZoom = false;
  else
  if (Sender = btSmoothOrbitAndZoom)
	lNeedToRecalculateZoom = true;
  else
  {
	lNeedToRecalculateZoom = false;
///	Assert(false);
  }
  FGLCameraController->OrbitToPosSmooth(lTargetPosition, lTime,
	FCameraSmoothAnimator_RelPos, lNeedToRecalculateZoom, &YHmgVector);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btSmoothOrbitToPosAdvClick(TObject *Sender)
{
  float lAngle; // In radians.
  float lTime;
  Glvectorgeometry::TVector lTargetPosition;

  GetInput(btSmoothOrbitToPosAdv);

  lTargetPosition = dcSphere->LocalToAbsolute(PointMake(DextX, DextY, DextZ));

  lAngle = AngleBetweenVectors(GLCamera->AbsolutePosition, lTargetPosition, GLSphere1->AbsolutePosition);

  lTime = lAngle; // Speed can be controled by applying a multiplier here.

  FCameraSmoothAnimator_AbsPos->TargetValue->DirectVector = GLCamera->AbsolutePosition;
  FCameraSmoothAnimator_AbsPos->Enabled = true;
  FGLCameraController->OrbitToPosAdvancedSmooth(
	lTargetPosition.X, lTargetPosition.Y, lTargetPosition.Z,
	lTime, FCameraSmoothAnimator_AbsPos);

}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnOrbitToPosAdvClick(TObject *Sender)
{
  Glvectorgeometry::TVector lTargetPosition;

  GetInput(btnOrbitToPosAdv);
  lTargetPosition = dcSphere->LocalToAbsolute(PointMake(DextX, DextY, DextZ));
  FGLCameraController->OrbitToPosAdvanced(lTargetPosition.X, lTargetPosition.Y,
       lTargetPosition.Z, Time, UpAxis->Checked);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  FCameraSmoothAnimator_AbsPos->Enabled = false;
  FCameraSmoothAnimator_RelPos->Enabled = false;
  GLCamera->AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  if (cbMoveParent->Checked)
	dcMovingParent->Position->X = Sin(newTime * 1.5) * 8;

  // For btSmoothOrbitAndZoomClick Order of these commands is important.
  GLSmoothNavigator->AdjustDistanceToTarget(0, deltaTime);

  FGLCameraController->Step(deltaTime, newTime);

  // This component requires FixedDeltaTime higher than FMaxExpectedDeltatime.
  GLSmoothNavigator->AnimateCustomItems(deltaTime);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
  FCameraSmoothAnimator_AbsPos->Enabled = false;
  FCameraSmoothAnimator_RelPos->Enabled = false;
  FGLCameraController->StopMovement();

   if (Shift.Contains(ssLeft))
   {
	 mx = X; my = Y;
   }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
  if (Shift.Contains(ssLeft))
  {
	GLCamera->MoveAroundTarget(my-Y, mx-X);
	mx = X; my = Y;
	Caption = "Camera Controller - camera position = " +
	  FormatFloat("0",GLCamera->Position->X)+"/"+
	  FormatFloat("0",GLCamera->Position->Y)+"/"+
	  FormatFloat("0",GLCamera->Position->Z);

  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseUp(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
  Caption = "Camera Controller";
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  FormatFloat(camDirX->Text, GLCamera->Direction->X);
  FormatFloat(camDirY->Text, GLCamera->Direction->Y);
  FormatFloat(camDirZ->Text, GLCamera->Direction->Z);

  FormatFloat(camUpX->Text, GLCamera->Up->X);
  FormatFloat(camUpY->Text, GLCamera->Up->Y);
  FormatFloat(camUpZ->Text, GLCamera->Up->Z);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  GLCadencer1->Enabled = false;
}
//---------------------------------------------------------------------------


