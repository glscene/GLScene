//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "fCameraC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.GeomObjects"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyPress(TObject *Sender, System::WideChar &Key)
{
  switch (Key) {
	case '7' : Teapot1->RotateAbsolute(-15,  0,  0); break;
	case '9' : Teapot1->RotateAbsolute(+15,  0,  0); break;
	case '4' : Teapot1->RotateAbsolute(  0,-15,  0); break;
	case '6' : Teapot1->RotateAbsolute(  0,+15,  0); break;
	case '1' : Teapot1->RotateAbsolute(  0,  0,-15); break;
	case '3' : Teapot1->RotateAbsolute(  0,  0,+15); break;
  default: ;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RadioGroup1Click(TObject *Sender)
{
  switch (RadioGroup1->ItemIndex) {
	case 0: GLCamera1->CameraStyle = csPerspective; break;
	case 1: GLCamera1->CameraStyle = csInfinitePerspective; break;
	case 2: GLCamera1->CameraStyle = csPerspectiveKeepFOV; break;
	case 3: GLCamera1->CameraStyle = csCustom; break;
  default: ;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RadioGroup2Click(TObject *Sender)
{
  GLCamera1->KeepFOVMode = TGLCameraKeepFOVMode(RadioGroup2->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCamera1CustomPerspective(const TRectangle &viewport, int width,
          int height, int DPI, float &viewPortRadius)
{
  TGLMatrix Mat;

  Mat =  CreatePerspectiveMatrix(GLCamera1->GetFieldOfView(Width)/4,
	Width / Height, GLCamera1->NearPlaneBias, GLCamera1->DepthOfView);
  Mat = MatrixMultiply(Mat, CreateRotationMatrixZ(a));
  *CurrentGLContext()->PipelineTransformation->ProjectionMatrix = Mat;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  a = M_PI * sin(newTime) / 18;
  GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
	// store mouse coordinates when a button went down
	mdx = X; mdy = Y;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
	int dx, dy;
	TGLVector v;

	// calculate delta since last move or last mousedown
	dx = mdx-X; dy = mdy-Y;
	mdx = X; mdy = Y;
	if (Shift.Contains(ssLeft))
	  if (Shift.Contains(ssShift))
		 // right button with shift rotates the teapot
		 // (rotation happens around camera's axis)
		GLCamera1->RotateObject(Teapot1, dy, dx);
	  else
		// right button without shift changes camera angle
		// (we're moving around the parent and target dummycube)
		GLCamera1->MoveAroundTarget(dy, dx);
	else
	if (Shift.Contains(ssRight))
	{
		// left button moves our target and parent dummycube
		v = GLCamera1->ScreenDeltaToVectorXY(dx, -dy,
			0.12*GLCamera1->DistanceToTarget()/GLCamera1->FocalLength);
		DummyCube1->Position->Translate(v);
		// notify camera that its position/target has been changed
		GLCamera1->TransformationChanged();
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
	// Note that 1 wheel-step induces a WheelDelta of 120,
	// this code adjusts the distance to target with a 10% per wheel-step ratio
	GLCamera1->AdjustDistanceToTarget(Power((float)1.1, WheelDelta/120));
}
//---------------------------------------------------------------------------
