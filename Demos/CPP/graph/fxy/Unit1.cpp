//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGraph"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGraph"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGraph"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGraph"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Formula0(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint)
{
   // 0ro formula
   z = VectorNorm(x, y);
   z = x*y;
   VectorLerp(clrBlue, clrRed, (z+1)/2, color);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Formula1(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint)
{
	// 1st formula
   z = VectorNorm(x, y);
   z = x*y*z;
  // z = (x*x)*(y*y);
   VectorLerp(clrBlue, clrRed, (z+1)/2, color);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Formula2(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint)
{
   // 2nd formula
  z = VectorNorm(x, y);
  z = sin(z*12)/(2*(z*6.28+1));
   VectorLerp(clrBlue, clrRed, (z+1)/2, color);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Formula3(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint)
{
   // 3rd formula
   z = VectorNorm(x, y);
   z = (pow(x,2) + pow(y,2)) * sin(8*atan2(x,y));
   VectorLerp(clrBlue, clrRed, (z+1)/2, color);
}


//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox1Click(TObject * Sender)
{
  if(CheckBox1->Checked)
  {
	XZGrid->YSamplingScale->Origin = 0;
	YZGrid->XSamplingScale->Origin = 0;
	XYGrid->ZSamplingScale->Origin = 0;
  }
  else
  {
	XZGrid->YSamplingScale->Origin = -1;
	YZGrid->XSamplingScale->Origin = -1;
	XYGrid->ZSamplingScale->Origin = -1;
  }
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar1Change(TObject * Sender)
{
  XYGrid->ZSamplingScale->Origin = -((float)TrackBar1->Position / 10);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar2Change(TObject *Sender)
{
  XZGrid->YSamplingScale->Origin = -((float)TrackBar2->Position / 10);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar3Change(TObject *Sender)
{
  YZGrid->XSamplingScale->Origin = -((float)TrackBar3->Position / 10);
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject * Sender,
												TMouseButton Button,
												TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject * Sender,
												TShiftState Shift, int X, int Y)
{
	if (Shift.Contains(ssLeft))
		GLCamera1->MoveAroundTarget(my - Y, mx - X);
	else if (Shift.Contains(ssRight))
		GLCamera1->RotateTarget(my - Y, mx - X, 0);
	mx = X;
	my = Y;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
		  TPoint &MousePos, bool &Handled)
{
  GLCamera1->
   AdjustDistanceToTarget(Power(1.1, (WheelDelta / 120.0)));
}

//---------------------------------------------------------------------------



void __fastcall TForm1::RadioGroup1Click(TObject *Sender)
{
  switch (RadioGroup1->ItemIndex) {
	 case 0: GLHeightField1->OnGetHeight = Formula0; break;
	 case 1: GLHeightField1->OnGetHeight = Formula1; break;
	 case 2: GLHeightField1->OnGetHeight = Formula2; break;
	 case 3: GLHeightField1->OnGetHeight = Formula3; break;
   default:
	  ;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  RadioGroup1Click(Sender);
}
//---------------------------------------------------------------------------

