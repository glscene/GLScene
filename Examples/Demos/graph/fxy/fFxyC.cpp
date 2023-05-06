//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fFxyC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

#pragma link "GLS.Graph"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma resource "*.dfm"
TFormFxy* FormFxy;

//---------------------------------------------------------------------------

__fastcall TFormFxy::TFormFxy(TComponent* Owner) : TForm(Owner) {}

//---------------------------------------------------------------------------
void __fastcall TFormFxy::Formula0(const float x, const float y, float &z,
	TVector4f &color, TTexPoint &texPoint)
{
	// 0ro formula
	z = VectorNorm(x, y);
	z = x * y;
	VectorLerp(clrBlue, clrRed, (z + 1) / 2, color);
}

//---------------------------------------------------------------------------
void __fastcall TFormFxy::Formula1(const float x, const float y, float &z,
	TVector4f &color, TTexPoint &texPoint)
{
	// 1st formula
	z = VectorNorm(x, y);
	z = x * y * z;  // or z = (x*x)*(y*y);
	VectorLerp(clrBlue, clrRed, (z + 1) / 2, color);
}

//---------------------------------------------------------------------------
void __fastcall TFormFxy::Formula2(const float x, const float y, float &z,
	TVector4f &color, TTexPoint &texPoint)
{
	// 2nd formula
	z = VectorNorm(x, y);
	z = sin(z * 12) / (2 * (z * 6.28 + 1));
	VectorLerp(clrBlue, clrRed, (z + 1) / 2, color);
}

//---------------------------------------------------------------------------
void __fastcall TFormFxy::Formula3(const float x, const float y, float &z,
	TVector4f &color, TTexPoint &texPoint)
{
	// 3rd formula
	z = VectorNorm(x, y);
	z = (pow(x, 2) + pow(y, 2)) * sin(8 * atan2(x, y));
	VectorLerp(clrBlue, clrRed, (z + 1) / 2, color);
}

//---------------------------------------------------------------------------

void __fastcall TFormFxy::FormCreate(TObject* Sender)
{
	rgFormulaClick(Sender);
}

//---------------------------------------------------------------------------

void __fastcall TFormFxy::chbCenterClick(TObject* Sender)
{
	if (chbCenter->Checked) {
		XZGrid->YSamplingScale->Origin = 0;
		YZGrid->XSamplingScale->Origin = 0;
		XYGrid->ZSamplingScale->Origin = 0;
	} else {
		XZGrid->YSamplingScale->Origin = -1;
		YZGrid->XSamplingScale->Origin = -1;
		XYGrid->ZSamplingScale->Origin = -1;
	}
}

//---------------------------------------------------------------------------

void __fastcall TFormFxy::TrackBarYChange(TObject* Sender)
{
	XYGrid->ZSamplingScale->Origin = -((float)TrackBarY->Position / 10);
}

//---------------------------------------------------------------------------

void __fastcall TFormFxy::TrackBarXChange(TObject* Sender)
{
	XZGrid->YSamplingScale->Origin = -((float)TrackBarX->Position / 10);
}
//---------------------------------------------------------------------------

void __fastcall TFormFxy::TrackBarZChange(TObject* Sender)
{
	YZGrid->XSamplingScale->Origin = -((float)TrackBarZ->Position / 10);
}
//---------------------------------------------------------------------------

void __fastcall TFormFxy::ViewerMouseDown(
	TObject* Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	mx = X;
	my = Y;
}

//---------------------------------------------------------------------------
void __fastcall TFormFxy::ViewerMouseMove(
	TObject* Sender, TShiftState Shift, int X, int Y)
{
	if (Shift.Contains(ssLeft))
		Camera->MoveAroundTarget(my - Y, mx - X);
	else if (Shift.Contains(ssRight))
		Camera->RotateTarget(my - Y, mx - X, 0);
	mx = X;
	my = Y;
}

//---------------------------------------------------------------------------
void __fastcall TFormFxy::FormMouseWheel(TObject* Sender, TShiftState Shift,
	int WheelDelta, TPoint &MousePos, bool &Handled)
{
	Camera->AdjustDistanceToTarget(Power(1.1, (WheelDelta / 120.0)));
}

//---------------------------------------------------------------------------

void __fastcall TFormFxy::rgFormulaClick(TObject *Sender)
{
	switch (rgFormula->ItemIndex) {
		case 0:
			HeightField->OnGetHeight = Formula0;
			break;
		case 1:
			HeightField->OnGetHeight = Formula1;
			break;
		case 2:
			HeightField->OnGetHeight = Formula2;
			break;
		case 3:
			HeightField->OnGetHeight = Formula3;
			break;
		default:;
	}
}

//---------------------------------------------------------------------------

void __fastcall TFormFxy::rgPolygonModeClick(TObject* Sender)
{
	switch (rgPolygonMode->ItemIndex) {
		case 0:
			HeightField->Material->PolygonMode = pmFill;
			break;
		case 1:
			HeightField->Material->PolygonMode = pmLines;
			break;
		case 2:
			HeightField->Material->PolygonMode = pmPoints;
			break;
		default:;
	}
   HeightField->StructureChanged();
}

//---------------------------------------------------------------------------

