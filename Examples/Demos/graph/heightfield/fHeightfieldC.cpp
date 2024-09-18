//---------------------------------------------------------------------------

#include <vcl.h>
#include <System.Math.hpp>
#pragma hdrstop

#include "fHeightfieldC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Graph"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormHeightField* FormHeightField;
//---------------------------------------------------------------------------
__fastcall TFormHeightField::TFormHeightField(TComponent* Owner) : TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TFormHeightField::FormCreate(TObject* Sender)
{
    // start with first formula
    HeightField1->OnGetHeight = Formula1;
    // no per-vertex coloring
    ComboBox1->ItemIndex = 1;
    ComboBox1Change(Sender);
}

//---------------------------------------------------------------------------
void __fastcall TFormHeightField::Formula1(const float x, const float y,
    float &z, TVector4f &color, TTexPoint &texPoint)
{ // first formula
    z = VectorNorm(x, y);
    z = cos(z * 12) / (2 * (z * 6.28 + 1));
    VectorLerp(clrBlue, clrRed, (z + 1) / 2, color);
}

//---------------------------------------------------------------------------
void __fastcall TFormHeightField::Formula2(const float x, const float y,
    float &z, TVector4f &color, TTexPoint &texPoint)
{
    // 2nd formula
    z = (x * x) * (y * y);
    VectorLerp(clrBlue, clrRed, (z + 1) / 2, color);
}

//---------------------------------------------------------------------------
void __fastcall TFormHeightField::Formula3(const float x, const float y,
    float &z, TVector4f &color, TTexPoint &texPoint)
{
    // 3rd formula, dynamic
    z = 1 /
        (1 + VectorNorm(Sphere1->Position->X - x, Sphere1->Position->Y - y));
    if (((ceil(x * 4) + ceil(y * 4)) && 1) == 1)
        color = clrBlue;
    else
        color = clrYellow;
}

//---------------------------------------------------------------------------

void __fastcall TFormHeightField::Sphere1Progress(
    TObject* Sender, const double deltaTime, const double newTime)

{
    // move our little sphere around
    if (Sphere1->Visible) {
        Sphere1->Position->SetPoint(cos(newTime * 2.3), sin(newTime), 1.5);
        HeightField1->StructureChanged();
    }
}

//---------------------------------------------------------------------------
void __fastcall TFormHeightField::ComboBox1Change(TObject* Sender)
{
    // change per vertex color mode
    switch (ComboBox1->ItemIndex) {
        case 0:
            HeightField1->ColorMode = hfcmNone;
            break;
        case 1:
            HeightField1->ColorMode = hfcmEmission;
            break;
        case 2:
            HeightField1->ColorMode = hfcmDiffuse;
            break;
        default:;
    }
}
//---------------------------------------------------------------------------
void __fastcall TFormHeightField::CheckBox1Click(TObject* Sender)
{
    // enable two sided surface
    if (CheckBox1->Checked)
        HeightField1->Options = HeightField1->Options << hfoTextureCoordinates,
        hfoTwoSided;
    else
        HeightField1->Options = HeightField1->Options << hfoTextureCoordinates;
}
//---------------------------------------------------------------------------

void __fastcall TFormHeightField::TrackBar1Change(TObject* Sender)
{
    // adjust X extents
    HeightField1->XSamplingScale->Min = -TrackBar1->Position / 10;
    HeightField1->XSamplingScale->Max = TrackBar1->Position / 10;
}

//---------------------------------------------------------------------------
void __fastcall TFormHeightField::TrackBar2Change(TObject* Sender)
{
    // adjust Y extents
    HeightField1->YSamplingScale->Min = -TrackBar2->Position / 10;
    HeightField1->YSamplingScale->Max = TrackBar2->Position / 10;
}

//---------------------------------------------------------------------------
void __fastcall TFormHeightField::TrackBar3Change(TObject* Sender)
{
    // adjust grid steps (resolution)
    HeightField1->XSamplingScale->Step = (float)TrackBar3->Position / 1000;
    HeightField1->YSamplingScale->Step = (float)TrackBar3->Position / 1000;
}

//---------------------------------------------------------------------------

void __fastcall TFormHeightField::GLSceneViewerMouseMove(
    TObject* Sender, TShiftState Shift, int X, int Y)
{
    if (Shift.Contains(ssLeft))
        GLCamera1->MoveAroundTarget(my - Y, mx - X);
    mx = X;
    my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TFormHeightField::GLSceneViewerMouseDown(
    TObject* Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
    mx = X;
    my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TFormHeightField::RadioGroup1Click(TObject* Sender)
{
    Sphere1->Visible = false;
    // switch between formulas
    switch (RadioGroup1->ItemIndex) {
        case 0:
            HeightField1->OnGetHeight = Formula1;
            break;
        case 1:
            HeightField1->OnGetHeight = Formula2;
            break;
        case 2: {
            HeightField1->OnGetHeight = Formula3;
            Sphere1->Visible = true;
            break;
        };
        default:;
    }
}
//---------------------------------------------------------------------------

void __fastcall TFormHeightField::CheckBox2Click(TObject* Sender)
{
    GLLightSource1->Shining = CheckBox2->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TFormHeightField::Timer1Timer(TObject* Sender)
{
    // Display number of triangles used in the mesh
	// You will note that this number quickly gets out of hand if you are
	// using large high-resolution grids
///	LabelFPS->Caption = Format("%d Triangles - %.2f FPS",
///		ARRAYOFCONST((HeightField1->TriangleCount, GLSceneViewer1->FramesPerSecond())));
	LabelFPS->Caption =  GLSceneViewer->FramesPerSecond();
	GLSceneViewer->ResetPerformanceMonitor();
}

//---------------------------------------------------------------------------

void __fastcall TFormHeightField::FormMouseWheel(TObject* Sender,
    TShiftState Shift, int WheelDelta, TPoint &MousePos, bool &Handled)
{
    GLCamera1->AdjustDistanceToTarget(Power(1.1, (WheelDelta / 120.0)));
}
//---------------------------------------------------------------------------


