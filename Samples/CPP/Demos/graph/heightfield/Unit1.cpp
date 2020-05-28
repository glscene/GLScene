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
   // start with first formula
   HeightField1->OnGetHeight = Formula1;
   // no per-vertex coloring
   ComboBox1->ItemIndex = 1;
   ComboBox1Change(Sender);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Formula1(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint)
{ // first formula
   z = VectorNorm(x, y);
   z = cos(z*12)/(2*(z*6.28+1));
   VectorLerp(clrBlue, clrRed, (z+1)/2, color);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Formula2(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint)
{
	// 2nd formula
   z = (x*x)*(y*y);
   VectorLerp(clrBlue, clrRed, (z+1)/2, color);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Formula3(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint)
{
   // 3rd formula, dynamic
   z = 1/(1+VectorNorm(Sphere1->Position->X-x,Sphere1->Position->Y-y));
   if (((Round(x*4)+Round(y*4)) && 1)==1)
	  color = clrBlue;
   else
	  color=clrYellow;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Sphere1Progress(TObject *Sender, const double deltaTime, const double newTime)

{
   // move our little sphere around
   if (Sphere1->Visible) {
	  Sphere1->Position->SetPoint(cos(newTime*2.3), sin(newTime), 1.5);
	  HeightField1->StructureChanged();
   }
}

//---------------------------------------------------------------------------
void __fastcall TForm1::ComboBox1Change(TObject *Sender)
{
   // change per vertex color mode
   switch (ComboBox1->ItemIndex) {
	 case 0 : HeightField1->ColorMode = hfcmNone; break;
	 case 1 : HeightField1->ColorMode = hfcmEmission; break;
	 case 2 : HeightField1->ColorMode = hfcmDiffuse; break;
	default :;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
   // enable two sided surface
   if (CheckBox1->Checked)
	  HeightField1->Options = HeightField1->Options << hfoTextureCoordinates, hfoTwoSided;
   else
	  HeightField1->Options = HeightField1->Options << hfoTextureCoordinates;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar1Change(TObject *Sender)
{
   // adjust X extents
   HeightField1->XSamplingScale->Min = -TrackBar1->Position/10;
   HeightField1->XSamplingScale->Max = TrackBar1->Position/10;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBar2Change(TObject *Sender)
{
   // adjust Y extents
   HeightField1->YSamplingScale->Min = -TrackBar2->Position/10;
   HeightField1->YSamplingScale->Max = TrackBar2->Position/10;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBar3Change(TObject *Sender)
{
   // adjust grid steps (resolution)
  HeightField1->XSamplingScale->Step = (float)TrackBar3->Position/1000;
  HeightField1->YSamplingScale->Step = (float)TrackBar3->Position/1000;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
   if (Shift.Contains(ssLeft))
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
	mx = X; my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
   mx = X; my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RadioGroup1Click(TObject *Sender)
{
   Sphere1->Visible = false;
   // switch between formulas
   switch (RadioGroup1->ItemIndex) {
	 case 0: HeightField1->OnGetHeight = Formula1; break;
	 case 1: HeightField1->OnGetHeight = Formula2; break;
	 case 2:{
			 HeightField1->OnGetHeight = Formula3;
			 Sphere1->Visible = true; break;
			};
	default :;
   }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox2Click(TObject *Sender)
{
   GLLightSource1->Shining = CheckBox2->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   // Display number of triangles used in the mesh
   // You will note that this number quickly gets out of hand if you are
   // using large high-resolution grids
   LabelFPS->Caption = Format("%d Triangles - %.2f FPS",
	   ARRAYOFCONST ((HeightField1->TriangleCount,
	   GLSceneViewer1->FramesPerSecond())));
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------



void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  GLCamera1->
   AdjustDistanceToTarget(Power(1.1, (WheelDelta / 120.0)));
}
//---------------------------------------------------------------------------

