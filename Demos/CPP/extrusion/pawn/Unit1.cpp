//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLExtrusion"
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
  SetGLSceneMediaDir();
  RotationSolid1->Material->Texture->Image->LoadFromFile("ashwood.jpg");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
   if (CheckBox1->Checked)
	  RotationSolid1->SplineMode = lsmCubicSpline;
   else
      RotationSolid1->SplineMode = lsmLines;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox2Click(TObject *Sender)
{
  if (CheckBox2->Checked)
	  RotationSolid1->Normals = nsSmooth;
   else
	  RotationSolid1->Normals = nsFlat;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox3Click(TObject *Sender)
{
   RotationSolid1->Material->Texture->Disabled = !CheckBox3->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox4Click(TObject *Sender)
{
  if (CheckBox4->Checked)
	RotationSolid1->Material->Texture->TextureMode = tmModulate;
  else
    RotationSolid1->Material->Texture->TextureMode = tmDecal;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBar1Change(TObject *Sender)
{
   RotationSolid1->StopAngle = TrackBar1->Position;
   if (TrackBar1->Position==360)
	  RotationSolid1->Parts = RotationSolid1->Parts<<rspStartPolygon, rspStopPolygon;
   else
      RotationSolid1->Parts = RotationSolid1->Parts<<rspStartPolygon, rspStopPolygon;

}
//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar2Change(TObject *Sender)
{
  RotationSolid1->Slices = TrackBar2->Position;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar3Change(TObject *Sender)
{
   RotationSolid1->Division = TrackBar3->Position;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  LabelTri->Caption = Format("%d Triangles", ARRAYOFCONST((RotationSolid1->TriangleCount)));
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
   mx = X; my = Y;
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

