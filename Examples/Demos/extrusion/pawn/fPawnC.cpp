//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "fPawnC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Extrusion"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormPawn *FormPawn;
//---------------------------------------------------------------------------
__fastcall TFormPawn::TFormPawn(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormPawn::FormCreate(TObject *Sender)
{
  TFileName PathToData = GetCurrentAssetPath();
  SetCurrentDir(PathToData + "\\texture");
  RotationSolid1->Material->Texture->Image->LoadFromFile("ashwood.jpg");
}
//---------------------------------------------------------------------------
void __fastcall TFormPawn::CheckBox1Click(TObject *Sender)
{
   if (CheckBox1->Checked)
	  RotationSolid1->SplineMode = lsmCubicSpline;
   else
      RotationSolid1->SplineMode = lsmLines;
}
//---------------------------------------------------------------------------
void __fastcall TFormPawn::CheckBox2Click(TObject *Sender)
{
  if (CheckBox2->Checked)
	  RotationSolid1->Normals = nsSmooth;
   else
	  RotationSolid1->Normals = nsFlat;
}
//---------------------------------------------------------------------------
void __fastcall TFormPawn::CheckBox3Click(TObject *Sender)
{
   RotationSolid1->Material->Texture->Disabled = !CheckBox3->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TFormPawn::CheckBox4Click(TObject *Sender)
{
  if (CheckBox4->Checked)
	RotationSolid1->Material->Texture->TextureMode = tmModulate;
  else
    RotationSolid1->Material->Texture->TextureMode = tmDecal;
}
//---------------------------------------------------------------------------
void __fastcall TFormPawn::TrackBar1Change(TObject *Sender)
{
   RotationSolid1->StopAngle = TrackBar1->Position;
   if (TrackBar1->Position==360)
	  RotationSolid1->Parts = RotationSolid1->Parts<<rspStartPolygon, rspStopPolygon;
   else
      RotationSolid1->Parts = RotationSolid1->Parts<<rspStartPolygon, rspStopPolygon;

}
//---------------------------------------------------------------------------

void __fastcall TFormPawn::TrackBar2Change(TObject *Sender)
{
  RotationSolid1->Slices = TrackBar2->Position;
}
//---------------------------------------------------------------------------

void __fastcall TFormPawn::TrackBar3Change(TObject *Sender)
{
   RotationSolid1->Division = TrackBar3->Position;
}
//---------------------------------------------------------------------------

void __fastcall TFormPawn::Timer1Timer(TObject *Sender)
{
  LabelTri->Caption = Format("%d Triangles", ARRAYOFCONST((RotationSolid1->TriangleCount)));
}
//---------------------------------------------------------------------------

void __fastcall TFormPawn::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
   mx = X; my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TFormPawn::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
   if (Shift.Contains(ssLeft))
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
   mx = X; my = Y;
}
//---------------------------------------------------------------------------

