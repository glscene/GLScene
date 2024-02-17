//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fLiningShadersC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma link "GLS.GeomObjects"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLSL.LineShaders"

#pragma resource "*.dfm"
TFormLining *FormLining;
//---------------------------------------------------------------------------
__fastcall TFormLining::TFormLining(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormLining::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
   mx = X; my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TFormLining::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
   if (Shift.Contains(ssLeft))
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
   else
   if (Shift.Contains(ssRight))
	  GLCamera1->RotateTarget(my-Y, mx-X);
   mx = X; my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TFormLining::CheckBox1Click(TObject *Sender)
{
  GLOutlineShader1->Enabled = CheckBox1->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TFormLining::CheckBox2Click(TObject *Sender)
{
 GLHiddenLineShader2->Enabled = CheckBox2->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TFormLining::CheckBox3Click(TObject *Sender)
{
  GLHiddenLineShader2->Solid = CheckBox3->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TFormLining::CheckBox4Click(TObject *Sender)
{
	if (CheckBox4->Checked)
	  GLHiddenLineShader2->BackLine->Pattern = 0xFF00;  // bit pattern
	else
	  GLHiddenLineShader2->BackLine->Pattern = 0xFFFF;
}
//---------------------------------------------------------------------------
void __fastcall TFormLining::CheckBox5Click(TObject *Sender)
{
  GLHiddenLineShader2->SurfaceLit = CheckBox5->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TFormLining::CheckBox6Click(TObject *Sender)
{
  if (CheckBox6->Checked)
	GLHiddenLineShader2->ShadeModel = smFlat;
  else
	GLHiddenLineShader2->ShadeModel = smSmooth;
}
//---------------------------------------------------------------------------
