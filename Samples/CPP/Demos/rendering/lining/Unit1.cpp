//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLHiddenLineShader"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLOutlineShader"
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
   else
   if (Shift.Contains(ssRight))
	  GLCamera1->RotateTarget(my-Y, mx-X);
   mx = X; my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
  GLOutlineShader1->Enabled = CheckBox1->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox2Click(TObject *Sender)
{
 GLHiddenLineShader2->Enabled = CheckBox2->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox3Click(TObject *Sender)
{
  GLHiddenLineShader2->Solid = CheckBox3->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox4Click(TObject *Sender)
{
	if (CheckBox4->Checked)
	  GLHiddenLineShader2->BackLine->Pattern = 0xFF00;  // bit pattern
	else
	  GLHiddenLineShader2->BackLine->Pattern = 0xFFFF;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox5Click(TObject *Sender)
{
  GLHiddenLineShader2->SurfaceLit = CheckBox5->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox6Click(TObject *Sender)
{
  if (CheckBox6->Checked)
	GLHiddenLineShader2->ShadeModel = smFlat;
  else
	GLHiddenLineShader2->ShadeModel = smSmooth;
}
//---------------------------------------------------------------------------
