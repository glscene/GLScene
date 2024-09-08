//---------------------------------------------------------------------------

#include <fmx.h>
#ifdef _WIN32
#include <tchar.h>
#endif

#pragma hdrstop

#include "fcCube.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GBE.CubeExtend"
#pragma resource "*.fmx"
TFormCube *FormCube;

//---------------------------------------------------------------------------
__fastcall TFormCube::TFormCube(TComponent* Owner)
	: TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TFormCube::FloatAnimation1Process(TObject *Sender)
{
  GBECubeExtend1->RotationAngle->X = GBECubeExtend1->RotationAngle->X + 1;
  GBECubeExtend1->RotationAngle->Z = GBECubeExtend1->RotationAngle->Z + 2;
}

//---------------------------------------------------------------------------
void __fastcall TFormCube::CheckBoxFrontChange(TObject *Sender)
{
  GBECubeExtend1->FaceFrontVisible = CheckBoxFront->IsChecked;
}

//---------------------------------------------------------------------------
void __fastcall TFormCube::CheckBoxRightChange(TObject *Sender)
{
  GBECubeExtend1->FaceRightVisible = CheckBoxRight->IsChecked;
}

//---------------------------------------------------------------------------

void __fastcall TFormCube::CheckBoxBackChange(TObject *Sender)
{
  GBECubeExtend1->FaceBackVisible = CheckBoxBack->IsChecked;
}

//---------------------------------------------------------------------------

void __fastcall TFormCube::CheckBoxLeftChange(TObject *Sender)
{
  GBECubeExtend1->FaceLeftVisible = CheckBoxLeft->IsChecked;
}

//---------------------------------------------------------------------------

void __fastcall TFormCube::CheckBoxTopChange(TObject *Sender)
{
  GBECubeExtend1->FaceTopVisible = CheckBoxTop->IsChecked;
}

//---------------------------------------------------------------------------

void __fastcall TFormCube::CheckBoxBottomChange(TObject *Sender)
{
  GBECubeExtend1->FaceBottomVisible = CheckBoxBottom->IsChecked;
}
//---------------------------------------------------------------------------

