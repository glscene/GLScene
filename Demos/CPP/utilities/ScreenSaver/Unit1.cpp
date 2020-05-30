//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
#include "Unit2.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBehaviours"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLScreenSaver"
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
  switch (GetMeshResolutions())
  {
	// read our option
	case 0 :
	{// Low Res, this is one ugly torus
			Torus1->Rings = 8;
			Torus1->Sides = 6;
	} break;
	case  1 :
	{// High Res, should still look smooth at high resolutions
			Torus1->Rings = 64;
			Torus1->Sides = 32;
	}
  default: ;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormShow(TObject *Sender)
{
	// let the show begin :)
	GLCadencer1->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormResize(TObject *Sender)
{
	// "Rescale" when form size is changed so our saver always looks the same
	GLCamera1->FocalLength = (float)50*Width/400;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLScreenSaver1PropertiesRequested(TObject *Sender)
{
	// we create the dialog dans display it
	// we do not need to free it (TApplication will take care of this)
	Application->CreateForm(__classid(TForm2), &Form2);
	Form2->ShowModal();
}
//---------------------------------------------------------------------------

