//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fReflectC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.CgShader"
#pragma link "GLScene.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLScene.Coordinates"
#pragma link "GLS.GeomObjects"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.VectorFileObjects"
#pragma resource "*.dfm"

TFormRef *FormRef;
float  ref;

//---------------------------------------------------------------------------
__fastcall TFormRef::TFormRef(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
