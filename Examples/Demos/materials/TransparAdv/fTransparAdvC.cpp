//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fTransparAdvC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.BitmapFont"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"
#pragma link "GLS.FBORenderer"
#pragma link "GLS.GeomObjects"
#pragma link "GLS.HUDObjects"
#pragma link "GLS.Material"
#pragma link "GLS.Mesh"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLS.WindowsFont"
#pragma link "GLSL.CustomShader"
#pragma link "GLSL.Shader"
#pragma resource "*.dfm"
TFormTransparAdv *FormTransparAdv;
//---------------------------------------------------------------------------
__fastcall TFormTransparAdv::TFormTransparAdv(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
