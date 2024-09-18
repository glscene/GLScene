//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fEarthC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"
#pragma link "GLS.LensFlare"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.SkyDome"
#pragma link "GLSL.TextureShaders"
#pragma resource "*.dfm"
TfrmEarth *frmEarth;
//---------------------------------------------------------------------------
__fastcall TfrmEarth::TfrmEarth(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
