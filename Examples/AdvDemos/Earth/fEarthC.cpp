//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fEarthC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLScene.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLScene.Coordinates"
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
