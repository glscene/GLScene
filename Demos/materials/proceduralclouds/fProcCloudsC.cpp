//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fProcCloudsC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.Texture"
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormClouds *FormClouds;
//---------------------------------------------------------------------------
__fastcall TFormClouds::TFormClouds(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormClouds::FormCreate(TObject *Sender)
{
  CBFormat->ItemIndex = 3;
  CBCompression->ItemIndex = 0;
  //CBFormatChange(Sender);
}
//---------------------------------------------------------------------------

