//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fcGLSViewer.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.AsyncTimer"
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"
#pragma link "GLS.Graph"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLS.VectorFileObjects"
#pragma resource "*.dfm"
TFormViewer *FormViewer;
//---------------------------------------------------------------------------
__fastcall TFormViewer::TFormViewer(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormViewer::tvSceneClick(TObject *Sender)
{
 //
}
//---------------------------------------------------------------------------
