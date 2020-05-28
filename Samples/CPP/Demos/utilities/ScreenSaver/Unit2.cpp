//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

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
#pragma link "GLSpaceText"
#pragma link "GLWin32Viewer"

#pragma resource "*.dfm"
TForm2 *Form2;

const String
	cSaverRegistryKey = "Software\\GLScene\\Samples\\CPP\\Demos\\ScreenSaver";
const String
	cSaverRegistryMeshResolutions = "MeshResolutions";

//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
int __fastcall GetMeshResolutions()
{
	TRegistry *reg;
	reg = new TRegistry;
	reg->OpenKey(cSaverRegistryKey, true);
	// If the value cannot be found, we default to hi-resolution
	if (reg->ValueExists(cSaverRegistryMeshResolutions))
	  return reg->ReadInteger(cSaverRegistryMeshResolutions);
	else
	  return 1;
	delete reg;
}
//---------------------------------------------------------------------------
void __fastcall  SetMeshResolutions(int MeshResolutions)
{
	TRegistry *reg;
	reg = new TRegistry;
	reg->OpenKey(cSaverRegistryKey, true);
	reg->WriteInteger(cSaverRegistryMeshResolutions, MeshResolutions);
	delete reg;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::FormCreate(TObject *Sender)
{
	// we highlight the current resolution
	SetSelected(GetMeshResolutions());
	SetHot(-1);
}
//---------------------------------------------------------------------------

void __fastcall TForm2::SetSelected(int nb)
{
	switch (nb)
	{
	case 0: Torus1->Material->FrontProperties->Emission->AsWinColor = clNavy;
	case 1:	Torus2->Material->FrontProperties->Emission->AsWinColor = clBlue; break;
	default:;
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm2::SetHot(int nb)
{
	switch (nb)
	{
	case 0: Torus1->Material->FrontProperties->Diffuse->AsWinColor = clGray;
	case 1:	Torus2->Material->FrontProperties->Diffuse->AsWinColor = clWhite; break;
	default:;
	}
}
void __fastcall TForm2::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
	TGLBaseSceneObject *bso;

	// Here I used the trick of setting Torus1.Tag=1 and Torus.Tag=2
	// other objects have a Tag of 0
	bso = GLSceneViewer1->Buffer->GetPickedObject(X, Y);
	if (bso && (bso->Tag > 0))
	  SetHot(bso->Tag-1);
	else
	  SetHot(-1);
}
//---------------------------------------------------------------------------
void __fastcall TForm2::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
	if (FLastHotNb >= 0)
	{
		SetSelected(FLastHotNb);
		SetMeshResolutions(FLastHotNb);
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm2::Button2Click(TObject *Sender)
{
  // a call to "Form1.ScreenSaver1.SetPassword;" would have done the same
  SetScreenSaverPassword;
}
//---------------------------------------------------------------------------
