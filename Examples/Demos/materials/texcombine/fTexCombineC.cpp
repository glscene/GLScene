//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fTexCombineC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"
#pragma link "GLS.HUDObjects"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLSL.TextureShaders"
#pragma resource "*.dfm"
TFormCombine *FormCombine;
//---------------------------------------------------------------------------
__fastcall TFormCombine::TFormCombine(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormCombine::FormCreate(TObject *Sender)
{
  // load the textures
  PathToData = GetCurrentAssetPath();
  SetCurrentDir(PathToData  + "\\texture");
  Image1->Picture->LoadFromFile("beigemarble.jpg");
  GLMaterialLibrary->Materials->Items[0]->Material->Texture->Image->Assign(Image1->Picture);
  Image2->Picture->LoadFromFile("flare1.bmp");
  GLMaterialLibrary->Materials->Items[1]->Material->Texture->Image->Assign(Image2->Picture);
  Image3->Picture->LoadFromFile("clover.jpg");
  GLMaterialLibrary->Materials->Items[2]->Material->Texture->Image->Assign(Image3->Picture);
  Image4->Picture->LoadFromFile("concrete.jpg");
  GLMaterialLibrary->Materials->Items[3]->Material->Texture->Image->Assign(Image4->Picture);
  BUApplyClick(Sender);
  Application->HintHidePause = 30000;

}
//---------------------------------------------------------------------------
void __fastcall TFormCombine::BUApplyClick(TObject *Sender)
{
  // Apply new combiner code
  // Depending on shader and hardware, errors may be triggered during render
  GLTexCombineShader->Combiners->Clear();
  GLTexCombineShader->Combiners->AddStrings(MECombiner->Lines);
}
//---------------------------------------------------------------------------
void __fastcall TFormCombine::SceneViewerPostRender(TObject *Sender)
{
  // disable whatever texture units are not supported by the local hardware
  int n = SceneViewer->Buffer->LimitOf[limNbTextureUnits];
  PATex1->Visible = (n < 2);
  CBTex1->Enabled = (n >= 2);
  PATex2->Visible = (n < 3);
  CBTex2->Enabled = (n >= 3);
  PATex3->Visible = (n < 4);
  CBTex3->Enabled = (n >= 4);
  CBTex1->Checked = (CBTex1->Checked && CBTex1->Enabled);
}

//---------------------------------------------------------------------------
void __fastcall TFormCombine::Shape1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y)
{
  // Allow choosing the primary color
  ColorDialog->Color = Shape1->Brush->Color; //  PAPrimary->Color;
  if (ColorDialog->Execute())
  {
	Shape1->Brush->Color = ColorDialog->Color;
	GLMaterialLibrary->Materials->Items[0]->Material->FrontProperties->Diffuse->AsWinColor = ColorDialog->Color;
	SceneViewer->Invalidate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFormCombine::CBTex0Click(TObject *Sender)
{
  TGLLibMaterial* libMat;
  libMat = new TGLLibMaterial(GLMaterialLibrary->Materials);
  // This event is used for all 4 checkboxes of the 4 texture units
///  libMat = GLMaterialLibrary->Materials->GetLibMaterialByName((Sender as TCheckBox)->Caption);
///  if Assigned(libMat)
///	libMat->Material->Texture->Enabled = CBTex0(Sender)->Checked;

}
//---------------------------------------------------------------------------
