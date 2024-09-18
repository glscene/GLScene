//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "fMultiTextureC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormMultiTexture *FormMultiTexture;
//---------------------------------------------------------------------------
__fastcall TFormMultiTexture::TFormMultiTexture(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormMultiTexture::FormCreate(TObject *Sender)
{
   TFileName Path = GetCurrentAssetPath();
   SetCurrentDir(Path  + "\\texture");
   // prepare images to merge in the multitexture
   Image1->Picture->LoadFromFile("ashwood.jpg");
   GLMaterialLibrary1->Materials->Items[0]->Material->Texture->Image->Assign(Image1->Picture);
   Image2->Picture->LoadFromFile("Flare1.bmp");
   GLMaterialLibrary1->Materials->Items[1]->Material->Texture->Image->Assign(Image2->Picture);
}
//---------------------------------------------------------------------------
void __fastcall TFormMultiTexture::Image1Click(TObject *Sender)
{
   // load a new Image1
   if (OpenPictureDialog1->Execute()) {
	  Image1->Picture->LoadFromFile(OpenPictureDialog1->FileName);
	  GLMaterialLibrary1->Materials->Items[0]->Material->Texture->Image->Assign(Image1->Picture);
   }
}
//---------------------------------------------------------------------------
void __fastcall TFormMultiTexture::Image2Click(TObject *Sender)
{
   // load a new Image2
   if (OpenPictureDialog1->Execute())  {
	  Image2->Picture->LoadFromFile(OpenPictureDialog1->FileName);
	  GLMaterialLibrary1->Materials->Items[1]->Material->Texture->Image->Assign(Image2->Picture);
   }
}
//---------------------------------------------------------------------------
void __fastcall TFormMultiTexture::TrackBar1Change(TObject *Sender)
{
   // adjust scale
   GLMaterialLibrary1->Materials->Items[1]->TextureScale->X = TrackBar1->Position/10;
   GLMaterialLibrary1->Materials->Items[1]->TextureScale->Y = TrackBar1->Position/10;
}
//---------------------------------------------------------------------------
