//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Graph"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.TilePlane"
#pragma link "GLS.SceneViewer"
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
   int i, j;
   SetGLSceneMediaDir();
   GLMaterialLibrary->TexturePaths = GetCurrentDir();

  GLMaterialLibrary->LibMaterialByName("beigemarble")->Material->Texture->Image->LoadFromFile("beigemarble.jpg");
  GLMaterialLibrary->LibMaterialByName("marbletiles")->Material->Texture->Image->LoadFromFile("marbletiles.jpg");
  GLMaterialLibrary->LibMaterialByName("walkway")->Material->Texture->Image->LoadFromFile("walkway.jpg");

   // fill the tiled area with random tiles
   RandSeed = 0;

 /*
   for (i=-20;i<=20;i++)
	 for (j=-20;j<=20;j++)
	  GLTilePlane->Tiles[i,j] = Random(GLMaterialLibrary->Materials->Count-1)+1;
*/
   // set all tile materials to anisotropic,
   // add them to the material selection combo
   for (i=0;i<(GLMaterialLibrary->Materials->Count-1);i++)
   {
	 GLMaterialLibrary->Materials->Items[i]->Material->Texture->FilteringQuality = tfAnisotropic;
		 CBMaterial->Items->Add(GLMaterialLibrary->Materials->Items[i]->Name);
   }
   CBMaterial->ItemIndex = 0;

}
//---------------------------------------------------------------------------
