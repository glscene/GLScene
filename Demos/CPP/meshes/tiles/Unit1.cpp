//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGraph"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLTilePlane"
#pragma link "GLWin32Viewer"
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
