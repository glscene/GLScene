//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "fTilesC.h"
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
TForm1* Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject* Sender)
{
	int i, j;
	TFileName Path = GetCurrentAssetPath();
	GLMaterialLibrary->TexturePaths = GetCurrentDir();

	GLMaterialLibrary->LibMaterialByName("beigemarble")
		->Material->Texture->Image->LoadFromFile("beigemarble.jpg");
	GLMaterialLibrary->LibMaterialByName("marbletiles")
		->Material->Texture->Image->LoadFromFile("marbletiles.jpg");
	GLMaterialLibrary->LibMaterialByName("walkway")
		->Material->Texture->Image->LoadFromFile("walkway.jpg");

	// fill the tiled area with random tiles
	RandSeed = 0;

   for (i=-20;i<=20;i++)
	 for (j=-20;j<=20;j++) {
	 // GLTilePlane->Tiles->Item[i][j] = 1; //Random(GLMaterialLibrary->Materials->Count-1)+1;
	 }

	// set all tile materials to anisotropic,
	// add them to the material selection combo
	for (i = 0; i < (GLMaterialLibrary->Materials->Count - 1); i++) {
		GLMaterialLibrary->Materials->Items[i]
			->Material->Texture->FilteringQuality = tfAnisotropic;
		CBMaterial->Items->Add(GLMaterialLibrary->Materials->Items[i]->Name);
	}
	CBMaterial->ItemIndex = 0;

/*

  // fill the tiled area with random tiles
  RandSeed := 0;
  for i := -20 to 20 do
	for j := -20 to 20 do
	  GLTilePlane.Tiles[i, j] := Random(GLMaterialLibrary.Materials.Count - 1) + 1;

  // set all tile materials to anisotropic,
  // add them to the material selection combo
  for i := 0 to GLMaterialLibrary.Materials.Count - 1 do
  begin
	GLMaterialLibrary.Materials[i].Material.Texture.FilteringQuality :=
	  tfAnisotropic;
	CBMaterial.Items.Add(GLMaterialLibrary.Materials[i].Name);
  end;
  CBMaterial.ItemIndex := 0;
 */

}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  GLCamera->AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLDirectOpenGLRender(TObject *Sender, TGLRenderContextInfo &rci)

{
  // we clear the depth buffer, so that the grid is always in front of the
  // tile plane and won't Z-Fight with it
  glClear(GL_DEPTH_BUFFER_BIT);
}
//---------------------------------------------------------------------------

