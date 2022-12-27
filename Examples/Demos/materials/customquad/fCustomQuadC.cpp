// ---------------------------------------------------------------------------
#pragma hdrstop

#include "fCustomQuadC.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Behaviours"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.GeomObjects"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.Context"
#pragma resource "*.dfm"
TForm1 *Form1;

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender) {
	TFileName Path = GetCurrentAssetPath();
	// dynamically create 2 materials and load 2 textures
	GLMaterialLibrary->AddTextureMaterial("wood", "ashwood.jpg")
		->Material->FrontProperties->Emission->Color = clrGray50;
	GLMaterialLibrary->AddTextureMaterial("wood", "ashwood.jpg")
		->Material->FaceCulling = fcNoCull;

	GLMaterialLibrary->AddTextureMaterial("stone", "walkway.jpg")
		->Material->FrontProperties->Emission->Color = clrGray50;
	GLMaterialLibrary->AddTextureMaterial("stone", "walkway.jpg")
		->Material->FaceCulling = fcNoCull;
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::DirectOpenGL1Render(TObject *Sender,
	TGLRenderContextInfo &rci)

{
	glDisable(GL_CULL_FACE);
	// 1st quad, textured with 'wood', using standard method
	GLMaterialLibrary->ApplyMaterial("wood", rci);
	glBegin(GL_QUADS);
	glTexCoord2f(0, 1);
	glVertex3f(0.5, 0.5, -0.5);
	glTexCoord2f(0, 0);
	glVertex3f(-0.5, 0.5, -0.5);
	glTexCoord2f(1, 0);
	glVertex3f(-0.5, 0, 0.5);
	glTexCoord2f(1, 1);
	glVertex3f(0.5, 0, 0.5);
	glEnd;
	GLMaterialLibrary->UnApplyMaterial(rci);
	// 2nd quad, textured with 'stone'
	// we could apply the material "manually", but this can be usefull if you want to have
	// some dynamic material control
	GLMaterialLibrary->ApplyMaterial("stone", rci);
	glBegin(GL_QUADS);
	glTexCoord2f(0, 1);
	glVertex3f(0.5, -0.5, -0.5);
	glTexCoord2f(0, 0);
	glVertex3f(0.5, 0, 0.5);
	glTexCoord2f(1, 0);
	glVertex3f(-0.5, 0, 0.5);
	glTexCoord2f(1, 1);
	glVertex3f(-0.5, -0.5, -0.5);
	glEnd;
	GLMaterialLibrary->UnApplyMaterial(rci);
	glEnable(GL_CULL_FACE);
}
// ---------------------------------------------------------------------------
