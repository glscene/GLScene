//---------------------------------------------------------------------------
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBehaviours"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLContext"
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
   SetGLSceneMediaDir();
   // dynamically create 2 materials and load 2 textures
   GLMaterialLibrary->AddTextureMaterial("wood", "ashwood.jpg")->Material->FrontProperties->Emission->Color = clrGray50;
   GLMaterialLibrary->AddTextureMaterial("wood", "ashwood.jpg")->Material->FaceCulling = fcNoCull;
   GLMaterialLibrary->AddTextureMaterial("stone","walkway.jpg")->Material->FrontProperties->Emission->Color = clrGray50;
   GLMaterialLibrary->AddTextureMaterial("stone","walkway.jpg")->Material->FaceCulling = fcNoCull;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DirectOpenGL1Render(TObject *Sender, TGLRenderContextInfo &rci)

{
   TGLLibMaterial *material;

   // disable face culling
   glDisable(GL_CULL_FACE);

   // 1st quad, textured with 'wood', using standard method
   GLMaterialLibrary->ApplyMaterial("wood", rci);

   glBegin(GL_QUADS);
	  glTexCoord2f(0, 1);  glVertex3f(0.5, 0.5, -0.5);
	  glTexCoord2f(0, 0);  glVertex3f(-0.5, 0.5, -0.5);
	  glTexCoord2f(1, 0);  glVertex3f(-0.5, 0, 0.5);
	  glTexCoord2f(1, 1);  glVertex3f(0.5, 0, 0.5);
   glEnd;
   GLMaterialLibrary->UnApplyMaterial(rci);
   // 2nd quad, textured with 'stone'
   // we "manually" apply the material, this can be usefull if you want to have
   // some dynamic material control
   material = GLMaterialLibrary->Materials->GetLibMaterialByName("stone");
//   material->Material->Apply(rci);  - unconsistent content
   glBegin(GL_QUADS);
	  glTexCoord2f(0, 1);  glVertex3f(0.5, -0.5, -0.5);
	  glTexCoord2f(0, 0);  glVertex3f(0.5, 0, 0.5);
	  glTexCoord2f(1, 0);  glVertex3f(-0.5, 0, 0.5);
	  glTexCoord2f(1, 1);  glVertex3f(-0.5, -0.5, -0.5);
   glEnd;
   material->Material->UnApply(rci);
   // enable face culling again
   glEnable(GL_CULL_FACE);

}
//---------------------------------------------------------------------------
