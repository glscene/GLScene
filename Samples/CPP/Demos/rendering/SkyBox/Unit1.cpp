//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLLensFlare"
#pragma link "GLMaterial"
#pragma link "GLNavigator"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
#pragma link "GLSkyBox"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

TGLLibMaterial* __fastcall TForm1::LoadTexture(String Matname, String Filename)
{
  GLMaterialLibrary1->AddTextureMaterial(Matname, Filename);
  GLMaterialLibrary1->Materials->Items[0]->Material->Texture->Disabled = false;
  return GLMaterialLibrary1->Materials->GetLibMaterialByName(Matname);
}
void __fastcall TForm1::FormCreate(TObject *Sender)
{
	 SetGLSceneMediaDir();
	 // --------------
	 // Load graphics
	 // --------------
	 // Skybox textures
	 LoadTexture("Left","icecraterlf.jpg");
	GLMaterialLibrary1->Materials->Items[0]->Material->Texture->TextureMode = tmDecal;
	 LoadTexture("Right","icecraterrt.jpg");
	GLMaterialLibrary1->Materials->Items[1]->Material->Texture->TextureMode = tmDecal;
	 LoadTexture("Top","icecraterup.jpg");
	GLMaterialLibrary1->Materials->Items[2]->Material->Texture->TextureMode = tmDecal;
	 LoadTexture("Bottom","icecraterdn.jpg");
	GLMaterialLibrary1->Materials->Items[3]->Material->Texture->TextureMode = tmDecal;
	 LoadTexture("Front","icecraterft.jpg");
	GLMaterialLibrary1->Materials->Items[4]->Material->Texture->TextureMode = tmDecal;
	 LoadTexture("Back","icecraterbk.jpg");
	GLMaterialLibrary1->Materials->Items[5]->Material->Texture->TextureMode = tmDecal;
	 // Add transparency to clouds
	 LoadTexture("Clouds","Clouds.jpg")->Material->BlendingMode = bmTransparency;
	 LoadTexture("Clouds","Clouds.jpg")->Material->FrontProperties->Diffuse->Alpha = 0.2;

	  // scale the clouds texture
	  LoadTexture("Clouds","Clouds.jpg")->TextureScale->X = 8;
	  LoadTexture("Clouds","Clouds.jpg")->TextureScale->Y = 8;

	 // bricks
	  LoadTexture("Bricks","rawwall.jpg")->TextureScale->X = 1;
	  LoadTexture("Bricks","rawwall.jpg")->TextureScale->Y = 32;
	  LoadTexture("Bricks","rawwall.jpg")->Material->Texture->TextureMode = tmModulate;

	  LoadTexture("Bricks2","marbletiles.jpg")->TextureScale->X = 6;
	  LoadTexture("Bricks2","marbletiles.jpg")->TextureScale->Y = 1;
	  LoadTexture("Bricks2","marbletiles.jpg")->Material->Texture->TextureMode = tmModulate;

	 //Moon
	 LoadTexture("Moon","unwrapped moon.jpg")->Material->Texture->TextureMode = tmModulate;


	 //-----------------------------------------
	 // Assign materials to objects
	 //-----------------------------------------
	 GLCube1->Material->LibMaterialName = "Bricks";
	 GLCube11->Material->LibMaterialName  = "Bricks";
	 GLCube111->Material->LibMaterialName = "Bricks";
	 GLCube112->Material->LibMaterialName = "Bricks";
	 GLCube2->Material->LibMaterialName = "Bricks2";
	 GLCube21->Material->LibMaterialName = "Bricks2";
	 GLCube21->Material->LibMaterialName = "Bricks2";
	 GLCube211->Material->LibMaterialName = "Bricks2";
	 GLCube212->Material->LibMaterialName = "Bricks2";
	 GLSphere1->Material->LibMaterialName = "Moon";
	 GLSphere2->Material->LibMaterialName = "Moon";

	 GLUserInterface1->MouseLookActive = true;

}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
   // Make clouds Texture slide
   GLMaterialLibrary1->Materials->GetLibMaterialByName("Clouds")->TextureOffset->X =
	  GLMaterialLibrary1->Materials->GetLibMaterialByName("Clouds")->TextureOffset->X + deltaTime * 0.02;
   GLMaterialLibrary1->Materials->GetLibMaterialByName("Clouds")->TextureOffset->Y =
	  GLMaterialLibrary1->Materials->GetLibMaterialByName("Clouds")->TextureOffset->Y + deltaTime * 0.03;

   // Rotate moons
   GLSphere1->Turn(deltaTime * 7);
   GLSphere2->Turn(deltaTime * 10);

   HandleKeys(deltaTime);
   GLUserInterface1->MouseLook();
   GLUserInterface1->MouseUpdate();

   GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::HandleKeys(double d)
{
	if (IsKeyDown('W')||IsKeyDown('Z'))
		 GLCamera1->Move(d);
	if (IsKeyDown('S'))
		 GLCamera1->Move(-d);
	if (IsKeyDown('A') || IsKeyDown('A'))
		 GLCamera1->Slide(-d);
	if (IsKeyDown('D'))
		 GLCamera1->Slide(d);

	if(IsKeyDown(VK_SPACE))
	   Castle->Visible = !Castle->Visible;
	if(IsKeyDown(VK_ESCAPE))
	   Close();

}






