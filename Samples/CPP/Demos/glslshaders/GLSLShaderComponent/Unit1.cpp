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
#pragma link "GLCustomShader"
#pragma link "GLGeomObjects"
#pragma link "GLGraph"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
#pragma link "GLSLShader"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFileSMD"
#pragma link "GLFileMD2"
#pragma link "GLFile3DS"
#pragma link "GLFileMS3D"

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
  //First load scripts from shader directory in project dir
  GLSLShader->LoadShaderPrograms("Shaders\\Shader.Vert","Shaders\\Shader.Frag");
  GLSLShader->Enabled = true;


  //Second load models from media directory

  Fighter->LoadFromFile("waste.md2"); //Fighter
  Fighter->SwitchToAnimation(0, true);
  Fighter->AnimationMode = aamLoop;
  Fighter->Scale->Scale(3);

  Teapot->LoadFromFile("Teapot.3ds"); //Teapot (no texture coordinates)
  Teapot->Scale->Scale(0.8);

  Sphere_big->LoadFromFile("Sphere_big.3DS"); //Sphere_big
  Sphere_big->Scale->Scale(70);

  Sphere_little->LoadFromFile("Sphere_little.3ds"); //Sphere_little
  Sphere_little->Scale->Scale(4);

  // Then load textures.
  MaterialLibrary->LibMaterialByName("Earth")->Material->Texture->Image->LoadFromFile("Earth.jpg");


}
//---------------------------------------------------------------------------
void __fastcall TForm1::ShadeEnabledCheckBoxClick(TObject *Sender)
{
  GLSLShader->Enabled = ShadeEnabledCheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSLShaderApply(TGLCustomGLSLShader *Shader)
{
	Shader->Param["DiffuseColor"]->AsVector4f = VectorMake(1, 1, 1, 1);
	Shader->Param["AmbientColor"]->AsVector4f = VectorMake(0.2, 0.2, 0.2, 1);
	Shader->Param["LightIntensity"]->AsVector1f = 1;
	Shader->SetTex("MainTexture", MaterialLibrary->LibMaterialByName("Earth")->Material->Texture);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSLShaderInitialize(TGLCustomGLSLShader *Shader)
{
	// Do nothing.
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSLShaderUnApply(TGLCustomGLSLShader *Shader, bool &ThereAreMorePasses)

{
  // Do nothing.
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CadencerProgress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  Viewer->Invalidate();
  if (PitchRollTurnCheckBox->Checked) {
	Sphere_big->Pitch(40 * deltaTime);
	Sphere_big->Turn(40 * deltaTime);
	Sphere_little->Roll(40 * deltaTime);
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::LightCubeProgress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  if (LightMovingCheckBox->Checked)
	LightCube->MoveObjectAround(Camera->TargetObject, sin(newTime) * deltaTime * 10, deltaTime * 20);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  Cadencer->Enabled = false;
}
//---------------------------------------------------------------------------
