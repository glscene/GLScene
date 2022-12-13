//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "fShaderCompC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLSL.CustomShader"
#pragma link "GLS.GeomObjects"
#pragma link "GLS.Graph"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLSL.Shader"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.FileSMD"
#pragma link "GLS.FileMD2"
#pragma link "GLS.File3DS"
#pragma link "GLS.FileMS3D"

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
  TFileName Path = GetCurrentAssetPath();

  //First load scripts from shader asset directory
  SetCurrentDir(Path  + "\\shader");
  GLSLShader->LoadShaderPrograms("shader.vert","shader.frag");
  GLSLShader->Enabled = true;

  //Second load static models
  SetCurrentDir(Path  + "\\model");
  Teapot->LoadFromFile("Teapot.3ds"); //Teapot has no texture coordinates
  Teapot->Scale->Scale(0.8);

  Sphere_big->LoadFromFile("Sphere_big.3DS"); //Sphere_big
  Sphere_big->Scale->Scale(70);

  Sphere_little->LoadFromFile("Sphere_little.3ds"); //Sphere_little
  Sphere_little->Scale->Scale(4);

  //Third loading dynamic models with skeletal animation
  SetCurrentDir(Path  + "\\modelext");
  Fighter->LoadFromFile("waste.md2"); //Fighter
  Fighter->SwitchToAnimation(0, true);
  Fighter->AnimationMode = aamLoop;
  Fighter->Scale->Scale(3);

  // Then load textures.
  SetCurrentDir(Path  + "\\texture");
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
