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
#pragma link "GLGeomObjects"
#pragma link "GLGraph"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLPostEffects"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFileMD2"
#pragma link "GLFileMS3D"
#pragma link "GLFile3DS"

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
  // First load models.
  SetGLSceneMediaDir();
  Fighter->LoadFromFile("waste.md2"); //Fighter
  Fighter->SwitchToAnimation(0, true);
  Fighter->AnimationMode = aamLoop;
  Fighter->Scale->Scale(2);

  Teapot->LoadFromFile("Teapot.3ds"); //Teapot (no texture coordinates)
  Teapot->Scale->Scale(0.8);

  Sphere_big->LoadFromFile("Sphere_big.3DS");
  Sphere_big->Scale->Scale(70);

  Sphere_little->LoadFromFile("Sphere_little.3ds");
  Sphere_little->Scale->Scale(4);

  // Then load textures.
  MaterialLibrary->LibMaterialByName("Earth")->Material->Texture->Image->LoadFromFile("Earth.jpg");
  MaterialLibrary->LibMaterialByName("Fighter")->Material->Texture->Image->LoadFromFile("Waste.jpg");
  MaterialLibrary->LibMaterialByName("Noise")->Material->Texture->Image->LoadFromFile("Flare1.bmp");

  // Blur Shader
  BlurShader = new TGLSLPostBlurShader(this);
  PostShaderHolder->Shaders->Add()->Shader = BlurShader;

  ShaderCheckListBox->Items->AddObject("Blur Shader", BlurShader);
  ShaderCheckListBox->Checked[0] = True;

  // Transformation Shader
  TransformationShader = new TGLCGPostTransformationShader(this);
  TransformationShader->TransformationTexture = MaterialLibrary->LibMaterialByName("Noise")->Material->Texture;
  PostShaderHolder->Shaders->Add()->Shader = TransformationShader;

  ShaderCheckListBox->Items->AddObject("Transformation Shader", TransformationShader);
  ShaderCheckListBox->Checked[1] = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CadencerProgress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  Viewer->Invalidate();

  if (TurnPitchrollCheckBox->Checked) {
	Fighter->Roll(20 * deltaTime);
	Sphere_big->Pitch(40 * deltaTime);
	Sphere_big->Turn(40 * deltaTime);
	Sphere_little->Roll(40 * deltaTime);
	Teapot->Roll(-20 * deltaTime);
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
void __fastcall TForm1::BigBlurThicknessCheckboxClick(TObject *Sender)
{
  if (BigBlurThicknessCheckbox->Checked)
	BlurShader->Threshold = 0.005;
  else
	BlurShader->Threshold = 0.2;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::ShaderCheckListBoxClick(TObject *Sender)
{
  int I;
  if (ShaderCheckListBox->Items->Count != 0)
	for (I = 0; I < ShaderCheckListBox->Items->Count - 1; I++) {
///	  TGLShader(ShaderCheckListBox->Items->Objects[I])->Enabled = ShaderCheckListBox->Checked[I];
	}
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  Cadencer->Enabled = false;
}
//---------------------------------------------------------------------------
