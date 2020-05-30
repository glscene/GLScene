//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#include <System.Math.hpp>
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
#pragma link "GLPolyhedron"
#pragma link "GLScene"
#pragma link "GLSLShader"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFileSMD"
#pragma link "GLFileMD2"
#pragma link "GLFile3DS"
#pragma link "DDSImage"
#pragma link "GLFileMS3D"
#pragma link "GLBaseClasses"
#pragma link "GLSLBumpShader"
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
  int I;
  //First load models
  SetGLSceneMediaDir();
  Fighter->LoadFromFile("TRINITYrage.smd"); //Fighter
  Fighter->AddDataFromFile("walk.smd");
  Fighter->Animations->Items[1]->MakeSkeletalTranslationStatic();
  Fighter->AddDataFromFile("run.smd");
  Fighter->Animations->Items[2]->MakeSkeletalTranslationStatic();
  Fighter->AddDataFromFile("long_jump.smd");
  Fighter->Animations->Items[3]->MakeSkeletalTranslationStatic();
  Fighter->AddDataFromFile("jump.smd");
  Fighter->AddDataFromFile("look_left_right.smd");
  Fighter->Animations->Items[5]->MakeSkeletalRotationDelta();
  Fighter->SwitchToAnimation(1);
/*
  // or use a quake md2 model
  Fighter->LoadFromFile("waste.md2"); //Fighter
  Fighter->SwitchToAnimation(0, True);
  Fighter->AnimationMode = aamLoop;
  Fighter->Scale->Scale(3);
*/
  Fighter->AnimationMode = aamLoop;
  Fighter->Scale->Scale(3);
//  Fighter->MeshObjects->BuildTangentSpace;

  Teapot->LoadFromFile("Teapot.3ds"); //Teapot
  Teapot->Scale->Scale(0.8);
  //  Teapot.MeshObjects->BuildTangentSpace; does not have texture coordinates...

  Sphere_big->LoadFromFile("Sphere_big.3DS"); //Sphere_big
  Sphere_big->Scale->Scale(70);
  Sphere_big->MeshObjects->BuildTangentSpace();

  Sphere_little->LoadFromFile("Sphere_little.3ds"); //Sphere_little
  Sphere_little->Scale->Scale(4);
  Sphere_little->MeshObjects->BuildTangentSpace();

  // Then load textures
  MaterialLibrary->LibMaterialByName("Earth")->Material->Texture->Image->LoadFromFile("Earth.jpg");
  MaterialLibrary->LibMaterialByName("EarthGross")->Material->Texture->Image->LoadFromFile("EarthSpec.dds");
  MaterialLibrary->LibMaterialByName("EarthNormals")->Material->Texture->Image->LoadFromFile("EarthNormals.jpg");

  // Create Shader
  MultiLightShader = new TGLSLMLBumpShader(this);
  MultiLightShader->LightSources = MultiLightShader->LightSources << 1, 2 ;
  MultiLightShader->LightCompensation = 0.7;
  MultiLightShader->NormalTexture = MaterialLibrary->LibMaterialByName("EarthNormals")->Material->Texture;
  MultiLightShader->SpecularTexture = MaterialLibrary->LibMaterialByName("EarthGross")->Material->Texture;

  // Attach shader to the material
  MaterialLibrary->LibMaterialByName("Earth")->Shader = MyBumpShader;
  for (I = 0; I < TrinityMatlib->Materials->Count - 1; I++)
	TrinityMatlib->Materials->Items[I]->Shader = MyBumpShader;

  ShowNotGLSceneObjectsCheckBoxClick(this);
  MyBumpShader->Enabled = ShaderEnabledCheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ShaderEnabledCheckBoxClick(TObject *Sender)
{
  MyBumpShader->Enabled = ShaderEnabledCheckBox->Checked;
  MultiLightShader->Enabled = ShaderEnabledCheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ShowNotGLSceneObjectsCheckBoxClick(TObject *Sender)
{
  Teapot->Visible = ShowNotGLSceneObjectsCheckBox->Checked;
  Fighter->Visible = ShowNotGLSceneObjectsCheckBox->Checked;
  GLCube->Visible = ShowNotGLSceneObjectsCheckBox->Checked;
  GLDodecahedron->Visible = ShowNotGLSceneObjectsCheckBox->Checked;
  GLSphere->Visible = ShowNotGLSceneObjectsCheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MultiLightShaderCheckBoxClick(TObject *Sender)
{
  int I;
  if (MultiLightShaderCheckBox->Checked)
  {
	MaterialLibrary->LibMaterialByName("Earth")->Shader = MultiLightShader;
	for (I = 0; I < TrinityMatlib->Materials->Count - 1; I++)
	  TrinityMatlib->Materials->Items[I]->Shader = MultiLightShader;
  }
  else
  {
	MaterialLibrary->LibMaterialByName("Earth")->Shader = MyBumpShader;
	for (I = 0; I < TrinityMatlib->Materials->Count - 1; I++)
	  TrinityMatlib->Materials->Items[I]->Shader = MyBumpShader;

  }
  Light2->Shining = MultiLightShaderCheckBox->Checked;
  LightCube2->Visible = MultiLightShaderCheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::UseSpecularTextureCheckBoxClick(TObject *Sender)
{
  if (UseSpecularTextureCheckBox->Checked) {
	MyBumpShader->SpecularTexture = MaterialLibrary->LibMaterialByName("EarthGross")->Material->Texture;
	MultiLightShader->SpecularTexture = MaterialLibrary->LibMaterialByName("EarthGross")->Material->Texture;
  }
  else {
	MyBumpShader->SpecularTexture = NULL;
	MultiLightShader->SpecularTexture = NULL;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::UseNormalTextureCheckBoxClick(TObject *Sender)
{
  if (UseNormalTextureCheckBox->Checked) {
	MyBumpShader->NormalTexture = MaterialLibrary->LibMaterialByName("EarthNormals")->Material->Texture;
	MultiLightShader->NormalTexture = MaterialLibrary->LibMaterialByName("EarthNormals")->Material->Texture;
  }
  else {
	MyBumpShader->NormalTexture = NULL;
	MultiLightShader->NormalTexture = NULL;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CadencerProgress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  Viewer->Invalidate();

  if (RollPitchTurnCheckBox->Checked)
  {
	Sphere_big->Turn(deltaTime * 40);
	Sphere_big->Roll(deltaTime * 40);
	Sphere_little->Pitch(deltaTime * 20);
	Fighter->Roll(deltaTime * 20);
	Teapot->Roll(-deltaTime * 10);
	GLCube->Pitch(-deltaTime * 10);
	GLDodecahedron->Pitch(deltaTime * 10);
	GLSphere->Roll(-deltaTime * 10);
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y)
{
  if (Shift.Contains(ssRight) && Shift.Contains(ssLeft))
	Camera->AdjustDistanceToTarget(Power(1.01, Y - my));
  else
  if (Shift.Contains(ssRight) || Shift.Contains(ssLeft))
	Camera->MoveAroundTarget(my - Y, mx - X);
  mx = X;
  my = Y;

}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  Caption = "GLSL Bump Shader - " + Viewer->FramesPerSecondText();
  Viewer->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  Camera->AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
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

