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
#pragma link "GLFBORenderer"
#pragma link "GLGeomObjects"
#pragma link "GLHUDObjects"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
#pragma link "GLSLShader"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFileDDS"
#pragma link "GLFileMD2"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TForm1::PrepareShadowMappingRender(TObject *Sender, TGLRenderContextInfo &rci)

{
  // prepare shadow mapping matrix
  FInvCameraMatrix = rci.PipelineTransformation->InvModelViewMatrix;
  // go from eye space to light's "eye" space
  FEyeToLightMatrix = MatrixMultiply(FInvCameraMatrix, FLightModelViewMatrix);
  // then to clip space
  FEyeToLightMatrix = MatrixMultiply(FEyeToLightMatrix, FLightProjMatrix);
  // and finally make the [-1..1] coordinates into [0..1]
  FEyeToLightMatrix = MatrixMultiply(FEyeToLightMatrix, FBiasMatrix);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  // Loading textures
  SetGLSceneMediaDir();

  //with GLMaterialLibrary1 do
  GLMaterialLibrary1->TextureByName("Chekers")->Image->LoadFromFile("marbletiles.jpg");
  GLMaterialLibrary1->TextureByName("Chekers")->Disabled = false;

  GLMaterialLibrary1->TextureByName("Chekers2")->Image->LoadFromFile("concrete.jpg");
  GLMaterialLibrary1->TextureByName("Chekers2")->Disabled = false;

  GLMaterialLibrary1->TextureByName("Lightspot")->Image->LoadFromFile("flare1.bmp");
  GLMaterialLibrary1->TextureByName("Lightspot")->Disabled = false;

  GLMaterialLibrary1->TextureByName("bark")->Image->LoadFromFile("waste.jpg");
  GLMaterialLibrary1->TextureByName("bark")->Disabled = false;

  GLMaterialLibrary1->TextureByName("mask")->Image->LoadFromFile("masks.dds");
  GLMaterialLibrary1->TextureByName("mask")->Disabled = false;

  // Loading models
  GLFreeForm1->LoadFromFile("waste.md2");
  GLFreeForm1->Scale->Scale(0.05);
  GLFreeForm1->Position->Y = GLPlane1->Position->Y + 0.6;

  FBiasMatrix =
	CreateScaleAndTranslationMatrix(VectorMake(0.5, 0.5, 0.5), VectorMake(0.5, 0.5, 0.5));

  // Loading shader
  GLSLShader1->VertexProgram->LoadFromFile("shadowmap_vp.glsl");
  GLSLShader1->FragmentProgram->LoadFromFile("shadowmapvis_fp.glsl");
  GLSLShader1->Enabled = true;

  GLSLShader2->VertexProgram->LoadFromFile("shadowmap_vp.glsl");
  GLSLShader2->FragmentProgram->LoadFromFile("shadowmap_fp.glsl");
  GLSLShader2->Enabled = true;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormResize(TObject *Sender)
{
  GLSceneViewer1->Camera->SceneScale = GLSceneViewer1->ClientWidth / 400;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  GLTorus1->Turn(deltaTime * 25);
  GLCylinder1->Turn(deltaTime * 50);
  GLCamera2->Position->Rotate(VectorMake(0, 1, 0), deltaTime * 0.1);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSLShader1Apply(TGLCustomGLSLShader *Shader)
{
  Shader->Param["ShadowMap"]->AsTexture2D[0] =
	GLMaterialLibrary1->TextureByName(LightFBORenderer->DepthTextureName);
  // set compare to none so we can read off the depth value directly
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_NONE);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSLShader1UnApply(TGLCustomGLSLShader *Shader, bool &ThereAreMorePasses)

{
  // reset the compare mode to default
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_COMPARE_R_TO_TEXTURE_ARB);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSLShader2Apply(TGLCustomGLSLShader *Shader)
{
  Shader->Param["ShadowMap"]->AsTexture2D[1] =
	GLMaterialLibrary1->TextureByName(LightFBORenderer->DepthTextureName);
  Shader->Param["LightspotMap"]->AsTexture2D[2] =
	GLMaterialLibrary1->TextureByName("Lightspot");
  Shader->Param["Scale"]->AsFloat = 16.0;
  Shader->Param["Softly"]->AsInteger = 1;
  Shader->Param["EyeToLightMatrix"]->AsMatrix4f = FEyeToLightMatrix;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSLShader2Initialize(TGLCustomGLSLShader *Shader)
{
  Shader->Param["TextureMap"]->AsTexture2D[0] =
	GLMaterialLibrary1->TextureByName("Chekers2");
  Shader->Param["ShadowMap"]->AsTexture2D[1] =
	GLMaterialLibrary1->TextureByName(LightFBORenderer->DepthTextureName);
  Shader->Param["LightspotMap"]->AsTexture2D[2] =
	GLMaterialLibrary1->TextureByName("Lightspot");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1BeforeRender(TObject *Sender)
{
  if ((GLSceneViewer1->Buffer->RenderingContext->GL->EXT_framebuffer_object) = false)
  {
	ShowMessage("Sorry, this demo requires GL_EXT_framebuffer_object and either");
	Close();
  }
}
//---------------------------------------------------------------------------
