//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLAsyncTimer"
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCgShader"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFileMD2"

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
  float r;
  SetGLSceneMediaDir();

  // Load the vertex and fragment Cg programs from project dir
  CgCellShader->VertexProgram->LoadFromFile("Shaders\\cellshading_vp.cg");
  CgCellShader->FragmentProgram->LoadFromFile("Shaders\\cellshading_fp.cg");

  // Load and scale the actor from media dir
  GLActor1->LoadFromFile("waste.md2");

  r = GLActor1->BoundingSphereRadius();
  GLActor1->Scale->SetVector(2.5/r,2.5/r,2.5/r);
  GLActor1->AnimationMode = aamLoop;
  // Load the texture
  GLMaterialLibrary1->Materials->Items[0]->Material->Texture->Image->LoadFromFile("wastecell.jpg");

}
//---------------------------------------------------------------------------
void __fastcall TForm1::CgCellShaderApplyVP(TCgProgram *CgProgram, TObject *Sender)

{
	// Apply the per frame uniform parameters
	CgProgram->ParamByName("LightDir")->SetAsVector(GLLightSource1->AbsoluteDirection);
	CgProgram->ParamByName("ModelViewProj")->SetAsStateMatrix(CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);
	CgProgram->ParamByName("ModelViewIT")->SetAsStateMatrix(CG_GL_MODELVIEW_MATRIX, CG_GL_MATRIX_INVERSE_TRANSPOSE);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CgCellShaderInitialize(TCustomCgShader *CgShader)
{
  // Set up the texture sampler parameter
  CgCellShader->FragmentProgram->ParamByName("Map0")->SetAsTexture2D(
     GLMaterialLibrary1->Materials->Items[0]->Material->Texture->Handle);

}
//---------------------------------------------------------------------------
void __fastcall TForm1::CgCellShaderApplyFP(TCgProgram *CgProgram, TObject *Sender)

{
  // Enable the texture map sampler for use in the fragment
  // program
  CgProgram->ParamByName("Map0")->EnableTexture();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CgCellShaderUnApplyFP(TCgProgram *CgProgram)
{
  // Disable the texture map sampler
  CgProgram->ParamByName("Map0")->DisableTexture();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
  if (Shift.Contains(ssLeft))
	GLCamera1->MoveAroundTarget(my-Y,mx-X);
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::AsyncTimer1Timer(TObject *Sender)
{
  Form1->Caption = Format("Cg Cell Shading - %.2f FPS",
	ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond())));
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
