//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.GeomObjects"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLSL.ProjectedTextures"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLFileLMTS"
#pragma link "GLS.FileTGA"


#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  int I;

  for (I = 1; I < GLSLProjectedTextures1->Emitters->Count - 1; I++)
	GLSLProjectedTextures1->Emitters->Items[I]->Emitter->Turn(deltaTime * (I + 1) * 10);

  GLSceneViewer1->Invalidate();
  GLArrowLine1->Position->Y  = GLArrowLine1->Position->Y + sdir * deltaTime;
  if (GLArrowLine1->Position->Y > 20)
  {
	GLArrowLine1->Position->Y = 20;
	sdir = -10;
  }
  if (GLArrowLine1->Position->Y < 10)
  {
	GLArrowLine1->Position->Y = 10;
	sdir = 10;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCamera1CustomPerspective(const TRectangle &viewport, int width,
          int height, int DPI, float &viewPortRadius)
{
  * CurrentGLContext()->PipelineTransformation->ProjectionMatrix =
	CreatePerspectiveMatrix((float)GLCamera1->GetFieldOfView(Width)/2, (float)Width / Height,
							GLCamera1->NearPlaneBias, GLCamera1->DepthOfView);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
  if (Shift.Contains(ssLeft))
  {
	GLCamera1->MoveAroundTarget(my - Y, mx - X);
	mx = X;
	my = Y;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  GLCamera1->AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  Caption  = "GLSL Projected Texture " +GLSceneViewer1->FramesPerSecondText();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  int I;

  Randomize;
  sdir = -10;
  GLCamera1->CameraStyle = csCustom;

  SetGLSceneMediaDir();

  GLSLProjectedTextures1->Material->Texture->Image->LoadFromFile("flare1.bmp");
  GLSLProjectedTextures1->Material->Texture->Disabled = false;
  GLSLProjectedTextures1->Material->Texture->TextureWrap = twNone;
  GLSLProjectedTextures1->Material->Texture->MinFilter = miLinear;
  GLSLProjectedTextures1->Material->Texture->MagFilter = maLinear;
  GLSLProjectedTextures1->UseLightmaps = true;
  GLCube1->Material->Texture->Image->LoadFromFile("ashwood.jpg");
  GLCube1->Material->Texture->Disabled = false;


  GLFreeForm1->LoadFromFile("groundtest.lmts");
  GLFreeForm1->ObjectStyle = GLFreeForm1->ObjectStyle << osDirectDraw;

  for (I = 0; I < GLMaterialLibrary1->Materials->Count - 1; I++)
	GLMaterialLibrary1->Materials->Items[I]->Material->MaterialOptions =
	GLMaterialLibrary1->Materials->Items[I]->Material->MaterialOptions << moNoLighting;
}
//---------------------------------------------------------------------------
