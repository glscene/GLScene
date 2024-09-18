//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMultiMaterialC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLSL.MultiMaterialShader"
#pragma link "GLSL.TextureShaders"
#pragma resource "*.dfm"
TForm1* Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject* Sender)
{
	TGLLibMaterial* LibMat;
	TFileName Path = GetCurrentAssetPath() + "\\texture";
	SetCurrentDir(Path);

	// GLMatLib1 is the source of the first image
	// Add the specular material using tmModulate for shiny text
    GLMatLib1->AddTextureMaterial("specular", "glscene_alpha.bmp");
    GLMatLib1->Materials->GetLibMaterialByName("specular")->Material->Texture->TextureMode = tmModulate;
  // use TextureMode := tmBlend; for shiny background
    GLMatLib1->Materials->GetLibMaterialByName("specular")->Material->BlendingMode = bmAdditive;
    GLMatLib1->Materials->GetLibMaterialByName("specular")->Texture2Name = "specular_tex2";

    GLMatLib1->AddTextureMaterial("specular_tex2", "rainbow.bmp");
    GLMatLib1->Materials->GetLibMaterialByName("specular_tex2")->Material->Texture->MappingMode = tmmCubeMapReflection;
    GLMatLib1->Materials->GetLibMaterialByName("specular_tex2")->Material->Texture->ImageBrightness = 0.3;

  // GLMatLib2 is the source of the GLMultiMaterialShader passes.

  // Pass 1: Base texture
  GLMatLib2->AddTextureMaterial("Pass1", "glscene.bmp"); // or use glscene_delphi.bmp  

  // Pass 2: Add a bit of detail
  GLMatLib2->AddTextureMaterial("Pass2", "detailmap.jpg");
  GLMatLib2->Materials->GetLibMaterialByName("Pass2")->Material->Texture->TextureMode = tmBlend;
  GLMatLib2->Materials->GetLibMaterialByName("Pass2")->Material->BlendingMode = bmAdditive;

   // Pass 3 : And a little specular reflection
   LibMat = new TGLLibMaterial(GLMatLib2->Materials);
   LibMat->Material->MaterialLibrary = GLMatLib1;
   LibMat->Material->LibMaterialName = "specular";

	// This isn't limited to 3, try adding some more passes!
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(
	TObject* Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	mx = X;
	my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheel(TObject* Sender, TShiftState Shift,
	int WheelDelta, TPoint &MousePos, bool &Handled)
{
    GLCamera1->AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
    Handled = true;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(
    TObject* Sender, const double deltaTime, const double newTime)
{
	GLCube1->Turn(deltaTime * 10);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(
    TObject* Sender, TShiftState Shift, int X, int Y)
{
    if (Shift.Contains(ssLeft))
        GLCamera1->MoveAroundTarget(my - Y, mx - X);
    mx = X;
    my = Y;
}
//---------------------------------------------------------------------------

