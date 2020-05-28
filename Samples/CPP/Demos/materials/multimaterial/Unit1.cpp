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
#pragma link "GLMaterial"
#pragma link "GLMultiMaterialShader"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLTexCombineShader"
#pragma link "GLWin32Viewer"
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

  // Add the specular pass
  // tmBlend for shiny background
  //Material->Texture->TextureMode = tmBlend;
  // tmModulate for shiny text
  GLMaterialLibrary1->AddTextureMaterial("specular","GLScene_alpha.bmp")->Material->Texture->TextureMode = tmModulate;
  GLMaterialLibrary1->AddTextureMaterial("specular","GLScene_alpha.bmp")->Material->BlendingMode = bmAdditive;
  GLMaterialLibrary1->AddTextureMaterial("specular","GLScene_alpha.bmp")->Texture2Name = "specular_tex2";
  GLMaterialLibrary1->AddTextureMaterial("specular_tex2","chrome_buckle.bmp")->Material->Texture->MappingMode = tmmCubeMapReflection;
  GLMaterialLibrary1->AddTextureMaterial("specular_tex2","chrome_buckle.bmp")->Material->Texture->ImageBrightness = 0.3;

  // GLMaterialLibrary2 is the source of the GLMultiMaterialShader
  // passes.
	// Pass 1 : Base texture
  GLMaterialLibrary2->AddTextureMaterial("Pass1","glscene.bmp");//}

	// Pass 2 : Add a bit of detail
  GLMaterialLibrary2->AddTextureMaterial("Pass2","detailmap.jpg")->Material->Texture->TextureMode=tmBlend;
  GLMaterialLibrary2->AddTextureMaterial("Pass2","detailmap.jpg")->Material->BlendingMode=bmAdditive;

	// Pass 3 : And a little specular reflection
  (new TGLLibMaterial(GLMaterialLibrary2->Materials))->Material->MaterialLibrary=GLMaterialLibrary1;
  (new TGLLibMaterial(GLMaterialLibrary2->Materials))->Material->LibMaterialName="specular";

	// This isn't limited to 3, try adding some more passes!
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
  mx=X;
  my=Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  GLCamera1->AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
  Handled = true;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  GLCube1->Turn(deltaTime*100);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
  if (Shift.Contains(ssLeft))
	GLCamera1->MoveAroundTarget(my-Y,mx-X);
  mx=X;
  my=Y;
}
//---------------------------------------------------------------------------

