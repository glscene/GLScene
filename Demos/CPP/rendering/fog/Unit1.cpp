//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLCadencer"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLTexture"
#pragma link "GLWin32Viewer"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
#pragma resource "*.dfm"
TForm1 *Form1;

#define cSpacing  2
#define cEdgeLength 0.7
#define cNb 4

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
		: TForm(Owner)
{
  int X, Y, Z;
  TGLCube *Cube;

  SetGLSceneMediaDir();
  GLMaterialLibrary1->AddTextureMaterial("glscene", "glscene.bmp",true);
  for (X=-cNb; X<cNb; X++)
	for (Y=-cNb; Y<cNb; Y++)
	  for (Z=-cNb; Z<cNb; Z++)
		if ((X & Y & Z) != 0)
		{
		  Cube = (TGLCube *) GLDummyCube1->AddNewChild(__classid(TGLCube));
		  Cube->Material->MaterialLibrary = GLMaterialLibrary1;
          Cube->Material->LibMaterialName = "glscene";
          Cube->Position->SetPoint(X * cSpacing, Y * cSpacing, Z * cSpacing);
          Cube->CubeWidth = cEdgeLength;
          Cube->CubeHeight = cEdgeLength;
          Cube->CubeDepth = cEdgeLength;
        }
  ApplyFogSettings();
}
//---------------------------------------------------------------------------
void TForm1::ApplyFogSettings(void)
{
  TGLFogEnvironment *fog = GLSceneViewer1->Buffer->FogEnvironment;

  fog->FogMode = (TFogMode) RGFogMode->ItemIndex;
  fog->FogDistance = (TFogDistance) RGFogDistance->ItemIndex;
  fog->FogColor->AsWinColor = SFogColor->Brush->Color;
  fog->FogColor->Alpha = (float)StrToInt(EFogDensity->Text) / 1000;
  if (CBApplyToBackground->Checked)
    GLSceneViewer1->Buffer->BackgroundColor = SFogColor->Brush->Color;
  fog->FogStart = StrToInt(EFogStart->Text);
  fog->FogEnd = StrToInt(EFogEnd->Text);

  GLSceneViewer1->Buffer->FogEnable = CBFogEnable->Checked;
  GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
  if (Shift.Contains(ssLeft))
  {
	GLCamera1->MoveAroundTarget(my-Y, mx-X);
    mx = X;
    my = Y;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CBFogEnableClick(TObject *Sender)
{
  ApplyFogSettings();        
}
//---------------------------------------------------------------------------

void __fastcall TForm1::EFogStartChange(TObject *Sender)
{
  if (((TEdit *)(Sender))->Text != "")
    ApplyFogSettings();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::SFogColorMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  if (ColorDialog1->Execute())
  {
    SFogColor->Brush->Color = ColorDialog1->Color;
    ApplyFogSettings();
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RGFogModeClick(TObject *Sender)
{
  ApplyFogSettings();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CBApplyToBackgroundClick(TObject *Sender)
{
  ApplyFogSettings();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CBTextureEnabledClick(TObject *Sender)
{
  GLMaterialLibrary1->Materials->Items[0]->Material->Texture->Enabled = CBTextureEnabled->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CBTextureIgnoreFogClick(TObject *Sender)
{
  if (CBTextureIgnoreFog->Checked)
    GLMaterialLibrary1->Materials->Items[0]->Material->MaterialOptions << moIgnoreFog;
  else
    GLMaterialLibrary1->Materials->Items[0]->Material->MaterialOptions >> moIgnoreFog;
  ApplyFogSettings();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
 GLCamera1->AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
}
//---------------------------------------------------------------------------

