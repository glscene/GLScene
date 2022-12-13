//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "fPostEffectC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Material"
#pragma link "GLPostEffects"
#pragma link "GLS.Scene"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLSL.PostEffects"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FormCreate(TObject *Sender)
{
  TFileName Path = GetCurrentAssetPath();
  SetCurrentDir(Path  + "\\model");

  GLActor1->LoadFromFile("waste.md2");

  SetCurrentDir(Path  + "\\texture");
  GLActor1->Material->Texture->Image->LoadFromFile("waste.jpg");
  GLActor1->Material->Texture->Enabled = True;
///  GLActor1->SwitchToAnimation(GLActor1->Animations[0]);

  GLActor1->AnimationMode = aamLoop;
///  GLActor1->ObjectStyle = GLActor1->ObjectStyle + [osDirectDraw];
  GLActor1->Reference = aarMorph;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ComboBox1Change(TObject *Sender)
{
  switch (ComboBox1->ItemIndex) {
	case 0: GLPostEffect1->Preset = pepNone; break;
	case 1: GLPostEffect1->Preset = pepGray; break;
	case 2: GLPostEffect1->Preset = pepNegative; break;
	case 3: GLPostEffect1->Preset = pepDistort; break;
	case 4: GLPostEffect1->Preset = pepNoise; break;
	case 5: GLPostEffect1->Preset = pepNightVision; break;
	case 6: GLPostEffect1->Preset = pepBlur; break;
	case 7: GLPostEffect1->Preset = pepCustom; break;

  default:;
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::GLPostEffect1CustomEffect(TObject *Sender, TGLRenderContextInfo &rci,
		  TGLPostEffectBuffer &Buffer)
{
  int i;

  for (i = 0; i <= 7; i++) {
	&Buffer[i]->r = (int)(&Buffer[i + 5]->r * 2);
	&Buffer[i]->g = (int)(&Buffer[i]->g * 1.5);
	&Buffer[i]->b = (int)(&Buffer[i + 5]->b * 1.5);
  }
}
//---------------------------------------------------------------------------
