//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "uMainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
#pragma link "GLPostEffects"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
#pragma link "GLVectorFileObjects"
#pragma link "GLSceneViewer"
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
  SetGLSceneMediaDir();
  GLActor1->LoadFromFile("waste.md2");
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

  default:
	  ;
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::GLPostEffect1CustomEffect(TObject *Sender, TGLRenderContextInfo &rci,
		  TGLPostEffectBuffer &Buffer)
{
  int i;

  for (i = 0; i <= 7; i++) {
	Buffer[i]->r = (int)(Buffer[i + 5]->r * 2);
	Buffer[i]->g = Round(Buffer[i]->g * 1.5);
	Buffer[i]->b = Round(Buffer[i + 5]->b * 1.5);
  }
}
//---------------------------------------------------------------------------
