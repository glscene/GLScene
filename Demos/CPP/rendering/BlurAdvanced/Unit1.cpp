//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBlur"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLHUDObjects"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
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
  // Blur GLDummyCube1and it's children
  GLBlur1->TargetObject = GLDummyCube1;
  // point to GLDummyCube1
  GLCamera1->TargetObject = GLDummyCube1;
  // load materials
  GLMaterialLibrary1->Materials->Items[0]->Material->Texture->Image->LoadFromFile("beigemarble.jpg");
  GLMaterialLibrary1->Materials->Items[1]->Material->Texture->Image->LoadFromFile("moon.bmp");

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLBlur1BeforeTargetRender(TObject *Sender)
{
     TorusImpostor->Visible = true; // GLBlur1 must render the Torusimpostor
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLBlur1AfterTargetRender(TObject *Sender)
{
     TorusImpostor->Visible = false; // GLSCeneViewer1 must NOT render the Torusimpostor
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
     GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
	 if (Shift.Contains(ssRight) && (Y > 10))
		GLCamera1->AdjustDistanceToTarget(my/Y);
	 if (Shift.Contains(ssLeft))
		GLCamera1->MoveAroundTarget(my-Y,mx-X);
	 mx = X;
	 my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
	 LabelFPS->Caption  = GLSceneViewer1->FramesPerSecondText(0);
	 GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::trkAdvancedBlurHiClampChange(TObject *Sender)
{
	 GLBlur1->AdvancedBlurHiClamp = trkAdvancedBlurHiClamp->Position;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::trkAdvancedBlurLoClampChange(TObject *Sender)
{
	 GLBlur1->AdvancedBlurLowClamp = trkAdvancedBlurLoClamp->Position;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::edtAdvancedBlurAmpChange(TObject *Sender)
{
	 GLBlur1->AdvancedBlurAmp = StrToFloat(edtAdvancedBlurAmp->Text);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::edtAdvancedBlurPassesChange(TObject *Sender)
{
     GLBlur1->AdvancedBlurPasses = StrToInt(edtAdvancedBlurPasses->Text);
}
//---------------------------------------------------------------------------
