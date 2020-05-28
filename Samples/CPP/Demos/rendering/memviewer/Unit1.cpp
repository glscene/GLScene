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
  textureFramerateRatio = 1;
  n  = 0;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RB1to1Click(TObject *Sender)
{
  textureFramerateRatio = RB1to1->Tag;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
  if (CheckBox1->Checked)
	GLSceneViewer1->VSync = vsmSync;
  else
	GLSceneViewer1->VSync = vsmNoSync;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1AfterRender(TObject *Sender)
{
  if (!GLSceneViewer1->Buffer->RenderingContext->GL->W_ARB_pbuffer)
  {
	ShowMessage("WGL_ARB_pbuffer not supported...\r\n\
	  Get newer graphics hardware or try updating your drivers!");
	GLSceneViewer1->AfterRender = NULL;
	exit;
  }
  n++;
  try {
	if (n >= textureFramerateRatio)
	{
	  // render to the viewer
	  GLMemoryViewer1->Render();
	  // copy result to the textures
	  GLMemoryViewer1->CopyToTexture(Cube1->Material->Texture);
	  n = 0;
	}

  } catch (...) {
	// pbuffer not supported... catchall for exotic ICDs...
	GLSceneViewer1->AfterRender = NULL;
	exit; //raise;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  LabelFPS->Caption = Format("%.1f FPS", ARRAYOFCONST((GLSceneViewer1->FramesPerSecond())));
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  DummyCube1->TurnAngle = newTime * 60;
}
//---------------------------------------------------------------------------
