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
#pragma link "GLFBORenderer"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
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
  Triger = false;
  FramerateRatio = 1;
  N = 0;
  GLCube1->Material->LibMaterialName = GLFBORenderer2->ColorTextureName;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  GLSceneViewer1->ResetPerformanceMonitor();
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

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  GLSceneViewer1->Invalidate();
  GLCube1->Turn(60*deltaTime);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLDirectOpenGL1Render(TObject *Sender, TGLRenderContextInfo &rci)

{
  N++;
  if (N >= FramerateRatio)
  {
	GLFBORenderer2->Active = Triger;
	Triger =!Triger;
	GLFBORenderer1->Active = Triger;
	N = 0;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLFBORenderer1AfterRender(TObject *Sender, TGLRenderContextInfo &rci)

{
  GLCube1->Material->LibMaterialName = GLFBORenderer1->ColorTextureName;
  GLFBORenderer1->Active = false;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLFBORenderer2AfterRender(TObject *Sender, TGLRenderContextInfo &rci)

{
  GLCube1->Material->LibMaterialName = GLFBORenderer2->ColorTextureName;
  GLFBORenderer2->Active = false;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::SBClick(TObject *Sender)
{
  int Size;
  switch (SB->ItemIndex) {
	case 0: Size = 256; break;
	case 1: Size = 512; break;
	case 2: Size = 2048; break;
  default:
	  ;
  }
  GLFBORenderer1->Width = Size;
  GLFBORenderer1->Height = Size;
  GLFBORenderer2->Width = Size;
  GLFBORenderer2->Height = Size;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::RBClick(TObject *Sender)
{
  switch (RB->ItemIndex) {
	case 0: FramerateRatio = 1; break;
	case 1: FramerateRatio = 2; break;
	case 2: FramerateRatio = 10; break;
  default:
	  ;
  }
}
//---------------------------------------------------------------------------

