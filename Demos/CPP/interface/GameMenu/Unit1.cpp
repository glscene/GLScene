//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBitmapFont"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLWindowsFont"

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
  GLMaterialLibrary1->Materials->Items[0]->Material->Texture->Image->LoadFromFile("GLScene.bmp");

  GameMenu = (TGLGameMenu *)GLScene1->Objects->AddNewChild(__classid(TGLGameMenu));
  GameMenu->MaterialLibrary = GLMaterialLibrary1;
  GameMenu->TitleMaterialName = "LibMaterial";
  GameMenu->TitleHeight = 80;
  GameMenu->TitleWidth = 200;
  GameMenu->Font = GLWindowsBitmapFont1;
  GameMenu->Items->Add("test line 1");
  GameMenu->Items->Add("test line 2");
  GameMenu->Items->Add("test line 3");
  GameMenu->Items->Add("test line 4");
  GameMenu->Items->Add("test line 5");
  GameMenu->Items->Add("test line 6");
  GameMenu->Spacing = 1;
  GameMenu->Selected = 0;
  GameMenu->Position->Y = 200;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)

{
  if (IsKeyDown('w') || IsKeyDown('W') || IsKeyDown(VK_UP))
	GameMenu->SelectPrev();
  if (IsKeyDown('s') || IsKeyDown('S') || IsKeyDown(VK_DOWN))
	GameMenu->SelectNext();
  if (IsKeyDown(VK_RETURN))
  {
	if (GameMenu->Selected != -1)
	  ShowMessage("You have selected option: " + GameMenu->SelectedText);
  }
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
  GameMenu->MouseMenuSelect(X, Y);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
  GameMenu->MouseMenuSelect(X, Y);
  if (GameMenu->Selected != -1)
	ShowMessage("You have selected option: " + GameMenu->SelectedText);

}
//---------------------------------------------------------------------------
void __fastcall TForm1::ShowTitleCheckboxClick(TObject *Sender)
{
  if (GameMenu->TitleHeight == 0)
	GameMenu->TitleHeight = 80;
  else
   GameMenu->TitleHeight = 0;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MainPanelResize(TObject *Sender)
{
  GameMenu->Position->X = MainPanel->Width / 2;
}

