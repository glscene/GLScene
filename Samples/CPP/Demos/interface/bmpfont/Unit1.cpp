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
#pragma link "GLHUDObjects"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLTeapot"
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
   // Load the font bitmap from media dir
   SetGLSceneMediaDir();
   BitmapFont1->Glyphs->LoadFromFile("darkgold_font.bmp");
   // sorry, couldn't resist...

   HUDText1->Text = "Hello World !\r\
					 This is me, \r\
					 the HUD Text.\r\
					 Bitmap Fonts!";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
   // make things move a little
   HUDText2->Rotation = HUDText2->Rotation+15*deltaTime;
   HUDText3->Scale->X = 0.5*sin(newTime)+1;
   HUDText3->Scale->Y = 0.5*cos(newTime)+1;
   GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   HUDTextFPS->Text = Format("%.1f FPS",
	  ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond())));
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1Click(TObject *Sender)
{
  Teapot1->Visible = !Teapot1->Visible;
}
//---------------------------------------------------------------------------
