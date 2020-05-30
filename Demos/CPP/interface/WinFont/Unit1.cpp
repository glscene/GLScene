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
#pragma link "GLScene"
#pragma link "GLTeapot"
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
   // sorry, couldn't resist again...
   HUDText1->Text = "Lorem ipsum dolor sit amer, consectetaur adipisicing elit,\r\
				  sed do eiusmod tempor incididunt ut labore et dolore magna\r\
				  aliqua. Ut enim ad minim veniam, quis nostrud exercitation\r\
				  ullamco laboris nisi ut aliquip ex ea commodo consequat.\r\
				  Duis aute irure dolor in reprehenderit in voluptate velit\r\
				  esse cillum dolore eu fugiat nulla pariatur. Excepteur sint\r\
				  occaecat cupidatat non proident, sunt in culpa qui officia\r\
				  deserunt mollit anim id est laborum.\r\
				  Woblis ten caracuro Zapothek it Setag!"; // I needed an uppercase 'W' too...

  HUDText1->Text = HUDText1->Text + "Unicode text..." +
	WideChar(0x0699)+WideChar(0x069A)+WideChar(0x963f)+WideChar(0x54c0);
  WindowsBitmapFont1->EnsureString(HUDText1->Text);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MIPickFontClick(TObject *Sender)
{
   FontDialog1->Font = WindowsBitmapFont1->Font;
   if (FontDialog1->Execute())
   {
	  WindowsBitmapFont1->Font = FontDialog1->Font;
	  HUDText1->ModulateColor->AsWinColor = FontDialog1->Font->Color;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MIViewTextureClick(TObject *Sender)
{
   {
	  Form2->Image1->Picture = WindowsBitmapFont1->Glyphs;
	  Form2->Width = Form2->Image1->Picture->Width;
	  Form2->Height = Form2->Image1->Picture->Height;
   }
   Form2->Show();

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
   // make things move a little
   HUDText2->Rotation = HUDText2->Rotation+15*deltaTime;
   HUDText3->Scale->X = sin(newTime)+1.5;
   HUDText3->Scale->Y = cos(newTime)+1.5;
   GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   MIFPS->Caption = Format("%.1f FPS - %d x %d Font Texture",
			 ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond(),
					WindowsBitmapFont1->FontTextureWidth(),
					WindowsBitmapFont1->FontTextureHeight())));
   GLSceneViewer1->ResetPerformanceMonitor();

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1Click(TObject *Sender)
{
  Teapot1->Visible = !Teapot1->Visible;
}
//---------------------------------------------------------------------------
