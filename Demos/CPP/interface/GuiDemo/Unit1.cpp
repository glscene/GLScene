//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBitmapFont"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGui"
#pragma link "GLMaterial"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLWindows"
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
  GLForm1->Caption = "Unicode caption..."; //#$0699#$069A#$963f#$54c0;
  WindowsBitmapFont1->EnsureString(GLForm1->Caption);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  GLForm1->DoChanges();
   // make things move a little
   GLSceneViewer1->Invalidate();

}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   miFPS->Caption = Format("%.1f FPS",
	  ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond())));
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::WindowsFont1Click(TObject *Sender)
{
   FontDialog1->Font = WindowsBitmapFont1->Font;
   if (FontDialog1->Execute())
	  WindowsBitmapFont1->Font = FontDialog1->Font;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
  GLForm1->MouseDown(Sender,TGLMouseButton(Button),Shift,X,Y);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
  GLForm1->MouseMove(Sender,Shift,X,Y);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseUp(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
  GLForm1->MouseUp(Sender,TGLMouseButton(Button),Shift,X,Y);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)

{
  GLForm1->KeyDown(Sender,Key,Shift);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyPress(TObject *Sender, System::WideChar &Key)
{
  GLForm1->KeyPress(Sender,Key);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyUp(TObject *Sender, WORD &Key, TShiftState Shift)
{
  GLForm1->KeyUp(Sender,Key,Shift);
}
//---------------------------------------------------------------------------


void __fastcall TForm1::GLButton1ButtonClick(TObject *Sender)
{
  String OldCaption;

  OldCaption = GLForm1->Caption;
  GLForm1->Caption = GLEdit1->Caption;
  GLEdit1->Caption = OldCaption;
}
//---------------------------------------------------------------------------

