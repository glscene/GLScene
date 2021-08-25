//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "fGuiPaintC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.BitmapFont"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Material"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.Windows"
#pragma link "GLS.WindowsFont"
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
  GLMaterialLibrary1->TexturePaths = GetCurrentDir();
  GLCanvas->MaxInvalidRenderCount = 40;
  StartX = -1;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  // set frame rate to 10 when program is not focused to reduce cpu usage...
  if (Form1->Focused())
	GLCadencer1->SleepLength = 0;
  else
	GLCadencer1->SleepLength = 100;

  // make things move a little
  GLForm1->DoChanges();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   miFPS->Caption = Format("%.1f FPS",
	 ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond())));
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::miWindowsFont1Click(TObject *Sender)
{
   FontDialog1->Font = WindowsBitmapFont1->Font;
   if (FontDialog1->Execute())
	  WindowsBitmapFont1->Font = FontDialog1->Font;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
  GuiRoot->MouseDown(Sender,TMouseButton(Button),Shift,X,Y);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
  GuiRoot->MouseMove(Sender,Shift,X,Y);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseUp(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
  GuiRoot->MouseUp(Sender,TMouseButton(Button),Shift,X,Y);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)

{
 GuiRoot->KeyDown(Sender,Key,Shift);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyPress(TObject *Sender, System::WideChar &Key)
{
  GuiRoot->KeyPress(Sender,Key);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyUp(TObject *Sender, WORD &Key, TShiftState Shift)
{
  GuiRoot->KeyUp(Sender,Key,Shift);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCanvasMouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
  if (Button == mbLeft)
  {
	// Make sure all mouse events are sent to the canvas before other GuiComponents, see GLCanvasAcceptMouseQuery.
	GuiRoot->ActiveControl = GLCanvas;
	// Set a status not to send mouse message to child components if any, see GLCanvasAcceptMouseQuery.
	GLCanvas->KeepMouseEvents = true;
	StartX = X;
	StartY = Y;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCanvasMouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y)
{
  CurrentX = X;
  CurrentY = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCanvasMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y)
{
  if (Button == mbLeft)
  {
	StartX = -1;
	StartY = -1;
	// Set normal mouse message handling, see GLCanvasAcceptMouseQuery.
	GuiRoot->ActiveControl = NULL;
	// Set that childs are allowed to get mouse events, meant for then, see GLCanvasAcceptMouseQuery.
	GLCanvas->KeepMouseEvents = false;
  }
}
//---------------------------------------------------------------------------




void __fastcall TForm1::GLCanvasRender(TGLCustomControl *Sender, TBitmap *Bitmap)

{
  Bitmap->Width = Int(GLCanvas->Width);
  Bitmap->Height = Int(GLCanvas->Height);
  if (StartX != -1)
  {
	Bitmap->Canvas->MoveTo(StartX-Int(Sender->Position->X),StartY-Int(Sender->Position->Y));
	Bitmap->Canvas->LineTo(CurrentX-Int(Sender->Position->X),CurrentY-Int(Sender->Position->Y));
	StartX = CurrentX;
	StartY = CurrentY;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::PenButtonButtonClick(TObject *Sender)
{
  GLCanvas->Bitmap->Canvas->Pen->Width = 1;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::BrushButtonButtonClick(TObject *Sender)
{
  GLCanvas->Bitmap->Canvas->Pen->Width = 5;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::WhiteButtonButtonClick(TObject *Sender)
{
  GLCanvas->Bitmap->Canvas->Pen->Color = clWhite;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::BlackButtonButtonClick(TObject *Sender)
{
  GLCanvas->Bitmap->Canvas->Pen->Color = clBlack;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RedButtonButtonClick(TObject *Sender)
{
  GLCanvas->Bitmap->Canvas->Pen->Color = clRed;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GreenButtonButtonClick(TObject *Sender)
{
  GLCanvas->Bitmap->Canvas->Pen->Color = clGreen;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::BlueButtonButtonClick(TObject *Sender)
{
  GLCanvas->Bitmap->Canvas->Pen->Color = clBlue;
}
//---------------------------------------------------------------------------


void __fastcall TForm1::GLCanvasAcceptMouseQuery(TGLBaseControl *Sender, TShiftState Shift,
          TGLMouseAction Action, TMouseButton Button, int X, int Y, bool &Accept)

{
// Sender.KeepMouseEvents is set when drawing,
// if drawing this component, gets mouse events even if they are out of bounds!
  if (Sender->KeepMouseEvents) Accept = true;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLForm1Moving(TGLForm *Sender, float &Left, float &Top)
{
// make sure the form isn't moved out of bounds...

  if (Left > GLSceneViewer1->Width-32)
	Left = GLSceneViewer1->Width-32;

  if (Left + Sender->Width < 32)
	Left = 32 - Sender->Width;

  if (Top > GLSceneViewer1->Height-32)
	Top = GLSceneViewer1->Height-32;

  if (Top < 0)
	Top = 0;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::miOpen1Click(TObject *Sender)
{
  if (OpenDialog1->Execute())
	GLCanvas->Bitmap->LoadFromFile(OpenDialog1->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::miSave1Click(TObject *Sender)
{
  if (SaveDialog1->Execute())
	GLCanvas->Bitmap->SaveToFile(SaveDialog1->FileName);
}
//---------------------------------------------------------------------------

