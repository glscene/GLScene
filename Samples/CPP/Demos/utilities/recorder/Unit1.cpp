//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLWin32Viewer"
#pragma link "GLAVIRecorder"
#pragma link "GLCadencer"
#pragma link "GLAsyncTimer"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLAVIRecorder"
#pragma link "GLKeyboard"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"

#pragma resource "*.dfm"
TForm1 *Form1;
bool UserAbort;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBarChange(TObject * Sender)
{
  int t = TrackBar->Position;
  // the "sun" spins slowly
  Cube1->TurnAngle = (float)t / 4;
  // "earth" rotates around the sun and spins
  DummyCube1->TurnAngle = (float)-t;
  Cube2->TurnAngle = (float)t *2;
  // "moon" rotates around earth and spins
  DummyCube2->RollAngle = (float)3 *t;
  Cube3->TurnAngle = (float)4 *t;
  // update FPS count
  StaticText1->Caption =
    IntToStr(Trunc(GLSceneViewer1->FramesPerSecond())) + " FPS";
}

//---------------------------------------------------------------------------

void __fastcall TForm1::FormResize(TObject * Sender)
{
  GLSceneViewer1->ResetPerformanceMonitor();
  AVIRecorder1->Width = GLSceneViewer1->Width;
  AVIRecorder1->Height = GLSceneViewer1->Height;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Button1Click(TObject * Sender)
{
  if(!AVIRecorder1->CreateAVIFile(0))
    return;
  // if AVIRecorder1.filename is empty, a dialog box will appear asking
  // for the filename. CreateAVIFile() will return a bool
  // indicating if user presses "cancel" in the dialog box.

  String SavedCap = Caption;
  Caption = "Press ESC to abort";
  UserAbort = false;
  StaticText1->Visible = false; // the FPS shown is not correct now,
  // so just hide it for the time being.

  int i = 0;

  Button1->Enabled = false;
  TrackBar->Enabled = false;

  try
  {
    while((i < 360) && !UserAbort)
    {
      TrackBar->Position = i;
      TrackBarChange(this);

      AVIRecorder1->AddAVIFrame();

      // you might want to update your progress bar here.

      Application->ProcessMessages();   // so that our app. is not freezed,
      // and will accept user abort.
      i++;
    }
  }
  __finally
  {
    AVIRecorder1->CloseAVIFile(UserAbort);      // if UserAbort, CloseAVIFile will
    // also delete the unfinished file.
    Caption = SavedCap;
    StaticText1->Visible = true;
    Button1->Enabled = true;
    TrackBar->Enabled = true;
  }
}

//---------------------------------------------------------------------------

void __fastcall TForm1::AVIRecorder1PostProcessEvent(TObject * Sender,
                                                     TBitmap * frame)
{
// PostProcess event is used to add a "watermark"
  // that will be in the AVI, but isn't visible on-screen
  TCanvas *c = frame->Canvas;
  c->Font->Color = clAqua;
  c->Font->Name = "Courrier New";
  c->Font->Size = 24;
  c->Font->Style = (c->Font->Style << fsBold);
  c->Brush->Style = bsClear;
  String s;
  s.printf("GLScene %.3d", TrackBar->Position);
  c->TextOut(20, 20, s);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyPress(TObject *Sender, char &Key)
{
  UserAbort = IsKeyDown(VK_ESCAPE);
}
//---------------------------------------------------------------------------

