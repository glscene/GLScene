//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#include <math.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLTexture"
#pragma link "GLWin32Viewer"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
#pragma link "GLKeyBoard"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
		: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BtnLoadClick(TObject *Sender)
{
//  SetGLSceneMediaDir();
  OpenPictureDialog1->InitialDir = ExtractFilePath(ParamStr(0));
  OpenPictureDialog1->FileName = "sejourstmathieu2048.jpg";
  if (OpenPictureDialog1->Execute())
  GLMaterialLibrary1->Materials->Items[0]->Material->
  Texture->Image->LoadFromFile(OpenPictureDialog1->FileName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBar1Change(TObject *Sender)
{
  GLCamera1->FocalLength = TrackBar1->Position;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender,
	  const double deltaTime, const double newTime)
{
  const int step_size = 20;
  float delta;
  float dx, dy;
  delta = step_size * 40/GLCamera1->FocalLength * deltaTime;
  dx = 0;
  dy = 0;
  if (IsKeyDown(VK_LEFT) ) dx = dx+delta;
  if (IsKeyDown(VK_UP)   ) dy = dy+delta;
  if (IsKeyDown(VK_RIGHT)) dx = dx-delta;
  if (IsKeyDown(VK_DOWN) ) dy = dy-delta;
  PanCameraAround(dx, dy);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyDown(TObject *Sender, WORD &Key,
	  TShiftState Shift)
{
  Key = 0; // all keys handled by Form1
}
//---------------------------------------------------------------------------
void TForm1::PanCameraAround(float dx, float dy)
{
  pitch = pitch+dy;
  yaw = yaw-dx;

  if (pitch>90) pitch = 90;
  if (pitch<-90) pitch = -90;
  if (yaw>360) yaw = yaw-360;
  if (yaw<0) yaw = yaw+360;

  GLCamera1->Up->SetVector(0, 1, 0);
  GLCamera1->Direction->SetVector( sin(DegToRad(yaw)),
								   sin(DegToRad(pitch)),
								  -cos(DegToRad(yaw)));

  LabelPitch->Caption = Format("Pitch: %3f", ARRAYOFCONST((pitch)));
  LabelYaw->Caption = Format("Yaw: %3f", ARRAYOFCONST((yaw)));
}


//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender,
	  TMouseButton Button, TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender,
	  TShiftState Shift, int X, int Y)
{
  float dx, dy, f;
  if (Shift.Contains(ssLeft))
	{
	 f = 0.2*40/GLCamera1->FocalLength;
	 dx = (X-mx)*f;
	 dy = (Y-my)*f;
	 PanCameraAround(dx, dy);
	}
  mx = X;
  my = Y;
}

//---------------------------------------------------------------------------

//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  TrackBar1->Position = TrackBar1->Position+(int)(2*WheelDelta/120);
}
//---------------------------------------------------------------------------

