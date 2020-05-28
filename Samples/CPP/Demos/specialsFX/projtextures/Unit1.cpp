//---------------------------------------------------------------------------

#include <vcl.h>
#include <math.h>
#pragma hdrstop

#include "Unit1.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLFileTGA"
#pragma resource "*.dfm"

TForm1 *Form1;
float  ang;
int  mx, my, mk;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender,
      const double deltaTime, const double newTime)
{
  ang = ang + deltaTime*20;

  Light->Position->Y = sin(DegToRad(ang));
  Light->Position->X = cos(DegToRad(ang));

  light2->Pitch(deltaTime*20);

  viewer->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  String MediaPath = ExtractFilePath(ParamStr(0));
  String SubStr = "Samples";
  int I = MediaPath.Pos(SubStr);
  if (I != 0) {
	MediaPath.Delete(I+8,MediaPath.Length()-I);
	MediaPath += "Media\\";
	SetCurrentDir(MediaPath);
  }
  matLib->Materials->Items[0]->Material->Texture->Image->LoadFromFile("projector.tga");
  matLib->Materials->Items[1]->Material->Texture->Image->LoadFromFile("flare1.bmp");

  emitter1->Material->MaterialLibrary = matLib;
  emitter1->Material->LibMaterialName = "spot";

  emitter2->Material->MaterialLibrary = matLib;
  emitter2->Material->LibMaterialName = "spot2";
  emitter2->FOVy = 40;

  GLPlane1->Material->Texture->Image->LoadFromFile("cm_front.jpg");
  GLPlane2->Material->Texture->Image->LoadFromFile("cm_left.jpg");
  GLPlane3->Material->Texture->Image->LoadFromFile("cm_bottom.jpg");

  ProjLight->Emitters->AddEmitter(emitter1);
  ProjLight->Emitters->AddEmitter(emitter2);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  Form1->Caption ="GLScene Projected Textures - "+ Format("%f", ARRAYOFCONST((viewer->FramesPerSecond())));
  viewer->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::viewerMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  mk = 1;
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::viewerMouseUp(TObject *Sender, TMouseButton Button,
      TShiftState Shift, int X, int Y)
{
  mk = 0;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::viewerMouseMove(TObject *Sender, TShiftState Shift,
      int X, int Y)
{
  if (mk == 1)
  {
	if (Shift.Contains(ssLeft))
	  GLCamera1->MoveAroundTarget(my - Y, mx - X);
	else if (Shift.Contains(ssRight))
	  GLCamera1->AdjustDistanceToTarget(1.0 + (my - Y) * 0.01);
  }

  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyDown(TObject *Sender, WORD &Key,
	  TShiftState Shift)
{
  if (Key == VK_ADD)
	  emitter1->FOVy = emitter1->FOVy + 5;
  else if (Key == VK_SUBTRACT)
	  emitter1->FOVy = emitter1->FOVy - 5;

  if (Key == 'S')
	  if (ProjLight->Style == ptsOriginal)
		   ProjLight->Style = ptsInverse;
	  else
		   ProjLight->Style = ptsOriginal;
}
//---------------------------------------------------------------------------
