//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
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
  Randomize;
  RayStart = AffineVectorMake(Random()*2 -1, Random()*2 -1, Random()*2 -1);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  TAffineVector iPnt, afScale ;
	// Change pos.
  if (CheckBox2->Checked)
  {
	BoxPos   = AffineVectorMake(Random()*2 -1, Random()*2 -1, Random()*2 -1);
	DCCamTarg->Position->SetPoint(BoxPos);

	BoxScale = AffineVectorMake(Random()*1 +0.5, Random()*1 +0.5, Random()*1 +0.5);
	DCCube1->Scale->SetVector(BoxScale);
	afScale = VectorScale(BoxScale, 0.5);

	RayStart = AffineVectorMake(Random()*3 -1.5, Random() *3 -1.5, Random() *3 -1.5);
  }
  RayDir = AffineVectorMake(Random()*2 -1, Random() *2 -1, Random()*2 -1);
  NormalizeVector(RayDir);

  GLLines1->Nodes->Clear();
  GLLines1->Nodes->AddNode(RayStart);
  GLLines1->Nodes->AddNode(VectorAdd(RayStart, VectorScale(RayDir, 8)) );
  GLPoints1->Positions->Clear();
  GLPoints1->Positions->Add(RayStart);
  GLPoints1->Positions->Add(BoxPos);
  GLPoints1->Positions->Add(VectorSubtract(BoxPos, afScale));
  GLPoints1->Positions->Add(VectorAdd(BoxPos, afScale));

  if (RayCastBoxIntersect(RayStart, RayDir, VectorSubtract(
	BoxPos, afScale), VectorAdd(BoxPos, afScale), &iPnt))
  {
	Label1->Caption =  Format("Intersect point: %.3f %.3f %.3f",
	   ARRAYOFCONST	((iPnt.X, iPnt.Y, iPnt.Z)));
	GLPoints1->Positions->Add(iPnt);
	Beep();
  }
  else
	Label1->Caption = "no intersection";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
  GLCube1->Visible = CheckBox1->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencerProgress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  if (Form1->Active)
	Viewer->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  LabelFPS->Caption = Format("%.1f FPS",
	ARRAYOFCONST ((Viewer->FramesPerSecond())));
  Viewer->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y)
{
  if (Shift.Contains(ssLeft))
	 GLCamera1->MoveAroundTarget(mdy -Y, mdx - X);
  mdx = X;
  mdy = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  GLCamera1->AdjustDistanceToTarget(Power(1.02, WheelDelta/120));
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormResize(TObject *Sender)
{
  GLCamera1->FocalLength = MinInteger(Height, Width) / 10;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyPress(TObject *Sender, System::WideChar &Key)
{
  if (Key = 0x27)
	Close();
}
//---------------------------------------------------------------------------
