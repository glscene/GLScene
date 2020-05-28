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
#pragma link "GLGeomObjects"
#pragma link "GLGraph"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;

const int  SizePos   = 10;
const int  ScaleSize = 3;
const int  TrigRect  = 10;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Randomize;
  BoxScale = XYZVector;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::MakeRandomData()
{
  int i;
  // Change position.
  if (CheckBox1->Checked)
	BoxPos   = AffineVectorMake( Random()*SizePos -SizePos/2,
								 Random()*SizePos -SizePos/2,
								 Random()*SizePos -SizePos/2  );
	// Change scale.
  if (CheckBox2->Checked)
	BoxScale = AffineVectorMake( Random()*ScaleSize +ScaleSize/2,
								 Random()*ScaleSize +ScaleSize/2,
								 Random()*ScaleSize +ScaleSize/2 );
	// Change triange.
  if (CheckBox3->Checked)
	for (i = 0; i < 2; i++)
	  TriangePos[i] = AffineVectorMake(Random()*TrigRect -TrigRect/2,
									   Random()*TrigRect -TrigRect/2,
									   Random()*TrigRect -TrigRect/2 );
   // Calc extends.
  MinExtend = VectorSubtract(BoxPos, VectorScale(BoxScale, 0.5));
  MaxExtend = VectorAdd(BoxPos, VectorScale(BoxScale, 0.5));

}
//---------------------------------------------------------------------------
void __fastcall TForm1::DrawResult()
{
  int i;

  GLPolygon1->Nodes->Clear();
  GLPolygon1->AddNode(TriangePos[0]);
  GLPolygon1->AddNode(TriangePos[1]);
  GLPolygon1->AddNode(TriangePos[2]);

  GLPoints1->Positions->Clear();
  GLPoints1->Colors->Add(1, 0, 0, 1);
  for (i = 0; i < 2; i++)
	GLPoints1->Positions->Add(TriangePos[i]);

  GLLines2->Nodes->Clear();
  GLLines2->Nodes->AddNode(TriangePos[0]);
  GLLines2->Nodes->AddNode(TriangePos[1]);
  GLLines2->Nodes->AddNode(TriangePos[2]);
  GLLines2->Nodes->AddNode(TriangePos[0]);

  DCCamTarget->Position->SetPoint(BoxPos);
  DCCamTarget->Scale->SetVector(BoxScale);
  GLCube1->Position->SetPoint(BoxPos);
  GLCube1->Scale->SetVector(BoxScale);
}


void __fastcall TForm1::Button1Click(TObject *Sender)
{
  int IterCnt;
  bool Res1;

  IterCnt = 0;
  while (Res1)
  {
	MakeRandomData();
	Res1 =  IntersectTriangleBox( TriangePos[0], TriangePos[1], TriangePos[2],
								   MinExtend, MaxExtend);
	IterCnt++;
	if (IterCnt >= 10000)
	{
	  DrawResult();
	  ShowMessage("Intersection not found!");
	  exit;
	}
  }
  DrawResult();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject *Sender)
{
  int IterCnt;
  bool Res1;

  IterCnt = 0;
  while (!Res1)
  {
	MakeRandomData();
	Res1 =  IntersectTriangleBox(TriangePos[0], TriangePos[1], TriangePos[2],
								  MinExtend, MaxExtend);
	IterCnt++;
	if (IterCnt >= 10000)
	{
	  DrawResult();
	  ShowMessage("Intersection not found!");
	  exit;
	}
  }
  DrawResult();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox4Click(TObject *Sender)
{
  GLCube1->Visible    = CheckBox4->Checked;
  GLXYZGrid1->Visible = CheckBox6->Checked;
  GLLines1->Visible   = CheckBox5->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencerProgress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  if (Form1->Active)
    Viewer->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y)
{
  if (Shift.Contains(ssLeft))
	GLCamera1->MoveAroundTarget(mdy -Y, mdx -X);
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

