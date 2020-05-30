//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "GLParametricSurfaces.hpp"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLFileNURBS"
#pragma resource "*.dfm"
TForm1 *Form1;
int mx, my;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  String MediaPath = ExtractFilePath(ParamStr(0));
  int I = MediaPath.Pos("Samples");
  if (I != 0) {
	MediaPath.Delete(I+8,MediaPath.Length()-I);
	MediaPath += "Media\\";
	SetCurrentDir(MediaPath);
  }
  // Load the nurbs data
  GLActor1->LoadFromFile("duck1.nurbs");
  GLActor1->AddDataFromFile("duck2.nurbs");
  GLActor1->AddDataFromFile("duck3.nurbs");

  // { Translate FreeForm based on the first mesh object's average
  // control point. Quick and dirty ... or maybe just dirty :P }
  TAffineVectorList *cp =
	((TMOParametricSurface *) (GLActor1->MeshObjects->Items[0]))->ControlPoints;
  GLActor1->Position->Translate(VectorNegate(VectorScale(cp->Sum(),1.0/cp->Count)));

}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject * Sender,
												TMouseButton Button,
												TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar1Change(TObject * Sender)
{
  for(int i = 0; i < GLActor1->MeshObjects->Count; i++)
	((TMOParametricSurface *) (GLActor1->MeshObjects->Items[i]))->
	  Resolution = TrackBar1->Position;

  GLActor1->StructureChanged();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox1Click(TObject * Sender)
{
  if(CheckBox1->Checked)
  {
	GLActor1->Material->PolygonMode = pmLines;
	GLActor1->Material->FaceCulling = fcNoCull;
  }
  else
  {
	GLActor1->Material->PolygonMode = pmFill;
	GLActor1->Material->FaceCulling = fcBufferDefault;
  }
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
  if(Shift.Contains(ssShift))
  {
	GLCamera1->MoveAroundTarget(my - Y, mx - X);
	mx = X;
	my = Y;
  }
}
//---------------------------------------------------------------------------

