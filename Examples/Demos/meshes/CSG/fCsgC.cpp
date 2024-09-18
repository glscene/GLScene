//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fCsgC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.File3DS"

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
  TFileName Path = GetCurrentAssetPath();
  SetCurrentDir(Path  + "\\model");
  // scaled 40
  GLFreeForm1->LoadFromFile("polyhedron.3ds");

  // scaled 20, position.x = 16
  GLFreeForm2->LoadFromFile("polyhedron.3ds");
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
 Drag = true;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseUp(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
  Drag = false;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
  if (Drag)
	GLCamera1->MoveAroundTarget(my-Y, mx-X);
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheelDown(TObject *Sender, TShiftState Shift, TPoint &MousePos,
		  bool &Handled)
{
  GLCamera1->AdjustDistanceToTarget(1.1);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheelUp(TObject *Sender, TShiftState Shift, TPoint &MousePos,
		  bool &Handled)
{
  GLCamera1->AdjustDistanceToTarget(1/1.1);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ButtonClearClick(TObject *Sender)
{
  GLFreeForm3->MeshObjects->Clear();
  GLFreeForm3->StructureChanged();

  GLFreeForm1->Material->PolygonMode = pmFill;
  GLFreeForm2->Material->PolygonMode = pmFill;

}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnUnionAandBClick(TObject *Sender)
{
  // Union
  TGLMeshObject *Mesh;
  ButtonClearClick(Sender);
  if (GLFreeForm3->MeshObjects->Count == 0)
  {
    //Delphi:  TGLMeshObject.CreateOwned(GLFreeForm3.MeshObjects).Mode := momFaceGroups;
  	Mesh = new  (TGLMeshObject);
	  Mesh->Mode = momFaceGroups;
    GLFreeForm3->MeshObjects->Add(Mesh);
  }
  CSG_Operation(GLFreeForm1->MeshObjects->Items[0],
				GLFreeForm2->MeshObjects->Items[0],
				CSG_Union,Mesh,'1','2');
  GLFreeForm3->StructureChanged();

  GLFreeForm1->Material->PolygonMode = pmLines;
  GLFreeForm2->Material->PolygonMode = pmLines;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnSubtractABClick(TObject *Sender)
{
  // Subtract A-B
  TGLMeshObject *Mesh;
  ButtonClearClick(Sender);
  if (GLFreeForm3->MeshObjects->Count == 0)
  {
    //Delphi:  TGLMeshObject.CreateOwned(GLFreeForm3.MeshObjects).Mode := momFaceGroups;
    Mesh = new (TGLMeshObject);
    Mesh->Mode = momFaceGroups;
    GLFreeForm3->MeshObjects->Add(Mesh);

  }
  CSG_Operation(GLFreeForm1->MeshObjects->Items[0],
				GLFreeForm2->MeshObjects->Items[0],
				CSG_Subtraction,Mesh,'1','2');
  GLFreeForm3->StructureChanged();

  GLFreeForm1->Material->PolygonMode = pmLines;
  GLFreeForm2->Material->PolygonMode = pmLines;

}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnSubtractBAClick(TObject *Sender)
{
  // Subtract B-A
  TGLMeshObject *Mesh;
  ButtonClearClick(Sender);
  if (GLFreeForm3->MeshObjects->Count == 0)
  {
//Delphi:  TGLMeshObject.CreateOwned(GLFreeForm3.MeshObjects).Mode := momFaceGroups;
   Mesh = new (TGLMeshObject);
   Mesh->Mode = momFaceGroups;
   GLFreeForm3->MeshObjects->Add(Mesh);

  }
  CSG_Operation(GLFreeForm2->MeshObjects->Items[0],
				GLFreeForm1->MeshObjects->Items[0],
				CSG_Subtraction,Mesh,'1','2');
  GLFreeForm3->StructureChanged();

  GLFreeForm1->Material->PolygonMode = pmLines;
  GLFreeForm2->Material->PolygonMode = pmLines;

}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnIntersectAorBClick(TObject *Sender)
{
  // Intersect
  TGLMeshObject *Mesh;
  ButtonClearClick(Sender);
  if (GLFreeForm3->MeshObjects->Count == 0)
  {
//Delphi:  TGLMeshObject.CreateOwned(GLFreeForm3.MeshObjects).Mode := momFaceGroups;
   Mesh = new (TGLMeshObject);
   Mesh->Mode = momFaceGroups;
   GLFreeForm3->MeshObjects->Add(Mesh);

  }
  CSG_Operation(GLFreeForm1->MeshObjects->Items[0],
				GLFreeForm2->MeshObjects->Items[0],
				CSG_Intersection,Mesh,'1','2');
  GLFreeForm3->StructureChanged();

  GLFreeForm1->Material->PolygonMode = pmLines;
  GLFreeForm2->Material->PolygonMode = pmLines;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
  if (CheckBox1->Checked)
  {
   	GLMaterialLibrary1->Materials->Items[0]->Material->PolygonMode = pmFill;
  	GLMaterialLibrary1->Materials->Items[1]->Material->PolygonMode = pmFill;
  }
  else
  {
	  GLMaterialLibrary1->Materials->Items[0]->Material->PolygonMode = pmLines;
	  GLMaterialLibrary1->Materials->Items[1]->Material->PolygonMode = pmLines;
  }
  GLFreeForm3->StructureChanged();
}
//---------------------------------------------------------------------------

