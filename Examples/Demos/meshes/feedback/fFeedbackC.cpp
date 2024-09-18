//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fFeedbackC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Feedback"
#pragma link "GLS.Objects"
#pragma link "GLS.GeomObjects"
#pragma link "GLS.Scene"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.Mesh"
#pragma link "GLS.Feedback"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  TGLMeshObject *mo;
  TFGIndexTexCoordList *fg;

  // Clear our freeform of any meshes
  GLFreeForm1->MeshObjects->Clear();

  // Set feedback to active, will feedback render child
  // objects into it's buffer
  GLFeedback1->Active = true;

  // Process the first mesh object (GLCube and GLDodecahedron)

  // Make the objects visible that we want to buffer
  MeshObject1->Visible = true;

  // Render the feedback object to buffer it's child object
  // that are visible
  GLSceneViewer1->Buffer->Render(GLFeedback1);

  // Hide the child objects we rendered
  /// MeshObject1->Visible = false;   /// need to be true

  // Create a new mesh object in our freeform
  // Delphi -  mo := TGLMeshObject.CreateOwned(GLFreeForm1.MeshObjects);
  mo = new TGLMeshObject;
  mo = (TGLMeshObject *) (GLFreeForm1->MeshObjects);
  mo->Mode = momTriangles;

  // Process the feedback buffer for polygon data
  // and build a mesh (normals are recalculated
  // since feedback only yields position and
  // texcoords)
  GLFeedback1->BuildMeshFromBuffer(
	mo->Vertices, mo->Normals, mo->Colors, mo->TexCoords, NULL);

  // Process the second mesh object (GLSphere)
  // (comments from first mesh object apply here also)
  MeshObject2->Visible = true;
  GLSceneViewer1->Buffer->Render(GLFeedback1);
  /// MeshObject2->Visible = false;  /// need to be true

  // Vertex indices are required for smooth normals
  // in Delphi - mo := TGLMeshObject.CreateOwned(GLFreeForm1.MeshObjects);
  // GLFreeForm1->MeshObjects->Add(Mesh);
  mo = new TGLMeshObject;
  mo = GLFreeForm1->MeshObjects->Items[0];
  mo->Mode = momFaceGroups;
  // in Delphi - fg := TFGIndexTexCoordList.CreateOwned(mo.FaceGroups);
  fg = new TFGIndexTexCoordList;
  fg->Mode = fgmmTriangles;
  GLFeedback1->BuildMeshFromBuffer(
	mo->Vertices, mo->Normals, NULL, fg->TexCoords, fg->VertexIndices);

  // Deactivate the feedback object
  GLFeedback1->Active = false;
  GLFreeForm1->StructureChanged();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
  if (Shift.Contains(ssLeft))
	GLCamera1->MoveAroundTarget(my - Y, mx - X);
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------
