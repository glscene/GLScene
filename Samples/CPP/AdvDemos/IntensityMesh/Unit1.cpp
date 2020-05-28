//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBitmapFont"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLHUDObjects"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLUserShader"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
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
   TMeshObject *mo;
   TFGVertexIndexList *fgQuads, *fgTris;
   int i;
   TFileStream *str;

   // load our raw data
   str = new TFileStream("IntensityMesh.data", fmOpenRead);
   str->Read(0, 4);
   //-System::AnsiString::SetLength(DataNodes, i);
   str->Read(0, 4);

/*
   Ex:
   DynamicArray<int> arrayOfInt;
   arrayOfInt.Length = 10;
   cout << "ArrayLength: " << arrayOfInt.Length << endl;
   SetLength(arrayOfInt, 10);
*/
   //-SetLength(DataPrimitives, i);
   //-str->Read(DataNodes[0], Length(DataNodes)*SizeOf(TDataNode));
   //-str->Read(DataPrimitives[0], Length(DataPrimitives)*SizeOf(TDataPrimitive));
   str->Free();

   // fill the freeform with our data

   // first create a mesh object
   mo = new TMeshObject(); //Delphi - mo = TMeshObject.CreateOwned(GLFreeForm.MeshObjects);
   mo = (TMeshObject *) (GLFreeForm->MeshObjects);
   mo->Mode = momFaceGroups;
   // Specify vertex and texcoords data (intensity is stored a texcoord)

   /*
   Ex:
   int TotalArray(const DynamicArray<int>& arrayOfInt)
	{
	  int total=0;
	  for (int i=arrayOfInt.Low; i<=arrayOfInt.High; i++)
		total += arrayOfInt[i];
	  return total;
	}
   */
//--------------------------------
/*
   for (i=0; i<High(DataNodes);i++)
   {
	  mo->Vertices->Add(DataNodes[i]->X, DataNodes[i]->Y, DataNodes[i]->Z);
	  mo->TexCoords->Add(DataNodes[i]->Intensity*0.001, 0);
   }
   // Then create the facegroups that will hold our quads and triangles
   fgQuads = TFGVertexIndexList.CreateOwned(mo.FaceGroups);
   fgQuads->Mode = fgmmQuads;
   fgTris = TFGVertexIndexList.CreateOwned(mo.FaceGroups);
   fgTris->Mode = fgmmTriangles;
   // and fill them with our primitives
   for (i=1; i < High(DataPrimitives);i++)
   with DataPrimitives[i]-> do
   {
	  if (Node4<>$FFFF)
	  {
		 fgQuads.VertexIndices.Add(Node1, Node2);
		 fgQuads.VertexIndices.Add(Node4, Node3);
	  }
	  else
		 fgTris->VertexIndices->Add(Node1, Node2, Node3);
   }
*/
   // auto center
   GLFreeForm->PerformAutoCentering();
   // and initialize scale
   TBScaleChange(this);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
   mx=X; my=Y;
   GLSceneViewer1->SetFocus();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
   if (Shift.Contains(ssLeft))
	  GLCamera->MoveAroundTarget(my-Y, mx-X);
   if (Shift.Contains(ssRight)) {
	  DCTarget->Position->AddScaledVector((mx-X)/30, GLCamera->AbsoluteRightVectorToTarget());
	  DCTarget->Position->AddScaledVector((Y-my)/30, GLCamera->AbsoluteUpVectorToTarget());
   }
   mx=X; my=Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::TBScaleChange(TObject *Sender)
{
   GLMaterialLibrary1->Materials->Items[0]->TextureScale->X = (float)TBScale->Position/100;
   HTPaletteRight->Text = FormatFloat("%d", (TBScale->Position*10));
   GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------

