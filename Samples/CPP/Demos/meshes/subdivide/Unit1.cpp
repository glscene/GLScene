//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
#pragma link "GLScene"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFile3DS"
#pragma link "GLFileMD2"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BULoadClick(TObject *Sender)
{
   SetGLSceneMediaDir();
   BUSubdivide->Enabled = true;

//   GLFreeForm1->LoadFromFile("polyhedron.3ds");
//   GLFreeForm1->LoadFromFile("mushroom.3ds");
//   GLFreeForm1->LoadFromFile("trinityrage.smd");
//   GLFreeForm1->LoadFromFile("HighPolyObject.3ds");

/*
   GLActor1->LoadFromFile("trinityrage.smd");
   GLActor1->AddDataFromFile("run.smd");
   GLActor1->Animations[1].MakeSkeletalTranslationStatic;
   GLActor1->SwitchToAnimation(GLActor1.Animations[1]);
*/

   GLActor1->LoadFromFile("waste.md2");
   GLActor1->Material->Texture->Image->LoadFromFile("waste.jpg");
   GLActor1->Material->Texture->Enabled = true;
   GLActor1->SwitchToAnimation(GLActor1->Animations->Items[0]);

   CBAnimateClick(Sender);

}
//---------------------------------------------------------------------------
void __fastcall TForm1::BUSubdivideClick(TObject *Sender)
{
   int i, j;
   TAffineVectorList *tris, *norms, *tex, *buf, *morphTris, *morphNorms;
   TIntegerList *indices, *texIndices;
   TIntegerList *firstRemap, *subdivideRemap, *bufRemap;
   __int64 t;

   BUSubdivide->Enabled = false;

   Screen->Cursor = crHourGlass;
   t = StartPrecisionTimer();

   for (i = 0; i < GLActor1->MeshObjects->Count-1; i++ )
   {
	  tex = new TAffineVectorList;
	  tris = GLActor1->MeshObjects->Items[i]->ExtractTriangles(tex);
	  indices = BuildVectorCountOptimizedIndices(tris);
	  firstRemap = (TIntegerList *)(indices->CreateClone());
	  RemapAndCleanupReferences(tris, indices);

	  norms = BuildNormals(tris, indices);

	  // subdivide geometry
	  SubdivideTriangles((float)TrackBar1->Position*0.1, tris, indices, norms);

	  texIndices = BuildVectorCountOptimizedIndices(tex);
	  RemapAndCleanupReferences(tex, texIndices);

	  // subdivide texture space
	  SubdivideTriangles(0, tex, texIndices);

	  // Re-expand everything
	  buf = new TAffineVectorList;
	  try
	  {
		 ConvertIndexedListToList(tris, indices, buf);
		 tris->Assign(buf);
		 buf->Count = 0;
		 ConvertIndexedListToList(norms, indices, buf);
		 norms->Assign(buf);
		 buf->Count = 0;
		 ConvertIndexedListToList(tex, texIndices, buf);
		 tex->Assign(buf);
	  }
	  __finally
	  {
		 buf->Free();
	  }
	  // Pack & Optimize the expanded stuff
	  indices->Free();
	  indices = BuildVectorCountOptimizedIndices(tris, norms, tex);
	  subdivideRemap = (TIntegerList *)(indices->CreateClone());
	  RemapReferences(norms, indices);
	  RemapReferences(tex, indices);
	  RemapAndCleanupReferences(tris, indices);

	  IncreaseCoherency(indices, 13);

	  bufRemap = new TIntegerList;
	 for (j = 0; j < GLActor1->MeshObjects->MorphTargetCount()-1; j++)
	 {
		GLActor1->MeshObjects->MorphTo(j);

		morphTris = GLActor1->MeshObjects->ExtractTriangles();
		bufRemap->Assign(firstRemap);
		RemapAndCleanupReferences(morphTris, bufRemap);

		morphNorms = BuildNormals(morphTris, bufRemap);

		SubdivideTriangles(TrackBar1->Position*0.1, morphTris, bufRemap, morphNorms);

		buf = new TAffineVectorList;
		try
		{
		   ConvertIndexedListToList(morphTris, bufRemap, buf);
		   morphTris->Assign(buf);
		   ConvertIndexedListToList(morphNorms, bufRemap, buf);
		   morphNorms->Assign(buf);
		}
		__finally
		{
		   buf->Free();
		}
		RemapReferences(morphTris, subdivideRemap);
		RemapReferences(morphNorms, subdivideRemap);

		GLActor1->MeshObjects->Items[j]->Vertices = morphTris;
		GLActor1->MeshObjects->Items[j]->Normals = morphNorms;

		morphTris->Free();
		morphNorms->Free();
	 }
	 bufRemap->Free();

	 GLActor1->MeshObjects->Items[i]->Vertices = tris;
	 GLActor1->MeshObjects->Items[i]->Normals = norms;
	 GLActor1->MeshObjects->Items[i]->TexCoords = tex;
	 GLActor1->MeshObjects->Items[i]->FaceGroups->Clear();

	 //GLActor1->MeshObjects->Items[i]->FaceGroups->Items[i] = indices;
	 GLActor1->MeshObjects->Items[i]->Mode = fgmmTriangles;

	 texIndices->Free();
	 subdivideRemap->Free();
	 firstRemap->Free();
	 tex->Free();
	 indices->Free();
	 norms->Free();
	 tris->Free();
   }

//   (GLActor1->MeshObjects->Items[0] as TGLSkeletonMeshObject)->PrepareBoneMatrixInvertedMeshes;

   LASubdivideTime->Caption = Format("%.1f ms",
	 ARRAYOFCONST ((StopPrecisionTimer(t)*1000)));
   // Initial perf: 1412 ms
   // Basic Edges Hash: 464 ms
   // Several transfer optims: 377 ms
   // morph & subdivide normals too : 527 ms
   Screen->Cursor =crDefault;
   GLActor1->StructureChanged();
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
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
   else
   if (Shift.Contains(ssRight))
	  GLCamera1->RotateTarget(my-Y, mx-X);
   mx = X;
   my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RBWireFrameClick(TObject *Sender)
{
   GLActor1->Material->PolygonMode = pmLines;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RBSolidClick(TObject *Sender)
{
   GLActor1->Material->PolygonMode = pmFill;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   Caption = "Subdivide " + Format("%.1f FPS -  %d Triangles",
		  ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond(),
					GLActor1->MeshObjects->TriangleCount())));
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
   GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBAnimateClick(TObject *Sender)
{
   // not only turns on/off animation, but also forces the TGLActor
   // to generate a display list when animation is off
   if (CBAnimate->Checked)
   {
	  GLActor1->AnimationMode = aamLoop;
	  GLActor1->ObjectStyle = GLActor1->ObjectStyle << osDirectDraw;
	  GLActor1->Reference = aarMorph;
   }
   else
   {
	  GLActor1->AnimationMode = aamNone;
	  GLActor1->MeshObjects->MorphTo(0);
	  GLActor1->Reference = aarNone;
	  GLActor1->StructureChanged();
	  GLActor1->ObjectStyle = GLActor1->ObjectStyle >> osDirectDraw;
   }
}
//---------------------------------------------------------------------------
