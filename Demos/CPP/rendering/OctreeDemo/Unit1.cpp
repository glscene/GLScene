//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
//#include <gl\gl.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
#pragma link "GLWin32Viewer"
#pragma link "GLCrossPlatform"
#pragma resource "*.dfm"
TForm1 *Form1;

const cBOX_SIZE = 14.2;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  int i;

  Randomize;

  Octree = new TOctreeSpacePartition;
  Octree->SetSize(AffineVectorMake(-15, -15, -15), AffineVectorMake(15, 15, 15));
  Octree->MaxTreeDepth = 6;
  Octree->LeafThreshold = 10;

  TrackBar_LeafThresholdChange(NULL);

  for (i = 0; i < 300; i ++)
	CreateBox();

  VerifySpacialMisc();
}

// class TGLSpacePartitionLeaf

__fastcall TGLSpacePartitionLeaf::CreateGLOwned(TBaseSpacePartition *SpacePartition, TGLBaseSceneObject *aGLBaseSceneObject)
{
  GLBaseSceneObject = aGLBaseSceneObject;

  // Set them all off in the same direction
  Direction.X = rand();
  Direction.Y = rand();
  Direction.Z = rand();

  NormalizeVector(Direction);

//  inherited CreateOwned(SpacePartition);
}

void __fastcall TGLSpacePartitionLeaf::UpdateCachedAABBAndBSphere()
{
  FCachedAABB = GLBaseSceneObject->AxisAlignedBoundingBox();
  FCachedAABB.Min = GLBaseSceneObject->LocalToAbsolute(FCachedAABB.Min);
  FCachedAABB.Max = GLBaseSceneObject->LocalToAbsolute(FCachedAABB.Max);

  FCachedBSphere.Radius = GLBaseSceneObject->BoundingSphereRadius();
  FCachedBSphere.Center = GLBaseSceneObject->Position->AsAffineVector;
}

//---------------------------------------------------------------------------

TAffineVector RandomPos()
  {
	int	c1 = 10;
	TAffineVector result;
   //MakeVector(result, rand()*c1-c1/2, rand()*c1-c1/2, rand()*c1-c1/2);
	MakeVector(result, rand(), rand(), rand());
	return result;
  };

float RandomSize()
  {
	return 0.1+rand()*0.5;
  };

void __fastcall TForm1::CreateBox()
{
  TGLCube *GLCube;
  TGLSpacePartitionLeaf * GLSpacePartitionLeaf;

  GLCube = (TGLCube *)GLScene1->Objects->AddNewChild(__classid(TGLCube));
  GLCube->Position->AsAffineVector = RandomPos();
  GLCube->CubeWidth = RandomSize();
  GLCube->CubeHeight = RandomSize();
  GLCube->CubeDepth = RandomSize();

   GLSpacePartitionLeaf = new TGLSpacePartitionLeaf;
   GLSpacePartitionLeaf->CreateGLOwned(Octree, GLCube); //C++
}
//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar_LeafThresholdChange(TObject *Sender)
{
  Label3->Caption = Format("Leaf Threshold : %d",
	 ARRAYOFCONST ((TrackBar_LeafThreshold->Position)));

  Octree->LeafThreshold = TrackBar_LeafThreshold->Position;
}

//---------------------------------------------------------------------------
float TestMove(float pos, float dir, const double deltaTime)
{
  const cSPEED = 2;
  {
	if (abs(pos+dir*deltaTime*cSPEED)>=cBOX_SIZE)
	  dir = -dir;
	return (pos + dir * deltaTime*cSPEED);
  }
}

void __fastcall TForm1::RenderAABB(TAABB* AABB, float w, float r, float g, float b)
{
/*
   rci.GLStates->LineWidth = w;
   glColor3f(r,g,b);
   glBegin(GL_LINE_STRIP);
		glVertex3f(AABB.min.X,AABB.min.Y, AABB.min.Z);
		glVertex3f(AABB.min.X,AABB.max.Y, AABB.min.Z);
		glVertex3f(AABB.max.X,AABB.max.Y, AABB.min.Z);
		glVertex3f(AABB.max.X,AABB.min.Y, AABB.min.Z);
		glVertex3f(AABB.min.X,AABB.min.Y, AABB.min.Z);

		glVertex3f(AABB.min.X,AABB.min.Y, AABB.max.Z);
		glVertex3f(AABB.min.X,AABB.max.Y, AABB.max.Z);
		glVertex3f(AABB.max.X,AABB.max.Y, AABB.max.Z);
		glVertex3f(AABB.max.X,AABB.min.Y, AABB.max.Z);
		glVertex3f(AABB.min.X,AABB.min.Y, AABB.max.Z);
	  glEnd();

	  glBegin(GL_LINES);
		glVertex3f(AABB.min.X,AABB.max.Y, AABB.min.Z);
		glVertex3f(AABB.min.X,AABB.max.Y, AABB.max.Z);

		glVertex3f(AABB.max.X,AABB.max.Y, AABB.min.Z);
		glVertex3f(AABB.max.X,AABB.max.Y, AABB.max.Z);

		glVertex3f(AABB.max.X,AABB.min.Y, AABB.min.Z);
		glVertex3f(AABB.max.X,AABB.min.Y, AABB.max.Z);
	  glEnd();
*/
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RenderOctreeNode(TSectorNode* Node)
{
	int i;
	TAABB* AABB;


	if (Node->NoChildren)
	{
	  AABB = new TAABB;
//	  AABB = Node->AABB;
	  if (Node->RecursiveLeafCount > 0)
		RenderAABB(AABB, 1, 0, 0, 0);
	  else
		RenderAABB(AABB, 1, 0.8, 0.8, 0.8);
	}
	else
	{
	  for (i = 0; i < Node->ChildCount-1; i++)
		RenderOctreeNode(Node->Children[i]);
	}
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLDirectOpenGL1Render(TObject *Sender, TGLRenderContextInfo &rci)

{
  TAABB* AABB;
  rci.GLStates->Disable(stLighting);
  rci.GLStates->Enable(stColorMaterial);

  MakeVector(AABB->Min, -cBOX_SIZE, -cBOX_SIZE, -cBOX_SIZE);
  MakeVector(AABB->Max,  cBOX_SIZE,  cBOX_SIZE,  cBOX_SIZE);
  RenderAABB(AABB,2, 0,0,0);
  RenderOctreeNode(Octree->RootNode);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  const cSPEED = 2;
  TAABB* AABB;
  TBSphere* BSphere;
  TGLSpacePartitionLeaf* Leaf;
  TGLSpacePartitionLeaf* TestLeaf;
  TGLCube* Cube;
  int i, j, CollidingLeafCount;

  for (i = 0; i < Octree->Leaves->Count-1; i++)
  {
	Leaf = (TGLSpacePartitionLeaf*)(Octree->Leaves->Items[i]);
	Cube = (TGLCube*)(Leaf->GLBaseSceneObject);
	Cube->Position->X = TestMove(Cube->Position->X, Leaf->GLBaseSceneObject->Direction->X, deltaTime);
	Cube->Position->Y = TestMove(Cube->Position->Y, Leaf->GLBaseSceneObject->Direction->Y, deltaTime);
	Cube->Position->Z = TestMove(Cube->Position->Z, Leaf->GLBaseSceneObject->Direction->Z, deltaTime);

	Leaf->Changed();
  }

  for (i = 0; i < Octree->Leaves->Count-1; i++)
  {
	Leaf = (TGLSpacePartitionLeaf*)(Octree->Leaves->Items[i]);
	Cube = (TGLCube*)(Leaf->GLBaseSceneObject);
	Cube->Material->FrontProperties->Emission->Red = 0;
	Cube->Material->FrontProperties->Emission->Green = 0;
	Cube->Material->FrontProperties->Emission->Blue = 0;
  }

  // AABB collision
  AABB = new TAABB();
  /*
  AABB = GLCube1->AxisAlignedBoundingBox;
  AABB->Max = GLCube1->LocalToAbsolute(AABB->Min);
  AABB->Max = GLCube1->LocalToAbsolute(AABB->Max);
  Octree->QueryAABB(AABB);
  */

}

//---------------------------------------------------------------------------
void __fastcall TForm1::VerifySpacialMisc()
{
  TAABB* AABBmajor;
  TAABB* AABBfull;
  TAABB* AABBoff;
  TAABB* AABBpartial;


  AABBmajor = new TAABB;
  MakeVector(AABBmajor->Min, 0, 0, 0);
  MakeVector(AABBmajor->Max, 5, 5, 5);

 /*

  MakeVector(AABBpartial->Min, 4, 4, 4);
  MakeVector(AABBpartial->Max, 6, 6, 6);

  MakeVector(AABBfull->Min, 1, 1, 1);
  MakeVector(AABBfull->Max, 2, 2, 2);

  MakeVector(AABBoff->Min, 7, 7, 7);
  MakeVector(AABBoff->Max, 8, 8, 8);
  */
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Button_ResetOctreeSizeClick(TObject *Sender)
{
  Octree->GrowMethod = gmBestFit;
  Octree->UpdateStructureSize(0.05);
  Octree->GrowMethod = gmIncreaseToFitAll;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormDestroy(TObject *Sender)
{
  Octree->Free();
}
//---------------------------------------------------------------------------


