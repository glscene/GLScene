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
#pragma link "GLNGDManager"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
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
  TNGDSurfaceItem *SurfaceTrampoline;
  TNGDSurfaceItem *SurfaceFriction;
  TNGDSurfaceItem *SurfaceCube2;
  TNGDSurfaceItem *SurfaceCube3;
  TNGDSurfaceItem *SurfaceCube4;
  TNGDSurfaceItem *SurfaceSphere1_Sphere2_Cube1;

  TNGDSurfacePair *ObjectOnTrampoline;
  TNGDSurfacePair *FrictionOnCube2;
  TNGDSurfacePair *FrictionOnCube3;
  TNGDSurfacePair *FrictionOnCube4;

  // Get each SurfaceItem
  SurfaceTrampoline = (TNGDSurfaceItem *)GLNGDManager1->NewtonSurfaceItem->Items[0];
  SurfaceFriction = (TNGDSurfaceItem *)GLNGDManager1->NewtonSurfaceItem->Items[1];
  SurfaceCube2 = (TNGDSurfaceItem *)GLNGDManager1->NewtonSurfaceItem->Items[2];
  SurfaceCube3 = (TNGDSurfaceItem *)GLNGDManager1->NewtonSurfaceItem->Items[3];
  SurfaceCube4 = (TNGDSurfaceItem *)GLNGDManager1->NewtonSurfaceItem->Items[4];
  SurfaceSphere1_Sphere2_Cube1 = (TNGDSurfaceItem *)GLNGDManager1->NewtonSurfaceItem->Items[5];

  // Set them to Behaviours
  GetNGDStatic(Trampoline)->NGDSurfaceItem = SurfaceTrampoline;
  GetNGDStatic(Friction)->NGDSurfaceItem = SurfaceFriction;
  GetNGDDynamic(GLCube2)->NGDSurfaceItem = SurfaceCube2;
  GetNGDDynamic(GLCube3)->NGDSurfaceItem = SurfaceCube3;
  GetNGDDynamic(GLCube4)->NGDSurfaceItem = SurfaceCube4;
  GetNGDDynamic(GLCube1)->NGDSurfaceItem = SurfaceSphere1_Sphere2_Cube1;
  GetNGDDynamic(GLSphere1)->NGDSurfaceItem = SurfaceSphere1_Sphere2_Cube1;
  GetNGDDynamic(GLSphere2)->NGDSurfaceItem = SurfaceSphere1_Sphere2_Cube1;

  // Get each SurfacePair
  ObjectOnTrampoline = (TNGDSurfacePair *)GLNGDManager1->NewtonSurfacePair->Items[0];
  FrictionOnCube2 = (TNGDSurfacePair *)GLNGDManager1->NewtonSurfacePair->Items[1];
  FrictionOnCube3 = (TNGDSurfacePair *)GLNGDManager1->NewtonSurfacePair->Items[2];
  FrictionOnCube4 = (TNGDSurfacePair *)GLNGDManager1->NewtonSurfacePair->Items[3];

  // Set SurfaceItems to SurfacePair
  ObjectOnTrampoline->SetMaterialItems(SurfaceTrampoline, SurfaceSphere1_Sphere2_Cube1);

  FrictionOnCube2->SetMaterialItems(SurfaceFriction, SurfaceCube2);
  FrictionOnCube3->SetMaterialItems(SurfaceFriction, SurfaceCube3);
  FrictionOnCube4->SetMaterialItems(SurfaceFriction, SurfaceCube4);

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  GLNGDManager1->Step(deltaTime);
}
//---------------------------------------------------------------------------
