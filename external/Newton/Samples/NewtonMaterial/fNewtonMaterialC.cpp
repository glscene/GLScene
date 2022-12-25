//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fNewtonMaterialC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "Physics.NGDManager"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLS.SceneViewer"
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
  TGLNGDSurfaceItem *SurfaceTrampoline;
  TGLNGDSurfaceItem *SurfaceFriction;
  TGLNGDSurfaceItem *SurfaceCube2;
  TGLNGDSurfaceItem *SurfaceCube3;
  TGLNGDSurfaceItem *SurfaceCube4;
  TGLNGDSurfaceItem *SurfaceSphere1_Sphere2_Cube1;

  TGLNGDSurfacePair *ObjectOnTrampoline;
  TGLNGDSurfacePair *FrictionOnCube2;
  TGLNGDSurfacePair *FrictionOnCube3;
  TGLNGDSurfacePair *FrictionOnCube4;

  // Get each SurfaceItem
  SurfaceTrampoline = (TGLNGDSurfaceItem *)GLNGDManager1->NewtonSurfaceItem->Items[0];
  SurfaceFriction = (TGLNGDSurfaceItem *)GLNGDManager1->NewtonSurfaceItem->Items[1];
  SurfaceCube2 = (TGLNGDSurfaceItem *)GLNGDManager1->NewtonSurfaceItem->Items[2];
  SurfaceCube3 = (TGLNGDSurfaceItem *)GLNGDManager1->NewtonSurfaceItem->Items[3];
  SurfaceCube4 = (TGLNGDSurfaceItem *)GLNGDManager1->NewtonSurfaceItem->Items[4];
  SurfaceSphere1_Sphere2_Cube1 = (TGLNGDSurfaceItem *)GLNGDManager1->NewtonSurfaceItem->Items[5];

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
  ObjectOnTrampoline = (TGLNGDSurfacePair *)GLNGDManager1->NewtonSurfacePair->Items[0];
  FrictionOnCube2 = (TGLNGDSurfacePair *)GLNGDManager1->NewtonSurfacePair->Items[1];
  FrictionOnCube3 = (TGLNGDSurfacePair *)GLNGDManager1->NewtonSurfacePair->Items[2];
  FrictionOnCube4 = (TGLNGDSurfacePair *)GLNGDManager1->NewtonSurfacePair->Items[3];

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
