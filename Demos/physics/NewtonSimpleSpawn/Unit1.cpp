//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.HUDObjects"
#pragma link "Physics.NGDManager"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.XCollection"
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
  TGLCube *GLCube1;
  TGLNGDDynamic *DynNGDBehav;

  GLCube1 = (TGLCube *)(GLDummyCube1->AddNewChild(__classid(TGLCube)));
  GLCube1->Material->FrontProperties->Diffuse->RandomColor();
  GLCube1->Position->SetPoint(Random(), 2, Random());

  DynNGDBehav = (TGLNGDDynamic *)GLCube1->GetOrCreateBehaviour(__classid(TGLNGDDynamic));
  DynNGDBehav->Manager = GLNGDManager1;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  TGLSphere *GLSphere1;
  TGLNGDDynamic *DynNGDBehav;

  GLSphere1 = (TGLSphere *)(GLDummyCube1->AddNewChild(__classid(TGLSphere)));
  GLSphere1->Material->FrontProperties->Diffuse->RandomColor();
  GLSphere1->Position->SetPoint(Random(), 2, Random());
  DynNGDBehav = (TGLNGDDynamic *)GLSphere1->GetOrCreateBehaviour(__classid(TGLNGDDynamic));
  DynNGDBehav->Manager = GLNGDManager1;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button3Click(TObject *Sender)
{
  TGLCone *GLCone1;
  TGLNGDDynamic *DynNGDBehav;

  GLCone1 = (TGLCone *)(GLDummyCube1->AddNewChild(__classid(TGLCone)));
  GLCone1->Material->FrontProperties->Diffuse->RandomColor();
  GLCone1->Position->SetPoint(Random(), 2, Random());
  DynNGDBehav = (TGLNGDDynamic *)GLCone1->GetOrCreateBehaviour(__classid(TGLNGDDynamic));
  DynNGDBehav->Manager = GLNGDManager1;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button4Click(TObject *Sender)
{
  TGLCylinder *GLCylinder1;
  TGLNGDDynamic *DynNGDBehav;

  GLCylinder1 = (TGLCylinder *)(GLDummyCube1->AddNewChild(__classid(TGLCylinder)));
  GLCylinder1->Material->FrontProperties->Diffuse->RandomColor();
  GLCylinder1->Position->SetPoint(Random(), 2, Random());
  DynNGDBehav = (TGLNGDDynamic *)GLCylinder1->GetOrCreateBehaviour(__classid(TGLNGDDynamic));
  DynNGDBehav->Manager = GLNGDManager1;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button5Click(TObject *Sender)
{
  TGLCapsule *GLCapsule1;
  TGLNGDDynamic *DynNGDBehav;

  GLCapsule1 = (TGLCapsule *)(GLDummyCube1->AddNewChild(__classid(TGLCapsule)));
  GLCapsule1->Material->FrontProperties->Diffuse->RandomColor();
  GLCapsule1->Position->SetPoint(Random(), 2, Random());
  GLCapsule1->PitchAngle = 90;

  DynNGDBehav = (TGLNGDDynamic *)GLCapsule1->GetOrCreateBehaviour(__classid(TGLNGDDynamic));
  DynNGDBehav->Manager = GLNGDManager1;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button6Click(TObject *Sender)
{
  GLDummyCube1->DeleteChildren();
  GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  GLNGDManager1->Step(deltaTime);
  GLResolutionIndependantHUDText1->Text = "Bodycount:=" + IntToStr
	(GLNGDManager1->NewtonBodyCount);

}
