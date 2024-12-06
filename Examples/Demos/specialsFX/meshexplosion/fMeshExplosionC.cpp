//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMeshExplosionC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.ExplosionFx"
#pragma link "GLS.Scene"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.File3DS"


#pragma resource "*.dfm"
TFormMeshExplosion *FormMeshExplosion;
//---------------------------------------------------------------------------
__fastcall TFormMeshExplosion::TFormMeshExplosion(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormMeshExplosion::FormCreate(TObject *Sender)
{
  TFileName Path = GetCurrentAssetPath();
  SetCurrentDir(Path  + "\\model");
  //load mesh
  mesh->LoadFromFile("mushroom.3ds");
  //cache information
  Cache = new TGLMeshObjectList;
  Cache->Assign(mesh->MeshObjects);
  //default settings
  expl = (TGLBExplosionFX *)(mesh->Effects->Items[0]);
  expl->MaxSteps = 0;
  expl->Speed = 0.1;

}
//---------------------------------------------------------------------------
void __fastcall TFormMeshExplosion::CheckOnClick(TObject *Sender)
{
  //turn on/off
  expl->Enabled = CheckOn->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TFormMeshExplosion::Button1Click(TObject *Sender)
{
   //reset simulation
   expl->Reset();
   CheckOn->Checked = false;
   //restore the mesh
   mesh->MeshObjects->Assign(Cache);
   mesh->StructureChanged();

}
//---------------------------------------------------------------------------
void __fastcall TFormMeshExplosion::ViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y)
{
	 if (Shift.Contains(ssLeft))
	 {
	   Camera1->MoveAroundTarget(Y - vy, X - vx);
	   vx = X; vy = Y;
	 }

}
//---------------------------------------------------------------------------
void __fastcall TFormMeshExplosion::ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
     vx = X; vy = Y;
}
//---------------------------------------------------------------------------
void __fastcall TFormMeshExplosion::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
	 Viewer->Invalidate();
	 StepBar->Position = expl->Step;

}
//---------------------------------------------------------------------------
void __fastcall TFormMeshExplosion::SpeedBarChange(TObject *Sender)
{
   expl->Speed = (float) SpeedBar->Position / 10;
}
//---------------------------------------------------------------------------
void __fastcall TFormMeshExplosion::MaxStepsBarChange(TObject *Sender)
{
  expl->MaxSteps = MaxStepsBar->Position;
  StepBar->Max = MaxStepsBar->Position;
}
//---------------------------------------------------------------------------
