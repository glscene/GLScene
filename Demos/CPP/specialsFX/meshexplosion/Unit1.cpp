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
#pragma link "GLExplosionFx"
#pragma link "GLScene"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFile3DS"


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
  SetGLSceneMediaDir();
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
void __fastcall TForm1::CheckOnClick(TObject *Sender)
{
  //turn on/off
  expl->Enabled = CheckOn->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
   //reset simulation
   expl->Reset();
   CheckOn->Checked = false;
   //restore the mesh
   mesh->MeshObjects->Assign(Cache);
   mesh->StructureChanged();

}
//---------------------------------------------------------------------------
void __fastcall TForm1::ViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y)
{
	 if (Shift.Contains(ssLeft))
	 {
	   Camera1->MoveAroundTarget(Y - vy, X - vx);
	   vx = X; vy = Y;
	 }

}
//---------------------------------------------------------------------------
void __fastcall TForm1::ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
     vx = X; vy = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
	 Viewer->Invalidate();
	 StepBar->Position = expl->Step;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::SpeedBarChange(TObject *Sender)
{
   expl->Speed = (float) SpeedBar->Position / 10;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MaxStepsBarChange(TObject *Sender)
{
  expl->MaxSteps = MaxStepsBar->Position;
  StepBar->Max = MaxStepsBar->Position;
}
//---------------------------------------------------------------------------
