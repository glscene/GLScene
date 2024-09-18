//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#pragma package(smart_init)

#include "fFPSMovementC.h"
//---------------------------------------------------------------------------
#pragma link "GLS.File3DS"
#pragma link "GLS.FileJPEG"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLS.Coordinates"

#pragma link "GLS.FPSMovement"
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"

#pragma link "GLS.FPSMovement"
#pragma link "GLS.Material"
#pragma link "GLS.Navigator"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormFPSMovement *FormFPSMovement;

TGLBFPSMovement *behav, *behav2;
double yangle = 90;
double xangle = 0;
bool WireFrame;

//---------------------------------------------------------------------------
__fastcall TFormFPSMovement::TFormFPSMovement(TComponent * Owner):TForm(Owner)
{
  TFileName Path = GetCurrentAssetPath();
  SetCurrentDir(Path + "\\model");
  Map1->LoadFromFile("map.3ds");
  Map1->BuildOctree(3);
  Map1->Up->SetVector(0, 1, 0);

  Map2->LoadFromFile("beer.3ds");
  Map2->BuildOctree(3);

  ShowCursor(false);
  SetCursorPos(Vcl::Forms::Screen->Width / 2, Vcl::Forms::Screen->Height / 2);

  behav = GetFPSMovement(Player);
  behav2 = GetFPSMovement(Bot);
}

//---------------------------------------------------------------------------


void __fastcall TFormFPSMovement::FormKeyDown(TObject * Sender, WORD & Key,
									TShiftState Shift)
{
//  if(Key=Ord("R") ) resetScene;
  if(Key == VK_ESCAPE)
	Close();

  //show/hide arrows
  if(Key == VK_F1)
	behav->ShowArrows = !behav->ShowArrows;

  //pause / unpause
  if(Key == VK_PAUSE)
	GLCadencer1->Enabled = !GLCadencer1->Enabled;
  //first person
  if(Key == VK_F2)
	GLSceneViewer1->Camera = FirstPersonCamera;
  //third person
  if(Key == VK_F3)
	GLSceneViewer1->Camera = ThirdPersonCamera;
  // solid / wireframe
  if(IsKeyDown(VK_F5))
  {
	WireFrame = !WireFrame;
	if(WireFrame)
	{
	  Map1->UseMeshMaterials = false;
	  Map1->Material->PolygonMode = pmLines;
	  Map2->UseMeshMaterials = false;
	  Map2->Material->PolygonMode = pmLines;
	}
	else
	{
	  Map1->UseMeshMaterials = true;
	  Map1->Material->PolygonMode = pmFill;
	  Map2->UseMeshMaterials = true;
	  Map2->Material->PolygonMode = pmFill;
	}
  }
}

//---------------------------------------------------------------------------

void __fastcall TFormFPSMovement::GLCadencer1Progress(TObject * Sender,
											const double deltaTime,
											const double newTime)
{
  float MovementScale = MovManager->MovementScale;

  //) update position according to Keys being pressed
  if(IsKeyDown('W') || IsKeyDown('Z'))
	behav->MoveForward(MovementScale * deltaTime);
  if(IsKeyDown('S'))
	behav->MoveForward(-MovementScale * deltaTime);
  if(IsKeyDown('A') || IsKeyDown('Q'))
	behav->StrafeHorizontal(-MovementScale * deltaTime);
  if(IsKeyDown('D'))
	behav->StrafeHorizontal(MovementScale * deltaTime);

  //move up/down (f||debugging)
  if(IsKeyDown(VK_PRIOR) || IsKeyDown(VK_SPACE))
	behav->StrafeVertical(MovementScale * deltaTime);
  if(IsKeyDown(VK_NEXT))
	behav->StrafeVertical(-MovementScale * deltaTime);

  //move bot
  if(IsKeyDown('I'))
	behav2->MoveForward(MovementScale * deltaTime);
  if(IsKeyDown('K'))
	behav2->MoveForward(-MovementScale * deltaTime);
  if(IsKeyDown('J'))
	behav2->StrafeHorizontal(-MovementScale * deltaTime);
  if(IsKeyDown('L'))
	behav2->StrafeHorizontal(MovementScale * deltaTime);
  if(IsKeyDown('O'))
	behav2->StrafeVertical(MovementScale * deltaTime);
  if(IsKeyDown('P'))
	behav->StrafeVertical(-MovementScale * deltaTime);

  if(IsKeyDown(VK_LEFT))
	behav->TurnHorizontal(-70 * deltaTime);
  if(IsKeyDown(VK_RIGHT))
	behav->TurnHorizontal(70 * deltaTime);
  if(IsKeyDown(VK_UP))
	behav->TurnVertical(-70 * deltaTime);
  if(IsKeyDown(VK_DOWN))
	behav->TurnVertical(70 * deltaTime);

  //update mouse view
  xangle = Mouse->CursorPos.x - Vcl::Forms::Screen->Width / 2;
  yangle = Mouse->CursorPos.y - Vcl::Forms::Screen->Height / 2;
  SetCursorPos(Vcl::Forms::Screen->Width / 2, Vcl::Forms::Screen->Height / 2);
  behav->TurnHorizontal(xangle * 40 * deltaTime);
  behav->TurnVertical(-yangle * 20 * deltaTime);

  GLSceneViewer1->Invalidate();
}

//---------------------------------------------------------------------------


