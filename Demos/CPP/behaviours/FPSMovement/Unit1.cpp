//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#pragma package(smart_init)

#include "Unit1.h"
#pragma link "GLFile3DS"
#pragma link "GLFileJPEG"


//---------------------------------------------------------------------------
#pragma link "GLSimpleNavigation"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLFPSMovement"
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLFPSMovement"
#pragma link "GLMaterial"
#pragma link "GLNavigator"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;

TGLBFPSMovement *behav, *behav2;
double yangle = 90;
double xangle = 0;
bool WireFrame;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  String MediaPath = ExtractFilePath(ParamStr(0));
  int I = MediaPath.Pos("Samples");
  if (I != 0) {
	MediaPath.Delete(I+8,MediaPath.Length()-I);
	SetCurrentDir(MediaPath+"Media\\");
  }
  Map1->LoadFromFile("map.3ds");
  Map1->BuildOctree(3);
  Map1->Up->SetVector(0, 1, 0);

  Map2->LoadFromFile("beer.3ds");
  Map2->BuildOctree(3);

  ShowCursor(false);
  SetCursorPos(Screen->Width / 2, Screen->Height / 2);

  behav = GetFPSMovement(Player);
  behav2 = GetFPSMovement(Bot);
}

//---------------------------------------------------------------------------


void __fastcall TForm1::FormKeyDown(TObject * Sender, WORD & Key,
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

void __fastcall TForm1::GLCadencer1Progress(TObject * Sender,
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
  xangle = Mouse->CursorPos.x - Screen->Width / 2;
  yangle = Mouse->CursorPos.y - Screen->Height / 2;
  SetCursorPos(Screen->Width / 2, Screen->Height / 2);
  behav->TurnHorizontal(xangle * 40 * deltaTime);
  behav->TurnVertical(-yangle * 20 * deltaTime);

  GLSceneViewer1->Invalidate();
}

//---------------------------------------------------------------------------


