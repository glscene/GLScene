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
void TForm1::PathTravelStop(TObject *Sender, TGLMovementPath *Path, bool Looped)
{
  if (!Application->Terminated)
   InformationDlg("Path Travel Stopped");
}

void TForm1::PathAllTravelledOver(TObject *Sender)
{
   if (!Application->Terminated)
	  InformationDlg("All Path(es) Traveled Over");
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormActivate(TObject *Sender)
{
  TGLMovement *Movement;
  TGLMovementPath *Path;
  TGLPathNode *Node;

  // Create a movement, a path and the first node of the path.
  Movement   = GetOrCreateMovement(Cube2);
//  Movement->OnPathTravelStop = PathTravelStop();
//  Movement->OnAllPathTravelledOver = PathAllTravelledOver();
  Path       = Movement->AddPath();
  Path->ShowPath = True;

  // Path.StartTime := 2;
  // Path.Looped := True;

  Node       = Path->AddNodeFromObject(Cube2);
  Node->Speed = 4.0;

  // Add a node.
  Node       = Path->AddNode();
  Node->Speed = 4.0;
  Node->PositionAsVector = VectorMake(-10, 0, 0, 1);
  Node->RotationAsVector = VectorMake(0, 0, 0);

  // Add a node.
  Node       = Path->AddNode();
  Node->Speed = 4.0;
  Node->PositionAsVector = VectorMake(0, 5, - 5);
  Node->RotationAsVector = VectorMake(0, 90, 0);

  // Add a node.
  Node       = Path->AddNode();
  Node->Speed = 4.0;
  Node->PositionAsVector = VectorMake(6, - 5, 2);
  Node->RotationAsVector = VectorMake(0, 180, 0);

  // Add a node.
  Node       = Path->AddNode();
  Node->Speed = 4.0;
  Node->PositionAsVector = VectorMake(-6, 0, 0);
  Node->RotationAsVector = VectorMake(0, 259, 0);

  // Activatived the current path.
  Movement->ActivePathIndex = 0;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::MoveBtnClick(TObject *Sender)
{
  TGLMovement *Movement;

  Movement = GetMovement(Cube2);
  if (Movement) {
	Movement->StartPathTravel();
	GLCadencer1->Enabled = true;
  }
}
//---------------------------------------------------------------------------


