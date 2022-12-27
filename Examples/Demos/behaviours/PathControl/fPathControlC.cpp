// ---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fPathControlC.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormPathControl *FormPathControl;

// ---------------------------------------------------------------------------
__fastcall TFormPathControl::TFormPathControl(TComponent* Owner) : TForm(Owner) {
}

void TFormPathControl::PathTravelStop(TObject *Sender, TGLMovementPath *Path, bool Looped)
{
	if (!Application->Terminated)
		InformationDlg("Path Travel Stopped");
}

void TFormPathControl::PathAllTravelledOver(TObject *Sender) {
	if (!Application->Terminated)
		InformationDlg("All Path(es) Traveled Over");
}

// ---------------------------------------------------------------------------
void __fastcall TFormPathControl::FormActivate(TObject *Sender) {
	TGLMovement *Movement;
	TGLMovementPath *Path;
	TGLPathNode *Node;

	// Create a movement, a path and the first node of the path.
	Movement = GetOrCreateMovement(Cube);
	// Movement->OnPathTravelStop = PathTravelStop();
	// Movement->OnAllPathTravelledOver = PathAllTravelledOver();
	Path = Movement->AddPath();
	Path->ShowPath = True;

	// Path.StartTime := 2;
	// Path.Looped := True;

	Node = Path->AddNodeFromObject(Cube);
	Node->Speed = 4.0;

	// Add a node.
	Node = Path->AddNode();
	Node->Speed = 4.0;
	Node->PositionAsVector = VectorMake(-10, 0, 0, 1);
	Node->RotationAsVector = VectorMake(0, 0, 0);

	// Add a node.
	Node = Path->AddNode();
	Node->Speed = 4.0;
	Node->PositionAsVector = VectorMake(0, 5, -5);
	Node->RotationAsVector = VectorMake(0, 90, 0);

	// Add a node.
	Node = Path->AddNode();
	Node->Speed = 4.0;
	Node->PositionAsVector = VectorMake(6, -5, 2);
	Node->RotationAsVector = VectorMake(0, 180, 0);

	// Add a node.
	Node = Path->AddNode();
	Node->Speed = 4.0;
	Node->PositionAsVector = VectorMake(-6, 0, 0);
	Node->RotationAsVector = VectorMake(0, 259, 0);

	// Activatived the current path.
	Movement->ActivePathIndex = 0;

}

// ---------------------------------------------------------------------------
void __fastcall TFormPathControl::MoveBtnClick(TObject *Sender) {
	TGLMovement *Movement;

	Movement = GetMovement(Cube);
	if (Movement) {
		Movement->StartPathTravel();
		GLCadencer1->Enabled = true;
	}
}
// ---------------------------------------------------------------------------
