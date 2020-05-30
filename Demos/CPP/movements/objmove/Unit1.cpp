//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBitmapFont"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLHUDObjects"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSpaceText"
#pragma link "GLWin32Viewer"
#pragma link "GLWindowsFont"
#pragma resource "*.dfm"
TForm1 *Form1;

const TColorVector
  SelectionColor[]  = {0.243, 0.243, 0.243, 1.000};


//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  UpdateHudText();
}
//---------------------------------------------------------------------------
Glvectorgeometry::TVector __fastcall TForm1::MouseWorldPos(int X, int Y)
{
  Glvectorgeometry::TVector v;
  Glvectorgeometry::TVector Result;

  Y = Scn->Height - Y;
  if (CurrentPick)
  {
	SetVector(v, X, Y, 0);
	if (movingOnZ)
	  Scn->Buffer->ScreenVectorIntersectWithPlaneXZ(
		 v, CurrentPick->Position->Y, Result);
	else
	  Scn->Buffer->ScreenVectorIntersectWithPlaneXY(
		 v, CurrentPick->Position->Z, Result);
  }
  else
	SetVector(Result, NullVector);
  return Result;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::UpdateHudText()
{
  TAffineVector objPos, winPos;

  if (CurrentPick)
  {
	SetVector(objPos, CurrentPick->AbsolutePosition);

	TopText->Text = Format(
	  "New Object Position: Xn: %4.4f, Yn: %4.4f, Zn: %4.4f",
	  ARRAYOFCONST ((objPos.X, objPos.Y, objPos.Z)));

	winPos = Scn->Buffer->WorldToScreen(objPos);

	ObjText->Visible = true;
	ObjText->Text = CurrentPick->Name;
	ObjText->Position->X = winPos.X + 10;
	ObjText->Position->Y = Scn->Height - winPos.Y + 10;
  }
  else
  {
	TopText->Text = "No selected object";
	ObjText->Visible = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ProcessPick(TGLBaseSceneObject* pick)
{
  if (pick)
  {
	// Only Cube1 and Cube2 can be selected
	if ((pick->Name != "Cube1") && (pick->Name != "Cube2"))
	  pick = NULL;
  }
  if (pick != CurrentPick)
  {
	if (CurrentPick)
	{
	  CurrentPick->ShowAxes = false;
	  CurrentPick->Material->FrontProperties->Emission->Color = clrBlack;
	}
	CurrentPick = (TGLCustomSceneObject *)(pick);
	if (CurrentPick)
	{
	  if (ShowAxes->Checked)
		CurrentPick->ShowAxes = true;
//not translated  CurrentPick->Material->FrontProperties->Emission->Color = SelectionColor;
	}
  }
  UpdateHudText();
}



//---------------------------------------------------------------------------

void __fastcall TForm1::ScnMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y)
{
  TGLBaseSceneObject* pick;

  movingOnZ = Shift.Contains(ssShift);
  // If an object is picked...
  pick = (Scn->Buffer->GetPickedObject(X, Y)); // as TGLCustomSceneObject);
  ProcessPick(pick);

  // store mouse pos
  if (CurrentPick)
	lastMouseWorldPos = MouseWorldPos(X, Y);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ScnMouseMove(TObject *Sender, TShiftState Shift, int X, int Y)

{
  Glvectorgeometry::TVector newPos;

  ScnMouseMoveCnt++;
////not translated  Assert(ScnMouseMoveCnt < 2);
  if (Shift.Contains(ssLeft))
  {
	// handle hold/unhold of shift
	if (Shift.Contains(ssShift) != movingOnZ)
	{
	  movingOnZ = (Shift.Contains(ssShift));
	  lastMouseWorldPos = MouseWorldPos(X, Y);
	}
	newPos = MouseWorldPos(X, Y);
	if (CurrentPick && (VectorNorm(lastMouseWorldPos) != 0))
	  CurrentPick->Position->Translate(VectorSubtract(newPos, lastMouseWorldPos));
	lastMouseWorldPos = newPos;

	UpdateHudText();
  }
  ScnMouseMoveCnt--;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ShowAxesClick(TObject *Sender)
{
  // Unselect all
  ProcessPick(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  // Note that 1 wheel-step induces a WheelDelta of 120,
  // this code adjusts the distance to target with a 10% per wheel-step ratio
  if (WheelDelta != 0)
	GLCamera1->AdjustDistanceToTarget(Power(1.1, -WheelDelta / 120));
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyPress(TObject *Sender, System::WideChar &Key)
{
  switch (Key)
  {
	case '2':
		GLCamera1->MoveAroundTarget(3, 0); break;
	case '4':
		GLCamera1->MoveAroundTarget(0, -3);break;
	case '6':
		GLCamera1->MoveAroundTarget(0, 3);break;
	case '8':
		GLCamera1->MoveAroundTarget(-3, 0);break;
	case '-':
		GLCamera1->AdjustDistanceToTarget(1.1);break;
	case '+':
		GLCamera1->AdjustDistanceToTarget(1 / 1.1);break;
  default:
	  ;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyUp(TObject *Sender, WORD &Key, TShiftState Shift)
{
  if (CurrentPick)
  {
  switch (Key)
    {
	  case VK_UP:
		  if (Shift.Contains(ssShift))
			CurrentPick->Translate(0, 0, 0.3);
		  else
			CurrentPick->Translate(-0.3, 0, 0);
		   break;
	  case VK_DOWN:
		  if (Shift.Contains(ssShift))
			CurrentPick->Translate(0, 0, -0.3);
		  else
			CurrentPick->Translate(0.3, 0, 0);
		   break;
	  case	VK_LEFT:
		  CurrentPick->Translate(0, -0.3, 0); break;
	  case	VK_RIGHT:
		  CurrentPick->Translate(0, 0.3, 0);  break;
	  default:
	  ;
	}
  }
}
//---------------------------------------------------------------------------
