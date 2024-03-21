//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fObjmoveC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.BitmapFont"
#pragma link "GLS.Coordinates"

#pragma link "GLS.GeomObjects"
#pragma link "GLS.HUDObjects"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SpaceText"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.WindowsFont"
#pragma link "GLS.Navigator"
#pragma link "GLS.SmoothNavigator"
#pragma resource "*.dfm"
TFormObjmove *FormObjmove;

const TGLColorVector
  SelectionColor[]  = {0.243, 0.243, 0.243, 1.000};


//---------------------------------------------------------------------------
void __fastcall TFormObjmove::FormCreate(TObject *Sender)
{
  UpdateHUDText();
}
//---------------------------------------------------------------------------
TGLVector __fastcall TFormObjmove::MouseWorldPos(int X, int Y)
{
  TGLVector v;
  TGLVector Result;

  Y = Scene->Height - Y;
  if (CurrentPick)
  {
	SetVector(v, X, Y, 0);
	if (movingOnZ)
	  Scene->Buffer->ScreenVectorIntersectWithPlaneXZ(
		 v, CurrentPick->Position->Y, Result);
	else
	  Scene->Buffer->ScreenVectorIntersectWithPlaneXY(
		 v, CurrentPick->Position->Z, Result);
  }
  else
	SetVector(Result, NullVector);
  return Result;
}

//---------------------------------------------------------------------------
void __fastcall TFormObjmove::UpdateHUDText()
{
  TAffineVector objPos, winPos;

  if (CurrentPick)
  {
 	SetVector(objPos, CurrentPick->AbsolutePosition);
   /*
	HUDText->Text = Format("New Object Position: Xn: %4.4f, Yn: %4.4f, Zn: %4.4f",
	  ARRAYOFCONST ((objPos.X, objPos.Y, objPos.Z)));
   */
	HUDText->Text = "New Object Position:";
	HUDText->Text = " Xn: " + FloatToStrF(objPos.X,ffFixed, 4, 4) +
					" Yn: " + FloatToStrF(objPos.Y,ffFixed, 4, 4) +
                    " Zn: " + FloatToStrF(objPos.Z,ffFixed, 4, 4);

	winPos = Scene->Buffer->WorldToScreen(objPos);

	HUDTextObj->Visible = true;
	HUDTextObj->Text = CurrentPick->Name;   // outtext for Cube1 or Cube2
   	HUDTextObj->Position->X = winPos.X + 20;
	HUDTextObj->Position->Y = Scene->Height - winPos.Y + 20;
  }
  else
  {
	HUDText->Text = "No selected object";
	HUDTextObj->Visible = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFormObjmove::ProcessPick(TGLBaseSceneObject* pick)
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
	}
  }
  UpdateHUDText();
}

//---------------------------------------------------------------------------
__fastcall TFormObjmove::TFormObjmove(TComponent* Owner)
	: TForm(Owner)
{
}

//---------------------------------------------------------------------------

void __fastcall TFormObjmove::SceneMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y)
{
  TGLBaseSceneObject* pick;
  movingOnZ = Shift.Contains(ssShift);
  // If an object is picked...
  pick = (Scene->Buffer->GetPickedObject(X, Y)); // as TGLCustomSceneObject);
  ProcessPick(pick);
  // store mouse pos
  if (CurrentPick)
	lastMouseWorldPos = MouseWorldPos(X, Y);
}
//---------------------------------------------------------------------------
void __fastcall TFormObjmove::SceneMouseMove(TObject *Sender, TShiftState Shift, int X, int Y)

{
  TGLVector newPos;
  SceneMouseMoveCnt++;
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

	UpdateHUDText();
  }
  SceneMouseMoveCnt--;
}
//---------------------------------------------------------------------------
void __fastcall TFormObjmove::ShowAxesClick(TObject *Sender)
{
  // Unselect all
  ProcessPick(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TFormObjmove::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  // Note that 1 wheel-step induces a WheelDelta of 120,
  // this code adjusts the distance to target with a 10% per wheel-step ratio
  if (WheelDelta != 0)
	GLCamera1->AdjustDistanceToTarget(Power(1.1, -WheelDelta / 120));
}
//---------------------------------------------------------------------------
void __fastcall TFormObjmove::FormKeyPress(TObject *Sender, System::WideChar &Key)
{
  switch (Key)
  {
	case '1': GLCamera1->MoveAroundTarget(3, 0); break;
	case '2': GLCamera1->MoveAroundTarget(0, -3); break;
	case '3': GLCamera1->MoveAroundTarget(0, 3); break;
	case '4': GLCamera1->MoveAroundTarget(-3, 0); break;
	case '-': GLCamera1->AdjustDistanceToTarget(1.1); break;
	case '+': GLCamera1->AdjustDistanceToTarget(1 / 1.1); break;
	default: ; break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFormObjmove::FormKeyUp(
	TObject* Sender, WORD &Key, TShiftState Shift)
{
	if (CurrentPick) {
		switch (Key) {
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
			case VK_LEFT:
				CurrentPick->Translate(0, -0.3, 0);
				break;
			case VK_RIGHT:
				CurrentPick->Translate(0, 0.3, 0);
				break;
			default:;
		}
	}
}
//---------------------------------------------------------------------------

