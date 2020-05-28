//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBitmapFont"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLObjects"
#pragma link "GLPolyhedron"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLWindowsFont"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  Viewer->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::OptPickModeClick(TObject *Sender)
{
  Gizmo->PickMode = (TGLGizmoPickMode)OptPickMode->ItemIndex;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
  mx = X;
  my = Y;
  Gizmo->ViewerMouseDown(X, Y);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y)
{
  //if noMouseMotion then exit;
  if (Shift.Contains(ssLeft) && (Gizmo->SelectedObj == NULL))
	Camera->MoveAroundTarget(Y - my, mx - X);
  else
  if (Shift.Contains(ssRight) && (Gizmo->SelectedObj == NULL))
  {
	if (my > Y)
	  Camera->AdjustDistanceToTarget(1.05);
	else
	  Camera->AdjustDistanceToTarget(0.95);
	Gizmo->MoveCoef = Camera->DistanceToTarget() / 1000;
  }
  else
	Gizmo->ViewerMouseMove(X, Y);

  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ViewerMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
  Gizmo->ViewerMouseUp(X, Y);
}
//---------------------------------------------------------------------------
// Recurse root object to fill list of pickable objects when using PickMode=pmRaycast
void __fastcall TForm1::FillPickableObjectsList(TGLBaseSceneObject *root, bool doClearList)
{
  int t;
  if (doClearList)
	Gizmo->PickableObjectsWithRayCast->Clear();
  for (t = 0; t < root->Count - 1; t++)
  {
	Gizmo->PickableObjectsWithRayCast->Add(&root[t]);
 // FillPickableObjectsList(&root[t], false);  // raised exception with stack overflow
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormShow(TObject *Sender)
{
  Viewer->SetFocus();
  Gizmo->RootGizmo = RootGizmo;
  // Fill list of pickable objects when using PickMode=pmRaycast
  FillPickableObjectsList(GLDummyCube1, true);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::edAutoZoomFactorKeyPress(TObject *Sender, System::WideChar &Key)

{
  if (Key != ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', ','))
	Key = 0;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox12Click(TObject *Sender)
{
  bool Check;
  // (Sender as TCheckBox).Checked:=Not((Sender as TCheckBox).Checked);
  Check = CheckBox12->Checked;
  switch (CheckBox12->Tag)
  {
	case 0: Gizmo->Enabled = Check; break;
	case 1: Gizmo->ExcludeObjects = Check; break;
	case 2:
	{
	  Gizmo->ForceAxis = Check;
	  CBXAxis->Enabled = Check;
	  break;
	};
	case 3:
	{
	  Gizmo->ForceOperation = Check;
	  CBXOperation->Enabled = Check;
	  break;

	}
	case 4: Gizmo->ForceUniformScale = Check; break;
	case 5: if (Check)
			  Gizmo->GizmoElements = Gizmo->GizmoElements << geAxisLabel;
			else
			  Gizmo->GizmoElements = Gizmo->GizmoElements >> geAxisLabel;
			break;
	case 6:
	{
	  if (Check)
	  {
		Gizmo->GizmoElements = Gizmo->GizmoElements << geObjectInfos;
		CheckBox7->Enabled = Check;
		CheckBox8->Enabled = Check;
		CheckBox9->Enabled = Check;
	  }
	  else
	  {
		Gizmo->GizmoElements = Gizmo->GizmoElements >> geObjectInfos;
		CheckBox7->Enabled = Check;
		CheckBox8->Enabled = Check;
		CheckBox9->Enabled = Check;
	  }
	  break;
	}
	case 7: Gizmo->NoZWrite = Check; break;
	case 8:
	{
	  Gizmo->AutoZoom = Check;
	  if (Check)
	  {
		edAutoZoomFactor->Enabled = true;
		edZoomFactor->Enabled = false;
	  }
	  else
	  {
		edAutoZoomFactor->Enabled = false;
		edZoomFactor->Enabled = true;
	  }
	  break;
	}
	case 9: if (Check)
			  Gizmo->VisibleInfoLabels = Gizmo->VisibleInfoLabels << vliName;
			else
			  Gizmo->VisibleInfoLabels = Gizmo->VisibleInfoLabels >> vliName;
			break;
	case 10: if (Check)
			   Gizmo->VisibleInfoLabels = Gizmo->VisibleInfoLabels << vliOperation;
			else
			  Gizmo->VisibleInfoLabels = Gizmo->VisibleInfoLabels >> vliOperation;
			break;
	case 11: if (Check)
			  Gizmo->VisibleInfoLabels = Gizmo->VisibleInfoLabels << vliCoords;
			else
			 Gizmo->VisibleInfoLabels = Gizmo->VisibleInfoLabels >> vliCoords;
			break;
	case 12: if (Check)
			   Gizmo->GizmoElements = Gizmo->GizmoElements << geMove;
			 else
			   Gizmo->GizmoElements = Gizmo->GizmoElements >> geMove;
			 break;
	case 13: if (Check)
			   Gizmo->GizmoElements = Gizmo->GizmoElements << geRotate;
			 else
			   Gizmo->GizmoElements = Gizmo->GizmoElements >> geRotate;
			 break;
	case 14: if (Check)
			   Gizmo->GizmoElements = Gizmo->GizmoElements << geScale;
			 else
			   Gizmo->GizmoElements = Gizmo->GizmoElements >> geScale;
			 break;
  default:
	  ;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBXAxisChange(TObject *Sender)
{
  switch (CBXAxis->ItemIndex)
  {
  case 0: Gizmo->SelAxis = gaNone; break;
  case 1: Gizmo->SelAxis = gaX; break;
  case 2: Gizmo->SelAxis = gaXY; break;
  case 3: Gizmo->SelAxis = gaXZ; break;
  case 4: Gizmo->SelAxis = gaY; break;
  case 5: Gizmo->SelAxis = gaYZ; break;
  case 6: Gizmo->SelAxis = gaZ; break;
  default: ;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBXOperationChange(TObject *Sender)
{
 switch (CBXOperation->ItemIndex)
 {
  case 0: Gizmo->Operation = gopNone; break;
  case 1: Gizmo->Operation = gopMove; break;
  case 2: Gizmo->Operation = gopRotate; break;
  case 3: Gizmo->Operation = gopScale; break;
 default:
	 ;
 }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::edMoveCoefChange(TObject *Sender)
{
  if (edMoveCoef->Text != "")
	Gizmo->MoveCoef = StrToFloatDef(edMoveCoef->Text);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::edRotateCoefChange(TObject *Sender)
{
  if (edRotateCoef->Text != "")
	Gizmo->RotationCoef = StrToFloatDef(edRotateCoef->Text);

}
//---------------------------------------------------------------------------
void __fastcall TForm1::edGizmoThicknessChange(TObject *Sender)
{
  Gizmo->GizmoThickness = StrToFloatDef(edGizmoThickness->Text);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::edScaleCoefChange(TObject *Sender)
{
  if (edScaleCoef->Text != "")
	Gizmo->ScaleCoef = StrToFloatDef(edScaleCoef->Text);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::edAutoZoomFactorChange(TObject *Sender)
{
  if (edAutoZoomFactor->Text != "")
	Gizmo->AutoZoomFactor = StrToFloatDef(edAutoZoomFactor->Text);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::edZoomFactorChange(TObject *Sender)
{
  if (edZoomFactor->Text != "")
	Gizmo->ZoomFactor = StrToFloatDef(edZoomFactor->Text);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ColorBox1Change(TObject *Sender)
{
  switch (ColorBox1->Tag)
  {
   case	0: {Gizmo->BoundingBoxColor->AsWinColor = ColorBox1->Selected; break;}
   case	1: {Gizmo->VisibleInfoLabelsColor->AsWinColor = ColorBox2->Selected;  break;}
   case	2: {Gizmo->SelectedColor->AsWinColor = ColorBox3->Selected;  break;}
  default:
	  ;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Gizmo = new TGLGizmo(this);
  Gizmo->LabelFont = WindowsBitmapFont;
  Gizmo->Viewer = Viewer;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormDestroy(TObject *Sender)
{
  Gizmo->Free();
}
//---------------------------------------------------------------------------
