//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "fGizmoExC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.BitmapFont"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.GeomObjects"
#pragma link "GLS.Graph"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.WindowsFont"
#pragma link "GLS.GizmoEx"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}

void __fastcall TForm1::UpdateTreeView()
{
   int I;
   TTreeNode *ObjectNode;
   TTreeNode *CurrentNode;

   TreeView1->Items->Clear();
   // -- add two root nodes --
   ObjectNode = TreeView1->Items->AddFirst(NULL, "RootTempObjects");
   // -- get the object's tree --
   TreeView1->Items->BeginUpdate();
   // -- objects (with children too) --
   if (RootTempObjects)
	   {
	 ObjectNode->Data = RootTempObjects;
	   for (I = 0; I < RootTempObjects->Count -1; I++)
		   {
//			 AddNodes(ObjectNode, Children[I]);
		 if (IsSubComponent(RootTempObjects->Children[I]))  {
		   ObjectNode = TreeView1->Selected;
		   exit;
		 }
		 else {
		   CurrentNode = TreeView1->Items->
			  AddChildObject(ObjectNode , RootTempObjects->Children[I]->Name, RootTempObjects->Children[I]);
		   for (I = 0; I < RootTempObjects->Children[I]->Count -1; I++)
//			 AddNodes(ObjectNode, Children[I]);
		 if (IsSubComponent(RootTempObjects->Children[I]))  {
		   ObjectNode = TreeView1->Selected;
		   exit;
		 }
		 else {
		   CurrentNode = TreeView1->Items->
			  AddChildObject(ObjectNode , RootTempObjects->Children[I]->Name, RootTempObjects->Children[I]);
		 }
		}
		 ObjectNode->Expand(true);
	}
	TreeView1->Items->EndUpdate();

	// -- add two root nodes --
	ObjectNode = TreeView1->Items->AddFirst(NULL, "World");
	// -- get the object's tree --
	TreeView1->Items->BeginUpdate();
	   // -- objects (with children too) --
   if (GLRootObjects)
	   {
	   ObjectNode->Data = GLRootObjects;
	   for (I = 0; I < RootTempObjects->Count -1; I++)
	   {
//		 AddNodes(ObjectNode, Children[I]);
		 if (IsSubComponent(RootTempObjects->Children[I]))  {
		   ObjectNode = TreeView1->Selected;
		   exit;
		 }
		 else {
		   CurrentNode = TreeView1->Items->
			  AddChildObject(ObjectNode , RootTempObjects->Children[I]->Name, RootTempObjects->Children[I]);
		 }
	   }
	   ObjectNode->Expand(true);
	   }
	TreeView1->Items->EndUpdate();
  }
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Gizmo = new TGLGizmoEx(this);
  Gizmo->LabelFont = WindowsBitmapFont;
  Gizmo->Viewer = Viewer;
  Gizmo->ExcludeClassnameList->Add("TGLSphere");
  FCreationScenarious = -1;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormDestroy(TObject *Sender)
{
  Gizmo->Free();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)

{
  Gizmo->CanAddObjToSelectionList = (Key==vkControl);
  Gizmo-> CanRemoveObjFromSelectionList = (Key==vkMenu);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyUp(TObject *Sender, WORD &Key, TShiftState Shift)
{
  Gizmo->CanAddObjToSelectionList = false;
  Gizmo->CanRemoveObjFromSelectionList = false;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  Camera->AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
  Gizmo->UpdateGizmo();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  Viewer->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  Panel1->Caption = Viewer->FramesPerSecondText();
  Viewer->ResetPerformanceMonitor();

  if (GLScene1->IsUpdating())
	UpdateTreeView();
}
//---------------------------------------------------------------------------

