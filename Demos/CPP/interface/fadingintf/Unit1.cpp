//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
   TIPickTimer->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::TIPickTimerTimer(TObject *Sender)
{
   TPoint cp;
	// get what is under the mouse
   GetCursorPos(&cp);
   cp = GLSceneViewer1->ScreenToClient(cp);
   currentPick = (TGLCustomSceneObject *) (GLSceneViewer1->Buffer->GetPickedObject(cp.X, cp.Y));
   TIPickTimer->Enabled = false;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
	TGLCustomSceneObject *pick;
	// if an object is picked...
	pick = (TGLCustomSceneObject *) (GLSceneViewer1->Buffer->GetPickedObject(X, Y));
	if (pick)
	{
		// ...turn it to yellow and show its name
		pick->Material->FrontProperties->Emission->Color = clrYellow;
		ShowMessage("You clicked the "+pick->Name);
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
	// trigger progression (we don't use time in this sample)
  GLScene1->Progress(0, 0);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SphereProgress(TObject *Sender, const double deltaTime, const double newTime)

{
	TColorVector targetColor;
//	with Sender as TGLCustomSceneObject do begin
	// if we are picked, target color is red, else it is black (no emission)
	if (Sender=currentPick)
	   targetColor = clrRed;
	else
	   targetColor = clrBlack;
		// Set new color at 66% between current and target color
	Sphere->Material->FrontProperties->Emission->
		Color = VectorLerp(targetColor, Sphere->Material->FrontProperties->Emission->
		Color, 0.66);

}
//---------------------------------------------------------------------------
