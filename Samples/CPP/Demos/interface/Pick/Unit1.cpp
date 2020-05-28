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

	TGLCustomSceneObject *pick;

	// find what's under the mouse
	pick = (TGLCustomSceneObject *) GLSceneViewer1->Buffer->GetPickedObject(X, Y);
	// if it has changed since last MouseMove...
	if (pick != oldPick)
	{
		// ...turn to black previous "hot" object...
		if (oldPick)
			oldPick->Material->FrontProperties->Emission->Color = clrBlack;
		// ...and heat up the new selection...
		if  (pick)
			pick->Material->FrontProperties->Emission->Color = clrRed;
		// ...and don't forget it !
		oldPick = pick;
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{

	TGLCustomSceneObject *pick;
	// if an object is picked...
	pick = (TGLCustomSceneObject *) GLSceneViewer1->Buffer->GetPickedObject(X, Y);
	if (pick)
		// ...turn it to yellow and show its name
		pick->Material->FrontProperties->Emission->Color = clrYellow;
		ShowMessage("You clicked the "+pick->Name);

}
//---------------------------------------------------------------------------

