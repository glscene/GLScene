// ---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fOdeMachineC.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.BitmapFont"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.GeomObjects"
#pragma link "GLS.HUDObjects"
#pragma link "GLS.Objects"
#pragma link "Physics.ODEManager"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.WindowsFont"
#pragma resource "*.dfm"
TForm1 *Form1;

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender,
	TMouseButton Button, TShiftState Shift, int X, int Y) {
	my = Y;
	mx = X;
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender,
	TShiftState Shift, int X, int Y) {
	if (Shift.Contains(ssShift))
		GLCamera1->MoveAroundTarget(my - Y, mx - X);
	my = Y;
	mx = X;
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender,
	const double deltaTime, const double newTime) {
	GLODEManager1->Step(deltaTime);

	/*
	 PdVector3 velWheel = dBodyGetAngularVel(TGLODEDynamic(Wheel->Behaviours)->Body);
	 PdVector3 velPin2 = dBodyGetLinearVel(TGLODEDynamic(Pin2->Behaviours[0])->Body);
	 GLHUDText1->Text = Format(
	 "Wheel Angular Velocity (Y-Axis) = %.1f\r\
	 Pin2 Linear Velocity (X-Axis) = %.1f",
	 ARRAYOFCONST((velWheel[1], velPin2[0])));
	 */
}
// ---------------------------------------------------------------------------
