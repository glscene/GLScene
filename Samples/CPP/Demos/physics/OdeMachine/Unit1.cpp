// ---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBitmapFont"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLHUDObjects"
#pragma link "GLObjects"
#pragma link "GLODEManager"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLWindowsFont"
#pragma resource "*.dfm"
TForm1 *Form1;

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {
	TAffineVector av = {-250, 0, 0};
	((TGLODEDynamic*)Pin2->Behaviours->Items[0])->AddForce(av);
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
