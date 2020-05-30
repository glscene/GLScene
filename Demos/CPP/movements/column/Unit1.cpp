//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "Math.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;

const int
	cNbPlanes = 30;
const int
	cStackHeight = 8;


//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
	int i;
	TGLPlane *plane;
	// our column is just a stack of planes
	for (i=0; i < cNbPlanes-1; i++)
	{
		// create planes as child of the dummycube
		plane = (TGLPlane *)(DummyCube1->AddNewChild(__classid (TGLPlane)));
		// default plane size is 1x1, we want bigger planes !
		plane->Width = 2;
		plane->Height = 2;
		// orient and position then planes in the stack
		plane->Position->Y = cStackHeight*(0.5-(float)i/cNbPlanes);
		plane->Direction->AsVector = YHmgVector;
		// we use the emission color, since there is no light in the scene
		// (allows 50+ FPS with software opengl on <400 Mhz CPUs ;)
		plane->Material->FrontProperties->Emission->Color = VectorLerp(clrBlue, clrYellow, i/cNbPlanes);
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
	int i;
	// for all planes (all childs of the dummycube)
	for (i=0; i < DummyCube1->Count-1; i++)
		// roll them accordingly to our time reference and position in the stack
		DummyCube1->Children[i]->RollAngle = 90*cos(newTime+i*M_PI/cNbPlanes);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
	// update FPS and reset counter for the next second
	StaticText1->Caption = Format("%.1f FPS",
	  ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond())));
	GLSceneViewer1->ResetPerformanceMonitor();

}
//---------------------------------------------------------------------------
