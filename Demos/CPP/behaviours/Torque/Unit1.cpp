//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLObjects"
#pragma link "GLPolyhedron"
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
void __fastcall TForm1::FormCreate(TObject *Sender)
{
	// Initialize last time
	lastTime = 24*3600*(Now());
	// Initialize rotation dampings...
	// ...using properties...
	GetOrCreateInertia(Hexahedron->Behaviours)->RotationDamping->Constant = 1;
	GetOrCreateInertia(Hexahedron->Behaviours)->RotationDamping->Linear = 1;
	GetOrCreateInertia(Hexahedron->Behaviours)->RotationDamping->Quadratic = 0;
	// ...using helper function on the TGLBehaviours...
	GetOrCreateInertia(Dodecahedron->Behaviours)->RotationDamping->SetDamping(10, 0, 0.01);
	// ...or using helper function directly on the TGLBaseSceneObject
	GetOrCreateInertia(Octahedron)->RotationDamping->SetDamping(0, 0, 0.01);
	GetOrCreateInertia(Icosahedron)->RotationDamping->SetDamping(10, 0, 0.01);
	GetOrCreateInertia(Tetrahedron)->RotationDamping->SetDamping(0, 0, 0.01);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
	// Mouse moved, get what's underneath
	pickedObject = GLSceneViewer1->Buffer->GetPickedObject(X, Y);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
	// apply some "torque" to the pickedObject if any
  if (pickedObject)
	 GetOrCreateInertia(pickedObject)->ApplyTorque(deltaTime, 200, 0, 0);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
	int i;
	Single mass;

	if (CheckBox1->Checked)
		mass = 2;
	else
		mass = 1;
	// all our objects are child of the DummyCube1
	for (i=0; i < DummyCube1->Count-1; i++)
		GetOrCreateInertia(DummyCube1->Children[i])->Mass = mass;
}
//---------------------------------------------------------------------------
