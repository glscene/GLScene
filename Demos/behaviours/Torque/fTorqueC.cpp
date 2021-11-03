//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fTorqueC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Objects"
#pragma link "GLS.GeomObjects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.BitmapFont"
#pragma link "GLS.HUDObjects"
#pragma resource "*.dfm"
TFormTorqueC* FormTorqueC;
//---------------------------------------------------------------------------

__fastcall TFormTorqueC::TFormTorqueC(TComponent* Owner) : TForm(Owner) {}
//---------------------------------------------------------------------------

void __fastcall TFormTorqueC::FormCreate(TObject* Sender)
{
    // Initialize last time
    lastTime = double(Now()) * 3600 * 24;
    // Initialize rotation dampings...
    // ...using properties...
    GetOrCreateInertia(Hexahedron->Behaviours)->RotationDamping->Constant = 1;
    GetOrCreateInertia(Hexahedron->Behaviours)->RotationDamping->Linear = 1;
    GetOrCreateInertia(Hexahedron->Behaviours)->RotationDamping->Quadratic = 0;
    // ...using helper function on the TGLBehaviours...
    GetOrCreateInertia(Dodecahedron->Behaviours)
        ->RotationDamping->SetDamping(10, 0, 0.01);
    // ...or using helper function directly on the TGLBaseSceneObject
    GetOrCreateInertia(Octahedron)->RotationDamping->SetDamping(0, 0, 0.01);
    GetOrCreateInertia(Icosahedron)->RotationDamping->SetDamping(10, 0, 0.01);
    GetOrCreateInertia(Tetrahedron)->RotationDamping->SetDamping(0, 0, 0.01);
}
//---------------------------------------------------------------------------
void __fastcall TFormTorqueC::GLSceneViewer1MouseMove(
    TObject* Sender, TShiftState Shift, int X, int Y)
{
    // Mouse moved, get what's underneath
    pickedObject = GLSceneViewer1->Buffer->GetPickedObject(X, Y);
}
//---------------------------------------------------------------------------
void __fastcall TFormTorqueC::GLCadencer1Progress(
    TObject* Sender, const double deltaTime, const double newTime)
{
    // apply some "torque" to the pickedObject if any
    if (pickedObject)
        GetOrCreateInertia(pickedObject)->ApplyTorque(deltaTime, 200, 0, 0);
}
//---------------------------------------------------------------------------
void __fastcall TFormTorqueC::CheckBox1Click(TObject* Sender)
{
    int i;
    Single mass;

    if (CheckBox1->Checked)
        mass = 2;
    else
        mass = 1;
    // all our objects are child of the DummyCube1
    for (i = 0; i < DummyCube1->Count - 1; i++)
        GetOrCreateInertia(DummyCube1->Children[i])->Mass = mass;
}
//---------------------------------------------------------------------------

