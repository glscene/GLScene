//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fBoomC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Behaviours"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.FireFX"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormBoom* FormBoom;
//---------------------------------------------------------------------------
__fastcall TFormBoom::TFormBoom(TComponent* Owner) : TForm(Owner) {}
//---------------------------------------------------------------------------
void __fastcall TFormBoom::Button1Click(TObject* Sender)
{
    // A button click triggers the small animation sequence
    // first, we enabled the cadencer, to get Progression events
    GLCadencer1->Enabled = True;
    // then we set (or reset) the sphere position and initial speed
    Sphere1->Position->AsVector = NullHmgPoint;
    GetOrCreateInertia(Sphere1)->TranslationSpeed->SetVector(
        Random(), 9, Random());
    // the tagfloat is used as timer (before explosion)
    Sphere1->TagFloat = 3.5;
    // Here we reinitialize the FireFX (starts enabled) and SmokeFX (disabled at first)
    FireFX->ParticleSize = 0.5;
	FireFX->Disabled = false;
    FireFX->FireInit();
    SmokeFX->Disabled = true;
    SmokeFX->FireInit();
}
//---------------------------------------------------------------------------
void __fastcall TFormBoom::GLCadencer1Progress(
    TObject* Sender, const double deltaTime, const double newTime)
{
    // have we exploded yet?
    if (Sphere1->TagFloat > 0) {
        // no, so decrease time before explosion
        Sphere1->TagFloat = Sphere1->TagFloat - deltaTime;
        // explosion time?
        if (Sphere1->TagFloat < 0) {
            // yep! make particles bigger,
            FireFX->ParticleSize = 2;
            // fire the explosion
            if (Random() > 0.5)
                FireFX->IsotropicExplosion(8, 10, 5);
            else
                FireFX->RingExplosion(8, 10, 5, XVector, ZVector);
            // stop the fire trail
            FireFX->Disabled = true;
            // and start the smoke trail
            SmokeFX->Disabled = false;
        }
    }
    // A gravity-like acceleration is applied to the sphere
    GetOrCreateInertia(Sphere1)->ApplyTranslationAcceleration(
        deltaTime, VectorMake(0, -2, 0));
    // restart effect when sphere fell too low
    if (Sphere1->Position->Y < -2)
        Button1Click(this);
}
//---------------------------------------------------------------------------
void __fastcall TFormBoom::Timer1Timer(TObject* Sender)
{
    // standard issue framerate & particle count update
    Caption =
        "Boom - " + Format("%.1f FPS - %d Particles",
                        ARRAYOFCONST((GLSceneViewer1->FramesPerSecond(),
                            FireFX->ParticleCount + SmokeFX->ParticleCount)));
    GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TFormBoom::FormResize(TObject* Sender)
{
    // take care of zooming if window is resize
    GLCamera1->FocalLength = Width * 0.1;
}
//---------------------------------------------------------------------------
void __fastcall TFormBoom::GLSceneViewer1MouseMove(
    TObject* Sender, TShiftState Shift, int X, int Y)
{
    if (Shift.Contains(ssLeft) || Shift.Contains(ssRight)) {
        // move around target
        GLCamera1->MoveAroundTarget(my - Y, mx - X);
        mx = X;
        my = Y;
        GLCadencer1->Progress();
    }
}
//---------------------------------------------------------------------------
void __fastcall TFormBoom::GLSceneViewer1MouseDown(
    TObject* Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
    mx = X;
    my = Y;
}
//---------------------------------------------------------------------------

