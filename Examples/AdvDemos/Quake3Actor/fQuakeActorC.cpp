// ---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fQuakeActorC.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.ParticleFX"
#pragma link "GLS.Scene"
#pragma link "GLS.ShadowPlane"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.FileMD3"

#pragma link "GLS.ParticleFX"
#pragma resource "*.dfm"
TFormQuakeActor* FormQuakeActor;
String ModelPath;

// ---------------------------------------------------------------------------
__fastcall TFormQuakeActor::TFormQuakeActor(TComponent* Owner) : TForm(Owner) {}

// ---------------------------------------------------------------------------
void __fastcall TFormQuakeActor::FormCreate(TObject* Sender)
{
    // Path to models
    ModelPath = ExtractFilePath(ParamStr(0));
    SetCurrentDir(ModelPath);
    // Build the model
    BuildModel();

    ModelCube->Scale->SetVector(0.044, 0.044, 0.044);

    Legs->AnimationMode = aamLoop;
    Torso->AnimationMode = aamLoop;

    // Populate the combo boxes with the names of the
    // loaded animations
    Legs->Animations->SetToStrings(ComboBox1->Items);
    Torso->Animations->SetToStrings(ComboBox2->Items);

    // Set up some initial animations
    ComboBox1->ItemIndex = ComboBox1->Items->IndexOf("LEGS_IDLE");
    ComboBox2->ItemIndex = ComboBox2->Items->IndexOf("TORSO_STAND");

    // And trigger them
    ComboBox1Change(NULL);
    ComboBox2Change(NULL);
}

// ---------------------------------------------------------------------------
TGLMatrix __fastcall TFormQuakeActor::InterpolateMatrix(
    TGLMatrix m1, TGLMatrix m2, float delta)
{
    int i, j;
    TGLMatrix mat;
    // This is used for interpolating between 2 matrices. The result
    // is used to reposition the model parts each frame.
    //
    for (j = 0; j < 3; j++)
        for (i = 0; i < 3; i++)
            mat.V[i].V[j] =
                m1.V[i].V[j] + (m2.V[i].V[j] - m1.V[i].V[j]) * delta;
    return mat;
}

// ---------------------------------------------------------------------------
void __fastcall TFormQuakeActor::BuildModel()
{
    // Load model data from MD3 files into the actor
    //
    Legs->LoadFromFile(ModelPath + "\\model\\lower.md3");
    Torso->LoadFromFile(ModelPath + "\\model\\upper.md3");
    Head->LoadFromFile(ModelPath + "\\model\\head.md3");
    Weapon->LoadFromFile(ModelPath + "\\model\\plasma.md3");

    // Load the required tag lists
    // These are used to locally transform the separate
    // parts of the model into the correct places
    //
    LegsTags = new TMD3TagList;
    LegsTags->LoadFromFile(ModelPath + "\\model\\lower.md3");
    TorsoTags = new TMD3TagList;
    TorsoTags->LoadFromFile(ModelPath + "\\model\\upper.md3");

    // The tag_flash tag in the railgun model gives the
    // transform offset for the nozzle of the gun. I've
    // added a GunSmoke dummycube there to demonstrate with
    // a smoke like effect
    //
    WeaponTags = new TMD3TagList;
    WeaponTags->LoadFromFile(ModelPath + "\\model\\plasma.md3");
    *GunSmoke->Matrix = WeaponTags->GetTransform("tag_flash", 0);

    // Apply textures to preloaded materials
    // The md3 file loader puts a material into the actors
    // assigned material library (if there is one) with
    // the names of the mesh objects. The skin and/or shader
    // files can tell you which objects need which textures loaded
    //
    LoadQ3Skin(ModelPath + "\\model\\lower_default.skin", Legs);
    LoadQ3Skin(ModelPath + "\\model\\upper_default.skin", Torso);
    LoadQ3Skin(ModelPath + "\\model\\head_default.skin", Head);

    // Load the weapon textures
    //
    MatLib->Materials->GetLibMaterialByName("plasma2")
        ->Material->Texture->Image->LoadFromFile(
            ModelPath + "\\model\\plasma2.jpg");

    // Load the animation data from the cfg file
    // This procedure populates an animation list from a
    // file or TStrings object. The last parameter tells
    // it which class of animation is to be loaded.
    //
    LoadQ3Anims(Legs->Animations, ModelPath + "\\model\\animation.cfg", "BOTH");
    LoadQ3Anims(Legs->Animations, ModelPath + "\\model\\animation.cfg", "LEGS");
    LoadQ3Anims(
        Torso->Animations, ModelPath + "\\model\\animation.cfg", "BOTH");
    LoadQ3Anims(
        Torso->Animations, ModelPath + "\\model\\animation.cfg", "TORSO");
}

// ---------------------------------------------------------------------------

void __fastcall TFormQuakeActor::ComboBox1Change(TObject* Sender)
{
    Legs->SwitchToAnimation(ComboBox1->Text, false);
}

// ---------------------------------------------------------------------------
void __fastcall TFormQuakeActor::ComboBox2Change(TObject* Sender)
{
    Torso->SwitchToAnimation(ComboBox2->Text, false);
}

// ---------------------------------------------------------------------------
void __fastcall TFormQuakeActor::GLCadencer1Progress(
    TObject* Sender, const double deltaTime, const double newTime)
{
    TGLMatrix m1;
    TGLMatrix m2;

    // Set the transform for the torso
    m1 = LegsTags->GetTransform("tag_torso", Legs->CurrentFrame);
    m2 = LegsTags->GetTransform("tag_torso", Legs->NextFrameIndex());
    *Torso->Matrix = InterpolateMatrix(m1, m2, Legs->CurrentFrameDelta);
    Torso->Roll(-TrackBar1->Position);
    Torso->Turn(-TrackBar2->Position);

    // Set the transform for the head
    m1 = TorsoTags->GetTransform("tag_head", Torso->CurrentFrame);
    m2 = TorsoTags->GetTransform("tag_head", Torso->NextFrameIndex());
    *Head->Matrix = InterpolateMatrix(m1, m2, Torso->CurrentFrameDelta);
    Head->Roll(-TrackBar3->Position);
    Head->Turn(-TrackBar4->Position);

    // Set the transform for the weapon
    m1 = TorsoTags->GetTransform("tag_weapon", Torso->CurrentFrame);
    m2 = TorsoTags->GetTransform("tag_weapon", Torso->NextFrameIndex());
    *Weapon->Matrix = InterpolateMatrix(m1, m2, Torso->CurrentFrameDelta);

    GLSceneViewer1->Invalidate();
}

// ---------------------------------------------------------------------------
void __fastcall TFormQuakeActor::GLSceneViewer1MouseDown(
    TObject* Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
    mx = X;
    my = Y;
}

// ---------------------------------------------------------------------------
void __fastcall TFormQuakeActor::GLSceneViewer1MouseMove(
    TObject* Sender, TShiftState Shift, int X, int Y)
{
    if (Shift.Contains(ssLeft))
        GLCamera1->MoveAroundTarget(my - Y, mx - X);
    if (Shift.Contains(ssRight))
        GLCamera1->AdjustDistanceToTarget(Power(1.05, my - Y));
    mx = X;
    my = Y;
}

// ---------------------------------------------------------------------------
void __fastcall TFormQuakeActor::Timer1Timer(TObject* Sender)
{
	Caption = "Quake Actor " + GLSceneViewer1->FramesPerSecondText(0);
	GLSceneViewer1->ResetPerformanceMonitor();
}

// ---------------------------------------------------------------------------
void __fastcall TFormQuakeActor::ComboSkinChange(TObject* Sender)
{
	switch (ComboSkin->ItemIndex) {
		case 0: {
			LoadQ3Skin(ModelPath + "\\model\\lower_default.skin", Legs);
			LoadQ3Skin(ModelPath + "\\model\\upper_default.skin", Torso);
			LoadQ3Skin(ModelPath + "\\model\\head_default.skin", Head);
			break;
		}
		case 1: {
			LoadQ3Skin(ModelPath + "\\model\\lower_red.skin", Legs);
			LoadQ3Skin(ModelPath + "\\model\\upper_red.skin", Torso);
			LoadQ3Skin(ModelPath + "\\model\\head_red.skin", Head);
			break;
		}
		case 2: {
            LoadQ3Skin(ModelPath + "\\model\\lower_blue.skin", Legs);
            LoadQ3Skin(ModelPath + "\\model\\upper_blue.skin", Torso);
            LoadQ3Skin(ModelPath + "\\model\\head_blue.skin", Head);
            break;
        }
        default:;
    }
}
// ---------------------------------------------------------------------------

void __fastcall TFormQuakeActor::FormDestroy(TObject* Sender)
{
    LegsTags->Free();
    TorsoTags->Free();
    WeaponTags->Free();
}
// ---------------------------------------------------------------------------

