// ---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fActorMS3dC.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLSL.CustomShader"
#pragma link "GLS.FBORenderer"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"

#pragma link "GLS.Utils"
#pragma link "GLS.GeomObjects"
#pragma link "GLS.Graphics"
#pragma link "GLS.VectorTypes"
#pragma link "GLS.RenderContextInfo"
#pragma link "GLS.ShadowPlane"
#pragma link "GLS.VectorGeometry"
#pragma link "GLS.Mesh"
#pragma link "GLS.Windows"
#pragma link "GLS.State"
#pragma link "GLS.Context"

#pragma link "GLS.ArchiveManager"
#pragma link "GLS.FileZLIB"
#pragma link "GLS.TextureFormat"

#pragma link "GLS.Scene"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.CompositeImage"

#pragma link "GLS.FileMS3D"
#pragma link "GLS.FileJPEG"
#pragma link "GLS.FilePNG"
#pragma link "GLS.FileTGA"

#pragma resource "*.dfm"
TForm1* Form1;

int mdx, mdy;
TMatrix4f FBiasMatrix;
TMatrix4f FLightModelViewMatrix;
TMatrix4f FLightProjMatrix;
TGLMatrix FInvCameraMatrix;
TMatrix4f FEyeToLightMatrix;

TMatrix4f FLightModelViewMatrix2;
TMatrix4f FLightProjMatrix2;
TMatrix4f FInvCameraMatrix2;
TMatrix4f FEyeToLightMatrix2;

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {}
// ---------------------------------------------------------------------------

void __fastcall TForm1::Actor1EndFrameReached(TObject* Sender)
{
	if (Actor1->AnimationMode == aamNone) {
        btnStartStop->Caption = "Start";
        Timer1->Enabled = false;
        aniPos->Enabled = true;
    }
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::aniBoxSelect(TObject* Sender)
{
    Actor1->AnimationMode = aamNone;
    if (aniBox->ItemIndex != -1) {
        Chair1->Visible = aniBox->ItemIndex == 6;
        Timer1->Enabled = false;
        aniPos->Enabled = false;
        Actor1->SwitchToAnimation(aniBox->Text, false);

        aniPos->Min = 0;
        aniPos->Max = Actor1->EndFrame - Actor1->StartFrame;
        aniPos->Position = 0;
        aniPos->Enabled = true;
        btnStartStop->Caption = "Start";
    }
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::aniPosChange(TObject* Sender)
{
    if (aniPos->Enabled) {
        Actor1->CurrentFrame = Actor1->StartFrame + aniPos->Position;
    }
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::btnStartStopClick(TObject* Sender)
{
    if (Actor1->AnimationMode == aamNone) {
        if (Actor1->CurrentFrame == Actor1->EndFrame) {
            Actor1->CurrentFrame = Actor1->StartFrame;
        }
        Actor1->AnimationMode = aamPlayOnce;
        btnStartStop->Caption = "Stop";
        Timer1->Enabled = true;
        aniPos->Enabled = false;
    } else {
        Actor1->AnimationMode = aamNone;
        btnStartStop->Caption = "Start";
        Timer1->Enabled = false;
        aniPos->Enabled = true;
    }
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject* Sender)
{
    Actor1->NextFrame();
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::Button4Click(TObject* Sender)
{
    Actor1->PrevFrame();
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::FormCloseQuery(TObject* Sender, bool &CanClose)
{
	Actor1->AnimationMode = aamNone;
	GLCadencer1->Enabled = false;
}

// ---------------------------------------------------------------------------
void _fastcall TForm1::LoadTexture(String AName, String ext)
{
	TGLCompositeImage* img;
	TStream* strm = new TStream();
	img = (TGLCompositeImage*)MatLib->TextureByName(AName)->Image;
	strm = GLSArchiveManager1->Archives->Items[0]->GetContent(
		"Main/" + AName + "." + ext);
	img->LoadFromStream(strm);
}

void __fastcall TForm1::FormCreate(TObject* Sender)
{
	TFileName Path = GetCurrentAssetPath();

	// Loading an archive, to edit it you can use  ..\utilities\ArchiveEdit
	SetCurrentDir(Path + "//modelext//");
	GLSArchiveManager1->Archives->Items[0]->LoadFromFile("ActorMS3D.zlib");
	Actor1->LoadFromStream("Woman4.ms3d", GLSArchiveManager1->Archives->Items[0]->GetContent("Main/Woman4.ms3d"));
	Chair1->LoadFromStream("Chair.ms3d", GLSArchiveManager1->Archives->Items[0]->GetContent("Main/Chair.ms3d"));

	// Loading textures from the archive as composite images and assigned to MatLib
	LoadTexture("Hair", "png");
	LoadTexture("Chair", "png");

	// Loading skins
	SetCurrentDir(Path + "//skin//");
	Actor1->Material->LibMaterialName = MatLib->Materials->Items[4]->Name;  //"Woman4_skin";

	// Loading other textures as assets directly to objects
	SetCurrentDir(Path + "//texture//");
	Globus->Material->Texture->Image->LoadFromFile("Earth.jpg");
	GLPlane1->Material->Texture->Image->LoadFromFile("floor_parquet.jpg");


	Actor1->AnimationMode = aamNone;
	Actor1->Scale->SetVector(0.1, 0.1, 0.1, 0);
	Chair1->Scale->SetVector(0.35, 0.35, 0.35, 0);

	Actor1->Animations->Add();
	Actor1->Animations->Items[0]->Reference = aarSkeleton;
	Actor1->Animations->Items[0]->StartFrame = 2;
	Actor1->Animations->Items[0]->EndFrame = 855;
	Actor1->Animations->Items[0]->Name = "Dance";

	Actor1->Animations->Items[1]->Reference = aarSkeleton;
	Actor1->Animations->Items[1]->StartFrame = 856;
	Actor1->Animations->Items[1]->EndFrame = 1166;
	Actor1->Animations->Items[1]->Name = "Sexy Walk";
	Actor1->Animations->Add();

	Actor1->Animations->Items[2]->Reference = aarSkeleton;
	Actor1->Animations->Items[2]->StartFrame = 1168;
	Actor1->Animations->Items[2]->EndFrame = 1203;
	Actor1->Animations->Items[2]->Name = "Cartwheel";
	Actor1->Animations->Add();

	Actor1->Animations->Items[3]->Reference = aarSkeleton;
	Actor1->Animations->Items[3]->StartFrame = 1205;
	Actor1->Animations->Items[3]->EndFrame = 1306;
	Actor1->Animations->Items[3]->Name = "Hand Flip";
	Actor1->Animations->Add();

	Actor1->Animations->Items[4]->Reference = aarSkeleton;
	Actor1->Animations->Items[4]->StartFrame = 1308;
	Actor1->Animations->Items[4]->EndFrame = 1395;
	Actor1->Animations->Items[4]->Name = "Wave";
	Actor1->Animations->Add();

	Actor1->Animations->Items[5]->Reference = aarSkeleton;
	Actor1->Animations->Items[5]->StartFrame = 1397;
	Actor1->Animations->Items[5]->EndFrame = 2014;
	Actor1->Animations->Items[5]->Name = "Sun Salutation";
	Actor1->Animations->Add();

	Actor1->Animations->Items[6]->Reference = aarSkeleton;
	Actor1->Animations->Items[6]->StartFrame = 2016;
	Actor1->Animations->Items[6]->EndFrame = 2133;
	Actor1->Animations->Items[6]->Name = "Sit";
	Actor1->Animations->Add();

	FBiasMatrix = CreateScaleAndTranslationMatrix(
		VectorMake(0.5, 0.5, 0.5), VectorMake(0.5, 0.5, 0.5));
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::FormShow(TObject* Sender)
{
	aniBox->ItemIndex = 0;
	aniBoxSelect(Sender);
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(
	TObject* Sender, const double deltaTime, const double newTime)
{
	TAffineVector af, af2, pv, pv2;
	GLCamera2->Position->Rotate(VectorMake(0, 1, 0), deltaTime * 0.1);
	af = Actor1->Skeleton->CurrentFrame->Position->Items[0];
	ScaleVector(af, Actor1->Scale->AsAffineVector);
	af2 = GLCamera2->Position->AsAffineVector;
	pv = VectorSubtract(af, af2);
	NormalizeVector(pv);
	GLCamera2->Direction->AsAffineVector = pv;
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::GLDirectOpenGL1Render(
	TObject* Sender, TGLRenderContextInfo &rci)

{
	FInvCameraMatrix = *rci.PipelineTransformation->InvModelViewMatrix;
	FEyeToLightMatrix = MatrixMultiply(FInvCameraMatrix, FLightModelViewMatrix);
	FEyeToLightMatrix = MatrixMultiply(FEyeToLightMatrix, FLightProjMatrix);
	FEyeToLightMatrix = MatrixMultiply(FEyeToLightMatrix, FBiasMatrix);
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::GLFrameBufferAfterRender(
	TObject* Sender, TGLRenderContextInfo &rci)

{
	CurrentGLContext()->GLStates->Disable(stPolygonOffsetFill);
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::GLFrameBufferBeforeRender(
	TObject* Sender, TGLRenderContextInfo &rci)

{
	FLightModelViewMatrix =
		*CurrentGLContext()->PipelineTransformation->ModelViewMatrix;
	FLightProjMatrix =
		*CurrentGLContext()->PipelineTransformation->ProjectionMatrix;
	CurrentGLContext()->GLStates->Enable(stPolygonOffsetFill);
	CurrentGLContext()->GLStates->PolygonOffsetFactor = 2;
	CurrentGLContext()->GLStates->PolygonOffsetUnits = 2;
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject* Sender)
{
    aniPos->Position =
        Actor1->CurrentFrame -
        Actor1->Animations->Items[aniBox->ItemIndex + 1]->StartFrame;
}
// ---------------------------------------------------------------------------

