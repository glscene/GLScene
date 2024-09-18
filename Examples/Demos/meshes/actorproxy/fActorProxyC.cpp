//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fActorProxyC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.GeomObjects"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.ProxyObjects"
#pragma link "GLS.Scene"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.FileSMD"
#pragma link "GLS.BaseClasses"
#pragma resource "*.dfm"
TFormActorProxy *FormActorProxy;
//---------------------------------------------------------------------------
__fastcall TFormActorProxy::TFormActorProxy(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormActorProxy::FormCreate(TObject *Sender)
{
  int i;

  TFileName Path = GetCurrentAssetPath();
  SetCurrentDir(Path  + "\\modelext");
  MasterActor->LoadFromFile("TRINITYrage.smd");
  MasterActor->AddDataFromFile("run.smd");
  MasterActor->AddDataFromFile("jump.smd");

  MasterActor->Animations->Items[0]->Name = "still";
  MasterActor->Animations->Items[1]->Name = "walk";
  MasterActor->Animations->Items[2]->Name = "jump";

  for (i = 0; i < MasterActor->Animations->Count-1; i++)
  {
	MasterActor->Animations->Items[i]->MakeSkeletalTranslationStatic();
	MasterActor->SwitchToAnimation(i); // forces animations to be initialized for ActorsProxies
  }
  MasterActor->SwitchToAnimation(0);   // revert back to empty animation (not necessary)
  MasterActor->AnimationMode = aamLoop; // animationmode is shared between proxies.

  GLActorProxy1->StoreBonesMatrix = true;
  GLActorProxy2->StoreBonesMatrix = true;


  GLActorProxy1->Animation = MasterActor->Animations->Items[1]->Name;
  GLActorProxy2->Animation = MasterActor->Animations->Items[2]->Name;

}

//---------------------------------------------------------------------------
void __fastcall TFormActorProxy::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
	 mouseX = X;
	 mouseY = Y;
 }
//---------------------------------------------------------------------------

void __fastcall TFormActorProxy::DoRaycastStuff()
{
   TGLVector rayStart;
   TGLVector rayVector;
   TGLVector iPoint;
   TGLVector iNormal;

   SetVector(rayStart, GLCamera1->AbsolutePosition);
   SetVector(rayVector, GLSceneViewer1->Buffer->ScreenToVector(
             AffineVectorMake(mouseX, GLSceneViewer1->Height-mouseY, 0)));
	 NormalizeVector(rayVector);

	 if (GLActorProxy1->RayCastIntersect(
		 rayStart, rayVector, &iPoint, &iNormal))
	 {
		GLSphere1->Position->AsVector = iPoint;
		GLSphere1->Direction->AsVector = VectorNormalize(iNormal);
	 }
	 else
	 if (GLActorProxy2->RayCastIntersect(rayStart,rayVector,&iPoint,&iNormal))
	 {
		GLSphere1->Position->AsVector = iPoint;
		GLSphere1->Direction->AsVector = VectorNormalize(iNormal);
	 }
	 else
	 {
		GLSphere1->Position->AsVector = rayStart;
		GLSphere1->Direction->AsVector = rayVector;
	 }

}


//---------------------------------------------------------------------------
void __fastcall TFormActorProxy::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
    // Align object to hand
    *GLArrowLine1->Matrix = GLActorProxy1->BoneMatrix("Bip01 R Finger1");
    *GLArrowLine2->Matrix = GLActorProxy2->BoneMatrix("Bip01 R Finger1");

    // turn actors
    if (chbActorsAreTurning->Checked) {
        GLActorProxy1->Turn(-deltaTime * 130);
        GLActorProxy2->Turn(deltaTime * 100);
    }

	// show master actor
	dcInvisible->Visible = chbShowMasterActor->Checked;

    DoRaycastStuff();
}

void __fastcall TFormActorProxy::Timer1Timer(TObject *Sender)
{
   Panel1->Caption = GLSceneViewer1->FramesPerSecondText(0);
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------



