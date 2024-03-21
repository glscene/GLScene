//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fcMeshes.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TFormMeshes *FormMeshes;
//---------------------------------------------------------------------------
__fastcall TFormMeshes::TFormMeshes(TComponent* Owner)
	: TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TFormMeshes::FormCreate(TObject *Sender)
{
   tvMeshesClick(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TFormMeshes::tvMeshesClick(TObject *Sender)
{
	switch (tvMeshes->Selected->Index) {
	case 0: {
	  // Actor
		FormActor = new TFormActor(pnMeshes);
		FormActor->Parent = pnMeshes;
		FormActor->Align = alClient;
		FormActor->BorderStyle = bsNone;
		FormActor->Show();
		break;
	}
	case 1: {
	  // ActorProxy
		FormActorProxy = new TFormActorProxy(pnMeshes);
		FormActorProxy->Parent = pnMeshes;
		FormActorProxy->Align = alClient;
		FormActorProxy->BorderStyle = bsNone;
		FormActorProxy->Show();
		break;
	}
	case 2: {
		// ActorTwocam
		FormActorTwocam = new TFormActorTwocam(pnMeshes);
		FormActorTwocam->Parent = pnMeshes;
		FormActorTwocam->Align = alClient;
		FormActorTwocam->BorderStyle = bsNone;
		FormActorTwocam->Show();
		break;
	}
/*
	case 3: {
		  // Manual
		FormManual = new TFormManual(pnMeshes);
		FormManual->Parent = pnMeshes;
		FormManual->Align = alClient;
		FormManual->BorderStyle = bsNone;
		FormManual->Show();
		break;

	}
	case 4: {
		  // Objmove
		FormObjmove = new TFormObjmove(pnMeshes);
		FormObjmove->Parent = pnMeshes;
		FormObjmove->Align = alClient;
		FormObjmove->BorderStyle = bsNone;
		FormObjmove->Show();
		break;
	}
	case 5: {
		  // Pointto
		FormPointto = new TFormPointto(pnMeshes);
		FormPointto->Parent = pnMeshes;
		FormPointto->Align = alClient;
		FormPointto->BorderStyle = bsNone;
		FormPointto->Show();
		break;
	}
	case 6: {
		  // Pong
		FormPong = new TFormPong(pnMeshes);
		FormPong->Parent = pnMeshes;
		FormPong->Align = alClient;
		FormPong->BorderStyle = bsNone;
		FormPong->Show();
		break;
	}
	case 7: {
		  // Smoothnavi
		FormSmoothnavi = new TFormSmoothnavi(pnMeshes);
		FormSmoothnavi->Parent = pnMeshes;
		FormSmoothnavi->Align = alClient;
		FormSmoothnavi->BorderStyle = bsNone;
		FormSmoothnavi->Show();
		break;
	}
	case 8: {
		  // Tweening
		FormTweening = new TFormTweening(pnMeshes);
		FormTweening->Parent = pnMeshes;
		FormTweening->Align = alClient;
		FormTweening->BorderStyle = bsNone;
		FormTweening->Show();
		break;
	}
    */
   }

}
//---------------------------------------------------------------------------

