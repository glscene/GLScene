//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fcExtrusion.h"

#pragma link "fBendingC"
#pragma link "fCutoutStarC"
#pragma link "fNutsnBoltsC"
#pragma link "fPawnC"
#pragma link "fTentaclesC"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TFormExtrusion *FormExtrusion;
//---------------------------------------------------------------------------
__fastcall TFormExtrusion::TFormExtrusion(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormExtrusion::FormCreate(TObject *Sender)
{
  // Bending
  FormBending = new TFormBending(tsBending);
  FormBending->Parent = tsBending;
  FormBending->Align = alClient;
  FormBending->BorderStyle = bsNone;
  FormBending->Show();

  // Cutoutstar
  FormCutoutstar = new TFormCutoutstar(tsCutoutStar);
  FormCutoutstar->Parent = tsCutoutStar;
  FormCutoutstar->Align = alClient;
  FormCutoutstar->BorderStyle = bsNone;
  FormCutoutstar->Show();

  // NutsnBolts
  FormNutsnBolts = new TFormNutsnBolts(tsNutsnBolts);
  FormNutsnBolts->Parent = tsNutsnBolts;
  FormNutsnBolts->Align = alClient;
  FormNutsnBolts->BorderStyle = bsNone;
  FormNutsnBolts->Show();

  // Pawn
  FormPawn = new TFormPawn(tsPawn);
  FormPawn->Parent = tsPawn;
  FormPawn->Align = alClient;
  FormPawn->BorderStyle = bsNone;
  FormPawn->Show();

  // Tentacles
  FormTentacles = new TFormTentacles(tsTentacles);
  FormTentacles->Parent = tsTentacles;
  FormTentacles->Align = alClient;
  FormTentacles->BorderStyle = bsNone;
  FormTentacles->Show();

}
//---------------------------------------------------------------------------

void __fastcall TFormExtrusion::tvExtrusionClick(TObject *Sender)
{
   switch (tvExtrusion->Selected->Index) {
	case 0: {
	  PageControl->ActivePage = tsBending; break;
	}
	case 1: {
	  PageControl->ActivePage = tsCutoutStar; break;
	}
	case 2: {
	  PageControl->ActivePage = tsNutsnBolts; break;
	}
	case 3: {
	  PageControl->ActivePage = tsPawn; break;
	}
	case 4: {
	  PageControl->ActivePage = tsTentacles; break;
	}
	default: {
	  PageControl->ActivePage = tsBending; break;
	}
   }
}
//---------------------------------------------------------------------------

void __fastcall TFormExtrusion::FormShow(TObject *Sender)
{
  PageControl->ActivePage = tsBending;
}
//---------------------------------------------------------------------------

