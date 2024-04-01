//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fcMovements.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

#pragma link "fColumnC"
#pragma link "fEventsC"
#pragma link "fHierarchC"
#pragma link "fManualC"
#pragma link "fObjmoveC"
#pragma link "fPointtoC"
#pragma link "fPongC"
#pragma link "fSmoothNaviC"
#pragma link "fTweeningC"



TFormMovements *FormMovements;
//---------------------------------------------------------------------------
__fastcall TFormMovements::TFormMovements(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormMovements::FormCreate(TObject *Sender)
{
  PageControl->ActivePage = tsMovements;
//  tvMovements->Select(tvMovements->Items[0]);  // goto to column  0
  tvMovementsClick(Sender);
}
//---------------------------------------------------------------------------


void __fastcall TFormMovements::tvMovementsClick(TObject *Sender)
{
 //
	switch (tvMovements->Selected->Index) {
	case 0: {
	  // Column
		FormColumn = new TFormColumn(tsMovements);
		FormColumn->Parent = tsMovements;
		FormColumn->Align = alClient;
		FormColumn->BorderStyle = bsNone;
		FormColumn->Show();
		break;
	}
	case 1: {
	  // Events
		FormEvents = new TFormEvents(tsMovements);
		FormEvents->Parent = tsMovements;
		FormEvents->Align = alClient;
		FormEvents->BorderStyle = bsNone;
		FormEvents->Show();
		break;
	}
	case 2: {
		// Hierarch
		FormHierarch = new TFormHierarch(tsMovements);
		FormHierarch->Parent = tsMovements;
		FormHierarch->Align = alClient;
		FormHierarch->BorderStyle = bsNone;
		FormHierarch->Show();
		break;
	}
	case 3: {
		  // Manual
		FormManual = new TFormManual(tsMovements);
		FormManual->Parent = tsMovements;
		FormManual->Align = alClient;
		FormManual->BorderStyle = bsNone;
		FormManual->Show();
		break;

	}
	case 4: {
		  // Objmove
		FormObjmove = new TFormObjmove(tsMovements);
		FormObjmove->Parent = tsMovements;
		FormObjmove->Align = alClient;
		FormObjmove->BorderStyle = bsNone;
		FormObjmove->Show();
		break;
	}
	case 5: {
		  // Pointto
		FormPointto = new TFormPointto(tsMovements);
		FormPointto->Parent = tsMovements;
		FormPointto->Align = alClient;
		FormPointto->BorderStyle = bsNone;
		FormPointto->Show();
		break;
	}
	case 6: {
		  // Pong
		FormPong = new TFormPong(tsMovements);
		FormPong->Parent = tsMovements;
		FormPong->Align = alClient;
		FormPong->BorderStyle = bsNone;
		FormPong->Show();
		break;
	}
	case 7: {
		  // Smoothnavi
		FormSmoothnavi = new TFormSmoothnavi(tsMovements);
		FormSmoothnavi->Parent = tsMovements;
		FormSmoothnavi->Align = alClient;
		FormSmoothnavi->BorderStyle = bsNone;
		FormSmoothnavi->Show();
		break;
	}
   }

}
//---------------------------------------------------------------------------

