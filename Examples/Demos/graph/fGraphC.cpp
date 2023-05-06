//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fGraphC.h"
#include "fFxyC.h"
#include "fHeightFieldC.h"
#include "fPointsC.h"
#include "fProjectionC.h"
#include "fSplinesC.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TFormGraphC *FormGraphC;
//---------------------------------------------------------------------------
__fastcall TFormGraphC::TFormGraphC(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormGraphC::FormCreate(TObject *Sender)
{
  // Fxy
  FormFxy = new TFormFxy(tsFxy);
  FormFxy->Top = 10;
  FormFxy->Left = 10;
  FormFxy->Parent = tsFxy;
  FormFxy->Align = alClient;
  FormFxy->BorderStyle = bsNone;
  FormFxy->Show();

  // HeightField
  FormHeightField = new TFormHeightField(tsHeightField);
  FormHeightField->Top = 10;
  FormHeightField->Left = 10;
  FormHeightField->Parent = tsHeightField;
  FormHeightField->Align = alClient;
  FormHeightField->BorderStyle = bsNone;
  FormHeightField->Show();

  // Points
  FormPoints = new TFormPoints(tsPoints);
  FormPoints->Top = 10;
  FormPoints->Left = 10;
  FormPoints->Parent = tsPoints;
  FormPoints->Align = alClient;
  FormPoints->BorderStyle = bsNone;
  FormPoints->Show();

   // Projection
  FormProjection = new TFormProjection(tsProjection);
  FormProjection->Top = 10;
  FormProjection->Left = 10;
  FormProjection->Parent = tsProjection;
  FormProjection->Align = alClient;
  FormProjection->BorderStyle = bsNone;
  FormProjection->Show();

  // Splines
  FormSplines = new TFormSplines(tsSplines);
  FormSplines->Top = 10;
  FormSplines->Left = 10;
  FormSplines->Parent = tsSplines;
  FormSplines->Align = alClient;
  FormSplines->BorderStyle = bsNone;
  FormSplines->Show();
}
//---------------------------------------------------------------------------

void __fastcall TFormGraphC::FormShow(TObject *Sender)
{
  PageControl->ActivePage = tsFxy;
}
//---------------------------------------------------------------------------

void __fastcall TFormGraphC::tvGraphClick(TObject *Sender)
{
  ///tvGraph->Items[0]->DropHighlighted = false;
   switch (tvGraph->Selected->Index) {
	case 0: {
	  PageControl->ActivePage = tsFxy; break;
	}
	case 1: {
	  PageControl->ActivePage = tsHeightField; break;
	}
	case 2: {
	  PageControl->ActivePage = tsPoints; break;
	}
	case 3: {
	  PageControl->ActivePage = tsProjection; break;
	}
	case 4: {
	  PageControl->ActivePage = tsSplines; break;
	}
	default: {
	  PageControl->ActivePage = tsFxy; break;
	}
   }

}
//---------------------------------------------------------------------------

void __fastcall TFormGraphC::FormDestroy(TObject *Sender)
{
  FormFxy->Free();
  FormHeightField->Free();
  FormPoints->Free();
  FormProjection->Free();
  FormSplines->Free();
}
//---------------------------------------------------------------------------

