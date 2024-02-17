//---------------------------------------------------------------------------

#include <vcl.h>

#pragma hdrstop

#include "fcGraph.h"

#pragma link "fFxyC"
#pragma link "fHeightFieldC"
#pragma link "fPointsC"
#pragma link "fProjectionC"
#pragma link "fSplinesC"


//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TFormGraph *FormGraph;
//---------------------------------------------------------------------------
__fastcall TFormGraph::TFormGraph(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormGraph::FormCreate(TObject *Sender)
{
  // Fxy
  FormFxy = new TFormFxy(tsFxy);
  FormFxy->Parent = tsFxy;
  FormFxy->Align = alClient;
  FormFxy->BorderStyle = bsNone;
  FormFxy->Show();

  // HeightField
  FormHeightField = new TFormHeightField(tsHeightField);
  FormHeightField->Parent = tsHeightField;
  FormHeightField->Align = alClient;
  FormHeightField->BorderStyle = bsNone;
  FormHeightField->Show();

  // Points
  FormPoints = new TFormPoints(tsPoints);
  FormPoints->Parent = tsPoints;
  FormPoints->Align = alClient;
  FormPoints->BorderStyle = bsNone;
  FormPoints->Show();

   // Projection
  FormProjection = new TFormProjection(tsProjection);
  FormProjection->Parent = tsProjection;
  FormProjection->Align = alClient;
  FormProjection->BorderStyle = bsNone;
  FormProjection->Show();

  // Splines
  FormSplines = new TFormSplines(tsSplines);
  FormSplines->Parent = tsSplines;
  FormSplines->Align = alClient;
  FormSplines->BorderStyle = bsNone;
  FormSplines->Show();
}
//---------------------------------------------------------------------------

void __fastcall TFormGraph::FormShow(TObject *Sender)
{
  PageControl->ActivePage = tsFxy;
}
//---------------------------------------------------------------------------

void __fastcall TFormGraph::tvGraphClick(TObject *Sender)
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


