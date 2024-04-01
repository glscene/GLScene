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
TFormGraph* FormGraph;
//---------------------------------------------------------------------------
__fastcall TFormGraph::TFormGraph(TComponent* Owner) : TForm(Owner) {}

//---------------------------------------------------------------------------

void __fastcall TFormGraph::FormCreate(TObject *Sender)
{
  tvGraph->Select(tvGraph->Items->Item[2]);  // goto to item 2
}

//---------------------------------------------------------------------------

void __fastcall TFormGraph::FormShow(TObject *Sender)
{
  tvGraphClick(this);
}

//---------------------------------------------------------------------------

void __fastcall TFormGraph::tvGraphClick(TObject* Sender)
{
	switch (tvGraph->Selected->Index) {
		case 0: { // Fxy
			FormFxy = new TFormFxy(FormGraph);
			FormFxy->Parent = FormGraph;
			FormFxy->Align = alClient;
			FormFxy->BorderStyle = bsNone;
			FormFxy->Show();
			break;
		}
		case 1: { // HeightField
			FormHeightField = new TFormHeightField(FormGraph);
			FormHeightField->Parent = FormGraph;
			FormHeightField->Align = alClient;
			FormHeightField->BorderStyle = bsNone;
			FormHeightField->Show();
			break;
		}
		case 2: { // Points
			FormPoints = new TFormPoints(FormGraph);
			FormPoints->Parent = FormGraph;
			FormPoints->Align = alClient;
			FormPoints->BorderStyle = bsNone;
			FormPoints->Show();
			break;
		}
		case 3: { // Projection
			FormProjection = new TFormProjection(FormGraph);
			FormProjection->Parent = FormGraph;
			FormProjection->Align = alClient;
			FormProjection->BorderStyle = bsNone;
			FormProjection->Show();
			break;
		}
		case 4: { // Splines
			FormSplines = new TFormSplines(FormGraph);
			FormSplines->Parent = FormGraph;
			FormSplines->Align = alClient;
			FormSplines->BorderStyle = bsNone;
			FormSplines->Show();
			break;
		}
		default: {
			break;
		}
	}
}

//---------------------------------------------------------------------------

