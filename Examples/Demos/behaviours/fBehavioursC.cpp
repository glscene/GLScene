//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fBehavioursC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TBehaviours *Behaviours;
//---------------------------------------------------------------------------
__fastcall TBehaviours::TBehaviours(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
