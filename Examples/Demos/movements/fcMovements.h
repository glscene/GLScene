//---------------------------------------------------------------------------

#ifndef fcMovementsH
#define fcMovementsH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "fColumnC.h"
#include "fEventsC.h"
#include "fHierarchC.h"
#include "fManualC.h"
#include "fObjmoveC.h"
#include "fPointtoC.h"
#include "fPongC.h"
#include "fSmoothNaviC.h"
#include "fTweeningC.h"


//---------------------------------------------------------------------------
class TFormMovements : public TForm
{
__published:	// IDE-managed Components
	TPanel *PanelLeft;
	TTreeView *tvMovements;
	TPageControl *PageControl;
	TTabSheet *tsMovements;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall tvMovementsClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormMovements(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMovements *FormMovements;
//---------------------------------------------------------------------------
#endif
