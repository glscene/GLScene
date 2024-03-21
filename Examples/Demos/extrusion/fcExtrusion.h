//---------------------------------------------------------------------------

#ifndef fcExtrusionH
#define fcExtrusionH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>

#include "fBendingC.h"
#include "fCutoutStarC.h"
#include "fNutsnBoltsC.h"
#include "fPawnC.h"
#include "fTentaclesC.h"

//---------------------------------------------------------------------------
class TFormExtrusion : public TForm
{
__published:	// IDE-managed Components
	TPanel *PanelLeft;
	TTreeView *tvExtrusion;
	TPageControl *PageControl;
	TTabSheet *tsBending;
	TTabSheet *tsCutoutStar;
	TTabSheet *tsNutsnBolts;
	TTabSheet *tsPawn;
	TTabSheet *tsTentacles;
	TMainMenu *MainMenu;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall tvExtrusionClick(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormExtrusion(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormExtrusion *FormExtrusion;
//---------------------------------------------------------------------------
#endif
