//---------------------------------------------------------------------------

#ifndef fcBenchH
#define fcBenchH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "fCanvasC.h"
#include "fMegaCubeC.h"
#include "fMegaglassC.h"
#include "fSmokingC.h"
#include "fVolcanoC.h"
#include "fWhirlC.h"


//---------------------------------------------------------------------------
class TFirmBench : public TForm
{
__published:	// IDE-managed Components
	TPanel *PanelLeft;
	TTreeView *tvBench;
	TPageControl *PageControl;
	TTabSheet *tsCanvas;
	TTabSheet *tsMegacube;
	TTabSheet *tsMegaglasscube;
	TTabSheet *tsSmoking;
	TTabSheet *tsVolcano;
	TTabSheet *tsWhirlwind;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall tvBenchClick(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFirmBench(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFirmBench *FirmBench;
//---------------------------------------------------------------------------
#endif
