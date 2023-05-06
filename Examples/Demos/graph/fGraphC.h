//---------------------------------------------------------------------------

#ifndef fGraphCH
#define fGraphCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>
//---------------------------------------------------------------------------
class TFormGraphC : public TForm
{
__published:	// IDE-managed Components
	TPanel *PanelLeft;
	TTreeView *tvGraph;
	TPageControl *PageControl;
	TTabSheet *tsFxy;
	TTabSheet *tsHeightField;
	TTabSheet *tsPoints;
	TTabSheet *tsProjection;
	TTabSheet *tsSplines;
	TMainMenu *MainMenu;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall tvGraphClick(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormGraphC(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormGraphC *FormGraphC;
//---------------------------------------------------------------------------
#endif
