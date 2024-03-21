//---------------------------------------------------------------------------

#ifndef fcMeshesH
#define fcMeshesH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "fActorC.h"
#include "fActorProxyC.h"
#include "fActorTwocamC.h"

//---------------------------------------------------------------------------
class TFormMeshes : public TForm
{
__published:	// IDE-managed Components
	TPanel *PanelLeft;
	TTreeView *tvMeshes;
	TPanel *pnMeshes;
	void __fastcall tvMeshesClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormMeshes(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMeshes *FormMeshes;
//---------------------------------------------------------------------------
#endif
