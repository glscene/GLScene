//---------------------------------------------------------------------------

#ifndef fcMaterialsH
#define fcMaterialsH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
//---------------------------------------------------------------------------
class TFormMaterials : public TForm
{
__published:	// IDE-managed Components
	TTreeView *TreeViewMaterials;
private:	// User declarations
public:		// User declarations
	__fastcall TFormMaterials(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMaterials *FormMaterials;
//---------------------------------------------------------------------------
#endif
