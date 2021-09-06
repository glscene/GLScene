//---------------------------------------------------------------------------

#ifndef fLineStipplingCH
#define fLineStipplingCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GR32_Image.hpp"
//---------------------------------------------------------------------------
class TFormLineStipplingC : public TForm
{
__published:	// IDE-managed Components
	TImage32 *Image;
	TScrollBar *ScrollBar;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ScrollBarChange(TObject *Sender);
private:	// User declarations
public:		// User declarations
    void __fastcall Spiral (int X, int Y);
	__fastcall TFormLineStipplingC(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormLineStipplingC *FormLineStipplingC;
//---------------------------------------------------------------------------
#endif
