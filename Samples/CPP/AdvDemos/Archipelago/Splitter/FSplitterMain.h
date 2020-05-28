//---------------------------------------------------------------------------

#ifndef FSplitterMainH
#define FSplitterMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <Vcl.Imaging.jpeg.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label1;
        TLabel *LAAction;
        TEdit *EDFile;
        TButton *Button1;
        TEdit *EDTileSize;
        TEdit *EDMask;
        TProgressBar *ProgressBar;
        TRadioButton *RBFull;
        TRadioButton *RBHalf;
        TRadioButton *RBLow;
        void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
