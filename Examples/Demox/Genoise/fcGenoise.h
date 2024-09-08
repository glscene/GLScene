//---------------------------------------------------------------------------

#ifndef fcGenoiseH
#define fcGenoiseH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Colors.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
//---------------------------------------------------------------------------
class TFormGenoise : public TForm
{
__published:	// IDE-managed Components
	TImage *Image1;
	TLayout *Layout1;
	TButton *Button1;
	TLabel *Label1;
	TComboBox *ComboBox1;
	TLabel *Label2;
	TComboBox *ComboBox2;
	TButton *Button2;
	TComboColorBox *ComboColorBox1;
	TCheckBox *CheckBox1;
	TSaveDialog *SaveDialog1;
private:	// User declarations
public:		// User declarations
	__fastcall TFormGenoise(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormGenoise *FormGenoise;
//---------------------------------------------------------------------------
#endif
