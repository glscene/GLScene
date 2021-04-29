//---------------------------------------------------------------------------

#ifndef fMainCH
#define fMainCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GR32_Image.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TLabel *LabelOverlay;
	TLabel *LabelBlendSettings;
	TLabel *LabelVisible;
	TLabel *LabelMergeHint;
	TLabel *LabelBlendHint;
	TImage32 *DstImg;
	TRadioButton *RadioButtonBlend;
	TRadioButton *RadioButtonMerge;
	TCheckBox *CheckBoxForeground;
	TCheckBox *CheckBoxBackground;
	TCheckBox *CheckBoxTransparent;
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
