//---------------------------------------------------------------------------

#ifndef mainH
#define mainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>                   
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include "GLWindowsFont.hpp"
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
        TGroupBox *GroupBox1;
        TPanel *Panel1;
        TButton *Button1;
        TGroupBox *GroupBox2;
        TImage *Image1;
        TGroupBox *GroupBox3;
        TButton *Button2;
        TFontDialog *FontDialog1;
        TSaveDialog *SaveDialog1;
        void __fastcall Button1Click(TObject *Sender);
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall Button2Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
	   TGLWindowsBitmapFont *FFont;
        __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
