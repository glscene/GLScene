// ---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
// ---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.BitmapFont.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Gui.hpp"
#include "GLS.Material.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Windows.hpp"
#include "GLS.WindowsFont.hpp"
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>

// ---------------------------------------------------------------------------
class TForm1 : public TForm {
__published: // IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLBaseControl *GuiRoot;
	TGLForm *GLForm1;
	TGLButton *PenButton;
	TGLButton *BrushButton;
	TGLPanel *GLPanel1;
	TGLCustomControl *GLCanvas;
	TGLButton *WhiteButton;
	TGLButton *BlackButton;
	TGLButton *RedButton;
	TGLButton *GreenButton;
	TGLButton *BlueButton;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLWindowsBitmapFont *WindowsBitmapFont1;
	TMainMenu *MainMenu1;
	TMenuItem *File1;
	TMenuItem *miOpen1;
	TMenuItem *miSave1;
	TMenuItem *miFont1;
	TMenuItem *miWindowsFont1;
	TMenuItem *miFPS;
	TFontDialog *FontDialog1;
	TGLGuiLayout *GLGuiLayout1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TOpenDialog *OpenDialog1;
	TSaveDialog *SaveDialog1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime,
		const double newTime);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall miWindowsFont1Click(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender,
		TMouseButton Button, TShiftState Shift, int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		int X, int Y);
	void __fastcall GLSceneViewer1MouseUp(TObject *Sender, TMouseButton Button,
		TShiftState Shift, int X, int Y);
	void __fastcall FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall FormKeyPress(TObject *Sender, System::WideChar &Key);
	void __fastcall FormKeyUp(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall GLCanvasMouseDown(TObject *Sender, TMouseButton Button,
		TShiftState Shift, int X, int Y);
	void __fastcall GLCanvasMouseMove(TObject *Sender, TShiftState Shift,
		int X, int Y);
	void __fastcall GLCanvasMouseUp(TObject *Sender, TMouseButton Button,
		TShiftState Shift, int X, int Y);
	void __fastcall GLCanvasRender(TGLCustomControl *Sender, TBitmap *Bitmap);
	void __fastcall PenButtonButtonClick(TObject *Sender);
	void __fastcall BrushButtonButtonClick(TObject *Sender);
	void __fastcall WhiteButtonButtonClick(TObject *Sender);
	void __fastcall BlackButtonButtonClick(TObject *Sender);
	void __fastcall RedButtonButtonClick(TObject *Sender);
	void __fastcall GreenButtonButtonClick(TObject *Sender);
	void __fastcall BlueButtonButtonClick(TObject *Sender);
	void __fastcall GLCanvasAcceptMouseQuery(TGLBaseControl *Sender,
		TShiftState Shift, TGLMouseAction Action, TMouseButton Button, int X,
		int Y, bool &Accept);
	void __fastcall GLForm1Moving(TGLForm *Sender, float &Left, float &Top);
	void __fastcall miOpen1Click(TObject *Sender);
	void __fastcall miSave1Click(TObject *Sender);
private: // User declarations
	int StartX;
	int StartY;
	int CurrentX;
	int CurrentY;
public: // User declarations
	__fastcall TForm1(TComponent* Owner);
};

// ---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
// ---------------------------------------------------------------------------
#endif
