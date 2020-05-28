//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLScene.hpp"
#include "GLObjects.hpp"
#include "GLCadencer.hpp"
#include "GLWin32Viewer.hpp"
#include "GLBaseClasses.hpp"
#include "GLCrossPlatform.hpp"
#include "GLTexture.hpp"
#include "GLBitmapFont.hpp"
#include "GLWindowsFont.hpp"
#include "GLBehaviours.hpp"
#include "GLConsole.hpp"
#include "GLCoordinates.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLUtils.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TSplitter *Splitter1;
	TGLSceneViewer *Viewer;
	TPanel *Panel1;
	TSplitter *Splitter2;
	TGroupBox *GroupBox1;
	TLabel *Label1;
	TLabel *Label2;
	TCheckBox *CheckBox1;
	TCheckBox *CheckBox2;
	TCheckBox *CheckBox3;
	TButton *Button1;
	TButton *Button2;
	TButton *Button6;
	TButton *Button7;
	TListBox *ListBox1;
	TGLCadencer *GLCadencer1;
	TGLScene *Scene;
	TGLCube *GLCube1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLWindowsBitmapFont *Font1;
	TTimer *Timer1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	TGLConsole *GLConsole1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall FormKeyPress(TObject *Sender, System::WideChar &Key);
	void __fastcall FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall CheckBox1Click(TObject *Sender);
	void __fastcall CheckBox2Click(TObject *Sender);
	void __fastcall CheckBox3Click(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button6Click(TObject *Sender);
	void __fastcall Button7Click(TObject *Sender);



private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
	void __fastcall OnHelloCommand(const TGLConsoleCommand *ConsoleCommand,
		  const TGLCustomConsole *Console, TGLUserInputCommand &Command);
	void __fastcall OnCommand(const TGLConsoleCommand *ConsoleCommand,
		  const TGLCustomConsole *Console, TGLUserInputCommand &Command);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
