//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>
#include "GLBaseClasses.hpp"
#include "GLBitmapFont.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLHUDObjects.hpp"
#include "GLScene.hpp"
#include "GLTeapot.hpp"
#include "GLWin32Viewer.hpp"
#include "GLWindowsFont.hpp"
#include "Unit2.h"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLTeapot *Teapot1;
	TGLHUDText *HUDText1;
	TGLHUDText *HUDText2;
	TGLHUDText *HUDText3;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLWindowsBitmapFont *WindowsBitmapFont1;
	TMainMenu *MainMenu1;
	TMenuItem *MIPickFont;
	TMenuItem *MIViewTexture;
	TMenuItem *MIFPS;
	TFontDialog *FontDialog1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall MIPickFontClick(TObject *Sender);
	void __fastcall MIViewTextureClick(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall GLSceneViewer1Click(TObject *Sender);

private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
