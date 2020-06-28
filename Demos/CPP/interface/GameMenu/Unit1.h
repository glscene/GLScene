//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Windows.hpp>
#include <Messages.hpp>
#include <SysUtils.hpp>
#include <Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <System.Classes.hpp>
#include <Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Imaging.jpeg.hpp>

#include "GLScene.hpp"
#include "GLObjects.hpp"
#include "GLSceneViewer.hpp"
#include "GLGeomObjects.hpp"
#include "GLBitmapFont.hpp"
#include "GLWindowsFont.hpp"
#include "GLGameMenu.hpp"
#include "GLCadencer.hpp"
#include "GLTexture.hpp"
#include "GLKeyboard.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLCoordinates.hpp"
#include "GLBaseClasses.hpp"
#include "GLUtils.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *MainPanel;
	TLabel *Label1;
	TCheckBox *ShowTitleCheckbox;
	TGLScene *GLScene1;
	TGLDummyCube *GLDummyCube1;
	TGLCube *GLCube1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLWindowsBitmapFont *GLWindowsBitmapFont1;
	TGLCadencer *GLCadencer1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall ShowTitleCheckboxClick(TObject *Sender);
	void __fastcall MainPanelResize(TObject *Sender);

private:	// User declarations
	int mx, my;
	TGLGameMenu *GameMenu;

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
