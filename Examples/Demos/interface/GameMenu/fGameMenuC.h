//---------------------------------------------------------------------------

#ifndef fGameMenuCH
#define fGameMenuCH
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

#include "GLS.Scene.hpp"
#include "GLS.Objects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.BitmapFont.hpp"
#include "GLS.WindowsFont.hpp"
#include "GLS.GameMenu.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Texture.hpp"
#include "Stage.Keyboard.hpp"

#include "GLS.Material.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.BaseClasses.hpp"
#include "Stage.Utils.hpp"

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
