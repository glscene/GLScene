//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.jpeg.hpp>

#include "GLScene.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLObjects.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLFileMD2.hpp"
#include "GLBaseClasses.hpp"
#include "GLUtils.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TStatusBar *StatusBar1;
	TPanel *Panel1;
	TSpeedButton *SBPlay;
	TSpeedButton *SBStop;
	TSpeedButton *SBFrameToFrame;
	TLabel *Label1;
	TLabel *LabelFPS;
	TComboBox *CBAnimations;
	TBitBtn *BBLoadWeapon;
	TCheckBox *CBSmooth;
	TGLScene *GLScene1;
	TGLDummyCube *DummyCube1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLDisk *Disk1;
	TGLActor *Actor1;
	TGLActor *Actor2;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall CBAnimationsChange(TObject *Sender);
	void __fastcall SBFrameToFrameClick(TObject *Sender);
	void __fastcall SBPlayClick(TObject *Sender);
	void __fastcall SBStopClick(TObject *Sender);
	void __fastcall BBLoadWeaponClick(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
