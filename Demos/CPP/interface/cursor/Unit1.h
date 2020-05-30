//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ExtDlgs.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLHUDObjects.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLParticles.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "GLUtils.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
	TGLSceneViewer *GLSceneViewer1;
	TStatusBar *StatusBar1;
	TGLScene *GLScene1;
	TGLHUDSprite *HSBitmap;
	TGLParticles *GLParticles1;
	TGLHUDSprite *HSParticle;
	TGLHUDSprite *HSCursor;
	TGLCamera *GLCamera1;
	TMainMenu *MainMenu1;
	TMenuItem *MIFile;
	TMenuItem *MILoadImage;
	TMenuItem *N1;
	TMenuItem *MIExit;
	TMenuItem *O1;
	TMenuItem *MITrail;
	TMenuItem *miFPS;
	TOpenPictureDialog *OpenPictureDialog1;
	TGLCadencer *GLCadencer1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TTimer *Timer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall MILoadImageClick(TObject *Sender);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall GLSceneViewer1AfterRender(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall HSParticleProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLParticles1ActivateParticle(TObject *Sender, TGLBaseSceneObject *particle);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall MITrailClick(TObject *Sender);
	void __fastcall MIExitClick(TObject *Sender);



private:	// User declarations
	bool handleMouseMoves;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
