//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H

#include <vcl.h>
#include <tchar.h>

//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.Jpeg.hpp>

#include "GLScene.hpp"
#include "GLCadencer.hpp"
#include "GLWin32Viewer.hpp"
#include "GLObjects.hpp"
#include "GLGeomObjects.hpp"
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLNavigator.hpp"
#include "GLSkydome.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLKeyboard.hpp"
#include "GLVectorGeometry.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label1;
	TCheckBox *CBMouseLook;
	TGLScene *GLScene1;
	TGLSkyDome *SkyDome1;
	TGLDisk *Disk1;
	TGLLightSource *GLLightSource2;
	TGLDummyCube *DummyCube1;
	TGLFreeForm *FreeForm1;
	TGLDummyCube *DummyCube2;
	TGLCamera *GLCamera2;
	TGLActor *Actor1;
	TGLActor *Actor2;
	TGLDummyCube *DummyCube3;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLNavigator *GLNavigator1;
	TGLUserInterface *GLUserInterface1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall CBMouseLookClick(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall Timer1Timer(TObject *Sender);

private:	// User declarations
	void __fastcall AddMushrooms();
	void __fastcall HandleKeys(const double deltaTime);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
