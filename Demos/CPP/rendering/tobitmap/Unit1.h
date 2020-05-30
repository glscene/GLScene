//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "Jpeg.hpp"
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLHUDObjects.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSpaceText.hpp"
#include "GLWin32Viewer.hpp"

#include "Unit2.h"


//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TButton *BUSnapShot;
	TButton *BURenderToBitmap;
	TButton *BUBitmapx2;
	TButton *BUBitmap600;
	TButton *BUBitmap300;
	TButton *BUViewerSnapShot;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLHUDSprite *HUDSprite1;
	TGLPlane *Plane1;
	TGLSpaceText *SpaceText1;
	TGLSphere *Sphere1;
	TGLDummyCube *DummyCube1;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall BUViewerSnapShotClick(TObject *Sender);
	void __fastcall BUSnapShotClick(TObject *Sender);
	void __fastcall BURenderToBitmapClick(TObject *Sender);
	void __fastcall BUBitmapx2Click(TObject *Sender);
	void __fastcall BUBitmap300Click(TObject *Sender);
	void __fastcall BUBitmap600Click(TObject *Sender);
	void __fastcall Sphere1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall FormResize(TObject *Sender);

private:	// User declarations
	void ViewBitmap(TBitmap *aBitmap, String caption);
	void RenderToBitmap(Single scale);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
