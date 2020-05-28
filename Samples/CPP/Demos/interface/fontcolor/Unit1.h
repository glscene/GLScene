//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLBitmapFont.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLHUDObjects.hpp"
#include "GLScene.hpp"
#include "GLTeapot.hpp"
#include "GLTimeEventsMgr.hpp"
#include "GLWin32Viewer.hpp"
#include "GLUtils.hpp"

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
	TGLHUDText *HUDText4;
	TGLCamera *GLCamera1;
	TGLBitmapFont *BitmapFont;
	TGLTimeEventsMGR *GLTimeEventsMGR1;
	TGLCadencer *GLCadencer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLTimeEventsMGR1Events0Event(TTimeEvent *event);
	void __fastcall GLTimeEventsMGR1Events1Event(TTimeEvent *event);
	void __fastcall GLTimeEventsMGR1Events2Event(TTimeEvent *event);

private:	// User declarations
	int FadeOutCount;
	int FadeInCount;
    TVector4f OriginalColor;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
