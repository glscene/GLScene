//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.BitmapFont.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.HUDObjects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.TimeEventsMgr.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Utils.hpp"

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
