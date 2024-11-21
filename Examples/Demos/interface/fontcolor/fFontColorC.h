//---------------------------------------------------------------------------

#ifndef fFontColorCH
#define fFontColorCH
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
#include "Stage.Utils.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLTeapot *Teapot1;
	TGLHUDText *HUDTextFadingOut;
	TGLHUDText *HUDTextTheEnd;
	TGLHUDText *HUDTextRedRed;
	TGLHUDText *HUDTextTransparent;
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
