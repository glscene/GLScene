//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>
#include "GLBaseClasses.hpp"
#include "GLBehaviours.hpp"
#include "GLBitmapFont.hpp"
#include "GLBlur.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLHUDObjects.hpp"
#include "GLNavigator.hpp"
#include "GLObjects.hpp"
#include "GLParticleFX.hpp"
#include "GLPerlinPFX.hpp"
#include "GLScene.hpp"
#include "GLSpaceText.hpp"
#include "GLWin32Viewer.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TCheckBox *chkMouseLook;
	TCheckBox *chkFloor;
	TCheckBox *chkBlur;
	TGLCadencer *GLCadencer1;
	TGLPerlinPFXManager *PFXElectro;
	TGLPerlinPFXManager *PFXRail;
	TGLPerlinPFXManager *PFXBurning;
	TGLPerlinPFXManager *PFXSmoke;
	TGLBitmapFont *BitmapFont1;
	TGLScene *GLScene1;
	TGLDummyCube *WorldRoot;
	TGLLightSource *GLLightSource1;
	TGLPlane *GLPlane1;
	TGLSpaceText *ICE;
	TGLSpaceText *MAGMA;
	TGLSpaceText *SMOKE;
	TGLSpaceText *FOG;
	TGLSpaceText *FIRE;
	TGLSpaceText *RAIL;
	TGLSpaceText *ELECTRIC;
	TGLSpaceText *WATER;
	TGLParticleFXRenderer *PfxRenderer;
	TGLBlur *GLBlur1;
	TGLCamera *GLCamera1;
	TGLPerlinPFXManager *PFXBlueArea;
	TTimer *Timer1;
	TGLNavigator *GLNavigator1;
	TGLUserInterface *GLUserInterface1;
	TGLPerlinPFXManager *PFXRedArea;
	TGLPerlinPFXManager *PFXFog;
	TGLPerlinPFXManager *PFXWaterfall;
	void __fastcall chkFloorClick(TObject *Sender);
	void __fastcall chkBlurClick(TObject *Sender);
	void __fastcall chkMouseLookClick(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall FormCreate(TObject *Sender);

private:	// User declarations
	void __fastcall HandleKeys(double deltaTime);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
