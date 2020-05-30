//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLBaseClasses.hpp"
#include "GLBitmapFont.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLHeightData.hpp"
#include "GLHUDObjects.hpp"
#include "GLLensFlare.hpp"
#include "GLMaterial.hpp"
#include "GLNavigator.hpp"
#include "GLObjects.hpp"
#include "GLODECustomColliders.hpp"
#include "GLODEManager.hpp"
#include "GLScene.hpp"
#include "GLSkydome.hpp"
#include "GLTerrainRenderer.hpp"
#include "GLWin32Viewer.hpp"
#include "GLUtils.hpp"
#include "JPeg.hpp"
#include "GLKeyBoard.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLBitmapHDS *GLBitmapHDS1;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLSkyDome *SkyDome1;
	TGLSprite *SPMoon;
	TGLSprite *SPSun;
	TGLTerrainRenderer *TerrainRenderer1;
	TGLDummyCube *ODEObjects;
	TGLRenderPoint *ODERenderPoint;
	TGLHUDText *HUDText1;
	TGLLensFlare *GLLensFlare;
	TGLDummyCube *GLDummyCube1;
	TGLCamera *GLCamera1;
	TGLDummyCube *ODEDrop;
	TTimer *Timer1;
	TGLCadencer *GLCadencer1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLBitmapFont *BitmapFont1;
	TGLODEManager *GLODEManager1;
	TGLNavigator *GLNavigator1;
	TGLUserInterface *GLUserInterface1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall FormKeyPress(TObject *Sender, System::WideChar &Key);
	void __fastcall GLSceneViewer1BeforeRender(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
