//---------------------------------------------------------------------------

#ifndef fOdeTerrainCH
#define fOdeTerrainCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLS.BaseClasses.hpp"
#include "GLS.BitmapFont.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.HeightData.hpp"
#include "GLS.HUDObjects.hpp"
#include "GLS.LensFlare.hpp"
#include "GLS.Material.hpp"
#include "GLS.Navigator.hpp"
#include "GLS.Objects.hpp"
#include "GLS.ODEManager.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SkyDome.hpp"
#include "GLS.TerrainRenderer.hpp"
#include "GLS.SceneViewer.hpp"
#include "Stage.Utils.hpp"
#include "JPeg.hpp"
#include "Stage.Keyboard.hpp"
#include "GLS.ODEManager.hpp"

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
