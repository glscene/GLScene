//---------------------------------------------------------------------------

#ifndef fTerrainCH
#define fTerrainCH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <StdCtrls.hpp>
#include <ExtCtrls.hpp>
#include <GIFImg.hpp>

#include "GLS.LensFlare.hpp"
#include "GLS.VectorGeometry.hpp"
#include "Sounds.BASS.hpp"
#include "GLS.Sound.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.SkyDome.hpp"
#include "GLS.BitmapFont.hpp"
#include "GLS.HUDObjects.hpp"
#include "GLS.Texture.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.HeightData.hpp"
#include "GLS.Objects.hpp"
#include "GLS.TerrainRenderer.hpp"
#include "GLS.Scene.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Material.hpp"
#include "GLS.FileMP3.hpp"
#include "Jpeg.hpp"
#include "GLS.Utils.hpp"
//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLSceneViewer * GLSceneViewer1;
  TGLBitmapHDS *GLBitmapHDS1;
  TGLScene *GLScene1;
  TGLCamera *GLCamera1;
  TGLDummyCube *DummyCube1;
  TGLTerrainRenderer *TerrainRenderer1;
  TTimer *Timer1;
  TGLCadencer *GLCadencer1;
  TGLMaterialLibrary *GLMaterialLibrary1;
  TGLBitmapFont *BitmapFont1;
  TGLHUDText *HUDText1;
  TGLSkyDome *SkyDome1;
  TGLSprite *SPMoon;
  TGLSprite *SPSun;
  TGLDummyCube *DCSound;
  TGLSMBASS *GLSMBASS1;
  TTimer *TISound;
  TGLSoundLibrary *GLSoundLibrary;
  TGLLensFlare *GLLensFlare;
  TGLDummyCube *GLDummyCube1;
  TGLRenderPoint *InitialRenderPoint;
  void __fastcall GLCadencer1Progress(TObject * Sender, const double deltaTime,
									  const double newTime);
  void __fastcall GLSceneViewer1MouseDown(TObject * Sender, TMouseButton Button,
										  TShiftState Shift, int X, int Y);
  void __fastcall GLSceneViewer1MouseMove(TObject * Sender, TShiftState Shift,
										  int X, int Y);
  void __fastcall Timer1Timer(TObject * Sender);
  void __fastcall FormKeyPress(TObject * Sender, char &Key);
  void __fastcall TISoundTimer(TObject * Sender);
private:                       // User declarations
public:                        // User declarations
	__fastcall TForm1(TComponent * Owner);

  int mx, my;
  bool fullScreen;
  float FCamHeight;
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

