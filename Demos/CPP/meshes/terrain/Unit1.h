//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <StdCtrls.hpp>
#include <ExtCtrls.hpp>
#include <GIFImg.hpp>

#include <GLLensFlare.hpp>
#include <GLVectorGeometry.hpp>
#include <GLSMBASS.hpp>
#include <GLSound.hpp>
#include <GLWin32Viewer.hpp>
#include <GLSkydome.hpp>
#include <GLBitmapFont.hpp>
#include <GLHUDObjects.hpp>
#include <GLTexture.hpp>
#include <GLCadencer.hpp>
#include <GLHeightData.hpp>
#include <GLObjects.hpp>
#include <GLTerrainRenderer.hpp>
#include <GLScene.hpp>
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLFileMP3.hpp"
#include "Jpeg.hpp"
#include "GLUtils.hpp"
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

