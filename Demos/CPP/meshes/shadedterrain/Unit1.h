//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>


#include "OpenGL1x.hpp"
#include "GLTexCombineShader.hpp"
#include "GLBumpmapHDS.hpp"
#include "GLLensFlare.hpp"
#include "GLVectorGeometry.hpp"
#include "GLWin32Viewer.hpp"
#include "GLSkydome.hpp"
#include "GLTexture.hpp"
#include "GLCadencer.hpp"
#include "GLHeightData.hpp"
#include "GLObjects.hpp"
#include "GLTerrainRenderer.hpp"
#include "GLScene.hpp"
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"

#include "GLKeyBoard.hpp"
#include <jpeg.hpp>             // Pascal unit

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
  TGLSkyDome *SkyDome1;
  TGLSprite *SPSun;
  TGLLensFlare *GLLensFlare;
  TGLDummyCube *GLDummyCube1;
  TGLTexCombineShader *GLTexCombineShader1;
  TGLBumpmapHDS *GLBumpmapHDS1;
  TPanel *Panel1;
  TLabel *Label1;
  TTrackBar *TBSubSampling;
  TLabel *LASubFactor;
  TLabel *Label2;
  TTrackBar *TBIntensity;
  TLabel *LABumpIntensity;
  void __fastcall FormShow(TObject * Sender);
  void __fastcall GLBumpmapHDS1NewTilePrepared(TGLBumpmapHDS * Sender,
                                               TGLHeightData * heightData,
                                               TGLLibMaterial *
                                               normalMapMaterial);
  void __fastcall GLCadencer1Progress(TObject * Sender,
                                      const double deltaTime,
                                      const double newTime);
  void __fastcall GLSceneViewer1MouseDown(TObject * Sender,
                                          TMouseButton Button,
                                          TShiftState Shift, int X, int Y);
  void __fastcall GLSceneViewer1MouseMove(TObject * Sender,
                                          TShiftState Shift, int X, int Y);
  void __fastcall Timer1Timer(TObject * Sender);
  void __fastcall FormKeyPress(TObject * Sender, char &Key);
  void __fastcall GLSceneViewer1BeforeRender(TObject * Sender);
  void __fastcall TBSubSamplingChange(TObject * Sender);
  void __fastcall TBIntensityChange(TObject * Sender);
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

