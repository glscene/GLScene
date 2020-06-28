//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Forms.hpp>

#include "OpenGLx.hpp"
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLFPSMovement.hpp"
#include "GLMaterial.hpp"
#include "GLNavigator.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLSceneViewer.hpp"
#include "GLKeyboard.hpp"
#include "GLGeomObjects.hpp"
#include "GLVectorGeometry.hpp"
#include "GLOctree.hpp"
#include "GLVectorLists.hpp"
#include "GLNavigator.hpp"
#include "GLCollision.hpp"
#include "GLTexture.hpp"

//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLScene * GLScene1;
  TGLSceneViewer *GLSceneViewer1;
  TGLCadencer *GLCadencer1;
  TGLCamera *FirstPersonCamera;
  TGLFreeForm *Map1;
  TGLMaterialLibrary *GLMaterialLibrary1;
  TGLLightSource *GLLight;
  TGLDummyCube *World;
  TGLCamera *ThirdPersonCamera;
  TGLSphere *PlayerSphere;
  TGLLightSource *GLLightSource1;
  TGLSphere *PlayerCentre;
  TGLDummyCube *Player;
  TGLFreeForm *Map2;
  TGLDummyCube *Bot;
  TGLSphere *BotCenter;
  TGLSphere *BotSphere;
  TGLNavigator *Navigator1;
  TGLFPSMovementManager *MovManager;
	TGLSimpleNavigation *GLSimpleNavigation1;
  void __fastcall FormKeyDown(TObject * Sender, WORD & Key, TShiftState Shift);
  void __fastcall GLCadencer1Progress(TObject * Sender, const double deltaTime,
                                      const double newTime);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

