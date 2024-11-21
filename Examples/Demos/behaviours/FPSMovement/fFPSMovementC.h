//---------------------------------------------------------------------------

#ifndef fFPSMovementCH
#define fFPSMovementCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>

#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.FileJPEG.hpp"
#include "GLS.FPSMovement.hpp"
#include "GLS.Material.hpp"
#include "GLS.Navigator.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.Screen.hpp"
#include "GLS.SimpleNavigation.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "Stage.Keyboard.hpp"
#include "GLS.GeomObjects.hpp"
#include "Stage.VectorGeometry.hpp"
#include "GLS.Octree.hpp"
#include "GLS.VectorLists.hpp"
#include "GLS.Collision.hpp"
#include "GLS.Texture.hpp"
#include <System.Classes.hpp>

//---------------------------------------------------------------------------
class TFormFPSMovement:public TForm
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
    __fastcall TFormFPSMovement(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormFPSMovement *FormFPSMovement;
//---------------------------------------------------------------------------
#endif

