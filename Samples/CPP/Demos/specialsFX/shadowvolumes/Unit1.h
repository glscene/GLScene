//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLShadowVolume.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"

#pragma hdrstop

//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLSceneViewer * GLSceneViewer;
  TGLScene *GLScene1;
  TGLCadencer *GLCadencer1;
  TGLCamera *GLCamera;
  TGLDummyCube *DCCamera;
  TGLLightSource *GLLightSource1;
  TGLShadowVolume *GLShadowVolume;
  TGLSphere *GLSphere1;
  TGLDummyCube *DCLight1Turn;
  TGLDummyCube *DCLight1Pitch;
  TGLPlane *GLPlane1;
  TGLPlane *GLPlane2;
  TGLPlane *GLPlane3;
  TTimer *Timer1;
  TPanel *Panel1;
  TCheckBox *CBShowVolumes;
	TLabel *LabelFPS;
  TRadioButton *RBZFail;
  TRadioButton *RBZPass;
  TRadioButton *RBNoShadows;
  TRadioButton *RBDarkening;
  TGLDummyCube *DCLight2;
  TGLLightSource *GLLightSource2;
  TGLSphere *GLSphere2;
  TCheckBox *CBMainLight;
  TCheckBox *CBBlueLight;
  TGLDummyCube *DCLight3;
  TGLLightSource *GLLightSource3;
  TGLSphere *GLSphere3;
  TCheckBox *CBRedLight;
  TGLDummyCube *DCSpheres;
  TGLFreeForm *GLFreeForm;
  TGLCube *GLCube1;
  TGLMaterialLibrary *GLMaterialLibrary1;
  TGLCylinder *GLCylinder1;
  TGLSphere *GLSphere4;
  TGLSphere *GLSphere_Shadow;
  TLabel *Label2;
  TScrollBar *ScrollBar_ShadowResolution;
  TButton *Button_GenerateSilhouette;
  TGLLines *GLLines1;
  void __fastcall GLCadencer1Progress(TObject * Sender, const double deltaTime,
                                      const double newTime);
  void __fastcall CBShowVolumesClick(TObject * Sender);
  void __fastcall RBZFailClick(TObject * Sender);
  void __fastcall GLSceneViewerMouseDown(TObject * Sender, TMouseButton Button,
                                         TShiftState Shift, int X, int Y);
  void __fastcall GLSceneViewerMouseMove(TObject * Sender, TShiftState Shift,
                                         int X, int Y);
  void __fastcall Timer1Timer(TObject * Sender);
  void __fastcall FormResize(TObject * Sender);
  void __fastcall CBMainLightClick(TObject * Sender);
  void __fastcall CBBlueLightClick(TObject * Sender);
  void __fastcall CBRedLightClick(TObject * Sender);
  void __fastcall ScrollBar_ShadowResolutionChange(TObject * Sender);
  void __fastcall Button_GenerateSilhouetteClick(TObject * Sender);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);

  int mx, my;
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
 
