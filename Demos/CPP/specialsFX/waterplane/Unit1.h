//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Forms.hpp>
#include <Jpeg.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGraph.hpp"
#include "GLMaterial.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLUserShader.hpp"
#include "GLWaterPlane.hpp"
#include "GLWin32Viewer.hpp"
#include "GLObjects.hpp"
#include "GLUtils.hpp"

#pragma hdrstop

#include "OpenGL1x.hpp"


//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLScene * GLScene1;
  TGLSceneViewer *GLSceneViewer1;
  TGLCamera *GLCamera1;
  TGLDummyCube *DCTarget;
  TGLCadencer *GLCadencer1;
  TGLWaterPlane *GLWaterPlane1;
  TGLMaterialLibrary *GLMaterialLibrary1;
  TGLUserShader *GLUserShader1;
  TGLSphere *GLSphere1;
  TGLDirectOpenGL *GLDirectOpenGL1;
  TGLHeightField *GLHeightField1;
  TGLLightSource *GLLightSource1;
	TGLSimpleNavigation *GLSimpleNavigation1;
  void __fastcall GLSceneViewer1MouseDown(TObject * Sender,
                                          TMouseButton Button,
                                          TShiftState Shift, int X, int Y);
  void __fastcall GLSceneViewer1MouseMove(TObject * Sender,
                                          TShiftState Shift, int X, int Y);
  void __fastcall GLUserShader1DoApply(TObject * Sender,
                                       TGLRenderContextInfo & rci);
  void __fastcall GLUserShader1DoUnApply(TObject * Sender, int Pass,
                                         TGLRenderContextInfo & rci,
                                         bool & Continue);
  void __fastcall GLSceneViewer1BeforeRender(TObject * Sender);
  void __fastcall GLDirectOpenGL1Render(TObject * Sender,
                                        TGLRenderContextInfo & rci);
  void __fastcall GLCadencer1Progress(TObject * Sender,
                                      const double deltaTime,
                                      const double newTime);
  void __fastcall GLHeightField1GetHeight(const float x, const float y,
                                          float &z, TVector4f & color,
                                          TTexPoint & texPoint);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);

  int mx, my;
  bool reflectionToggle;
  void ClickWater(int x, int y);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
 
