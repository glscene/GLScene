//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>

#include <GLObjects.hpp>        // Pascal unit
#include <GLTexture.hpp>        // Pascal unit
#include <GLGeomObjects.hpp>    // Pascal unit
#include <GLCelShader.hpp>      // Pascal unit
#include <GLAsyncTimer.hpp>       // Pascal unit
#include <GLVectorFileObjects.hpp>      // Pascal unit
#include <GLWin32Viewer.hpp>    // Pascal unit
#include <GLCadencer.hpp>       // Pascal unit
#include <GLScene.hpp>
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"          // Pascal unit
#include "GLKeyboard.hpp"
#include "GLFileMD2.hpp"
#include "GLUtils.hpp"



//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLScene * GLScene1;
  TGLSceneViewer *GLSceneViewer1;
  TGLMaterialLibrary *GLMaterialLibrary1;
  TGLCadencer *GLCadencer1;
  TGLCamera *GLCamera1;
  TGLDummyCube *GLDummyCube1;
  TGLLightSource *GLLightSource1;
  TGLActor *GLActor1;
  TGLAsyncTimer *AsyncTimer1;
  TGLCelShader *GLTexturedCelShader;
  TGLCelShader *GLColoredCelShader;
  TGLTorus *GLTorus1;
  void __fastcall GLSceneViewer1MouseDown(TObject * Sender,
                                          TMouseButton Button,
                                          TShiftState Shift, int X, int Y);
  void __fastcall GLSceneViewer1MouseMove(TObject * Sender, TShiftState Shift,
                                          int X, int Y);
  void __fastcall AsyncTimer1Timer(TObject * Sender);
  void __fastcall GLCadencer1Progress(TObject * Sender,
                                      const double deltaTime,
                                      const double newTime);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);

  int mx, my, lx, ly;
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
 
