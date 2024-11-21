//---------------------------------------------------------------------------

#ifndef fCelShadingCH
#define fCelShadingCH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>

#include <GLS.Objects.hpp>        // Pascal unit
#include <GLS.Texture.hpp>        // Pascal unit
#include <GLS.GeomObjects.hpp>    // Pascal unit
#include <GLSL.ShapeShaders.hpp>      // Pascal unit
#include <GLS.AsyncTimer.hpp>       // Pascal unit
#include <GLS.VectorFileObjects.hpp>      // Pascal unit
#include <GLS.SceneViewer.hpp>    // Pascal unit
#include <GLS.Cadencer.hpp>       // Pascal unit
#include <GLS.Scene.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Material.hpp"          // Pascal unit
#include "Stage.Keyboard.hpp"
#include "GLS.FileMD2.hpp"
#include "Stage.Utils.hpp"



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
 
