#ifndef fSimpleShaderCH
#define fSimpleShaderCH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "GLS.OpenGLAdapter.hpp"
#include "GLS.BitmapFont.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.HUDObjects.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.Texture.hpp"
#include "GLSL.UserShader.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.WindowsFont.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Material.hpp"
#include "GLS.Context.hpp"
#include "GLS.SimpleNavigation.hpp"

//---------------------------------------------------------------------------

#define MAXSHADERS 10

class TForm1 : public TForm
{
  __published: // IDE-managed Components
    TGLSceneViewer* GLSceneViewer1;
    TGLScene* GLScene1;
    TGLDirectOpenGL* GLDOInitialize;
    TGLDummyCube* Scene;
    TGLCube* GLCube1;
    TGLSphere* GLSphere1;
    TGLCone* GLCone1;
    TGLFreeForm* GLFreeForm1;
    TGLDummyCube* DummyLight;
    TGLLightSource* Light;
    TGLHUDText* GLHUDText1;
    TGLCamera* Cam;
    TGLCadencer* GLCadencer1;
    TGLUserShader* GLUserShader1;
    TTimer* Timer1;
    TGLWindowsBitmapFont* GLWindowsBitmapFont1;
    TGLMaterialLibrary* MatLib;
	TGLSimpleNavigation *GLSimpleNavigation1;
    void __fastcall GLCadencer1Progress(
        TObject* Sender, const double deltaTime, const double newTime);
    void __fastcall GLUserShader1DoApply(
        TObject* Sender, TGLRenderContextInfo &rci);
    void __fastcall GLUserShader1DoUnApply(
        TObject* Sender, int Pass, TGLRenderContextInfo &rci, bool &Continue);
    void __fastcall Timer1Timer(TObject* Sender);
    void __fastcall GLSceneViewer1MouseDown(
        TObject* Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
    void __fastcall GLSceneViewer1MouseMove(
        TObject* Sender, TShiftState Shift, int X, int Y);
    void __fastcall FormCreate(TObject* Sender);
    void __fastcall FormClose(TObject* Sender, TCloseAction &Action);
    void __fastcall GLDOInitializeRender(
        TObject* Sender, TGLRenderContextInfo &rci);
    void __fastcall FormMouseWheel(TObject* Sender, TShiftState Shift,
        int WheelDelta, TPoint &MousePos, bool &Handled);
  private: // User declarations
    bool ShaderActived;
    int mx, my;
  public: // User declarations
    TFileName Path;
    TGLProgramHandle* GLSLProg[MAXSHADERS - 1];
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1* Form1;
//---------------------------------------------------------------------------
#endif

