//---------------------------------------------------------------------------

#ifndef fOceanCH
#define fOceanCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Graph.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SimpleNavigation.hpp"
#include "GLS.SkyDome.hpp"
#include "GLSL.UserShader.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.TextureFormat.hpp"
#include "GLS.Color.hpp"
#include "GLS.OpenGLTokens.hpp"
#include "GLS.OpenGLAdapter.hpp"
#include "GLS.Context.hpp"
#include "GLS.RenderContextInfo.hpp"
#include "GLS.State.hpp"
#include "GLS.Utils.hpp"
#include "GLS.FileTGA.hpp"


//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLSphere *GLSphere1;
	TGLDirectOpenGL *DOInitialize;
	TGLDirectOpenGL *DOOceanPlane;
	TGLHeightField *GLHeightField1;
	TGLSphere *GLSphere2;
	TGLCamera *GLCamera;
	TGLMaterialLibrary *MatLib;
	TGLCadencer *GLCadencer1;
	TGLUserShader *GLUserShader1;
	TGLMemoryViewer *GLMemoryViewer1;
	TGLScene *GLScene2;
	TGLEarthSkyDome *GLEarthSkyDome1;
	TGLCamera *CameraCubeMap;
	TGLSimpleNavigation *GLSimpleNavigation1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall DOInitializeRender(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall GLUserShader1DoApply(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall GLUserShader1DoUnApply(TObject *Sender, int Pass, TGLRenderContextInfo &rci,
          bool &Continue);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLHeightField1GetHeight(const float x, const float y, float &z,
          TVector4f &color, TTexPoint &texPoint);
	void __fastcall DOOceanPlaneRender(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall GLSceneViewer1BeforeRender(TObject *Sender);


private:	// User declarations
	int mx, my, dmx, dmy;
	TGLProgramHandle *programObject;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
