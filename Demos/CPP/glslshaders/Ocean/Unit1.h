//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGraph.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLSkydome.hpp"
#include "GLUserShader.hpp"
#include "GLWin32Viewer.hpp"
#include "GLTextureFormat.hpp"
#include "GLColor.hpp"
#include "OpenGLTokens.hpp"
#include "OpenGLAdapter.hpp"
#include "GLContext.hpp"
#include "GLRenderContextInfo.hpp"
#include "GLState.hpp"
#include "GLUtils.hpp"
#include "GLFileTGA.hpp"


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
