//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Imaging.Jpeg.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLSL.CustomShader.hpp"
#include "GLS.FBORenderer.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.HUDObjects.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SimpleNavigation.hpp"
#include "GLSLShader.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "OpenGLx.hpp"
#include "Formats.DDSImage.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLCamera *GLCamera1;
	TGLCamera *GLCamera2;
	TGLLightSource *GLLightSource1;
	TGLSphere *GLSphere1;
	TGLFBORenderer *LightFBORenderer;
	TGLDummyCube *SceneRoot;
	TGLDirectOpenGL *PrepareShadowMapping;
	TGLPlane *GLPlane1;
	TGLTorus *GLTorus1;
	TGLCylinder *GLCylinder1;
	TGLFreeForm *GLFreeForm1;
	TGLHUDSprite *GLShadowTextureSprite;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLSLShader *GLSLShader1;
	TGLSLShader *GLSLShader2;
	TGLSimpleNavigation *GLNavigation;
	void __fastcall GLSLShader1Apply(TGLCustomGLSLShader *Shader);
	void __fastcall GLSLShader1UnApply(TGLCustomGLSLShader *Shader, bool &ThereAreMorePasses);
	void __fastcall GLSLShader2Apply(TGLCustomGLSLShader *Shader);
	void __fastcall GLSLShader2Initialize(TGLCustomGLSLShader *Shader);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLSceneViewer1BeforeRender(TObject *Sender);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall PrepareShadowMappingRender(TObject *Sender, TGLRenderContextInfo &rci);

private:	// User declarations
	TMatrix FBiasMatrix;
	TMatrix FLightModelViewMatrix;
	TMatrix FLightProjMatrix;
	TMatrix FInvCameraMatrix;
	TMatrix FEyeToLightMatrix;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
