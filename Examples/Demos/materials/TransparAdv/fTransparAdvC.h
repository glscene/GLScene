//---------------------------------------------------------------------------

#ifndef fTransparAdvCH
#define fTransparAdvCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.BitmapFont.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.FBORenderer.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.HUDObjects.hpp"
#include "GLS.Material.hpp"
#include "GLS.Mesh.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.SimpleNavigation.hpp"
#include "GLS.WindowsFont.hpp"
#include "GLSL.CustomShader.hpp"
#include "GLSL.Shader.hpp"
//---------------------------------------------------------------------------
class TFormTransparAdv : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLDirectOpenGL *ClearFrameBuffer;
	TGLDummyCube *ObjectContainer;
	TGLDummyCube *Surround;
	TGLCylinder *GLCylinder1;
	TGLDisk *GLDisk1;
	TGLMesh *GLMesh1;
	TGLMesh *GLMesh2;
	TGLMesh *GLMesh3;
	TGLMesh *GLMesh4;
	TGLMesh *GLMesh5;
	TGLFBORenderer *LayeredFrameBuffer;
	TGLDirectOpenGL *CustomRederer;
	TGLHUDSprite *ScreenQuad;
	TGLHUDText *GLHUDText1;
	TGLCadencer *GLCadencer1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLSLShader *GLSLShader1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	TGLWindowsBitmapFont *GLWindowsBitmapFont1;
private:	// User declarations
public:		// User declarations
	__fastcall TFormTransparAdv(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormTransparAdv *FormTransparAdv;
//---------------------------------------------------------------------------
#endif
