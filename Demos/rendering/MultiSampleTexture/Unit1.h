//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.BitmapFont.hpp"
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
#include "GLSL.Shader.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.WindowsFont.hpp"
#include "GLS.State.hpp"
#include "GLS.Context.hpp"
#include "GLS.Keyboard.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *MainViewer;
	TGLScene *MainScene;
	TGLCamera *MainCamera;
	TGLLightSource *TestLight;
	TGLDummyCube *FBOContainer;
	TGLFBORenderer *MultisampleFBO;
	TGLDummyCube *SceneObjects;
	TGLSphere *GLSphere1;
	TGLTorus *GLTorus1;
	TGLTorus *GLTorus2;
	TGLCone *GLCone1;
	TGLLines *GLLines1;
	TGLHUDSprite *GLScreenQuad;
	TGLHUDText *GLHUDText1;
	TGLCadencer *MainCadencer;
	TGLMaterialLibrary *MainMaterialLibrary;
	TGLSLShader *GLSLShader1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	TGLWindowsBitmapFont *GLWindowsBitmapFont1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall MainCadencerProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall GLSLShader1Apply(TGLCustomGLSLShader *Shader);
	void __fastcall MainViewerBeforeRender(TObject *Sender);

private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
