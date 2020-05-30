//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLCustomShader.hpp"
#include "GLGeomObjects.hpp"
#include "GLGraph.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLSLShader.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLUtils.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *Viewer;
	TPanel *Panel1;
	TCheckBox *LightMovingCheckBox;
	TCheckBox *ShadeEnabledCheckBox;
	TCheckBox *PitchRollTurnCheckBox;
	TGLScene *Scene;
	TGLDummyCube *GUICube;
	TGLArrowLine *GLArrowLine1;
	TGLXYZGrid *GLXYZGrid1;
	TGLDummyCube *LightCube;
	TGLLightSource *Light;
	TGLSphere *GLSphere1;
	TGLDummyCube *WorldCube;
	TGLActor *Fighter;
	TGLActor *Teapot;
	TGLActor *Sphere_big;
	TGLActor *Sphere_little;
	TGLCamera *Camera;
	TGLCadencer *Cadencer;
	TGLMaterialLibrary *MaterialLibrary;
	TGLSLShader *GLSLShader;
	TGLSimpleNavigation *GLSimpleNavigation1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ShadeEnabledCheckBoxClick(TObject *Sender);
	void __fastcall GLSLShaderApply(TGLCustomGLSLShader *Shader);
	void __fastcall GLSLShaderInitialize(TGLCustomGLSLShader *Shader);
	void __fastcall GLSLShaderUnApply(TGLCustomGLSLShader *Shader, bool &ThereAreMorePasses);
	void __fastcall CadencerProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall LightCubeProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);



private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
