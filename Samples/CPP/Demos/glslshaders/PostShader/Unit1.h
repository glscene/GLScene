//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.CheckLst.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLGraph.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLPostEffects.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLUtils.hpp"

#include "GLSLPostShaders.hpp"
#include "GLCGPostTransformationShader.hpp"

// FileFormats
#include "GLFileTGA.hpp"
#include "GLFileMD2.hpp"
#include "GLFileMS3D.hpp"
#include "GLFile3DS.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *Viewer;
	TPanel *Panel1;
	TCheckBox *LightMovingCheckBox;
	TCheckBox *TurnPitchrollCheckBox;
	TPanel *Panel2;
	TLabel *Label1;
	TCheckListBox *ShaderCheckListBox;
	TCheckBox *BigBlurThicknessCheckbox;
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
	TGLPostShaderHolder *PostShaderHolder;
	TGLCamera *Camera;
	TGLCadencer *Cadencer;
	TGLMaterialLibrary *MaterialLibrary;
	TGLSimpleNavigation *GLSimpleNavigation1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall CadencerProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall LightCubeProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall BigBlurThicknessCheckboxClick(TObject *Sender);
	void __fastcall ShaderCheckListBoxClick(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);


private:	// User declarations
	int mx, my;
	TGLSLPostBlurShader *BlurShader;
	TGLCGPostTransformationShader *TransformationShader;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
