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
#include "GLSL.CustomShader.hpp"
#include "GLGeomObjects.hpp"
#include "GLGraph.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLSL.Shader.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLSceneViewer.hpp"
#include "GLFileTGA.hpp"
#include "GLFileSMD.hpp"
#include "GLFileMD2.hpp"
#include "GLFile3DS.hpp"
#include "FileDDSImage.hpp"
#include "GLFileMS3D.hpp"
#include "GLS.Utils.hpp"
#include "GLSL.DiffuseSpecularShader.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *Viewer;
	TPanel *Panel1;
	TCheckBox *LightMovingCheckBox;
	TCheckBox *ShaderEnabledCheckBox;
	TCheckBox *PitchRollTurnCheckBox;
	TCheckBox *RealisticSpecularCheckBox;
	TCheckBox *MultiLightShaderCheckBox;
	TCheckBox *EnableFogCheckBox;
	TGLScene *Scene;
	TGLDummyCube *GUICube;
	TGLArrowLine *GLArrowLine1;
	TGLXYZGrid *GLXYZGrid1;
	TGLDummyCube *LightCube;
	TGLLightSource *Light;
	TGLSphere *GLSphere1;
	TGLDummyCube *LightCube2;
	TGLLightSource *Light2;
	TGLSphere *GLSphere2;
	TGLDummyCube *WorldCube;
	TGLActor *Fighter;
	TGLActor *Teapot;
	TGLActor *Sphere_big;
	TGLActor *Sphere_little;
	TGLCamera *Camera;
	TGLCadencer *Cadencer;
	TGLMaterialLibrary *MaterialLibrary;
	TGLSLDiffuseSpecularShader *DiffuseSpecularShader;
	TGLSimpleNavigation *GLSimpleNavigation1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall EnableFogCheckBoxClick(TObject *Sender);
	void __fastcall CadencerProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall MultiLightShaderCheckBoxClick(TObject *Sender);
	void __fastcall RealisticSpecularCheckBoxClick(TObject *Sender);
	void __fastcall LightCubeProgress(TObject *Sender, const double deltaTime, const double newTime);


private:	// User declarations
    int mx,my;
	TGLSLMLDiffuseSpecularShader *MultiLightShader;

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
