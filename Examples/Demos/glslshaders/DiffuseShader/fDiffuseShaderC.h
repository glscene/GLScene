//---------------------------------------------------------------------------

#ifndef fDiffuseShaderCH
#define fDiffuseShaderCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLSL.CustomShader.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.Graph.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SimpleNavigation.hpp"
#include "GLSL.Shader.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.FileTGA.hpp"
#include "GLS.FileSMD.hpp"
#include "GLS.FileMD2.hpp"
#include "GLS.File3DS.hpp"
#include "GLS.FileMS3D.hpp"
#include "Stage.Utils.hpp"
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
