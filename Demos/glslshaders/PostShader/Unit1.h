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
#include <Vcl.ComCtrls.hpp>

#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.Graph.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SimpleNavigation.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Utils.hpp"

// FileFormats
#include "GLS.FileTGA.hpp"
#include "GLS.FileMD2.hpp"
#include "GLS.FileMS3D.hpp"
#include "GLS.File3DS.hpp"

// Shader headers
#include "GLSL.PostShaders.hpp"
#include "GLSL.PostEffects.hpp"
#include "Cg.PostTransformationShader.hpp"

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
	void __fastcall ShaderCheckListBoxClick(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall tbThermalThresholdChange(TObject *Sender);
	void __fastcall tbBlurValueChange(TObject *Sender);
	void __fastcall tbDreamThresholdChange(TObject *Sender);
	void __fastcall tbThermalIntensityChange(TObject *Sender);
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
