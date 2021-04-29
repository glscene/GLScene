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
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLSL.CustomShader.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.Graph.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.FileTGA.hpp"
#include "GLS.FileSMD.hpp"
#include "GLS.FileMD2.hpp"
#include "GLS.File3DS.hpp"
#include "Formats.DDSImage.hpp"
#include "GLS.FileMS3D.hpp"

#include "GLS.Utils.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLSL.Shader.hpp"
#include "GLSL.BumpShaders.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *Viewer;
	TPanel *Panel1;
	TCheckBox *LightMovingCheckBox;
	TCheckBox *RollPitchTurnCheckBox;
	TCheckBox *ShaderEnabledCheckBox;
	TCheckBox *MultiLightShaderCheckBox;
	TCheckBox *UseSpecularTextureCheckBox;
	TCheckBox *UseNormalTextureCheckBox;
	TCheckBox *ShowNotGLSceneObjectsCheckBox;
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
	TGLActor *Sphere_little;
	TGLActor *Sphere_big;
	TGLActor *Teapot;
	TGLActor *Fighter;
	TGLCube *GLCube;
	TGLDodecahedron *GLDodecahedron;
	TGLSphere *GLSphere;
	TGLCamera *Camera;
	TGLCadencer *Cadencer;
	TTimer *Timer1;
	TGLMaterialLibrary *MaterialLibrary;
	TGLSLBumpShader *MyBumpShader;
	TGLMaterialLibrary *TrinityMatlib;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ShaderEnabledCheckBoxClick(TObject *Sender);
	void __fastcall ShowNotGLSceneObjectsCheckBoxClick(TObject *Sender);
	void __fastcall MultiLightShaderCheckBoxClick(TObject *Sender);
	void __fastcall UseSpecularTextureCheckBoxClick(TObject *Sender);
	void __fastcall UseNormalTextureCheckBoxClick(TObject *Sender);
	void __fastcall CadencerProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall ViewerMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall LightCubeProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);



private:	// User declarations
  int mx, my;
  TGLSLMLBumpShader *MultiLightShader;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
