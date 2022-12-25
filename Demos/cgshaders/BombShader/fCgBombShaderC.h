//---------------------------------------------------------------------------

#ifndef fCgBombShaderCH
#define fCgBombShaderCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Graph.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SimpleNavigation.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "Cg.BombShader.hpp"
#include "Cg.Shader.hpp"
#include "JPeg.hpp"
#include "GLS.FileMD2.hpp"
#include "GLS.File3DS.hpp"
#include "GLS.Utils.hpp"


//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TSplitter *Splitter1;
	TPanel *Panel1;
	TComboBox *ComboBox1;
	TGroupBox *GroupBox1;
	TCheckBox *CheckBox1;
	TCheckBox *CheckBox2;
	TCheckBox *CheckBox3;
	TCheckBox *CheckBox4;
	TCheckBox *ShaderEnabledCheckBox;
	TTrackBar *TrackBar1;
	TTrackBar *TrackBar2;
	TTrackBar *TrackBar3;
	TTrackBar *TrackBar4;
	TTrackBar *TrackBar5;
	TTrackBar *TrackBar6;
	TTrackBar *TrackBar7;
	TTrackBar *TrackBar8;
	TTrackBar *TrackBar9;
	TPanel *Panel9;
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLFreeForm *ffSphere1;
	TGLFreeForm *ffSphere2;
	TGLFreeForm *ffTeapot;
	TGLDummyCube *GLDummyCube1;
	TGLActor *GLActor1;
	TGLXYZGrid *GLXYZGrid1;
	TGLLightSource *GLLightSource1;
	TGLCube *JustATestCube;
	TGLCamera *GLCamera1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ComboBox1Change(TObject *Sender);
	void __fastcall CheckBox1Click(TObject *Sender);
	void __fastcall TrackBar1Change(TObject *Sender);
	void __fastcall TrackBar2Change(TObject *Sender);
	void __fastcall TrackBar3Change(TObject *Sender);
	void __fastcall TrackBar4Change(TObject *Sender);
	void __fastcall TrackBar5Change(TObject *Sender);
	void __fastcall TrackBar6Change(TObject *Sender);
	void __fastcall TrackBar7Change(TObject *Sender);
	void __fastcall TrackBar8Change(TObject *Sender);
	void __fastcall TrackBar9Change(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall ShaderEnabledCheckBoxClick(TObject *Sender);

private:	// User declarations
	int mx, my;
	TCgBombShader *MyShader;
	void __fastcall ResetPositions();
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);

};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
