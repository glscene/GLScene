//---------------------------------------------------------------------------

#ifndef fCelShadingCH
#define fCelShadingCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include "GLS.AsyncTimer.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.FileMD2.hpp"
#include "GLS.Utils.hpp"
#include "Cg.GL.hpp"
#include "Cg.Shader.hpp"
#include <Vcl.ComCtrls.hpp>

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLDummyCube *GLDummyCube1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLActor *GLActor1;
	TCgShader *CgCellShader;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLCadencer *GLCadencer1;
	TGLAsyncTimer *AsyncTimer1;
	TStatusBar *StatusBar1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall CgCellShaderApplyVP(TCgProgram *CgProgram, TObject *Sender);
	void __fastcall CgCellShaderInitialize(TCustomCgShader *CgShader);
	void __fastcall CgCellShaderApplyFP(TCgProgram *CgProgram, TObject *Sender);
	void __fastcall CgCellShaderUnApplyFP(TCgProgram *CgProgram);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall AsyncTimer1Timer(TObject *Sender);
private:	// User declarations
    int mx,my;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
