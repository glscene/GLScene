//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include "GLAsyncTimer.hpp"
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCgShader.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLFileMD2.hpp"
#include "GLUtils.hpp"
#include "CgGL.hpp"

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
