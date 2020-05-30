//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLGraph.hpp"
#include "GLWin32Viewer.hpp"
#include "GLCadencer.hpp"
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLVectorFileObjects.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *LabelFPS;
	TButton *BULongJump;
	TCheckBox *CheckBox1;
	TButton *BUHighJump;
	TRadioButton *RBWalk;
	TRadioButton *RBRun;
	TPanel *Panel2;
	TTrackBar *TrackBar1;
	TCheckBox *CBBlend;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLActor *Actor1;
	TGLDummyCube *DummyCube1;
	TGLXYZGrid *XYZGrid1;
	TGLCamera *GLCamera1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TTimer *Timer1;
	TGLCadencer *GLCadencer1;
	TGLAnimationControler *AnimationControler1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall RBWalkClick(TObject *Sender);
	void __fastcall RBRunClick(TObject *Sender);
	void __fastcall BULongJumpClick(TObject *Sender);
	void __fastcall BUHighJumpClick(TObject *Sender);
	void __fastcall Actor1EndFrameReached(TObject *Sender);
	void __fastcall CBBlendClick(TObject *Sender);
	void __fastcall TrackBar1Change(TObject *Sender);
	void __fastcall CheckBox1Click(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);

private:	// User declarations
	String baseAnimation;
	int mx, my;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
