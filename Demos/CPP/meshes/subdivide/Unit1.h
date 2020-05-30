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
#include <Vcl.Imaging.jpeg.hpp>

#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLScene.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"

#include "GLMeshUtils.hpp"
#include "GLVectorGeometry.hpp"

#include "GLFileTGA.hpp"
#include "GLFileObj.hpp"
#include "GLFile3DS.hpp"
#include "GLFileMD2.hpp"
#include "GLFileSMD.hpp"

#include "GLUtils.hpp"
#include "GLMeshUtils.hpp"


//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label1;
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *LASubdivideTime;
	TButton *BULoad;
	TButton *BUSubdivide;
	TTrackBar *TrackBar1;
	TRadioButton *RBWireFrame;
	TRadioButton *RBSolid;
	TCheckBox *CBAnimate;
	TGLScene *GLScene1;
	TGLActor *GLActor1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TTimer *Timer1;
	TGLCadencer *GLCadencer1;
	void __fastcall BULoadClick(TObject *Sender);
	void __fastcall BUSubdivideClick(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall RBWireFrameClick(TObject *Sender);
	void __fastcall RBSolidClick(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall CBAnimateClick(TObject *Sender);

private:	// User declarations
	int mx, my;

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
