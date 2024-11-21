//---------------------------------------------------------------------------

#ifndef fSubdivideCH
#define fSubdivideCH
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

#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Material.hpp"
#include "GLS.Scene.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"

#include "GLS.MeshUtils.hpp"
#include "Stage.VectorGeometry.hpp"

#include "GLS.FileTGA.hpp"
#include "GLS.FileObj.hpp"
#include "GLS.File3DS.hpp"
#include "GLS.FileMD2.hpp"
#include "GLS.FileSMD.hpp"

#include "Stage.Utils.hpp"
#include "GLS.MeshUtils.hpp"


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
