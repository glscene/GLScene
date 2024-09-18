//---------------------------------------------------------------------------

#ifndef fBlurAdvancedCH
#define fBlurAdvancedCH
//---------------------------------------------------------------------------
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLS.BaseClasses.hpp"
#include "GLS.Blur.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.GeomObjects.hpp"
#include "GLS.HUDObjects.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include <Vcl.Imaging.jpeg.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TBevel *Bevel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TLabel *LabelFPS;
	TEdit *edtAdvancedBlurAmp;
	TEdit *edtAdvancedBlurPasses;
	TTrackBar *trkAdvancedBlurHiClamp;
	TTrackBar *trkAdvancedBlurLoClamp;
	TMemo *Memo1;
	TGLScene *GLScene1;
	TGLSphere *GLSphere1;
	TGLTorus *GLTorus2;
	TGLDummyCube *GLDummyCube1;
	TGLTorus *TorusImpostor;
	TGLCube *GLCube1;
	TGLAnnulus *GLAnnulus1;
	TGLBlur *GLBlur1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLBlur1BeforeTargetRender(TObject *Sender);
	void __fastcall GLBlur1AfterTargetRender(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall trkAdvancedBlurHiClampChange(TObject *Sender);
	void __fastcall trkAdvancedBlurLoClampChange(TObject *Sender);
	void __fastcall edtAdvancedBlurAmpChange(TObject *Sender);
	void __fastcall edtAdvancedBlurPassesChange(TObject *Sender);

private:	// User declarations
	int mx,my;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
