//---------------------------------------------------------------------------

#ifndef fMainCH
#define fMainCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SkyDome.hpp"
#include "GLS.GeomObjects.hpp"
///#include "GLS.Imposter.hpp"   ///don't work properly
#include "GLS.SceneViewer.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *LabelTexSize;
	TLabel *Label2;
	TLabel *LabelFPS;
	TCheckBox *CBShowTeapot;
	TCheckBox *CBShowImposter;
	TComboBox *CBSampleSize;
	TGLScene *GLScene1;
	TGLDummyCube *GLDummyCube1;
	TGLSkyDome *GLSkyDome1;
	TGLDirectOpenGL *GLDirectOpenGL1;
	TGLTeapot *GLTeapot1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall CBShowTeapotClick(TObject *Sender);
	void __fastcall CBShowImposterClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall GLDirectOpenGL1Render(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall FormResize(TObject *Sender);


private:	// User declarations
  int mx, my;
///  TGLStaticImposterBuilder *impBuilder;
///  TGLRenderPoint *renderPoint;

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
