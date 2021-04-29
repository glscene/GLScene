//---------------------------------------------------------------------------

#ifndef fWhirlCH
#define fWhirlCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Behaviours.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Objects.hpp"
#include "GLS.Particles.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TFormWhirlC : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLScene *GLScene1;
	TGLParticles *GLParticles1;
	TGLDummyCube *DummyCube1;
	TGLSprite *Sprite1;
	TGLCamera *GLCamera1;
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y);
	void __fastcall GLDummyCube1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLParticles1ActivateParticle(TObject *Sender, TGLBaseSceneObject *particle);
	void __fastcall Timer1Timer(TObject *Sender);




private:	// User declarations
    int mx, my;
public:		// User declarations
	__fastcall TFormWhirlC(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormWhirlC *FormWhirlC;
//---------------------------------------------------------------------------
#endif
