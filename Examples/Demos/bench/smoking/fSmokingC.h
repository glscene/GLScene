//---------------------------------------------------------------------------

#ifndef fSmokingCH
#define fSmokingCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Objects.hpp"
#include "GLS.ParticleFX.hpp"
#include "GLS.PerlinPFX.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TFormSmoking : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer;
	TPanel *Panel1;
	TGLScene *GLScene;
	TGLDummyCube *DCFire1;
	TGLDummyCube *DCFire2;
	TGLDummyCube *DCFire3;
	TGLDummyCube *DCFire4;
	TGLDummyCube *DCFire5;
	TGLParticleFXRenderer *ParticleFXRenderer;
	TGLDummyCube *DCTarget;
	TGLCamera *GLCamera;
	TGLPerlinPFXManager *SmokePFX;
	TGLCustomSpritePFXManager *FlamePFX;
	TGLCadencer *GLCadencer;
	TTimer *Timer;
	void __fastcall GLCadencerProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall TimerTimer(TObject *Sender);

private:	// User declarations
public:		// User declarations
	__fastcall TFormSmoking(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormSmoking *FormSmoking;
//---------------------------------------------------------------------------
#endif
