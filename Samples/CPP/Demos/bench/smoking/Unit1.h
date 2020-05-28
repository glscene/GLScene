//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLObjects.hpp"
#include "GLParticleFX.hpp"
#include "GLPerlinPFX.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
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
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
