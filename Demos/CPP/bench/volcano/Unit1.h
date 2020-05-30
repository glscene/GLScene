//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLBehaviours.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLObjects.hpp"
#include "GLParticleFX.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TRadioGroup *RadioGroup1;
	TPanel *Panel1;
	TGLScene *GLScene1;
	TGLDummyCube *DCVolcano;
	TGLSphere *Sphere1;
	TGLParticleFXRenderer *PFXRenderer;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *DCCamera;
	TGLCamera *GLCamera1;
	TGLPolygonPFXManager *PFXVolcano;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLPolygonPFXManager *PFXBlue;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall RadioGroup1Click(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall Timer1Timer(TObject *Sender);

private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
