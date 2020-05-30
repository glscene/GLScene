//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMultiProxy.hpp"
#include "GLObjects.hpp"
#include "GLParticles.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "GLTexture.hpp"
#include "GLVectorgeometry.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *LabelFPS;
	TRadioButton *RBUseLODs;
	TRadioButton *RBHighRes;
	TCheckBox *CBColorize;
	TRadioButton *RBLowRes;
	TGLScene *GLScene;
	TGLDummyCube *DCTarget;
	TGLParticles *GLParticles;
	TGLMultiProxy *MPSphere;
	TGLDummyCube *DCReferences;
	TGLSphere *SPHighRes;
	TGLSphere *SPMedRes;
	TGLSphere *SPLowRes;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera;
	TGLCadencer *GLCadencer;
	TTimer *Timer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall RBUseLODsClick(TObject *Sender);
	void __fastcall MPSphereProgress(TObject *Sender, const double deltaTime, const double newTime);


private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
