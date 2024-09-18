//---------------------------------------------------------------------------

#ifndef fNewtonDensityCH
#define fNewtonDensityCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Samples.Spin.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.GeomObjects.hpp"
#include "GLS.HUDObjects.hpp"
#include "NGD.Import.hpp"
#include "GLS.NGDManager.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SimpleNavigation.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.BitmapFont.hpp"
#include "GLS.NGDManager.hpp";

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TSpinEdit *SpinEdit1;
	TSpinEdit *SpinEdit2;
	TSpinEdit *SpinEdit3;
	TGLScene *GLScene1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLPlane *GLPlane1;
	TGLDummyCube *Mag;
	TGLDummyCube *obj;
	TGLCylinder *GLCylinder1;
	TGLCone *GLCone1;
	TGLCube *GLCube2;
	TGLCube *SubMarine;
	TGLSphere *GLLeadSphere;
	TGLSphere *GLPaperSphere;
	TGLCapsule *GLCapsule1;
	TGLCube *GLCube1;
	TGLHUDText *GLHUDText1;
	TGLHUDText *GLHUDText2;
	TGLHUDText *GLHUDText3;
	TGLCadencer *GLCadencer1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	TGLNGDManager *GLNGDManager1;
	TGLBitmapFont *GLBitmapFont1;
	TGLSimpleNavigation *GLSimpleNavigation2;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);

private:	// User declarations
	int mx,my;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);

	void MyForceAndTorqueDensity(const PNewtonBody cbody,
	  dFloat timestep, int threadIndex);

	void Shoot(void);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
