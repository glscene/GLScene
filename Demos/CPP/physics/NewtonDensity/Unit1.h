//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Samples.Spin.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLHUDObjects.hpp"
#include "GLNGDManager.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLSceneViewer.hpp"
#include "NGDImport.hpp"
#include "GLBitmapFont.hpp";
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
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);

private:	// User declarations
	int mx,my;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);

	void MyForceAndTorqueDensity(const PNewtonBody cbody,
	  NGDFloat timestep, int threadIndex);

	void Shoot(void);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
