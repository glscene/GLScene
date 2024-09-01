//---------------------------------------------------------------------------

#ifndef fOdeMachineCH
#define fOdeMachineCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.BitmapFont.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.GeomObjects.hpp"
#include "GLS.HUDObjects.hpp"
#include "GLS.Objects.hpp"
#include "GLS.ODEManager.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.WindowsFont.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLDummyCube *GLDummyCube1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *Machine;
	TGLCylinder *Wheel;
	TGLCylinder *Axle;
	TGLCylinder *Pin1;
	TGLCube *Arm;
	TGLCylinder *Pin2;
	TGLCube *Slider;
	TGLRenderPoint *ODERenderPoint;
	TGLHUDText *GLHUDText1;
	TGLODEManager *GLODEManager1;
	TGLODEJointList *GLODEJointList1;
	TGLCadencer *GLCadencer1;
	TGLWindowsBitmapFont *GLWindowsBitmapFont1;
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);

private:	// User declarations
    int my, mx;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
