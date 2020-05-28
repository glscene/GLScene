//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLBitmapFont.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLHUDObjects.hpp"
#include "GLObjects.hpp"
#include "GLODEManager.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "GLWindowsFont.hpp"
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
