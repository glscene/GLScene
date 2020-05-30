//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLExtrusion.hpp"
#include "GLGeomObjects.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *DummyCube1;
	TGLDummyCube *Bolt;
	TGLRevolutionSolid *RSBoltHead;
	TGLCylinder *CYBoltShaft;
	TGLRevolutionSolid *RSBoltThreads;
	TGLDummyCube *Nut;
	TGLRevolutionSolid *RSNutThreads;
	TGLRevolutionSolid *RSNutPans;
	TGLAnnulus *Annulus1;
	TGLCamera *GLCamera1;
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
private:	// User declarations
    int mx, my;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
