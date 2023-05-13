//---------------------------------------------------------------------------

#ifndef fNutsnBoltsCH
#define fNutsnBoltsCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Extrusion.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
//---------------------------------------------------------------------------
class TFormNutsnBolts : public TForm
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
	__fastcall TFormNutsnBolts(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormNutsnBolts *FormNutsnBolts;
//---------------------------------------------------------------------------
#endif
