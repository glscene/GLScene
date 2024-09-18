//---------------------------------------------------------------------------

#ifndef fEventsCH
#define fEventsCH
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.TimeEventsMgr.hpp"
#include "GLS.SceneViewer.hpp"
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
//---------------------------------------------------------------------------
class TFormEvents : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLDummyCube *DummyCube1;
	TGLCube *Cube1;
	TGLCube *Cube2;
	TGLCube *Cube3;
	TGLCamera *Camera1;
	TTimer *Timer1;
	TGLCadencer *GLCadencer1;
	TGLTimeEventsMGR *GLTimeEventsMGR1;
	TStatusBar *StatusBar;
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall GLTimeEventsMGR1Events0Event(TTimeEvent *event);
	void __fastcall GLTimeEventsMGR1Events1Event(TTimeEvent *event);
	void __fastcall GLTimeEventsMGR1Events2Event(TTimeEvent *event);
	void __fastcall GLTimeEventsMGR1Events3Event(TTimeEvent *event);
	void __fastcall GLTimeEventsMGR1Events4Event(TTimeEvent *event);
	void __fastcall GLTimeEventsMGR1Events5Event(TTimeEvent *event);
private:	// User declarations
public:		// User declarations
	__fastcall TFormEvents(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormEvents *FormEvents;
//---------------------------------------------------------------------------
#endif
