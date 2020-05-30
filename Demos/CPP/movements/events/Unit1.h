//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
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
#include "GLScene.hpp"
#include "GLTimeEventsMgr.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
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
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall GLTimeEventsMGR1Events0Event(TTimeEvent *event);
	void __fastcall GLTimeEventsMGR1Events1Event(TTimeEvent *event);
	void __fastcall GLTimeEventsMGR1Events2Event(TTimeEvent *event);
	void __fastcall GLTimeEventsMGR1Events3Event(TTimeEvent *event);
	void __fastcall GLTimeEventsMGR1Events4Event(TTimeEvent *event);
	void __fastcall GLTimeEventsMGR1Events5Event(TTimeEvent *event);
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
