//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLExtrusion.hpp"
#include "GLFireFX.hpp"
#include "GLGeomObjects.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TTrackBar *TrackBar1;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *DummyCube1;
	TGLRevolutionSolid *RevolutionSolid1;
	TGLCylinder *Candle;
	TGLLines *Lines1;
	TGLDummyCube *DummyCube2;
	TGLPlane *Plane1;
	TGLProxyObject *GLProxyObject1;
	TGLProxyObject *GLProxyObject2;
	TGLCamera *GLCamera1;
	TGLFireFXManager *GLFireFXManager1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	void __fastcall FormResize(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall TrackBar1Change(TObject *Sender);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall Timer1Timer(TObject *Sender);

private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
